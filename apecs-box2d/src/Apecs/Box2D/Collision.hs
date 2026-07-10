{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-| Collision, impact, sensor, joint and body-move events from the
last 'Apecs.Box2D.Space.stepPhysics', read as global components.
-}
module Apecs.Box2D.Collision where

import Apecs
import Apecs.Core
import Control.Monad (forM)
import Control.Monad.IO.Class (MonadIO)
import Data.IORef
import Data.IntMap.Strict qualified as IM
import Data.Maybe (catMaybes)
import Data.Vector.Storable qualified as VS
import Foreign.Marshal.Utils (toBool)

import Box2D.Body qualified as B2Body
import Box2D.Contact qualified as B2Contact
import Box2D.Events qualified as B2Events
import Box2D.Id (ShapeId)
import Box2D.MathFunctions (rotGetAngle)
import Box2D.MathTypes (Transform (..), vec2Add)
import Box2D.Shape qualified as B2Shape
import Box2D.Types qualified as B2T

import Apecs.Box2D.Geometry
import Apecs.Box2D.Types

{- | A contact manifold: the surface normal and the world contact
points. Box2D uses speculative contacts, so a begin-touch manifold can
contain slightly separated points (positive separation) and can even
momentarily have no points.
-}
data ContactManifold = ContactManifold
  { contactNormal :: !WVec
  -- ^ Contact normal, pointing from A to B.
  , contactPoints :: ![WVec]
  -- ^ World contact points, up to 2 in 2D.
  }
  deriving (Eq, Show)

{- | A contact from the last 'Apecs.Box2D.Space.stepPhysics': the shapes involved, the
bodies they hang off and — for begin-touch events — the contact
manifold. 'CollisionsEnd' events never carry a manifold; a begin-touch
event lacks one only when its contact died between the step and the
read (a shape destroyed after the step).

Equality compares the participants only, ignoring the manifold, so a
begin-touch value and the end-touch value of the same contact compare
equal — active-contact bookkeeping can pair them up with e.g.
'Data.List.delete'.
-}
data Collision = Collision
  { collisionBodyA :: !Entity
  , collisionShapeA :: !Entity
  , collisionBodyB :: !Entity
  , collisionShapeB :: !Entity
  , collisionManifold :: !(Maybe ContactManifold)
  }
  deriving (Show)

instance Eq Collision where
  a == b =
    (collisionBodyA a, collisionShapeA a, collisionBodyB a, collisionShapeB a)
      == (collisionBodyA b, collisionShapeA b, collisionBodyB b, collisionShapeB b)

{- | The shape/body entities behind a contact's two shape ids, if both
are still alive and registered; no manifold.
-}
toCollision :: B2Space c -> ShapeId -> ShapeId -> IO (Maybe Collision)
toCollision sp sA sB = do
  ma <- shapeEntities sp sA
  mb <- shapeEntities sp sB
  pure $ do
    (sa, ba) <- ma
    (sb, bb) <- mb
    Just (Collision ba sa bb sb Nothing)

{- | 'toCollision' for a begin-touch event, with the contact manifold
filled in: the normal plus the world position of each manifold point
(body A's world center of mass plus the point's A-side anchor). If the
contact is no longer valid (a shape was destroyed after the step) the
manifold stays 'Nothing'.
-}
toBeginCollision :: B2Space c -> B2T.ContactBeginTouchEvent -> IO (Maybe Collision)
toBeginCollision sp ev = do
  let contact = B2T.contactBeginTouchEventContactId ev
  valid <- B2Contact.isValid contact
  if not valid then
    toCollision sp (B2T.contactBeginTouchEventShapeIdA ev) (B2T.contactBeginTouchEventShapeIdB ev)
  else do
    cd <- B2Contact.getData contact
    -- resolve from the ContactData's own shape order, so the A/B
    -- entities stay consistent with the manifold normal's A-to-B
    -- orientation
    mc <- toCollision sp (B2T.contactDataShapeIdA cd) (B2T.contactDataShapeIdB cd)
    forM mc $ \c -> do
      let
        m = B2T.contactDataManifold cd
        anchors = B2T.manifoldPoints m
      pts <-
        if VS.null anchors then
          pure []
        else do
          -- body A's world center of mass turns the manifold's A-side
          -- anchors into world points; its id comes from the body
          -- registry (toCollision just resolved the entity) rather
          -- than a getBody FFI round-trip
          bodies <- readIORef (spBodies sp)
          let Entity bIx = collisionBodyA c
          bodyA <- maybe (B2Shape.getBody (B2T.contactDataShapeIdA cd)) pure (IM.lookup bIx bodies)
          comA <- B2Body.getWorldCenter bodyA
          pure (map (vec2Add comA . B2T.manifoldPointAnchorA) (VS.toList anchors))
      pure c{collisionManifold = Just (ContactManifold (B2T.manifoldNormal m) pts)}

{- | The begin-touch contacts of the last 'Apecs.Box2D.Space.stepPhysics', a read-only
global: @Collisions touches <- get global@ after stepping. Shapes
created by this layer opt into contact events; events whose shapes
were destroyed since the step are dropped.
-}
newtype Collisions = Collisions [Collision]
  deriving (Show)

instance Component Collisions where
  type Storage Collisions = B2Space Collisions

instance (MonadIO m, Has w m Physics) => Has w m Collisions where
  getStore = cast <$> (getStore :: SystemT w m (B2Space Physics))

instance (MonadIO m) => ExplGet m (B2Space Collisions) where
  explExists _ _ = pure True
  explGet sp _ = liftIO $ do
    evs <- B2Events.contactBeginTouchEvents (spWorld sp)
    fmap (Collisions . catMaybes) . forM (VS.toList evs) $ \ev ->
      toBeginCollision sp ev

{- | The end-touch contacts of the last 'Apecs.Box2D.Space.stepPhysics', a read-only
global: @CollisionsEnd separations <- get global@ after stepping — the
counterpart of 'Collisions' for contacts that stopped touching. Events
whose shapes were destroyed since the step are dropped; this bites
harder here than for begin-touch, since destroying a shape mid-contact
drops its end event — clean up any per-contact bookkeeping when
destroying shapes. End events carry no manifold ('collisionManifold'
is 'Nothing').
-}
newtype CollisionsEnd = CollisionsEnd [Collision]
  deriving (Show)

instance Component CollisionsEnd where
  type Storage CollisionsEnd = B2Space CollisionsEnd

instance (MonadIO m, Has w m Physics) => Has w m CollisionsEnd where
  getStore = cast <$> (getStore :: SystemT w m (B2Space Physics))

instance (MonadIO m) => ExplGet m (B2Space CollisionsEnd) where
  explExists _ _ = pure True
  explGet sp _ = liftIO $ do
    evs <- B2Events.contactEndTouchEvents (spWorld sp)
    fmap (CollisionsEnd . catMaybes) . forM (VS.toList evs) $ \ev ->
      toCollision sp (B2T.contactEndTouchEventShapeIdA ev) (B2T.contactEndTouchEventShapeIdB ev)

{- | An above-threshold impact from the last 'Apecs.Box2D.Space.stepPhysics': the entities
involved, the world-space contact point, the contact normal (pointing
from A to B) and the approach speed. Only generated when the approach
speed exceeds the world's hit-event threshold (engine default 1;
tune with 'Apecs.Box2D.Space.HitEventThreshold').
-}
data Impact = Impact
  { impactBodyA :: !Entity
  , impactShapeA :: !Entity
  , impactBodyB :: !Entity
  , impactShapeB :: !Entity
  , impactPoint :: !WVec
  , impactNormal :: !WVec
  , impactSpeed :: !Float
  }
  deriving (Eq, Show)

{- | The impacts of the last 'Apecs.Box2D.Space.stepPhysics', a read-only global:
@Impacts hits <- get global@ after stepping.
-}
newtype Impacts = Impacts [Impact]
  deriving (Show)

instance Component Impacts where
  type Storage Impacts = B2Space Impacts

instance (MonadIO m, Has w m Physics) => Has w m Impacts where
  getStore = cast <$> (getStore :: SystemT w m (B2Space Physics))

instance (MonadIO m) => ExplGet m (B2Space Impacts) where
  explExists _ _ = pure True
  explGet sp _ = liftIO $ do
    evs <- B2Events.contactHitEvents (spWorld sp)
    fmap (Impacts . catMaybes) . forM (VS.toList evs) $ \ev -> do
      ma <- shapeEntities sp (B2T.contactHitEventShapeIdA ev)
      mb <- shapeEntities sp (B2T.contactHitEventShapeIdB ev)
      pure $ do
        (sa, ba) <- ma
        (sb, bb) <- mb
        Just
          Impact
            { impactBodyA = ba
            , impactShapeA = sa
            , impactBodyB = bb
            , impactShapeB = sb
            , impactPoint = B2T.contactHitEventPoint ev
            , impactNormal = B2T.contactHitEventNormal ev
            , impactSpeed = B2T.contactHitEventApproachSpeed ev
            }

{- | A sensor overlap that began or ended during the last 'Apecs.Box2D.Space.stepPhysics':
the 'Apecs.Box2D.Shape.Sensor' shape (and the body it hangs off) and the visitor shape
(and its body) that overlapped it.
-}
data SensorEvent = SensorEvent
  { sensorBody :: !Entity
  , sensorShape :: !Entity
  , visitorBody :: !Entity
  , visitorShape :: !Entity
  }
  deriving (Eq, Show)

-- | The shape/body entities behind a sensor overlap's two shape ids, if both are still alive and registered.
toSensorEvent :: B2Space c -> ShapeId -> ShapeId -> IO (Maybe SensorEvent)
toSensorEvent sp sensorS visitorS = do
  ms <- shapeEntities sp sensorS
  mv <- shapeEntities sp visitorS
  pure $ do
    (sShape, sBody) <- ms
    (vShape, vBody) <- mv
    Just (SensorEvent sBody sShape vBody vShape)

{- | The sensor overlaps that began and ended during the last
'Apecs.Box2D.Space.stepPhysics', a read-only global: @SensorEvents begins ends <- get
global@ after stepping. Shapes created by this layer opt into sensor
events, both as sensors and as visitors; events whose sensor or visitor
shape was destroyed since the step are dropped.
-}
data SensorEvents = SensorEvents
  { sensorBegins :: [SensorEvent]
  , sensorEnds :: [SensorEvent]
  }
  deriving (Show)

instance Component SensorEvents where
  type Storage SensorEvents = B2Space SensorEvents

instance (MonadIO m, Has w m Physics) => Has w m SensorEvents where
  getStore = cast <$> (getStore :: SystemT w m (B2Space Physics))

instance (MonadIO m) => ExplGet m (B2Space SensorEvents) where
  explExists _ _ = pure True
  explGet sp _ = liftIO $ do
    begins <- B2Events.sensorBeginTouchEvents (spWorld sp)
    ends <- B2Events.sensorEndTouchEvents (spWorld sp)
    beginEvs <-
      fmap catMaybes . forM (VS.toList begins) $ \ev ->
        toSensorEvent sp (B2T.sensorBeginTouchEventSensorShapeId ev) (B2T.sensorBeginTouchEventVisitorShapeId ev)
    endEvs <-
      fmap catMaybes . forM (VS.toList ends) $ \ev ->
        toSensorEvent sp (B2T.sensorEndTouchEventSensorShapeId ev) (B2T.sensorEndTouchEventVisitorShapeId ev)
    pure (SensorEvents beginEvs endEvs)

{- | The joints whose force or torque threshold ('Apecs.Box2D.Joint.JointForceThreshold',
'Apecs.Box2D.Joint.JointTorqueThreshold') was exceeded during the last 'Apecs.Box2D.Space.stepPhysics', a
read-only global: @JointEvents overloaded <- get global@ after
stepping. The engine leaves the joint intact — destroy the entity's
'Joint' (or lower the thresholds) yourself if it should break. Events
whose joints were destroyed since the step are dropped.
-}
newtype JointEvents = JointEvents [Entity]
  deriving (Show)

instance Component JointEvents where
  type Storage JointEvents = B2Space JointEvents

instance (MonadIO m, Has w m Physics) => Has w m JointEvents where
  getStore = cast <$> (getStore :: SystemT w m (B2Space Physics))

instance (MonadIO m) => ExplGet m (B2Space JointEvents) where
  explExists _ _ = pure True
  explGet sp _ = liftIO $ do
    evs <- B2Events.jointEvents (spWorld sp)
    fmap (JointEvents . catMaybes) . forM (VS.toList evs) $ \ev ->
      jointEntity sp (B2T.jointEventJointId ev)

{- | A body that moved during the last 'Apecs.Box2D.Space.stepPhysics': its entity, its new
transform, and whether it fell asleep on this step (sleeping bodies stop
emitting moves — use the flag for a final render sync).
-}
data BodyMove = BodyMove
  { bodyMoveBody :: !Entity
  , bodyMovePosition :: !WVec
  , bodyMoveAngle :: !Float
  , bodyMoveFellAsleep :: !Bool
  }
  deriving (Eq, Show)

{- | The bodies that moved during the last 'Apecs.Box2D.Space.stepPhysics', a read-only
global: @Moved moves <- get global@ after stepping. Iterating this
instead of every 'Apecs.Box2D.Body.Position' makes render sync O(moved) instead of
O(bodies): sleeping and static bodies don't appear. Box2D generates move
events unconditionally — there is no per-body opt-in flag, unlike
'Collisions'\/'Impacts'\/'SensorEvents', which need contact\/hit\/sensor
events enabled per shape. Events whose bodies were destroyed since the
step are dropped.
-}
newtype Moved = Moved [BodyMove]
  deriving (Show)

instance Component Moved where
  type Storage Moved = B2Space Moved

instance (MonadIO m, Has w m Physics) => Has w m Moved where
  getStore = cast <$> (getStore :: SystemT w m (B2Space Physics))

instance (MonadIO m) => ExplGet m (B2Space Moved) where
  explExists _ _ = pure True
  explGet sp _ = liftIO $ do
    evs <- B2Events.bodyMoveEvents (spWorld sp)
    fmap (Moved . catMaybes) . forM (VS.toList evs) $ \ev -> do
      met <- bodyEntity sp (B2T.bodyMoveEventBodyId ev)
      pure $ do
        ety <- met
        let Transform pos rot = B2T.bodyMoveEventTransform ev
        Just
          BodyMove
            { bodyMoveBody = ety
            , bodyMovePosition = pos
            , bodyMoveAngle = rotGetAngle rot
            , bodyMoveFellAsleep = toBool (B2T.bodyMoveEventFellAsleep ev)
            }
