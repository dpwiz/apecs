{-# LANGUAGE TypeFamilies #-}

{-| The store shared by every component in "Apecs.Box2D" ('B2Space'),
the component values its registries embed ('Shape', 'Chain', 'Joint'),
and the resolution of engine objects back to their entities.
-}
module Apecs.Box2D.Types where

import Apecs
import Apecs.Core
import Control.Monad (filterM)
import Control.Monad.IO.Class (MonadIO)
import Data.IORef
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IM
import Data.IntSet qualified as IS
import Data.Vector.Storable qualified as VS
import Data.Vector.Unboxed qualified as U

import Box2D.Body qualified as B2Body
import Box2D.Id (BodyId, ChainId, JointId, ShapeId (..), WorldId)
import Box2D.Joint qualified as B2Joint
import Box2D.MathTypes (Vec2)
import Box2D.Shape qualified as B2Shape
import Box2D.Types qualified as B2T
import Box2D.UserData (getUserIndex)
import Box2D.World qualified as B2World

import Apecs.Box2D.Geometry

-- | Uninhabited component; add it to your world to get a physics space.
data Physics

-- | The engine shape plus the exact 'Shape' value that created it.
data ShapeRecord = ShapeRecord !ShapeId !Shape

-- | The engine joint plus the exact 'Joint' value that created it.
data JointRecord = JointRecord !JointId !Joint

{- | The engine chain, the packed ids of the segment shapes the engine
generated for it (the full 'ShapeId' words, so stale entries can never
match a live shape that reuses an index slot), and the exact 'Chain'
value that created it.
-}
data ChainRecord = ChainRecord !ChainId !IS.IntSet !Chain

{- | The store shared by 'Physics' and all its sub-components: the engine
world plus entity registries for bodies, shapes, joints and chains.
-}
data B2Space c = B2Space
  { spWorld :: !WorldId
  , spBodyDef :: !B2T.BodyDef
  , spShapeDef :: !B2T.ShapeDef
  , spBodies :: !(IORef (IntMap BodyId))
  , spShapes :: !(IORef (IntMap ShapeRecord))
  , spJoints :: !(IORef (IntMap JointRecord))
  , spChains :: !(IORef (IntMap ChainRecord))
  , spSubsteps :: !(IORef Int)
  }

cast :: B2Space a -> B2Space b
cast (B2Space w bd sd b s j c i) = B2Space w bd sd b s j c i

type instance Elem (B2Space c) = c

instance Component Physics where
  type Storage Physics = B2Space Physics

instance (MonadIO m) => ExplInit m (B2Space Physics) where
  explInit = liftIO $ do
    wd <- B2T.defaultWorldDef
    sd <- B2T.defaultShapeDef
    B2Space
      <$> B2World.create wd
      <*> B2T.defaultBodyDef
      -- Box2D defaults contact, hit and sensor event flags off; opt every
      -- layer-created shape in so 'Apecs.Box2D.Collision.Collisions', 'Apecs.Box2D.Collision.Impacts' and
      -- 'Apecs.Box2D.Collision.SensorEvents' have something to read (a shape both generates
      -- sensor events when it is itself a sensor and is visible to other
      -- sensors when it is a visitor).
      <*> pure
        sd
          { B2T.shapeDefEnableContactEvents = 1
          , B2T.shapeDefEnableHitEvents = 1
          , B2T.shapeDefEnableSensorEvents = 1
          }
      <*> newIORef mempty
      <*> newIORef mempty
      <*> newIORef mempty
      <*> newIORef mempty
      <*> newIORef 4

-- Registries ----------------------------------------------------------------

{- | Look up an entity's engine object. Only safe under the apecs 'ExplGet'
contract: the caller has checked existence.
-}
withReg :: String -> IORef (IntMap v) -> Int -> (v -> IO a) -> IO a
withReg what ref ety f = do
  m <- readIORef ref
  case IM.lookup ety m of
    Just v -> f v
    Nothing -> error ("Entity " <> show ety <> " has no Box2D " <> what)

{- | Run an action over an entity's engine object, or do nothing when the
entity has none — setter semantics, matching apecs-physics.
-}
overReg :: IORef (IntMap v) -> Int -> (v -> IO ()) -> IO ()
overReg ref ety f = readIORef ref >>= mapM_ f . IM.lookup ety

regExists :: (MonadIO m) => IORef (IntMap v) -> Int -> m Bool
regExists ref ety = liftIO $ IM.member ety <$> readIORef ref

regMembers :: (MonadIO m) => IORef (IntMap v) -> m (U.Vector Int)
regMembers ref = liftIO $ do
  m <- readIORef ref
  pure (U.fromListN (IM.size m) (IM.keys m))

withBody :: B2Space c -> Int -> (BodyId -> IO a) -> IO a
withBody sp = withReg "Body" (spBodies sp)

overBody :: B2Space c -> Int -> (BodyId -> IO ()) -> IO ()
overBody sp = overReg (spBodies sp)

bodyExists :: (MonadIO m) => B2Space c -> Int -> m Bool
bodyExists sp = regExists (spBodies sp)

bodyMembers :: (MonadIO m) => B2Space c -> m (U.Vector Int)
bodyMembers sp = regMembers (spBodies sp)

withShape :: B2Space c -> Int -> (ShapeId -> IO a) -> IO a
withShape sp ety f = withReg "Shape" (spShapes sp) ety (\(ShapeRecord s _) -> f s)

overShape :: B2Space c -> Int -> (ShapeId -> IO ()) -> IO ()
overShape sp ety f = overReg (spShapes sp) ety (\(ShapeRecord s _) -> f s)

shapeExists :: (MonadIO m) => B2Space c -> Int -> m Bool
shapeExists sp = regExists (spShapes sp)

shapeMembers :: (MonadIO m) => B2Space c -> m (U.Vector Int)
shapeMembers sp = regMembers (spShapes sp)

withJoint :: B2Space c -> Int -> (JointId -> IO a) -> IO a
withJoint sp ety f = withReg "Joint" (spJoints sp) ety (\(JointRecord j _) -> f j)

overJoint :: B2Space c -> Int -> (JointId -> IO ()) -> IO ()
overJoint sp ety f = overReg (spJoints sp) ety (\(JointRecord j _) -> f j)

jointExists :: (MonadIO m) => B2Space c -> Int -> m Bool
jointExists sp = regExists (spJoints sp)

jointMembers :: (MonadIO m) => B2Space c -> m (U.Vector Int)
jointMembers sp = regMembers (spJoints sp)

withChain :: B2Space c -> Int -> (ChainId -> IO a) -> IO a
withChain sp ety f = withReg "Chain" (spChains sp) ety (\(ChainRecord c _ _) -> f c)

chainExists :: (MonadIO m) => B2Space c -> Int -> m Bool
chainExists sp = regExists (spChains sp)

chainMembers :: (MonadIO m) => B2Space c -> m (U.Vector Int)
chainMembers sp = regMembers (spChains sp)

-- | Whether an entity has a 'Joint' whose engine type is one of the given kinds.
jointIsKind :: B2Space c -> Int -> [B2T.JointType] -> IO Bool
jointIsKind sp ety kinds = do
  m <- readIORef (spJoints sp)
  case IM.lookup ety m of
    Nothing -> pure False
    Just (JointRecord j _) -> (`elem` kinds) <$> B2Joint.getType j

{- | The entities whose 'Joint' engine type is one of the given kinds.
Kind-restricted components must keep their members consistent with
'jointIsKind' in @explExists@: @cmap@\/@cfold@ call @explGet@ on every
member without an existence check, and an unfiltered members list
would hand joints of the wrong kind to a type-specific engine getter.
-}
jointKindMembers :: (MonadIO m) => B2Space c -> [B2T.JointType] -> m (U.Vector Int)
jointKindMembers sp kinds = liftIO $ do
  m <- readIORef (spJoints sp)
  U.fromList . map fst
    <$> filterM (\(_, JointRecord j _) -> (`elem` kinds) <$> B2Joint.getType j) (IM.toList m)

{- | Gives an entity a collision shape attached to the 'Apecs.Box2D.Body.Body' of the given
entity (which may be the same entity). Carries the sub-components
'Apecs.Box2D.Shape.Density', 'Apecs.Box2D.Shape.Friction' and 'Apecs.Box2D.Shape.Elasticity'; re-setting the geometry
preserves them. Reads return the exact value written; geometry mutated
through the raw engine is not reflected.
-}
data Shape = Shape Entity Geometry
  deriving (Eq, Show)

{- | Gives an entity a chain of connected line segments attached to the
'Apecs.Box2D.Body.Body' of the given entity — smooth static terrain outlines without the
ghost collisions of separate 'GeoSegment's. Points are in body-local
coordinates. Collision is one-sided: the solid face is to the right when
facing from one point towards the next, so for a loop a
counter-clockwise winding faces outward and a clockwise winding faces
inward (the same convention 'GeoPolygon' uses for its CCW hull). When the
loop flag is set the chain closes by connecting the last point back to
the first; either way at least 4 points are required, and setting fewer
raises an error in the style of 'GeoPolygon'. Chains are meant for
static bodies. Re-setting recreates the engine chain; reads return the
exact value written. Setting it on an entity whose body entity has no
'Apecs.Box2D.Body.Body' is a silent no-op.

The segments the engine creates for a chain are its own internal
@b2ChainSegment@ shapes; there is no matching 'Shape' component for
them, so they are not entered into this layer's shape registry. Instead,
each segment is stamped with the chain entity's user index directly (see
'Box2D.UserData.setUserIndex'), and the chain's contact and hit events
are switched on for every segment at creation — mirroring what 'explInit'
does per-shape for layer-created 'Shape's. Event and query resolution
falls back to a chain lookup keyed by that index when a shape isn't found
in the shape registry, so chain segments now surface in 'Apecs.Box2D.Collision.Collisions',
'Apecs.Box2D.Collision.CollisionsEnd' and 'Apecs.Box2D.Collision.Impacts', and are visible to the queries
('Apecs.Box2D.Query.segmentQuery', 'Apecs.Box2D.Query.segmentQueryAll', 'Apecs.Box2D.Query.aabbQuery', 'Apecs.Box2D.Query.pointQuery',
'Apecs.Box2D.Query.overlapQuery', 'Apecs.Box2D.Query.sweepQuery') instead of being dropped
('Apecs.Box2D.Query.containsPointQuery' is the exception: its exact refinement,
@b2Shape_TestPoint@, reports no containment for chain segments, same as
for plain 'GeoSegment' shapes, so chains never pass it)
— in particular 'Apecs.Box2D.Query.segmentQuery' no longer returns 'Nothing' just
because a chain segment is the closest hit. In every case the CHAIN
entity is reported in the shape slot, not a per-segment entity: a reader
following 'Apecs.Box2D.Collision.collisionShapeA' (or 'Apecs.Box2D.Query.rayHitShape', etc.) to a 'Shape'
component won't find one, but will find a 'Chain'. Chain creation also
turns on @chainDefEnableSensorEvents@, the chain-level counterpart of
'Shape'\'s 'Apecs.Box2D.Shape.Sensor' visitor opt-in, so chains are visible to sensors the
same way ordinary shapes are.
-}
data Chain = Chain Entity (VS.Vector Vec2) Bool
  deriving (Eq, Show)

{- | A joint between two bodies, specified in world space at creation
time. Joint frames are derived from the given world points with zero
reference rotation, except for the prismatic and wheel variants, whose
frames are additionally aligned to the given world axis.
-}
data JointSpec
  = -- | A revolute joint: the bodies rotate around a shared world point.
    PivotJoint WVec
  | -- | Keeps the two world anchor points at their current distance.
    DistanceJoint WVec WVec
  | -- | Rigidly welds the bodies together at a world point.
    WeldJoint WVec
  | {- | A damped spring between two world anchors, resting at their
    current distance: stiffness in Hertz and a damping ratio.
    -}
    SpringJoint WVec WVec Float Float
  | -- | The anchor distance moves freely between a minimum and maximum.
    SlideJoint WVec WVec Float Float
  | {- | A pivot with an angular spring back to the creation orientation:
    stiffness in Hertz and a damping ratio.
    -}
    RotarySpringJoint WVec Float Float
  | -- | A pivot with the relative angle limited to (lower, upper) radians.
    RotaryLimitJoint WVec Float Float
  | {- | A motorised pivot driving the relative angle at a speed (radians
    per second) with a maximum torque.
    -}
    RotaryMotorJoint WVec Float Float
  | {- | A prismatic joint: the bodies slide relative to each other along
    a world-space axis through the anchor, free between (lower, upper)
    meters from the anchor, with no relative rotation.
    -}
    PrismaticJoint WVec WVec Float Float
  | {- | A prismatic joint with a damped spring back to the creation
    translation: stiffness in Hertz and a damping ratio.
    -}
    PrismaticSpringJoint WVec WVec Float Float
  | {- | A motorised prismatic joint driving the translation at a speed
    (meters per second) with a maximum force.
    -}
    PrismaticMotorJoint WVec WVec Float Float
  | {- | A wheel joint: entity A is the chassis and entity B the wheel,
    which spins freely and rides the suspension spring along the axis
    through the anchor, at the given stiffness (Hertz) and damping
    ratio.
    -}
    WheelJoint WVec WVec Float Float
  | {- | Drives the relative velocity between the bodies at the anchor:
    desired linear velocity and its maximum force, then desired angular
    velocity and its maximum torque. With zero velocities it acts as
    top-down friction, damping relative motion without pinning the
    bodies to the anchor.
    -}
    MotorJoint WVec WVec Float Float Float
  deriving (Eq, Show)

{- | Gives an entity a joint connecting the 'Apecs.Box2D.Body.Body's of the two given
entities, which must be distinct (the engine rejects self-joints;
setting one is a silent no-op). Reads return the exact value written.
The tuning sub-components ('Apecs.Box2D.Joint.MotorSpeed', 'Apecs.Box2D.Joint.JointLimits', ...) mutate
the live engine joint without touching the stored spec, so a re-set
'Joint' recreates the joint from the original spec and discards
tuning.
-}
data Joint = Joint Entity Entity JointSpec
  deriving (Eq, Show)

{- | The shape and body entities behind an engine shape, if it is still
alive and registered (event buffers can reference shapes destroyed
after the step). The shape registry is the fast path; a shape not found
there (in particular, a chain's internal @b2ChainSegment@, which is
never entered into it) falls back to a lookup in the chain registry,
resolving to the CHAIN entity as the "shape" and its body entity — see
the 'Chain' haddock.
-}
shapeEntities :: B2Space c -> ShapeId -> IO (Maybe (Entity, Entity))
shapeEntities sp s@(ShapeId w) = do
  alive <- B2Shape.isValid s
  if not alive then
    pure Nothing
  else do
    ix <- getUserIndex s
    shapes <- readIORef (spShapes sp)
    case IM.lookup ix shapes of
      -- shapes created through the raw engine API have no user index and
      -- read back as 0, a legitimate entity; requiring the registered
      -- engine shape to be this very shape drops them instead
      Just (ShapeRecord s' (Shape bodyEty _)) | s' == s -> pure (Just (Entity ix, bodyEty))
      _ -> do
        chains <- readIORef (spChains sp)
        pure $ case IM.lookup ix chains of
          -- same raw-API index-0 guard as above, checked against the
          -- chain's own recorded segment ids instead of a single shape id
          Just (ChainRecord _ segSet (Chain bodyEty _ _))
            | IS.member (fromIntegral w) segSet -> Just (Entity ix, bodyEty)
          _ -> Nothing

{- | The joint entity behind an engine joint, if it is still alive and
registered (event buffers can reference joints destroyed after the
step).
-}
jointEntity :: B2Space c -> JointId -> IO (Maybe Entity)
jointEntity sp j = do
  alive <- B2Joint.isValid j
  if not alive then
    pure Nothing
  else do
    ix <- getUserIndex j
    joints <- readIORef (spJoints sp)
    pure $ case IM.lookup ix joints of
      -- joints created through the raw engine API have no user index and
      -- read back as 0, a legitimate entity; requiring the registered
      -- engine joint to be this very joint drops them instead
      Just (JointRecord j' _) | j' == j -> Just (Entity ix)
      _ -> Nothing

{- | The entity behind an engine body id, if it is still alive and
registered (event buffers can reference bodies destroyed after the
step).
-}
bodyEntity :: B2Space c -> BodyId -> IO (Maybe Entity)
bodyEntity sp b = do
  alive <- B2Body.isValid b
  if not alive then
    pure Nothing
  else do
    ix <- getUserIndex b
    bodies <- readIORef (spBodies sp)
    pure $ case IM.lookup ix bodies of
      -- bodies created through the raw engine API have no user index and
      -- read back as 0, a legitimate entity; requiring the registered
      -- engine body to be this very body drops them instead
      Just b' | b' == b -> Just (Entity ix)
      _ -> Nothing
