{-# LANGUAGE TypeFamilies #-}

{-| The store shared by every component in "Apecs.Box3D" ('B3Space'),
the component values its registries embed ('Shape', 'Joint'), and the
resolution of engine objects back to their entities.
-}
module Apecs.Box3D.Types where

import Apecs
import Apecs.Core
import Control.Monad (filterM)
import Control.Monad.IO.Class (MonadIO)
import Data.IORef
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IM
import Data.Vector.Unboxed qualified as U

import Box3D.Body qualified as B3Body
import Box3D.Id (BodyId, JointId, ShapeId, WorldId)
import Box3D.Joint qualified as B3Joint
import Box3D.Shape qualified as B3Shape
import Box3D.Types qualified as B3T
import Box3D.UserData (getUserIndex)
import Box3D.World qualified as B3World

import Apecs.Box3D.Geometry

-- | Uninhabited component; add it to your world to get a physics space.
data Physics

-- | The engine shape plus the exact 'Shape' value that created it.
data ShapeRecord = ShapeRecord !ShapeId !Shape

-- | The engine joint plus the exact 'Joint' value that created it.
data JointRecord = JointRecord !JointId !Joint

{- | The store shared by 'Physics' and all its sub-components: the engine
world plus entity registries for bodies, shapes and joints.
-}
data B3Space c = B3Space
  { spWorld :: !WorldId
  , spBodyDef :: !B3T.BodyDef
  , spShapeDef :: !B3T.ShapeDef
  , spBodies :: !(IORef (IntMap BodyId))
  , spShapes :: !(IORef (IntMap ShapeRecord))
  , spJoints :: !(IORef (IntMap JointRecord))
  , spSubsteps :: !(IORef Int)
  }

cast :: B3Space a -> B3Space b
cast (B3Space w bd sd b s j i) = B3Space w bd sd b s j i

type instance Elem (B3Space c) = c

instance Component Physics where
  type Storage Physics = B3Space Physics

instance (MonadIO m) => ExplInit m (B3Space Physics) where
  explInit = liftIO $ do
    wd <- B3T.defaultWorldDef
    sd <- B3T.defaultShapeDef
    B3Space
      <$> B3World.create wd
      <*> B3T.defaultBodyDef
      -- Box3D defaults contact, hit and sensor event flags off; opt every
      -- layer-created shape in so 'Apecs.Box3D.Collision.Collisions', 'Apecs.Box3D.Collision.Impacts' and
      -- 'Apecs.Box3D.Collision.SensorEvents' have something to read (a shape both generates
      -- sensor events when it is itself a sensor and is visible to other
      -- sensors when it is a visitor).
      <*> pure
        sd
          { B3T.shapeDefEnableContactEvents = 1
          , B3T.shapeDefEnableHitEvents = 1
          , B3T.shapeDefEnableSensorEvents = 1
          }
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
    Nothing -> error ("Entity " <> show ety <> " has no Box3D " <> what)

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

withBody :: B3Space c -> Int -> (BodyId -> IO a) -> IO a
withBody sp = withReg "Body" (spBodies sp)

overBody :: B3Space c -> Int -> (BodyId -> IO ()) -> IO ()
overBody sp = overReg (spBodies sp)

bodyExists :: (MonadIO m) => B3Space c -> Int -> m Bool
bodyExists sp = regExists (spBodies sp)

bodyMembers :: (MonadIO m) => B3Space c -> m (U.Vector Int)
bodyMembers sp = regMembers (spBodies sp)

withShape :: B3Space c -> Int -> (ShapeId -> IO a) -> IO a
withShape sp ety f = withReg "Shape" (spShapes sp) ety (\(ShapeRecord s _) -> f s)

overShape :: B3Space c -> Int -> (ShapeId -> IO ()) -> IO ()
overShape sp ety f = overReg (spShapes sp) ety (\(ShapeRecord s _) -> f s)

shapeExists :: (MonadIO m) => B3Space c -> Int -> m Bool
shapeExists sp = regExists (spShapes sp)

shapeMembers :: (MonadIO m) => B3Space c -> m (U.Vector Int)
shapeMembers sp = regMembers (spShapes sp)

withJoint :: B3Space c -> Int -> (JointId -> IO a) -> IO a
withJoint sp ety f = withReg "Joint" (spJoints sp) ety (\(JointRecord j _) -> f j)

overJoint :: B3Space c -> Int -> (JointId -> IO ()) -> IO ()
overJoint sp ety f = overReg (spJoints sp) ety (\(JointRecord j _) -> f j)

jointExists :: (MonadIO m) => B3Space c -> Int -> m Bool
jointExists sp = regExists (spJoints sp)

jointMembers :: (MonadIO m) => B3Space c -> m (U.Vector Int)
jointMembers sp = regMembers (spJoints sp)

-- | Whether an entity has a 'Joint' whose engine type is one of the given kinds.
jointIsKind :: B3Space c -> Int -> [B3T.JointType] -> IO Bool
jointIsKind sp ety kinds = do
  m <- readIORef (spJoints sp)
  case IM.lookup ety m of
    Nothing -> pure False
    Just (JointRecord j _) -> (`elem` kinds) <$> B3Joint.getType j

{- | The entities whose 'Joint' engine type is one of the given kinds.
Kind-restricted components must keep their members consistent with
'jointIsKind' in @explExists@: @cmap@\/@cfold@ call @explGet@ on every
member without an existence check, and an unfiltered members list
would hand joints of the wrong kind to a type-specific engine getter.
-}
jointKindMembers :: (MonadIO m) => B3Space c -> [B3T.JointType] -> m (U.Vector Int)
jointKindMembers sp kinds = liftIO $ do
  m <- readIORef (spJoints sp)
  U.fromList . map fst
    <$> filterM (\(_, JointRecord j _) -> (`elem` kinds) <$> B3Joint.getType j) (IM.toList m)

{- | Gives an entity a collision shape attached to the 'Apecs.Box3D.Body.Body' of the given
entity (which may be the same entity). Carries the sub-components
'Apecs.Box3D.Shape.Density', 'Apecs.Box3D.Shape.Friction' and 'Apecs.Box3D.Shape.Elasticity'; re-setting the geometry
preserves them. Reads return the exact value written; geometry mutated
through the raw engine is not reflected.
-}
data Shape = Shape Entity Geometry
  deriving (Eq, Show)

{- | A joint between two bodies, specified in world space at creation
time. Joint frames are derived from the given world points with zero
reference rotation, except for the hinge, prismatic and wheel variants,
whose frames are additionally aligned to the given world axis or axes.
-}
data JointSpec
  = {- | A spherical (ball-socket) joint: the bodies pivot around a
    shared world point.
    -}
    PivotJoint WVec
  | -- | Keeps the two world anchor points at their current distance.
    DistanceJoint WVec WVec
  | -- | Rigidly welds the bodies together at a world point.
    WeldJoint WVec
  | {- | A revolute (hinge) joint: the bodies rotate relative to each
    other about a shared world point, constrained to a world-space
    axis.
    -}
    HingeJoint WVec WVec
  | {- | A hinge with an angular spring back to the creation orientation:
    stiffness in Hertz and a damping ratio.
    -}
    HingeSpringJoint WVec WVec Float Float
  | -- | A hinge with the relative angle limited to (lower, upper) radians.
    HingeLimitJoint WVec WVec Float Float
  | {- | A motorised hinge driving the relative angle at a speed (radians
    per second) with a maximum torque.
    -}
    HingeMotorJoint WVec WVec Float Float
  | {- | A prismatic (slider) joint: the bodies translate relative to
    each other along a world-space axis through the anchor, free
    between (lower, upper) meters.
    -}
    PrismaticJoint WVec WVec Float Float
  | {- | A prismatic (slider) joint with a damped spring back to the
    creation translation: stiffness in Hertz and a damping ratio.
    -}
    PrismaticSpringJoint WVec WVec Float Float
  | {- | A motorised prismatic (slider) joint driving the translation at
    a speed (meters per second) with a maximum force.
    -}
    PrismaticMotorJoint WVec WVec Float Float
  | {- | A wheel joint: entity A is the chassis and entity B the wheel.
    The wheel spins about the axle axis and the suspension lets it
    translate along the suspension axis through the anchor; the
    suspension spring is enabled with the given stiffness (Hertz) and
    damping ratio, and the spin motor and steering are left at engine
    defaults.
    -}
    WheelJoint WVec WVec WVec Float Float
  deriving (Eq, Show)

{- | Gives an entity a joint connecting the 'Apecs.Box3D.Body.Body's of the two given
entities, which must be distinct (the engine rejects self-joints;
setting one is a silent no-op). Reads return the exact value written.
The tuning sub-components ('Apecs.Box3D.Joint.MotorSpeed', 'Apecs.Box3D.Joint.JointLimits', ...) mutate
the live engine joint without touching the stored spec, so a re-set
'Joint' recreates the joint from the original spec and discards
tuning.
-}
data Joint = Joint Entity Entity JointSpec
  deriving (Eq, Show)

{- | The shape and body entities behind an engine shape, if it is still
alive and registered (event buffers can reference shapes destroyed
after the step).
-}
shapeEntities :: B3Space c -> ShapeId -> IO (Maybe (Entity, Entity))
shapeEntities sp s = do
  alive <- B3Shape.isValid s
  if not alive then
    pure Nothing
  else do
    ix <- getUserIndex s
    shapes <- readIORef (spShapes sp)
    pure $ case IM.lookup ix shapes of
      -- shapes created through the raw engine API have no user index and
      -- read back as 0, a legitimate entity; requiring the registered
      -- engine shape to be this very shape drops them instead
      Just (ShapeRecord s' (Shape bodyEty _)) | s' == s -> Just (Entity ix, bodyEty)
      _ -> Nothing

{- | The joint entity behind an engine joint, if it is still alive and
registered (event buffers can reference joints destroyed after the
step).
-}
jointEntity :: B3Space c -> JointId -> IO (Maybe Entity)
jointEntity sp j = do
  alive <- B3Joint.isValid j
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
bodyEntity :: B3Space c -> BodyId -> IO (Maybe Entity)
bodyEntity sp b = do
  alive <- B3Body.isValid b
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
