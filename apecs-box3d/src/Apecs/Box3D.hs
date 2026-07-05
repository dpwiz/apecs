{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-| Apecs glue for the Box3D physics engine, the 3D sibling of
apecs-box2d (both modelled on apecs-physics).

Add 'Physics' to your world to get a Box3D world. Giving an entity a
'Body' component creates an engine body and unlocks its sub-components
('Position', 'Velocity', 'Rotation', ...), which read and write the
engine directly instead of mirroring state into apecs stores. Shapes
hang off a body entity through the 'Shape' component. Setting a
sub-component on an entity that has no 'Body' (or 'Shape') is a silent
no-op.

Vectors are Box3D's native single-precision 'Vec3' and rotations are
quaternions ('Quat'); convert at the boundary. 'Elasticity' is Box3D
restitution.

The raw engine is reachable through 'B3BodyId', 'B3ShapeId' and
'getWorldId' together with the "Box3D" modules.
-}
module Apecs.Box3D
  ( -- * World
    Physics
  , B3Space
  , Gravity (..)
  , earthGravity
  , Substeps (..)
  , SleepingEnabled (..)
  , ContinuousEnabled (..)
  , HitEventThreshold (..)
  , RestitutionThreshold (..)
  , MaximumLinearSpeed (..)
  , stepPhysics
  , destroyPhysics
  , explode
  , getWorldId

    -- * Body
  , Body (..)
  , Position (..)
  , Velocity (..)
  , Rotation (..)
  , AngularVelocity (..)
  , BodyMass (..)
  , Force (..)
  , Torque (..)
  , LinearImpulse (..)
  , AngularImpulse (..)
  , ForceAt (..)
  , ImpulseAt (..)
  , TargetTransform (..)
  , LinearDamping (..)
  , AngularDamping (..)
  , GravityScale (..)
  , BulletBody (..)
  , BodyEnabled (..)
  , Awake (..)
  , MotionLocks (..)
  , SleepEnabled (..)
  , SleepThreshold (..)
  , B3BodyId (..)

    -- * Shape
  , Geometry (..)
  , Mesh
  , HeightField
  , Hull
  , Shape (..)
  , Density (..)
  , Friction (..)
  , Elasticity (..)
  , CollisionFilter (..)
  , Sensor (..)
  , Filter (..)
  , B3ShapeId (..)

    -- * Static geometry
  , boxMesh
  , hollowBoxMesh
  , platformMesh
  , gridMesh
  , torusMesh
  , waveMesh
  , gridHeightField
  , waveHeightField
  , rockHull
  , coneHull
  , cylinderHull

    -- * Joint
  , JointSpec (..)
  , Joint (..)
  , MotorSpeed (..)
  , MotorMaxTorque (..)
  , MotorMaxForce (..)
  , JointLimits (..)
  , CollideConnected (..)
  , JointForce (..)
  , JointTorque (..)
  , JointForceThreshold (..)
  , JointTorqueThreshold (..)
  , B3JointId (..)

    -- * Queries
  , RayHit (..)
  , segmentQuery
  , segmentQueryAll
  , aabbQuery
  , pointQuery
  , containsPointQuery

    -- * Character mover
  , MoverResult (..)
  , moveCharacter

    -- * Collisions
  , Collision (..)
  , Collisions (..)
  , CollisionsEnd (..)
  , Impact (..)
  , Impacts (..)
  , SensorEvent (..)
  , SensorEvents (..)
  , JointEvents (..)

    -- * Vectors
  , Vec3 (..)
  , vec3Zero
  , Quat (..)
  , quatIdentity
  , BVec
  , WVec
  ) where

import Apecs
import Apecs.Core
import Control.Monad (forM, forM_, when)
import Control.Monad.IO.Class (MonadIO)
import Data.IORef
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IM
import Data.IntSet qualified as IS
import Data.List (sortOn)
import Data.Maybe (catMaybes)
import Data.Vector.Storable qualified as VS
import Data.Vector.Unboxed qualified as U
import Foreign.Concurrent qualified as Concurrent
import Foreign.ForeignPtr (ForeignPtr, withForeignPtr)
import Foreign.Marshal.Utils (fromBool, toBool)
import Foreign.Ptr (Ptr, nullFunPtr, nullPtr)

import Box3D.Body qualified as B3Body
import Box3D.BoxMesh qualified as B3BoxMesh
import Box3D.Callbacks (withCastResultFcn, withOverlapResultFcn, withPlaneResultFcn)
import Box3D.Cone qualified as B3Cone
import Box3D.Cylinder qualified as B3Cylinder
import Box3D.DistanceJoint qualified as B3DistanceJoint
import Box3D.Events qualified as B3Events
import Box3D.Grid qualified as B3Grid
import Box3D.GridMesh qualified as B3GridMesh
import Box3D.HeightField qualified as B3HeightField
import Box3D.HollowBoxMesh qualified as B3HollowBoxMesh
import Box3D.Hull qualified as B3Hull
import Box3D.Id (BodyId, JointId, ShapeId, WorldId)
import Box3D.Joint qualified as B3Joint
import Box3D.MathFunctions (computeQuatBetweenUnitVectors)
import Box3D.MathTypes (AABB (..), Plane (..), Quat (..), Transform (..), Vec3 (..), quatIdentity, vec3Zero)
import Box3D.Mesh qualified as B3Mesh
import Box3D.PlatformMesh qualified as B3PlatformMesh
import Box3D.PrismaticJoint qualified as B3PrismaticJoint
import Box3D.RevoluteJoint qualified as B3RevoluteJoint
import Box3D.Rock qualified as B3Rock
import Box3D.Shape qualified as B3Shape
import Box3D.SphericalJoint qualified as B3SphericalJoint
import Box3D.Tags (HeightFieldData, HullData, MeshData)
import Box3D.TorusMesh qualified as B3TorusMesh
import Box3D.Types (Filter (..))
import Box3D.Types qualified as B3T
import Box3D.UserData (getUserIndex, setUserIndex)
import Box3D.Wave qualified as B3Wave
import Box3D.WaveMesh qualified as B3WaveMesh
import Box3D.WeldJoint qualified as B3WeldJoint
import Box3D.WheelJoint qualified as B3WheelJoint
import Box3D.World qualified as B3World

-- | A vector in body-space coordinates.
type BVec = Vec3

-- | A vector in world-space coordinates.
type WVec = Vec3

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
      -- layer-created shape in so 'Collisions', 'Impacts' and
      -- 'SensorEvents' have something to read (a shape both generates
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

-- | The raw Box3D world, for use with the "Box3D" modules directly.
getWorldId :: forall w m. (MonadIO m, Has w m Physics) => SystemT w m WorldId
getWorldId = spWorld <$> (getStore :: SystemT w m (B3Space Physics))

{- | Advance the simulation by a time delta, resolving contacts with the
'Substeps' number of substeps.
-}
stepPhysics :: forall w m. (MonadIO m, Has w m Physics) => Float -> SystemT w m ()
stepPhysics dT = do
  sp :: B3Space Physics <- getStore
  liftIO $ do
    substeps <- readIORef (spSubsteps sp)
    B3World.step (spWorld sp) dT substeps

{- | Destroy the engine world along with all its bodies and shapes, and
clear the registries. The store is unusable afterwards; call this on
teardown. Box3D keeps worlds in a fixed-size global registry, so
sessions that repeatedly create worlds (test suites, GHCi reloads) must
destroy them too or world creation eventually fails.
-}
destroyPhysics :: forall w m. (MonadIO m, Has w m Physics) => SystemT w m ()
destroyPhysics = do
  sp :: B3Space Physics <- getStore
  liftIO $ do
    B3World.destroy (spWorld sp)
    writeIORef (spBodies sp) mempty
    writeIORef (spShapes sp) mempty
    writeIORef (spJoints sp) mempty

{- | Apply a radial impulse to every dynamic body within a radius of a
world point, as if from an explosion: each affected shape is pushed
away from the center along the line to its nearest surface point,
scaled by how much of its area faces the blast. Only spheres, capsules
and hulls receive an impulse; a body is woken even if it was asleep.
The impulse has no soft falloff by default, so it cuts off sharply at
the radius, and every shape passes the default filter (nothing is
masked out). A negative impulse pulls bodies inward instead of pushing
them.
-}
explode
  :: forall w m
   . (MonadIO m, Has w m Physics)
  => WVec
  -- ^ Explosion center, in world coordinates.
  -> Float
  -- ^ Radius: shapes within this distance get the full impulse.
  -> Float
  {- ^ Impulse per unit area of shape surface facing the blast;
  negative for an implosion.
  -}
  -> SystemT w m ()
explode center radius impulse = do
  sp :: B3Space Physics <- getStore
  liftIO $ do
    def <- B3T.defaultExplosionDef
    B3World.explode
      (spWorld sp)
      def
        { B3T.explosionDefPosition = center
        , B3T.explosionDefRadius = radius
        , B3T.explosionDefImpulsePerArea = impulse
        }

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

-- Space sub-components ----------------------------------------------------

-- | The world's gravity vector.
newtype Gravity = Gravity WVec
  deriving (Eq, Show)

earthGravity :: Gravity
earthGravity = Gravity (Vec3 0 (-9.81) 0)

instance Component Gravity where
  type Storage Gravity = B3Space Gravity

instance (MonadIO m, Has w m Physics) => Has w m Gravity where
  getStore = cast <$> (getStore :: SystemT w m (B3Space Physics))

instance (MonadIO m) => ExplGet m (B3Space Gravity) where
  explExists _ _ = pure True
  explGet sp _ = liftIO $ Gravity <$> B3World.getGravity (spWorld sp)

instance (MonadIO m) => ExplSet m (B3Space Gravity) where
  explSet sp _ (Gravity v) = liftIO $ B3World.setGravity (spWorld sp) v

{- | The number of contact substeps per 'stepPhysics' call. Defaults to 4;
clamped to at least 1.
-}
newtype Substeps = Substeps Int
  deriving (Eq, Show)

instance Component Substeps where
  type Storage Substeps = B3Space Substeps

instance (MonadIO m, Has w m Physics) => Has w m Substeps where
  getStore = cast <$> (getStore :: SystemT w m (B3Space Physics))

instance (MonadIO m) => ExplGet m (B3Space Substeps) where
  explExists _ _ = pure True
  explGet sp _ = liftIO $ Substeps <$> readIORef (spSubsteps sp)

instance (MonadIO m) => ExplSet m (B3Space Substeps) where
  explSet sp _ (Substeps n) = liftIO $ writeIORef (spSubsteps sp) (max 1 n)

{- | Whether bodies in this world may fall asleep at all (on by
default). Disabling it wakes everything and saves the bookkeeping when
nothing would sleep anyway; sleeping gains performance on large scenes
where most bodies are at rest. Per-body control is 'SleepEnabled'.
-}
newtype SleepingEnabled = SleepingEnabled Bool
  deriving (Eq, Show)

instance Component SleepingEnabled where
  type Storage SleepingEnabled = B3Space SleepingEnabled

instance (MonadIO m, Has w m Physics) => Has w m SleepingEnabled where
  getStore = cast <$> (getStore :: SystemT w m (B3Space Physics))

instance (MonadIO m) => ExplGet m (B3Space SleepingEnabled) where
  explExists _ _ = pure True
  explGet sp _ = liftIO $ SleepingEnabled <$> B3World.isSleepingEnabled (spWorld sp)

instance (MonadIO m) => ExplSet m (B3Space SleepingEnabled) where
  explSet sp _ (SleepingEnabled e) = liftIO $ B3World.enableSleeping (spWorld sp) e

{- | Whether continuous collision detection runs between fast dynamic
bodies and static geometry, keeping them from tunnelling through walls
between substeps (on by default; disabling it is a minor performance
gain). Continuous detection between two dynamic bodies is a separate,
per-body opt-in: see 'BulletBody'.
-}
newtype ContinuousEnabled = ContinuousEnabled Bool
  deriving (Eq, Show)

instance Component ContinuousEnabled where
  type Storage ContinuousEnabled = B3Space ContinuousEnabled

instance (MonadIO m, Has w m Physics) => Has w m ContinuousEnabled where
  getStore = cast <$> (getStore :: SystemT w m (B3Space Physics))

instance (MonadIO m) => ExplGet m (B3Space ContinuousEnabled) where
  explExists _ _ = pure True
  explGet sp _ = liftIO $ ContinuousEnabled <$> B3World.isContinuousEnabled (spWorld sp)

instance (MonadIO m) => ExplSet m (B3Space ContinuousEnabled) where
  explSet sp _ (ContinuousEnabled e) = liftIO $ B3World.enableContinuous (spWorld sp) e

{- | The approach speed above which a contact generates a hit event,
usually in meters per second (engine default 1). Read by 'Impacts',
which also needs hit events enabled per shape — on by default for every
shape this layer creates.
-}
newtype HitEventThreshold = HitEventThreshold Float
  deriving (Eq, Show)

instance Component HitEventThreshold where
  type Storage HitEventThreshold = B3Space HitEventThreshold

instance (MonadIO m, Has w m Physics) => Has w m HitEventThreshold where
  getStore = cast <$> (getStore :: SystemT w m (B3Space Physics))

instance (MonadIO m) => ExplGet m (B3Space HitEventThreshold) where
  explExists _ _ = pure True
  explGet sp _ = liftIO $ HitEventThreshold <$> B3World.getHitEventThreshold (spWorld sp)

instance (MonadIO m) => ExplSet m (B3Space HitEventThreshold) where
  explSet sp _ (HitEventThreshold t) = liftIO $ B3World.setHitEventThreshold (spWorld sp) t

{- | The relative approach speed below which a contact's 'Elasticity'
is ignored and it doesn't bounce, usually in meters per second. Don't
set this very low: contacts hovering just above the threshold keep
bouncing instead of settling, which prevents bodies from falling
asleep.
-}
newtype RestitutionThreshold = RestitutionThreshold Float
  deriving (Eq, Show)

instance Component RestitutionThreshold where
  type Storage RestitutionThreshold = B3Space RestitutionThreshold

instance (MonadIO m, Has w m Physics) => Has w m RestitutionThreshold where
  getStore = cast <$> (getStore :: SystemT w m (B3Space Physics))

instance (MonadIO m) => ExplGet m (B3Space RestitutionThreshold) where
  explExists _ _ = pure True
  explGet sp _ = liftIO $ RestitutionThreshold <$> B3World.getRestitutionThreshold (spWorld sp)

instance (MonadIO m) => ExplSet m (B3Space RestitutionThreshold) where
  explSet sp _ (RestitutionThreshold t) = liftIO $ B3World.setRestitutionThreshold (spWorld sp) t

{- | The speed cap applied to every 'Body' in this world, usually in
meters per second: velocities that would exceed it are clamped each
step. Guards against tunnelling and blow-ups from stray forces or
impulses.
-}
newtype MaximumLinearSpeed = MaximumLinearSpeed Float
  deriving (Eq, Show)

instance Component MaximumLinearSpeed where
  type Storage MaximumLinearSpeed = B3Space MaximumLinearSpeed

instance (MonadIO m, Has w m Physics) => Has w m MaximumLinearSpeed where
  getStore = cast <$> (getStore :: SystemT w m (B3Space Physics))

instance (MonadIO m) => ExplGet m (B3Space MaximumLinearSpeed) where
  explExists _ _ = pure True
  explGet sp _ = liftIO $ MaximumLinearSpeed <$> B3World.getMaximumLinearSpeed (spWorld sp)

instance (MonadIO m) => ExplSet m (B3Space MaximumLinearSpeed) where
  explSet sp _ (MaximumLinearSpeed s) = liftIO $ B3World.setMaximumLinearSpeed (spWorld sp) s

-- Body --------------------------------------------------------------------

{- | Gives an entity a Box3D body. Deleting it also deletes the shapes
attached to it. A body carries the sub-components 'Position',
'Velocity', 'Rotation', 'AngularVelocity', 'BodyMass', 'Force' and
'Torque'; they exist as long as the entity has a @Body@, and setting
them on an entity without one does nothing.
-}
data Body = DynamicBody | KinematicBody | StaticBody
  deriving (Eq, Ord, Enum, Show)

toB3BodyType :: Body -> B3T.BodyType
toB3BodyType DynamicBody = B3T.DynamicBody
toB3BodyType KinematicBody = B3T.KinematicBody
toB3BodyType StaticBody = B3T.StaticBody

fromB3BodyType :: B3T.BodyType -> Body
fromB3BodyType ty = case ty of
  B3T.DynamicBody -> DynamicBody
  B3T.KinematicBody -> KinematicBody
  B3T.StaticBody -> StaticBody

instance Component Body where
  type Storage Body = B3Space Body

instance (MonadIO m, Has w m Physics) => Has w m Body where
  getStore = cast <$> (getStore :: SystemT w m (B3Space Physics))

instance (MonadIO m) => ExplSet m (B3Space Body) where
  explSet sp ety btype = liftIO $ do
    bodies <- readIORef (spBodies sp)
    case IM.lookup ety bodies of
      Just b -> B3Body.setType b (toB3BodyType btype)
      Nothing -> do
        b <- B3Body.create (spWorld sp) (spBodyDef sp){B3T.bodyDefType = toB3BodyType btype}
        setUserIndex b ety
        modifyIORef' (spBodies sp) (IM.insert ety b)

instance (MonadIO m) => ExplGet m (B3Space Body) where
  explExists = bodyExists
  explGet sp ety =
    liftIO $
      withBody sp ety $
        fmap fromB3BodyType . B3Body.getType

instance (MonadIO m) => ExplDestroy m (B3Space Body) where
  explDestroy sp ety = liftIO $ do
    bodies <- readIORef (spBodies sp)
    forM_ (IM.lookup ety bodies) $ \b -> do
      -- the engine destroys attached shapes and joints along with the
      -- body, so drop their entity records too
      modifyIORef' (spShapes sp) (IM.filter (\(ShapeRecord _ (Shape (Entity be) _)) -> be /= ety))
      modifyIORef' (spJoints sp) (IM.filter (\(JointRecord _ (Joint (Entity a) (Entity b') _)) -> a /= ety && b' /= ety))
      modifyIORef' (spBodies sp) (IM.delete ety)
      B3Body.destroy b

instance (MonadIO m) => ExplMembers m (B3Space Body) where
  explMembers = bodyMembers

-- | The raw Box3D body of an entity, for use with "Box3D.Body" directly.
newtype B3BodyId = B3BodyId BodyId
  deriving (Eq, Show)

instance Component B3BodyId where
  type Storage B3BodyId = B3Space B3BodyId

instance (MonadIO m, Has w m Physics) => Has w m B3BodyId where
  getStore = cast <$> (getStore :: SystemT w m (B3Space Physics))

instance (MonadIO m) => ExplGet m (B3Space B3BodyId) where
  explExists = bodyExists
  explGet sp ety = liftIO $ withBody sp ety (pure . B3BodyId)

instance (MonadIO m) => ExplMembers m (B3Space B3BodyId) where
  explMembers = bodyMembers

-- Body sub-components ------------------------------------------------------

-- | Where a 'Body' is, in world coordinates.
newtype Position = Position WVec
  deriving (Eq, Show)

instance Component Position where
  type Storage Position = B3Space Position

instance (MonadIO m, Has w m Physics) => Has w m Position where
  getStore = cast <$> (getStore :: SystemT w m (B3Space Physics))

instance (MonadIO m) => ExplGet m (B3Space Position) where
  explExists = bodyExists
  explGet sp ety = liftIO $ withBody sp ety $ fmap Position . B3Body.getPosition

instance (MonadIO m) => ExplSet m (B3Space Position) where
  explSet sp ety (Position p) = liftIO $
    overBody sp ety $ \b -> do
      rot <- B3Body.getRotation b
      B3Body.setTransform b p rot

instance (MonadIO m) => ExplMembers m (B3Space Position) where
  explMembers = bodyMembers

-- | Where a 'Body' is going, in world coordinates.
newtype Velocity = Velocity WVec
  deriving (Eq, Show)

instance Component Velocity where
  type Storage Velocity = B3Space Velocity

instance (MonadIO m, Has w m Physics) => Has w m Velocity where
  getStore = cast <$> (getStore :: SystemT w m (B3Space Physics))

instance (MonadIO m) => ExplGet m (B3Space Velocity) where
  explExists = bodyExists
  explGet sp ety = liftIO $ withBody sp ety $ fmap Velocity . B3Body.getLinearVelocity

instance (MonadIO m) => ExplSet m (B3Space Velocity) where
  explSet sp ety (Velocity v) = liftIO $
    overBody sp ety $ \b ->
      B3Body.setLinearVelocity b v

instance (MonadIO m) => ExplMembers m (B3Space Velocity) where
  explMembers = bodyMembers

{- | A 'Body'\'s orientation quaternion. Setting it normalizes the
quaternion on the way in (the engine requires unit rotations); setting
a zero quaternion is a no-op.
-}
newtype Rotation = Rotation Quat
  deriving (Eq, Show)

-- | 'Nothing' for a zero (or NaN) quaternion, which has no direction.
normalizeQuat :: Quat -> Maybe Quat
normalizeQuat (Quat (Vec3 x y z) w)
  | m2 > 0 = Just (Quat (Vec3 (x / m) (y / m) (z / m)) (w / m))
  | otherwise = Nothing
  where
    m2 = x * x + y * y + z * z + w * w
    m = sqrt m2

instance Component Rotation where
  type Storage Rotation = B3Space Rotation

instance (MonadIO m, Has w m Physics) => Has w m Rotation where
  getStore = cast <$> (getStore :: SystemT w m (B3Space Physics))

instance (MonadIO m) => ExplGet m (B3Space Rotation) where
  explExists = bodyExists
  explGet sp ety = liftIO $ withBody sp ety $ fmap Rotation . B3Body.getRotation

instance (MonadIO m) => ExplSet m (B3Space Rotation) where
  explSet sp ety (Rotation q) = liftIO $
    overBody sp ety $ \b ->
      -- the engine asserts unit rotations; normalize so hand-built or
      -- interpolated quaternions are safe to set
      forM_ (normalizeQuat q) $ \q' -> do
        pos <- B3Body.getPosition b
        B3Body.setTransform b pos q'

instance (MonadIO m) => ExplMembers m (B3Space Rotation) where
  explMembers = bodyMembers

-- | A 'Body'\'s angular velocity, in radians per second about each axis.
newtype AngularVelocity = AngularVelocity Vec3
  deriving (Eq, Show)

instance Component AngularVelocity where
  type Storage AngularVelocity = B3Space AngularVelocity

instance (MonadIO m, Has w m Physics) => Has w m AngularVelocity where
  getStore = cast <$> (getStore :: SystemT w m (B3Space Physics))

instance (MonadIO m) => ExplGet m (B3Space AngularVelocity) where
  explExists = bodyExists
  explGet sp ety =
    liftIO $
      withBody sp ety $
        fmap AngularVelocity . B3Body.getAngularVelocity

instance (MonadIO m) => ExplSet m (B3Space AngularVelocity) where
  explSet sp ety (AngularVelocity omega) = liftIO $
    overBody sp ety $ \b ->
      B3Body.setAngularVelocity b omega

instance (MonadIO m) => ExplMembers m (B3Space AngularVelocity) where
  explMembers = bodyMembers

{- | The mass of a 'Body'. Read-only: Box3D computes it from the attached
shapes' densities.
-}
newtype BodyMass = BodyMass Float
  deriving (Eq, Show)

instance Component BodyMass where
  type Storage BodyMass = B3Space BodyMass

instance (MonadIO m, Has w m Physics) => Has w m BodyMass where
  getStore = cast <$> (getStore :: SystemT w m (B3Space Physics))

instance (MonadIO m) => ExplGet m (B3Space BodyMass) where
  explExists = bodyExists
  explGet sp ety = liftIO $ withBody sp ety $ fmap BodyMass . B3Body.getMass

instance (MonadIO m) => ExplMembers m (B3Space BodyMass) where
  explMembers = bodyMembers

{- | Write-only: setting it applies a force to the 'Body'\'s center.
Forces are additive and reset by the next 'stepPhysics'.
-}
newtype Force = Force WVec
  deriving (Eq, Show)

instance Component Force where
  type Storage Force = B3Space Force

instance (MonadIO m, Has w m Physics) => Has w m Force where
  getStore = cast <$> (getStore :: SystemT w m (B3Space Physics))

instance (MonadIO m) => ExplSet m (B3Space Force) where
  explSet sp ety (Force v) = liftIO $
    overBody sp ety $ \b ->
      B3Body.applyForceToCenter b v True

{- | Write-only: setting it applies a torque to the 'Body'. Torques are
additive and reset by the next 'stepPhysics'.
-}
newtype Torque = Torque Vec3
  deriving (Eq, Show)

instance Component Torque where
  type Storage Torque = B3Space Torque

instance (MonadIO m, Has w m Physics) => Has w m Torque where
  getStore = cast <$> (getStore :: SystemT w m (B3Space Physics))

instance (MonadIO m) => ExplSet m (B3Space Torque) where
  explSet sp ety (Torque t) = liftIO $
    overBody sp ety $
      \b -> B3Body.applyTorque b t True

-- | Write-only: setting it applies an impulse to the 'Body'\'s center.
newtype LinearImpulse = LinearImpulse WVec
  deriving (Eq, Show)

instance Component LinearImpulse where
  type Storage LinearImpulse = B3Space LinearImpulse

instance (MonadIO m, Has w m Physics) => Has w m LinearImpulse where
  getStore = cast <$> (getStore :: SystemT w m (B3Space Physics))

instance (MonadIO m) => ExplSet m (B3Space LinearImpulse) where
  explSet sp ety (LinearImpulse v) = liftIO $
    overBody sp ety $ \b ->
      B3Body.applyLinearImpulseToCenter b v True

-- | Write-only: setting it applies an angular impulse to the 'Body'.
newtype AngularImpulse = AngularImpulse Vec3
  deriving (Eq, Show)

instance Component AngularImpulse where
  type Storage AngularImpulse = B3Space AngularImpulse

instance (MonadIO m, Has w m Physics) => Has w m AngularImpulse where
  getStore = cast <$> (getStore :: SystemT w m (B3Space Physics))

instance (MonadIO m) => ExplSet m (B3Space AngularImpulse) where
  explSet sp ety (AngularImpulse i) = liftIO $
    overBody sp ety $ \b ->
      B3Body.applyAngularImpulse b i True

{- | Write-only: setting it applies a force to the 'Body' at a world
point; applying off the center of mass also induces spin. Forces are
additive and reset by the next 'stepPhysics'.
-}
data ForceAt = ForceAt WVec WVec
  deriving (Eq, Show)

instance Component ForceAt where
  type Storage ForceAt = B3Space ForceAt

instance (MonadIO m, Has w m Physics) => Has w m ForceAt where
  getStore = cast <$> (getStore :: SystemT w m (B3Space Physics))

instance (MonadIO m) => ExplSet m (B3Space ForceAt) where
  explSet sp ety (ForceAt v p) = liftIO $
    overBody sp ety $ \b ->
      B3Body.applyForce b v p True

{- | Write-only: setting it applies an impulse to the 'Body' at a world
point; applying off the center of mass also induces spin.
-}
data ImpulseAt = ImpulseAt WVec WVec
  deriving (Eq, Show)

instance Component ImpulseAt where
  type Storage ImpulseAt = B3Space ImpulseAt

instance (MonadIO m, Has w m Physics) => Has w m ImpulseAt where
  getStore = cast <$> (getStore :: SystemT w m (B3Space Physics))

instance (MonadIO m) => ExplSet m (B3Space ImpulseAt) where
  explSet sp ety (ImpulseAt v p) = liftIO $
    overBody sp ety $ \b ->
      B3Body.applyLinearImpulse b v p True

{- | Write-only: setting it sets a kinematic 'Body'\'s velocity so it reaches
the given world position and rotation over the given time step — pass the
time delta of your next 'stepPhysics' call. This is the engine path for
moving platforms: unlike teleporting via 'Position', the body carries real
velocity, so it pushes and carries riders. The target is skipped when the
implied velocity is below the sleep threshold; otherwise the body is woken
if asleep, but only when the movement is significant.
-}
data TargetTransform = TargetTransform WVec Quat Float
  deriving (Eq, Show)

instance Component TargetTransform where
  type Storage TargetTransform = B3Space TargetTransform

instance (MonadIO m, Has w m Physics) => Has w m TargetTransform where
  getStore = cast <$> (getStore :: SystemT w m (B3Space Physics))

instance (MonadIO m) => ExplSet m (B3Space TargetTransform) where
  explSet sp ety (TargetTransform p q dt) = liftIO $
    overBody sp ety $ \b ->
      B3Body.setTargetTransform b (Transform p q) dt True

-- | A 'Body'\'s linear velocity damping.
newtype LinearDamping = LinearDamping Float
  deriving (Eq, Show)

instance Component LinearDamping where
  type Storage LinearDamping = B3Space LinearDamping

instance (MonadIO m, Has w m Physics) => Has w m LinearDamping where
  getStore = cast <$> (getStore :: SystemT w m (B3Space Physics))

instance (MonadIO m) => ExplGet m (B3Space LinearDamping) where
  explExists = bodyExists
  explGet sp ety = liftIO $ withBody sp ety $ fmap LinearDamping . B3Body.getLinearDamping

instance (MonadIO m) => ExplSet m (B3Space LinearDamping) where
  explSet sp ety (LinearDamping d) = liftIO $
    overBody sp ety $ \b ->
      B3Body.setLinearDamping b d

instance (MonadIO m) => ExplMembers m (B3Space LinearDamping) where
  explMembers = bodyMembers

-- | A 'Body'\'s angular velocity damping.
newtype AngularDamping = AngularDamping Float
  deriving (Eq, Show)

instance Component AngularDamping where
  type Storage AngularDamping = B3Space AngularDamping

instance (MonadIO m, Has w m Physics) => Has w m AngularDamping where
  getStore = cast <$> (getStore :: SystemT w m (B3Space Physics))

instance (MonadIO m) => ExplGet m (B3Space AngularDamping) where
  explExists = bodyExists
  explGet sp ety = liftIO $ withBody sp ety $ fmap AngularDamping . B3Body.getAngularDamping

instance (MonadIO m) => ExplSet m (B3Space AngularDamping) where
  explSet sp ety (AngularDamping d) = liftIO $
    overBody sp ety $ \b ->
      B3Body.setAngularDamping b d

instance (MonadIO m) => ExplMembers m (B3Space AngularDamping) where
  explMembers = bodyMembers

-- | How strongly gravity affects a 'Body'; 1 is normal, 0 disables it.
newtype GravityScale = GravityScale Float
  deriving (Eq, Show)

instance Component GravityScale where
  type Storage GravityScale = B3Space GravityScale

instance (MonadIO m, Has w m Physics) => Has w m GravityScale where
  getStore = cast <$> (getStore :: SystemT w m (B3Space Physics))

instance (MonadIO m) => ExplGet m (B3Space GravityScale) where
  explExists = bodyExists
  explGet sp ety = liftIO $ withBody sp ety $ fmap GravityScale . B3Body.getGravityScale

instance (MonadIO m) => ExplSet m (B3Space GravityScale) where
  explSet sp ety (GravityScale g) = liftIO $
    overBody sp ety $ \b ->
      B3Body.setGravityScale b g

instance (MonadIO m) => ExplMembers m (B3Space GravityScale) where
  explMembers = bodyMembers

{- | Continuous collision detection for this body (the engine's "bullet"
flag): keeps small, fast bodies from tunnelling through other dynamic
bodies between substeps. Off by default; the cost scales with speed.
-}
newtype BulletBody = BulletBody Bool
  deriving (Eq, Show)

instance Component BulletBody where
  type Storage BulletBody = B3Space BulletBody

instance (MonadIO m, Has w m Physics) => Has w m BulletBody where
  getStore = cast <$> (getStore :: SystemT w m (B3Space Physics))

instance (MonadIO m) => ExplGet m (B3Space BulletBody) where
  explExists = bodyExists
  explGet sp ety = liftIO $ withBody sp ety $ fmap BulletBody . B3Body.isBullet

instance (MonadIO m) => ExplSet m (B3Space BulletBody) where
  explSet sp ety (BulletBody b) = liftIO $
    overBody sp ety $ \bd ->
      B3Body.setBullet bd b

instance (MonadIO m) => ExplMembers m (B3Space BulletBody) where
  explMembers = bodyMembers

{- | Whether a 'Body' participates in the simulation at all (on by
default). Disabling removes the body and its shapes from the world
without destroying them — cheap despawn/pooling; enabling puts them
back.
-}
newtype BodyEnabled = BodyEnabled Bool
  deriving (Eq, Show)

instance Component BodyEnabled where
  type Storage BodyEnabled = B3Space BodyEnabled

instance (MonadIO m, Has w m Physics) => Has w m BodyEnabled where
  getStore = cast <$> (getStore :: SystemT w m (B3Space Physics))

instance (MonadIO m) => ExplGet m (B3Space BodyEnabled) where
  explExists = bodyExists
  explGet sp ety = liftIO $ withBody sp ety $ fmap BodyEnabled . B3Body.isEnabled

instance (MonadIO m) => ExplSet m (B3Space BodyEnabled) where
  explSet sp ety (BodyEnabled e) = liftIO $
    overBody sp ety $ \b ->
      if e then B3Body.enable b else B3Body.disable b

instance (MonadIO m) => ExplMembers m (B3Space BodyEnabled) where
  explMembers = bodyMembers

{- | Whether a 'Body' is currently awake and simulating. Set it to wake
a body explicitly — e.g. after teleporting it via 'Position' — or to
put it to sleep. Waking or sleeping a body extends to the whole island
of bodies touching it.
-}
newtype Awake = Awake Bool
  deriving (Eq, Show)

instance Component Awake where
  type Storage Awake = B3Space Awake

instance (MonadIO m, Has w m Physics) => Has w m Awake where
  getStore = cast <$> (getStore :: SystemT w m (B3Space Physics))

instance (MonadIO m) => ExplGet m (B3Space Awake) where
  explExists = bodyExists
  explGet sp ety = liftIO $ withBody sp ety $ fmap Awake . B3Body.isAwake

instance (MonadIO m) => ExplSet m (B3Space Awake) where
  explSet sp ety (Awake a) = liftIO $
    overBody sp ety $ \b ->
      B3Body.setAwake b a

instance (MonadIO m) => ExplMembers m (B3Space Awake) where
  explMembers = bodyMembers

{- | Per-axis motion locks on a 'Body': locking a linear axis prevents
translation along it, and locking an angular axis prevents rotation
about it. Locking all three angular axes is the 3D analog of Box2D's
"fixed rotation" (contacts and off-center forces can't spin the body);
locking a single linear axis constrains movement to a plane. All axes
are unlocked by default.
-}
data MotionLocks = MotionLocks
  { lockLinearX :: Bool
  , lockLinearY :: Bool
  , lockLinearZ :: Bool
  , lockAngularX :: Bool
  , lockAngularY :: Bool
  , lockAngularZ :: Bool
  }
  deriving (Eq, Show)

toB3MotionLocks :: MotionLocks -> B3T.MotionLocks
toB3MotionLocks (MotionLocks lx ly lz ax ay az) =
  B3T.MotionLocks (fromBool lx) (fromBool ly) (fromBool lz) (fromBool ax) (fromBool ay) (fromBool az)

fromB3MotionLocks :: B3T.MotionLocks -> MotionLocks
fromB3MotionLocks (B3T.MotionLocks lx ly lz ax ay az) =
  MotionLocks (toBool lx) (toBool ly) (toBool lz) (toBool ax) (toBool ay) (toBool az)

instance Component MotionLocks where
  type Storage MotionLocks = B3Space MotionLocks

instance (MonadIO m, Has w m Physics) => Has w m MotionLocks where
  getStore = cast <$> (getStore :: SystemT w m (B3Space Physics))

instance (MonadIO m) => ExplGet m (B3Space MotionLocks) where
  explExists = bodyExists
  explGet sp ety = liftIO $ withBody sp ety $ fmap fromB3MotionLocks . B3Body.getMotionLocks

instance (MonadIO m) => ExplSet m (B3Space MotionLocks) where
  explSet sp ety locks = liftIO $
    overBody sp ety $ \b ->
      B3Body.setMotionLocks b (toB3MotionLocks locks)

instance (MonadIO m) => ExplMembers m (B3Space MotionLocks) where
  explMembers = bodyMembers

{- | Whether a 'Body' may fall asleep at all (on by default). Disabling
it wakes the body (and its island). World-level control is
'SleepingEnabled'.
-}
newtype SleepEnabled = SleepEnabled Bool
  deriving (Eq, Show)

instance Component SleepEnabled where
  type Storage SleepEnabled = B3Space SleepEnabled

instance (MonadIO m, Has w m Physics) => Has w m SleepEnabled where
  getStore = cast <$> (getStore :: SystemT w m (B3Space Physics))

instance (MonadIO m) => ExplGet m (B3Space SleepEnabled) where
  explExists = bodyExists
  explGet sp ety = liftIO $ withBody sp ety $ fmap SleepEnabled . B3Body.isSleepEnabled

instance (MonadIO m) => ExplSet m (B3Space SleepEnabled) where
  explSet sp ety (SleepEnabled e) = liftIO $
    overBody sp ety $ \b ->
      B3Body.enableSleep b e

instance (MonadIO m) => ExplMembers m (B3Space SleepEnabled) where
  explMembers = bodyMembers

{- | The speed below which a 'Body' may fall asleep, usually in meters
per second.
-}
newtype SleepThreshold = SleepThreshold Float
  deriving (Eq, Show)

instance Component SleepThreshold where
  type Storage SleepThreshold = B3Space SleepThreshold

instance (MonadIO m, Has w m Physics) => Has w m SleepThreshold where
  getStore = cast <$> (getStore :: SystemT w m (B3Space Physics))

instance (MonadIO m) => ExplGet m (B3Space SleepThreshold) where
  explExists = bodyExists
  explGet sp ety = liftIO $ withBody sp ety $ fmap SleepThreshold . B3Body.getSleepThreshold

instance (MonadIO m) => ExplSet m (B3Space SleepThreshold) where
  explSet sp ety (SleepThreshold t) = liftIO $
    overBody sp ety $ \b ->
      B3Body.setSleepThreshold b t

instance (MonadIO m) => ExplMembers m (B3Space SleepThreshold) where
  explMembers = bodyMembers

-- Shape ---------------------------------------------------------------------

{- | Wrap a freshly generated engine data pointer in a 'ForeignPtr' whose
finalizer destroys it, and hand it to GC: once the last 'Shape' (and the
last user binding) referencing it is gone, the finalizer runs and frees
the engine-side data. Errors immediately, without allocating a
'ForeignPtr', if the generator rejected its parameters and returned a
null pointer.
-}
wrapGenerated :: String -> (Ptr a -> IO ()) -> IO (Ptr a) -> IO (ForeignPtr a)
wrapGenerated what destroyIt gen = do
  p <- gen
  when (p == nullPtr) $
    error (what <> ": the engine rejected the parameters (returned a null pointer)")
  Concurrent.newForeignPtr p (destroyIt p)

{- | Shared triangle-mesh collision data, produced by the mesh generators
('boxMesh', 'hollowBoxMesh', 'platformMesh', 'gridMesh', 'torusMesh',
'waveMesh'). A 'GeoMesh' shape references this data instead of cloning
it, so the same 'Mesh' can be shared between shapes at different
per-shape scales. The underlying engine mesh is destroyed automatically
once no 'Shape' (and no user binding) references it any more.
-}
newtype Mesh = Mesh (ForeignPtr MeshData)
  deriving (Eq, Show)

{- | Shared height-field collision data, produced by 'gridHeightField' and
'waveHeightField'. A 'GeoHeightField' shape references this data
instead of cloning it, so the same 'HeightField' can be shared between
shapes. The underlying engine height field is destroyed automatically
once no 'Shape' (and no user binding) references it any more.
-}
newtype HeightField = HeightField (ForeignPtr HeightFieldData)
  deriving (Eq, Show)

{- | A pre-built convex hull, produced by 'rockHull', 'coneHull' or
'cylinderHull'. Unlike mesh and height-field data, the engine clones a
hull into the shape at creation time, so the handle only needs to stay
alive until the shape referencing it (a 'GeoReadyHull') is created; the
underlying engine hull is destroyed automatically once no 'Shape' (and
no user binding) references it any more.
-}
newtype Hull = Hull (ForeignPtr HullData)
  deriving (Eq, Show)

-- | Shape geometry in body-local coordinates.
data Geometry
  = -- | Center and radius.
    GeoSphere BVec Float
  | {- | The two hemisphere centers and the radius around the segment
    between them.
    -}
    GeoCapsule BVec BVec Float
  | {- | A box from a local center and half-extents along each axis.
    A zero half-extent makes the corners coplanar and raises an error;
    give flat geometry a small thickness.
    -}
    GeoBox BVec Vec3
  | {- | The convex hull of at least 4 points. Setting a degenerate
    (coplanar) point set raises an error.
    -}
    GeoHull (VS.Vector Vec3)
  | {- | A triangle mesh at a per-shape scale ('Vec3 1 1 1' for
    unscaled). Static bodies only: mesh contacts are only generated
    against static bodies, so attaching this to a dynamic or kinematic
    body creates a shape with no contacts. The engine does /not/ clone
    the mesh data the way it clones hulls — it keeps a reference to the
    'Mesh' for as long as the engine shape exists, which is exactly why
    'Mesh' is GC-lifetime managed; see its docs.
    -}
    GeoMesh Mesh Vec3
  | {- | A height field. Static bodies only, for the same reason as
    'GeoMesh', and likewise the engine references the 'HeightField'
    rather than cloning it.
    -}
    GeoHeightField HeightField
  | {- | A pre-built convex hull from 'rockHull', 'coneHull' or
    'cylinderHull'. Unlike 'GeoMesh'\/'GeoHeightField', the engine
    clones the hull data into the shape, so the 'Hull' handle only needs
    to live until the shape is created.
    -}
    GeoReadyHull Hull
  deriving (Eq, Show)

{- | Gives an entity a collision shape attached to the 'Body' of the given
entity (which may be the same entity). Carries the sub-components
'Density', 'Friction' and 'Elasticity'; re-setting the geometry
preserves them. Reads return the exact value written; geometry mutated
through the raw engine is not reflected.
-}
data Shape = Shape Entity Geometry
  deriving (Eq, Show)

instance Component Shape where
  type Storage Shape = B3Space Shape

instance (MonadIO m, Has w m Physics) => Has w m Shape where
  getStore = cast <$> (getStore :: SystemT w m (B3Space Physics))

{- | Build a hull shape from a point cloud; the engine clones the hull
data, so the intermediate hull is destroyed right after. The label
names the originating 'Geometry' constructor in errors.
-}
createHullShape :: String -> BodyId -> B3T.ShapeDef -> VS.Vector Vec3 -> IO ShapeId
createHullShape what b sd pts = do
  let n = VS.length pts
  when (n < 4) $
    error (what <> " needs at least 4 points, got " <> show n)
  hull <- VS.unsafeWith pts $ \p -> B3Hull.create p n n
  when (hull == nullPtr) $
    error (what <> " points are degenerate (coplanar or coincident)")
  s <- B3Shape.createHull b sd hull
  B3Hull.destroy hull
  pure s

boxCorners :: Vec3 -> Vec3 -> VS.Vector Vec3
boxCorners (Vec3 cx cy cz) (Vec3 hx hy hz) =
  VS.fromList
    [ Vec3 (cx + sx * hx) (cy + sy * hy) (cz + sz * hz)
    | sx <- [-1, 1]
    , sy <- [-1, 1]
    , sz <- [-1, 1]
    ]

-- | Create the engine geometry for a 'Geometry' value on a body.
createGeometry :: BodyId -> B3T.ShapeDef -> Geometry -> IO ShapeId
createGeometry b sd geo = case geo of
  GeoSphere c r -> B3Shape.createSphere b sd (B3T.Sphere c r)
  GeoCapsule c1 c2 r -> B3Shape.createCapsule b sd (B3T.Capsule c1 c2 r)
  GeoBox c half -> createHullShape "GeoBox" b sd (boxCorners c half)
  GeoHull pts -> createHullShape "GeoHull" b sd pts
  GeoMesh (Mesh fp) scale -> withForeignPtr fp $ \p -> B3Shape.createMesh b sd p scale
  GeoHeightField (HeightField fp) -> withForeignPtr fp $ \p -> B3Shape.createHeightField b sd p
  GeoReadyHull (Hull fp) -> withForeignPtr fp $ \p -> B3Shape.createHull b sd p

-- Static geometry generators ------------------------------------------------

{- | A solid box mesh: 12 triangles from a local center and half-extents
along each axis (matching 'GeoBox'\'s half-extent convention). Triangle
adjacency is always identified (see the "internal edges" note on
'gridMesh') since there is no legitimate reason to skip it for a shape
this small.
-}
boxMesh :: BVec -> Vec3 -> IO Mesh
boxMesh center halfExtents =
  Mesh <$> wrapGenerated "boxMesh" B3Mesh.destroy (B3BoxMesh.create center halfExtents True)

{- | A hollow box mesh: the same box as 'boxMesh' but with every triangle's
winding reversed, so its inside faces are solid and its outside is
open — a box-shaped room instead of a box-shaped solid.
-}
hollowBoxMesh :: BVec -> Vec3 -> IO Mesh
hollowBoxMesh center halfExtents =
  Mesh <$> wrapGenerated "hollowBoxMesh" B3Mesh.destroy (B3HollowBoxMesh.create center halfExtents)

{- | A platform mesh: a truncated pyramid (frustum) centered locally on
'center', with a 'topWidth' square face at +height\/2 and a
'bottomWidth' square face at -height\/2.
-}
platformMesh :: BVec -> Float -> Float -> Float -> IO Mesh
platformMesh center height topWidth bottomWidth =
  Mesh <$> wrapGenerated "platformMesh" B3Mesh.destroy (B3PlatformMesh.create center height topWidth bottomWidth)

{- | A flat grid mesh of @xCount * zCount@ cells (each 'cellWidth' wide) in
the local XZ plane, centered on the origin. 'materialCount' round-robins
the triangles across that many per-shape material slots for
'B3Shape.setMeshMaterial' (0 or 1 for a single material). Triangle
adjacency is always identified: this flags shared edges between
coplanar (or near-coplanar) triangles as non-colliding "internal
edges", which is what stops a ball or capsule from catching on the
seams between a mesh's triangles as it rolls across them. There is no
real use case for turning this off, so unlike the upstream C API this
binding does not expose the choice.
-}
gridMesh :: Int -> Int -> Float -> Int -> IO Mesh
gridMesh xCount zCount cellWidth materialCount =
  Mesh <$> wrapGenerated "gridMesh" B3Mesh.destroy (B3GridMesh.create xCount zCount cellWidth materialCount True)

{- | A torus mesh centered on the origin, its main ring lying in the local
XY plane (the tube's axis is local Z). 'radialResolution' is the number
of segments around the tube's circular cross-section and
'tubularResolution' is the number of segments around the main ring;
'radius' is the distance from the origin to the tube's center line and
'thickness' is the tube's cross-section radius.
-}
torusMesh :: Int -> Int -> Float -> Float -> IO Mesh
torusMesh radialResolution tubularResolution radius thickness =
  Mesh <$> wrapGenerated "torusMesh" B3Mesh.destroy (B3TorusMesh.create radialResolution tubularResolution radius thickness)

{- | A wavy grid mesh like 'gridMesh', with the vertex at row @ix@, column
@iz@ (0-based, along local x and z respectively) displaced to height
@amplitude * sin (2*pi*columnFrequency*cellWidth*ix) * sin
(2*pi*rowFrequency*cellWidth*iz)@. Note this is /not/ a typo in this
binding: in the upstream generator, 'columnFrequency' is the frequency
along x and 'rowFrequency' is the frequency along z — the reverse of
what the names suggest. Triangle adjacency is always identified, as
for 'gridMesh'.
-}
waveMesh :: Int -> Int -> Float -> Float -> Float -> Float -> IO Mesh
waveMesh xCount zCount cellWidth amplitude rowFrequency columnFrequency =
  Mesh
    <$> wrapGenerated
      "waveMesh"
      B3Mesh.destroy
      (B3WaveMesh.create xCount zCount cellWidth amplitude rowFrequency columnFrequency)

{- | A flat height-field grid of 'rowCount' * 'columnCount' samples.
'scale' converts grid index space to local space: index spacing along
x is @scale@\'s x component, spacing along z is its z component, and
sample heights (all zero for a flat grid) are multiplied by its y
component. Unlike the mesh generators, a height field's local origin
is a corner (grid index @(0, 0)@), not its center.

When 'makeHoles' is true, every 16th cell (by row-major index,
starting at the 16th) is punched out as a hole shapes fall through —
a fixed test pattern baked into the upstream generator, not a
configurable spacing; use 'gridMesh'\/a custom 'GeoMesh' instead if you
need holes somewhere specific.
-}
gridHeightField :: Int -> Int -> Vec3 -> Bool -> IO HeightField
gridHeightField rowCount columnCount scale makeHoles =
  HeightField <$> wrapGenerated "gridHeightField" B3HeightField.destroy (B3Grid.create rowCount columnCount scale makeHoles)

{- | A wavy height-field grid like 'gridHeightField', with the sample at
row @i@, column @j@ (0-based, in grid index space) set to
@sin (2*pi*rowFrequency*i) * sin (2*pi*columnFrequency*j)@ — unlike
'waveMesh', these frequencies are cycles per grid cell, not per local
unit. Raw samples are therefore always in @[-1, 1]@; scale the result
in local space with 'scale'\'s y component. 'makeHoles' is as in
'gridHeightField'.
-}
waveHeightField :: Int -> Int -> Vec3 -> Float -> Float -> Bool -> IO HeightField
waveHeightField rowCount columnCount scale rowFrequency columnFrequency makeHoles =
  HeightField
    <$> wrapGenerated
      "waveHeightField"
      B3HeightField.destroy
      (B3Wave.create rowCount columnCount scale rowFrequency columnFrequency makeHoles)

{- | A rock-shaped convex hull: 10 points spread over a sphere of the given
radius by a Fibonacci lattice, giving an irregular but bounded hull
useful for scatter/debris. Errors if the points come out degenerate
(e.g. 'radius' is zero, collapsing them to a point).
-}
rockHull :: Float -> IO Hull
rockHull radius =
  Hull <$> wrapGenerated "rockHull" B3Hull.destroy (B3Rock.create radius)

{- | A tessellated cone as a convex hull: a 'radius1' circle at local y 0
and a 'radius2' circle at local y 'height', joined by 'slices' sides
(the engine clamps this to [4, 32]). Equal radii give a cylinder-like
shape, but prefer 'cylinderHull' for that since it also supports an
axial offset.
-}
coneHull :: Float -> Float -> Float -> Int -> IO Hull
coneHull height radius1 radius2 slices =
  Hull <$> wrapGenerated "coneHull" B3Hull.destroy (B3Cone.create height radius1 radius2 slices)

{- | A tessellated cylinder as a convex hull: 'radius' circles at local y
'yOffset' and 'yOffset' + 'height', joined by 'sides' sides (the engine
clamps this to [3, 32]).
-}
cylinderHull :: Float -> Float -> Float -> Int -> IO Hull
cylinderHull height radius yOffset sides =
  Hull <$> wrapGenerated "cylinderHull" B3Hull.destroy (B3Cylinder.create height radius yOffset sides)

{- | A shape def with the surface material, density and filter carried
over from the shape being replaced, if any.
-}
carryMaterial :: B3T.ShapeDef -> Maybe ShapeRecord -> IO B3T.ShapeDef
carryMaterial sd Nothing = pure sd
carryMaterial sd (Just (ShapeRecord s _)) = do
  material <- B3Shape.getSurfaceMaterial s
  density <- B3Shape.getDensity s
  filtr <- B3Shape.getFilter s
  pure
    sd
      { B3T.shapeDefBaseMaterial = material
      , B3T.shapeDefDensity = density
      , B3T.shapeDefFilter = filtr
      }

{- | Create a fresh engine shape for a 'Shape' value with the given def,
tag it with the entity's user index, destroy the shape it replaces (if
any) only after the new one exists (so a failed create, e.g. a bad
hull, leaves everything intact), and update the shape registry. Shared
by 'Shape' and 'Sensor', which both recreate the shape while preserving
its material state.
-}
recreateShape :: B3Space c -> BodyId -> Int -> B3T.ShapeDef -> Shape -> Maybe ShapeRecord -> IO ()
recreateShape sp b ety sd shape@(Shape _ geo) old = do
  s <- createGeometry b sd geo
  setUserIndex s ety
  forM_ old $ \(ShapeRecord s' _) -> B3Shape.destroy s' True
  modifyIORef' (spShapes sp) (IM.insert ety (ShapeRecord s shape))

instance (MonadIO m) => ExplSet m (B3Space Shape) where
  explSet sp ety shape@(Shape (Entity bEty) _) = liftIO $
    overBody sp bEty $ \b -> do
      old <- IM.lookup ety <$> readIORef (spShapes sp)
      sd <- carryMaterial (spShapeDef sp) old
      recreateShape sp b ety sd shape old

instance (MonadIO m) => ExplGet m (B3Space Shape) where
  explExists = shapeExists
  explGet sp ety = liftIO $
    withReg "Shape" (spShapes sp) ety $
      \(ShapeRecord _ shape) -> pure shape

instance (MonadIO m) => ExplDestroy m (B3Space Shape) where
  explDestroy sp ety = liftIO $ do
    shapes <- readIORef (spShapes sp)
    forM_ (IM.lookup ety shapes) $ \(ShapeRecord s _) -> do
      modifyIORef' (spShapes sp) (IM.delete ety)
      B3Shape.destroy s True

instance (MonadIO m) => ExplMembers m (B3Space Shape) where
  explMembers = shapeMembers

-- | The raw Box3D shape of an entity, for use with "Box3D.Shape" directly.
newtype B3ShapeId = B3ShapeId ShapeId
  deriving (Eq, Show)

instance Component B3ShapeId where
  type Storage B3ShapeId = B3Space B3ShapeId

instance (MonadIO m, Has w m Physics) => Has w m B3ShapeId where
  getStore = cast <$> (getStore :: SystemT w m (B3Space Physics))

instance (MonadIO m) => ExplGet m (B3Space B3ShapeId) where
  explExists = shapeExists
  explGet sp ety = liftIO $ withShape sp ety (pure . B3ShapeId)

instance (MonadIO m) => ExplMembers m (B3Space B3ShapeId) where
  explMembers = shapeMembers

-- Shape sub-components -----------------------------------------------------

-- | The density of a 'Shape'. Setting it updates the body's mass.
newtype Density = Density Float
  deriving (Eq, Show)

instance Component Density where
  type Storage Density = B3Space Density

instance (MonadIO m, Has w m Physics) => Has w m Density where
  getStore = cast <$> (getStore :: SystemT w m (B3Space Physics))

instance (MonadIO m) => ExplGet m (B3Space Density) where
  explExists = shapeExists
  explGet sp ety = liftIO $ withShape sp ety $ fmap Density . B3Shape.getDensity

instance (MonadIO m) => ExplSet m (B3Space Density) where
  explSet sp ety (Density d) = liftIO $
    overShape sp ety $
      \s -> B3Shape.setDensity s d True

instance (MonadIO m) => ExplMembers m (B3Space Density) where
  explMembers = shapeMembers

-- | The friction coefficient of a 'Shape'.
newtype Friction = Friction Float
  deriving (Eq, Show)

instance Component Friction where
  type Storage Friction = B3Space Friction

instance (MonadIO m, Has w m Physics) => Has w m Friction where
  getStore = cast <$> (getStore :: SystemT w m (B3Space Physics))

instance (MonadIO m) => ExplGet m (B3Space Friction) where
  explExists = shapeExists
  explGet sp ety = liftIO $ withShape sp ety $ fmap Friction . B3Shape.getFriction

instance (MonadIO m) => ExplSet m (B3Space Friction) where
  explSet sp ety (Friction f) = liftIO $
    overShape sp ety $
      \s -> B3Shape.setFriction s f

instance (MonadIO m) => ExplMembers m (B3Space Friction) where
  explMembers = shapeMembers

-- | The elasticity of a 'Shape' (Box3D calls this restitution).
newtype Elasticity = Elasticity Float
  deriving (Eq, Show)

instance Component Elasticity where
  type Storage Elasticity = B3Space Elasticity

instance (MonadIO m, Has w m Physics) => Has w m Elasticity where
  getStore = cast <$> (getStore :: SystemT w m (B3Space Physics))

instance (MonadIO m) => ExplGet m (B3Space Elasticity) where
  explExists = shapeExists
  explGet sp ety = liftIO $ withShape sp ety $ fmap Elasticity . B3Shape.getRestitution

instance (MonadIO m) => ExplSet m (B3Space Elasticity) where
  explSet sp ety (Elasticity e) = liftIO $
    overShape sp ety $
      \s -> B3Shape.setRestitution s e

instance (MonadIO m) => ExplMembers m (B3Space Elasticity) where
  explMembers = shapeMembers

-- | The collision 'Filter' of a 'Shape' (category, mask, group).
newtype CollisionFilter = CollisionFilter Filter
  deriving (Eq, Show)

instance Component CollisionFilter where
  type Storage CollisionFilter = B3Space CollisionFilter

instance (MonadIO m, Has w m Physics) => Has w m CollisionFilter where
  getStore = cast <$> (getStore :: SystemT w m (B3Space Physics))

instance (MonadIO m) => ExplGet m (B3Space CollisionFilter) where
  explExists = shapeExists
  explGet sp ety = liftIO $ withShape sp ety $ fmap CollisionFilter . B3Shape.getFilter

instance (MonadIO m) => ExplSet m (B3Space CollisionFilter) where
  explSet sp ety (CollisionFilter f) = liftIO $
    overShape sp ety $
      \s -> B3Shape.setFilter s f True

instance (MonadIO m) => ExplMembers m (B3Space CollisionFilter) where
  explMembers = shapeMembers

{- | Whether a 'Shape' is a sensor: a trigger volume that reports
overlaps through 'SensorEvents' instead of generating contacts. Box3D
has no way to change a live shape's sensor flag, so setting this
recreates the engine shape (as 'Shape' does, preserving 'Density',
'Friction', 'Elasticity' and 'CollisionFilter') whenever the requested
value differs from the shape's current one; setting the value it
already has is a no-op. Reads reflect the engine.
-}
newtype Sensor = Sensor Bool
  deriving (Eq, Show)

instance Component Sensor where
  type Storage Sensor = B3Space Sensor

instance (MonadIO m, Has w m Physics) => Has w m Sensor where
  getStore = cast <$> (getStore :: SystemT w m (B3Space Physics))

instance (MonadIO m) => ExplGet m (B3Space Sensor) where
  explExists = shapeExists
  explGet sp ety = liftIO $ withShape sp ety $ fmap Sensor . B3Shape.isSensor

instance (MonadIO m) => ExplSet m (B3Space Sensor) where
  explSet sp ety (Sensor wantSensor) = liftIO $ do
    old <- IM.lookup ety <$> readIORef (spShapes sp)
    forM_ old $ \old'@(ShapeRecord s shape@(Shape (Entity bEty) _)) -> do
      isSensorNow <- B3Shape.isSensor s
      when (isSensorNow /= wantSensor) $
        overBody sp bEty $ \b -> do
          sd <- carryMaterial (spShapeDef sp) (Just old')
          recreateShape sp b ety sd{B3T.shapeDefIsSensor = fromBool wantSensor} shape (Just old')

instance (MonadIO m) => ExplMembers m (B3Space Sensor) where
  explMembers = shapeMembers

-- Joint ----------------------------------------------------------------------

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

{- | Gives an entity a joint connecting the 'Body's of the two given
entities, which must be distinct (the engine rejects self-joints;
setting one is a silent no-op). Reads return the exact value written.
The tuning sub-components ('MotorSpeed', 'JointLimits', ...) mutate
the live engine joint without touching the stored spec, so a re-set
'Joint' recreates the joint from the original spec and discards
tuning.
-}
data Joint = Joint Entity Entity JointSpec
  deriving (Eq, Show)

instance Component Joint where
  type Storage Joint = B3Space Joint

instance (MonadIO m, Has w m Physics) => Has w m Joint where
  getStore = cast <$> (getStore :: SystemT w m (B3Space Physics))

-- | A joint frame at a world point, with zero rotation in world space.
frameAt :: BodyId -> Vec3 -> IO Transform
frameAt b p = do
  local <- B3Body.getLocalPoint b p
  Quat (Vec3 x y z) w <- B3Body.getRotation b
  pure (Transform local (Quat (Vec3 (-x) (-y) (-z)) w))

{- | Fill a joint def's base with the two bodies and their frames at
their respective world anchors (shared-point joints pass the same
anchor twice).
-}
baseAt :: B3T.JointDef -> BodyId -> BodyId -> Vec3 -> Vec3 -> IO B3T.JointDef
baseAt jd a b pA pB = do
  fa <- frameAt a pA
  fb <- frameAt b pB
  pure
    jd
      { B3T.jointDefBodyIdA = a
      , B3T.jointDefBodyIdB = b
      , B3T.jointDefLocalFrameA = fa
      , B3T.jointDefLocalFrameB = fb
      }

-- | Conjugate (inverse for unit quaternions).
qConj :: Quat -> Quat
qConj (Quat (Vec3 x y z) w) = Quat (Vec3 (-x) (-y) (-z)) w

-- | Hamilton product; composes rotations (apply the right one first).
qMul :: Quat -> Quat -> Quat
qMul (Quat (Vec3 x1 y1 z1) w1) (Quat (Vec3 x2 y2 z2) w2) =
  Quat
    ( Vec3
        (w1 * x2 + x1 * w2 + y1 * z2 - z1 * y2)
        (w1 * y2 - x1 * z2 + y1 * w2 + z1 * x2)
        (w1 * z2 + x1 * y2 - y1 * x2 + z1 * w2)
    )
    (w1 * w2 - x1 * x2 - y1 * y2 - z1 * z2)

-- | Normalize a vector; errors on a zero (or NaN) length, which has no direction.
vNormalize :: Vec3 -> Vec3
vNormalize (Vec3 x y z)
  | m > 0 = Vec3 (x / m) (y / m) (z / m)
  | otherwise = error "vNormalize: zero-length vector"
  where
    m = sqrt (x * x + y * y + z * z)

{- | A joint frame at a world point whose canonical axis points along a
world axis. World orientation of a joint frame is @q_body * q_local@;
cancelling the body rotation and then composing with the aligning
rotation from the canonical axis to the world axis gives a frame whose
canonical axis is that world axis, independent of the body's own
orientation.
-}
axisFrameAt :: Vec3 -> BodyId -> WVec -> WVec -> IO Transform
axisFrameAt canonical b p axis = do
  local <- B3Body.getLocalPoint b p
  qb <- B3Body.getRotation b
  qa <- computeQuatBetweenUnitVectors canonical (vNormalize axis)
  pure (Transform local (qMul (qConj qb) qa))

{- | Fill a joint def's base with the two bodies and frames at a shared
world anchor, both oriented so the frame's canonical axis points along
a world axis.
-}
axisBaseAt :: B3T.JointDef -> BodyId -> BodyId -> Vec3 -> Vec3 -> Vec3 -> IO B3T.JointDef
axisBaseAt jd a b canonical p axis = do
  fa <- axisFrameAt canonical a p axis
  fb <- axisFrameAt canonical b p axis
  pure
    jd
      { B3T.jointDefBodyIdA = a
      , B3T.jointDefBodyIdB = b
      , B3T.jointDefLocalFrameA = fa
      , B3T.jointDefLocalFrameB = fb
      }

{- | The quaternion rotating the world axes onto an orthonormal
right-handed basis given as the columns (x, y, z) of a rotation matrix.
Shepperd's method: picks the numerically stable branch by the sign of
the trace and the largest diagonal element.
-}
quatFromBasis :: Vec3 -> Vec3 -> Vec3 -> Quat
quatFromBasis (Vec3 m00 m10 m20) (Vec3 m01 m11 m21) (Vec3 m02 m12 m22)
  | trace > 0 =
      let s = sqrt (trace + 1) * 2
      in mk ((m21 - m12) / s) ((m02 - m20) / s) ((m10 - m01) / s) (0.25 * s)
  | m00 > m11 && m00 > m22 =
      let s = sqrt (1 + m00 - m11 - m22) * 2
      in mk (0.25 * s) ((m01 + m10) / s) ((m02 + m20) / s) ((m21 - m12) / s)
  | m11 > m22 =
      let s = sqrt (1 + m11 - m00 - m22) * 2
      in mk ((m01 + m10) / s) (0.25 * s) ((m12 + m21) / s) ((m02 - m20) / s)
  | otherwise =
      let s = sqrt (1 + m22 - m00 - m11) * 2
      in mk ((m02 + m20) / s) ((m12 + m21) / s) (0.25 * s) ((m10 - m01) / s)
  where
    trace = m00 + m11 + m22
    mk x y z w = Quat (Vec3 x y z) w

{- | Fill a wheel joint def's base: both bodies get a frame at the
shared world anchor built from an orthonormal basis whose x-axis is the
suspension direction and whose z-axis is the axle direction, obtained
by Gram-Schmidt against the suspension axis (erroring if the axle is
parallel to the suspension axis, since no such basis exists then).
-}
wheelBaseAt :: B3T.JointDef -> BodyId -> BodyId -> WVec -> WVec -> WVec -> IO B3T.JointDef
wheelBaseAt jd a b p suspension axle = do
  la <- B3Body.getLocalPoint a p
  lb <- B3Body.getLocalPoint b p
  qa <- B3Body.getRotation a
  qb <- B3Body.getRotation b
  let
    xAxis@(Vec3 xx xy xz) = vNormalize suspension
    Vec3 ax ay az = axle
    onto = ax * xx + ay * xy + az * xz
    zAxis@(Vec3 zx zy zz) = vNormalize (Vec3 (ax - onto * xx) (ay - onto * xy) (az - onto * xz))
    yAxis = Vec3 (zy * xz - zz * xy) (zz * xx - zx * xz) (zx * xy - zy * xx)
    qAlign = quatFromBasis xAxis yAxis zAxis
  pure
    jd
      { B3T.jointDefBodyIdA = a
      , B3T.jointDefBodyIdB = b
      , B3T.jointDefLocalFrameA = Transform la (qMul (qConj qa) qAlign)
      , B3T.jointDefLocalFrameB = Transform lb (qMul (qConj qb) qAlign)
      }

createJoint :: WorldId -> BodyId -> BodyId -> JointSpec -> IO JointId
createJoint w a b spec = case spec of
  PivotJoint p -> do
    jd <- B3T.defaultSphericalJointDef
    base <- baseAt (B3T.sphericalJointDefBase jd) a b p p
    B3SphericalJoint.create w jd{B3T.sphericalJointDefBase = base}
  DistanceJoint pA pB -> do
    jd <- B3T.defaultDistanceJointDef
    base <- baseAt (B3T.distanceJointDefBase jd) a b pA pB
    let
      Vec3 x1 y1 z1 = pA
      Vec3 x2 y2 z2 = pB
      len = sqrt ((x2 - x1) ^ two + (y2 - y1) ^ two + (z2 - z1) ^ two)
      two = 2 :: Int
    B3DistanceJoint.create w jd{B3T.distanceJointDefBase = base, B3T.distanceJointDefLength = len}
  WeldJoint p -> do
    jd <- B3T.defaultWeldJointDef
    base <- baseAt (B3T.weldJointDefBase jd) a b p p
    B3WeldJoint.create w jd{B3T.weldJointDefBase = base}
  HingeJoint p axis -> hingeAt p axis id
  HingeSpringJoint p axis hertz damping ->
    hingeAt p axis $ \jd ->
      jd
        { B3T.revoluteJointDefEnableSpring = 1
        , B3T.revoluteJointDefHertz = hertz
        , B3T.revoluteJointDefDampingRatio = damping
        }
  HingeLimitJoint p axis lower upper ->
    hingeAt p axis $ \jd ->
      jd
        { B3T.revoluteJointDefEnableLimit = 1
        , B3T.revoluteJointDefLowerAngle = lower
        , B3T.revoluteJointDefUpperAngle = upper
        }
  HingeMotorJoint p axis speed maxTorque ->
    hingeAt p axis $ \jd ->
      jd
        { B3T.revoluteJointDefEnableMotor = 1
        , B3T.revoluteJointDefMotorSpeed = speed
        , B3T.revoluteJointDefMaxMotorTorque = maxTorque
        }
  PrismaticJoint p axis lower upper ->
    -- prismatic joints already forbid relative rotation, so the limit
    -- alone is enough to keep the translation free within it
    sliderAt p axis $ \jd ->
      jd
        { B3T.prismaticJointDefEnableLimit = 1
        , B3T.prismaticJointDefLowerTranslation = lower
        , B3T.prismaticJointDefUpperTranslation = upper
        }
  PrismaticSpringJoint p axis hertz damping ->
    sliderAt p axis $ \jd ->
      jd
        { B3T.prismaticJointDefEnableSpring = 1
        , B3T.prismaticJointDefHertz = hertz
        , B3T.prismaticJointDefDampingRatio = damping
        }
  PrismaticMotorJoint p axis speed maxForce ->
    sliderAt p axis $ \jd ->
      jd
        { B3T.prismaticJointDefEnableMotor = 1
        , B3T.prismaticJointDefMotorSpeed = speed
        , B3T.prismaticJointDefMaxMotorForce = maxForce
        }
  WheelJoint p suspension axle hertz damping -> do
    jd <- B3T.defaultWheelJointDef
    base <- wheelBaseAt (B3T.wheelJointDefBase jd) a b p suspension axle
    B3WheelJoint.create
      w
      jd
        { B3T.wheelJointDefBase = base
        , B3T.wheelJointDefEnableSuspensionSpring = 1
        , B3T.wheelJointDefSuspensionHertz = hertz
        , B3T.wheelJointDefSuspensionDampingRatio = damping
        }
  where
    hingeAt p axis f = do
      jd <- B3T.defaultRevoluteJointDef
      base <- axisBaseAt (B3T.revoluteJointDefBase jd) a b (Vec3 0 0 1) p axis
      B3RevoluteJoint.create w (f jd){B3T.revoluteJointDefBase = base}
    sliderAt p axis f = do
      jd <- B3T.defaultPrismaticJointDef
      base <- axisBaseAt (B3T.prismaticJointDefBase jd) a b (Vec3 1 0 0) p axis
      B3PrismaticJoint.create w (f jd){B3T.prismaticJointDefBase = base}

instance (MonadIO m) => ExplSet m (B3Space Joint) where
  explSet sp ety joint@(Joint (Entity aEty) (Entity bEty) spec) = liftIO $ when (aEty /= bEty) $ do
    bodies <- readIORef (spBodies sp)
    forM_ ((,) <$> IM.lookup aEty bodies <*> IM.lookup bEty bodies) $ \(a, b) -> do
      old <- IM.lookup ety <$> readIORef (spJoints sp)
      j <- createJoint (spWorld sp) a b spec
      setUserIndex j ety
      forM_ old $ \(JointRecord j' _) -> B3Joint.destroy j' True
      modifyIORef' (spJoints sp) (IM.insert ety (JointRecord j joint))

instance (MonadIO m) => ExplGet m (B3Space Joint) where
  explExists = jointExists
  explGet sp ety = liftIO $
    withReg "Joint" (spJoints sp) ety $
      \(JointRecord _ joint) -> pure joint

instance (MonadIO m) => ExplDestroy m (B3Space Joint) where
  explDestroy sp ety = liftIO $ do
    joints <- readIORef (spJoints sp)
    forM_ (IM.lookup ety joints) $ \(JointRecord j _) -> do
      modifyIORef' (spJoints sp) (IM.delete ety)
      B3Joint.destroy j True

instance (MonadIO m) => ExplMembers m (B3Space Joint) where
  explMembers = jointMembers

-- | The raw Box3D joint of an entity, for use with the joint modules.
newtype B3JointId = B3JointId JointId
  deriving (Eq, Show)

instance Component B3JointId where
  type Storage B3JointId = B3Space B3JointId

instance (MonadIO m, Has w m Physics) => Has w m B3JointId where
  getStore = cast <$> (getStore :: SystemT w m (B3Space Physics))

instance (MonadIO m) => ExplGet m (B3Space B3JointId) where
  explExists = jointExists
  explGet sp ety = liftIO $ withJoint sp ety (pure . B3JointId)

instance (MonadIO m) => ExplMembers m (B3Space B3JointId) where
  explMembers = jointMembers

{- | The motor's target speed on a 'Joint': radians per second on a
hinge (revolute) joint ('HingeJoint', 'HingeSpringJoint',
'HingeLimitJoint', 'HingeMotorJoint'), meters per second on a
prismatic joint ('PrismaticJoint', 'PrismaticSpringJoint',
'PrismaticMotorJoint'), or radians per second on a wheel joint's spin
motor ('WheelJoint'). Setting this also enables the corresponding
motor, so a speed always takes effect immediately; use
'MotorMaxTorque'\/'MotorMaxForce' to cap it without starting it. The
wheel's suspension spring\/limit and steering are not covered by this
component. Setting it on any other joint kind, or on an entity with
no 'Joint', is a silent no-op.
-}
newtype MotorSpeed = MotorSpeed Float
  deriving (Eq, Show)

instance Component MotorSpeed where
  type Storage MotorSpeed = B3Space MotorSpeed

instance (MonadIO m, Has w m Physics) => Has w m MotorSpeed where
  getStore = cast <$> (getStore :: SystemT w m (B3Space Physics))

instance (MonadIO m) => ExplGet m (B3Space MotorSpeed) where
  explExists sp ety = liftIO $ jointIsKind sp ety [B3T.RevoluteJoint, B3T.PrismaticJoint, B3T.WheelJoint]
  explGet sp ety = liftIO $ withJoint sp ety $ \j -> do
    ty <- B3Joint.getType j
    MotorSpeed <$> case ty of
      B3T.PrismaticJoint -> B3PrismaticJoint.getMotorSpeed j
      B3T.WheelJoint -> B3WheelJoint.getSpinMotorSpeed j
      _ -> B3RevoluteJoint.getMotorSpeed j

instance (MonadIO m) => ExplSet m (B3Space MotorSpeed) where
  explSet sp ety (MotorSpeed v) = liftIO $
    overJoint sp ety $ \j -> do
      ty <- B3Joint.getType j
      case ty of
        B3T.RevoluteJoint -> B3RevoluteJoint.setMotorSpeed j v >> B3RevoluteJoint.enableMotor j True
        B3T.PrismaticJoint -> B3PrismaticJoint.setMotorSpeed j v >> B3PrismaticJoint.enableMotor j True
        B3T.WheelJoint -> B3WheelJoint.setSpinMotorSpeed j v >> B3WheelJoint.enableSpinMotor j True
        _ -> pure ()

instance (MonadIO m) => ExplMembers m (B3Space MotorSpeed) where
  explMembers = jointMembers

{- | The motor's maximum torque on a 'Joint', usually in newton-meters:
a hinge (revolute) joint's motor, or a wheel joint's spin motor
('WheelJoint', suspension and steering not covered). Unlike
'MotorSpeed', setting this only sets the cap — it does not enable the
motor, so setting a cap alone does not start it. Setting it on any
other joint kind, or on an entity with no 'Joint', is a silent no-op.
-}
newtype MotorMaxTorque = MotorMaxTorque Float
  deriving (Eq, Show)

instance Component MotorMaxTorque where
  type Storage MotorMaxTorque = B3Space MotorMaxTorque

instance (MonadIO m, Has w m Physics) => Has w m MotorMaxTorque where
  getStore = cast <$> (getStore :: SystemT w m (B3Space Physics))

instance (MonadIO m) => ExplGet m (B3Space MotorMaxTorque) where
  explExists sp ety = liftIO $ jointIsKind sp ety [B3T.RevoluteJoint, B3T.WheelJoint]
  explGet sp ety = liftIO $ withJoint sp ety $ \j -> do
    ty <- B3Joint.getType j
    MotorMaxTorque <$> case ty of
      B3T.WheelJoint -> B3WheelJoint.getMaxSpinTorque j
      _ -> B3RevoluteJoint.getMaxMotorTorque j

instance (MonadIO m) => ExplSet m (B3Space MotorMaxTorque) where
  explSet sp ety (MotorMaxTorque v) = liftIO $
    overJoint sp ety $ \j -> do
      ty <- B3Joint.getType j
      case ty of
        B3T.RevoluteJoint -> B3RevoluteJoint.setMaxMotorTorque j v
        B3T.WheelJoint -> B3WheelJoint.setMaxSpinTorque j v
        _ -> pure ()

instance (MonadIO m) => ExplMembers m (B3Space MotorMaxTorque) where
  explMembers = jointMembers

{- | The motor's maximum force on a prismatic 'Joint' ('PrismaticJoint',
'PrismaticSpringJoint', 'PrismaticMotorJoint'), usually in newtons.
Like 'MotorMaxTorque', setting this only sets the cap — it does not
enable the motor. Setting it on any other joint kind, or on an entity
with no 'Joint', is a silent no-op.
-}
newtype MotorMaxForce = MotorMaxForce Float
  deriving (Eq, Show)

instance Component MotorMaxForce where
  type Storage MotorMaxForce = B3Space MotorMaxForce

instance (MonadIO m, Has w m Physics) => Has w m MotorMaxForce where
  getStore = cast <$> (getStore :: SystemT w m (B3Space Physics))

instance (MonadIO m) => ExplGet m (B3Space MotorMaxForce) where
  explExists sp ety = liftIO $ jointIsKind sp ety [B3T.PrismaticJoint]
  explGet sp ety = liftIO $ withJoint sp ety $ fmap MotorMaxForce . B3PrismaticJoint.getMaxMotorForce

instance (MonadIO m) => ExplSet m (B3Space MotorMaxForce) where
  explSet sp ety (MotorMaxForce v) = liftIO $
    overJoint sp ety $ \j -> do
      ty <- B3Joint.getType j
      case ty of
        B3T.PrismaticJoint -> B3PrismaticJoint.setMaxMotorForce j v
        _ -> pure ()

instance (MonadIO m) => ExplMembers m (B3Space MotorMaxForce) where
  explMembers = jointMembers

{- | The (lower, upper) limit range on a 'Joint': radians on a hinge
(revolute) joint, or meters on a prismatic joint. Setting this also
enables the limit. Setting it on any other joint kind, or on an
entity with no 'Joint', is a silent no-op.
-}
data JointLimits = JointLimits !Float !Float
  deriving (Eq, Show)

instance Component JointLimits where
  type Storage JointLimits = B3Space JointLimits

instance (MonadIO m, Has w m Physics) => Has w m JointLimits where
  getStore = cast <$> (getStore :: SystemT w m (B3Space Physics))

instance (MonadIO m) => ExplGet m (B3Space JointLimits) where
  explExists sp ety = liftIO $ jointIsKind sp ety [B3T.RevoluteJoint, B3T.PrismaticJoint]
  explGet sp ety = liftIO $ withJoint sp ety $ \j -> do
    ty <- B3Joint.getType j
    case ty of
      B3T.PrismaticJoint -> JointLimits <$> B3PrismaticJoint.getLowerLimit j <*> B3PrismaticJoint.getUpperLimit j
      _ -> JointLimits <$> B3RevoluteJoint.getLowerLimit j <*> B3RevoluteJoint.getUpperLimit j

instance (MonadIO m) => ExplSet m (B3Space JointLimits) where
  explSet sp ety (JointLimits lo hi) = liftIO $
    overJoint sp ety $ \j -> do
      ty <- B3Joint.getType j
      case ty of
        B3T.RevoluteJoint -> B3RevoluteJoint.enableLimit j True >> B3RevoluteJoint.setLimits j lo hi
        B3T.PrismaticJoint -> B3PrismaticJoint.enableLimit j True >> B3PrismaticJoint.setLimits j lo hi
        _ -> pure ()

instance (MonadIO m) => ExplMembers m (B3Space JointLimits) where
  explMembers = jointMembers

{- | Whether the two bodies connected by a 'Joint' can collide with each
other. Applies to every joint kind. Restores the parity apecs-physics
has through @CollideBodies@. Setting it on an entity with no 'Joint'
is a silent no-op.
-}
newtype CollideConnected = CollideConnected Bool
  deriving (Eq, Show)

instance Component CollideConnected where
  type Storage CollideConnected = B3Space CollideConnected

instance (MonadIO m, Has w m Physics) => Has w m CollideConnected where
  getStore = cast <$> (getStore :: SystemT w m (B3Space Physics))

instance (MonadIO m) => ExplGet m (B3Space CollideConnected) where
  explExists = jointExists
  explGet sp ety = liftIO $ withJoint sp ety $ fmap CollideConnected . B3Joint.getCollideConnected

instance (MonadIO m) => ExplSet m (B3Space CollideConnected) where
  explSet sp ety (CollideConnected c) = liftIO $
    overJoint sp ety $
      \j -> B3Joint.setCollideConnected j c

instance (MonadIO m) => ExplMembers m (B3Space CollideConnected) where
  explMembers = jointMembers

{- | The constraint force a 'Joint' is exerting to hold, as of the last
'stepPhysics'. Applies to every joint kind; useful for breakage logic.
Read-only: Box3D computes it during the step.
-}
newtype JointForce = JointForce Vec3
  deriving (Eq, Show)

instance Component JointForce where
  type Storage JointForce = B3Space JointForce

instance (MonadIO m, Has w m Physics) => Has w m JointForce where
  getStore = cast <$> (getStore :: SystemT w m (B3Space Physics))

instance (MonadIO m) => ExplGet m (B3Space JointForce) where
  explExists = jointExists
  explGet sp ety = liftIO $ withJoint sp ety $ fmap JointForce . B3Joint.getConstraintForce

instance (MonadIO m) => ExplMembers m (B3Space JointForce) where
  explMembers = jointMembers

{- | The constraint torque a 'Joint' is exerting to hold, as of the last
'stepPhysics'. Unlike the 2D binding, Box3D's constraint torque is a
full 'Vec3' (rotation is 3-DOF here). Applies to every joint kind;
useful for breakage logic. Read-only: Box3D computes it during the
step.
-}
newtype JointTorque = JointTorque Vec3
  deriving (Eq, Show)

instance Component JointTorque where
  type Storage JointTorque = B3Space JointTorque

instance (MonadIO m, Has w m Physics) => Has w m JointTorque where
  getStore = cast <$> (getStore :: SystemT w m (B3Space Physics))

instance (MonadIO m) => ExplGet m (B3Space JointTorque) where
  explExists = jointExists
  explGet sp ety = liftIO $ withJoint sp ety $ fmap JointTorque . B3Joint.getConstraintTorque

instance (MonadIO m) => ExplMembers m (B3Space JointTorque) where
  explMembers = jointMembers

{- | The constraint force a 'Joint' must exceed, in Newtons, for the
engine to report it in 'JointEvents'. Applies to every joint kind.
Defaults to @FLT_MAX@ (effectively off) until set. The engine only
raises the event — it never destroys the joint itself; break it (or
lower the thresholds further) from your own systems after reading
'JointEvents'. A joint that stays overloaded across several steps
raises the event once per step it is exceeded in, not once overall.
-}
newtype JointForceThreshold = JointForceThreshold Float
  deriving (Eq, Show)

instance Component JointForceThreshold where
  type Storage JointForceThreshold = B3Space JointForceThreshold

instance (MonadIO m, Has w m Physics) => Has w m JointForceThreshold where
  getStore = cast <$> (getStore :: SystemT w m (B3Space Physics))

instance (MonadIO m) => ExplGet m (B3Space JointForceThreshold) where
  explExists = jointExists
  explGet sp ety = liftIO $ withJoint sp ety $ fmap JointForceThreshold . B3Joint.getForceThreshold

instance (MonadIO m) => ExplSet m (B3Space JointForceThreshold) where
  explSet sp ety (JointForceThreshold t) = liftIO $
    overJoint sp ety $
      \j -> B3Joint.setForceThreshold j t

instance (MonadIO m) => ExplMembers m (B3Space JointForceThreshold) where
  explMembers = jointMembers

{- | The constraint torque a 'Joint' must exceed, in Newton-meters, for
the engine to report it in 'JointEvents'. Applies to every joint kind.
Unlike 'JointTorque', the threshold itself is always a scalar magnitude
even though Box3D's constraint torque is a full 'Vec3'. Defaults to
@FLT_MAX@ (effectively off) until set. As with 'JointForceThreshold',
the engine only raises the event and leaves the joint intact.
-}
newtype JointTorqueThreshold = JointTorqueThreshold Float
  deriving (Eq, Show)

instance Component JointTorqueThreshold where
  type Storage JointTorqueThreshold = B3Space JointTorqueThreshold

instance (MonadIO m, Has w m Physics) => Has w m JointTorqueThreshold where
  getStore = cast <$> (getStore :: SystemT w m (B3Space Physics))

instance (MonadIO m) => ExplGet m (B3Space JointTorqueThreshold) where
  explExists = jointExists
  explGet sp ety = liftIO $ withJoint sp ety $ fmap JointTorqueThreshold . B3Joint.getTorqueThreshold

instance (MonadIO m) => ExplSet m (B3Space JointTorqueThreshold) where
  explSet sp ety (JointTorqueThreshold t) = liftIO $
    overJoint sp ety $
      \j -> B3Joint.setTorqueThreshold j t

instance (MonadIO m) => ExplMembers m (B3Space JointTorqueThreshold) where
  explMembers = jointMembers

-- * Queries

{- | The closest shape a 'segmentQuery' found: the shape entity, the
body entity it hangs off, the world-space impact point and surface
normal, and the fraction along the segment (0 at the start, 1 at the
end).
-}
data RayHit = RayHit
  { rayHitShape :: !Entity
  , rayHitBody :: !Entity
  , rayHitPoint :: !WVec
  , rayHitNormal :: !WVec
  , rayHitFraction :: !Float
  }
  deriving (Eq, Show)

{- | Queries match a 'Filter''s category and mask bits against shape
filters (see 'CollisionFilter'); 'filterGroupIndex' does not apply.
@Filter maxBound maxBound 0@ queries everything. Note Box3D's default
shape category is /all bits set/ (unlike Box2D's category 1), so any
query mask matches shapes with default filters — give shapes explicit
'CollisionFilter' categories to partition them for queries.
-}
toQueryFilter :: Filter -> IO B3T.QueryFilter
toQueryFilter f = do
  qf <- B3T.defaultQueryFilter
  pure
    qf
      { B3T.queryFilterCategoryBits = filterCategoryBits f
      , B3T.queryFilterMaskBits = filterMaskBits f
      }

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

{- | The closest shape along a world-space segment, if any. Initial
overlaps are ignored: a segment starting inside a shape does not hit
it.
-}
segmentQuery
  :: forall w m
   . (MonadIO m, Has w m Physics)
  => WVec
  -> WVec
  -> Filter
  -> SystemT w m (Maybe RayHit)
segmentQuery start end fltr = do
  sp :: B3Space Physics <- getStore
  liftIO $ do
    qf <- toQueryFilter fltr
    let
      Vec3 sx sy sz = start
      Vec3 ex ey ez = end
    res <- B3World.castRayClosest (spWorld sp) start (Vec3 (ex - sx) (ey - sy) (ez - sz)) qf
    if B3T.rayResultHit res == 0 then
      pure Nothing
    else
      fmap
        ( \(shapeEty, bodyEty) ->
            RayHit
              { rayHitShape = shapeEty
              , rayHitBody = bodyEty
              , rayHitPoint = B3T.rayResultPoint res
              , rayHitNormal = B3T.rayResultNormal res
              , rayHitFraction = B3T.rayResultFraction res
              }
        )
        <$> shapeEntities sp (B3T.rayResultShapeId res)

{- | Every shape along a world-space segment, sorted nearest-first by
'rayHitFraction'. Filter semantics match 'segmentQuery'. Unlike
'segmentQuery', which goes through the engine's @b3World_CastRayClosest@
convenience path, this drives the general @b3World_CastRay@ callback
directly — and that path does /not/ ignore initial overlaps itself (the
"ignore initial overlap" behaviour lives in the closest-hit callback, which
skips fraction-0 hits before they reach the caller). So a segment starting
inside a shape here reports that shape too, with 'rayHitFraction' 0. Hits
whose shapes were destroyed since the last 'stepPhysics' are dropped, same
as 'segmentQuery'.
-}
segmentQueryAll
  :: forall w m
   . (MonadIO m, Has w m Physics)
  => WVec
  -> WVec
  -> Filter
  -> SystemT w m [RayHit]
segmentQueryAll start end fltr = do
  sp :: B3Space Physics <- getStore
  liftIO $ do
    qf <- toQueryFilter fltr
    found <- newIORef []
    let
      Vec3 sx sy sz = start
      Vec3 ex ey ez = end
      visit s point normal frac _matId _triangleIx _childIx = do
        hit <- shapeEntities sp s
        forM_ hit $ \(shapeEty, bodyEty) ->
          modifyIORef'
            found
            ( RayHit
                { rayHitShape = shapeEty
                , rayHitBody = bodyEty
                , rayHitPoint = point
                , rayHitNormal = normal
                , rayHitFraction = frac
                }
                :
            )
        pure 1
    _ <- withCastResultFcn visit $ \fp ctx ->
      B3World.castRay (spWorld sp) start (Vec3 (ex - sx) (ey - sy) (ez - sz)) qf fp ctx
    sortOn rayHitFraction <$> readIORef found

{- | The body entities whose shapes' broad-phase bounding boxes overlap
the world-space box spanned by two corners (any order). Broad-phase:
the test is against shape AABBs, not exact geometry.
-}
aabbQuery
  :: forall w m
   . (MonadIO m, Has w m Physics)
  => WVec
  -> WVec
  -> Filter
  -> SystemT w m [Entity]
aabbQuery cornerA cornerB fltr = do
  sp :: B3Space Physics <- getStore
  liftIO $ do
    qf <- toQueryFilter fltr
    found <- newIORef IS.empty
    let visit s = do
          hit <- shapeEntities sp s
          forM_ hit $ \(_, Entity bodyIx) -> modifyIORef' found (IS.insert bodyIx)
          pure True
    _ <- withOverlapResultFcn visit $ \fp ctx ->
      B3World.overlapAABB (spWorld sp) box qf fp ctx
    map Entity . IS.toList <$> readIORef found
  where
    Vec3 ax ay az = cornerA
    Vec3 bx by bz = cornerB
    box = AABB (Vec3 (min ax bx) (min ay by) (min az bz)) (Vec3 (max ax bx) (max ay by) (max az bz))

{- | 'aabbQuery' of the cube reaching @r@ along each axis from a point:
the body entities with shapes broad-phase within reach. This is
broad-phase AABB reach, /not/ exact containment — a shape's AABB is
larger than the shape itself, so this can return bodies whose shape
doesn't actually contain the point. See 'containsPointQuery' for the
exact test.
-}
pointQuery :: (MonadIO m, Has w m Physics) => WVec -> Float -> Filter -> SystemT w m [Entity]
pointQuery (Vec3 x y z) r =
  aabbQuery (Vec3 (x - r) (y - r) (z - r)) (Vec3 (x + r) (y + r) (z + r))

{- | The body entities with a shape that actually contains a world point:
an exact geometry test, unlike the broad-phase 'pointQuery'. Candidates
come from a broad-phase 'B3World.overlapAABB' at a degenerate (zero-size)
AABB pinned to the point, then refined against each candidate's exact
geometry; bodies are deduplicated when more than one of their shapes
contains the point.

Box3D has no direct point-in-shape test, so the refinement step uses
'B3Shape.getClosestPoint' (the nearest point on a shape to a target)
instead: it runs a GJK query between the shape and the target treated as
a degenerate point shape, and for a target that is inside or on the shape
the query simplex encloses the origin, at which point the returned
\"closest\" point is a witness point that, worked out in exact arithmetic,
coincides with the target itself (checked against the
@b3Shape_GetClosestPoint@\/@b3ShapeDistance@ source: the target is the
only point in its proxy, so every barycentric blend of it is the target
unchanged, and the overlap branch solves for the target and the witness
point differing by exactly the origin). In floating point the simplex
weights can still land the witness point a small distance off, so
containment is decided by comparing squared distance against a small
scale-relative tolerance rather than requiring bit-exact equality.
-}
containsPointQuery
  :: forall w m
   . (MonadIO m, Has w m Physics)
  => WVec
  -> Filter
  -> SystemT w m [Entity]
containsPointQuery point@(Vec3 px py pz) fltr = do
  sp :: B3Space Physics <- getStore
  liftIO $ do
    qf <- toQueryFilter fltr
    found <- newIORef IS.empty
    let
      -- scale-relative tolerance: float rounding in the GJK simplex
      -- weights is roughly proportional to the magnitude of the
      -- coordinates involved, so a fixed epsilon would be too tight far
      -- from the origin and too loose near it
      tolerance = 1e-9 * (1 + px * px + py * py + pz * pz)
      visit s = do
        hit <- shapeEntities sp s
        forM_ hit $ \(_, Entity bodyIx) -> do
          Vec3 cx cy cz <- B3Shape.getClosestPoint s point
          let
            dx = cx - px
            dy = cy - py
            dz = cz - pz
            distSq = dx * dx + dy * dy + dz * dz
          when (distSq <= tolerance) $ modifyIORef' found (IS.insert bodyIx)
        pure True
    _ <- withOverlapResultFcn visit $ \fp ctx ->
      B3World.overlapAABB (spWorld sp) (AABB point point) qf fp ctx
    map Entity . IS.toList <$> readIORef found

-- * Character mover

{- | One collision plane gathered by 'moveCharacter' for a single mover
step, ported from @mover.c@'s @b3CollisionPlane@: the geometric plane
plus the solver's per-plane push accumulator. Not exported — the solver
resets 'mpPush' to 0 at the start of every 'solveMoverPlanes' call, so a
stale accumulator from a previous step is never visible.
-}
data MoverPlane = MoverPlane
  { mpPlane :: !Plane
  , mpPushLimit :: !Float
  , mpPush :: !Float
  , mpClipVelocity :: !Bool
  }

{- | @mover.c@'s @b3PlaneSeparation@: signed distance of a point from a
plane, using the engine's @dot(normal, point) - offset@ convention (see
'Plane').
-}
planeSeparation :: Plane -> Vec3 -> Float
planeSeparation (Plane n o) p = vecDot n p - o

{- | @B3_LINEAR_SLOP@ at the engine's default length-units-per-meter (1),
same value as Box2D's slop. Box3D lets a world rescale its length
units, which would rescale this too, but that isn't exposed to query;
this layer assumes the default.
-}
linearSlop :: Float
linearSlop = 0.005

-- | @sample.cpp@'s @CharacterMover::m_planeCapacity@: at most this many planes are kept per step. Same value as the 2D sample.
planeCapacity :: Int
planeCapacity = 8

-- | @sample.cpp@'s @Player::Update@ outer collide\/solve\/cast loop count — identical to the 2D sample.
moverStepIterations :: Int
moverStepIterations = 5

-- | @sample.cpp@'s per-iteration break tolerance on the swept translation — identical to the 2D sample.
moverStepTolerance :: Float
moverStepTolerance = 0.01

-- | @b3SolvePlanes@'s inner accumulated-push iteration cap — identical to the 2D @b2SolvePlanes@.
solverIterations :: Int
solverIterations = 20

vecAdd :: Vec3 -> Vec3 -> Vec3
vecAdd (Vec3 ax ay az) (Vec3 bx by bz) = Vec3 (ax + bx) (ay + by) (az + bz)

vecSub :: Vec3 -> Vec3 -> Vec3
vecSub (Vec3 ax ay az) (Vec3 bx by bz) = Vec3 (ax - bx) (ay - by) (az - bz)

vecScale :: Float -> Vec3 -> Vec3
vecScale s (Vec3 x y z) = Vec3 (s * x) (s * y) (s * z)

vecDot :: Vec3 -> Vec3 -> Float
vecDot (Vec3 ax ay az) (Vec3 bx by bz) = ax * bx + ay * by + az * bz

vecLenSq :: Vec3 -> Float
vecLenSq v = vecDot v v

-- | @b3MulAdd@: @a + s*b@.
vecMulAdd :: Vec3 -> Float -> Vec3 -> Vec3
vecMulAdd a s b = vecAdd a (vecScale s b)

-- | @b3MulSub@: @a - s*b@.
vecMulSub :: Vec3 -> Float -> Vec3 -> Vec3
vecMulSub a s b = vecSub a (vecScale s b)

clampFloat :: Float -> Float -> Float -> Float
clampFloat x lo hi = max lo (min hi x)

-- | A 'B3T.PlaneResult' as a fresh 'MoverPlane': no push limit (the sample's per-shape @maxPush@ user data isn't exposed here) and velocity always clipped.
mkMoverPlane :: B3T.PlaneResult -> MoverPlane
mkMoverPlane pr =
  MoverPlane
    { mpPlane = B3T.planeResultPlane pr
    , mpPushLimit = 1 / 0
    , mpPush = 0
    , mpClipVelocity = True
    }

-- | One sweep of @b3SolvePlanes@'s inner loop over every plane, threading the resolved delta and each plane's updated push accumulator.
solvePlanesStep :: Vec3 -> [MoverPlane] -> (Vec3, [MoverPlane], Float)
solvePlanesStep = go [] 0
  where
    go acc total delta [] = (delta, reverse acc, total)
    go acc total delta (p : ps) =
      let
        -- Add slop to prevent jitter.
        separation = planeSeparation (mpPlane p) delta + linearSlop
        push = negate separation
        accumulated = mpPush p
        newPush = clampFloat (accumulated + push) 0 (mpPushLimit p)
        pushDelta = newPush - accumulated
        Plane n _ = mpPlane p
        delta' = vecMulAdd delta pushDelta n
      in
        go (p{mpPush = newPush} : acc) (total + abs pushDelta) delta' ps

{- | Port of @mover.c@'s @b3SolvePlanes@: resolve a desired translation
against a set of collision planes by accumulating a clamped push along
each plane's normal, iterated until the total push converges below
'linearSlop' or 'solverIterations' is reached. Returns the resolved
translation and the planes with their final push accumulators (used
afterwards by 'clipMoverVector'). Identical in structure to the 2D
@b2SolvePlanes@ port — same iteration count, same slop tolerance.
-}
solveMoverPlanes :: Vec3 -> [MoverPlane] -> (Vec3, [MoverPlane])
solveMoverPlanes targetDelta planes0 = go (0 :: Int) [p{mpPush = 0} | p <- planes0] targetDelta
  where
    go iteration planes delta
      | iteration >= solverIterations = (delta, planes)
      | otherwise =
          let (delta', planes', totalPush) = solvePlanesStep delta planes
          in if totalPush < linearSlop then
               (delta', planes')
             else
               go (iteration + 1) planes' delta'

{- | Port of @mover.c@'s @b3ClipVector@: kill the into-the-plane component
of a vector for every plane that pushed (nonzero 'mpPush') and asked for
clipping ('mpClipVelocity'), leaving components along or away from the
plane untouched.
-}
clipMoverVector :: Vec3 -> [MoverPlane] -> Vec3
clipMoverVector = foldl' step
  where
    step v p
      | mpPush p == 0 || not (mpClipVelocity p) = v
      | otherwise =
          let Plane n _ = mpPlane p
          in vecMulSub v (min 0 (vecDot v n)) n

{- | What 'moveCharacter' produced: where the mover ended up and its
velocity clipped against every surface it touched (kill the
into-the-wall component so speed doesn't build up against obstacles).
-}
data MoverResult = MoverResult
  { moverPosition :: !WVec
  , moverVelocity :: !WVec
  }
  deriving (Eq, Show)

{- | Move a character capsule from its current position toward a target,
sliding along whatever it hits — the engine-blessed kinematic character
controller (collide → solve planes → sweep, iterated). The capsule is
given in local space like 'GeoCapsule' (two centers and a radius) and
does not need any 'Body' or 'Shape' — the mover is pure query, it does
not push bodies around. Pass the current velocity to get it clipped
against the surfaces touched this step; integrate gravity/input into it
yourself before calling. Filter semantics match the other queries (see
'toQueryFilter').

Mirrors @samples/sample.cpp@'s @Player@ character controller faithfully
— its collide\/solve\/cast loop is structurally identical to the 2D
@sample_character.cpp@ @Mover@, with the same constants (5 outer
iterations breaking below a 0.01-unit translation, up to 8 planes per
step, a 20-iteration inner solver): each iteration gathers fresh
collision planes via 'B3World.collideMover', resolves the target delta
against them with 'solveMoverPlanes' (a direct port of @mover.c@'s
@b3SolvePlanes@), and sweeps the resolved translation with
'B3World.castMover'. The final velocity is clipped ('clipMoverVector'\/
@b3ClipVector@) against the planes gathered in whichever iteration ran
last — same as the sample, which never clears its plane buffer after the
loop exits. Every plane is treated as unlimited push with clipping on;
the sample's per-shape @maxPush@\/@clipVelocity@ come from shape user
data, which this layer doesn't expose.

'B3World.castMover' takes an optional per-shape mover filter callback;
this layer passes none (a null function pointer, which the engine
tolerates — see @b3World_CastMover@'s @fcn@ parameter), so per-shape
mover-cast filtering beyond 'Filter' bits isn't available through this
function.
-}
moveCharacter
  :: forall w m
   . (MonadIO m, Has w m Physics)
  => BVec
  -- ^ mover capsule center 1, local
  -> BVec
  -- ^ mover capsule center 2, local
  -> Float
  -- ^ mover capsule radius
  -> WVec
  -- ^ current position (world origin of the capsule frame)
  -> WVec
  -- ^ target position for this step
  -> WVec
  -- ^ current velocity
  -> Filter
  -> SystemT w m MoverResult
moveCharacter c1 c2 radius pos0 target vel0 fltr = do
  sp :: B3Space Physics <- getStore
  liftIO $ do
    qf <- toQueryFilter fltr
    let
      capsule = B3T.Capsule c1 c2 radius

      gatherPlanes pos = do
        planesRef <- newIORef []
        let visit _shapeId prs = do
              modifyIORef' planesRef $ \ps ->
                foldl'
                  (\acc pr -> if length acc >= planeCapacity then acc else mkMoverPlane pr : acc)
                  ps
                  (VS.toList prs)
              pure True
        _ <-
          withPlaneResultFcn visit $ \fp ctx ->
            B3World.collideMover (spWorld sp) pos capsule qf fp ctx
        reverse <$> readIORef planesRef

      step i pos lastPlanes
        | i >= moverStepIterations = pure (pos, lastPlanes)
        | otherwise = do
            planes <- gatherPlanes pos
            let (translation, planes') = solveMoverPlanes (vecSub target pos) planes
            fraction <- B3World.castMover (spWorld sp) pos capsule translation qf nullFunPtr nullPtr
            let
              delta = vecScale fraction translation
              pos' = vecAdd pos delta
            if vecLenSq delta < moverStepTolerance * moverStepTolerance then
              pure (pos', planes')
            else
              step (i + 1) pos' planes'

    (finalPos, finalPlanes) <- step (0 :: Int) pos0 []
    pure (MoverResult finalPos (clipMoverVector vel0 finalPlanes))

-- * Collisions

{- | A contact that began touching during the last 'stepPhysics': the
shapes involved and the bodies they hang off.
-}
data Collision = Collision
  { collisionBodyA :: !Entity
  , collisionShapeA :: !Entity
  , collisionBodyB :: !Entity
  , collisionShapeB :: !Entity
  }
  deriving (Eq, Show)

-- | The shape/body entities behind a contact's two shape ids, if both are still alive and registered.
toCollision :: B3Space c -> ShapeId -> ShapeId -> IO (Maybe Collision)
toCollision sp sA sB = do
  ma <- shapeEntities sp sA
  mb <- shapeEntities sp sB
  pure $ do
    (sa, ba) <- ma
    (sb, bb) <- mb
    Just (Collision ba sa bb sb)

{- | The begin-touch contacts of the last 'stepPhysics', a read-only
global: @Collisions touches <- get global@ after stepping. Shapes
created by this layer opt into contact events; events whose shapes
were destroyed since the step are dropped.
-}
newtype Collisions = Collisions [Collision]
  deriving (Show)

instance Component Collisions where
  type Storage Collisions = B3Space Collisions

instance (MonadIO m, Has w m Physics) => Has w m Collisions where
  getStore = cast <$> (getStore :: SystemT w m (B3Space Physics))

instance (MonadIO m) => ExplGet m (B3Space Collisions) where
  explExists _ _ = pure True
  explGet sp _ = liftIO $ do
    evs <- B3Events.contactBeginTouchEvents (spWorld sp)
    fmap (Collisions . catMaybes) . forM (VS.toList evs) $ \ev ->
      toCollision sp (B3T.contactBeginTouchEventShapeIdA ev) (B3T.contactBeginTouchEventShapeIdB ev)

{- | The end-touch contacts of the last 'stepPhysics', a read-only
global: @CollisionsEnd separations <- get global@ after stepping — the
counterpart of 'Collisions' for contacts that stopped touching. Events
whose shapes were destroyed since the step are dropped; this bites
harder here than for begin-touch, since destroying a shape mid-contact
drops its end event — clean up any per-contact bookkeeping when
destroying shapes.
-}
newtype CollisionsEnd = CollisionsEnd [Collision]
  deriving (Show)

instance Component CollisionsEnd where
  type Storage CollisionsEnd = B3Space CollisionsEnd

instance (MonadIO m, Has w m Physics) => Has w m CollisionsEnd where
  getStore = cast <$> (getStore :: SystemT w m (B3Space Physics))

instance (MonadIO m) => ExplGet m (B3Space CollisionsEnd) where
  explExists _ _ = pure True
  explGet sp _ = liftIO $ do
    evs <- B3Events.contactEndTouchEvents (spWorld sp)
    fmap (CollisionsEnd . catMaybes) . forM (VS.toList evs) $ \ev ->
      toCollision sp (B3T.contactEndTouchEventShapeIdA ev) (B3T.contactEndTouchEventShapeIdB ev)

{- | An above-threshold impact from the last 'stepPhysics': the entities
involved, the world-space contact point, the contact normal (pointing
from A to B) and the approach speed. Only generated when the approach
speed exceeds the world's hit-event threshold (engine default 1;
tune with 'HitEventThreshold').
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

{- | The impacts of the last 'stepPhysics', a read-only global:
@Impacts hits <- get global@ after stepping.
-}
newtype Impacts = Impacts [Impact]
  deriving (Show)

instance Component Impacts where
  type Storage Impacts = B3Space Impacts

instance (MonadIO m, Has w m Physics) => Has w m Impacts where
  getStore = cast <$> (getStore :: SystemT w m (B3Space Physics))

instance (MonadIO m) => ExplGet m (B3Space Impacts) where
  explExists _ _ = pure True
  explGet sp _ = liftIO $ do
    evs <- B3Events.contactHitEvents (spWorld sp)
    fmap (Impacts . catMaybes) . forM (VS.toList evs) $ \ev -> do
      ma <- shapeEntities sp (B3T.contactHitEventShapeIdA ev)
      mb <- shapeEntities sp (B3T.contactHitEventShapeIdB ev)
      pure $ do
        (sa, ba) <- ma
        (sb, bb) <- mb
        Just
          Impact
            { impactBodyA = ba
            , impactShapeA = sa
            , impactBodyB = bb
            , impactShapeB = sb
            , impactPoint = B3T.contactHitEventPoint ev
            , impactNormal = B3T.contactHitEventNormal ev
            , impactSpeed = B3T.contactHitEventApproachSpeed ev
            }

{- | A sensor overlap that began or ended during the last 'stepPhysics':
the 'Sensor' shape (and the body it hangs off) and the visitor shape
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
toSensorEvent :: B3Space c -> ShapeId -> ShapeId -> IO (Maybe SensorEvent)
toSensorEvent sp sensorS visitorS = do
  ms <- shapeEntities sp sensorS
  mv <- shapeEntities sp visitorS
  pure $ do
    (sShape, sBody) <- ms
    (vShape, vBody) <- mv
    Just (SensorEvent sBody sShape vBody vShape)

{- | The sensor overlaps that began and ended during the last
'stepPhysics', a read-only global: @SensorEvents begins ends <- get
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
  type Storage SensorEvents = B3Space SensorEvents

instance (MonadIO m, Has w m Physics) => Has w m SensorEvents where
  getStore = cast <$> (getStore :: SystemT w m (B3Space Physics))

instance (MonadIO m) => ExplGet m (B3Space SensorEvents) where
  explExists _ _ = pure True
  explGet sp _ = liftIO $ do
    begins <- B3Events.sensorBeginTouchEvents (spWorld sp)
    ends <- B3Events.sensorEndTouchEvents (spWorld sp)
    beginEvs <-
      fmap catMaybes . forM (VS.toList begins) $ \ev ->
        toSensorEvent sp (B3T.sensorBeginTouchEventSensorShapeId ev) (B3T.sensorBeginTouchEventVisitorShapeId ev)
    endEvs <-
      fmap catMaybes . forM (VS.toList ends) $ \ev ->
        toSensorEvent sp (B3T.sensorEndTouchEventSensorShapeId ev) (B3T.sensorEndTouchEventVisitorShapeId ev)
    pure (SensorEvents beginEvs endEvs)

{- | The joints whose force or torque threshold ('JointForceThreshold',
'JointTorqueThreshold') was exceeded during the last 'stepPhysics', a
read-only global: @JointEvents overloaded <- get global@ after
stepping. The engine leaves the joint intact — destroy the entity's
'Joint' (or lower the thresholds) yourself if it should break. Events
whose joints were destroyed since the step are dropped.
-}
newtype JointEvents = JointEvents [Entity]
  deriving (Show)

instance Component JointEvents where
  type Storage JointEvents = B3Space JointEvents

instance (MonadIO m, Has w m Physics) => Has w m JointEvents where
  getStore = cast <$> (getStore :: SystemT w m (B3Space Physics))

instance (MonadIO m) => ExplGet m (B3Space JointEvents) where
  explExists _ _ = pure True
  explGet sp _ = liftIO $ do
    evs <- B3Events.jointEvents (spWorld sp)
    fmap (JointEvents . catMaybes) . forM (VS.toList evs) $ \ev ->
      jointEntity sp (B3T.jointEventJointId ev)
