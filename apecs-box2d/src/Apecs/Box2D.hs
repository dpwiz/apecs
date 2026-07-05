{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-| Apecs glue for the Box2D physics engine, modelled on apecs-physics.

Add 'Physics' to your world to get a Box2D world. Giving an entity a
'Body' component creates an engine body and unlocks its sub-components
('Position', 'Velocity', 'Angle', ...), which read and write the engine
directly instead of mirroring state into apecs stores. Shapes hang off
a body entity through the 'Shape' component, like in apecs-physics.
As in apecs-physics, setting a sub-component on an entity that has no
'Body' (or 'Shape') is a silent no-op.

Deviations from apecs-physics: vectors are Box2D's native
single-precision 'Vec2' (convert to your vector library of choice at
the boundary); 'Elasticity' is Box2D restitution; 'Substeps' replaces
@Iterations@.

The raw engine is reachable through 'B2BodyId', 'B2ShapeId' and
'getWorldId' together with the "Box2D" modules. The wrapper owns
the engine's user-index channel: it stamps every body, shape and
joint with its entity id and resolves events and queries through
it, so raw-API users must not call @setUserIndex@ on
wrapper-created objects — and objects created directly through the
raw API are invisible to the wrapper's components, events and
queries.
-}
module Apecs.Box2D
  ( -- * World
    Physics
  , B2Space
  , Gravity (..)
  , earthGravity
  , Substeps (..)
  , SleepingEnabled (..)
  , ContinuousEnabled (..)
  , HitEventThreshold (..)
  , RestitutionThreshold (..)
  , MaximumLinearSpeed (..)
  , WorkerCount (..)
  , stepPhysics
  , destroyPhysics
  , explode
  , getWorldId

    -- * Body
  , Body (..)
  , Position (..)
  , Velocity (..)
  , Angle (..)
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
  , FixedRotation (..)
  , SleepEnabled (..)
  , SleepThreshold (..)
  , CenterOfMass (..)
  , RotationalInertia (..)
  , BodyName (..)
  , B2BodyId (..)

    -- * Shape
  , Geometry (..)
  , Shape (..)
  , Density (..)
  , Friction (..)
  , Elasticity (..)
  , CollisionFilter (..)
  , Sensor (..)
  , Filter (..)
  , B2ShapeId (..)
  , Chain (..)
  , B2ChainId (..)

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
  , B2JointId (..)

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

    -- * Recording
  , Recording
  , newRecording
  , destroyRecording
  , startRecording
  , stopRecording
  , saveRecording
  , loadRecording
  , validateRecording

    -- * Snapshot
  , Snapshot
  , snapshotWorld
  , restoreWorld

    -- * Collisions
  , Collision (..)
  , Collisions (..)
  , CollisionsEnd (..)
  , Impact (..)
  , Impacts (..)
  , SensorEvent (..)
  , SensorEvents (..)
  , JointEvents (..)
  , BodyMove (..)
  , Moved (..)

    -- * Vectors
  , Vec2 (..)
  , vec2Zero
  , BVec
  , WVec
  ) where

import Apecs
import Apecs.Core
import Control.Monad (filterM, forM, forM_, when)
import Control.Monad.IO.Class (MonadIO)
import Data.IORef
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IM
import Data.IntSet qualified as IS
import Data.List (sortOn)
import Data.Maybe (catMaybes)
import Data.Vector.Storable qualified as VS
import Data.Vector.Unboxed qualified as U
import Data.Word (Word8)
import Foreign.C.String (peekCString, withCString)
import Foreign.ForeignPtr (ForeignPtr, mallocForeignPtrBytes, withForeignPtr)
import Foreign.Marshal.Utils (fromBool, toBool)
import Foreign.Ptr (Ptr, castPtr, nullPtr)

import Box2D.Body qualified as B2Body
import Box2D.Callbacks (withCastResultFcn, withOverlapResultFcn, withPlaneResultFcn)
import Box2D.Chain qualified as B2Chain
import Box2D.Collision qualified as B2Collision
import Box2D.DistanceJoint qualified as B2DistanceJoint
import Box2D.Events qualified as B2Events
import Box2D.Id (BodyId, ChainId, JointId, ShapeId, WorldId)
import Box2D.Joint qualified as B2Joint
import Box2D.MathFunctions (makeRot, rotGetAngle)
import Box2D.MathTypes (AABB (..), Rot (..), Transform (..), Vec2 (..), vec2Zero)
import Box2D.MotorJoint qualified as B2MotorJoint
import Box2D.Mover qualified as B2Mover
import Box2D.PrismaticJoint qualified as B2PrismaticJoint
import Box2D.Recording qualified as B2Recording
import Box2D.RevoluteJoint qualified as B2RevoluteJoint
import Box2D.Shape qualified as B2Shape
import Box2D.Tags qualified as B2Tags
import Box2D.Types (Filter (..))
import Box2D.Types qualified as B2T
import Box2D.UserData (getUserIndex, setUserIndex)
import Box2D.WeldJoint qualified as B2WeldJoint
import Box2D.WheelJoint qualified as B2WheelJoint
import Box2D.World qualified as B2World

-- | A vector in body-space coordinates.
type BVec = Vec2

-- | A vector in world-space coordinates.
type WVec = Vec2

-- | Uninhabited component; add it to your world to get a physics space.
data Physics

-- | The engine shape plus the exact 'Shape' value that created it.
data ShapeRecord = ShapeRecord !ShapeId !Shape

-- | The engine joint plus the exact 'Joint' value that created it.
data JointRecord = JointRecord !JointId !Joint

-- | The engine chain plus the exact 'Chain' value that created it.
data ChainRecord = ChainRecord !ChainId !Chain

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
      -- layer-created shape in so 'Collisions', 'Impacts' and
      -- 'SensorEvents' have something to read (a shape both generates
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

-- | The raw Box2D world, for use with the "Box2D" modules directly.
getWorldId :: forall w m. (MonadIO m, Has w m Physics) => SystemT w m WorldId
getWorldId = spWorld <$> (getStore :: SystemT w m (B2Space Physics))

{- | Advance the simulation by a time delta, resolving contacts with the
'Substeps' number of substeps.
-}
stepPhysics :: forall w m. (MonadIO m, Has w m Physics) => Float -> SystemT w m ()
stepPhysics dT = do
  sp :: B2Space Physics <- getStore
  liftIO $ do
    substeps <- readIORef (spSubsteps sp)
    B2World.step (spWorld sp) dT substeps

{- | Destroy the engine world along with all its bodies and shapes, and
clear the registries. The store is unusable afterwards; call this on
teardown. Box2D keeps worlds in a fixed-size global registry, so
sessions that repeatedly create worlds (test suites, GHCi reloads) must
destroy them too or world creation eventually fails.
-}
destroyPhysics :: forall w m. (MonadIO m, Has w m Physics) => SystemT w m ()
destroyPhysics = do
  sp :: B2Space Physics <- getStore
  liftIO $ do
    B2World.destroy (spWorld sp)
    writeIORef (spBodies sp) mempty
    writeIORef (spShapes sp) mempty
    writeIORef (spJoints sp) mempty
    writeIORef (spChains sp) mempty

{- | Apply a radial impulse to every dynamic body within a radius of a
world point, as if from an explosion: each affected shape is pushed
away from the center along the line to its nearest surface point,
scaled by how much of its perimeter faces the blast. Only circles,
capsules and polygons receive an impulse (segments do not); a body is
woken even if it was asleep. The impulse has no soft falloff by
default, so it cuts off sharply at the radius, and every shape passes
the default filter (nothing is masked out). A negative impulse pulls
bodies inward instead of pushing them.
-}
explode
  :: forall w m
   . (MonadIO m, Has w m Physics)
  => WVec
  -- ^ Explosion center, in world coordinates.
  -> Float
  -- ^ Radius: shapes within this distance get the full impulse.
  -> Float
  {- ^ Impulse per unit length of shape perimeter facing the blast;
  negative for an implosion.
  -}
  -> SystemT w m ()
explode center radius impulse = do
  sp :: B2Space Physics <- getStore
  liftIO $ do
    def <- B2T.defaultExplosionDef
    B2World.explode
      (spWorld sp)
      def
        { B2T.explosionDefPosition = center
        , B2T.explosionDefRadius = radius
        , B2T.explosionDefImpulsePerLength = impulse
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
withChain sp ety f = withReg "Chain" (spChains sp) ety (\(ChainRecord c _) -> f c)

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

-- Space sub-components ----------------------------------------------------

-- | The world's gravity vector.
newtype Gravity = Gravity WVec
  deriving (Eq, Show)

earthGravity :: Gravity
earthGravity = Gravity (Vec2 0 (-9.81))

instance Component Gravity where
  type Storage Gravity = B2Space Gravity

instance (MonadIO m, Has w m Physics) => Has w m Gravity where
  getStore = cast <$> (getStore :: SystemT w m (B2Space Physics))

instance (MonadIO m) => ExplGet m (B2Space Gravity) where
  explExists _ _ = pure True
  explGet sp _ = liftIO $ Gravity <$> B2World.getGravity (spWorld sp)

instance (MonadIO m) => ExplSet m (B2Space Gravity) where
  explSet sp _ (Gravity v) = liftIO $ B2World.setGravity (spWorld sp) v

{- | The number of contact substeps per 'stepPhysics' call (the analog of
apecs-physics @Iterations@). Defaults to 4; clamped to at least 1.
-}
newtype Substeps = Substeps Int
  deriving (Eq, Show)

instance Component Substeps where
  type Storage Substeps = B2Space Substeps

instance (MonadIO m, Has w m Physics) => Has w m Substeps where
  getStore = cast <$> (getStore :: SystemT w m (B2Space Physics))

instance (MonadIO m) => ExplGet m (B2Space Substeps) where
  explExists _ _ = pure True
  explGet sp _ = liftIO $ Substeps <$> readIORef (spSubsteps sp)

instance (MonadIO m) => ExplSet m (B2Space Substeps) where
  explSet sp _ (Substeps n) = liftIO $ writeIORef (spSubsteps sp) (max 1 n)

{- | Whether bodies in this world may fall asleep at all (on by
default). Disabling it wakes everything and saves the bookkeeping when
nothing would sleep anyway; sleeping gains performance on large scenes
where most bodies are at rest. Per-body control is 'SleepEnabled'.
-}
newtype SleepingEnabled = SleepingEnabled Bool
  deriving (Eq, Show)

instance Component SleepingEnabled where
  type Storage SleepingEnabled = B2Space SleepingEnabled

instance (MonadIO m, Has w m Physics) => Has w m SleepingEnabled where
  getStore = cast <$> (getStore :: SystemT w m (B2Space Physics))

instance (MonadIO m) => ExplGet m (B2Space SleepingEnabled) where
  explExists _ _ = pure True
  explGet sp _ = liftIO $ SleepingEnabled <$> B2World.isSleepingEnabled (spWorld sp)

instance (MonadIO m) => ExplSet m (B2Space SleepingEnabled) where
  explSet sp _ (SleepingEnabled e) = liftIO $ B2World.enableSleeping (spWorld sp) e

{- | Whether continuous collision detection runs between fast dynamic
bodies and static geometry, keeping them from tunnelling through walls
between substeps (on by default; disabling it is a minor performance
gain). Continuous detection between two dynamic bodies is a separate,
per-body opt-in: see 'BulletBody'.
-}
newtype ContinuousEnabled = ContinuousEnabled Bool
  deriving (Eq, Show)

instance Component ContinuousEnabled where
  type Storage ContinuousEnabled = B2Space ContinuousEnabled

instance (MonadIO m, Has w m Physics) => Has w m ContinuousEnabled where
  getStore = cast <$> (getStore :: SystemT w m (B2Space Physics))

instance (MonadIO m) => ExplGet m (B2Space ContinuousEnabled) where
  explExists _ _ = pure True
  explGet sp _ = liftIO $ ContinuousEnabled <$> B2World.isContinuousEnabled (spWorld sp)

instance (MonadIO m) => ExplSet m (B2Space ContinuousEnabled) where
  explSet sp _ (ContinuousEnabled e) = liftIO $ B2World.enableContinuous (spWorld sp) e

{- | The approach speed above which a contact generates a hit event,
usually in meters per second (engine default 1). Read by 'Impacts',
which also needs hit events enabled per shape — on by default for every
shape this layer creates.
-}
newtype HitEventThreshold = HitEventThreshold Float
  deriving (Eq, Show)

instance Component HitEventThreshold where
  type Storage HitEventThreshold = B2Space HitEventThreshold

instance (MonadIO m, Has w m Physics) => Has w m HitEventThreshold where
  getStore = cast <$> (getStore :: SystemT w m (B2Space Physics))

instance (MonadIO m) => ExplGet m (B2Space HitEventThreshold) where
  explExists _ _ = pure True
  explGet sp _ = liftIO $ HitEventThreshold <$> B2World.getHitEventThreshold (spWorld sp)

instance (MonadIO m) => ExplSet m (B2Space HitEventThreshold) where
  explSet sp _ (HitEventThreshold t) = liftIO $ B2World.setHitEventThreshold (spWorld sp) t

{- | The relative approach speed below which a contact's 'Elasticity'
is ignored and it doesn't bounce, usually in meters per second. Don't
set this very low: contacts hovering just above the threshold keep
bouncing instead of settling, which prevents bodies from falling
asleep.
-}
newtype RestitutionThreshold = RestitutionThreshold Float
  deriving (Eq, Show)

instance Component RestitutionThreshold where
  type Storage RestitutionThreshold = B2Space RestitutionThreshold

instance (MonadIO m, Has w m Physics) => Has w m RestitutionThreshold where
  getStore = cast <$> (getStore :: SystemT w m (B2Space Physics))

instance (MonadIO m) => ExplGet m (B2Space RestitutionThreshold) where
  explExists _ _ = pure True
  explGet sp _ = liftIO $ RestitutionThreshold <$> B2World.getRestitutionThreshold (spWorld sp)

instance (MonadIO m) => ExplSet m (B2Space RestitutionThreshold) where
  explSet sp _ (RestitutionThreshold t) = liftIO $ B2World.setRestitutionThreshold (spWorld sp) t

{- | The speed cap applied to every 'Body' in this world, usually in
meters per second: velocities that would exceed it are clamped each
step. Guards against tunnelling and blow-ups from stray forces or
impulses.
-}
newtype MaximumLinearSpeed = MaximumLinearSpeed Float
  deriving (Eq, Show)

instance Component MaximumLinearSpeed where
  type Storage MaximumLinearSpeed = B2Space MaximumLinearSpeed

instance (MonadIO m, Has w m Physics) => Has w m MaximumLinearSpeed where
  getStore = cast <$> (getStore :: SystemT w m (B2Space Physics))

instance (MonadIO m) => ExplGet m (B2Space MaximumLinearSpeed) where
  explExists _ _ = pure True
  explGet sp _ = liftIO $ MaximumLinearSpeed <$> B2World.getMaximumLinearSpeed (spWorld sp)

instance (MonadIO m) => ExplSet m (B2Space MaximumLinearSpeed) where
  explSet sp _ (MaximumLinearSpeed s) = liftIO $ B2World.setMaximumLinearSpeed (spWorld sp) s

{- | The number of solver worker threads the world uses (default 1).
Raising it parallelises the solver across islands; it only pays off on
scenes with many independent islands, and the program must be built
with the threaded runtime. Settable at any time between 'stepPhysics'
calls. Must be in the range [1, B2_MAX_WORKERS].
-}
newtype WorkerCount = WorkerCount Int
  deriving (Eq, Show)

instance Component WorkerCount where
  type Storage WorkerCount = B2Space WorkerCount

instance (MonadIO m, Has w m Physics) => Has w m WorkerCount where
  getStore = cast <$> (getStore :: SystemT w m (B2Space Physics))

instance (MonadIO m) => ExplGet m (B2Space WorkerCount) where
  explExists _ _ = pure True
  explGet sp _ = liftIO $ WorkerCount <$> B2World.getWorkerCount (spWorld sp)

instance (MonadIO m) => ExplSet m (B2Space WorkerCount) where
  explSet sp _ (WorkerCount c) = liftIO $ B2World.setWorkerCount (spWorld sp) c

-- Body --------------------------------------------------------------------

{- | Gives an entity a Box2D body. Deleting it also deletes the shapes
attached to it. A body carries the sub-components 'Position',
'Velocity', 'Angle', 'AngularVelocity', 'BodyMass', 'Force' and
'Torque'; they exist as long as the entity has a @Body@, and setting
them on an entity without one does nothing.
-}
data Body = DynamicBody | KinematicBody | StaticBody
  deriving (Eq, Ord, Enum, Show)

toB2BodyType :: Body -> B2T.BodyType
toB2BodyType DynamicBody = B2T.DynamicBody
toB2BodyType KinematicBody = B2T.KinematicBody
toB2BodyType StaticBody = B2T.StaticBody

fromB2BodyType :: B2T.BodyType -> Body
fromB2BodyType ty = case ty of
  B2T.DynamicBody -> DynamicBody
  B2T.KinematicBody -> KinematicBody
  B2T.StaticBody -> StaticBody

instance Component Body where
  type Storage Body = B2Space Body

instance (MonadIO m, Has w m Physics) => Has w m Body where
  getStore = cast <$> (getStore :: SystemT w m (B2Space Physics))

instance (MonadIO m) => ExplSet m (B2Space Body) where
  explSet sp ety btype = liftIO $ do
    bodies <- readIORef (spBodies sp)
    case IM.lookup ety bodies of
      Just b -> B2Body.setType b (toB2BodyType btype)
      Nothing -> do
        b <- B2Body.create (spWorld sp) (spBodyDef sp){B2T.bodyDefType = toB2BodyType btype}
        setUserIndex b ety
        modifyIORef' (spBodies sp) (IM.insert ety b)

instance (MonadIO m) => ExplGet m (B2Space Body) where
  explExists = bodyExists
  explGet sp ety =
    liftIO $
      withBody sp ety $
        fmap fromB2BodyType . B2Body.getType

instance (MonadIO m) => ExplDestroy m (B2Space Body) where
  explDestroy sp ety = liftIO $ do
    bodies <- readIORef (spBodies sp)
    forM_ (IM.lookup ety bodies) $ \b -> do
      -- the engine destroys attached shapes, joints and chains along with
      -- the body, so drop their entity records too
      modifyIORef' (spShapes sp) (IM.filter (\(ShapeRecord _ (Shape (Entity be) _)) -> be /= ety))
      modifyIORef' (spJoints sp) (IM.filter (\(JointRecord _ (Joint (Entity a) (Entity b') _)) -> a /= ety && b' /= ety))
      modifyIORef' (spChains sp) (IM.filter (\(ChainRecord _ (Chain (Entity be) _ _)) -> be /= ety))
      modifyIORef' (spBodies sp) (IM.delete ety)
      B2Body.destroy b

instance (MonadIO m) => ExplMembers m (B2Space Body) where
  explMembers = bodyMembers

-- | The raw Box2D body of an entity, for use with "Box2D.Body" directly.
newtype B2BodyId = B2BodyId BodyId
  deriving (Eq, Show)

instance Component B2BodyId where
  type Storage B2BodyId = B2Space B2BodyId

instance (MonadIO m, Has w m Physics) => Has w m B2BodyId where
  getStore = cast <$> (getStore :: SystemT w m (B2Space Physics))

instance (MonadIO m) => ExplGet m (B2Space B2BodyId) where
  explExists = bodyExists
  explGet sp ety = liftIO $ withBody sp ety (pure . B2BodyId)

instance (MonadIO m) => ExplMembers m (B2Space B2BodyId) where
  explMembers = bodyMembers

-- Body sub-components ------------------------------------------------------

-- | Where a 'Body' is, in world coordinates.
newtype Position = Position WVec
  deriving (Eq, Show)

instance Component Position where
  type Storage Position = B2Space Position

instance (MonadIO m, Has w m Physics) => Has w m Position where
  getStore = cast <$> (getStore :: SystemT w m (B2Space Physics))

instance (MonadIO m) => ExplGet m (B2Space Position) where
  explExists = bodyExists
  explGet sp ety = liftIO $ withBody sp ety $ fmap Position . B2Body.getPosition

instance (MonadIO m) => ExplSet m (B2Space Position) where
  explSet sp ety (Position p) = liftIO $
    overBody sp ety $ \b -> do
      rot <- B2Body.getRotation b
      B2Body.setTransform b p rot

instance (MonadIO m) => ExplMembers m (B2Space Position) where
  explMembers = bodyMembers

-- | Where a 'Body' is going, in world coordinates.
newtype Velocity = Velocity WVec
  deriving (Eq, Show)

instance Component Velocity where
  type Storage Velocity = B2Space Velocity

instance (MonadIO m, Has w m Physics) => Has w m Velocity where
  getStore = cast <$> (getStore :: SystemT w m (B2Space Physics))

instance (MonadIO m) => ExplGet m (B2Space Velocity) where
  explExists = bodyExists
  explGet sp ety = liftIO $ withBody sp ety $ fmap Velocity . B2Body.getLinearVelocity

instance (MonadIO m) => ExplSet m (B2Space Velocity) where
  explSet sp ety (Velocity v) = liftIO $
    overBody sp ety $ \b ->
      B2Body.setLinearVelocity b v

instance (MonadIO m) => ExplMembers m (B2Space Velocity) where
  explMembers = bodyMembers

-- | A 'Body'\'s rotation, in radians.
newtype Angle = Angle Float
  deriving (Eq, Show)

instance Component Angle where
  type Storage Angle = B2Space Angle

instance (MonadIO m, Has w m Physics) => Has w m Angle where
  getStore = cast <$> (getStore :: SystemT w m (B2Space Physics))

instance (MonadIO m) => ExplGet m (B2Space Angle) where
  explExists = bodyExists
  explGet sp ety =
    liftIO $
      withBody sp ety $
        fmap (Angle . rotGetAngle) . B2Body.getRotation

instance (MonadIO m) => ExplSet m (B2Space Angle) where
  explSet sp ety (Angle theta) = liftIO $
    overBody sp ety $ \b -> do
      pos <- B2Body.getPosition b
      rot <- makeRot theta
      B2Body.setTransform b pos rot

instance (MonadIO m) => ExplMembers m (B2Space Angle) where
  explMembers = bodyMembers

-- | A 'Body'\'s angular velocity, in radians per second.
newtype AngularVelocity = AngularVelocity Float
  deriving (Eq, Show)

instance Component AngularVelocity where
  type Storage AngularVelocity = B2Space AngularVelocity

instance (MonadIO m, Has w m Physics) => Has w m AngularVelocity where
  getStore = cast <$> (getStore :: SystemT w m (B2Space Physics))

instance (MonadIO m) => ExplGet m (B2Space AngularVelocity) where
  explExists = bodyExists
  explGet sp ety =
    liftIO $
      withBody sp ety $
        fmap AngularVelocity . B2Body.getAngularVelocity

instance (MonadIO m) => ExplSet m (B2Space AngularVelocity) where
  explSet sp ety (AngularVelocity omega) = liftIO $
    overBody sp ety $ \b ->
      B2Body.setAngularVelocity b omega

instance (MonadIO m) => ExplMembers m (B2Space AngularVelocity) where
  explMembers = bodyMembers

{- | The mass of a 'Body'. Read-only: Box2D computes it from the attached
shapes' densities.
-}
newtype BodyMass = BodyMass Float
  deriving (Eq, Show)

instance Component BodyMass where
  type Storage BodyMass = B2Space BodyMass

instance (MonadIO m, Has w m Physics) => Has w m BodyMass where
  getStore = cast <$> (getStore :: SystemT w m (B2Space Physics))

instance (MonadIO m) => ExplGet m (B2Space BodyMass) where
  explExists = bodyExists
  explGet sp ety = liftIO $ withBody sp ety $ fmap BodyMass . B2Body.getMass

instance (MonadIO m) => ExplMembers m (B2Space BodyMass) where
  explMembers = bodyMembers

{- | Write-only: setting it applies a force to the 'Body'\'s center.
Forces are additive and reset by the next 'stepPhysics'.
-}
newtype Force = Force WVec
  deriving (Eq, Show)

instance Component Force where
  type Storage Force = B2Space Force

instance (MonadIO m, Has w m Physics) => Has w m Force where
  getStore = cast <$> (getStore :: SystemT w m (B2Space Physics))

instance (MonadIO m) => ExplSet m (B2Space Force) where
  explSet sp ety (Force v) = liftIO $
    overBody sp ety $ \b ->
      B2Body.applyForceToCenter b v True

{- | Write-only: setting it applies a torque to the 'Body'. Torques are
additive and reset by the next 'stepPhysics'.
-}
newtype Torque = Torque Float
  deriving (Eq, Show)

instance Component Torque where
  type Storage Torque = B2Space Torque

instance (MonadIO m, Has w m Physics) => Has w m Torque where
  getStore = cast <$> (getStore :: SystemT w m (B2Space Physics))

instance (MonadIO m) => ExplSet m (B2Space Torque) where
  explSet sp ety (Torque t) = liftIO $
    overBody sp ety $
      \b -> B2Body.applyTorque b t True

-- | Write-only: setting it applies an impulse to the 'Body'\'s center.
newtype LinearImpulse = LinearImpulse WVec
  deriving (Eq, Show)

instance Component LinearImpulse where
  type Storage LinearImpulse = B2Space LinearImpulse

instance (MonadIO m, Has w m Physics) => Has w m LinearImpulse where
  getStore = cast <$> (getStore :: SystemT w m (B2Space Physics))

instance (MonadIO m) => ExplSet m (B2Space LinearImpulse) where
  explSet sp ety (LinearImpulse v) = liftIO $
    overBody sp ety $ \b ->
      B2Body.applyLinearImpulseToCenter b v True

-- | Write-only: setting it applies an angular impulse to the 'Body'.
newtype AngularImpulse = AngularImpulse Float
  deriving (Eq, Show)

instance Component AngularImpulse where
  type Storage AngularImpulse = B2Space AngularImpulse

instance (MonadIO m, Has w m Physics) => Has w m AngularImpulse where
  getStore = cast <$> (getStore :: SystemT w m (B2Space Physics))

instance (MonadIO m) => ExplSet m (B2Space AngularImpulse) where
  explSet sp ety (AngularImpulse i) = liftIO $
    overBody sp ety $ \b ->
      B2Body.applyAngularImpulse b i True

{- | Write-only: setting it applies a force to the 'Body' at a world
point; applying off the center of mass also induces spin. Forces are
additive and reset by the next 'stepPhysics'.
-}
data ForceAt = ForceAt WVec WVec
  deriving (Eq, Show)

instance Component ForceAt where
  type Storage ForceAt = B2Space ForceAt

instance (MonadIO m, Has w m Physics) => Has w m ForceAt where
  getStore = cast <$> (getStore :: SystemT w m (B2Space Physics))

instance (MonadIO m) => ExplSet m (B2Space ForceAt) where
  explSet sp ety (ForceAt v p) = liftIO $
    overBody sp ety $ \b ->
      B2Body.applyForce b v p True

{- | Write-only: setting it applies an impulse to the 'Body' at a world
point; applying off the center of mass also induces spin.
-}
data ImpulseAt = ImpulseAt WVec WVec
  deriving (Eq, Show)

instance Component ImpulseAt where
  type Storage ImpulseAt = B2Space ImpulseAt

instance (MonadIO m, Has w m Physics) => Has w m ImpulseAt where
  getStore = cast <$> (getStore :: SystemT w m (B2Space Physics))

instance (MonadIO m) => ExplSet m (B2Space ImpulseAt) where
  explSet sp ety (ImpulseAt v p) = liftIO $
    overBody sp ety $ \b ->
      B2Body.applyLinearImpulse b v p True

{- | Write-only: setting it sets a kinematic 'Body'\'s velocity so it reaches
the given world position and angle (radians) over the given time step — pass
the time delta of your next 'stepPhysics' call. This is the engine path for
moving platforms: unlike teleporting via 'Position', the body carries real
velocity, so it pushes and carries riders. The target is skipped when the
body is asleep and the implied velocity is below the sleep threshold.
-}
data TargetTransform = TargetTransform WVec Float Float
  deriving (Eq, Show)

instance Component TargetTransform where
  type Storage TargetTransform = B2Space TargetTransform

instance (MonadIO m, Has w m Physics) => Has w m TargetTransform where
  getStore = cast <$> (getStore :: SystemT w m (B2Space Physics))

instance (MonadIO m) => ExplSet m (B2Space TargetTransform) where
  explSet sp ety (TargetTransform p theta dt) = liftIO $
    overBody sp ety $ \b -> do
      rot <- makeRot theta
      B2Body.setTargetTransform b (Transform p rot) dt True

-- | A 'Body'\'s linear velocity damping.
newtype LinearDamping = LinearDamping Float
  deriving (Eq, Show)

instance Component LinearDamping where
  type Storage LinearDamping = B2Space LinearDamping

instance (MonadIO m, Has w m Physics) => Has w m LinearDamping where
  getStore = cast <$> (getStore :: SystemT w m (B2Space Physics))

instance (MonadIO m) => ExplGet m (B2Space LinearDamping) where
  explExists = bodyExists
  explGet sp ety = liftIO $ withBody sp ety $ fmap LinearDamping . B2Body.getLinearDamping

instance (MonadIO m) => ExplSet m (B2Space LinearDamping) where
  explSet sp ety (LinearDamping d) = liftIO $
    overBody sp ety $ \b ->
      B2Body.setLinearDamping b d

instance (MonadIO m) => ExplMembers m (B2Space LinearDamping) where
  explMembers = bodyMembers

-- | A 'Body'\'s angular velocity damping.
newtype AngularDamping = AngularDamping Float
  deriving (Eq, Show)

instance Component AngularDamping where
  type Storage AngularDamping = B2Space AngularDamping

instance (MonadIO m, Has w m Physics) => Has w m AngularDamping where
  getStore = cast <$> (getStore :: SystemT w m (B2Space Physics))

instance (MonadIO m) => ExplGet m (B2Space AngularDamping) where
  explExists = bodyExists
  explGet sp ety = liftIO $ withBody sp ety $ fmap AngularDamping . B2Body.getAngularDamping

instance (MonadIO m) => ExplSet m (B2Space AngularDamping) where
  explSet sp ety (AngularDamping d) = liftIO $
    overBody sp ety $ \b ->
      B2Body.setAngularDamping b d

instance (MonadIO m) => ExplMembers m (B2Space AngularDamping) where
  explMembers = bodyMembers

-- | How strongly gravity affects a 'Body'; 1 is normal, 0 disables it.
newtype GravityScale = GravityScale Float
  deriving (Eq, Show)

instance Component GravityScale where
  type Storage GravityScale = B2Space GravityScale

instance (MonadIO m, Has w m Physics) => Has w m GravityScale where
  getStore = cast <$> (getStore :: SystemT w m (B2Space Physics))

instance (MonadIO m) => ExplGet m (B2Space GravityScale) where
  explExists = bodyExists
  explGet sp ety = liftIO $ withBody sp ety $ fmap GravityScale . B2Body.getGravityScale

instance (MonadIO m) => ExplSet m (B2Space GravityScale) where
  explSet sp ety (GravityScale g) = liftIO $
    overBody sp ety $ \b ->
      B2Body.setGravityScale b g

instance (MonadIO m) => ExplMembers m (B2Space GravityScale) where
  explMembers = bodyMembers

{- | Continuous collision detection for this body (the engine's "bullet"
flag): keeps small, fast bodies from tunnelling through other dynamic
bodies between substeps. Off by default; the cost scales with speed.
-}
newtype BulletBody = BulletBody Bool
  deriving (Eq, Show)

instance Component BulletBody where
  type Storage BulletBody = B2Space BulletBody

instance (MonadIO m, Has w m Physics) => Has w m BulletBody where
  getStore = cast <$> (getStore :: SystemT w m (B2Space Physics))

instance (MonadIO m) => ExplGet m (B2Space BulletBody) where
  explExists = bodyExists
  explGet sp ety = liftIO $ withBody sp ety $ fmap BulletBody . B2Body.isBullet

instance (MonadIO m) => ExplSet m (B2Space BulletBody) where
  explSet sp ety (BulletBody b) = liftIO $
    overBody sp ety $ \bd ->
      B2Body.setBullet bd b

instance (MonadIO m) => ExplMembers m (B2Space BulletBody) where
  explMembers = bodyMembers

{- | Whether a 'Body' participates in the simulation at all (on by
default). Disabling removes the body and its shapes from the world
without destroying them — cheap despawn/pooling; enabling puts them
back.
-}
newtype BodyEnabled = BodyEnabled Bool
  deriving (Eq, Show)

instance Component BodyEnabled where
  type Storage BodyEnabled = B2Space BodyEnabled

instance (MonadIO m, Has w m Physics) => Has w m BodyEnabled where
  getStore = cast <$> (getStore :: SystemT w m (B2Space Physics))

instance (MonadIO m) => ExplGet m (B2Space BodyEnabled) where
  explExists = bodyExists
  explGet sp ety = liftIO $ withBody sp ety $ fmap BodyEnabled . B2Body.isEnabled

instance (MonadIO m) => ExplSet m (B2Space BodyEnabled) where
  explSet sp ety (BodyEnabled e) = liftIO $
    overBody sp ety $ \b ->
      if e then B2Body.enable b else B2Body.disable b

instance (MonadIO m) => ExplMembers m (B2Space BodyEnabled) where
  explMembers = bodyMembers

{- | Whether a 'Body' is currently awake and simulating. Set it to wake
a body explicitly — e.g. after teleporting it via 'Position' — or to
put it to sleep. Waking or sleeping a body extends to the whole island
of bodies touching it.
-}
newtype Awake = Awake Bool
  deriving (Eq, Show)

instance Component Awake where
  type Storage Awake = B2Space Awake

instance (MonadIO m, Has w m Physics) => Has w m Awake where
  getStore = cast <$> (getStore :: SystemT w m (B2Space Physics))

instance (MonadIO m) => ExplGet m (B2Space Awake) where
  explExists = bodyExists
  explGet sp ety = liftIO $ withBody sp ety $ fmap Awake . B2Body.isAwake

instance (MonadIO m) => ExplSet m (B2Space Awake) where
  explSet sp ety (Awake a) = liftIO $
    overBody sp ety $ \b ->
      B2Body.setAwake b a

instance (MonadIO m) => ExplMembers m (B2Space Awake) where
  explMembers = bodyMembers

{- | Per-axis motion locks on a 'Body': locking a linear axis prevents
translation along it, and locking the angular axis prevents rotation
about it. Locked rotation is the classic platformer/top-down "fixed
rotation" (see 'FixedRotation'); locking a linear axis constrains a
body to rail-style movement along the other. All axes are unlocked by
default.
-}
data MotionLocks = MotionLocks
  { lockLinearX :: Bool
  , lockLinearY :: Bool
  , lockAngularZ :: Bool
  }
  deriving (Eq, Show)

toB2MotionLocks :: MotionLocks -> B2T.MotionLocks
toB2MotionLocks (MotionLocks lx ly az) =
  B2T.MotionLocks (fromBool lx) (fromBool ly) (fromBool az)

fromB2MotionLocks :: B2T.MotionLocks -> MotionLocks
fromB2MotionLocks (B2T.MotionLocks lx ly az) =
  MotionLocks (toBool lx) (toBool ly) (toBool az)

instance Component MotionLocks where
  type Storage MotionLocks = B2Space MotionLocks

instance (MonadIO m, Has w m Physics) => Has w m MotionLocks where
  getStore = cast <$> (getStore :: SystemT w m (B2Space Physics))

instance (MonadIO m) => ExplGet m (B2Space MotionLocks) where
  explExists = bodyExists
  explGet sp ety = liftIO $ withBody sp ety $ fmap fromB2MotionLocks . B2Body.getMotionLocks

instance (MonadIO m) => ExplSet m (B2Space MotionLocks) where
  explSet sp ety locks = liftIO $
    overBody sp ety $ \b ->
      B2Body.setMotionLocks b (toB2MotionLocks locks)

instance (MonadIO m) => ExplMembers m (B2Space MotionLocks) where
  explMembers = bodyMembers

{- | Whether a 'Body'\'s rotation is locked: top-down and platformer
characters lock rotation so contacts and off-center forces can't spin
them. Sugar over the 'MotionLocks' angular-Z lock; setting it preserves
the linear locks.
-}
newtype FixedRotation = FixedRotation Bool
  deriving (Eq, Show)

instance Component FixedRotation where
  type Storage FixedRotation = B2Space FixedRotation

instance (MonadIO m, Has w m Physics) => Has w m FixedRotation where
  getStore = cast <$> (getStore :: SystemT w m (B2Space Physics))

instance (MonadIO m) => ExplGet m (B2Space FixedRotation) where
  explExists = bodyExists
  explGet sp ety =
    liftIO $
      withBody sp ety $
        fmap (FixedRotation . toBool . B2T.motionLocksAngularZ) . B2Body.getMotionLocks

instance (MonadIO m) => ExplSet m (B2Space FixedRotation) where
  explSet sp ety (FixedRotation fixed) = liftIO $
    overBody sp ety $ \b -> do
      locks <- B2Body.getMotionLocks b
      B2Body.setMotionLocks b locks{B2T.motionLocksAngularZ = fromBool fixed}

instance (MonadIO m) => ExplMembers m (B2Space FixedRotation) where
  explMembers = bodyMembers

{- | Whether a 'Body' may fall asleep at all (on by default). Disabling
it wakes the body (and its island). World-level control is
'SleepingEnabled'.
-}
newtype SleepEnabled = SleepEnabled Bool
  deriving (Eq, Show)

instance Component SleepEnabled where
  type Storage SleepEnabled = B2Space SleepEnabled

instance (MonadIO m, Has w m Physics) => Has w m SleepEnabled where
  getStore = cast <$> (getStore :: SystemT w m (B2Space Physics))

instance (MonadIO m) => ExplGet m (B2Space SleepEnabled) where
  explExists = bodyExists
  explGet sp ety = liftIO $ withBody sp ety $ fmap SleepEnabled . B2Body.isSleepEnabled

instance (MonadIO m) => ExplSet m (B2Space SleepEnabled) where
  explSet sp ety (SleepEnabled e) = liftIO $
    overBody sp ety $ \b ->
      B2Body.enableSleep b e

instance (MonadIO m) => ExplMembers m (B2Space SleepEnabled) where
  explMembers = bodyMembers

{- | The speed below which a 'Body' may fall asleep, usually in meters
per second.
-}
newtype SleepThreshold = SleepThreshold Float
  deriving (Eq, Show)

instance Component SleepThreshold where
  type Storage SleepThreshold = B2Space SleepThreshold

instance (MonadIO m, Has w m Physics) => Has w m SleepThreshold where
  getStore = cast <$> (getStore :: SystemT w m (B2Space Physics))

instance (MonadIO m) => ExplGet m (B2Space SleepThreshold) where
  explExists = bodyExists
  explGet sp ety = liftIO $ withBody sp ety $ fmap SleepThreshold . B2Body.getSleepThreshold

instance (MonadIO m) => ExplSet m (B2Space SleepThreshold) where
  explSet sp ety (SleepThreshold t) = liftIO $
    overBody sp ety $ \b ->
      B2Body.setSleepThreshold b t

instance (MonadIO m) => ExplMembers m (B2Space SleepThreshold) where
  explMembers = bodyMembers

{- | The center of mass of a 'Body' in local (body) space. Read-only:
Box2D computes it from the attached shapes' densities. The
apecs-physics analog is @CenterOfGravity@.
-}
newtype CenterOfMass = CenterOfMass BVec
  deriving (Eq, Show)

instance Component CenterOfMass where
  type Storage CenterOfMass = B2Space CenterOfMass

instance (MonadIO m, Has w m Physics) => Has w m CenterOfMass where
  getStore = cast <$> (getStore :: SystemT w m (B2Space Physics))

instance (MonadIO m) => ExplGet m (B2Space CenterOfMass) where
  explExists = bodyExists
  explGet sp ety = liftIO $ withBody sp ety $ fmap CenterOfMass . B2Body.getLocalCenter

instance (MonadIO m) => ExplMembers m (B2Space CenterOfMass) where
  explMembers = bodyMembers

{- | The rotational inertia of a 'Body', usually in kg*m^2. Read-only:
Box2D computes it from the attached shapes' densities. The
apecs-physics analog is @Moment@.
-}
newtype RotationalInertia = RotationalInertia Float
  deriving (Eq, Show)

instance Component RotationalInertia where
  type Storage RotationalInertia = B2Space RotationalInertia

instance (MonadIO m, Has w m Physics) => Has w m RotationalInertia where
  getStore = cast <$> (getStore :: SystemT w m (B2Space Physics))

instance (MonadIO m) => ExplGet m (B2Space RotationalInertia) where
  explExists = bodyExists
  explGet sp ety = liftIO $ withBody sp ety $ fmap RotationalInertia . B2Body.getRotationalInertia

instance (MonadIO m) => ExplMembers m (B2Space RotationalInertia) where
  explMembers = bodyMembers

{- | An optional name for a 'Body', for debugging\/tooling. The engine
stores names in a fixed 10-byte buffer (@B2_NAME_LENGTH@); longer names
are silently truncated to 10 bytes on write, excluding the terminating
null.
-}
newtype BodyName = BodyName String
  deriving (Eq, Show)

instance Component BodyName where
  type Storage BodyName = B2Space BodyName

instance (MonadIO m, Has w m Physics) => Has w m BodyName where
  getStore = cast <$> (getStore :: SystemT w m (B2Space Physics))

instance (MonadIO m) => ExplGet m (B2Space BodyName) where
  explExists = bodyExists
  explGet sp ety = liftIO $ withBody sp ety $ \b ->
    BodyName <$> (B2Body.getName b >>= peekCString)

instance (MonadIO m) => ExplSet m (B2Space BodyName) where
  explSet sp ety (BodyName name) = liftIO $
    overBody sp ety $ \b ->
      withCString name (B2Body.setName b)

instance (MonadIO m) => ExplMembers m (B2Space BodyName) where
  explMembers = bodyMembers

-- Shape ---------------------------------------------------------------------

-- | Shape geometry in body-local coordinates.
data Geometry
  = -- | Center and radius.
    GeoCircle BVec Float
  | -- | The two centers and the radius around the segment between them.
    GeoCapsule BVec BVec Float
  | -- | A two-sided line segment.
    GeoSegment BVec BVec
  | -- | An axis-aligned box from half-width and half-height.
    GeoBox Float Float
  | -- | Half-width, half-height, corner radius: a box with rounded corners.
    GeoRoundedBox Float Float Float
  | {- | Half-width, half-height, local center, local rotation angle
    (radians): a box placed off the body origin.
    -}
    GeoOffsetBox Float Float BVec Float
  | {- | The convex hull of 3 to 'B2T.maxPolygonVertices' points. Setting
    an out-of-range or degenerate (collinear) point set raises an error.
    -}
    GeoPolygon (VS.Vector Vec2)
  | {- | The convex hull of 3 to 'B2T.maxPolygonVertices' points, placed
    off the body origin at a local center and rotation angle (radians)
    and rounded by the given corner radius. Setting an out-of-range or
    degenerate (collinear) point set raises an error.
    -}
    GeoOffsetRoundedPolygon (VS.Vector Vec2) BVec Float Float
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
  type Storage Shape = B2Space Shape

instance (MonadIO m, Has w m Physics) => Has w m Shape where
  getStore = cast <$> (getStore :: SystemT w m (B2Space Physics))

{- | Compute a validated convex hull for a polygon-shaped 'Geometry',
shared by 'GeoPolygon' and 'GeoOffsetRoundedPolygon'.
-}
computeValidHull :: VS.Vector Vec2 -> IO B2T.Hull
computeValidHull pts = do
  let n = VS.length pts
  when (n < 3 || n > B2T.maxPolygonVertices) $
    error ("polygon needs 3 to " <> show B2T.maxPolygonVertices <> " points, got " <> show n)
  hull <- VS.unsafeWith pts $ \p -> B2Collision.computeHull p n
  when (VS.length (B2T.hullPoints hull) < 3) $
    error "polygon points are degenerate (collinear or coincident)"
  pure hull

-- | Create the engine geometry for a 'Geometry' value on a body.
createGeometry :: BodyId -> B2T.ShapeDef -> Geometry -> IO ShapeId
createGeometry b sd geo = case geo of
  GeoCircle c r -> B2Shape.createCircle b sd (B2T.Circle c r)
  GeoCapsule c1 c2 r -> B2Shape.createCapsule b sd (B2T.Capsule c1 c2 r)
  GeoSegment p1 p2 -> B2Shape.createSegment b sd (B2T.Segment p1 p2)
  GeoBox hw hh -> B2Collision.makeBox hw hh >>= B2Shape.createPolygon b sd
  GeoRoundedBox hw hh r -> B2Collision.makeRoundedBox hw hh r >>= B2Shape.createPolygon b sd
  GeoOffsetBox hw hh center angle -> do
    rot <- makeRot angle
    B2Collision.makeOffsetBox hw hh center rot >>= B2Shape.createPolygon b sd
  GeoPolygon pts -> do
    hull <- computeValidHull pts
    B2Collision.makePolygon hull 0 >>= B2Shape.createPolygon b sd
  GeoOffsetRoundedPolygon pts center angle r -> do
    hull <- computeValidHull pts
    rot <- makeRot angle
    B2Collision.makeOffsetRoundedPolygon hull center rot r >>= B2Shape.createPolygon b sd

{- | A shape def with the surface material, density, filter and sensor
flag carried over from the shape being replaced, if any.
-}
carryMaterial :: B2T.ShapeDef -> Maybe ShapeRecord -> IO B2T.ShapeDef
carryMaterial sd Nothing = pure sd
carryMaterial sd (Just (ShapeRecord s _)) = do
  material <- B2Shape.getSurfaceMaterial s
  density <- B2Shape.getDensity s
  filtr <- B2Shape.getFilter s
  sensor <- B2Shape.isSensor s
  pure
    sd
      { B2T.shapeDefMaterial = material
      , B2T.shapeDefDensity = density
      , B2T.shapeDefFilter = filtr
      , B2T.shapeDefIsSensor = fromBool sensor
      }

{- | Create a fresh engine shape for a 'Shape' value with the given def,
tag it with the entity's user index, destroy the shape it replaces (if
any) only after the new one exists (so a failed create, e.g. a bad
polygon, leaves everything intact), and update the shape registry.
Shared by 'Shape' and 'Sensor', which both recreate the shape while
preserving its material state.
-}
recreateShape :: B2Space c -> BodyId -> Int -> B2T.ShapeDef -> Shape -> Maybe ShapeRecord -> IO ()
recreateShape sp b ety sd shape@(Shape _ geo) old = do
  s <- createGeometry b sd geo
  setUserIndex s ety
  forM_ old $ \(ShapeRecord s' _) -> B2Shape.destroy s' True
  modifyIORef' (spShapes sp) (IM.insert ety (ShapeRecord s shape))

instance (MonadIO m) => ExplSet m (B2Space Shape) where
  explSet sp ety shape@(Shape (Entity bEty) _) = liftIO $
    overBody sp bEty $ \b -> do
      old <- IM.lookup ety <$> readIORef (spShapes sp)
      sd <- carryMaterial (spShapeDef sp) old
      recreateShape sp b ety sd shape old

instance (MonadIO m) => ExplGet m (B2Space Shape) where
  explExists = shapeExists
  explGet sp ety = liftIO $
    withReg "Shape" (spShapes sp) ety $
      \(ShapeRecord _ shape) -> pure shape

instance (MonadIO m) => ExplDestroy m (B2Space Shape) where
  explDestroy sp ety = liftIO $ do
    shapes <- readIORef (spShapes sp)
    forM_ (IM.lookup ety shapes) $ \(ShapeRecord s _) -> do
      modifyIORef' (spShapes sp) (IM.delete ety)
      B2Shape.destroy s True

instance (MonadIO m) => ExplMembers m (B2Space Shape) where
  explMembers = shapeMembers

-- | The raw Box2D shape of an entity, for use with "Box2D.Shape" directly.
newtype B2ShapeId = B2ShapeId ShapeId
  deriving (Eq, Show)

instance Component B2ShapeId where
  type Storage B2ShapeId = B2Space B2ShapeId

instance (MonadIO m, Has w m Physics) => Has w m B2ShapeId where
  getStore = cast <$> (getStore :: SystemT w m (B2Space Physics))

instance (MonadIO m) => ExplGet m (B2Space B2ShapeId) where
  explExists = shapeExists
  explGet sp ety = liftIO $ withShape sp ety (pure . B2ShapeId)

instance (MonadIO m) => ExplMembers m (B2Space B2ShapeId) where
  explMembers = shapeMembers

{- | The fewest points the engine accepts for a chain (both open and
looped), per @b2ChainDef@: it uses the extra points as ghost vertices
to suppress ghost collisions at internal joints.
-}
minChainPoints :: Int
minChainPoints = 4

{- | Gives an entity a chain of connected line segments attached to the
'Body' of the given entity — smooth static terrain outlines without the
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
'Body' is a silent no-op.

The segments the engine creates for a chain are its own internal
@b2ChainSegment@ shapes, never registered in this layer's shape registry
(there is no matching 'Shape' component for them), so contacts against
them do not currently surface in 'Collisions', 'CollisionsEnd' or
'Impacts' — those globals resolve engine shapes back to entities through
the registry and silently drop anything not found there. The queries
have the same blind spot: 'segmentQueryAll' and 'containsPointQuery'
drop hits on chain segments, and 'segmentQuery' returns 'Nothing'
outright when a chain segment is the closest hit — the chain occludes
whatever lies behind it rather than being skipped. Chains still collide
normally; only event and query reporting is affected.
-}
data Chain = Chain Entity (VS.Vector Vec2) Bool
  deriving (Eq, Show)

instance Component Chain where
  type Storage Chain = B2Space Chain

instance (MonadIO m, Has w m Physics) => Has w m Chain where
  getStore = cast <$> (getStore :: SystemT w m (B2Space Physics))

{- | Create a fresh engine chain for a 'Chain' value, destroy the chain it
replaces (if any) only after the new one exists (so a failed create,
e.g. too few points, leaves everything intact), and update the chain
registry. Mirrors 'recreateShape'.
-}
recreateChain :: B2Space c -> BodyId -> Int -> Chain -> Maybe ChainRecord -> IO ()
recreateChain sp b ety chain@(Chain _ pts isLoop) old = do
  let n = VS.length pts
  when (n < minChainPoints) $
    error ("chain needs at least " <> show minChainPoints <> " points, got " <> show n)
  def <- B2T.defaultChainDef
  c <- VS.unsafeWith pts $ \p ->
    B2Chain.create
      b
      def
        { B2T.chainDefPoints = p
        , B2T.chainDefCount = fromIntegral n
        , B2T.chainDefIsLoop = fromBool isLoop
        }
  -- Unlike BodyId/ShapeId/JointId, ChainId has no 'Box2D.UserData.HasUserData'
  -- instance, so the chain itself cannot be stamped with the entity's user
  -- index (the chain registry below, keyed by entity, is what
  -- 'explGet'/'B2ChainId' use instead). Its segment 'ShapeId's could be
  -- stamped, but that would not help event resolution either: 'shapeEntities'
  -- only recognises shapes present in the shape registry, and chain segments
  -- never are (see the haddock above).
  forM_ old $ \(ChainRecord c' _) -> B2Chain.destroy c'
  modifyIORef' (spChains sp) (IM.insert ety (ChainRecord c chain))

instance (MonadIO m) => ExplSet m (B2Space Chain) where
  explSet sp ety chain@(Chain (Entity bEty) _ _) = liftIO $
    overBody sp bEty $ \b -> do
      old <- IM.lookup ety <$> readIORef (spChains sp)
      recreateChain sp b ety chain old

instance (MonadIO m) => ExplGet m (B2Space Chain) where
  explExists = chainExists
  explGet sp ety = liftIO $
    withReg "Chain" (spChains sp) ety $
      \(ChainRecord _ chain) -> pure chain

instance (MonadIO m) => ExplDestroy m (B2Space Chain) where
  explDestroy sp ety = liftIO $ do
    chains <- readIORef (spChains sp)
    forM_ (IM.lookup ety chains) $ \(ChainRecord c _) -> do
      modifyIORef' (spChains sp) (IM.delete ety)
      B2Chain.destroy c

instance (MonadIO m) => ExplMembers m (B2Space Chain) where
  explMembers = chainMembers

-- | The raw Box2D chain of an entity, for use with "Box2D.Chain" directly.
newtype B2ChainId = B2ChainId ChainId
  deriving (Eq, Show)

instance Component B2ChainId where
  type Storage B2ChainId = B2Space B2ChainId

instance (MonadIO m, Has w m Physics) => Has w m B2ChainId where
  getStore = cast <$> (getStore :: SystemT w m (B2Space Physics))

instance (MonadIO m) => ExplGet m (B2Space B2ChainId) where
  explExists = chainExists
  explGet sp ety = liftIO $ withChain sp ety (pure . B2ChainId)

instance (MonadIO m) => ExplMembers m (B2Space B2ChainId) where
  explMembers = chainMembers

-- Shape sub-components -----------------------------------------------------

-- | The density of a 'Shape'. Setting it updates the body's mass.
newtype Density = Density Float
  deriving (Eq, Show)

instance Component Density where
  type Storage Density = B2Space Density

instance (MonadIO m, Has w m Physics) => Has w m Density where
  getStore = cast <$> (getStore :: SystemT w m (B2Space Physics))

instance (MonadIO m) => ExplGet m (B2Space Density) where
  explExists = shapeExists
  explGet sp ety = liftIO $ withShape sp ety $ fmap Density . B2Shape.getDensity

instance (MonadIO m) => ExplSet m (B2Space Density) where
  explSet sp ety (Density d) = liftIO $
    overShape sp ety $
      \s -> B2Shape.setDensity s d True

instance (MonadIO m) => ExplMembers m (B2Space Density) where
  explMembers = shapeMembers

-- | The friction coefficient of a 'Shape'.
newtype Friction = Friction Float
  deriving (Eq, Show)

instance Component Friction where
  type Storage Friction = B2Space Friction

instance (MonadIO m, Has w m Physics) => Has w m Friction where
  getStore = cast <$> (getStore :: SystemT w m (B2Space Physics))

instance (MonadIO m) => ExplGet m (B2Space Friction) where
  explExists = shapeExists
  explGet sp ety = liftIO $ withShape sp ety $ fmap Friction . B2Shape.getFriction

instance (MonadIO m) => ExplSet m (B2Space Friction) where
  explSet sp ety (Friction f) = liftIO $
    overShape sp ety $
      \s -> B2Shape.setFriction s f

instance (MonadIO m) => ExplMembers m (B2Space Friction) where
  explMembers = shapeMembers

-- | The elasticity of a 'Shape' (Box2D calls this restitution).
newtype Elasticity = Elasticity Float
  deriving (Eq, Show)

instance Component Elasticity where
  type Storage Elasticity = B2Space Elasticity

instance (MonadIO m, Has w m Physics) => Has w m Elasticity where
  getStore = cast <$> (getStore :: SystemT w m (B2Space Physics))

instance (MonadIO m) => ExplGet m (B2Space Elasticity) where
  explExists = shapeExists
  explGet sp ety = liftIO $ withShape sp ety $ fmap Elasticity . B2Shape.getRestitution

instance (MonadIO m) => ExplSet m (B2Space Elasticity) where
  explSet sp ety (Elasticity e) = liftIO $
    overShape sp ety $
      \s -> B2Shape.setRestitution s e

instance (MonadIO m) => ExplMembers m (B2Space Elasticity) where
  explMembers = shapeMembers

-- | The collision 'Filter' of a 'Shape' (category, mask, group).
newtype CollisionFilter = CollisionFilter Filter
  deriving (Eq, Show)

instance Component CollisionFilter where
  type Storage CollisionFilter = B2Space CollisionFilter

instance (MonadIO m, Has w m Physics) => Has w m CollisionFilter where
  getStore = cast <$> (getStore :: SystemT w m (B2Space Physics))

instance (MonadIO m) => ExplGet m (B2Space CollisionFilter) where
  explExists = shapeExists
  explGet sp ety = liftIO $ withShape sp ety $ fmap CollisionFilter . B2Shape.getFilter

instance (MonadIO m) => ExplSet m (B2Space CollisionFilter) where
  explSet sp ety (CollisionFilter f) = liftIO $
    overShape sp ety $
      \s -> B2Shape.setFilter s f

instance (MonadIO m) => ExplMembers m (B2Space CollisionFilter) where
  explMembers = shapeMembers

{- | Whether a 'Shape' is a sensor: a trigger volume that reports
overlaps through 'SensorEvents' instead of generating contacts. Box2D
cannot change a live shape from sensor to solid or back, so setting
this recreates the engine shape (as 'Shape' does, preserving 'Density',
'Friction', 'Elasticity' and 'CollisionFilter') whenever the requested
value differs from the shape's current one; setting the value it
already has is a no-op. Re-setting 'Shape' preserves the sensor flag
the same way. Reads reflect the engine. Setting it on an entity that
has no 'Shape' yet is a silent no-op, so in a 'newEntity' tuple put
'Shape' before 'Sensor' — components are set left to right.
-}
newtype Sensor = Sensor Bool
  deriving (Eq, Show)

instance Component Sensor where
  type Storage Sensor = B2Space Sensor

instance (MonadIO m, Has w m Physics) => Has w m Sensor where
  getStore = cast <$> (getStore :: SystemT w m (B2Space Physics))

instance (MonadIO m) => ExplGet m (B2Space Sensor) where
  explExists = shapeExists
  explGet sp ety = liftIO $ withShape sp ety $ fmap Sensor . B2Shape.isSensor

instance (MonadIO m) => ExplSet m (B2Space Sensor) where
  explSet sp ety (Sensor wantSensor) = liftIO $ do
    old <- IM.lookup ety <$> readIORef (spShapes sp)
    forM_ old $ \old'@(ShapeRecord s shape@(Shape (Entity bEty) _)) -> do
      isSensorNow <- B2Shape.isSensor s
      when (isSensorNow /= wantSensor) $
        overBody sp bEty $ \b -> do
          sd <- carryMaterial (spShapeDef sp) (Just old')
          recreateShape sp b ety sd{B2T.shapeDefIsSensor = fromBool wantSensor} shape (Just old')

instance (MonadIO m) => ExplMembers m (B2Space Sensor) where
  explMembers = shapeMembers

-- Joint ----------------------------------------------------------------------

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
  type Storage Joint = B2Space Joint

instance (MonadIO m, Has w m Physics) => Has w m Joint where
  getStore = cast <$> (getStore :: SystemT w m (B2Space Physics))

-- | A joint frame at a world point, with zero rotation in world space.
frameAt :: BodyId -> Vec2 -> IO Transform
frameAt b p = do
  local <- B2Body.getLocalPoint b p
  Rot c s <- B2Body.getRotation b
  pure (Transform local (Rot c (-s)))

{- | Fill a joint def's base with the two bodies and their frames at
their respective world anchors (shared-point joints pass the same
anchor twice).
-}
baseAt :: B2T.JointDef -> BodyId -> BodyId -> Vec2 -> Vec2 -> IO B2T.JointDef
baseAt jd a b pA pB = do
  fa <- frameAt a pA
  fb <- frameAt b pB
  pure
    jd
      { B2T.jointDefBodyIdA = a
      , B2T.jointDefBodyIdB = b
      , B2T.jointDefLocalFrameA = fa
      , B2T.jointDefLocalFrameB = fb
      }

-- | Compose two 2D rotations (apply the right one first).
rotMul :: Rot -> Rot -> Rot
rotMul (Rot c1 s1) (Rot c2 s2) = Rot (c1 * c2 - s1 * s2) (s1 * c2 + c1 * s2)

-- | Normalize a vector; errors on a zero (or NaN) length, which has no direction.
normalizeAxis :: Vec2 -> Vec2
normalizeAxis (Vec2 x y)
  | m > 0 = Vec2 (x / m) (y / m)
  | otherwise = error "joint axis has zero length"
  where
    m = sqrt (x * x + y * y)

{- | A joint frame at a world point whose x-axis points along a world
axis. World orientation of a joint frame is @rot(body) * rot(local)@;
cancelling the body rotation and then composing with the rotation that
carries the canonical x-axis onto the world axis (a unit direction
vector is itself that rotation) gives a frame whose x-axis is that
world axis, independent of the body's own orientation.
-}
axisFrameAt :: BodyId -> Vec2 -> Vec2 -> IO Transform
axisFrameAt b p axis = do
  local <- B2Body.getLocalPoint b p
  Rot c s <- B2Body.getRotation b
  let Vec2 ux uy = normalizeAxis axis
  pure (Transform local (rotMul (Rot c (-s)) (Rot ux uy)))

{- | Fill a joint def's base with the two bodies and frames at a shared
world anchor, both x-axis aligned to a world axis.
-}
axisBaseAt :: B2T.JointDef -> BodyId -> BodyId -> Vec2 -> Vec2 -> IO B2T.JointDef
axisBaseAt jd a b p axis = do
  fa <- axisFrameAt a p axis
  fb <- axisFrameAt b p axis
  pure
    jd
      { B2T.jointDefBodyIdA = a
      , B2T.jointDefBodyIdB = b
      , B2T.jointDefLocalFrameA = fa
      , B2T.jointDefLocalFrameB = fb
      }

createJoint :: WorldId -> BodyId -> BodyId -> JointSpec -> IO JointId
createJoint w a b spec = case spec of
  PivotJoint p -> revoluteAt p id
  RotarySpringJoint p hertz damping ->
    revoluteAt p $ \jd ->
      jd
        { B2T.revoluteJointDefEnableSpring = 1
        , B2T.revoluteJointDefHertz = hertz
        , B2T.revoluteJointDefDampingRatio = damping
        }
  RotaryLimitJoint p lower upper ->
    revoluteAt p $ \jd ->
      jd
        { B2T.revoluteJointDefEnableLimit = 1
        , B2T.revoluteJointDefLowerAngle = lower
        , B2T.revoluteJointDefUpperAngle = upper
        }
  RotaryMotorJoint p speed maxTorque ->
    revoluteAt p $ \jd ->
      jd
        { B2T.revoluteJointDefEnableMotor = 1
        , B2T.revoluteJointDefMotorSpeed = speed
        , B2T.revoluteJointDefMaxMotorTorque = maxTorque
        }
  DistanceJoint pA pB -> distanceAt pA pB id
  SpringJoint pA pB hertz damping ->
    distanceAt pA pB $ \jd ->
      jd
        { B2T.distanceJointDefEnableSpring = 1
        , B2T.distanceJointDefHertz = hertz
        , B2T.distanceJointDefDampingRatio = damping
        }
  SlideJoint pA pB minLen maxLen ->
    -- a zero-stiffness spring exerts no force, leaving the distance free
    -- within the enabled limits
    distanceAt pA pB $ \jd ->
      jd
        { B2T.distanceJointDefEnableSpring = 1
        , B2T.distanceJointDefHertz = 0
        , B2T.distanceJointDefEnableLimit = 1
        , B2T.distanceJointDefMinLength = minLen
        , B2T.distanceJointDefMaxLength = maxLen
        }
  WeldJoint p -> do
    jd <- B2T.defaultWeldJointDef
    base <- baseAt (B2T.weldJointDefBase jd) a b p p
    B2WeldJoint.create w jd{B2T.weldJointDefBase = base}
  PrismaticJoint p axis lower upper ->
    -- prismatic joints already forbid relative rotation, so the limit
    -- alone is enough to keep the translation free within it
    prismaticAt p axis $ \jd ->
      jd
        { B2T.prismaticJointDefEnableLimit = 1
        , B2T.prismaticJointDefLowerTranslation = lower
        , B2T.prismaticJointDefUpperTranslation = upper
        }
  PrismaticSpringJoint p axis hertz damping ->
    prismaticAt p axis $ \jd ->
      jd
        { B2T.prismaticJointDefEnableSpring = 1
        , B2T.prismaticJointDefHertz = hertz
        , B2T.prismaticJointDefDampingRatio = damping
        }
  PrismaticMotorJoint p axis speed maxForce ->
    prismaticAt p axis $ \jd ->
      jd
        { B2T.prismaticJointDefEnableMotor = 1
        , B2T.prismaticJointDefMotorSpeed = speed
        , B2T.prismaticJointDefMaxMotorForce = maxForce
        }
  WheelJoint p axis hertz damping -> do
    jd <- B2T.defaultWheelJointDef
    base <- axisBaseAt (B2T.wheelJointDefBase jd) a b p axis
    B2WheelJoint.create
      w
      jd
        { B2T.wheelJointDefBase = base
        , B2T.wheelJointDefEnableSpring = 1
        , B2T.wheelJointDefHertz = hertz
        , B2T.wheelJointDefDampingRatio = damping
        }
  MotorJoint p linVel maxForce angVel maxTorque -> do
    jd <- B2T.defaultMotorJointDef
    base <- baseAt (B2T.motorJointDefBase jd) a b p p
    B2MotorJoint.create
      w
      jd
        { B2T.motorJointDefBase = base
        , B2T.motorJointDefLinearVelocity = linVel
        , B2T.motorJointDefMaxVelocityForce = maxForce
        , B2T.motorJointDefAngularVelocity = angVel
        , B2T.motorJointDefMaxVelocityTorque = maxTorque
        }
  where
    revoluteAt p f = do
      jd <- B2T.defaultRevoluteJointDef
      base <- baseAt (B2T.revoluteJointDefBase jd) a b p p
      B2RevoluteJoint.create w (f jd){B2T.revoluteJointDefBase = base}
    distanceAt pA pB f = do
      jd <- B2T.defaultDistanceJointDef
      base <- baseAt (B2T.distanceJointDefBase jd) a b pA pB
      let
        Vec2 x1 y1 = pA
        Vec2 x2 y2 = pB
        len = sqrt ((x2 - x1) ^ (2 :: Int) + (y2 - y1) ^ (2 :: Int))
      B2DistanceJoint.create w (f jd){B2T.distanceJointDefBase = base, B2T.distanceJointDefLength = len}
    prismaticAt p axis f = do
      jd <- B2T.defaultPrismaticJointDef
      base <- axisBaseAt (B2T.prismaticJointDefBase jd) a b p axis
      B2PrismaticJoint.create w (f jd){B2T.prismaticJointDefBase = base}

instance (MonadIO m) => ExplSet m (B2Space Joint) where
  explSet sp ety joint@(Joint (Entity aEty) (Entity bEty) spec) = liftIO $ when (aEty /= bEty) $ do
    bodies <- readIORef (spBodies sp)
    forM_ ((,) <$> IM.lookup aEty bodies <*> IM.lookup bEty bodies) $ \(a, b) -> do
      old <- IM.lookup ety <$> readIORef (spJoints sp)
      j <- createJoint (spWorld sp) a b spec
      setUserIndex j ety
      forM_ old $ \(JointRecord j' _) -> B2Joint.destroy j' True
      modifyIORef' (spJoints sp) (IM.insert ety (JointRecord j joint))

instance (MonadIO m) => ExplGet m (B2Space Joint) where
  explExists = jointExists
  explGet sp ety = liftIO $
    withReg "Joint" (spJoints sp) ety $
      \(JointRecord _ joint) -> pure joint

instance (MonadIO m) => ExplDestroy m (B2Space Joint) where
  explDestroy sp ety = liftIO $ do
    joints <- readIORef (spJoints sp)
    forM_ (IM.lookup ety joints) $ \(JointRecord j _) -> do
      modifyIORef' (spJoints sp) (IM.delete ety)
      B2Joint.destroy j True

instance (MonadIO m) => ExplMembers m (B2Space Joint) where
  explMembers = jointMembers

-- | The raw Box2D joint of an entity, for use with the joint modules.
newtype B2JointId = B2JointId JointId
  deriving (Eq, Show)

instance Component B2JointId where
  type Storage B2JointId = B2Space B2JointId

instance (MonadIO m, Has w m Physics) => Has w m B2JointId where
  getStore = cast <$> (getStore :: SystemT w m (B2Space Physics))

instance (MonadIO m) => ExplGet m (B2Space B2JointId) where
  explExists = jointExists
  explGet sp ety = liftIO $ withJoint sp ety (pure . B2JointId)

instance (MonadIO m) => ExplMembers m (B2Space B2JointId) where
  explMembers = jointMembers

{- | The motor's target speed on a 'Joint': radians per second on a
revolute joint ('PivotJoint', 'RotarySpringJoint', 'RotaryLimitJoint',
'RotaryMotorJoint'), meters per second on a prismatic joint
('PrismaticJoint', 'PrismaticSpringJoint', 'PrismaticMotorJoint'), or
radians per second on a wheel joint's spin motor ('WheelJoint').
Setting this also enables the corresponding motor, so a speed always
takes effect immediately; use 'MotorMaxTorque'\/'MotorMaxForce' to cap
it without starting it. The wheel's suspension spring and limit are
not covered by this component. Setting it on any other joint kind, or
on an entity with no 'Joint', is a silent no-op.
-}
newtype MotorSpeed = MotorSpeed Float
  deriving (Eq, Show)

-- | Joint kinds 'MotorSpeed' covers; keeps exists\/get\/members in sync.
motorSpeedKinds :: [B2T.JointType]
motorSpeedKinds = [B2T.RevoluteJoint, B2T.PrismaticJoint, B2T.WheelJoint]

instance Component MotorSpeed where
  type Storage MotorSpeed = B2Space MotorSpeed

instance (MonadIO m, Has w m Physics) => Has w m MotorSpeed where
  getStore = cast <$> (getStore :: SystemT w m (B2Space Physics))

instance (MonadIO m) => ExplGet m (B2Space MotorSpeed) where
  explExists sp ety = liftIO $ jointIsKind sp ety motorSpeedKinds
  explGet sp ety = liftIO $ withJoint sp ety $ \j -> do
    ty <- B2Joint.getType j
    MotorSpeed <$> case ty of
      B2T.PrismaticJoint -> B2PrismaticJoint.getMotorSpeed j
      B2T.WheelJoint -> B2WheelJoint.getMotorSpeed j
      _ -> B2RevoluteJoint.getMotorSpeed j

instance (MonadIO m) => ExplSet m (B2Space MotorSpeed) where
  explSet sp ety (MotorSpeed v) = liftIO $
    overJoint sp ety $ \j -> do
      ty <- B2Joint.getType j
      case ty of
        B2T.RevoluteJoint -> B2RevoluteJoint.setMotorSpeed j v >> B2RevoluteJoint.enableMotor j True
        B2T.PrismaticJoint -> B2PrismaticJoint.setMotorSpeed j v >> B2PrismaticJoint.enableMotor j True
        B2T.WheelJoint -> B2WheelJoint.setMotorSpeed j v >> B2WheelJoint.enableMotor j True
        _ -> pure ()

instance (MonadIO m) => ExplMembers m (B2Space MotorSpeed) where
  explMembers sp = jointKindMembers sp motorSpeedKinds

{- | The motor's maximum torque on a 'Joint', usually in newton-meters:
a revolute joint's motor, or a wheel joint's spin motor ('WheelJoint',
suspension and limit not covered). Unlike 'MotorSpeed', setting this
only sets the cap — it does not enable the motor, so setting a cap
alone does not start it. Setting it on any other joint kind, or on an
entity with no 'Joint', is a silent no-op.
-}
newtype MotorMaxTorque = MotorMaxTorque Float
  deriving (Eq, Show)

-- | Joint kinds 'MotorMaxTorque' covers; keeps exists\/get\/members in sync.
motorMaxTorqueKinds :: [B2T.JointType]
motorMaxTorqueKinds = [B2T.RevoluteJoint, B2T.WheelJoint]

instance Component MotorMaxTorque where
  type Storage MotorMaxTorque = B2Space MotorMaxTorque

instance (MonadIO m, Has w m Physics) => Has w m MotorMaxTorque where
  getStore = cast <$> (getStore :: SystemT w m (B2Space Physics))

instance (MonadIO m) => ExplGet m (B2Space MotorMaxTorque) where
  explExists sp ety = liftIO $ jointIsKind sp ety motorMaxTorqueKinds
  explGet sp ety = liftIO $ withJoint sp ety $ \j -> do
    ty <- B2Joint.getType j
    MotorMaxTorque <$> case ty of
      B2T.WheelJoint -> B2WheelJoint.getMaxMotorTorque j
      _ -> B2RevoluteJoint.getMaxMotorTorque j

instance (MonadIO m) => ExplSet m (B2Space MotorMaxTorque) where
  explSet sp ety (MotorMaxTorque v) = liftIO $
    overJoint sp ety $ \j -> do
      ty <- B2Joint.getType j
      case ty of
        B2T.RevoluteJoint -> B2RevoluteJoint.setMaxMotorTorque j v
        B2T.WheelJoint -> B2WheelJoint.setMaxMotorTorque j v
        _ -> pure ()

instance (MonadIO m) => ExplMembers m (B2Space MotorMaxTorque) where
  explMembers sp = jointKindMembers sp motorMaxTorqueKinds

{- | The motor's maximum force on a prismatic 'Joint' ('PrismaticJoint',
'PrismaticSpringJoint', 'PrismaticMotorJoint'), usually in newtons.
Like 'MotorMaxTorque', setting this only sets the cap — it does not
enable the motor. Setting it on any other joint kind, or on an entity
with no 'Joint', is a silent no-op.
-}
newtype MotorMaxForce = MotorMaxForce Float
  deriving (Eq, Show)

-- | Joint kinds 'MotorMaxForce' covers; keeps exists\/get\/members in sync.
motorMaxForceKinds :: [B2T.JointType]
motorMaxForceKinds = [B2T.PrismaticJoint]

instance Component MotorMaxForce where
  type Storage MotorMaxForce = B2Space MotorMaxForce

instance (MonadIO m, Has w m Physics) => Has w m MotorMaxForce where
  getStore = cast <$> (getStore :: SystemT w m (B2Space Physics))

instance (MonadIO m) => ExplGet m (B2Space MotorMaxForce) where
  explExists sp ety = liftIO $ jointIsKind sp ety motorMaxForceKinds
  explGet sp ety = liftIO $ withJoint sp ety $ fmap MotorMaxForce . B2PrismaticJoint.getMaxMotorForce

instance (MonadIO m) => ExplSet m (B2Space MotorMaxForce) where
  explSet sp ety (MotorMaxForce v) = liftIO $
    overJoint sp ety $ \j -> do
      ty <- B2Joint.getType j
      case ty of
        B2T.PrismaticJoint -> B2PrismaticJoint.setMaxMotorForce j v
        _ -> pure ()

instance (MonadIO m) => ExplMembers m (B2Space MotorMaxForce) where
  explMembers sp = jointKindMembers sp motorMaxForceKinds

{- | The (lower, upper) limit range on a 'Joint': radians on a revolute
joint, meters on a prismatic joint, or the (minimum, maximum) length
in meters on a distance joint ('DistanceJoint', 'SpringJoint',
'SlideJoint'). Setting this also enables the limit; on a distance
joint the limit only has an effect while its spring is enabled (see
'SpringJoint'\/'SlideJoint'). Setting it on any other joint kind, or
on an entity with no 'Joint', is a silent no-op.
-}
data JointLimits = JointLimits !Float !Float
  deriving (Eq, Show)

-- | Joint kinds 'JointLimits' covers; keeps exists\/get\/members in sync.
jointLimitsKinds :: [B2T.JointType]
jointLimitsKinds = [B2T.RevoluteJoint, B2T.PrismaticJoint, B2T.DistanceJoint]

instance Component JointLimits where
  type Storage JointLimits = B2Space JointLimits

instance (MonadIO m, Has w m Physics) => Has w m JointLimits where
  getStore = cast <$> (getStore :: SystemT w m (B2Space Physics))

instance (MonadIO m) => ExplGet m (B2Space JointLimits) where
  explExists sp ety = liftIO $ jointIsKind sp ety jointLimitsKinds
  explGet sp ety = liftIO $ withJoint sp ety $ \j -> do
    ty <- B2Joint.getType j
    case ty of
      B2T.PrismaticJoint -> JointLimits <$> B2PrismaticJoint.getLowerLimit j <*> B2PrismaticJoint.getUpperLimit j
      B2T.DistanceJoint -> JointLimits <$> B2DistanceJoint.getMinLength j <*> B2DistanceJoint.getMaxLength j
      _ -> JointLimits <$> B2RevoluteJoint.getLowerLimit j <*> B2RevoluteJoint.getUpperLimit j

instance (MonadIO m) => ExplSet m (B2Space JointLimits) where
  explSet sp ety (JointLimits lo hi) = liftIO $
    overJoint sp ety $ \j -> do
      ty <- B2Joint.getType j
      case ty of
        B2T.RevoluteJoint -> B2RevoluteJoint.enableLimit j True >> B2RevoluteJoint.setLimits j lo hi
        B2T.PrismaticJoint -> B2PrismaticJoint.enableLimit j True >> B2PrismaticJoint.setLimits j lo hi
        B2T.DistanceJoint -> B2DistanceJoint.enableLimit j True >> B2DistanceJoint.setLengthRange j lo hi
        _ -> pure ()

instance (MonadIO m) => ExplMembers m (B2Space JointLimits) where
  explMembers sp = jointKindMembers sp jointLimitsKinds

{- | Whether the two bodies connected by a 'Joint' can collide with each
other. Applies to every joint kind. Restores the parity apecs-physics
has through @CollideBodies@. Setting it on an entity with no 'Joint'
is a silent no-op.
-}
newtype CollideConnected = CollideConnected Bool
  deriving (Eq, Show)

instance Component CollideConnected where
  type Storage CollideConnected = B2Space CollideConnected

instance (MonadIO m, Has w m Physics) => Has w m CollideConnected where
  getStore = cast <$> (getStore :: SystemT w m (B2Space Physics))

instance (MonadIO m) => ExplGet m (B2Space CollideConnected) where
  explExists = jointExists
  explGet sp ety = liftIO $ withJoint sp ety $ fmap CollideConnected . B2Joint.getCollideConnected

instance (MonadIO m) => ExplSet m (B2Space CollideConnected) where
  explSet sp ety (CollideConnected c) = liftIO $
    overJoint sp ety $
      \j -> B2Joint.setCollideConnected j c

instance (MonadIO m) => ExplMembers m (B2Space CollideConnected) where
  explMembers = jointMembers

{- | The constraint force a 'Joint' is exerting to hold, as of the last
'stepPhysics', usually in Newtons. Applies to every joint kind; useful
for breakage logic. Read-only: Box2D computes it during the step.
-}
newtype JointForce = JointForce Vec2
  deriving (Eq, Show)

instance Component JointForce where
  type Storage JointForce = B2Space JointForce

instance (MonadIO m, Has w m Physics) => Has w m JointForce where
  getStore = cast <$> (getStore :: SystemT w m (B2Space Physics))

instance (MonadIO m) => ExplGet m (B2Space JointForce) where
  explExists = jointExists
  explGet sp ety = liftIO $ withJoint sp ety $ fmap JointForce . B2Joint.getConstraintForce

instance (MonadIO m) => ExplMembers m (B2Space JointForce) where
  explMembers = jointMembers

{- | The constraint torque a 'Joint' is exerting to hold, as of the last
'stepPhysics', usually in Newton-meters. Applies to every joint kind;
useful for breakage logic. Read-only: Box2D computes it during the
step.
-}
newtype JointTorque = JointTorque Float
  deriving (Eq, Show)

instance Component JointTorque where
  type Storage JointTorque = B2Space JointTorque

instance (MonadIO m, Has w m Physics) => Has w m JointTorque where
  getStore = cast <$> (getStore :: SystemT w m (B2Space Physics))

instance (MonadIO m) => ExplGet m (B2Space JointTorque) where
  explExists = jointExists
  explGet sp ety = liftIO $ withJoint sp ety $ fmap JointTorque . B2Joint.getConstraintTorque

instance (MonadIO m) => ExplMembers m (B2Space JointTorque) where
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
  type Storage JointForceThreshold = B2Space JointForceThreshold

instance (MonadIO m, Has w m Physics) => Has w m JointForceThreshold where
  getStore = cast <$> (getStore :: SystemT w m (B2Space Physics))

instance (MonadIO m) => ExplGet m (B2Space JointForceThreshold) where
  explExists = jointExists
  explGet sp ety = liftIO $ withJoint sp ety $ fmap JointForceThreshold . B2Joint.getForceThreshold

instance (MonadIO m) => ExplSet m (B2Space JointForceThreshold) where
  explSet sp ety (JointForceThreshold t) = liftIO $
    overJoint sp ety $
      \j -> B2Joint.setForceThreshold j t

instance (MonadIO m) => ExplMembers m (B2Space JointForceThreshold) where
  explMembers = jointMembers

{- | The constraint torque a 'Joint' must exceed, in Newton-meters, for
the engine to report it in 'JointEvents'. Applies to every joint kind.
Defaults to @FLT_MAX@ (effectively off) until set. As with
'JointForceThreshold', the engine only raises the event and leaves the
joint intact.
-}
newtype JointTorqueThreshold = JointTorqueThreshold Float
  deriving (Eq, Show)

instance Component JointTorqueThreshold where
  type Storage JointTorqueThreshold = B2Space JointTorqueThreshold

instance (MonadIO m, Has w m Physics) => Has w m JointTorqueThreshold where
  getStore = cast <$> (getStore :: SystemT w m (B2Space Physics))

instance (MonadIO m) => ExplGet m (B2Space JointTorqueThreshold) where
  explExists = jointExists
  explGet sp ety = liftIO $ withJoint sp ety $ fmap JointTorqueThreshold . B2Joint.getTorqueThreshold

instance (MonadIO m) => ExplSet m (B2Space JointTorqueThreshold) where
  explSet sp ety (JointTorqueThreshold t) = liftIO $
    overJoint sp ety $
      \j -> B2Joint.setTorqueThreshold j t

instance (MonadIO m) => ExplMembers m (B2Space JointTorqueThreshold) where
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
@Filter 1 maxBound 0@ queries everything (the default shape category
is 1).
-}
toQueryFilter :: Filter -> IO B2T.QueryFilter
toQueryFilter f = do
  qf <- B2T.defaultQueryFilter
  pure
    qf
      { B2T.queryFilterCategoryBits = filterCategoryBits f
      , B2T.queryFilterMaskBits = filterMaskBits f
      }

{- | The shape and body entities behind an engine shape, if it is still
alive and registered (event buffers can reference shapes destroyed
after the step).
-}
shapeEntities :: B2Space c -> ShapeId -> IO (Maybe (Entity, Entity))
shapeEntities sp s = do
  alive <- B2Shape.isValid s
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
  sp :: B2Space Physics <- getStore
  liftIO $ do
    qf <- toQueryFilter fltr
    let
      Vec2 sx sy = start
      Vec2 ex ey = end
    res <- B2World.castRayClosest (spWorld sp) start (Vec2 (ex - sx) (ey - sy)) qf
    if B2T.rayResultHit res == 0 then
      pure Nothing
    else
      fmap
        ( \(shapeEty, bodyEty) ->
            RayHit
              { rayHitShape = shapeEty
              , rayHitBody = bodyEty
              , rayHitPoint = B2T.rayResultPoint res
              , rayHitNormal = B2T.rayResultNormal res
              , rayHitFraction = B2T.rayResultFraction res
              }
        )
        <$> shapeEntities sp (B2T.rayResultShapeId res)

{- | Every shape along a world-space segment, sorted nearest-first by
'rayHitFraction'. Filter semantics match 'segmentQuery'. Unlike
'segmentQuery', which goes through the engine's @b2World_CastRayClosest@
convenience path, this drives the general @b2World_CastRay@ callback
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
  sp :: B2Space Physics <- getStore
  liftIO $ do
    qf <- toQueryFilter fltr
    found <- newIORef []
    let
      Vec2 sx sy = start
      Vec2 ex ey = end
      visit s point normal frac = do
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
      B2World.castRay (spWorld sp) start (Vec2 (ex - sx) (ey - sy)) qf fp ctx
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
  sp :: B2Space Physics <- getStore
  liftIO $ do
    qf <- toQueryFilter fltr
    found <- newIORef IS.empty
    let visit s = do
          hit <- shapeEntities sp s
          forM_ hit $ \(_, Entity bodyIx) -> modifyIORef' found (IS.insert bodyIx)
          pure True
    _ <- withOverlapResultFcn visit $ \fp ctx ->
      B2World.overlapAABB (spWorld sp) vec2Zero box qf fp ctx
    map Entity . IS.toList <$> readIORef found
  where
    Vec2 ax ay = cornerA
    Vec2 bx by = cornerB
    box = AABB (Vec2 (min ax bx) (min ay by)) (Vec2 (max ax bx) (max ay by))

{- | 'aabbQuery' of the square reaching @r@ along each axis from a
point: the body entities with shapes broad-phase within reach. This is
broad-phase AABB reach, /not/ exact containment — a shape's AABB is
larger than the shape itself, so this can return bodies whose shape
doesn't actually contain the point. See 'containsPointQuery' for the
exact test.
-}
pointQuery :: (MonadIO m, Has w m Physics) => WVec -> Float -> Filter -> SystemT w m [Entity]
pointQuery (Vec2 x y) r =
  aabbQuery (Vec2 (x - r) (y - r)) (Vec2 (x + r) (y + r))

{- | The body entities with a shape that actually contains a world point:
an exact geometry test, unlike the broad-phase 'pointQuery'. Candidates
come from a broad-phase 'B2World.overlapAABB' at a degenerate (zero-size)
AABB pinned to the point — the engine's AABB validity check only requires
@upper - lower >= 0@, so a point AABB is accepted — and each candidate
shape is then refined with 'B2Shape.testPoint', an exact point-in-shape
test; bodies are deduplicated when more than one of their shapes contains
the point.
-}
containsPointQuery
  :: forall w m
   . (MonadIO m, Has w m Physics)
  => WVec
  -> Filter
  -> SystemT w m [Entity]
containsPointQuery point fltr = do
  sp :: B2Space Physics <- getStore
  liftIO $ do
    qf <- toQueryFilter fltr
    found <- newIORef IS.empty
    let visit s = do
          -- the exact test first: most broad-phase candidates only
          -- overlap by AABB, and testPoint is one FFI call while entity
          -- resolution is two plus a registry lookup
          inside <- B2Shape.testPoint s point
          when inside $ do
            hit <- shapeEntities sp s
            forM_ hit $ \(_, Entity bodyIx) -> modifyIORef' found (IS.insert bodyIx)
          pure True
    _ <- withOverlapResultFcn visit $ \fp ctx ->
      B2World.overlapAABB (spWorld sp) vec2Zero (AABB point point) qf fp ctx
    map Entity . IS.toList <$> readIORef found

-- * Character mover

-- | @sample_character.cpp@'s @Mover::m_planeCapacity@: at most this many planes are kept per step.
planeCapacity :: Int
planeCapacity = 8

-- | @sample_character.cpp@'s outer collide\/solve\/cast loop count.
moverStepIterations :: Int
moverStepIterations = 5

-- | @sample_character.cpp@'s per-iteration break tolerance on the swept translation.
moverStepTolerance :: Float
moverStepTolerance = 0.01

vecAdd :: Vec2 -> Vec2 -> Vec2
vecAdd (Vec2 ax ay) (Vec2 bx by) = Vec2 (ax + bx) (ay + by)

vecSub :: Vec2 -> Vec2 -> Vec2
vecSub (Vec2 ax ay) (Vec2 bx by) = Vec2 (ax - bx) (ay - by)

vecScale :: Float -> Vec2 -> Vec2
vecScale s (Vec2 x y) = Vec2 (s * x) (s * y)

vecDot :: Vec2 -> Vec2 -> Float
vecDot (Vec2 ax ay) (Vec2 bx by) = ax * bx + ay * by

vecLenSq :: Vec2 -> Float
vecLenSq v = vecDot v v

{- | A 'B2T.PlaneResult' as a fresh 'B2T.CollisionPlane' for
"Box2D.Mover": no push limit (the sample's per-shape @maxPush@ user
data isn't exposed here) and velocity always clipped.
-}
mkCollisionPlane :: B2T.PlaneResult -> B2T.CollisionPlane
mkCollisionPlane pr =
  B2T.CollisionPlane
    { B2T.collisionPlanePlane = B2T.planeResultPlane pr
    , B2T.collisionPlanePushLimit = 1 / 0
    , B2T.collisionPlanePush = 0
    , B2T.collisionPlaneClipVelocity = fromBool True
    }

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

Mirrors @sample_character.cpp@'s @Mover@ faithfully: up to 5
collide\/solve\/cast iterations, breaking early once a step's swept
translation is shorter than 0.01 units; each iteration gathers up to 8
collision planes fresh via 'B2World.collideMover', resolves the target
delta against them with the engine's own solver ("Box2D.Mover"'s
'B2Mover.solvePlanes'), and sweeps the resolved translation with
'B2World.castMover'. The final velocity is clipped
('B2Mover.clipVector') against the planes gathered in whichever
iteration ran last — same as the sample, which never clears its plane
buffer after the loop exits. Every plane is treated as unlimited push
with clipping on; the sample's per-shape @maxPush@\/@clipVelocity@ come
from shape user data, which this layer doesn't expose.
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
  sp :: B2Space Physics <- getStore
  liftIO $ do
    qf <- toQueryFilter fltr
    let capsule = B2T.Capsule c1 c2 radius
    -- (count, planes gathered so far this iteration); reset before every
    -- 'B2World.collideMover' call so the FunPtr below can be wrapped once
    -- for the whole call instead of once per iteration.
    gatherRef <- newIORef (0 :: Int, [] :: [B2T.CollisionPlane])
    let visit _shapeId pr = do
          when (toBool (B2T.planeResultHit pr)) $
            modifyIORef' gatherRef $ \(n, ps) ->
              if n >= planeCapacity then (n, ps) else (n + 1, mkCollisionPlane pr : ps)
          pure True
    withPlaneResultFcn visit $ \fp ctx -> do
      let
        gatherPlanes pos = do
          writeIORef gatherRef (0, [])
          _ <- B2World.collideMover (spWorld sp) pos capsule qf fp ctx
          (_, ps) <- readIORef gatherRef
          pure (VS.fromList (reverse ps))

        step i pos lastPlanes
          | i >= moverStepIterations = pure (pos, lastPlanes)
          | otherwise = do
              planes <- gatherPlanes pos
              (translation, planes', _iters) <- B2Mover.solvePlanes (vecSub target pos) planes
              fraction <- B2World.castMover (spWorld sp) pos capsule translation qf
              let
                delta = vecScale fraction translation
                pos' = vecAdd pos delta
              if vecLenSq delta < moverStepTolerance * moverStepTolerance then
                pure (pos', planes')
              else
                step (i + 1) pos' planes'

      (finalPos, finalPlanes) <- step (0 :: Int) pos0 VS.empty
      finalVel <- B2Mover.clipVector vel0 finalPlanes
      pure (MoverResult finalPos finalVel)

-- * Recording

{- | A recording buffer for a 'Physics' world: hand it to 'startRecording'
to capture a session, 'stopRecording' to end it, then either
'saveRecording' it to disk or 'validateRecording' it in place. Not
managed automatically like the store's other engine handles — the
engine may still be writing into the buffer while a recording is in
progress, so an automatic finalizer could race the writer. Call
'destroyRecording' yourself once you are done with it.
-}
newtype Recording = Recording (Ptr B2Tags.Recording)

{- | Create a recording buffer with a starting capacity in bytes; pass 0
for the engine's small default. The buffer grows on demand as
'startRecording' writes into it, so this is only a pre-sizing hint for a
session of known length.
-}
newRecording :: (MonadIO m) => Int -> m Recording
newRecording capacity = liftIO $ Recording <$> B2Recording.create capacity

{- | Free a recording buffer's memory. Do not use the handle again
afterwards, and do not call this while a recording is still in progress
— 'stopRecording' it first.
-}
destroyRecording :: (MonadIO m) => Recording -> m ()
destroyRecording (Recording p) = liftIO $ B2Recording.destroy p

{- | Begin recording every mutation applied to the world into the given
buffer — the basis for deterministic capture\/replay: record a session,
save it, and later confirm with 'validateRecording' that a replay
reproduces it bit-for-bit, which makes a solid regression test for
physics behaviour in place of eyeballing it. Start before the first
'stepPhysics' to capture the whole session. The buffer must outlive the
recording session: do not 'destroyRecording' it before 'stopRecording'.
-}
startRecording :: forall w m. (MonadIO m, Has w m Physics) => Recording -> SystemT w m ()
startRecording (Recording p) = do
  sp :: B2Space Physics <- getStore
  liftIO $ B2World.startRecording (spWorld sp) p

{- | End the recording session started by 'startRecording'. The buffer
keeps its recorded bytes; save or validate it, then 'destroyRecording'
it when you are done.
-}
stopRecording :: forall w m. (MonadIO m, Has w m Physics) => SystemT w m ()
stopRecording = do
  sp :: B2Space Physics <- getStore
  liftIO $ B2World.stopRecording (spWorld sp)

-- | Save a recording's bytes to a file. Returns 'False' if the file could not be written.
saveRecording :: (MonadIO m) => Recording -> FilePath -> m Bool
saveRecording (Recording p) path = liftIO $ withCString path (B2Collision.saveRecordingToFile p)

{- | Load a recording previously written by 'saveRecording'. Returns
'Nothing' if the file does not exist or is not a valid recording.
Destroy the result with 'destroyRecording' once you are done with it.
-}
loadRecording :: (MonadIO m) => FilePath -> m (Maybe Recording)
loadRecording path = liftIO $ do
  p <- withCString path B2Collision.loadRecordingFromFile
  pure $ if p == nullPtr then Nothing else Just (Recording p)

{- | Replay a recording by re-running the engine and checking it
reproduces the recorded session exactly: 'True' means the replay matched
bit-for-bit, 'False' means it diverged somewhere. @workerCount@ selects
how many worker threads the replay runs with; 0 falls back to the serial
single-worker path.
-}
validateRecording :: (MonadIO m) => Recording -> Int -> m Bool
validateRecording (Recording p) workerCount = liftIO $ do
  dat <- B2Recording.getData p
  size <- B2Recording.getSize p
  B2Collision.validateReplay (castPtr dat) size workerCount

-- * Snapshot

{- | A saved simulation state of a 'Physics' world, produced by
'snapshotWorld' and restorable into the world it came from with
'restoreWorld'. [2D-only]: the 3D engine exposes no equivalent
snapshot\/restore mechanism. Unlike 'Recording', the engine only touches
the underlying buffer during the 'snapshotWorld' call itself, so this
handle is safe to manage automatically — its memory is freed once no
'Snapshot' value references it any more.
-}
data Snapshot = Snapshot !(ForeignPtr Word8) !Int

{- | Serialize the world's current simulation state into a fresh
'Snapshot', for saving or transmitting and restoring later with
'restoreWorld'. Must be called at a step boundary, not from inside a
callback mid-'stepPhysics'; returns 'Nothing' if the world was mid-step
when asked.
-}
snapshotWorld :: forall w m. (MonadIO m, Has w m Physics) => SystemT w m (Maybe Snapshot)
snapshotWorld = do
  sp :: B2Space Physics <- getStore
  liftIO $ do
    let wid = spWorld sp
    need <- B2World.snapshot wid nullPtr 0
    if need <= 0 then
      pure Nothing
    else do
      fp <- mallocForeignPtrBytes need
      written <- withForeignPtr fp $ \buf -> B2World.snapshot wid buf need
      pure $ if written /= need then Nothing else Just (Snapshot fp need)

{- | Restore the world in place from a 'Snapshot' taken from it earlier.
'B2BodyId'\/'B2ShapeId'\/'B2JointId' values held from objects that
existed at snapshot time stay valid; anything created since is gone.
Restore into the same world the snapshot came from — held ids are only
meaningful there. Must be called at a step boundary. Returns 'False' on
a rejected image (bad magic\/version\/layout), which leaves the world
unchanged; a corrupt payload detected mid-rebuild also returns 'False'
but leaves the world unusable, so 'destroyPhysics' it in that case.

The wrapper's entity registries are /not/ rewound with the engine:
entities whose bodies, shapes or joints were created after the snapshot
keep now-dead engine ids, and engine objects the restore resurrects are
not re-registered. Restoring is therefore only safe while the set of
physics entities is unchanged since the snapshot — rewinding a fixed
scene, not undoing spawns and despawns.
-}
restoreWorld :: forall w m. (MonadIO m, Has w m Physics) => Snapshot -> SystemT w m Bool
restoreWorld (Snapshot fp size) = do
  sp :: B2Space Physics <- getStore
  liftIO $ withForeignPtr fp $ \p -> B2World.restore (spWorld sp) p size

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
toCollision :: B2Space c -> ShapeId -> ShapeId -> IO (Maybe Collision)
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
  type Storage Collisions = B2Space Collisions

instance (MonadIO m, Has w m Physics) => Has w m Collisions where
  getStore = cast <$> (getStore :: SystemT w m (B2Space Physics))

instance (MonadIO m) => ExplGet m (B2Space Collisions) where
  explExists _ _ = pure True
  explGet sp _ = liftIO $ do
    evs <- B2Events.contactBeginTouchEvents (spWorld sp)
    fmap (Collisions . catMaybes) . forM (VS.toList evs) $ \ev ->
      toCollision sp (B2T.contactBeginTouchEventShapeIdA ev) (B2T.contactBeginTouchEventShapeIdB ev)

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
  type Storage CollisionsEnd = B2Space CollisionsEnd

instance (MonadIO m, Has w m Physics) => Has w m CollisionsEnd where
  getStore = cast <$> (getStore :: SystemT w m (B2Space Physics))

instance (MonadIO m) => ExplGet m (B2Space CollisionsEnd) where
  explExists _ _ = pure True
  explGet sp _ = liftIO $ do
    evs <- B2Events.contactEndTouchEvents (spWorld sp)
    fmap (CollisionsEnd . catMaybes) . forM (VS.toList evs) $ \ev ->
      toCollision sp (B2T.contactEndTouchEventShapeIdA ev) (B2T.contactEndTouchEventShapeIdB ev)

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
toSensorEvent :: B2Space c -> ShapeId -> ShapeId -> IO (Maybe SensorEvent)
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
  type Storage JointEvents = B2Space JointEvents

instance (MonadIO m, Has w m Physics) => Has w m JointEvents where
  getStore = cast <$> (getStore :: SystemT w m (B2Space Physics))

instance (MonadIO m) => ExplGet m (B2Space JointEvents) where
  explExists _ _ = pure True
  explGet sp _ = liftIO $ do
    evs <- B2Events.jointEvents (spWorld sp)
    fmap (JointEvents . catMaybes) . forM (VS.toList evs) $ \ev ->
      jointEntity sp (B2T.jointEventJointId ev)

{- | A body that moved during the last 'stepPhysics': its entity, its new
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

{- | The bodies that moved during the last 'stepPhysics', a read-only
global: @Moved moves <- get global@ after stepping. Iterating this
instead of every 'Position' makes render sync O(moved) instead of
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
