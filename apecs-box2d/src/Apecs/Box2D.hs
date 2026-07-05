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
'getWorldId' together with the "Box2D" modules.
-}
module Apecs.Box2D
  ( -- * World
    Physics
  , B2Space
  , Gravity (..)
  , earthGravity
  , Substeps (..)
  , SleepingEnabled (..)
  , stepPhysics
  , destroyPhysics
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
  , LinearDamping (..)
  , AngularDamping (..)
  , GravityScale (..)
  , BulletBody (..)
  , Awake (..)
  , SleepEnabled (..)
  , SleepThreshold (..)
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

    -- * Joint
  , JointSpec (..)
  , Joint (..)
  , B2JointId (..)

    -- * Queries
  , RayHit (..)
  , segmentQuery
  , aabbQuery
  , pointQuery

    -- * Collisions
  , Collision (..)
  , Collisions (..)
  , CollisionsEnd (..)
  , Impact (..)
  , Impacts (..)
  , SensorEvent (..)
  , SensorEvents (..)

    -- * Vectors
  , Vec2 (..)
  , vec2Zero
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
import Data.Maybe (catMaybes)
import Data.Vector.Storable qualified as VS
import Data.Vector.Unboxed qualified as U
import Foreign.Marshal.Utils (fromBool)

import Box2D.Body qualified as B2Body
import Box2D.Callbacks (withOverlapResultFcn)
import Box2D.Collision qualified as B2Collision
import Box2D.DistanceJoint qualified as B2DistanceJoint
import Box2D.Events qualified as B2Events
import Box2D.Id (BodyId, JointId, ShapeId, WorldId)
import Box2D.Joint qualified as B2Joint
import Box2D.MathFunctions (makeRot, rotGetAngle)
import Box2D.MathTypes (AABB (..), Rot (..), Transform (..), Vec2 (..), vec2Zero)
import Box2D.RevoluteJoint qualified as B2RevoluteJoint
import Box2D.Shape qualified as B2Shape
import Box2D.Types (Filter (..))
import Box2D.Types qualified as B2T
import Box2D.UserData (getUserIndex, setUserIndex)
import Box2D.WeldJoint qualified as B2WeldJoint
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

{- | The store shared by 'Physics' and all its sub-components: the engine
world plus entity registries for bodies, shapes and joints.
-}
data B2Space c = B2Space
  { spWorld :: !WorldId
  , spBodyDef :: !B2T.BodyDef
  , spShapeDef :: !B2T.ShapeDef
  , spBodies :: !(IORef (IntMap BodyId))
  , spShapes :: !(IORef (IntMap ShapeRecord))
  , spJoints :: !(IORef (IntMap JointRecord))
  , spSubsteps :: !(IORef Int)
  }

cast :: B2Space a -> B2Space b
cast (B2Space w bd sd b s j i) = B2Space w bd sd b s j i

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

jointExists :: (MonadIO m) => B2Space c -> Int -> m Bool
jointExists sp = regExists (spJoints sp)

jointMembers :: (MonadIO m) => B2Space c -> m (U.Vector Int)
jointMembers sp = regMembers (spJoints sp)

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
      -- the engine destroys attached shapes and joints along with the
      -- body, so drop their entity records too
      modifyIORef' (spShapes sp) (IM.filter (\(ShapeRecord _ (Shape (Entity be) _)) -> be /= ety))
      modifyIORef' (spJoints sp) (IM.filter (\(JointRecord _ (Joint (Entity a) (Entity b') _)) -> a /= ety && b' /= ety))
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
  | {- | The convex hull of 3 to 'B2T.maxPolygonVertices' points. Setting
    an out-of-range or degenerate (collinear) point set raises an error.
    -}
    GeoPolygon (VS.Vector Vec2)
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

-- | Create the engine geometry for a 'Geometry' value on a body.
createGeometry :: BodyId -> B2T.ShapeDef -> Geometry -> IO ShapeId
createGeometry b sd geo = case geo of
  GeoCircle c r -> B2Shape.createCircle b sd (B2T.Circle c r)
  GeoCapsule c1 c2 r -> B2Shape.createCapsule b sd (B2T.Capsule c1 c2 r)
  GeoSegment p1 p2 -> B2Shape.createSegment b sd (B2T.Segment p1 p2)
  GeoBox hw hh -> B2Collision.makeBox hw hh >>= B2Shape.createPolygon b sd
  GeoPolygon pts -> do
    let n = VS.length pts
    when (n < 3 || n > B2T.maxPolygonVertices) $
      error ("GeoPolygon needs 3 to " <> show B2T.maxPolygonVertices <> " points, got " <> show n)
    hull <- VS.unsafeWith pts $ \p -> B2Collision.computeHull p n
    when (VS.length (B2T.hullPoints hull) < 3) $
      error "GeoPolygon points are degenerate (collinear or coincident)"
    B2Collision.makePolygon hull 0 >>= B2Shape.createPolygon b sd

{- | A shape def with the surface material, density and filter carried
over from the shape being replaced, if any.
-}
carryMaterial :: B2T.ShapeDef -> Maybe ShapeRecord -> IO B2T.ShapeDef
carryMaterial sd Nothing = pure sd
carryMaterial sd (Just (ShapeRecord s _)) = do
  material <- B2Shape.getSurfaceMaterial s
  density <- B2Shape.getDensity s
  filtr <- B2Shape.getFilter s
  pure
    sd
      { B2T.shapeDefMaterial = material
      , B2T.shapeDefDensity = density
      , B2T.shapeDefFilter = filtr
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
already has is a no-op. Reads reflect the engine.
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
reference rotation.
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
    MotorJoint WVec Float Float
  deriving (Eq, Show)

{- | Gives an entity a joint connecting the 'Body's of the two given
entities, which must be distinct (the engine rejects self-joints;
setting one is a silent no-op). Reads return the exact value written.
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
  MotorJoint p speed maxTorque ->
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
point: the body entities with shapes broad-phase within reach.
-}
pointQuery :: (MonadIO m, Has w m Physics) => WVec -> Float -> Filter -> SystemT w m [Entity]
pointQuery (Vec2 x y) r =
  aabbQuery (Vec2 (x - r) (y - r)) (Vec2 (x + r) (y + r))

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
tune with 'Box2D.World.setHitEventThreshold' via 'getWorldId').
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
