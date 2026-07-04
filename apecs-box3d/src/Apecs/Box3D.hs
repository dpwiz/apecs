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
  , stepPhysics
  , destroyPhysics
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
  , LinearDamping (..)
  , AngularDamping (..)
  , GravityScale (..)
  , B3BodyId (..)

    -- * Shape
  , Geometry (..)
  , Shape (..)
  , Density (..)
  , Friction (..)
  , Elasticity (..)
  , CollisionFilter (..)
  , Filter (..)
  , B3ShapeId (..)

    -- * Joint
  , JointSpec (..)
  , Joint (..)
  , B3JointId (..)

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
import Control.Monad (forM_, when)
import Control.Monad.IO.Class (MonadIO)
import Data.IORef
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IM
import Data.Vector.Storable qualified as VS
import Data.Vector.Unboxed qualified as U
import Foreign.Ptr (nullPtr)

import Box3D.Body qualified as B3Body
import Box3D.DistanceJoint qualified as B3DistanceJoint
import Box3D.Hull qualified as B3Hull
import Box3D.Id (BodyId, JointId, ShapeId, WorldId)
import Box3D.Joint qualified as B3Joint
import Box3D.MathTypes (Quat (..), Transform (..), Vec3 (..), quatIdentity, vec3Zero)
import Box3D.Shape qualified as B3Shape
import Box3D.SphericalJoint qualified as B3SphericalJoint
import Box3D.Types (Filter (..))
import Box3D.Types qualified as B3T
import Box3D.UserData (setUserIndex)
import Box3D.WeldJoint qualified as B3WeldJoint
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
    B3Space
      <$> B3World.create wd
      <*> B3T.defaultBodyDef
      <*> B3T.defaultShapeDef
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
teardown.
-}
destroyPhysics :: forall w m. (MonadIO m, Has w m Physics) => SystemT w m ()
destroyPhysics = do
  sp :: B3Space Physics <- getStore
  liftIO $ do
    B3World.destroy (spWorld sp)
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

jointExists :: (MonadIO m) => B3Space c -> Int -> m Bool
jointExists sp = regExists (spJoints sp)

jointMembers :: (MonadIO m) => B3Space c -> m (U.Vector Int)
jointMembers sp = regMembers (spJoints sp)

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

-- | A 'Body'\'s orientation quaternion.
newtype Rotation = Rotation Quat
  deriving (Eq, Show)

instance Component Rotation where
  type Storage Rotation = B3Space Rotation

instance (MonadIO m, Has w m Physics) => Has w m Rotation where
  getStore = cast <$> (getStore :: SystemT w m (B3Space Physics))

instance (MonadIO m) => ExplGet m (B3Space Rotation) where
  explExists = bodyExists
  explGet sp ety = liftIO $ withBody sp ety $ fmap Rotation . B3Body.getRotation

instance (MonadIO m) => ExplSet m (B3Space Rotation) where
  explSet sp ety (Rotation q) = liftIO $
    overBody sp ety $ \b -> do
      pos <- B3Body.getPosition b
      B3Body.setTransform b pos q

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

-- Shape ---------------------------------------------------------------------

-- | Shape geometry in body-local coordinates.
data Geometry
  = -- | Center and radius.
    GeoSphere BVec Float
  | {- | The two hemisphere centers and the radius around the segment
    between them.
    -}
    GeoCapsule BVec BVec Float
  | -- | A box from a local center and half-extents along each axis.
    GeoBox BVec Vec3
  | {- | The convex hull of at least 4 points. Setting a degenerate
    (coplanar) point set raises an error.
    -}
    GeoHull (VS.Vector Vec3)
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
data, so the intermediate hull is destroyed right after.
-}
createHullShape :: BodyId -> B3T.ShapeDef -> VS.Vector Vec3 -> IO ShapeId
createHullShape b sd pts = do
  let n = VS.length pts
  when (n < 4) $
    error ("GeoHull needs at least 4 points, got " <> show n)
  hull <- VS.unsafeWith pts $ \p -> B3Hull.create p n n
  when (hull == nullPtr) $
    error "GeoHull points are degenerate (coplanar or coincident)"
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

instance (MonadIO m) => ExplSet m (B3Space Shape) where
  explSet sp ety shape@(Shape (Entity bEty) geo) = liftIO $
    overBody sp bEty $ \b -> do
      old <- IM.lookup ety <$> readIORef (spShapes sp)
      -- carry the material state over the recreate; the old shape is only
      -- destroyed after the new one exists, so a failed create (e.g. a bad
      -- hull) leaves everything intact
      sd <- case old of
        Nothing -> pure (spShapeDef sp)
        Just (ShapeRecord s _) -> do
          material <- B3Shape.getSurfaceMaterial s
          density <- B3Shape.getDensity s
          filtr <- B3Shape.getFilter s
          pure
            (spShapeDef sp)
              { B3T.shapeDefBaseMaterial = material
              , B3T.shapeDefDensity = density
              , B3T.shapeDefFilter = filtr
              }
      s <- case geo of
        GeoSphere c r -> B3Shape.createSphere b sd (B3T.Sphere c r)
        GeoCapsule c1 c2 r -> B3Shape.createCapsule b sd (B3T.Capsule c1 c2 r)
        GeoBox c half -> createHullShape b sd (boxCorners c half)
        GeoHull pts -> createHullShape b sd pts
      setUserIndex s ety
      forM_ old $ \(ShapeRecord s' _) -> B3Shape.destroy s' True
      modifyIORef' (spShapes sp) (IM.insert ety (ShapeRecord s shape))

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

-- Joint ----------------------------------------------------------------------

{- | A joint between two bodies, specified in world space at creation
time. Joint frames are derived from the given world points with zero
reference rotation.
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
  deriving (Eq, Show)

{- | Gives an entity a joint connecting the 'Body's of the two given
entities. Reads return the exact value written.
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

{- | Fill a joint def's base with the two bodies and their frames at a
shared world anchor.
-}
baseAt :: B3T.JointDef -> BodyId -> BodyId -> Vec3 -> IO B3T.JointDef
baseAt jd a b p = do
  fa <- frameAt a p
  fb <- frameAt b p
  pure
    jd
      { B3T.jointDefBodyIdA = a
      , B3T.jointDefBodyIdB = b
      , B3T.jointDefLocalFrameA = fa
      , B3T.jointDefLocalFrameB = fb
      }

createJoint :: WorldId -> BodyId -> BodyId -> JointSpec -> IO JointId
createJoint w a b spec = case spec of
  PivotJoint p -> do
    jd <- B3T.defaultSphericalJointDef
    base <- baseAt (B3T.sphericalJointDefBase jd) a b p
    B3SphericalJoint.create w jd{B3T.sphericalJointDefBase = base}
  DistanceJoint pA pB -> do
    jd <- B3T.defaultDistanceJointDef
    fa <- frameAt a pA
    fb <- frameAt b pB
    let
      Vec3 x1 y1 z1 = pA
      Vec3 x2 y2 z2 = pB
      len = sqrt ((x2 - x1) ^ two + (y2 - y1) ^ two + (z2 - z1) ^ two)
      two = 2 :: Int
      base =
        (B3T.distanceJointDefBase jd)
          { B3T.jointDefBodyIdA = a
          , B3T.jointDefBodyIdB = b
          , B3T.jointDefLocalFrameA = fa
          , B3T.jointDefLocalFrameB = fb
          }
    B3DistanceJoint.create w jd{B3T.distanceJointDefBase = base, B3T.distanceJointDefLength = len}
  WeldJoint p -> do
    jd <- B3T.defaultWeldJointDef
    base <- baseAt (B3T.weldJointDefBase jd) a b p
    B3WeldJoint.create w jd{B3T.weldJointDefBase = base}

instance (MonadIO m) => ExplSet m (B3Space Joint) where
  explSet sp ety joint@(Joint (Entity aEty) (Entity bEty) spec) = liftIO $ do
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
