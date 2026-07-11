{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
-- 'Joint' itself lives in "Apecs.Box3D.Types" — the store's joint
-- registry embeds it — so its instances here are orphans.
{-# OPTIONS_GHC -Wno-orphans #-}

{-| The 'Joint' component, engine joint creation from 'JointSpec', and
the joint tuning sub-components ('MotorSpeed', 'JointLimits', ...).
-}
module Apecs.Box3D.Joint where

import Apecs
import Apecs.Core
import Control.Monad (forM_, when)
import Control.Monad.IO.Class (MonadIO)
import Data.IORef
import Data.IntMap.Strict qualified as IM

import Box3D.Body qualified as B3Body
import Box3D.DistanceJoint qualified as B3DistanceJoint
import Box3D.Id (BodyId, JointId, WorldId)
import Box3D.Joint qualified as B3Joint
import Box3D.MathFunctions (computeQuatBetweenUnitVectors)
import Box3D.MathTypes (Quat (..), Transform (..), Vec3 (..))
import Box3D.PrismaticJoint qualified as B3PrismaticJoint
import Box3D.RevoluteJoint qualified as B3RevoluteJoint
import Box3D.SphericalJoint qualified as B3SphericalJoint
import Box3D.Types qualified as B3T
import Box3D.UserData (setUserIndex)
import Box3D.WeldJoint qualified as B3WeldJoint
import Box3D.WheelJoint qualified as B3WheelJoint

import Apecs.Box3D.Geometry
import Apecs.Box3D.Types

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
    bodies <- readIORef sp.bodies
    forM_ ((,) <$> IM.lookup aEty bodies <*> IM.lookup bEty bodies) $ \(a, b) -> do
      old <- IM.lookup ety <$> readIORef sp.joints
      j <- createJoint sp.world a b spec
      setUserIndex j ety
      forM_ old $ \(JointRecord j' _) -> B3Joint.destroy j' True
      modifyIORef' sp.joints (IM.insert ety (JointRecord j joint))

instance (MonadIO m) => ExplGet m (B3Space Joint) where
  explExists = jointExists
  explGet sp ety = liftIO $
    withReg "Joint" sp.joints ety $
      \(JointRecord _ joint) -> pure joint

instance (MonadIO m) => ExplDestroy m (B3Space Joint) where
  explDestroy sp ety = liftIO $ do
    joints <- readIORef sp.joints
    forM_ (IM.lookup ety joints) $ \(JointRecord j _) -> do
      modifyIORef' sp.joints (IM.delete ety)
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

-- | Joint kinds 'MotorSpeed' covers; keeps exists\/get\/members in sync.
motorSpeedKinds :: [B3T.JointType]
motorSpeedKinds = [B3T.RevoluteJoint, B3T.PrismaticJoint, B3T.WheelJoint]

instance Component MotorSpeed where
  type Storage MotorSpeed = B3Space MotorSpeed

instance (MonadIO m, Has w m Physics) => Has w m MotorSpeed where
  getStore = cast <$> (getStore :: SystemT w m (B3Space Physics))

instance (MonadIO m) => ExplGet m (B3Space MotorSpeed) where
  explExists sp ety = liftIO $ jointIsKind sp ety motorSpeedKinds
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
  explMembers sp = jointKindMembers sp motorSpeedKinds

{- | The motor's maximum torque on a 'Joint', usually in newton-meters:
a hinge (revolute) joint's motor, or a wheel joint's spin motor
('WheelJoint', suspension and steering not covered). Unlike
'MotorSpeed', setting this only sets the cap — it does not enable the
motor, so setting a cap alone does not start it. Setting it on any
other joint kind, or on an entity with no 'Joint', is a silent no-op.
-}
newtype MotorMaxTorque = MotorMaxTorque Float
  deriving (Eq, Show)

-- | Joint kinds 'MotorMaxTorque' covers; keeps exists\/get\/members in sync.
motorMaxTorqueKinds :: [B3T.JointType]
motorMaxTorqueKinds = [B3T.RevoluteJoint, B3T.WheelJoint]

instance Component MotorMaxTorque where
  type Storage MotorMaxTorque = B3Space MotorMaxTorque

instance (MonadIO m, Has w m Physics) => Has w m MotorMaxTorque where
  getStore = cast <$> (getStore :: SystemT w m (B3Space Physics))

instance (MonadIO m) => ExplGet m (B3Space MotorMaxTorque) where
  explExists sp ety = liftIO $ jointIsKind sp ety motorMaxTorqueKinds
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
motorMaxForceKinds :: [B3T.JointType]
motorMaxForceKinds = [B3T.PrismaticJoint]

instance Component MotorMaxForce where
  type Storage MotorMaxForce = B3Space MotorMaxForce

instance (MonadIO m, Has w m Physics) => Has w m MotorMaxForce where
  getStore = cast <$> (getStore :: SystemT w m (B3Space Physics))

instance (MonadIO m) => ExplGet m (B3Space MotorMaxForce) where
  explExists sp ety = liftIO $ jointIsKind sp ety motorMaxForceKinds
  explGet sp ety = liftIO $ withJoint sp ety $ fmap MotorMaxForce . B3PrismaticJoint.getMaxMotorForce

instance (MonadIO m) => ExplSet m (B3Space MotorMaxForce) where
  explSet sp ety (MotorMaxForce v) = liftIO $
    overJoint sp ety $ \j -> do
      ty <- B3Joint.getType j
      case ty of
        B3T.PrismaticJoint -> B3PrismaticJoint.setMaxMotorForce j v
        _ -> pure ()

instance (MonadIO m) => ExplMembers m (B3Space MotorMaxForce) where
  explMembers sp = jointKindMembers sp motorMaxForceKinds

{- | The (lower, upper) limit range on a 'Joint': radians on a hinge
(revolute) joint, or meters on a prismatic joint. Setting this also
enables the limit. Setting it on any other joint kind, or on an
entity with no 'Joint', is a silent no-op.
-}
data JointLimits = JointLimits !Float !Float
  deriving (Eq, Show)

-- | Joint kinds 'JointLimits' covers; keeps exists\/get\/members in sync.
jointLimitsKinds :: [B3T.JointType]
jointLimitsKinds = [B3T.RevoluteJoint, B3T.PrismaticJoint]

instance Component JointLimits where
  type Storage JointLimits = B3Space JointLimits

instance (MonadIO m, Has w m Physics) => Has w m JointLimits where
  getStore = cast <$> (getStore :: SystemT w m (B3Space Physics))

instance (MonadIO m) => ExplGet m (B3Space JointLimits) where
  explExists sp ety = liftIO $ jointIsKind sp ety jointLimitsKinds
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
  explMembers sp = jointKindMembers sp jointLimitsKinds

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
'Apecs.Box3D.Space.stepPhysics'. Applies to every joint kind; useful for breakage logic.
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
'Apecs.Box3D.Space.stepPhysics'. Unlike the 2D binding, Box3D's constraint torque is a
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
engine to report it in 'Apecs.Box3D.Collision.JointEvents'. Applies to every joint kind.
Defaults to @FLT_MAX@ (effectively off) until set. The engine only
raises the event — it never destroys the joint itself; break it (or
lower the thresholds further) from your own systems after reading
'Apecs.Box3D.Collision.JointEvents'. A joint that stays overloaded across several steps
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
the engine to report it in 'Apecs.Box3D.Collision.JointEvents'. Applies to every joint kind.
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
