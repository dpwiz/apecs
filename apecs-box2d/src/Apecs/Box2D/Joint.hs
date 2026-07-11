{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
-- 'Joint' itself lives in "Apecs.Box2D.Types" — the store's joint
-- registry embeds it — so its instances here are orphans.
{-# OPTIONS_GHC -Wno-orphans #-}

{-| The 'Joint' component, engine joint creation from 'JointSpec', and
the joint tuning sub-components ('MotorSpeed', 'JointLimits', ...).
-}
module Apecs.Box2D.Joint where

import Apecs
import Apecs.Core
import Control.Monad (forM_, when)
import Control.Monad.IO.Class (MonadIO)
import Data.IORef
import Data.IntMap.Strict qualified as IM

import Box2D.Body qualified as B2Body
import Box2D.DistanceJoint qualified as B2DistanceJoint
import Box2D.Id (BodyId, JointId, WorldId)
import Box2D.Joint qualified as B2Joint
import Box2D.MathTypes (Rot (..), Transform (..), Vec2 (..))
import Box2D.MotorJoint qualified as B2MotorJoint
import Box2D.PrismaticJoint qualified as B2PrismaticJoint
import Box2D.RevoluteJoint qualified as B2RevoluteJoint
import Box2D.Types qualified as B2T
import Box2D.UserData (setUserIndex)
import Box2D.WeldJoint qualified as B2WeldJoint
import Box2D.WheelJoint qualified as B2WheelJoint

import Apecs.Box2D.Types

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
    bodies <- readIORef sp.bodies
    forM_ ((,) <$> IM.lookup aEty bodies <*> IM.lookup bEty bodies) $ \(a, b) -> do
      old <- IM.lookup ety <$> readIORef sp.joints
      j <- createJoint sp.world a b spec
      setUserIndex j ety
      forM_ old $ \(JointRecord j' _) -> B2Joint.destroy j' True
      modifyIORef' sp.joints (IM.insert ety (JointRecord j joint))

instance (MonadIO m) => ExplGet m (B2Space Joint) where
  explExists = jointExists
  explGet sp ety = liftIO $
    withReg "Joint" sp.joints ety $
      \(JointRecord _ joint) -> pure joint

instance (MonadIO m) => ExplDestroy m (B2Space Joint) where
  explDestroy sp ety = liftIO $ do
    joints <- readIORef sp.joints
    forM_ (IM.lookup ety joints) $ \(JointRecord j _) -> do
      modifyIORef' sp.joints (IM.delete ety)
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
'Apecs.Box2D.Space.stepPhysics', usually in Newtons. Applies to every joint kind; useful
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
'Apecs.Box2D.Space.stepPhysics', usually in Newton-meters. Applies to every joint kind;
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
engine to report it in 'Apecs.Box2D.Collision.JointEvents'. Applies to every joint kind.
Defaults to @FLT_MAX@ (effectively off) until set. The engine only
raises the event — it never destroys the joint itself; break it (or
lower the thresholds further) from your own systems after reading
'Apecs.Box2D.Collision.JointEvents'. A joint that stays overloaded across several steps
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
the engine to report it in 'Apecs.Box2D.Collision.JointEvents'. Applies to every joint kind.
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
