{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

{-| The tumbler, one dimension up: a spinning kinematic box full of
spheres and cubes, simulated by Box3D and rendered with apecs-gloss
through a CPU perspective projection. There is no depth buffer, so the
scene is drawn painter-style: bodies never interpenetrate, which makes
a per-piece depth sort correct, and the front faces of a convex cube
never overlap on screen, so backface culling handles the rest.
Click to drop in more debris. The camera orbits on its own.
-}
module Main (main) where

import Apecs
import Apecs.Gloss
import Control.Monad (forM_, replicateM_)
import Data.List (sortOn)
import System.Random (randomRIO)

import Apecs.Box3D

-- | How an entity is rendered; physics state comes from its body.
data Look
  = LookBall Color Float
  | LookCube Color Vec3
  | LookCage Float

instance Component Look where
  type Storage Look = Map Look

-- | Camera orbit angle, advanced every frame.
newtype Orbit = Orbit Float

instance Semigroup Orbit where
  _ <> b = b

instance Monoid Orbit where
  mempty = Orbit 0

instance Component Orbit where
  type Storage Orbit = Global Orbit

makeWorld "World" [''Physics, ''Look, ''Orbit, ''Camera]

cageHalf, wallThickness :: Float
cageHalf = 2.5
wallThickness = 0.15

initialize :: SystemT World IO ()
initialize = do
  set global (Gravity (Vec3 0 (-10) 0))
  makeCage
  makeDumbbell
  replicateM_ 70 (spawnDebris False)
  replicateM_ 25 (spawnDebris True)

-- | Two green spheres linked by a rigid distance joint.
makeDumbbell :: SystemT World IO ()
makeDumbbell = do
  let mk p = do
        e <- newEntity (DynamicBody, Position p, LookBall chartreuse 0.22)
        newEntity_ (Shape e (GeoSphere vec3Zero 0.22))
        pure e
  a <- mk (Vec3 (-0.7) 1.5 0)
  b <- mk (Vec3 0.7 1.5 0)
  newEntity_ (Joint a b (DistanceJoint (Vec3 (-0.7) 1.5 0) (Vec3 0.7 1.5 0)))

{- | A kinematic box cage tumbling about two axes. The walls are thin
boxes just outside the cavity; only the cavity edges are drawn, as a
wireframe.
-}
makeCage :: SystemT World IO ()
makeCage = do
  cage <-
    newEntity
      ( KinematicBody
      , AngularVelocity (Vec3 0.15 0 (-0.4))
      , LookCage cageHalf
      )
  let
    h = cageHalf
    t = wallThickness
    walls =
      [ (Vec3 (s * (h + t)) 0 0, Vec3 t (h + 2 * t) (h + 2 * t))
      | s <- [-1, 1]
      ]
        <> [ (Vec3 0 (s * (h + t)) 0, Vec3 (h + 2 * t) t (h + 2 * t))
           | s <- [-1, 1]
           ]
        <> [ (Vec3 0 0 (s * (h + t)), Vec3 (h + 2 * t) (h + 2 * t) t)
           | s <- [-1, 1]
           ]
  forM_ walls $ \(center, half) ->
    newEntity_ (Shape cage (GeoBox center half))

spawnDebris :: Bool -> SystemT World IO ()
spawnDebris cube = do
  let range = cageHalf - 0.7
  x <- liftIO (randomRIO (-range, range))
  y <- liftIO (randomRIO (-range, range))
  z <- liftIO (randomRIO (-range, range))
  r <- liftIO (randomRIO (0.15, 0.3))
  let
    tint = mixColors (0.3 - r) (r - 0.15) azure orange
    look
      | cube = LookCube tint (Vec3 r r r)
      | otherwise = LookBall tint r
  body <- newEntity (DynamicBody, Position (Vec3 x y z), look)
  let geo
        | cube = GeoBox vec3Zero (Vec3 r r r)
        | otherwise = GeoSphere vec3Zero r
  newEntity_ (Shape body geo, Elasticity 0.25)

-- Vector and quaternion helpers ---------------------------------------------

vadd, vsub, vcross, vmul :: Vec3 -> Vec3 -> Vec3
vadd (Vec3 a b c) (Vec3 x y z) = Vec3 (a + x) (b + y) (c + z)
vsub (Vec3 a b c) (Vec3 x y z) = Vec3 (a - x) (b - y) (c - z)
vcross (Vec3 a b c) (Vec3 x y z) = Vec3 (b * z - c * y) (c * x - a * z) (a * y - b * x)
vmul (Vec3 a b c) (Vec3 x y z) = Vec3 (a * x) (b * y) (c * z)

vscale :: Float -> Vec3 -> Vec3
vscale k (Vec3 x y z) = Vec3 (k * x) (k * y) (k * z)

vdot :: Vec3 -> Vec3 -> Float
vdot (Vec3 a b c) (Vec3 x y z) = a * x + b * y + c * z

vnorm :: Vec3 -> Vec3
vnorm v = vscale (1 / sqrt (vdot v v)) v

-- | Rotate a vector by a unit quaternion.
qrot :: Quat -> Vec3 -> Vec3
qrot (Quat u w) v = v `vadd` vscale w t `vadd` vcross u t
  where
    t = vscale 2 (vcross u v)

-- Camera and projection ------------------------------------------------------

data Cam = Cam
  { camPos :: Vec3
  , camRight :: Vec3
  , camUp :: Vec3
  , camFwd :: Vec3
  }

-- | Orbit around the Y axis, looking at the origin.
mkCam :: Float -> Cam
mkCam a = Cam{camPos = pos, camRight = right, camUp = up, camFwd = fwd}
  where
    pos = Vec3 (9 * cos a) 4 (9 * sin a)
    fwd = vnorm (vscale (-1) pos)
    right = vnorm (vcross fwd (Vec3 0 1 0))
    up = vcross right fwd

-- | To view space: x right, y up, z depth into the screen.
viewP :: Cam -> Vec3 -> Vec3
viewP cam p = Vec3 (vdot d (camRight cam)) (vdot d (camUp cam)) (vdot d (camFwd cam))
  where
    d = p `vsub` camPos cam

focal, nearPlane :: Float
focal = 520
nearPlane = 0.5

projP :: Vec3 -> (Float, Float)
projP (Vec3 x y z) = (focal * x / z, focal * y / z)

lightDir :: Vec3
lightDir = vnorm (Vec3 (-0.4) 1 0.55)

-- | Distance haze: dim colors as depth grows.
fog :: Float -> Float
fog vz = max 0.35 (min 1 (1.3 - vz / 16))

dimBy :: Float -> Color -> Color
dimBy k c = mixColors k (1 - k) c black

-- Drawing ---------------------------------------------------------------------

-- | Depth-tagged pieces; the scene is assembled far-to-near.
type Piece = (Float, Picture)

drawBall :: Cam -> Color -> Float -> Vec3 -> [Piece]
drawBall cam col r pos =
  [ (vz, Translate px py (ball <> shine))
  | let v@(Vec3 _ _ vz) = viewP cam pos
  , vz > nearPlane
  , let
      (px, py) = projP v
      rs = focal * r / vz
      lit = 0.55 + 0.45 * vdot (vnorm (camPos cam `vsub` pos)) lightDir
      base = dimBy (lit * fog vz) col
      ball = Color base (circleSolid rs)
      shine = Color (mixColors 0.6 0.4 base white) (Translate (-rs * 0.3) (rs * 0.3) (circleSolid (rs * 0.35)))
  ]

-- | Unit-cube faces: outward normal and corners in cycle order.
cubeFaces :: [(Vec3, [Vec3])]
cubeFaces =
  [ (Vec3 1 0 0, [Vec3 1 1 1, Vec3 1 1 (-1), Vec3 1 (-1) (-1), Vec3 1 (-1) 1])
  , (Vec3 (-1) 0 0, [Vec3 (-1) 1 1, Vec3 (-1) 1 (-1), Vec3 (-1) (-1) (-1), Vec3 (-1) (-1) 1])
  , (Vec3 0 1 0, [Vec3 1 1 1, Vec3 1 1 (-1), Vec3 (-1) 1 (-1), Vec3 (-1) 1 1])
  , (Vec3 0 (-1) 0, [Vec3 1 (-1) 1, Vec3 1 (-1) (-1), Vec3 (-1) (-1) (-1), Vec3 (-1) (-1) 1])
  , (Vec3 0 0 1, [Vec3 1 1 1, Vec3 1 (-1) 1, Vec3 (-1) (-1) 1, Vec3 (-1) 1 1])
  , (Vec3 0 0 (-1), [Vec3 1 1 (-1), Vec3 1 (-1) (-1), Vec3 (-1) (-1) (-1), Vec3 (-1) 1 (-1)])
  ]

{- | Flat-shaded convex cube: backface-culled faces need no sorting among
themselves, so each face joins the global paint order on its own.
-}
drawCube :: Cam -> Color -> Vec3 -> Vec3 -> Quat -> [Piece]
drawCube cam col half pos q =
  [ (depth, Color shade (Polygon (map projP vs)))
  | (ln, corners) <- cubeFaces
  , let
      n = qrot q ln
      ws = [pos `vadd` qrot q (c `vmul` half) | c <- corners]
      center = vscale 0.25 (foldr vadd vec3Zero ws)
  , vdot n (center `vsub` camPos cam) < 0
  , let vs = map (viewP cam) ws
  , all (\(Vec3 _ _ z) -> z > nearPlane) vs
  , let
      depth = sum [z | Vec3 _ _ z <- vs] / 4
      lit = 0.3 + 0.7 * max 0 (vdot n lightDir)
      shade = dimBy (lit * fog depth) col
  ]

{- | The cavity edges as a wireframe, subdivided so segments sort against
the debris passing in front of and behind them.
-}
drawCage :: Cam -> Float -> Vec3 -> Quat -> [Piece]
drawCage cam h pos q =
  [ (vza, Color (dimBy (0.8 * fog vza) white) (Line [projP a, projP b]))
  | (c1, c2) <- edges
  , let
      w1 = pos `vadd` qrot q c1
      w2 = pos `vadd` qrot q c2
  , i <- [0 .. subdivs - 1]
  , let
      a = viewP cam (lerp (fromIntegral i / n) w1 w2)
      b = viewP cam (lerp (fromIntegral (i + 1) / n) w1 w2)
      vza = (vz a + vz b) / 2
  , vz a > nearPlane && vz b > nearPlane
  ]
  where
    subdivs = 6 :: Int
    n = fromIntegral subdivs
    vz (Vec3 _ _ z) = z
    lerp t u v = u `vadd` vscale t (v `vsub` u)
    corners = [Vec3 (sx * h) (sy * h) (sz * h) | sx <- [-1, 1], sy <- [-1, 1], sz <- [-1, 1]]
    edges =
      [ (a, b)
      | (i, a) <- zip [0 :: Int ..] corners
      , (j, b) <- zip [0 ..] corners
      , i < j
      , sameAxes a b == 2
      ]
    sameAxes (Vec3 a b c) (Vec3 x y z) =
      length (filter id [a == x, b == y, c == z])

drawLook :: Cam -> Vec3 -> Quat -> Look -> [Piece]
drawLook cam pos q look = case look of
  LookBall col r -> drawBall cam col r pos
  LookCube col half -> drawCube cam col half pos q
  LookCage h -> drawCage cam h pos q

draw :: SystemT World IO Picture
draw = do
  Orbit a <- get global
  let cam = mkCam a
  pieces <- cfold (\acc (Position p, Rotation q, l :: Look) -> drawLook cam p q l <> acc) []
  pure (Pictures (map snd (sortOn (negate . fst) pieces)))

handle :: Event -> SystemT World IO ()
handle (EventKey (MouseButton LeftButton) Down _ _) = do
  cube <- liftIO (randomRIO (0 :: Int, 2))
  spawnDebris (cube == 0)
handle _ = pure ()

step :: Float -> SystemT World IO ()
step dT = do
  stepPhysics (min dT (1 / 30))
  Orbit a <- get global
  set global (Orbit (a + dT * 0.25))

main :: IO ()
main = do
  w <- initWorld
  runSystem (initialize >> play disp black 60 draw handle step) w
  where
    disp = InWindow "apecs-box3d demo" (720, 720) (10, 10)
