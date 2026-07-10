-- | World queries: ray and shape casts, AABB, point and overlap tests.
module Apecs.Box2D.Query where

import Apecs
import Control.Monad (forM_, when)
import Control.Monad.IO.Class (MonadIO)
import Data.IORef
import Data.IntSet qualified as IS
import Data.List (sortOn)
import Foreign.Ptr (FunPtr, Ptr)

import Box2D.Callbacks (withCastResultFcn, withOverlapResultFcn)
import Box2D.Id (ShapeId)
import Box2D.MathTypes (AABB (..), Vec2 (..), vec2Sub, vec2Zero)
import Box2D.Shape qualified as B2Shape
import Box2D.Tags qualified as B2Tags
import Box2D.Types (Filter (..))
import Box2D.Types qualified as B2T
import Box2D.World qualified as B2World

import Apecs.Box2D.Geometry
import Apecs.Box2D.Types

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

{- | The query filter that matches every shape: category 1 (the default
shape category), full mask — the "just give me everything" argument
for 'aabbQuery', 'overlapQuery' and friends.
-}
everything :: Filter
everything =
  Filter
    { filterCategoryBits = 1
    , filterMaskBits = maxBound
    , filterGroupIndex = 0
    }

{- | Queries match a 'Filter''s category and mask bits against shape
filters (see 'Apecs.Box2D.Shape.CollisionFilter'); 'filterGroupIndex' does not apply.
'everything' queries everything.
-}
toQueryFilter :: Filter -> IO B2T.QueryFilter
toQueryFilter f = do
  qf <- B2T.defaultQueryFilter
  pure
    qf
      { B2T.queryFilterCategoryBits = filterCategoryBits f
      , B2T.queryFilterMaskBits = filterMaskBits f
      }

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

{- | Drive a cast-style engine query ('B2World.castRay',
'B2World.castShape') with the collect-everything visitor: each reported
shape becomes a 'RayHit' (hits whose shapes were destroyed since the last
'Apecs.Box2D.Space.stepPhysics' are dropped), sorted nearest-first by 'rayHitFraction'.
-}
collectCastHits :: B2Space c -> (FunPtr B2Tags.CastResultFcn -> Ptr () -> IO r) -> IO [RayHit]
collectCastHits sp run = do
  found <- newIORef []
  let visit s point normal frac = do
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
  _ <- withCastResultFcn visit run
  sortOn rayHitFraction <$> readIORef found

{- | Drive an overlap-style engine query ('B2World.overlapAABB',
'B2World.overlapShape'), collecting the deduplicated body entities of
every reported shape that passes the keep test. The keep test runs
before entity resolution: it is at most one FFI call while resolution is
two plus a registry lookup, and (for the exact tests) most broad-phase
candidates fail it.
-}
collectOverlapBodies
  :: B2Space c
  -> (ShapeId -> IO Bool)
  -> (FunPtr B2Tags.OverlapResultFcn -> Ptr () -> IO r)
  -> IO [Entity]
collectOverlapBodies sp keep run = do
  found <- newIORef IS.empty
  let visit s = do
        wanted <- keep s
        when wanted $ do
          hit <- shapeEntities sp s
          forM_ hit $ \(_, Entity bodyIx) -> modifyIORef' found (IS.insert bodyIx)
        pure True
  _ <- withOverlapResultFcn visit run
  map Entity . IS.toList <$> readIORef found

{- | Every shape along a world-space segment, sorted nearest-first by
'rayHitFraction'. Filter semantics match 'segmentQuery'. Unlike
'segmentQuery', which goes through the engine's @b2World_CastRayClosest@
convenience path, this drives the general @b2World_CastRay@ callback
directly — and that path does /not/ ignore initial overlaps itself (the
"ignore initial overlap" behaviour lives in the closest-hit callback, which
skips fraction-0 hits before they reach the caller). So a segment starting
inside a shape here reports that shape too, with 'rayHitFraction' 0. Hits
whose shapes were destroyed since the last 'Apecs.Box2D.Space.stepPhysics' are dropped, same
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
    collectCastHits sp $ \fp ctx ->
      B2World.castRay (spWorld sp) start (vec2Sub end start) qf fp ctx

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
    collectOverlapBodies sp (\_ -> pure True) $ \fp ctx ->
      B2World.overlapAABB (spWorld sp) vec2Zero box qf fp ctx
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
    collectOverlapBodies sp (`B2Shape.testPoint` point) $ \fp ctx ->
      B2World.overlapAABB (spWorld sp) vec2Zero (AABB point point) qf fp ctx

{- | The body entities whose shapes actually overlap a query shape, given
as a world-space 'Geometry' — exact narrow-phase overlap, the
shape-shaped big brother of 'containsPointQuery'.
-}
overlapQuery
  :: forall w m
   . (MonadIO m, Has w m Physics)
  => Geometry
  -> Filter
  -> SystemT w m [Entity]
overlapQuery geo fltr = do
  sp :: B2Space Physics <- getStore
  liftIO $ do
    qf <- toQueryFilter fltr
    (points, radius) <- geometryProxy geo
    collectOverlapBodies sp (\_ -> pure True) $ \fp ctx ->
      B2T.withShapeProxy points radius $ \proxy ->
        B2World.overlapShape (spWorld sp) vec2Zero proxy qf fp ctx

{- | Sweep a query shape (a world-space 'Geometry') along a translation
and collect everything it would hit, sorted nearest-first by
'rayHitFraction' — 'segmentQueryAll' with volume. A shape that already
overlaps the query shape at the start of the sweep is reported too, at
fraction 0 (matching upstream @b2World_CastShape@, which treats an
initial overlap as an immediate hit rather than skipping it).
-}
sweepQuery
  :: forall w m
   . (MonadIO m, Has w m Physics)
  => Geometry
  -> WVec
  -> Filter
  -> SystemT w m [RayHit]
sweepQuery geo translation fltr = do
  sp :: B2Space Physics <- getStore
  liftIO $ do
    qf <- toQueryFilter fltr
    (points, radius) <- geometryProxy geo
    collectCastHits sp $ \fp ctx ->
      B2T.withShapeProxy points radius $ \proxy ->
        B2World.castShape (spWorld sp) vec2Zero proxy translation qf fp ctx
