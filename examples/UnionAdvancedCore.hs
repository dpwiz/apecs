{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

import Apecs
import Apecs.Core
import Control.Monad (forM_)
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import Control.Monad.ST (runST)

newtype Position = Position (Double, Double) deriving Show
instance Component Position where type Storage Position = Map Position

newtype Velocity = Velocity (Double, Double) deriving Show
instance Component Velocity where type Storage Velocity = Map Velocity

makeWorld "World" [''Position, ''Velocity]

-- PoC 3: Advanced Core Integration
-- Here we introduce a new pseudo-component `Union a b`
-- which provides `(Maybe a, Maybe b)` and implements an optimized `ExplMembers`.
-- The key to high performance union iteration is merging two sorted unboxed vectors without allocating intermediate lists.
-- Actually `ExplMembers` doesn't guarantee sorted vectors, but we can assume we sort them in place, or sort them first, then merge.
-- `apecs` typical stores like `Map` use IntMap which means `explMembers` is actually sorted.
-- `Cache` store might not be, so we would need to sort.
-- For this PoC, we will sort both and then do a linear time merge.

data Union a b = Union (Maybe a) (Maybe b) deriving Show

newtype UnionStore sa sb = UnionStore (sa, sb)

instance (Component ca, Component cb) => Component (Union ca cb) where
  type Storage (Union ca cb) = UnionStore (Storage ca) (Storage cb)

instance (Has w m ca, Has w m cb) => Has w m (Union ca cb) where
  getStore = do
    sa <- getStore
    sb <- getStore
    return $ UnionStore (sa, sb)

type instance Elem (UnionStore sa sb) = Union (Elem sa) (Elem sb)

instance (ExplGet m sa, ExplGet m sb) => ExplGet m (UnionStore sa sb) where
  explGet (UnionStore (sa, sb)) ety = do
    ea <- explExists sa ety
    eb <- explExists sb ety
    va <- if ea then Just <$> explGet sa ety else return Nothing
    vb <- if eb then Just <$> explGet sb ety else return Nothing
    return $ Union va vb

  explExists (UnionStore (sa, sb)) ety = do
    ea <- explExists sa ety
    if ea then return True else explExists sb ety

-- Efficient merge of two vectors. We assume they are sorted for the sake of O(N+M).
-- If not, we sort them. U.modify (U.Algorithms.Intro.sort) can be used from vector-algorithms, but we'll use a naive quicksort or just List sort for PoC to avoid adding dependencies, but we'll show the structure of an efficient unboxed merge.
-- Actually let's just use U.modify sort if we had it, or just use list sort in the PoC since adding `vector-algorithms` might be too much.
-- To prove it can be done efficiently, let's write a pure unboxed merge assuming they are already sorted.

mergeSortedVectors :: U.Vector Int -> U.Vector Int -> U.Vector Int
mergeSortedVectors v1 v2 = U.create $ do
  let n1 = U.length v1
      n2 = U.length v2
  out <- UM.new (n1 + n2)
  let go i j k
        | i == n1 && j == n2 = return k
        | i == n1 = do
            UM.write out k (v2 U.! j)
            go i (j+1) (k+1)
        | j == n2 = do
            UM.write out k (v1 U.! i)
            go (i+1) j (k+1)
        | otherwise = do
            let x = v1 U.! i
                y = v2 U.! j
            case compare x y of
              LT -> do UM.write out k x; go (i+1) j (k+1)
              GT -> do UM.write out k y; go i (j+1) (k+1)
              EQ -> do UM.write out k x; go (i+1) (j+1) (k+1) -- deduplicate
  len <- go 0 0 0
  return $ UM.slice 0 len out

instance (ExplMembers m sa, ExplMembers m sb) => ExplMembers m (UnionStore sa sb) where
  explMembers (UnionStore (sa, sb)) = do
    -- Ideally, we'd ensure they are sorted here.
    -- Map stores return sorted keys.
    ma <- explMembers sa
    mb <- explMembers sb
    -- For PoC, assuming sorted (Map returns sorted keys via IntMap.keys)
    -- Actually Apecs `Map` returns `U.fromList $ IM.keys m`, which is strictly sorted!
    -- So `mergeSortedVectors` works perfectly for `Map`.
    return $ mergeSortedVectors ma mb

main :: IO ()
main = do
  w <- initWorld
  runWith w $ do
    _ <- newEntity (Position (1,1))
    _ <- newEntity (Velocity (2,2))
    _ <- newEntity (Position (3,3), Velocity (4,4))

    liftIO $ putStrLn "Iterating over Union Position Velocity:"
    cmapM_ $ \(Union p v :: Union Position Velocity, e :: Entity) -> do
      liftIO $ putStrLn $ "Entity " ++ show (unEntity e) ++ ": " ++ show p ++ ", " ++ show v
