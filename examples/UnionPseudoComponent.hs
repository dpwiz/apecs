{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

import Apecs
import Apecs.Core
import Apecs.Components (EitherStore(..))
import qualified Data.List as L
import Control.Monad (forM_)
import qualified Data.Vector.Unboxed as U

newtype Position = Position (Double, Double) deriving Show
instance Component Position where type Storage Position = Map Position

newtype Velocity = Velocity (Double, Double) deriving Show
instance Component Velocity where type Storage Velocity = Map Velocity

makeWorld "World" [''Position, ''Velocity]

-- PoC 2: Custom pseudo-component or instance for Either / (Maybe a, Maybe b)
-- We'll try to implement ExplMembers for `Either a b` which Apecs already has a Store for.
-- The core problem is merging two sorted unboxed vectors. Since apecs member vectors are usually returned in unspecified order depending on the store (e.g. Map vs Cache), we'll do a simple O(N log N) or O(N + M) merge.
-- Actually, apecs ExplMembers typically returns a Vector of Ints. We can use U.fromList . Data.List.nub . Data.List.sort . U.toList.
-- Since this is just a PoC, we'll do the simple list-based merge to see if it works.


-- Wait, EitherStore is defined in Apecs.Components.
-- We can't easily add orphan instances for `ExplMembers` of `EitherStore` if they exist or don't exist without potential conflicts, but let's try.
-- Wait, EitherStore already exists but lacks ExplMembers.

instance (ExplMembers m sa, ExplMembers m sb) => ExplMembers m (EitherStore sa sb) where
  explMembers (EitherStore sa sb) = do
    ma <- explMembers sa
    mb <- explMembers sb
    -- A naive merge:
    let listA = U.toList ma
        listB = U.toList mb
    return $ U.fromList $ L.nub $ L.sort $ listA ++ listB

-- Let's test `cmap` over `Either Position Velocity`
main :: IO ()
main = do
  w <- initWorld
  runWith w $ do
    -- Setup
    _ <- newEntity (Position (1,1))
    _ <- newEntity (Velocity (2,2))
    _ <- newEntity (Position (3,3), Velocity (4,4))

    -- Iterate using cmap over Either Position Velocity
    -- Wait, cmap requires `Set` for the output, but we just want to print or collect.
    -- Let's use `cmapM_`
    liftIO $ putStrLn "Iterating over Either Position Velocity:"
    cmapM_ $ \(x :: Either Position Velocity, e :: Entity) -> do
      liftIO $ putStrLn $ "Entity " ++ show (unEntity e) ++ ": " ++ show x

    -- What if we want both? We can use the Either for iteration, and fetch Maybe for both.
    liftIO $ putStrLn "\nIterating over Either Position Velocity, fetching Maybe for both:"
    cmapM_ $ \(_ :: Either Position Velocity, e :: Entity) -> do
      p :: Maybe Position <- get e
      v :: Maybe Velocity <- get e
      liftIO $ putStrLn $ "Entity " ++ show (unEntity e) ++ ": " ++ show p ++ ", " ++ show v
