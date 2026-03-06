{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

import Apecs
import Data.Maybe (catMaybes)
import Control.Monad (forM_)
import qualified Data.Vector.Unboxed as U

newtype Position = Position (Double, Double) deriving Show
instance Component Position where type Storage Position = Map Position

newtype Velocity = Velocity (Double, Double) deriving Show
instance Component Velocity where type Storage Velocity = Map Velocity

makeWorld "World" [''Position, ''Velocity]

-- User-space composition:
-- Goal: iterate over entities with Position, Velocity, or both.
-- We want to retrieve Entity IDs or tuples (Maybe Position, Maybe Velocity).
--
-- Approach: Collect all entities that have A, then collect all entities that have B, but lack A to avoid duplicates.
-- Then iterate over those entities to get values.

getUnionEntities :: System World [Entity]
getUnionEntities = do
  -- All entities with Position
  es1 <- collect $ \(_ :: Position, e :: Entity) -> Just e
  -- All entities with Velocity, but *no* Position
  es2 <- collect $ \(_ :: Velocity, _ :: Not Position, e :: Entity) -> Just e
  return (es1 ++ es2)

main :: IO ()
main = do
  w <- initWorld
  runWith w $ do
    -- Setup
    _ <- newEntity (Position (1,1))
    _ <- newEntity (Velocity (2,2))
    _ <- newEntity (Position (3,3), Velocity (4,4))
    _ <- newEntity () -- entity with nothing

    -- Get union entities
    entities <- getUnionEntities

    liftIO $ putStrLn "Union Entities:"
    forM_ entities $ \e -> do
      p :: Maybe Position <- get e
      v :: Maybe Velocity <- get e
      liftIO $ putStrLn $ "Entity " ++ show (unEntity e) ++ ": " ++ show p ++ ", " ++ show v
