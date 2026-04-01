{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Apecs
import Apecs.Experimental.Reactive

newtype Color = Color String deriving (Show, Eq, Ord)
newtype Position = Position Int deriving (Show, Num)

-- We use an OrdMap for the Color component
instance Component Color where type Storage Color = Reactive (OrdMap Color) (Map Color)
instance Component Position where type Storage Position = Map Position

-- We also demonstrate EnumMap
data Status = Offline | Online | Away deriving (Show, Eq, Enum)
instance Component Status where type Storage Status = Reactive (EnumMap Status) (Map Status)

makeWorld "World" [''Color, ''Position, ''Status]

game :: System World ()
game = do
  -- Create some entities
  newEntity (Color "red", Position 0, Online)
  newEntity (Color "green", Position 1, Offline)
  newEntity (Color "blue", Position 2, Online)
  newEntity (Color "red", Position 3, Offline)
  newEntity (Color "red", Position 4, Away)
  newEntity (Color "blue", Position 5, Away)

  -- Look up entities by color using the reactive OrdMap
  redEntities <- withReactive $ ordLookup (Color "red")
  blueEntities <- withReactive $ ordLookup (Color "blue")
  greenEntities <- withReactive $ ordLookup (Color "green")

  liftIO $ putStrLn $ "Red entities: " ++ show redEntities
  liftIO $ putStrLn $ "Blue entities: " ++ show blueEntities
  liftIO $ putStrLn $ "Green entities: " ++ show greenEntities

  -- Look up entities by status using the reactive EnumMap
  onlineEntities <- withReactive $ enumLookup Online
  offlineEntities <- withReactive $ enumLookup Offline
  awayEntities <- withReactive $ enumLookup Away

  liftIO $ putStrLn ""
  liftIO $ putStrLn $ "Online entities: " ++ show onlineEntities
  liftIO $ putStrLn $ "Offline entities: " ++ show offlineEntities
  liftIO $ putStrLn $ "Away entities: " ++ show awayEntities

  -- Change a color and a status
  cmap $ \(Color c, Position p) -> if c == "red" then Color "yellow" else Color c
  cmap $ \s -> if s == Away then Online else s

  -- Look up entities by color and status again
  redEntities' <- withReactive $ ordLookup (Color "red")
  yellowEntities <- withReactive $ ordLookup (Color "yellow")
  onlineEntities' <- withReactive $ enumLookup Online
  awayEntities' <- withReactive $ enumLookup Away

  liftIO $ putStrLn "\nAfter changing red to yellow, and Away to Online:"
  liftIO $ putStrLn $ "Red entities: " ++ show redEntities'
  liftIO $ putStrLn $ "Yellow entities: " ++ show yellowEntities
  liftIO $ putStrLn $ "Online entities: " ++ show onlineEntities'
  liftIO $ putStrLn $ "Away entities: " ++ show awayEntities'

main :: IO ()
main = initWorld >>= runSystem game
