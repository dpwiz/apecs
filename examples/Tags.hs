{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

import Control.Monad (forM_)
import Apecs
import Apecs.TH.Tags

newtype Position = Position (Double, Double) deriving Show
newtype Velocity = Velocity (Double, Double) deriving Show
data Player = Player deriving Show
data Enemy = Enemy deriving Show

makeWorldAndComponents "World" [''Position, ''Velocity, ''Player, ''Enemy]
makeTaggedComponents "World" [''Position, ''Velocity, ''Player, ''Enemy]

game :: System World ()
game = do
  -- Create some entities
  e1 <- newEntity (Position (0, 0), Velocity (1, 1), Player)
  e2 <- newEntity (Position (10, 10), Enemy)

  -- Use introspection to print what components e1 has
  liftIO $ putStrLn "Inspecting e1:"
  forM_ [minBound .. maxBound :: WorldTag] $ \tag -> do
    c <- lookupWorldTag e1 tag
    case c of
      Just comp -> liftIO $ putStrLn $ "  Has component " ++ show tag ++ ": " ++ show comp
      Nothing   -> return ()

  liftIO $ putStrLn "Inspecting e2:"
  forM_ [minBound .. maxBound :: WorldTag] $ \tag -> do
    c <- lookupWorldTag e2 tag
    case c of
      Just comp -> liftIO $ putStrLn $ "  Has component " ++ show tag ++ ": " ++ show comp
      Nothing   -> return ()

main :: IO ()
main = initWorld >>= runSystem game
