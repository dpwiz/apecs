{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

{- | STM Colony War: a massively-concurrent apecs-stm + gloss showcase.

Every unit is its own thread running its own system, all mutating shared
component stores concurrently. The gloss step loop does /nothing/: all game
logic lives in the forked threads, synchronised purely through STM.

This is exactly the workload that breaks a non-STM ECS:

  * Hundreds of threads allocate entities through the shared, atomic STM
    'EntityCounter' (vanilla @nextEntity@ would hand out duplicate ids).

  * Each unit's whole turn -- find nearest enemy, check it is still alive,
    deal damage, score the kill, destroy the corpse -- runs as a single
    'atomically' transaction. Two attackers landing a lethal blow on the same
    victim in the same instant cannot both score the kill: STM serialises them.

Build with @-threaded -N@ so the threads actually run in parallel.
-}
module Main (main) where

import Control.Monad (forM_, forever, void, when)

import Apecs.Gloss
import Apecs.STM.Prelude
import Linear (V2 (..), normalize, quadrance, (^*))
import System.Exit (exitSuccess)
import System.Random (randomRIO)

-- Components ----------------------------------------------------------------

newtype Position = Position (V2 Float) deriving (Show)
instance Component Position where type Storage Position = Map Position

newtype Velocity = Velocity (V2 Float) deriving (Show)
instance Component Velocity where type Storage Velocity = Map Velocity

newtype Health = Health Float deriving (Show)
instance Component Health where type Storage Health = Map Health

data Team = Red | Blue deriving (Eq, Show)
instance Component Team where type Storage Team = Map Team

-- | Kills scored by (Red, Blue). A 'Global', so it belongs to every entity.
data KillScore = KillScore !Int !Int deriving (Show)
instance Semigroup KillScore where
  KillScore a b <> KillScore c d = KillScore (a + c) (b + d)
instance Monoid KillScore where
  mempty = KillScore 0 0
instance Component KillScore where type Storage KillScore = Global KillScore

-- | Every component an entity owns, so we can delete it in one go.
type All = (Position, Velocity, Health, Team)

makeWorld "World" [''Position, ''Velocity, ''Health, ''Team, ''KillScore, ''Camera]

type SystemIO a = SystemT World IO a
type SystemSTM a = SystemT World STM a

-- Tunables ------------------------------------------------------------------

capPerTeam :: Int
capPerTeam = 80

startHp, damage, unitSpeed, attackRange, dt :: Float
startHp = 100
damage = 8
unitSpeed = 60
attackRange = 12
dt = fromIntegral tickMicros / 1e6

tickMicros, spawnMicros :: Int
tickMicros = 20000 -- 50 Hz AI tick
spawnMicros = 100000 -- one reinforcement attempt per team per 0.1s

-- | Spawn anchor for a team, at the left/right edge of the battlefield.
basePos :: Team -> V2 Float
basePos Red = V2 (-260) 0
basePos Blue = V2 260 0

teamColor :: Team -> Color
teamColor Red = makeColor 0.9 0.3 0.3 1
teamColor Blue = makeColor 0.3 0.5 0.9 1

addKill :: Team -> KillScore -> KillScore
addKill Red (KillScore r b) = KillScore (r + 1) b
addKill Blue (KillScore r b) = KillScore r (b + 1)

-- Per-unit AI ---------------------------------------------------------------

{- | One atomic turn for a single unit. Returns whether the unit is still
alive afterwards. Running this via 'atomically' is what makes the
find-check-damage-kill-score-destroy sequence indivisible.
-}
stepUnit :: Entity -> SystemSTM Bool
stepUnit ety = do
  alive <- exists ety (Proxy @Health)
  if not alive
    then pure False
    else do
      Health hp <- get ety
      Team myTeam <- get ety
      Position p <- get ety
      if hp <= 0
        then do
          destroy ety (Proxy @All)
          pure False
        else do
          -- Nearest living enemy, by squared distance.
          nearest <-
            cfoldM
              ( \acc (Team t, Position q, e) ->
                  pure $
                    if t == myTeam
                      then acc
                      else
                        let d2 = quadrance (q - p)
                         in case acc of
                              Just (_, _, best) | best <= d2 -> acc
                              _ -> Just (e, q, d2)
              )
              Nothing
          case nearest of
            Nothing -> pure True -- no enemies left; idle
            Just (enemy, epos, d2)
              | d2 <= attackRange * attackRange -> do
                  attack myTeam enemy
                  pure True
              | otherwise -> do
                  let dir = normalize (epos - p)
                  set ety (Velocity (dir ^* unitSpeed))
                  set ety (Position (p + dir ^* (unitSpeed * dt)))
                  pure True

-- | Deal damage to a victim within the caller's transaction.
attack :: Team -> Entity -> SystemSTM ()
attack killer victim = do
  stillThere <- exists victim (Proxy @Health)
  when stillThere $ do
    Health h <- get victim
    if h - damage <= 0
      then do
        destroy victim (Proxy @All)
        modify global (addKill killer)
      else set victim (Health (h - damage))

-- | The thread driving one unit: tick, sleep, repeat, until it dies.
unitAI :: Entity -> SystemIO ()
unitAI ety = do
  living <- atomically (stepUnit ety)
  when living $ do
    threadDelay tickMicros
    unitAI ety

-- Spawning ------------------------------------------------------------------

teamCount :: Team -> SystemSTM Int
teamCount team =
  cfold (\n (Team t) -> if t == team then n + 1 else n) (0 :: Int)

{- | Allocate a new unit for a team, blocking (via 'check'/STM 'retry') while
that team is already at its population cap. Unblocks automatically when one of
its units dies and frees a slot.
-}
spawnUnit :: Team -> V2 Float -> SystemSTM Entity
spawnUnit team offset = do
  n <- teamCount team
  check (n < capPerTeam)
  newEntity
    ( team
    , Position (basePos team + offset)
    , Velocity 0
    , Health startHp
    )

-- | A reinforcement thread, one per team.
spawner :: Team -> SystemIO ()
spawner team = forever $ do
  dy <- liftIO $ randomRIO (-120, 120)
  ety <- atomically (spawnUnit team (V2 0 dy))
  void $ forkSys (unitAI ety)
  threadDelay spawnMicros

-- Setup ---------------------------------------------------------------------

initialize :: SystemIO ()
initialize = do
  set global (Camera 0 1)
  -- A starting platoon per team, each with its own AI thread.
  forM_ [Red, Blue] $ \team ->
    forM_ [1 .. 20 :: Int] $ \i -> do
      let dy = fromIntegral (i - 10) * 11
      ety <- atomically $ newEntity (team, Position (basePos team + V2 0 dy), Velocity 0, Health startHp)
      void $ forkSys (unitAI ety)
  void $ forkSys (spawner Red)
  void $ forkSys (spawner Blue)

-- Rendering & input (main thread) -------------------------------------------

draw :: SystemIO Picture
draw = do
  units <- foldDraw $ \(Team t, Position (V2 x y), Health hp) ->
    translate x y . color (teamColor t) $ circleSolid (2 + hp / 20)
  KillScore r b <- get global
  let hud =
        color white
          . translate (-300) 210
          . scale 0.12 0.12
          . Text
          $ "Red " ++ show r ++ "   Blue " ++ show b
  pure (units <> hud)

handleEvent :: Event -> SystemIO ()
handleEvent (EventKey (SpecialKey KeyEsc) Down _ _) = liftIO exitSuccess
handleEvent _ = pure ()

-- | Deliberately empty: all game logic runs on the forked unit threads.
step :: Float -> SystemIO ()
step _ = pure ()

main :: IO ()
main = do
  w <- initWorld
  runWith w $ do
    initialize
    play (InWindow "STM Colony War" (640, 480) (10, 10)) black 60 draw handleEvent step
