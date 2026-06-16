{-# LANGUAGE BangPatterns #-}
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

Every unit and every base is its own thread running its own system, all
mutating shared component stores concurrently. The gloss step loop does
/nothing/: all game logic lives in the forked threads, synchronised purely
through STM.

This is exactly the workload that breaks a non-STM ECS:

  * Hundreds of threads allocate entities through the shared, atomic STM
    'EntityCounter' (vanilla @nextEntity@ would hand out duplicate ids).

  * Each unit's whole turn -- find nearest enemy, check it is still alive,
    deal damage, score the kill, destroy the corpse -- runs as a single
    'atomically' transaction. Two attackers landing a lethal blow on the same
    victim in the same instant cannot both score the kill: STM serialises them.

  * The renderer folds the entire frame inside one transaction, so it always
    draws a consistent snapshot even while units are being destroyed under it.

Run windowed (default) or headless and faster-than-realtime for testing:

  > stack run stm-colony-war               -- gloss window
  > stack run stm-colony-war -- --headless -- N rounds, no window, fast clock

Build with @-threaded -N@ so the threads actually run in parallel.
-}
module Main (main) where

import Control.Monad (forM_, void, when)
import Debug.Trace (traceM)
import System.Environment (getArgs)

import Apecs.Gloss
import Apecs.STM.Prelude
import Linear (V2 (..), normalize, quadrance, (^*))
import System.Exit (exitSuccess)
import System.Random (randomRIO)

-- Components ----------------------------------------------------------------

newtype Position = Position (V2 Float) deriving (Show)
instance Component Position where type Storage Position = Map Position

newtype Health = Health Float deriving (Show)
instance Component Health where type Storage Health = Map Health

data Team = Red | Blue deriving (Eq, Show)
instance Component Team where type Storage Team = Map Team

-- | What an entity /is/. Soldiers fight and move; a Base is the stationary
-- spawn-heart a team must protect.
data Kind = Soldier | Base deriving (Eq, Show)
instance Component Kind where type Storage Kind = Map Kind

-- | Kills scored this round, by (Red, Blue). A 'Global'.
data KillScore = KillScore !Int !Int deriving (Show)
instance Semigroup KillScore where
  KillScore a b <> KillScore c d = KillScore (a + c) (b + d)
instance Monoid KillScore where
  mempty = KillScore 0 0
instance Component KillScore where type Storage KillScore = Global KillScore

-- | Reinforcements each base may still enlist this round, by (Red, Blue). A
-- shared atomic 'Global': once a team's pool hits zero its soldiers can only
-- dwindle, so the enemy eventually marches on its undefended base.
data Reserves = Reserves !Int !Int deriving (Show)
instance Semigroup Reserves where
  Reserves a b <> Reserves c d = Reserves (a + c) (b + d)
instance Monoid Reserves where
  mempty = Reserves 0 0
instance Component Reserves where type Storage Reserves = Global Reserves

-- | Rounds won across the whole match, by (Red, Blue). A 'Global'.
data Wins = Wins !Int !Int deriving (Show)
instance Semigroup Wins where
  Wins a b <> Wins c d = Wins (a + c) (b + d)
instance Monoid Wins where
  mempty = Wins 0 0
instance Component Wins where type Storage Wins = Global Wins

-- | The shared round state. A 'Global'; last write wins.
data Phase = Playing | RoundOver Team deriving (Eq, Show)
instance Semigroup Phase where _ <> b = b
instance Monoid Phase where mempty = Playing
instance Component Phase where type Storage Phase = Global Phase

-- | Every component an entity owns, so we can delete it in one go (the extra
-- deletes are harmless no-ops for entities that lack a component).
type All = (Position, Health, Team, Kind)

makeWorld "World" [''Position, ''Health, ''Team, ''Kind, ''KillScore, ''Reserves, ''Wins, ''Phase, ''Camera]

type SystemIO a = SystemT World IO a
type SystemSTM a = SystemT World STM a

-- Run configuration ---------------------------------------------------------

data Config = Config
  { cHeadless :: Bool
  -- ^ No window; run the AI clock as fast as it will go.
  , cRounds :: Int
  -- ^ Stop after this many rounds (0 == run forever, for the window).
  , cTick :: Int
  -- ^ Per-soldier AI tick, microseconds.
  , cSpawn :: Int
  -- ^ Reinforcement interval per base, microseconds.
  , cPoll :: Int
  -- ^ How often the round coordinator checks for a winner, microseconds.
  , cOver :: Int
  -- ^ How long the score table lingers between rounds, microseconds.
  , cDebug :: Bool
  -- ^ Emit 'traceM' diagnostics.
  }

displayCfg :: Config
displayCfg =
  Config
    { cHeadless = False
    , cRounds = 0
    , cTick = 20000 -- 50 Hz
    , cSpawn = 120000
    , cPoll = 100000
    , cOver = 4000000
    , cDebug = False
    }

headlessCfg :: Config
headlessCfg =
  Config
    { cHeadless = True
    , cRounds = 3
    , cTick = 1500 -- ~660 Hz: the whole battle plays out in seconds
    , cSpawn = 8000
    , cPoll = 5000
    , cOver = 300000
    , cDebug = True
    }

dbg :: Config -> String -> SystemIO ()
dbg cfg msg = when (cDebug cfg) (traceM msg)

-- Tunables ------------------------------------------------------------------

capPerTeam, initialPlatoon, reservePool :: Int
capPerTeam = 60
initialPlatoon = 16
reservePool = 70 -- reinforcements a base may enlist per round

baseHp, startHp, damage, unitSpeed, attackRange :: Float
baseHp = 600
startHp = 100
damage = 12
unitSpeed = 85
attackRange = 14

-- | Soldiers shove each other apart so they don't stack into one pixel.
unitRadius, baseRadius, collideDist, sepStrength, spawnRadius :: Float
unitRadius = 5
baseRadius = 22
collideDist = 11
sepStrength = 0.6
spawnRadius = 45

attackRange2, collideDist2 :: Float
attackRange2 = attackRange * attackRange
collideDist2 = collideDist * collideDist

-- | Spawn anchor for a team, at the left/right edge of the battlefield.
basePos :: Team -> V2 Float
basePos Red = V2 (-185) 0
basePos Blue = V2 185 0

teamColor :: Team -> Color
teamColor Red = makeColor 0.9 0.3 0.3 1
teamColor Blue = makeColor 0.3 0.5 0.9 1

isPlaying :: Phase -> Bool
isPlaying Playing = True
isPlaying _ = False

addKill :: Team -> KillScore -> KillScore
addKill Red (KillScore r b) = KillScore (r + 1) b
addKill Blue (KillScore r b) = KillScore r (b + 1)

addWin :: Team -> Wins -> Wins
addWin Red (Wins r b) = Wins (r + 1) b
addWin Blue (Wins r b) = Wins r (b + 1)

teamReserves :: Team -> Reserves -> Int
teamReserves Red (Reserves r _) = r
teamReserves Blue (Reserves _ b) = b

spendReserve :: Team -> Reserves -> Reserves
spendReserve Red (Reserves r b) = Reserves (r - 1) b
spendReserve Blue (Reserves r b) = Reserves r (b - 1)

-- Per-soldier AI ------------------------------------------------------------

{- | One atomic turn for a single soldier. Returns whether it is still alive
afterwards. Running this via 'atomically' makes the whole
find-target / separate / move / attack / kill / score sequence indivisible.

A single fold over the world gathers three things at once:

  * the nearest enemy soldier (the primary target),
  * the nearest enemy base    (the target once their soldiers are gone),
  * a separation push away from crowding friend-or-foe soldiers.
-}
stepUnit :: Config -> Entity -> SystemSTM Bool
stepUnit cfg ety = do
  alive <- exists ety (Proxy @Health)
  if not alive
    then pure False
    else do
      myTeam <- get ety
      Position p <- get ety
      (mUnit, mBase, sep) <-
        cfoldM (gather p myTeam) (Nothing, Nothing, V2 0 0)
      let target = case mUnit of
            Just u -> Just u
            Nothing -> mBase
      -- Attack if a target is in reach.
      case target of
        Just (tEnt, _, d2) | d2 <= attackRange2 -> attack myTeam tEnt
        _ -> pure ()
      -- Advance toward the target (if any) plus the crowd-avoidance push.
      let approach = case target of
            Just (_, tPos, d2)
              | d2 > attackRange2 -> normalize (tPos - p) ^* (unitSpeed * dt)
            _ -> V2 0 0
      set ety (Position (p + approach + sep))
      pure True
  where
    dt = fromIntegral (cTick cfg) / 1e6
    gather p myTeam (mUnit, mBase, sep) (t :: Team, k :: Kind, Position q, e) =
      pure (mUnit', mBase', sep')
      where
        dv = q - p
        d2 = quadrance dv
        enemy = t /= myTeam
        sep'
          | k == Soldier && e /= ety && d2 > 1e-6 && d2 < collideDist2 =
              let d = sqrt d2
               in sep + (p - q) ^* ((collideDist - d) / d * sepStrength)
          | otherwise = sep
        mUnit'
          | enemy && k == Soldier = closer mUnit e q d2
          | otherwise = mUnit
        mBase'
          | enemy && k == Base = closer mBase e q d2
          | otherwise = mBase
    closer acc e q d2 = case acc of
      Just (_, _, best) | best <= d2 -> acc
      _ -> Just (e, q, d2)

-- | Deal damage to a victim within the caller's transaction. Soldier kills
-- score; razing a base does not (the coordinator notices that separately).
attack :: Team -> Entity -> SystemSTM ()
attack killer victim = do
  stillThere <- exists victim (Proxy @Health)
  when stillThere $ do
    Health h <- get victim
    if h - damage <= 0
      then do
        k <- get victim
        destroy victim (Proxy @All)
        when (k == Soldier) $ modify global (addKill killer)
      else set victim (Health (h - damage))

-- | The thread driving one soldier: tick, sleep, repeat, until it dies.
unitAI :: Config -> Entity -> SystemIO ()
unitAI cfg ety = do
  living <- atomically (stepUnit cfg ety)
  when living $ do
    threadDelay (cTick cfg)
    unitAI cfg ety

-- Spawning ------------------------------------------------------------------

-- | Living soldiers a team currently fields.
teamUnitCount :: Team -> SystemSTM Int
teamUnitCount team =
  cfold
    (\n (t :: Team, k :: Kind) -> if t == team && k == Soldier then n + 1 else n)
    (0 :: Int)

-- | A point uniformly inside a disk of the given radius.
randomDisk :: Float -> SystemIO (V2 Float)
randomDisk r = liftIO $ do
  a <- randomRIO (0, 2 * pi)
  rr <- randomRIO (0, r)
  pure (V2 (rr * cos a) (rr * sin a))

{- | A base's reinforcement thread. Each iteration it tries to enlist one
soldier, but the enlistment transaction blocks on STM 'check'/'retry' while
the team is at its population cap, unblocking the instant a casualty frees a
slot. The thread stops when its base is destroyed or the round ends.

Crucially the liveness guard lives /inside/ the transaction, so when the round
is reset (every entity destroyed at once) a parked enlistment re-runs, sees its
base gone, and aborts cleanly instead of spawning into a dead round.
-}
spawnerThread :: Config -> Entity -> Team -> SystemIO ()
spawnerThread cfg base team = loop
  where
    loop = do
      off <- randomDisk spawnRadius
      mE <- atomically $ do
        baseAlive <- exists base (Proxy @Health)
        ph <- get global
        reserves <- teamReserves team <$> get global
        if not (baseAlive && isPlaying ph) || reserves <= 0
          then pure Nothing
          else do
            n <- teamUnitCount team
            check (n < capPerTeam) -- block here until a slot frees
            modify global (spendReserve team)
            Just <$> newEntity (team, Soldier, Position (basePos team + off), Health startHp)
      case mE of
        Nothing -> pure () -- base dead, round over, or reserves spent: retire
        Just e -> do
          void $ forkSys (unitAI cfg e)
          threadDelay (cSpawn cfg)
          loop

-- Round lifecycle -----------------------------------------------------------

-- | Wipe the battlefield and stand up a fresh round: two bases, a starting
-- platoon each, and a reinforcement thread per base. Returns the base ids so
-- the coordinator can watch them.
startRound :: Config -> SystemIO (Entity, Entity)
startRound cfg = do
  atomically $ do
    cmapM_ $ \(_ :: Team, e :: Entity) -> destroy e (Proxy @All)
    set global (mempty :: KillScore)
    set global (Reserves reservePool reservePool)
    set global Playing
  redBase <- atomically $ newEntity (Red, Base, Position (basePos Red), Health baseHp)
  blueBase <- atomically $ newEntity (Blue, Base, Position (basePos Blue), Health baseHp)
  forM_ [Red, Blue] $ \team ->
    forM_ [1 .. initialPlatoon] $ \(_ :: Int) -> do
      off <- randomDisk spawnRadius
      e <- atomically $ newEntity (team, Soldier, Position (basePos team + off), Health startHp)
      void $ forkSys (unitAI cfg e)
  void $ forkSys (spawnerThread cfg redBase Red)
  void $ forkSys (spawnerThread cfg blueBase Blue)
  dbg cfg "[round] started: 2 bases, 2 platoons, 2 spawners"
  pure (redBase, blueBase)

-- | The single round coordinator. Runs the whole match: start a round, wait
-- for a base to fall, tally the win, linger on the score table, repeat.
coordinator :: Config -> SystemIO ()
coordinator cfg = loop 1
  where
    loop n = do
      (redBase, blueBase) <- startRound cfg
      when (cDebug cfg) (void $ forkSys heartbeat)
      winner <- waitWinner redBase blueBase
      atomically $ do
        modify global (addWin winner)
        set global (RoundOver winner)
      report n winner
      threadDelay (cOver cfg)
      if cRounds cfg /= 0 && n >= cRounds cfg
        then liftIO $ putStrLn "=== Match over ==="
        else loop (n + 1)

    -- A gated 2 Hz pulse of the round's vital signs: soldier counts, unspent
    -- reserves, and total soldier HP. Falling HP means the line is fighting;
    -- counts and reserves trending to zero means a side is losing.
    heartbeat = do
      (ph, ru, bu, Reserves rr br, hp) <- atomically $ do
        ph <- get global
        ru <- teamUnitCount Red
        bu <- teamUnitCount Blue
        res <- get global
        hp <- cfold (\acc (Health h, k :: Kind) -> if k == Soldier then acc + h else acc) (0 :: Float)
        pure (ph, ru, bu, res, hp)
      traceM $
        "[hb] R u=" ++ show ru ++ " res=" ++ show rr
          ++ " | B u=" ++ show bu ++ " res=" ++ show br
          ++ " | soldierHP=" ++ show (round hp :: Int)
      when (isPlaying ph) (threadDelay 500000 >> heartbeat)

    waitWinner redBase blueBase = do
      m <- atomically $ do
        redAlive <- exists redBase (Proxy @Health)
        blueAlive <- exists blueBase (Proxy @Health)
        if not blueAlive
          then pure (Just Red)
          else
            if not redAlive
              then pure (Just Blue)
              else do
                -- Neither base razed yet. If both teams are simultaneously
                -- out of soldiers and reserves, no one can land the final
                -- blow, so break the deadlock on surviving base health.
                redUnits <- teamUnitCount Red
                blueUnits <- teamUnitCount Blue
                Reserves redRes blueRes <- get global
                let redSpent = redUnits == 0 && redRes <= 0
                    blueSpent = blueUnits == 0 && blueRes <= 0
                if redSpent && blueSpent
                  then do
                    Health redHp <- get redBase
                    Health blueHp <- get blueBase
                    pure (Just (if blueHp >= redHp then Blue else Red))
                  else pure Nothing
      case m of
        Just w -> pure w
        Nothing -> threadDelay (cPoll cfg) >> waitWinner redBase blueBase

    report n winner = do
      KillScore kr kb <- atomically (get global)
      Wins wr wb <- atomically (get global)
      liftIO . putStrLn $
        concat
          [ "Round "
          , show n
          , ": "
          , show winner
          , " wins"
          , "  | kills R/B "
          , show kr
          , "/"
          , show kb
          , "  | match wins R/B "
          , show wr
          , "/"
          , show wb
          ]

-- Rendering & input (main thread) -------------------------------------------

label :: Color -> Float -> Float -> String -> Picture
label col x y = color col . translate x y . scale 0.12 0.12 . Text

draw :: SystemIO Picture
draw = do
  -- Fold the whole frame in a single STM transaction: a consistent snapshot,
  -- never a half-destroyed unit, even with hundreds of threads mutating the
  -- stores. We tally live populations in the same pass.
  (units, redPop, bluePop, KillScore kr kb, Wins wr wb, phase) <-
    atomically $ do
      (pic, rp, bp) <-
        cfoldM
          ( \(!acc, !rp, !bp) (t :: Team, k :: Kind, Position (V2 x y), Health hp) ->
              let glyph = case k of
                    Soldier -> color (teamColor t) (circleSolid unitRadius)
                    Base ->
                      color white (circle baseRadius)
                        <> color (teamColor t) (circleSolid (baseRadius * max 0.05 (hp / baseHp)))
                  acc' = acc <> translate x y glyph
                  (rp', bp') = case (t, k) of
                    (Red, Soldier) -> (rp + 1, bp)
                    (Blue, Soldier) -> (rp, bp + 1)
                    _ -> (rp, bp)
               in pure (acc', rp', bp')
          )
          (mempty, 0 :: Int, 0 :: Int)
      ks <- get global :: SystemSTM KillScore
      wns <- get global :: SystemSTM Wins
      ph <- get global :: SystemSTM Phase
      pure (pic, rp, bp, ks, wns, ph)
  -- Two left-aligned rows: a single long row clips off the right window edge.
  let hud =
        label (teamColor Red) (-310) 208 ("RED   pop " ++ show redPop ++ "   kills " ++ show kr ++ "   wins " ++ show wr)
          <> label (teamColor Blue) (-310) 188 ("BLUE  pop " ++ show bluePop ++ "   kills " ++ show kb ++ "   wins " ++ show wb)
      overlay = case phase of
        Playing -> mempty
        RoundOver w ->
          color (withAlpha 0.6 black) (rectangleSolid 640 480)
            <> label white (-150) 60 "ROUND OVER"
            <> label (teamColor w) (-150) 10 (show w ++ " TEAM WINS")
            <> label white (-150) (-40) ("kills  R " ++ show kr ++ "   B " ++ show kb)
            <> label white (-150) (-80) ("match  R " ++ show wr ++ "   B " ++ show wb)
  pure (units <> hud <> overlay)

handleEvent :: Event -> SystemIO ()
handleEvent (EventKey (SpecialKey KeyEsc) Down _ _) = liftIO exitSuccess
handleEvent _ = pure ()

-- | Deliberately empty: all game logic runs on the forked threads.
step :: Float -> SystemIO ()
step _ = pure ()

main :: IO ()
main = do
  args <- getArgs
  let cfg
        | any (`elem` ["--headless", "headless"]) args = headlessCfg
        | otherwise = displayCfg
  w <- initWorld
  runWith w $ do
    set global (Camera 0 1)
    if cHeadless cfg
      then coordinator cfg -- runs in the main thread; process exits when it returns
      else do
        void $ forkSys (coordinator cfg)
        play (InWindow "STM Colony War" (640, 480) (10, 10)) black 60 draw handleEvent step
