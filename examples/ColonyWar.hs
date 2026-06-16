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

Every unit, every base, and every colony's strategist is its own thread running
its own system, all mutating shared component stores concurrently. The gloss
step loop does /nothing/: all game logic lives in the forked threads,
synchronised purely through STM.

This is exactly the workload that breaks a non-STM ECS:

  * Hundreds of threads allocate entities through the shared, atomic STM
    'EntityCounter' (vanilla @nextEntity@ would hand out duplicate ids).

  * Each unit's whole turn -- find target, deal damage, score the kill, destroy
    the corpse -- runs as a single 'atomically' transaction. Two attackers
    landing a lethal blow on the same victim in the same instant cannot both
    score the kill: STM serialises them.

  * Two strategist threads concurrently read the world through a fog of war and
    write each colony's spawn plan into one shared 'Plans' cell; STM keeps the
    two read-modify-writes from clobbering each other.

  * The renderer folds the entire frame inside one transaction, so it always
    draws a consistent snapshot even while units are being destroyed under it.

Gameplay:

  * Three unit types form a rock-paper-scissors triad -- Warrior > Scout,
    Scout > Siege, Siege > Warrior (countering type deals double, countered
    deals half; Siege also wrecks bases).

  * Fog of war: a base sees ~a quarter of the field around itself; units (the
    far-sighted Scouts especially) extend that with recon. Each colony's
    strategist only counts the enemies it can actually see, and queues the type
    that counters whatever it spots.

  * The strategist also plants a waypoint; fresh units march there to muster,
    then break off to engage once an enemy enters their own vision.

Controls: @v@ toggles the vision overlay, @Esc@ quits.

Run windowed (default) or headless and faster-than-realtime for testing:

  > stack run stm-colony-war               -- gloss window
  > stack run stm-colony-war -- --headless -- no window, fast clock, time-boxed

Build with @-threaded -N@ so the threads actually run in parallel.
-}
module Main (main) where

import Control.Monad (forM_, void, when)
import Debug.Trace (traceM)
import System.Environment (getArgs)

import Apecs.Gloss
import Apecs.STM.Prelude
import Linear (V2 (..), normalize, quadrance, (^*), (^/))
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

-- | The rock-paper-scissors triad. Only soldiers carry one.
data UnitType = Warrior | Scout | Siege deriving (Eq, Show, Enum, Bounded)
instance Component UnitType where type Storage UnitType = Map UnitType

-- | Kills scored this round, by (Red, Blue). A 'Global'.
data KillScore = KillScore !Int !Int deriving (Show)
instance Semigroup KillScore where
  KillScore a b <> KillScore c d = KillScore (a + c) (b + d)
instance Monoid KillScore where
  mempty = KillScore 0 0
instance Component KillScore where type Storage KillScore = Global KillScore

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

-- | One colony's standing orders: which type to enlist next, and where to
-- muster. Set by the strategist, read by the spawner and every unit.
data TeamPlan = TeamPlan
  { planNext :: !UnitType
  , planWaypoint :: !(V2 Float)
  }
  deriving (Show)

-- | Both colonies' plans, by (Red, Blue). A single shared 'Global' that two
-- strategist threads concurrently read-modify-write.
data Plans = Plans TeamPlan TeamPlan deriving (Show)
instance Semigroup Plans where _ <> b = b
instance Monoid Plans where
  mempty = Plans (TeamPlan Scout (V2 0 0)) (TeamPlan Scout (V2 0 0))
instance Component Plans where type Storage Plans = Global Plans

-- | Whether to draw the vision overlay. A 'Global' toggled from input.
newtype ShowVision = ShowVision Bool
instance Semigroup ShowVision where _ <> b = b
instance Monoid ShowVision where mempty = ShowVision False
instance Component ShowVision where type Storage ShowVision = Global ShowVision

-- | Every component an entity owns, so we can delete it in one go (the extra
-- deletes are harmless no-ops for entities that lack a component).
type All = (Position, Health, Team, Kind, UnitType)

makeWorld
  "World"
  [ ''Position
  , ''Health
  , ''Team
  , ''Kind
  , ''UnitType
  , ''KillScore
  , ''Wins
  , ''Phase
  , ''Plans
  , ''ShowVision
  , ''Camera
  ]

type SystemIO a = SystemT World IO a
type SystemSTM a = SystemT World STM a

-- Run configuration ---------------------------------------------------------

data Config = Config
  { cHeadless :: Bool
  -- ^ No window; run the AI clock as fast as it will go.
  , cTick :: Int
  -- ^ Per-soldier AI tick, microseconds.
  , cSpawn :: Int
  -- ^ Reinforcement interval per base, microseconds.
  , cStrategy :: Int
  -- ^ How often each colony re-reads the fog and re-plans, microseconds.
  , cPoll :: Int
  -- ^ How often the round coordinator checks for a winner, microseconds.
  , cOver :: Int
  -- ^ How long the score table lingers between rounds, microseconds.
  , cMaxSeconds :: Int
  -- ^ Headless wall-clock budget before the process exits (0 == unbounded).
  , cDebug :: Bool
  -- ^ Emit 'traceM' diagnostics.
  }

displayCfg :: Config
displayCfg =
  Config
    { cHeadless = False
    , cTick = 20000 -- 50 Hz
    , cSpawn = 110000
    , cStrategy = 250000
    , cPoll = 100000
    , cOver = 4000000
    , cMaxSeconds = 0
    , cDebug = False
    }

headlessCfg :: Config
headlessCfg =
  Config
    { cHeadless = True
    , cTick = 1500 -- the whole battle plays out fast
    , cSpawn = 8000
    , cStrategy = 30000
    , cPoll = 5000
    , cOver = 300000
    , cMaxSeconds = 35
    , cDebug = True
    }

dbg :: Config -> String -> SystemIO ()
dbg cfg msg = when (cDebug cfg) (traceM msg)

-- Tunables ------------------------------------------------------------------

capPerTeam, initialPlatoon :: Int
capPerTeam = 60
initialPlatoon = 14

baseHp :: Float
baseHp = 600

-- | A base sees roughly a quarter of the (640-wide) field around itself.
baseVision, waypointReach :: Float
baseVision = 160
waypointReach = 28

-- | Soldiers shove each other apart so they don't stack into one pixel.
baseRadius, collideDist, sepStrength, spawnRadius :: Float
baseRadius = 22
collideDist = 11
sepStrength = 0.6
spawnRadius = 40

collideDist2, waypointReach2 :: Float
collideDist2 = collideDist * collideDist
waypointReach2 = waypointReach * waypointReach

sq :: Float -> Float
sq x = x * x

-- Per-type stats. The triad: fragile far-seeing Scouts, balanced Warriors,
-- slow hard-hitting short-sighted Siege.
typeHp, typeSpeed, typeDamage, typeVision, typeRange :: UnitType -> Float
typeHp Warrior = 100
typeHp Scout = 55
typeHp Siege = 170
typeSpeed Warrior = 70
typeSpeed Scout = 120
typeSpeed Siege = 44
typeDamage Warrior = 12
typeDamage Scout = 7
typeDamage Siege = 24
typeVision Warrior = 95
typeVision Scout = 170
typeVision Siege = 75
typeRange Warrior = 14
typeRange Scout = 12
typeRange Siege = 20

-- | Does an attacker of the first type hard-counter a defender of the second?
beats :: UnitType -> UnitType -> Bool
beats Warrior Scout = True
beats Scout Siege = True
beats Siege Warrior = True
beats _ _ = False

-- | The type to enlist in order to beat the given enemy type.
counter :: UnitType -> UnitType
counter Scout = Warrior
counter Siege = Scout
counter Warrior = Siege

-- | Damage multiplier from the triad: double if you counter, half if countered.
advantage :: UnitType -> UnitType -> Float
advantage atk def
  | beats atk def = 2.0
  | beats def atk = 0.5
  | otherwise = 1.0

-- | Spawn anchor for a team, at the left/right edge of the battlefield.
basePos :: Team -> V2 Float
basePos Red = V2 (-260) 0
basePos Blue = V2 260 0

enemyOf :: Team -> Team
enemyOf Red = Blue
enemyOf Blue = Red

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

teamPlan :: Team -> Plans -> TeamPlan
teamPlan Red (Plans r _) = r
teamPlan Blue (Plans _ b) = b

setTeamPlan :: Team -> TeamPlan -> Plans -> Plans
setTeamPlan Red p (Plans _ b) = Plans p b
setTeamPlan Blue p (Plans r _) = Plans r p

-- | A forward muster point at round start: a third of the way to the foe.
initialPlan :: Team -> TeamPlan
initialPlan team = TeamPlan Scout (b + (e - b) ^* 0.35)
  where
    b = basePos team
    e = basePos (enemyOf team)

-- Per-soldier AI ------------------------------------------------------------

{- | One atomic turn for a single soldier. Returns whether it is still alive
afterwards. Running this via 'atomically' makes the whole
find-target / separate / move / attack / kill / score sequence indivisible.

A single fog-limited fold over the world gathers, all within this soldier's own
vision radius:

  * the nearest visible enemy soldier (the primary target),
  * the nearest visible enemy base    (the target once their soldiers clear),
  * a separation push away from crowding soldiers.

With nothing in sight the soldier marches to its colony's waypoint instead.
-}
stepUnit :: Config -> Entity -> SystemSTM Bool
stepUnit cfg ety = do
  alive <- exists ety (Proxy @Health)
  if not alive
    then pure False
    else do
      myTeam <- get ety
      myType <- get ety
      Position p <- get ety
      let vis2 = sq (typeVision myType)
          range2 = sq (typeRange myType)
          speed = typeSpeed myType
      (mUnit, mBase, sep) <-
        cfoldM (gather p myTeam vis2) (Nothing, Nothing, V2 0 0)
      let target = case mUnit of
            Just u -> Just u
            Nothing -> mBase
      -- Attack a visible target that is in reach.
      case target of
        Just (tEnt, _, d2) | d2 <= range2 -> attack myTeam myType tEnt
        _ -> pure ()
      -- Move: close on the target, else muster at the colony's waypoint.
      wp <- planWaypoint . teamPlan myTeam <$> get global
      let dest = case target of
            Just (_, tPos, d2) | d2 > range2 -> Just tPos
            Just _ -> Nothing -- in range: hold and fight
            Nothing
              | quadrance (wp - p) > waypointReach2 -> Just wp
              | otherwise -> Nothing
          approach = case dest of
            Just d -> normalize (d - p) ^* (speed * dt)
            Nothing -> V2 0 0
      set ety (Position (p + approach + sep))
      pure True
  where
    dt = fromIntegral (cTick cfg) / 1e6
    gather p myTeam vis2 (mUnit, mBase, sep) (t :: Team, k :: Kind, Position q, e) =
      pure (mUnit', mBase', sep')
      where
        dv = q - p
        d2 = quadrance dv
        seen = t /= myTeam && d2 <= vis2
        sep'
          | k == Soldier && e /= ety && d2 > 1e-6 && d2 < collideDist2 =
              let d = sqrt d2
               in sep + (p - q) ^* ((collideDist - d) / d * sepStrength)
          | otherwise = sep
        mUnit'
          | seen && k == Soldier = closer mUnit e q d2
          | otherwise = mUnit
        mBase'
          | seen && k == Base = closer mBase e q d2
          | otherwise = mBase
    closer acc e q d2 = case acc of
      Just (_, _, best) | best <= d2 -> acc
      _ -> Just (e, q, d2)

{- | Deal damage to a victim within the caller's transaction. Triad advantage
scales soldier-vs-soldier damage; Siege does double against a base. Soldier
kills score; razing a base does not (the coordinator notices that separately).
-}
attack :: Team -> UnitType -> Entity -> SystemSTM ()
attack killer atkType victim = do
  stillThere <- exists victim (Proxy @Health)
  when stillThere $ do
    Health h <- get victim
    k <- get victim
    dmg <- case k of
      Base -> pure (typeDamage atkType * (if atkType == Siege then 2 else 1))
      Soldier -> do
        defType <- get victim
        pure (typeDamage atkType * advantage atkType defType)
    if h - dmg <= 0
      then do
        destroy victim (Proxy @All)
        when (k == Soldier) $ modify global (addKill killer)
      else set victim (Health (h - dmg))

-- | The thread driving one soldier: tick, sleep, repeat, until it dies.
unitAI :: Config -> Entity -> SystemIO ()
unitAI cfg ety = do
  living <- atomically (stepUnit cfg ety)
  when living $ do
    threadDelay (cTick cfg)
    unitAI cfg ety

-- Recon & strategy ----------------------------------------------------------

-- | Live soldiers of a team, tallied by type as (Warrior, Scout, Siege).
type Census = (Int, Int, Int)

bumpType :: UnitType -> Census -> Census
bumpType Warrior (w, s, c) = (w + 1, s, c)
bumpType Scout (w, s, c) = (w, s + 1, c)
bumpType Siege (w, s, c) = (w, s, c + 1)

-- | The most numerous type in a census (ties favour Warrior, then Scout).
majorityType :: Census -> UnitType
majorityType (w, s, c)
  | w >= s && w >= c = Warrior
  | s >= c = Scout
  | otherwise = Siege

-- | What to enlist next given the visible enemy census: counter their main
-- force, or -- seeing nothing through the fog -- send Scouts to look.
chooseNext :: Census -> UnitType
chooseNext (0, 0, 0) = Scout
chooseNext census = counter (majorityType census)

-- | Living soldiers a team currently fields.
teamUnitCount :: Team -> SystemSTM Int
teamUnitCount team =
  cfold
    (\n (t :: Team, k :: Kind) -> if t == team && k == Soldier then n + 1 else n)
    (0 :: Int)

{- | One planning pass for a colony, run as a transaction. It looks through the
fog -- the union of its base's sight and every friendly soldier's recon -- to
census the enemies it can see, then writes the counter-type and a muster
waypoint into the shared 'Plans'. Two of these run concurrently; STM keeps the
read-modify-write on the single 'Plans' cell from racing.
-}
planFor :: Team -> SystemSTM ()
planFor team = do
  -- Friendly vision sources: the base, plus each soldier's own sight radius.
  unitSrcs <-
    cfold
      (\acc (t :: Team, ut :: UnitType, Position q) -> if t == team then (q, typeVision ut) : acc else acc)
      []
  let sources = (basePos team, baseVision) : unitSrcs
  enemies <-
    cfold
      (\acc (t :: Team, ut :: UnitType, Position q) -> if t /= team then (q, ut) : acc else acc)
      []
  let visible = [(q, ut) | (q, ut) <- enemies, seenBy sources q]
      census = foldr (bumpType . snd) (0, 0, 0) visible
      waypoint = chooseWaypoint team (map fst visible)
  modify global (setTeamPlan team (TeamPlan (chooseNext census) waypoint))
  where
    seenBy srcs q = any (\(s, r) -> quadrance (q - s) <= r * r) srcs

-- | Muster where the enemy was last seen; with the field dark, press forward.
chooseWaypoint :: Team -> [V2 Float] -> V2 Float
chooseWaypoint team [] = b + (e - b) ^* 0.4
  where
    b = basePos team
    e = basePos (enemyOf team)
chooseWaypoint _ ps = foldr (+) (V2 0 0) ps ^/ fromIntegral (length ps)

-- | A colony's strategist thread: re-plan from the fog until the round ends.
strategist :: Config -> Team -> Entity -> SystemIO ()
strategist cfg team base = loop
  where
    loop = do
      continue <- atomically $ do
        baseAlive <- exists base (Proxy @Health)
        ph <- get global
        let ok = baseAlive && isPlaying ph
        when ok (planFor team)
        pure ok
      when continue (threadDelay (cStrategy cfg) >> loop)

-- Spawning ------------------------------------------------------------------

-- | A point uniformly inside a disk of the given radius.
randomDisk :: Float -> SystemIO (V2 Float)
randomDisk r = liftIO $ do
  a <- randomRIO (0, 2 * pi)
  rr <- randomRIO (0, r)
  pure (V2 (rr * cos a) (rr * sin a))

{- | A base's reinforcement thread. Each iteration enlists one soldier of
whatever type its strategist currently wants, blocking on STM 'check'/'retry'
while the team is at its population cap and unblocking the instant a casualty
frees a slot. The thread stops when its base is destroyed or the round ends.

The liveness guard lives /inside/ the transaction, so when the round resets
(every entity destroyed at once) a parked enlistment re-runs, sees its base
gone, and aborts cleanly instead of spawning into a dead round.
-}
spawnerThread :: Config -> Entity -> Team -> SystemIO ()
spawnerThread cfg base team = loop
  where
    loop = do
      off <- randomDisk spawnRadius
      mE <- atomically $ do
        baseAlive <- exists base (Proxy @Health)
        ph <- get global
        if not (baseAlive && isPlaying ph)
          then pure Nothing
          else do
            n <- teamUnitCount team
            check (n < capPerTeam) -- block here until a slot frees
            ut <- planNext . teamPlan team <$> get global
            Just <$> newEntity (team, Soldier, ut, Position (basePos team + off), Health (typeHp ut))
      case mE of
        Nothing -> pure () -- base dead or round over: retire
        Just e -> do
          void $ forkSys (unitAI cfg e)
          threadDelay (cSpawn cfg)
          loop

-- Round lifecycle -----------------------------------------------------------

-- | Wipe the battlefield and stand up a fresh round: two bases, a starting
-- Warrior platoon each, and a spawner + strategist thread per colony. Returns
-- the base ids so the coordinator can watch them.
startRound :: Config -> SystemIO (Entity, Entity)
startRound cfg = do
  atomically $ do
    cmapM_ $ \(_ :: Team, e :: Entity) -> destroy e (Proxy @All)
    set global (mempty :: KillScore)
    set global (Plans (initialPlan Red) (initialPlan Blue))
    set global Playing
  redBase <- atomically $ newEntity (Red, Base, Position (basePos Red), Health baseHp)
  blueBase <- atomically $ newEntity (Blue, Base, Position (basePos Blue), Health baseHp)
  forM_ [Red, Blue] $ \team ->
    forM_ [1 .. initialPlatoon] $ \(_ :: Int) -> do
      off <- randomDisk spawnRadius
      e <- atomically $ newEntity (team, Soldier, Warrior, Position (basePos team + off), Health (typeHp Warrior))
      void $ forkSys (unitAI cfg e)
  void $ forkSys (spawnerThread cfg redBase Red)
  void $ forkSys (spawnerThread cfg blueBase Blue)
  void $ forkSys (strategist cfg Red redBase)
  void $ forkSys (strategist cfg Blue blueBase)
  dbg cfg "[round] started: 2 bases, 2 platoons, 2 spawners, 2 strategists"
  pure (redBase, blueBase)

{- | The single round coordinator: start a round, run until a base falls (which
may be a very long time -- the war is meant to ebb and flow), tally the win,
linger on the score table, repeat.
-}
coordinator :: Config -> SystemIO ()
coordinator cfg = loop (1 :: Int)
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
      loop (n + 1)

    -- A gated 2 Hz pulse of each colony's live composition and current plan,
    -- so you can watch the triad counter-play shift through the fog.
    heartbeat = do
      (ph, rc, bc, Plans rp bp) <- atomically $ do
        ph <- get global
        rc <- teamCensus Red
        bc <- teamCensus Blue
        pl <- get global
        pure (ph, rc, bc, pl)
      traceM $
        "[hb] R " ++ showCensus rc ++ " next=" ++ show (planNext rp)
          ++ " | B " ++ showCensus bc ++ " next=" ++ show (planNext bp)
      when (isPlaying ph) (threadDelay 500000 >> heartbeat)

    teamCensus team =
      cfold (\acc (t :: Team, ut :: UnitType) -> if t == team then bumpType ut acc else acc) (0, 0, 0)

    showCensus (w, s, c) = "W" ++ show w ++ "/S" ++ show s ++ "/C" ++ show c

    waitWinner redBase blueBase = do
      m <- atomically $ do
        redAlive <- exists redBase (Proxy @Health)
        blueAlive <- exists blueBase (Proxy @Health)
        pure $
          if not blueAlive then Just Red else if not redAlive then Just Blue else Nothing
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
          , " wins  | kills R/B "
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

-- | A soldier glyph, distinct per type: small Scout dot, Warrior disc, blocky
-- Siege square.
unitGlyph :: Team -> UnitType -> Picture
unitGlyph t Warrior = color (teamColor t) (circleSolid 5)
unitGlyph t Scout = color (teamColor t) (circleSolid 3)
unitGlyph t Siege = color (teamColor t) (rectangleSolid 11 11)

-- | A base: white ring with an inner disc that shrinks as its health drops.
baseGlyph :: Team -> Float -> Picture
baseGlyph t hp =
  color white (circle baseRadius)
    <> color (teamColor t) (circleSolid (baseRadius * max 0.05 (hp / baseHp)))

draw :: SystemIO Picture
draw = do
  -- Fold the whole frame in a single STM transaction: a consistent snapshot,
  -- never a half-destroyed unit, even with hundreds of threads mutating the
  -- stores. Bases, soldiers, populations and the optional vision overlay all
  -- come from this one snapshot.
  (visionPic, basePic, soldierPic, redPop, bluePop, KillScore kr kb, Wins wr wb, phase, Plans rPlan bPlan) <-
    atomically $ do
      basePic <-
        cfoldM
          ( \acc (t :: Team, k :: Kind, Position (V2 x y), Health hp) ->
              pure $ case k of
                Base -> acc <> translate x y (baseGlyph t hp)
                Soldier -> acc
          )
          mempty
      (soldierPic, rp, bp) <-
        cfoldM
          ( \(!acc, !rp, !bp) (t :: Team, ut :: UnitType, Position (V2 x y)) ->
              let acc' = acc <> translate x y (unitGlyph t ut)
                  (rp', bp') = case t of
                    Red -> (rp + 1, bp)
                    Blue -> (rp, bp + 1)
               in pure (acc', rp', bp')
          )
          (mempty, 0 :: Int, 0 :: Int)
      ShowVision showV <- get global
      visionPic <- if showV then visionOverlay else pure mempty
      ks <- get global :: SystemSTM KillScore
      wns <- get global :: SystemSTM Wins
      ph <- get global :: SystemSTM Phase
      pl <- get global :: SystemSTM Plans
      pure (visionPic, basePic, soldierPic, rp, bp, ks, wns, ph, pl)
  -- Two left-aligned rows: a single long row clips off the right window edge.
  let hud =
        label (teamColor Red) (-310) 210 ("RED   pop " ++ show redPop ++ "   next " ++ show (planNext rPlan) ++ "   kills " ++ show kr ++ "   wins " ++ show wr)
          <> label (teamColor Blue) (-310) 190 ("BLUE  pop " ++ show bluePop ++ "   next " ++ show (planNext bPlan) ++ "   kills " ++ show kb ++ "   wins " ++ show wb)
          <> label (greyN 0.5) (-310) (-230) "v: vision   esc: quit"
      overlay = case phase of
        Playing -> mempty
        RoundOver w ->
          color (withAlpha 0.6 black) (rectangleSolid 660 500)
            <> label white (-150) 60 "ROUND OVER"
            <> label (teamColor w) (-150) 10 (show w ++ " TEAM WINS")
            <> label white (-150) (-40) ("kills  R " ++ show kr ++ "   B " ++ show kb)
            <> label white (-150) (-80) ("match  R " ++ show wr ++ "   B " ++ show wb)
  pure (visionPic <> basePic <> soldierPic <> hud <> overlay)

-- | Faint discs for every sight source: a big one per base, a small one per
-- soldier (Scouts reach furthest). Overlapping discs build up where a colony's
-- recon is densest, sketching what it can actually see through the fog.
visionOverlay :: SystemSTM Picture
visionOverlay = do
  unitDiscs <-
    cfoldM
      ( \acc (t :: Team, ut :: UnitType, Position (V2 x y)) ->
          pure $ acc <> translate x y (color (withAlpha 0.04 (teamColor t)) (circleSolid (typeVision ut)))
      )
      mempty
  let baseDiscs =
        mconcat
          [ let V2 bx by = basePos t
             in translate bx by (color (withAlpha 0.07 (teamColor t)) (circleSolid baseVision))
          | t <- [Red, Blue]
          ]
  pure (baseDiscs <> unitDiscs)

handleEvent :: Event -> SystemIO ()
handleEvent (EventKey (SpecialKey KeyEsc) Down _ _) = liftIO exitSuccess
handleEvent (EventKey (Char 'v') Down _ _) = modify global (\(ShowVision b) -> ShowVision (not b))
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
    set global (ShowVision False)
    void $ forkSys (coordinator cfg)
    if cHeadless cfg
      then do
        threadDelay (cMaxSeconds cfg * 1000000)
        liftIO $ putStrLn "=== headless time limit reached ==="
      else play (InWindow "STM Colony War" (660, 500) (10, 10)) black 60 draw handleEvent step
