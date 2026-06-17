{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
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

import Control.Monad (foldM, forM, forM_, void, when)
import Data.Char (toLower)
import Data.List (intercalate, isPrefixOf)
import qualified Data.Map.Strict as DM
import Debug.Trace (traceM)
import System.Environment (getArgs)

import Apecs.Gloss
import Apecs.STM.Prelude
import Linear (V2 (..), normalize, quadrance, (^*), (^/))
import System.Exit (exitSuccess)
import System.IO (BufferMode (LineBuffering), hSetBuffering, stderr, stdout)
import System.Random (randomRIO)

-- Components ----------------------------------------------------------------

newtype Position = Position (V2 Float) deriving (Show)
instance Component Position where type Storage Position = Map Position

newtype Health = Health Float deriving (Show)
instance Component Health where type Storage Health = Map Health

data Team = Red | Blue deriving (Eq, Ord, Show)
instance Component Team where type Storage Team = Map Team

-- | What an entity /is/. Soldiers fight and move; a Base is the stationary
-- spawn-heart a team must protect.
data Kind = Soldier | Base deriving (Eq, Show)
instance Component Kind where type Storage Kind = Map Kind

-- | The rock-paper-scissors triad. Only soldiers carry one.
data UnitType = Warrior | Scout | Siege deriving (Eq, Ord, Show, Enum, Bounded)
instance Component UnitType where type Storage UnitType = Map UnitType

-- | A transient marker on a soldier that struck this tick, carrying the point
-- it struck at. Set/cleared each turn by 'stepUnit', read only by the renderer
-- to draw a tracer line from attacker to victim. Carries no game meaning.
newtype Attacking = Attacking (V2 Float) deriving (Show)
instance Component Attacking where type Storage Attacking = Map Attacking

-- | Kills scored this round, by (Red, Blue). A 'Global'.
data KillScore = KillScore !Int !Int deriving (Show)
instance Semigroup KillScore where
  KillScore a b <> KillScore c d = KillScore (a + c) (b + d)
instance Monoid KillScore where
  mempty = KillScore 0 0
instance Component KillScore where type Storage KillScore = Global KillScore

-- | Per-round damage ledger: how much damage each team's attacker type dealt
-- to each enemy defender type. Keyed by (attacker team, attacker type,
-- defender type). A shared 'Global' every attacker pokes -- the system is
-- already all-conflicting through the position reads, so this hot cell adds
-- little: it just rides the existing serialisation.
newtype DamageLog = DamageLog (DM.Map (Team, UnitType, UnitType) Float)
instance Semigroup DamageLog where
  DamageLog a <> DamageLog b = DamageLog (DM.unionWith (+) a b)
instance Monoid DamageLog where
  mempty = DamageLog DM.empty
instance Component DamageLog where type Storage DamageLog = Global DamageLog

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

-- | Whether to draw the attack-range overlay. A 'Global' toggled from input.
newtype ShowRange = ShowRange Bool
instance Semigroup ShowRange where _ <> b = b
instance Monoid ShowRange where mempty = ShowRange False
instance Component ShowRange where type Storage ShowRange = Global ShowRange

-- | Whether to draw attacker->victim tracer lines. On by default; toggled from
-- input.
newtype ShowAttacks = ShowAttacks Bool
instance Semigroup ShowAttacks where _ <> b = b
instance Monoid ShowAttacks where mempty = ShowAttacks True
instance Component ShowAttacks where type Storage ShowAttacks = Global ShowAttacks

-- | Every component an entity owns, so we can delete it in one go (the extra
-- deletes are harmless no-ops for entities that lack a component).
type All = (Position, Health, Team, Kind, UnitType, Attacking)

makeWorld
  "World"
  [ ''Position
  , ''Health
  , ''Team
  , ''Kind
  , ''UnitType
  , ''Attacking
  , ''KillScore
  , ''DamageLog
  , ''Wins
  , ''Phase
  , ''Plans
  , ''ShowVision
  , ''ShowRange
  , ''ShowAttacks
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
  , cMuster :: Bool
  -- ^ Whether soldiers rally-and-wave (mass up before committing). Off in the
  -- matchup harness to measure raw stat combat with this behaviour ablated.
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
    , cMuster = True
    }

headlessCfg :: Config
headlessCfg =
  Config
    { cHeadless = True
    , cTick = 1500 -- the whole battle plays out fast
    , cSpawn = 14000
    , cStrategy = 30000
    , cPoll = 5000
    , cOver = 300000
    , cMaxSeconds = 180
    , cDebug = True
    , cMuster = True
    }

dbg :: Config -> String -> SystemIO ()
dbg cfg msg = when (cDebug cfg) (traceM msg)

-- Tunables ------------------------------------------------------------------

capPerTeam, initialPlatoon :: Int
capPerTeam = 34
initialPlatoon = 12

baseHp :: Float
baseHp = 380

-- | A base sees roughly a quarter of the (640-wide) field around itself.
baseVision, waypointReach :: Float
baseVision = 160
waypointReach = 28

-- | Reinforcements rally and attack in waves instead of feeding in one at a
-- time. A soldier with fewer than 'musterMin' friendly soldiers within
-- 'musterRadius' is "under-supported": rather than charge a blob that would melt
-- it, it falls back to a staging point 'rallyDist' in front of its base and
-- waits for the clump to build, committing only once it is strong enough.
musterRadius, rallyDist, musterRadius2 :: Float
musterRadius = 58
rallyDist = 95
musterRadius2 = musterRadius * musterRadius

musterMin :: Int
musterMin = 6

-- | A team's staging point: in front of its base, on the line to the enemy.
rallyPoint :: Team -> V2 Float
rallyPoint team = b + normalize (basePos (enemyOf team) - b) ^* rallyDist
  where
    b = basePos team

-- | Soldiers shove each other apart so they don't stack into one pixel. This
-- packing is deliberately tight: concentration is what lets a winning army
-- overwhelm a thinner one locally and break through, which is how a round
-- actually ends. (Roomy spacing was tried and turned every round into an
-- endless even-trade grind.) 'lineSpacing' only sets the cosmetic width of the
-- opening muster lines.
baseRadius, collideDist, sepStrength, spawnRadius, lineSpacing :: Float
baseRadius = 22
collideDist = 11
sepStrength = 0.6
spawnRadius = 40
lineSpacing = 16

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

-- | Record that @team@'s @atk@-type dealt @d@ damage to an enemy @def@-type.
logDamage :: Team -> UnitType -> UnitType -> Float -> DamageLog -> DamageLog
logDamage team atk def d (DamageLog m) = DamageLog (DM.insertWith (+) (team, atk, def) d m)

-- | A team's ledger as one row per attacker type, each carrying the damage it
-- dealt to (Warrior, Scout, Siege) defenders.
teamMatrix :: Team -> DamageLog -> [(UnitType, (Int, Int, Int))]
teamMatrix team (DamageLog m) =
  [ (atk, (val atk Warrior, val atk Scout, val atk Siege)) | atk <- [Warrior, Scout, Siege]
  ]
  where
    val atk def = round (DM.findWithDefault 0 (team, atk, def) m)

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
      (mUnit, mBase, sep, allyNear) <-
        cfoldM (gather p myTeam vis2) (Nothing, Nothing, V2 0 0, 0 :: Int)
      let target = case mUnit of
            Just u -> Just u
            Nothing -> mBase
      -- Attack a visible target that is in reach.
      let struck = case target of
            Just (tEnt, tPos, d2) | d2 <= range2 -> Just (tEnt, tPos)
            _ -> Nothing
          fighting = case struck of
            Just _ -> True
            Nothing -> False
      case struck of
        Just (tEnt, _) -> attack myTeam myType tEnt
        Nothing -> pure ()
      -- Record (display only) a tracer to whatever we hit, or clear last tick's.
      when (not (cHeadless cfg)) $ case struck of
        Just (_, tPos) -> set ety (Attacking tPos)
        Nothing -> destroy ety (Proxy @Attacking)
      -- Move. A soldier already trading blows holds and fights. One with enough
      -- friends nearby presses the target (or marches to the muster). One that
      -- is under-supported falls back to the rally point to mass up first,
      -- rather than feed itself piecemeal into the enemy blob.
      wp <- planWaypoint . teamPlan myTeam <$> get global
      let supported = not (cMuster cfg) || allyNear + 1 >= musterMin
          rally = rallyPoint myTeam
          dest
            | fighting = Nothing
            | supported = case target of
                Just (_, tPos, d2) | d2 > range2 -> Just tPos
                Just _ -> Nothing
                Nothing
                  | quadrance (wp - p) > waypointReach2 -> Just wp
                  | otherwise -> Nothing
            | quadrance (rally - p) > waypointReach2 = Just rally
            | otherwise = Nothing
          approach = case dest of
            Just d -> normalize (d - p) ^* (speed * dt)
            Nothing -> V2 0 0
      set ety (Position (p + approach + sep))
      pure True
  where
    dt = fromIntegral (cTick cfg) / 1e6
    gather p myTeam vis2 (mUnit, mBase, sep, allies) (t :: Team, k :: Kind, Position q, e) =
      pure (mUnit', mBase', sep', allies')
      where
        dv = q - p
        d2 = quadrance dv
        seen = t /= myTeam && d2 <= vis2
        sep'
          | k == Soldier && e /= ety && d2 > 1e-6 && d2 < collideDist2 =
              let d = sqrt d2
               in sep + (p - q) ^* ((collideDist - d) / d * sepStrength)
          | otherwise = sep
        -- Friendly soldiers close enough to count as local support.
        allies'
          | t == myTeam && k == Soldier && e /= ety && d2 <= musterRadius2 = allies + 1
          | otherwise = allies
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
    (dmg, mDef) <- case k of
      Base -> pure (typeDamage atkType * (if atkType == Siege then 2 else 1), Nothing)
      Soldier -> do
        defType <- get victim
        pure (typeDamage atkType * advantage atkType defType, Just defType)
    -- Ledger soldier-vs-soldier damage (the part actually applied).
    forM_ mDef $ \defType ->
      modify global (logDamage killer atkType defType (min dmg h))
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

-- | What to enlist next given the visible enemy census: the type that best
-- answers their /whole/ composition, or -- seeing nothing through the fog --
-- the far-seeing Scout to go look.
--
-- "Best answer" is the type maximising summed triad advantage over every enemy
-- unit, not merely the counter to their most numerous type. That distinction is
-- the whole game: a Siege ball screened by a few Warriors should /not/ be met
-- with Scouts (the Warriors gut them) -- weighing the Warriors in drags Scout's
-- score below Siege's, so the colony answers with Siege instead of trickling
-- fragile Scouts to their death.
chooseNext :: Census -> UnitType
chooseNext (0, 0, 0) = Scout
chooseNext census = bestResponse census

-- | The single type with the greatest summed triad advantage against a census.
-- Ties fall to the sturdier type (Siege > Scout > Warrior by 'Ord').
bestResponse :: Census -> UnitType
bestResponse (w, s, c) =
  snd (maximum [(score t, t) | t <- [Warrior, Scout, Siege]])
  where
    fw = fromIntegral w
    fs = fromIntegral s
    fc = fromIntegral c
    score t = fw * advantage t Warrior + fs * advantage t Scout + fc * advantage t Siege

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

-- | Muster on the enemy the colony can see; with the field dark, press forward
-- and march on the enemy base so a won fight turns into a breakthrough instead
-- of milling at the centre line.
chooseWaypoint :: Team -> [V2 Float] -> V2 Float
chooseWaypoint team [] = basePos (enemyOf team)
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

-- | Where a fresh soldier appears: on the base perimeter at 'spawnRadius', in
-- the direction of the muster waypoint, nudged sideways by the given tangential
-- offset so a salvo of spawns fans into a facing line instead of stacking on a
-- single pixel.
edgeSpawn :: Team -> V2 Float -> Float -> V2 Float
edgeSpawn team wp tang = b + dir ^* spawnRadius + perp ^* tang
  where
    b = basePos team
    toWp = wp - b
    dir
      | quadrance toWp > 1e-3 = normalize toWp
      | otherwise = normalize (basePos (enemyOf team) - b)
    perp = let V2 dx dy = dir in V2 (-dy) dx

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
      tang <- liftIO (randomRIO (-spawnRadius, spawnRadius))
      mE <- atomically $ do
        baseAlive <- exists base (Proxy @Health)
        ph <- get global
        if not (baseAlive && isPlaying ph)
          then pure Nothing
          else do
            n <- teamUnitCount team
            check (n < capPerTeam) -- block here until a slot frees
            plan <- teamPlan team <$> get global
            let ut = planNext plan
                pos = edgeSpawn team (planWaypoint plan) tang
            Just <$> newEntity (team, Soldier, ut, Position pos, Health (typeHp ut))
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
    set global (mempty :: DamageLog)
    set global (Plans (initialPlan Red) (initialPlan Blue))
    set global Playing
  redBase <- atomically $ newEntity (Red, Base, Position (basePos Red), Health baseHp)
  blueBase <- atomically $ newEntity (Blue, Base, Position (basePos Blue), Health baseHp)
  -- Stand both platoons up as facing battle lines. Create every soldier first
  -- and only then fork their AI, so neither colony gets a head-start of live
  -- ticks while the other is still being spawned (that asymmetry quietly biased
  -- the whole match toward Red, who used to spawn entirely first).
  units <- forM [0 .. initialPlatoon - 1] $ \i ->
    forM [Red, Blue] $ \team -> do
      let wp = planWaypoint (initialPlan team)
          off = (fromIntegral i - fromIntegral (initialPlatoon - 1) / 2) * lineSpacing
      atomically $ newEntity (team, Soldier, Warrior, Position (edgeSpawn team wp off), Health (typeHp Warrior))
  forM_ (concat units) (void . forkSys . unitAI cfg)
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
      (ph, rc, bc, Plans rp bp, DamageLog dm, rHp, bHp) <- atomically $ do
        ph <- get global
        rc <- teamCensus Red
        bc <- teamCensus Blue
        pl <- get global
        dl <- get global
        rh <- baseHpOf Red
        bh <- baseHpOf Blue
        pure (ph, rc, bc, pl, dl, rh, bh)
      let teamDmg t = round (sum [v | ((t', _, _), v) <- DM.toList dm, t' == t]) :: Int
      traceM $
        "[hb] R " ++ showCensus rc ++ " base=" ++ show (round rHp :: Int) ++ " next=" ++ show (planNext rp) ++ " dmg=" ++ show (teamDmg Red)
          ++ " | B " ++ showCensus bc ++ " base=" ++ show (round bHp :: Int) ++ " next=" ++ show (planNext bp) ++ " dmg=" ++ show (teamDmg Blue)
      when (isPlaying ph) (threadDelay 500000 >> heartbeat)

    baseHpOf team =
      cfold (\acc (t :: Team, k :: Kind, Health h) -> if t == team && k == Base then h else acc) baseHp

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
      (KillScore kr kb, Wins wr wb, dl) <-
        atomically ((,,) <$> get global <*> get global <*> get global)
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
      liftIO $ putStrLn ("  RED  " ++ matrixLine (teamMatrix Red dl))
      liftIO $ putStrLn ("  BLUE " ++ matrixLine (teamMatrix Blue dl))

    -- One-line ledger: each attacker type with the damage it dealt to
    -- (vs Warrior / vs Scout / vs Siege).
    matrixLine rows =
      "damage " ++ intercalate "  " [show atk ++ " " ++ show (w, s, c) | (atk, (w, s, c)) <- rows]

-- Rendering & input (main thread) -------------------------------------------

label :: Color -> Float -> Float -> String -> Picture
label col x y = color col . translate x y . scale 0.12 0.12 . Text

-- | A soldier glyph, distinct per type: small Scout dot, Warrior disc, and a
-- bigger ringed disc for the heavy Siege (a filled circle with a darker rim).
unitGlyph :: Team -> UnitType -> Picture
unitGlyph t Warrior = color (teamColor t) (circleSolid 5)
unitGlyph t Scout = color (teamColor t) (circleSolid 3)
unitGlyph t Siege =
  color (teamColor t) (circleSolid 7)
    <> color (dark (dark (teamColor t))) (thickCircle 7 2)

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
  (visionPic, attackPic, basePic, soldierPic, redPop, bluePop, KillScore kr kb, Wins wr wb, phase, Plans rPlan bPlan, dl) <-
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
      ShowRange showR <- get global
      ShowAttacks showA <- get global
      visionPic <-
        mappend
          <$> (if showV then visionOverlay else pure mempty)
          <*> (if showR then rangeOverlay else pure mempty)
      attackPic <- if showA then attackLines else pure mempty
      ks <- get global :: SystemSTM KillScore
      wns <- get global :: SystemSTM Wins
      ph <- get global :: SystemSTM Phase
      pl <- get global :: SystemSTM Plans
      dmg <- get global :: SystemSTM DamageLog
      pure (visionPic, attackPic, basePic, soldierPic, rp, bp, ks, wns, ph, pl, dmg)
  -- Two left-aligned rows: a single long row clips off the right window edge.
  let hud =
        label (teamColor Red) (-310) 210 ("RED   pop " ++ show redPop ++ "   next " ++ show (planNext rPlan) ++ "   kills " ++ show kr ++ "   wins " ++ show wr)
          <> label (teamColor Blue) (-310) 190 ("BLUE  pop " ++ show bluePop ++ "   next " ++ show (planNext bPlan) ++ "   kills " ++ show kb ++ "   wins " ++ show wb)
          <> label (greyN 0.5) (-310) (-230) "v: vision   r: range   a: attacks   esc: quit"
      overlay = case phase of
        Playing -> mempty
        RoundOver w ->
          color (withAlpha 0.74 black) (rectangleSolid 660 500)
            <> label white (-58) 215 "ROUND OVER"
            <> label (teamColor w) (-85) 185 (show w ++ " TEAM WINS")
            <> label white (-150) 158 ("kills R " ++ show kr ++ " / B " ++ show kb ++ "     match R " ++ show wr ++ " / B " ++ show wb)
            <> damageBlock (teamColor Red) (-312) 116 "RED damage  (attacker vs defender)" (teamMatrix Red dl)
            <> damageBlock (teamColor Blue) (-312) (-24) "BLUE damage  (attacker vs defender)" (teamMatrix Blue dl)
  -- Tracers go on /top/ of the glyphs: a hit only fires within attack range, so
  -- the line is short and would otherwise hide under the units it connects.
  pure (visionPic <> basePic <> soldierPic <> attackPic <> hud <> overlay)

-- | A round-end ledger block: a title and one line per attacker type listing
-- the damage it dealt to (vs Warrior / vs Scout / vs Siege).
damageBlock :: Color -> Float -> Float -> String -> [(UnitType, (Int, Int, Int))] -> Picture
damageBlock col x y title rows =
  smallLabel col x y title
    <> mconcat
      [ smallLabel col x (y - 18 * fromIntegral (i + 1)) (rowText atk w s c)
      | (i, (atk, (w, s, c))) <- zip [0 :: Int ..] rows
      ]
  where
    rowText atk w s c =
      pad 9 (show atk) ++ "vsW " ++ pad 6 (show w) ++ "vsS " ++ pad 6 (show s) ++ "vsC " ++ show c
    pad n s = take n (s ++ repeat ' ')

smallLabel :: Color -> Float -> Float -> String -> Picture
smallLabel col x y = color col . translate x y . scale 0.1 0.1 . Text

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

-- | An unfilled ring at each soldier's attack reach, so Siege's longer bite is
-- visible at a glance.
rangeOverlay :: SystemSTM Picture
rangeOverlay =
  cfoldM
    ( \acc (t :: Team, ut :: UnitType, Position (V2 x y)) ->
        pure $ acc <> translate x y (color (withAlpha 0.5 (teamColor t)) (circle (typeRange ut)))
    )
    mempty

-- | A faint team-coloured tracer from every soldier that struck this tick to
-- the point it struck, so the who-fights-whom of the melee is legible at a
-- glance. Each line lasts exactly one tick: 'stepUnit' clears it next turn.
attackLines :: SystemSTM Picture
attackLines =
  cfoldM
    ( \acc (t :: Team, Position (V2 x y), Attacking (V2 tx ty)) ->
        let col = withAlpha 0.85 (light (light (teamColor t)))
            -- A dot at the struck point makes a sub-20px tracer legible.
         in pure $ acc <> color col (line [(x, y), (tx, ty)] <> translate tx ty (circleSolid 1.5))
    )
    mempty

handleEvent :: Event -> SystemIO ()
handleEvent (EventKey (SpecialKey KeyEsc) Down _ _) = liftIO exitSuccess
handleEvent (EventKey (Char 'v') Down _ _) = modify global (\(ShowVision b) -> ShowVision (not b))
handleEvent (EventKey (Char 'r') Down _ _) = modify global (\(ShowRange b) -> ShowRange (not b))
handleEvent (EventKey (Char 'a') Down _ _) = modify global (\(ShowAttacks b) -> ShowAttacks (not b))
handleEvent _ = pure ()

-- | Deliberately empty: all game logic runs on the forked threads.
step :: Float -> SystemIO ()
step _ = pure ()

-- Matchup harness -----------------------------------------------------------

{- | A controlled, scriptable combat test that isolates unit balance from the
strategist. Two fixed armies are spawned facing each other and stepped
/synchronously/ (no threads, no sleeps, full information) until one side is
eliminated, so thousands of battles run in seconds and a result is decisive.
This is the instrument behind the "real counters" / "no fixpoint" hypotheses:
sweep the type x type grid at equal numbers and read off who actually wins.

  > stm-colony-war --matchup warrior:10 scout:10 [reps] [--muster]
-}
matchupCfg :: Bool -> Config
matchupCfg muster =
  headlessCfg {cTick = 16000, cDebug = False, cMuster = muster}

-- | Parse @"warrior:10,siege:5"@ into a unit roster.
parseComp :: String -> [(UnitType, Int)]
parseComp s = [parse1 part | part <- splitOn ',' s]
  where
    parse1 p = case splitOn ':' p of
      [t, n] -> (parseType t, read n)
      _ -> error ("matchup: bad component " ++ show p)
    parseType t = case map toLower t of
      "warrior" -> Warrior
      "w" -> Warrior
      "scout" -> Scout
      "s" -> Scout
      "siege" -> Siege
      "c" -> Siege
      other -> error ("matchup: bad unit type " ++ show other)

splitOn :: Char -> String -> [String]
splitOn c s = case break (== c) s of
  (a, []) -> [a]
  (a, _ : rest) -> a : splitOn c rest

-- | Spawn a roster around an anchor on a small deterministic grid (separation
-- then spreads them); no base, no spawner, no strategist.
spawnArmy :: Team -> [(UnitType, Int)] -> V2 Float -> SystemIO ()
spawnArmy team comp (V2 ax ay) =
  forM_ (zip [0 :: Int ..] roster) $ \(i, ut) -> do
    let gx = fromIntegral (i `mod` 5) * 9 - 18
        gy = fromIntegral (i `div` 5) * 9
    void . atomically $
      newEntity (team, Soldier, ut, Position (V2 (ax + gx) (ay + gy)), Health (typeHp ut))
  where
    roster = concat [replicate n ut | (ut, n) <- comp]

-- | Step every soldier once per tick until one side is gone or the clock caps.
runBattle :: Config -> SystemIO (Maybe Team)
runBattle cfg = loop (0 :: Int)
  where
    maxTicks = 6000
    loop t
      | t >= maxTicks = pure Nothing -- unresolved: a draw/stalemate
      | otherwise = do
          ents <-
            atomically $
              cfold (\acc (k :: Kind, e :: Entity) -> if k == Soldier then e : acc else acc) []
          mapM_ (\e -> atomically (void (stepUnit cfg e))) ents
          (r, b) <- atomically teamCounts
          if
            | r == 0 && b == 0 -> pure Nothing
            | b == 0 -> pure (Just Red)
            | r == 0 -> pure (Just Blue)
            | otherwise -> loop (t + 1)
    teamCounts =
      cfold
        (\(r, b) (tm :: Team, k :: Kind) -> if k == Soldier then (if tm == Red then (r + 1, b) else (r, b + 1)) else (r, b))
        (0 :: Int, 0 :: Int)

-- | Run @reps@ battles of one matchup and print a single structured RESULT line.
runMatchups :: [String] -> IO ()
runMatchups args = do
  let muster = "--muster" `elem` args
      positional = filter (not . isPrefixOf "--") args
  case positional of
    (redS : blueS : rest) -> do
      let reps = case rest of (r : _) -> read r; _ -> 200 :: Int
          cfg = matchupCfg muster
          redC = parseComp redS
          blueC = parseComp blueS
      w <- initWorld
      (rw, bw, dr) <- runWith w $ do
        set global (Camera 0 1)
        let one (r, b, d) _ = do
              atomically $ cmapM_ (\(_ :: Team, e :: Entity) -> destroy e (Proxy @All))
              spawnArmy Red redC (V2 (-70) 0)
              spawnArmy Blue blueC (V2 70 0)
              o <- runBattle cfg
              pure $ case o of
                Just Red -> (r + 1, b, d)
                Just Blue -> (r, b + 1, d)
                Nothing -> (r, b, d + 1)
        foldM one (0 :: Int, 0 :: Int, 0 :: Int) [1 .. reps]
      putStrLn $
        unwords
          [ "RESULT"
          , "red=" ++ redS
          , "blue=" ++ blueS
          , "reps=" ++ show reps
          , "muster=" ++ show muster
          , "red_wins=" ++ show rw
          , "blue_wins=" ++ show bw
          , "draws=" ++ show dr
          ]
    _ -> putStrLn "usage: --matchup <redComp> <blueComp> [reps] [--muster]"

main :: IO ()
main = do
  -- Line-buffer both streams so round reports and heartbeats appear promptly
  -- even when redirected to a file (block buffering would hide them until exit).
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  args <- getArgs
  if "--matchup" `elem` args
    then runMatchups (filter (/= "--matchup") args)
    else runGame args

runGame :: [String] -> IO ()
runGame args = do
  let cfg
        | any (`elem` ["--headless", "headless"]) args = headlessCfg
        | otherwise = displayCfg
  w <- initWorld
  runWith w $ do
    set global (Camera 0 1)
    set global (ShowVision False)
    set global (ShowRange False)
    set global (ShowAttacks True)
    void $ forkSys (coordinator cfg)
    if cHeadless cfg
      then do
        threadDelay (cMaxSeconds cfg * 1000000)
        dl <- atomically (get global)
        liftIO $ do
          putStrLn "=== headless time limit reached ==="
          putStrLn ("  RED  " ++ matrixDump (teamMatrix Red dl))
          putStrLn ("  BLUE " ++ matrixDump (teamMatrix Blue dl))
      else play (InWindow "STM Colony War" (660, 500) (10, 10)) black 60 draw handleEvent step
  where
    matrixDump rows =
      "damage " ++ intercalate "  " [show atk ++ " " ++ show wsc | (atk, wsc) <- rows]
