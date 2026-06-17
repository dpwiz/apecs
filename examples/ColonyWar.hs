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

  * Three unit types form an /emergent/ rock-paper-scissors -- Hunter > Lance,
    Lance > Guard, Guard > Hunter -- with no damage lookup table. The cycle
    falls out of range, speed, and a single kiting behaviour: the fast long-
    ranged Hunter kites the slow short-ranged Lance; the Guard range-matches the
    Hunter (so kiting buys nothing) and out-brawls it; the tanky Lance out-brawls
    the Guard (which is too slow to kite it) and wrecks bases.

  * Fog of war: a base sees ~a quarter of the field around itself; units (the
    far-sighted Hunters especially) extend that with recon. Each colony's
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

import Control.Monad (foldM, forM_, void, when)
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

-- | The three archetypes. Their rock-paper-scissors is /emergent/: it falls out
-- of range, speed, and the one kiting behaviour, not a damage lookup table.
--
--   * 'Hunter' — fast, long-ranged, fragile recon. Kites the slow short-ranged
--     Lance to death (and can't be caught), but loses a stand-up brawl.
--   * 'Guard'  — long-ranged (matches the Hunter, so kiting buys nothing) but
--     slow; out-brawls the Hunter, yet too slow to kite the Lance.
--   * 'Lance'  — short-ranged, tanky, hard-hitting base-breaker. Out-brawls the
--     Guard; helpless against a Hunter that simply backs away and fires.
--
-- So Hunter > Lance > Guard > Hunter, with no dominant type (proven in the lab,
-- exp 004). Only soldiers carry one.
data UnitType = Hunter | Guard | Lance deriving (Eq, Ord, Show, Enum, Bounded)
instance Component UnitType where type Storage UnitType = Map UnitType

-- | A soldier's task-force role (maneuver warfare, exp 007), assigned at spawn.
--
--   * 'MainBody' fixes the enemy front -- engages and holds, the base case.
--   * 'Flank' is the main effort: it avoids the enemy line and sweeps wide
--     through the open flank to strike the lightly-held enemy base.
--   * 'Recon' screens and scouts -- spreads out to light up the map and watch
--     the team's own flanks against a backstab.
data Role = MainBody | Flank | Recon deriving (Eq, Show)
instance Component Role where type Storage Role = Map Role

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

-- | One colony's standing orders: which type to enlist next, where the main
-- body musters, and which vertical flank (+1 top / -1 bottom) the strategist has
-- judged the open gap for the flanking force. Set by the strategist, read by the
-- spawner and every unit.
data TeamPlan = TeamPlan
  { planNext :: !UnitType
  , planWaypoint :: !(V2 Float)
  , planGap :: !Float
  }
  deriving (Show)

-- | Both colonies' plans, by (Red, Blue). A single shared 'Global' that two
-- strategist threads concurrently read-modify-write.
data Plans = Plans TeamPlan TeamPlan deriving (Show)
instance Semigroup Plans where _ <> b = b
instance Monoid Plans where
  mempty = Plans (TeamPlan Hunter (V2 0 0) 1) (TeamPlan Hunter (V2 0 0) 1)
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

-- | Whose fog to render through. 'ViewAll' draws everything (omniscient, the
-- default); 'ViewSide' t draws only what team @t@ can actually see -- its own
-- units and bases, plus enemies inside its vision -- so you can watch the battle
-- through one colony's partial information. Toggled with the @[@ and @]@ keys.
data Viewpoint = ViewAll | ViewSide Team
instance Semigroup Viewpoint where _ <> b = b
instance Monoid Viewpoint where mempty = ViewAll
instance Component Viewpoint where type Storage Viewpoint = Global Viewpoint

-- | Every component an entity owns, so we can delete it in one go (the extra
-- deletes are harmless no-ops for entities that lack a component).
type All = (Position, Health, Team, Kind, UnitType, Attacking, Role)

makeWorld
  "World"
  [ ''Position
  , ''Health
  , ''Team
  , ''Kind
  , ''UnitType
  , ''Attacking
  , ''Role
  , ''KillScore
  , ''DamageLog
  , ''Wins
  , ''Phase
  , ''Plans
  , ''ShowVision
  , ''ShowRange
  , ''ShowAttacks
  , ''Viewpoint
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
  , cKite :: Bool
  -- ^ Whether a soldier that out-ranges /and/ out-runs its target kites it
  -- (fires while backing off to hold the range gap). Ablation flag for exp 003.
  , cTriad :: Bool
  -- ^ Legacy: whether the old rock-paper-scissors /damage multiplier/ applies.
  -- Off in the live game (the cycle is emergent now); kept so the harness can
  -- still reproduce the pre-redesign stat-triad experiments with @--triad@.
  , cArena :: Float
  -- ^ Matchup harness only: if > 0, clamp positions to a +/- this square so
  -- kiters can be cornered (0 = unbounded). The full game uses its own field.
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
    , cKite = True
    , cTriad = False
    , cArena = 0
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
    , cKite = True
    , cTriad = False
    , cArena = 0
    }

dbg :: Config -> String -> SystemIO ()
dbg cfg msg = when (cDebug cfg) (traceM msg)

-- Tunables ------------------------------------------------------------------

capPerTeam :: Int
capPerTeam = 34

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

-- | Flanking maneuver (exp 007): the flanking force sweeps to this vertical
-- offset (well clear of the y≈0 frontal grind) along the open flank, then turns
-- in to the enemy base once within 'flankTurnIn' of the base's x.
flankY, flankTurnIn :: Float
flankY = 250
flankTurnIn = 150

-- | A team's staging point: in front of its base, on the line to the enemy.
rallyPoint :: Team -> V2 Float
rallyPoint team = b + normalize (basePos (enemyOf team) - b) ^* rallyDist
  where
    b = basePos team

-- | Soldiers shove each other apart so they don't stack into one pixel. This
-- packing is deliberately tight: concentration is what lets a winning army
-- overwhelm a thinner one locally and break through, which is how a round
-- actually ends. (Roomy spacing was tried and turned every round into an
-- endless even-trade grind.)
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

-- Per-type stats. The cycle is emergent (exp 004): a Hunter out-ranges AND
-- out-runs a Lance, so it kites; a Guard range-matches the Hunter (kiting buys
-- nothing) but is too slow to kite a Lance; a Lance out-brawls the Guard.
--   Hunter: long range, fast, fragile, far-seeing recon.
--   Guard:  long range (= Hunter), slow, mid HP/dmg, the anchor.
--   Lance:  short range, tanky, hard-hitting; razes bases (double vs a base).
typeHp, typeSpeed, typeDamage, typeVision, typeRange :: UnitType -> Float
typeHp Hunter = 55
typeHp Guard = 110
typeHp Lance = 170
typeSpeed Hunter = 130
typeSpeed Guard = 50
typeSpeed Lance = 70
typeDamage Hunter = 8
typeDamage Guard = 9
typeDamage Lance = 22
typeVision Hunter = 170
typeVision Guard = 95
typeVision Lance = 75
typeRange Hunter = 18
typeRange Guard = 18
typeRange Lance = 12

-- | Does the first type beat the second in the emergent cycle? This is /not/ a
-- damage multiplier (combat applies none) -- it is the empirical outcome of the
-- range/speed/kiting interplay (exp 004), used by each colony's strategist to
-- pick a counter. Hunter > Lance > Guard > Hunter.
beats :: UnitType -> UnitType -> Bool
beats Guard Hunter = True
beats Hunter Lance = True
beats Lance Guard = True
beats _ _ = False

-- | A strategy-only score for fielding @atk@ against a @def@ the colony can see:
-- favourable if @atk@ beats it, unfavourable if it is beaten. Drives the
-- counter-picker; it no longer scales any damage.
advantage :: UnitType -> UnitType -> Float
advantage atk def
  | beats atk def = 2.0
  | beats def atk = 0.5
  | otherwise = 1.0

-- | Spawn anchor for a team, at the left/right edge of the battlefield. Set wide
-- (the field is ~2x the old one) so there is room to manoeuvre and so a base's
-- own sight no longer spans midfield -- recon has to earn the picture.
basePos :: Team -> V2 Float
basePos Red = V2 (-480) 0
basePos Blue = V2 480 0

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
-- dealt to (Hunter, Guard, Lance) defenders.
teamMatrix :: Team -> DamageLog -> [(UnitType, (Int, Int, Int))]
teamMatrix team (DamageLog m) =
  [ (atk, (val atk Hunter, val atk Guard, val atk Lance)) | atk <- [Hunter, Guard, Lance]
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
initialPlan team = TeamPlan Hunter (b + (e - b) ^* 0.35) 1
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
      -- The nearest enemy /soldier/'s type, for the kiting decision (one read,
      -- same transaction/snapshot as the gather, so it is consistent). Only
      -- needed when kiting is enabled, so the normal game pays nothing.
      mUnitType <-
        if cKite cfg
          then case mUnit of
            Just (e, _, _) -> Just <$> get e
            Nothing -> pure Nothing
          else pure Nothing
      myRole <- get ety
      -- The movement target: close on the nearest enemy soldier, or the base
      -- once their soldiers clear.
      let target = case mUnit of
            Just u -> Just u
            Nothing -> mBase
          -- What to actually hit: whatever is in reach. A Flank unit is the
          -- base-breaker -- it razes the base ahead of chasing soldiers; everyone
          -- else hits the nearest soldier but falls back to a base in reach (so a
          -- unit standing on the enemy base never sits there doing nothing).
          attackables = case myRole of
            Flank -> [mBase, mUnit]
            _ -> [mUnit, mBase]
          struck = case [(tEnt, tPos) | Just (tEnt, tPos, d2) <- attackables, d2 <= range2] of
            (s : _) -> Just s
            [] -> Nothing
      case struck of
        Just (tEnt, _) -> attack cfg myTeam myType tEnt
        Nothing -> pure ()
      -- Record (display only) a tracer to whatever we hit, or clear last tick's.
      when (not (cHeadless cfg)) $ case struck of
        Just (_, tPos) -> set ety (Attacking tPos)
        Nothing -> destroy ety (Proxy @Attacking)
      -- Move. A soldier with enough friends nearby presses its target into
      -- melee -- it does not hold at max range, or two long-ranged lines just sit
      -- and trade forever without either breaking. Closing packs the fight tight
      -- so a local edge snowballs into a breakthrough (which is how a round ends).
      -- A kiter is the exception: it holds the range gap. An under-supported
      -- soldier falls back to the rally point to mass up first, rather than feed
      -- itself piecemeal into the enemy blob. With no enemy in sight, it marches
      -- to the muster waypoint.
      plan <- teamPlan myTeam <$> get global
      let wp = planWaypoint plan
          gap = planGap plan
          supported = not (cMuster cfg) || allyNear + 1 >= musterMin
          rally = rallyPoint myTeam
          -- Kiting: if I out-range and out-run my target, hold it at the edge of
          -- my reach -- close in if it slips out, back straight off if it gets
          -- too near -- so I keep firing while it never lands a blow.
          kiteOK = cKite cfg && case mUnitType of
            Just tt -> typeRange myType > typeRange tt && typeSpeed myType > typeSpeed tt
            Nothing -> False
          kiteDest = case (kiteOK, mUnit) of
            (True, Just (_, tPos, d2))
              | d2 > range2 -> Just tPos -- out of reach: close the distance
              | sqrt d2 < typeRange myType * 0.92 ->
                  let away = let v = p - tPos in if quadrance v > 1e-6 then normalize v else V2 1 0
                   in Just (p + away ^* 60) -- too close: back off, still firing
              | otherwise -> Nothing -- in the sweet spot: hold and fire
            _ -> Nothing
          -- The maneuver path: sweep wide to (enemyBaseX, gap*flankY), clear of
          -- the frontal grind, then turn in and crash the lightly-held base.
          enemyBase = basePos (enemyOf myTeam)
          flankDest =
            let V2 ebx _ = enemyBase
                V2 px _ = p
             in if abs (px - ebx) > flankTurnIn then V2 ebx (gap * flankY) else enemyBase
          -- Recon saturates the enemy half: each scout takes a low-discrepancy
          -- (golden-ratio) point spread over both depth and height, so they fan
          -- out evenly instead of forming a rigid line, and never close the base.
          reconScout =
            let V2 ebx _ = enemyBase
                Entity eid = ety
                frac z = z - fromIntegral (floor z :: Int)
                sx = 0.15 + 0.55 * frac (fromIntegral eid * 0.6180339887)
                sy = frac (fromIntegral eid * 0.3819660113) * 2 - 1
             in V2 (ebx * sx) (sy * 250)
          -- Self-preservation: a scout backs straight off a nearby threat unless
          -- it can simply kite it. Information rarely outvalues the scout itself.
          dangerR2 = sq (typeVision myType * 0.45)
          fleePoint = case mUnit of
            Just (_, tPos, _) -> p + (let v = p - tPos in if quadrance v > 1e-6 then normalize v else V2 0 1) ^* 90
            Nothing -> p
          reconDanger = case mUnit of Just (_, _, d2) -> d2 < dangerR2; Nothing -> False
          dest = case myRole of
            -- Maneuver force: ignore the frontal fight, head for the base.
            Flank -> Just flankDest
            -- Recon: kite what it can, flee what it can't, otherwise spread and scout.
            Recon
              | Just kd <- kiteDest -> Just kd
              | reconDanger -> Just fleePoint
              | otherwise -> Just reconScout
            -- Main body: fix the enemy front (press into melee when supported,
            -- else fall back and mass at the rally point).
            MainBody
              | Just kd <- kiteDest -> Just kd
              | supported -> case target of
                  Just (_, tPos, _) -> Just tPos
                  Nothing
                    | quadrance (wp - p) > waypointReach2 -> Just wp
                    | otherwise -> Nothing
              | quadrance (rally - p) > waypointReach2 -> Just rally
              | otherwise -> Nothing
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

{- | Deal damage to a victim within the caller's transaction. Soldier-vs-soldier
damage is flat (the counter cycle is emergent, not a multiplier) unless 'cTriad'
is on for a legacy experiment; a Lance does double against a base. Soldier kills
score; razing a base does not (the coordinator notices that separately).
-}
attack :: Config -> Team -> UnitType -> Entity -> SystemSTM ()
attack cfg killer atkType victim = do
  stillThere <- exists victim (Proxy @Health)
  when stillThere $ do
    Health h <- get victim
    k <- get victim
    (dmg, mDef) <- case k of
      Base -> pure (typeDamage atkType * (if atkType == Lance then 2 else 1), Nothing)
      Soldier -> do
        defType <- get victim
        let mult = if cTriad cfg then advantage atkType defType else 1.0
        pure (typeDamage atkType * mult, Just defType)
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

-- | Live soldiers of a team, tallied by type as (Hunter, Guard, Lance).
type Census = (Int, Int, Int)

bumpType :: UnitType -> Census -> Census
bumpType Hunter (h, g, l) = (h + 1, g, l)
bumpType Guard (h, g, l) = (h, g + 1, l)
bumpType Lance (h, g, l) = (h, g, l + 1)

-- | What to enlist next. The enemy here is often the /fog/: a colony that has
-- not actually seen the enemy composition must not gamble its whole build on a
-- hard counter to a read it doesn't have. The known answer to uncertainty is a
-- balanced, combined-arms force -- robust to anything, hard-countered by nothing
-- -- so:
--
--   * a __diversity floor__ keeps a minimum of every type in the field (never a
--     brittle mono build, and it guarantees the Hunters that do the scouting);
--   * only a __confident__ sighting (enough enemies actually seen) sharpens the
--     surplus toward a hard counter to their /whole/ composition;
--   * blind, the surplus just balances the mix and sends Hunters to look.
planNextFor :: Census -> Int -> Census -> UnitType
planNextFor own@(h, g, l) seenN enemyCensus
  | Just t <- belowFloor = t
  | seenN >= confidentSightings = bestResponse enemyCensus
  | otherwise = leastOf own
  where
    total = h + g + l
    floorEach = fromIntegral total * minDiversity :: Double
    belowFloor = case [t | (t, c) <- [(Hunter, h), (Guard, g), (Lance, l)], fromIntegral c < floorEach] of
      (t : _) -> Just t
      [] -> Nothing

-- | Fraction of every type a colony keeps in the field no matter what, so it is
-- never a brittle mono build; and how many enemies it must actually see before
-- it trusts a hard counter over a balanced hedge.
minDiversity :: Double
minDiversity = 0.2

confidentSightings :: Int
confidentSightings = 10

-- | The type a team has fewest of (ties to the lighter type by 'Ord') -- used to
-- even out a force when there is nothing reliable to counter.
leastOf :: Census -> UnitType
leastOf (h, g, l) = snd (minimum [(h, Hunter), (g, Guard), (l, Lance)])

-- | The single type with the greatest summed cycle advantage against a census.
-- Ties fall to the sturdier type (Lance > Guard > Hunter by 'Ord').
bestResponse :: Census -> UnitType
bestResponse (h, g, l) =
  snd (maximum [(score t, t) | t <- [Hunter, Guard, Lance]])
  where
    fh = fromIntegral h
    fg = fromIntegral g
    fl = fromIntegral l
    score t = fh * advantage t Hunter + fg * advantage t Guard + fl * advantage t Lance

-- | Living soldiers a team currently fields.
teamUnitCount :: Team -> SystemSTM Int
teamUnitCount team =
  cfold
    (\n (t :: Team, k :: Kind) -> if t == team && k == Soldier then n + 1 else n)
    (0 :: Int)

-- | Telemetry (exp 006): how much of the enemy a team actually sees through its
-- fog -- its base sight plus every friendly soldier's recon -- as (seen, total)
-- enemy soldiers. Coverage seen/total ~ 1 means the fog is collapsed (vision
-- spans the whole field, recon is worthless, the counter-game goes deterministic).
enemyCoverage :: Team -> SystemSTM (Int, Int)
enemyCoverage team = do
  unitSrcs <-
    cfold
      (\acc (t :: Team, ut :: UnitType, Position q) -> if t == team then (q, typeVision ut) : acc else acc)
      []
  let sources = (basePos team, baseVision) : unitSrcs
  enemies <-
    cfold
      (\acc (t :: Team, k :: Kind, Position q) -> if t /= team && k == Soldier then q : acc else acc)
      []
  let seen = length [() | q <- enemies, any (\(s, r) -> quadrance (q - s) <= r * r) sources]
  pure (seen, length enemies)

-- | Telemetry: a team's soldier front as (mean x, max |y|), so a pinned-at-centre
-- front (no travel) and the y-spread vs vision radius (how 2D the fight really
-- is) are both legible.
teamFront :: Team -> SystemSTM (Float, Float)
teamFront team = do
  (sx, n, maxY) <-
    cfold
      ( \(sx, n, my) (t :: Team, k :: Kind, Position (V2 x y)) ->
          if t == team && k == Soldier then (sx + x, n + 1 :: Int, max my (abs y)) else (sx, n, my)
      )
      (0, 0, 0)
  pure (if n == 0 then 0 else sx / fromIntegral n, maxY)

-- | Telemetry: how many of a team's soldiers are sitting on the /enemy/ base
-- (within a few base-radii). Cross-read with the enemy base HP: bodies on the
-- base while its HP holds flat is the signature of units squatting an objective
-- instead of reducing it -- invisible to any aggregate count.
siegeCount :: Team -> SystemSTM Int
siegeCount team =
  cfold
    (\n (t :: Team, k :: Kind, Position q) -> if t == team && k == Soldier && quadrance (q - eb) <= r2 then n + 1 else n)
    (0 :: Int)
  where
    eb = basePos (enemyOf team)
    r2 = sq (baseRadius * 4)

-- | Telemetry (over-eager assaults). Two measurable faces of "too eager to
-- advance into certain death", per team:
--   * bad  = fraction of soldiers whose /nearest/ enemy hard-counters them (a
--     fight they should be kiting away from, not standing in);
--   * out  = fraction locally outnumbered -- more enemies than friends within a
--     skirmish radius -- i.e. charging into superior force instead of massing.
engageRates :: SystemSTM (Float, Float, Float, Float)
engageRates = do
  sol <- cfold (\acc (t :: Team, ut :: UnitType, Position q) -> (t, ut, q) : acc) []
  let (rb, ro) = ratesFor Red sol
      (bb, bo) = ratesFor Blue sol
  pure (rb, bb, ro, bo)
  where
    skirmish2 = 70 * 70
    ratesFor team sol =
      let mine = [(ut, q) | (t, ut, q) <- sol, t == team]
          foes = [(ut, q) | (t, ut, q) <- sol, t /= team]
          near c pts = length [() | (_, q) <- pts, quadrance (q - c) <= skirmish2]
          inBad (ut, q) = case nearest q foes of Just fut -> beats fut ut; Nothing -> False
          outnum (_, q) = near q foes > near q mine -- mine includes self, so strict >
          n = length mine
          frac p = if n == 0 then 0 else fromIntegral (length (filter p mine)) / fromIntegral n
       in (frac inBad, frac outnum)
    nearest q foes = case foes of
      [] -> Nothing
      _ -> Just (snd (minimum [(quadrance (q - fq), fut) | (fut, fq) <- foes]))

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
  own <-
    cfold
      (\acc (t :: Team, ut :: UnitType) -> if t == team then bumpType ut acc else acc)
      (0, 0, 0)
  let visible = [(q, ut) | (q, ut) <- enemies, seenBy sources q]
      census = foldr (bumpType . snd) (0, 0, 0) visible
      waypoint = chooseWaypoint team (map fst visible)
      -- The open flank for the maneuver force: the vertical half (top +y /
      -- bottom -y) holding fewer of the enemies we can see. Recon pull -- the
      -- gap is chosen from what the fog actually reveals. Ties / a dark field
      -- default to the top.
      up = length [() | (V2 _ y, _) <- visible, y > 0]
      down = length [() | (V2 _ y, _) <- visible, y <= 0]
      gap = if down < up then -1 else 1
  modify global (setTeamPlan team (TeamPlan (planNextFor own (length visible) census) waypoint gap))
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

-- | The share of (non-Hunter) reinforcements peeled off as the flanking force.
flankShare :: Double
flankShare = 0.35

-- | Task-force role for a fresh soldier (exp 007): Hunters scout (Recon); the
-- rest mostly hold the line, with 'flankShare' peeling off to strike the base.
rollRole :: UnitType -> Double -> Role
rollRole Hunter _ = Recon
rollRole _ r = if r < flankShare then Flank else MainBody

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
      roleRoll <- liftIO (randomRIO (0, 1) :: IO Double)
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
                role = rollRole ut roleRoll
            Just <$> newEntity (team, Soldier, ut, Position pos, Health (typeHp ut), role)
      case mE of
        Nothing -> pure () -- base dead or round over: retire
        Just e -> do
          void $ forkSys (unitAI cfg e)
          threadDelay (cSpawn cfg)
          loop

-- Round lifecycle -----------------------------------------------------------

-- | Wipe the battlefield and stand up a fresh round: just two bases, each with a
-- spawner + strategist thread. Both colonies grow their armies from scratch out
-- of an empty field -- no starting platoon -- so the opening is itself a
-- recon-and-build contest. Returns the base ids so the coordinator can watch them.
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
  void $ forkSys (spawnerThread cfg redBase Red)
  void $ forkSys (spawnerThread cfg blueBase Blue)
  void $ forkSys (strategist cfg Red redBase)
  void $ forkSys (strategist cfg Blue blueBase)
  dbg cfg "[round] started: 2 bases, 2 spawners, 2 strategists (no starting platoon)"
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
      (ph, rc, bc, Plans rp bp, DamageLog dm, rHp, bHp, rCov, bCov, rFront, bFront, rBad, bBad) <- atomically $ do
        ph <- get global
        rc <- teamCensus Red
        bc <- teamCensus Blue
        pl <- get global
        dl <- get global
        rh <- baseHpOf Red
        bh <- baseHpOf Blue
        rcov <- enemyCoverage Red
        bcov <- enemyCoverage Blue
        rf <- teamFront Red
        bf <- teamFront Blue
        (rbad, bbad, rout, bout) <- engageRates
        rsg <- siegeCount Red
        bsg <- siegeCount Blue
        pure (ph, rc, bc, pl, dl, rh, bh, rcov, bcov, rf, bf, (rbad, rout, rsg), (bbad, bout, bsg))
      let teamDmg t = round (sum [v | ((t', _, _), v) <- DM.toList dm, t' == t]) :: Int
      traceM $
        "[hb] R " ++ showCensus rc ++ " base=" ++ show (round rHp :: Int) ++ " next=" ++ show (planNext rp) ++ " dmg=" ++ show (teamDmg Red) ++ " " ++ showTel rCov rFront rBad
          ++ " | B " ++ showCensus bc ++ " base=" ++ show (round bHp :: Int) ++ " next=" ++ show (planNext bp) ++ " dmg=" ++ show (teamDmg Blue) ++ " " ++ showTel bCov bFront bBad
      when (isPlaying ph) (threadDelay 500000 >> heartbeat)

    -- Coverage seen/total, front mean-x, combat y-spread, the over-eager pair
    -- (bad = nearest foe counters me, out = locally outnumbered), and siege =
    -- own bodies on the enemy base (read against that base's HP above).
    showTel (seen, tot) (cx, maxY) (bad, out, sg) =
      "cov=" ++ show seen ++ "/" ++ show tot ++ " frontx=" ++ show (round cx :: Int) ++ " ymax=" ++ show (round maxY :: Int)
        ++ " bad=" ++ show (round (bad * 100) :: Int) ++ "% out=" ++ show (round (out * 100) :: Int) ++ "% siege=" ++ show sg

    baseHpOf team =
      cfold (\acc (t :: Team, k :: Kind, Health h) -> if t == team && k == Base then h else acc) baseHp

    teamCensus team =
      cfold (\acc (t :: Team, ut :: UnitType) -> if t == team then bumpType ut acc else acc) (0, 0, 0)

    showCensus (h, g, l) = "H" ++ show h ++ "/G" ++ show g ++ "/L" ++ show l

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
    -- (vs Hunter / vs Guard / vs Lance).
    matrixLine rows =
      "damage " ++ intercalate "  " [show atk ++ " " ++ show (h, g, l) | (atk, (h, g, l)) <- rows]

-- Rendering & input (main thread) -------------------------------------------

label :: Color -> Float -> Float -> String -> Picture
label col x y = color col . translate x y . scale 0.12 0.12 . Text

-- | A soldier glyph, distinct per type: small Hunter dot, Guard disc, and a
-- bigger ringed disc for the heavy Lance (a filled circle with a darker rim).
unitGlyph :: Team -> UnitType -> Picture
unitGlyph t Hunter = color (teamColor t) (circleSolid 3)
unitGlyph t Guard = color (teamColor t) (circleSolid 5)
unitGlyph t Lance =
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
  (visionPic, attackPic, basePic, soldierPic, fogPic, vp, redPop, bluePop, KillScore kr kb, Wins wr wb, phase, Plans rPlan bPlan, dl) <-
    atomically $ do
      vp <- get global :: SystemSTM Viewpoint
      -- When viewing through one side's eyes, gather that side's sight sources
      -- (its base plus every friendly soldier's recon); enemies outside them are
      -- hidden. 'vis t q' = is a thing of team t at q visible to the viewer?
      let viewer = case vp of ViewAll -> Nothing; ViewSide s -> Just s
      sources <- case viewer of
        Nothing -> pure []
        Just s -> do
          us <-
            cfold
              (\acc (t :: Team, ut :: UnitType, Position q) -> if t == s then (q, typeVision ut) : acc else acc)
              []
          pure ((basePos s, baseVision) : us)
      let vis t q = case viewer of
            Nothing -> True
            Just s -> t == s || any (\(c, r) -> quadrance (q - c) <= r * r) sources
      basePic <-
        cfoldM
          ( \acc (t :: Team, k :: Kind, Position (V2 x y), Health hp) ->
              pure $ case k of
                Base | vis t (V2 x y) -> acc <> translate x y (baseGlyph t hp)
                _ -> acc
          )
          mempty
      -- Pop counts are the true totals (HUD meta), but only visible glyphs draw.
      (soldierPic, rp, bp) <-
        cfoldM
          ( \(!acc, !rp, !bp) (t :: Team, ut :: UnitType, Position (V2 x y)) ->
              let acc' = if vis t (V2 x y) then acc <> translate x y (unitGlyph t ut) else acc
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
      -- A faint wash over the viewer's own sight discs, marking the lit area so
      -- the dark unseen field is obvious.
      let fogPic = case viewer of
            Nothing -> mempty
            Just s -> mconcat [translate cx cy (color (withAlpha 0.05 (teamColor s)) (circleSolid r)) | (V2 cx cy, r) <- sources]
      ks <- get global :: SystemSTM KillScore
      wns <- get global :: SystemSTM Wins
      ph <- get global :: SystemSTM Phase
      pl <- get global :: SystemSTM Plans
      dmg <- get global :: SystemSTM DamageLog
      pure (visionPic, attackPic, basePic, soldierPic, fogPic, vp, rp, bp, ks, wns, ph, pl, dmg)
  -- Two left-aligned rows in the top-left of the (1600x900) window.
  let viewLabel = case vp of
        ViewAll -> "viewpoint: ALL (omniscient)"
        ViewSide Red -> "viewpoint: RED  (fog -- only what Red sees)"
        ViewSide Blue -> "viewpoint: BLUE (fog -- only what Blue sees)"
      viewCol = case vp of ViewAll -> greyN 0.5; ViewSide t -> teamColor t
      hud =
        label (teamColor Red) (-780) 420 ("RED   pop " ++ show redPop ++ "   next " ++ show (planNext rPlan) ++ "   kills " ++ show kr ++ "   wins " ++ show wr)
          <> label (teamColor Blue) (-780) 396 ("BLUE  pop " ++ show bluePop ++ "   next " ++ show (planNext bPlan) ++ "   kills " ++ show kb ++ "   wins " ++ show wb)
          <> label viewCol (-780) 372 viewLabel
          <> label (greyN 0.5) (-780) (-430) "v: vision   r: range   a: attacks   [ ]: viewpoint   esc: quit"
      overlay = case phase of
        Playing -> mempty
        RoundOver w ->
          color (withAlpha 0.74 black) (rectangleSolid 1600 900)
            <> label white (-58) 215 "ROUND OVER"
            <> label (teamColor w) (-85) 185 (show w ++ " TEAM WINS")
            <> label white (-150) 158 ("kills R " ++ show kr ++ " / B " ++ show kb ++ "     match R " ++ show wr ++ " / B " ++ show wb)
            <> damageBlock (teamColor Red) (-312) 116 "RED damage  (attacker vs defender)" (teamMatrix Red dl)
            <> damageBlock (teamColor Blue) (-312) (-24) "BLUE damage  (attacker vs defender)" (teamMatrix Blue dl)
  -- Fog wash sits under the glyphs; tracers go on /top/ (a hit fires within attack
  -- range, so the line is short and would otherwise hide under the units).
  pure (fogPic <> visionPic <> basePic <> soldierPic <> attackPic <> hud <> overlay)

-- | A round-end ledger block: a title and one line per attacker type listing
-- the damage it dealt to (vs Hunter / vs Guard / vs Lance).
damageBlock :: Color -> Float -> Float -> String -> [(UnitType, (Int, Int, Int))] -> Picture
damageBlock col x y title rows =
  smallLabel col x y title
    <> mconcat
      [ smallLabel col x (y - 18 * fromIntegral (i + 1)) (rowText atk h g l)
      | (i, (atk, (h, g, l))) <- zip [0 :: Int ..] rows
      ]
  where
    rowText atk h g l =
      pad 9 (show atk) ++ "vsH " ++ pad 6 (show h) ++ "vsG " ++ pad 6 (show g) ++ "vsL " ++ show l
    pad n s = take n (s ++ repeat ' ')

smallLabel :: Color -> Float -> Float -> String -> Picture
smallLabel col x y = color col . translate x y . scale 0.1 0.1 . Text

-- | Faint discs for every sight source: a big one per base, a small one per
-- soldier (Hunters reach furthest). Overlapping discs build up where a colony's
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

-- | An unfilled ring at each soldier's attack reach, so the Hunter and Guard's
-- longer reach (and the Lance's short bite) is visible at a glance.
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
-- '[' views through Red (left), ']' through Blue (right); pressing the active
-- side's key again clears back to the omniscient view.
handleEvent (EventKey (Char '[') Down _ _) = modify global (toggleView Red)
handleEvent (EventKey (Char ']') Down _ _) = modify global (toggleView Blue)
handleEvent _ = pure ()

toggleView :: Team -> Viewpoint -> Viewpoint
toggleView t (ViewSide s) | s == t = ViewAll
toggleView t _ = ViewSide t

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

  > stm-colony-war --matchup hunter:10 guard:10 [reps] [--muster]
-}
matchupCfg :: Bool -> Bool -> Bool -> Float -> Config
matchupCfg muster kite triad arena =
  headlessCfg {cTick = 16000, cDebug = False, cMuster = muster, cKite = kite, cTriad = triad, cArena = arena}

-- | Parse @"hunter:10,lance:5"@ into a unit roster.
parseComp :: String -> [(UnitType, Int)]
parseComp s = [parse1 part | part <- splitOn ',' s]
  where
    parse1 p = case splitOn ':' p of
      [t, n] -> (parseType t, read n)
      _ -> error ("matchup: bad component " ++ show p)
    parseType t = case map toLower t of
      "hunter" -> Hunter
      "h" -> Hunter
      "guard" -> Guard
      "g" -> Guard
      "lance" -> Lance
      "l" -> Lance
      other -> error ("matchup: bad unit type " ++ show other)

splitOn :: Char -> String -> [String]
splitOn c s = case break (== c) s of
  (a, []) -> [a]
  (a, _ : rest) -> a : splitOn c rest

-- | Spawn a roster on a small grid whose columns recede /away/ from the centre,
-- so the two armies are mirror-symmetric about x=0 (separation then spreads
-- them); no base, no spawner, no strategist.
spawnArmy :: Team -> [(UnitType, Int)] -> V2 Float -> SystemIO ()
spawnArmy team comp (V2 ax ay) =
  forM_ (zip [0 :: Int ..] roster) $ \(i, ut) -> do
    let face = signum ax -- columns recede outward, front rank faces the centre
        gx = face * fromIntegral (i `mod` 5) * 9
        gy = fromIntegral (i `div` 5) * 9 - 16
    void . atomically $
      newEntity (team, Soldier, ut, Position (V2 (ax + gx) (ay + gy)), Health (typeHp ut), MainBody)
  where
    roster = concat [replicate n ut | (ut, n) <- comp]

-- | Fisher-Yates shuffle (list-based; the rosters are tiny).
shuffleIO :: [a] -> IO [a]
shuffleIO [] = pure []
shuffleIO xs = do
  i <- randomRIO (0, length xs - 1)
  case splitAt i xs of
    (a, x : b) -> (x :) <$> shuffleIO (a ++ b)
    (a, []) -> shuffleIO a -- unreachable: i < length xs


-- | Step every soldier once per tick until one side is gone or the clock caps.
-- The step order is reshuffled each tick so neither side gets a systematic
-- first-strike advantage (a fixed order makes mirror matchups 100/0).
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
          order <- liftIO (shuffleIO ents)
          mapM_ (\e -> atomically (void (stepUnit cfg e))) order
          when (cArena cfg > 0) $
            atomically $
              cmap $ \(Position (V2 x y)) ->
                let a = cArena cfg
                    cl v = max (-a) (min a v)
                 in Position (V2 (cl x) (cl y))
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
  -- Defaults mirror the live combat model (kiting on, no triad multiplier);
  -- ablate with --nokite / --triad. Muster stays off to isolate raw combat.
  let muster = "--muster" `elem` args
      kite = not ("--nokite" `elem` args)
      triad = "--triad" `elem` args
      arena = if "--arena" `elem` args then 160 else 0
      positional = filter (not . isPrefixOf "--") args
  case positional of
    (redS : blueS : rest) -> do
      let reps = case rest of (r : _) -> read r; _ -> 200 :: Int
          cfg = matchupCfg muster kite triad arena
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
          , "kite=" ++ show kite
          , "triad=" ++ show triad
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
      -- 16:9 window (fits a 1080p screen); --fullscreen for a true full-screen.
      display
        | "--fullscreen" `elem` args = FullScreen
        | otherwise = InWindow "STM Colony War" (1600, 900) (10, 10)
  w <- initWorld
  runWith w $ do
    set global (Camera 0 1)
    set global (ShowVision False)
    set global (ShowRange False)
    set global (ShowAttacks True)
    set global (ViewAll :: Viewpoint)
    void $ forkSys (coordinator cfg)
    if cHeadless cfg
      then do
        threadDelay (cMaxSeconds cfg * 1000000)
        dl <- atomically (get global)
        liftIO $ do
          putStrLn "=== headless time limit reached ==="
          putStrLn ("  RED  " ++ matrixDump (teamMatrix Red dl))
          putStrLn ("  BLUE " ++ matrixDump (teamMatrix Blue dl))
      else play display black 60 draw handleEvent step
  where
    matrixDump rows =
      "damage " ++ intercalate "  " [show atk ++ " " ++ show wsc | (atk, wsc) <- rows]
