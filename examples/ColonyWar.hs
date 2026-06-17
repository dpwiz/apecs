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
import Data.Bits (shiftR, xor)
import Data.Char (toLower)
import Data.List (foldl', intercalate, isPrefixOf, partition)
import Data.Maybe (isJust)
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
--   * 'Defend' garrisons home -- rings the base and intercepts intruders so a
--     flank can't waltz onto an undefended spawner.
--   * 'Recon' screens and scouts -- spreads out to light up the whole map, deny
--     the enemy's scouts, and watch the team's own flanks against a backstab.
data Role = MainBody | Flank | Defend | Recon deriving (Eq, Show)
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

-- | Per-round damage each team has put on the /enemy base/, by (Red, Blue) --
-- the center-of-gravity ledger for the Warfighting scorecard (exp 008): how much
-- of a side's effort lands on the heart vs on bodies.
data BaseDamage = BaseDamage !Float !Float deriving (Show)
instance Semigroup BaseDamage where BaseDamage a b <> BaseDamage c d = BaseDamage (a + c) (b + d)
instance Monoid BaseDamage where mempty = BaseDamage 0 0
instance Component BaseDamage where type Storage BaseDamage = Global BaseDamage

addBaseDmg :: Team -> Float -> BaseDamage -> BaseDamage
addBaseDmg Red d (BaseDamage r b) = BaseDamage (r + d) b
addBaseDmg Blue d (BaseDamage r b) = BaseDamage r (b + d)

-- | Rounds won across the whole match, by (Red, Blue). A 'Global'.
data Wins = Wins !Int !Int deriving (Show)
instance Semigroup Wins where
  Wins a b <> Wins c d = Wins (a + c) (b + d)
instance Monoid Wins where
  mempty = Wins 0 0
instance Component Wins where type Storage Wins = Global Wins

-- | Why a round drew. 'MutualFall' = both spawners fell within the grace
-- countdown (both committed equally suicidally). 'Stalemate' = the round cap was
-- hit with both spawners still standing (neither could crack the other) -- these
-- read very differently and must never be conflated.
data DrawReason = MutualFall | Stalemate deriving (Eq, Show)

-- | The outcome of a round, as decided by the coordinator.
data Outcome = Win Team | Draw DrawReason deriving (Eq, Show)

-- | The shared round state. A 'Global'; last write wins. 'RoundDraw' carries the
-- reason so the scoreboard tells the truth about which kind of draw it was.
data Phase = Playing | RoundOver Team | RoundDraw DrawReason deriving (Eq, Show)
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
  , -- | The enemy spawner's position, /once a friendly unit has actually seen it/.
    -- 'Nothing' until discovered: the strike force may not be aimed at a base it
    -- has never laid eyes on -- recon must re-acquire it each round. Sticky within
    -- a round (remembered after it slips back into the fog); reset at round start.
    planEnemyBase :: !(Maybe (V2 Float))
  , -- | The earned centroid of where the enemy actually IS, from sightings. The
    -- colony does not know which way the enemy lies -- recon must find it. While
    -- this is 'Nothing' the colony is blind: recon explores outward in every
    -- bearing and the force holds at home (no compass to rush). Once recon makes
    -- contact the force is /pulled/ to it. Sticky within a round.
    planContact :: !(Maybe (V2 Float))
  }
  deriving (Show)

-- | Both colonies' plans, by (Red, Blue). A single shared 'Global' that two
-- strategist threads concurrently read-modify-write.
data Plans = Plans TeamPlan TeamPlan deriving (Show)
instance Semigroup Plans where _ <> b = b
instance Monoid Plans where
  mempty = Plans (TeamPlan Hunter (V2 0 0) 1 Nothing Nothing) (TeamPlan Hunter (V2 0 0) 1 Nothing Nothing)
instance Component Plans where type Storage Plans = Global Plans

-- | A colony's fading recollection of where it has /seen/ the enemy: decaying
-- enemy weight on the top (+y) and bottom (-y) flank, plus the flank it has
-- committed its maneuver force to. Memory, not a live read -- so a colony still
-- defends/avoids an axis after the enemy slips back into the fog, and the two
-- colonies (seeing different things) diverge instead of mirroring.
data Threat = Threat
  { thUp :: !Float
  , thDown :: !Float
  , thGap :: !Float
  , thSurprise :: !Float
  -- ^ EMA of the per-pass /surprisal/ (bits): cross-entropy of the latest
  -- sightings against the prior belief over enemy flank -- the colony's
  -- in-the-moment prediction error / "free-energy" signal.
  , thKL :: !Float
  -- ^ EMA of the per-pass /Bayesian surprise/ (bits): how far the sighting moved
  -- the belief, D_KL(posterior || prior).
  }
  deriving (Show)

-- | Both colonies' threat memory, by (Red, Blue). Seeded /asymmetrically/ (Red
-- favours the top flank, Blue the bottom) so the opening is not a mirror.
data ThreatMem = ThreatMem Threat Threat deriving (Show)
instance Semigroup ThreatMem where _ <> b = b
instance Monoid ThreatMem where
  mempty = ThreatMem (Threat 0 0 1 0 0) (Threat 0 0 (-1) 0 0)
instance Component ThreatMem where type Storage ThreatMem = Global ThreatMem

teamThreat :: Team -> ThreatMem -> Threat
teamThreat Red (ThreatMem r _) = r
teamThreat Blue (ThreatMem _ b) = b

setTeamThreat :: Team -> Threat -> ThreatMem -> ThreatMem
setTeamThreat Red t (ThreatMem _ b) = ThreatMem t b
setTeamThreat Blue t (ThreatMem r _) = ThreatMem r t

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

-- | A snapshot spatial index: occupied cells -> the units/bases in them. Rebuilt
-- once per tick by a single refresher, read by every unit, so a unit's neighbour
-- query (separation, cohesion, target) scans only its 3x3 cell block instead of
-- folding over ALL entities -- turning the per-tick cost from O(n) per unit
-- (O(n^2) overall) into O(local density). The snapshot is at most one tick stale,
-- which is fine for forces that already act on independent clocks. A 'Global'.
type GridEntry = (Team, Kind, Role, Entity, V2 Float)

newtype Grid = Grid (DM.Map (Int, Int) [GridEntry])
instance Semigroup Grid where _ <> b = b
instance Monoid Grid where mempty = Grid DM.empty
instance Component Grid where type Storage Grid = Global Grid

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
  , ''BaseDamage
  , ''Wins
  , ''Phase
  , ''Plans
  , ''ThreatMem
  , ''ShowVision
  , ''ShowRange
  , ''ShowAttacks
  , ''Viewpoint
  , ''Grid
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
  , cGrace :: Int
  -- ^ After the first base falls, how long (microseconds) to wait before scoring.
  -- If the other base also falls within this window the round is a draw -- a
  -- mutual strike that traded both spawners is not a win for whoever landed first.
  , cRoundCap :: Int
  -- ^ A round with no spawner razed by this wall-clock (microseconds; 0 ==
  -- unbounded) is a legitimate draw: two competent defenses neither side could
  -- crack. Not a rig -- the defenders /held/; the attackers simply did not earn
  -- a breach. (The cure for too many of these is a real breakthrough avenue, not
  -- a weaker garrison.)
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
    , cGrace = 2500000
    , cRoundCap = 90000000
    , cMaxSeconds = 0
    , cDebug = False
    , cMuster = True
    , cKite = True
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
    , cGrace = 2500000
    , cRoundCap = 90000000
    , cMaxSeconds = 180
    , cDebug = True
    , cMuster = True
    , cKite = True
    , cArena = 0
    }

dbg :: Config -> String -> SystemIO ()
dbg cfg msg = when (cDebug cfg) (traceM msg)

-- Tunables ------------------------------------------------------------------

-- | Population ceiling per colony. We want this in the thousands (it's a
-- concurrency lab; green threads are cheap). The per-tick neighbour scan is now
-- O(local density) via the spatial grid rather than O(n) per unit, so the cap can
-- climb -- raised toward that as the grid and declumping let it run smoothly.
capPerTeam :: Int
capPerTeam = 256

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
musterMin = 3

-- | Flanking maneuver (exp 007): the flanking force sweeps to this vertical
-- offset (well clear of the y≈0 frontal grind) along the open flank, then turns
-- in to the enemy base once within 'flankTurnIn' of the base's x.
flankY, flankTurnIn :: Float
flankY = 250
flankTurnIn = 150

-- | Soldiers shove each other apart so they don't stack into one pixel. Packing
-- stays fairly tight -- concentration is what lets a winning army overwhelm a
-- thinner one locally and break through, which is how a round actually ends, and
-- roomy spacing once turned every round into an endless even-trade grind -- but
-- 'collideDist' sets the personal space a cohered group settles at, so an
-- over-small value clumps units into an illegible (and, for the spatial grid,
-- query-heavy) ball. Held at a value that keeps the breakthrough while spreading
-- the clump enough to read.
baseRadius, collideDist, sepStrength, spawnRadius :: Float
baseRadius = 22
collideDist = 26
sepStrength = 0.6
spawnRadius = 40

collideDist2, waypointReach2 :: Float
collideDist2 = collideDist * collideDist
waypointReach2 = waypointReach * waypointReach

-- | Cohesion (exp 009): maneuver/fighting units pull toward friendly soldiers in
-- the band just beyond shoving range out to 'cohesionRadius', forming local
-- groups that move as one. 'cohesionPull' is the per-tick draw (kept below a
-- unit's step so the objective still leads).
cohesionRadius, cohesionPull :: Float
cohesionRadius = 80
cohesionPull = 0.5

cohesionRadius2 :: Float
cohesionRadius2 = cohesionRadius * cohesionRadius

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

-- | A cheap deterministic PRNG -- a SplitMix64 finalizer -- for the few moments
-- the AI ought to surprise itself and the enemy rather than follow a fixed,
-- bankable pattern. Seeded per team from the spawner position ('teamSeed',
-- guaranteed distinct between the two colonies), usually mixed with a per-round
-- nonce so the draw varies each round.
mix64 :: Int -> Int
mix64 x0 =
  let x1 = (x0 `xor` (x0 `shiftR` 33)) * 6364136223846793005
      x2 = (x1 `xor` (x1 `shiftR` 29)) * 1442695040888963407
   in x2 `xor` (x2 `shiftR` 32)

teamSeed :: Team -> Int
teamSeed team = let V2 x y = basePos team in round (x * 73856093 + y * 19349663)

-- | A +/-1 coin from a seed.
coinSign :: Int -> Float
coinSign s = if even (mix64 s) then 1 else -1

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

-- | At round start a colony is blind: it musters at home and knows neither where
-- the enemy is nor which way it lies. Recon must find out.
initialPlan :: Team -> TeamPlan
initialPlan team = TeamPlan Hunter (basePos team) 1 Nothing Nothing

-- Spatial index --------------------------------------------------------------

-- | Grid cell size = the largest neighbour-query radius (max unit vision), so a
-- unit's own cell plus its 8 neighbours is guaranteed to contain every entity
-- within its vision -- no query needs to look further than one cell away.
gridCellSize :: Float
gridCellSize = 170

gridCell :: V2 Float -> (Int, Int)
gridCell (V2 x y) = (floor (x / gridCellSize), floor (y / gridCellSize))

-- | Rebuild the snapshot in one transaction. Used by the synchronous matchup
-- harness, where there is no concurrent writer to contend with. Bases carry a
-- Role too, so they land in the grid and stay visible as targets.
rebuildGrid :: SystemSTM ()
rebuildGrid = do
  m <-
    cfold
      (\acc (t :: Team, k :: Kind, r :: Role, Position q, e :: Entity) -> DM.insertWith (++) (gridCell q) [(t, k, r, e, q)] acc)
      DM.empty
  set global (Grid m)

-- | Every entity in the 3x3 cell block around a point -- the candidate neighbours.
gridNeighbors :: Grid -> V2 Float -> [GridEntry]
gridNeighbors (Grid m) p =
  let (cx, cy) = gridCell p
   in concat [DM.findWithDefault [] (cx + dx, cy + dy) m | dx <- [-1, 0, 1], dy <- [-1, 0, 1]]

-- | How often the spatial snapshot is rebuilt. Deliberately MUCH coarser than the
-- unit tick: every unit reads the grid, so a frequent rewrite would invalidate
-- their in-flight transactions and cause an STM retry storm (it is a single-
-- writer/many-reader hot cell). At ~25 ms the snapshot is only a few px stale --
-- negligible against a 170 px cell -- while the rewrite barely contends.
gridRefreshUs :: Int
gridRefreshUs = 25000

-- | A standalone thread that refreshes the spatial snapshot for the whole match
-- (across rounds). The naive version -- read EVERY position in one transaction --
-- livelocks: with the unit threads constantly writing positions, a single giant
-- read set can never commit, so it spins forever burning a core and starving the
-- sim. Instead: list the entities by their STATIC attrs (Team/Kind/Role/Entity --
-- never written after spawn, so conflict-free with movement), then read each
-- position in its own tiny transaction (conflicts only with that one unit, and
-- only briefly). The snapshot is mildly inconsistent across entities, which is
-- fine -- it is already a stale approximation.
gridRefresher :: Config -> SystemIO ()
gridRefresher _cfg = loop
  where
    loop = do
      ents <- atomically $ cfold (\acc (t :: Team, k :: Kind, r :: Role, e :: Entity) -> (t, k, r, e) : acc) []
      entries <- foldM addEntry [] ents
      atomically $ set global (Grid (DM.fromListWith (++) entries))
      threadDelay gridRefreshUs
      loop
    addEntry acc (t, k, r, e) = do
      mq <- atomically $ do
        ok <- exists e (Proxy @Position)
        if ok then (\(Position q) -> Just q) <$> get e else pure Nothing
      pure $ case mq of
        Just q -> (gridCell q, [(t, k, r, e, q)]) : acc
        Nothing -> acc

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
      myRole <- get ety
      Position p <- get ety
      let vis2 = sq (typeVision myType)
          range2 = sq (typeRange myType)
          speed = typeSpeed myType
      -- Cohesion is /role-local/: a unit pulls only toward same-role friends, so
      -- the flank coheres into its own fist (concentration at the decisive point)
      -- rather than being sucked into the main-body blob.
      grid <- get global
      let (mUnit, mBase, sep, allyNear, coh) =
            foldl' (gather p myTeam myRole vis2) (Nothing, Nothing, V2 0 0, 0 :: Int, V2 0 0) (gridNeighbors grid p)
      -- The nearest enemy /soldier/'s type, for the kiting decision. The target
      -- comes from the (slightly stale) grid snapshot, so it may have died since;
      -- guard the read -- a missing type just means "don't kite this tick".
      mUnitType <-
        if cKite cfg
          then case mUnit of
            Just (e, _, _) -> do
              ok <- exists e (Proxy @UnitType)
              if ok then Just <$> get e else pure Nothing
            Nothing -> pure Nothing
          else pure Nothing
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
        Just (tEnt, _) -> attack myTeam myType tEnt
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
          -- The colony assumes NOTHING about where the enemy is. The strike may
          -- only be aimed at a spawner recon has SEEN ('planEnemyBase'); the force
          -- only knows where the enemy lies once recon makes contact
          -- ('planContact'). Both are earned, never handed out.
          mEnemyBase = planEnemyBase plan
          mContact = planContact plan
          homeBase = basePos myTeam
          Entity eid = ety
          frac z = z - fromIntegral (floor z :: Int)
          -- A distinct bearing + depth per unit (R2/Roberts: two INDEPENDENT
          -- irrationals 1/plastic, 1/plastic^2 -- the old 0.618/0.382 pair was
          -- linearly dependent and collapsed onto a line).
          uang = 2 * pi * frac (fromIntegral eid * 0.5698402910)
          udepth = frac (fromIntegral eid * 0.7548776662)
          udir = V2 (cos uang) (sin uang)
          -- The home holding ring: where the force waits when it has no contact
          -- and so no direction to commit to -- fanned by bearing to watch every
          -- approach, not bunched on the spawner.
          stagePoint = homeBase + udir ^* rallyDist
          -- The maneuver path, once the base is known: sweep wide along the open
          -- flank (clear of the y≈0 grind) then turn in and crash it.
          flankDest eb =
            let V2 ebx _ = eb
                V2 px _ = p
             in if abs (px - ebx) > flankTurnIn then V2 ebx (gap * flankY) else eb
          -- Recon explores to FIND the enemy, then keeps eyes on him. Blind, it
          -- fans out in every bearing from home, ranging deep (the enemy could be
          -- any direction). In contact, it closes on the contact area to map the
          -- force and hunt the spawner. With the spawner found, half the scouts
          -- picket it (keep the fix fresh) while the rest track the force.
          picket = even (mix64 eid)
          reconScout = case (mEnemyBase, mContact) of
            (Just eb, _)
              | picket -> eb + udir ^* (typeVision myType * 0.85)
              | otherwise -> eb + udir ^* (60 + 200 * udepth)
            -- In contact, spawner still hidden: do NOT all pile toward the fight.
            -- SPLIT the scouts -- only the pickets man the contact net (a wide arc
            -- leaning lightly toward the contact, spanning depth toward the base);
            -- the rest keep fanning the whole field (security + other approaches).
            -- Halving the scouts on the cone is what actually cuts its over-density
            -- (ovl), independent of how many Hunters the build fields.
            (Nothing, Just c)
              | picket ->
                  let toC = c - homeBase
                      dist = sqrt (quadrance toC)
                      bear = if dist > 1 then toC ^/ dist else udir
                      mixed = bear ^* 0.45 + udir ^* 0.55
                      mdir = if quadrance mixed > 1e-6 then normalize mixed else bear
                   in homeBase + mdir ^* (max 200 dist * (0.4 + 1.6 * udepth))
              | otherwise -> homeBase + udir ^* (220 + 760 * udepth)
            (Nothing, Nothing) -> homeBase + udir ^* (220 + 760 * udepth)
          -- Counter-recon: an enemy /scout/ nearby gets hunted (deny their eyes);
          -- a real threat it can't kite gets fled (a scout rarely outvalues itself).
          enemyScoutNear = mUnitType == Just Hunter && case mUnit of
            Just (_, _, d2) -> d2 < sq (typeVision myType * 0.6)
            Nothing -> False
          dangerR2 = sq (typeVision myType * 0.45)
          fleePoint = case mUnit of
            Just (_, tPos, _) -> p + (let v = p - tPos in if quadrance v > 1e-6 then normalize v else V2 0 1) ^* 90
            Nothing -> p
          reconDanger = case mUnit of Just (_, _, d2) -> d2 < dangerR2; Nothing -> False
          -- Home garrison: screen the base and intercept anything that reaches it.
          -- Each defender takes a distinct bearing AND a distinct radius (R2 over
          -- an annulus: sqrt(udepth) for even area density), so they spread into a
          -- defensive disc instead of squatting on each other along one thin ring.
          -- The annulus stays close enough that an attacker on the base is still in
          -- sight and gets answered.
          ringPoint = homeBase + udir ^* (baseRadius * 1.6 + sqrt udepth * 60)
          homeThreatR2 = sq 240
          dest = case myRole of
            -- Maneuver force: once recon has FOUND the spawner, ignore the frontal
            -- fight and crash it. Until then, do not march on a rumour -- stage
            -- forward at the muster and let recon acquire it; and if it blunders
            -- into trouble in the dark, pull back rather than feed itself in.
            Flank -> case mEnemyBase of
              -- Spawner found: commit -- sweep wide and crash it.
              Just eb -> Just (flankDest eb)
              Nothing -> case mContact of
                -- In contact but spawner not yet found: advance to the contact
                -- muster to support the find; pull back if it hits trouble blind.
                Just _
                  | reconDanger -> Just fleePoint
                  | otherwise -> Just wp
                -- No contact at all: hold the home ring -- no bearing to commit to.
                Nothing -> Just stagePoint
            -- Recon: kite what it can, hunt enemy scouts, flee what would kill it,
            -- otherwise explore/observe per reconScout.
            Recon
              | Just kd <- kiteDest -> Just kd
              | enemyScoutNear -> case mUnit of Just (_, tPos, _) -> Just tPos; Nothing -> Just reconScout
              | reconDanger -> Just fleePoint
              | otherwise -> Just reconScout
            -- Garrison: intercept an enemy that has reached home, else hold a ring
            -- slot -- but with a tolerance, so once it is roughly on its slot it
            -- STOPS pulling back to the exact point. Without that, separation bumps
            -- a defender a pixel off, it darts back, gets bumped again -- an endless
            -- jitter; the deadzone lets the slots settle, spaced by separation.
            Defend -> case mUnit of
              Just (_, tPos, _) | quadrance (tPos - homeBase) <= homeThreatR2 -> Just tPos
              _ | quadrance (ringPoint - p) <= sq (collideDist * 1.3) -> Nothing
                | otherwise -> Just ringPoint
            -- Main body: when recon has made contact, fix the enemy front (press
            -- into melee when supported, else mass at the home ring). With no
            -- contact, hold the home ring -- there is no front to march on yet.
            MainBody
              | Just kd <- kiteDest -> Just kd
              | supported -> case target of
                  Just (_, tPos, _) -> Just tPos
                  Nothing -> case mContact of
                    Just _
                      | quadrance (wp - p) > waypointReach2 -> Just wp
                      | otherwise -> Nothing
                    Nothing
                      | quadrance (stagePoint - p) > waypointReach2 -> Just stagePoint
                      | otherwise -> Nothing
              | quadrance (stagePoint - p) > waypointReach2 -> Just stagePoint
              | otherwise -> Nothing
          approach = case dest of
            Just d -> normalize (d - p) ^* (speed * dt)
            Nothing -> V2 0 0
          -- Main body and flank both cohere -- now role-locally (the gather only
          -- summed same-role friends), so the flank forms its own fist (mass at
          -- the decisive point) instead of merging into the front. A unit in
          -- reach of a target releases cohesion and presses (else two cohered
          -- blobs bounce off and the front freezes). Recon and the garrison
          -- spread, never cohere.
          cohesion = case (myRole, struck) of
            (Recon, _) -> V2 0 0
            (Defend, _) -> V2 0 0
            (_, Just _) -> V2 0 0
            _ | quadrance coh > 1e-6 -> normalize coh ^* cohesionPull
              | otherwise -> V2 0 0
      -- Cap the summed separation to a few advance-steps so a dense crowd still
      -- flows toward its objective (a big pile sums many small pushes); enough to
      -- spread the spawn, not so much it overwhelms the advance.
      let sepCap = speed * dt * 4
          sepClamped = if quadrance sep > sepCap * sepCap then normalize sep ^* sepCap else sep
      set ety (Position (p + approach + sepClamped + cohesion))
      pure True
  where
    dt = fromIntegral (cTick cfg) / 1e6
    gather p myTeam myRole vis2 (mUnit, mBase, sep, allies, coh) (t, k, r, e, q) =
      (mUnit', mBase', sep', allies', coh')
      where
        dv = q - p
        d2 = quadrance dv
        seen = t /= myTeam && d2 <= vis2
        sep'
          | k == Soldier && e /= ety && d2 > 1e-6 && d2 < collideDist2 =
              -- Linear falloff (sepStrength at contact, 0 at collideDist) instead
              -- of the old 1/d term that blew up as d->0 -- at high density a pile
              -- of near-coincident units produced an unbounded force that swamped
              -- the advance step and gridlocked the crowd. Direction only; the
              -- total is magnitude-capped at the integration site.
              let d = sqrt d2
               in sep + normalize (p - q) ^* (sepStrength * (collideDist - d) / collideDist)
          | otherwise = sep
        -- Friendly soldiers close enough to count as local support.
        allies'
          | t == myTeam && k == Soldier && e /= ety && d2 <= musterRadius2 = allies + 1
          | otherwise = allies
        -- Cohesion: pull toward friendly soldiers /of my own role/ in the band
        -- beyond shoving range out to 'cohesionRadius', so each task force draws
        -- into its own local group -- the flank into a concentrated fist, the
        -- main body into the front -- rather than smearing or merging together.
        coh'
          | t == myTeam && k == Soldier && r == myRole && e /= ety && d2 > collideDist2 && d2 < cohesionRadius2 = coh + dv
          | otherwise = coh
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
damage is flat -- the counter cycle is emergent (range/speed/HP + kiting), not a
multiplier; a Lance does double against a base. Soldier kills score; razing a base
does not (the coordinator notices that separately).
-}
attack :: Team -> UnitType -> Entity -> SystemSTM ()
attack killer atkType victim = do
  stillThere <- exists victim (Proxy @Health)
  when stillThere $ do
    Health h <- get victim
    k <- get victim
    (dmg, mDef) <- case k of
      Base -> pure (typeDamage atkType * (if atkType == Lance then 2 else 1), Nothing)
      Soldier -> do
        defType <- get victim
        pure (typeDamage atkType, Just defType)
    -- Ledger the applied damage: soldier-vs-soldier into the type matrix, base
    -- damage into the center-of-gravity ledger.
    case mDef of
      Just defType -> modify global (logDamage killer atkType defType (min dmg h))
      Nothing -> modify global (addBaseDmg killer (min dmg h))
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

-- | Telemetry reference: the force size at which a colony counts as fully
-- "present" for the security metric. `sec` is the unseen fraction GATED by
-- presence = min 1 (force/secRef), so a wiped or vanishing army reads ~0 instead
-- of the degenerate "0/0 = perfectly hidden", while any real force (>= secRef) is
-- scored on denial quality as before. See exp 011.
secRef :: Float
secRef = 12

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

-- | Telemetry: recon REDUNDANCY -- the mean number of OTHER friendly scouts whose
-- vision heavily overlaps each scout's (centre within one vision radius). This is
-- the instrument the plain 'recon' coverage metric was missing. Coverage
-- (seen/total) is an outcome and is /satisfied by piling/: two scouts stacked in
-- one cone see the same enemies, gaining nothing, yet coverage never flags the
-- wasted unit -- so the AI happily wastes a whole swarm covering one cone with
-- overlapping ranges. Redundancy makes the waste visible: ~0 means the vision
-- disks tile fresh ground (each scout earns its keep); a high value means scouts
-- are stacked, re-covering the same cone. The lesson generalises: an outcome
-- metric with no EFFICIENCY/overlap term rewards redundancy and hides waste.
reconOverlap :: Team -> SystemSTM Float
reconOverlap team = do
  scouts <-
    cfold
      (\acc (t :: Team, r :: Role, ut :: UnitType, Position q, e :: Entity) -> if t == team && r == Recon then (e, q, typeVision ut) : acc else acc)
      []
  let n = length scouts
      degree (e1, q1, vr) = length [() | (e2, q2, _) <- scouts, e1 /= e2, quadrance (q1 - q2) < vr * vr]
  pure (if n == 0 then 0 else fromIntegral (sum (map degree scouts)) / fromIntegral n)

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

-- | Warfighting scorecard (exp 008) helpers.
--
-- Focus / Schwerpunkt: the largest local cluster of a team's force over its
-- total -- ~1 when concentrated at a point, small when smeared across the field.
concentration :: Team -> SystemSTM Float
concentration team = do
  pts <- cfold (\acc (t :: Team, k :: Kind, Position q) -> if t == team && k == Soldier then q : acc else acc) []
  let n = length pts
      r2 = sq 130
      dens c = length [() | q <- pts, quadrance (q - c) <= r2]
      best = if null pts then 0 else maximum (map dens pts)
  pure (if n == 0 then 0 else fromIntegral best / fromIntegral n)

-- Local groups: the number of distinct clusters a team's force breaks into
-- (single-linkage connected components within a squad radius). Centralized
-- smearing → many isolated specks; cohesion → a few dense groups.
groupCount :: Team -> SystemSTM Int
groupCount team = do
  pts <- cfold (\acc (t :: Team, k :: Kind, Position q) -> if t == team && k == Soldier then q : acc else acc) []
  pure (length (components pts))
  where
    r2 = sq 95
    components [] = []
    components (q : qs) = let (cl, rest) = flood [q] qs in cl : components rest
    flood cluster pool =
      case partition (\x -> any (\c -> quadrance (x - c) <= r2) cluster) pool of
        ([], _) -> (cluster, pool)
        (near, far) -> flood (cluster ++ near) far

-- Combined arms: Shannon entropy of the (H,G,L) mix, normalised to 0..1 (1 = an
-- even three-way split, 0 = mono).
armsEntropy :: Census -> Float
armsEntropy (h, g, l) =
  let tot = h + g + l
      ps = [fromIntegral x / fromIntegral tot | x <- [h, g, l], x > 0]
   in if tot == 0 then 0 else negate (sum [p * logBase 3 p | p <- ps])

-- Initiative: how far into enemy territory a team's front sits (0 = own base,
-- 1 = enemy base), from its mean soldier x.
initiative :: Team -> Float -> Float
initiative team mx =
  let V2 ox _ = basePos team
      V2 ex _ = basePos (enemyOf team)
   in max 0 (min 1 ((mx - ox) / (ex - ox)))

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
  Threat up0 down0 g0 surp0 kl0 <- teamThreat team <$> get global
  prevPlan <- teamPlan team <$> get global
  let prevBase = planEnemyBase prevPlan
      prevContact = planContact prevPlan
      visible = [(q, ut) | (q, ut) <- enemies, seenBy sources q]
      -- Discover the enemy spawner: once any friendly source covers its position
      -- it is known and remembered for the round; until then the strike force is
      -- aiming at a rumour and must not commit. Sticky so it survives the fog.
      ebPos = basePos (enemyOf team)
      enemyBaseSeen = case prevBase of
        Just b -> Just b
        Nothing -> if seenBy sources ebPos then Just ebPos else Nothing
      -- Contact: the earned centroid of where the enemy actually is. Updated from
      -- this pass's sightings, sticky through the fog. Nothing == still blind, so
      -- recon explores and the force holds. This is what makes the force orient on
      -- the enemy it has FOUND rather than a direction it was handed.
      contact' = case map fst visible of
        [] -> prevContact
        ps -> Just (foldr (+) (V2 0 0) ps ^/ fromIntegral (length ps))
      census = foldr (bumpType . snd) (0, 0, 0) visible
      waypoint = chooseWaypoint team (map fst visible)
      -- Fold this pass's sightings into the decaying flank memory, then commit
      -- the maneuver to the flank the colony /remembers/ as emptier (with
      -- hysteresis). Memory, not a live read, so the choice persists after the
      -- enemy slips into the fog -- and the two colonies, seeing different
      -- things, diverge instead of both lunging at the same flank.
      upSeen = fromIntegral (length [() | (V2 _ y, _) <- visible, y > 0])
      downSeen = fromIntegral (length [() | (V2 _ y, _) <- visible, y <= 0])
      up' = up0 * threatDecay + upSeen
      down' = down0 * threatDecay + downSeen
      gap = chooseGap g0 up' down'
      -- Treat the (normalised) flank memory as a belief over where the enemy is,
      -- and read off this pass's information dynamics: surprisal (cross-entropy
      -- of the sightings against the PRIOR belief = prediction error), and the
      -- Bayesian surprise KL(posterior || prior). Both EMA-smoothed for the
      -- scorecard; held over passes with no sighting.
      (surp', kl') = beliefSurprise (up0, down0) (up', down') (upSeen, downSeen) surp0 kl0
  modify global (setTeamThreat team (Threat up' down' gap surp' kl'))
  modify global (setTeamPlan team (TeamPlan (planNextFor own (length visible) census) waypoint gap enemyBaseSeen contact'))
  where
    seenBy srcs q = any (\(s, r) -> quadrance (q - s) <= r * r) srcs

-- | How fast flank memory fades each planning pass (0 = goldfish, 1 = elephant).
threatDecay :: Float
threatDecay = 0.75

-- | Information dynamics of one flank-belief update (all in bits). Inputs: the
-- prior counts, the posterior counts, this pass's raw sightings, and the running
-- EMAs. Returns the updated (surprisal, KL) EMAs -- surprisal = cross-entropy of
-- the sightings against the /prior/ (prediction error / "free energy"), KL =
-- D_KL(posterior || prior) (how much the belief moved). With no sighting the
-- EMAs are held: nothing was observed, so there is no surprise.
beliefSurprise :: (Float, Float) -> (Float, Float) -> (Float, Float) -> Float -> Float -> (Float, Float)
beliefSurprise (u0, d0) (u1, d1) (us, ds) surp0 kl0
  | us + ds <= 0 = (surp0, kl0)
  | otherwise = (ema surp0 surprisal, ema kl0 kl)
  where
    eps = 0.5
    norm a b = let s = a + b + 2 * eps in ((a + eps) / s, (b + eps) / s)
    (pu0, pd0) = norm u0 d0
    (pu1, pd1) = norm u1 d1
    (ou, od) = let s = us + ds in (us / s, ds / s)
    surprisal = negate (ou * logBase 2 pu0 + od * logBase 2 pd0)
    kl = pu1 * logBase 2 (pu1 / pu0) + pd1 * logBase 2 (pd1 / pd0)
    ema old new = old * 0.7 + new * 0.3

-- | Entropy (bits) of a colony's belief over the enemy flank -- its uncertainty.
beliefEntropy :: Float -> Float -> Float
beliefEntropy u d =
  let eps = 0.5
      s = u + d + 2 * eps
      pu = (u + eps) / s
      pd = (d + eps) / s
      h x = if x <= 0 then 0 else x * logBase 2 (1 / x)
   in h pu + h pd

-- | Commit the maneuver to the flank remembered as emptier, with hysteresis:
-- only switch when the other flank is clearly (≥40%) lighter, so the force does
-- not dither between flanks every planning tick.
chooseGap :: Float -> Float -> Float -> Float
chooseGap g0 up down
  | up < down * 0.6 = 1 -- top clearly emptier -> flank top
  | down < up * 0.6 = -1 -- bottom clearly emptier -> flank bottom
  | otherwise = g0 -- ambiguous: hold the committed flank

-- | Muster on the enemy the colony can see; with the field dark, muster at HOME
-- -- the colony has no idea which way the enemy lies, so it does not stream off
-- on a bearing it was never given. The force holds while recon finds the enemy.
chooseWaypoint :: Team -> [V2 Float] -> V2 Float
chooseWaypoint team [] = basePos team
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

-- | How the non-Hunter reinforcements divide between the three ground roles:
-- a flanking force, a home garrison, and the main body (the remainder).
flankShare, defendShare :: Double
flankShare = 0.45
defendShare = 0.2

-- | Task-force role for a fresh soldier (exp 007): Hunters scout + screen
-- (Recon); the rest split into the Flank (strike the base), a Defend garrison
-- (hold home so the enemy flank can't waltz onto an empty spawner), and the
-- MainBody that fixes the front.
rollRole :: UnitType -> Double -> Role
rollRole Hunter _ = Recon
rollRole _ r
  | r < flankShare = Flank
  | r < flankShare + defendShare = Defend
  | otherwise = MainBody

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
startRound :: Config -> Int -> SystemIO (Entity, Entity)
startRound cfg n = do
  -- The two colonies commit to OPPOSITE opening flanks (so the maneuvers don't
  -- mirror and collide), but which colony takes top vs bottom flips
  -- unpredictably each round -- a cheap coin seeded from the round and both
  -- spawner positions, so neither the enemy nor the AI itself can bank on it.
  let pick = coinSign (mix64 n + teamSeed Red + teamSeed Blue)
  atomically $ do
    cmapM_ $ \(_ :: Team, e :: Entity) -> destroy e (Proxy @All)
    set global (mempty :: KillScore)
    set global (mempty :: DamageLog)
    set global (mempty :: BaseDamage)
    set global (Plans (initialPlan Red) (initialPlan Blue))
    set global (ThreatMem (Threat 0 0 pick 0 0) (Threat 0 0 (negate pick) 0 0))
    set global Playing
  -- Bases carry a (behaviourally inert) Role only so the role-aware gather fold
  -- still sees them as targets; Defend reads sensibly for the thing defended.
  redBase <- atomically $ newEntity (Red, Base, Position (basePos Red), Health baseHp, Defend)
  blueBase <- atomically $ newEntity (Blue, Base, Position (basePos Blue), Health baseHp, Defend)
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
      (redBase, blueBase) <- startRound cfg n
      when (cDebug cfg) (void $ forkSys heartbeat)
      outcome <- waitWinner redBase blueBase
      atomically $ case outcome of
        Win winner -> modify global (addWin winner) >> set global (RoundOver winner)
        Draw reason -> set global (RoundDraw reason)
      report n outcome
      threadDelay (cOver cfg)
      loop (n + 1)

    -- A gated 2 Hz pulse of each colony's live composition and current plan,
    -- so you can watch the counter-play shift through the fog.
    heartbeat = do
      (ph, rc, bc, Plans rp bp, DamageLog dm, rHp, bHp, rCov, bCov, rFront, bFront, rBad, bBad, rConc, bConc, BaseDamage bdR bdB) <- atomically $ do
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
        rconc <- concentration Red
        bconc <- concentration Blue
        rgrp <- groupCount Red
        bgrp <- groupCount Blue
        rovl <- reconOverlap Red
        bovl <- reconOverlap Blue
        tm <- get global
        bd <- get global
        pure (ph, rc, bc, pl, dl, rh, bh, rcov, bcov, rf, bf, (rbad, rout, rsg), (bbad, bout, bsg), (rconc, rgrp, rovl, teamThreat Red tm), (bconc, bgrp, bovl, teamThreat Blue tm), bd)
      let teamDmgF t = sum [v | ((t', _, _), v) <- DM.toList dm, t' == t]
          teamDmg t = round (teamDmgF t) :: Int
      let foundFlag p = (if isJust (planContact p) then "c" else "-") ++ (if isJust (planEnemyBase p) then "B" else "-")
      traceM $
        "[hb] R " ++ showCensus rc ++ " base=" ++ show (round rHp :: Int) ++ " next=" ++ show (planNext rp) ++ " find=" ++ foundFlag rp ++ " dmg=" ++ show (teamDmg Red) ++ " " ++ showTel rCov rFront rBad
          ++ " | B " ++ showCensus bc ++ " base=" ++ show (round bHp :: Int) ++ " next=" ++ show (planNext bp) ++ " find=" ++ foundFlag bp ++ " dmg=" ++ show (teamDmg Blue) ++ " " ++ showTel bCov bFront bBad
      -- Warfighting adherence scorecard (exp 008), per side.
      traceM $
        "[score] R " ++ scoreLine Red rCov bCov rc rFront rConc (teamDmgF Red) bdR
          ++ " | B " ++ scoreLine Blue bCov rCov bc bFront bConc (teamDmgF Blue) bdB
      when (isPlaying ph) (threadDelay 500000 >> heartbeat)

    -- recon = my coverage of the enemy; sec = my hidden-force count / secRef (NOT
    -- the unseen fraction -- annihilation must not read as perfect security);
    -- ovl = recon REDUNDANCY (mean overlapping scouts per scout) -- the efficiency
    -- term coverage alone lacks: high ovl with low recon == a swarm piled in one
    -- cone, re-covering the same ground; arms = combined-arms mix entropy; init = how deep my front is in his half;
    -- focus = force concentration; grp = number of distinct groups (smear vs
    -- cohere); cog = base-damage share * base-worth landed (a scratch reads ~0).
    -- unc/surp/kl = the in-the-
    -- moment information dynamics of my belief over the enemy flank (bits):
    -- uncertainty (entropy), surprisal (prediction error), Bayesian surprise.
    scoreLine team myCov foeCov census (mx, _) (conc, grp, ovl, thr) soldierDmg baseDmg =
      let frac (s, t) = if t == 0 then 0 :: Float else fromIntegral s / fromIntegral t
          pct x = show (round (x * 100) :: Int)
          bits x = show (fromIntegral (round (x * 100) :: Int) / 100 :: Float)
          -- Security = the unseen fraction (denial quality, discriminating) GATED
          -- by force presence, so it stays informative in normal play but a wiped
          -- (or vanishing) army reads ~0 instead of "0/0 = perfectly hidden"
          -- (exp 011). presence saturates at 1 for any real force (>= secRef).
          secScore =
            let (s, t) = foeCov
                unseenFrac = if t == 0 then 0 else fromIntegral (max 0 (t - s)) / fromIntegral t
                presence = min 1 (fromIntegral t / secRef)
             in unseenFrac * presence
          -- CoG focus paired with MAGNITUDE: the share of damage on the base, times
          -- how much of a base's worth has actually landed. A single scratch (share
          -- 1.0 but ~0 damage) no longer reads as a decisive strike (exp 011).
          cogShare = if soldierDmg + baseDmg <= 0 then 0 else baseDmg / (soldierDmg + baseDmg)
          cogScore = cogShare * min 1 (baseDmg / baseHp)
       in "recon=" ++ pct (frac myCov) ++ " sec=" ++ pct secScore
            ++ " ovl=" ++ bits ovl
            ++ " arms=" ++ pct (armsEntropy census) ++ " init=" ++ pct (initiative team mx)
            ++ " focus=" ++ pct conc ++ " grp=" ++ show grp
            ++ " cog=" ++ pct cogScore
            ++ " unc=" ++ bits (beliefEntropy (thUp thr) (thDown thr))
            ++ " surp=" ++ bits (thSurprise thr) ++ " kl=" ++ bits (thKL thr)

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

    bothAlive redBase blueBase =
      atomically ((,) <$> exists redBase (Proxy @Health) <*> exists blueBase (Proxy @Health))

    -- Poll until a base falls (or the round cap is hit -- a stalemate draw, two
    -- competent defenses neither could crack), then hold a grace countdown: if
    -- the other base falls within it too, that is also a draw (a mutual strike
    -- traded both spawners); otherwise the survivor wins. Nothing == draw.
    waitWinner redBase blueBase = poll cap
      where
        cap = if cRoundCap cfg <= 0 then maxBound else cRoundCap cfg `div` max 1 (cPoll cfg)
        poll k = do
          (ra, ba) <- bothAlive redBase blueBase
          if ra && ba
            then
              if k <= (0 :: Int)
                then pure (Draw Stalemate)
                else threadDelay (cPoll cfg) >> poll (k - 1)
            else do
              threadDelay (cGrace cfg)
              (ra', ba') <- bothAlive redBase blueBase
              pure $ case (ra', ba') of
                (False, False) -> Draw MutualFall
                (True, _) -> Win Red
                (_, True) -> Win Blue

    report n outcome = do
      (KillScore kr kb, Wins wr wb, dl) <-
        atomically ((,,) <$> get global <*> get global <*> get global)
      let result = case outcome of
            Win w -> show w ++ " wins"
            Draw MutualFall -> "Draw (both spawners fell)"
            Draw Stalemate -> "Draw (stalemate -- round cap, both standing)"
      liftIO . putStrLn $
        concat
          [ "Round "
          , show n
          , ": "
          , result
          , "  | kills R/B "
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

-- | Honest on-screen subtitle for each kind of draw.
drawText :: DrawReason -> String
drawText MutualFall = "BOTH SPAWNERS FELL"
drawText Stalemate = "STALEMATE -- ROUND CAP, BOTH STANDING"

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
      scoreCard banner =
        color (withAlpha 0.74 black) (rectangleSolid 1600 900)
          <> banner
          <> label white (-150) 158 ("kills R " ++ show kr ++ " / B " ++ show kb ++ "     match R " ++ show wr ++ " / B " ++ show wb)
          <> damageBlock (teamColor Red) (-312) 116 "RED damage  (attacker vs defender)" (teamMatrix Red dl)
          <> damageBlock (teamColor Blue) (-312) (-24) "BLUE damage  (attacker vs defender)" (teamMatrix Blue dl)
      overlay = case phase of
        Playing -> mempty
        RoundOver w ->
          scoreCard (label white (-58) 215 "ROUND OVER" <> label (teamColor w) (-85) 185 (show w ++ " TEAM WINS"))
        RoundDraw reason ->
          scoreCard (label white (-32) 215 "DRAW" <> label (greyN 0.7) (-200) 185 (drawText reason))
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
matchupCfg :: Bool -> Bool -> Float -> Config
matchupCfg muster kite arena =
  headlessCfg {cTick = 16000, cDebug = False, cMuster = muster, cKite = kite, cArena = arena}

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
          atomically rebuildGrid
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
  -- Defaults mirror the live combat model (kiting on); ablate with --nokite.
  -- Muster stays off to isolate raw combat.
  let muster = "--muster" `elem` args
      kite = not ("--nokite" `elem` args)
      arena = if "--arena" `elem` args then 160 else 0
      positional = filter (not . isPrefixOf "--") args
  case positional of
    (redS : blueS : rest) -> do
      let reps = case rest of (r : _) -> read r; _ -> 200 :: Int
          cfg = matchupCfg muster kite arena
          redC = parseComp redS
          blueC = parseComp blueS
      w <- initWorld
      (rw, bw, dr) <- runWith w $ do
        set global (Camera 0 1)
        let one (r, b, d) _ = do
              atomically $ cmapM_ (\(_ :: Team, e :: Entity) -> destroy e (Proxy @All))
              spawnArmy Red redC (V2 (-70) 0)
              spawnArmy Blue blueC (V2 70 0)
              -- There is no strategist in the harness, so hand each side a standing
              -- plan that points at the other: contact at the enemy column so the
              -- main body closes and fights (without it, short-sighted units never
              -- make contact and every non-Hunter matchup draws). Raw combat, which
              -- is what the harness is meant to measure.
              atomically $
                set
                  global
                  ( Plans
                      (TeamPlan Hunter (V2 70 0) 1 Nothing (Just (V2 70 0)))
                      (TeamPlan Hunter (V2 (-70) 0) 1 Nothing (Just (V2 (-70) 0)))
                  )
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
    void $ forkSys (gridRefresher cfg)
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
