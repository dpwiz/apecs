# Exp 006 — Result (in progress)

## Coverage telemetry confirms the fog-collapse diagnosis

Added `enemyCoverage` (fraction of enemy soldiers a team sees through base + unit
vision) and `teamFront` (mean soldier x, max |y|) to the headless heartbeat.
A 50 s run of the current build:

- **`ymax` (combat vertical spread) ≈ 20–56 px**, vs vision radii **75 (Lance) /
  95 (Guard) / 170 (Hunter)** and **base 160**. The fight lives in a thin
  horizontal band several times *narrower* than a single unit's sight. The
  nominally-2D arena is collapsed to a 1D lane.
- **Coverage frequently `34/34`** (a side sees the *entire* enemy army), dipping
  only transiently. Fog is largely defeated → the strategist's counter-pick is
  near-deterministic → predetermined mirror / no scouting payoff.

This is the user's diagnosis, now measured: *vision covers the whole used width
of the field, so partial information collapses into full information.* The
instrument the old telemetry lacked.

## Fix direction (entropy = restore partial information)

Make the battlefield genuinely 2D so vision (and thus knowledge) is partial:
- spread combat across the field height (units use the vertical dimension —
  wider lanes, flanks, multiple contact points) so `ymax` approaches/exceeds
  vision radius, and/or
- shrink vision so it no longer spans the combat band — making recon (Hunters) a
  real investment with real payoff, and the counter-game uncertain → divergent,
  decisive rounds.

## Wider field (bases ±480) already restores fog

Side effect of the 2× field: enemy coverage drops from 34/34 to **~5–17/34**
each side — fog is consequential from arena size alone, before any vision tuning.

## Over-eager instruments: bad-fight + outnumbered rates

Added `engageRates` to the heartbeat, per team:
- **bad** = fraction whose nearest enemy hard-counters them (a fight to avoid),
- **out** = fraction locally outnumbered within a 70px skirmish radius.

Headless reads **both low** (mostly 0%, rare 3–15% spikes). Units are *not*
systematically standing in counter matchups (the two sides keep mirrored comps,
so nearest-enemy is usually the same type) nor charging into superior local
force. So the "too eager to advance into certain death" the user saw is most
likely the **relentless press-into-melee** itself (units always close, added to
break the stalemate) rather than bad target selection — a behaviour to temper
carefully (tempering risks re-stalemating). Needs the user's live read on the new
(wider-field) build to confirm before changing the press.

## Still to add

- **Per-round draw terminator** (`cRoundCap`) so draw-rate is countable, plus a
  per-round aggregator (divergence, lead-flips, front travel).
- Possibly **trade efficiency** (per-unit damage dealt before death) if bad/out
  prove too coarse for the feeding the user observed.
