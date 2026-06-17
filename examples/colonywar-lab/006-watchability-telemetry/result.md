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

## Still to add (engagement-quality + decisiveness instruments)

- **Kill matrix** by (attacker→victim) type → feed-rate (units dying into losing
  matchups) to measure the *suicidal advance* problem.
- **Per-round draw terminator** (`cRoundCap`) so draw-rate is countable, plus a
  per-round summary aggregator (divergence, lead-flips, front travel).
