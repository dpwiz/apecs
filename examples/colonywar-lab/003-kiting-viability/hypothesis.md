# Exp 003 — Is kiting (behaviour) a viable way to create a counter?

## Question

Exp 002 showed stats can't make a healthy counter (combat is winner-take-all).
Does a **behaviour** — kiting (fire while backing off to hold the range gap) —
let a fast, longer-ranged "Hunter" beat a slow short-ranged "Lance" bruiser that
it *loses to in a stand-up fight*? And does it produce a numeric **gradient**?

## Method

Added `cKite` ablation flag + kiting logic to `stepUnit`: a unit that
out-ranges AND out-runs its target holds it at ~0.92× its own range (closes if it
slips out, backs straight off if it gets too close). Temp archetypes via the
matchup harness: Scout = Hunter (long range, fast, low HP), Siege = Lance (short
range, slow, tanky). Also added `cArena` to clamp the harness to a box so kiters
can be cornered.

Ablation: `--matchup scout:12 siege:12 400 [--kite] [--arena]`, sweeping Hunter
range and numbers.

## Predictions

- Kiting **viable as a counter** if it flips a stand-up *loss* (≈0.00) into a
  *win* (>0.5) with stats unchanged — only the flag toggled.
- Kiting gives a **gradient** if cornering (arena) and numbers move the win-rate
  off 0.00/1.00 into a band.

## Result

See [result.md](result.md).
