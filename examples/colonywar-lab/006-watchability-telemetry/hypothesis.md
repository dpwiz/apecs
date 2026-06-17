# Exp 006 — Telemetry that can see watchability (entropy + engagement quality)

## Why (the process failure this fixes)

The harness measured the unit cycle (win-rates, no-fixpoint) but missed two
problems the user saw instantly on playtest: units **advancing suicidally**, and
rounds **drawing from too little entropy** (predetermined symmetric reactions).
Win-rate in a reductive harness (no fog/strategist/respawn) structurally cannot
show either. Fix the *instruments* before fixing the game.

**The user's refined entropy diagnosis (the actual root cause):** the "entropy"
deficit is a **fog collapse**. The arena is nominally 2D, but vision ranges
(Hunter 170, base 160) cover the whole *vertical width* of the combat lane, so
partial information collapses into near-full information — the field is
effectively **1D** and both sides see almost everything. With ~full information
the rock-paper-scissors counter-game is deterministic (each side always knows the
true counter) → predetermined symmetric mirror → draw, and **recon has no payoff**
(the "Fog matters" goal fails). So the headline entropy instrument is a
**coverage metric**: what fraction of the enemy is actually hidden.

## Operationalize "watchable" into measurable proxies

| Quality | Proxy metric | Bad-looks-like |
|---|---|---|
| Both sides play well (not suicidal) | **kill matrix** by (attacker→victim) type; feed-rate = kills *against* the cycle direction (e.g. Hunters dying to Guards) | many deaths into losing matchups |
| Decisive (rounds resolve) | **outcome distribution** over many rounds (Red/Blue/**Draw**) | high draw-rate |
| Dramatic (not predetermined) | **composition divergence** = L1 distance between the two teams' (H,G,L) over time; **lead-flips** (kill-diff sign changes); **front travel** = range of the soldier-centroid x | divergence ~0 (mirrored), front pinned at x≈0, ~0 lead-flips |
| Fog matters / partial info | **coverage** = fraction of enemy soldiers each side can see through its vision sources (base + units); also the y-spread of combat vs vision radius | coverage ≈ 1.0 (nothing hidden → field collapsed to 1D, recon worthless) |

## Instruments to add

1. **Per-round draw terminator** (`cRoundCap` seconds; 0 = unbounded, live game).
   Stalemates must terminate as `Draw` so draw-rate is countable. This is a
   *measurement* device, not the gameplay resolution.
2. **Front centroid** per team in the heartbeat (mean soldier x) → front-travel /
   pinned-stalemate signal.
3. **Kill matrix** global: kills by (team, attacker type, victim type), emitted at
   round end → feeding / engagement-quality signal.
4. **Aggregator** (`agg_watch.py`) over a headless run: per-round outcome, draw
   rate, mean divergence, front travel, lead-flips, feed-rate; printed as a report.

## Validation (the instrument must catch the known-bad build)

Pre-registered expectation for the *current* stalemating build:
- draw-rate **high** (most/all rounds), front travel **near 0** (pinned center),
  composition divergence **low** (mirrored counter-chase). If the telemetry does
  NOT show these, the instrument is wrong, not the game.
- feed-rate should expose the suicidal pressing (units dying into counters).

Only once these read as expected do we trust the telemetry to guide the fixes
(de-eager the units; inject entropy).

## Result

See [result.md](result.md).
