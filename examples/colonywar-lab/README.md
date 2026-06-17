# ColonyWar lab — hypothesis & experiment ledger

A measured-loop log for tuning the STM Colony War showcase (`examples/ColonyWar.hs`).
Every design claim is a **hypothesis** with a **metric** and a **pre-registered
falsification threshold**, evaluated by a harness that emits structured data.
The sentence we want to be able to write after each change is: *"I set X to make
A>B by ≥k; the data says Z."*

## Operationalized goals

| Goal | Metric | Falsify if |
|---|---|---|
| Fair | side win-rate over N rounds, ±95% CI (Wilson) | CI excludes 0.50 |
| No fixpoint | strategy round-robin payoff matrix; exploitability of best pure comp | a pure comp ≥ ~0.55 vs the field, or empirical Nash is pure |
| Real counters | k×k matchup matrix at equal cost; per-edge win-rate + break-even ratio | an intended counter wins <0.60 at parity, or needs >1.2× numbers |
| Dramatic | round-length dist; lead-flips/round; comeback rate; kill-margin mix | >X% rounds hit the time cap; ~0 lead-flips; comeback ~0 |
| Fog matters | win-rate(adaptive+recon) − win-rate(blind) | recon investment has ~no win-rate effect |

## Instruments

- **Matchup harness** (`ColonyWar.hs --matchup`): scripted armies, synchronous
  step-to-elimination, full information, no strategist. Fast (~18 ms/battle).
  `--muster` toggles the rally-wave behaviour (ablation).
- **Full-game headless** (`--headless`): the real concurrent game; emits
  `Round N:` results + heartbeats. Aggregated by `../tmp` scripts into win-rate
  CIs and drama metrics.

## Experiments

| # | Question | Verdict |
|---|---|---|
| [001](001-current-triad/) | Is the current Warrior/Scout/Siege triad a real cost-parity counter cycle? | ❌ No — **Siege strictly dominant** (a fixpoint); Scout combat-dead; S>C reversed. |
| [002](002-cycle-feasibility/) | Can a real cycle come from stat retune, or are new behaviours needed? | 🔬 Stats can't — combat is **winner-take-all** (1.00/0.00, no gradient). Drama needs behaviours (kite/screen) + fog. |
| [003](003-kiting-viability/) | Is kiting a viable way to create a counter? | ✅ Yes — flips a 0.00 stand-up loss to 1.00 from the flag alone. Duels stay winner-take-all (fine: decisive RPS; drama is fog+macro). |

## Baseline scorecard (committed `0a8b067`, 104 full-game rounds)

- **Fair**: ✅ win-rate 0.490, CI [0.396, 0.585].
- **Dramatic**: ✅ 48% nail-biters / 38% blowouts, ~4 lead-flips/round, ~0 stalemates.
- **Real counters / No fixpoint / Fog matters**: ⚠️ unmeasured → motivates the harness (exp 001+).
