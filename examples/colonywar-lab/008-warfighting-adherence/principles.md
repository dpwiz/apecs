# Exp 008 — Warfighting (MCDP1) principles, operationalized and measured

The brief: write down the core + supporting principles of *Warfighting*; measure
each side's **adherence**; where adherence is low, hypothesise a fix and falsify
it. And watch for the failure mode: if, after the fixes, the sides still fight
**indefinitely**, the game has "tic-tac-toe'd" — a symmetric drawn equilibrium,
an *entropy deficit* — and the cure is not more polish but more genuine **avenues
for risky, high-reward play**.

## Core principles (the essence of maneuver warfare)

1. **Tempo / initiative.** Act faster than the enemy can react (the OODA loop);
   dictate the action rather than respond. Generate a rhythm he can't match.
2. **Focus — Schwerpunkt / main effort.** Concentrate combat power at the
   decisive place and time; everywhere else, economy of force. Mass, don't smear.
3. **Surfaces and gaps.** Avoid enemy strength (surfaces); flow through and
   exploit weakness (gaps). *Reconnaissance pull* — let what recon finds draw the
   force to the gap.
4. **Targeting cohesion — center of gravity / critical vulnerability.** Defeat the
   enemy by shattering his cohesion, not by grinding him down. Strike the thing
   that makes him a coherent whole (here: the base) through a vulnerability.
5. **Surprise and boldness.** Unexpected action; create and exploit opportunity;
   accept risk for decisive results. Predictability is death.

## Supporting principles

6. **Combined arms.** Pose a *dilemma*, not a problem: countering one threat must
   expose you to another. (Here: a mixed Hunter/Guard/Lance force.)
7. **Orienting on the enemy.** Adapt to the actual enemy, not a script — read him
   and counter what he is actually doing.
8. **Security / counter-reconnaissance.** Protect your own cohesion and *deny the
   enemy information* — blind his scouts while keeping your own picture.
9. **Mission command / decentralization.** Subordinates act on intent, not detailed
   orders (inherent to the thread-per-unit architecture — not separately scored).
10. **Economy of force.** Minimum committed to secondary efforts so the main
    effort can be decisive.

## Operationalization — a per-side adherence scorecard

| # | Principle | Metric (per side, 0..1 unless noted) | Adherence is high when |
|---|---|---|---|
| 1 | Tempo / initiative | `init` = mean own front-x normalised into enemy territory (advancing vs pushed) | you sit in *his* half |
| 2 | Focus / Schwerpunkt | `focus` = largest local cluster of own force / total (concentration) | force is concentrated, not smeared |
| 3 | Surfaces & gaps | `gap` = 1 − enemy density at your point of main effort (flank/base) | you strike where he isn't |
| 4 | CoG / critical vuln | `cog` = damage you put on the enemy **base** / total damage you deal | you hit the heart, not just bodies |
| 5 | Surprise / boldness | `surprise` = entropy of your flank/build choices across rounds (match-level) | you're unpredictable |
| 6 | Combined arms | `arms` = normalised entropy of your (H,G,L) mix | genuinely mixed, no mono |
| 7 | Orienting | `orient` = agreement of your build with the counter to the *seen* enemy mix | you answer what's there |
| 8 | Security / counter-recon | `sec` = 1 − enemy's coverage of your force | he can't see you |
| 8b | Reconnaissance | `recon` = your coverage of his force | you can see him |
| 10 | Economy of force | `econ` = share of force at the main effort vs idle/secondary | mass at the point |

Most are computable from telemetry already present (coverage, front, census,
siege) plus a base-damage ledger and a concentration fold.

## Method

1. **Measure** the scorecard for both sides over many rounds (headless). Because
   the two AIs are symmetric, *expect roughly equal scores* — the interesting
   signal is which principles **both sides score LOW on** (a shared blind spot
   the design doesn't support), and whether any score is suspiciously pinned.
2. For each low/shared-gap principle, **hypothesise** a concrete mechanic and a
   **falsification threshold** (e.g., "biasing the garrison to the remembered
   flank raises `gap` by ≥0.1 and lifts the maneuver-win rate; falsify if not").
3. **Falsify** in the harness / headless.
4. **Entropy-deficit check.** If adherence is *high on both sides* and rounds
   still won't resolve (draw-rate high, front pinned), the conclusion is a
   **tic-tac-toe equilibrium**: symmetric optimal-ish play with no high-variance
   payoff to break it. The fix is then a *design* change — add a genuine
   risk/reward avenue (a gamble that wins big or loses big), not more tuning.

## Result

See [result.md](result.md).
