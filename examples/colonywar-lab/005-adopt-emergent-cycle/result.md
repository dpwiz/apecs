# Exp 005 — Adopt the emergent cycle in the live game

**Status: PARTIAL. The emergent cycle is adopted and the no-fixpoint matrix is
proven under live combat settings. But the full base-razing game STALEMATES —
rounds never end — so the redesign is not yet playable. Diagnosis below; needs a
design decision on how rounds should resolve.**

## What was done

Renamed Warrior/Scout/Siege → **Guard/Hunter/Lance**; set the proven archetype
stats; **removed the rock-paper-scissors damage multiplier** (`cTriad=False` live)
so the counter cycle is purely emergent; **enabled kiting** (`cKite=True` live);
removed the dead screen behaviour. Strategist's counter-picker reused unchanged
(the `beats` relation is identical under the rename). Live combat = kiting on,
flat damage. Harness defaults flipped to match (kite on, triad off).

## Win: the no-fixpoint matrix holds under live settings

3×3, 300 reps, `kite=True triad=False muster=False` (live combat model):

```
            vs Hunter   vs Guard   vs Lance
Hunter        .52         .00        1.00
Guard        1.00         .50        .00
Lance         .00        1.00        .48
```

Every type beats exactly one, loses to one; mirrors ~0.5. No dominant row →
**no fixpoint**. This is the design goal, achieved, with no `beats` damage table.

## Problem: the full game stalemates (0 rounds in 120s, bases stay full)

Headless heartbeat timeline shows two compounding failures:

1. **Static firing lines.** Hunter & Guard both have range 18. A unit with a
   target in range holds and fires (never closes). With flat damage + high HP,
   opposing lines sit at 18px trading glacially while reinforcements top both
   sides up symmetrically — a perfect stalemate line. (The old game's shorter
   ranges + the 2× triad burst made lines *break* into snowballs/breakthroughs.)
2. **Composition counter-chase.** Each strategist flips the *whole* army to
   counter the enemy's current comp (Guard→Lance→Hunter→…). Rebuild lag means by
   the time you've countered, they've flipped — round-robin forever, no stable
   type advantage long enough to convert into a breakthrough to the base.

Net: damage accrues into the hundreds of thousands, neither base is ever touched.

A naive fix (uniform 2× damage to restore fast kills) **failed twice**: it did
not end rounds (the static line persisted), and it **broke the L>G edge**
(Lance>Guard → Guard 127/73) because doubling damage also doubled the Guard's
range-chip during the Lance's approach — the very margin L>G depends on. So the
cycle is *tuning-fragile* and round-ending needs more than a damage knob.

## Three targeted fixes tried — none end rounds (root cause is structural)

| fix | intent | cycle? | rounds end? |
|---|---|---|---|
| **press into melee** (supported units close, don't hold at range) | break static range-18 lines | ✅ holds, and makes **L>G robust** (200/0 — the range-chip that made it fragile is gone) | ❌ no |
| **2× damage** (with press) | faster kills → routs → breakthroughs | ✅ holds (uniform scaling is now L>G-safe with press) | ❌ no |
| **cap 34→16** | comp advantage wipes the *whole* enemy army → clear field to march | n/a | ❌ no |

In every case the bases stay pinned at full 380 — **no unit ever reaches a base.**
The heartbeat shows why: two equally-competent strategists counter-chase
(R: Lance→Guard→Hunter, B chasing the counter), continuous respawn keeps both
armies at full cap, and the front never clears. A unit only marches on the enemy
base when it sees *no* enemy (clear-field waypoint), which never happens. The
stalemate is a **perpetual-respawn symmetric equilibrium**, immune to combat
decisiveness or army size. The old game escaped it only because the 2× triad
burst occasionally landed a comp-counter hard enough to rout-and-march before the
re-counter arrived; the smoother emergent cycle removes that.

**Kept:** the press behaviour (a genuine improvement — fixes static lines, makes
L>G robust, cycle-safe). Reverted: the 2× damage and cap experiments (didn't
help; left at cycle baseline 8/9/22, cap 34).

## Decision needed (design fork)

The root cause is the win model, not the numbers. Options:

- **A. Exhaustible reinforcements / attrition economy (recommended).** Each base
  gets a finite reinforcement pool per round; when spent, no respawn → attrition
  forces a winner, whose survivors then march and raze. Kills the perpetual-
  respawn equilibrium directly, adds husbanding strategy, fits the "spawn-heart"
  theme. Guarantees endings.
- **B. Round timer + tiebreak.** Hard per-round time cap; if no base falls, award
  the win on base HP / kills / proximity-to-enemy-base. Simple, guaranteed
  endings, but the climax becomes a clock rather than a breakthrough.
- **C. De-symmetrise the counter-chase.** Make strategists committal / imperfect
  so a comp advantage persists long enough to convert (with press + decisive
  combat). Preserves unlimited respawn, but risks re-introducing imbalance and
  may still stalemate.

Two broad directions to make rounds resolve dramatically:

- **A. Restore decisiveness** so breakthroughs raze bases as before: per-type TTK
  tuning (not uniform), shorter effective firing-line behaviour (close-to-melee
  bias), lower base HP / cap, dampen the strategist's oscillation (mixed armies /
  hysteresis). Keeps base-razing as the climax; more tuning, cycle stays fragile.
- **B. Change the end condition** so a round resolves without a full breakthrough:
  a round timer with a tiebreak (base HP / kills / territory), or first-to-N
  kills, or an attrition/economy clock. Guarantees endings; changes the feel.

Cycle + rename committed as a WIP checkpoint (game currently stalemates — branch
work in progress, not playable yet).
