# Exp 001 — Result

**Verdict: the current triad is NOT a counter cycle. Siege is strictly dominant
(a fixpoint). Scout is strictly dominated. Hypothesis confirmed and then some.**

## Equal-count matrix (red win-rate, 12 vs 12, 400 reps, muster off)

```
        W       S       C
 W    0.49    1.00    0.00
 S    0.00    0.46    0.00
 C    1.00    1.00    0.51
```

Mirrors (diagonal) are 0.49 / 0.46 / 0.51 → instrument validated (after fixing a
fixed-step-order first-strike bias that made mirrors 1.00/0.75; now the order is
reshuffled each tick).

## Counter edges at parity

| claimed | actual win-rate | verdict |
|---|---|---|
| W > S | 1.00 (CI 0.98–1.00) | holds, but *total* |
| S > C | **0.00** (CI 0.00–0.02) | **BROKEN — reversed** |
| C > W | 1.00 (CI 0.98–1.00) | holds, but *total* |

## Break-even (winner:k vs loser:12)

- **W > S**: Warrior wins even at **6 v 12** (0.5×). Dominant.
- **S > C**: Scout loses at **24 v 12** (2×). Never wins — the counter is fictional.
- **C > W**: Siege wins even at **6 v 12** (0.5×). Dominant.

## What this means

1. **It's a fixpoint, not a cycle.** Real dominance order is **Siege > Warrior >
   Scout**, and Siege *also* beats Scout. Nothing beats Siege → the strategist
   correctly converges everyone to Siege, and "no fixpoint" fails hard. This is
   the root cause of the monotonous full-game endgames.
2. **Scout is combat-dead.** It loses to both other types at any tested ratio; it
   only earns its keep as recon (vision), which combat tests don't reward. This
   is the "self-defeating Scout trickle" the player saw — mathematically the
   Scout *cannot* win that fight.
3. **Combat is hyper-decisive (1.00/0.00, no middle).** Whoever holds the
   advantage wins ~100% regardless of numbers, because attacks are deterministic
   hitscan and **range out-trades everything** (out-ranged units die before they
   fire). Numbers barely matter → little room for drama or comeback.

## Implications for the redesign (→ exp 002)

The target matrix for a *real* cycle:
- mirrors ≈ 0.50 (keep),
- each intended counter wins at **~0.65–0.80 at parity** (decisive but not total),
- break-even near **1.0–1.3×** (so a numerical underdog with the right counter can
  still contest → drama, comebacks).

Since range out-trades within the current "march to range + hitscan" model,
plain stat edits almost certainly can't make the fast unit beat the artillery —
that's the case for **behaviours** (kiting to deny the range gap, screening to
deny kiting space). Exp 002 will test exactly that, lowest-churn first.
