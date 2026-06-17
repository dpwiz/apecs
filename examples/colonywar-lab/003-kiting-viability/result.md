# Exp 003 — Result

**Verdict: YES, kiting is viable — behaviour creates a counter that stats
provably cannot. Duels stay winner-take-all even with kiting/cornering, but that
is fine: it makes the meta a *decisive* rock-paper-scissors, and the round-level
drama comes from fog + macro (which the baseline already passes).**

## Kiting flips a loss to a win, from the flag alone

Hunter (Scout slot) vs Lance (Siege slot, range 13), 12v12, 400 reps, stats
identical between OFF and ON — only `cKite` toggled:

| Hunter range | kite OFF | kite ON | note |
|---|---|---|---|
| 14 | 0.00 (0/400) | 0.01 (3/397) | standoff 0.92×14=12.9 < 13 → kite holds *inside* Lance reach, fails |
| **15** | **0.00 (0/400)** | **1.00 (400/0)** | clean flip: behaviour alone makes the counter |
| 16 | 0.00 (0/400) | 1.00 (400/0) | same |

At range 15 the stand-up is a *guaranteed loss*; turning on kiting makes it a
*guaranteed win* with no stat change. **Behaviour creates the counter** — exactly
what exp 002 proved stats could not. There's a sharp threshold (range 14 fails)
where the standoff distance crosses the bruiser's reach.

## But duels stay winner-take-all (no gradient)

```
Hunter(r16) vs Lance(r13), kite ON, 12v12:  unbounded 1.00 | arena160 1.00
arena160, kite ON, Hunter:k vs Lance:12:  k=8..18 all 1.00
```

Cornering (clamp to ±160) and outnumbering the kiter both leave it at 1.00,
because an out-ranging unit out-trades even while stationary (range dominance,
exp 001) — the slow bruiser dies crossing the last few px under massed fire
before it can punish the corner. Lanchester combat compounds any edge to a total
win; behaviour changes *who* has the edge, not the all-or-nothing of duels.

## Reframe: winner-take-all duels are the right design

A decisive hard-counter meta (StarCraft-style) is good, *provided*:
1. the composition graph is a real **cycle** (no dominant unit → no fixpoint), and
2. round drama comes from the **information + macro** game, not 50/50 duels.

(2) is already true: the committed baseline scores 48% nail-biters and ~4
lead-flips/round — that drama is fog + reinforcement + positioning, not duel
coin-flips. So winner-take-all duels are acceptable; what matters is building the
cycle.

## Decision → next

A is viable. Proceed to **build the full behaviour cycle** and prove it's a cycle
in the harness:
- **Hunter > Lance** — kiting ✅ (this exp).
- **Lance > Guard** — concentration/breach (rally-wave exists; verify it beats a
  screen at parity).
- **Guard > Hunter** — screening: a wide line denies kiting room (needs a
  screen/spread behaviour; the missing piece).
Then check **no fixpoint** (matrix has no dominant row) and that the full game
stays fair + dramatic.

## Kept in code (gated off)

`cKite` + kiting logic and `cArena` are committed but **off in both game configs**
(`cKite=False`, `cArena=0`), so the shipped game is byte-for-byte unchanged
(verified: mirror 105/95, headless round still resolves). They are the
foundation for the redesign.
