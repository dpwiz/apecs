# Exp 002 — Result

**Verdict: stat-only retune cannot produce a *healthy* cycle. The combat model is
winner-take-all (no numerical gradient), so the best a stat triangle can do is a
brittle 1.00/0.00 rock-paper-scissors. A real, dramatic cycle needs behaviours +
positioning + fog — confirming the role-based redesign is necessary, not flavour.**

## Probes (Scout vs Siege, 12v12, 400 reps, muster off; baseline S>C = 0.00)

| probe | change to Scout | S vs C | note |
|---|---|---|---|
| baseline | — | 0.00 | loses everything |
| A | range 12→20 (parity) | **0.00** | range parity *insufficient* |
| A' | range 20, 18 v 12 | 0.00 | even outnumbering doesn't help |
| D | range 20, dmg 7→40, hp 55→130 | **1.00** | out-statting → *total* win |
| D' | same, **6 v 12** (half numbers) | **1.00** | wins at half numbers |

## What this proves

1. **Range parity ≠ counter.** Even at equal range Scout loses, because HP/damage
   attrition (55 vs 170 HP) decides a stand-up blob fight. The dominance is
   multi-factor, not just range.
2. **Combat is winner-take-all.** The moment Scout out-stats Siege it wins
   **1.00 even at half numbers**. There is no 0.6–0.8 band and numbers barely
   move the result — a Lanchester square-law blowout: any per-unit edge
   compounds (fewer units → less return fire → snowball) into a total win.
3. Therefore a **stat-only cycle would be a knife-edge 1.00/0.00 triangle**:
   whoever fields the counter wins outright, numbers irrelevant, decided at
   composition-guess time. Under fog that's a coin-flip on the guess, not a
   contest — the opposite of "opportunities to exploit, dramatic conclusions."

## Decision

The gradient that makes play dramatic (partial outcomes, comebacks, numbers
mattering, execution mattering) **cannot come from stats** in this model. It has
to come from **geometry and behaviour**:

- **kiting** — a fast/long unit that fires while retreating takes outcome-varying
  return fire → gradient from positioning, and a genuine answer to a slow bruiser
  that doesn't depend on out-statting it;
- **screening / lines** — concentration vs spread changes *local* ratios, enabling
  flanks and breaches (partial, geometry-dependent outcomes);
- **fog / recon** — makes the composition guess an information game, not a blind bet.

→ Proceed to the behaviour-based redesign (exp 003: implement kiting and re-score
the Hunter-vs-bruiser edge in the harness with `muster`/behaviour ablations,
looking for a real 0.65–0.80 gradient instead of 1.00/0.00).
