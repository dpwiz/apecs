# Exp 008 — Result

## The scorecard (per side, averaged over a round)

Added `[score]` to the headless telemetry: `recon`, `sec`, `arms`, `init`,
`focus`, `cog` (see [principles.md](principles.md) for definitions), plus a
base-damage ledger (`BaseDamage`) for `cog`. Both sides are symmetric, so the
signal is the **shared** level of each principle.

| Principle | metric | reading | verdict |
|---|---|---|---|
| Combined arms | `arms` | **88–99%** | ✅ excellent (diversity floor) |
| Security | `sec` | 60–88% | ✅ good (fog hides you) |
| Reconnaissance | `recon` | 15–40% | ⚠️ partial |
| Focus / Schwerpunkt | `focus` | 30–55% | ⚠️ middling |
| **Tempo / initiative** | `init` | **9–28%** | ❌ **low both sides** |
| **CoG / critical vuln** | `cog` | **0% all round** | ❌ **the blow never lands** |

**Diagnosis = the user's prediction, measured.** The two symmetric AIs sit back
(low `init`) and the center of gravity is *never* struck (`cog`=0): a passive,
drawn equilibrium — the game had **tic-tac-toe'd**. The garrison (exp 007.4)
stopped the *under-committed* flank entirely, trading the old free instakill for
a stalemate where no decisive blow lands.

## Hypothesis & falsification

**H:** the blow doesn't land because the main effort is under-committed — low
adherence to **Schwerpunkt**. Concentrating more force into the flank should
raise `cog` and resolve rounds. *Falsify if* `cog` stays ~0 / rounds still don't
resolve.

**Test** (`flankShare` 0.3 → 0.55, headless): `cog` rose **0 → 7%** (the flank now
lands a contested, *earned* strike — not an instakill, the garrison still mostly
holds) and **rounds resolve** (2 in 150s). At 0.45 rounds also resolve and stay
fair (1 Red / 1 Blue), though more via attrition (`cog`~0 = army wiped then a fast
raze). **H supported**: `flankShare` is the dial between attrition (low → the
tic-tac-toe stalemate) and decisive maneuver (high → the CoG strike lands), and
`cog` is its readout. Committed at **0.45** (resolves, fair).

## The deeper read (entropy / risk-reward)

Bumping `flankShare` resolves the indefinite fighting, but it is *symmetric*
commitment — both sides simply commit more. It removes the stalemate without
adding the **high-variance, high-reward** play the user is pointing at. The
genuine "risky avenue" is to make the commitment a **gamble**: a colony that
*concentrates a decisive flank wave* wins big if it lands on the CoG, but pays
with a **weakened front** if the enemy guessed the flank — outcome variance from
the fog-driven mismatch of attack vs defended flank. That is the design lever for
dramatic, asymmetric conclusions, and the scorecard (`init`, `cog`, `focus`) is
now the instrument to tell whether such a mechanic actually raises adherence.

### Next
- Make the flank a **strategic gamble** (concentrate a wave on the chosen flank;
  the front bears the risk) and measure `init`/`cog`/win-variance against this
  baseline.
- **Memory-driven defense** (bias the garrison to the remembered-threatened
  flank) so a *read* enemy gamble is punished — closing the risk/reward loop.
