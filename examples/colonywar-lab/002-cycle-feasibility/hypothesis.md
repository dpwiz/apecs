# Exp 002 — Does a real cycle need new behaviours, or just stat retune?

## Question

Exp 001 showed Scout cannot beat Siege at any number ratio. The proximate cause
is **range**: Siege (20) out-ranges Scout (12), so Scouts die crossing the gap
before they fire. Two ways out:

- **Stat fix** — shrink the range gap (give the fast counter range parity), no
  new code.
- **Behaviour fix** — keep the range gap but let the fast unit *kite* (fire while
  retreating) so it fights at its own range. New AI.

## Probes (S vs C, 12v12, 400 reps, muster off)

Temporarily edit one Scout stat, rebuild, measure `scout:12 vs siege:12`.
Baseline S>C win-rate = 0.00.

| probe | change | S vs C win-rate |
|---|---|---|
| baseline | — | 0.00 |
| A | Scout range 12→20 (parity) | (see result) |
| B | Scout range 12→16 | |
| C | Scout speed 120→240 only | |

## Falsification / decision

- If a **stat-only** probe lifts S>C into the target 0.65–0.80 band → the cycle
  is reachable by retuning; **no kiting needed** (much less churn).
- If even range parity barely moves it → range isn't sufficient; **behaviours
  required**.

## Result

See [result.md](result.md).
