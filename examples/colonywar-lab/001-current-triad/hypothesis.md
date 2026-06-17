# Exp 001 — Is the current Warrior/Scout/Siege triad a real counter cycle?

## Hypothesis

The shipped triad claims **Warrior > Scout > Siege > Warrior** (via the `beats`
relation and a 2× damage multiplier). I expect the *as-shipped stats* do **not**
form a real cost-parity cycle — specifically the **Scout > Siege** edge is a lie
in practice, because Siege out-ranges Scout (20 vs 12) and has 3× the HP
(170 vs 55), so it melts Scouts before the 2× bonus pays off.

## Method

Matchup harness (`--matchup`, `muster=False`, full information, synchronous
step-to-elimination). Equal-count 12v12 for all type pairs (+ mirrors as a
sanity check), then break-even sweeps `winner:k vs loser:12`.

`scripts/matchup_grid.sh raw/raw_matchups.txt 300`
`scripts/agg_matchup.py raw/raw_matchups.txt`

## Predictions (what would confirm / falsify)

- **Mirrors ≈ 0.50** (else the harness or spawn placement is biased — kills the whole instrument).
- An edge is a **REAL** counter only if the winner takes ≥0.60 at parity *and*
  break-even ≤ 1.0× numbers.
- **Falsified "the triad is real"** if any intended edge wins <0.60 at parity,
  or needs >1.2× numbers. Prediction: **S>C fails**; W>S and C>W likely hold
  (melee, HP/damage favour them).

## Result

See [result.md](result.md).
