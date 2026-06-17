#!/usr/bin/env bash
# Sweep the unit-type matchup grid via the --matchup harness.
# Usage: matchup_grid.sh [output_file] [reps]
set -euo pipefail

ROOT=$(git rev-parse --show-toplevel)
BIN=$(find "$ROOT/.stack-work" -type f -name stm-colony-war | head -1)
OUT=${1:-raw_matchups.txt}
REPS=${2:-300}
: > "$OUT"

TYPES=(warrior scout siege)

echo "# equal-count matrix (12 vs 12, reps=$REPS)" | tee -a "$OUT"
for a in "${TYPES[@]}"; do
  for b in "${TYPES[@]}"; do
    "$BIN" --matchup "$a:12" "$b:12" "$REPS" | tee -a "$OUT"
  done
done

echo "# break-even sweeps: winner:k vs loser:12 (reps=200)" | tee -a "$OUT"
# pairs are (intended winner, loser) per the triad: W>S, S>C, C>W
for pair in "warrior scout" "scout siege" "siege warrior"; do
  # shellcheck disable=SC2086
  set -- $pair; a=$1; b=$2
  for k in 6 8 10 12 14 16 18 20 24; do
    "$BIN" --matchup "$a:$k" "$b:12" 200 | tee -a "$OUT"
  done
done

echo "done -> $OUT"
