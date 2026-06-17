# Exp 007 — Result (in progress)

## Increment 1: fix-and-flank task forces

Added a `Role` per soldier (assigned at spawn: Hunters → Recon; the rest split
`flankShare`=35% Flank / 65% MainBody). Strategist picks the open `gap` flank
(vertical half with fewer visible enemies). Movement by role:
- **MainBody** fixes the front (press into melee / rally), as before.
- **Flank** ignores the frontal fight and sweeps to `(enemyBaseX, gap*flankY)`
  (flankY=250, clear of the y≈0 grind), then turns in to crash the base.
- **Recon** (Hunters) scouts the flank toward the enemy, kiting/harassing if met.

Harness cycle unaffected (its units are MainBody): edges still 150/0, mirrors ~0.5.

### What changed (telemetry)
- **The fight became 2D**: `ymax` rose from ~30 to **48–179** — units genuinely
  spread into separate task forces and maneuver vertically. Visible task-force
  structure, not one stream.

### What's still broken
- **Rounds still don't end** (bases stay 380). Root cause: both *symmetric*
  strategists default to the **same** (top) flank, so the two flanking forces
  collide in the top lane and form a **second front** rather than reaching the
  undefended bases. Mirror-symmetry resurfaces one level up.
- **bad-fight rate rose to 30–56%**: flankers/recon meet their counters
  mid-maneuver (no mass-before-commit discipline yet).

### Next
- **Increment 2 (recon-pull + security):** choose the gap from where the *enemy*
  is actually weak/absent (not just where its units are), and screen own flanks;
  break the mirror so a flank hits a genuinely undefended base.
- **Increment 3 (mass before commit):** the flank stages until strong, then
  strikes together — a coherent force can overpower the light base defense, and
  it removes the gladiatorial trickle. Likely the key to clean endings.
