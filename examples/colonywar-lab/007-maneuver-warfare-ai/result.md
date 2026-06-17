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

## Increment 2: hedge under uncertainty + recon that saturates & survives

From a playtest (6e6ab18): recon clumped idle/suicidal on the enemy base; forces
had no memory/anticipation; and both sides built **mono** while blind — "the enemy
is the lack of information, and the counter to that is a balanced force."

- **`planNextFor`**: never mono. A **diversity floor** (`minDiversity`=0.2) keeps
  ≥20% of every type (guaranteeing scouting Hunters); only a **confident** sighting
  (`confidentSightings`=10 enemies seen) sharpens the surplus to a hard counter;
  blind, it just balances the mix.
- **Recon behaviour**: scouts disperse across the enemy half (a different lane per
  unit id — saturate, don't clump), kite what they can, **flee** what they can't
  (self-preservation), and never close onto the base.

### Effect (telemetry)
- Composition is now genuinely **mixed** (e.g. H15/G11/L8 vs H12/G15/L7), no mono.
- Recon **saturates**: `ymax=270` (full height) and **coverage 22–30/34** (up from
  ~10) — the fog is being actively beaten by recon now.

### Still open
- **Rounds still don't end** — the **mirror-flank** persists: both symmetric
  strategists pick the same gap deterministically, so flanks collide.
- **No memory** (point 2): forces don't anticipate a threat from a direction
  enemies were massing.

## Increment 2.5: bug fixes + objective-progress telemetry

Playtest (0810aa6) caught two things — "2 hints for telemetry and its design
process": a recon clumped as a rigid vertical line, and a unit sat *on* the enemy
base dealing no damage.

- **Bug fix — base never razed:** target selection always preferred the nearest
  enemy *soldier*; a unit on the base, with any soldier in *vision* (even out of
  range), targeted that soldier, failed the in-range check, and ignored the base
  under it. Now: hit whatever is in reach; **Flank** units prioritize the base.
- **Recon dispersion:** replaced the rigid per-id *lane* (a vertical line) with a
  golden-ratio 2D scatter over the enemy half — natural saturation.
- **Telemetry — `siege`** = own bodies within a few radii of the enemy base.
  Read against the (already shown) enemy base HP, it makes "units squatting an
  objective without reducing it" visible — which aggregate counts hid entirely.

### What the new instrument revealed
- **`siege=0` throughout** — flankers *never reach* the base; they collide in the
  mirrored flank. So the base never falls and rounds never end. The blocker is
  unambiguously the **mirror-flank symmetry**, not the razing. Process lesson
  saved to memory: instrument *objective accomplishment*, not just existence.

## Increment 3: threat memory — and rounds finally END

Also: rounds now start **from scratch** (no opening platoon) — the opening is a
recon-and-build contest.

Added a per-team **`Threat`** memory (Global `ThreatMem`): decaying enemy weight
on the top/bottom flank, from *that* team's own fog-limited sightings, plus the
flank it has committed its maneuver force to. Each planning pass decays the
memory (`threatDecay`=0.75), folds in this pass's sightings, and `chooseGap`
commits the maneuver to the remembered-emptier flank with hysteresis. Crucially
the memory is **seeded asymmetrically** — Red favours the top flank, Blue the
bottom — so the opening is not a mirror.

### Result — the stalemate is broken
- **Rounds resolve**: 3 rounds in a 180 s headless run (~60 s each), both sides
  winning (2 Blue / 1 Red). The asymmetric flanks no longer collide; a flank
  reaches the lightly-held base (`siege`>0, base HP falls).
- **Real battles, not grinds**: 20–50 kills/round (was 1000+ symmetric).
- **Maneuver beats attrition**: Round 3 Red *won while losing the body-count
  36/48* — it razed the base by maneuver. Exactly the MCDP1 outcome we wanted.

This is the milestone the whole arc was blocked on. The combination that did it:
emergent cycle + press + fix-and-flank task forces + recon saturation + mixed
builds + the base-razing fix + **asymmetric memory-driven flanks**.

### Next
- **Defensive anticipation (point 2, the other half):** the same memory should
  make a colony *screen its own threatened flank* — right now nobody defends, so
  the flank razes a free base (rounds may end too cheaply). Defense makes the
  flank *earn* it → counterplay and drama.
- **Mass-before-commit** for the flank; remove the last gladiatorial nudge.
- A ~100-round batch to certify fairness (Wilson CI), once it feels right.
