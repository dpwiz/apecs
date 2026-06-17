# Exp 007 — Maneuver-warfare AI players (MCDP1 Warfighting)

## Problem (from playtest of d1ec336)

The AI fights pure **attrition**: two undifferentiated streams gush into one
frontal blob at x≈0. Fog is now real (most of the map dark) but the strategists
don't *value* information — no flanking, no scouting for advantage, trivially
backstab-able (the enemy base sits open on 3 dark sides). And a leftover
"gladiatorial nudge" makes units engage without massing for decisive advantage.

## MCDP1 → mechanics

| Doctrine | Current (broken) | Mechanic to add |
|---|---|---|
| **Critical vulnerability / center of gravity** | everyone grinds the front | main *effort* = strike the enemy **base** through its undefended dark flanks |
| **Surfaces and gaps** | march straight into the enemy (surface) | a **flank** task force avoids the main body, routes through the empty vertical **gap** |
| **Combined arms / task org** | one stream | split into **task forces**: a *fixing* force, a *flanking* force, a *recon screen* |
| **Reconnaissance pull** | base+units see only the center lane | **Hunters scout** the flanks → pick the gap; force flows to it |
| **Security / counter-recon** | flanks/rear wide open | recon **screens own flanks**; detect+answer an enemy flank (don't get backstabbed) |
| **Schwerpunkt / mass** | trickle/gladiatorial rush | **mass before commit**: the flank stages to a threshold, then strikes together |
| **Tempo, mission command** | central waypoint | decentralized: each unit acts on its **role + objective** (fits the thread-per-unit design) |

This also fixes round-endings *non-gladiatorially*: rounds resolve when a flank
reaches the lightly-defended base, not when the frontal grind happens to break.

## Increments (build + playtest each)

1. **Fix-and-flank task forces.** Assign each soldier a **role** at spawn
   (Recon = Hunters; the rest split Main / Flank). Strategist picks the **gap**
   flank (fewer visible enemies) and publishes per-role objectives. Flankers
   sweep wide (y ≈ ±flankY, clear of the y≈0 grind) to the enemy base and ignore
   surfaces; Main body fixes the front; Recon spreads to scout + screen.
2. **Recon-pull + security.** Hunters actively cover the map and own flanks;
   the gap choice follows what recon sees; a detected enemy flank pulls a response.
3. **Mass before commit (remove the gladiatorial nudge).** Flank stages until
   strong enough; Main body holds/fixes rather than charging to trade.

## Metrics (from exp 006 + new)

- **Front travel / base HP**: rounds should now resolve via base damage (flank
  reaching the base), not perpetual center grind.
- **Coverage**: recon should raise own coverage and the flank should arrive with
  the enemy's coverage of it **low** (it came through a gap).
- **bad / out** stay low (no suicidal frontal feeding); **task-force separation**:
  units cluster into ≥2 groups, not one blob (measurable as spatial spread).

## Result

See [result.md](result.md).
