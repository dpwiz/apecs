# Exp 004 — Does a screening behaviour let a Guard beat a Hunter (Guard > Hunter)?

## Question

This is the make-or-break edge of the behaviour cycle (Lance > Guard > Hunter >
Lance). Exp 003 proved a Hunter kites a slow bruiser to death — **and** that even
*cornering* the kiter in the arena doesn't help a slow tanky clump (Lance:8..18
all lose 1.00). The diagnosis: a single clustered blob doesn't corner a kiter in
**2D** — the kiter slides *along* the arena wall (vertically), orbiting the slow
clump, never leaving its range.

So the screen must do something specific: **spread into a wide line spanning the
arena perpendicular to the approach axis**, so a kiter sliding along the wall
meets a Guard at every height, runs out of lateral escape, and is forced into a
stand-up brawl — which the tanky Guard wins.

## Method

Add `cScreen` ablation flag + a *lateral spread* behaviour to `stepUnit`: a
screening unit (Warrior = Guard archetype proxy) repels nearby allied screeners
along the axis **perpendicular to its direction-to-enemy**, fanning a clump into
a tall wall that spans the arena, while still advancing to close. Hunters (Scout
proxy, kiting from exp 003) are unaffected (`isScreener` gates on type).

Temp stats: Scout range 12→16 (so the Hunter actually out-ranges the Guard and
can kite it; everything else shipped). Guard = Warrior shipped (hp100, dmg12,
range14) — it crushes a Hunter (hp55, dmg7) in any forced brawl, so the *only*
question under test is whether the screen forces the brawl.

Ablation ladder, `warrior:12` (Guard) vs `scout:12` (Hunter), 400 reps:

| run | flags | predict |
|---|---|---|
| stand-up | (none) | Guard ~1.00 (Hunter loses a brawl) |
| open kite | `--kite` | Hunter ~1.00 (kites in open field) |
| cornered clump | `--kite --arena` | Hunter wins (orbits the clump, per exp 003) |
| **screen** | `--kite --screen --arena` | **Guard > 0.60 (the flip under test)** |
| screen, no walls | `--kite --screen` | Hunter wins (open field: can't pin a faster unit) |

## Predictions / falsification

- **Screen viable** if `--kite --screen --arena` flips the cornered-clump loss
  into a Guard win (> 0.60), from the screen flag alone.
- Should **not** need numbers: Guard:12 vs Hunter:12. Falsify the edge if it
  needs > 1.2× Guards (sweep guard:k vs hunter:12).
- Honest limit: I expect screen to **need the arena** (a faster unit can't be
  pinned in open field). The real game field is bounded, so that's acceptable —
  but I'll measure `--kite --screen` (no arena) to state it plainly.

## Result

See [result.md](result.md).
