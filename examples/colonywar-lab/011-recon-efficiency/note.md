# Exp 011 — The recon swarm that covered one cone (a blind-metric blunder)

## Symptom

With honest-fog recon-pull working (blind hold → outward explore → contact → the
force is pulled in), a playtest showed **lots of recon units wasted covering the
same cone with overlapping vision**, leaving the rest of the field dark.

## What the logs said

~22 Hunters per side (vision 170 each), yet enemy **coverage** sat at a median of
**13/64**, frequently **1–5/64**. Twenty-two scouts delivering the coverage of
~4 well-placed ones — they were stacked in one cone, all seeing the *same* handful
of enemies.

## Why it got away (the real lesson — a telemetry blunder, not just an AI bug)

The `recon` metric is `seen / total` enemies — a pure **outcome**. Two scouts
stacked on the same spot both "see" the same enemies, so coverage doesn't rise,
yet **the metric never flags the wasted second unit**. An outcome metric with no
**efficiency / overlap** term is *satisfied by piling*: it cannot tell 4 good
scouts from 22 redundant ones, so nothing in the instrumentation ever objected
while the AI wasted a whole swarm on one cone.

**The class of blunder:** *an outcome metric with no cost/distribution term
rewards the cheapest degenerate way to hit the outcome, and hides the waste.*
Coverage without efficiency → piling. (Same shape as: win-rate without margin
hides blowouts; kills without objective-damage hides squatting — see
[007](../007-maneuver-warfare-ai/) `siege`, [008](../008-warfighting-adherence/).)

## The behavioural cause

The contact-phase recon net blended `bear*0.6 + udir*0.4` — the 0.6 weight on the
*shared* contact bearing collapsed every scout into one cone; the 0.4 per-unit
spread was too weak to disperse them.

## The fix — add the missing instrument first

`reconOverlap` / **`ovl`** in the `[score]` line: the mean number of *other*
friendly scouts whose vision centre lies within a scout's own vision radius
(heavy-overlap degree). ~0 = vision disks tile fresh ground (each scout earns its
keep); high = scouts stacked, re-covering one cone.

**Validated against the known failure** (the discipline: an instrument you can't
see fail on a *known* bug you don't trust): on the piled swarm, `ovl` reads a
median of **3.4** (max **20**), and it is **anti-correlated with `recon`** — when
`recon`≈0%, `ovl`≈3.8–5.5; when `recon` climbs to ~9%, `ovl` falls to ~1.9. The
fingerprint *low recon + high ovl = a swarm in one cone* is now legible.

## Rule of thumb (to avoid the class)

- Never ship an **outcome** metric (coverage, kills, win-rate) without its paired
  **efficiency / distribution** metric (overlap, margin, objective-share).
- The degenerate optimum of a one-sided metric is the bug you'll ship blind.
- **Validate the new instrument on the known failure** before trusting it on the
  unknown ones — confirm it actually moves when the bug is present.

## The degenerate-satisfier check (run on EVERY metric)

> For each metric ask: **what is the cheapest or most degenerate way to max it
> *without* achieving the goal it stands for?** If such a path exists, the metric
> is one-sided — pair it with a cost / magnitude / distribution term that the
> degenerate path fails.

Applying it to the existing `[score]`/`[hb]` scorecard found the same class
several times over:

| Metric | Degenerate satisfier | Status |
|---|---|---|
| `recon` = seen/total | scouts **piled in one cone** see the same enemies | ✅ paired with **`ovl`** |
| `sec` = unseen fraction | **annihilation** = 0/0 = "perfectly hidden"; losing the visible front *raises* it (survivors are the hidden ones) | ✅ fixed → **absolute hidden count / `secRef`**, so no force reads 0 and dying doesn't spike it |
| `cog` = base-dmg / total-dmg | **one scratch** on the base = share 1.0 = "decisive" while the base sits at 379/380 | ✅ fixed → share **× base-worth landed** (`min 1 (baseDmg/baseHp)`); a scratch reads ~0 |
| `focus` = biggest cluster / total | a **scared huddle at home** scores 100 (concentration magnitude, not *at the decisive point*) | ⏳ deferred — needs cluster-centroid vs objective |
| `arms` = global (H,G,L) entropy | three **separated mono-groups** (H scout / L siege / G home) read 100 — "combined arms" with zero combining | ⏳ deferred — needs local/per-group mix |
| `init` = mean front-x | **mean hides distribution** (half-dead-forward + half-cowering reads as a tidy midfield); rewards overextension | ⏳ deferred — partly paired by `bad`/`out`; consider "fraction past midfield" |
| `out` = locally outnumbered | **passivity** wins it (hang back at home → never outnumbered) | ⏳ deferred — read against `init`/aggression |
| `grp` = single-linkage components | a connected **smear** and a tight **fist** both read as 1 (resolution flaw, not degeneracy) | ⏳ superseded by `focus`/`ovl` |

## Applied this pass

- **`ovl`** added (recon redundancy), validated against the known pile.
- **`sec`** = unseen-fraction × presence, `presence = min 1 (force/secRef)`
  (secRef=12): a wiped/vanishing army reads ~0 instead of "0/0 = perfectly
  hidden", while a real force is still scored on denial quality. Validated: reads
  8→100, ~0 at annihilation; the high values are faithful (in honest-fog the
  enemy sees only 5–9% of you, so ~92% really is hidden) — not a ceiling artifact.
- **`cog`** = share × `min 1 (baseDmg/baseHp)`: a scratch no longer reads decisive.

## Next

- Disperse the contact-phase net (raise per-scout spread / penalise overlap in the
  target assignment) and watch `ovl` fall while `recon` rises.
- Work the deferred metrics (`focus` location, `arms` locality, `init`
  distribution) when their structural terms are cheap to compute.
