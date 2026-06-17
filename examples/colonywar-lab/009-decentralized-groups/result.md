# Exp 009 — Decentralized command: local groups (cohesion)

## The user's diagnosis (correct)

Trickle should never be a viable tactic, yet it was. Why? Because **command is
centralized**: one strategist per team writes one global plan (next-type,
waypoint, gap) and every unit slavishly reads that same cell. The thread-per-unit
design buys *parallelism*, not *decentralized decision-making* — one unitary will
trying to be everywhere, so it **smears**: units stream individually toward a
single objective and arrive piecemeal. MCDP1 calls for **mission command** —
local initiative groups that cohere, mass, and exploit local opportunity within
the commander's intent. The fields never formed them because nothing made units
**cohere** (separation pushes apart; nothing pulled into groups).

This is the exp 008 reading too: `focus` ≈ 30–55% *is* the smear.

## Increment 1: cohesion (form the groups)

Maneuver/fighting units (MainBody, Flank) now pull toward friendly soldiers in
the band beyond shoving range out to `cohesionRadius`=80 (`cohesionPull`=0.5/tick,
below a step so the objective still leads). Recon and the garrison are exempt —
they're meant to spread. Added a `groupCount` metric (single-linkage components)
and `grp`/`focus` to the scorecard.

### A/B (flankShare=0.45, headless)

| | cohesion OFF | cohesion ON |
|---|---|---|
| `focus` (concentration) | 41 | **64** (+23) |
| rounds in 120 s | **0 — stalemate** | **2 — resolved** |

Cohesion makes units form **dense local groups instead of a thin smear**, and —
the payoff — the *committed flank coheres into a fist and breaks through*: rounds
**resolve** where the smear **stalemated**. Trickle stops being viable because
units now arrive together. Harness cycle unaffected (its MainBody units cohere
but the edges hold: hunter>lance 100/0, mirrors ~0.5).

(`grp` barely moves, 4.2→4.8: the connected-components radius lumps a smear and
tight groups alike; `focus` is the better smear-vs-cohere readout.)

## Next — local *initiative* (the other half of mission command)

Cohesion gives the **groups**; it does not yet give them **initiative**. Today a
group still moves toward the *global* objective. Mission command wants a group to
read its *local* situation and seize a local opportunity — a gap, an exposed
flank, an isolated enemy — on its own, within the commander's intent (the gap /
schwerpunkt the strategist publishes). That is the decentralized-decision step,
and the scorecard (`init`, `cog`, `focus`, win-variance) is the instrument to
test whether it raises tempo and lands the decisive blow.
