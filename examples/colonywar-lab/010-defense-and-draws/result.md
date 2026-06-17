# Exp 010 — Working defense, draws, and the cohesion/maneuver untangle

Playtest of c32f161 surfaced four coupled issues; fixing them revealed the
coupling.

## Fixes
1. **Idle defenders ("play the field, not the rules").** Garrison ringed at
   `baseRadius*5`=110px but had only 75–95 vision, so an attacker closing *onto*
   the base passed out of sight and the defenders held position, blind, as it was
   razed. Tightened the ring to `baseRadius*2.5`=55px (inside vision) → defenders
   see and answer the attacker.
2. **De-facto draws.** Two strike groups would trade spawners moments apart, and
   whoever landed first "won". Now losing a spawner starts a **grace countdown**
   (`cGrace`=2.5 s); if the other falls within it, the round is a legitimate
   **Draw** (`Phase=RoundDraw`, scored as no win for either). No pause before
   scoring, so the second strike still counts.
3. **Front froze** once cohesion (exp 009) met the enemy: two cohered blobs
   bounced off each other. Fix: a unit **in reach of a target releases cohesion**
   and presses in.
4. **Flank absorbed.** Cohesion pulled flankers back into the main-body blob, so
   the flank never reached the base (`siege`=0 → stalemate). Fix: **only the main
   body coheres**; the flank peels off free.

## The coupling
Fixing the idle defenders *removed the round-resolution path* — rounds had been
ending because strike groups slipped past *blind* defenders. With defenders that
work, resolution needs the flank to genuinely break through. Main-body-only
cohesion (front fights as a group, flank maneuvers free) + a thin garrison
(`defendShare` 0.2→0.1) restores it: the flank fights *through* the garrison.

## Result
3 rounds resolved in 180 s (both sides win); `siege` up to 11 and **min base HP
47** — the flank earns the base through the defenders, no instakill; maneuver
wins still happen (a side winning while losing the body count). Draws now
possible. Harness cycle intact.

## Still open (from the same playtest)
- **Recon** still scatters as isolated lone scouts (vulnerable, patchy islands).
- **Spawn legibility**: burst/trickle/nothing is the cap+death-rate dynamic but
  reads as opaque; worth surfacing or regularising.
