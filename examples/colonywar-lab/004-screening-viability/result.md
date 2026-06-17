# Exp 004 — Result

**Verdict: the screen behaviour FAILED (a clean negative — it changes nothing),
but failing it found the real structure: the counter to a kiter is to *match its
range* (a stat relationship, not a behaviour), and on that basis a genuine
3-cycle with NO fixpoint exists from a *single* behaviour (kiting) + range/speed/HP
geometry — with the rock-paper-scissors damage table turned OFF. The RPS emerges
from physics; the `beats` lookup table is unnecessary.**

## 1. The screen does nothing (negative)

Guard(warrior) vs Hunter(scout, range16), 12v12, 400 reps, **triad off** so only
behaviour can decide (`raw/ladder_notriad.txt`):

| run | flags | result | predicted |
|---|---|---|---|
| stand-up | — | Guard 400/0 | Guard (brawl) ✓ |
| open kite | `--kite` | **Hunter 400/0** | Hunter ✓ |
| cornered clump | `--kite --arena` | Hunter 399/1 | Hunter (orbit) ✓ |
| **screen** | `--kite --screen --arena` | **Hunter 400/0** | hoped Guard >0.6 ✗ |
| screen no walls | `--kite --screen` | Hunter 400/0 | Hunter ✓ |

*I wanted the lateral-spread screen to pin the kiter and force a brawl; I hoped a
spanning wall would deny lateral escape; the data shows it makes zero difference
(0/400 with screen vs 1/400 without).* A spaced picket has range gaps a fast
point-unit threads, and — decisively — even a perfect pin can't help a Guard that
is **out-ranged**: the Hunter sits at its range edge, still outside the Guard's
shorter reach, and out-trades. A behaviour cannot manufacture range.

## 2. The real kite counter: match the range (stat, `raw/` range16 probe)

Give the Guard range 18 = the Hunter's, no screen, triad off:

```
stand-up           Guard 400/0
open kite           Guard 400/0   <- kiting now yields NOTHING
open kite + arena   Guard 400/0
```

Kiting only works when you **out-range**; backing off to your range edge only
buys free shots if the target can't reach you there. Range-match closes the gap →
the squishy Hunter loses the even trade. **The counter to kiting is range, not a
behaviour.** (Consistent with exp 002: counters in this model are stat/geometry,
not conjurable from behaviour alone.)

## 3. A real 3-cycle from kiting alone, triad OFF (`raw/cycle_final.txt`)

Archetypes (Warrior=Guard, Scout=Hunter, Siege=Lance):

| | range | speed | hp | dmg | idea |
|---|---|---|---|---|---|
| **Hunter** | 18 | 130 | 55 | 8 | fast, long, frail — kites slow short-ranged prey |
| **Guard** | 18 | 50 | 110 | 9 | range-matches Hunter (kite-proof), too SLOW to kite, mid brawl |
| **Lance** | 12 | 70 | 170 | 22 | short range, tanky, hard-hitting brawler |

12v12, 400 reps, `--kite --notriad`, **each edge run both side-orders**:

```
H>L  scout vs siege   400/0   (siege vs scout 0/400)
G>H  warrior vs scout 400/0   (scout vs warrior 0/400)
L>G  siege vs warrior 400/0   (warrior vs siege 0/400)
mirrors  W 108/92   S 108/92   C 103/97   (~0.5: instrument clean)
```

Why it closes:
- **H > L**: Hunter out-ranges (18>12) AND out-runs (130>70) → kites → Lance never
  lands a blow.
- **G > H**: equal range (18=18) → no kite possible → stand-up brawl → Guard
  (110×9) out-HP/DPSes Hunter (55×8).
- **L > G**: Guard out-ranges Lance (18>12) but is **slower** (50<70) so cannot
  kite it → brawl. Guard's longer range chips the Lance on the approach, but the
  Lance's brawl mass (170×22) overwhelms the deliberately-weak Guard (110×9).

The Guard's damage (9) was tuned down from 14 specifically so it loses L>G while
still crushing G>H — that was the one knob: at dmg 14 the range-chip flipped L>G
to Guard 254/146; at dmg 9 it is Lance 400/0 and G>H stays 400/0. (`raw/cycle_kiteonly.txt`,
`raw/cycle_guard9.txt`.)

## 4. What this means

- **The behaviour zoo is unnecessary.** One behaviour (kiting) + a range/speed/HP
  triangle yields a true non-fixpoint cycle. Screen/mass are not needed.
- **The `beats`/`advantage` triad table can be removed.** The RPS *emerges* from
  geometry — a far better showcase ("rock-paper-scissors with no RPS table").
- Edges stay winner-take-all (400/0) as exp 002 predicted — accepted: decisive
  RPS, round drama from fog + macro.
- Caveat: proven in the harness (full info, no fog, no muster, no base). Adopting
  it means re-statting the live units, ripping out the triad multiplier, enabling
  kiting, and **rewriting the strategist** (which currently counters via `beats`)
  to counter via the emergent cycle — then re-validating full-game fairness+drama.

## Decision → next (NEEDS USER SIGN-OFF)

This redesigns the live game's core combat + strategy. Recommend adopting the
kite-only emergent cycle. Pending the go-ahead, the live game is left byte-for-byte
unchanged (temp archetype stats reverted; `cKite`/`cScreen`/`cTriad` all gated off
in `displayCfg`/`headlessCfg`). The screen code is kept gated for now as the
experiment record; it can be removed as cleanup once the direction is settled.
