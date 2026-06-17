#!/usr/bin/env python3
"""Parse RESULT lines from the matchup harness into a matrix + break-even table."""
import sys, re, math

res = re.compile(
    r'RESULT red=(\S+) blue=(\S+) reps=(\d+) muster=(\w+) '
    r'red_wins=(\d+) blue_wins=(\d+) draws=(\d+)')

rows = []
for f in sys.argv[1:]:
    for ln in open(f):
        m = res.search(ln)
        if m:
            red, blue, reps, mus, rw, bw, dr = m.groups()
            rows.append(dict(red=red, blue=blue, reps=int(reps), muster=mus,
                             rw=int(rw), bw=int(bw), dr=int(dr)))

def comp(s):  # "warrior:12" -> ("warrior", 12)
    t, n = s.split(':'); return t, int(n)

def winrate(r):
    n = r['rw'] + r['bw']  # draws excluded from win-rate
    return (r['rw'] / n) if n else float('nan')

def wilson(k, n, z=1.96):
    if n == 0: return (float('nan'), float('nan'))
    p = k/n; d = 1+z*z/n; c = p+z*z/(2*n)
    h = z*math.sqrt(p*(1-p)/n + z*z/(4*n*n))
    return ((c-h)/d, (c+h)/d)

TYPES = ['warrior', 'scout', 'siege']
AB = {'warrior': 'W', 'scout': 'S', 'siege': 'C'}

# --- equal-count matrix (12v12) ---
cell = {}
for r in rows:
    (rt, rn), (bt, bn) = comp(r['red']), comp(r['blue'])
    if rn == 12 and bn == 12:
        cell[(rt, bt)] = r

print("=== equal-count matrix: red win-rate (red row, blue col, 12v12) ===")
print("        " + "".join(f"{AB[b]:>8}" for b in TYPES))
for a in TYPES:
    line = f"{AB[a]:>6}  "
    for b in TYPES:
        r = cell.get((a, b))
        line += f"{winrate(r):>8.2f}" if r else f"{'-':>8}"
    print(line)
print("(0.50 = even; rows are the attacker type fielded by Red)")

# --- counter edges with CI + draws ---
print("\n=== intended counter edges at parity (12v12) ===")
for a, b in [('warrior', 'scout'), ('scout', 'siege'), ('siege', 'warrior')]:
    r = cell.get((a, b))
    if not r: continue
    wr = winrate(r); lo, hi = wilson(r['rw'], r['rw']+r['bw'])
    verdict = "REAL" if lo >= 0.60 else ("weak" if wr > 0.5 else "BROKEN")
    print(f"{AB[a]}>{AB[b]}: win {wr:.2f} CI[{lo:.2f},{hi:.2f}] draws={r['dr']}  -> {verdict}")

# --- break-even sweeps ---
print("\n=== break-even: winner:k vs loser:12  (k where win-rate crosses 0.50) ===")
for a, b in [('warrior', 'scout'), ('scout', 'siege'), ('siege', 'warrior')]:
    pts = []
    for r in rows:
        (rt, rn), (bt, bn) = comp(r['red']), comp(r['blue'])
        if rt == a and bt == b and bn == 12 and r['reps'] == 200:
            pts.append((rn, winrate(r)))
    pts.sort()
    series = "  ".join(f"{k}:{wr:.2f}" for k, wr in pts)
    # linear-interpolate the crossing
    be = None
    for (k1, w1), (k2, w2) in zip(pts, pts[1:]):
        if (w1 - 0.5) * (w2 - 0.5) <= 0 and w2 != w1:
            be = k1 + (0.5 - w1) * (k2 - k1) / (w2 - w1); break
    bestr = f"{be/12:.2f}x" if be else "off-scale"
    print(f"{AB[a]}>{AB[b]}: {series}")
    print(f"   break-even ~{be:.1f} vs 12  ({bestr} numbers)  "
          + ("GOOD (<=1.0x)" if be and be <= 12 else "WEAK (needs more numbers)"))
