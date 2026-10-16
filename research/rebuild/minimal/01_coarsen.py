"""
01_coarsen.py  (minimal example, step 1, coarsened zeros; 2026-10-14)

What it does: applies the rule in research/rebuild/minimal/08_STATE_2026-10-14.md
("Rule for the dollar line") to SCF interest, dividends and capital gains, for
each SCF tax year:
  1. weighted holder share (amount != 0) in the SCF year and in the PUF 2015
     (the PUF 2015 share is used for every SCF year: there is no later PUF);
  2. SCF share lower by more than 1 point: the line f is the smallest PUF
     dollar amount such that the PUF's weighted share with amount > f is at
     or below the SCF holder share (search over the PUF's sorted amounts);
  3. SCF share higher by more than 1 point: no line, flagged;
  4. within 1 point: no line.
Capital gains use the absolute amount (losses count as holders).
Check: weighted 10th / 50th / 90th percentiles of the absolute amount among
holders above f, SCF against PUF (for 2018 and 2021 the SCF is in later
dollars, so levels are not strictly comparable).

Then it adds, for each of the three variables, `<var>_coarse_hi` to
stacked.parquet and stacked_years.parquet: for SCF rows at exactly zero, the
highest positive band of that source-year whose upper dollar edge is <= f
(the zero row then means "zero or a band up to this one"); -1 otherwise,
including when no band fits under f. For capital gains, small losses are
holders too, so it also adds `capital_gains_coarse_neg`: for the same rows,
the most negative loss band lying wholly within [-f, 0); -1 if none. Band
columns and band_edges*.json are not changed.

Writes (in /nfs/roberts/scratch/pi_nrs36/jar335/minimal/data/):
  stacked.parquet, stacked_years.parquet   rewritten with the new columns
  coarsen_lines.json                        f and the shares, per file/source/year/variable
  coarsen_check.csv                         the check table
"""
import json
import os
import numpy as np
import pandas as pd

D = '/nfs/roberts/scratch/pi_nrs36/jar335/minimal/data'
VARS = ['interest', 'dividends', 'capital_gains']
POS0, NEG0 = 11, 1


def wq(x, w, probs):
    o = np.argsort(x, kind='stable')
    xs, F = x[o], np.cumsum(w[o]) / w.sum()
    return xs[np.minimum(np.searchsorted(F, np.asarray(probs) - 1e-12), len(xs) - 1)]


def line_for(puf_abs, puf_w, target):
    """Smallest PUF amount v with weighted share(|x| > v) <= target."""
    tot = puf_w.sum()
    hold = puf_abs > 0
    a, w = puf_abs[hold], puf_w[hold]
    o = np.argsort(a, kind='stable')
    a, w = a[o], w[o]
    vals, idx = np.unique(a, return_index=True)
    above = (w.sum() - np.cumsum(w)[np.r_[idx[1:] - 1, len(a) - 1]]) / tot  # share with |x| > vals[i]
    i = int(np.argmax(above <= target))
    return float(vals[i]), float(above[i])


def run(fname, edges_name, tag):
    df = pd.read_parquet(f'{D}/{fname}')
    edges = json.load(open(f'{D}/{edges_name}'))
    puf = df[df.source == 'puf']
    lines, checks = {}, []
    for v in VARS:
        df[f'{v}_coarse_hi'] = np.int16(-1)
    df['capital_gains_coarse_neg'] = np.int16(-1)
    for yr in sorted(df.loc[df.source == 'scf', 'year'].unique()):
        scf = df[(df.source == 'scf') & (df.year == yr)]
        for v in VARS:
            pa, pw = puf[v].abs().to_numpy(), puf['weight'].to_numpy()
            sa, sw = scf[v].abs().to_numpy(), scf['weight'].to_numpy()
            ps, ss = pw[pa > 0].sum() / pw.sum(), sw[sa > 0].sum() / sw.sum()
            rec = {'puf_2015_holder_share': ps, 'scf_holder_share': ss}
            if ss < ps - 0.01:
                f, ach = line_for(pa, pw, ss)
                rec.update(decision='line', f=f, puf_share_above_f=ach)
            else:
                rec.update(decision='flag: SCF share higher' if ss > ps + 0.01 else 'within 1 point: no line', f=None)
            if rec['f'] is not None:
                f = rec['f']
                pp, sp = pa > f, sa > f
                qp, qs = wq(pa[pp], pw[pp], [.1, .5, .9]), wq(sa[sp], sw[sp], [.1, .5, .9])
                checks.append({'file': tag, 'year': int(yr), 'variable': v, 'f': f,
                               'puf_p10': qp[0], 'scf_p10': qs[0], 'puf_p50': qp[1], 'scf_p50': qs[1],
                               'puf_p90': qp[2], 'scf_p90': qs[2]})
                e = edges[f'scf/{int(yr)}/{v}']
                pc = np.array(e['positive']['cuts'])          # upper edge of band POS0 + i
                ok = np.nonzero(pc <= f)[0]
                hi = POS0 + int(ok.max()) if len(ok) else -1
                zero = (df.source == 'scf') & (df.year == yr) & (df[v] == 0)
                df.loc[zero, f'{v}_coarse_hi'] = hi
                rec['coarse_hi'] = hi
                rec['scf_holder_weight_share_in_coarsened_bands'] = (
                    float(sw[(scf[f'{v}_band'].to_numpy() >= POS0) & (scf[f'{v}_band'].to_numpy() <= hi)].sum() / sw[sa > 0].sum())
                    if hi >= 0 else 0.0)
                if v == 'capital_gains':
                    nc = e['negative']['cuts']                # band k (k >= 2) lies in (nc[k-2], nc[k-1]]
                    ks = [k for k in range(2, NEG0 + e['negative']['bands_used']) if nc[k - 2] >= -f]
                    lo = min(ks) if ks else -1
                    df.loc[zero, 'capital_gains_coarse_neg'] = lo
                    rec['coarse_neg'] = lo
            lines[f'scf/{int(yr)}/{v}'] = rec
    tmp = f'{D}/.{fname}.tmp'
    df.to_parquet(tmp, index=False)
    os.replace(tmp, f'{D}/{fname}')
    return lines, checks


out, allchk = {}, []
for fname, en, tag in [('stacked.parquet', 'band_edges.json', 'single'),
                       ('stacked_years.parquet', 'band_edges_years.json', 'years')]:
    lines, chk = run(fname, en, tag)
    out[tag] = lines
    allchk += chk
out['meta'] = {'rule': 'research/rebuild/minimal/08_STATE_2026-10-14.md, Rule for the dollar line',
               'note': 'PUF 2015 shares and amounts used for every SCF year; capital gains on absolute amounts',
               'coarse_hi': 'SCF zero rows: highest positive band with upper edge <= f; -1 = exact',
               'coarse_neg': 'capital gains, SCF zero rows: most negative loss band wholly within [-f, 0); -1 = none'}
json.dump(out, open(f'{D}/coarsen_lines.json', 'w'), indent=1)
pd.DataFrame(allchk).to_csv(f'{D}/coarsen_check.csv', index=False)
open(f'{D}/COARSEN_DONE', 'w').close()
open(f'{D}/COARSEN_YEARS_DONE', 'w').close()
pd.set_option('display.width', 250)
print(json.dumps(out, indent=1))
print(pd.DataFrame(allchk).to_string())
