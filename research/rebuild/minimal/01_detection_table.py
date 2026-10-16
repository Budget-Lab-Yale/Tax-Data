"""
01_detection_table.py  (minimal example: PUF against SCF by dollar bracket)

What it does. Reads the stacked table (PUF and SCF, tax year 2015) and, for
wages, interest, dividends and capital gains, tabulates the weighted share of
units in each dollar bracket in each source. Capital gains are bracketed on
the absolute amount, so a loss counts as holding the item; the share with a
negative amount is reported on its own line. Shares are within source (each
source's weights sum to one), so the SCF's implicate weighting does not
matter.

Brackets: exactly 0; above 0 and under 100 (labelled 1-100); 100-500;
500-2,000; 2,000-10,000; 10,000-50,000; 50,000-200,000; 200,000 and over.
Each bracket includes its lower bound and excludes its upper bound.

Per variable it writes three tables:
  1. share of units by bracket in each source, and SCF share / PUF share;
  2. the reporting rate taking the PUF as the truth: the SCF share divided
     by the PUF share by bracket, and the cumulative version (share of units
     at or above the bracket's lower bound, SCF over PUF). A star marks the
     first bracket above zero, counting up, where each ratio is 0.9 or more;
  3. the weighted mean amount (absolute amount for capital gains) within
     each bracket in each source.

Reads:  /nfs/roberts/scratch/pi_nrs36/jar335/minimal/data/stacked.parquet
        (not modified)
Writes: research/rebuild/minimal/01_DETECTION_TABLE.md
Run:    sbatch through Slurm (partition day); see the job line in the log.
"""
import numpy as np
import pandas as pd

DATA = '/nfs/roberts/scratch/pi_nrs36/jar335/minimal/data/stacked.parquet'
OUT = '/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data/research/rebuild/minimal/01_DETECTION_TABLE.md'
VARS = ['wages', 'interest', 'dividends', 'capital_gains']
LOWER = [100, 500, 2_000, 10_000, 50_000, 200_000]
LABELS = ['0', '1-100', '100-500', '500-2,000', '2,000-10,000',
          '10,000-50,000', '50,000-200,000', '200,000+']

cols = ['source', 'weight'] + VARS
schema_cols = pd.read_parquet(DATA, columns=None).columns
d = pd.read_parquet(DATA, columns=cols + (['year'] if 'year' in schema_cols else []))
print(d.dtypes)
print(d['source'].value_counts())
year_note = ''
if 'year' in d:
    print(d.groupby(['source', 'year']).size())
    if d['year'].nunique() > 1:
        year_note = ' Restricted to rows with year 2015.'
        d = d[d['year'] == 2015]
    assert set(d['year']) == {2015}
assert set(d['source']) == {'puf', 'scf'}
na = d[cols].isna().sum()
assert na.sum() == 0, na
assert (d['weight'] > 0).all()


def bracket(a):
    """Bracket index 0-7 of an absolute amount."""
    b = np.where(a == 0, 0, 1 + np.searchsorted(LOWER, a, side='right'))
    return b


def fmt_pct(x):
    return f'{100 * x:.2f}%'


def fmt_ratio(x):
    return '' if not np.isfinite(x) else f'{x:.3f}'


lines = ['---',
         'title: "Minimal example: PUF against SCF by dollar bracket, the detection table"',
         'role: evidence', 'workstream: rebuild', 'status: current',
         'updated: 2026-10-15', 'sot: research/rebuild/design/plan.md',
         'supersedes: []', 'superseded_by: null', '---', '',
         '# Minimal example: PUF against SCF by dollar bracket, the detection table', '',
         'For each variable, the first table gives the weighted share of units in each dollar bracket '
         'in the PUF and in the SCF (tax year 2015, shares within source, capital gains on the absolute '
         'amount) and the ratio SCF/PUF; the second gives that ratio as a reporting rate taking the PUF '
         'as the truth, by bracket and cumulatively (share at or above the bracket lower bound, SCF over '
         'PUF), with a star on the first bracket above zero where the ratio is 0.9 or more; the third '
         'gives the weighted mean amount within each bracket in each source. '
         'Brackets include the lower bound and exclude the upper one, "1-100" covers amounts above 0 '
         'and under 100, and the record counts are unweighted.' + year_note, '',
         'Produced by `research/rebuild/minimal/01_detection_table.py` from '
         '`/nfs/roberts/scratch/pi_nrs36/jar335/minimal/data/stacked.parquet`.', '']

for v in VARS:
    rows = {}
    for s in ['puf', 'scf']:
        x = d[d['source'] == s]
        a = x[v].abs().to_numpy()
        w = x['weight'].to_numpy()
        b = bracket(a)
        W = w.sum()
        share = np.array([w[b == k].sum() / W for k in range(8)])
        n = np.array([(b == k).sum() for k in range(8)])
        mean = np.array([np.average(a[b == k], weights=w[b == k]) if (b == k).any() else np.nan
                         for k in range(8)])
        cum = np.array([share[k:].sum() for k in range(8)])
        neg = w[x[v].to_numpy() < 0].sum() / W
        rows[s] = dict(share=share, n=n, mean=mean, cum=cum, neg=neg)
    p, q = rows['puf'], rows['scf']
    ratio = q['share'] / p['share']
    cratio = q['cum'] / p['cum']

    def first90(r):
        for k in range(1, 8):
            if np.isfinite(r[k]) and r[k] >= 0.9:
                return k
        return None
    f1, f2 = first90(ratio), first90(cratio)

    name = v.replace('_', ' ')
    lines += [f'## {name.capitalize()}', '', f'### Share of units by bracket ({name}' +
              (', absolute amount' if v == 'capital_gains' else '') + ')', '',
              '| Bracket ($) | PUF share | SCF share | SCF / PUF | PUF records | SCF records |',
              '|---|---:|---:|---:|---:|---:|']
    for k in range(8):
        lines.append(f'| {LABELS[k]} | {fmt_pct(p["share"][k])} | {fmt_pct(q["share"][k])} | '
                     f'{fmt_ratio(ratio[k])} | {p["n"][k]:,} | {q["n"][k]:,} |')
    if v == 'capital_gains':
        lines += ['', f'Share with a negative amount (a net loss): PUF {fmt_pct(p["neg"])}, '
                  f'SCF {fmt_pct(q["neg"])}, SCF / PUF {fmt_ratio(q["neg"] / p["neg"])}.']
    lines += ['', '### Reporting rate taking the PUF as the truth', '',
              '| Bracket ($) | By bracket: SCF / PUF | Cumulative: PUF share at or above | '
              'Cumulative: SCF share at or above | Cumulative: SCF / PUF |',
              '|---|---:|---:|---:|---:|']
    for k in range(8):
        r1 = fmt_ratio(ratio[k]) + (' *' if k == f1 else '')
        r2 = fmt_ratio(cratio[k]) + (' *' if k == f2 else '')
        lines.append(f'| {LABELS[k]} | {r1} | {fmt_pct(p["cum"][k])} | {fmt_pct(q["cum"][k])} | {r2} |')
    lines += ['', '\\* first bracket above zero, counting up, where the ratio is 0.9 or more'
              + ('' if f1 is not None and f2 is not None else '; none reached it in a column left unmarked') + '.',
              '', '### Weighted mean amount within each bracket ($' +
              (', absolute amount' if v == 'capital_gains' else '') + ')', '',
              '| Bracket ($) | PUF mean | SCF mean | SCF / PUF |', '|---|---:|---:|---:|']
    for k in range(8):
        pm, qm = p['mean'][k], q['mean'][k]
        r = qm / pm if k > 0 else np.nan
        lines.append(f'| {LABELS[k]} | {pm:,.0f} | {qm:,.0f} | {fmt_ratio(r)} |')
    lines.append('')

open(OUT, 'w').write('\n'.join(lines))
print('\n'.join(lines))
