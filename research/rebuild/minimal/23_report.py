"""
23_report.py  (step 23: v4c against N1, v4c with crude non-filers, and N2,
N1 with decision 18's latent amounts; 23_NONFILERS.md)

For each run: the stack with its 2019 dollars (05_tilt.generated_2019), the
weights before and after the tilt (tilt/<run>/weights_<run>.parquet), and
the SCF 2019 wave. Net worth in nominal 2019 dollars (2015 dollars x
1.2493059 / 1.1592053, the asset factors of the 2016 and 2019 waves);
incomes of the generated files in 2019 dollars, of the SCF in 2018 dollars
(its income year). Groups: wages, any interest, any dividends, both none.
Each statistic over filers only and over all units (filers and non-filers;
v4c has filers only).

Writes tilt/step23_report.md.
"""
import importlib.util
import os

import numpy as np
import pandas as pd

HERE = 'research/rebuild/minimal'
R = '/nfs/roberts/scratch/pi_nrs36/jar335/minimal'
TO_2019 = 1.2493059 / 1.1592053
RUNS = {'v4c': ('v4c', 'v4c_scfbase'), 'N1 non-filers': ('n1', 'n1'), 'N2 non-filers + latent': ('n2', 'n2'),
        # overnight (24_OVERNIGHT.md): seed replicates, and the count base on the file's units
        'N1 seed 2': ('n1s2', 'n1s2'), 'N1 seed 3': ('n1s3', 'n1s3'),
        'N2 seed 2': ('n2s2', 'n2s2'), 'N2 seed 3': ('n2s3', 'n2s3_t1'),   # the 2-thread tilt missed the tolerance; 1 thread converged
        'N1, counts on file units': ('n1', 'n1_file_units'), 'N2, counts on file units': ('n2', 'n2_file_units')}
RUNS = {k: v for k, v in RUNS.items() if os.path.exists(f'{R}/tilt/{v[1]}/summary_{v[0]}.csv')}


def load(name, f):
    s = importlib.util.spec_from_file_location(name, f'{HERE}/{f}')
    m = importlib.util.module_from_spec(s)
    s.loader.exec_module(m)
    return m


def stats(df, w, nw):
    """Rows of statistics: totals, median, share negative, means by group."""
    out = {}
    out['units (millions)'] = w.sum() / 1e6
    out['total net worth ($ trillion)'] = (w * nw).sum() / 1e12
    o = np.argsort(nw)
    c = np.cumsum(w[o]) / w.sum()
    out['median ($)'] = nw[o][np.searchsorted(c, 0.5)]
    out['share negative (%)'] = 100 * w[nw < 0].sum() / w.sum()
    wg = df['wages'].to_numpy()
    for name, lo, hi in [('wages 0', 0, 1), ('wages 1-25k', 1, 25e3), ('wages 25-50k', 25e3, 50e3),
                         ('wages 50-100k', 50e3, 1e5), ('wages 100-200k', 1e5, 2e5), ('wages 200k+', 2e5, np.inf)]:
        m = (wg >= lo) & (wg < hi)
        out[f'mean, {name} ($)'] = (w[m] * nw[m]).sum() / w[m].sum()
    i0, d0 = df['interest'].to_numpy() == 0, df['dividends'].to_numpy() == 0
    for name, m in [('no interest', i0), ('some interest', ~i0), ('no dividends', d0),
                    ('some dividends', ~d0), ('neither', i0 & d0)]:
        out[f'mean, {name} ($)'] = (w[m] * nw[m]).sum() / w[m].sum()
        out[f'share with {name} (%)'] = 100 * w[m].sum() / w.sum()
    return out


def main():
    os.environ['MINIMAL_MAP_DIR'] = f'{R}/map/V4'
    wt = load('wt', '06_wealth_tables.py')
    tm = wt.tiltmod
    cols, moves = {}, []
    u19, x19, w19, _ = wt.scf2019()
    cols['SCF 2019, all units'] = stats(u19, w19.to_numpy(float) if hasattr(w19, 'to_numpy') else w19,
                                        np.asarray(x19, float) * TO_2019)
    for label, (v, tag) in RUNS.items():
        os.environ['GEN_FILE'] = f'{R}/gen/gen_puf_stack_{v}.parquet'
        g = tm.generated_2019()
        wts = pd.read_parquet(f'{R}/tilt/{tag}/weights_{v}.parquet')
        assert (wts['record_id'].to_numpy() == g['record_id'].to_numpy()).all()
        nw = g['net_worth'].to_numpy(float) * TO_2019
        filer = g['filer'].to_numpy() == 1 if 'filer' in g else np.ones(len(g), bool)
        for when, w in [('before', wts['weight_before'].to_numpy()), ('after', wts['weight_after'].to_numpy())]:
            cols[f'{label}, {when}, filers'] = stats(g[filer].reset_index(drop=True), w[filer], nw[filer])
            if not filer.all():
                cols[f'{label}, {when}, all'] = stats(g, w, nw)
        s = pd.read_csv(f'{R}/tilt/{tag}/summary_{v}.csv')
        moves.append({'run': label, 'move, 136 IRS (nats)': s['kl_nats'].iloc[0],
                      'move, 136 IRS + wealth (nats)': s['kl_nats'].iloc[1],
                      'records kept, effective (%)': 100 * s['ess_ratio'].iloc[1]})
    t = pd.DataFrame(cols)
    fmt = lambda r, x: (f'{x:,.1f}' if ('%' in r or 'trillion' in r or 'millions' in r) else f'{x:,.0f}')
    lines = ['# Step 23: v4c, N1 (crude non-filers), N2 (crude non-filers + latent amounts)', '',
             'Net worth in nominal 2019 dollars. The non-filers are a crude stopgap (23_NONFILERS.md).', '',
             '## The move', '', pd.DataFrame(moves).to_markdown(index=False, floatfmt='.4f'), '']
    for part, keep in [('Filers only', lambda c: c.endswith('filers') or c.startswith('SCF')),
                       ('All units', lambda c: c.endswith('all') or c.startswith('SCF') or c.startswith('v4c'))]:
        sub = t[[c for c in t.columns if keep(c)]]
        lines += [f'## {part}', '', '| | ' + ' | '.join(sub.columns) + ' |', '|---|' + '---:|' * len(sub.columns)]
        for r in sub.index:
            lines.append(f'| {r} | ' + ' | '.join(fmt(r, x) for x in sub.loc[r]) + ' |')
        lines.append('')
    open(f'{R}/tilt/step23_report.md', 'w').write('\n'.join(lines) + '\n')
    print('\n'.join(lines))


if __name__ == '__main__':
    main()
