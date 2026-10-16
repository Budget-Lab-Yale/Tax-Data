"""
15_checks.py  (minimal example, v4 baseline: the copy check and the stack checks)

Usage (through Slurm, 15_run_cpu.sbatch), from the repo root:
  python 15_checks.py copy    the copy check of the V4 model
  python 15_checks.py stack   the stack against the unoversampled V4 file

The copy check. The V4 model is trained on every record except a 2%
held-back slice (02_model.py, MINIMAL_ALL_RECORDS=1). A model that copies
its training records generates records that match training records more
often than records it never saw. For the 200,000 generated PUF-style records
of gen/V4/gen_puf.parquet, on the seven PUF slots (married, n_dep, and the
bands of wages, interest, dividends, capital gains, other_agi):
  - share that exactly match at least one training record;
  - share that exactly match at least one held-back record;
  - the same share against a random slice of the training records the size
    of the held-back slice (the yardstick: if the model does not copy, the
    held-back share and this one are equal up to noise);
  - matches per generated record divided by the number of records in the
    set, training against held-back (the held-back share scaled by the slice
    size); a ratio well above 1 means copying.
Each also for the generated records with any of the five amounts in its
top 1% bands, where copying would show first, and for the 40,000 SCF-style
records against the SCF rows (seven slots: married, n_dep, four shared
bands, net worth).

The stack checks. Importance weights sum to 400,000, each file to 100,000.
Then, in 2015 dollars (V4 maps, MINIMAL_MAP_DIR), the stack against the
plain V4 file (gen/V4/gen_puf_with_net_worth.parquet), with a second plain
file (seed 35, written by 03_stack.py) as the noise yardstick: holder shares,
net worth statistics, mean net worth by wage group.

Writes R/checks/V4/copy_check.csv, stack_check.csv and checks.md.
"""
import importlib.util
import os
import sys

import numpy as np
import pandas as pd

ROOT = '/nfs/roberts/scratch/pi_nrs36/jar335/minimal'
HERE = os.path.dirname(os.path.abspath(__file__))
OUT = os.environ.get('MINIMAL_CHECKS_DIR', f'{ROOT}/checks/V4')   # v4b: checks/V4b, v4c: checks/V4c
os.environ.setdefault('MINIMAL_TABLE', 'E4')
os.environ.setdefault('MINIMAL_RUN', 'V4')
os.environ.setdefault('MINIMAL_ALL_RECORDS', '1')
os.environ.setdefault('MINIMAL_MAP_DIR', f'{ROOT}/map/V4')


def load(name, file):
    spec = importlib.util.spec_from_file_location(name, os.path.join(HERE, file))
    mod = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(mod)
    return mod


def md(df, digits=4):
    def f(x):
        return f'{x:.{digits}g}' if isinstance(x, float) else str(x)
    return '\n'.join(['| ' + ' | '.join(df.columns) + ' |', '|' + '---|' * len(df.columns)] +
                     ['| ' + ' | '.join(f(x) for x in r) + ' |' for r in df.itertuples(index=False)])


# ---------------------------------------------------------------- copy check

def keys(df, cols):
    return pd.MultiIndex.from_frame(df[cols].astype(int)).to_flat_index()


def match_stats(gen_keys, ref_keys):
    """Per generated record: does it match at least one reference record, and
    how many reference records it matches."""
    counts = pd.Series(ref_keys).value_counts()
    n = pd.Series(gen_keys).map(counts).fillna(0).to_numpy()
    return n > 0, n


def copy_check(m):
    table = m.read_table()
    rows = []
    rng = np.random.default_rng(20261015)
    for source, gen_file, cols in [
            ('puf', 'gen_puf.parquet', ['married', 'n_dep'] + [f'{v}_band' for v in
                                        ['wages', 'interest', 'dividends', 'capital_gains', 'other_agi']]),
            ('scf', 'gen_scf.parquet', ['married', 'n_dep'] + [f'{v}_band' for v in
                                        ['wages', 'interest', 'dividends', 'capital_gains', 'net_worth']])]:
        t = table[table.source == source]
        gen = pd.read_parquet(f'{m.GEN_DIR}/{gen_file}')
        gen = gen[gen.source == source].reset_index(drop=True)
        assert (gen[cols] >= 0).all().all() and (t[cols] >= 0).all().all()
        tr, hb = t[t.split == 'train'], t[t.split == 'test']
        # yardstick: a random slice of the training records, the held-back size
        yard = tr.iloc[rng.choice(len(tr), size=len(hb), replace=False)]
        k_gen = keys(gen, cols)
        top = np.zeros(len(gen), bool)
        for v in [c[:-5] for c in cols if c.endswith('_band')]:
            top |= np.isin(gen[f'{v}_band'].to_numpy(), m.positive_bands(v, 0.99, 1.0))
        res = {name: match_stats(k_gen, keys(ref, cols)) for name, ref in
               [('train', tr), ('held', hb), ('yard', yard)]}
        for subset, sel in [('all generated records', np.ones(len(gen), bool)),
                            ('records with an amount in its top 1% bands', top)]:
            r = {'source': source, 'generated records': subset, 'records': int(sel.sum()),
                 'training records': len(tr), 'held-back records': len(hb)}
            for name, label in [('train', 'match a training record (%)'),
                                ('held', 'match a held-back record (%)'),
                                ('yard', 'match a training slice of held-back size (%)')]:
                r[label] = 100 * res[name][0][sel].mean()
            rate_tr = res['train'][1][sel].mean() / len(tr)
            rate_hb = res['held'][1][sel].mean() / len(hb)
            r['matches per training record'] = rate_tr
            r['matches per held-back record'] = rate_hb
            r['ratio, training to held-back'] = rate_tr / rate_hb if rate_hb > 0 else np.nan
            rows.append(r)
            print(r, flush=True)
    return pd.DataFrame(rows)


# ---------------------------------------------------------------- stack check

def stack_check():
    wt = load('wt', '06_wealth_tables.py')
    dollars = wt.dollars
    meta_v = os.environ.get('STACK_VERSION', 'v4')
    stack = pd.read_parquet(f'{ROOT}/gen/gen_puf_stack_{meta_v}.parquet')
    sums = stack.groupby('stratifier')['weight'].agg(['size', 'sum'])
    rows = [{'check': f'weight sum, {k}', 'value': float(r['sum']), 'records': int(r['size'])}
            for k, r in sums.iterrows()]
    rows.append({'check': 'weight sum, all', 'value': float(stack['weight'].sum()), 'records': len(stack)})
    weights_md = md(pd.DataFrame(rows), 9)
    plain = pd.read_parquet(os.environ.get('MINIMAL_STACK_PLAIN',
                                           f'{ROOT}/gen/V4/gen_puf_with_net_worth.parquet'))
    plain2 = pd.read_parquet(f'{ROOT}/gen/gen_plain_seed35_{meta_v}.parquet')
    maps = dollars.puf_maps(2015)
    stats = {}
    for name, f in [('plain', plain), ('plain, second seed', plain2), ('stack', stack)]:
        f = f[f.source == 'puf'].reset_index(drop=True)
        d, _ = dollars.draw_dollars(f, maps)
        w = f['weight'].to_numpy(float)
        x = d['net_worth'].to_numpy(float)
        s = {}
        for v in ['interest', 'dividends', 'capital_gains']:
            s[f'holders: {v} (%)'] = 100 * w[d[v].to_numpy(float) != 0].sum() / w.sum()
        s['holders: wages (%)'] = 100 * w[d['wages'].to_numpy(float) != 0].sum() / w.sum()
        s['joint (%)'] = 100 * w[d['married'].to_numpy() == 1].sum() / w.sum()
        u = wt.unconditional(x, w)
        for k in ['mean ($)', 'median ($)', 'share negative (%)', 'top 10% share (%)',
                  'top 1% share (%)', 'top 0.1% share (%)', '99th percentile ($)', '99.9th percentile ($)']:
            s[f'net worth: {k}'] = u[k]
        c = wt.conditional(d, x, w)
        for r in c[c.grouping == 'wages'].itertuples():
            s[f'mean net worth, wages {r.group} ($)'] = c.loc[r.Index, 'mean ($)']
            s[f'share of units, wages {r.group} (%)'] = c.loc[r.Index, 'share of units (%)']
        for v in ['wages', 'capital_gains', 'other_agi', 'interest', 'dividends']:
            s[f'mean {v} ($, 2015)'] = (d[v].to_numpy(float) * w).sum() / w.sum()
        stats[name] = s
    tab = pd.DataFrame(stats)
    tab['stack minus plain'] = tab['stack'] - tab['plain']
    tab['second seed minus plain'] = tab['plain, second seed'] - tab['plain']
    tab = tab.reset_index().rename(columns={'index': 'statistic'})
    return pd.DataFrame(rows), tab, weights_md


def tilt_more():
    """For tilt/v4 (filer targets) and tilt/v4_allunits: the move with 0 log 0 = 0
    (09_score.short_move; weights that underflow to zero made 05's sum NaN),
    the threshold-form targets and their misses, and the records behind the
    largest redraw miss."""
    wt = load('wt', '06_wealth_tables.py')
    dollars, tiltmod = wt.dollars, wt.tiltmod
    base = load('base', '07_baseline.py')
    v = os.environ.get('STACK_VERSION', 'v4')
    stack = pd.read_parquet(f'{ROOT}/gen/gen_puf_stack_{v}.parquet')
    g15, _ = dollars.draw_dollars(stack, dollars.puf_maps(2015))
    x = g15['net_worth'].to_numpy(float)
    returns = tiltmod.target_set('totals')[1].query("cell == 'returns'")['value'].iloc[0]
    rows, tq, text = [], [], []
    # MINIMAL_TILT_TAGS (v4b): which tilt folders, as folder:units pairs
    tags = os.environ.get('MINIMAL_TILT_TAGS', 'v4:filers,v4_allunits:all')
    for tag, units in [t.split(':') for t in tags.split(',')]:
        w = pd.read_parquet(f'{ROOT}/tilt/{tag}/weights_{v}.parquet')
        w0, w1, w2 = (w[c].to_numpy(float) for c in ['weight_before', 'weight_after_136', 'weight_after'])
        for name, ww in [('136 alone', w1), ('136 + eight wealth', w2)]:
            p0, p1 = w0 / w0.sum(), ww / ww.sum()
            pos = p1 > 0
            ess = lambda v: v.sum() ** 2 / (v ** 2).sum()
            rows.append({'wealth targets from': units, 'tilt': name,
                         'move (nats)': float(np.sum(p1[pos] * np.log(p1[pos] / p0[pos]))),
                         'ESS ratio': ess(ww) / ess(w0), 'records at zero weight': int((ww == 0).sum()),
                         'records under 0.1x': int((ww / w0 < 0.1).sum()),
                         'records over 10x': int((ww / w0 > 10).sum())})
        targets, threshold = wt.wealth_targets(units)
        Fq, Tq = base.threshold_form(x, returns, targets, threshold, units)
        _, x19, w19, _ = wt.scf2019(units)
        qs = dollars.wquantile(x19, w19, [0.99, 0.90, 0.50])
        names = []
        for q, lab in zip(qs, ['99th', '90th', '50th']):
            names += [f'units above the SCF {lab} percentile (${q:,.0f})', f'net worth above the SCF {lab} percentile']
        names += ['total net worth', 'units above $1 million (2019 dollars)']
        for n_, f, t in zip(names, Fq.T, Tq):
            tq.append({'wealth targets from': units, 'target': n_, 'target value': t,
                       'pretrain': f @ w0, 'posttrain': f @ w2, 'posttrain miss %': 100 * (f @ w2 / t - 1)})
        rd = pd.read_csv(f'{ROOT}/tilt/{tag}/redraw_{v}.csv')
        worst = rd.loc[rd['gap_pct'].abs().idxmax()]
        text.append(f"{units}: worst redraw cell {worst['bracket']} / {worst['cell']}: target {worst['value']:.4g}, "
                    f"redraw {worst['redrawn']:.4g}, z {worst['z']:.2f}")
        bad = rd[rd['z'].abs() > 2][['bracket', 'cell', 'value', 'redrawn', 'gap_pct', 'z']]
        text.append(md(bad))
    return pd.DataFrame(rows), pd.DataFrame(tq), text


def main():
    os.makedirs(OUT, exist_ok=True)
    what = sys.argv[1]
    if what == 'tiltmore':
        mv, tq, text = tilt_more()
        mv.to_csv(f'{OUT}/tilt_moves.csv', index=False)
        tq.to_csv(f'{OUT}/threshold_targets.csv', index=False)
        out = '### Moves\n\n' + md(mv) + '\n\n### Threshold-form targets\n\n' + md(tq) + '\n\n' + '\n\n'.join(text) + '\n'
        open(f'{OUT}/tilt_more.md', 'w').write(out)
        print(out)
        return
    if what == 'copy':
        m = load('m', '02_model.py')
        out = copy_check(m)
        out.to_csv(f'{OUT}/copy_check.csv', index=False)
        open(f'{OUT}/copy_check.md', 'w').write(md(out) + '\n')
        print(md(out))
    elif what == 'stack':
        w, tab, wmd = stack_check()
        tab.to_csv(f'{OUT}/stack_check.csv', index=False)
        text = '### Weight sums\n\n' + wmd + '\n\n### Stack against the plain file (2015 dollars)\n\n' + md(tab) + '\n'
        open(f'{OUT}/stack_check.md', 'w').write(text)
        print(text)
    else:
        raise SystemExit(f'unknown check {what}')


if __name__ == '__main__':
    main()
