"""
10_score_years.py  (minimal example, set B of 10_TRANSLATION_EXPERIMENTS.md;
2026-10-15)

Usage (through Slurm, 10_score_years.sbatch):  python 10_score_years.py <T>
  T is one of Y0, Y1, Y2, Y3: a model trained on table stacked_<T>.parquet by
  09_run_one.sbatch, with files generated per year by 03_generate.py
  (MINIMAL_GEN_YEARS=2015,2018,2021): gen_puf_with_net_worth_<year>,
  gen_puf_with_net_worth_ownerlabel_<year>, gen_scf_<year>.

What it scores, per year t in 2015, 2018, 2021:
  1. Net worth on the PUF-style records (net worth drawn under the PUF label,
     and under the SCF label), against the actual SCF wave of year t (survey
     weights). Dollars come from the rank-dollar map of that wave's own net
     worth bands (real SCF values of year t in each band, drawn by weight,
     04_dollars.draw_one; empty bands filled by 04's fill_row). Net worth is in
     the wave's interview-year dollars on both sides. The generated records
     get equal weights summing to the wave's weighted count, so the total
     ratio equals the mean ratio. Reported: total (= mean) ratio, median
     ratio, 99th percentile ratio, top 1% share generated and actual.
  2. Holder shares (amount not zero) of interest, dividends and capital gains
     on the PUF-style records, against the SCF wave of year t (raw amounts,
     weighted), against the IRS share of returns with the item in year t
     (Table 1.4: returns with taxable interest, with ordinary dividends, with
     a Schedule D gain or loss, over all returns), and for 2015 against the
     weighted PUF.
  3. The SCF-style records of year t against the same wave: net worth
     statistics and holder shares (does the model reproduce each wave?).

Income dollars are not drawn: the scores here need only net worth dollars
and the holder flags, which come from the bands.

Writes R/score/<T>/score_years.json and score_years.md, touches
R/markers/SCORE_YEARS_<T>_DONE, and rewrites the summary table between the
TABLE markers of 10_RESULTS_YEARS.md from every run scored so far.
"""
import fcntl
import importlib.util
import json
import os
import sys
import numpy as np
import pandas as pd

ROOT = '/nfs/roberts/scratch/pi_nrs36/jar335/minimal'
HERE = os.path.dirname(os.path.abspath(__file__))
RESULTS_MD = os.path.join(HERE, '10_RESULTS_YEARS.md')
SOI = '/nfs/roberts/project/pi_nrs36/shared/raw_data/IRS-Ind/aligned/bysize_income_sources.csv'
YEARS = [2015, 2018, 2021]
INV = ['interest', 'dividends', 'capital_gains']
RUNS = ['Y0', 'Y1', 'Y2', 'Y3']
DESCRIBE = {'Y0': 'each wave on its own cut points',
            'Y1': 'PUF 2015 cuts grown by SOI totals',
            'Y2': 'PUF 2015 cuts unchanged',
            'Y3': 'PUF 2015 cuts moved by SCF percentile growth'}


def load(name, file):
    spec = importlib.util.spec_from_file_location(name, os.path.join(HERE, file))
    mod = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(mod)
    return mod


wt = load('wt', '06_wealth_tables.py')
dollars = wt.dollars


# ---------------------------------------------------------------- inputs

def irs_holder_shares():
    """Share of all returns with the item, per year, from SOI Table 1.4."""
    t = pd.read_csv(SOI, dtype=str)
    t = t[(t['table'] == '1.4') & (t['panel'] == 'All returns') & (t['row_label'] == 'All returns, total')]
    item = {'number of returns': 'returns',
            'taxable interest > number of returns': 'interest',
            'ordinary dividends > number of returns': 'dividends',
            'sales of capital assets reported on form 1040, schedule d > taxable net gain > number of returns': 'kg_gain',
            'sales of capital assets reported on form 1040, schedule d > taxable net loss > number of returns': 'kg_loss'}
    t = t[t['item'].isin(item)]
    out = {}
    for y in YEARS:
        c = {item[r.item]: float(r.value) for r in t[t.tax_year == str(y)].itertuples()}
        assert len(c) == len(item), (y, c)
        out[y] = {'interest': c['interest'] / c['returns'], 'dividends': c['dividends'] / c['returns'],
                  'capital_gains': (c['kg_gain'] + c['kg_loss']) / c['returns']}
    return out


def nw_map(scf_t, edge):
    """Rank-dollar map of net worth for one wave: real values per band by
    weight, plus 04's fill rows for bands with no record."""
    b = scf_t['net_worth_band'].to_numpy()
    x = scf_t['net_worth'].to_numpy(float)
    w = scf_t['weight'].to_numpy(float)
    assert (b >= 0).all() and not np.isnan(x).any(), 'an SCF row lacks net worth'
    m = pd.DataFrame({'variable': 'net_worth', 'band': b, 'value': x, 'weight': w,
                      'filled': False, 'rule': 'real records'})
    missing = sorted(set(range(edge['n_bands'])) - set(b))
    fills = [dict(zip(['value', 'rule'], dollars.fill_row(edge, k)), variable='net_worth', band=k,
                  weight=1.0, filled=True) for k in missing]
    m = pd.concat([m, pd.DataFrame(fills, columns=m.columns)], ignore_index=True)
    return m.sort_values(['band', 'value'], kind='stable').reset_index(drop=True), len(missing)


def gen_net_worth(path, source, vmap, year):
    g = pd.read_parquet(path)
    assert (g['source'] == source).all() and (g['year'] == year).all(), path
    b = g['net_worth_band'].to_numpy()
    assert (b >= 0).all(), f'{path}: a record lacks a net worth band'
    x, rule = dollars.draw_one(b, vmap, np.random.default_rng([dollars.SEED, 5, year]))
    return g, x, int((rule != 'real records').sum())


def stats(x, w):
    u = wt.unconditional(x, w)
    return {k: float(v) for k, v in u.items()}


def compare(gen, act):
    return {'total ratio': gen['total net worth ($ trillion)'] / act['total net worth ($ trillion)'],
            'mean ratio': gen['mean ($)'] / act['mean ($)'],
            'median ratio': gen['median ($)'] / act['median ($)'],
            'p99 ratio': gen['99th percentile ($)'] / act['99th percentile ($)'],
            'top 1% share generated (%)': gen['top 1% share (%)'],
            'top 1% share actual (%)': act['top 1% share (%)']}


def holders_band(g):
    return {v: float((g[f'{v}_band'].to_numpy() > 0).mean()) for v in INV}


def holders_raw(df):
    w = df['weight'].to_numpy(float)
    return {v: float(w[df[v].to_numpy(float) != 0].sum() / w.sum()) for v in INV}


# ---------------------------------------------------------------- score one run

def score(T):
    model, gen = f'{ROOT}/model/{T}', f'{ROOT}/gen/{T}'
    meta = json.load(open(f'{model}/train_meta.json'))
    assert meta['table'] == T, meta['table']
    table = pd.read_parquet(f'{ROOT}/data/stacked_{T}.parquet')
    edges = json.load(open(f'{ROOT}/data/band_edges_{T}.json'))
    irs = irs_holder_shares()
    puf = table[table.source == 'puf']
    S = {'run': T, 'describe': DESCRIBE[T], 'table_meta': edges['meta'], 'years': {},
         'best_val_loss': meta.get('best_val_loss'), 'epochs_run': meta.get('epochs_run')}
    for y in YEARS:
        scf_t = table[(table.source == 'scf') & (table.year == y)]
        vmap, n_fill = nw_map(scf_t, edges[f'scf/{y}/net_worth'])
        wa = scf_t['weight'].to_numpy(float)
        act = stats(scf_t['net_worth'].to_numpy(float), wa)
        Y = {'actual SCF': act, 'actual SCF weighted count (millions)': wa.sum() / 1e6,
             'net worth bands filled (no record)': n_fill}
        for name, file, src in [('PUF-style, PUF label', f'gen_puf_with_net_worth_{y}', 'puf'),
                                ('PUF-style, SCF label', f'gen_puf_with_net_worth_ownerlabel_{y}', 'puf'),
                                ('SCF-style', f'gen_scf_{y}', 'scf')]:
            g, x, nf = gen_net_worth(f'{gen}/{file}.parquet', src, vmap, y)
            w = np.full(len(x), wa.sum() / len(x))
            s = stats(x, w)
            Y[name] = {'stats': s, 'vs actual': compare(s, act), 'holders': holders_band(g),
                       'draws from fill rows': nf, 'records': len(g)}
        Y['holders, actual SCF wave'] = holders_raw(scf_t)
        Y['holders, IRS'] = irs[y]
        if y == 2015:
            Y['holders, PUF 2015 weighted'] = holders_raw(puf)
        S['years'][str(y)] = Y
    return S


# ---------------------------------------------------------------- writing

def f2(v):
    return '—' if v is None or (isinstance(v, float) and np.isnan(v)) else f'{v:.2f}'


def pct(d):
    return '/'.join(f'{100 * d[v]:.1f}' for v in INV)


def md_score(S):
    out = [f'# Run {S["run"]}: {S["describe"]}', '',
           f'Table: {S["table_meta"].get("table")}', '',
           f'Training: best validation loss {f2(S["best_val_loss"])} after {S["epochs_run"]} epochs.', '',
           'Net worth in each wave\'s interview-year dollars. Generated records have equal weights '
           'summing to the wave\'s weighted count, so the total ratio equals the mean ratio.', '']
    cols = ['actual SCF', 'PUF-style, PUF label', 'PUF-style, SCF label', 'SCF-style']
    keys = ['units (millions)', 'total net worth ($ trillion)', 'mean ($)', 'median ($)', 'share negative (%)',
            'top 10% share (%)', 'top 1% share (%)', 'top 0.1% share (%)', '90th percentile ($)',
            '99th percentile ($)', '99.9th percentile ($)']
    for y, Y in S['years'].items():
        out += [f'## Tax year {y} (SCF wave {int(y) + 1})', '',
                '| Statistic | ' + ' | '.join(cols) + ' |', '|---|' + '---:|' * len(cols)]
        for k in keys:
            vals = [Y['actual SCF'][k]] + [Y[c]['stats'][k] for c in cols[1:]]
            out.append(f'| {k} | ' + ' | '.join(wt.sig3(v, pct=k.endswith('(%)')) for v in vals) + ' |')
        out += ['', '| Ratio to actual | ' + ' | '.join(cols[1:]) + ' |', '|---|' + '---:|' * 3]
        for k in ['total ratio', 'median ratio', 'p99 ratio']:
            out.append(f'| {k} | ' + ' | '.join(f2(Y[c]['vs actual'][k]) for c in cols[1:]) + ' |')
        hcols = [('PUF-style generated', Y['PUF-style, PUF label']['holders']),
                 ('SCF-style generated', Y['SCF-style']['holders']),
                 ('actual SCF wave', Y['holders, actual SCF wave']),
                 ('IRS, share of returns', Y['holders, IRS'])]
        if 'holders, PUF 2015 weighted' in Y:
            hcols.append(('PUF 2015 (weighted)', Y['holders, PUF 2015 weighted']))
        out += ['', 'Holder shares (%, amount not zero):', '',
                '| Item | ' + ' | '.join(n for n, _ in hcols) + ' |', '|---|' + '---:|' * len(hcols)]
        for v in INV:
            out.append(f'| {v} | ' + ' | '.join(f'{100 * d[v]:.1f}' for _, d in hcols) + ' |')
        fills = {c: Y[c]['draws from fill rows'] for c in cols[1:]}
        out += ['', f'Net worth bands with no SCF record in this wave: {Y["net worth bands filled (no record)"]}; '
                    f'generated draws that landed in one: {fills}.', '']
    return '\n'.join(out)


def summary_rows():
    rows = []
    for T in RUNS:
        p = f'{ROOT}/score/{T}/score_years.json'
        if not os.path.exists(p):
            continue
        S = json.load(open(p))
        for y, Y in S['years'].items():
            a, b, c = Y['PUF-style, PUF label'], Y['PUF-style, SCF label'], Y['SCF-style']
            puf_ref = Y.get('holders, PUF 2015 weighted')
            rows.append(
                f'| {T} | {y} | {f2(a["vs actual"]["total ratio"])} / {f2(b["vs actual"]["total ratio"])} '
                f'| {f2(a["vs actual"]["median ratio"])} / {f2(b["vs actual"]["median ratio"])} '
                f'| {a["stats"]["top 1% share (%)"]:.1f} / {b["stats"]["top 1% share (%)"]:.1f} '
                f'({Y["actual SCF"]["top 1% share (%)"]:.1f}) '
                f'| {f2(a["vs actual"]["p99 ratio"])} '
                f'| {pct(a["holders"])} | {pct(Y["holders, IRS"])}'
                + (f' (PUF {pct(puf_ref)})' if puf_ref else '') + f' | {pct(Y["holders, actual SCF wave"])} '
                f'| {f2(c["vs actual"]["total ratio"])} / {f2(c["vs actual"]["median ratio"])} '
                f'| {pct(c["holders"])} |')
    return rows


HEADER = ('| Run | Tax year | Net worth total, ratio to SCF wave (PUF / SCF label) '
          '| Median ratio (PUF / SCF label) | Top 1% share, % (PUF / SCF label; actual) '
          '| p99 ratio (PUF label) | Holders % int/div/kg, PUF-style | IRS holders % '
          '| SCF wave holders % | SCF-style net worth: total / median ratio | SCF-style holders % |\n'
          '|---|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|')


def write_summary():
    with open(f'{RESULTS_MD}.lock', 'w') as lk:
        fcntl.flock(lk, fcntl.LOCK_EX)
        text = open(RESULTS_MD).read()
        a, b = text.index('<!-- TABLE START -->'), text.index('<!-- TABLE END -->')
        body = HEADER + '\n' + '\n'.join(summary_rows())
        text = text[:a] + '<!-- TABLE START -->\n' + body + '\n' + text[b:]
        open(RESULTS_MD, 'w').write(text)
    print(body)


def main():
    T = sys.argv[1]
    assert T in RUNS, T
    S = score(T)
    out = f'{ROOT}/score/{T}'
    os.makedirs(out, exist_ok=True)
    json.dump(S, open(f'{out}/score_years.json', 'w'), indent=1, default=float)
    md = md_score(S)
    open(f'{out}/score_years.md', 'w').write(md)
    print(md)
    write_summary()
    open(f'{ROOT}/markers/SCORE_YEARS_{T}_DONE', 'w').close()


if __name__ == '__main__':
    main()
