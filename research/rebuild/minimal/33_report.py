"""
33_report.py  (minimal example, step 33: the report tables. Status: 33_STATUS.md.)

Reads whatever step-33 outputs exist and writes step33/report.md:
  A. what ran (runs found, network size, passes);
  B. the PUF fit: each PUF-side variable, generated stack (2015 dollars, the
     stack's importance weights) against the real PUF side of table E33
     (filers plus crude non-filers, survey weights): share nonzero, share
     negative, mean, P50 / P90 / P99 of the nonzero amounts;
  C. relationships: by wage group, share with the item and mean, real
     against generated; pairs of items held together; two consistency
     checks on Social Security;
  D. net worth before and after the tilt, on both count bases, against SCF
     2019 and against step 31's cell method (Ks1-3), as 31_RESULTS.md;
  E. the tilt's move, records kept, and the largest miss on the 136 targets.
Environment: 33_env.sh (sourced by 33_cpu.sbatch).
"""
import importlib.util
import json
import os
import traceback

import numpy as np
import pandas as pd

HERE = 'research/rebuild/minimal'
R = '/nfs/roberts/scratch/pi_nrs36/jar335/minimal'
OUT = os.environ.get('STEP33_REPORT_OUT', f'{R}/step33')
RUNS = os.environ.get('STEP33_RUNS', 'Ns1,Ns2,Ns3,Bs1,Bs2').split(',')
NEW = ['business_income', 'retirement_gross', 'tax_exempt_interest', 'other_gains', 'ira_taxable',
       'pensions_taxable', 'ss_taxable', 'unemployment', 'alimony', 'state_refunds', 'adjustments']
PIECES = ['kg_st', 'kg_lt', 'kg_dist', 'kg_other']
FOLD = {'business_income': 1, 'other_gains': 1, 'ira_taxable': 1, 'pensions_taxable': 1, 'ss_taxable': 1,
        'unemployment': 1, 'alimony': 1, 'state_refunds': 1, 'adjustments': -1}
WAGE_GROUPS = [('none', 0, 1), ('$1-25k', 1, 25e3), ('$25-50k', 25e3, 50e3), ('$50-100k', 50e3, 1e5),
               ('$100-200k', 1e5, 2e5), ('$200k and over', 2e5, np.inf)]


def load(name, f):
    s = importlib.util.spec_from_file_location(name, f'{HERE}/{f}')
    m = importlib.util.module_from_spec(s)
    s.loader.exec_module(m)
    return m


def md(df, fmt=',.1f'):
    return df.to_markdown(index=False, floatfmt=fmt) if len(df) else '(nothing yet)'


def wq(x, w, probs):
    o = np.argsort(x, kind='stable')
    xs, F = x[o], np.cumsum(w[o]) / w.sum()
    idx = np.minimum(np.searchsorted(F, np.asarray(probs) - 1e-12, side='left'), len(xs) - 1)
    return xs[idx]


def runs_done():
    return [r for r in RUNS if os.path.exists(f'{R}/gen/gen_meta_s33_{r.lower()}.json')]


def real_puf_side():
    t = pd.read_parquet(f'{R}/data/stacked_E33.parquet')
    t = t[t.source == 'puf'].reset_index(drop=True)
    t['agi'] = t[['wages', 'interest', 'dividends', 'capital_gains', 'other_agi']].sum(axis=1) + \
        sum(s * t[v] for v, s in FOLD.items())
    t['remainder'] = t['other_agi']
    t['capital_gains_line13'] = t['capital_gains']
    return t, t['weight'].to_numpy(float)


def gen_2015(run, dollars):
    """A run's stack in 2015 dollars (PUF map 2015 of map/V33), importance weights."""
    os.environ['MINIMAL_MAP_DIR'] = f'{R}/map/V33'
    g = pd.read_parquet(f'{R}/gen/gen_puf_stack_s33_{run.lower()}.parquet')
    g = g[g['source'] == 'puf'].reset_index(drop=True)
    g, _ = dollars.draw_dollars(g, dollars.puf_maps(2015))
    g['remainder'] = g['other_agi_remainder']
    g['agi'] = g[['wages', 'interest', 'dividends', 'capital_gains', 'other_agi']].sum(axis=1)
    return g, g['weight'].to_numpy(float)


def gen_2015_step31(run, dollars):
    """Step 31's stack (cell method, table E27) in 2015 dollars with map/V27, for the variables it has."""
    os.environ['MINIMAL_MAP_DIR'] = f'{R}/map/V27'
    g = pd.read_parquet(f'{R}/gen/gen_puf_stack_s31_{run.lower()}.parquet')
    g = g[g['source'] == 'puf'].reset_index(drop=True)
    g, _ = dollars.draw_dollars(g, dollars.puf_maps(2015))
    g['agi'] = g[['wages', 'interest', 'dividends', 'capital_gains', 'other_agi']].sum(axis=1)
    return g, g['weight'].to_numpy(float)


def describe(x, w):
    x = np.asarray(x, float)
    assert not np.isnan(x).any()
    nz = x != 0
    out = {'share nonzero (%)': 100 * w[nz].sum() / w.sum(), 'share negative (%)': 100 * w[x < 0].sum() / w.sum(),
           'mean ($)': (w * x).sum() / w.sum()}
    q = wq(x[nz], w[nz], [0.5, 0.9, 0.99]) if nz.any() else [np.nan] * 3
    out.update({'P50 of nonzero ($)': q[0], 'P90 of nonzero ($)': q[1], 'P99 of nonzero ($)': q[2]})
    return out


FIT_VARS = ['wages', 'interest', 'dividends', 'capital_gains_line13'] + PIECES + NEW + ['remainder', 'agi']


def puf_fit(real, wr, gens):
    lines = ['## B. The PUF fit (2015 dollars; all PUF-side units: filers and crude non-filers)', '',
             'Real: table E33\'s PUF side with its weights. Generated: each run\'s 400,000-record stack with its '
             'importance weights (before any reweighting). "Remainder" is other income after the eleven items; '
             '"agi" is computed from the parts on both sides. Percentiles are of the nonzero amounts.', '']
    for v in FIT_VARS:
        rows = [{'file': 'PUF (real)', **describe(real[v], wr)}]
        for run, (g, wg) in gens.items():
            if v in g:
                rows.append({'file': run, **describe(g[v], wg)})
        lines += [f'### {v}', '', md(pd.DataFrame(rows), ',.1f'), '']
    # one summary table: generated mean of the base runs against real, ratio
    base = [r for r in gens if r.startswith('N')]
    summ = []
    for v in FIT_VARS:
        d = describe(real[v], wr)
        row = {'variable': v, 'real share nonzero (%)': d['share nonzero (%)']}
        for lab, rs in [('step 31', [r for r in gens if r.startswith('K') and v in gens[r][0]]),
                        ('base network', base), ('bigger network', [r for r in gens if r.startswith('B')])]:
            if not rs:
                continue
            gs = [describe(gens[r][0][v], gens[r][1]) for r in rs]
            row[f'{lab}: share nonzero (%)'] = np.mean([x['share nonzero (%)'] for x in gs])
            for k in ['mean ($)', 'P50 of nonzero ($)', 'P90 of nonzero ($)', 'P99 of nonzero ($)']:
                row[f'{lab}: {k.split(" (")[0]}, ratio to real'] = np.mean([x[k] for x in gs]) / d[k] if d[k] else np.nan
        summ.append(row)
    lines = lines[:4] + ['### Summary: generated against real (ratios; base network = mean of its seeds)', '',
                         md(pd.DataFrame(summ), ',.3f'), '', 'Step 31 rows: Ks1-3 (table E27; agi there is its own AGI '
                         'computed from its parts).', ''] + lines[4:]
    misc = []
    for name, (x, w) in [('PUF (real)', (real, wr))] + [(r, gv) for r, gv in gens.items()]:
        misc.append({'file': name, 'married (%)': 100 * (w * x['married']).sum() / w.sum(),
                     'mean dependents': (w * x['n_dep']).sum() / w.sum(),
                     'filers (%)': 100 * (w * x['filer']).sum() / w.sum()})
    lines += ['### married, dependents, filers', '', md(pd.DataFrame(misc), ',.2f'), '']
    return lines


def relationships(real, wr, gens):
    lines = ['## C. Relationships', '']
    items = ['business_income', 'retirement_gross', 'ss_taxable', 'pensions_taxable', 'ira_taxable',
             'tax_exempt_interest', 'adjustments', 'dividends']
    for v in items:
        rows = []
        for name, (x, w) in [('PUF (real)', (real, wr))] + [(r, gv) for r, gv in gens.items()]:
            if v not in x:
                continue
            row = {'file': name}
            wg = x['wages'].to_numpy(float)
            for lab, lo, hi in WAGE_GROUPS:
                m = (wg >= lo) & (wg < hi)
                xv = x[v].to_numpy(float)[m]
                row[f'{lab}: with it (%)'] = 100 * w[m][xv != 0].sum() / w[m].sum()
                row[f'{lab}: mean ($)'] = (w[m] * xv).sum() / w[m].sum()
            rows.append(row)
        lines += [f'### {v} by wage group', '', md(pd.DataFrame(rows), ',.1f'), '']
    pairs = [('business_income', 'adjustments'), ('business_income', 'wages'), ('retirement_gross', 'ss_taxable'),
             ('retirement_gross', 'pensions_taxable'), ('retirement_gross', 'ira_taxable'),
             ('tax_exempt_interest', 'dividends'), ('tax_exempt_interest', 'interest'),
             ('business_income', 'capital_gains_line13'), ('unemployment', 'wages'), ('state_refunds', 'wages')]
    rows = []
    for a, b in pairs:
        row = {'pair': f'{a} and {b}'}
        for name, (x, w) in [('PUF (real)', (real, wr))] + [(r, gv) for r, gv in gens.items()]:
            if a not in x or b not in x:
                continue
            xa, xb = x[a].to_numpy(float) != 0, x[b].to_numpy(float) != 0
            row[f'{name}: both (%)'] = 100 * w[xa & xb].sum() / w.sum()
            row[f'{name}: second among holders of the first (%)'] = 100 * w[xa & xb].sum() / w[xa].sum() if xa.any() else np.nan
        rows.append(row)
    lines += ['### Items held together (% of units)', '', md(pd.DataFrame(rows), ',.1f'), '']
    rows = []
    for name, (x, w) in [('PUF (real)', (real, wr))] + [(r, gv) for r, gv in gens.items()]:
        if 'ss_taxable' not in x:
            continue
        st, rg = x['ss_taxable'].to_numpy(float), x['retirement_gross'].to_numpy(float)
        a = x['agi'].to_numpy(float)
        rows.append({'file': name,
                     'taxable Social Security with no Social Security or pensions (% of units)': 100 * w[(st > 0) & (rg == 0)].sum() / w.sum(),
                     'taxable Social Security above 85% of Social Security plus pensions (% of units)': 100 * w[st > 0.85 * rg + 1].sum() / w.sum(),
                     'AGI below zero (% of units)': 100 * w[a < 0].sum() / w.sum(),
                     'AGI P50 ($)': wq(a, w, [0.5])[0], 'AGI P90 ($)': wq(a, w, [0.9])[0], 'AGI P99 ($)': wq(a, w, [0.99])[0]})
    lines += ['### Consistency checks and AGI', '', 'The first two cannot happen on a real return. '
              '(Gross Social Security is not on the file by itself: retirement_gross adds gross pensions, so the second '
              'check is a loose bound.)', '', md(pd.DataFrame(rows), ',.2f'), '']
    return lines


def tilt_sections():
    rep27 = load('rep27', '27_report.py')
    rep29 = load('rep29', '29_report.py')
    wt = load('wt', '06_wealth_tables.py')
    tm = wt.tiltmod
    dollars = tm.dollars
    cols, moves = {}, []
    u19, x19, w19, _ = wt.scf2019()
    cols['SCF 2019, all units'] = rep27.stats(u19, np.asarray(w19, float), np.asarray(x19, float) * rep29.TO_2019)
    specs = [('step31', f'Ks{k}', f'{R}/map/V27', 's31') for k in (1, 2, 3)] + \
            [('step33', r, f'{R}/map/V33', 's33') for r in RUNS]
    for base, sfx in [('scf_units', ''), ('file_units', '_file')]:
        for step, run, mapdir, pre in specs:
            v = f'{pre}_{run.lower()}'
            tag = f'{step}/{v}{sfx}' if not (step == 'step31' and base == 'scf_units') else f'step31/{v}'
            if not os.path.exists(f'{R}/tilt/{tag}/summary_{v}.csv'):
                continue
            os.environ['MINIMAL_MAP_DIR'] = mapdir
            os.environ['GEN_FILE'] = f'{R}/gen/gen_puf_stack_{v}.parquet'
            g = tm.generated_2019()
            wts = pd.read_parquet(f'{R}/tilt/{tag}/weights_{v}.parquet')
            assert (wts['record_id'].to_numpy() == g['record_id'].to_numpy()).all()
            nw = g['net_worth'].to_numpy(float) * rep29.TO_2019
            if base == 'scf_units':
                cols[f'{run}, before'] = rep27.stats(g, wts['weight_before'].to_numpy(), nw)
            cols[f'{run}, after ({base})'] = rep27.stats(g, wts['weight_after'].to_numpy(), nw)
            s = pd.read_csv(f'{R}/tilt/{tag}/summary_{v}.csv')
            moves.append({'step': step, 'run': run, 'wealth count base': base,
                          'move, 136 IRS (nats)': s['kl_nats'].iloc[0],
                          'move, 136 IRS + 8 wealth (nats)': s['kl_nats'].iloc[1],
                          'records kept, effective (%)': 100 * s['ess_ratio'].iloc[1],
                          'largest miss on the 136 targets (%)': s['max_abs_gap_136_targets_pct'].iloc[1]})
    tei = f'{R}/tilt/step33/s33_ns1_tei/summary_s33_ns1.csv'
    if os.path.exists(tei):
        s = pd.read_csv(tei)
        moves.append({'step': 'step33', 'run': 'Ns1', 'wealth count base': 'scf_units, plus tax-exempt interest targets',
                      'move, 136 IRS (nats)': s['kl_nats'].iloc[0], 'move, 136 IRS + 8 wealth (nats)': s['kl_nats'].iloc[1],
                      'records kept, effective (%)': 100 * s['ess_ratio'].iloc[1],
                      'largest miss on the 136 targets (%)': s['max_abs_gap_136_targets_pct'].iloc[1]})
    mv = pd.DataFrame(moves)
    lines = ['## D. The tilt to 2019: move and targets', '']
    if len(mv):
        mv['group'] = np.where(mv['run'].str.startswith('B'), 'step 33, bigger network',
                               np.where(mv['step'] == 'step33', 'step 33, base network', 'step 31, cell method'))
        lines += ['Mean over seeds:', '', md(mv.groupby(['group', 'wealth count base'], sort=False)
                                             .mean(numeric_only=True).reset_index(), '.4f'), '',
                  'Every run:', '', md(mv.drop(columns=['group']), '.4f'), '']
    t = pd.DataFrame(cols)
    groups = {'step 31 (Ks)': lambda c: c.startswith('Ks'), 'step 33 base (Ns)': lambda c: c.startswith('Ns'),
              'step 33 bigger (Bs1)': lambda c: c.startswith('Bs')}
    for gname, f in groups.items():
        for when in ['before', 'after (scf_units)', 'after (file_units)']:
            cs = [c for c in t.columns if f(c) and c.endswith(when)]
            if cs:
                t[f'{gname}, mean of {len(cs)}, {when}'] = t[cs].mean(axis=1)
    keep = ['SCF 2019, all units'] + [c for c in t.columns if 'mean of' in c]
    lines += ['## E. Net worth before and after the tilt (nominal 2019 dollars)', ''] + \
        rep27.fmt_table(t[keep]) + ['', '### Every run', ''] + rep27.fmt_table(t) + ['']
    return lines


def groups_of(x, w):
    """none / loss / positive by weighted rank among positives: bottom half, 50-90, top 10%."""
    x = np.asarray(x, float)
    out = np.full(len(x), 'none', dtype=object)
    out[x < 0] = 'loss'
    pos = x > 0
    if pos.any():
        q50, q90 = wq(x[pos], w[pos], [0.5, 0.9])
        out[pos & (x <= q50)] = 'positive, bottom half'
        out[pos & (x > q50) & (x <= q90)] = 'positive, 50th-90th percentile'
        out[pos & (x > q90)] = 'positive, top 10%'
    return out


def wealth_by_items():
    """Mean net worth by business income, retirement income and tax-exempt
    interest group: SCF tax year 2018 (2019 wave) rows of table E33 as
    reported, against each step-33 run before and after the tilt (SCF-units
    base). Groups by rank within each file, since business income is placed
    on the PUF's terms by rank."""
    rep29 = load('rep29b', '29_report.py')
    wt = load('wtb', '06_wealth_tables.py')
    tm = wt.tiltmod
    t = pd.read_parquet(f'{R}/data/stacked_E33.parquet')
    s = t[(t.source == 'scf') & (t.year == 2018)]
    files = {'SCF 2019 wave, as reported': (s, s['weight'].to_numpy(float), s['net_worth'].to_numpy(float))}
    for r in runs_done():
        v = f's33_{r.lower()}'
        p = f'{R}/tilt/step33/{v}/weights_{v}.parquet'
        if not os.path.exists(p):
            continue
        os.environ['MINIMAL_MAP_DIR'] = f'{R}/map/V33'
        os.environ['GEN_FILE'] = f'{R}/gen/gen_puf_stack_{v}.parquet'
        g = tm.generated_2019()
        wts = pd.read_parquet(p)
        nw = g['net_worth'].to_numpy(float) * rep29.TO_2019
        files[f'{r}, before'] = (g, wts['weight_before'].to_numpy(float), nw)
        files[f'{r}, after'] = (g, wts['weight_after'].to_numpy(float), nw)
    lines = ['## F. Net worth by the new shared items (nominal 2019 dollars, mean per unit)', '',
             'SCF: the 2019 wave\'s units in table E33 (their own net worth; items as reported, tax year 2018). '
             'Generated: each run\'s file in 2019 dollars, before and after the tilt (wealth counts on the SCF\'s '
             'units). Groups are by rank within each file, so the SCF\'s and the file\'s dollar levels need not agree.', '']
    for item in ['business_income', 'retirement_gross', 'tax_exempt_interest']:
        rows = {}
        for name, (x, w, nw) in files.items():
            gr = groups_of(x[item].to_numpy(float), w)
            col = {}
            for lab in ['none', 'loss', 'positive, bottom half', 'positive, 50th-90th percentile', 'positive, top 10%']:
                m = gr == lab
                if m.any():
                    col[f'{lab}: share of units (%)'] = 100 * w[m].sum() / w.sum()
                    col[f'{lab}: mean net worth ($)'] = (w[m] * nw[m]).sum() / w[m].sum()
            rows[name] = col
        tab = pd.DataFrame(rows)
        lines += [f'### {item}', '', md(tab.reset_index().rename(columns={'index': ''}), ',.1f'), '']
    return lines


def main():
    os.makedirs(OUT, exist_ok=True)
    lines = ['# Step 33: report tables', '', 'Written by 33_report.py from what exists on disk.', '']
    done = runs_done()
    meta = []
    for r in done:
        p = f'{R}/model/step33/{r}/train_meta.json'
        if os.path.exists(p):
            m = json.load(open(p))
            fl = m.get('flags', {})
            meta.append({'run': r, 'seed': m.get('seed'), 'passes': m.get('max_epochs'),
                         'layers': fl.get('MINIMAL_LAYERS', '4'), 'width': fl.get('MINIMAL_WIDTH', '128'),
                         'minutes': m.get('minutes'), 'last held-back loss': m.get('last_val_loss')})
    lines += ['## A. Runs found', '', md(pd.DataFrame(meta), '.0f') if meta else str(done), '']
    gens, real, wr = {}, None, None
    try:
        dollars = load('dollars33', '04_dollars.py')
        real, wr = real_puf_side()
        for r in done:
            gens[r] = gen_2015(r, dollars)
        for k in (1, 2, 3):
            if os.path.exists(f'{R}/gen/gen_puf_stack_s31_ks{k}.parquet'):
                gens[f'Ks{k}'] = gen_2015_step31(f'Ks{k}', dollars)
    except Exception as ex:
        lines += [f'**Loading failed: {ex!r}**', '```', traceback.format_exc(), '```']
    for part in [lambda: puf_fit(real, wr, gens), lambda: relationships(real, wr, gens), tilt_sections,
                 wealth_by_items]:
        try:
            lines += part()
        except Exception as ex:
            lines += ['', f'**Section failed: {ex!r}**', '', '```', traceback.format_exc(), '```', '']
        open(f'{OUT}/report.md', 'w').write('\n'.join(lines) + '\n')
    print('\n'.join(lines))


if __name__ == '__main__':
    main()
