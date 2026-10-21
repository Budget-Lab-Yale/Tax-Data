"""
34_report.py  (minimal example, step 34: the report tables. Status: 34_STATUS.md.)

Reads whatever step-34 outputs exist and writes step34/report.md:
  A. what ran (runs found, passes, minutes, held-back loss);
  B. the PUF fit of every business income part and of the business income
     total computed from them, generated stack (2015 dollars, importance
     weights) against the real PUF side of table E34 (filers plus crude
     non-filers): share nonzero, share negative, mean, and P10 / P50 / P90 /
     P99 separately for the positive and the negative amounts; then the
     summary of every other PUF-side variable as in step 33;
  C. how the parts combine on one record (pairs of holdings, number of parts
     held), real against generated;
  D. business income total by wage group: real, step 33 (drawn as one
     item), step 34 (computed from the parts);
  E. where the business losses come from: units with a net business loss,
     the share of them with each kind of loss and the share of the gross
     loss dollars from each part;
  F. the tilt (move, records kept, largest miss) and net worth before and
     after on both count bases, step 33 base runs against step 34;
  G. net worth by business income group, SCF 2019 wave against the runs.
Environment: 34_env.sh (sourced by 34_cpu.sbatch).
"""
import importlib.util
import json
import os
import traceback

import numpy as np
import pandas as pd

HERE = 'research/rebuild/minimal'
R = '/nfs/roberts/scratch/pi_nrs36/jar335/minimal'
OUT = os.environ.get('STEP34_REPORT_OUT', f'{R}/step34')
RUNS = os.environ.get('STEP34_RUNS', 'Ns1,Ns2,Ns3').split(',')   # step 34 runs; one name only via --export
RUNS33 = ['Ns1', 'Ns2', 'Ns3']   # step 33's base runs, the comparison
SHEET = json.load(open(f'{HERE}/34_settings_scf.json'))['step34_parts']
PARTS = [p for p in SHEET if p != '_what']
SIGN = {p: SHEET[p]['sign'] for p in PARTS}
FIELD = {p: SHEET[p]['puf_field'] for p in PARTS}


def load(name, f):
    s = importlib.util.spec_from_file_location(name, f'{HERE}/{f}')
    mod = importlib.util.module_from_spec(s)
    s.loader.exec_module(mod)
    return mod


rep33 = load('rep33', '33_report.py')
md, wq, WAGE_GROUPS = rep33.md, rep33.wq, rep33.WAGE_GROUPS
FOLD = rep33.FOLD


def runs_done(pre):
    return [r for r in (RUNS33 if pre == 's33' else RUNS) if os.path.exists(f'{R}/gen/gen_meta_{pre}_{r.lower()}.json')]


def real_puf_side():
    t = pd.read_parquet(f'{R}/data/stacked_E34.parquet')
    t = t[t.source == 'puf'].reset_index(drop=True)
    t['agi'] = t[['wages', 'interest', 'dividends', 'capital_gains', 'other_agi']].sum(axis=1) + \
        sum(s * t[v] for v, s in FOLD.items())
    t['remainder'] = t['other_agi']
    t['capital_gains_line13'] = t['capital_gains']
    chk = sum(SIGN[p] * t[p] for p in PARTS) - t['business_income']
    assert np.abs(chk).max() < 0.01, 'parts do not add to business_income on the real side'
    return t, t['weight'].to_numpy(float)


def gen_2015(pre, run, dollars, mapdir):
    os.environ['MINIMAL_MAP_DIR'] = mapdir
    g = pd.read_parquet(f'{R}/gen/gen_puf_stack_{pre}_{run.lower()}.parquet')
    g = g[g['source'] == 'puf'].reset_index(drop=True)
    g, _ = dollars.draw_dollars(g, dollars.puf_maps(2015))
    g['remainder'] = g['other_agi_remainder']
    g['agi'] = g[['wages', 'interest', 'dividends', 'capital_gains', 'other_agi']].sum(axis=1)
    if pre == 's34':
        chk = sum(SIGN[p] * g[p] for p in PARTS) - g['business_income']
        assert np.abs(chk).max() < 0.01, 'computed business_income is not the sum of the parts'
    return g, g['weight'].to_numpy(float)


def describe(x, w):
    x = np.asarray(x, float)
    assert not np.isnan(x).any()
    out = {'nonzero (%)': 100 * w[x != 0].sum() / w.sum(), 'negative (%)': 100 * w[x < 0].sum() / w.sum(),
           'mean ($)': (w * x).sum() / w.sum()}
    for lab, sel in [('gains', x > 0), ('losses', x < 0)]:
        q = wq(x[sel], w[sel], [0.1, 0.5, 0.9, 0.99]) if sel.any() else [np.nan] * 4
        out.update({f'{lab} P10': q[0], f'{lab} P50': q[1], f'{lab} P90': q[2], f'{lab} P99': q[3]})
    return out


def part_fit(real, wr, g34, g33):
    lines = ['## B. The PUF fit of the parts (2015 dollars; all PUF-side units: filers and crude non-filers)', '',
             'Real: table E34\'s PUF side with its weights. Generated: each step-34 run\'s 400,000-record stack with its '
             'importance weights, before any reweighting. Loss fields the PUF stores as positive numbers are shown as '
             'positive numbers (their sign in the sum is -1). Percentiles are of the positive amounts ("gains") and of the '
             'negative amounts ("losses") separately. business_income: real = the PUF; step 34 = computed from the drawn '
             'parts; step 33 = drawn as one item.', '']
    summ = []
    for v in PARTS + ['business_income']:
        rows = [{'file': 'PUF (real)', **describe(real[v], wr)}]
        for r, (g, wg) in g34.items():
            rows.append({'file': f'step 34 {r}', **describe(g[v], wg)})
        if v == 'business_income':
            for r, (g, wg) in g33.items():
                rows.append({'file': f'step 33 {r}', **describe(g[v], wg)})
        head = f'### {v}' + (f' ({FIELD[v]}, sign {SIGN[v]:+d})' if v in SIGN else ' (computed total)')
        lines += [head, '', md(pd.DataFrame(rows), ',.1f'), '']
        d = describe(real[v], wr)
        gs = [describe(g[v], wg) for g, wg in g34.values()]
        if gs:
            row = {'part': v, 'real nonzero (%)': d['nonzero (%)'],
                   'generated nonzero (%)': np.mean([x['nonzero (%)'] for x in gs]),
                   'real negative (%)': d['negative (%)'],
                   'generated negative (%)': np.mean([x['negative (%)'] for x in gs]),
                   'mean, ratio': np.mean([x['mean ($)'] for x in gs]) / d['mean ($)'] if d['mean ($)'] else np.nan}
            for k in ['gains P50', 'gains P90', 'gains P99', 'losses P50', 'losses P90']:
                row[f'{k}, ratio'] = np.mean([x[k] for x in gs]) / d[k] if d[k] and not np.isnan(d[k]) else np.nan
            summ.append(row)
    lines = lines[:4] + ['### Summary: step 34 base network (mean of its seeds) against the PUF', '',
                         md(pd.DataFrame(summ), ',.3f'), ''] + lines[4:]
    return lines


def other_fit(real, wr, g34, g33):
    vs = ['wages', 'interest', 'dividends', 'capital_gains_line13'] + rep33.PIECES + \
        [v for v in rep33.NEW if v != 'business_income'] + ['remainder', 'agi']
    rows = []
    for v in vs:
        d = rep33.describe(real[v], wr)
        row = {'variable': v, 'real nonzero (%)': d['share nonzero (%)']}
        for lab, gens in [('step 33', g33), ('step 34', g34)]:
            gs = [rep33.describe(g[v], wg) for g, wg in gens.values() if v in g]
            if gs:
                row[f'{lab} nonzero (%)'] = np.mean([x['share nonzero (%)'] for x in gs])
                for k in ['mean ($)', 'P50 of nonzero ($)', 'P90 of nonzero ($)', 'P99 of nonzero ($)']:
                    row[f'{lab} {k.split(" (")[0]}, ratio'] = np.mean([x[k] for x in gs]) / d[k] if d[k] else np.nan
        rows.append(row)
    return ['### The other PUF-side variables: step 33 and step 34 base runs (mean of seeds) against the PUF', '',
            md(pd.DataFrame(rows), ',.3f'), '']


def holdings(x):
    """Indicators of the kinds of business income a unit has."""
    f = lambda v: x[v].to_numpy(float)
    return {
        'Schedule C profit': f('sch_c') > 0, 'Schedule C loss': f('sch_c') < 0,
        'partnership income': (f('part_pass_inc') > 0) | (f('part_nonpass_inc') > 0),
        'partnership loss': (f('part_pass_loss') > 0) | (f('part_nonpass_loss') > 0),
        'S corporation income': (f('scorp_pass_inc') > 0) | (f('scorp_nonpass_inc') > 0),
        'S corporation loss': (f('scorp_pass_loss') > 0) | (f('scorp_nonpass_loss') > 0),
        'rent or royalty income': f('rent_inc') > 0, 'rent or royalty loss': f('rent_loss') > 0,
        'estate or trust': (f('estate_inc') > 0) | (f('estate_loss') > 0),
        'farm (Schedule F)': f('farm') != 0,
    }


def combinations(real, wr, g34):
    files = [('PUF (real)', real, wr)] + [(f'step 34 {r}', g, wg) for r, (g, wg) in g34.items()]
    H = {name: holdings(x) for name, x, _ in files}
    W = {name: w for name, _, w in files}
    keys = list(H['PUF (real)'])
    rows = []
    for i, a in enumerate(keys):
        for b in keys[i + 1:]:
            row = {'pair': f'{a} and {b}'}
            for name in H:
                both = H[name][a] & H[name][b]
                row[name] = 100 * W[name][both].sum() / W[name].sum()
            gen = [row[n] for n in H if n != 'PUF (real)']
            row['generated mean'] = np.mean(gen) if gen else np.nan
            row['ratio'] = row['generated mean'] / row['PUF (real)'] if row['PUF (real)'] else np.nan
            rows.append(row)
    pairs = pd.DataFrame(rows).sort_values('PUF (real)', ascending=False)
    rows = []
    for name in H:
        cnt = sum(H[name][k].astype(int) for k in keys)
        w = W[name]
        row = {'file': name}
        for k, lab in [(0, 'none'), (1, 'one'), (2, 'two'), (3, 'three or more')]:
            sel = cnt == k if k < 3 else cnt >= 3
            row[f'{lab} (%)'] = 100 * w[sel].sum() / w.sum()
        rows.append(row)
    single = []
    for k in keys:
        row = {'kind': k}
        for name in H:
            row[name] = 100 * W[name][H[name][k]].sum() / W[name].sum()
        single.append(row)
    return ['## C. How the parts combine on one record (% of all PUF-side units)', '',
            'Kinds: Schedule C profit or loss (sch_c above or below zero); partnership income (passive or nonpassive '
            'income field above zero); partnership loss (passive or nonpassive loss field above zero); the same for S '
            'corporations; rent or royalty net income or net loss; estate or trust income or loss; farm (Schedule F) '
            'nonzero. A unit can be in several kinds.', '',
            '### Each kind', '', md(pd.DataFrame(single), ',.2f'), '',
            '### Number of kinds held', '', md(pd.DataFrame(rows), ',.2f'), '',
            '### Pairs of kinds held together, most common first on the PUF', '', md(pairs, ',.2f'), '']


def by_wage_group(real, wr, g34, g33):
    rows = []
    files = [('PUF (real)', real, wr)] + [(f'step 33 {r}', g, w) for r, (g, w) in g33.items()] + \
        [(f'step 34 {r}', g, w) for r, (g, w) in g34.items()]
    for name, x, w in files:
        wg = x['wages'].to_numpy(float)
        b = x['business_income'].to_numpy(float)
        for lab, lo, hi in WAGE_GROUPS:
            m = (wg >= lo) & (wg < hi)
            bm, wm = b[m], w[m]
            pos, neg = bm > 0, bm < 0
            rows.append({'file': name, 'wage group': lab, 'units (%)': 100 * wm.sum() / w.sum(),
                         'with income (%)': 100 * wm[pos].sum() / wm.sum(), 'with a loss (%)': 100 * wm[neg].sum() / wm.sum(),
                         'mean ($)': (wm * bm).sum() / wm.sum(),
                         'P50 of income ($)': wq(bm[pos], wm[pos], [0.5])[0] if pos.any() else np.nan,
                         'P90 of income ($)': wq(bm[pos], wm[pos], [0.9])[0] if pos.any() else np.nan,
                         'P50 of losses ($)': wq(bm[neg], wm[neg], [0.5])[0] if neg.any() else np.nan})
    t = pd.DataFrame(rows)
    t['group'] = t['file'].str.replace(r' Ns\d$', '', regex=True)
    mean = t.groupby(['group', 'wage group'], sort=False).mean(numeric_only=True).reset_index()
    return ['## D. Business income total by wage group (2015 dollars)', '',
            'Real: the PUF side of table E34. Step 33: business income drawn as one item. Step 34: computed from the '
            'drawn parts. Means of the three seeds.', '', md(mean, ',.1f'), '',
            '### Every run', '', md(t.drop(columns=['group']), ',.1f'), '']


def loss_sources(real, wr, g34):
    lines = ['## E. Where the business losses come from (2015 dollars)', '',
             'Units with a net business loss: business income (Schedules C, E and F together) below zero. For each part, '
             'the share of these units on which the part pulls the total down (a loss field above zero, or a signed field '
             'below zero), and the part\'s share of all the negative dollars on these units ("gross losses"). Positive '
             'parts on the same units (offsets) are shown as a share of the gross losses too.', '']
    files = [('PUF (real)', real, wr)] + [(f'step 34 {r}', g, w) for r, (g, w) in g34.items()]
    out = {}
    head = []
    for name, x, w in files:
        b = x['business_income'].to_numpy(float)
        m = b < 0
        wm = w[m]
        contrib = {p: SIGN[p] * x[p].to_numpy(float)[m] for p in PARTS}
        gross_neg = sum(np.minimum(c, 0) for c in contrib.values())
        G = -(wm * gross_neg).sum()
        head.append({'file': name, 'units with a net business loss (% of all units)': 100 * wm.sum() / w.sum(),
                     'net business loss on them ($ billion, at these weights)': (wm * b[m]).sum() / 1e9,
                     'gross losses ($ billion)': G / 1e9})
        col = {}
        for p in PARTS:
            c = contrib[p]
            col[(p, 'units with it pulling down (%)')] = 100 * wm[c < 0].sum() / wm.sum()
            col[(p, 'share of gross losses (%)')] = 100 * -(wm * np.minimum(c, 0)).sum() / G
            col[(p, 'offsets, as % of gross losses')] = 100 * (wm * np.maximum(c, 0)).sum() / G
        out[name] = col
    t = pd.DataFrame(out)
    gen = [c for c in t.columns if c != 'PUF (real)']
    if gen:
        t['generated mean'] = t[gen].mean(axis=1)
    t.index = pd.MultiIndex.from_tuples(t.index)
    for stat in ['units with it pulling down (%)', 'share of gross losses (%)', 'offsets, as % of gross losses']:
        s = t.xs(stat, level=1).reset_index().rename(columns={'index': 'part'})
        lines += [f'### {stat}', '', md(s, ',.1f'), '']
    note = ('The dollar totals use the file\'s own weights: for the PUF the survey weights (filers plus crude '
            'non-filers, 2015); for a generated stack the importance weights, which sum to the stack size, so only '
            'its shares are comparable.')
    return lines[:4] + [md(pd.DataFrame(head), ',.2f'), '', note, ''] + lines[4:]


def tilt_sections():
    rep27 = load('rep27', '27_report.py')
    rep29 = load('rep29', '29_report.py')
    wt = load('wt', '06_wealth_tables.py')
    tm = wt.tiltmod
    cols, moves = {}, []
    u19, x19, w19, _ = wt.scf2019()
    cols['SCF 2019, all units'] = rep27.stats(u19, np.asarray(w19, float), np.asarray(x19, float) * rep29.TO_2019)
    specs = [('step33', r, f'{R}/map/V33', 's33') for r in RUNS33] + [('step34', r, f'{R}/map/V34', 's34') for r in RUNS]
    for base, sfx in [('scf_units', ''), ('file_units', '_file'), ('scf_units + business', '_bus'),
                      ('scf_units + business, pre-scaled', '_bus_ps'),
                      ('file_units + business, pre-scaled', '_file_bus_ps')]:
        for step, run, mapdir, pre in specs:
            v = f'{pre}_{run.lower()}'
            tag = f'{step}/{v}{sfx}'
            if not os.path.exists(f'{R}/tilt/{tag}/summary_{v}.csv'):
                continue
            os.environ['MINIMAL_MAP_DIR'] = mapdir
            os.environ['GEN_FILE'] = f'{R}/gen/gen_puf_stack_{v}.parquet'
            g = tm.generated_2019()
            wts = pd.read_parquet(f'{R}/tilt/{tag}/weights_{v}.parquet')
            assert (wts['record_id'].to_numpy() == g['record_id'].to_numpy()).all()
            nw = g['net_worth'].to_numpy(float) * rep29.TO_2019
            lab = f'{step[-2:]} {run}'
            if base == 'scf_units':
                cols[f'{lab}, before'] = rep27.stats(g, wts['weight_before'].to_numpy(), nw)
            cols[f'{lab}, after ({base})'] = rep27.stats(g, wts['weight_after'].to_numpy(), nw)
            s = pd.read_csv(f'{R}/tilt/{tag}/summary_{v}.csv')
            moves.append({'step': step, 'run': run, 'wealth count base': base,
                          'move, 136 IRS (nats)': s['kl_nats'].iloc[0],
                          'move, 136 IRS + 8 wealth (nats)': s['kl_nats'].iloc[1],
                          'records kept, effective (%)': 100 * s['ess_ratio'].iloc[1],
                          'largest miss on the 136 targets (%)': s['max_abs_gap_136_targets_pct'].iloc[1]})
    mv = pd.DataFrame(moves)
    lines = ['## F. The tilt to 2019: move and targets', '',
             '"Move" is the distance the tilt moves the weights (Kullback-Leibler divergence, in nats); "records kept, '
             'effective" is the effective sample size after the tilt as a share of the records.', '']
    if len(mv):
        lines += ['Mean over seeds:', '', md(mv.groupby(['step', 'wealth count base'], sort=False)
                                             .mean(numeric_only=True).reset_index(), '.4f'), '',
                  'Every run:', '', md(mv, '.4f'), '']
    t = pd.DataFrame(cols)
    for step in ['33', '34']:
        for when in ['before', 'after (scf_units)', 'after (file_units)', 'after (scf_units + business)',
                     'after (scf_units + business, pre-scaled)', 'after (file_units + business, pre-scaled)']:
            cs = [c for c in t.columns if c.startswith(step + ' ') and c.endswith(when)]
            if cs:
                t[f'step {step}, mean of {len(cs)}, {when}'] = t[cs].mean(axis=1)
    keep = ['SCF 2019, all units'] + [c for c in t.columns if 'mean of' in c]
    lines += ['## F2. Net worth before and after the tilt (nominal 2019 dollars)', ''] + \
        rep27.fmt_table(t[keep]) + ['', '### Every run', ''] + rep27.fmt_table(t) + ['']
    return lines


def wealth_by_business():
    rep29 = load('rep29b', '29_report.py')
    wt = load('wtb', '06_wealth_tables.py')
    tm = wt.tiltmod
    t = pd.read_parquet(f'{R}/data/stacked_E34.parquet', columns=['source', 'year', 'weight', 'business_income', 'net_worth'])
    s = t[(t.source == 'scf') & (t.year == 2018)]
    files = {'SCF 2019 wave, as reported': (s, s['weight'].to_numpy(float), s['net_worth'].to_numpy(float))}
    for step, pre, mapdir in [('step33', 's33', f'{R}/map/V33'), ('step34', 's34', f'{R}/map/V34')]:
        for r in runs_done(pre):
            v = f'{pre}_{r.lower()}'
            p = f'{R}/tilt/{step}/{v}/weights_{v}.parquet'
            if not os.path.exists(p):
                continue
            os.environ['MINIMAL_MAP_DIR'] = mapdir
            os.environ['GEN_FILE'] = f'{R}/gen/gen_puf_stack_{v}.parquet'
            g = tm.generated_2019()
            wts = pd.read_parquet(p)
            nw = g['net_worth'].to_numpy(float) * rep29.TO_2019
            files[f'{step} {r}, before'] = (g, wts['weight_before'].to_numpy(float), nw)
            files[f'{step} {r}, after'] = (g, wts['weight_after'].to_numpy(float), nw)
    rows = {}
    for name, (x, w, nw) in files.items():
        gr = rep33.groups_of(x['business_income'].to_numpy(float), w)
        col = {}
        for lab in ['none', 'loss', 'positive, bottom half', 'positive, 50th-90th percentile', 'positive, top 10%']:
            m = gr == lab
            if m.any():
                col[f'{lab}: share of units (%)'] = 100 * w[m].sum() / w.sum()
                col[f'{lab}: mean net worth ($)'] = (w[m] * nw[m]).sum() / w[m].sum()
        rows[name] = col
    tab = pd.DataFrame(rows)
    return ['## G. Net worth by business income group (nominal 2019 dollars, mean per unit)', '',
            'SCF: the 2019 wave\'s units in table E34 (business income as reported, tax year 2018). Generated: each run\'s '
            'file in 2019 dollars, before and after the tilt (SCF-units base); step 34\'s business income is computed '
            'from its parts. Groups are by rank within each file.', '',
            md(tab.reset_index().rename(columns={'index': ''}), ',.1f'), '']


def main():
    os.makedirs(OUT, exist_ok=True)
    lines = ['# Step 34: report tables', '', 'Written by 34_report.py from what exists on disk.', '']
    meta = []
    for step, pre in [('step33', 's33'), ('step34', 's34')]:
        for r in runs_done(pre):
            p = f'{R}/model/{step}/{r}/train_meta.json'
            if os.path.exists(p):
                mt = json.load(open(p))
                meta.append({'step': step, 'run': r, 'seed': mt.get('seed'), 'passes': mt.get('max_epochs'),
                             'minutes': mt.get('minutes'), 'last held-back loss': mt.get('last_val_loss')})
    lines += ['## A. Runs found', '', md(pd.DataFrame(meta), '.3f') if meta else '(none)', '']
    g34, g33, real, wr = {}, {}, None, None
    try:
        dollars = load('dollars34', '04_dollars.py')
        real, wr = real_puf_side()
        for r in runs_done('s34'):
            g34[r] = gen_2015('s34', r, dollars, f'{R}/map/V34')
        for r in runs_done('s33'):
            g33[r] = gen_2015('s33', r, dollars, f'{R}/map/V33')
    except Exception as ex:
        lines += [f'**Loading failed: {ex!r}**', '```', traceback.format_exc(), '```']
    for part in [lambda: part_fit(real, wr, g34, g33), lambda: other_fit(real, wr, g34, g33),
                 lambda: combinations(real, wr, g34), lambda: by_wage_group(real, wr, g34, g33),
                 lambda: loss_sources(real, wr, g34), tilt_sections, wealth_by_business]:
        try:
            lines += part()
        except Exception as ex:
            lines += ['', f'**Section failed: {ex!r}**', '', '```', traceback.format_exc(), '```', '']
        open(f'{OUT}/report.md', 'w').write('\n'.join(lines) + '\n')
    print('\n'.join(lines))


if __name__ == '__main__':
    main()
