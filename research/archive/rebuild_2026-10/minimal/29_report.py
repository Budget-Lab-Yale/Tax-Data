"""
29_report.py  (minimal example, step 29: the report tables. Status:
29_STATUS.md; write-up: 29_RESULTS.md.)

Reads whatever step 29 outputs exist (missing runs are listed as missing),
plus step 27's R2J and R0 runs for comparison (read only), and writes
step29/report.md:
  A. the fake survey: hidden holders among the survey's zeros and their
     dollars against the truth, overall and by cell; other income of the
     PUF-half returns against the truth; rule 2's learned curve middles
     against the planted ones;
  B. the real SCF: cells and their sizes; hidden holders among SCF zeros by
     wave and by cell (2016 wave); their sizes; rule 2's curve middles by
     cell; rule 3's cell targets; holders after filling in against the PUF
     side and the IRS count, per wave;
  C. the tilt's move (136 IRS targets, and with the 8 wealth targets) and
     records kept;
  D. net worth before and after the tilt (nominal 2019 dollars): total,
     median, top 1% share, mean by wage group, mean for units with none of
     interest, dividends or capital gains.
"""
import glob
import importlib.util
import json
import math
import os

import numpy as np
import pandas as pd

HERE = 'research/rebuild/minimal'
R = '/nfs/roberts/scratch/pi_nrs36/jar335/minimal'
OUT = f'{R}/step29'
TO_2019 = 1.2493059 / 1.1592053
LATENT = ['interest', 'dividends', 'capital_gains']
METHODS = [('C1', 'step29', 'rule 1: one S-curve, dollar factor 1'),
           ('C2', 'step29', 'rule 2: S-curve middle per cell, dollar factor 1'),
           ('C3', 'step29', 'rule 3: per-cell counts and slices, factor h from the 2016 wave used in every wave'),
           ('C4', 'step29', 'rule 3 wave by wave: the 2016 cell shares of hidden holders pinned in every wave'),
           ('C5', 'step29', 'as C4, hidden holders spread over slices smallest first'),
           ('R2J', 'step27', 'step 27 R2J: one S-curve, learned dollar factor'),
           ('R0', 'step27', 'step 27 R0: SCF zeros taken as zeros')]
SEEDS = (1, 2, 3, 4, 5)     # rules 1 and 2 got seeds 4 and 5 after capital gains ran away in two runs
RUNAWAY = set()             # runs where, in some wave, an item's holders after filling in exceed 1.5 x the IRS share


def load(name, f):
    s = importlib.util.spec_from_file_location(name, f'{HERE}/{f}')
    m = importlib.util.module_from_spec(s)
    s.loader.exec_module(m)
    return m


def md(df, fmt='.1f'):
    return df.to_markdown(index=False, floatfmt=fmt) if len(df) else '(nothing yet)'


def fake_section():
    lines = ['## A. Fake survey (table F29)', '']
    notep = f'{OUT}/fake/fake_note.json'
    if os.path.exists(notep):
        note = json.load(open(notep))
        lines += ['Planted: ' + json.dumps(note), '']
    J = {}
    for p in sorted(glob.glob(f'{OUT}/fake/F29*.json')):
        d = json.load(open(p))
        J[d['run']] = d
    if not J:
        return lines + ['(no fake-survey checks yet)', '']
    lines += ['Runs checked: ' + ', '.join(J), '']
    meth = lambda r: r.rsplit('s', 1)[0]
    rows, cellrows, oi = [], [], []
    for r, d in J.items():
        for h in d['hidden holders']:
            (rows if h['cell'] == 'all' else cellrows).append({'method': meth(r), 'run': r, **h})
        oi.append({'method': meth(r), 'run': r, **d['other income gap (%)']})
    h = pd.DataFrame(rows)
    num = [c for c in h.columns if c not in ('method', 'run', 'item', 'cell')]
    hm = h.groupby(['item', 'method'], sort=False)[num].mean().reset_index()
    lines += ['### Hidden holders among the survey zeros, all households (mean over seeds)', '',
              'Model dollars use the PUF\'s mean dollars in each filled slice; truth uses the true amounts.', '',
              md(hm, '.2f'), '']
    o = pd.DataFrame(oi)
    lines += ['### Other income of the PUF-half returns: gap of the model\'s mean from the truth (%)', '',
              md(o.groupby('method', sort=False).mean(numeric_only=True).reset_index(), '+.1f'), '',
              'Every seed:', '', md(o, '+.1f'), '']
    c = pd.DataFrame(cellrows)
    cm = c.groupby(['item', 'cell', 'method'], sort=False)[['hidden holders among zeros, truth (%)',
                                                            'hidden holders among zeros, model (%)']].mean().reset_index()
    wide = cm.pivot_table(index=['item', 'cell'], columns='method', values='hidden holders among zeros, model (%)',
                          sort=False).reset_index()
    tru = cm.groupby(['item', 'cell'], sort=False)['hidden holders among zeros, truth (%)'].first().reset_index()
    wide = tru.merge(wide, on=['item', 'cell'])
    lines += ['### Hidden holders among the survey zeros by cell (%; truth, then each method, mean over seeds)', '',
              md(wide, '.1f'), '']
    # rule 2 middles against the plant
    mids = []
    for r, d in J.items():
        st = d.get('settings') or {}
        for v in LATENT:
            s = st.get(v) or {}
            k = 'curve middle (dollars, 2015 PUF scale) by cell'
            if k in s:
                for ci, m in enumerate(s[k]):
                    mids.append({'run': r, 'item': v, 'cell': d['cells'][ci]['label'], 'middle ($)': m,
                                 'steepness': s['beta (shared)']})
            elif 'curve middle (dollars)' in s:
                mids.append({'run': r, 'item': v, 'cell': 'all', 'middle ($)': s['curve middle (dollars)'][0],
                             'steepness': s['beta (shared)']})
    if mids:
        lines += ['### Learned curve middles (planted: $303 below $25,000 of wages, $175 at $25,000-75,000, '
                  '$101 from $75,000; planted steepness 1.3)', '', md(pd.DataFrame(mids), '.2f'), '']
    return lines


def real_section(t29):
    lines = ['## B. Real SCF (table E27)', '']
    e = pd.read_parquet(f'{R}/data/stacked_E27.parquet')
    edges = json.load(open(f'{R}/data/band_edges_E27.json'))
    e['cell'], cells = t29.cell_map(e, edges)
    lines += ['### Cells (wage group x married; 2016-wave households)', '',
              md(pd.DataFrame(cells).drop(columns=['wage groups']), '.1f'), '']
    scf_cells = e.loc[e.source == 'scf', ['record_id', 'year', 'cell']]
    assert not scf_cells.duplicated(['record_id', 'year']).any(), 'SCF record ids repeat within a wave'
    puf = e[e.source == 'puf']
    pw = puf['weight'].to_numpy(float)
    meanx, puf_share, puf_cell_share = {}, {}, {}
    for v in LATENT:
        x, b = puf[v].to_numpy(float), puf[f'{v}_band'].to_numpy()
        NB = int(e[f'{v}_band'].max()) + 1
        meanx[v] = np.array([np.average(x[b == k], weights=pw[b == k]) if (b == k).any() else 0.0 for k in range(NB)])
        puf_share[v] = 100 * pw[x != 0].sum() / pw.sum()
        pc = puf['cell'].to_numpy()
        puf_cell_share[v] = {c: 100 * pw[(pc == c) & (x != 0)].sum() / pw[pc == c].sum() for c in range(len(cells))}
    irs = pd.read_csv(f'{R}/step27/irs_counts.csv')
    irs = irs[irs.statistic == 'count'].set_index('year')
    wave_rows, cell_rows, size_rows, missing = [], [], [], []
    for code, step, _ in METHODS:
        for k in SEEDS:
            run = f'{code}s{k}'
            fp = f'{R}/model/{step}/{run}/fills.parquet'
            if code == 'R0':
                fp = None
            elif not os.path.exists(fp):
                missing.append(run)
                continue
            if fp is None:
                if not os.path.exists(f'{R}/model/{step}/{run}/model.pt'):
                    missing.append(run)
                    continue
                f = e[e.source == 'scf'][['record_id', 'year', 'weight', 'cell'] + LATENT].copy()
                for v in LATENT:
                    f[f'{v}_reported'] = f[v]
                    for dd in range(1, 6):
                        f[f'{v}_fill{dd}'] = (f[v].to_numpy() != 0).astype(int)     # zeros stay zeros
            else:
                f = pd.read_parquet(fp).drop(columns=['cell'], errors='ignore')
                n0 = len(f)
                f = f.merge(scf_cells, on=['record_id', 'year'], how='left', validate='one_to_one')
                assert len(f) == n0 and not f['cell'].isna().any(), f'{run}: fills rows without a cell'
                f['cell'] = f['cell'].astype(int)
            for y, g in f.groupby('year'):
                w = g['weight'].to_numpy(float)
                for v in LATENT:
                    rep = g[f'{v}_reported'].to_numpy() != 0
                    fills = [g[f'{v}_fill{dd}'].to_numpy() for dd in range(1, 6)]
                    held = np.mean([w[b != 0].sum() for b in fills])
                    wave_rows.append({'method': code, 'run': run, 'tax year': int(y), 'item': v,
                                      'hidden holders among SCF zeros (%)': 100 * np.mean([w[~rep & (b != 0)].sum() for b in fills]) / w[~rep].sum(),
                                      'SCF reporting (% of units)': 100 * w[rep].sum() / w.sum(),
                                      'SCF after filling in (% of units)': 100 * held / w.sum(),
                                      'PUF side, 2015 (% of units)': puf_share[v],
                                      'IRS (% of returns)': 100 * float(irs.loc[int(y), v]) / float(irs.loc[int(y), 'returns']),
                                      'SCF after filling in (millions)': held / 1e6,
                                      'IRS returns with it (millions)': float(irs.loc[int(y), v]) / 1e6})
                    if int(y) == 2015:
                        cg = g['cell'].to_numpy()
                        for c in range(len(cells)):
                            m = cg == c
                            cell_rows.append({'method': code, 'run': run, 'item': v, 'cell': cells[c]['label'],
                                              'hidden holders among zeros (%)': 100 * np.mean([w[m & ~rep & (b != 0)].sum() for b in fills]) / max(w[m & ~rep].sum(), 1e-9),
                                              'SCF reporting (%)': 100 * w[m & rep].sum() / w[m].sum(),
                                              'SCF after filling in (%)': 100 * np.mean([w[m & (b != 0)].sum() for b in fills]) / w[m].sum(),
                                              'PUF side (%)': puf_cell_share[v][c]})
                        if fp is not None:
                            hid_w, bins = [], []
                            for b in fills:
                                hh = ~rep & (b != 0)
                                dollars = meanx[v][b[hh]]
                                ww = w[hh]
                                bins.append([ww[np.abs(dollars) < 500].sum(), ww[(np.abs(dollars) >= 500) & (np.abs(dollars) < 5000)].sum(),
                                             ww[np.abs(dollars) >= 5000].sum(), ww[dollars < 0].sum(), (ww * dollars).sum(), ww.sum()])
                            bsum = np.array(bins).mean(0)
                            rd = meanx[v][g[f'{v}_reported_band'].to_numpy()[rep]] if f'{v}_reported_band' in g else None
                            size_rows.append({'method': code, 'run': run, 'item': v,
                                              'hidden holders under $500 (%)': 100 * bsum[0] / bsum[5],
                                              '$500-5,000 (%)': 100 * bsum[1] / bsum[5],
                                              '$5,000 and over (%)': 100 * bsum[2] / bsum[5],
                                              'losses (%)': 100 * bsum[3] / bsum[5],
                                              'mean $ per hidden holder': bsum[4] / bsum[5],
                                              'mean $ per SCF reporter (same scale)': float(np.average(rd, weights=w[rep])) if rd is not None else float('nan')})
    lines += ['Runs missing: ' + (', '.join(missing) or 'none'), '']
    W = pd.DataFrame(wave_rows)
    if len(W):
        bad = W[W['SCF after filling in (% of units)'] > 1.5 * W['IRS (% of returns)']]
        RUNAWAY.update(bad['run'].unique().tolist())
        lines += ['**Runs whose fill-ins ran away** (some wave and item with holders after filling in above 1.5 times '
                  'the IRS share of returns): ' + (', '.join(sorted(RUNAWAY)) or 'none') + '. Tables below marked '
                  '"stable seeds" leave these runs out.', '',
                  md(bad[['run', 'tax year', 'item', 'hidden holders among SCF zeros (%)', 'SCF after filling in (% of units)',
                          'IRS (% of returns)']], '.1f'), '']
        Ws = W[~W['run'].isin(RUNAWAY)]
        num = [c for c in W.columns if c not in ('method', 'run', 'tax year', 'item')]
        Wsm = Ws.groupby(['item', 'tax year', 'method'], sort=False)[num].mean().reset_index()
        Wsm['seeds'] = Ws.groupby(['item', 'tax year', 'method'], sort=False)['run'].nunique().to_numpy()
        Wsm['after filling / IRS count'] = Wsm['SCF after filling in (millions)'] / Wsm['IRS returns with it (millions)']
        lines += ['### Stable seeds: hidden holders among SCF zeros, and holders after filling in, by wave (mean over '
                  'stable seeds)', '', md(Wsm, '.2f'), '']
        num = [c for c in W.columns if c not in ('method', 'run', 'tax year', 'item')]
        Wm = W.groupby(['item', 'tax year', 'method'], sort=False)[num].mean().reset_index()
        Wm['after filling / IRS count'] = Wm['SCF after filling in (millions)'] / Wm['IRS returns with it (millions)']
        lines += ['### Hidden holders among SCF zeros, and holders after filling in, by wave (mean over seeds)', '',
                  md(Wm, '.2f'), '']
        sp = W.groupby(['method', 'item', 'tax year'], sort=False)['hidden holders among SCF zeros (%)'].agg(['min', 'max']).reset_index()
        lines += ['Range over seeds of the hidden-holder share:', '', md(sp, '.2f'), '']
    Cc = pd.DataFrame(cell_rows)
    if len(Cc):
        Cc = Cc[~Cc['run'].isin(RUNAWAY)]
        lines += ['By-cell and size tables use stable seeds only.', '']
        num = [c for c in Cc.columns if c not in ('method', 'run', 'item', 'cell')]
        Cm = Cc.groupby(['item', 'cell', 'method'], sort=False)[num].mean().reset_index()
        wide = Cm.pivot_table(index=['item', 'cell'], columns='method', values='hidden holders among zeros (%)',
                              sort=False).reset_index()
        lines += ['### Hidden holders among SCF zeros by cell, 2016 wave (%, mean over seeds)', '', md(wide, '.1f'), '',
                  '### By cell, 2016 wave: reporting, after filling in, PUF side (% of units; mean over seeds)', '',
                  md(Cm, '.1f'), '']
    S = pd.DataFrame(size_rows)
    if len(S):
        S = S[~S['run'].isin(RUNAWAY)]
        num = [c for c in S.columns if c not in ('method', 'run', 'item')]
        lines += ['### Sizes of the hidden holdings, 2016 wave (2015 dollars on the PUF\'s scale: the PUF\'s mean '
                  'dollars in each filled slice; mean over seeds)', '',
                  md(S.groupby(['item', 'method'], sort=False)[num].mean().reset_index(), '.1f'), '']
    # curve middles
    mids = []
    for code, step, _ in [m for m in METHODS if m[0] in ('C1', 'C2', 'R2J')]:
        for k in SEEDS:
            p = f'{R}/model/{step}/{code}s{k}/train_meta.json'
            if not os.path.exists(p):
                continue
            m = json.load(open(p))
            for v in LATENT:
                s = m['settings'][v]
                if code == 'R2J':
                    a, b = s['alpha'], s['beta']
                    mids.append({'method': code, 'seed': k, 'item': v, 'cell': 'all', 'middle ($)': 150 * math.exp(-a / b),
                                 'steepness': b, 'dollar factor': s['final dollar factor']})
                elif 'curve middle (dollars, 2015 PUF scale) by cell' in s:
                    for ci, x in enumerate(s['curve middle (dollars, 2015 PUF scale) by cell']):
                        mids.append({'method': code, 'seed': k, 'item': v, 'cell': m['cells'][ci]['label'],
                                     'middle ($)': x, 'steepness': s['beta (shared)'], 'dollar factor': 1.0})
                elif 'curve middle (dollars)' in s:
                    mids.append({'method': code, 'seed': k, 'item': v, 'cell': 'all', 'middle ($)': s['curve middle (dollars)'][0],
                                 'steepness': s['beta (shared)'], 'dollar factor': 1.0})
    if mids:
        Md = pd.DataFrame(mids)
        lines += ['### Curve middles (dollars at which half of holders report; 2015 PUF scale)', '',
                  md(Md.groupby(['item', 'method', 'cell'], sort=False)[['middle ($)', 'steepness', 'dollar factor']]
                     .agg(['mean', 'min', 'max']).round(2).reset_index().pipe(
                         lambda d: d.set_axis([' '.join(c).strip() for c in d.columns], axis=1)), '.2f'), '']
    for p in [f'{R}/model/step29/C3s1/train_meta.json', f'{R}/model/step29/C5s1/train_meta.json']:
      if os.path.exists(p):
        ct = pd.DataFrame(json.load(open(p))['cell_targets']).drop(columns=['cell', 'SCF fit weight (training units)'])
        alloc = json.load(open(p)).get('rule3_alloc', 'cumulative')
        lines += [f'### Rule 3: the cell targets, run {p.split("/")[-2]} (slices spread: {alloc}; 2016 wave, fit '
                  'households; dollars per unit on the PUF\'s 2015 scale)', '',
                  'cumulative: the PUF side\'s holders minus the SCF\'s reporters counted above each amount, from the '
                  'largest amounts toward zero, never falling, then scaled to the count of hidden holders. '
                  'smallest_first: slice by slice from the smallest amounts up, the difference floored at zero, '
                  'until the count is reached. The column "slice by slice, floored" is a third version, not used.', '',
                  md(ct, '.2f'), '']
        lg = pd.read_csv(os.path.join(os.path.dirname(p), 'train_log.csv'))
        cols = [c for c in lg.columns if c.endswith('_ipf_max_miss')]
        lines += ['Rule 3, largest miss of the fitted hidden-holder count per cell after the last pass (share of '
                  'the cell): ' + ', '.join(f'{c}: {lg[c].iloc[-1]:.2e}' for c in cols), '']
    return lines


def tilt_sections():
    rep27 = load('rep27', '27_report.py')
    wt = load('wt', '06_wealth_tables.py')
    tm = wt.tiltmod
    cols, moves = {}, []
    u19, x19, w19, _ = wt.scf2019()
    cols['SCF 2019, all units'] = rep27.stats(u19, np.asarray(w19, float), np.asarray(x19, float) * TO_2019) \
        if all(c in u19 for c in ['wages', 'interest', 'dividends', 'capital_gains']) else {}
    for code, step, _ in METHODS:
        for k in SEEDS:
            run = f'{code}s{k}'
            v = f's{step[-2:]}_{run.lower()}'
            tag = f'{step}/{v}'
            if not os.path.exists(f'{R}/tilt/{tag}/summary_{v}.csv'):
                continue
            os.environ['MINIMAL_MAP_DIR'] = f'{R}/map/V27'
            os.environ['GEN_FILE'] = f'{R}/gen/gen_puf_stack_{v}.parquet'
            g = tm.generated_2019()
            wts = pd.read_parquet(f'{R}/tilt/{tag}/weights_{v}.parquet')
            assert (wts['record_id'].to_numpy() == g['record_id'].to_numpy()).all()
            nw = g['net_worth'].to_numpy(float) * TO_2019
            for when, w in [('before', wts['weight_before'].to_numpy()), ('after', wts['weight_after'].to_numpy())]:
                cols[f'{run}, {when}'] = rep27.stats(g, w, nw)
            s = pd.read_csv(f'{R}/tilt/{tag}/summary_{v}.csv')
            moves.append({'method': code, 'run': run, 'move, 136 IRS (nats)': s['kl_nats'].iloc[0],
                          'move, 136 IRS + 8 wealth (nats)': s['kl_nats'].iloc[1],
                          'records kept, effective (%)': 100 * s['ess_ratio'].iloc[1]})
    mv = pd.DataFrame(moves)
    lines = ['## C. The tilt to 2019', '', 'Runs whose fill-ins ran away: ' + (', '.join(sorted(RUNAWAY)) or 'none'), '']
    if len(mv):
        mv['ran away'] = mv['run'].isin(RUNAWAY)
        lines += ['Mean over stable seeds:', '',
                  md(mv[~mv['ran away']].groupby('method', sort=False).mean(numeric_only=True).reset_index(), '.4f'), '',
                  'Every seed:', '', md(mv, '.4f'), '']
    t = pd.DataFrame(cols)
    for code, _, _ in METHODS:
        for when in ['before', 'after']:
            cs = [c for c in t.columns if c.startswith(code + 's') and c.endswith(when)
                  and c.split(',')[0] not in RUNAWAY]
            if cs:
                t[f'{code} mean of {len(cs)} stable seeds, {when}'] = t[cs].mean(axis=1)
    keep = [c for c in t.columns if 'mean of' in c or c.startswith('SCF')]
    lines += ['## D. Net worth (nominal 2019 dollars), before and after the tilt; stable seeds averaged', '',
              'Runs left out of the means (fill-ins ran away): ' + (', '.join(sorted(RUNAWAY)) or 'none'), ''] + \
        rep27.fmt_table(t[keep]) + ['', '### Every seed', ''] + \
        rep27.fmt_table(t[[c for c in t.columns if 'mean of' not in c]]) + ['']
    return lines


def main():
    os.makedirs(OUT, exist_ok=True)
    t29 = load('train29', '29_train.py')
    lines = ['# Step 29: report tables', '', 'Written by 29_report.py from what exists on disk. Methods: '
             + '; '.join(f'{c} = {d}' for c, _, d in METHODS) + '.', '']
    for part in (fake_section, lambda: real_section(t29), tilt_sections):
        try:
            lines += part()
        except Exception as ex:          # report what failed, keep the other sections
            import traceback
            lines += ['', f'**Section failed: {ex!r}**', '', '```', traceback.format_exc(), '```', '']
        open(f'{OUT}/report.md', 'w').write('\n'.join(lines) + '\n')
    print('\n'.join(lines))


if __name__ == '__main__':
    main()
