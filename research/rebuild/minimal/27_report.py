"""
27_report.py  (minimal example, step 27: the report tables; 27_MAPPING_SPEC.md
section 9. Write-up: 27_RESULTS.md.)

Reads whatever step 27 outputs exist (missing runs are listed as missing) and
writes step27/report.md:
  1. the settings sheet;
  2. per mapped item and run: the probability of family A and B, A's settings
     (chance of reporting at $50, $500, $5,000; the dollar factor), the
     scores with the dollar factor held at 0.8, 0.9, 1.0, 1.1;
  3. the capital gains basis mix (closed form, 27_tables.py);
  4. holders by wave after filling in, against the units reporting and the
     IRS count;
  5. the tilt's move and records kept; net worth by wage group, for units
     with no wages, for units with none of interest, dividends or capital
     gains, the top 1% share, before and after the tilt, for R0, R1, R2 and
     the step-23 runs N1 and N2; the generated PUF variables (share holding
     each, pretilt) against the PUF side of table E27.
Net worth in nominal 2019 dollars as in 23_report.py.
"""
import importlib.util
import json
import os

import numpy as np
import pandas as pd

HERE = 'research/rebuild/minimal'
R = '/nfs/roberts/scratch/pi_nrs36/jar335/minimal'
OUT = f'{R}/step27'
TO_2019 = 1.2493059 / 1.1592053
METHODS = ['R0', 'R2J', 'R2JAB', 'R1JAB', 'R2PJ', 'R1', 'R2', 'R1AB', 'R2AB', 'R2W', 'R2P']
RUNS = [f'{r}s{k}' for r in METHODS for k in (1, 2, 3)]
RUNS = [r for r in RUNS if os.path.exists(f'{R}/model/step27/{r}')]
LATENT = ['interest', 'dividends', 'capital_gains']


def load(name, f):
    s = importlib.util.spec_from_file_location(name, f'{HERE}/{f}')
    m = importlib.util.module_from_spec(s)
    s.loader.exec_module(m)
    return m


def stats(df, w, nw):
    out = {'units (millions)': w.sum() / 1e6, 'total net worth ($ trillion)': (w * nw).sum() / 1e12}
    o = np.argsort(nw)
    c = np.cumsum(w[o]) / w.sum()
    out['median ($)'] = nw[o][np.searchsorted(c, 0.5)]
    top = o[c > 0.99]
    out['top 1% share of net worth (%)'] = 100 * (w[top] * nw[top]).sum() / (w * nw).sum()
    wg = df['wages'].to_numpy()
    for name, lo, hi in [('wages 0', 0, 1), ('wages 1-25k', 1, 25e3), ('wages 25-50k', 25e3, 50e3),
                         ('wages 50-100k', 50e3, 1e5), ('wages 100-200k', 1e5, 2e5), ('wages 200k+', 2e5, np.inf)]:
        m = (wg >= lo) & (wg < hi)
        out[f'mean, {name} ($)'] = (w[m] * nw[m]).sum() / w[m].sum()
    cg = df['capital_gains_line13'] if 'capital_gains_line13' in df else df['capital_gains']
    none = (df['interest'].to_numpy() == 0) & (df['dividends'].to_numpy() == 0) & (cg.to_numpy() == 0)
    out['share with none of interest, dividends, capital gains (%)'] = 100 * w[none].sum() / w.sum()
    out['mean, none of interest, dividends, capital gains ($)'] = (w[none] * nw[none]).sum() / w[none].sum()
    return out


def fmt_table(t):
    lines = ['| | ' + ' | '.join(t.columns) + ' |', '|---|' + '---:|' * len(t.columns)]
    for r in t.index:
        cells = []
        for x in t.loc[r]:
            if isinstance(x, (float, np.floating)) and np.isnan(x):
                cells.append('')
            elif '%' in r or 'trillion' in r or 'millions' in r:
                cells.append(f'{x:,.1f}')
            else:
                cells.append(f'{x:,.0f}')
        lines.append(f'| {r} | ' + ' | '.join(cells) + ' |')
    return lines


def main():
    lines = ['# Step 27: report tables', '', 'Written by 27_report.py from what exists on disk.', '']
    have = {r: os.path.exists(f'{R}/tilt/step27/s27_{r.lower()}/summary_s27_{r.lower()}.csv') for r in RUNS}
    lines += ['Runs with a finished tilt: ' + ', '.join(r for r in RUNS if have[r]) + '; missing: '
              + (', '.join(r for r in RUNS if not have[r]) or 'none'), '']
    # 1. settings sheet
    lines += ['## 1. The settings sheet', '',
              '| SCF variable | Type | Concept (PUF formula) | Mapping fit on |', '|---|---|---|---|',
              '| married | assumed correct | married | |', '| dependents | assumed correct | dependents | |',
              '| wages | assumed correct | E00200 | |',
              '| interest | mapped, family A | E00300 | married, dependents, wages (2016 wave, 80% of households) |',
              '| dividends | mapped, family A | E00600 | married, dependents, wages (same) |',
              '| capital gains | mapped, family A, after-limit basis | kg_st + kg_lt + kg_dist + kg_other (= E01000 + E01100); '
              'a reported loss beyond $3,000 read as -$3,000 | married, dependents, wages (same) |', '']
    # 2. families and settings
    rows = []
    for r in RUNS:
        p = f'{R}/model/step27/{r}/score.json'
        if not os.path.exists(p):
            continue
        s = json.load(open(p))
        for v, d in s['items'].items():
            a = d['family A: chance of reporting at $50, $500, $5,000 (%)']
            pin = d['log score, family A with the dollar factor held at']
            rows.append({'run': r, 'item': v, 'P(A)': d['probability family A is right'],
                         'P(B)': d['probability family B is right'],
                         'log score A (nats)': d['log score, family A, learned settings (nats, households)'],
                         'log score B (nats)': d['log score, family B (nats, households)'],
                         'report at $50 (%)': a[0], 'at $500 (%)': a[1], 'at $5,000 (%)': a[2],
                         'dollar factor': d['family A: alpha, beta, dollar factor'][2],
                         **{f'score, factor {k}': x for k, x in pin.items()},
                         'held-back households': s['held-back households']})
    fam = pd.DataFrame(rows)
    lines += ['## 2. The two families and family A\'s settings (held-back 2016-wave households)', '',
              fam.to_markdown(index=False, floatfmt='.3g') if len(fam) else '(no scores yet)', '']
    # 3. mix
    if os.path.exists(f'{OUT}/kg_mix.json'):
        mix = json.load(open(f'{OUT}/kg_mix.json'))
        lines += ['## 3. The capital gains basis mix (closed form, 2016 wave)', '']
        lines += [f'- {k}: {v:,.2f}' for k, v in mix.items()] + ['']
    # 4. holders by wave
    hrows = []
    for r in RUNS:
        p = f'{R}/model/step27/{r}/score.json'
        if os.path.exists(p):
            for h in json.load(open(p))['holders by wave']:
                hrows.append({'run': r.rsplit('s', 1)[0], 'seed': r, **h})
    if hrows:
        h = pd.DataFrame(hrows)
        num = [c for c in h.columns if c not in ('run', 'seed', 'tax year', 'item')]
        hm = h.groupby(['run', 'tax year', 'item'], sort=False)[num].mean().reset_index()
        lines += ['## 4. Holders by wave after filling in (millions of units; mean over seeds)', '',
                  hm.to_markdown(index=False, floatfmt='.2f'), '']
    # 5. tilt and net worth
    wt = load('wt', '06_wealth_tables.py')
    tm, dollars = wt.tiltmod, wt.dollars
    cols, moves, gens = {}, [], {}
    u19, x19, w19, _ = wt.scf2019()
    cols['SCF 2019, all units'] = stats(u19.assign(capital_gains=u19['capital_gains']) if 'capital_gains' in u19 else u19,
                                        np.asarray(w19, float), np.asarray(x19, float) * TO_2019) \
        if all(c in u19 for c in ['wages', 'interest', 'dividends', 'capital_gains']) else {}
    runs = [(r, f's27_{r.lower()}', f'step27/s27_{r.lower()}', f'{R}/map/V27') for r in RUNS if have[r]]
    # the tilts with the capital gains targets restated for the pieces (27_tilt.sbatch <run> dist)
    runs += [(f'{r} restated', f's27_{r.lower()}', f'step27/s27_{r.lower()}_dist', f'{R}/map/V27') for r in RUNS
             if os.path.exists(f'{R}/tilt/step27/s27_{r.lower()}_dist/summary_s27_{r.lower()}.csv')]
    runs += [(lab, v, tag, f'{R}/map/V4') for lab, v, tag in [('N1 (step 23)', 'n1', 'n1'), ('N2 (step 23)', 'n2', 'n2')]
             if os.path.exists(f'{R}/tilt/{tag}/summary_{v}.csv')]
    for label, v, tag, mapdir in runs:
        os.environ['MINIMAL_MAP_DIR'] = mapdir
        os.environ['GEN_FILE'] = f'{R}/gen/gen_puf_stack_{v}.parquet'
        g = tm.generated_2019()
        wts = pd.read_parquet(f'{R}/tilt/{tag}/weights_{v}.parquet')
        assert (wts['record_id'].to_numpy() == g['record_id'].to_numpy()).all()
        nw = g['net_worth'].to_numpy(float) * TO_2019
        for when, w in [('before', wts['weight_before'].to_numpy()), ('after', wts['weight_after'].to_numpy())]:
            cols[f'{label}, {when}'] = stats(g, w, nw)
        s = pd.read_csv(f'{R}/tilt/{tag}/summary_{v}.csv')
        moves.append({'run': label, 'move, 136 IRS (nats)': s['kl_nats'].iloc[0],
                      'move, 136 IRS + wealth (nats)': s['kl_nats'].iloc[1],
                      'records kept, effective (%)': 100 * s['ess_ratio'].iloc[1]})
        if label.startswith('R') and 'restated' not in label:
            w = wts['weight_before'].to_numpy()
            gens[label] = {f'{x} held (%)': 100 * w[g[x].to_numpy() != 0].sum() / w.sum()
                           for x in ['wages', 'interest', 'dividends', 'kg_st', 'kg_lt', 'kg_dist', 'kg_other',
                                     'capital_gains_line13', 'other_agi'] if x in g}
            gens[label]['filers (%)'] = 100 * w[g['filer'].to_numpy() == 1].sum() / w.sum()
            if 'kg_dist' in g:      # other_agi here includes kg_dist (04_dollars MINIMAL_KG_PIECES): take it out
                oa = (g['other_agi'] - g['kg_dist']).to_numpy()
                gens[label]['other_agi held (%)'] = 100 * w[np.abs(oa) > 1e-6].sum() / w.sum()
    lines += ['## 5. The tilt to 2019', '', 'Rows marked "restated": the tilt with the capital gains targets restated '
              'for the pieces (distributions reported without a Schedule D as their own count and amount per '
              'bracket, 166 IRS targets instead of 136).', '',
              pd.DataFrame(moves).to_markdown(index=False, floatfmt='.4f'), '']
    mv = pd.DataFrame(moves)
    if len(mv):
        mv['method'] = [r.split(' ')[0].rsplit('s', 1)[0] + (' restated' if 'restated' in r else '')
                        if r.startswith('R') else r for r in mv['run']]
        lines += ['Means over seeds:', '', mv.groupby('method', sort=False)[
            ['move, 136 IRS (nats)', 'move, 136 IRS + wealth (nats)', 'records kept, effective (%)']].mean()
            .to_markdown(floatfmt='.4f'), '']
    t = pd.DataFrame(cols)
    # mean over seeds per method
    for meth in METHODS:
        for when in ['before', 'after']:
            for kind in ['', ' restated']:
                cs = [c for c in t.columns if c.startswith(meth + 's') and c.endswith(when)
                      and (('restated' in c) == bool(kind))]
                if cs and not (kind and when == 'before'):
                    t[f'{meth}{kind} mean of {len(cs)} seeds, {when}'] = t[cs].mean(axis=1)
    keep = [c for c in t.columns if 'mean of' in c or c.startswith('N') or c.startswith('SCF')]
    lines += ['## 6. Net worth (nominal 2019 dollars), before and after the tilt; seeds averaged', ''] + \
        fmt_table(t[keep]) + ['', '### Every seed', ''] + fmt_table(t) + ['']
    # generated PUF variables against the PUF side of E27 (2015, shares holding)
    e = pd.read_parquet(f'{R}/data/stacked_E27.parquet')
    p = e[e.source == 'puf']
    w = p['weight'].to_numpy(float)
    ref = {f'{x} held (%)': 100 * w[(p['capital_gains'] if x == 'capital_gains_line13' else p[x]).to_numpy() != 0].sum() / w.sum()
           for x in ['wages', 'interest', 'dividends', 'kg_st', 'kg_lt', 'kg_dist', 'kg_other', 'capital_gains_line13',
                     'other_agi']}
    ref['filers (%)'] = 100 * w[p['filer'].to_numpy() == 1].sum() / w.sum()
    gt = pd.DataFrame({'PUF side of E27 (2015)': ref, **gens})
    lines += ['## 7. Generated PUF variables before the tilt against the PUF side (share of units holding each)', ''] + \
        fmt_table(gt) + ['']
    # R0 held-back loss per variable, against the latent runs' last validation loss
    lr = []
    for r in RUNS:
        mp = f'{R}/model/step27/{r}/train_meta.json'
        if os.path.exists(mp):
            m = json.load(open(mp))
            lr.append({'run': r, 'held-back loss, last pass (nats)': m.get('heldback_loss_last_epoch', m.get('last_val_loss')),
                       'training minutes': m.get('minutes')})
    lines += ['## 8. Training', '', pd.DataFrame(lr).to_markdown(index=False, floatfmt='.4f') if lr else '', '']
    open(f'{OUT}/report.md', 'w').write('\n'.join(lines) + '\n')
    print('\n'.join(lines))


if __name__ == '__main__':
    main()
