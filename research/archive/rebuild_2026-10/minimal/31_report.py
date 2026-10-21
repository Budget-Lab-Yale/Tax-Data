"""
31_report.py  (minimal example, step 31: the report tables. Status:
31_STATUS.md.)

Reads whatever step 31 outputs exist and writes step31/report.md, with the
items design/cell_method.md section 8 asks for. The cell method's runs
(K, seeds 1-3) beside today's rule with the label on (step 27 R0), step 27's
learned curve (R2J) and step 29's per-cell counts by slice (C4).

  0. the settings sheet;
  1. the cell table, every recorded excess and gap above the top line;
  2. the filled-in survey against the PUF side by cell and range (2016 wave),
     and dollars per unit;
  3. who the hidden holders are, by cell (2016 wave): net worth, wages, the
     other items, for reporters, hidden holders and remaining zeros;
  4. each wave's holders after filling in against the IRS count (29_report.py
     section B, reused);
  5. the tilt's move and records kept, and net worth before and after (total,
     median, top 1% share, by wage group, units with none of the three items),
     on both count bases (SCF units: 29_report.py sections C and D, reused;
     file units: the same tables from the "_file" tilts).
"""
import importlib.util
import json
import os

import numpy as np
import pandas as pd

HERE = 'research/rebuild/minimal'
R = '/nfs/roberts/scratch/pi_nrs36/jar335/minimal'
OUT = os.environ.get('STEP31_REPORT_OUT', f'{R}/step31')
# runs of the cell method to read (a smoke test: STEP31_K_RUNS=smokeK)
K_NAMES = os.environ.get('STEP31_K_RUNS', 'Ks1,Ks2,Ks3').split(',')
LATENT = ['interest', 'dividends', 'capital_gains']
SEEDS = (1, 2, 3)
METHODS = [('K', 'step31', 'the cell method (design/cell_method.md)'),
           ('C4', 'step29', 'step 29 C4: per-cell counts by slice, wave by wave'),
           ('R2J', 'step27', 'step 27 R2J: one S-curve, learned dollar factor'),
           ('R0', 'step27', 'step 27 R0: SCF zeros taken as zeros')]


def load(name, f):
    s = importlib.util.spec_from_file_location(name, f'{HERE}/{f}')
    m = importlib.util.module_from_spec(s)
    s.loader.exec_module(m)
    return m


def md(df, fmt='.1f'):
    return df.to_markdown(index=False, floatfmt=fmt) if len(df) else '(nothing yet)'


def k_runs():
    return [r for r in K_NAMES if os.path.exists(f'{R}/model/step31/{r}/fills.parquet')]


def settings_and_table():
    runs = k_runs()
    if not runs:
        return ['## 0-1. Settings and cell table', '', '(no finished K run)', '']
    p = f'{R}/model/step31/{runs[0]}/train_meta.json'
    m = json.load(open(p))
    ct = pd.DataFrame(m['cell_table'])
    lines = ['## 0. The settings sheet (as read by the code)', '', '```json', json.dumps(m['settings_sheet'], indent=1),
             '```', '', '## 1. The cell table (2016 wave against the 2015 PUF side; % of the cell\'s units)', '',
             'The same in every seed (it depends on the data only). "Filled in (% of zeros)" is the share carried '
             'to the other waves. Recorded columns: where SCF reporters exceed the PUF side (left as reported), and '
             'gaps above the top line (not filled in).', '']
    cells = pd.DataFrame(m['cells']).drop(columns=['wage groups'])
    lines += ['### Cells', '', md(cells, '.1f'), '']
    for v in LATENT:
        lines += [f'### {v}', '', md(ct[ct.item == v].drop(columns=['item', 'cell']), '.2f'), '']
    tot = ct[ct.range != 'zero'].groupby('item', sort=False)[
        ['filled in (% of units)', 'recorded, SCF excess (points)', 'recorded, gap above the top line (points)']].sum()
    lines += ['Sums over cells and ranges (points of a cell, added across cells, unweighted; a quick size check only):',
              '', md(tot.reset_index(), '.2f'), '']
    wt = pd.DataFrame(m['wave_targets']).groupby(['item', 'tax year'], sort=False)[
        ['zeros (millions)', 'hidden holders to fill (millions)']].sum().reset_index()
    lines += ['### Hidden holders to fill, by wave (training rows, weighted, millions)', '', md(wt, '.2f'), '']
    return lines


def scf_rows():
    e = pd.read_parquet(f'{R}/data/stacked_E27.parquet')
    s = e[e.source == 'scf'][['record_id', 'year', 'household_id', 'weight', 'wages', 'net_worth']].copy()
    return e, s


def filled_vs_puf_and_who():
    runs = k_runs()
    if not runs:
        return ['## 2-3. Filled-in survey and who the hidden holders are', '', '(no finished K run)', '']
    m = json.load(open(f'{R}/model/step31/{runs[0]}/train_meta.json'))
    labels = {c['cell']: c['label'] for c in m['cells']}
    e, s = scf_rows()
    puf = e[e.source == 'puf']
    pw = puf['weight'].to_numpy(float)
    meanx, puf_mean = {}, {}
    for v in LATENT:
        x, b = puf[v].to_numpy(float), puf[f'{v}_band'].to_numpy()
        NB = int(e[f'{v}_band'].max()) + 1
        meanx[v] = np.array([np.average(x[b == k], weights=pw[b == k]) if (b == k).any() else 0.0 for k in range(NB)])
    ranges = {v: m['settings_sheet']['comparison_ranges'][v] for v in LATENT}
    ct = pd.DataFrame(m['cell_table'])
    rows, who, dol = [], [], []
    for run in runs:
        f = pd.read_parquet(f'{R}/model/step31/{run}/fills.parquet')
        f = f[f.year == 2015].merge(s[['record_id', 'year', 'wages', 'net_worth']], on=['record_id', 'year'],
                                    how='left', validate='one_to_one')
        w = f['weight'].to_numpy(float)
        cell = f['cell'].to_numpy()
        for v in LATENT:
            ro = np.array(m['slice_to_range'][v])
            rep = f[f'{v}_reported_band'].to_numpy()
            fills = [f[f'{v}_fill{d}'].to_numpy() for d in range(1, 6)]
            names = ['zero'] + [ct[(ct.item == v) & (ct.cell == 0)]['range'].tolist()[i] for i in range(len(ranges[v]))] \
                + ['unused slices']
            for c in sorted(labels):
                mc = cell == c
                for r, nm in enumerate(names):
                    sh_rep = w[mc & (ro[rep] == r)].sum() / w[mc].sum()
                    sh_fill = np.mean([w[mc & (ro[b] == r)].sum() for b in fills]) / w[mc].sum()
                    rows.append({'run': run, 'item': v, 'cell': labels[c], 'range': nm,
                                 'SCF reported (%)': 100 * sh_rep, 'SCF filled in (%)': 100 * sh_fill})
            # dollars per unit (2015 dollars, the PUF's mean dollars in each slice)
            dol.append({'run': run, 'item': v,
                        'SCF reported, $ per unit': float((w * meanx[v][rep]).sum() / w.sum()),
                        'SCF filled in, $ per unit': float(np.mean([(w * meanx[v][b]).sum() for b in fills]) / w.sum())})
            # who: reporters, hidden holders, remaining zeros; net worth only where the SCF asked it
            # (split-off units have none: the universe below is units with net worth, counted)
            others = [o for o in LATENT if o != v]
            for c in list(sorted(labels)) + ['all']:
                mc = np.ones(len(f), bool) if c == 'all' else cell == c
                groups = {'reporters': [mc & (rep != 0)] * 5,
                          'hidden holders': [mc & (rep == 0) & (b != 0) for b in fills],
                          'remaining zeros': [mc & (rep == 0) & (b == 0) for b in fills]}
                for gname, masks in groups.items():
                    st = []
                    for d, msk in enumerate(masks):
                        nw_ok = msk & f['net_worth'].notna().to_numpy()
                        ww, nw = w[nw_ok], f['net_worth'].to_numpy(float)[nw_ok]
                        if ww.sum() == 0:
                            continue
                        o = np.argsort(nw)
                        med = nw[o][np.searchsorted(np.cumsum(ww[o]) / ww.sum(), 0.5)]
                        st.append({'share of cell units (%)': 100 * w[msk].sum() / w[mc].sum(),
                                   'net worth, median ($)': med,
                                   'net worth, mean ($)': float((ww * nw).sum() / ww.sum()),
                                   'units without net worth (split-off, % of group)': 100 * (1 - ww.sum() / w[msk].sum()),
                                   'wages, mean ($)': float((w[msk] * f['wages'].to_numpy(float)[msk]).sum() / w[msk].sum()),
                                   **{f'report {o} (%)': 100 * w[msk & (f[f'{o}_reported'].to_numpy() != 0)].sum() / w[msk].sum()
                                      for o in others},
                                   **{f'{o} after filling, draw-matched (%)': 100 * w[msk & (f[f'{o}_fill{d + 1}'].to_numpy() != 0)].sum() / w[msk].sum()
                                      for o in others}})
                    if st:
                        who.append({'run': run, 'item': v, 'cell': 'all cells' if c == 'all' else labels[c],
                                    'group': gname, **pd.DataFrame(st).mean().to_dict()})
    R_ = pd.DataFrame(rows).groupby(['item', 'cell', 'range'], sort=False)[['SCF reported (%)', 'SCF filled in (%)']] \
        .mean().reset_index()
    R_ = R_.merge(ct[['item', 'cell label', 'range', 'PUF side (% of units)']].rename(columns={'cell label': 'cell'}),
                  on=['item', 'cell', 'range'], how='left')
    R_['filled in minus PUF side (points)'] = R_['SCF filled in (%)'] - R_['PUF side (% of units)']
    lines = ['## 2. The filled-in survey against the PUF side, 2016 wave, by cell and range (% of the cell\'s units; '
             f'mean over five draws and the runs {", ".join(runs)})', '',
             'A slice counts in the range of its 2015 PUF median (the code\'s map), so "reported" here can differ '
             'slightly from the cell table, which uses the same map. Where the PUF side exceeds the SCF below the top '
             'line, filled in should equal the PUF side up to draw noise; elsewhere it equals the SCF as reported.', '']
    for v in LATENT:
        lines += [f'### {v}', '', md(R_[R_.item == v].drop(columns=['item']), '.2f'), '']
    D = pd.DataFrame(dol).groupby('item', sort=False).mean(numeric_only=True).reset_index()
    D['PUF side, $ per unit (whole PUF side, not at the SCF\'s cell mix)'] = [
        float((pw * puf[v].to_numpy(float)).sum() / pw.sum()) for v in D['item']]
    lines += ['### Dollars per unit, 2016 wave (2015 dollars; slice means of the PUF side)', '', md(D, '.0f'), '']
    Wd = pd.DataFrame(who)
    num = [c for c in Wd.columns if c not in ('run', 'item', 'cell', 'group')]
    Wm = Wd.groupby(['item', 'cell', 'group'], sort=False)[num].mean().reset_index()
    lines += ['## 3. Who the hidden holders are, 2016 wave (2015 dollars; mean over five draws and runs)', '',
              'Net worth over units the SCF asked it of (split-off units have none; their share of each group is '
              'shown). "After filling, draw-matched": the other item in the same draw.', '']
    for v in LATENT:
        lines += [f'### {v}', '', md(Wm[Wm.item == v].drop(columns=['item']), '.1f'), '']
    return lines


def file_base_tilts(rep29):
    """As 29_report.tilt_sections, on the file-units tilts (tilt/step31/<v>_file)."""
    rep27 = rep29.load('rep27', '27_report.py')
    wt = rep29.load('wt', '06_wealth_tables.py')
    tm = wt.tiltmod
    cols, moves = {}, []
    for code, step, _ in METHODS:
        for k in SEEDS:
            run = f'{code}s{k}'
            v = f's{step[-2:]}_{run.lower()}'
            tag = f'step31/{v}_file'
            if not os.path.exists(f'{R}/tilt/{tag}/summary_{v}.csv'):
                continue
            os.environ['MINIMAL_MAP_DIR'] = f'{R}/map/V27'
            os.environ['GEN_FILE'] = f'{R}/gen/gen_puf_stack_{v}.parquet'
            g = tm.generated_2019()
            wts = pd.read_parquet(f'{R}/tilt/{tag}/weights_{v}.parquet')
            assert (wts['record_id'].to_numpy() == g['record_id'].to_numpy()).all()
            nw = g['net_worth'].to_numpy(float) * rep29.TO_2019
            cols[f'{run}, after'] = rep27.stats(g, wts['weight_after'].to_numpy(), nw)
            s = pd.read_csv(f'{R}/tilt/{tag}/summary_{v}.csv')
            moves.append({'method': code, 'run': run, 'move, 136 IRS (nats)': s['kl_nats'].iloc[0],
                          'move, 136 IRS + 8 wealth (nats)': s['kl_nats'].iloc[1],
                          'records kept, effective (%)': 100 * s['ess_ratio'].iloc[1]})
    lines = ['## 5b. The tilt with the wealth counts on the file\'s units (note, step 13)', '']
    mv = pd.DataFrame(moves)
    if len(mv):
        lines += [md(mv.groupby('method', sort=False).mean(numeric_only=True).reset_index(), '.4f'), '',
                  'Every seed:', '', md(mv, '.4f'), '']
    t = pd.DataFrame(cols)
    for code, _, _ in METHODS:
        cs = [c for c in t.columns if c.startswith(code + 's')]
        if cs:
            t[f'{code} mean of {len(cs)} seeds, after'] = t[cs].mean(axis=1)
    if len(t):
        lines += ['Net worth after the tilt (nominal 2019 dollars):', ''] + \
            rep27.fmt_table(t[[c for c in t.columns if 'mean of' in c]]) + ['']
    return lines


def main():
    os.makedirs(OUT, exist_ok=True)
    rep29 = load('rep29', '29_report.py')
    rep29.METHODS, rep29.SEEDS, rep29.OUT = METHODS, SEEDS, OUT
    t31 = load('train31', '31_train.py')
    lines = ['# Step 31: report tables', '', 'Written by 31_report.py from what exists on disk. Methods: '
             + '; '.join(f'{c} = {d}' for c, _, d in METHODS) + '.', '']
    parts = [settings_and_table, filled_vs_puf_and_who,
             lambda: ['## 4. Holders after filling in, by wave, against the IRS (29_report.py section B)', '']
             + rep29.real_section(t31),
             lambda: ['## 5a. The tilt with the wealth counts on the SCF\'s units (29_report.py sections C and D)', '']
             + rep29.tilt_sections(),
             lambda: file_base_tilts(rep29)]
    if os.environ.get('STEP31_REPORT_PARTS') == 'k_only':      # smoke test of sections 0-3
        parts = parts[:2]
    for part in parts:
        try:
            lines += part()
        except Exception as ex:          # report what failed, keep the other sections
            import traceback
            lines += ['', f'**Section failed: {ex!r}**', '', '```', traceback.format_exc(), '```', '']
        open(f'{OUT}/report.md', 'w').write('\n'.join(lines) + '\n')
    print('\n'.join(lines))


if __name__ == '__main__':
    main()
