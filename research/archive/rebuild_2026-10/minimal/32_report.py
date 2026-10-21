"""
32_report.py  (minimal example, step 32: a 2019 starting file that mixes SCF
waves. Status: 32_STATUS.md.)

Reads whatever step 32 outputs exist and writes step32/report.md:
  0. the maps' checks (map/V32/map_check_v32.csv) and the stacks' checks
     (step32/stack_check_ks<k>.csv);
  1. the reproduction check: the 100%-2016-token file (m16) tilted through
     the new path against step 31's own tilt, weight by weight;
  2. net worth before and after the tilt, nominal 2019 dollars: total,
     median, top 1% share, mean by wage group, mean for units with none of
     interest, dividends or capital gains (27_report.stats, as step 31);
  3. the tilt: move with the 136 IRS targets alone and with the 8 wealth
     targets, records kept, records cut below a tenth of their weight,
     whether every target is met;
  4. the 2019 token's share of the weight in the mixed file, before and after.
With argument "check" only section 1 is written, to step32/check_m16.md.

Files (32_mix.py): m16 = 100% 2016 wave (step 31's stack), m80 = 80% 2019
wave and 20% 2016 wave, m19 = 100% 2019 wave. Dollar rules: "own wave" =
each rank into dollars by its own wave's map (m16 under it is step 31);
"2019 map" (t19) = every rank into dollars by the 2019 wave's map.
"""
import importlib.util
import os
import sys

import numpy as np
import pandas as pd

HERE = 'research/rebuild/minimal'
R = '/nfs/roberts/scratch/pi_nrs36/jar335/minimal'
OUT = f'{R}/step32'
TO_2019 = 1.2493059 / 1.1592053        # 2015 dollars to nominal 2019 (29_report.py)
SEEDS = (1, 2, 3)
# (column label, mix, dollar rule)
VARIANTS = [('100% 2016 wave (step 31)', 'm16', ''),
            ('80% 2019 / 20% 2016', 'm80', ''),
            ('100% 2019 wave', 'm19', ''),
            ('100% 2016 wave, 2019 map', 'm16', 't19'),
            ('80/20, 2019 map', 'm80', 't19')]
BASES = [('SCF units', 'scf_units', ''), ('file units', 'file_units', '_file')]


def load(name, f):
    s = importlib.util.spec_from_file_location(name, f'{HERE}/{f}')
    m = importlib.util.module_from_spec(s)
    s.loader.exec_module(m)
    return m


def md(df, fmt='.4f'):
    return df.to_markdown(index=False, floatfmt=fmt) if len(df) else '(nothing yet)'


def tag(k, mix, rule, sfx):
    v = f's32_ks{k}_{mix}'
    return v, f'step32/{v}' + (f'_{rule}' if rule else '') + sfx


def check_section():
    rows = []
    for k in SEEDS:
        for bname, _, sfx in BASES:
            v, t = tag(k, 'm16', '', sfx)
            a = f'{R}/tilt/step31/s31_ks{k}{sfx}'
            if not (os.path.exists(f'{R}/tilt/{t}/weights_{v}.parquet') and os.path.exists(f'{a}/weights_s31_ks{k}.parquet')):
                rows.append({'seed': k, 'base': bname, 'status': 'missing'})
                continue
            new = pd.read_parquet(f'{R}/tilt/{t}/weights_{v}.parquet')
            old = pd.read_parquet(f'{a}/weights_s31_ks{k}.parquet')
            sn = pd.read_csv(f'{R}/tilt/{t}/summary_{v}.csv')
            so = pd.read_csv(f'{a}/summary_s31_ks{k}.csv')
            rel = {c: float(np.max(np.abs(new[c].to_numpy() / old[c].to_numpy() - 1)))
                   for c in ['weight_before', 'weight_after_136', 'weight_after']}
            ok = len(new) == len(old) and max(rel.values()) < 1e-9
            rows.append({'seed': k, 'base': bname, 'status': 'reproduced' if ok else 'DIFFERS',
                         'largest relative difference, weight before': rel['weight_before'],
                         'same, after 136 targets': rel['weight_after_136'],
                         'same, after all targets': rel['weight_after'],
                         'move with wealth, step 32 (nats)': sn['kl_nats'].iloc[1],
                         'move with wealth, step 31 (nats)': so['kl_nats'].iloc[1]})
    df = pd.DataFrame(rows)
    return ['## 1. Reproduction check: 100% 2016 token through the new path against step 31', '',
            'The file is step 31\'s stack with a net worth year column added; net worth dollars come '
            'through the per-wave path (map/V32). "Reproduced" means every weight equals step 31\'s '
            'to a relative 1e-9.', '', md(df, '.3g'), '']


def stats_rows(rep27, tm, k, mix, rule, sfx, when):
    v, t = tag(k, mix, rule, sfx)
    if not os.path.exists(f'{R}/tilt/{t}/weights_{v}.parquet'):
        return None, None
    os.environ['MINIMAL_MAP_DIR'] = f'{R}/map/V32'
    os.environ['MINIMAL_NW_MAP_YEARS'] = '2015,2018'
    if rule == 't19':
        os.environ['MINIMAL_NW_TARGET_MAP_YEAR'] = '2018'
    else:
        os.environ.pop('MINIMAL_NW_TARGET_MAP_YEAR', None)
    os.environ['GEN_FILE'] = f'{R}/gen/gen_puf_stack_{v}.parquet'
    g = tm.generated_2019()
    wts = pd.read_parquet(f'{R}/tilt/{t}/weights_{v}.parquet')
    assert (wts['record_id'].to_numpy() == g['record_id'].to_numpy()).all()
    nw = g['net_worth'].to_numpy(float) * TO_2019
    assert not np.isnan(nw).any(), 'net worth missing on a generated record'
    out = {w: rep27.stats(g, wts[c].to_numpy(), nw) for w, c in [('before', 'weight_before'), ('after', 'weight_after')]
           if w in when}
    tok = g['net_worth_year'].to_numpy()
    share = {w: 100 * wts.loc[tok == 2018, c].sum() / wts[c].sum()
             for w, c in [('before', 'weight_before'), ('after', 'weight_after')]}
    return out, share


def wealth_sections():
    rep27 = load('rep27', '27_report.py')
    wt = load('wt', '06_wealth_tables.py')
    tm = wt.tiltmod
    u19, x19, w19, _ = wt.scf2019()
    scf = rep27.stats(u19, np.asarray(w19, float), np.asarray(x19, float) * TO_2019)
    cols, shares, moves = {}, [], []
    for label, mix, rule in VARIANTS:
        for bname, _, sfx in BASES:
            for k in SEEDS:
                when = ('before', 'after') if sfx == '' else ('after',)
                st, sh = stats_rows(rep27, tm, k, mix, rule, sfx, when)
                if st is None:
                    continue
                for w, s in st.items():
                    key = f'{label}, before' if w == 'before' else f'{label}, after ({bname})'
                    cols.setdefault(key, []).append(pd.Series(s))
                if mix == 'm80':
                    shares.append({'file': label, 'base': bname, 'seed': k,
                                   '2019 token share of weight, before (%)': sh['before'],
                                   'after (%)': sh['after']})
                v, t = tag(k, mix, rule, sfx)
                s = pd.read_csv(f'{R}/tilt/{t}/summary_{v}.csv')
                lines = pd.read_csv(f'{R}/tilt/{t}/wealth_lines_{v}.csv')
                fit = pd.read_csv(f'{R}/tilt/{t}/fit_{v}.csv')
                moves.append({'file': label, 'base': bname, 'seed': k,
                              'move, 136 IRS (nats)': s['kl_nats'].iloc[0],
                              'move, 136 IRS + 8 wealth (nats)': s['kl_nats'].iloc[1],
                              'records kept, effective (%)': 100 * s['ess_ratio'].iloc[1],
                              'records cut below 0.1': int(s['records_factor_under_0.1'].iloc[1]),
                              'converged': bool(s['converged'].iloc[1]),
                              'largest miss, 136 IRS (%)': float(fit['gap_after_pct'].abs().max()),
                              'largest miss, 8 wealth (%)': float(lines['posttrain miss %'].abs().max())})
    lines = ['## 2. Net worth before and after the tilt (nominal 2019 dollars; mean of the seeds found)', '']
    t = pd.DataFrame({'SCF 2019, all units': pd.Series(scf)})
    for key, ss in cols.items():
        t[f'{key} [{len(ss)}]'] = pd.concat(ss, axis=1).mean(axis=1)
    order = [c for c in t.columns if c.startswith('SCF')] + [c for c in t.columns if ', before' in c] + \
            [c for c in t.columns if 'SCF units)' in c] + [c for c in t.columns if 'file units)' in c]
    for part, sel in [('Before the tilt', lambda c: c.startswith('SCF') or ', before' in c),
                      ('After the tilt, wealth counts on the SCF\'s units', lambda c: c.startswith('SCF') or 'SCF units)' in c),
                      ('After the tilt, wealth counts on the file\'s units', lambda c: c.startswith('SCF') or 'file units)' in c)]:
        lines += [f'### {part}', '', '[n] = seeds averaged.', ''] + rep27.fmt_table(t[[c for c in order if sel(c)]]) + ['']
    lines += ['### Every seed, total net worth ($ trillion) and median ($)', '']
    per = []
    for key, ss in cols.items():
        for i, s in enumerate(ss):
            per.append({'column': key, 'seed (order found)': i + 1, 'total ($T)': s['total net worth ($ trillion)'],
                        'median ($)': s['median ($)'], 'top 1% share (%)': s['top 1% share of net worth (%)'],
                        'none of the three, mean ($)': s['mean, none of interest, dividends, capital gains ($)']})
    lines += [md(pd.DataFrame(per), '.1f'), '']
    mv = pd.DataFrame(moves)
    lines += ['## 3. The tilt', '']
    if len(mv):
        lines += ['Mean over seeds:', '',
                  md(mv.groupby(['file', 'base'], sort=False).mean(numeric_only=True).reset_index(), '.4f'), '',
                  'Every seed:', '', md(mv, '.4f'), '']
    lines += ['## 4. The 2019 token\'s share of the weight, 80/20 file', '', md(pd.DataFrame(shares), '.2f'), '']
    return lines


def setup_section():
    lines = ['## 0. Maps and stacks', '']
    p = f'{R}/map/V32/map_check_v32.csv'
    if os.path.exists(p):
        lines += [md(pd.read_csv(p), '.5f'), '']
    for k in SEEDS:
        p = f'{OUT}/stack_check_ks{k}.csv'
        if os.path.exists(p):
            lines += [f'Seed {k}:', '', md(pd.read_csv(p), '.4g'), '']
    return lines


def main():
    os.makedirs(OUT, exist_ok=True)
    if len(sys.argv) > 1 and sys.argv[1] == 'check':
        text = '\n'.join(['# Step 32: reproduction check', ''] + check_section()) + '\n'
        open(f'{OUT}/check_m16.md', 'w').write(text)
        print(text)
        return
    lines = ['# Step 32: report tables', '', 'Written by 32_report.py from what exists on disk.', '']
    for part in [setup_section, check_section, wealth_sections]:
        try:
            lines += part()
        except Exception as ex:          # report what failed, keep the other sections
            import traceback
            lines += ['', f'**Section failed: {ex!r}**', '', '```', traceback.format_exc(), '```', '']
        open(f'{OUT}/report.md', 'w').write('\n'.join(lines) + '\n')
    print('\n'.join(lines))


if __name__ == '__main__':
    main()
