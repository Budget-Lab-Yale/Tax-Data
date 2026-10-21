"""Business income split into parts: where the shortfall in the total comes from.

John, 19 October: the business income total computed from its eighteen drawn
parts averages 89% (base network) to 92% (bigger network) of the PUF's, against
98% when the total was drawn as one item (step 33). Before choosing between
adding SOI targets for the parts and drawing the total as one item, find where
the gap sits. Nothing is trained; reads the existing stacks.

  A. The gap in mean business income per unit, part by part, each split into
     "too few or too many holders" and "holders' amounts too low or high".
  B. The total by size group: share of units and dollars per unit (all units),
     real against generated, including step 33's one-item draw.
  C. By how many kinds of business income a unit holds: share of units and
     dollars per unit.
  D. Sampling error of the mean: how much of the seed-to-seed spread is the
     400,000-record stack itself rather than the model.

Output: $R/step34/shortfall.md.
  sbatch -J short34 --partition=day --time=1:00:00 --mem=48G --wrap \
    "module load ...; python research/rebuild/minimal/34_shortfall.py"
"""
import importlib.util
import os

import numpy as np
import pandas as pd

HERE = os.path.dirname(os.path.abspath(__file__))
R = '/nfs/roberts/scratch/pi_nrs36/jar335/minimal'


def load(name, f):
    s = importlib.util.spec_from_file_location(name, f'{HERE}/{f}')
    mod = importlib.util.module_from_spec(s)
    s.loader.exec_module(mod)
    return mod


rep = load('rep34', '34_report.py')
PARTS, SIGN, md = rep.PARTS, rep.SIGN, rep.md
RUNS34 = ['Ns1', 'Ns2', 'Ns3', 'Bs1', 'Bs2']
RUNS33 = ['Ns1', 'Ns2', 'Ns3']

# Kinds, as in the report's section C: a unit holds a kind if any of its fields is nonzero.
KINDS = {'Schedule C': ['sch_c'],
         'partnership': ['part_pass_inc', 'part_nonpass_inc', 'part_pass_loss', 'part_nonpass_loss', 'part_179'],
         'S corporation': ['scorp_pass_inc', 'scorp_nonpass_inc', 'scorp_pass_loss', 'scorp_nonpass_loss', 'scorp_179'],
         'rent or royalty': ['rent_inc', 'rent_loss'],
         'estate or trust': ['estate_inc', 'estate_loss'],
         'other Schedule E': ['farm_rent', 'sche_other'],
         'farm': ['farm']}
SIZE = [(-np.inf, -1e5, 'loss over $100k'), (-1e5, -1e4, 'loss $10k-100k'), (-1e4, 0, 'loss under $10k'),
        (0, 0, 'none'), (0, 1e4, '$1-10k'), (1e4, 5e4, '$10k-50k'), (5e4, 1e5, '$50k-100k'),
        (1e5, 2.5e5, '$100k-250k'), (2.5e5, 1e6, '$250k-1m'), (1e6, 5e6, '$1m-5m'), (5e6, np.inf, '$5m and over')]


def in_size(x, lo, hi):
    if lo == hi:
        return x == lo
    return (x > lo) & (x <= hi) if lo >= 0 else (x > lo) & (x <= hi) & (x < 0)


def wmean(x, w):
    return float((w * x).sum() / w.sum())


def se_mean(x, w):
    """Standard error of a weighted mean, treating records as independent draws."""
    p = w / w.sum()
    m = (p * x).sum()
    return float(np.sqrt((p ** 2 * (x - m) ** 2).sum()))


def main():
    dollars = load('dollars', '04_dollars.py')
    real, wr = rep.real_puf_side()
    gens = {}
    for r in RUNS34:
        if os.path.exists(f'{R}/gen/gen_puf_stack_s34_{r.lower()}.parquet'):
            gens[f'parts {r}'] = rep.gen_2015('s34', r, dollars, f'{R}/map/V34')
    g33 = {}
    for r in RUNS33:
        if os.path.exists(f'{R}/gen/gen_puf_stack_s33_{r.lower()}.parquet'):
            g33[f'one item {r}'] = rep.gen_2015('s33', r, dollars, f'{R}/map/V33')
    lines = ['# Business income split into parts: where the shortfall comes from', '',
             'Written by `34_shortfall.py`. 2015 dollars, all PUF-side units (filers and crude non-filers). '
             'Real: table E34\'s PUF side with its weights. Generated: each run\'s 400,000-record stack with its '
             'importance weights, before reweighting. "parts" = step 34 (total computed from eighteen drawn parts; '
             'Ns = base network, Bs = bigger network); "one item" = step 33 (total drawn as one item).', '']

    # A. part by part
    lines += ['## A. Gap in mean business income per unit, by part ($ per unit, signed as in the sum)', '',
              'For each part: real and generated contribution to mean business income per unit (sign applied), '
              'and the gap split into the holder effect (holder share gap x real mean amount per holder) and the '
              'amount effect (generated holder share x gap in mean amount per holder). Mean of the runs listed.', '']
    for lab, runs in [('base network (Ns1-3)', [k for k in gens if 'Ns' in k]),
                      ('bigger network (Bs1-2)', [k for k in gens if 'Bs' in k])]:
        if not runs:
            continue
        rows = []
        for p in PARTS + ['business_income']:
            s = SIGN.get(p, 1)
            xr = real[p].to_numpy(float)
            hr = wr[xr != 0].sum() / wr.sum()
            ar = (wr * xr)[xr != 0].sum() / wr[xr != 0].sum() if hr > 0 else 0.0
            hold_eff, amt_eff, contrib = [], [], []
            for k in runs:
                g, wg = gens[k]
                xg = g[p].to_numpy(float)
                hg = wg[xg != 0].sum() / wg.sum()
                ag = (wg * xg)[xg != 0].sum() / wg[xg != 0].sum() if hg > 0 else 0.0
                hold_eff.append(s * (hg - hr) * ar)
                amt_eff.append(s * hg * (ag - ar))
                contrib.append(s * wmean(xg, wg))
            rows.append({'part': p, 'real ($/unit)': s * wmean(xr, wr), 'generated ($/unit)': np.mean(contrib),
                         'gap': np.mean(contrib) - s * wmean(xr, wr),
                         'holder effect': np.mean(hold_eff), 'amount effect': np.mean(amt_eff),
                         'real holders (%)': 100 * hr,
                         'real mean amount per holder ($)': ar})
        lines += [f'### {lab}', '', md(pd.DataFrame(rows), ',.1f'), '']

    # B. by size of the total
    lines += ['## B. Business income total by size group', '',
              'Share of all units (%) and dollars per unit of all units contributed by the group ($). '
              'The dollar columns add to the mean.', '']
    allg = {**gens, **g33}
    share, dpu = [], []
    for lo, hi, lab in SIZE:
        srow, drow = {'group': lab}, {'group': lab}
        for name, (t, w) in [('PUF (real)', (real, wr))] + list(allg.items()):
            x = t['business_income'].to_numpy(float)
            sel = in_size(x, lo, hi)
            srow[name] = 100 * w[sel].sum() / w.sum()
            drow[name] = (w * x)[sel].sum() / w.sum()
        share.append(srow)
        dpu.append(drow)
    tot = {'group': 'mean (all units)'}
    for name, (t, w) in [('PUF (real)', (real, wr))] + list(allg.items()):
        tot[name] = wmean(t['business_income'].to_numpy(float), w)
    lines += ['### Share of units (%)', '', md(pd.DataFrame(share), ',.3f'), '',
              '### Dollars per unit ($)', '', md(pd.DataFrame(dpu + [tot]), ',.1f'), '']

    # C. by number of kinds held
    lines += ['## C. By number of kinds of business income held', '',
              'Kinds as in the report\'s section C (any field of the kind nonzero). Share of units (%), and mean '
              'business income per unit of the group ($).', '']
    rows = []
    for name, (t, w) in [('PUF (real)', (real, wr))] + list(gens.items()):
        nk = sum(np.any(np.stack([t[f].to_numpy(float) != 0 for f in fs]), axis=0) for fs in KINDS.values())
        x = t['business_income'].to_numpy(float)
        row = {'file': name}
        for k, lab in [(0, 'none'), (1, 'one'), (2, 'two'), (3, 'three or more')]:
            sel = nk == k if k < 3 else nk >= 3
            row[f'{lab}: units (%)'] = 100 * w[sel].sum() / w.sum()
            row[f'{lab}: mean ($)'] = wmean(x[sel], w[sel]) if sel.any() else np.nan
            row[f'{lab}: $ per unit of all'] = (w * x)[sel].sum() / w.sum()
        rows.append(row)
    lines += [md(pd.DataFrame(rows), ',.2f'), '']

    # D. sampling error
    rows = []
    for name, (t, w) in [('PUF (real)', (real, wr))] + list(allg.items()):
        x = t['business_income'].to_numpy(float)
        m, se = wmean(x, w), se_mean(x, w)
        rows.append({'file': name, 'records': len(x), 'mean ($)': m, 'standard error ($)': se,
                     'standard error (% of mean)': 100 * se / m,
                     'mean, ratio to PUF': m / wmean(real['business_income'].to_numpy(float), wr)})
    lines += ['## D. Sampling error of the mean', '',
              'Standard error of the weighted mean, treating records as independent draws with their weights. '
              'For a generated stack this is the error from drawing 400,000 records from the model, not the '
              'model\'s own error.', '', md(pd.DataFrame(rows), ',.2f'), '']

    os.makedirs(f'{R}/step34', exist_ok=True)
    open(f'{R}/step34/shortfall.md', 'w').write('\n'.join(lines) + '\n')
    print('\n'.join(lines))


if __name__ == '__main__':
    main()
