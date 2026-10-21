"""
36_tilt.py  (minimal example, step 36: the 2019 tilt with business income targets)

John, 19 October. Does the tilt absorb the shortfall in business income
holders, and what does it cost? And how many business targets, built how?
For one step-34 stack (STACK_VERSION), the tilt of 07_baseline.py (the 136
IRS targets and the non-filer count exact, the eight wealth targets soft at
2%) is run again with the SOI 2019 business targets added (exact), at three
levels of detail:

  totals     each family's net income and net loss, returns and amount: 24 targets
  coarse     the same in six AGI groups (no AGI, under $50k, $50k-100k,
             $100k-200k, $200k-1m, $1m and over): 144
  brackets   the same in the 136 targets' fifteen AGI brackets: 360

Families and how a return counts (net income, net loss) as
36_business_targets.py chose from the 2015 PUF against SOI 2015
(step36/family_rule.json). Targets are filers only, as the 136.

Reported for each: whether the tilt converges, its cost (effective sample
size against the start, the KL move, the spread of weight factors), the
business targets before and after, and what the business targets leave
untargeted: holders of each of the PUF's parts (partnership against S
corporation, passive against nonpassive), units holding both partnership and
S corporation income, and the wealth lines.

Constructed targets (section E). Where SOI does not publish an item, a target
would have to be built. The test here, on items SOI does publish: build the
2019 business cells from the real 2015 PUF two ways and compare with the
published 2019 cells:
  naive   2015 PUF counts times the growth in returns, amounts times the
          growth in returns times the growth in AGI per return
  tilted  the real 2015 PUF (filers), its dollars grown to 2019 (the five AGI
          items by step 4's factors, the business parts by step 34's
          per-holder factors), tilted to the 136 IRS targets; the cells read
          off the tilted file
The gap between either and the published 2019 numbers is the error a
constructed target would carry for items like these.

Writes $R/tilt/step36/<v>/: summary.csv, business_fit.csv, untargeted.csv,
constructed.csv, weights.parquet, tables.md.
Usage: research/rebuild/minimal/36_tilt.sbatch <run>   (as 34_tilt.sbatch)
"""
import importlib.util
import json
import os
import sys

import numpy as np
import pandas as pd

HERE = os.path.dirname(os.path.abspath(__file__))
ROOT = '/nfs/roberts/scratch/pi_nrs36/jar335/minimal'
V = os.environ['STACK_VERSION']
OUT = f'{ROOT}/tilt/step36/{V}'


def load(name, f):
    s = importlib.util.spec_from_file_location(name, f'{HERE}/{f}')
    mod = importlib.util.module_from_spec(s)
    s.loader.exec_module(mod)
    return mod


b07 = load('b07', '07_baseline.py')
BT = load('bt36', '36_business_targets.py')
wt, dollars, tiltmod = b07.wt, b07.dollars, b07.tiltmod
RULE = json.load(open(f'{ROOT}/step36/family_rule.json'))
FAMILIES = list(BT.FIELDS)
COARSE = [(-np.inf, 1, 'no AGI'), (1, 50e3, 'under $50k'), (50e3, 100e3, '$50k-100k'),
          (100e3, 200e3, '$100k-200k'), (200e3, 1e6, '$200k-1m'), (1e6, np.inf, '$1m and over')]


SOI = pd.read_csv(f'{ROOT}/targets/soi_business_2015_2019.csv')


def business_cells(groups, year=2019):
    """Published cells per family, side and AGI group; groups merged, per
    family and side, where SOI combined cells across a group edge."""
    rows = []
    for fam in FAMILIES:
        for side in ('net_income', 'net_loss'):
            for lo, hi, lab, c, m in BT.family_groups(SOI, year, fam, side, groups):
                rows += [{'family': fam, 'side': side, 'group': lab, 'lo': lo, 'hi': hi,
                          'statistic': 'count', 'value': c},
                         {'family': fam, 'side': side, 'group': lab, 'lo': lo, 'hi': hi,
                          'statistic': 'amount', 'value': m}]
    return pd.DataFrame(rows)


def business_targets(groups, year=2019):
    return business_cells(groups, year).drop(columns=['lo', 'hi'])


def business_contributions(df, groups, year=2019):
    """Records-by-cells, in business_cells' order: 1/0 for returns, dollars for amounts."""
    a = tiltmod.soi_cells.agi(df)
    sides = {fam: dict(zip(['net_income', 'net_loss'], BT.family_sides(df, fam, RULE[fam]))) for fam in FAMILIES}
    cols = []
    for r in business_cells(groups, year).itertuples():
        x = sides[r.family][r.side]
        inside = (a >= r.lo) & (a < r.hi)
        cols.append(inside * (x > 0) if r.statistic == 'count' else inside * x)
    return np.column_stack(cols).astype(float)


def untargeted(df, w):
    """What the business targets do not pin down, on one file and weights (filers)."""
    s = w.sum()
    out = {}
    for p in BT.PARTNERSHIP + BT.SCORP + ['rent_inc', 'rent_loss', 'estate_inc', 'estate_loss', 'sch_c', 'farm']:
        out[f'holders: {p} (%)'] = 100 * w[df[p].to_numpy(float) != 0].sum() / s
    pn, sn = df['part_nonpass_inc'].to_numpy() != 0, df['scorp_nonpass_inc'].to_numpy() != 0
    anyp = (df[BT.PARTNERSHIP].to_numpy(float) != 0).any(1)
    anys = (df[BT.SCORP].to_numpy(float) != 0).any(1)
    out['holders: any partnership item (%)'] = 100 * w[anyp].sum() / s
    out['holders: any S corporation item (%)'] = 100 * w[anys].sum() / s
    out['both partnership and S corporation items (%)'] = 100 * w[anyp & anys].sum() / s
    out['S corp nonpassive income among partnership nonpassive holders (%)'] = \
        100 * w[pn & sn].sum() / w[pn].sum()
    inc_p = df[['part_pass_inc', 'part_nonpass_inc']].to_numpy(float).sum(1)
    inc_s = df[['scorp_pass_inc', 'scorp_nonpass_inc']].to_numpy(float).sum(1)
    out['partnership share of partnership + S corp income (%)'] = \
        100 * (w * inc_p).sum() / (w * (inc_p + inc_s)).sum()
    out['passive share of partnership + S corp income (%)'] = 100 * (w * df[['part_pass_inc', 'scorp_pass_inc']]
                                                                     .to_numpy(float).sum(1)).sum() / \
        (w * (inc_p + inc_s)).sum()
    return out


def real_2019():
    """The real 2015 PUF filers, dollars grown to 2019 (section E)."""
    rep = load('rep34', '34_report.py')
    real, w = rep.real_puf_side()
    f = real['filer'].to_numpy() == 1
    real, w = real[f].reset_index(drop=True), w[f]
    bus15 = real['business_income'].to_numpy(float)
    other_full = real['agi'].to_numpy(float) - real[['wages', 'interest', 'dividends', 'capital_gains']] \
        .to_numpy(float).sum(1)
    core = pd.read_csv(f'{ROOT}/map/growth_factors.csv')
    for v, fac in zip(core['variable'], core['factor']):
        if v != 'other_agi':
            real[v] = dollars.grow_values(v, real[v], fac)
    g34 = pd.read_csv(f'{os.environ["MINIMAL_MAP_DIR"]}/growth_factors_34.csv')
    for part, fp, fn in zip(g34['part'], g34['factor, positive values'], g34['factor, negative values']):
        x = real[part].to_numpy(float)
        real[part] = np.where(x >= 0, x * fp, x * fn)
    bus19 = sum(rep.SIGN[p] * real[p].to_numpy(float) for p in rep.PARTS)
    f_other = float(core.loc[core['variable'] == 'other_agi', 'factor'].iloc[0])
    # AGI minus the four items: everything but business income grown with other_agi's factor,
    # business income replaced by its grown parts (an approximation for bracket placement)
    real['other_agi'] = (other_full - bus15) * f_other + bus19
    return real, w


def main():
    os.makedirs(OUT, exist_ok=True)
    os.environ['GEN_FILE'] = b07.STACK
    g19 = tiltmod.generated_2019()
    raw = pd.read_parquet(b07.STACK)
    g15, _ = dollars.draw_dollars(raw, dollars.puf_maps(2015))
    imp = g19['weight'].to_numpy(float)
    brackets, cells = tiltmod.target_set('brackets')
    returns = tiltmod.target_set('totals')[1].query("cell == 'returns'")['value'].iloc[0]
    is_filer = g19['filer'].to_numpy().astype(float)
    f_ = is_filer == 1
    n_nonfilers = b07.nonfiler_count_2019()
    w0 = np.where(f_, imp * returns / imp[f_].sum(), imp * n_nonfilers / imp[~f_].sum())
    F = tiltmod.contributions(g19, brackets) * is_filer[:, None]
    T = cells['value'].to_numpy(float)
    x = g15['net_worth'].to_numpy(float)
    units = os.environ.get('WEALTH_UNITS', 'all')
    targets, threshold = wt.wealth_targets(units)
    Fq, Tq = b07.threshold_form(x, returns, targets, threshold, units)
    base_F = np.column_stack([F, (1 - is_filer)[:, None], Fq])
    base_T = np.r_[T, n_nonfilers, Tq]
    base_tol = np.r_[np.zeros(len(T) + 1), np.full(len(Tq), wt.SIGMA)]

    levels = {'totals': [(-np.inf, np.inf, 'all')], 'coarse': COARSE, 'brackets': brackets}
    summary, fits, unt, W = [], [], [], {'start': w0}
    unt.append({'weights': 'start (pretrain)', **untargeted(g19[f_], w0[f_])})
    for name in ['none'] + list(levels):
        if name == 'none':
            Fb, Tb, bt = np.zeros((len(g19), 0)), np.zeros(0), None
        else:
            bt = business_targets(levels[name])
            Fb = business_contributions(g19, levels[name]) * is_filer[:, None]
            Tb = bt['value'].to_numpy(float)
            keep = Tb != 0
            reach = np.array([np.any(c > 0) for c in Fb.T])
            drop = keep & ~reach
            if drop.any():
                print(f'{name}: {int(drop.sum())} published cells no record can reach, left out:\n'
                      f'{bt[drop].to_string()}', flush=True)
            use = keep & reach
            zero = ~keep
            if zero.any():
                print(f'{name}: {int(zero.sum())} published cells are zero, left out', flush=True)
            bt = bt.assign(used=use)
            Fb, Tb = Fb[:, use], Tb[use]
        w1, ok, it = tiltmod.tilt(w0, np.column_stack([base_F, Fb]), np.r_[base_T, Tb],
                                  rel_tol=np.r_[base_tol, np.zeros(len(Tb))])
        mode = 'exact'
        if not ok:
            print(f'{name}: exact business targets did not converge after {it} steps; soft at 2%', flush=True)
            w1, ok, it = tiltmod.tilt(w0, np.column_stack([base_F, Fb]), np.r_[base_T, Tb],
                                      rel_tol=np.r_[base_tol, np.full(len(Tb), wt.SIGMA)])
            mode = 'soft, 2%'
        W[name] = w1
        gap136 = 100 * np.abs(F.T @ w1 / T - 1).max()
        summary.append({'business targets': name, 'number': len(Tb), 'form': mode, 'converged': ok,
                        'newton steps': it, 'largest miss on the 136 (%)': gap136,
                        **tiltmod.move(w0, w1)})
        unt.append({'weights': f'tilted, business targets: {name}', **untargeted(g19[f_], w1[f_])})
        print(pd.Series(summary[-1]).to_string(), flush=True)
    # business fit by family, totals level, under every set of weights
    bt = business_targets(levels['totals'])
    Fb = business_contributions(g19, levels['totals']) * is_filer[:, None]
    fit = bt.drop(columns='group').rename(columns={'value': 'SOI 2019'})
    for k, w in W.items():
        fit[k] = Fb.T @ w
        fit[f'{k}, ratio'] = fit[k] / fit['SOI 2019']
    fit.to_csv(f'{OUT}/business_fit.csv', index=False)
    btc = business_targets(COARSE)
    Fc = business_contributions(g19, COARSE) * is_filer[:, None]
    fitc = btc.rename(columns={'value': 'SOI 2019'})
    for k in ['start', 'none', 'totals']:
        fitc[f'{k}, ratio'] = (Fc.T @ W[k]) / fitc['SOI 2019']
    # wealth lines after each
    wl = pd.DataFrame({'wealth target': [n for n, _, _ in b07.threshold_names(threshold, units)], 'target': Tq})
    for k, w in W.items():
        wl[f'{k}, ratio'] = (Fq.T @ w) / Tq

    # E. constructed targets against the published 2019 cells
    real, wr = real_2019()
    Fr = tiltmod.contributions(real, brackets)
    wr1, okr, _ = tiltmod.tilt(wr, Fr, T)
    assert okr, 'real PUF tilt to the 136 did not converge'
    tot15 = pd.read_csv(f'{ROOT}/targets/soi_totals_2015_2019.csv')
    tot15 = tot15[(tot15['table'] == 1.4) & (tot15['filing_status'] == 'all') & (tot15['agi_bracket_label'] == 'all')]
    val = {(r.year, r.variable): r.value for r in tot15.itertuples() if r.statistic in ('count', 'amount')}
    g_ret = val[(2019, 'returns')] / val[(2015, 'returns')]
    g_agi = (val[(2019, 'agi')] / val[(2019, 'returns')]) / (val[(2015, 'agi')] / val[(2015, 'returns')])
    cons = []
    for lev in ['totals', 'coarse']:
        b19 = business_targets(levels[lev], 2019)
        real15, w15 = load('rep34b', '34_report.py').real_puf_side()
        f15 = real15['filer'].to_numpy() == 1
        real15, w15 = real15[f15].reset_index(drop=True), w15[f15]
        real15 = real15.assign(other_agi=real15['agi'] - real15[['wages', 'interest', 'dividends', 'capital_gains']]
                               .sum(axis=1))
        naive = business_contributions(real15, levels[lev]).T @ w15
        naive = naive * np.where(b19['statistic'] == 'count', g_ret, g_ret * g_agi)
        tilted = business_contributions(real, levels[lev]).T @ wr1
        cons.append(b19.assign(level=lev, naive=naive, tilted=tilted,
                               **{'naive, ratio': naive / b19['value'], 'tilted, ratio': tilted / b19['value']}))
    cons = pd.concat(cons, ignore_index=True).rename(columns={'value': 'SOI 2019'})
    cons.to_csv(f'{OUT}/constructed.csv', index=False)

    summary = pd.DataFrame(summary)
    summary.to_csv(f'{OUT}/summary.csv', index=False)
    unt = pd.DataFrame(unt).set_index('weights').T.reset_index().rename(columns={'index': 'statistic'})
    real15, w15 = real_2019()
    unt.insert(1, 'real PUF 2015 (filers)', [untargeted(real15, w15)[k] for k in unt['statistic']])
    unt.to_csv(f'{OUT}/untargeted.csv', index=False)
    pd.DataFrame({'record_id': g19['record_id'], **{f'weight_{k}': w for k, w in W.items()}}).to_parquet(
        f'{OUT}/weights.parquet', index=False)
    md = lambda d, f=',.3f': d.to_markdown(index=False, floatfmt=f)
    text = '\n'.join([
        f'# The 2019 tilt with business income targets: {V}', '',
        'Written by `36_tilt.py`. Every tilt: the 136 IRS targets and the non-filer count exact, the eight '
        'wealth targets soft (2%); then the SOI 2019 business targets (exact unless stated) at the level shown. '
        'Effective sample size and KL against the start (the importance weights scaled to the counts). '
        'Family readings: ' + ', '.join(f'{k} {v}' for k, v in RULE.items()) + '.', '',
        '## A. Fit and cost', '', md(summary), '',
        '## B. Business targets, all AGI together (SOI 2019 and each set of weights)', '', md(fit), '',
        '## C. Business targets by coarse AGI group: ratio to SOI 2019', '', md(fitc), '',
        '## D. Untargeted (filers)', '', md(unt), '',
        '## Wealth targets after each tilt (ratio to target)', '', md(wl), '',
        '## E. Constructed targets against the published 2019 cells', '',
        f'naive: 2015 PUF counts x {g_ret:.4f} (growth in returns), amounts x {g_ret * g_agi:.4f} (and in AGI per '
        'return); tilted: real 2015 PUF grown and tilted to the 136.', '', md(cons), ''])
    open(f'{OUT}/tables.md', 'w').write(text)
    print(text)


if __name__ == '__main__':
    sys.exit(main())
