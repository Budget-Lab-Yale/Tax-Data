"""The dollar maps: for each band of each amount, the dollars a drawn record in that band gets, in target-year dollars
(the PUF's map grown by each item's growth line; one map per SCF wave grown per tax unit to the target year's wave; the
CEX, CPS and SIPP maps of the generation year and of each survey year in the target year's mix). The training table
build (prep/build_table.py) writes them for its own year; each target year runs this stage alone, from the table and
band edges already built.

  python -m scale.maps
"""
import json
import os

import numpy as np
import pandas as pd

from prep import build_table as BT
from shared import common
from shared import growth as GR
from shared.common import PUF_YEAR, TARGET_YEAR


def fill_value(edge, band):
    """The map value for a band with no record: a lump band's cut, else the band's own edge."""
    pos0 = edge['n_bands'] - (len(edge['positive']['cuts']) + 1)
    n_neg = pos0 - 1
    k = edge['negative']['bands_used']
    if 1 <= band <= n_neg and k == 0:
        return 0.0, 'no negatives in data: zero'
    if 1 <= band <= n_neg and band > k:        # an item with few losses uses fewer than n_neg loss bands
        c = edge['negative']['cuts']
        return (float(c[-1]) if c else -1.0), "loss band beyond this item's loss bands: its least negative cut"
    n_neg = k if 1 <= band <= n_neg else n_neg
    if band >= pos0:
        cuts, i = edge['positive']['cuts'], band - pos0
        lo, hi = (cuts[i - 1] if i >= 1 else 0.0), (cuts[i] if i < len(cuts) else None)
    else:
        cuts = edge['negative']['cuts']
        lo, hi = (cuts[band - 2] if band >= 2 else None), (cuts[band - 1] if band < n_neg else 0.0)
    if lo is not None and lo == hi:
        return float(lo), 'empty band: lump value at its cut'
    own, other = (hi, lo) if band >= pos0 else (lo, hi)
    return float(own if own is not None else other), 'no records in this subset: band edge'


def build_map(df, edges, source, variables, year=PUF_YEAR):
    rows = []
    for v in variables:
        x, b, w = df[v].to_numpy(float), df[f'{v}_band'].to_numpy(), df['weight'].to_numpy(float)
        ok = b >= 0
        m = pd.DataFrame({'variable': v, 'band': b[ok], 'value': x[ok], 'weight': w[ok], 'filled': False,
                          'rule': 'real records'})
        edge = edges[f'{source}/{year}/{v}']
        fills = [dict(zip(['value', 'rule'], fill_value(edge, k)), variable=v, band=k, weight=1.0, filled=True)
                 for k in sorted(set(range(edge['n_bands'])) - set(m['band']))]
        rows += [m, pd.DataFrame(fills, columns=m.columns)]
    return pd.concat(rows, ignore_index=True).sort_values(['variable', 'band', 'value'], kind='stable') \
        .reset_index(drop=True)


def cap_scf_map(m, base, wave):
    """The wave's dollar map with each item in settings dollars.scf_caps held to that percentile among the wave's
    holders (weighted, the wave's own dollars, before growth). Only the dollars a drawn band turns into change; the
    training table and its bands do not. Prints the cap and how many map values it lowers."""
    for v, q in common.SETTINGS['dollars'].get('scf_caps', {}).items():
        x, w = base[v].to_numpy(float), base['weight'].to_numpy(float)
        assert np.isfinite(x).all(), f'SCF wave {wave}: {v} missing or not finite'
        h = x > 0
        assert h.any(), f'SCF wave {wave}: no holders of {v}'
        order = np.argsort(x[h], kind='stable')
        xs, cw = x[h][order], np.cumsum(w[h][order]) / w[h].sum()
        cap = float(xs[min(np.searchsorted(cw, q), len(xs) - 1)])
        r = (m['variable'] == v).to_numpy() & (m['value'].to_numpy(float) > cap)
        common.say(f'SCF wave {wave}: {v} held to ${cap:,.0f} (the {q:.1%} point among holders); '
                   f'{int(r.sum())} map values lowered, the largest ${m.loc[r, "value"].max() if r.any() else 0:,.0f}')
        m.loc[r, 'value'] = cap
    return m


def scf_growth(scf, y_from, y_to):
    """{balance-sheet item: its growth per tax unit from the SCF wave of income year y_from to that of y_to}: the ratio
    of the two waves' weighted means per tax unit, each in its own dollars (all units and all five implicates, zeros
    included; scf: SCF rows with year, weight and the items in dollars). It carries price change and the change in
    holdings per unit together. The mortgage's share of the home's value (w_primary_ltv) is a ratio and is not grown:
    the mortgage in dollars moves with the home. An amount the SCF does not ask of some units (SCF_NOT_ASKED: elective
    deferrals, not asked outside the PEU): the mean over the units asked. An item whose growth.yaml line names 'scf net
    worth per unit between waves' (inheritance, since 3 November) takes the growth of net worth per tax unit (all units),
    net worth computed from the balance sheet where the frame has no net_worth column (the home mortgage as its share of
    the home's value times the home)."""
    out = {}
    if 'net_worth' not in scf:
        scf = scf.assign(net_worth=scf_net_worth(scf))
    for v in BT.SCF_AMOUNTS:
        if v in BT.SCF_RATIOS:
            # The mortgage share changes after the final wave so that the home and mortgage each follow their own Z.1 series.
            last = max(int(y) for y in scf['year'].unique()) + 1
            a, b = max(last, y_from + 1), max(last, y_to + 1)
            out[v] = (GR.scf_extension(a, b, 'value.primary_mortgage') /
                      GR.scf_extension(a, b, 'value.primary_home')) if v == 'w_primary_ltv' else 1.0
            continue
        # an item whose growth.yaml line names 'scf net worth per unit between waves' (inheritance, 3 November) is grown
        # by the SCF's net worth per tax unit, not by its own mean
        src = GR.line(v)['source']
        by = 'net_worth' if isinstance(src, dict) and src.get('series') == 'scf net worth per unit between waves' else v
        mean = {y: scf_mean(scf, by, y) for y in (y_from, y_to)}
        # a mean at or below zero (possible for the unrealized gains) makes the ratio meaningless: no factor
        out[v] = mean[y_to] / mean[y_from] if mean[y_from] > 0 and mean[y_to] > 0 else np.nan
    return out


def scf_net_worth(scf):
    """Net worth of SCF rows from the balance sheet: the 14 assets less the 6 debts, the home mortgage as its share of the
    home's value times the home (loan_to_value; the same as the units' own net worth)."""
    nw = np.zeros(len(scf))
    for c, sign in BT.NET_WORTH_PARTS.items():
        x = (scf['w_primary_ltv'] * scf['value.primary_home']) if c == 'value.primary_mortgage' else scf[c]
        nw = nw + sign * x.to_numpy(float)
    assert not np.isnan(nw).any(), 'SCF net worth: a balance-sheet item missing'
    return nw


def _wave_mean(scf, v, y):
    d = scf[scf.year == y]
    assert len(d), f'no SCF wave of income year {y}'
    if v in BT.SCF_NOT_ASKED:     # the units asked only: the others are missing by design, not zero
        d = d[d[v].notna()]
    assert not d[[v, 'weight']].isna().any().any(), f'SCF {y}: {v} or the weight missing on some unit'
    return float(np.average(d[v].to_numpy(float), weights=d['weight'].to_numpy(float)))


def scf_mean(scf, v, y):
    """The SCF's weighted mean per tax unit of item v at income year y (30 October, targets for every year): a wave's
    own mean where the table has a wave of income year y; between two waves, the geometric interpolation of their means
    (y = a + l (b - a): mean_a^(1 - l) mean_b^l; the arithmetic one if either mean is at or below zero); after the last
    wave, the last wave's mean grown by household net worth per person (the Federal Reserve's Financial Accounts, Z.1,
    over the Social Security area population; stock years y + 1 over the last wave's, the wave of income year y being
    interviewed in year y + 1; past 2025 Z.1 is held at its ratio to nominal GDP; growth.scf_extension). Before the
    first wave: an error."""
    have = sorted(int(x) for x in scf['year'].unique())
    if y in have:
        return _wave_mean(scf, v, y)
    lo, hi = max([w for w in have if w < y], default=None), min([w for w in have if w > y], default=None)
    assert lo is not None, f'SCF: income year {y} is before the first wave ({have[0]})'
    if hi is None:
        return _wave_mean(scf, v, lo) * GR.scf_extension(lo + 1, y + 1, v)
    a, b, lam = _wave_mean(scf, v, lo), _wave_mean(scf, v, hi), (y - lo) / (hi - lo)
    return a ** (1 - lam) * b ** lam if a > 0 and b > 0 else a + lam * (b - a)


def scf_units_at(scf, y):
    """The SCF's number of tax units (sum of weights, as the wave's rows carry them) at income year y, by the rule of
    scf_mean: a wave's own; between waves geometric interpolation; after the last wave grown with the Social Security
    area population (stock years y + 1 over the last wave's)."""
    have = sorted(int(x) for x in scf['year'].unique())
    n = lambda w: float(scf.loc[scf.year == w, 'weight'].sum())
    if y in have:
        return n(y)
    lo, hi = max([w for w in have if w < y], default=None), min([w for w in have if w > y], default=None)
    assert lo is not None, f'SCF: income year {y} is before the first wave ({have[0]})'
    if hi is None:
        return n(lo) * GR.population(y + 1) / GR.population(lo + 1)
    lam = (y - lo) / (hi - lo)
    return n(lo) ** (1 - lam) * n(hi) ** lam


def puf_target_map(m_puf, y=TARGET_YEAR):
    """The PUF year's map with every value grown to year y by its item's growth line (positive and negative values
    each by their own factor); and the factors."""
    g = {v: BT.growth(v, y) for v in BT.PUF_AMOUNTS}
    g.update({v: (1.0, 1.0) for v in BT.PUF_SHARES if v in set(m_puf['variable'])})   # a share is not grown
    bad = {k: f for k, f in g.items() if not all(np.isfinite(x) and x > 0 for x in f)}
    assert not bad, f'growth factors missing or not positive (growth.yaml): {bad}'
    m_target = m_puf.copy()
    for v, (fp, fn) in g.items():
        r = (m_target['variable'] == v).to_numpy()
        x = m_target.loc[r, 'value'].to_numpy(float).copy()
        x[x > 0] *= fp
        x[x < 0] *= fn
        m_target.loc[r, 'value'] = x
    return m_target, g


def build_maps(t, edges):
    os.makedirs(common.MAPS, exist_ok=True)
    GR.check(TARGET_YEAR)   # every dollar item has a growth line (stops otherwise)
    pside = t[t.source == 'puf'].reset_index(drop=True)
    m_puf = build_map(pside, edges, 'puf', BT.PUF_AMOUNTS + BT.PUF_SHARES)   # (the shares: the PUF's own values, every source)
    m_puf.to_parquet(f'{common.MAPS}/map_puf_{PUF_YEAR}.parquet', index=False)
    m_target, g = puf_target_map(m_puf)   # to the target year, by each item's growth line
    m_target.to_parquet(f'{common.MAPS}/map_puf_{TARGET_YEAR}.parquet', index=False)
    # the balance sheet: one map per wave, from the wave's own records on its own band edges, each item grown per tax
    # unit to the target year's wave (scf_growth). Generation draws each record's balance sheet from one wave
    # (generate.py), and dollars.py turns its bands into dollars with that wave's map.
    scf = t[t.source == 'scf']
    rows = []
    for y in sorted(scf['year'].unique()):
        base = scf[scf.year == y].reset_index(drop=True)
        f = scf_growth(scf, y, TARGET_YEAR - 1)
        rows += [{'wave': int(y) + 1, 'income year': int(y), 'variable': v, 'factor to the target year': f[v]}
                 for v in BT.SCF_AMOUNTS]
        no_factor = [v for v, x in f.items() if not np.isfinite(x)]
        if no_factor:   # the wave cannot be put in target-year dollars; generation stops if the year's mix uses it
            common.say(f'SCF wave {int(y) + 1}: NO MAP for {TARGET_YEAR}; a mean per unit at or below zero in '
                       f'{no_factor}')
            continue
        m = build_map(base, edges, 'scf', BT.SCF_AMOUNTS, year=y)
        m = cap_scf_map(m, base, int(y) + 1)
        m['value'] = m['value'].to_numpy(float) * m['variable'].map(f).to_numpy(float)
        assert not m['value'].isna().any(), f'SCF {y}: a map value with no growth factor'
        m.to_parquet(f'{common.MAPS}/map_scf_wave{int(y) + 1}_{TARGET_YEAR}.parquet', index=False)
    pd.DataFrame(rows).to_csv(f'{common.MAPS}/scf_growth_factors.csv', index=False)
    common.say('SCF balance sheet, growth per tax unit to the target year\'s wave, by wave:\n'
               + pd.DataFrame(rows).pivot(index='variable', columns='wave', values='factor to the target year')
               .to_string(float_format=lambda x: f'{x:.4f}'))
    # the CEX's categories, the CPS-only amounts and the SIPP's tips: the generation year's real records, and the same
    # grown to the target year by their growth lines (the CEX's by NIPA PCE per return, the category's PCE over SOI's
    # return count; the CPS's childcare and overtime and the SIPP's tips by SOI's growth in wages per return with wages,
    # a placeholder: no series measures them per unit; the CPS's shares, weeks and hours not grown; no growth when the
    # generation year is the target year)
    for src, sec, amounts in [('cex', BT.CX, BT.CEX_AMOUNTS), ('cps', BT.CP, BT.CPS_AMOUNTS), ('sipp', BT.SP, BT.SIPP_AMOUNTS)]:
        yg = sec['generation_year']
        m = build_map(t[(t.source == src) & (t.year == yg)].reset_index(drop=True), edges, src, amounts, year=yg)
        m.to_parquet(f'{common.MAPS}/map_{src}_{yg}.parquet', index=False)
        for v in amounts:
            f = GR.between(v, yg, TARGET_YEAR)
            g[v] = (f, f)
            r = (m['variable'] == v).to_numpy()
            m.loc[r, 'value'] = m.loc[r, 'value'] * f
        m.to_parquet(f'{common.MAPS}/map_{src}_{TARGET_YEAR}.parquet', index=False)
    pd.DataFrame([{'variable': v, 'factor, positive values': fp, 'factor, negative values': fn}
                  for v, (fp, fn) in g.items()]).to_csv(f'{common.MAPS}/growth_factors.csv', index=False)
    build_year_maps(t, edges)


def build_year_maps(t, edges):
    """One map per survey year of the CEX, the CPS and the SIPP in the target year's file (TARGETS.md section 7,
    survey_mix.py): that year's own records, every value grown from that year to the target year, by the same rules
    the single-year maps above use (each item's growth line, growth.yaml). The CEX's categories: NIPA PCE per return,
    the category's PCE over SOI's return count, target year over the survey year. The CPS's childcare and the SIPP's tips: SOI's wages per return with
    wages, target year over the survey year (a placeholder: no series measures them per unit); the CPS's shares, weeks
    and hours are not grown. For the target year itself the factor is 1. Files map_<survey>_from<year>_to<target>.parquet;
    the factors are in survey_year_growth_factors.csv."""
    rows = []
    for src, amounts in [('cex', BT.CEX_AMOUNTS), ('cps', BT.CPS_AMOUNTS), ('sipp', BT.SIPP_AMOUNTS)]:
        for y in BT.mix_years(src):
            m = build_map(t[(t.source == src) & (t.year == y)].reset_index(drop=True), edges, src, amounts, year=y)
            for v in amounts:
                f = GR.between(v, y, TARGET_YEAR)   # the item's growth line (growth.yaml)
                r = (m['variable'] == v).to_numpy()
                m.loc[r, 'value'] = m.loc[r, 'value'].to_numpy(float) * f
                rows.append({'survey': src, 'year': y, 'variable': v, 'factor to the target year': f})
            m.to_parquet(f'{common.MAPS}/map_{src}_from{y}_to{TARGET_YEAR}.parquet', index=False)
    f = pd.DataFrame(rows)
    f.to_csv(f'{common.MAPS}/survey_year_growth_factors.csv', index=False)
    common.say('CEX, CPS and SIPP maps by year, growth factor to the target year (wages per return for the CPS and SIPP '
               'dollars):\n' + f.pivot_table(index=['survey', 'variable'], columns='year', values='factor to the target year')
               .to_string(float_format=lambda x: f'{x:.4f}'))




def main():
    """The target year's dollar maps alone (30 October, targets for every year), from the table and band edges already
    built (common.TABLE, common.EDGES): the networks trained on that table stay valid, and each target year gets its
    maps by the same build_maps the full build runs."""
    t = pd.read_parquet(common.TABLE)
    edges = json.load(open(common.EDGES))
    build_maps(t, edges)
    common.say(f'maps for {TARGET_YEAR} written to {common.MAPS}')


if __name__ == '__main__':
    main()
