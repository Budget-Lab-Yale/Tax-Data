"""The top of the file, added to the start before the reweighting.

Above $1 billion of net worth the file is the Forbes list; below it, the SCF. network_part needs the networks and runs
in the top stage (top.py). It removes an earlier year's billionaires from a start built from that year's file, adds
Part A (the PUF's aggregate records rebuilt, top_hidden.py), and draws again every generated balance sheet at or above
$1 billion. add_list runs in the reweighting. It adds the year's Forbes list, or the last list grown to the year, and
holds every other record below $1 billion. FEATURES.md, "The top of the file", describes the method.
"""
import os

import numpy as np
import pandas as pd

from shared import common
from shared import dollars as D
from reweight import target_list as TLS
from generate import top_billionaires as TB

POPULATION = 'scf_units_below_a_billion'      # the target population whose net worth ceiling is the line


# ---- Steps 1 to 3: the part that needs the networks ----


def _hidden_returns(df, year, mode, notes):
    """Part A's records, or None when Part A does not apply. Their weights are in the same units as the generated
    filers' weights; they are scaled with the filers and are free in the reweighting. hidden_group is 0 for negative
    AGI, 1 for $0 to $10 million, 2 for $10 to $100 million and 3 for over $100 million, in 2015 dollars."""
    if common.REAL_PUF:
        notes.append('Part A (the PUF\'s hidden top returns): off, because settings generation.filers is real_puf: the '
                     'PUF\'s four aggregate records are in the file as real records')
        return None
    if mode != 'generate':
        notes.append('WARNING: Part A (the PUF\'s hidden top returns) is built for a generated start only; this start '
                     'is an earlier year\'s file, so it is skipped')
        return None
    from generate import top_hidden
    h = top_hidden.rebuild_hidden_returns(df)
    assert (h['synthetic_top'] == 1).all() and h['weight'].notna().all() and (h['weight'] > 0).all()
    h = h.drop(columns=['weight_2015', 'donor_record_id']).rename(columns={'hidden_wealth_fallback': 'wealth_redraw_fallback'})
    h['wealth_redrawn'] = 1
    notes.append(f"Part A: {len(h)} rebuilt hidden returns ({', '.join(f'group {g}: {n}' for g, n in h['hidden_group'].value_counts().sort_index().items())}); "
                 f"their balance sheets drawn again below the ceiling by the same function (redraw_record), "
                 f"{int(h['wealth_redraw_fallback'].sum())} fallbacks")
    return h


def network_part(g19, raw, imp, year, mode):
    """Steps 1 to 3: remove an earlier year's billionaires, add Part A, and draw the generated balance sheets of $1
    billion or more again. g19 is the start in target-year dollars; raw holds the same records with their bands and
    stored picks; imp holds their starting weights; mode is 'generate' or 'from_year'. Returns the records, their
    starting weights, the masks of records whose weights are given and fixed (all False here), and the report."""
    notes, rep = [], {}
    ceiling = TLS.net_worth_ceiling(POPULATION)
    assert ceiling is not None, f'population {POPULATION} has no net_worth_below'
    imp = np.asarray(imp, float).copy()
    # 1. Remove an earlier year's billionaires.
    if 'billionaire' in g19:
        old = g19['billionaire'].to_numpy() == 1
        notes.append(f'start: {int(old.sum())} billionaire records of the earlier year removed')
        g19, raw, imp = g19.loc[~old].reset_index(drop=True), raw.loc[~old].reset_index(drop=True), imp[~old]
    given = np.zeros(len(g19), bool)
    # 2. Part A.
    h = _hidden_returns(g19, year, mode, notes)
    # 3. Draw the generated balance sheets of $1 billion or more again. Part A's records were drawn again when they
    #    were built.
    if mode == 'generate':
        g19, tab = TB.redraw_balance_sheets(g19, raw, year, ceiling, notes)
        rep['redraw'] = tab
    else:
        # An earlier year's file was drawn again in the year it was generated, and it lacks the slots the redraw
        # needs. A record grown past the ceiling stays as it is and is counted in add_list.
        notes.append('start from an earlier year\'s file: the redraw at the ceiling is not run again (done in the '
                     'year the records were generated)')
    for c in ['wealth_redrawn', 'wealth_redraw_fallback']:
        if c not in g19:
            g19[c] = 0
    # The flags of Part A's records; an earlier year's file keeps its own.
    for c, none in [('synthetic_top', 0), ('hidden_group', -1)]:
        if c not in g19:
            g19[c] = none
    if h is not None:
        assert not (h['net_worth'] >= ceiling).any(), 'Part A: a rebuilt return at or above the ceiling'
        assert not set(h['record_id']) & set(g19['record_id'])
        g19 = pd.concat([g19, h[g19.columns]], ignore_index=True)
        imp = np.r_[imp, h['weight'].to_numpy(float)]
        given = np.r_[given, np.zeros(len(h), bool)]
        rep['hidden'] = h
    rep['notes'] = notes
    return g19, imp, given, np.zeros(len(g19), bool), rep


# ---- Steps 4 and 5: the Forbes list, and every other record below $1 billion ----

# The balance-sheet dollar columns other than net worth's parts. They are scaled with the parts, as are the cost bases
# (basis.*). Ratios are left as they are.
BALANCE_DOLLARS = ['w_kg_primary_home', 'w_kg_other_re', 'w_kg_pass_throughs', 'w_kg_other', 'w_dc_stocks']
HOLD_SHARE = 0.999   # a held record's net worth is this share of the ceiling


def hold_below_ceiling(g19, imp, ceiling, notes):
    """Step 5. The Forbes list is taken to be every US billionaire. A record that is not on the list but is at or
    above the ceiling has its whole balance sheet scaled by one factor, which puts its net worth at HOLD_SHARE x the
    ceiling. Its income and weight are unchanged. Returns the records and a table with one row per record held."""
    nw = g19['net_worth'].to_numpy(float)
    over = (nw >= ceiling) & (g19['billionaire'].to_numpy() != 1)
    if not over.any():
        notes.append('held below the ceiling: no record other than the list at or above it')
        return g19, pd.DataFrame()
    parts = list(D.NET_WORTH_PARTS)
    assert np.allclose(D.signed_sum(g19.loc[over], D.NET_WORTH_PARTS), nw[over], rtol=1e-9), \
        'a record over the ceiling whose net worth is not the sum of its parts'
    f = HOLD_SHARE * ceiling / nw[over]
    cols = [c for c in parts + BALANCE_DOLLARS if c in g19] + [c for c in g19.columns if c.startswith('basis.')]
    g19 = g19.copy()
    idx = np.flatnonzero(over)
    for c in cols:
        j = g19.columns.get_loc(c)
        g19.iloc[idx, j] = g19.iloc[idx, j].to_numpy(float) * f
    g19.iloc[idx, g19.columns.get_loc('net_worth')] = D.signed_sum(g19.iloc[idx], D.NET_WORTH_PARTS)
    after = g19['net_worth'].to_numpy(float)
    assert (after[idx] < ceiling).all() and np.allclose(after[idx], HOLD_SHARE * ceiling, rtol=1e-9)
    w = np.asarray(imp, float)[idx]
    tab = pd.DataFrame({'record_id': g19['record_id'].to_numpy()[idx], 'starting weight': w,
                        'net worth before': nw[idx], 'net worth after': after[idx], 'factor': f})
    notes.append(f'held below the ceiling: {len(idx)} records other than the list at or above ${ceiling / 1e9:g} billion '
                 f'({w.sum():,.1f} on their starting weights; net worth ${(w * nw[idx]).sum() / 1e12:.3f} trillion '
                 f'before, ${(w * after[idx]).sum() / 1e12:.3f} trillion after); each balance sheet scaled to '
                 f'{HOLD_SHARE:g} x the ceiling (factors {f.min():.3f} to {f.max():.3f}); income and weights unchanged')
    return g19, tab


def net_worth_growth(g19, imp, year, list_year):
    """The one factor by which every fortune on the last list grows to the year. It is chosen so that the list's net
    worth on its starting weights grows as Z.1 household net worth from the list's year. The starting weights depend
    only on the records' age and marital cells, so a first build with no growth gives them. Returns the factor and a
    description."""
    from shared import growth as GR
    from scale import projection_demographics as PD
    first = TB.grow_list(TB.read_list(list_year), list_year, year, factor=(1.0, 'no growth: the weights depend on the cells alone'))
    b0, _ = TB.build_billionaires(first, g19, imp, year, [])
    w0, _ = PD.population_cells(b0.assign(weight=1.0), year, from_year=list_year)
    w0 = np.asarray(w0, float)
    units, _ = TB.tax_units(TB.read_list(list_year))
    nw_s = b0['forbes_uri'].map(units.set_index('unit_uri')['net_worth']).to_numpy(float)
    assert np.isfinite(nw_s).all() and np.isclose(nw_s.sum(), units['net_worth'].sum(), rtol=1e-12), \
        'the list\'s units and the built records differ'
    z = GR._csv('z1_household_net_worth.csv')
    zg = float(z.loc[year]) / float(z.loc[list_year])
    wg = (w0 * nw_s).sum() / nw_s.sum()
    f = zg / wg
    return f, (f'one factor for every part: Z.1 household net worth {list_year} to {year} x {zg:.5f} over the list\'s '
               f'weight growth (net-worth-weighted) x {wg:.5f}')


def add_list(g19, imp, given, fixed, rep, year):
    """Steps 4 and 5, on the result of steps 1 to 3. In a year with a Forbes list, its records enter at weight 1.
    After the last list, the last list grown to the year enters at weight 1 times its cells' population growth since
    the list's year. These weights are held fixed in the reweighting. (Left free, the solver cuts the richest records
    to weights near zero.)"""
    from scale import projection_demographics as PD
    rep = dict(rep)
    notes = list(rep['notes'])
    ceiling = TLS.net_worth_ceiling(POPULATION)
    # 4. Add the year's list, or the last list grown to the year.
    kind, lst, list_year = TB.list_for_year(year)
    g19 = g19.copy()
    g19['billionaire'], g19['billionaire_rank'], g19['forbes_uri'] = 0, -1, ''
    if kind == 'grown':
        lst = TB.grow_list(TB.read_list(list_year), list_year, year,
                           factor=net_worth_growth(g19, imp, year, list_year))
    if lst is None:
        notes.append(f'WARNING: no Forbes list for {year} (settings tilt.top.billionaire_lists): the file has no units '
                     f'at or above ${ceiling / 1e9:g} billion, and the SCF targets cover the units below it only')
    else:
        if kind == 'grown':
            notes.append(f'billionaires: the {list_year} list grown to {year} (top_billionaires.grow_list): '
                         + '; '.join(f'{k} x {f:.4f} ({src[:90]})' for k, (f, src) in lst.attrs['growth'].items())
                         + f"; net worth ${lst['net_worth_list'].sum() / 1e12:.3f} trillion on the list, "
                         f"${lst['net_worth'].sum() / 1e12:.3f} trillion grown; ages raised by {year - list_year} years")
        b, brep = TB.build_billionaires(lst, g19, imp, year, notes)
        rep.update(brep)
        rep['list'] = {'kind': kind, 'list year': list_year}
        b = b[g19.columns]
        assert not set(b['record_id']) & set(g19['record_id']), 'a billionaire id already in the file'
        if kind == 'list':
            w_b = np.ones(len(b))
            fix_b = np.ones(len(b), bool)
        else:
            # Weight 1 on the list's date, grown by the population growth of the record's cell since the list's year.
            w_b, _ = PD.population_cells(b.assign(weight=1.0), year, from_year=list_year)
            w_b = np.asarray(w_b, float)
            # Check: the list's net worth on these weights grows as Z.1's from the list's year.
            from shared import growth as GR
            z = GR._csv('z1_household_net_worth.csv')
            want = lst['net_worth_list'].sum() * float(z.loc[year]) / float(z.loc[list_year])
            assert np.isclose((w_b * b['net_worth'].to_numpy(float)).sum(), want, rtol=1e-6), 'list growth off'
            # The records are held at these starting weights, as a list year holds its records at 1.
            fix_b = np.full(len(b), True)
            notes.append(f'billionaires: starting weights 1 x the population growth of their cell from {list_year} to '
                         f'{year}: {w_b.min():.4f} to {w_b.max():.4f} (sum {w_b.sum():,.1f} for {len(b)} records); '
                         + 'held fixed at them in the reweighting')
        g19 = pd.concat([g19, b], ignore_index=True)
        imp = np.r_[imp, w_b]
        given = np.r_[given, np.ones(len(b), bool)]
        fixed = np.r_[fixed, fix_b]
    assert g19['record_id'].is_unique, 'a tax unit appears twice'
    # 5. In a list year, hold every record not on the list below the ceiling.
    if lst is not None and kind == 'list':
        g19, rep['held below the ceiling'] = hold_below_ceiling(g19, imp, ceiling, notes)
    over = (g19['net_worth'].to_numpy(float) >= ceiling) & (g19['billionaire'].to_numpy() != 1)
    rep['over ceiling, not on the list'] = int(over.sum())
    if over.any():
        notes.append(f'{int(over.sum())} records other than the list at or above the ceiling (redraw fallbacks)')
    rep['notes'] = notes
    for n in notes:
        common.say(f'top: {n}')
    return g19, imp, given, fixed, rep


# ---- The report ----

def write_report(rep, folder, year):
    """Writes the top step's tables and top_notes.md to folder."""
    if 'units' in rep:
        u = rep['units'].drop(columns=['uri', 'sp_uri'], errors='ignore')
        u.to_csv(os.path.join(folder, 'billionaire_units.csv'), index=False)
        rep['groups'].to_csv(os.path.join(folder, 'billionaire_groups.csv'), index=False)
    if len(rep.get('redraw', [])):
        rep['redraw'].to_csv(os.path.join(folder, 'redraw.csv'), index=False)
    if len(rep.get('held below the ceiling', [])):
        rep['held below the ceiling'].to_csv(os.path.join(folder, 'held_below_ceiling.csv'), index=False)
    lines = [f'# The top of the {year} file: notes from the preparation (top_records.py)', '']
    lines += [f'- {n}' for n in rep['notes']] + ['']
    if 'mix' in rep:
        lines += ['## The item mix used for the billionaires\' tax items', '', f"Group: {rep['mix group']}", '']
        for k, v in rep['mix'].items():
            lines.append(f'- {k}: {v}')
    open(os.path.join(folder, 'top_notes.md'), 'w').write('\n'.join(lines) + '\n')
