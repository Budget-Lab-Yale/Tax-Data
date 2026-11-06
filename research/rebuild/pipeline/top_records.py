"""The very top of the file, added after generation and before the reweighting (PLAN.md of
explore/2026-10-29/top_research, "Where it runs"; John, 29 October 2026). tilt.main() runs these steps on the start,
in target-year dollars, before it saves the population masks, builds the target conditions or does the demographic
prescaling; the ordinary non-microdata step then runs on the whole file (it may add no rows and change no column, so
it must come after).

The steps, in order:
  1. a start from an earlier year's tilted file: its billionaires are removed (the year's own list replaces them;
     PLAN.md open question 4);
  2. Part A, the PUF's hidden top returns rebuilt from generated records (top_hidden.rebuild_hidden_returns, merged
     29 October), when settings tilt.top.hidden_returns is true and the start is generated; otherwise skipped, and the
     summary says so. Its records come in the generated records' weight units, with synthetic_top = 1 and
     hidden_group; they are scaled with the generated records and free to move in the tilt;
  3. the redraw of every generated balance sheet at or above the ceiling (top_billionaires.redraw_balance_sheets;
     the ceiling is catalog population scf_units_below_a_billion's net_worth_below, $1 billion);
  4. the billionaires (top_billionaires.build_billionaires; add_list). In a year with a Forbes list (2015-2026 since 2
     November 2026), the year's own list as records of weight 1, held fixed in the tilt. In a year after the last list
     (2027-2035; John, 2 November 2026: "their weights grow just like any other person's"), the last list grown to the
     year (top_billionaires.grow_list), its records starting at weight 1 times the population growth of their
     demographic cell from the list's year to the year (projection_demographics.population_cells, the start scaling
     every other record of a projection year gets from 2023). Since 2 November 2026 they are held fixed at that
     starting weight (settings tilt.top.after_last_list.fixed true; kept as the rule by John, 4 November 2026): left
     free (false), the solver cut the richest to weights near zero.
  5. in a year with a Forbes list, every other record at or above the ceiling held just below it (hold_below_ceiling;
     John, 4 November 2026): the list is every US billionaire, so no other record may be one. In 2024-2026 these are
     2023 records that growth carried past $1 billion; in a generated year, redraw fallbacks (none since 2 November).
Returns the frame, the starting weights (relative importance weights for generated records, as before; the weight in
returns or units for records whose weight is given), the mask of records whose starting weight is given, the mask of
records held fixed, and the report (notes and tables, written by write_report).

Steps 1-3 (network_part) draw from the network and want a GPU; step 4 (add_list) does not. tilt.main caches the
result of steps 1-3 (REBUILD_TOP_CACHE) and runs step 4 on every run; a cache written before 2 November, which held
step 4's records too, has them removed first (strip_list).
"""
import os

import numpy as np
import pandas as pd

import common
import dollars as D
import target_list as TLS
import top_billionaires as TB

S = common.SETTINGS['tilt'].get('top', {})
POPULATION = 'scf_units_below_a_billion'


def _hidden_returns(df, year, mode, notes):
    """Part A's records (see the top), or None. They come in df's units (top_hidden.rebuild_hidden_returns: each
    rebuilt return's 2015 population weight in the generated filers' own units), so the start's scaling of filers to
    SOI's returns grows them with the count of returns; they are free in the tilt. Kept with them: hidden_group (0
    negative AGI, 1 $0-10 million, 2 $10-100 million, 3 over $100 million, in 2015 dollars) and their redraw's fallback
    flag."""
    if common.REAL_PUF:   # (4 November 2026) the four aggregate records are in the file as real records
        notes.append('Part A (the PUF\'s hidden top returns): off, because settings generation.filers is real_puf: the '
                     'PUF\'s four aggregate records are in the file as real records'
                     + (' (settings tilt.top.hidden_returns true is overridden)' if S.get('hidden_returns') else ''))
        return None
    if not S.get('hidden_returns', False):
        notes.append('Part A (the PUF\'s hidden top returns): off (settings tilt.top.hidden_returns)')
        return None
    if mode != 'generate':
        notes.append('WARNING: Part A (the PUF\'s hidden top returns) is built for a generated start only; this start '
                     'is an earlier year\'s file, so it is skipped')
        return None
    import top_hidden
    # a stack drawn before the blindness rule (the retrain of 28 October) carries blindness columns Part A's rows do not
    old = [c for c in ['blind', 'blind_drawn1', 'blind_drawn2', 'blind_chance1', 'blind_chance2'] if c in df]
    assert not old, f'the stack has the blindness columns {old} (drawn before 28 October)'
    h = top_hidden.rebuild_hidden_returns(df, year, details=True)
    assert (h['synthetic_top'] == 1).all() and h['weight'].notna().all() and (h['weight'] > 0).all()
    h = h.drop(columns=['weight_2015', 'donor_record_id']).rename(columns={'hidden_wealth_fallback': 'wealth_redraw_fallback'})
    h['wealth_redrawn'] = 1
    notes.append(f"Part A: {len(h)} rebuilt hidden returns ({', '.join(f'group {g}: {n}' for g, n in h['hidden_group'].value_counts().sort_index().items())}); "
                 f"their balance sheets drawn again below the ceiling by the same function (redraw_record), "
                 f"{int(h['wealth_redraw_fallback'].sum())} fallbacks")
    return h


# The columns a real-PUF start has and Part A's rows do not, and what a record that is not a PUF record holds in them
# (generate.py gives the non-filers the same); the four the caller sets itself are dropped before the rows are made
REAL_ONLY = {'puf_recid': -1, 'puf_copy': -1, 'puf_aggregate': 0, 'age_range_drawn': 0}
MADE_BY_NETWORK_PART = ['synthetic_top', 'hidden_group', 'wealth_redrawn', 'wealth_redraw_fallback']


def replace_aggregates(g19, imp, given, fixed, rep, year, mode):
    """(Exploration of 5 November 2026; settings generation.real_puf_aggregates 'rebuilt') On the result of steps 1-3 of
    a real-PUF past year: the copies of the PUF's four aggregate records leave the file, and Part A's rebuilt returns
    take their place, with the same share of the filers' weight in the same units (top_hidden.rebuild_hidden_returns
    reads the rows from <gen>/hidden/hidden_<year>.parquet). The rows' group totals are the four records' own means times
    their weights, so the copies must go or those dollars are in the file twice. A start from an earlier year's file
    already holds whatever its first year chose, so nothing happens there, as for Part A in a synthetic run. Otherwise
    the arguments come back unchanged."""
    if common.REAL_AGGREGATES != 'rebuilt' or mode != 'generate':
        return g19, imp, given, fixed, rep
    import top_hidden
    agg = g19['puf_aggregate'].to_numpy() == 1
    assert agg.sum() == 4 * len(common.SETTINGS['generation']['models']), f'{int(agg.sum())} aggregate record copies'
    assert not given[agg].any() and not fixed[agg].any()
    notes = [n for n in rep['notes'] if not n.startswith('Part A')]
    removed = np.asarray(imp, float)[agg].sum()
    keep = ~agg
    g19, imp, given, fixed = g19.loc[keep].reset_index(drop=True), np.asarray(imp, float)[keep], given[keep], fixed[keep]
    h = top_hidden.rebuild_hidden_returns(g19.drop(columns=list(REAL_ONLY) + MADE_BY_NETWORK_PART), year, details=True)
    assert (h['synthetic_top'] == 1).all() and h['weight'].notna().all() and (h['weight'] > 0).all()
    h = h.drop(columns=['weight_2015', 'donor_record_id']).rename(columns={'hidden_wealth_fallback': 'wealth_redraw_fallback'})
    h['wealth_redrawn'] = 1
    for c, v in REAL_ONLY.items():
        h[c] = v
    ceiling = TLS.net_worth_ceiling(POPULATION)
    assert not (h['net_worth'] >= ceiling).any(), 'Part A: a rebuilt return at or above the ceiling'
    assert not set(h['record_id']) & set(g19['record_id'])
    added = h['weight'].to_numpy(float)
    assert np.isclose(added.sum(), removed, rtol=1e-9), f'the rebuilt returns weigh {added.sum()}, the copies {removed}'
    notes.append(f"Part A: the {int(agg.sum())} copies of the four aggregate records removed and {len(h)} rebuilt returns added "
                 f"({', '.join(f'group {g}: {n}' for g, n in h['hidden_group'].value_counts().sort_index().items())}), "
                 f"{added.sum():,.6f} on the start's own scale against {removed:,.6f} removed; "
                 f"{int(h['wealth_redraw_fallback'].sum())} balance-sheet fallbacks")
    g19 = pd.concat([g19, h[g19.columns]], ignore_index=True)
    imp = np.r_[imp, added]
    given, fixed = np.r_[given, np.zeros(len(h), bool)], np.r_[fixed, np.zeros(len(h), bool)]
    return g19, imp, given, fixed, {**rep, 'notes': notes, 'hidden': h}


def network_part(g19, raw, imp, year, mode):
    """Steps 1-3 (see the top). g19: the start (target-year dollars); raw: the same records with their bands and stored
    picks (the generated stack, or the start file itself); imp: their starting weights; mode: 'generate' or
    'from_year'. Returns the frame, starting weights, given and fixed masks (all False) and report."""
    notes, rep = [], {}
    ceiling = TLS.net_worth_ceiling(POPULATION)
    assert ceiling is not None, f'settings tilt.top.enabled, but population {POPULATION} has no net_worth_below'
    imp = np.asarray(imp, float).copy()
    if 'billionaire' in g19:              # 1. an earlier year's billionaires out
        old = g19['billionaire'].to_numpy() == 1
        notes.append(f'start: {int(old.sum())} billionaire records of the earlier year removed')
        g19, raw, imp = g19.loc[~old].reset_index(drop=True), raw.loc[~old].reset_index(drop=True), imp[~old]
    given = np.zeros(len(g19), bool)
    h = _hidden_returns(g19, year, mode, notes)  # 2. Part A
    # 3. the redraw, on the generated records (Part A's records were drawn again in its own build, by redraw_record)
    if mode == 'generate':
        g19, tab = TB.redraw_balance_sheets(g19, raw, year, ceiling, notes, check_same=True)
        rep['redraw'] = tab
    else:
        # (30 October) a start from an earlier year's file: its records were redrawn below the ceiling in the year they
        # were generated; the redraw needs the network and slots the finished file does not keep, so it is not run
        # again. A record that growth carries to the ceiling stays as it is and is counted below
        notes.append('start from an earlier year\'s file: the redraw at the ceiling is not run again (done in the '
                     'year the records were generated)')
    for c in ['wealth_redrawn', 'wealth_redraw_fallback']:
        if c not in g19:
            g19[c] = 0
    for c, none in [('synthetic_top', 0), ('hidden_group', -1)]:   # an earlier year's file keeps its own labels
        if c not in g19:
            g19[c] = none
    if h is not None:
        assert not (h['net_worth'] >= ceiling).any(), 'Part A: a rebuilt return at or above the ceiling'
        assert not set(h['record_id']) & set(g19['record_id'])
        g19 = pd.concat([g19, h[g19.columns]], ignore_index=True)
        imp = np.r_[imp, h['weight'].to_numpy(float)]   # in the generated records' units: scaled with them
        given = np.r_[given, np.zeros(len(h), bool)]
        rep['hidden'] = h
    rep['notes'] = notes
    return g19, imp, given, np.zeros(len(g19), bool), rep


LIST_KEYS = ['units', 'people', 'groups', 'mix', 'mix group', 'bsyz', 'list', 'over ceiling, not on the list',
             'held below the ceiling']
LIST_NOTES = ('billionaires', 'item mix:', 'WARNING: no Forbes list', 'records other than the list',
              'held below the ceiling')

# The balance sheet's dollar columns besides net worth's parts: the unrealized gains (w_kg_*, dollars) and the DC
# account's stocks (w_dc_stocks = its share x value.dc); the cost bases (basis.*) are taken wherever the file has them.
# Ratios (w_primary_ltv, w_dc_stock_share) are not dollars and are left as they are.
BALANCE_DOLLARS = ['w_kg_primary_home', 'w_kg_other_re', 'w_kg_pass_throughs', 'w_kg_other', 'w_dc_stocks']
HOLD_SHARE = 0.999   # a held record's net worth: this share of the ceiling


def hold_below_ceiling(g19, imp, ceiling, notes):
    """Step 5 (John, 4 November 2026). In a year with a Forbes list the list is every US billionaire, so a record not on
    it at or above the ceiling has its whole balance sheet (net worth's assets and debts, the unrealized gains, the cost
    bases, the DC account's stocks) multiplied by one factor that puts its net worth at HOLD_SHARE x the ceiling. Its
    income, its weight and every other column are unchanged. Returns the frame and a table, one row per record held."""
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


def strip_list(g19, imp, given, fixed, rep):
    """The result of steps 1-3 from a cache: a cache written before 2 November 2026 also held step 4's records
    (billionaire = 1) and their report; they are removed, so that add_list builds them again."""
    rep = {k: v for k, v in rep.items() if k not in LIST_KEYS}
    rep['notes'] = [n for n in rep['notes'] if not n.startswith(LIST_NOTES) and 'records other than the list' not in n]
    if 'billionaire' in g19:
        old = g19['billionaire'].to_numpy() == 1
        g19, imp = g19.loc[~old].reset_index(drop=True), np.asarray(imp)[~old]
        given, fixed = np.asarray(given)[~old], np.asarray(fixed)[~old]
        assert not given.any() and not fixed.any(), 'a cached record other than the list with a given or fixed weight'
    return g19, imp, given, fixed, rep


def net_worth_growth(g19, imp, year, list_year):
    """(John, 4 November 2026: "I want them to grow at the same rate") The one factor by which every fortune on the
    last list grows to the year when settings tilt.top.after_last_list.wealth_growth is 'net worth': the factor that
    makes the list's net worth on its starting weights (1 x its cells' population growth from the list's year) grow
    from the list's year as Z.1 household net worth (resources/growth/z1_household_net_worth.csv, the series of the
    file's own net worth target), so the list keeps its share of the file's net worth. The weights depend on the
    records' cells only (ages and marital status), not on their wealth, so a first build with any growth gives them.
    Returns (factor, source) for top_billionaires.grow_list."""
    import growth as GR
    import projection_demographics as PD
    first = TB.grow_list(TB.read_list(list_year), list_year, year)
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
    """Step 4 (see the top) on the result of steps 1-3."""
    import projection_demographics as PD
    rep = dict(rep)
    notes = list(rep['notes'])
    ceiling = TLS.net_worth_ceiling(POPULATION)
    kind, lst, list_year = TB.list_for_year(year)
    g19 = g19.copy()
    g19['billionaire'], g19['billionaire_rank'], g19['forbes_uri'] = 0, -1, ''
    rule = (S.get('after_last_list') or {}).get('wealth_growth', 'parts')
    assert rule in ('parts', 'net worth'), f'settings tilt.top.after_last_list.wealth_growth: {rule!r}'
    if kind == 'grown' and rule == 'net worth':
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
            # weight 1 on the list's date, grown by the population growth of the record's cell from the list's year to
            # the year, as every projection-year record's weight is grown from 2023 (tilt.main, population_cells)
            w_b, _ = PD.population_cells(b.assign(weight=1.0), year, from_year=list_year)
            w_b = np.asarray(w_b, float)
            if rule == 'net worth':   # the list's net worth on these weights grows as Z.1's from the list's year
                import growth as GR
                z = GR._csv('z1_household_net_worth.csv')
                want = lst['net_worth_list'].sum() * float(z.loc[year]) / float(z.loc[list_year])
                assert np.isclose((w_b * b['net_worth'].to_numpy(float)).sum(), want, rtol=1e-6), 'list growth off'
            # (2 November 2026; the rule since John, 4 November 2026) settings
            # tilt.top.after_last_list.fixed: true holds them at that starting weight in the solver, as the list years
            # hold theirs at 1; false leaves them free (the 2 November run: the solver then cut the richest to ~0)
            hold = S['after_last_list']['fixed']
            assert isinstance(hold, bool), f'settings tilt.top.after_last_list.fixed must be true or false, not {hold!r}'
            fix_b = np.full(len(b), hold)
            notes.append(f'billionaires: starting weights 1 x the population growth of their cell from {list_year} to '
                         f'{year}: {w_b.min():.4f} to {w_b.max():.4f} (sum {w_b.sum():,.1f} for {len(b)} records); '
                         + ('held fixed at them in the reweighting (settings tilt.top.after_last_list.fixed)' if hold else
                            'free in the reweighting, under the soft cap'))
        g19 = pd.concat([g19, b], ignore_index=True)
        imp = np.r_[imp, w_b]
        given = np.r_[given, np.ones(len(b), bool)]
        fixed = np.r_[fixed, fix_b]
    assert g19['record_id'].is_unique, 'a tax unit appears twice'
    if lst is not None and kind == 'list':   # 5. (John, 4 November 2026) the list is every US billionaire
        g19, rep['held below the ceiling'] = hold_below_ceiling(g19, imp, ceiling, notes)
    over = (g19['net_worth'].to_numpy(float) >= ceiling) & (g19['billionaire'].to_numpy() != 1)
    rep['over ceiling, not on the list'] = int(over.sum())
    if over.any():
        notes.append(f'{int(over.sum())} records other than the list at or above the ceiling (redraw fallbacks)')
    rep['notes'] = notes
    for n in notes:
        common.say(f'top: {n}')
    return g19, imp, given, fixed, rep


def write_report(rep, folder, year):
    """The preparation's tables: billionaire_units.csv (one row per tax unit), billionaire_groups.csv, redraw.csv,
    held_below_ceiling.csv, and
    top_notes.md (the notes and the item mix)."""
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
