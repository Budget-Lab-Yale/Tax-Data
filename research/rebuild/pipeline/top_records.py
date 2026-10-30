"""The very top of the file, added after generation and before the reweighting (PLAN.md of
explore/2026-10-29/top_research, "Where it runs"; John, 29 October 2026). tilt.main() calls prepare_top() on the start,
in target-year dollars, before it saves the population masks, builds the target conditions or does the demographic
prescaling; the ordinary non-microdata step then runs on the whole file (it may add no rows and change no column, so
it must come after).

prepare_top, in order:
  1. a start from an earlier year's tilted file: its billionaires are removed (the year's own list replaces them;
     PLAN.md open question 4);
  2. Part A, the PUF's hidden top returns rebuilt from generated records (top_hidden.rebuild_hidden_returns, merged
     29 October), when settings tilt.top.hidden_returns is true and the start is generated; otherwise skipped, and the
     summary says so. Its records come in the generated records' weight units, with synthetic_top = 1 and
     hidden_group; they are scaled with the generated records and free to move in the tilt;
  3. the redraw of every generated balance sheet at or above the ceiling (top_billionaires.redraw_balance_sheets;
     the ceiling is catalog population scf_units_below_a_billion's net_worth_below, $1 billion);
  4. the year's Forbes list as records of weight 1 (top_billionaires.build_billionaires), held fixed in the tilt.
Returns the frame, the starting weights (relative importance weights for generated records, as before; the weight in
returns or units for records whose weight is given), the mask of records whose starting weight is given, the mask of
records held fixed, and the report (notes and tables, written by write_report).
"""
import os

import numpy as np
import pandas as pd

import common
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
    if not S.get('hidden_returns', False):
        notes.append('Part A (the PUF\'s hidden top returns): off (settings tilt.top.hidden_returns)')
        return None
    if not os.path.exists(os.path.join(common.HERE, 'top_hidden.py')):
        notes.append('Part A (the PUF\'s hidden top returns): settings ask for it, but top_hidden.py is not in this '
                     'pipeline; skipped')
        return None
    if mode != 'generate':
        notes.append('WARNING: Part A (the PUF\'s hidden top returns) is built for a generated start only; this start '
                     'is an earlier year\'s file, so it is skipped')
        return None
    import top_hidden
    # a stack drawn before the blindness rule (the retrain of 28 October) carries four blindness columns Part A's rows
    # do not: they are given the neutral values (not blind; no draw)
    old = [c for c in ['blind', 'blind_drawn1', 'blind_drawn2', 'blind_chance1', 'blind_chance2'] if c in df]
    h = top_hidden.rebuild_hidden_returns(df.drop(columns=old), year, details=True)
    neutral = {'blind': 1, 'blind_drawn1': 0, 'blind_drawn2': 0, 'blind_chance1': 0.0}
    for c in old:
        h[c] = np.where(h['married'] == 1, 0.0, -1.0) if c == 'blind_chance2' else neutral[c]
        h[c] = h[c].astype(df[c].dtype)
    assert (h['synthetic_top'] == 1).all() and h['weight'].notna().all() and (h['weight'] > 0).all()
    h = h.drop(columns=['weight_2015', 'donor_record_id']).rename(columns={'hidden_wealth_fallback': 'wealth_redraw_fallback'})
    h['wealth_redrawn'] = 1
    notes.append(f"Part A: {len(h)} rebuilt hidden returns ({', '.join(f'group {g}: {n}' for g, n in h['hidden_group'].value_counts().sort_index().items())}); "
                 f"their balance sheets drawn again below the ceiling by the same function (redraw_record), "
                 f"{int(h['wealth_redraw_fallback'].sum())} fallbacks")
    return h


def prepare_top(g19, raw, imp, year, mode):
    """See the top. g19: the start (target-year dollars); raw: the same records with their bands and stored picks
    (the generated stack, or the start file itself); imp: their starting weights; mode: 'generate' or 'from_year'."""
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
    # 4. the billionaires
    lst = TB.read_list(year)
    g19['billionaire'], g19['billionaire_rank'], g19['forbes_uri'] = 0, -1, ''
    fixed = np.zeros(len(g19), bool)
    if lst is None:
        notes.append(f'WARNING: no Forbes list for {year} (settings tilt.top.billionaire_lists): the file has no units '
                     f'at or above ${ceiling / 1e9:g} billion, and the SCF targets cover the units below it only')
    else:
        b, brep = TB.build_billionaires(lst, g19, imp, year, notes)
        rep.update(brep)
        b = b[g19.columns]
        assert not set(b['record_id']) & set(g19['record_id']), 'a billionaire id already in the file'
        g19 = pd.concat([g19, b], ignore_index=True)
        imp = np.r_[imp, np.ones(len(b))]
        given = np.r_[given, np.ones(len(b), bool)]
        fixed = np.r_[fixed, np.ones(len(b), bool)]
    assert g19['record_id'].is_unique, 'a tax unit appears twice'
    over = (g19['net_worth'].to_numpy(float) >= ceiling) & (g19['billionaire'].to_numpy() != 1)
    rep['over ceiling, not on the list'] = int(over.sum())
    if over.any():
        notes.append(f'{int(over.sum())} records other than the list at or above the ceiling (redraw fallbacks)')
    rep['notes'] = notes
    for n in notes:
        common.say(f'top: {n}')
    return g19, imp, given, fixed, rep


def write_report(rep, folder, year):
    """The preparation's tables: billionaire_units.csv (one row per tax unit), billionaire_groups.csv, redraw.csv, and
    top_notes.md (the notes and the item mix)."""
    if 'units' in rep:
        u = rep['units'].drop(columns=['uri', 'sp_uri'], errors='ignore')
        u.to_csv(os.path.join(folder, 'billionaire_units.csv'), index=False)
        rep['groups'].to_csv(os.path.join(folder, 'billionaire_groups.csv'), index=False)
    if len(rep.get('redraw', [])):
        rep['redraw'].to_csv(os.path.join(folder, 'redraw.csv'), index=False)
    lines = [f'# The top of the {year} file: notes from the preparation (top_records.py)', '']
    lines += [f'- {n}' for n in rep['notes']] + ['']
    if 'mix' in rep:
        lines += ['## The item mix used for the billionaires\' tax items', '', f"Group: {rep['mix group']}", '']
        for k, v in rep['mix'].items():
            lines.append(f'- {k}: {v}')
    open(os.path.join(folder, 'top_notes.md'), 'w').write('\n'.join(lines) + '\n')
