"""The year's start, in target-year dollars: the generated stack with its dollars drawn from the year's maps (a past
year), or an earlier year's file with its weights calibrated by projection_steps.step1 and its dollars grown item by
item (a projection year). The top step (generate/top.py) and the reweighting (reweight/tilt.py) both start here.
"""
import numpy as np
import pandas as pd

from shared import common
from shared import dollars as D
from scale import non_micro_imputations as NMI
from reweight import target_list as TLS
from shared.common import TARGET_YEAR as Y


def year_start(spec):
    """The start for target_list.start_spec's spec: (frame in target-year dollars, the frame with its bands and stored
    picks, the starting weights, the growth table or None)."""
    growth = None
    if spec['mode'] == 'generate':
        raw = pd.read_parquet(f'{common.GEN}/stack.parquet')
        g19 = D.draw_dollars(raw, D.load_maps(Y), Y)   # every dollar amount in target-year dollars
        imp = g19['weight'].to_numpy(float)
    else:
        g19, imp, growth = start_from_year(spec)   # every dollar amount grown to the target year
        raw = g19
    return g19, raw, imp, growth


def recombine(df, year):
    """The columns dollars.draw_dollars computes from the drawn amounts, computed again from the (grown) drawn
    amounts in the same order: the capital gains totals, business income and its two schedules from the eighteen
    parts, other AGI (the drawn remainder plus capital gain distributions plus the AGI items folded in plus taxable
    Social Security by the section 86 worksheet, dollars.retirement_items, since 3 November), gross pensions, total
    consumption, the amounts made from a share (the spouse's wages, the home mortgage), the remainders (the primary
    filer's wages), net worth, and (with the elective deferrals) the traditional deferrals and Medicare wages of tax year
    year. Returns the frame and the columns recomputed."""
    out, done = df.copy(), []

    def put(col, values):
        out[col] = values
        done.append(col)
    # the Schedule D gains and losses before the loss limit; AGI's capital gain applies it (as dollars.draw_dollars)
    put('capital_gains', common.allowed_capital_gain(out['kg_st'] + out['kg_lt'],
                                                     out['filing_status'] if 'filing_status' in out else None))
    put('capital_gains_line13', out['capital_gains'] + out['kg_dist'])
    totals = D.META['computed_totals']
    for total in ['business_income'] + [k for k in totals if k != 'business_income']:   # then the schedules, dividends, adjustments
        put(total, D.signed_sum(out, totals[total]))
    put('other_agi', out['other_agi_remainder'] + out['kg_dist']
        + D.signed_sum(out, D.FOLD))
    # (3 November) gross pensions from the two parts, and taxable Social Security by the section 86 worksheet on the
    # (grown) amounts, into other_agi: the thresholds are not indexed, so growth moves it as the law does
    D.retirement_items(out)
    done += [c for c in ['ss_taxable', common.GROSS_PENSIONS] if c in out]
    cats = list(D.META['cex_amounts'])
    if all(c in out for c in cats):
        put('total_consumption', out[cats].to_numpy(float).sum(1))
    for slot, (item, base) in D.RATIOS.items():
        if f'{slot}_band' in out:
            put(item, out[slot] * out[base])
    for item, (total, other) in D.REMAINDERS.items():
        if other in out:
            put(item, out[total] - out[other])
    have = [slot for slot in D.PERSON_CAPS if f'{slot}_band' in out]   # tips and overtime, each where drawn
    if have:                                                           # tips1, tips2, tips, the flags, overtime
        D.person_items(out, out)
        made = {D.PERSON_CAPS[slot][0] for slot in have}
        done += sorted(made) + [k for k, parts in D.SUMS.items() if all(p in made for p in parts)] + \
            [k for k, (_, amount) in D.FLAGS_WITH.items() if amount in made]
    if all(c in out for c in D.NET_WORTH_PARTS):
        put('net_worth', D.signed_sum(out, D.NET_WORTH_PARTS))
    if 'elective_deferrals' in out:                       # the traditional deferrals and Medicare wages (28 October)
        D.deferral_items(out, year)
        done += ['elective1', 'elective2', 'trad_contr_er1', 'trad_contr_er2', 'trad_deferrals', 'medicare_wages']
    return out, done


def grow_dollars(df, s, t, mode='per_return_growth'):
    # (3 November, agent A) mode 'item_factor': each column handed to projection_steps.step2_factor, which applies the
    # items table's series and makes each 'total' item's start total grow exactly at its series on the step 1 weights
    # (projection_steps.STATE, set by step1 before this call); a 'current' row's own rule (growth.yaml, the SCF's growth
    # per unit) is computed here and passed on (4 November: only for 'current' rows, all of kind per_unit)
    """Each record's amounts grown item by item from year s to year t (TARGETS.md S7, per-return growth), by the
    series the dollar maps are grown by (maps.build_maps), each item's line in the target year's growth.yaml
    (growth.py), as a ratio of year t's factor to year s's:
      - PUF amounts: SOI Table 1.4's dollars per return with the item where Table 1.4 has it, else the item's declared
        series (build_table.growth: positive and negative values separately; since 29-30 October capital
        losses grown with the gains, the loss limit applied again by recombine where AGI is made);
      - the balance sheet: growth per tax unit, item by item, the mean per unit of the wave of income year t - 1
        over that of s - 1 (maps.scf_growth, the maps' rule); when either wave is missing, or an item's mean
        per unit is not positive in both, the run stops (until 30 October those were left ungrown);
      - consumption: NIPA PCE per return, by category;
      - the CPS-only dollars (childcare, overtime) and the SIPP's tips: SOI's wages per return; the CPS shares, weeks
        and hours not grown.
    Then every computed column is computed again from the grown parts (recombine; checked first to reproduce the
    file's own columns). Ages and every category are left as they are. Returns the grown frame and a table of the
    factors, one row per column."""
    from prep import build_table as BT
    from scale import maps as MP
    from shared import growth as GR
    same, done = recombine(df, s)
    for c in done:
        assert np.allclose(same[c].to_numpy(float), df[c].to_numpy(float), rtol=1e-9, atol=1e-6, equal_nan=True), \
            f'{c}: the year-{s} file\'s column is not what its parts give (dollars.draw_dollars changed?)'
    out, rows = df.copy(), []
    # in item mode an items-table row with series 'current' is a per-unit rule (growth.yaml or the SCF's growth per unit)
    item_mode = mode == 'item_factor'
    if item_mode:
        from scale import projection_steps as PS
        tab = PS.items_table()
        bad = tab[(tab['series'] == 'current') & (tab['kind'] != 'per_unit')].index.tolist()
        assert not bad, (f'items table: rows with series current must be kind per_unit (a growth.yaml or SCF rule per '
                         f'unit); a CBO series goes in a series file (resources/projection_series_current.csv): {bad}')
        R_total = float('nan')   # read by step2_factor only for a 'current' row of kind total, ruled out above
        deferred = []
    drawn_as = {'other_agi': 'other_agi_remainder', 'business_income': 'business_income_drawn'}
    item_of = {v: k for k, v in drawn_as.items()}   # a drawn column's item: other_agi_remainder is other_agi's
    puf_cols = [drawn_as.get(v, v) for v in BT.PUF_AMOUNTS
                if not (v == 'capital_gains' or (v in D.META['computed_totals'] and v != 'business_income'))
                and drawn_as.get(v, v) in out]
    scf_cols = [v for v in BT.SCF_AMOUNTS if v in out]
    other_cols = [v for v in BT.CEX_AMOUNTS + BT.CPS_AMOUNTS + BT.SIPP_AMOUNTS if v in out]

    def grow(col, thunk, series):
        if item_mode and PS.same_as(col):
            deferred.append(col)   # the factors of another column, applied after every other column
            return
        if item_mode and col in tab.index and tab.loc[col, 'series'] != 'current':
            fp = fn = 1.0          # not read: the row's own series gives the growth (projection_steps.step2_factor)
            kind, series = 'per_unit', ''
        else:
            fp, fn = thunk()
            kind = 'per_unit'
        x = out[col].to_numpy(float)
        if item_mode:
            fp, fn, series = PS.step2_factor(col, x, fp, fn, kind, series, s, t, R_total)
        assert np.isfinite(fp) and fp > 0 and np.isfinite(fn) and fn > 0, f'{col}: growth factor {fp}, {fn}'
        out[col] = np.where(x > 0, x * fp, np.where(x < 0, x * fn, x))
        rows.append({'column': col, 'factor, positive values': fp, 'factor, negative values': fn, 'series': series,
                     'records with a value': int((x != 0).sum())})

    def puf_factors(v):
        # (positive values, negative values): year t's growth over year s's (build_table.growth)
        gs, gt = BT.growth(v, s), BT.growth(v, t)
        return gt[0] / gs[0], gt[1] / gs[1]
    for col in puf_cols:
        v = item_of.get(col, col)
        grow(col, lambda v=v: puf_factors(v), GR.describe(v))
    # the wave of income year y - 1 is the SCF of year y; growth per tax unit from the table's SCF rows, as the maps
    scf = pd.read_parquet(common.TABLE, columns=['source', 'year', 'weight'] + BT.SCF_AMOUNTS)
    scf = scf[scf.source == 'scf']
    # (30 October) every dollar item grown, or the run stops. (30 October, targets for every year) a year with no wave
    # of its own is interpolated between waves, or after the last wave grown by Z.1 household net worth per person
    # (maps.scf_mean), so every year has a factor
    grown_scf = [v for v in scf_cols if v not in BT.SCF_RATIOS]
    cache = {}

    def f_scf_of(v):
        if 'f' not in cache:
            cache['f'] = MP.scf_growth(scf, s - 1, t - 1)
            none = [u for u in grown_scf if not np.isfinite(cache['f'][u])]
            assert not none, f'no growth factor {s} to {t} (a mean per unit at or below zero in the wave of {s - 1} or ' \
                             f'{t - 1}): {none}'
        return cache['f'][v]
    for v in scf_cols:
        if v in BT.SCF_RATIOS:
            # a share; the mortgage share changes after the last wave so that the home and the mortgage each follow
            # their own Z.1 series (rebuild-wealth-cats); every other ratio has factor 1
            grow(v, lambda v=v: (f_scf_of(v),) * 2,
                 'ratio adjusted for the mortgage and home growth series' if v == 'w_primary_ltv' else 'a ratio, not grown')
        else:
            grow(v, lambda v=v: (f_scf_of(v),) * 2, f'SCF mean per tax unit, wave of {t - 1} over wave of {s - 1}')
    for v in other_cols:
        grow(v, lambda v=v: (GR.between(v, s, t),) * 2, GR.describe(v))
    if item_mode:
        for col in deferred:
            other = PS.same_as(col)
            r = next((q for q in rows if q['column'] == other), None)
            assert r is not None, f'{col}: same_as {other}, which is not grown'
            x = out[col].to_numpy(float)
            A = PS.same_as_add_factor(col, s, t)   # the column's own add factors (exactly 1 with none)
            fp, fn = r['factor, positive values'], r['factor, negative values']
            if A != 1.0:
                fp, fn = fp * A, fn * A
            out[col] = np.where(x > 0, x * fp, np.where(x < 0, x * fn, x))
            PS.record_same_as(col, other, x, A)
            rows.append({'column': col, 'factor, positive values': fp, 'factor, negative values': fn,
                         'series': f'same as {other}', 'records with a value': int((x != 0).sum())})
        PS.check_add_factors_applied(t)
    # the allowed capital loss: since 29-30 October the short and long term pieces are the gains and losses before the
    # $3,000 limit, grown like any amount, and recombine() applies the nominal limit again in year t (before, a piece
    # kg_other held the limit and needed a rule here)
    out, _ = recombine(out, t)
    return out, pd.DataFrame(rows)


def start_from_year(spec):
    """Year s's tilted file as the start: its records, its tilted weights (kept as weight_<s>), and its dollars grown
    to the target year (start_scaling dollars). Returns the frame, the weights and the growth table (None if not
    grown)."""
    s = spec['year']
    path = f"{common.OUT}/tilt/{spec['run']}/file_{s}.parquet"
    df = pd.read_parquet(path)
    assert df['weight'].notna().all() and (df['weight'] >= 0).all(), f'{path}: a missing or negative weight'
    # (4 November 2026) a start built on the real PUF carries the PUF's record numbers (puf_recid); settings
    # generation.filers must say the same, so a real-PUF year never grows from a synthetic file or the reverse
    real_start = 'puf_recid' in df and bool((df['puf_recid'] >= 0).any())
    assert real_start == common.REAL_PUF, (f'settings generation.filers is {common.FILERS}, but the start {path} was '
                                           f'built {"on the real PUF" if real_start else "on synthetic filers"}')
    w = df['weight'].to_numpy(float)
    df = df.drop(columns=['weight_start']).rename(columns={'weight': f'weight_{s}'})
    # (30 October) the year-s file carries the non-microdata imputations' columns; they are made again on this start
    # (impute below refuses a frame that has them), so they are dropped and the written-back columns put back to
    # their drawn values first (non_micro_imputations.as_drawn)
    df = NMI.as_drawn(df)
    common.say(f'start: {path}, {len(df):,} records, {w.sum() / 1e6:,.2f} million units')
    # step 1, the calibration to people by age and marital status, then step 2's dollar factors on its weights
    from scale import projection_steps as PS
    assert spec['weights'] == 'persons' and spec['dollars'] == 'item_factor', \
        'a start from an earlier year takes start_scaling {weights: persons, dollars: item_factor}'
    w, _ = PS.step1(df, w, Y, s)
    df, growth = grow_dollars(df, s, Y, 'item_factor')
    df['weight'] = w
    return df, w, growth


def start_nonfilers():
    """The non-filer count the start's non-filer weights are scaled to: the CPS's expected non-filers in the year
    (target_list.nonfiler_count). A year with no CPS units (the CPS years are 2015, 2017, ..., 2023) takes the geometric mean of
    the nearest CPS years either side: a placeholder used only to scale the start, not a target. Returns the count
    and a note."""
    t = pd.read_parquet(common.TABLE, columns=['source', 'year', 'weight', 'filer_p'])
    t = t[t.source == 'cps']
    have = sorted(t['year'].unique())
    count = lambda y: float((t.loc[t.year == y, 'weight'] * (1 - t.loc[t.year == y, 'filer_p'])).sum())
    if Y in have:
        return TLS.nonfiler_count(Y), ''
    if Y > max(have):   # past the last CPS year (30 October): the last year's count grown with the population
        n = TLS.nonfiler_count(Y)
        return n, f'past the last CPS year ({max(have)}): its expected non-filers grown with the Social Security area population'
    lo, hi = max([y for y in have if y < Y], default=None), min([y for y in have if y > Y], default=None)
    assert lo is not None and hi is not None, f'no CPS units for {Y}, and no CPS year on both sides'
    n = float(np.sqrt(count(lo) * count(hi)))
    note = (f'no CPS units for {Y}: the geometric mean of the CPS\'s expected non-filers in {lo} '
            f'({count(lo) / 1e6:.2f} million) and {hi} ({count(hi) / 1e6:.2f} million), a placeholder for scaling the '
            f'start only')
    common.say(note)
    return n, note
