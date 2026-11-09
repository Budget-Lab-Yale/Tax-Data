"""The start of the reweighting: the target year's records and starting weights, with every dollar amount in
target-year dollars. The top step and the reweighting both begin here.

For a past year, the start is the generated records, with dollar amounts drawn from the year's dollar maps. For a
projection year, the start is an earlier year's reweighted file. Its weights are first calibrated to the target year's
population by age and marital status (projection_steps.step1). Its dollar amounts are then grown item by item to the
target year, and the columns computed from them (totals, net worth and so on) are recomputed. FEATURES.md, "4. Scaling
to the target year", describes the method.
"""
import numpy as np
import pandas as pd

from shared import common
from shared import dollars as D
from scale import non_micro_imputations as NMI
from reweight import target_list as TLS
from shared.common import TARGET_YEAR as Y


# ---- Recomputing the computed columns ----

def recombine(df, year):
    """Recomputes every column that dollars.draw_dollars builds from the drawn amounts, in the same order: the totals,
    other AGI, taxable Social Security, amounts made from a share, remainders, net worth, and the retirement-plan
    deferral items of tax year year. Returns the frame and the names of the columns recomputed."""
    out, done = df.copy(), []

    def put(col, values):
        out[col] = values
        done.append(col)
    # Short- and long-term gains (kg_st, kg_lt) are before the limit on a net capital loss; capital_gains applies it.
    put('capital_gains', common.allowed_capital_gain(out['kg_st'] + out['kg_lt'],
                                                     out['filing_status'] if 'filing_status' in out else None))
    put('capital_gains_line13', out['capital_gains'] + out['kg_dist'])
    totals = D.META['computed_totals']
    for total in ['business_income'] + [k for k in totals if k != 'business_income']:   # business income first
        put(total, D.signed_sum(out, totals[total]))
    put('other_agi', out['other_agi_remainder'] + out['kg_dist']
        + D.signed_sum(out, D.FOLD))
    # Taxable Social Security by the section 86 worksheet on the grown amounts. The worksheet's thresholds are not
    # indexed for inflation, and growth moves the taxable share as the law does.
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
    have = [slot for slot in D.PERSON_CAPS if f'{slot}_band' in out]   # tips and overtime, where they were drawn
    if have:
        D.person_items(out, out)
        made = {D.PERSON_CAPS[slot][0] for slot in have}
        done += sorted(made) + [k for k, parts in D.SUMS.items() if all(p in made for p in parts)] + \
            [k for k, (_, amount) in D.FLAGS_WITH.items() if amount in made]
    if all(c in out for c in D.NET_WORTH_PARTS):
        put('net_worth', D.signed_sum(out, D.NET_WORTH_PARTS))
    if 'elective_deferrals' in out:
        D.deferral_items(out, year)
        done += ['elective1', 'elective2', 'trad_contr_er1', 'trad_contr_er2', 'trad_deferrals', 'medicare_wages']
    return out, done


# ---- Growing a file's dollars from one year to another ----

def _grow_column(out, rows, deferred, tab, item_mode, s, t, col, factors, series):
    """Grows one column of out from year s to year t, in place, and adds a row to rows describing the factors.
    factors is a function that returns (factor for positive values, factor for negative values); series names the
    growth series. In a projection year (item_mode), a column that takes another column's factors (same_as) is only
    added to deferred and is grown later, and a column whose row in the items table names its own series is grown by
    that series in projection_steps.step2_factor."""
    from scale import projection_steps as PS
    if item_mode and PS.same_as(col):
        deferred.append(col)
        return
    if item_mode and col in tab.index and tab.loc[col, 'series'] != 'current':
        fp = fn = 1.0          # not used: step2_factor applies the row's own series
        series = ''
    else:
        fp, fn = factors()
    x = out[col].to_numpy(float)
    if item_mode:
        fp, fn, series = PS.step2_factor(col, x, fp, fn, series, s, t)
    assert np.isfinite(fp) and fp > 0 and np.isfinite(fn) and fn > 0, f'{col}: growth factor {fp}, {fn}'
    out[col] = np.where(x > 0, x * fp, np.where(x < 0, x * fn, x))
    rows.append({'column': col, 'factor, positive values': fp, 'factor, negative values': fn, 'series': series,
                 'records with a value': int((x != 0).sum())})


def _puf_factors(v, s, t):
    """A PUF item's growth from year s to year t, for positive and for negative values: year t's growth index over
    year s's."""
    from prep import build_table as BT
    gs, gt = BT.growth(v, s), BT.growth(v, t)
    return gt[0] / gs[0], gt[1] / gs[1]


def _scf_factor(cache, scf, grown_scf, s, t, v):
    """An SCF item's growth from year s to year t: the SCF's mean per tax unit in the wave of year t - 1 over its mean
    in the wave of year s - 1. The factors of every item are computed on the first call and kept in cache. Every grown
    item must get a factor, or the run stops."""
    from scale import maps as MP
    if 'f' not in cache:
        cache['f'] = MP.scf_growth(scf, s - 1, t - 1)
        none = [u for u in grown_scf if not np.isfinite(cache['f'][u])]
        assert not none, f'no growth factor {s} to {t} (a mean per unit at or below zero in the wave of {s - 1} or ' \
                         f'{t - 1}): {none}'
    return cache['f'][v]


def grow_dollars(df, s, t, mode='per_return_growth'):
    """Grows each record's dollar amounts from year s to year t, item by item, and returns the grown frame and a table
    of the factors. Each item's factor is year t's value of its growth series over year s's, as in the dollar maps.
    PUF items grow positive and negative values by separate factors. Balance-sheet items grow by the SCF's mean per tax
    unit from one wave to the other. With mode 'item_factor' (the projection years), each column's factor comes from
    projection_steps.step2_factor, which applies the series of the column's row in the items table. The columns that
    are computed from others (totals, net worth and so on) are then recomputed from the grown parts."""
    from prep import build_table as BT
    from scale import maps as MP
    from shared import growth as GR
    # 1. Check that the computed columns of the year-s file equal what their parts give.
    same, done = recombine(df, s)
    for c in done:
        assert np.allclose(same[c].to_numpy(float), df[c].to_numpy(float), rtol=1e-9, atol=1e-6, equal_nan=True), \
            f'{c}: the year-{s} file\'s column is not what its parts give (dollars.draw_dollars changed?)'

    # 2. List the columns to grow, by source. Two PUF items are grown through the column that holds their drawn part:
    #    other AGI through other_agi_remainder, and business income through business_income_drawn.
    out, rows = df.copy(), []
    item_mode = mode == 'item_factor'
    tab, deferred = None, None
    if item_mode:
        from scale import projection_steps as PS
        tab = PS.items_table()
        deferred = []
    drawn_as = {'other_agi': 'other_agi_remainder', 'business_income': 'business_income_drawn'}
    item_of = {v: k for k, v in drawn_as.items()}
    puf_cols = [drawn_as.get(v, v) for v in BT.PUF_AMOUNTS
                if not (v == 'capital_gains' or (v in D.META['computed_totals'] and v != 'business_income'))
                and drawn_as.get(v, v) in out]
    scf_cols = [v for v in BT.SCF_AMOUNTS if v in out]
    other_cols = [v for v in BT.CEX_AMOUNTS + BT.CPS_AMOUNTS + BT.SIPP_AMOUNTS if v in out]

    # 3. Grow the PUF items.
    for col in puf_cols:
        v = item_of.get(col, col)
        _grow_column(out, rows, deferred, tab, item_mode, s, t, col, lambda v=v: _puf_factors(v, s, t), GR.describe(v))

    # 4. Grow the SCF items. The SCF of year y is the wave of income year y - 1. The growth per tax unit comes from the
    #    SCF rows of the training table.
    scf = pd.read_parquet(common.TABLE, columns=['source', 'year', 'weight'] + BT.SCF_AMOUNTS)
    scf = scf[scf.source == 'scf']
    grown_scf = [v for v in scf_cols if v not in BT.SCF_RATIOS]
    cache = {}
    for v in scf_cols:
        if v in BT.SCF_RATIOS:
            # A ratio. Only the mortgage's share of the home's value changes, after the last wave; every other ratio
            # has factor 1.
            _grow_column(out, rows, deferred, tab, item_mode, s, t, v,
                         lambda v=v: (_scf_factor(cache, scf, grown_scf, s, t, v),) * 2,
                         'ratio adjusted for the mortgage and home growth series' if v == 'w_primary_ltv' else 'a ratio, not grown')
        else:
            _grow_column(out, rows, deferred, tab, item_mode, s, t, v,
                         lambda v=v: (_scf_factor(cache, scf, grown_scf, s, t, v),) * 2,
                         f'SCF mean per tax unit, wave of {t - 1} over wave of {s - 1}')

    # 5. Grow the CEX, CPS and SIPP items by their growth lines.
    for v in other_cols:
        _grow_column(out, rows, deferred, tab, item_mode, s, t, v, lambda v=v: (GR.between(v, s, t),) * 2,
                     GR.describe(v))

    # 6. In a projection year, grow each column that takes another column's factors, times its own add factor (1 if it
    #    has none). Then check that every add factor of the year was applied.
    if item_mode:
        for col in deferred:
            other = PS.same_as(col)
            r = next((q for q in rows if q['column'] == other), None)
            assert r is not None, f'{col}: same_as {other}, which is not grown'
            x = out[col].to_numpy(float)
            A = PS.same_as_add_factor(col, s, t)
            fp, fn = r['factor, positive values'], r['factor, negative values']
            if A != 1.0:
                fp, fn = fp * A, fn * A
            out[col] = np.where(x > 0, x * fp, np.where(x < 0, x * fn, x))
            PS.record_same_as(col, other, x, A)
            rows.append({'column': col, 'factor, positive values': fp, 'factor, negative values': fn,
                         'series': f'same as {other}', 'records with a value': int((x != 0).sum())})
        PS.check_add_factors_applied(t)

    # 7. Recompute the computed columns from the grown parts. Short- and long-term gains (kg_st, kg_lt) are grown
    #    before the $3,000 limit on a net capital loss, and recombine applies the limit again in year t.
    out, _ = recombine(out, t)
    return out, pd.DataFrame(rows)


# ---- The start of the year ----

def start_from_year(spec):
    """The start of a projection year: the reweighted file of an earlier year s. Returns its records with dollars grown
    to the target year (year s's weights kept in the column weight_<s>), the weights calibrated by
    projection_steps.step1, and the table of growth factors."""
    # 1. Read year s's file. A file built on the real PUF carries the PUF's record numbers. Settings generation.filers
    #    must agree with the file, so that a real-PUF year never grows from a synthetic file or the reverse.
    s = spec['year']
    path = f"{common.OUT}/tilt/{spec['run']}/file_{s}.parquet"
    df = pd.read_parquet(path)
    assert df['weight'].notna().all() and (df['weight'] >= 0).all(), f'{path}: a missing or negative weight'
    real_start = 'puf_recid' in df and bool((df['puf_recid'] >= 0).any())
    assert real_start == common.REAL_PUF, (f'settings generation.filers is {common.FILERS}, but the start {path} was '
                                           f'built {"on the real PUF" if real_start else "on synthetic filers"}')
    w = df['weight'].to_numpy(float)
    df = df.drop(columns=['weight_start']).rename(columns={'weight': f'weight_{s}'})
    # 2. Drop the columns of the non-microdata imputations and put back the drawn values of the columns they
    #    overwrote. The imputations are made again on this start.
    df = NMI.as_drawn(df)
    common.say(f'start: {path}, {len(df):,} records, {w.sum() / 1e6:,.2f} million units')
    # 3. Calibrate the weights to the target year's people by age and marital status (step 1). Then grow the dollars,
    #    using those weights where a column's growth is set as a total (step 2).
    from scale import projection_steps as PS
    assert spec['weights'] == 'persons' and spec['dollars'] == 'item_factor', \
        'a start from an earlier year takes start_scaling {weights: persons, dollars: item_factor}'
    w, _ = PS.step1(df, w, Y, s)
    df, growth = grow_dollars(df, s, Y, 'item_factor')
    df['weight'] = w
    return df, w, growth


def start_nonfilers():
    """The number of non-filers to which the start's non-filer weights are scaled: the CPS's expected number of
    non-filers in the year. A year between two CPS years takes the geometric mean of the nearest CPS years on either
    side. That value only scales the start; it is not a target. Returns the count and a note for summary.md."""
    have = TLS.cps_years()
    n = TLS.nonfiler_count(Y)
    if Y in have:
        return n, ''
    if Y > max(have):   # after the last CPS year: the last year's count grown with the population
        return n, f'past the last CPS year ({max(have)}): its expected non-filers grown with the Social Security area population'
    lo, hi = TLS.cps_neighbours(Y)
    note = (f'no CPS units for {Y}: the geometric mean of the CPS\'s expected non-filers in {lo} '
            f'({TLS.nonfiler_count(lo) / 1e6:.2f} million) and {hi} ({TLS.nonfiler_count(hi) / 1e6:.2f} million), a '
            f'placeholder for scaling the start only')
    common.say(note)
    return n, note


def year_start(spec):
    """The start described by spec (from target_list.start_spec). Returns four things: the records in target-year
    dollars; the records with their dollar bands and stored draws, from which the top step redraws; the starting
    weights; and the table of growth factors (None for a past year)."""
    growth = None
    if spec['mode'] == 'generate':
        raw = pd.read_parquet(f'{common.GEN}/stack.parquet')
        g19 = D.draw_dollars(raw, D.load_maps(Y), Y)
        imp = g19['weight'].to_numpy(float)
    else:
        g19, imp, growth = start_from_year(spec)
        raw = g19
    return g19, raw, imp, growth
