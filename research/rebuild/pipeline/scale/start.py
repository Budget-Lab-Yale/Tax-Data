"""The year's start, in target-year dollars. For a past year it is the generated stack with its dollars drawn from the
year's maps; for a projection year, an earlier year's reweighted file with its weights calibrated by
projection_steps.step1 and its dollars grown item by item. The top step and the reweighting both start here.
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
        g19 = D.draw_dollars(raw, D.load_maps(Y), Y)
        imp = g19['weight'].to_numpy(float)
    else:
        g19, imp, growth = start_from_year(spec)
        raw = g19
    return g19, raw, imp, growth


def recombine(df, year):
    """Recompute, in the order dollars.draw_dollars uses, every column it builds from the drawn amounts: the totals,
    other AGI, taxable Social Security, amounts made from a share, remainders, net worth, and the deferral items of tax
    year `year`. Returns the frame and the names of the columns recomputed."""
    out, done = df.copy(), []

    def put(col, values):
        out[col] = values
        done.append(col)
    # kg_st and kg_lt are before the capital loss limit; capital_gains applies it
    put('capital_gains', common.allowed_capital_gain(out['kg_st'] + out['kg_lt'],
                                                     out['filing_status'] if 'filing_status' in out else None))
    put('capital_gains_line13', out['capital_gains'] + out['kg_dist'])
    totals = D.META['computed_totals']
    for total in ['business_income'] + [k for k in totals if k != 'business_income']:   # business income first
        put(total, D.signed_sum(out, totals[total]))
    put('other_agi', out['other_agi_remainder'] + out['kg_dist']
        + D.signed_sum(out, D.FOLD))
    # Taxable Social Security by the section 86 worksheet on the grown amounts. Its thresholds are not indexed, so growth
    # moves the taxable share as the law does.
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


def grow_dollars(df, s, t, mode='per_return_growth'):
    """Each record's amounts grown item by item from year s to year t by the growth lines the dollar maps use, as year
    t's factor over year s's. PUF amounts grow positive and negative values separately; the balance sheet grows per tax
    unit between SCF waves. With mode 'item_factor' (projection years) each column goes through
    projection_steps.step2_factor, which applies the items table's series on the step 1 weights. The computed columns
    are then recomputed from the grown parts. Returns the grown frame and a table of the factors."""
    from prep import build_table as BT
    from scale import maps as MP
    from shared import growth as GR
    same, done = recombine(df, s)
    for c in done:
        assert np.allclose(same[c].to_numpy(float), df[c].to_numpy(float), rtol=1e-9, atol=1e-6, equal_nan=True), \
            f'{c}: the year-{s} file\'s column is not what its parts give (dollars.draw_dollars changed?)'
    out, rows = df.copy(), []
    item_mode = mode == 'item_factor'
    if item_mode:
        from scale import projection_steps as PS
        tab = PS.items_table()
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
            fp = fn = 1.0          # unused: step2_factor applies the row's own series
            series = ''
        else:
            fp, fn = thunk()
        x = out[col].to_numpy(float)
        if item_mode:
            fp, fn, series = PS.step2_factor(col, x, fp, fn, series, s, t)
        assert np.isfinite(fp) and fp > 0 and np.isfinite(fn) and fn > 0, f'{col}: growth factor {fp}, {fn}'
        out[col] = np.where(x > 0, x * fp, np.where(x < 0, x * fn, x))
        rows.append({'column': col, 'factor, positive values': fp, 'factor, negative values': fn, 'series': series,
                     'records with a value': int((x != 0).sum())})

    def puf_factors(v):
        # (positive values, negative values): year t's growth over year s's
        gs, gt = BT.growth(v, s), BT.growth(v, t)
        return gt[0] / gs[0], gt[1] / gs[1]
    for col in puf_cols:
        v = item_of.get(col, col)
        grow(col, lambda v=v: puf_factors(v), GR.describe(v))
    # The SCF of year y is the wave of income year y - 1; growth per tax unit comes from the table's SCF rows.
    scf = pd.read_parquet(common.TABLE, columns=['source', 'year', 'weight'] + BT.SCF_AMOUNTS)
    scf = scf[scf.source == 'scf']
    # Every dollar item must get a factor, or the run stops.
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
            # a share: only the mortgage share changes, after the last wave; every other ratio has factor 1
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
            A = PS.same_as_add_factor(col, s, t)   # the column's own add factor, 1 if it has none
            fp, fn = r['factor, positive values'], r['factor, negative values']
            if A != 1.0:
                fp, fn = fp * A, fn * A
            out[col] = np.where(x > 0, x * fp, np.where(x < 0, x * fn, x))
            PS.record_same_as(col, other, x, A)
            rows.append({'column': col, 'factor, positive values': fp, 'factor, negative values': fn,
                         'series': f'same as {other}', 'records with a value': int((x != 0).sum())})
        PS.check_add_factors_applied(t)
    # kg_st and kg_lt are grown before the $3,000 loss limit; recombine applies the limit again in year t
    out, _ = recombine(out, t)
    return out, pd.DataFrame(rows)


def start_from_year(spec):
    """Year s's reweighted file as the start: its records (with year s's weights kept as weight_<s>), weights calibrated
    by projection_steps.step1, and dollars grown to the target year. Returns the frame, the weights and the growth
    table."""
    s = spec['year']
    path = f"{common.OUT}/tilt/{spec['run']}/file_{s}.parquet"
    df = pd.read_parquet(path)
    assert df['weight'].notna().all() and (df['weight'] >= 0).all(), f'{path}: a missing or negative weight'
    # A start built on the real PUF carries the PUF's record numbers; settings generation.filers must agree, so a
    # real-PUF year never grows from a synthetic file or the reverse.
    real_start = 'puf_recid' in df and bool((df['puf_recid'] >= 0).any())
    assert real_start == common.REAL_PUF, (f'settings generation.filers is {common.FILERS}, but the start {path} was '
                                           f'built {"on the real PUF" if real_start else "on synthetic filers"}')
    w = df['weight'].to_numpy(float)
    df = df.drop(columns=['weight_start']).rename(columns={'weight': f'weight_{s}'})
    # The non-microdata imputations are made again on this start, so their columns are dropped and the columns they
    # overwrote are put back to their drawn values.
    df = NMI.as_drawn(df)
    common.say(f'start: {path}, {len(df):,} records, {w.sum() / 1e6:,.2f} million units')
    # Step 1 calibrates the weights to people by age and marital status; step 2 grows the dollars on those weights.
    from scale import projection_steps as PS
    assert spec['weights'] == 'persons' and spec['dollars'] == 'item_factor', \
        'a start from an earlier year takes start_scaling {weights: persons, dollars: item_factor}'
    w, _ = PS.step1(df, w, Y, s)
    df, growth = grow_dollars(df, s, Y, 'item_factor')
    df['weight'] = w
    return df, w, growth


def start_nonfilers():
    """The non-filer count the start's non-filer weights are scaled to: the CPS's expected non-filers in the year. A year
    between CPS years takes the geometric mean of the nearest CPS years either side, a placeholder used only to scale
    the start, not a target. Returns the count and a note."""
    have = TLS.cps_years()
    n = TLS.nonfiler_count(Y)
    if Y in have:
        return n, ''
    if Y > max(have):   # past the last CPS year: the last year's count grown with the population
        return n, f'past the last CPS year ({max(have)}): its expected non-filers grown with the Social Security area population'
    lo, hi = TLS.cps_neighbours(Y)
    note = (f'no CPS units for {Y}: the geometric mean of the CPS\'s expected non-filers in {lo} '
            f'({TLS.nonfiler_count(lo) / 1e6:.2f} million) and {hi} ({TLS.nonfiler_count(hi) / 1e6:.2f} million), a '
            f'placeholder for scaling the start only')
    common.say(note)
    return n, note
