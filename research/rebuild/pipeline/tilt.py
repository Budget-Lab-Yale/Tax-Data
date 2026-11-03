"""Post-training: the generated stack reweighted to the target year (settings "years"; 2019 by default).

One algorithm every year (TARGETS.md S7): build the start, build the targets, tilt.

The start. The year's targets.yaml (targets_config/<year>/targets.yaml, read by target_list.start_spec) says which:
  - start: generate (the default): the run's generated stack, its dollars drawn from the year's maps, each record
    carrying its importance weight from generation;
  - start: {from_year: s, run: <name>}: year s's tilted file, <out>/tilt/<name>/file_<s>.parquet, its tilted
    weights in place of the importance weights, and (start_scaling dollars: per_return_growth) each record's
    amounts grown item by item from s to the target year (grow_dollars).
Either way the weights are then scaled (start_scaling weights; default settings tilt.prescale):
  - filers to SOI's returns for the year and non-filers to the CPS's expected non-filers (the sum over its units of
    weight x (1 - filer_p); start_nonfilers);
  - "filing_status" and "demographic": filers within joint and non-joint returns to SOI's counts of each (Table 1.2);
  - "demographic" (TARGETS.md section 4): then each batch scaled to the year's demographic cells before any other
    target, by the same solver and the same target rows the tilt uses (prescale_demographic): filers to SOI Table
    1.6's returns by the primary filer's age range in joint and in non-joint returns, keeping the two totals;
    non-filers to the non-filer people targets, keeping their total. The cells stay targets in the tilt.
With hold_distribution in the year file, the start's distribution of each listed item across AGI rank groups
becomes targets too (target_list.hold_rows), built after the scaling from the scaled start.

The targets. The year's list from targets_config/ (target_list.expand), each with a value and a standard error
(errors.py); target_list.conditions writes each as a linear condition on the weights.

The non-microdata imputations (non_micro_imputations.py, settings non_micro_imputations; NEXT_STEPS item 1, Q1): run
on the start after the prescaling, before the solve, they add new columns only (dependent filers' ages and sex, child
credit flags, death probabilities, yearly unrealized gains, cost basis, the holding period and basis of long-term
gains, parents' rank, and the placeholders); the ranking items rank on the prescaled start's weights. No target reads
them yet, so the weights do not depend on them. Their checks, on the weights after the tilt, go to
non_micro_checks.md and non_micro_check_*.csv.

The taxes (taxes.py, settings taxes; 30 October): after the non-microdata imputations, every record's taxes under the
year's law from tax-simulator-gpu's batch command, the QBI dials solved first, joined by record id (columns calc_*);
then the year's calculator targets (kind calculator) are expanded on them and, with settings taxes.target, join the
solve. Their fit before and after is taxes/taxes_fit.csv either way; taxes/taxes_summary.md the totals and the dials.

The tilt. solver.solve, the one-cost reweighting (decisions 57 and 68): the weights that minimize the sum over targets
of 0.5 x importance x (miss / standard error)^2 plus tau x the Kullback-Leibler move from the start, identities exact
(settings tilt.reweighting; tau 1,000; at most 100 Newton steps in all), with a soft cap on each weight (settings
tilt.soft_cap). The demographic prescaling before it uses the same rule on its own cells. No target has a range and none is
dropped.

Writes, in common.TILT: file_<target year>.parquet (every record, every dollar column in nominal target-year
dollars, net worth and its parts included; weight_start and weight, before and after the tilt), fit.csv (every
target, before and after, with its miss in standard errors and the thin-cell measures), targets.csv and
resolved_<year>.yaml (target_list.write), scf_targets_by_wave.csv (each SCF target's value, standard error, lines
and sample households in each wave of the pool, scf_targets.py), summary.md. report_tilt.py writes the run report
from them.
"""
import os
import time

import numpy as np
import pandas as pd

import common
import dollars as D
import non_micro_imputations as NMI
import solver as SV
import target_list as TLS
import targets as TG
import taxes as TX
from common import TARGET_YEAR as Y, wquantile

S = common.SETTINGS['tilt']


# ---- how far the weights moved -------------------------------------------------------------------------------
def cost(w0, w1):
    """How far the weights moved from w0 to w1: summary.md's measures."""
    ess = SV.effective_sample
    f = w1 / w0
    return {'KL move (nats)': SV.kl_move(w0, w1),
            'effective sample kept (%)': 100 * ess(w1) / ess(w0),
            'weight factor, 1st percentile': np.quantile(f, 0.01), 'weight factor, 99th percentile': np.quantile(f, 0.99),
            'records moved more than tenfold up': int((f > 10).sum()),
            'records moved more than tenfold down': int((f < 0.1).sum())}


# ---- importance and the AGI groups -------------------------------------------------------------------------
def importance(kinds):
    """Each target's importance in the cost: 1 for the kinds in settings tilt.reweighting.importance_one_kinds (SOI's
    published figures, the W-2 statistics, the calculator's columns against SOI's), 1 / other_importance_divisor for every
    other kind (the survey and pipeline kinds)."""
    cfg = S['reweighting']
    return np.where(pd.Series(kinds).isin(set(cfg['importance_one_kinds'])).to_numpy(), 1.0,
                    1.0 / float(cfg['other_importance_divisor']))


AGI_GROUP_NAMES = ['$0 or less', '$0-50k', '$50-100k', '$100-200k', '$200-500k', '$500k-1m', '$1-10m', '$10m and over', 'non-filers']


def agi_groups(agi, filer):
    """The nine groups of the soft cap and the weight-factor table: the calculator's AGI ($0 or less, 0-50k, 50-100k,
    100-200k, 200-500k, 500k-1m, 1-10m, 10m and over) for filers; non-filers their own (index 8)."""
    assert np.isfinite(agi).all(), 'agi_groups: a missing calculator AGI'
    edges = [50e3, 100e3, 200e3, 500e3, 1e6, 10e6]
    g = np.where(agi <= 0, 0, 1 + np.searchsorted(edges, agi, side='right'))
    return np.where(filer, g, 8)


def weight_factor_table(w0, w1, grp):
    """The weight factor (final over starting weight) overall and by AGI group among the records with a starting weight:
    percentiles, the records raised more than 10x and 100x, cut below 0.1x, and the largest final weight."""
    pcs = [1, 5, 10, 25, 50, 75, 90, 95, 99, 99.9, 100]
    on = w0 > 0
    f = np.where(on, w1 / np.where(on, w0, 1.0), np.nan)
    rows = []
    for name, m in [('all', on)] + [(AGI_GROUP_NAMES[i], (grp == i) & on) for i in range(9)]:
        x = f[m]
        r = {'group': name, 'records': int(m.sum())}
        if len(x):
            r.update({f'p{q:g}': float(np.percentile(x, q)) for q in pcs})
            r.update({'raised over 10x': int((x > 10).sum()), 'raised over 100x': int((x > 100).sum()),
                      'cut below 0.1x': int((x < 0.1).sum()), 'largest final weight': float(w1[m].max())})
        rows.append(r)
    return pd.DataFrame(rows)


# ---- the demographic prescaling (TARGETS.md section 4) ------------------------------------------------
PRESCALE_CELLS = {'filers': ['age'], 'non-filers': ['non-filers', 'non-filer people']}   # target kinds per batch


def prescale_demographic(w0, F, T, se, identity, averages, rows, filer, married, soi, fixed=None):
    """The starting weights scaled to the year's demographic cells, each batch on its own: filers to SOI Table 1.6's
    returns by primary age range in joint and non-joint returns, keeping the joint and non-joint totals; non-filers
    to the non-filer people targets, keeping their total. Each is the smallest Kullback-Leibler move of that batch's
    weights, at the one cost, that meets its cells (solver.solve on the batch alone, with the target rows' own conditions,
    standard errors, importances and identities; the status totals are identities). Records in fixed (the billionaires) keep their weights:
    their part of each cell is a constant (solver.solve's fixed). Returns the new weights and, per batch, a dict for
    the summary: the solve, the batch's effective sample before and after, the factors, and each cell before and
    after."""
    w = w0.copy()
    fixed = np.zeros(len(w0), bool) if fixed is None else np.asarray(fixed, bool)
    out = []
    for batch, kinds in PRESCALE_CELLS.items():
        rec = filer if batch == 'filers' else ~filer
        cols = np.flatnonzero(rows['kind'].isin(kinds).to_numpy())
        if not len(cols):   # a year with no such cells (no CPS non-filer values in 2022): the batch is left as it is
            common.say(f'prescaling {batch}: the year has no targets of kinds {kinds}; left as scaled')
            continue
        assert all(averages[j] is None for j in cols), f'prescaling {batch}: an average among its cells'
        assert not (F[~rec][:, cols] != 0).any(), f'prescaling {batch}: a cell that records outside the batch reach'
        Fb, Tb, seb, idb = F[rec][:, cols], T[cols], se[cols], identity[cols]
        labels = (rows['kind'] + ' | ' + rows['group']).to_numpy()[cols].tolist()
        if batch == 'filers':
            # keep today's joint and non-joint scaling exactly: the two totals as identities
            extra = [((married[rec] == m).astype(float), TG.returns(soi, Y, '1.2', s), f'filers | {s} returns')
                     for m, s in [(1, 'joint'), (0, 'nonjoint')]]
            Fb = np.column_stack([Fb] + [c for c, _, _ in extra])
            Tb = np.append(Tb, [v for _, v, _ in extra])
            seb, idb = np.append(seb, [0.0, 0.0]), np.append(idb, [True, True])
            labels += [lab for _, _, lab in extra]
        rho = importance(rows['kind'].to_numpy()[cols])
        if batch == 'filers':
            rho = np.append(rho, [1.0, 1.0])
        wb0 = w[rec]
        wb1, info = SV.solve(wb0, Fb, Tb, seb, idb, [None] * len(Tb), {**S, 'solver_importance': rho}, fixed=fixed[rec])
        ess = SV.effective_sample
        f = wb1 / wb0
        out.append({'batch': batch, 'records': int(rec.sum()), 'cell count': len(Tb), 'converged': info['converged'],
                    'how': info['how'], 'effective records before': ess(wb0), 'effective records after': ess(wb1),
                    'effective sample kept (%)': 100 * ess(wb1) / ess(wb0), 'KL move (nats)': info['kl_move'],
                    'largest factor': f.max(), 'smallest factor': f.min(), 'Newton steps': info.get('newton_steps'),
                    'records moved more than twofold': int(((f > 2) | (f < 0.5)).sum()),
                    'cells': pd.DataFrame({'cell': labels, 'target': Tb, 'before': Fb.T @ wb0, 'after': Fb.T @ wb1})})
        common.say(f"prescaling {batch}: {info['how']}; effective sample kept {out[-1]['effective sample kept (%)']:.1f}%, "
                   f"factors {f.min():.3f} to {f.max():.3f}")
        w[rec] = wb1
    return w, out


def prescale_lines(prescaled):
    """summary.md's section on the prescaling."""
    if not prescaled:
        return []
    lines = ['## The demographic prescaling (before the tilt)', '']
    head = pd.DataFrame([{k: v for k, v in p.items() if k != 'cells'} for p in prescaled]).set_index('batch').T
    lines += [head.to_markdown(floatfmt=',.3f'), '']
    for p in prescaled:
        c = p['cells'].assign(**{'before - target (%)': lambda d: 100 * (d.before / d.target - 1)})
        lines += [f"### {p['batch']}: cells (people or returns)", '', c.to_markdown(index=False, floatfmt=',.1f'), '']
    return lines


# ---- the start from an earlier year's tilted file (TARGETS.md S7) ---------------------------------------------
def recombine(df, year):
    """The columns dollars.draw_dollars computes from the drawn amounts, computed again from the (grown) drawn
    amounts in the same order: the capital gains totals, business income and its two schedules from the eighteen
    parts, other AGI (the drawn remainder plus capital gain distributions plus the AGI items folded in), total
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


def grow_dollars(df, s, t, mode='per_return_growth', return_growth=None):
    """Each record's amounts grown item by item from year s to year t (TARGETS.md S7, per-return growth), by the
    series the dollar maps are grown by (build_table.build_maps), each item's line in the target year's growth.yaml
    (growth.py), as a ratio of year t's factor to year s's:
      - PUF amounts: SOI Table 1.4's dollars per return with the item where Table 1.4 has it, else the item's declared
        series (build_table.growth: positive and negative values separately; since 29-30 October capital
        losses grown with the gains, the loss limit applied again by recombine where AGI is made);
      - the balance sheet: growth per tax unit, item by item, the mean per unit of the wave of income year t - 1
        over that of s - 1 (build_table.scf_growth, the maps' rule); when either wave is missing, or an item's mean
        per unit is not positive in both, the run stops (until 30 October those were left ungrown);
      - consumption: NIPA PCE per return, by category;
      - the CPS-only dollars (childcare, overtime) and the SIPP's tips: SOI's wages per return; the CPS shares, weeks
        and hours not grown.
    Then every computed column is computed again from the grown parts (recombine; checked first to reproduce the
    file's own columns). Ages and every category are left as they are. Returns the grown frame and a table of the
    factors, one row per column."""
    import build_table as BT
    import growth as GR
    same, done = recombine(df, s)
    for c in done:
        assert np.allclose(same[c].to_numpy(float), df[c].to_numpy(float), rtol=1e-9, atol=1e-6, equal_nan=True), \
            f'{c}: the year-{s} file\'s column is not what its parts give (dollars.draw_dollars changed?)'
    out, rows = df.copy(), []
    cbo_mode = mode == 'cbo_per_return'
    drawn_as = {'other_agi': 'other_agi_remainder', 'business_income': 'business_income_drawn'}
    item_of = {v: k for k, v in drawn_as.items()}   # a drawn column's item: other_agi_remainder is other_agi's
    puf_cols = [drawn_as.get(v, v) for v in BT.PUF_AMOUNTS
                if not (v == 'capital_gains' or (v in D.META['computed_totals'] and v != 'business_income'))
                and drawn_as.get(v, v) in out]
    scf_cols = [v for v in BT.SCF_AMOUNTS if v in out]
    other_cols = [v for v in BT.CEX_AMOUNTS + BT.CPS_AMOUNTS + BT.SIPP_AMOUNTS if v in out]
    plan, fallback = {}, {}
    if cbo_mode:
        # (30 October, projection years as production builds them, DESIGN.md of explore/2026-10-30/demog; since decision
        # 74 every projection year starts from the 2023 file, s = 2023): each amount grown by its source's growth from s
        # to t divided by CBO's growth in the number of returns (projection_growth.plan_factors; from 2037, by the
        # demographic cells' growth in returns, return_growth). An item with no rule there (the mortgage interest, real estate tax, medical
        # expenses and other itemized items, some balance sheet items, the shares, weeks and hours) keeps the rule of
        # the year file's growth.yaml, as before, and is listed in the factors table as "fallback"; settings
        # tilt.projection_strict makes the run stop on such an item instead (decision 40)
        import projection_growth as PG
        assert t > s, f'a projection start grows forward, not {s} to {t}'
        names = {c: item_of.get(c, c) for c in puf_cols + [v for v in scf_cols if v not in BT.SCF_RATIOS] + other_cols}
        got, problems = PG.plan_factors(list(dict.fromkeys(names.values())), t, return_growth, strict=False, from_year=s)
        assert not (problems and S.get('projection_strict', False)), \
            f'{len(problems)} dollar items with no projection rule:\n  ' + '\n  '.join(problems)
        by_name = got.set_index('column')
        # (2 November) gross Social Security has no fallback: the growth.yaml rule is SOI's per return, carried by
        # nothing past 2023, so a missing rule here must stop the run, not pass as the taxable-benefit growth
        for need in ('gross_ss',):
            assert need not in names.values() or need in by_name.index, \
                f'{need}: no projection rule for {t}: ' + '; '.join(q for q in problems if q.startswith(need))
        plan = {c: (float(by_name.loc[n, 'factor']), by_name.loc[n, 'source']) for c, n in names.items() if n in by_name.index}
        fallback = {c: next(q.split(': ', 1)[1] for q in problems if q.split(': ', 1)[0] == n) for c, n in names.items()
                    if n not in by_name.index}
        common.say(f'projection growth {s} to {t}: {len(plan)} amounts by the CBO or macro series per return, '
                   f'{len(fallback)} by the growth.yaml rule (fallback)')

    def grow(col, thunk, series):
        if col in plan:
            fp = fn = plan[col][0]
            series = plan[col][1]
        else:
            fp, fn = thunk()
            if cbo_mode:
                series = f'fallback (no projection rule: {fallback.get(col, "a ratio")[:80]}); {series}'
        x = out[col].to_numpy(float)
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
    # (build_table.scf_mean), so every year has a factor
    grown_scf = [v for v in scf_cols if v not in BT.SCF_RATIOS]
    cache = {}

    def f_scf_of(v):
        if 'f' not in cache:
            cache['f'] = BT.scf_growth(scf, s - 1, t - 1)
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
    w = df['weight'].to_numpy(float)
    df = df.drop(columns=['weight_start']).rename(columns={'weight': f'weight_{s}'})
    # (30 October) the year-s file carries the non-microdata imputations' columns; they are made again on this start
    # (impute below refuses a frame that has them), so they are dropped and the written-back columns put back to
    # their drawn values first (non_micro_imputations.as_drawn)
    df = NMI.as_drawn(df)
    common.say(f'start: {path}, {len(df):,} records, {w.sum() / 1e6:,.2f} million units')
    if spec['dollars'] == 'per_return_growth':
        df, growth = grow_dollars(df, s, Y)
    elif spec['dollars'] == 'cbo_per_return':
        rg = None
        if Y > 2036:   # CBO's returns end in 2036: the growth in returns is the demographic cells' own (DESIGN.md)
            import projection_demographics as PD
            cols = ['record_id', 'filer', 'married', 'filing_status', 'age_range', 'age1']
            scaled, _ = PD.population_cells(df[cols].assign(weight=w), Y)
            f = df['filer'].to_numpy() == 1
            rg = float(scaled[f].sum() / w[f].sum())
            common.say(f'returns grow {rg:.5f} from {s} to {Y}: the previous file\'s filer cells grown by population')
        df, growth = grow_dollars(df, s, Y, 'cbo_per_return', rg)
    else:
        growth = None
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


def hold_table(rows, df, w0, w1):
    """The hold_distribution targets' shares, on the starting and the final weights, computed directly (the fit
    table's are the solver's average form, equal on the start and close after)."""
    out, a = [], D.agi(df)
    for r in rows[rows['kind'] == 'hold distribution'].itertuples(index=False):
        x = TLS.record_value(df, r.variable)
        pop = TLS.population(df, r.population)
        g = pop & (a >= r.agi_lo) & (a < r.agi_hi)
        sh = lambda w: (w[g] * x[g]).sum() / (w[pop] * x[pop]).sum()
        out.append({'item': r.variable, 'group': r.group, 'share at the start (target, %)': 100 * r.value,
                    'after the tilt (%)': 100 * sh(w1), 'miss (share points)': 100 * (sh(w1) - r.value),
                    'error (share points)': 100 * r.se})
    return pd.DataFrame(out)


# ---- net worth summary -------------------------------------------------------------
def top_share(x, w, p):
    o = np.argsort(-x, kind='stable')
    xs, ws = x[o], w[o]
    cum = np.cumsum(ws)
    take = np.clip(p * w.sum() - (cum - ws), 0, ws)
    return (xs * take).sum() / (x * w).sum()


def net_worth_table(files):
    rows = {}
    for name, (df, x, w) in files.items():
        r = {'units (millions)': w.sum() / 1e6, 'total ($ trillion)': (x * w).sum() / 1e12,
             'median ($)': wquantile(x, w, [0.5])[0], 'top 1% share (%)': 100 * top_share(x, w, 0.01)}
        wage = common.wage_group(df['wages'].to_numpy(float))
        for _, _, g in common.WAGE_GROUPS:
            m = wage == g
            r[f'mean, wages {g} ($)'] = (x[m] * w[m]).sum() / w[m].sum()
        rows[name] = r
    return pd.DataFrame(rows)


def main():
    t_start = time.time()
    os.makedirs(common.TILT, exist_ok=True)
    soi = TG.load()
    spec = TLS.start_spec(Y)   # the year file's start and start_scaling (TARGETS.md S7)
    growth, start_lines = None, []
    if spec['mode'] == 'generate':
        raw = pd.read_parquet(f'{common.GEN}/stack.parquet')
        g19 = D.draw_dollars(raw, D.load_maps(Y), Y)   # every dollar amount in target-year dollars
        imp = g19['weight'].to_numpy(float)
    else:
        g19, imp, growth = start_from_year(spec)   # every dollar amount grown to the target year
        raw = g19
    # the top of the file (top_records.py, settings tilt.top; 29 October): Part A's rebuilt hidden returns, the
    # redraw of balance sheets at $1 billion or more, and the Forbes list's billionaires, before any mask, condition or
    # prescaling. given: records whose starting weight is given (in returns or units); fixed: records held at it
    top = None
    given = fixed = np.zeros(len(g19), bool)
    if S.get('top', {}).get('enabled', False):
        # (30 October) The top step draws from the network (Part A's hidden returns, the redraw of balance sheets at $1
        # billion or more), which wants a GPU, while the rest of the tilt wants none. With REBUILD_TOP_CACHE set, a run
        # with REBUILD_TOP_ONLY=1 (on a GPU) does that part (top_records.network_part), writes its result to that file
        # and stops; a later run (on a CPU partition) reads it in place of the step. (2 November) The billionaires
        # (top_records.add_list) need no network and are built on every run, after the cache; a cache written before
        # 2 November that holds them has them removed first (top_records.strip_list).
        import pickle
        import top_records as TR
        cache = os.environ.get('REBUILD_TOP_CACHE')
        if cache and os.path.exists(cache) and not os.environ.get('REBUILD_TOP_ONLY'):
            with open(cache, 'rb') as fh:
                base = TR.strip_list(*pickle.load(fh))
            common.say(f'the top step read from {cache}')
        else:
            base = TR.network_part(g19, raw, imp, Y, spec['mode'])
            if cache:
                with open(cache + '.part', 'wb') as fh:
                    pickle.dump(base, fh, protocol=4)
                os.replace(cache + '.part', cache)
                common.say(f'the top step written to {cache}')
        if os.environ.get('REBUILD_TOP_ONLY'):
            return
        g19, imp, given, fixed, top = TR.add_list(*base, Y)
        del base
    del raw
    x = g19['net_worth'].to_numpy(float)
    filer = g19['filer'].to_numpy() == 1

    # the start: the generated records scaled so that, with the records whose weight is given, filers add up to SOI's
    # returns and non-filers to the expected non-filers
    returns = TG.returns(soi, Y)
    n_nonfilers, nf_note = start_nonfilers()
    w0 = imp.copy()
    if spec['weights'] == 'population_cells':
        # (30 October, projection years as production builds them) each record's previous weight times the population
        # growth of its cell (filers: joint status x the primary's age band, from the married or unmarried population;
        # non-filers: the age band), no scaling to SOI's or CBO's returns; the cells are identity targets (target_list)
        import projection_demographics as PD
        path = f"{common.OUT}/tilt/{spec['run']}/file_{spec['year']}.parquet"
        prev = pd.read_parquet(path, columns=['record_id', 'weight', 'filer', 'married', 'filing_status', 'age_range', 'age1'])
        scaled, cell_table = PD.population_cells(prev, Y, from_year=spec['year'])
        by_id = pd.Series(scaled, index=prev['record_id'].to_numpy())
        take = ~given
        ids = g19['record_id'].to_numpy()[take]
        assert pd.Index(ids).isin(by_id.index).all(), 'a record of the start is not in the previous year\'s file'
        w0[take] = by_id.loc[ids].to_numpy()
        common.say(f'population cells {spec["year"]} to {Y}: filers {w0[filer].sum() / 1e6:,.2f} million (previous '
                   f'{prev.weight[prev.filer == 1].sum() / 1e6:,.2f}), non-filers {w0[~filer].sum() / 1e6:,.2f} million '
                   f'(previous {prev.weight[prev.filer == 0].sum() / 1e6:,.2f}); zero cells: '
                   f'{int((cell_table.previous_weight == 0).sum())}')
        cell_table.to_csv(f'{common.TILT}/population_cells_{Y}.csv', index=False)
    else:
        for batch, total in [(filer, returns), (~filer, n_nonfilers)]:
            g = batch & ~given
            w0[g] = imp[g] * (total - imp[batch & given].sum()) / imp[g].sum()
    if given.any():
        # the records whose weight is given carry, in the weight column, their weight on the generated records'
        # relative scale, so that anything reading that column (expand's reach check, which ranks Table 4.3's groups
        # on it) sees them in proportion
        gen_f = filer & ~given
        g19.loc[given, 'weight'] = w0[given] * imp[gen_f].sum() / w0[gen_f].sum()
    w_plain = w0.copy()
    prescale = spec['weights']
    assert prescale in ('none', 'filing_status', 'demographic', 'population_cells'), f"start_scaling weights {prescale!r}"
    if prescale in ('filing_status', 'demographic'):
        for married, status in [(1, 'joint'), (0, 'nonjoint')]:
            m = filer & (g19['married'].to_numpy() == married)
            g = m & ~given
            w0[g] *= (TG.returns(soi, Y, '1.2', status) - w0[m & given].sum()) / w0[g].sum()

    # the targets (targets_config/, expanded by target_list.py, errors by errors.py) and the solve (solver.py):
    # every target hit exactly when they can all be hit; otherwise the standard errors decide which give way
    # the calculator targets wait for the taxes, computed after the non-microdata imputations (below)
    rows = TLS.expand(Y, start=g19, which='without calculator')
    TLS.check_nonfiler_bands(g19, rows)   # every non-filer head and spouse in exactly one non-filer people band
    F, T, averages = TLS.conditions(g19, rows, w0)
    se, identity = rows['se'].to_numpy(float), rows['identity'].to_numpy(bool)
    missing = rows.loc[~identity & ~(se > 0), ['kind', 'group', 'target']]
    assert missing.empty, f'targets with no standard error:\n{missing.to_string()}'
    prescaled, w_status = [], w0.copy()
    if prescale == 'demographic':
        w0, prescaled = prescale_demographic(w0, F, T, se, identity, averages, rows, filer,
                                             g19['married'].to_numpy(), soi, fixed)
    # the non-microdata imputations (non_micro_imputations.py; NEXT_STEPS item 1, Q1): on the start, in target-year
    # dollars and with its prescaled weights, before the solve. New columns only, except amt_nols, which the AMT cap
    # changes (its drawn value kept as amt_nols_drawn); the ranking items rank on w0, and no target reads their columns
    # or amt_nols yet
    g19, nmi_ctx = NMI.impute(g19, Y, w0)
    # the taxes (taxes.py; settings taxes, 30 October): every record's taxes under the year's law on the start, the QBI
    # dials solved first on the start's weights; then the calculator targets, on the computed columns
    g19, tax_ctx = TX.compute(g19, Y, w0, nmi_ctx, f'{common.TILT}/taxes')
    calc_rows = TLS.expand(Y, start=g19, which='calculator')
    common.say(f'calculator targets: {len(calc_rows)}' + ('' if TX.S.get('target', True) else
                                                          ' (reported only: settings taxes.target is false)'))
    targeted_calc = calc_rows if TX.S.get('target', True) else pd.DataFrame()
    # hold_distribution: built from the start as scaled, so its rank groups and shares are the scaled start's
    hold = TLS.hold_rows(Y, g19, w0)
    # income rank groups (the income x net worth grid) are set on the file's starting weights, so their conditions
    # are written again after the prescaling (a list without them is unchanged)
    ranked = 'rank_variable' in rows and rows['rank_variable'].fillna('').astype(str).ne('').any()
    if len(hold) or ranked or len(targeted_calc):
        if len(targeted_calc):
            rows = pd.concat([rows, targeted_calc], ignore_index=True)
        if len(hold):
            rows = pd.concat([rows, hold], ignore_index=True)
        F, T, averages = TLS.conditions(g19, rows, w0)
        se, identity = rows['se'].to_numpy(float), rows['identity'].to_numpy(bool)
    if ranked:
        common.say('income rank lines on the file (starting weights): '
                   + '; '.join(f'{k[0]} {k[1]} p{k[2]:g}: {v:,.0f}' for k, v in TLS.FILE_RANK_LINES.items()))
    TLS.write(rows, common.TILT, Y)
    S_main = S
    feas = None
    if fixed.any():
        # PLAN.md step 20: before the solve, each target's remainder after the fixed records, and whether the others
        # alone can reach it (solver.fixed_feasibility); whether they can all be met together is the solve's answer
        feas = pd.concat([rows[['kind', 'group', 'target', 'value']].reset_index(drop=True),
                          SV.fixed_feasibility(w0, F, T, se, identity, averages, fixed).drop(columns='target')],
                         axis=1)
        feas.to_csv(f'{common.TILT}/top_feasibility.csv', index=False)
        common.say(f"feasibility with the fixed records: {int(feas['billionaires alone exceed'].sum())} targets the "
                   f"fixed records alone exceed, {int((~feas['reachable by the others']).sum())} the others cannot reach")
    # (1 November) the check before the reweighting: flags only, nothing dropped (precheck.py; settings tilt.precheck)
    precheck_lines = []
    if S.get('precheck', {}).get('enabled', False):
        import errors as ERR
        import precheck as PRE
        _, precheck_lines = PRE.run(rows, F, w0, averages, ERR.published_value(rows).to_numpy(float), se, identity,
                                    common.TILT, S['precheck'])
        common.say('\n'.join(precheck_lines))
    # (1 November, John) settings tilt.soft_cap {k, beta}: each record's cap U_i is k times the MEAN STARTING weight of
    # its AGI group (agi_groups; records with a positive starting weight, held records left out; decision 73, John, 1 November: it was the median), and the cost gains 0.5 beta ((w_i - U_i)+ / U_i)^2
    # per record: nothing below the cap, quadratic above it (solver._solve)
    agi_c = g19['calc_agi'].to_numpy(float)
    grp = agi_groups(agi_c, filer)
    soft = S.get('soft_cap') or {}
    if soft.get('k'):
        k = float(soft['k'])
        upper = np.full(len(w0), np.nan)
        tab = []
        for gi, nm in enumerate(AGI_GROUP_NAMES):
            m_ = (grp == gi) & (w0 > 0) & ~fixed
            mean_w = float(np.mean(w0[m_]))
            upper[grp == gi] = k * mean_w
            tab.append({'group': nm, 'records': int(m_.sum()), 'mean starting weight': mean_w, 'cap U (k x mean)': k * mean_w,
                        'records starting above U': int((w0[m_] > k * mean_w).sum())})
        assert np.isfinite(upper).all()
        tab = pd.DataFrame(tab)
        tab.to_csv(f'{common.TILT}/soft_cap_groups.csv', index=False)
        np.save(f'{common.TILT}/soft_cap_upper.npy', upper)
        common.say(f"soft cap k = {soft['k']:g}, beta = {soft['beta']:g}:\n" + tab.to_string(index=False))
        S_main = {**S_main, 'solver_soft_upper': upper, 'solver_soft_beta': float(soft['beta'])}
    # settings tilt.reweighting: the one cost (solver._solve). Each target's importance rho is 1 for the kinds listed in
    # importance_one_kinds and 1 / other_importance_divisor for every other kind (importance())
    cfg = S['reweighting']
    rho = importance(rows['kind'].to_numpy())
    S_main = {**S_main, 'solver_importance': rho}
    pd.DataFrame({'kind': rows['kind'], 'group': rows['group'], 'target': rows['target'], 'importance': rho}).to_csv(
        f'{common.TILT}/importance.csv', index=False)
    common.say(f"one cost: tau {cfg['tau']:g}, importance 1 for kinds {sorted(cfg['importance_one_kinds'])} ({int((rho == 1).sum())} targets), "
               f"{1 / float(cfg['other_importance_divisor']):.4f} for the other {int((rho != 1).sum())}; budget "
               f"{cfg.get('newton_budget', 100)} Newton steps")
    t_solve = time.time()
    w1, info = SV.solve(w0, F, T, se, identity, averages, S_main, fixed=fixed)
    info['solve_seconds'] = time.time() - t_solve
    common.say(f"the whole tilt before the solve: {(t_solve - t_start) / 60:.1f} minutes (the start, the prescaling, the taxes, the targets)")
    common.say(f"the solve: {info['solve_seconds'] / 60:.1f} minutes, {info['newton_steps']} Newton steps in "
               f"{info['newton_calls']} calls ({info['newton_capped_calls']} ended without meeting the stopping size)")
    assert np.array_equal(w1[fixed], w0[fixed]), 'a fixed weight moved'
    if top is not None:
        TR.write_report(top, common.TILT, Y)
    if spec['mode'] == 'from_year':
        start_lines = [f"## The start: the {spec['year']} tilted file (TARGETS.md S7)", '',
                       f"- tilt/{spec['run']}/file_{spec['year']}.parquet: {len(g19):,} records; its tilted weights "
                       f"scaled by start_scaling weights '{prescale}'; dollars '{spec['dollars']}'.",
                       f"- non-filers scaled to {n_nonfilers / 1e6:.2f} million"
                       f"{' (' + nf_note + ')' if nf_note else ''}.", '']
        if growth is not None:
            start_lines += [f"Growth factors, {spec['year']} to {Y}, by column:", '',
                            growth.to_markdown(index=False, floatfmt='.4f'), '']
    elif nf_note:
        start_lines = [f'- non-filers scaled to {n_nonfilers / 1e6:.2f} million ({nf_note}).', '']
    if len(hold):
        start_lines += ['## Held distribution (hold_distribution targets): shares of each total by AGI rank group', '',
                        hold_table(rows, g19, w0, w1).to_markdown(index=False, floatfmt='.3f'), '']

    # outputs
    fit = SV.fit_table(w0, w1, F, T, se, averages, rows, identity)
    fit.to_csv(f'{common.TILT}/fit.csv', index=False)
    factors = weight_factor_table(w0, w1, grp)
    factors.to_csv(f'{common.TILT}/weight_factors.csv', index=False)
    g19.assign(weight_start=w0, weight=w1).to_parquet(f'{common.TILT}/file_{Y}.parquet', index=False)
    # the non-microdata imputations' published checks, on the weights after the tilt (Q9: reported only)
    NMI.write_checks(NMI.checks(g19, Y, w1, nmi_ctx), common.TILT, Y)
    # the calculator targets before and after (targeted or not), the totals and the dials
    tfit = fit[fit['kind'] == TLS.CALCULATOR_KIND] if len(targeted_calc) else TX.evaluate(g19, calc_rows, w0, w1)
    tfit.to_csv(f'{common.TILT}/taxes/taxes_fit.csv', index=False)
    tax_lines = TX.summary_lines(tax_ctx, g19, w0, w1)
    open(f'{common.TILT}/taxes/taxes_summary.md', 'w').write('\n'.join(tax_lines))
    import scf_targets
    if scf_targets.RESULTS:   # the SCF targets by wave (scf_targets.py), for the record
        scf_targets.details().to_csv(f'{common.TILT}/scf_targets_by_wave.csv', index=False)
    scf_path = f'{common.DATA}/scf_units_{Y}.parquet'   # a year with no SCF wave has no column of its own
    u = pd.read_parquet(scf_path) if os.path.exists(scf_path) else None
    pooled = scf_targets.reference(Y) if scf_targets.RESULTS else None
    nw = net_worth_table({**({f'SCF {Y} wave': (u, u['net_worth'].to_numpy(float), u['weight'].to_numpy(float))}
                             if u is not None else {}),
                          **({'SCF, the targets\' waves pooled': pooled} if pooled is not None else {}),
                          'before the tilt': (g19, x, w0), 'after the tilt': (g19, x, w1)})
    top_lines = []
    if top is not None:
        bil = g19['billionaire'].to_numpy() == 1
        top_lines = ['## The top of the file (top_records.py)', ''] + [f'- {n}' for n in top['notes']] + [
            f'- records held fixed: {int(fixed.sum())} ({w0[fixed].sum():,.0f} units); their weights after the tilt '
            f'all exactly as before: {bool(np.array_equal(w1[fixed], w0[fixed]))}',
            *([f'- the list\'s records, free in the reweighting (a year after the last list): {int(bil.sum())}; '
               f'weights before the tilt {w0[bil].sum():,.1f} in all ({w0[bil].min():.4f} to {w0[bil].max():.4f}), '
               f'after {w1[bil].sum():,.1f} ({w1[bil].min():.4f} to {w1[bil].max():.4f}); factor (after over before) '
               f'{np.min(w1[bil] / w0[bil]):.4f} to {np.max(w1[bil] / w0[bil]):.4f}, median '
               f'{np.median(w1[bil] / w0[bil]):.4f}'] if (bil & ~fixed).any() else []),
            f"- free records: effective sample kept {100 * info.get('effective_sample_kept_free', np.nan):.1f}%, "
            f"Kullback-Leibler move {info.get('kl_move_free', np.nan):.4f} nats",
            f'- net worth below the ceiling (all records but the list): before the tilt '
            f'${(w0 * x)[~bil].sum() / 1e12:,.2f} trillion, after ${(w1 * x)[~bil].sum() / 1e12:,.2f} trillion; '
            f'the list ${(w1 * x)[bil].sum() / 1e12:,.2f} trillion']
        if feas is not None:
            top_lines += [f"- targets the fixed records alone exceed: {int(feas['billionaires alone exceed'].sum())}; "
                          f"targets the others cannot reach on their own: {int((~feas['reachable by the others']).sum())} "
                          f"(top_feasibility.csv)"]
        top_lines += ['']
    summary = pd.Series({'targets': len(T), **cost(w0, w1),
                         'KL move from the unscaled start (nats)': cost(w_plain, w1)['KL move (nats)'],
                         **({'effective sample kept from the start before the demographic prescaling (%)':
                             cost(w_status, w1)['effective sample kept (%)']} if prescaled else {})})
    by_kind = fit.groupby('kind').agg(targets=('target', 'size'), over_1_se=('over_1_se', 'sum'),
                                      largest_miss_in_se=('miss_in_se', lambda m: m.abs().max()),
                                      warnings=('flag', lambda f: (f == 'warning').sum()),
                                      stops=('flag', lambda f: (f == 'stop').sum()))
    text = [f'# The {Y} tilt', '', *SV.summary_lines(info, fit), '', summary.to_frame('').to_markdown(floatfmt=',.4f'),
            '', *precheck_lines, *top_lines, *start_lines, *prescale_lines(prescaled), *tax_lines,
            '## The non-microdata imputations (on the start; checks in non_micro_checks.md)', '',
            *[f'- {n}' for n in nmi_ctx['notes']], '',
            '## Weight factors (final weight over starting weight), all records and by AGI group', '',
            factors.to_markdown(index=False, floatfmt=',.3g'), '',
            '## Targets by kind', '', by_kind.to_markdown(floatfmt='.3f'), '',
            f'## Net worth (nominal {Y} dollars)', '', nw.to_markdown(floatfmt=',.1f'), '']
    if pooled is not None:
        text += ['The pooled SCF is the SCF targets\' waves and shares (scf_targets.py), each wave grown per tax unit to '
                 f'the {Y} wave. Its wage groups use the training table\'s wages (the SCF\'s translated to the PUF\'s '
                 f'definition, in each wave\'s own income year); the {Y} wave column uses the SCF\'s own wages.', '']
    open(f'{common.TILT}/summary.md', 'w').write('\n'.join(text))
    common.say('\n'.join(text))


if __name__ == '__main__':
    main()
