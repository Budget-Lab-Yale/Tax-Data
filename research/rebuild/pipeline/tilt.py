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

The tilt. solver.solve: the smallest move of the weights, in Kullback-Leibler divergence, that puts every target
inside its range of settings tilt.range_in_se standard errors, or of range_in_se_by_kind's number for its kind
(identities exactly); where the ranges cannot all be met, the standard errors decide which give way (TARGETS.md S6,
S9).

Writes, in common.TILT: file_<target year>.parquet (every record, every dollar column in nominal target-year
dollars, net worth and its parts included; weight_start and weight, before and after the tilt), fit.csv (every
target, before and after, with its miss in standard errors and the thin-cell measures), targets.csv and
resolved_<year>.yaml (target_list.write), scf_targets_by_wave.csv (each SCF target's value, standard error, lines
and sample households in each wave of the pool, scf_targets.py), summary.md. report_tilt.py writes the run report
from them.
"""
import os

import numpy as np
import pandas as pd

import common
import dollars as D
import non_micro_imputations as NMI
import solver as SV
import target_list as TLS
import targets as TG
from common import TARGET_YEAR as Y, wquantile

S = common.SETTINGS['tilt']


# ---- how far the weights moved -------------------------------------------------------------------------------
def cost(w0, w1):
    """How far the weights moved from w0 to w1: summary.md's measures."""
    p0, p1 = w0 / w0.sum(), w1 / w1.sum()
    ess = lambda w: w.sum() ** 2 / (w ** 2).sum()
    f = w1 / w0
    pos = p1 > 0
    return {'KL move (nats)': float(np.sum(p1[pos] * np.log(p1[pos] / p0[pos]))),
            'effective sample kept (%)': 100 * ess(w1) / ess(w0),
            'weight factor, 1st percentile': np.quantile(f, 0.01), 'weight factor, 99th percentile': np.quantile(f, 0.99),
            'records moved more than tenfold up': int((f > 10).sum()),
            'records moved more than tenfold down': int((f < 0.1).sum())}


# ---- the demographic prescaling (TARGETS.md section 4) ------------------------------------------------
PRESCALE_CELLS = {'filers': ['age'], 'non-filers': ['non-filers', 'non-filer people']}   # target kinds per batch


def prescale_demographic(w0, F, T, se, identity, averages, rows, filer, married, soi, fixed=None):
    """The starting weights scaled to the year's demographic cells, each batch on its own: filers to SOI Table 1.6's
    returns by primary age range in joint and non-joint returns, keeping the joint and non-joint totals; non-filers
    to the non-filer people targets, keeping their total. Each is the smallest Kullback-Leibler move of that batch's
    weights that meets its cells (solver.solve on the batch alone, with the target rows' own conditions, standard
    errors and identities; the status totals are identities). Records in fixed (the billionaires) keep their weights:
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
        wb0 = w[rec]
        wb1, info = SV.solve(wb0, Fb, Tb, seb, idb, [None] * len(Tb), S, fixed=fixed[rec])
        ess = lambda v: v.sum() ** 2 / (v ** 2).sum()
        f = wb1 / wb0
        out.append({'batch': batch, 'records': int(rec.sum()), 'cell count': len(Tb), 'feasible': info['feasible'],
                    'how': info['how'], 'effective records before': ess(wb0), 'effective records after': ess(wb1),
                    'effective sample kept (%)': 100 * ess(wb1) / ess(wb0), 'KL move (nats)': info['kl_move'],
                    'largest factor': f.max(), 'smallest factor': f.min(),
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
    put('capital_gains_line13', out[D.KG_PIECES].sum(axis=1))
    put('capital_gains', out['kg_st'] + out['kg_lt'] + out['kg_other'])
    parts = D.META['computed_totals']['business_income']
    put('business_income', out[list(parts)].to_numpy(float) @ np.array(list(parts.values()), float))
    for sub in [k for k in D.META['computed_totals'] if k != 'business_income']:   # the schedules, dividends, adjustments
        sp = D.META['computed_totals'][sub]
        put(sub, out[list(sp)].to_numpy(float) @ np.array(list(sp.values()), float))
    put('other_agi', out['other_agi_remainder'] + out['kg_dist']
        + out[list(D.FOLD)].to_numpy(float) @ np.array(list(D.FOLD.values()), float))
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
        put('net_worth', out[list(D.NET_WORTH_PARTS)].to_numpy(float) @ np.array(list(D.NET_WORTH_PARTS.values()), float))
    if 'elective_deferrals' in out:                       # the traditional deferrals and Medicare wages (28 October)
        D.deferral_items(out, year)
        done += ['elective1', 'elective2', 'trad_contr_er1', 'trad_contr_er2', 'trad_deferrals', 'medicare_wages']
    return out, done


def grow_dollars(df, s, t):
    """Each record's amounts grown item by item from year s to year t (TARGETS.md S7, per-return growth), by the
    series the dollar maps are grown by (build_table.build_maps), each item's line in the target year's growth.yaml
    (growth.py), as a ratio of year t's factor to year s's:
      - PUF amounts: SOI Table 1.4's dollars per return with the item where Table 1.4 has it, else the item's declared
        series (build_table.growth: positive and negative values separately; capital losses not grown);
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

    def grow(col, fp, fn, series):
        x = out[col].to_numpy(float)
        assert np.isfinite(fp) and fp > 0 and np.isfinite(fn) and fn > 0, f'{col}: growth factor {fp}, {fn}'
        out[col] = np.where(x > 0, x * fp, np.where(x < 0, x * fn, x))
        rows.append({'column': col, 'factor, positive values': fp, 'factor, negative values': fn, 'series': series,
                     'records with a value': int((x != 0).sum())})
    drawn_as = {'other_agi': 'other_agi_remainder', 'business_income': 'business_income_drawn'}
    for v in BT.PUF_AMOUNTS:
        if v == 'capital_gains' or (v in D.META['computed_totals'] and v != 'business_income'):
            continue                                  # computed from their parts (recombine)
        col = drawn_as.get(v, v)
        if col not in out:
            continue
        gs, gt = BT.growth(v, s), BT.growth(v, t)
        grow(col, gt[0] / gs[0], gt[1] / gs[1], GR.describe(v))
    # the wave of income year y - 1 is the SCF of year y; growth per tax unit from the table's SCF rows, as the maps
    scf = pd.read_parquet(common.TABLE, columns=['source', 'year', 'weight'] + BT.SCF_AMOUNTS)
    scf = scf[scf.source == 'scf']
    # (30 October, review of rebuild-year-config) every dollar item grown, or the run stops. (30 October, targets for
    # every year) a year with no wave of its own is interpolated between waves, or after the last wave grown by Z.1
    # household net worth per person (build_table.scf_mean), so every year has a factor
    grown_scf = [v for v in BT.SCF_AMOUNTS if v in out and v not in BT.SCF_RATIOS]
    f_scf = BT.scf_growth(scf, s - 1, t - 1)
    if grown_scf:
        none = [v for v in grown_scf if not np.isfinite(f_scf[v])]
        assert not none, f'no growth factor {s} to {t} (a mean per unit at or below zero in the wave of {s - 1} or ' \
                         f'{t - 1}): {none}'
    for v in BT.SCF_AMOUNTS:
        if v in out:
            if v in BT.SCF_RATIOS:
                grow(v, 1.0, 1.0, 'a ratio, not grown')
            else:
                grow(v, f_scf[v], f_scf[v], f'SCF mean per tax unit, wave of {t - 1} over wave of {s - 1}')
    for v in BT.CEX_AMOUNTS + BT.CPS_AMOUNTS + BT.SIPP_AMOUNTS:
        if v in out:
            f = GR.between(v, s, t)
            grow(v, f, f, GR.describe(v))
    # the allowed capital loss (29 October, from Part A's top_hidden.py, where it was found): the rule grows the
    # positive "other" gain piece but not losses, so a return held at the allowed loss in year s (a large loss, with
    # kg_other taking up the cap) would turn into a gain. The cap is nominal: such a return stays at it, and no return
    # goes below it
    if all(c in out for c in ['kg_st', 'kg_lt', 'kg_other', 'capital_gains', 'mars']):
        cap = np.where(df['mars'].to_numpy() == 3, -1500.0, -3000.0)
        at_cap = df['capital_gains'].to_numpy(float) <= cap + 0.5
        floor = cap - out['kg_st'].to_numpy(float) - out['kg_lt'].to_numpy(float)
        before = out['kg_other'].to_numpy(float)
        out['kg_other'] = np.where(at_cap, floor, np.maximum(before, floor))
        rows.append({'column': 'kg_other', 'factor, positive values': np.nan, 'factor, negative values': np.nan,
                     'series': f'then held at the allowed capital loss: {int(at_cap.sum())} records at it in {s} kept at '
                               f'it, {int((~at_cap & (before < floor)).sum())} others raised to it',
                     'records with a value': int((before != 0).sum())})
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
    common.say(f'start: {path}, {len(df):,} records, {w.sum() / 1e6:,.2f} million units')
    if spec['dollars'] == 'per_return_growth':
        df, growth = grow_dollars(df, s, Y)
    else:
        growth = None
    df['weight'] = w
    return df, w, growth


def add_fixed_columns(df, spec):
    """Columns computed outside the pipeline for each record, joined by record id (settings tilt.fixed_columns; the
    calculator experiment of 28 October, CALCULATOR_CONTRACT.md): spec {'file': a parquet with id and the columns,
    'columns': {name in the frame: column of the file}}. Every record must have a value. The tilt moves weights only,
    so the values stay as computed; a target can then name them (a catalog variable with record: the name)."""
    t = pd.read_parquet(spec['file'], columns=['id'] + list(spec['columns'].values()))
    t = t.rename(columns={**{v: k for k, v in spec['columns'].items()}, 'id': 'record_id'})
    assert t['record_id'].is_unique, f'{spec["file"]}: an id twice'
    clash = set(spec['columns']) & set(df.columns)
    assert not clash, f'fixed columns already in the frame: {clash}'
    out = df.merge(t, on='record_id', how='left', validate='one_to_one')
    gone = out[list(spec['columns'])].isna().any(axis=1)
    assert not gone.any(), f'{int(gone.sum())} records with no value in {spec["file"]}'
    common.say(f'fixed columns from {spec["file"]}: ' + ', '.join(spec['columns']))
    return out


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
        import top_records as TR
        g19, imp, given, fixed, top = TR.prepare_top(g19, raw, imp, Y, spec['mode'])
    del raw
    # the fixed columns after the top of the file: their file must hold a value for every record, the rebuilt hidden
    # returns and the billionaires included (add_fixed_columns stops otherwise)
    if S.get('fixed_columns'):
        g19 = add_fixed_columns(g19, S['fixed_columns'])
    x = g19['net_worth'].to_numpy(float)
    filer = g19['filer'].to_numpy() == 1

    # the start: the generated records scaled so that, with the records whose weight is given, filers add up to SOI's
    # returns and non-filers to the expected non-filers
    returns = TG.returns(soi, Y)
    n_nonfilers, nf_note = start_nonfilers()
    w0 = imp.copy()
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
    assert prescale in ('none', 'filing_status', 'demographic'), f"start_scaling weights {prescale!r}"
    if prescale in ('filing_status', 'demographic'):
        for married, status in [(1, 'joint'), (0, 'nonjoint')]:
            m = filer & (g19['married'].to_numpy() == married)
            g = m & ~given
            w0[g] *= (TG.returns(soi, Y, '1.2', status) - w0[m & given].sum()) / w0[g].sum()

    # the targets (targets_config/, expanded by target_list.py, errors by errors.py) and the solve (solver.py):
    # every target hit exactly when they can all be hit; otherwise the standard errors decide which give way
    rows = TLS.expand(Y, start=g19)
    TLS.check_nonfiler_bands(g19, rows)   # every non-filer head and spouse in exactly one non-filer people band
    F, T, averages = TLS.conditions(g19, rows, w0, average_form=S.get('average_form', 'zero'))
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
    nmi_ctx = None
    if NMI.S.get('run_in_tilt', False):
        g19, nmi_ctx = NMI.impute(g19, Y, w0)
    # hold_distribution: built from the start as scaled, so its rank groups and shares are the scaled start's
    hold = TLS.hold_rows(Y, g19, w0)
    # income rank groups (the income x net worth grid) are set on the file's starting weights, so their conditions
    # are written again after the prescaling (a list without them is unchanged)
    ranked = 'rank_variable' in rows and rows['rank_variable'].fillna('').astype(str).ne('').any()
    if len(hold) or ranked:
        if len(hold):
            rows = pd.concat([rows, hold], ignore_index=True)
        F, T, averages = TLS.conditions(g19, rows, w0, average_form=S.get('average_form', 'zero'))
        se, identity = rows['se'].to_numpy(float), rows['identity'].to_numpy(bool)
    if ranked:
        common.say('income rank lines on the file (starting weights): '
                   + '; '.join(f'{k[0]} {k[1]} p{k[2]:g}: {v:,.0f}' for k, v in TLS.FILE_RANK_LINES.items()))
    TLS.write(rows, common.TILT, Y)
    S_main = S
    by_kind = S.get('range_in_se_by_kind') or {}
    if by_kind:
        # the range in standard errors by kind of target (settings tilt.range_in_se_by_kind, {kind: k}); every other
        # kind keeps range_in_se. The demographic prescaling above keeps range_in_se.
        unknown = set(by_kind) - set(rows['kind'])
        assert not unknown, f'settings tilt.range_in_se_by_kind: kinds with no targets this year {unknown}'
        base = float(S.get('range_in_se', 1.0))
        kv = rows['kind'].map(lambda q: float(by_kind.get(q, base))).to_numpy(float)
        S_main = {**S, 'range_in_se_by_target': kv}
        common.say(f'range in standard errors by kind (default {base:g}): '
                   + '; '.join(f'{q}: {kv[(rows.kind == q).to_numpy()][0]:g} ({int((rows.kind == q).sum())} targets)'
                               for q in rows['kind'].unique()))
    feas = None
    if fixed.any():
        # PLAN.md step 20: before the solve, each target's remainder after the fixed records, and whether the others
        # alone can reach it (solver.fixed_feasibility); whether they can all be met together is the solve's answer
        feas = pd.concat([rows[['kind', 'group', 'target', 'value']].reset_index(drop=True),
                          SV.fixed_feasibility(w0, F, T, se, identity, averages, fixed, S_main).drop(columns='target')],
                         axis=1)
        feas.to_csv(f'{common.TILT}/top_feasibility.csv', index=False)
        common.say(f"feasibility with the fixed records: {int(feas['billionaires alone exceed'].sum())} targets the "
                   f"fixed records alone exceed, {int((~feas['reachable by the others']).sum())} the others cannot reach")
    w1, info = SV.solve(w0, F, T, se, identity, averages, S_main, fixed=fixed)
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
    fit = SV.fit_table(w0, w1, F, T, se, averages, rows, S_main, identity)
    fit.to_csv(f'{common.TILT}/fit.csv', index=False)
    g19.assign(weight_start=w0, weight=w1).to_parquet(f'{common.TILT}/file_{Y}.parquet', index=False)
    if nmi_ctx is not None:   # their published checks, on the weights after the tilt (Q9: reported only)
        NMI.write_checks(NMI.checks(g19, Y, w1, nmi_ctx), common.TILT, Y)
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
    by_kind = fit.groupby('kind').agg(targets=('target', 'size'), gave_way=('gave_way', 'sum'),
                                      largest_miss_in_se=('miss_in_se', lambda m: m.abs().max()),
                                      warnings=('flag', lambda f: (f == 'warning').sum()),
                                      stops=('flag', lambda f: (f == 'stop').sum()))
    text = [f'# The {Y} tilt', '', *SV.summary_lines(info, fit), '', summary.to_frame('').to_markdown(floatfmt=',.4f'),
            '', *top_lines, *start_lines, *prescale_lines(prescaled),
            *(['## The non-microdata imputations (on the start; checks in non_micro_checks.md)', '']
              + [f'- {n}' for n in nmi_ctx['notes']] + [''] if nmi_ctx is not None else []),
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
