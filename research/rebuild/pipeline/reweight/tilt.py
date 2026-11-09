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

The targets. The year's list from targets_config/ (target_list.expand), each with a value and a standard error
(errors.py); target_list.conditions writes each as a linear condition on the weights.

The non-microdata imputations (non_micro_imputations.py, settings non_micro_imputations; NEXT_STEPS item 1, Q1): run
on the start after the prescaling, before the solve, they add new columns only (dependent filers' ages and sex, child
credit flags, death probabilities, yearly unrealized gains, cost basis, the holding period and basis of long-term
gains, parents' rank, and the placeholders); the ranking items rank on the prescaled start's weights. No target reads
them yet, so the weights do not depend on them. Their checks, on the weights after the tilt, go to
non_micro_checks.md and non_micro_check_*.csv.

The taxes (taxes.py, settings taxes; 30 October): after the non-microdata imputations, every record's taxes under the
year's law from tax-simulator-gpu's batch command, joined by record id (columns calc_*);
then the year's calculator targets (kind calculator) are expanded on them and, with settings taxes.target, join the
solve. Their fit before and after is taxes/taxes_fit.csv either way; taxes/taxes_summary.md the totals.

The tilt. solver.solve, the one-cost reweighting (decisions 57 and 68): the weights that minimize the sum over targets
of 0.5 x importance x (miss / standard error)^2 plus tau x the Kullback-Leibler move from the start, identities exact
(settings tilt.reweighting; tau 1,000; at most 150 Newton steps in all), with a soft cap on each weight (settings
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

from shared import common
from scale import non_micro_imputations as NMI
from scale import start as START
from reweight import solver as SV
from reweight import target_list as TLS
from prep import soi as TG
from reweight import taxes as TX
from shared.common import TARGET_YEAR as Y, wquantile

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
    start_lines = []
    g19, raw, imp, growth = START.year_start(spec)
    if growth is not None:   # each column's factors in full precision, read by package.py
        growth.to_csv(f'{common.TILT}/start_growth.csv', index=False)
    # the top of the file (top_records.py, settings tilt.top; 29 October): Part A's rebuilt hidden returns, the
    # redraw of balance sheets at $1 billion or more, and the Forbes list's billionaires, before any mask, condition or
    # prescaling. given: records whose starting weight is given (in returns or units); fixed: records held at it
    top = None
    given = fixed = np.zeros(len(g19), bool)
    # The top step's network part (Part A's hidden returns, the redraw of balance sheets at $1 billion or more) wants a
    # GPU: for a generated start the top stage (generate/top.py) writes it to the top cache, read here; a start from an
    # earlier year's file runs it here. The billionaires (top_records.add_list) need no network and are built here.
    import pickle
    from generate import top_records as TR
    cache = common.TOP_CACHE if spec['mode'] == 'generate' else None
    if common.REAL_PUF and cache:
        assert os.path.exists(cache), (f'filers real_puf: no top cache at {cache}. The top step\'s redraw at $1 billion '
                                       f'runs on a GPU: run the top stage first')
    if cache and os.path.exists(cache):
        with open(cache, 'rb') as fh:
            base = pickle.load(fh)
        common.say(f'the top step read from {cache}')
    else:
        base = TR.network_part(g19, raw, imp, Y, spec['mode'])
    g19, imp, given, fixed, top = TR.add_list(*base, Y)
    del base
    del raw
    x = g19['net_worth'].to_numpy(float)
    filer = g19['filer'].to_numpy() == 1

    # the start: the generated records scaled so that, with the records whose weight is given, filers add up to SOI's
    # returns and non-filers to the expected non-filers
    returns = TG.returns(soi, Y)
    n_nonfilers, nf_note = START.start_nonfilers()
    w0 = imp.copy()
    if spec['weights'] == 'persons':
        # (3 November, agent A) the step 1 weights as they are (start_from_year); the list's own weights are given
        from scale import projection_steps as PS
        assert np.isfinite(w0).all() and (w0 >= 0).all()
        common.say(f'projection start (step 1 weights): filers {w0[filer & ~given].sum() / 1e6:,.2f} million, '
                   f'non-filers {w0[~filer & ~given].sum() / 1e6:,.2f} million, the list {w0[given].sum():,.1f} units')
    else:
        for batch, total in [(filer, returns), (~filer, n_nonfilers)]:
            g = batch & ~given
            w0[g] = imp[g] * (total - imp[batch & given].sum()) / imp[g].sum()
    if given.any():
        # the records whose weight is given carry, in the weight column, their weight on the generated records'
        # relative scale, so that anything reading that column (expand's reach check, which ranks the income rank
        # groups on it) sees them in proportion
        gen_f = filer & ~given
        g19.loc[given, 'weight'] = w0[given] * imp[gen_f].sum() / w0[gen_f].sum()
    w_plain = w0.copy()
    prescale = spec['weights']
    assert prescale in ('demographic', 'persons'), f"start_scaling weights {prescale!r}"
    if prescale == 'demographic':
        for married, status in [(1, 'joint'), (0, 'nonjoint')]:
            m = filer & (g19['married'].to_numpy() == married)
            g = m & ~given
            w0[g] *= (TG.returns(soi, Y, '1.2', status) - w0[m & given].sum()) / w0[g].sum()

    # the targets (targets_config/, expanded by target_list.py, errors by errors.py) and the solve (solver.py):
    # every target hit exactly when they can all be hit; otherwise the standard errors decide which give way
    # the calculator targets wait for the taxes, computed after the non-microdata imputations (below)
    if prescale == 'persons':
        PS.STATE['start'] = (g19, w0, fixed)   # the projection targets (projection_steps.outside_rows) read it
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
    # the taxes (taxes.py; settings taxes, 30 October): every record's taxes under the year's law on the start; then the
    # calculator targets, on the computed columns
    g19, tax_ctx = TX.compute(g19, Y, w0, f'{common.TILT}/taxes')
    calc_rows = TLS.expand(Y, start=g19, which='calculator')
    common.say(f'calculator targets: {len(calc_rows)}')
    targeted_calc = calc_rows
    # income rank groups (the income x net worth grid) are set on the file's starting weights, so their conditions
    # are written again after the prescaling (a list without them is unchanged)
    ranked = 'rank_variable' in rows and rows['rank_variable'].fillna('').astype(str).ne('').any()
    if ranked or len(targeted_calc):
        if len(targeted_calc):
            rows = pd.concat([rows, targeted_calc], ignore_index=True)
        F, T, averages = TLS.conditions(g19, rows, w0)
        se, identity = rows['se'].to_numpy(float), rows['identity'].to_numpy(bool)
    if ranked:
        common.say('income rank lines on the file (starting weights): '
                   + '; '.join(f'{k[0]} {k[1]} p{k[2]:g}: {v:,.0f}' for k, v in TLS.FILE_RANK_LINES.items()))
    t43 = rows[(rows['kind'] == 'soi 4.3') & (rows['target'] == 'returns')]
    if len(t43):   # (3 November) Table 4.3's groups are at SOI's published AGI cutoffs
        common.say('Table 4.3 groups at SOI\'s published AGI cutoffs (returns on the starting weights, SOI): '
                   + '; '.join(f'{r.group} ${r.agi_lo:,.0f} ({float(w0 @ F[:, i]) / 1e3:,.1f} thousand, '
                               f'SOI {r.value / 1e3:,.1f} thousand)' for i, r in t43.iterrows()))
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
    from reweight import errors as ERR
    from reweight import precheck as PRE
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
               f"{cfg.get('newton_budget', 150)} Newton steps")
    # (6 November 2026, the joint solve of state weights, state_joint.py) with REBUILD_SAVE_SOLVE set, the solve's inputs are
    # written to that file, and the solve's multipliers beside it afterwards. Nothing else changes.
    save_solve = os.environ.get('REBUILD_SAVE_SOLVE')
    if save_solve:
        import pickle
        with open(save_solve, 'wb') as fh:
            pickle.dump({'record_id': g19['record_id'].to_numpy(), 'w0': w0, 'F': F, 'T': T, 'se': se, 'identity': identity,
                         'averages': averages, 'rho': rho, 'fixed': fixed, 'soft_upper': S_main.get('solver_soft_upper'),
                         'soft_beta': S_main.get('solver_soft_beta'), 'rows': rows, 'reweighting': cfg}, fh, protocol=4)
    t_solve = time.time()
    w1, info = SV.solve(w0, F, T, se, identity, averages, S_main, fixed=fixed)
    info['solve_seconds'] = time.time() - t_solve
    if save_solve:
        np.savez(save_solve + '.answer.npz', multipliers=info['multipliers'], w1=w1, newton_steps=info['newton_steps'],
                 converged=info['converged'])
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
    if prescale == 'persons':
        start_lines += PS.write(common.TILT)

    # outputs
    fit = SV.fit_table(w0, w1, F, T, se, averages, rows, identity)
    fit.to_csv(f'{common.TILT}/fit.csv', index=False)
    factors = weight_factor_table(w0, w1, grp)
    factors.to_csv(f'{common.TILT}/weight_factors.csv', index=False)
    g19.assign(weight_start=w0, weight=w1).to_parquet(f'{common.TILT}/file_{Y}.parquet', index=False)
    NMI.write_qbi_scalars(nmi_ctx, common.TILT)   # beside the file: the projection years read them from their start's folder
    # the non-microdata imputations' published checks, on the weights after the tilt (Q9: reported only)
    NMI.write_checks(NMI.checks(g19, Y, w1, nmi_ctx), common.TILT, Y)
    # the calculator targets before and after (targeted or not), the totals
    tfit = fit[fit['kind'] == TLS.CALCULATOR_KIND] if len(targeted_calc) else TX.evaluate(g19, calc_rows, w0, w1)
    tfit.to_csv(f'{common.TILT}/taxes/taxes_fit.csv', index=False)
    tax_lines = TX.summary_lines(tax_ctx, g19, w0, w1)
    open(f'{common.TILT}/taxes/taxes_summary.md', 'w').write('\n'.join(tax_lines))
    from reweight import scf_targets
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
