"""Reweighting: the stage that gives the target year's file its final weights.

It starts from the year's records and their starting weights (scale/start.py), adds the top of the file (the richest
records and the Forbes list), and scales the starting weights so that filers add up to SOI's count of returns and
non-filers to the expected count. It then runs the non-microdata imputations and the tax calculator on every record,
lists the year's targets, and writes each target as a linear condition on the weights. The solver (solver.py) then
finds the weights that come closest to the targets, given their standard errors, while moving the weights as little as
possible from their start.

Writes file_<year>.parquet (every record, with its weights before and after), fit.csv, targets.csv, the tax and
imputation checks and summary.md to the year's tilt folder. report_tilt.py then writes the report from them.
FEATURES.md, "5. Reweighting", describes the method.

The stage runs in one of three ways. Run with no argument, it does everything above: this is a projection year, or a
file built by hand. For a past year, the integrated solve of the national and the state weights (state_joint.py) takes
the place of the solve here, and the stage runs twice. `prepare` does everything before the solve and writes the
solve's inputs (solve_inputs.pkl in the year's folder), the file with every weight at its start, and what the outputs
on the final weights will need (outputs_context.pkl). `finish`, run in the year's final folder after the integrated
solve, reads the file with the final weights and fit.csv from that solve and writes the outputs and summary.md.
"""
import json
import os
import pickle
import sys
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
from reweight import precheck as PRE
from reweight import scf_targets
from shared.common import TARGET_YEAR as Y, wquantile

S = common.SETTINGS['tilt']


# ---- How far the weights moved ----

def cost(w0, w1):
    """How far the weights moved from w0 to w1, as reported in summary.md: the Kullback-Leibler divergence, the share
    of the effective number of records kept, the 1st and 99th percentiles of the weight factor (new weight over
    starting weight), and the number of records whose weight rose or fell more than tenfold."""
    ess = SV.effective_sample
    f = w1 / w0
    return {'KL move (nats)': SV.kl_move(w0, w1),
            'effective sample kept (%)': 100 * ess(w1) / ess(w0),
            'weight factor, 1st percentile': np.quantile(f, 0.01), 'weight factor, 99th percentile': np.quantile(f, 0.99),
            'records moved more than tenfold up': int((f > 10).sum()),
            'records moved more than tenfold down': int((f < 0.1).sum())}


# ---- The start and its scaling ----

def top_of_file(g19, raw, imp, spec):
    """Adds the top of the file (top_records.py, settings tilt.top): the rebuilt returns that the PUF hides at the very
    top, the balance sheets redrawn at $1 billion or more, and the Forbes list. This happens before any target or
    scaling. For a start made of generated records, the part that uses the network has already run on a GPU in the
    top stage, and its result is read from the top cache. A start from an earlier year's file runs that part here.

    Returns the records, their weights, given (the records whose starting weight is set by the top step), fixed (the
    records whose weight the reweighting may not change) and a dict for the report."""
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
    return TR.add_list(*base, Y)


def scale_start(g19, imp, given, filer, prescale, soi):
    """The starting weights. Returns them, a copy taken before the scaling by joint and non-joint returns, the expected
    number of non-filers, and a note on how that number was found.

    For a past year, the generated records are scaled so that, together with the records whose weight is given, filers
    add up to SOI's count of returns and non-filers to the expected number of non-filers. With start_scaling weights
    'demographic', the filers are then scaled again so that joint and non-joint returns each add up to SOI's count.
    For a projection year (weights 'persons'), the weights are already calibrated to people (projection_steps.step1)
    and are used as they are."""
    returns = TG.returns(soi, Y)
    n_nonfilers, nf_note = START.start_nonfilers()
    w0 = imp.copy()
    if prescale == 'persons':
        assert np.isfinite(w0).all() and (w0 >= 0).all()
        common.say(f'projection start (step 1 weights): filers {w0[filer & ~given].sum() / 1e6:,.2f} million, '
                   f'non-filers {w0[~filer & ~given].sum() / 1e6:,.2f} million, the list {w0[given].sum():,.1f} units')
    else:
        for batch, total in [(filer, returns), (~filer, n_nonfilers)]:
            g = batch & ~given
            w0[g] = imp[g] * (total - imp[batch & given].sum()) / imp[g].sum()
    if given.any():
        # The records whose weight is given carry, in the weight column, their weight on the same relative scale as
        # the generated records. Anything that reads that column (expand ranks the income rank groups on it) then
        # sees them in proportion.
        gen_f = filer & ~given
        g19.loc[given, 'weight'] = w0[given] * imp[gen_f].sum() / w0[gen_f].sum()
    w_plain = w0.copy()
    if prescale == 'demographic':
        for married, status in [(1, 'joint'), (0, 'nonjoint')]:
            m = filer & (g19['married'].to_numpy() == married)
            g = m & ~given
            w0[g] *= (TG.returns(soi, Y, '1.2', status) - w0[m & given].sum()) / w0[g].sum()
    return w0, w_plain, n_nonfilers, nf_note


# The demographic prescaling. Each batch of records is scaled to the target kinds listed for it.
PRESCALE_CELLS = {'filers': ['age'], 'non-filers': ['non-filers', 'non-filer people']}
PRESCALE_KINDS = [k for kinds in PRESCALE_CELLS.values() for k in kinds]


def prescale_demographic(w0, F, T, se, identity, averages, rows, filer, married, soi, fixed):
    """Scales the starting weights to the year's demographic cells, one batch at a time. Filers are scaled to SOI
    Table 1.6's returns by the primary filer's age range, in joint and in non-joint returns, keeping the totals of
    joint and of non-joint returns. Non-filers are scaled to the targets for non-filer people, keeping their total.

    Each batch takes the smallest Kullback-Leibler move that meets its cells, found by solver.solve with the same cost
    as the main solve. The records in fixed keep their weights. rows, F, T, se, identity and averages are the targets
    of the kinds in PRESCALE_KINDS and their conditions. Returns the new weights and a summary dict for each batch."""
    w = w0.copy()
    out = []
    for batch, kinds in PRESCALE_CELLS.items():
        rec = filer if batch == 'filers' else ~filer
        cols = np.flatnonzero(rows['kind'].isin(kinds).to_numpy())
        if not len(cols):   # a year with no such cells (2022 has no CPS non-filer values) leaves the batch as it is
            common.say(f'prescaling {batch}: the year has no targets of kinds {kinds}; left as scaled')
            continue
        assert all(averages[j] is None for j in cols), f'prescaling {batch}: an average among its cells'
        assert not (F[~rec][:, cols] != 0).any(), f'prescaling {batch}: a cell that records outside the batch reach'
        Fb, Tb, seb, idb = F[rec][:, cols], T[cols], se[cols], identity[cols]
        labels = (rows['kind'] + ' | ' + rows['group']).to_numpy()[cols].tolist()
        rho = importance(rows['kind'].to_numpy()[cols])
        if batch == 'filers':
            # The totals of joint and of non-joint returns, as identities.
            extra = [((married[rec] == m).astype(float), TG.returns(soi, Y, '1.2', s), f'filers | {s} returns')
                     for m, s in [(1, 'joint'), (0, 'nonjoint')]]
            Fb = np.column_stack([Fb] + [c for c, _, _ in extra])
            Tb = np.append(Tb, [v for _, v, _ in extra])
            seb, idb = np.append(seb, [0.0, 0.0]), np.append(idb, [True, True])
            labels += [lab for _, _, lab in extra]
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
    """The section of summary.md on the demographic prescaling."""
    if not prescaled:
        return []
    lines = ['## The demographic prescaling (before the tilt)', '']
    head = pd.DataFrame([{k: v for k, v in p.items() if k != 'cells'} for p in prescaled]).set_index('batch').T
    lines += [head.to_markdown(floatfmt=',.3f'), '']
    for p in prescaled:
        c = p['cells'].assign(**{'before - target (%)': lambda d: 100 * (d.before / d.target - 1)})
        lines += [f"### {p['batch']}: cells (people or returns)", '', c.to_markdown(index=False, floatfmt=',.1f'), '']
    return lines


# ---- The targets ----

def list_targets(g19, w0, fixed, filer, prescale, soi):
    """Lists the year's targets other than those on the calculator's columns, and checks that each has a standard
    error or is an identity. With start_scaling weights 'demographic', applies the demographic prescaling, which needs
    the conditions of its own cells only. Otherwise writes every listed target as a condition on the weights.

    Returns the targets, their conditions (None after the prescaling; they are built after the taxes), the starting
    weights after any prescaling, the starting weights before it, and the prescaling's summaries."""
    if prescale == 'persons':
        from scale import projection_steps as PS
        PS.STATE['start'] = (g19, w0, fixed)   # the projection targets (projection_steps.outside_rows) read it
    rows = TLS.expand(Y, start=g19, which='without calculator')
    TLS.check_nonfiler_bands(g19, rows)   # every non-filer head and spouse is in exactly one non-filer people band
    missing = rows.loc[~rows['identity'] & ~(rows['se'] > 0), ['kind', 'group', 'target']]
    assert missing.empty, f'targets with no standard error:\n{missing.to_string()}'
    prescaled, w_status = [], w0.copy()
    F = T = averages = None
    if prescale == 'demographic':
        sub = rows[rows['kind'].isin(PRESCALE_KINDS)].reset_index(drop=True)
        Fs, Ts, avs = TLS.conditions(g19, sub, w0)
        w0, prescaled = prescale_demographic(w0, Fs, Ts, sub['se'].to_numpy(float), sub['identity'].to_numpy(bool), avs,
                                             sub, filer, g19['married'].to_numpy(), soi, fixed)
        del Fs
    else:
        F, T, averages = TLS.conditions(g19, rows, w0)
    return rows, F, T, averages, w0, w_status, prescaled


def add_calculator_targets(g19, rows, F, T, averages, w0):
    """Adds the targets on the calculator's columns and writes the full list of conditions on the weights. The
    conditions are written again when there are calculator targets, when the prescaling ran, or when a target is an
    income rank group: those groups are set on the file's starting weights, which are only final after the
    prescaling. Writes targets.csv. Returns the targets, their conditions, standard errors and identity flags, and the
    calculator targets."""
    calc_rows = TLS.expand(Y, start=g19, which='calculator')
    common.say(f'calculator targets: {len(calc_rows)}')
    ranked = rows['rank_variable'].fillna('').astype(str).ne('').any()
    if ranked or len(calc_rows) or F is None:
        if len(calc_rows):
            rows = pd.concat([rows, calc_rows], ignore_index=True)
        F, T, averages = TLS.conditions(g19, rows, w0)
    se, identity = rows['se'].to_numpy(float), rows['identity'].to_numpy(bool)
    if ranked:
        common.say('income rank lines on the file (starting weights): '
                   + '; '.join(f'{k[0]} {k[1]} p{k[2]:g}: {v:,.0f}' for k, v in TLS.FILE_RANK_LINES.items()))
    t43 = rows[(rows['kind'] == 'soi 4.3') & (rows['target'] == 'returns')]
    if len(t43):   # Table 4.3's groups are at SOI's published AGI cutoffs
        common.say('Table 4.3 groups at SOI\'s published AGI cutoffs (returns on the starting weights, SOI): '
                   + '; '.join(f'{r.group} ${r.agi_lo:,.0f} ({float(w0 @ F[:, i]) / 1e3:,.1f} thousand, '
                               f'SOI {r.value / 1e3:,.1f} thousand)' for i, r in t43.iterrows()))
    TLS.write(rows, common.TILT, Y)
    return rows, F, T, averages, se, identity, calc_rows


def checks_before_solve(rows, F, T, se, identity, averages, w0, fixed):
    """Two checks before the solve. They only report. When some records are held fixed, the first finds, for each
    target, what is left after the fixed records' contribution and whether the other records alone can reach it
    (top_feasibility.csv). The second is the precheck (precheck.py). Returns the first check's table (None when no
    record is fixed) and the precheck's lines."""
    feas = None
    if fixed.any():
        feas = pd.concat([rows[['kind', 'group', 'target', 'value']].reset_index(drop=True),
                          SV.fixed_feasibility(w0, F, T, se, identity, averages, fixed).drop(columns='target')],
                         axis=1)
        feas.to_csv(f'{common.TILT}/top_feasibility.csv', index=False)
        common.say(f"feasibility with the fixed records: {int(feas['billionaires alone exceed'].sum())} targets the "
                   f"fixed records alone exceed, {int((~feas['reachable by the others']).sum())} the others cannot reach")
    _, precheck_lines = PRE.run(rows, F, w0, averages, rows['value'].to_numpy(float), se, identity, common.TILT, S['precheck'])
    common.say('\n'.join(precheck_lines))
    return feas, precheck_lines


# ---- The solve ----

def importance(kinds):
    """Each target's importance in the cost: 1 for the kinds in settings tilt.reweighting.importance_one_kinds (SOI's
    published figures, the W-2 statistics and the calculator's columns against SOI's), and 1 / other_importance_divisor
    for every other kind."""
    cfg = S['reweighting']
    return np.where(pd.Series(kinds).isin(set(cfg['importance_one_kinds'])).to_numpy(), 1.0,
                    1.0 / float(cfg['other_importance_divisor']))


AGI_GROUP_NAMES = ['$0 or less', '$0-50k', '$50-100k', '$100-200k', '$200-500k', '$500k-1m', '$1-10m', '$10m and over', 'non-filers']


def agi_groups(agi, filer):
    """Each record's group for the soft cap and the weight-factor table, numbered as in AGI_GROUP_NAMES: filers by the
    calculator's AGI, and non-filers in a group of their own."""
    assert np.isfinite(agi).all(), 'agi_groups: a missing calculator AGI'
    edges = [50e3, 100e3, 200e3, 500e3, 1e6, 10e6]
    g = np.where(agi <= 0, 0, 1 + np.searchsorted(edges, agi, side='right'))
    return np.where(filer, g, 8)


def soft_cap(w0, grp, fixed, folder):
    """Each record's cap U for the solver's soft cap: settings tilt.soft_cap.k times the mean starting weight of its
    AGI group. The mean is over the group's records with a positive starting weight, leaving out the fixed records. The
    cost then gains 0.5 beta ((w - U)+ / U)^2 for each record, where (x)+ is x when positive and zero otherwise. Writes
    soft_cap_groups.csv and soft_cap_upper.npy. Returns the caps and beta."""
    soft = S['soft_cap']
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
    tab.to_csv(f'{folder}/soft_cap_groups.csv', index=False)
    np.save(f'{folder}/soft_cap_upper.npy', upper)
    common.say(f"soft cap k = {soft['k']:g}, beta = {soft['beta']:g}:\n" + tab.to_string(index=False))
    return upper, float(soft['beta'])


def solver_settings(g19, filer, w0, fixed, rows):
    """The solver's settings for the main solve: the soft caps, beta and each target's importance. Writes
    importance.csv. Returns the settings, each record's AGI group, the caps, beta and the importances."""
    agi_c = g19['calc_agi'].to_numpy(float)
    grp = agi_groups(agi_c, filer)
    upper, beta = soft_cap(w0, grp, fixed, common.TILT)
    cfg = S['reweighting']
    rho = importance(rows['kind'].to_numpy())
    S_main = {**S, 'solver_soft_upper': upper, 'solver_soft_beta': beta, 'solver_importance': rho}
    pd.DataFrame({'kind': rows['kind'], 'group': rows['group'], 'target': rows['target'], 'importance': rho}).to_csv(
        f'{common.TILT}/importance.csv', index=False)
    common.say(f"one cost: tau {cfg['tau']:g}, importance 1 for kinds {sorted(cfg['importance_one_kinds'])} ({int((rho == 1).sum())} targets), "
               f"{1 / float(cfg['other_importance_divisor']):.4f} for the other {int((rho != 1).sum())}; budget "
               f"{cfg.get('newton_budget', 150)} Newton steps")
    return S_main, grp, upper, beta, rho


def solve_inputs(P):
    """The solve's inputs as the integrated solve of the state weights (state_joint.py) reads them: the records, their
    starting weights, every target's condition, standard error and importance, the fixed records, the soft caps and
    the solver's settings."""
    return {'record_id': P['g19']['record_id'].to_numpy(), 'w0': P['w0'], 'F': P['F'], 'T': P['T'], 'se': P['se'],
            'identity': P['identity'], 'averages': P['averages'], 'rho': P['rho'], 'fixed': P['fixed'],
            'soft_upper': P['upper'], 'soft_beta': P['beta'], 'rows': P['rows'], 'reweighting': S['reweighting']}


def run_solve(P, t_start):
    """Runs the main solve and returns the new weights and the solver's description of the solve. When the
    environment variable REBUILD_SAVE_SOLVE names a file, the solve's inputs are written to it beforehand, and the
    multipliers and new weights beside it afterwards."""
    w0, fixed = P['w0'], P['fixed']
    save_solve = os.environ.get('REBUILD_SAVE_SOLVE')
    if save_solve:
        with open(save_solve, 'wb') as fh:
            pickle.dump(solve_inputs(P), fh, protocol=4)
    t_solve = time.time()
    w1, info = SV.solve(w0, P['F'], P['T'], P['se'], P['identity'], P['averages'], P['S_main'], fixed=fixed)
    info['solve_seconds'] = time.time() - t_solve
    if save_solve:
        np.savez(save_solve + '.answer.npz', multipliers=info['multipliers'], w1=w1, newton_steps=info['newton_steps'],
                 converged=info['converged'])
    common.say(f"the whole tilt before the solve: {(t_solve - t_start) / 60:.1f} minutes (the start, the prescaling, the taxes, the targets)")
    common.say(f"the solve: {info['solve_seconds'] / 60:.1f} minutes, {info['newton_steps']} Newton steps in "
               f"{info['newton_calls']} calls ({info['newton_capped_calls']} ended without meeting the stopping size)")
    assert np.array_equal(w1[fixed], w0[fixed]), 'a fixed weight moved'
    return w1, info


# ---- Outputs and the summary ----

def weight_factor_table(w0, w1, grp):
    """The weight factor (final weight over starting weight) among the records with a starting weight, overall and by
    AGI group: its percentiles, the number of records raised more than 10 and 100 times or cut below a tenth, and the
    largest final weight."""
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


def top_share(x, w, p):
    """The share of the weighted total of x held by the top p (a fraction) of the weight, ranked by x. A record that
    straddles the line counts in part."""
    o = np.argsort(-x, kind='stable')
    xs, ws = x[o], w[o]
    cum = np.cumsum(ws)
    take = np.clip(p * w.sum() - (cum - ws), 0, ws)
    return (xs * take).sum() / (x * w).sum()


def net_worth_table(files):
    """A table of net worth for each file in files, a dict of name: (records, net worth, weights): the number of
    units, the total, the median, the top 1% share, and the mean in each wage group."""
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


def start_lines(spec, g19, prescale, n_nonfilers, nf_note, growth):
    """The section of summary.md on the start. For a projection year, projection_steps.write also writes its own
    files and returns its lines here."""
    lines = []
    if spec['mode'] == 'from_year':
        lines = [f"## The start: the {spec['year']} tilted file", '',
                 f"- tilt/{spec['run']}/file_{spec['year']}.parquet: {len(g19):,} records; its tilted weights "
                 f"scaled by start_scaling weights '{prescale}'; dollars '{spec['dollars']}'.",
                 f"- non-filers scaled to {n_nonfilers / 1e6:.2f} million"
                 f"{' (' + nf_note + ')' if nf_note else ''}.", '']
        if growth is not None:
            lines += [f"Growth factors, {spec['year']} to {Y}, by column:", '',
                      growth.to_markdown(index=False, floatfmt='.4f'), '']
    elif nf_note:
        lines = [f'- non-filers scaled to {n_nonfilers / 1e6:.2f} million ({nf_note}).', '']
    if prescale == 'persons':
        from scale import projection_steps as PS
        lines += PS.write(common.TILT)
    return lines


def start_outputs(P):
    """Writes the outputs that depend on the start alone: the top step's report, the non-microdata imputations'
    scalars (the projection years read them from their start's folder) and the SCF targets by wave. Returns the
    section of summary.md on the start."""
    from generate import top_records as TR
    TR.write_report(P['top'], common.TILT, Y)
    NMI.write_qbi_scalars(P['nmi_ctx'], common.TILT)
    if scf_targets.RESULTS:
        scf_targets.details().to_csv(f'{common.TILT}/scf_targets_by_wave.csv', index=False)
    return start_lines(P['spec'], P['g19'], P['spec']['weights'], P['n_nonfilers'], P['nf_note'], P['growth'])


def weight_outputs(g19, w0, w1, fit, grp, nmi_ctx, tax_ctx, calc_rows):
    """Writes the outputs on the final weights w1: weight_factors.csv, the non-microdata imputations' checks (reported
    only), the calculator targets' fit and the taxes summary. fit is the solver's fit table. Returns the weight-factor
    table and the tax lines for summary.md."""
    factors = weight_factor_table(w0, w1, grp)
    factors.to_csv(f'{common.TILT}/weight_factors.csv', index=False)
    NMI.write_checks(NMI.checks(g19, Y, w1, nmi_ctx), common.TILT, Y)
    # The calculator targets before and after the reweighting (whether targeted or not), and the totals.
    tfit = fit[fit['kind'] == TLS.CALCULATOR_KIND] if len(calc_rows) else TX.evaluate(g19, calc_rows, w0, w1)
    tfit.to_csv(f'{common.TILT}/taxes/taxes_fit.csv', index=False)
    tax_lines = TX.summary_lines(tax_ctx, g19, w0, w1)
    open(f'{common.TILT}/taxes/taxes_summary.md', 'w').write('\n'.join(tax_lines))
    return factors, tax_lines


def top_lines(g19, top, w0, w1, x, fixed, info, feas):
    """The section of summary.md on the top of the file."""
    bil = g19['billionaire'].to_numpy() == 1
    lines = ['## The top of the file (top_records.py)', ''] + [f'- {n}' for n in top['notes']] + [
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
        lines += [f"- targets the fixed records alone exceed: {int(feas['billionaires alone exceed'].sum())}; "
                  f"targets the others cannot reach on their own: {int((~feas['reachable by the others']).sum())} "
                  f"(top_feasibility.csv)"]
    lines += ['']
    return lines


def write_summary(g19, top, w0, w1, w_plain, w_status, x, n_targets, fixed, info, feas, fit, factors, prescaled, precheck_lines,
                  starts, tax_lines, nmi_ctx, scf_pooled):
    """Writes summary.md and prints it to the log. Its net worth table compares the SCF's own wave for the year (when
    there is one), the SCF waves used by the targets pooled together (when scf_pooled: the year has SCF targets), and
    the file before and after the reweighting."""
    scf_path = f'{common.DATA}/scf_units_{Y}.parquet'   # a year with no SCF wave has no column of its own
    u = pd.read_parquet(scf_path) if os.path.exists(scf_path) else None
    pooled = scf_targets.reference(Y) if scf_pooled else None
    nw = net_worth_table({**({f'SCF {Y} wave': (u, u['net_worth'].to_numpy(float), u['weight'].to_numpy(float))}
                             if u is not None else {}),
                          **({'SCF, the targets\' waves pooled': pooled} if pooled is not None else {}),
                          'before the tilt': (g19, x, w0), 'after the tilt': (g19, x, w1)})
    tops = top_lines(g19, top, w0, w1, x, fixed, info, feas)
    summary = pd.Series({'targets': n_targets, **cost(w0, w1),
                         'KL move from the unscaled start (nats)': cost(w_plain, w1)['KL move (nats)'],
                         **({'effective sample kept from the start before the demographic prescaling (%)':
                             cost(w_status, w1)['effective sample kept (%)']} if prescaled else {})})
    by_kind = fit.groupby('kind').agg(targets=('target', 'size'), over_1_se=('over_1_se', 'sum'),
                                      largest_miss_in_se=('miss_in_se', lambda m: m.abs().max()),
                                      warnings=('flag', lambda f: (f == 'warning').sum()),
                                      stops=('flag', lambda f: (f == 'stop').sum()))
    text = [f'# The {Y} tilt', '', *SV.summary_lines(info, fit), '', summary.to_frame('').to_markdown(floatfmt=',.4f'),
            '', *precheck_lines, *tops, *starts, *prescale_lines(prescaled), *tax_lines,
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


# ---- The whole stage ----

def prepare():
    """Everything before the solve, in seven steps. Returns a dict of what the solve and the outputs need."""
    soi = TG.load()

    # 1. Build the start: the year's records in target-year dollars, with the top of the file added. For a projection
    #    year, also write each column's growth factors in full precision (package.py reads them).
    spec = TLS.start_spec(Y)
    g19, raw, imp, growth = START.year_start(spec)
    if growth is not None:
        growth.to_csv(f'{common.TILT}/start_growth.csv', index=False)
    g19, imp, given, fixed, top = top_of_file(g19, raw, imp, spec)
    del raw
    filer = g19['filer'].to_numpy() == 1
    prescale = spec['weights']

    # 2. Scale the starting weights to SOI's count of returns and the expected number of non-filers.
    w0, w_plain, n_nonfilers, nf_note = scale_start(g19, imp, given, filer, prescale, soi)

    # 3. List the targets that do not need the calculator, and apply the demographic prescaling if the year uses it.
    rows, F, T, averages, w0, w_status, prescaled = list_targets(g19, w0, fixed, filer, prescale, soi)

    # 4. Run the non-microdata imputations on the start with its prescaled weights. They add columns, except amt_nols,
    #    whose drawn value is kept as amt_nols_drawn. No target reads them. Then compute every record's taxes under
    #    the year's law.
    g19, nmi_ctx = NMI.impute(g19, Y, w0)
    g19, tax_ctx = TX.compute(g19, Y, w0, f'{common.TILT}/taxes')

    # 5. Add the targets on the calculator's columns and write every target as a condition on the weights.
    rows, F, T, averages, se, identity, calc_rows = add_calculator_targets(g19, rows, F, T, averages, w0)

    # 6. The checks before the solve. They only report; no target is dropped.
    feas, precheck_lines = checks_before_solve(rows, F, T, se, identity, averages, w0, fixed)

    # 7. Set the soft caps and the importances.
    S_main, grp, upper, beta, rho = solver_settings(g19, filer, w0, fixed, rows)
    return dict(g19=g19, w0=w0, F=F, T=T, se=se, identity=identity, averages=averages, fixed=fixed, rows=rows, upper=upper,
                beta=beta, rho=rho, S_main=S_main, grp=grp, top=top, prescaled=prescaled, precheck_lines=precheck_lines,
                feas=feas, nmi_ctx=nmi_ctx, tax_ctx=tax_ctx, calc_rows=calc_rows, spec=spec, n_nonfilers=n_nonfilers,
                nf_note=nf_note, growth=growth, w_plain=w_plain, w_status=w_status)


def solve_and_write(P, t_start):
    """The stage run whole: solve for the new weights, then write the file, the fit table, the outputs and
    summary.md."""
    g19, w0 = P['g19'], P['w0']
    w1, info = run_solve(P, t_start)
    starts = start_outputs(P)
    fit = SV.fit_table(w0, w1, P['F'], P['T'], P['se'], P['averages'], P['rows'], P['identity'])
    fit.to_csv(f'{common.TILT}/fit.csv', index=False)
    g19.assign(weight_start=w0, weight=w1).to_parquet(f'{common.TILT}/file_{Y}.parquet', index=False)
    factors, tax_lines = weight_outputs(g19, w0, w1, fit, P['grp'], P['nmi_ctx'], P['tax_ctx'], P['calc_rows'])
    write_summary(g19, P['top'], w0, w1, P['w_plain'], P['w_status'], g19['net_worth'].to_numpy(float), len(P['T']), P['fixed'],
                  info, P['feas'], fit, factors, P['prescaled'], P['precheck_lines'], starts, tax_lines, P['nmi_ctx'],
                  bool(scf_targets.RESULTS))


def write_inputs(P, t_start):
    """The `prepare` run of a past year: write the solve's inputs for the integrated solve, the file with every weight
    at its start, the outputs that depend on the start alone, and outputs_context.pkl, which holds what `finish`
    needs to write the outputs on the final weights."""
    g19, w0 = P['g19'], P['w0']
    with open(f'{common.OUT}/solve_inputs.pkl', 'wb') as fh:
        pickle.dump(solve_inputs(P), fh, protocol=4)
    starts = start_outputs(P)
    g19.assign(weight_start=w0, weight=w0).to_parquet(f'{common.TILT}/file_{Y}.parquet', index=False)
    context = dict(top={'notes': P['top']['notes']}, prescaled=P['prescaled'], precheck_lines=P['precheck_lines'], feas=P['feas'],
                   starts=starts, nmi_ctx=NMI.context_to_save(P['nmi_ctx']), tax_ctx=P['tax_ctx'], calc_rows=P['calc_rows'],
                   w_plain=P['w_plain'], w_status=P['w_status'], fixed=P['fixed'], n_targets=len(P['T']),
                   scf_pooled=bool(scf_targets.RESULTS))
    with open(f'{common.TILT}/outputs_context.pkl', 'wb') as fh:
        pickle.dump(context, fh, protocol=4)
    common.say(f'the solve\'s inputs written for the integrated solve: {len(w0):,} records, {len(P["T"]):,} targets; '
               f'{(time.time() - t_start) / 60:.1f} minutes (the start, the prescaling, the taxes, the targets)')


def finish():
    """The `finish` run of a past year, in the year's final folder: the outputs and summary.md on the final weights.
    Reads the file with the final weights, fit.csv and solve_info.json from the integrated solve, and
    outputs_context.pkl from the `prepare` run."""
    with open(f'{common.TILT}/outputs_context.pkl', 'rb') as fh:
        C = pickle.load(fh)
    g19 = pd.read_parquet(f'{common.TILT}/file_{Y}.parquet')
    w0, w1 = g19['weight_start'].to_numpy(float), g19['weight'].to_numpy(float)
    fit = pd.read_csv(f'{common.TILT}/fit.csv')
    info = json.load(open(f'{common.TILT}/solve_info.json'))
    filer = g19['filer'].to_numpy() == 1
    grp = agi_groups(g19['calc_agi'].to_numpy(float), filer)
    factors, tax_lines = weight_outputs(g19, w0, w1, fit, grp, C['nmi_ctx'], C['tax_ctx'], C['calc_rows'])
    write_summary(g19, C['top'], w0, w1, C['w_plain'], C['w_status'], g19['net_worth'].to_numpy(float), C['n_targets'], C['fixed'],
                  info, C['feas'], fit, factors, C['prescaled'], C['precheck_lines'], C['starts'], tax_lines, C['nmi_ctx'],
                  C['scf_pooled'])


def main():
    what = sys.argv[1] if len(sys.argv) > 1 else 'solve'
    assert what in ('solve', 'prepare', 'finish'), f'reweight.tilt takes no argument, or prepare or finish, not {what}'
    t_start = time.time()
    os.makedirs(common.TILT, exist_ok=True)
    if what == 'finish':
        finish()
        return
    P = prepare()
    if what == 'prepare':
        write_inputs(P, t_start)
    else:
        solve_and_write(P, t_start)


if __name__ == '__main__':
    main()
