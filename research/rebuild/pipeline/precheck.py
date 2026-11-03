"""A check that runs before the reweighting and only reports (1 November 2026, precheck decisions 54 and 55; the
measurements are in explore/2026-11-01/precheck/RESULTS.md). It drops nothing and changes no weight.

It flags a target when any of three things holds, each found without a solve:
  1. The average the cell's two targets require is outside what the records can give. For a cell with both a count and
     an amount target, the amount divided by the count is the average per return the targets require. The records in
     the cell (those counted by the count's column) have a smallest and a largest amount per return. If the required
     average is above the largest or below the smallest by more than one standard error (the cell's two relative
     standard errors in quadrature), no weights can meet both. Both targets of the cell are flagged.
  2. The target's relative standard error (standard error over value) is below min_relative_se (0.1% today), for a
     target that is not an identity: a standard error so narrow that the cost's pull on it is enormous and no solve in a few hundred steps can reach it.
  3. The start is more than max_start_gap_se (300 today) standard errors from the target, for a target that is not
     an identity.
The three numbers are settings tilt.precheck (min_relative_se, max_start_gap_se and min_excess_se, the line of rule 1 in
standard errors; 1 today), passed to run() as cfg. Returns the flags table (one row per flagged target, with the reason)
and the summary lines; writes precheck_flags.csv and precheck.md to the folder.
"""
import numpy as np
import pandas as pd


def _start_values(F, w0, averages):
    """Each target's value on the start weights: a total F.T @ w0; an average the group's weighted mean of v."""
    before = F.T @ w0
    for j, a in enumerate(averages):
        if a is not None:
            mem, v, _ = a
            mem = np.asarray(mem, bool)
            assert np.isfinite(np.asarray(v, float)[mem]).all(), f'target {j}: a missing value in an average'
            before[j] = float((w0[mem] * np.asarray(v, float)[mem]).sum() / w0[mem].sum())
    return before


def run(rows, F, w0, averages, value, se, identity, folder, cfg):
    rows = rows.reset_index(drop=True)
    value, se, identity = (np.asarray(a) for a in (value, se, identity))
    value, se = value.astype(float), se.astype(float)
    before = _start_values(F, w0, averages)
    free = ~identity.astype(bool)
    assert np.isfinite(se[free]).all() and (se[free] > 0).all(), 'precheck: a target without a positive standard error'
    t = rows[['kind', 'group', 'target']].copy()
    t['value'], t['se'], t['start'] = value, se, before
    t['relative_se'] = np.where(free, se / np.where(value == 0, np.nan, np.abs(value)), np.nan)
    t['start_gap_se'] = np.where(free, (before - value) / np.where(free, se, 1.0), 0.0)
    t['rule 1: required average outside the records'] = False
    t['rule 2: relative se below the floor'] = free & (t['relative_se'] < cfg['min_relative_se'])
    t['rule 3: start gap above the line'] = free & (t['start_gap_se'].abs() > cfg['max_start_gap_se'])
    t['detail'] = ''
    # rule 1: cells with a count and an amount of the same variable
    cells = []
    if 'variable' in rows and 'entry' in rows:
        live = w0 > 0
        for (entry, group, var), idx in rows.groupby(['entry', 'group', 'variable']).groups.items():
            if not isinstance(var, str) or not var:
                continue
            st = {rows.statistic[i]: i for i in idx if rows.statistic[i] in ('count', 'amount')}
            if len(st) < 2 or len([i for i in idx if rows.statistic[i] in ('count', 'amount')]) != 2:
                continue
            ia, ic = st['amount'], st['count']
            if averages[ia] is not None or averages[ic] is not None or value[ic] == 0 or value[ia] == 0:
                continue
            cnt = F[:, ic]
            inside = (cnt != 0) & live
            if not inside.any():
                continue
            per_return = F[inside, ia] / cnt[inside]
            implied = value[ia] / value[ic]
            tol = float(np.hypot(se[ia] / abs(value[ia]), se[ic] / abs(value[ic])))
            lo, hi = float(per_return.min()), float(per_return.max())
            excess = max(implied - hi, lo - implied, 0.0) / abs(implied)
            cells.append({'entry': entry, 'group': group, 'variable': var, 'required average': implied,
                          'smallest per return': lo, 'largest per return': hi, 'excess (share of average)': excess,
                          'tolerance (share)': tol, 'excess in se': excess / tol})
            if excess / tol > cfg['min_excess_se']:
                for i in (ia, ic):
                    t.loc[i, 'rule 1: required average outside the records'] = True
                    t.loc[i, 'detail'] = (f'required average {implied:,.2f}, records span {lo:,.2f} to {hi:,.2f}; '
                                          f'{excess / tol:.1f} se beyond')
    cells = pd.DataFrame(cells)
    rules = [c for c in t.columns if c.startswith('rule ')]
    flagged = t[t[rules].any(axis=1)].copy()
    flagged.to_csv(f'{folder}/precheck_flags.csv', index=False)
    cells.to_csv(f'{folder}/precheck_cells.csv', index=False)
    lines = ['## Check before the reweighting (precheck.py; reports only, nothing dropped)', '',
             f'- {len(t)} targets ({int(free.sum())} not identities); {len(cells)} count-and-amount cells checked.',
             f'- Rule 1, required average outside the records by more than {cfg["min_excess_se"]:g} se: '
             f'{int(t[rules[0]].sum())} targets.',
             f'- Rule 2, relative standard error below {100 * cfg["min_relative_se"]:g}%: {int(t[rules[1]].sum())} targets.',
             f'- Rule 3, start more than {cfg["max_start_gap_se"]:g} se from the target: {int(t[rules[2]].sum())} targets.', '']
    if len(flagged):
        show = flagged[['kind', 'group', 'target', 'value', 'se', 'start_gap_se', 'detail'] + rules].copy()
        show['rules'] = show[rules].apply(lambda r: ','.join(c[5] for c, f in zip(rules, r) if f), axis=1)
        lines += [show.drop(columns=rules).to_markdown(index=False, floatfmt=',.4g'), '']
    open(f'{folder}/precheck.md', 'w').write('\n'.join(lines))
    return flagged, lines
