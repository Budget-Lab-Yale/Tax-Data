"""The reweighting solver (TARGETS.md section S6, which replaces section 5's "hit exactly") and the fit table with
its thin-cell measures.

The rule.
  - Each target that is not an identity is met anywhere within its range, its value plus or minus k standard
    errors (k = range_in_se, 1 by default; or one k per target, range_in_se_by_target, which tilt.py builds from
    settings range_in_se_by_kind), at no cost inside the range. Among the reweightings that meet every
    range, the one closest to the starting weights is chosen: the smallest move in Kullback-Leibler divergence. So
    the starting weights decide where inside its range each target lands. A target the start already puts inside
    its range is not pulled anywhere; it moves only as far as the other targets' moves carry it, and only to its
    edge if they would carry it out.
  - When the ranges cannot all be met, a miss beyond a range costs (miss beyond the range / standard error)^2. The
    file lands where the total cost of those misses is smallest, and the starting weights only break ties.
  - Identities are always hit exactly.
  With k = 0 this is section 5's rule: every target exact when they can all be hit.

How it is solved. Every target is a linear condition on the weights, F.T @ w = T, and a range is |F.T @ w - T| <= b
with b = k x se. The reweighting closest to the start that meets a set of linear conditions and ranges has the form
w = w0 x exp(F @ lambda), one multiplier per target, and the multipliers minimize a convex function (the dual), by
Newton's method. A range adds b |lambda| to the dual: a target's multiplier is zero
while the target sits inside its range (it does not bind); when it binds, the target sits at an edge (the low edge
if the other targets or the start would leave it below, the high edge if above). |lambda| has a kink at zero, which
Newton's method cannot work with, so it is smoothed as sqrt(lambda^2 + eps^2), with eps shrunk step by step
(solver_range_smoothing), each solve starting from the last one's multipliers. Then the smoothing is removed (the
finish): the targets that bind are pinned exactly at their edges, the others left free, and the problem solved
exactly once more. The finish is kept if every free target sits inside its range and every pinned one is pushed
from the right side; if not, those targets move between the two sets and the finish is repeated (up to
solver_finish_rounds). If it never checks out, the smoothed answer is kept and the summary says so; it is inside
every range, with its targets off their exact landing points by an amount of the order of eps.
  1. Ranges first. Newton's method on the dual with every range (and identity) as a hard condition. If it converges,
     every range is met and that is the answer. The dual also tells when the ranges cannot be met: at any
     multipliers, one minus the dual objective is a floor under the Kullback-Leibler move of every reweighting that
     meets them. Once that floor passes solver_max_kl_nats (20 nats: as large a move as putting all the weight on
     one part in 500 million of the file), the problem is called infeasible; if Newton's method merely runs out of
     steps, it is called slow. Both go to step 2, which tells them apart for good.
  2. Standard errors decide. Each non-identity condition gets a penalty (miss beyond the range / se)^2 / (2c),
     solved as in step 1, for c = 1, 0.1, 0.01, ... (each solve starts from the last one's multipliers). As c
     shrinks the penalty outweighs the Kullback-Leibler term more and more, and the solution approaches the rule
     above: first the smallest total cost of misses beyond the ranges, then the smallest move. The loop stops when
     those misses, in standard errors, change by less than solver_miss_change_in_se from one c to the next. If the
     misses have gone to zero, the ranges could be met after all (step 1 was only slow); step 1 is run again from
     the last multipliers to finish the job, and the result is reported as feasible.
Why not a cleaner exact formulation. The misses the rule picks are unique (the cost is a strictly convex function
of where the file lands, beyond the ranges), so the answer is the closest reweighting that lands there. When the
conflict is between targets that are linear combinations of each other (two tables putting the same group at
different counts), that landing point is a generalized least-squares projection, which could be computed directly.
But that needs a decision about which near-dependencies among several hundred columns count as exact, and it misses the other way
targets conflict (a cell that can only be met by pushing some weights to zero). The shrinking penalty needs no such
decision and handles both.

Averages and shares. The standard error of an average is on the average itself, and so is its range: m - k se <= the
group's average of v <= m + k se, with the group's weight after the tilt. That is two linear conditions, sum over
the group of w (v - m - k se) <= 0 and sum over the group of w (v - m + k se) >= 0, so the range on an average is
exact, whatever the group's weight turns out to be. Only one of the two can bind at a time, so one multiplier serves
both: a member's term in the exponent is lambda (v - m) + |lambda| k se. The same conversion holds for the
penalty: the condition "the group's average of v is m" is written as sum over the group of w (v - m) = 0, whose miss
is the group's weight times the miss on the average. So the standard error used in the penalty is the average's
standard error times the group's weight AFTER the tilt, so that a miss of one standard error beyond the range costs
exactly 1. The group's weight after the tilt is not known in advance: when an average misses its range, the solve
is repeated at the last c (up to solver_average_rounds in all), each time with the group weights from the last,
until they stop changing. When no average misses its range, its standard error does not affect the answer and this
is skipped.
The solver writes average conditions this way itself, from averages[j] = (member, v, m), in place of F[:, j] and
T[j]: target_list's 'units' form (average_condition, units + sum over the group of w (v / m - 1) = units) holds
only when the total number of units is exact, and with standard errors on the IRS and non-filer counts it is not.

Thin cells (section 5). For each target, after the tilt: the effective number of records behind it,
(sum_i w_i |f_i|)^2 / sum_i (w_i f_i)^2 over the records with f_i != 0, where f_i is the record's contribution (its
column of F; for an average, the record's v within the group, which is what the group's total of v rests on); and
the largest weight factor, final over starting weight, among those records that hold at least 1% of the cell's
weighted total after the tilt (thin_factor_min_share; without it, one record moved far flags every large cell it
sits in, though none of them rests on it). A warning below 30 effective records
or above a factor of 10; a stop below 10 or above 100 (placeholders, settings thin_*). A stop does not stop the
run: the solver returns it and the summary says STOP at the top.
"""
import numpy as np
import pandas as pd


def _get(settings, name, default):
    return default if settings is None else settings.get(name, default)


def _range_k(settings, m):
    """Each target's range in standard errors: settings range_in_se (one number, default 1); or, where tilt.py has
    set range_in_se_by_target (one number per target, from settings range_in_se_by_kind), that array. Returns the
    number, or the array of m numbers."""
    v = _get(settings, 'range_in_se_by_target', None)
    if v is None:
        k = float(_get(settings, 'range_in_se', 1.0))
        assert k >= 0, 'range_in_se must be 0 or more'
        return k
    k = np.asarray(v, float)
    assert k.shape == (m,) and np.isfinite(k).all() and (k >= 0).all(), 'range_in_se_by_target: m numbers, 0 or more'
    return k


def _range_name(k):
    """'1' for one number (or an array of one value); else each value with its count of targets."""
    u = np.unique(np.asarray(k, float))
    if len(u) == 1:
        return f'{u[0]:g}'
    return ' / '.join(f'{x:g} ({int((np.asarray(k) == x).sum())} targets)' for x in u)


# ---- the conditions --------------------------------------------------------------------
def _conditions(w0, F, T, averages, offset=None, extra=None, size_of=None):
    """The linear conditions the solver works with: F and T as given, except that an average or share is rewritten
    as sum over its group of w (v - m) = 0. Also each condition's size (to put every condition on a scale of about
    1), and for averages the group (member) whose weight converts the standard error.
    With records held fixed (solve's fixed; 29 October), the conditions are on the other records only: T is then the
    target less the fixed records' contribution, offset[j] the right-hand side of an average's condition (minus the
    fixed members' sum of w (v - m)), extra[j] the fixed members' weight in an average's group, and size_of the
    full targets, so each condition keeps the size it has on the whole file."""
    G, Tl = F.copy(), T.copy()
    member = [None] * len(T)
    size = np.abs(T if size_of is None else size_of).astype(float)
    for j, a in enumerate(averages):
        if a is None:
            continue
        mem, v, m = a
        mem = np.asarray(mem, bool)
        v = np.asarray(v, float)
        assert np.isfinite(v[mem]).all(), f'target {j}: a missing value in an average'
        G[:, j] = mem * (v - m)
        Tl[j] = 0.0 if offset is None else offset[j]
        member[j] = mem
        size[j] = abs(m) * (w0[mem].sum() + (0.0 if extra is None else extra[j]))
    # a condition whose target is zero (an average of zero, a count of zero): sized by the starting weight it rests on
    zero = size == 0
    size[zero] = np.abs(G[:, zero]).T @ w0
    assert (size > 0).all(), f'targets {np.flatnonzero(size == 0).tolist()}: no record contributes to them'
    return G, Tl, size, member


def contributions(F, averages):
    """Each record's contribution to each target, for the thin-cell measures: F's column for a total; for an average,
    the record's v within the group (what the group's total of v rests on)."""
    C = np.array(F, float, copy=True)
    for j, a in enumerate(averages):
        if a is not None:
            mem, v, _ = a
            C[:, j] = np.asarray(mem, bool) * np.asarray(v, float)
    return C


def _group_weight(w, member, extra=None):
    """Each average's group weight under w (plus the fixed members' weight, extra); NaN for a total."""
    g = np.array([np.nan if m is None else w[m].sum() for m in member])
    return g if extra is None else g + extra


# ---- Newton's method on the dual ---------------------------------------------------------
def _newton(X, t, p, s2, lam, max_steps, tol, max_kl=None, b=None, K=None, eps=0.0):
    """Minimize the dual sum(p exp(X lam + K |lam|)) - lam . t + sum(s2 lam^2) / 2 + sum(b |lam|) from lam, with
    |lam| smoothed as sqrt(lam^2 + eps^2). s2 per condition: 0 exact, else the penalty's variance (the condition's
    miss beyond its range costs miss^2 / (2 s2)). b per condition: the half-width of a total's range on the solver's
    scale (0: no range). K: None, or (columns, records x those columns), the averages' ranges: a member's k se on the
    solver's scale. Without ranges (b all 0, K None) this is the plain exact or penalized dual, and eps is not used.
    Returns the multipliers, the new weights (as shares of the starting total) and how it ended: 'converged';
    'infeasible' (no penalty: the dual has fallen so low that any reweighting meeting every condition would move
    more than max_kl nats); 'stalled' (no step lowers the dual); 'out of steps'."""
    m = len(t)
    b = np.zeros(m) if b is None else b
    kc, Kv = K if K is not None else (None, None)
    ranged = kc is not None or (b > 0).any()
    assert not ranged or eps > 0, 'ranges need a smoothing eps > 0'

    def kink(lam):
        """|lam| smoothed, its slope and its curvature."""
        if not ranged:
            return np.abs(lam), np.zeros(m), np.zeros(m)
        r = np.sqrt(lam ** 2 + eps ** 2)
        return r, lam / r, eps ** 2 / r ** 3

    def dual(lam):
        a = kink(lam)[0]
        with np.errstate(over='ignore', invalid='ignore'):
            z = X @ lam if kc is None else X @ lam + Kv @ a[kc]
            e = p * np.exp(z)
            return e.sum() - lam @ t + 0.5 * (s2 * lam ** 2).sum() + (b * a).sum(), e

    def gradient(lam, e):
        slope = kink(lam)[1]
        g = X.T @ e - t + s2 * lam + b * slope
        if kc is not None:
            g[kc] += (Kv.T @ e) * slope[kc]
        return g

    def hessian(lam, e):
        _, slope, curve = kink(lam)
        Xe = X * e[:, None]
        H = X.T @ Xe + np.diag(s2 + b * curve)
        if kc is not None:
            # the exponent's column for an average is X + K x slope; its curvature adds K' e x curve on the diagonal
            Ke = Kv * e[:, None]
            A = (X.T @ Ke) * slope[kc][None, :]
            H[:, kc] += A
            H[kc, :] += A.T
            H[np.ix_(kc, kc)] += (Kv.T @ Ke) * np.outer(slope[kc], slope[kc]) + np.diag((Kv.T @ e) * curve[kc])
        return H

    D, e = dual(lam)
    if not np.isfinite(D):            # a warm start that overflows: start again from the starting weights
        lam = np.zeros_like(lam)
        D, e = dual(lam)
    best, stuck = np.inf, 0
    for _ in range(max_steps):
        g = gradient(lam, e)
        if g.size == 0:   # (30 October) no condition left to hold (every range met at the start): the start stands
            return lam, e, 'converged'
        size_g = np.abs(g).max()
        if size_g < tol:
            return lam, e, 'converged'
        # weak duality: every reweighting meeting the conditions moves at least 1 - D nats (generalized
        # Kullback-Leibler divergence); the smoothing only raises D, so the floor still holds
        if max_kl is not None and 1.0 - D > max_kl:
            return lam, e, 'infeasible'
        # rounding sets a floor under the gradient; once it stops falling for five steps, below 1e-7 (a miss of one
        # part in ten million of a target), that is as close as the arithmetic gets
        stuck = stuck + 1 if size_g > 0.5 * best else 0
        best = min(best, size_g)
        if stuck >= 5 and size_g < 1e-7:
            return lam, e, 'converged'
        H = hessian(lam, e)
        d = np.sqrt(np.diag(H)) + 1e-300      # each multiplier on its own scale before solving
        step = np.linalg.solve(H / d[:, None] / d[None, :] + 1e-12 * np.eye(len(t)), -g / d) / d
        a = 1.0
        while True:
            D_new, e_new = dual(lam + a * step)
            if np.isfinite(D_new) and D_new <= D + 1e-4 * a * (g @ step):
                break
            # near the answer the fall in the dual is below its rounding error, so the test above cannot see it;
            # there, a step is taken if the dual does not visibly rise and the gradient shrinks
            if np.isfinite(D_new) and D_new <= D + 1e-13 * max(1.0, abs(D)) and \
                    np.abs(gradient(lam + a * step, e_new)).max() < size_g:
                break
            a /= 2
            if a < 1e-12:
                # rounding can stop the line search right at the answer; say converged if the gradient is tiny
                return lam, e, 'converged' if np.abs(g).max() < 100 * tol else 'stalled'
        lam, D, e = lam + a * step, D_new, e_new
    g = gradient(lam, e)
    return lam, e, 'converged' if np.abs(g).max() < tol else 'out of steps'


def _residual_and_width(X, t, b, K, e):
    """Each condition's residual on the solver's scale (where it lands minus its target) and its range's half-width
    there: b for a total; for an average, k se times the group's weight under e."""
    r = X.T @ e - t
    width = b.copy()
    if K is not None:
        width[K[0]] += K[1].T @ e
    return r, width


def _finish(X, t, p, s2, b, K, lam, free, s, eps, max_steps, tol, rounds):
    """Remove the smoothing. Each free target whose multiplier is clearly away from zero is pinned at the edge it
    binds at (side -1: the high edge, +1: the low edge; the side is the multiplier's sign), the others are left out,
    and the problem is solved exactly (with the penalty s2, if any, on the pinned ones). Kept if every target left
    out sits inside its range and every pinned one has a multiplier on its side; otherwise the targets that broke the
    check change sets and it is solved again. Returns the multipliers, the weights, whether it checked out, and a
    sentence."""
    m = len(t)
    kc = K[0] if K is not None else np.array([], int)
    kpos = {j: i for i, j in enumerate(kc)}
    side = np.where(free & (np.abs(lam) > 10 * eps), np.sign(lam), 0.0)
    for r in range(rounds):
        cols = np.flatnonzero(~free | (side != 0))
        Xp = X[:, cols].copy()
        for i, j in enumerate(cols):
            if side[j] != 0 and j in kpos:
                Xp[:, i] += side[j] * K[1][:, kpos[j]]
        tp = t[cols] - side[cols] * b[cols]
        lam_p, e, status = _newton(Xp, tp, p, s2[cols], lam[cols] * (side[cols] != 0) + lam[cols] * ~free[cols],
                                   max_steps, tol)
        if status != 'converged':
            return lam, None, False, f'finish without smoothing: {status} (round {r + 1}); the smoothed answer is kept'
        lam_new = np.zeros(m)
        lam_new[cols] = lam_p
        res, width = _residual_and_width(X, t, b, K, e)
        out = free & (side == 0) & (np.abs(res) > width + 1e-6 * s)        # left out, but outside its range
        wrong = (side != 0) & (lam_new * side <= 0)                          # pinned, but pulled from the wrong side
        if not out.any() and not wrong.any():
            return lam_new, e, True, (f'finish without smoothing: {int((side != 0).sum())} target(s) held by their '
                                      f'range (at its edge, or beyond it under the penalty), checked in {r + 1} '
                                      f'round(s)')
        side = np.where(out, -np.sign(res), np.where(wrong, 0.0, side))
        lam = lam_new
    return lam, None, False, f'finish without smoothing did not check out in {rounds} rounds; the smoothed answer is kept'


# ---- the solver --------------------------------------------------------------------------
def _solve(w0, F, T, se, identity, averages, settings=None, offset=None, extra=None, size_of=None):
    """The section S6 rule on the records given (solve without fixed records, or the free records of solve with
    them: offset, extra and size_of as in _conditions). Returns the new weights and the solve's own dict (feasible,
    converged, how, range_in_se, continuation, multipliers); the arguments as in solve."""
    w0 = np.asarray(w0, float)
    F = np.asarray(F, float)
    T = np.asarray(T, float)
    se = np.asarray(se, float)
    identity = np.asarray(identity, bool)
    n, m = F.shape
    averages = list(averages) if averages is not None else [None] * m
    assert w0.shape == (n,) and T.shape == se.shape == identity.shape == (m,) and len(averages) == m
    assert np.isfinite(w0).all() and (w0 >= 0).all() and w0.sum() > 0, 'starting weights must be finite and >= 0'
    assert np.isfinite(F).all() and np.isfinite(T).all(), 'a missing value in F or T'
    bad = ~identity & ~(np.isfinite(se) & (se > 0))
    assert not bad.any(), f'targets {np.flatnonzero(bad).tolist()}: not identities, but no positive standard error'

    k = _range_k(settings, m)            # one number, or one per target
    kv = np.broadcast_to(np.asarray(k, float), (m,))
    max_steps = _get(settings, 'solver_max_newton_steps', 200)
    tol = _get(settings, 'solver_gradient_tolerance', 1e-10)
    max_kl = _get(settings, 'solver_max_kl_nats', 20.0)
    c = _get(settings, 'solver_first_penalty', 1.0)
    shrink = _get(settings, 'solver_penalty_shrink', 10.0)
    c_min = _get(settings, 'solver_smallest_penalty', 1e-12)
    delta = _get(settings, 'solver_miss_change_in_se', 1e-4)
    rounds = _get(settings, 'solver_average_rounds', 5)
    round_change = _get(settings, 'solver_average_weight_change', 1e-6)
    zero_miss = _get(settings, 'solver_zero_miss_in_se', 1e-6)
    smoothing = list(_get(settings, 'solver_range_smoothing', [1e-2, 1e-4, 1e-6, 1e-8]))
    finish_rounds = _get(settings, 'solver_finish_rounds', 10)

    G, Tl, size, member = _conditions(w0, F, T, averages, offset, extra, size_of)
    W = w0.sum()
    p = w0 / W
    X = G * (W / size)                 # every condition on its own scale: the miss as a share of the target's size
    t = Tl / size
    is_avg = np.array([mm is not None for mm in member])
    free = ~identity

    # the ranges on the solver's scale: a total's half-width k se / size; an average's, k se for each member of the
    # group (times W / size), which the weights turn into k se times the group's weight
    ranged = bool((kv > 0).any())
    b = np.where(free & ~is_avg, k * np.where(free, se, 0.0) / size, 0.0)
    if extra is not None:   # an average's range on the fixed members' weight: a constant part of its half-width
        b = b + np.where(free & is_avg, kv * np.where(free, se, 0.0) * np.nan_to_num(extra) / size, 0.0)
    kc = np.flatnonzero(free & is_avg)
    K = (kc, np.column_stack([member[j] * (kv[j] * se[j] * W / size[j]) for j in kc])) if ranged and len(kc) else None
    if not ranged:
        b = np.zeros(m)
    eps_steps = smoothing if ranged else [0.0]
    eps_last = eps_steps[-1]

    def scaled_se(w):
        """The standard error of each condition on the solver's scale; on an average, times the group's weight w."""
        return np.where(is_avg, se * np.nan_to_num(_group_weight(w, member, extra)), se) / size * free

    def smoothed(lam, s2, kl):
        """Newton's method with the range smoothing shrunk step by step (no smoothing without ranges)."""
        status = 'converged'
        for eps in eps_steps:
            lam, e, status = _newton(X, t, p, s2, lam, max_steps, tol, kl, b, K, eps)
            if status != 'converged':
                break
        return lam, e, status

    def beyond(e, s):
        """Each free condition's miss beyond its range, in standard errors (0 inside the range)."""
        r, width = _residual_and_width(X, t, b, K, e)
        ex = np.sign(r) * np.maximum(np.abs(r) - width, 0.0)
        return np.where(free, ex / np.where(free, s, 1.0), 0.0)

    s0 = scaled_se(w0)
    name = f'ranges of {_range_name(k)} se' if ranged else 'exact'

    # 1. every range met (every target exact when k = 0)
    lam, e, status = smoothed(np.zeros(m), np.zeros(m), max_kl)
    how, steps = [f'{name}: {status}'], []
    feasible = converged = status == 'converged'
    if converged and ranged:
        lam_f, e_f, ok, note = _finish(X, t, p, np.zeros(m), b, K, lam, free, s0, eps_last, max_steps, tol,
                                       finish_rounds)
        how.append(note)
        if ok:
            lam, e = lam_f, e_f
    if not converged:
        # 2. standard errors decide: a shrinking penalty, then a few rounds for the averages' group weights
        lam = np.zeros(m)
        s = s0
        weights_start = _group_weight(w0, member, extra)
        last = None
        rounds = rounds if is_avg[free].any() else 1
        gave_line = _get(settings, 'gave_way_miss_in_se', 0.01)
        settled = False
        for r in range(rounds):
            while True:
                lam, e, status = smoothed(lam, c * s ** 2, None)
                miss = beyond(e, s)
                change = np.inf if last is None else np.abs(miss - last).max()
                steps.append({'round': r + 1, 'penalty c': c, 'newton': status,
                              'largest miss beyond range (se)': np.abs(miss).max(), 'change in misses (se)': change})
                last = miss
                if r == 0 and change < delta:
                    settled = True
                # later rounds re-solve once at the c where the misses settled: going smaller only costs precision
                if status != 'converged' or r > 0 or change < delta or c <= c_min:
                    break
                c /= shrink
            if status != 'converged' or r == rounds - 1:
                break
            # the averages' standard errors matter only where an average missed its range; then convert them again
            # with the group weights after the tilt, until those stop changing
            if not (np.abs(miss[is_avg & free]) > gave_line).any():
                break
            weights = _group_weight(W * e, member, extra)
            if np.nanmax(np.abs(weights / weights_start - 1)) < round_change:
                break
            s, weights_start = scaled_se(W * e), weights
        converged = status == 'converged' and settled
        how.append(f'penalty down to c = {c:.0e} over {steps[-1]["round"]} round(s): '
                   f'{"misses settled" if converged else "did not settle (" + status + ")"}')
        if converged and np.abs(last).max() < zero_miss:
            # the misses went to zero: every range can be met; finish as in step 1 from the last multipliers
            lam_x, e_x, status_x = smoothed(lam, np.zeros(m), None)
            how.append(f'misses went to zero; {name} again from there: {status_x}')
            if status_x == 'converged':
                lam, e, feasible = lam_x, e_x, True
                if ranged:
                    lam_f, e_f, ok, note = _finish(X, t, p, np.zeros(m), b, K, lam, free, s0, eps_last, max_steps,
                                                   tol, finish_rounds)
                    how.append(note)
                    if ok:
                        lam, e = lam_f, e_f
        elif converged and ranged:
            lam_f, e_f, ok, note = _finish(X, t, p, c * s ** 2, b, K, lam, free, s, eps_last, max_steps, tol,
                                           finish_rounds)
            how.append(note)
            if ok:
                lam, e = lam_f, e_f
    w1 = W * e
    return w1, {'feasible': bool(feasible), 'converged': bool(converged), 'how': '; '.join(how), 'range_in_se': k,
                'continuation': pd.DataFrame(steps), 'multipliers': lam}


def solve(w0, F, T, se, identity, averages, settings=None, fixed=None):
    """The section S6 rule. w0: starting weights (records). F: records x targets, so that target j is met when
    F[:, j] @ w == T[j]. se: each target's standard error in the published statistic's units (on the average itself
    for an average or share; ignored for identities); a target's range is T[j] +/- range_in_se x se[j]. identity:
    True where the target is exact by definition. averages[j]: (member, v, m) for 'the average of v over the group
    member is m' (F[:, j] and T[j] are then not used), else None.
    fixed (29 October; the billionaires, PLAN.md step 19): None, or a mask of records whose weights stay exactly at
    w0. Their contribution is a constant: a total's target becomes T less the fixed records' F.T @ w0; an average's
    condition, sum over the group of w (v - m) = 0, becomes sum over the free members = minus the fixed members' sum,
    and its range (k se times the group's weight) counts the fixed members' weight too. Only the other weights are
    solved for; every measure below is on the whole file.
    Returns the new weights and a dict: feasible (every non-identity target inside its range), converged, how (a
    sentence), range_in_se, the Kullback-Leibler move and effective sample kept (with fixed records, also the same
    two on the free records alone: kl_move_free, effective_sample_kept_free), and per-target arrays (achieved, miss,
    miss_in_se, range_low, range_high, inside_range, miss_beyond_range_in_se, at_range_edge, gave_way,
    effective_records, largest_weight_factor, records, flag), plus stop (any target flagged 'stop') and
    at_range_edge_count."""
    w0 = np.asarray(w0, float)
    F = np.asarray(F, float)
    T = np.asarray(T, float)
    m = len(T)
    averages = list(averages) if averages is not None else [None] * m
    if fixed is None or not np.asarray(fixed, bool).any():
        w1, info = _solve(w0, F, T, se, identity, averages, settings)
        fixed = np.zeros(len(w0), bool)
    else:
        fixed = np.asarray(fixed, bool)
        assert fixed.shape == w0.shape and (~fixed).any(), 'fixed: one flag per record, and some records free'
        assert np.isfinite(w0[fixed]).all() and (w0[fixed] >= 0).all(), 'fixed records: weights finite and >= 0'
        wf, free_ = w0[fixed], ~fixed
        T_free = T - F[fixed].T @ wf
        offset, extra = np.zeros(m), np.full(m, np.nan)
        av_free = [None] * m
        for j, a in enumerate(averages):
            if a is None:
                continue
            mem, v, mm = a
            mem, v = np.asarray(mem, bool), np.asarray(v, float)
            assert np.isfinite(v[mem & fixed]).all(), f'target {j}: a missing value in an average (fixed records)'
            offset[j] = -float((wf * mem[fixed] * (v[fixed] - mm)).sum())
            extra[j] = float(wf[mem[fixed]].sum())
            assert mem[free_].any(), f'target {j}: an average whose group has only fixed records'
            av_free[j] = (mem[free_], v[free_], mm)
        size_of = np.abs(T).astype(float)
        w1f, info = _solve(w0[free_], F[free_], T_free, se, identity, av_free, settings, offset, extra, size_of)
        w1 = w0.copy()
        w1[free_] = w1f
        mv = move(w0[free_], w1f)
        info.update({'kl_move_free': mv['kl_move'], 'effective_sample_kept_free': mv['effective_sample_kept'],
                     'fixed_records': int(fixed.sum()), 'fixed_weight': float(wf.sum())})
    assert np.array_equal(w1[fixed], w0[fixed]), 'a fixed weight moved'
    info.update(move(w0, w1))
    table = _per_target(w0, w1, F, T, se, identity, averages, settings)
    info.update({k_: table[k_].to_numpy() for k_ in table.columns})
    info['stop'] = bool((table['flag'] == 'stop').any())
    info['warnings'] = int((table['flag'] == 'warning').sum())
    info['stops'] = int((table['flag'] == 'stop').sum())
    info['at_range_edge_count'] = int(table['at_range_edge'].sum())
    return w1, info


def fixed_feasibility(w0, F, T, se, identity, averages, fixed, settings=None):
    """PLAN.md step 20, before the solve: for every target, what the fixed records (the billionaires) contribute, what
    is left for the others, and whether the others alone could reach it with nonnegative weights. A total: the free
    records' sum of w x F can be any number of the signs their contributions have (0 or more if every contribution is 0
    or more; any number if both signs occur; only 0 if none contributes), so the remainder's range (remainder plus or
    minus k standard errors; exactly the remainder for an identity) must meet that set. An average m over a group: the
    free members' sum of w (v - m) must equal minus the fixed members' sum, within the range's k se times the group's
    weight, which is checked the same way on the signs of v - m among the free members (with the fixed members' weight
    alone in the range, the least it can be). 'billionaires alone exceed': a total whose free contributions are all 0
    or more, which the fixed records alone put above its range. Each target on its own; whether they can all be met
    together is the solve's own answer (feasible). Returns one row per target."""
    w0, F, T, se = (np.asarray(a, float) for a in (w0, F, T, se))
    fixed = np.asarray(fixed, bool)
    identity = np.asarray(identity, bool)
    averages = list(averages) if averages is not None else [None] * len(T)
    k = np.broadcast_to(np.asarray(_range_k(settings, len(T)), float), (len(T),))
    half_total = np.where(identity, 0.0, k * se)
    free = ~fixed
    rows = []
    for j in range(len(T)):
        a = averages[j]
        if a is None:
            f = F[free, j]
            contrib = float(F[fixed, j] @ w0[fixed])
            rem, half = float(T[j] - contrib), float(half_total[j])
            fixed_w = np.nan
        else:
            mem, v, m = a
            mem, v = np.asarray(mem, bool), np.asarray(v, float)
            f = np.where(mem[free], v[free] - m, 0.0)
            contrib = float((w0[fixed] * mem[fixed] * (v[fixed] - m)).sum())
            fixed_w = float(w0[fixed][mem[fixed]].sum())
            rem, half = -contrib, float(half_total[j] * fixed_w)
        pos, neg = bool((f > 0).any()), bool((f < 0).any())
        lo, hi = rem - half, rem + half
        reach = (pos and neg) or (pos and hi >= 0) or (neg and lo <= 0) or (lo <= 0 <= hi)
        exceed = a is None and not neg and pos and hi < 0 and contrib > 0
        rows.append({'target': j, 'fixed contribution': contrib, 'fixed members\' weight': fixed_w,
                     'left for the others': rem, 'half-width of the range': half,
                     'others can add': 'both signs' if pos and neg else 'positive' if pos else 'negative' if neg else 'nothing',
                     'billionaires alone exceed': bool(exceed), 'reachable by the others': bool(reach)})
    return pd.DataFrame(rows)


def move(w0, w1):
    """How far the tilt moved the weights: Kullback-Leibler divergence of the new weights' shares from the starting
    shares, the share of the effective sample kept, and the smallest and largest weight factors."""
    p0, p1 = w0 / w0.sum(), w1 / w1.sum()
    pos = p1 > 0
    ess = lambda w: w.sum() ** 2 / (w ** 2).sum()
    on = w0 > 0
    f = w1[on] / w0[on]
    return {'kl_move': float(np.sum(p1[pos] * np.log(p1[pos] / p0[pos]))),
            'effective_sample_kept': float(ess(w1) / ess(w0)),
            'smallest_weight_factor': float(f.min()), 'largest_weight_factor_overall': float(f.max())}


# ---- per-target measures and the fit table -----------------------------------------------------
def thin_cells(w0, w1, C, settings=None):
    """Per target: the effective records behind it after the tilt; the largest weight factor among the records that
    hold at least thin_factor_min_share (1%) of the cell's weighted total after the tilt (one record moved far that
    holds a sliver of a large cell does not make the cell thin; its factor is still in the run's overall measures);
    the number of records behind it (contribution C != 0); the flag; and which test tripped it ('effective records',
    'weight factor', 'both', or 'no records')."""
    on = C != 0
    A = np.abs(C)
    wa = w1 @ A
    num = wa ** 2
    den = (w1 ** 2) @ (C ** 2)
    eff = np.where(den > 0, num / np.where(den > 0, den, 1.0), 0.0)
    factor = np.where(w0 > 0, w1 / np.where(w0 > 0, w0, 1.0), 0.0)
    material = (w1[:, None] * A) >= _get(settings, 'thin_factor_min_share', 0.01) * wa[None, :]
    material &= on
    largest = np.where(material.any(0), (material * factor[:, None]).max(0), np.nan)
    records = on.sum(0)
    few_w = eff < _get(settings, 'thin_warning_effective_records', 30)
    few_s = eff < _get(settings, 'thin_stop_effective_records', 10)
    far_w = np.nan_to_num(largest) > _get(settings, 'thin_warning_weight_factor', 10)
    far_s = np.nan_to_num(largest) > _get(settings, 'thin_stop_weight_factor', 100)
    none = records == 0
    flag = np.full(C.shape[1], '', dtype=object)
    flag[few_w | far_w] = 'warning'
    flag[few_s | far_s | none] = 'stop'
    # the reason names the test(s) behind the flag's level: for a stop, the stop tests; for a warning, the warning tests
    stop = flag == 'stop'
    few, far = np.where(stop, few_s, few_w), np.where(stop, far_s, far_w)
    reason = np.select([none, few & far, few, far], ['no records', 'both', 'effective records', 'weight factor'], '')
    reason = np.where(flag == '', '', reason)
    return eff, largest, records, flag, reason


def _per_target(w0, w1, F, T, se, identity, averages, settings, landed=None, G=None, Tl=None, size=None):
    """Per target: where the file lands before and after (the average itself for an average), the miss, the miss in
    standard errors, the range (value +/- range_in_se standard errors; the value itself for an identity), whether it
    landed inside, the miss beyond the range in standard errors (0 inside it), whether it sits at an edge of its
    range (within edge_tolerance_in_se, 0.001 se, of an edge and inside), whether it gave way (outside its range by
    more than gave_way_miss_in_se, 0.01 se; for an identity, missed at all), and the thin-cell measures."""
    if G is None:
        G, Tl, size, _ = _conditions(w0, F, T, averages)
    if landed is None:                  # where each solver condition lands (an average's: the group's weight x miss)
        landed = G.T @ w1
    value, before, after = T.astype(float).copy(), F.T @ w0, F.T @ w1
    for j, a in enumerate(averages):
        if a is not None:
            mem, v, mm = a
            mem = np.asarray(mem, bool)
            v = np.asarray(v, float)
            value[j] = mm
            before[j] = (w0[mem] * v[mem]).sum() / w0[mem].sum()
            after[j] = (w1[mem] * v[mem]).sum() / w1[mem].sum()
    miss = after - value
    se = np.asarray(se, float)
    exact = np.asarray(identity, bool) | ~(se > 0)
    k = _range_k(settings, len(T))       # one number, or one per target
    half = np.where(exact, 0.0, k * np.where(exact, 0.0, se))
    with np.errstate(divide='ignore', invalid='ignore'):
        miss_in_se = np.where(exact, np.nan, miss / se)
        beyond_in_se = np.where(exact, np.nan, np.sign(miss) * np.maximum(np.abs(miss) - half, 0.0) / se)
    gave = np.where(exact, np.abs(landed - Tl) / size > 1e-6,      # an identity missed: the solver failed
                    np.abs(np.nan_to_num(beyond_in_se)) > _get(settings, 'gave_way_miss_in_se', 0.01))
    edge_line = _get(settings, 'edge_tolerance_in_se', 1e-3)
    at_edge = ~exact & ~gave & (np.abs(np.nan_to_num(miss_in_se)) >= k - edge_line)
    eff, largest, records, flag, reason = thin_cells(w0, w1, contributions(F, averages), settings)
    return pd.DataFrame({'achieved': after, 'value_used': value, 'before_used': before, 'miss': miss,
                         'miss_in_se': miss_in_se, 'range_low': value - half, 'range_high': value + half,
                         'inside_range': ~gave, 'miss_beyond_range_in_se': beyond_in_se, 'at_range_edge': at_edge,
                         'gave_way': gave, 'effective_records': eff,
                         'largest_weight_factor': largest, 'records': records, 'flag': flag, 'flag_reason': reason})


def suggest_merges(cells, eff, flag):
    """For each flagged target, a merge that might fix it: a neighbouring cell of the same kind and variable. The
    candidates are the other rows of that kind and variable whose group label differs from this one in exactly one
    part, parts being separated by a comma and a space: 'age 26-34, married' has 'age under 26, married', 'age 35-44,
    married' and 'age 26-34, not married'; an AGI bracket has every other bracket. The neighbours are the candidates
    just before and after it in the rows' order, and the thinner of the two is suggested. Totals over everything
    ('all', or a group ending ', all') are not merge partners. Empty when there is no candidate. Only a suggestion:
    for nested cells (above the 99th percentile, above the 90th) it means widening the cell."""
    var = 'variable' if 'variable' in cells.columns else 'target'
    groups = cells['group'].astype(str).to_numpy()
    parts = [g.split(', ') for g in groups]     # comma and space: '$1,000,000 or more' is one part
    out = np.full(len(cells), '', dtype=object)
    overall = np.array([g == 'all' or g.endswith(', all') for g in groups])
    for _, idx in cells.groupby(['kind', var], sort=False).indices.items():
        idx = [i for i in idx if not overall[i]]
        for i in idx:
            if flag[i] == '':
                continue
            near = [k for k in idx if k == i or (len(parts[k]) == len(parts[i]) and
                                                 sum(a != b for a, b in zip(parts[k], parts[i])) == 1)]
            at = near.index(i)
            nb = near[max(at - 1, 0):at] + near[at + 1:at + 2]
            if nb:
                out[i] = groups[min(nb, key=lambda q: eff[q])]
    return out


def fit_table(w0, w1, F, T, se, averages, cells, settings=None, identity=None):
    """Today's fit.csv (group, target, value, kind, average, before, after, after / target; for a share or an average
    the value, before and after are the share or average itself) plus: se (in the published statistic's units),
    miss_in_se (empty for identities), range_low and range_high (value +/- range_in_se standard errors; the value
    for an identity), inside_range, miss_beyond_range_in_se (0 inside the range, empty for identities),
    at_range_edge (inside the range and within 0.001 se of an edge: the range binds), gave_way (outside the range),
    effective_records, largest_weight_factor (among the records holding at least 1% of the cell, see thin_cells),
    flag ('', 'warning', 'stop'), flag_reason, records (the number carrying the cell), effective_records_before (the
    same measure on the starting weights: a cell thin before the tilt is thin in the prior, not stretched by the
    tilt) and suggested_merge. Identities are the targets with se 0 unless identity is given."""
    w0, w1, F, T, se = (np.asarray(a, float) for a in (w0, w1, F, T, se))
    averages = list(averages) if averages is not None else [None] * len(T)
    identity = ~(se > 0) if identity is None else np.asarray(identity, bool)
    per = _per_target(w0, w1, F, T, se, identity, averages, settings)
    cells = cells.reset_index(drop=True)
    fit = pd.DataFrame({'group': cells['group'], 'target': cells['target'], 'value': per['value_used'],
                        'kind': cells['kind'],
                        'average': cells['average'] if 'average' in cells.columns else np.nan,
                        'before': per['before_used'], 'after': per['achieved']})
    fit['after / target'] = fit['after'] / fit['value']
    fit['se'] = np.where(identity, 0.0, se)
    for k in ('miss_in_se', 'range_low', 'range_high', 'inside_range', 'miss_beyond_range_in_se', 'at_range_edge',
              'gave_way', 'effective_records', 'largest_weight_factor', 'flag', 'flag_reason', 'records'):
        fit[k] = per[k].to_numpy()
    fit['effective_records_before'] = thin_cells(w0, w0, contributions(F, averages), settings)[0]   # the prior's own
    fit['suggested_merge'] = suggest_merges(cells, fit['effective_records'].to_numpy(), fit['flag'].to_numpy())
    return fit


def summary_lines(info, fit):
    """The solver's lines for the run summary: STOP first if any target is flagged 'stop'."""
    lines = []
    if info['stop']:
        lines += [f"**STOP: {info['stops']} target(s) rest on too few records or too large a weight factor "
                  f"(see the thin cells table).**", '']
    k = info.get('range_in_se', 1.0)
    within = f'inside its range of {_range_name(k)} standard error(s)' if np.any(np.asarray(k) > 0) else 'hit exactly'
    lines += [f"- Every non-identity target {within}: {'yes' if info['feasible'] else 'no'}"
              f"{'' if info['feasible'] else ' (standard errors decided which gave way)'}.",
              f"- Solver converged: {'yes' if info['converged'] or info['feasible'] else 'NO'} ({info['how']}).",
              f"- Targets outside their range: {int(fit['gave_way'].sum())}; at an edge of their range: "
              f"{int(fit['at_range_edge'].sum()) if 'at_range_edge' in fit else 'n/a'}; thin-cell warnings "
              f"{info['warnings']}, stops {info['stops']}.",
              f"- Kullback-Leibler move {info['kl_move']:.4f} nats; effective sample kept "
              f"{100 * info['effective_sample_kept']:.1f}%."]
    return lines
