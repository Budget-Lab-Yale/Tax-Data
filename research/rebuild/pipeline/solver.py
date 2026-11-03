"""The reweighting solver (the one-cost rule, 1 November 2026, decisions 57 and 68) and the fit table with its thin-cell
measures.

The rule. The new weights minimize one cost,

    sum over targets of 0.5 x importance x (miss / standard error)^2  +  tau x D(w),

where a target's miss is where the file lands less the target's value, and D(w) is the Kullback-Leibler move of the
weights from the starting weights, in nats (on the weights divided by the sum of the starting weights). Identities
(targets exact by definition, such as the total number of units) are met exactly. Every other target is pulled toward
its value with a force that grows with the miss measured in its own standard errors, and the pull is balanced against
the move of the weights. tau (settings tilt.reweighting.tau, 1,000) is the weight of the move against the misses: the
larger it is, the closer the file stays to the start and the larger the misses. A target's importance is 1 for the
published figures of the IRS and for the calculator's columns against them, and less for every other kind of target
(settings tilt.reweighting). No target is dropped and no target has a range: every one is in the cost, each at its own
scale.

Optionally a soft cap on each record's final weight: the cost gains 0.5 x beta x ((w_i - U_i)+ / U_i)^2 for each
record, where U_i is the record's cap (solver_soft_upper), nothing below U_i and a quadratic above it. beta = x^2
makes a record at twice its cap cost as much as an x standard-error miss on a target of importance 1.

How it is solved. Every target is a linear condition on the weights, F.T @ w = T. The weights closest to the start
that meet a set of linear conditions have the form w = w0 x exp(F @ lambda), one multiplier per condition, and with the
misses in the cost the multipliers minimize a smooth, strictly convex function (the dual) by Newton's method; the
miss on target j is then minus tau x s_j^2 x lambda_j / rho_j. An identity has a standard error of zero, so its
miss is zero. Newton's method is given a budget of steps in all (settings tilt.reweighting.newton_budget, 100; never
raised). With the soft cap, a record's weight solves a one-variable equation (_soft_weights) and the dual's curvature
changes above the cap.

Averages and shares. The standard error of an average is on the average itself. The condition "the group's average of
v is m" is written as sum over the group of w (v - m) = 0, whose miss is the group's weight times the miss on the
average, so the standard error used is the average's standard error times the group's weight AFTER the reweighting, so
that a miss of one standard error costs exactly 0.5. That weight is not known in advance: the solve is taken at the
start's group weight and, if any group moved by more than the settings' average_weight_change, once more at the first
answer's.
The solver writes average conditions this way itself, from averages[j] = (member, v, m), in place of F[:, j] and
T[j]. (target_list once had a 'units' form as well, units + sum over the group of w (v / m - 1) = units; it holds
only when the total number of units is exact, which with standard errors on the IRS and non-filer counts it is not,
and it was removed on 3 November 2026.)

Thin cells (section 5). For each target, after the reweighting: the effective number of records behind it,
(sum_i w_i |f_i|)^2 / sum_i (w_i f_i)^2 over the records with f_i != 0, where f_i is the record's contribution (its
column of F; for an average, the record's v within the group, which is what the group's total of v rests on); and
the largest weight factor, final over starting weight, among those records that hold at least 1% of the cell's
weighted total after the reweighting (THIN_FACTOR_MIN_SHARE; without it, one record moved far flags every large cell
it sits in, though none of them rests on it). A warning below 30 effective records or above a factor of 10; a stop
below 10 or above 100 (placeholders, THIN_WARNING and THIN_STOP). A stop does not stop the run: the solver returns it
and the summary says STOP at the top.
"""
import numpy as np
import pandas as pd


# What Newton's method did in the last solve. 'steps': Newton steps taken over every call of _newton in the solve (the
# budget is on this number); 'calls': the number of calls; 'capped_calls': calls that ended without meeting the
# stopping size. Set by _solve.
NEWTON = {'steps': 0, 'calls': 0, 'capped_calls': 0}


def _get(settings, name, default):
    return default if settings is None else settings.get(name, default)


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


# ---- the soft cap's record term (1 November, John) -----------------------------------------
def _soft_weights(z, p, c, U):
    """For each record the weight share e that solves log(e / p) + c (e - U)+ = z (c = beta / (tau U^2), U the cap in shares of
    the starting total). Below the cap it is p exp(z); above it the equation is w + log w = L with w = c e and
    L = log c + z + log p + c U, solved by Newton's method on w (increasing and concave in log w, so it converges from
    any start above the root's lower bound). Records with p = 0 get e = 0."""
    e = np.zeros_like(z)
    pos = p > 0
    zz = np.minimum(z, 700.0)
    e[pos] = p[pos] * np.exp(zz[pos])
    over = pos & (e > U)
    if over.any():
        L = np.log(c[over]) + z[over] + np.log(p[over]) + c[over] * U[over]
        w = np.maximum(L - np.log(np.maximum(L, 1.0)), 1e-300)      # a start close to the root for large L
        w = np.where(L < 1.0, np.exp(np.minimum(L, 50.0)) / (1.0 + np.exp(np.minimum(L, 50.0))), w)
        for _ in range(60):
            f = w + np.log(w) - L
            step = f / (1.0 + 1.0 / w)
            w = np.maximum(w - step, 1e-300)
            if np.abs(step / w).max() < 1e-15:
                break
        e[over] = np.maximum(w / c[over], U[over])
        # a record whose unconstrained weight is above the cap and whose root is not (rounding at the kink): keep it at the cap
    return e


def _soft_value(z, e, p, c, U):
    """The conjugate g(z) = z e - h(e), h(e) = e log(e / p) - e + p + (c / 2) ((e - U)+)^2, summed over records with p > 0."""
    pos = p > 0
    ee, pp = e[pos], p[pos]
    h = ee * np.log(np.where(ee > 0, ee / pp, 1.0)) - ee + pp + 0.5 * c[pos] * np.maximum(ee - U[pos], 0.0) ** 2   # 0 log 0 = 0
    return float((z[pos] * ee - h).sum())


# ---- Newton's method on the dual ---------------------------------------------------------
def _newton(X, t, p, s2, lam, max_steps, tol, soft=None):
    """Minimize the dual sum(p exp(X lam)) - lam . t + sum(s2 lam^2) / 2 from lam. s2 per condition: 0 for an identity
    (met exactly), else the misses' variance tau s^2 / rho (the condition's miss is -s2 lam on the solver's scale).
    soft: None, or (c, U), the soft cap's strength per record and cap in shares of the starting total (the record
    term is then the conjugate of the cost's, _soft_weights). Returns the multipliers, the new weights (as shares of
    the starting total) and how it ended: 'converged'; 'stalled' (no step lowers the dual); 'out of steps'."""
    m = len(t)
    if soft is not None:
        c_soft, U_soft = soft

    def dual(lam):
        with np.errstate(over='ignore', invalid='ignore'):
            z = X @ lam
            if soft is not None:
                e = _soft_weights(z, p, c_soft, U_soft)
                return _soft_value(z, e, p, c_soft, U_soft) - lam @ t + 0.5 * (s2 * lam ** 2).sum(), e
            e = p * np.exp(z)
            return e.sum() - lam @ t + 0.5 * (s2 * lam ** 2).sum(), e

    def gradient(lam, e):
        return X.T @ e - t + s2 * lam

    def hessian(lam, e):
        if soft is not None:         # the record term's curvature is e / (1 + c e) above the cap, e below it
            e = np.where(e > U_soft, e / (1.0 + c_soft * e), e)
        return X.T @ (X * e[:, None]) + np.diag(s2)

    D, e = dual(lam)
    if not np.isfinite(D):            # a warm start that overflows: start again from the starting weights
        lam = np.zeros_like(lam)
        D, e = dual(lam)
    best, stuck = np.inf, 0
    NEWTON['calls'] += 1
    for _ in range(max_steps):
        g = gradient(lam, e)
        if g.size == 0:   # no condition to hold: the start stands
            return lam, e, 'converged'
        size_g = np.abs(g).max()
        if size_g < tol:
            return lam, e, 'converged'
        # rounding sets a floor under the gradient; once it stops falling for five steps, below 1e-7 (a miss of one
        # part in ten million of a target), that is as close as the arithmetic gets
        stuck = stuck + 1 if size_g > 0.5 * best else 0
        best = min(best, size_g)
        if stuck >= 5 and size_g < 1e-7:
            return lam, e, 'converged'
        NEWTON['steps'] += 1
        H = hessian(lam, e)
        d = np.sqrt(np.diag(H)) + 1e-300      # each multiplier on its own scale before solving
        step = np.linalg.solve(H / d[:, None] / d[None, :] + 1e-12 * np.eye(m), -g / d) / d
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
    if np.abs(g).max() >= tol:
        NEWTON['capped_calls'] += 1
    return lam, e, 'converged' if np.abs(g).max() < tol else 'out of steps'


def _solve(w0, F, T, se, identity, averages, settings, offset=None, extra=None, size_of=None):
    """The one cost (module docstring) on the records given (solve without fixed records, or the free records of solve
    with them: offset, extra and size_of as in _conditions). With m_j the miss of target j, s_j its standard error and
    rho_j its importance:

        minimize   sum_j 0.5 rho_j (m_j / s_j)^2  +  tau x D(w),    identities exact,
        D(w) = sum_i [w_i log(w_i / w0_i) - w_i + w0_i]   on weights divided by sum(w0), so D is in nats.

    The answer is w_i = w0_i exp((X lambda)_i), with lambda the minimizer of the smooth, strictly convex function
        phi(lambda) = sum_i p_i exp((X lambda)_i) - t . lambda + (1/2) sum_j (tau s_j^2 / rho_j) lambda_j^2,
    whose gradient X'w - t + tau (s^2 / rho) lambda is zero when m_j = - tau s_j^2 lambda_j / rho_j (X, t and p are the
    conditions and weights on the solver's scale, as in _newton). An identity has s = 0, so its penalty term vanishes
    and the condition is met exactly. Settings read: 'reweighting' {tau, newton_budget (100), gradient_tolerance (1e-8),
    average_weight_change (1e-3)}; solver_importance (one positive number per target, 1 if absent); and, for the soft cap,
    solver_soft_upper (each record's cap U_i, a weight; one per record of this call) with solver_soft_beta. In the dual
    the soft cap's record term is the conjugate of h(e) = e log(e/p) - e + p + (c/2)((e - U)+)^2 with
    c = beta / (tau U^2) (shares of the starting total), solved record by record (_soft_weights).
    An average's standard error is on the average itself, so its penalty carries the group's weight after the
    reweighting: it is taken at the start's group weight, then once more at the first answer's if that moved any group
    by more than average_weight_change (and steps are left).
    The budget is the number of Newton steps in all, over every call."""
    cfg = _get(settings, 'reweighting', None)
    assert cfg is not None and float(cfg['tau']) > 0, "settings 'reweighting' with tau above 0"
    tau = float(cfg['tau'])
    budget = int(cfg.get('newton_budget', 100))
    tol = float(cfg.get('gradient_tolerance', 1e-8))
    avg_change = float(cfg.get('average_weight_change', 1e-3))
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
    rho = _get(settings, 'solver_importance', None)
    rho = np.ones(m) if rho is None else np.asarray(rho, float)
    assert rho.shape == (m,) and np.isfinite(rho).all() and (rho > 0).all(), 'solver_importance: m positive numbers'
    NEWTON.update({'steps': 0, 'calls': 0, 'capped_calls': 0})
    G, Tl, size, member = _conditions(w0, F, T, averages, offset, extra, size_of)
    W = w0.sum()
    p = w0 / W
    X = G * (W / size)
    t = Tl / size
    is_avg = np.array([mm is not None for mm in member])
    free = ~identity
    soft = None
    soft_u = _get(settings, 'solver_soft_upper', None)
    if soft_u is not None:
        soft_u = np.asarray(soft_u, float)
        beta = float(_get(settings, 'solver_soft_beta', 1.0))
        assert soft_u.shape == (n,) and (soft_u > 0).all() and beta > 0, 'solver_soft_upper: one positive weight per record; beta > 0'
        U_s = soft_u / W
        soft = (beta / (tau * U_s ** 2), U_s)

    def scaled_se(w):
        return np.where(is_avg, se * np.nan_to_num(_group_weight(w, member, extra)), se) / size * free

    def penalty_variance(s):
        return tau * s ** 2 / rho * free

    lam = np.zeros(m)
    s = scaled_se(w0)
    group0 = _group_weight(w0, member, extra)
    rounds, notes, status = 0, [], 'converged'
    while True:
        rounds += 1
        left = budget - NEWTON['steps']
        lam, e, status = _newton(X, t, p, penalty_variance(s), lam, max(left, 0), tol, soft=soft)
        if status != 'converged' or not is_avg[free].any() or rounds >= 3:
            break
        group = _group_weight(W * e, member, extra)
        change = float(np.nanmax(np.abs(group / group0 - 1)))
        notes.append(f'average groups moved by up to {100 * change:.2f}% (round {rounds})')
        if change < avg_change or NEWTON['steps'] >= budget:
            break
        s, group0 = scaled_se(W * e), group
    converged = status == 'converged'
    w1 = W * e
    g = X.T @ e - t + penalty_variance(s) * lam
    how = (f'one cost, tau = {tau:g}: {status} in {NEWTON["steps"]} Newton steps of a budget of {budget} '
           f'({rounds} round(s)); largest gradient {np.abs(g).max():.2e}' + ('; ' + '; '.join(notes) if notes else ''))
    return w1, {'converged': bool(converged), 'how': how, 'newton_steps': NEWTON['steps'], 'newton_calls': NEWTON['calls'],
                'newton_capped_calls': NEWTON['capped_calls'], 'multipliers': lam, 'soft_cap': soft is not None, 'tau': tau,
                'newton_budget': budget, 'status': status, 'largest_gradient': float(np.abs(g).max()), 'rho': rho}


def solve(w0, F, T, se, identity, averages, settings=None, fixed=None):
    """The one-cost reweighting (module docstring). w0: starting weights (records). F: records x targets, so that target j
    is met when F[:, j] @ w == T[j]. se: each target's standard error in the published statistic's units (on the
    average itself for an average or share; ignored for identities). identity: True where the target is exact by
    definition. averages[j]: (member, v, m) for 'the average of v over the group member is m' (F[:, j] and T[j] are
    then not used), else None. settings: see _solve.
    fixed (the billionaires): None, or a mask of records whose weights stay exactly at w0. Their contribution is a
    constant: a total's target becomes T less the fixed records' F.T @ w0; an average's condition, sum over the group
    of w (v - m) = 0, becomes sum over the free members = minus the fixed members' sum, and its standard error counts
    the fixed members' weight in the group's weight too. Only the other weights are solved for; every measure below is
    on the whole file.
    Returns the new weights and a dict: converged, how (a sentence), newton_steps, newton_calls, the Kullback-Leibler
    move and effective sample kept (with fixed records, also the same two on the free records alone: kl_move_free,
    effective_sample_kept_free), and per-target arrays (achieved, miss, miss_in_se, over_1_se, effective_records,
    largest_weight_factor, records, flag), plus stop (any target flagged 'stop')."""
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
        settings_free = settings
        sup = _get(settings, 'solver_soft_upper', None)
        if sup is not None:
            settings_free = {**settings, 'solver_soft_upper': np.asarray(sup, float)[free_]}
        w1f, info = _solve(w0[free_], F[free_], T_free, se, identity, av_free, settings_free, offset, extra, size_of)
        w1 = w0.copy()
        w1[free_] = w1f
        mv = move(w0[free_], w1f)
        info.update({'kl_move_free': mv['kl_move'], 'effective_sample_kept_free': mv['effective_sample_kept'],
                     'fixed_records': int(fixed.sum()), 'fixed_weight': float(wf.sum())})
    assert np.array_equal(w1[fixed], w0[fixed]), 'a fixed weight moved'
    info.update(move(w0, w1))
    table = _per_target(w0, w1, F, T, se, identity, averages)
    info.update({k_: table[k_].to_numpy() for k_ in table.columns})
    info['stop'] = bool((table['flag'] == 'stop').any())
    info['warnings'] = int((table['flag'] == 'warning').sum())
    info['stops'] = int((table['flag'] == 'stop').sum())
    return w1, info


def fixed_feasibility(w0, F, T, se, identity, averages, fixed):
    """Before the solve, for every target: what the fixed records (the billionaires) contribute, what is left for the
    others, and whether the others alone could reach it exactly with nonnegative weights. A total: the free records'
    sum of w x F can be any number of the signs their contributions have (0 or more if every contribution is 0 or
    more; any number if both signs occur; only 0 if none contributes), so the remainder must meet that set. An average m
    over a group: the free members' sum of w (v - m) must equal minus the fixed members' sum, which is checked the same
    way on the signs of v - m among the free members. 'billionaires alone exceed': a total whose free contributions are
    all 0 or more, which the fixed records alone put above its value. Each target on its own; whether they can all be
    met together is the solve's own answer. Returns one row per target."""
    w0, F, T, se = (np.asarray(a, float) for a in (w0, F, T, se))
    fixed = np.asarray(fixed, bool)
    averages = list(averages) if averages is not None else [None] * len(T)
    free = ~fixed
    rows = []
    for j in range(len(T)):
        a = averages[j]
        if a is None:
            f = F[free, j]
            contrib = float(F[fixed, j] @ w0[fixed])
            rem = float(T[j] - contrib)
            fixed_w = np.nan
        else:
            mem, v, m = a
            mem, v = np.asarray(mem, bool), np.asarray(v, float)
            f = np.where(mem[free], v[free] - m, 0.0)
            contrib = float((w0[fixed] * mem[fixed] * (v[fixed] - m)).sum())
            fixed_w = float(w0[fixed][mem[fixed]].sum())
            rem = -contrib
        pos, neg = bool((f > 0).any()), bool((f < 0).any())
        reach = (pos and neg) or (pos and rem >= 0) or (neg and rem <= 0) or rem == 0
        exceed = a is None and not neg and pos and rem < 0 and contrib > 0
        rows.append({'target': j, 'fixed contribution': contrib, 'fixed members\' weight': fixed_w,
                     'left for the others': rem,
                     'others can add': 'both signs' if pos and neg else 'positive' if pos else 'negative' if neg else 'nothing',
                     'billionaires alone exceed': bool(exceed), 'reachable by the others': bool(reach)})
    return pd.DataFrame(rows)


def effective_sample(w):
    """The effective number of records behind weights w: (sum of w)^2 / (sum of w^2)."""
    return w.sum() ** 2 / (w ** 2).sum()


def kl_move(w0, w1):
    """The Kullback-Leibler divergence, in nats, of the shares of weights w1 from the shares of w0."""
    p0, p1 = w0 / w0.sum(), w1 / w1.sum()
    pos = p1 > 0
    return float(np.sum(p1[pos] * np.log(p1[pos] / p0[pos])))


def move(w0, w1):
    """How far the tilt moved the weights: Kullback-Leibler divergence of the new weights' shares from the starting
    shares, the share of the effective sample kept, and the smallest and largest weight factors."""
    on = w0 > 0
    f = w1[on] / w0[on]
    return {'kl_move': kl_move(w0, w1),
            'effective_sample_kept': float(effective_sample(w1) / effective_sample(w0)),
            'smallest_weight_factor': float(f.min()), 'largest_weight_factor_overall': float(f.max())}


# ---- per-target measures and the fit table -----------------------------------------------------
# Thin cells (thin_cells): a cell is flagged 'warning' with fewer than THIN_WARNING[0] effective records or a weight
# factor above THIN_WARNING[1] among its material records, 'stop' with fewer than THIN_STOP[0] or above THIN_STOP[1];
# a record is material when it holds at least THIN_FACTOR_MIN_SHARE of the cell's weighted total after the tilt.
THIN_WARNING = (30, 10)
THIN_STOP = (10, 100)
THIN_FACTOR_MIN_SHARE = 0.01


def thin_cells(w0, w1, C):
    """Per target: the effective records behind it after the tilt; the largest weight factor among the records that
    hold at least THIN_FACTOR_MIN_SHARE (1%) of the cell's weighted total after the tilt (one record moved far that
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
    material = (w1[:, None] * A) >= THIN_FACTOR_MIN_SHARE * wa[None, :]
    material &= on
    largest = np.where(material.any(0), (material * factor[:, None]).max(0), np.nan)
    records = on.sum(0)
    few_w = eff < THIN_WARNING[0]
    few_s = eff < THIN_STOP[0]
    far_w = np.nan_to_num(largest) > THIN_WARNING[1]
    far_s = np.nan_to_num(largest) > THIN_STOP[1]
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


def _per_target(w0, w1, F, T, se, identity, averages):
    """Per target: where the file lands before and after (the average itself for an average), the miss, the miss in
    standard errors (empty for an identity), over_1_se (the miss is more than one standard error; for an identity, it is
    missed at all, which means the solver failed), and the thin-cell measures."""
    G, Tl, size, _ = _conditions(w0, F, T, averages)
    landed = G.T @ w1                    # where each solver condition lands (an average's: the group's weight x miss)
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
    with np.errstate(divide='ignore', invalid='ignore'):
        miss_in_se = np.where(exact, np.nan, miss / se)
    over = np.where(exact, np.abs(landed - Tl) / size > 1e-6,      # an identity missed: the solver failed
                    np.abs(np.nan_to_num(miss_in_se)) > 1.0)
    eff, largest, records, flag, reason = thin_cells(w0, w1, contributions(F, averages))
    return pd.DataFrame({'achieved': after, 'value_used': value, 'before_used': before, 'miss': miss,
                         'miss_in_se': miss_in_se, 'over_1_se': over, 'effective_records': eff,
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


def fit_table(w0, w1, F, T, se, averages, cells, identity=None):
    """Today's fit.csv (group, target, value, kind, average, before, after, after / target; for a share or an average
    the value, before and after are the share or average itself) plus: se (in the published statistic's units),
    miss_in_se (empty for identities), over_1_se (the miss is more than one standard error), effective_records,
    largest_weight_factor (among the records holding at least 1% of the cell, see thin_cells), flag ('', 'warning',
    'stop'), flag_reason, records (the number carrying the cell), effective_records_before (the same measure on the
    starting weights: a cell thin before the reweighting is thin in the prior, not stretched by it) and suggested_merge.
    Identities are the targets with se 0 unless identity is given."""
    w0, w1, F, T, se = (np.asarray(a, float) for a in (w0, w1, F, T, se))
    averages = list(averages) if averages is not None else [None] * len(T)
    identity = ~(se > 0) if identity is None else np.asarray(identity, bool)
    per = _per_target(w0, w1, F, T, se, identity, averages)
    cells = cells.reset_index(drop=True)
    fit = pd.DataFrame({'group': cells['group'], 'target': cells['target'], 'value': per['value_used'],
                        'kind': cells['kind'],
                        'average': cells['average'] if 'average' in cells.columns else np.nan,
                        'before': per['before_used'], 'after': per['achieved']})
    fit['after / target'] = fit['after'] / fit['value']
    fit['se'] = np.where(identity, 0.0, se)
    for k in ('miss_in_se', 'over_1_se', 'effective_records', 'largest_weight_factor', 'flag', 'flag_reason', 'records'):
        fit[k] = per[k].to_numpy()
    fit['effective_records_before'] = thin_cells(w0, w0, contributions(F, averages))[0]   # the prior's own
    fit['suggested_merge'] = suggest_merges(cells, fit['effective_records'].to_numpy(), fit['flag'].to_numpy())
    return fit


def summary_lines(info, fit):
    """The solver's lines for the run summary: STOP first if any target is flagged 'stop'."""
    lines = []
    if info['stop']:
        lines += [f"**STOP: {info['stops']} target(s) rest on too few records or too large a weight factor "
                  f"(see the thin cells table).**", '']
    miss = np.abs(np.nan_to_num(np.asarray(fit['miss_in_se'], float)))
    lines += [f"- One cost, tau = {info['tau']:g}: {'converged' if info['converged'] else 'NOT converged'} ({info['how']}).",
              f"- Largest miss {miss.max():.2f} standard errors; targets more than 0.1 se from their value: {int((miss > 0.1).sum())}, "
              f"more than 1 se: {int((miss > 1).sum())}, more than 2 se: {int((miss > 2).sum())}; thin-cell warnings "
              f"{info['warnings']}, stops {info['stops']}.",
              f"- Kullback-Leibler move {info['kl_move']:.4f} nats; effective sample kept "
              f"{100 * info['effective_sample_kept']:.1f}%.",
              f"- Newton steps: {info['newton_steps']} in {info['newton_calls']} calls, budget {info['newton_budget']}."
              + (' A soft cap on each weight (solver_soft_upper).' if info.get('soft_cap') else '')]
    return lines
