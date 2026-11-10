"""The reweighting solver, and the table that reports how well each target is met.

The solver chooses new weights for the records that make a cost as small as possible. The cost has three parts:

1. For each target, half its importance times the square of its miss in standard errors. The miss is the file's value
   under the new weights less the target's value. Importance is a number set per kind of target.
2. tau times the Kullback-Leibler divergence of the new weights from the starting weights. This measures how far the
   weights have moved: it is zero when no weight changes and grows as the weights move away from their start.
3. With the soft cap, a penalty on each record whose weight is above its cap U: half of beta times the square of the
   excess as a share of U. The soft cap discourages very large weights without forbidding them.

Identities (targets that hold by definition, such as a count that must be exact) are met exactly.

The solver does not search over the weights directly. Each target has a multiplier, a number that measures how strongly
the target pulls on the weights. Given the multipliers, each record's new weight is its starting weight times the
exponential of the sum, over targets, of the record's contribution to the target times the target's multiplier. The
best multipliers are those that minimize a smooth function of the multipliers alone, called the dual of the cost. There
is one multiplier per target, far fewer than there are records. The solver minimizes the dual by Newton's method: at
each step it approximates the dual by a quadratic function, built from the dual's gradient (its slope in each
direction) and its Hessian (the matrix of its second derivatives, which measures its curvature), and moves toward that
quadratic's minimum. It stops when the gradient is close to zero. _solve gives the formulas.

fit_table measures how close each target comes and how many records each target rests on. FEATURES.md,
"5. Reweighting", "The solve", describes the settings.

The table of conditions F comes in one of two forms. The year's full table is a sparse matrix, which stores only the
entries that are not zero (sparse_table.py). A small table, such as the prescaling's, is an ordinary numpy array. The
solver computes the same quantities from either form.
"""
import numpy as np
import pandas as pd

from reweight import sparse_table as ST


# A count of what Newton's method did in the last solve: the steps taken over every call (the step budget applies to
# this number), the number of calls, and the calls that ended before the gradient fell below the tolerance.
NEWTON = {'steps': 0, 'calls': 0, 'capped_calls': 0}


# ---- The conditions on the weights ----

def _conditions(w0, F, T, averages, offset=None, extra=None, size_of=None):
    """The targets written as linear conditions on the weights, in the form G' w = Tl.

    A total target j is the condition F[:, j] @ w = T[j], as given. An average or share target, "the average of v over
    the group is m", is rewritten as: the sum over the group of w (v - m) equals zero.

    Also returns each condition's size, which the solver divides by so that every condition is on a scale of about 1,
    and, for each average, the mask of its group. The group's weight converts the average's standard error into the
    units of its condition.

    When some records are held fixed (solve's fixed argument), T is the target less the fixed records' contribution,
    offset[j] is the right-hand side of average j's condition, extra[j] is the fixed records' weight in average j's
    group, and size_of holds the full targets. Each condition then keeps the size it has on the whole file."""
    sparse = ST.is_sparse(F)
    G, Tl = (None if sparse else F.copy()), T.copy()
    member = [None] * len(T)
    size = np.abs(T if size_of is None else size_of).astype(float)
    new = {}   # for a sparse table: the columns of the averages, put in place of F's below
    for j, a in enumerate(averages):
        if a is None:
            continue
        mem, v, m = a
        mem = np.asarray(mem, bool)
        v = np.asarray(v, float)
        assert np.isfinite(v[mem]).all(), f'target {j}: a missing value in an average'
        if sparse:
            new[j] = mem * (v - m)
        else:
            G[:, j] = mem * (v - m)
        Tl[j] = 0.0 if offset is None else offset[j]
        member[j] = mem
        size[j] = abs(m) * (w0[mem].sum() + (0.0 if extra is None else extra[j]))
    if sparse:
        G = ST.with_columns(F, new)
    # A condition whose target is zero (an average of zero, a count of zero) is sized by the starting weight it rests
    # on.
    zero = size == 0
    if sparse and zero.any():   # these few columns are written out in full
        size[zero] = np.abs(G[:, np.flatnonzero(zero)].toarray(order='F')).T @ w0
    elif not sparse:
        size[zero] = np.abs(G[:, zero]).T @ w0
    assert (size > 0).all(), f'targets {np.flatnonzero(size == 0).tolist()}: no record contributes to them'
    return G, Tl, size, member


def _group_weight(w, member, extra=None):
    """Each average's group weight under the weights w, plus the fixed records' weight in the group (extra). NaN for a
    total."""
    g = np.array([np.nan if m is None else w[m].sum() for m in member])
    return g if extra is None else g + extra


# ---- The soft cap ----
# Weights here are shares of the starting total: p is a record's starting share and e its new share. z is the record's
# sum of contribution times multiplier. Without a cap, e = p exp(z). With the soft cap, a record above its cap U is
# held below p exp(z) by the penalty. (x)+ means x when x is positive and zero otherwise.

def _soft_weights(z, p, c, U):
    """Each record's new weight share e under the soft cap: the e that solves log(e / p) + c (e - U)+ = z, with
    c = beta / (tau U^2) and U the cap as a share of the starting total.

    Below the cap the answer is e = p exp(z). Above it, write w = c e; the equation becomes w + log w = L, with
    L = log c + z + log p + c U. That equation is solved by Newton's method on w. The method converges from any start
    above the root's lower bound, because w + log w is increasing and concave in log w. Records with p = 0 get e = 0."""
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
        # A record whose uncapped weight is above the cap but whose root is below it (a rounding error at the cap)
        # stays at the cap.
        e[over] = np.maximum(w / c[over], U[over])
    return e


def _soft_value(z, e, p, c, U):
    """The record part of the dual under the soft cap: the sum over records with p > 0 of z e - h(e), where
    h(e) = e log(e / p) - e + p + (c / 2) ((e - U)+)^2 is the record's share of the cost. (This is the convex
    conjugate of h, evaluated at the e that _soft_weights gives.)"""
    pos = p > 0
    ee, pp = e[pos], p[pos]
    h = ee * np.log(np.where(ee > 0, ee / pp, 1.0)) - ee + pp + 0.5 * c[pos] * np.maximum(ee - U[pos], 0.0) ** 2   # 0 log 0 = 0
    return float((z[pos] * ee - h).sum())


# ---- Newton's method on the dual ----

def _newton(X, t, p, s2, lam, max_steps, tol, soft=None):
    """Minimizes the dual, sum(p exp(X lam)) - lam . t + sum(s2 lam^2) / 2, over the multipliers lam, starting from
    the lam given.

    s2 has one entry per condition: 0 for an identity, which is then met exactly, and tau s^2 / rho otherwise (s the
    standard error and rho the importance, on the solver's scale). At the answer each condition's miss is -s2 lam.
    soft is None, or (c, U): the soft cap's strength and cap for each record, with the cap as a share of the starting
    total. With the soft cap, the record part of the dual is _soft_value.

    Each step solves for the Newton step, then halves it until the dual falls enough (a line search). Returns the
    multipliers, the new weights as shares of the starting total, and how the method ended: 'converged', 'stalled' (no
    step lowers the dual) or 'out of steps'."""
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
        # A record above its soft cap enters the Hessian with e / (1 + c e) in place of its weight e.
        if soft is not None:
            e = np.where(e > U_soft, e / (1.0 + c_soft * e), e)
        # The Hessian is X' diag(e) X, plus s2 on the diagonal. The first part is computed as Y'Y, where Y is X with
        # each record's row multiplied by the square root of its e. The two are the same matrix. Numpy computes the
        # product of a matrix with itself faster than a general product.
        # For a sparse table the product is added up over blocks of records (sparse_table.weighted_square).
        if ST.is_sparse(X):
            H = ST.weighted_square(X, e)
        else:
            Y = X * np.sqrt(e)[:, None]
            H = Y.T @ Y
        H[np.diag_indices(m)] += s2
        return H

    D, e = dual(lam)
    if not np.isfinite(D):            # a warm start that overflows: start again from the starting weights
        lam = np.zeros_like(lam)
        D, e = dual(lam)
    best, stuck = np.inf, 0
    NEWTON['calls'] += 1
    for _ in range(max_steps):
        g = gradient(lam, e)
        if g.size == 0:   # no condition to meet: the starting weights stand
            return lam, e, 'converged'
        size_g = np.abs(g).max()
        if size_g < tol:
            return lam, e, 'converged'
        # Rounding error sets a floor under the gradient. The method stops when the gradient is below 1e-7 (a miss of
        # one part in ten million of a target) and has not halved in five steps.
        stuck = stuck + 1 if size_g > 0.5 * best else 0
        best = min(best, size_g)
        if stuck >= 5 and size_g < 1e-7:
            return lam, e, 'converged'
        NEWTON['steps'] += 1
        H = hessian(lam, e)
        d = np.sqrt(np.diag(H)) + 1e-300      # puts each multiplier on its own scale before solving
        step = np.linalg.solve(H / d[:, None] / d[None, :] + 1e-12 * np.eye(m), -g / d) / d
        a = 1.0
        while True:
            D_new, e_new = dual(lam + a * step)
            if np.isfinite(D_new) and D_new <= D + 1e-4 * a * (g @ step):
                break
            # Near the answer the fall in the dual is smaller than its rounding error, and the test above cannot
            # detect it. There, a step is taken if the dual does not visibly rise and the gradient shrinks.
            if np.isfinite(D_new) and D_new <= D + 1e-13 * max(1.0, abs(D)) and \
                    np.abs(gradient(lam + a * step, e_new)).max() < size_g:
                break
            a /= 2
            if a < 1e-12:
                # Rounding error can stop the line search at the answer. The method has converged if the gradient is
                # within 100 times the tolerance.
                return lam, e, 'converged' if np.abs(g).max() < 100 * tol else 'stalled'
        lam, D, e = lam + a * step, D_new, e_new
    g = gradient(lam, e)
    if np.abs(g).max() >= tol:
        NEWTON['capped_calls'] += 1
    return lam, e, 'converged' if np.abs(g).max() < tol else 'out of steps'


def _solve(w0, F, T, se, identity, averages, settings, offset=None, extra=None, size_of=None):
    """Minimizes the cost over the weights of the records given: all records, or only the free ones when solve holds
    some fixed (offset, extra and size_of as in _conditions). With m_j the miss of target j, s_j its standard error
    and rho_j its importance, the problem is

        minimize   sum_j 0.5 rho_j (m_j / s_j)^2  +  tau x D(w),    with the identities met exactly,
        D(w) = sum_i [w_i log(w_i / w0_i) - w_i + w0_i],

    on weights divided by sum(w0). D is then measured in nats. The answer is w_i = w0_i exp((X lambda)_i), where
    X holds each record's contributions to the conditions and lambda the multipliers. lambda minimizes the dual

        phi(lambda) = sum_i p_i exp((X lambda)_i) - t . lambda + (1/2) sum_j (tau s_j^2 / rho_j) lambda_j^2,

    which is smooth and has a single minimum. Its gradient is zero when m_j = - tau s_j^2 lambda_j / rho_j for every
    target (X, t and p on the solver's scale). An identity has s = 0 and so is met exactly. With the soft cap, the
    record part of the dual is _soft_value, and each record's weight is found on its own (_soft_weights).

    An average's standard error is a standard error of the average itself. Its penalty in the condition's units is
    therefore the standard error times the group's weight after the reweighting. The solve is first done with the
    starting group weights. If any group's weight then moved by more than average_weight_change, it is done again with
    the new group weights (at most three rounds). The Newton budget counts steps over all rounds."""
    cfg = settings['reweighting']
    tau = float(cfg['tau'])
    assert tau > 0, "settings 'reweighting' with tau above 0"
    budget = int(cfg.get('newton_budget', 150))
    tol = float(cfg.get('gradient_tolerance', 1e-8))
    avg_change = float(cfg.get('average_weight_change', 1e-3))
    w0 = np.asarray(w0, float)
    F = F if ST.is_sparse(F) else np.asarray(F, float)
    T = np.asarray(T, float)
    se = np.asarray(se, float)
    identity = np.asarray(identity, bool)
    n, m = F.shape
    assert w0.shape == (n,) and T.shape == se.shape == identity.shape == (m,) and len(averages) == m
    assert np.isfinite(w0).all() and (w0 >= 0).all() and w0.sum() > 0, 'starting weights must be finite and >= 0'
    assert ST.all_finite(F) and np.isfinite(T).all(), 'a missing value in F or T'
    bad = ~identity & ~(np.isfinite(se) & (se > 0))
    assert not bad.any(), f'targets {np.flatnonzero(bad).tolist()}: not identities, but no positive standard error'
    rho = settings.get('solver_importance')
    rho = np.ones(m) if rho is None else np.asarray(rho, float)
    assert rho.shape == (m,) and np.isfinite(rho).all() and (rho > 0).all(), 'solver_importance: m positive numbers'
    NEWTON.update({'steps': 0, 'calls': 0, 'capped_calls': 0})
    X, Tl, size, member = _conditions(w0, F, T, averages, offset, extra, size_of)
    del F   # the solve works on X from here; a table made for the free records alone is let go
    W = w0.sum()
    p = w0 / W
    # X is a copy of F made by _conditions, and it is scaled in place. A sparse table is then put in the layout by
    # row, which Newton's method works through in blocks of records.
    if ST.is_sparse(X):
        X = ST.scale_columns(X, W / size).tocsr()
    else:
        X *= W / size
    t = Tl / size
    is_avg = np.array([mm is not None for mm in member])
    free = ~identity
    soft = None
    soft_u = settings.get('solver_soft_upper')
    if soft_u is not None:
        soft_u = np.asarray(soft_u, float)
        beta = float(settings.get('solver_soft_beta', 1.0))
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


def solve(w0, F, T, se, identity, averages, settings, fixed=None):
    """The reweighting. Returns the new weights and a dict that describes the solve and how far the weights moved.

    - w0: the starting weights.
    - F: a matrix of records by targets, sparse or an ordinary array. Target j is met when F[:, j] @ w == T[j].
    - se: each target's standard error, in the units of the published statistic (for an average or share, of the
      average itself).
    - identity: True where the target holds exactly by definition.
    - averages[j]: (member, v, m) for the target "the average of v over the group member is m" (F[:, j] and T[j] are
      then not used), else None.
    - settings: see _solve.
    - fixed: None, or a mask of records whose weights stay exactly at w0. Their contribution enters each condition as
      a constant, and only the other weights are solved for."""
    w0 = np.asarray(w0, float)
    sparse = ST.is_sparse(F)
    F = F if sparse else np.asarray(F, float)
    T = np.asarray(T, float)
    m = len(T)
    if fixed is None or not np.asarray(fixed, bool).any():
        w1, info = _solve(w0, F, T, se, identity, averages, settings)
        fixed = np.zeros(len(w0), bool)
    else:
        fixed = np.asarray(fixed, bool)
        assert fixed.shape == w0.shape and (~fixed).any(), 'fixed: one flag per record, and some records free'
        assert np.isfinite(w0[fixed]).all() and (w0[fixed] >= 0).all(), 'fixed records: weights finite and >= 0'
        wf, free_ = w0[fixed], ~fixed
        T_free = T - (F.T @ np.where(fixed, w0, 0.0) if sparse else F[fixed].T @ wf)
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
        settings_free = settings
        sup = settings.get('solver_soft_upper')
        if sup is not None:
            settings_free = {**settings, 'solver_soft_upper': np.asarray(sup, float)[free_]}
        w1f, info = _solve(w0[free_], ST.records(F, free_) if sparse else F[free_], T_free, se, identity, av_free,
                           settings_free, offset, extra, np.abs(T))
        w1 = w0.copy()
        w1[free_] = w1f
        mv = move(w0[free_], w1f)
        info.update({'kl_move_free': mv['kl_move'], 'effective_sample_kept_free': mv['effective_sample_kept'],
                     'fixed_records': int(fixed.sum()), 'fixed_weight': float(wf.sum())})
    assert np.array_equal(w1[fixed], w0[fixed]), 'a fixed weight moved'
    info.update(move(w0, w1))
    return w1, info


def fixed_feasibility(w0, F, T, se, identity, averages, fixed):
    """A check before the solve, one row per target: what the fixed records contribute, what is left for the other
    records, and whether the other records alone could reach it with nonnegative weights. The other records can add any
    amount with the signs their contributions have. The target is reachable when what is left has one of those signs or
    is zero. 'billionaires alone exceed' marks a total that the other records can only add to and that the fixed
    records alone already put above its value. Each target is checked on its own."""
    w0, T = np.asarray(w0, float), np.asarray(T, float)
    F = F if ST.is_sparse(F) else np.asarray(F, float)
    fixed = np.asarray(fixed, bool)
    free = ~fixed
    rows = []
    for j in range(len(T)):
        a = averages[j]
        if a is None:
            col = ST.column(F, j)
            f = col[free]
            contrib = float(col[fixed] @ w0[fixed])
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
    """The effective number of records behind the weights w: (sum of w)^2 / (sum of w^2). It equals the number of
    records when all weights are equal, and falls as the weights become more unequal."""
    return w.sum() ** 2 / (w ** 2).sum()


def kl_move(w0, w1):
    """The Kullback-Leibler divergence, in nats, of the shares of the weights w1 from the shares of w0. It is a measure
    of how far the weights moved: zero when the shares are unchanged, larger as they move further."""
    p0, p1 = w0 / w0.sum(), w1 / w1.sum()
    pos = p1 > 0
    return float(np.sum(p1[pos] * np.log(p1[pos] / p0[pos])))


def move(w0, w1):
    """How far the weights moved: the Kullback-Leibler divergence of the new shares from the starting shares, the share
    of the effective number of records that is kept, and the smallest and largest weight factors (new weight over
    starting weight)."""
    on = w0 > 0
    f = w1[on] / w0[on]
    return {'kl_move': kl_move(w0, w1),
            'effective_sample_kept': float(effective_sample(w1) / effective_sample(w0)),
            'smallest_weight_factor': float(f.min()), 'largest_weight_factor_overall': float(f.max())}


# ---- How well each target is met ----
# A target's cell is "thin" when it rests on few records or on records whose weights moved far. thin_cells flags a
# cell 'warning' when it has fewer than THIN_WARNING[0] effective records, or a weight factor above THIN_WARNING[1]
# among its material records. It flags it 'stop' below THIN_STOP[0] records or above a factor of THIN_STOP[1]. A
# record is material when it holds at least THIN_FACTOR_MIN_SHARE of the cell's weighted total after the reweighting.
THIN_WARNING = (30, 10)
THIN_STOP = (10, 100)
THIN_FACTOR_MIN_SHARE = 0.01
BLOCK = 50   # thin_cells works on 50 targets at a time, to limit the memory its records-by-targets arrays use


def thin_cells(w0, w1, F, averages, factors=True):
    """Five measures for each target:

    - the effective number of records behind it, (sum w |f|)^2 / sum (w f)^2, with f a record's contribution;
    - the largest weight factor among the records that hold at least THIN_FACTOR_MIN_SHARE of the cell's weighted
      total (a record with a small share is left out; otherwise one record moved far would flag every large cell it
      is in);
    - the number of records with a contribution;
    - the flag ('', 'warning' or 'stop');
    - which test raised the flag.

    A record's contribution is its column of F, or for an average, its value of v if it is in the group. With factors
    False, only the effective number of records is computed."""
    n, m = F.shape
    eff, largest, records = np.zeros(m), np.full(m, np.nan), np.zeros(m, int)
    w1sq = w1 ** 2
    factor = np.where(w0 > 0, w1 / np.where(w0 > 0, w0, 1.0), 0.0)
    for j0 in range(0, m, BLOCK):
        j1 = min(j0 + BLOCK, m)
        C = ST.block(F, j0, j1)
        for j in range(j0, j1):
            if averages[j] is not None:
                mem, v, _ = averages[j]
                C[:, j - j0] = np.asarray(mem, bool) * np.asarray(v, float)
        on = C != 0
        A = np.abs(C)
        wa = w1 @ A
        den = w1sq @ (C ** 2)
        eff[j0:j1] = np.where(den > 0, wa ** 2 / np.where(den > 0, den, 1.0), 0.0)
        records[j0:j1] = on.sum(0)
        if factors:
            material = ((w1[:, None] * A) >= THIN_FACTOR_MIN_SHARE * wa[None, :]) & on
            largest[j0:j1] = np.where(material.any(0), (material * factor[:, None]).max(0), np.nan)
    few_w = eff < THIN_WARNING[0]
    few_s = eff < THIN_STOP[0]
    far_w = np.nan_to_num(largest) > THIN_WARNING[1]
    far_s = np.nan_to_num(largest) > THIN_STOP[1]
    none = records == 0
    flag = np.full(m, '', dtype=object)
    flag[few_w | far_w] = 'warning'
    flag[few_s | far_s | none] = 'stop'
    # The reason names the tests behind the flag: for a stop, the stop tests; for a warning, the warning tests.
    stop = flag == 'stop'
    few, far = np.where(stop, few_s, few_w), np.where(stop, far_s, far_w)
    reason = np.select([none, few & far, few, far], ['no records', 'both', 'effective records', 'weight factor'], '')
    reason = np.where(flag == '', '', reason)
    return eff, largest, records, flag, reason


def _per_target(w0, w1, F, T, se, identity, averages):
    """For each target: the file's value before and after the reweighting (for an average, the average itself), the
    miss, the miss in standard errors (empty for an identity), over_1_se, and the thin-cell measures. over_1_se is true
    when the miss is more than one standard error. For an identity it is true when the identity is missed at all, which
    means the solver failed."""
    landed = F.T @ w1                    # each condition's left-hand side (for an average: the group's weight x miss)
    Tl, size = T.astype(float).copy(), np.abs(T).astype(float)
    value, before, after = T.astype(float).copy(), F.T @ w0, F.T @ w1
    for j, a in enumerate(averages):
        if a is not None:
            mem, v, mm = a
            mem = np.asarray(mem, bool)
            v = np.asarray(v, float)
            value[j] = mm
            before[j] = (w0[mem] * v[mem]).sum() / w0[mem].sum()
            after[j] = (w1[mem] * v[mem]).sum() / w1[mem].sum()
            landed[j], Tl[j], size[j] = (mem * (v - mm)) @ w1, 0.0, abs(mm) * w0[mem].sum()
    zero = size == 0
    for j in np.flatnonzero(zero):   # a condition whose target is zero is sized by the starting weight it rests on
        a = averages[j]
        col = ST.column(F, j) if a is None else np.asarray(a[0], bool) * (np.asarray(a[1], float) - a[2])
        size[j] = np.abs(col) @ w0
    miss = after - value
    se = np.asarray(se, float)
    exact = np.asarray(identity, bool) | ~(se > 0)
    with np.errstate(divide='ignore', invalid='ignore'):
        miss_in_se = np.where(exact, np.nan, miss / se)
    over = np.where(exact, np.abs(landed - Tl) / size > 1e-6,      # an identity missed: the solver failed
                    np.abs(np.nan_to_num(miss_in_se)) > 1.0)
    eff, largest, records, flag, reason = thin_cells(w0, w1, F, averages)
    return pd.DataFrame({'achieved': after, 'value_used': value, 'before_used': before, 'miss': miss,
                         'miss_in_se': miss_in_se, 'over_1_se': over, 'effective_records': eff,
                         'largest_weight_factor': largest, 'records': records, 'flag': flag, 'flag_reason': reason})


def suggest_merges(cells, eff, flag):
    """For each flagged target, a neighbouring cell it might be merged with: a cell of the same kind and variable whose
    group label differs in exactly one comma-separated part. For example, 'age 26-34, married' has the neighbours
    'age 35-44, married' and 'age 26-34, not married'. Of the neighbours just before and after it in row order, the one
    with fewer effective records is suggested. Totals ('all', or a group ending ', all') are never suggested."""
    var = 'variable' if 'variable' in cells.columns else 'target'
    groups = cells['group'].astype(str).to_numpy()
    parts = [g.split(', ') for g in groups]     # split on comma and space, so '$1,000,000 or more' is one part
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
    """The table written to fit.csv, one row per target: its value; the file's value before and after (for a share or
    an average, the share or average itself); the standard error; the miss in standard errors; the thin-cell measures;
    the effective number of records under the starting weights; and a suggested merge. The effective records under
    the starting weights show whether a thin cell was already thin before the reweighting. Identities are the targets
    with a standard error of zero, unless identity is given."""
    w0, w1, T, se = (np.asarray(a, float) for a in (w0, w1, T, se))
    F = F if ST.is_sparse(F) else np.asarray(F, float)
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
    fit['effective_records_before'] = thin_cells(w0, w0, F, averages, factors=False)[0]
    fit['suggested_merge'] = suggest_merges(cells, fit['effective_records'].to_numpy(), fit['flag'].to_numpy())
    return fit


def summary_lines(info, fit):
    """The solver's lines for summary.md. They begin with a STOP line if any target is flagged 'stop'."""
    flag = fit['flag'].fillna('').to_numpy()
    stops, warnings = int((flag == 'stop').sum()), int((flag == 'warning').sum())
    lines = []
    if stops:
        lines += [f"**STOP: {stops} target(s) rest on too few records or too large a weight factor "
                  f"(see the thin cells table).**", '']
    miss = np.abs(np.nan_to_num(np.asarray(fit['miss_in_se'], float)))
    lines += [f"- One cost, tau = {info['tau']:g}: {'converged' if info['converged'] else 'NOT converged'} ({info['how']}).",
              f"- Largest miss {miss.max():.2f} standard errors; targets more than 0.1 se from their value: {int((miss > 0.1).sum())}, "
              f"more than 1 se: {int((miss > 1).sum())}, more than 2 se: {int((miss > 2).sum())}; thin-cell warnings "
              f"{warnings}, stops {stops}.",
              f"- Kullback-Leibler move {info['kl_move']:.4f} nats; effective sample kept "
              f"{100 * info['effective_sample_kept']:.1f}%.",
              f"- Newton steps: {info['newton_steps']} in {info['newton_calls']} calls, budget {info['newton_budget']}."
              + (' A soft cap on each weight (solver_soft_upper).' if info.get('soft_cap') else '')]
    return lines
