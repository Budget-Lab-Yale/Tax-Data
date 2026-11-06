"""The tilt of the state shares (FEATURES.md, "State weights"): a record's national weight stays fixed; only its split across
areas moves. solver.py moves weights and has no sum-to-one condition, so this module has its own Newton's method, with the
same form of cost and the same conventions.

The rule. In one AGI group (each record is in one group, and every target is in one group, so each group is its own problem):

    minimize   0.5 x sum over targets of (miss / standard error)^2  +  tau x D,
    D = sum over records of (w_i / W) x sum over areas of q_ia x log(q_ia / p_ia),   in nats,

with w_i the record's national weight, W the group's total weight, p_ia the record's starting share of area a and q_ia its new
share. A target's miss is sum over records of (w_i q_ia x_ik) + the fixed amount on the left side - the target, for area a and
item k. The answer has the form

    q_ia = p_ia exp(x_i' lambda_a) / sum over areas b of p_ib exp(x_i' lambda_b),

so each record's shares sum to 1 by construction, with one multiplier per target. The multipliers minimize the smooth, convex
function

    phi(lambda) = sum_i (w_i / W) log sum_a p_ia exp(z_i' lambda_a) - t . lambda + 0.5 sum_j tau s_j^2 lambda_j^2,

whose gradient is zero when miss_j = - tau s_j^2 lambda_j (dollars and returns scaled: z_ik = x_ik / c_k, c_k = sum_i (w_i / W)
|x_ik|; t and s are the targets and standard errors divided by W c_k). Newton's method with a line search, each group with a
budget of 150 steps (settings state_weights.newton_budget), stopping when the largest gradient is below 1e-8 of the group's
total of the item, or when rounding stops it falling below 1e-7, as in solver.py.

With every target's standard error a relative tolerance rho x |target|, the cost divided by tau is D + (1 / (2 tau rho^2)) x
sum (miss / target)^2, so only the product tau x rho^2 matters.

Records pinned to an area (the Forbes list) are held out of the shares: they enter as fixed amounts on the left side.
"""
import numpy as np
import scipy.linalg as sl
from scipy.special import logsumexp


def solve(w, p0, X, T, se, fixed=None, tau=1000.0, budget=150, tol=1e-8, verbose=None):
    """One group. w [n] national weights of the free records, p0 [n, A] starting shares (each row sums to 1; a zero stays
    zero), X [n, K] the items, T [A, K] targets (NaN: no target), se [A, K] their standard errors, fixed [A, K] the pinned
    records' amounts. Returns q [n, A] and a dict: steps, status, miss [A, K] (NaN where no target), kl (D, in nats)."""
    w = np.asarray(w, float)
    n, A = p0.shape
    K = X.shape[1]
    assert T.shape == (A, K) and se.shape == (A, K) and len(w) == n and X.shape[0] == n
    assert (w > 0).all() and np.isfinite(p0).all() and (p0 >= 0).all() and np.allclose(p0.sum(1), 1.0, atol=1e-6)
    assert np.isfinite(X).all()
    W = w.sum()
    a = w / W
    scale = (a[:, None] * np.abs(X)).sum(0)
    use = np.isfinite(T) & (scale[None, :] > 0)
    assert (se[use] > 0).all() and np.isfinite(se[use]).all(), 'a target with no positive standard error'
    Z = X / np.where(scale > 0, scale, 1.0)
    fx = np.zeros((A, K)) if fixed is None else np.asarray(fixed, float)
    t = np.where(use, (np.nan_to_num(T) - fx) / (W * np.where(scale > 0, scale, 1.0)), 0.0)
    nu = np.where(use, tau * (se / (W * np.where(scale > 0, scale, 1.0))) ** 2, 0.0)
    with np.errstate(divide='ignore'):
        logp = np.log(p0)
    idx = np.flatnonzero(use.ravel())           # a target's position: area a, item k -> a x K + k
    m = len(idx)
    lam = np.zeros(A * K)

    def shares(lam):
        E = logp + Z @ lam.reshape(A, K).T
        ls = logsumexp(E, axis=1)
        return np.exp(E - ls[:, None]), ls

    def value(lam, ls):
        return float(a @ ls - t.ravel() @ lam + 0.5 * (nu.ravel() * lam ** 2).sum())

    def gradient(lam, q):
        return ((a[:, None] * Z).T @ q).T.ravel() - t.ravel() + nu.ravel() * lam

    # the Hessian, block by block: sum_i a_i z_i z_i' (delta_ab q_ia - q_ia q_ib) + diag(nu)
    iu, ju = np.triu_indices(K)
    Z2 = Z[:, iu] * Z[:, ju]

    def hessian(q):
        H = np.zeros((A * K, A * K))
        for r in range(A):
            c = a * q[:, r]
            R = -(Z2.T @ (c[:, None] * q[:, r:]))
            R[:, 0] += Z2.T @ c
            for b in range(r, A):
                v = R[:, b - r]
                H[r * K + iu, b * K + ju] = v
                H[r * K + ju, b * K + iu] = v
                H[b * K + iu, r * K + ju] = v
                H[b * K + ju, r * K + iu] = v
        H = H[np.ix_(idx, idx)]
        H[np.diag_indices(m)] += nu.ravel()[idx]
        return H

    q, ls = shares(lam)
    D = value(lam, ls)
    best, stuck, steps, status = np.inf, 0, 0, 'out of steps'
    for _ in range(budget + 1):
        g = gradient(lam, q)[idx] if m else np.zeros(0)
        size_g = float(np.abs(g).max()) if m else 0.0
        if size_g < tol:
            status = 'converged'
            break
        stuck = stuck + 1 if size_g > 0.5 * best else 0
        best = min(best, size_g)
        if stuck >= 5 and size_g < 1e-7:
            status = 'converged'
            break
        if steps == budget:
            break
        steps += 1
        H = hessian(q)
        d = np.sqrt(np.diag(H)) + 1e-300
        B = H / d[:, None] / d[None, :]
        B[np.diag_indices(m)] += 1e-12
        step = -sl.cho_solve(sl.cho_factor(B, lower=True, check_finite=False), g / d, check_finite=False) / d
        full = np.zeros(A * K)
        full[idx] = step
        s = 1.0
        while True:
            lam_new = lam + s * full
            q_new, ls_new = shares(lam_new)
            D_new = value(lam_new, ls_new)
            if np.isfinite(D_new) and D_new <= D + 1e-4 * s * (g @ step):
                break
            if np.isfinite(D_new) and D_new <= D + 1e-13 * max(1.0, abs(D)) and \
                    np.abs(gradient(lam_new, q_new)[idx]).max() < size_g:
                break
            s /= 2
            if s < 1e-12:
                status = 'converged' if size_g < 100 * tol else 'stalled'
                break
        if s < 1e-12:
            break
        lam, q, ls, D = lam_new, q_new, ls_new, D_new
        if verbose:
            verbose(f'  step {steps}: largest gradient {size_g:.3e}, step size {s:.3g}')
    reached = ((w[:, None] * X).T @ q).T + fx
    miss = np.where(use, reached - np.nan_to_num(T), np.nan)
    with np.errstate(divide='ignore', invalid='ignore'):
        rel = np.where(q > 0, q * (np.log(q) - logp), 0.0)
    kl = float(a @ rel.sum(1))
    return q, dict(steps=steps, status=status, miss=miss, kl=kl, targets=m, records=n, largest_gradient=size_g,
                   reached=reached, dropped_no_holder=int((np.isfinite(T) & ~use).sum()))
