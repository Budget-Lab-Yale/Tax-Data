"""Test 3, piece C: method 2, tilt the reference model's probabilities.

The tilted model draws a record's 24 wealth variables one at a time in the
fixed order, exactly as the reference model does, except that at the step
for variable v every token in coarse bin b has its log-probability raised by

    s_i(v, b) = sum over the 11 families f of lambda[cell(t_i, f, g_f(i), v, b)]

and the step's probabilities are renormalized. lambda has one number per
target cell, in A's cell numbering (the same layout as B's lambda). Bins
whose cell has a zero target in any of the record's groups ("zero cells")
get probability exactly zero. The allowed-code restriction of the
reference is kept: disallowed tokens have probability zero before and after
the tilt. Within a coarse bin the tilt adds the same number to every token,
so the reference's distribution among the tokens of a bin is unchanged.

Fitting (see C_REPORT.md for the reasoning). lambda for variable v enters
only the step for v. So the variables are fitted one at a time in the fill
order, on one set of draws:

  1. every draw has its earlier variables already drawn from the tilted
     model with their final lambda;
  2. the reference network gives each draw's probabilities for variable v
     (float32); they are summed within coarse bins;
  3. lambda for v is solved so that the expected cell totals for v, computed
     from these step probabilities (not from sampled tokens), equal the
     targets exactly. Given the drawn earlier variables this is a convex
     problem with one unknown per (block, bin): the minimum-KL adjustment of
     the step's distributions subject to the cell totals. Solved by Newton's
     method with the exact Hessian;
  4. variable v is drawn from the tilted step with fixed random numbers
     (inverse-CDF: one uniform picks the bin, a second the token within it).

One pass of the 24 steps fits everything; no outer iteration is needed,
because nothing fitted later changes anything drawn earlier.

Model-agnostic: the network is reached through a "stepper" object with
    stepper.R, stepper.K, stepper.order (variable indices in fill order),
    stepper.logp_all(v) -> float32 [R, vocab_v] reference log-probabilities
                           (allowed-code restriction applied, -inf where not
                           allowed), for every row (row = record*K + draw),
    stepper.set(v, tokens [R]).
The real stepper (A's network and records) is RefStepper below; the
fabricated test uses a small hand-made model (c_dev_toy.py).
"""

import math
import time

import numpy as np
import torch

NEG_INF = float("-inf")


def log(msg, t0=[time.time()]):
    print(f"[{time.time() - t0[0]:8.1f}s] {msg}", flush=True)


# ---------------------------------------------------------------------------
# Cell geometry: which cells a record's bins fall in
# ---------------------------------------------------------------------------

class Geometry:
    """blk [N, F] block ids of each record (one per family), VB cells per
    block, vb_start [V] first within-block position of each variable,
    nbins [V], tok2bin: list of V int arrays (token -> coarse bin of that
    variable), target [C] float64 cell targets, W [N] record weights.
    Weights and targets are divided by the total weight internally.

    A block id of -1 means the record is in no group of that family (e.g. a
    PUF record whose age is unknown, when the age family covers only
    records with a known age). Such entries point at a dummy block after
    the real cells: its multipliers are held at zero, it is never a zero
    cell, and it is never counted in any total."""

    def __init__(self, blk, VB, vb_start, nbins, tok2bin, target, W, device):
        self.device = torch.device(device)
        blk = np.asarray(blk, np.int64).copy()
        self.n_missing_by_family = (blk < 0).sum(axis=0).tolist()
        self.VB = int(VB)
        self.vb_start = [int(x) for x in vb_start]
        self.nbins = [int(x) for x in nbins]
        self.tok2bin = [torch.as_tensor(np.asarray(t, np.int64), device=self.device)
                        for t in tok2bin]
        W = np.asarray(W, np.float64)
        if not np.all(np.isfinite(W)) or (W <= 0).any():
            raise ValueError("record weights must be positive and finite")
        target = np.asarray(target, np.float64)
        if np.isinf(target).any() or (target < 0).any():
            raise ValueError("targets must be finite (or NaN) and non-negative")
        # NaN = no target for that cell: never a zero cell, never a free
        # unknown (its multiplier stays 0), left out of error summaries.
        self.Wtot = float(W.sum())
        self.W = torch.as_tensor(W / self.Wtot, device=self.device)
        self.T = torch.as_tensor(target / self.Wtot, device=self.device)
        self.C = len(target)
        self.n_blocks = self.C // self.VB
        assert self.n_blocks * self.VB == self.C
        if (blk >= self.n_blocks).any():
            raise ValueError("block id out of range")
        blk[blk < 0] = self.n_blocks                     # the dummy block
        self.blk = torch.as_tensor(blk, device=self.device)
        self.N, self.F = self.blk.shape
        # targets with the dummy block appended (positive, so never a zero cell)
        self.T_ext = torch.cat([self.T, torch.ones(self.VB, dtype=torch.float64,
                                                   device=self.device)])
        # weight of each block (group total), from the records
        bw = torch.zeros(self.n_blocks + 1, dtype=torch.float64, device=self.device)
        for f in range(self.F):
            bw.index_add_(0, self.blk[:, f], self.W)
        self.block_w = bw[:self.n_blocks]
        self.relaxed = {}                                # variable -> records relaxed

    def cells(self, v):
        """[N, F, nb_v] cell ids of record i, family f, bin b of variable v."""
        b = torch.arange(self.nbins[v], device=self.device)
        return self.blk[:, :, None] * self.VB + self.vb_start[v] + b

    def forbidden(self, v):
        """[N, nb_v] True where the bin falls in a zero cell for some family."""
        return (self.T_ext[self.cells(v)] <= 0).any(dim=1)

    def tilt(self, lam, v):
        """[N, nb_v] the record's tilt of each bin of variable v."""
        lam_ext = torch.cat([lam, torch.zeros(self.VB, dtype=lam.dtype, device=lam.device)])
        return lam_ext[self.cells(v)].sum(dim=1)

    def group_total_of_cell(self):
        """[C] weight of the cell's block (the group total)."""
        return self.block_w.repeat_interleave(self.VB)


# ---------------------------------------------------------------------------
# One step: bins, sampling, expectations
# ---------------------------------------------------------------------------

def bin_logq(logp_tok, bin_of, nb):
    """log of the reference probability of each coarse bin. logp_tok
    [R, n_tok] float32 (-inf where disallowed); returns float64 [R, nb]
    (-inf where the bin has no allowed token)."""
    p = logp_tok.double().exp()
    q = torch.zeros(p.shape[0], nb, dtype=torch.float64, device=p.device)
    q.index_add_(1, bin_of[:p.shape[1]], p)
    return q.log()


def step_uniforms(seed, v, R, device):
    """Two fixed uniforms per row for the step of variable v (bin, token).
    The same seed gives the same numbers whatever the batch layout."""
    g = torch.Generator(device=device)
    g.manual_seed(int(seed) * 1009 + int(v) * 7919 + 17)
    u = torch.rand(R, 2, generator=g, device=device, dtype=torch.float64)
    return u.clamp(min=1e-15, max=1 - 1e-15)


def _inverse_cdf(prob, u):
    """Index drawn by inverse CDF. prob [R, m] non-negative, u [R]."""
    cdf = prob.cumsum(dim=1)
    cdf = cdf / cdf[:, -1:]
    idx = torch.searchsorted(cdf, u[:, None].contiguous()).squeeze(1)
    return idx.clamp(max=prob.shape[1] - 1)


def sample_step(logp_tok, logq, logP, bin_of, u):
    """Draw a bin from the tilted bin probabilities exp(logP) [R, nb] with
    u[:, 0], then a token within the bin from the reference's within-bin
    probabilities with u[:, 1]. Returns (token [R], bin [R], logp_new [R],
    logp_ref [R]); log-probabilities in float64 from float32 inputs."""
    P = logP.exp()
    b = _inverse_cdf(P, u[:, 0])
    n_tok = logp_tok.shape[1]
    bo = bin_of[:n_tok]
    p_tok = logp_tok.double().exp() * (bo[None, :] == b[:, None])
    tok = _inverse_cdf(p_tok, u[:, 1])
    if not bool((bo[tok] == b).all()):
        raise RuntimeError("drawn token outside its drawn bin")
    lp_ref = logp_tok.gather(1, tok[:, None]).squeeze(1).double()
    lp_new = (logP.gather(1, b[:, None]) - logq.gather(1, b[:, None])).squeeze(1) + lp_ref
    if not bool(torch.isfinite(lp_new).all()):
        raise RuntimeError("a drawn token has zero probability")
    return tok, b, lp_new, lp_ref


# ---------------------------------------------------------------------------
# The convex problem for one variable
# ---------------------------------------------------------------------------

def solve_step(logq, K, cells, W, T, block_w, VB, lam_v0=None, tol=1e-10, max_newton=60,
               chunk=2048, verbose=False, step_cap=5.0, lam_cap=40.0, C_real=None,
               ridge=0.0):
    """Fit lambda for one variable given the draws' bin log-probabilities.

    logq [N*K, nb] float64 (-inf for bins that are not allowed or forbidden),
    rows record-major; cells [N, F, nb] cell ids; W [N] normalized record
    weights; T [C] normalized targets; block_w [n_blocks] group totals.
    Unknowns: the cells of this variable with a positive target.

    Minimizes the dual  L = sum_i W_i mean_k log sum_b exp(logq_ikb + s_ib)
    - sum_c lam_c T_c, s_ib = sum_f lam[cells_ifb], whose gradient is
    (expected cell totals - targets). Newton with the exact Hessian (formed
    block by block), Levenberg-Marquardt damping (which also handles the
    redundant directions: the same number added to every bin of a block, and
    bin-wise shifts between families, change nothing), no multiplier moved
    by more than step_cap per iteration, and multipliers capped at
    +-lam_cap (a bin tilted down by 40 has its probability cut by a factor
    e^40, i.e. to zero for practical purposes). Cells whose targets can be
    met only in the limit (some record forced to a single bin by the
    combination of its groups) approach the cap and are reported.

    ridge > 0 adds (ridge/2) * sum_c G_c lam_c^2 to the dual (G_c the cell's
    group total): the primal then trades KL against squared cell errors,
    and at the optimum each cell's error as a share of its group total is
    -ridge * lam_c. For targets that cannot all be met at once.

    Returns (lam_v: dict cell -> value as (cell ids tensor, values tensor),
    E [n_free] expected totals, info)."""
    dev = logq.device
    N, F, nb = cells.shape
    R = logq.shape[0]
    assert R == N * K
    lq = logq.view(N, K, nb)
    # local unknowns: (block, bin) with positive target
    cell_ids = torch.unique(cells.reshape(-1))
    pos = T[cell_ids] > 0
    if C_real is not None:
        pos &= cell_ids < C_real
    free_ids = cell_ids[pos]
    nfree = len(free_ids)
    lut = torch.full((int(cells.max()) + 1,), nfree, dtype=torch.long, device=dev)
    lut[free_ids] = torch.arange(nfree, device=dev)
    loc = lut[cells]                                   # [N, F, nb]; nfree = fixed 0
    Tf = T[free_ids]
    gt = block_w[torch.div(free_ids, VB, rounding_mode="floor")]   # group total of each cell
    theta = torch.zeros(nfree + 1, dtype=torch.float64, device=dev)
    if lam_v0 is not None:
        theta[:nfree] = lam_v0[free_ids]
    # Every row must keep at least one possible bin.
    ok = torch.isfinite(lq).any(dim=2)
    if not bool(ok.all()):
        raise RuntimeError(f"{int((~ok).sum())} draws have no possible bin")

    # One-hot of the record's free-cell structure, by (family) -> we need
    # H[(c),(d)] = sum_i W_i * [loc_i hits c at bin a][loc_i hits d at bin b] C_i[a,b].
    # Each record touches, for bin a, the F cells loc[i, :, a].
    def evaluate(th, need_hess):
        s = th[loc].sum(dim=1)                          # [N, nb]
        z = lq + s[:, None, :]
        lse = torch.logsumexp(z, dim=2)                 # [N, K]
        L = (W * lse.mean(dim=1)).sum() - (th[:nfree] * Tf).sum()
        Pk = torch.exp(z - lse[:, :, None])            # [N, K, nb]
        Pbar = Pk.mean(dim=1)                           # [N, nb]
        E = torch.zeros(nfree + 1, dtype=torch.float64, device=dev)
        wP = W[:, None] * Pbar                          # [N, nb]
        for f in range(F):
            E.index_add_(0, loc[:, f, :].reshape(-1), wP.reshape(-1))
        g = E[:nfree] - Tf
        if ridge > 0:
            L = L + 0.5 * ridge * (gt * th[:nfree] ** 2).sum()
            g = g + ridge * gt * th[:nfree]
        H = None
        if need_hess:
            H = torch.zeros(nfree + 1, nfree + 1, dtype=torch.float64, device=dev)
            for s0 in range(0, N, chunk):
                sl = slice(s0, s0 + chunk)
                P_ = Pk[sl]
                Ci = torch.diag_embed(Pbar[sl]) - torch.einsum("nka,nkb->nab", P_, P_) / K
                Ci = Ci * W[sl, None, None]             # [n, nb, nb]
                n = Ci.shape[0]
                # one-hot of cells: O[n, a, c] = number of families whose cell
                # for bin a is c (0 or 1; distinct families give distinct cells)
                O = torch.zeros(n, nb, nfree + 1, dtype=torch.float64, device=dev)
                O.scatter_add_(2, loc[sl].permute(0, 2, 1),
                               torch.ones(n, nb, F, dtype=torch.float64, device=dev))
                # H += sum_n O[n]^T C_n O[n]
                X = torch.bmm(Ci, O)                    # [n, nb, nfree+1]
                H += torch.einsum("nac,nad->cd", O, X)
            H = H[:nfree, :nfree]
            if ridge > 0:
                H = H + torch.diag(ridge * gt)
        return L, g, E[:nfree], H

    info = dict(n_free=int(nfree), newton=0, rejected_steps=0, stop="max_newton")
    L, g, E, H = evaluate(theta, True)
    mu = 1e-10                       # Levenberg-Marquardt damping, relative to diag(H)
    eye = torch.eye(nfree, dtype=torch.float64, device=dev)
    for it in range(max_newton):
        rel = (g.abs() / gt).max().item() if nfree else 0.0
        if verbose:
            log(f"    newton {it}: L={L.item():.15e} max rel err={rel:.3e} mu={mu:.1e}")
        if rel <= tol:
            info["stop"] = "converged"
            break
        D = H.diagonal().clamp(min=1e-300)
        accepted = False
        while mu < 1e12:
            M = H + mu * torch.diag(D) + (1e-13 * float(D.max())) * eye
            Lc, info_c = torch.linalg.cholesky_ex(M)
            if int(info_c) != 0:
                mu *= 10
                continue
            step = -torch.cholesky_solve(g[:, None], Lc).squeeze(1)
            big = float(step.abs().max())
            if big > step_cap:                     # trust region on the multipliers
                step = step * (step_cap / big)
            slope = float((g * step).sum())
            th_new = theta.clone()
            th_new[:nfree] = (theta[:nfree] + step).clamp(-lam_cap, lam_cap)
            L_new, g_new, _, _ = evaluate(th_new, False)
            dL = L_new.item() - L.item()
            ok_armijo = dL <= 1e-4 * slope
            ok_flat = dL <= 8e-16 * abs(L.item()) and \
                float((g_new.abs() / gt).max()) < rel
            if ok_armijo or ok_flat:
                accepted = True
                mu = max(mu / 4, 1e-12)
                break
            info["rejected_steps"] += 1
            mu *= 10
        if not accepted:
            info["stop"] = "no_decrease"
            break
        theta = th_new
        L, g, E, H = evaluate(theta, True)
        info["newton"] = it + 1
    else:
        rel = (g.abs() / gt).max().item() if nfree else 0.0
        if rel <= tol:
            info["stop"] = "converged"
    rel_all = ((E - Tf).abs() / gt)                  # error without the ridge term
    info["max_rel_err"] = float(rel_all.max()) if nfree else 0.0
    info["n_not_reached_1e-6"] = int((rel_all > 1e-6).sum())
    info["max_abs_lambda"] = float(theta[:nfree].abs().max()) if nfree else 0.0
    info["n_at_cap"] = int((theta[:nfree].abs() >= lam_cap - 1e-9).sum())
    return free_ids, theta[:nfree], E, rel_all, info


def solve_variable(geom, logq, K, v, lam, auto_ridge=0.0, **kw):
    """Fit lambda for variable v in place. If the exact solve (ridge 0) does
    not converge or puts a multiplier at the cap, and auto_ridge > 0, the
    variable is refitted from zero with that ridge. Returns info."""
    cells = geom.cells(v)
    free_ids, th, E, rel, info = solve_step(logq, K, cells, geom.W, geom.T_ext, geom.block_w,
                                            geom.VB, lam_v0=lam, C_real=geom.C, **kw)
    info["ridge_used"] = kw.get("ridge", 0.0)
    info["exact_solve_stop"] = info["stop"]
    if auto_ridge > 0 and kw.get("ridge", 0.0) == 0 and \
            (info["stop"] != "converged" or info["n_at_cap"] > 0):
        kw2 = dict(kw, ridge=auto_ridge)
        lam0 = torch.zeros_like(lam)
        free_ids, th, E, rel, info2 = solve_step(logq, K, cells, geom.W, geom.T_ext,
                                                 geom.block_w, geom.VB, lam_v0=lam0,
                                                 C_real=geom.C, **kw2)
        info2["ridge_used"] = auto_ridge
        info2["exact_solve_stop"] = info["stop"]
        info2["exact_solve_max_rel_err"] = info["max_rel_err"]
        info = info2
    lam[free_ids] = th
    info["free_ids"] = free_ids
    info["rel_err"] = rel
    return info


# ---------------------------------------------------------------------------
# The pass over the 24 steps (fit or apply)
# ---------------------------------------------------------------------------

def run_pass(stepper, geom, lam, seed, fit, solve_opts=None, keep_logq=False):
    """Draw K rows per record from the tilted model, one variable at a time
    in the fill order. If fit, lambda for each variable is solved first
    (in place in `lam`, a float64 tensor [C]) from the draws' step
    probabilities. Returns dict of per-row results (torch, on the device):
      tokens [R, V], bins [R, V], logp_new_var, logp_ref_var [R, V] float64,
      kl_step [R, V] (bin-level KL of the tilted step from the reference
      step, exact given the earlier variables), expected [C] (normalized
      expected cell totals from step probabilities), realized [C]
      (normalized totals of the drawn bins), and step infos."""
    solve_opts = solve_opts or {}
    R, K = stepper.R, stepper.K
    N = geom.N
    assert R == N * K
    dev = geom.device
    V = len(geom.nbins)
    out = dict(tokens=torch.zeros(R, V, dtype=torch.long, device=dev),
               bins=torch.zeros(R, V, dtype=torch.long, device=dev),
               logp_new_var=torch.zeros(R, V, dtype=torch.float64, device=dev),
               logp_ref_var=torch.zeros(R, V, dtype=torch.float64, device=dev),
               kl_step=torch.zeros(R, V, dtype=torch.float64, device=dev),
               expected=torch.zeros(geom.C + geom.VB, dtype=torch.float64, device=dev),
               realized=torch.zeros(geom.C + geom.VB, dtype=torch.float64, device=dev),
               infos={})
    Wrow = geom.W.repeat_interleave(K) / K
    out["untilted"] = {}
    var_index = getattr(stepper, "var_index", lambda key: key)
    for key in stepper.order:
        t0 = time.time()
        lp = stepper.logp_all(key)                       # [R, n_tok] float32
        t_net = time.time() - t0
        v = var_index(key)
        if v is None:
            # A drawn variable with no target cells: drawn from the
            # reference, untilted (log-probabilities still recorded).
            u = step_uniforms(seed, 10_000 + int(key), R, dev)
            tok = _inverse_cdf(lp.double().exp(), u[:, 1])
            out["untilted"][int(key)] = tok
            out.setdefault("logp_untilted", torch.zeros(R, dtype=torch.float64, device=dev))
            out["logp_untilted"] += lp.gather(1, tok[:, None]).squeeze(1).double()
            stepper.set(key, tok)
            log(f"  step {key}: untilted, {time.time() - t0:.1f}s")
            continue
        nb = geom.nbins[v]
        bo = geom.tok2bin[v]
        logq = bin_logq(lp, bo, nb)
        forb = geom.forbidden(v)                         # [N, nb]
        # A record whose every possible bin is in some zero cell (possible
        # when targets do not come from the records themselves) keeps its
        # reference bins: the zero-cell rule is relaxed for that record and
        # variable, and the mass it puts in zero cells is reported.
        possible = torch.isfinite(logq).view(N, K, nb).any(dim=1)
        relax = ~(possible & ~forb).any(dim=1)
        if bool(relax.any()):
            forb = forb & ~relax[:, None]
        geom.relaxed[int(v)] = int(relax.sum())
        if not hasattr(geom, "relaxed_mask"):
            geom.relaxed_mask = {}
        geom.relaxed_mask[int(v)] = relax
        logq = logq.masked_fill(forb.repeat_interleave(K, dim=0), NEG_INF)
        bad = ~torch.isfinite(logq).any(dim=1)
        if bool(bad.any()):
            raise RuntimeError(f"variable {v}: {int(bad.sum())} draws with every bin "
                               "disallowed or in a zero cell")
        info = {}
        if fit:
            info = solve_variable(geom, logq, K, v, lam, **solve_opts)
        s = geom.tilt(lam, v)                            # [N, nb]
        logP = torch.log_softmax(logq + s.repeat_interleave(K, dim=0), dim=1)
        P = logP.exp()
        u = step_uniforms(seed, v, R, dev)
        tok, b, lp_new, lp_ref = sample_step(lp, logq, logP, bo, u)
        out["tokens"][:, v] = tok
        out["bins"][:, v] = b
        out["logp_new_var"][:, v] = lp_new
        out["logp_ref_var"][:, v] = lp_ref
        kl = torch.where(P > 0, P * (logP - logq), torch.zeros_like(P)).sum(dim=1)
        out["kl_step"][:, v] = kl
        cells = geom.cells(v)                            # [N, F, nb]
        Pbar = (P.view(N, K, nb).mean(dim=1)) * geom.W[:, None]
        rb = b.view(N, K)                                # drawn bin per row
        for f in range(geom.F):
            out["expected"].index_add_(0, cells[:, f, :].reshape(-1), Pbar.reshape(-1))
            rc = cells[:, f, :].gather(1, rb).reshape(-1)  # [R] cell of the drawn bin
            out["realized"].index_add_(0, rc, Wrow)
        stepper.set(key, tok)
        info_s = {k: (vv if not torch.is_tensor(vv) else None) for k, vv in info.items()}
        info_s = {k: vv for k, vv in info_s.items() if vv is not None}
        info_s["records_zero_rule_relaxed"] = geom.relaxed[int(v)]
        info_s["seconds_network"] = t_net
        info_s["seconds"] = time.time() - t0
        info_s["mean_kl_step"] = float((geom.W.repeat_interleave(K) / K * kl).sum())
        out["infos"][int(v)] = info_s
        log(f"  variable {v}: {info_s}")
    out["expected"] = out["expected"][:geom.C]
    out["realized"] = out["realized"][:geom.C]
    return out


# ---------------------------------------------------------------------------
# Summaries
# ---------------------------------------------------------------------------

def target_error(geom, totals, cell_family=None):
    """Absolute error of normalized cell totals as a share of the group total,
    over cells with a positive target and over zero cells. Returns dict."""
    gt = geom.group_total_of_cell()
    err = (totals - geom.T).abs() / gt
    pos = geom.T > 0
    zero = geom.T == 0
    out = dict(max_share_err_pos=float(err[pos].max()),
               mean_share_err_pos=float(err[pos].mean()),
               zero_cells_total_share=float(totals[zero].sum()),
               zero_cells_with_mass=int((totals[zero] > 0).sum()))
    return out, err


def kl_summary(geom, K, logp_new, logp_ref, kl_rb):
    """KL(tilted || reference) in nats per unit of weight: the sample
    estimate (weighted mean over draws of logp_new - logp_ref) and the
    step-wise version (sum of each step's exact KL given the drawn earlier
    variables; same expectation, less noise). Standard errors treat records
    as independent (implicates of one household are not, so they are a
    little optimistic)."""
    N = geom.N
    W = geom.W
    d = (logp_new - logp_ref).view(N, K)
    r = kl_rb.view(N, K)
    out = {}
    for name, x in (("kl_sample", d), ("kl_stepwise", r)):
        m = x.mean(dim=1)
        est = float((W * m).sum())
        se = float(torch.sqrt((W ** 2 * x.var(dim=1) / K).sum())) if K > 1 else float("nan")
        out[name] = est
        out[name + "_se"] = se
    out["kl_sample_per_record"] = d.mean(dim=1)
    out["kl_stepwise_per_record"] = r.mean(dim=1)
    return out


# ---------------------------------------------------------------------------
# The real reference network (A's interface)
# ---------------------------------------------------------------------------

class RefStepper:
    """Reference network and records through a_common. Rows are
    record-major (row = i*K + k); the network's inputs for every row are
    held on the GPU and filled step by step."""

    def __init__(self, A, model, ctx, K, vocab, rec_subset=None, batch_size=16384):
        self.A, self.model, self.ctx = A, model, ctx
        self.vocab = [int(x) for x in vocab]
        self.K = K
        recs = np.arange(ctx.N) if rec_subset is None else np.asarray(rec_subset)
        self.rec_idx = torch.as_tensor(np.repeat(recs, K), device=ctx.device)
        self.R = len(self.rec_idx)
        self.cur = ctx.tok_in[self.rec_idx].clone()
        self.hid = self.cur < 0
        self.bs = batch_size
        # step keys are positions in ctx.order; step_var gives the targeted
        # variable drawn at the step (None or -1: drawn untilted)
        self.order = list(range(len(ctx.order)))
        self.slot = list(ctx.order)
        self.tvar = [None if (k is None or int(k) < 0) else int(k) for k in ctx.step_var]
        self.n_tok_slot = [int(n) for n in getattr(ctx, "slot_vocab", [0] * len(ctx.order))]

    def var_index(self, key):
        return self.tvar[key]

    @torch.no_grad()
    def logp_all(self, key):
        j = self.slot[key]
        v = self.tvar[key]
        outs = []
        for s0 in range(0, self.R, self.bs):
            sl = slice(s0, s0 + self.bs)
            ri = self.rec_idx[sl]
            lp = self.A._step_logits(self.model, self.ctx, self.cur[sl], self.hid[sl], ri, j,
                                     -1 if v is None else v, v is not None, None)
            outs.append(lp)
        lp = torch.cat(outs, dim=0)
        if v is None:
            return lp.contiguous()     # padding tokens are -inf, never drawn
        return lp[:, :self.vocab[v]].contiguous()

    def set(self, key, tok):
        j = self.slot[key]
        self.cur[:, j] = tok
        self.hid[:, j] = False
