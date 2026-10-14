"""Test 3, piece B: method 1, reweight the draws (the model is unchanged).

Minimum-KL weights on a set of completed draws, subject to
  (i)  each record's draws summing to the record's own population weight, and
  (ii) every feasible target cell being hit exactly.

With the record constraints eliminated, draw j of record i gets
    w_ij = W_i * exp(lambda . a_ij) / sum_j' exp(lambda . a_ij'),
and lambda (one number per target cell) minimises the convex dual
    f(lambda) = sum_i W_i log sum_j exp(lambda . a_ij) - lambda . t,
whose gradient is (achieved cell totals - targets). The dual is solved by a
damped Newton method in which each Newton step is found by preconditioned
conjugate gradients using Hessian-vector products (the Hessian is never
formed); see `solve_dual` and B_REPORT.md for why.

Reusable entry points (pieces C, D and E call these for residual calibration):

    calibrate(draws_tokens, records, targets, **opts) -> (w, lam, report)
        draws_tokens  int array [N, K, 24], wealth tokens in WEALTH order
        records       A's records table (one row per record, in the same
                      order as draws_tokens; must have a `weight` column)
        targets       A's targets table (one row per cell, in cell-id order)
        returns w [N, K] float64 weights (each row sums to the record's
        weight; zero for dropped draws), lam [C] float64 (0 for cells not in
        the solve), report (dict, JSON-ready; report["cell_status"] is a list
        of length C with one status string per cell).

    calibrate_matrix(A, W, t, K, **opts) -> (w, lam, report)
        the same from an already-built contribution matrix A (scipy CSR,
        rows record-major: row i*K + j is draw j of record i).

Cell statuses: "active" (hit exactly), "zero" (target zero; draws in it get
weight zero), "zero_relaxed" (target zero but some record has no draw that
avoids every zero cell, so the cell is left unconstrained), "unreachable"
(positive target, no remaining draw falls in it), "above_reach" (target
exceeds the total weight of records that have a draw in it), "below_forced"
(target is less than the weight of records whose every draw falls in it),
"joint" (failed only in the joint solve: no exact solution with the other
cells; removed after the solve stalled).

CLI:  python b_calibrate.py --draws $P/a/ref_draws.npz --k 32 --tag ref
      python b_calibrate.py --selftest      (fabricated data, $P/dev/b/)
"""
import argparse
import json
import os
import sys
import time

import numpy as np
import scipy.sparse as sp
import torch

P = "/nfs/roberts/scratch/pi_nrs36/jar335/posttrain"
HERE = os.path.dirname(os.path.abspath(__file__))

STATUSES = ["active", "zero", "zero_relaxed", "unreachable", "above_reach", "block",
            "below_forced", "joint", "missed", "no_target"]


def log(msg):
    print(time.strftime("%H:%M:%S"), msg, flush=True)


# --------------------------------------------------------------------------
# The contribution matrix held on the GPU
# --------------------------------------------------------------------------

class Contrib:
    """A (rows = draws, record-major; columns = cells), binary, as a padded
    index array cols [R, m]: the cells each draw falls in. Padding points at
    an extra column C whose multiplier is held at zero. Built from a scipy
    CSR matrix or directly from an int array [R, m] of cell ids
    (a_common.contrib_index), which needs C."""

    def __init__(self, A, N, K, device, C=None, chunk_nnz=2 ** 28):
        if isinstance(A, np.ndarray):
            if C is None:
                raise ValueError("C (number of cells) needed with an index array")
            R, m = A.shape
            cols = A.astype(np.int32, copy=True)
            if cols.size and cols.max() > C:
                raise ValueError("cell id out of range")
            # negative or C = the draw is in no cell of that slot (a family whose
            # group is unknown for the record): point it at the padding column
            self.padded = bool((cols < 0).any())
            cols[cols < 0] = C
        else:
            A = sp.csr_matrix(A)
            R, C2 = A.shape
            if C is not None and C2 != C:
                raise ValueError(f"A has {C2} columns, targets {C}")
            C = C2
            if A.nnz and not np.all(A.data == 1):
                raise ValueError("contribution matrix must be binary (entries 1)")
            nnz_row = np.diff(A.indptr)
            m = int(nnz_row.max()) if R else 0
            self.padded = not np.all(nnz_row == m)
            if not self.padded:
                cols = A.indices.reshape(R, m).astype(np.int32)
            else:
                cols = np.full((R, m), C, np.int32)
                pos = np.arange(A.nnz) - np.repeat(A.indptr[:-1], nnz_row)
                cols[np.repeat(np.arange(R), nnz_row), pos] = A.indices
        assert R == N * K, f"A has {R} rows, expected N*K = {N * K}"
        self.N, self.K, self.R, self.C, self.m = N, K, R, C, m
        self.device = device
        self.cols = torch.from_numpy(np.ascontiguousarray(cols)).to(device)
        self.rows_per_chunk = max(1, chunk_nnz // max(m, 1))

    def chunks(self):
        for s in range(0, self.R, self.rows_per_chunk):
            yield s, min(self.R, s + self.rows_per_chunk)

    def Av(self, v):
        """v: [C] -> [N, K], sum of v over the cells each draw falls in."""
        ve = torch.cat([v, v.new_zeros(1)])
        out = torch.empty(self.R, dtype=v.dtype, device=self.device)
        for s, e in self.chunks():
            out[s:e] = ve[self.cols[s:e]].sum(1)
        return out.view(self.N, self.K)

    def ATz(self, z):
        """z: [N, K] -> [C], total of z over the draws falling in each cell."""
        z = z.reshape(-1)
        out = torch.zeros(self.C + 1, dtype=z.dtype, device=self.device)
        for s, e in self.chunks():
            out.index_add_(0, self.cols[s:e].reshape(-1).long(),
                           z[s:e, None].expand(-1, self.m).reshape(-1))
        return out[:self.C]

    def pairs(self, alive):
        """Unique (record, cell) pairs over alive draws, chunk by chunk.
        Returns (seg [R, m] int32 pair id per entry, -1 for dead/padding;
        pair_rec [U], pair_cell [U], pair_count [U] = number of alive draws
        of the record in the cell)."""
        seg = torch.full((self.R, self.m), -1, dtype=torch.int32, device=self.device)
        recs, cells, counts = [], [], []
        base = 0
        alive_r = alive.reshape(-1)
        rec_of_row = torch.arange(self.R, device=self.device) // self.K
        for s, e in self.chunks():
            c = self.cols[s:e].long()
            ok = alive_r[s:e, None] & (c < self.C)
            key = rec_of_row[s:e, None] * (self.C + 1) + c
            key = torch.where(ok, key, torch.full_like(key, -1))
            u, inv, cnt = torch.unique(key.reshape(-1), return_inverse=True,
                                       return_counts=True)
            has_neg = bool((u[0] < 0).item()) if len(u) else False
            inv = inv.view(e - s, self.m).to(torch.int32)
            if has_neg:
                u, cnt = u[1:], cnt[1:]
                inv = inv - 1          # the -1 key sorts first
            seg[s:e] = torch.where(ok, inv + base, torch.full_like(inv, -1))
            recs.append(u // (self.C + 1))
            cells.append(u % (self.C + 1))
            counts.append(cnt)
            base += len(u)
        return (seg, torch.cat(recs), torch.cat(cells), torch.cat(counts))


# --------------------------------------------------------------------------
# Pre-solve: zero cells, dead records, unreachable, out-of-bounds and tight
# cells
# --------------------------------------------------------------------------

def reach(Ac, W, alive):
    """For each cell: weight of records with at least one alive draw in it
    (reach_any) and weight of records with every alive draw in it (forced).
    Also returns the (record, cell) pairs for reuse."""
    C = Ac.C
    seg, prec, pcell, pcnt = Ac.pairs(alive)
    nalive = alive.sum(1)
    Wp = W[prec]
    ra = torch.zeros(C + 1, dtype=torch.float64, device=Ac.device)
    ra.index_add_(0, pcell, Wp)
    allin = pcnt == nalive[prec]
    fo = torch.zeros(C + 1, dtype=torch.float64, device=Ac.device)
    fo.index_add_(0, pcell[allin], Wp[allin])
    return ra[:C], fo[:C], (seg, prec, pcell, pcnt, allin)


def presolve(Ac, W, t, tol_rel=1e-9, tight_rel=1e-10, max_tight_rounds=20, draw_mask=None):
    """Statuses and alive draws before the solve. W [N], t [C] float64 on
    device. Returns status (np array of str), alive [N, K] bool, info."""
    C, N, K = Ac.C, Ac.N, Ac.K
    dev = Ac.device
    status = np.array(["active"] * C, dtype=object)
    zero = (t == 0)
    status[zero.cpu().numpy()] = "zero"

    # 1. Drop draws in zero cells. A record left with no draw at all would
    # have nowhere to put its weight; for such records relax the zero cells
    # of the draw that hits the fewest of them (ties: the first such draw).
    relaxed = torch.zeros(C, dtype=torch.bool, device=dev)
    rounds = 0
    while True:
        rounds += 1
        nzero = Ac.Av((zero & ~relaxed).to(torch.float64))
        alive = (nzero == 0) if draw_mask is None else ((nzero == 0) & draw_mask)
        dead = (~alive.any(1)) & (W > 0)
        nd = int(dead.sum())
        if nd == 0:
            break
        jbest = nzero.masked_fill(~dead[:, None], float("inf")).argmin(1)
        rows = (torch.arange(N, device=dev) * K + jbest)[dead]
        c = Ac.cols[rows].long().reshape(-1)
        c = c[c < C]
        hit = torch.zeros(C + 1, dtype=torch.bool, device=dev)
        hit[c] = True
        new = hit[:C] & zero & ~relaxed
        log(f"  presolve: {nd} records have no draw avoiding every zero cell; "
            f"relaxing {int(new.sum())} zero cells")
        if not bool(new.any()):
            raise RuntimeError("dead records but nothing left to relax")
        relaxed |= new
    status[relaxed.cpu().numpy()] = "zero_relaxed"
    info = {"zero_relax_rounds": rounds - 1,
            "records_dead_after_zero_cells": None,
            "draws_dropped_zero_cells": int((~alive).sum()),
            "draws_total": int(alive.numel())}

    # 2. Cells out of reach: positive target, but no alive draw in the cell,
    # or more weight than all records with a draw in it, or less than the
    # records whose every draw is in it.
    ra, fo, _ = reach(Ac, W, alive)
    pos = t > 0
    un = pos & (ra == 0)
    above = pos & ~un & (t > ra * (1 + tol_rel))
    below = pos & (t < fo * (1 - tol_rel))
    for name, msk in (("unreachable", un), ("above_reach", above), ("below_forced", below)):
        status[msk.cpu().numpy()] = name
    info["reach_any"], info["forced"] = ra.cpu().numpy(), fo.cpu().numpy()

    # 3. Tight cells. If a cell's target equals the weight of all records
    # that can reach it, each of those records must put all its weight on
    # its draws in the cell: drop its other draws. If the target equals the
    # weight of the records forced into it, the other records must put none
    # there: drop their draws in the cell. Exact zeros of this kind are a
    # boundary of the feasible set that the dual can approach only as the
    # multipliers go to infinity, so they are removed here. Repeat until
    # nothing changes. A record that would lose every draw keeps them all.
    tight_drops = 0
    st_t = torch.from_numpy(status == "active").to(dev)
    for tr in range(max_tight_rounds):
        ra, fo, (seg, prec, pcell, pcnt, allin) = reach(Ac, W, alive)
        hi = st_t & pos & (ra > fo) & (t >= ra * (1 - tight_rel))
        lo = st_t & pos & (ra > fo) & (t <= fo * (1 + tight_rel))
        if not bool(hi.any() or lo.any()):
            break
        kill = torch.zeros_like(alive)
        if bool(hi.any()):
            hif = hi.to(torch.float64)
            n_in = Ac.Av(hif)                                   # tight cells each draw is in
            n_reach = torch.zeros(N, dtype=torch.float64, device=dev)
            n_reach.index_add_(0, prec, hif[pcell.clamp_max(C - 1)] * (pcell < C))
            kill |= alive & (n_in < n_reach[:, None])
        if bool(lo.any()):
            # pairs (record, cell) with cell tight-low and record not forced in
            bad_pair = lo[pcell.clamp_max(C - 1)] & (pcell < C) & ~allin
            flag = torch.zeros(len(prec) + 1, dtype=torch.bool, device=dev)
            flag[:-1] = bad_pair
            sl = seg.long()
            sl = torch.where(sl >= 0, sl, torch.full_like(sl, len(prec)))
            hitbad = flag[sl].any(1).view(N, K)
            kill |= alive & hitbad
        new_alive = alive & ~kill
        dead = (~new_alive.any(1)) & (W > 0)
        new_alive[dead] = alive[dead]
        nk = int((alive & ~new_alive).sum())
        log(f"  presolve tight round {tr + 1}: {int(hi.sum())} cells at full reach, "
            f"{int(lo.sum())} at forced weight; dropping {nk} draws "
            f"({int(dead.sum())} records exempted)")
        if nk == 0:
            break
        tight_drops += nk
        alive = new_alive
    info["draws_dropped_tight"] = tight_drops
    info["draws_alive"] = int(alive.sum())
    info["records_with_one_alive_draw"] = int((alive.sum(1) == 1).sum())
    return status, alive, info


# --------------------------------------------------------------------------
# The dual solve
# --------------------------------------------------------------------------

def block_check(Ac, W, t, alive, status, cell_meta, tol_rel=1e-9):
    """Exact feasibility of each (wave, family, group, variable) block on its
    own. A block's cells are the coarse bins of one variable for the records
    of one group. Each record can reach the bins its alive draws fall in.
    The block can be hit exactly if and only if, for every set S of the
    block's constrained bins, the weight of records whose draws all fall in
    S is at most the target of S, and the target of S is at most the weight
    of records with at least one draw in S (a supply-and-demand condition;
    with 14 bins there are only 16,384 sets, so all are checked). Where the
    condition fails, bins are released one at a time, each time the bin
    that appears in the largest total violation, until it holds; released
    bins get status 'block'. Returns the updated status and a count."""
    dev = Ac.device
    C = Ac.C
    key_cols = [c for c in ("wave", "table", "family", "group", "variable")
                if cell_meta is not None and c in cell_meta.columns]
    if cell_meta is None or not all(c in key_cols for c in ("group", "variable")) or "bin" not in cell_meta.columns:
        log("  block check skipped: cell table lacks wave/family/group/variable/bin")
        return status, 0
    keys = cell_meta.groupby(key_cols, sort=False).ngroup().to_numpy()
    nkey = int(keys.max()) + 1
    binpos = cell_meta.groupby(key_cols, sort=False).cumcount().to_numpy()
    nb = int(binpos.max()) + 1
    if nb > 16:
        log(f"  block check skipped: {nb} bins per block")
        return status, 0
    key_t = torch.from_numpy(keys).to(dev)
    bit_t = torch.from_numpy(binpos).to(dev)
    seg, prec, pcell, pcnt = Ac.pairs(alive)
    del seg
    # bitmask of reachable bins per (record, key): pairs are unique, so a sum
    # of distinct powers of two is their OR.
    pk = key_t[pcell]
    rk = prec * nkey + pk
    u, inv = torch.unique(rk, return_inverse=True)
    mask = torch.zeros(len(u), dtype=torch.int64, device=dev)
    mask.index_add_(0, inv, (1 << bit_t[pcell]).to(torch.int64))
    urec = u // nkey
    ukey = u % nkey
    M = 1 << nb
    Fw = torch.zeros(nkey * M, dtype=torch.float64, device=dev)
    Fw.index_add_(0, ukey * M + mask, W[urec])
    Fw = Fw.view(nkey, M)
    total = Fw.sum(1, keepdim=True)
    # zeta transform: F[S] = weight of records whose mask is a subset of S
    for b in range(nb):
        v = Fw.view(nkey, M >> (b + 1), 2, 1 << b)
        v[:, :, 1, :] += v[:, :, 0, :]
    # target of each set S
    Tt = torch.zeros(nkey * M, dtype=torch.float64, device=dev)
    Tt.index_add_(0, key_t * M + (1 << bit_t), t)
    Tt = Tt.view(nkey, M)
    for b in range(nb):
        v = Tt.view(nkey, M >> (b + 1), 2, 1 << b)
        v[:, :, 1, :] += v[:, :, 0, :]
    full = M - 1
    Sidx = torch.arange(M, device=dev)
    cap = total - Fw[:, full ^ Sidx]          # records with a draw in S
    forced = Fw                               # records with every draw in S
    cons_cell = torch.from_numpy(status == "active").to(dev)
    B = torch.zeros(nkey, dtype=torch.int64, device=dev)
    B.index_add_(0, key_t[cons_cell], (1 << bit_t[cons_cell]).to(torch.int64))
    scale = total.clamp_min(1e-300)
    released = torch.zeros(C, dtype=torch.bool, device=dev)
    bits = (Sidx[:, None] >> torch.arange(nb, device=dev)[None, :]) & 1   # [M, nb]
    for it in range(nb):
        valid = (Sidx[None, :] & ~B[:, None]) == 0
        viol = ((Tt - cap).clamp_min(0) + (forced - Tt).clamp_min(0)) / scale
        viol = torch.where(valid & (viol > tol_rel), viol, torch.zeros_like(viol))
        bad = viol.sum(1) > 0
        if not bool(bad.any()):
            break
        per_bin = viol[bad] @ bits.to(torch.float64)                       # [nbad, nb]
        drop = per_bin.argmax(1)
        kb = torch.nonzero(bad).squeeze(1)
        B[kb] &= ~(1 << drop)
        # mark the released cells
        rel_key = torch.full((nkey,), -1, dtype=torch.int64, device=dev)
        rel_key[kb] = drop
        released |= cons_cell & (rel_key[key_t] == bit_t)
        log(f"  block check pass {it + 1}: {int(bad.sum())} blocks fail; releasing one bin in each")
    rel = released.cpu().numpy()
    status = status.copy()
    status[rel] = "block"
    return status, int(rel.sum())


def solve_ipf(Ac, W, t, alive, act, lam0=None, tol_rel=1e-10, max_sweeps=2000,
              lam_max=30.0, step_max=5.0, check_every=5, time_limit=None, verbose=True):
    """Raking (iterative proportional fitting) in multiplier form.

    Each column slot of the contribution index (one family x one variable in
    A's layout) splits the draws into that slot's cells. For one slot at a
    time, every cell's multiplier moves by log(target / achieved), which
    puts that slot's cells exactly on target; each record's draw shares are
    then renormalised so that the record keeps its weight. Sweeping over
    all slots repeatedly converges to the same minimum-KL weights as the
    Newton solve whenever the targets can be hit, and it never diverges when
    they cannot: a cell that cannot be reached sees its multiplier drift
    steadily, and once it passes lam_max (a tilt of exp(30), about 1e13) the
    cell is released, listed as 'joint' and left out. W, t normalised.
    Returns lam, q, miss, info (info['released'] is a bool tensor)."""
    dev = Ac.device
    C = Ac.C
    act = act.clone()
    lam = torch.zeros(C, dtype=torch.float64, device=dev) if lam0 is None else lam0.clone() * act
    hasw = W > 0
    # Each cell must belong to one slot only.
    slot_of = torch.full((C + 1,), -1, dtype=torch.int64, device=dev)
    for j in range(Ac.m):
        cj = torch.unique(Ac.cols[:, j].long())
        prev = slot_of[cj]
        clash = (prev >= 0) & (prev != j) & (cj < C)
        if bool(clash.any()):
            raise RuntimeError("a cell appears in more than one column slot; raking needs disjoint slots")
        slot_of[cj] = j
    tt = torch.cat([t, t.new_zeros(1)])

    def scores(lam):
        return Ac.Av(lam).masked_fill(~alive, -float("inf"))

    def shares(s):
        lse = torch.logsumexp(s, 1, keepdim=True)
        q = torch.exp(s - lse).masked_fill(~alive, 0.0)
        return torch.where(hasw[:, None], q, torch.zeros_like(q))

    t0 = time.time()
    hist = []
    released = torch.zeros(C, dtype=torch.bool, device=dev)
    converged = False
    s = scores(lam)
    for sweep in range(max_sweeps + 1):
        if sweep % check_every == 0 or sweep == max_sweeps:
            s = scores(lam)                    # clear accumulated rounding
            q = shares(s)
            miss = (Ac.ATz(W[:, None] * q) - t) * act
            rel = torch.where(t > 0, miss.abs() / torch.where(t > 0, t, torch.ones_like(t)), torch.zeros_like(t))
            mrel = float(rel.max())
            nbad = int(((miss.abs() > tol_rel * t + 1e-17) & (t > 0)).sum())
            out = act & (lam.abs() > lam_max)
            nout = int(out.sum())
            hist.append({"sweep": sweep, "max_rel_miss": mrel, "n_above_tol": nbad,
                         "max_abs_lambda": float(lam.abs().max()), "n_released": nout,
                         "seconds": time.time() - t0})
            if verbose:
                log(f"  raking sweep {sweep:4d}: max rel miss {mrel:.3e}, {nbad} cells above "
                    f"tolerance, max|lam| {float(lam.abs().max()):.2f}, releasing {nout}")
            if nout:
                released |= out
                act &= ~out
                lam = lam * act
                s = scores(lam)
                continue
            if nbad == 0:
                converged = True
                break
            if sweep == max_sweeps or (time_limit and time.time() - t0 > time_limit):
                break
        for j in range(Ac.m):
            cj = Ac.cols[:, j].long()
            q = shares(s)
            a = torch.zeros(C + 1, dtype=torch.float64, device=dev)
            a.index_add_(0, cj.view(-1), (W[:, None] * q).reshape(-1))
            ok = torch.cat([act, act.new_zeros(1)]) & (slot_of == j)
            d = torch.where(ok & (a > 0) & (tt > 0), torch.log(tt / a.clamp_min(1e-300)),
                            torch.zeros_like(a))
            d = torch.where(ok & (a <= 0) & (tt > 0), torch.full_like(a, step_max), d)
            d = d.clamp(-step_max, step_max)
            d[C] = 0.0
            lam = lam + d[:C]
            s = s + d[cj].view(Ac.N, Ac.K)
    q = shares(scores(lam))
    miss = (Ac.ATz(W[:, None] * q) - t) * act
    info = {"method": "raking", "converged": converged, "sweeps": hist[-1]["sweep"] if hist else 0,
            "seconds": time.time() - t0, "n_released_lam_max": int(released.sum()),
            "final_max_rel_miss_active": hist[-1]["max_rel_miss"] if hist else None,
            "history": hist, "released": released, "newton_iters": 0, "cg_iters": 0}
    return lam, q, miss, info


MU_PATH = [1e-1, 1e-2, 1e-3, 1e-4, 1e-5, 1e-6, 1e-7, 1e-8, 1e-9, 1e-10,
           1e-11, 1e-12, 0.0]


def solve_dual(Ac, W, t, alive, act, rho=30.0, kappa=1.0, lam0=None, tol_rel=1e-12,
               tol_abs=1e-17, max_newton=150, stage_newton=25, max_cg=1000,
               mus=MU_PATH, lam_max=50.0, verbose=True):
    """Newton-CG on the dual, along a path of decreasing ridge terms.
    W, t normalised (sum W = 1); act [C] bool: cells in the solve. Minimises
        F(lam) = sum_i W_i log sum_j exp(lam . a_ij) - lam . t
                 + (kappa / 2) sum_c (|lam_c| - rho)_+^2
                 + (mu / 2) sum_c s_c lam_c^2,
    for mu in `mus` (each stage warm-started from the last; the last is 0),
    with s_c the cell's target (floored at the smallest positive target).
    The kappa term is the dual of a primal penalty on missing a target:
    rho nats per unit of weight missed plus a small quadratic. Any set of
    targets that can be hit with multipliers inside [-rho, rho] is hit
    exactly; a target that can only be approached by pushing some draws'
    weights to exactly zero is hit to within about exp(-rho) of its value;
    a target that cannot be hit together with the others is missed, and the
    penalty concentrates the misses on few cells (the L1 property). The mu
    term lets each target be missed by a share mu*|lam_c| of itself; walking
    mu down to zero keeps every Newton step in a region where the quadratic
    model is good. Returns lam, q [N, K], miss [C] (achieved - target), info."""
    dev = Ac.device
    C = Ac.C
    actf = act.to(torch.float64)
    lam = torch.zeros(C, dtype=torch.float64, device=dev) if lam0 is None else lam0.clone() * actf
    tA = t * actf
    tpos = t[(t > 0) & act]
    sfloor = float(tpos.min()) if len(tpos) else 1.0
    sc = torch.clamp(t, min=sfloor) * actf
    seg, prec, pcell, pcnt = Ac.pairs(alive)
    # seg stays int32 [R, m] (pair id per entry, -1 for dead draws/padding)
    U = len(prec)
    Wp = W[prec]
    hasw = W > 0

    def evaluate(lam, mu):
        s = Ac.Av(lam).masked_fill(~alive, -float("inf"))
        lse = torch.logsumexp(s, 1)
        lse = torch.where(hasw, lse, torch.zeros_like(lse))
        q = torch.exp(s - lse[:, None]).masked_fill(~alive, 0.0)
        q = torch.where(hasw[:, None], q, torch.zeros_like(q))
        over = (lam.abs() - rho).clamp_min(0) * actf
        f = ((W * lse).sum() - (tA * lam).sum() + 0.5 * kappa * (sc * over * over).sum()
             + 0.5 * mu * (sc * lam * lam).sum())
        return f, q

    def gradient(lam, q, mu):
        w = W[:, None] * q
        miss = (Ac.ATz(w) - tA) * actf
        over = (lam.abs() - rho).clamp_min(0) * actf
        return miss + kappa * sc * over * torch.sign(lam) + mu * sc * lam, miss, w

    def diag_hess(q):
        qe = q.reshape(-1)[:, None].expand(-1, Ac.m)
        p = torch.zeros(U, dtype=torch.float64, device=dev)
        for s, e in Ac.chunks():
            sg = seg[s:e]
            ok = sg >= 0
            p.index_add_(0, sg[ok].long(), qe[s:e][ok])
        d = torch.zeros(C + 1, dtype=torch.float64, device=dev)
        d.index_add_(0, pcell, Wp * (p - p * p).clamp_min(0))
        return d[:C] * actf

    def hvp(q, w, v):
        u = Ac.Av(v * actf).masked_fill(~alive, 0.0)
        ubar = (q * u).sum(1, keepdim=True)
        return Ac.ATz(w * (u - ubar)) * actf

    hist = []
    t0 = time.time()
    total_cg = 0
    converged = False
    max_step = 20.0      # largest change in any multiplier per iteration (nats)
    dropped = torch.zeros_like(act)
    for si, mu in enumerate(mus):
        final = si == len(mus) - 1
        thr = (tol_rel if final else max(tol_rel, mu)) * t + tol_abs
        n_it = max_newton if final else stage_newton
        f, q = evaluate(lam, mu)
        g, miss, w = gradient(lam, q, mu)
        damp = 1e-4
        stall = 0
        stage_conv = False
        for it in range(n_it + 1):
            viol = (g.abs() > thr) & act
            nviol = int(viol.sum())
            gmax = float(g.abs().max())
            rel = float((g.abs() / torch.where(t > 0, t, torch.ones_like(t))).max())
            mrel = float((miss.abs() / torch.where(t > 0, t, torch.ones_like(t))).max())
            nover = int(((lam.abs() > rho) & act).sum())
            hist.append({"stage": si, "mu": mu, "iter": it, "f": float(f), "max_abs_grad": gmax,
                         "max_rel_grad": rel, "max_rel_miss": mrel, "n_cells_above_tol": nviol,
                         "n_beyond_rho": nover, "max_abs_lambda": float(lam.abs().max()),
                         "cg": total_cg, "seconds": time.time() - t0})
            if verbose:
                log(f"  mu={mu:.0e} newton {it:3d}  F={float(f):.15e}  max|grad|={gmax:.3e} "
                    f"rel={rel:.2e} rel_miss={mrel:.2e} above_tol={nviol} beyond_rho={nover} "
                    f"max|lam|={float(lam.abs().max()):.2f} cg={total_cg}")
            if nviol == 0:
                stage_conv = True
                break
            if it == n_it:
                break
            overm = ((lam.abs() > rho) & act).to(torch.float64)
            D = diag_hess(q) + kappa * sc * overm + mu * sc
            delta = damp
            Minv = torch.where(act & (D > 0), 1.0 / (D * (1 + delta)), torch.zeros_like(D))
            eta = min(0.1, gmax ** 0.5)
            x = torch.zeros_like(g)
            r = -g.clone()
            z = Minv * r
            pdir = z.clone()
            rz = (r * z).sum()
            r0 = float(r.norm())
            k = 0
            for k in range(max_cg):
                Hp = hvp(q, w, pdir) + (kappa * sc * overm + mu * sc + delta * D) * pdir
                pHp = (pdir * Hp).sum()
                if float(pHp) <= 0:
                    break
                a = rz / pHp
                x = x + a * pdir
                r = r - a * Hp
                if float(r.norm()) <= eta * r0:
                    k += 1
                    break
                z = Minv * r
                rz_new = (r * z).sum()
                pdir = z + (rz_new / rz) * pdir
                rz = rz_new
            total_cg += k + 1
            if float((x * g).sum()) >= 0:
                x = -Minv * g
            xmax = float(x.abs().max())
            capped = xmax > max_step
            if capped:
                # clip each multiplier's change separately, so one runaway
                # cell does not shrink the step of all the others
                x = x.clamp(-max_step, max_step)
                if float((x * g).sum()) >= 0:
                    x = (-Minv * g).clamp(-max_step, max_step)
            gx = float((x * g).sum())
            step = 1.0
            accepted = False
            for _ in range(50):
                fn, qn = evaluate(lam + step * x, mu)
                if float(fn) <= float(f) + 1e-4 * step * gx:
                    accepted = True
                    break
                step *= 0.5
            if not accepted:
                # F is flat to floating-point resolution: accept the full step
                # if it lowers the largest gradient, otherwise count a stall.
                fn, qn = evaluate(lam + x, mu)
                gn, _, _ = gradient(lam + x, qn, mu)
                if float(gn.abs().max()) < gmax:
                    accepted, step = True, 1.0
            if not accepted:
                stall += 1
                damp = min(damp * 10, 1e6)
                if stall >= 3:
                    break
                continue
            if step == 1.0 and not capped:
                damp = max(damp / 4, 1e-10)
            elif step < 0.5:
                damp = min(damp * 4, 1e6)
            stall = 0
            lam = lam + step * x
            f, q = fn, qn
            g, miss, w = gradient(lam, q, mu)
        # Cells whose multiplier has run past lam_max are ones the ridge is
        # holding off a target that cannot be reached: a tilt of exp(lam_max)
        # is beyond any reasonable reweighting. Release them (they leave the
        # solve) and carry on down the path; this keeps every multiplier
        # bounded, so Newton stays well behaved.
        if lam_max is not None and not final:
            out = act & (lam.abs() > lam_max)
            nout = int(out.sum())
            if nout:
                dropped |= out
                act = act & ~out
                actf = act.to(torch.float64)
                tA = t * actf
                sc = sc * actf
                lam = lam * actf
                log(f"  mu={mu:.0e}: released {nout} cells with |lambda| > {lam_max:g}")
        if final:
            converged = stage_conv
    info = {"converged": converged, "newton_iters": sum(1 for h in hist) - len(mus),
            "cg_iters": total_cg, "seconds": time.time() - t0,
            "n_beyond_rho": int(((lam.abs() > rho) & act).sum()),
            "final_max_rel_grad": hist[-1]["max_rel_grad"],
            "final_max_rel_miss_active": hist[-1]["max_rel_miss"],
            "n_released_lam_max": int(dropped.sum()), "released": dropped,
            "history": hist}
    return lam, q, miss, info


# --------------------------------------------------------------------------
# Top level
# --------------------------------------------------------------------------

def calibrate_matrix(A, W, t, K, device=None, cell_meta=None, tol_rel=1e-12,
                     mu_final=1e-10, miss_rel=1e-6, max_newton=40, max_rounds=20,
                     polish=True, lam0=None, draw_mask=None, mus=None, method="raking",
                     rake_tol=1e-7, max_sweeps=400, rake_time=900, solve_time=2700, good_enough=1e-9,
                     newton_from=float("inf"),
                     verbose=True):
    """Minimum-KL weights from an already-built contribution matrix.
    A: scipy CSR [N*K, C] record-major, or an int array [N*K, m] of the cell
    ids each draw falls in (a_common.contrib_index); W [N] record weights;
    t [C] targets. cell_meta: optional DataFrame (one row per cell) with
    wave / family / group / variable / bin columns (block check and report).
    lam0: optional starting multipliers [C].

    Steps: presolve (zero cells, single-cell bounds); the block check
    (exact supply-and-demand feasibility of each wave x family x group x
    variable block); then the dual solved along a path of ridge terms down
    to mu_final, which lets a target be missed by a share mu*|lam| of itself.
    A cell still missed by more than miss_rel of its target is one that
    cannot be hit jointly with the rest: it gets status 'joint' and the
    solve is repeated without it. Finally a polish stage with no ridge at
    all; kept if it converges, otherwise the mu_final solution stands.
    Returns (w [N, K], lam [C], report)."""
    t_start = time.time()
    if device is None:
        device = "cuda" if torch.cuda.is_available() else "cpu"
    W = np.asarray(W, np.float64)
    t = np.asarray(t, np.float64)
    N = len(W)
    C = len(t)
    if np.isnan(W).any():
        raise ValueError(f"NaN in record weights ({np.isnan(W).sum()})")
    # A NaN target means the cell has no target (for example a group the
    # SCF does not have): it is left out of the problem, counted and listed
    # with status 'no_target'. It is never treated as zero.
    no_t = np.isnan(t)
    if no_t.any():
        log(f"  {int(no_t.sum())} cells have no target (NaN); left out with status 'no_target'")
        t = np.where(no_t, 0.0, t)
    if (W < 0).any() or (t < 0).any():
        raise ValueError("negative weight or target")
    Ac = Contrib(A, N, K, device, C=C)
    if no_t.any():
        remap = torch.arange(C + 1, dtype=torch.int32, device=device)
        remap[torch.from_numpy(np.where(no_t)[0]).to(device)] = C
        for s, e in Ac.chunks():
            Ac.cols[s:e] = remap[Ac.cols[s:e].long()]
        Ac.padded = True
    S = W.sum()
    Wn = torch.from_numpy(W / S).to(device)
    tn = torch.from_numpy(t / S).to(device)
    log(f"calibrate: N={N} K={K} C={C} entries per draw={Ac.m} padded={Ac.padded} device={device}")

    dm = None if draw_mask is None else torch.as_tensor(np.asarray(draw_mask, bool)).to(device)
    if dm is not None and bool(((~dm.any(1)) & (Wn > 0)).any()):
        raise ValueError("a record with positive weight has no draws in draw_mask")
    status, alive, pinfo = presolve(Ac, Wn, tn, max_tight_rounds=0, draw_mask=dm)
    status[no_t] = "no_target"
    status, nblock = block_check(Ac, Wn, tn, alive, status, cell_meta)
    log("  presolve statuses: " + ", ".join(f"{s}={int((status == s).sum())}" for s in STATUSES))
    lam = None if lam0 is None else torch.as_tensor(np.asarray(lam0), dtype=torch.float64, device=device)
    rounds = []
    polished = False

    def max_rel(miss_t, act_np):
        m = np.abs(miss_t.cpu().numpy()) * S
        sel = act_np & (t > 0)
        return float((m[sel] / t[sel]).max()) if sel.any() else 0.0

    def show_misses(miss_t, lam_t):
        if not verbose or cell_meta is None:
            return
        mabs = np.abs(miss_t.cpu().numpy()) * S
        top = np.argsort(-mabs)[:10]
        cols = [c for c in ("wave", "table", "family", "group", "variable", "bin_label", "n_records")
                if c in cell_meta.columns]
        tab = cell_meta.iloc[top][cols].copy()
        tab["status"] = status[top]
        tab["target"] = t[top]
        tab["miss"] = (miss_t.cpu().numpy() * S)[top]
        tab["lambda"] = lam_t.cpu().numpy()[top]
        tab["reach_any"] = pinfo["reach_any"][top] * S
        log("  largest misses:\n" + tab.to_string())

    t_solve = time.time()
    for rnd in range(max_rounds):
        # Cells in the solve: the active ones, and relaxed zero cells (target
        # 0: the solver keeps as little weight there as the records allow).
        act_np = np.isin(status, ["active", "zero_relaxed"])
        act = torch.from_numpy(act_np).to(device)
        if method == "raking":
            # Raking first (robust far from the solution), then Newton from
            # where raking stopped (fast close to it). Keep whichever ends
            # nearer the targets.
            try:
                lam, q, miss, sinfo = solve_ipf(Ac, Wn, tn, alive, act, lam0=lam,
                                                tol_rel=max(tol_rel, rake_tol), max_sweeps=max_sweeps,
                                                time_limit=rake_time, verbose=verbose)
            except RuntimeError as e:
                log(f"  raking not possible ({e}); using the Newton path instead")
                method = "newton"
                continue
            relc = sinfo.pop("released").cpu().numpy()
            status[relc & (status == "active")] = "joint"
            sinfo["stage"] = f"round {rnd + 1}: raking"
            rounds.append(sinfo)
            act_np = np.isin(status, ["active", "zero_relaxed"])
            act = torch.from_numpy(act_np).to(device)
            r_rake = max_rel(miss, act_np)
            if r_rake > newton_from:
                # Raking is still far off somewhere: Newton from here wastes
                # time (its multipliers run away); keep raking instead.
                log(f"  round {rnd + 1}: largest relative miss after raking {r_rake:.2e}; "
                    f"more raking (Newton only below {newton_from:g})")
                if time.time() - t_solve > solve_time:
                    log("  time limit for the solve reached")
                    break
                continue
            lamN, qN, missN, ninfo = solve_dual(Ac, Wn, tn, alive, act, kappa=0.0, lam0=lam,
                                                tol_rel=tol_rel, max_newton=max_newton, mus=[0.0],
                                                verbose=verbose)
            ninfo.pop("released")
            ninfo["stage"] = f"round {rnd + 1}: Newton from raking"
            rounds.append(ninfo)
            r_newt = max_rel(missN, act_np)
            l1_rake = float(miss[act].abs().sum())
            l1_newt = float(missN[act].abs().sum())
            log(f"  round {rnd + 1}: largest relative miss after raking {r_rake:.2e}, "
                f"after Newton {r_newt:.2e} (Newton converged: {ninfo['converged']})")
            if l1_newt < l1_rake and r_newt < r_rake:
                lam, q, miss = lamN, qN, missN
            if ninfo["converged"] or min(r_newt, r_rake) <= good_enough:
                polished = True
                break
            if time.time() - t_solve > solve_time:
                log("  time limit for the solve reached")
                break
        else:
            mus_r = path0 if (rnd == 0 and lam0 is None) else [m for m in path0 if m <= 1e-6]
            lam, q, miss, sinfo = solve_dual(Ac, Wn, tn, alive, act, kappa=0.0, lam0=lam,
                                             tol_rel=tol_rel, max_newton=max_newton, mus=mus_r,
                                             verbose=verbose)
            relc = sinfo.pop("released").cpu().numpy()
            status[relc & (status == "active")] = "joint"
            sinfo["stage"] = f"round {rnd + 1}: Newton path"
            rounds.append(sinfo)
            if sinfo["converged"]:
                polished = True
                break
    show_misses(miss, lam)
    # Cells still off target when the solver stopped: listed as 'missed'
    # (not proven infeasible; the solver ran out of iterations or time).
    mn = np.abs(miss.cpu().numpy()) * S
    missed = (status == "active") & (mn > miss_rel * t + 1e-17 * S)
    status[missed] = "missed"
    log(f"  solve finished: exact={polished}; {int(missed.sum())} active cells still off target "
        f"by more than {miss_rel:g} of their target (status 'missed')")
    w = (Wn[:, None] * q * S).cpu().numpy()
    lam_np = lam.cpu().numpy()
    achieved = Ac.ATz(torch.from_numpy(w).to(device)).cpu().numpy()
    Kvec = np.full(N, K) if draw_mask is None else np.asarray(draw_mask, bool).sum(1)
    rep = report_from(w, W, t, achieved, status, Kvec, cell_meta)
    rep.update({"presolve": {k: v for k, v in pinfo.items() if k not in ("reach_any", "forced")},
                "cells_released_by_block_check": nblock,
                "solve_rounds": rounds,
                "converged": bool(polished) and not (status == "missed").any(),
                "polished_exact": polished,
                "method": method,
                "tol_rel": tol_rel, "miss_rel": miss_rel,
                "newton_iters_total": int(sum(r["newton_iters"] for r in rounds)),
                "cg_iters_total": int(sum(r["cg_iters"] for r in rounds)),
                "seconds_total": time.time() - t_start,
                "device": str(device),
                "gpu": torch.cuda.get_device_name(0) if str(device).startswith("cuda") else None,
                "max_abs_lambda": float(np.abs(lam_np).max()),
                "gpu_peak_memory_gb": (torch.cuda.max_memory_allocated() / 1e9) if str(device).startswith("cuda") else None})
    rep["cell_status"] = list(status)
    rep["reach_any"] = (pinfo["reach_any"] * S).tolist()
    rep["forced"] = (pinfo["forced"] * S).tolist()
    return w, lam_np, rep


def report_from(w, W, t, achieved, status, K, cell_meta=None):
    N = len(W)
    S = W.sum()
    q = np.divide(w, W[:, None], out=np.zeros_like(w), where=W[:, None] > 0)
    with np.errstate(divide="ignore", invalid="ignore"):
        term = np.where(q > 0, q * np.log(np.asarray(K, np.float64).reshape(-1, 1) * q), 0.0)
    kl = float((W * term.sum(1)).sum() / S)
    eff = np.divide(1.0, (q ** 2).sum(1), out=np.zeros(N), where=W > 0)
    err = achieved - t
    act = status == "active"
    rec_err = np.abs(w.sum(1) - W)
    rep = {
        "n_records": int(N), "K": int(np.max(K)), "n_cells": int(len(t)),
        "total_weight": float(S),
        "status_counts": {s: int((status == s).sum()) for s in STATUSES},
        "target_weight_by_status": {s: float(t[status == s].sum()) for s in STATUSES},
        "feasible_max_abs_error": float(np.abs(err[act]).max()) if act.any() else 0.0,
        "feasible_mean_abs_error": float(np.abs(err[act]).mean()) if act.any() else 0.0,
        "feasible_max_rel_error": float((np.abs(err[act]) / t[act]).max()) if act.any() else 0.0,
        "zero_cells_max_abs_error": float(np.abs(err[status == "zero"]).max()) if (status == "zero").any() else 0.0,
        "all_cells_max_abs_error": float(np.abs(err).max()),
        "all_cells_mean_abs_error": float(np.abs(err).mean()),
        "record_total_max_abs_error": float(rec_err.max()),
        "record_total_max_rel_error": float((rec_err / np.where(W > 0, W, 1)).max()),
        "kl_nats_per_unit_weight": kl,
        "effective_draws_weighted_mean": float((W * eff).sum() / S),
        "effective_draws_unweighted_mean": float(eff.mean()),
        "effective_draws_quantiles_unweighted": {str(p): float(np.quantile(eff, p)) for p in (0.01, 0.05, 0.1, 0.25, 0.5)},
        "share_records_below4_effective_draws_weighted": float(W[eff < 4].sum() / S),
        "share_records_below4_effective_draws_unweighted": float((eff < 4).mean()),
        "share_records_below2_effective_draws_weighted": float(W[eff < 2].sum() / S),
        "max_draw_share_of_record_weight": float(q.max()),
    }
    # How close the positive-target cells are, whatever their status: share
    # of cells (and of target weight) within each relative tolerance.
    pos = t > 0
    relerr = np.abs(err[pos]) / t[pos]
    rep["positive_cells_within_tolerance"] = {
        f"{tol:g}": {"share_of_cells": float((relerr <= tol).mean()) if pos.any() else 0.0,
                     "share_of_target_weight": float(t[pos][relerr <= tol].sum() / t[pos].sum())
                     if pos.any() else 0.0}
        for tol in (1e-9, 1e-6, 1e-3, 1e-2, 0.1)}
    rep["total_abs_miss_share_of_weight"] = float(np.abs(err[pos]).sum() / S) if pos.any() else 0.0
    rep["weight_in_zero_target_cells_share"] = float(achieved[t == 0].sum() / S)
    if cell_meta is not None:
        bad = ~act
        out = {}
        for col in ("family", "variable", "bin_label"):
            if col in cell_meta.columns:
                tab = {}
                for key, idx in cell_meta.groupby(col).indices.items():
                    st = status[idx]
                    tab[str(key)] = {s: int((st == s).sum()) for s in STATUSES if (st == s).any()}
                    tab[str(key)]["target_weight_infeasible"] = float(
                        t[idx][np.isin(st, ["unreachable", "above_reach", "block", "below_forced", "joint", "missed", "zero_relaxed"])].sum())
                    tab[str(key)]["target_weight_total"] = float(t[idx].sum())
                out[f"by_{col}"] = tab
        rep["status_by"] = out
        # Error relative to the group total (the sum over the bins of the
        # same wave, family, group and variable), where the columns exist.
        gcols = [c for c in ("wave", "family", "group", "variable") if c in cell_meta.columns]
        if len(gcols) == 4:
            gt = cell_meta.assign(_t=t).groupby(gcols)["_t"].transform("sum").to_numpy()
            rel = np.abs(err) / np.where(gt > 0, gt, np.nan)
            rep["feasible_max_error_share_of_group"] = float(np.nanmax(rel[act])) if act.any() else 0.0
            rep["all_cells_max_error_share_of_group"] = float(np.nanmax(rel))
            infeas = np.isin(status, ["unreachable", "above_reach", "block", "below_forced", "joint", "missed", "zero_relaxed"])
            rep["infeasible_cells_mean_error_share_of_group"] = float(np.nanmean(rel[infeas])) if infeas.any() else 0.0
    return rep


def calibrate(draws_tokens, records, targets, contrib_fn=None, **opts):
    """Calibrate draws [N, K, 24] of `records` to `targets` (A's tables).
    Uses A's contribution builder unless contrib_fn is given."""
    if contrib_fn is None:
        sys.path.insert(0, HERE)
        from a_common import contrib_index as contrib_fn
    draws_tokens = np.asarray(draws_tokens)
    N, K = draws_tokens.shape[:2]
    if N != len(records):
        raise ValueError(f"{N} records of draws, {len(records)} in the records table")
    A = contrib_fn(draws_tokens, records)
    W = records["weight"].to_numpy(np.float64)
    tcol = _target_col(targets)
    t = targets[tcol].to_numpy(np.float64)
    if "cell" in targets.columns and not np.array_equal(targets["cell"].to_numpy(), np.arange(len(targets))):
        raise ValueError("targets must be in cell-id order")
    return calibrate_matrix(A, W, t, K, cell_meta=targets, **opts)


def _target_col(targets):
    for c in ("target", "target_weight", "weight", "t"):
        if c in targets.columns:
            return c
    raise KeyError(f"no target column among {list(targets.columns)}")


# --------------------------------------------------------------------------
# Self-test on fabricated data
# --------------------------------------------------------------------------

def fabricate(seed=0, N=3000, K=8, include_truth=True, n_vars=5, n_bins=8):
    """Fabricated records, groups and draws in the same layout as A's:
    families partition records into groups; a cell is (wave, family, group,
    variable, bin). One family is an exact copy of another (collinear
    columns). Returns (tokens [N,K,V] bins, records DataFrame, targets
    DataFrame, contrib_fn)."""
    import pandas as pd
    rng = np.random.default_rng(seed)
    fam_sizes = {"inc": 5, "age": 6, "mar": 2, "x_a": 4}
    rec = pd.DataFrame({"wave": rng.integers(0, 2, N),
                        "weight": rng.lognormal(8, 1, N)})
    for f, G in fam_sizes.items():
        rec[f] = rng.integers(0, G, N)
    rec["inc_copy"] = rec["inc"]                 # exact duplicate family
    fams = list(fam_sizes) + ["inc_copy"]
    # Truth: bin depends on the income group; tail bins rare.
    base = rng.normal(size=(N, n_vars)) + 0.6 * rec["inc"].to_numpy()[:, None]
    truth = np.clip(np.floor(base + 3), 0, n_bins - 1).astype(np.int16)
    # Draws: noisy versions of a shifted truth (a model that is "off").
    noise = rng.normal(scale=1.0, size=(N, K, n_vars)) + 0.3
    draws = np.clip(np.floor(base[:, None, :] + 3 + noise), 0, n_bins - 1).astype(np.int16)
    if include_truth:
        draws[:, 0, :] = truth
    # Cells
    rows = []
    for wv in range(2):
        for f in fams:
            for g in range(rec[f].max() + 1):
                for v in range(n_vars):
                    for b in range(n_bins):
                        rows.append((wv, f, g, f"v{v}", b))
    cells = pd.DataFrame(rows, columns=["wave", "family", "group", "variable", "bin"])
    cid = {k: i for i, k in enumerate(map(tuple, cells.to_numpy()))}
    # Index arithmetic for the cell of (record, family, var, bin)
    ngroups = {f: int(rec[f].max() + 1) for f in fams}
    base_idx = np.zeros((N, len(fams), n_vars), np.int64)
    # vectorised: cell id of (wave, f, g, v, 0)
    first = {}
    for k, i in cid.items():
        if k[4] == 0:
            first[(k[0], k[1], k[2], k[3])] = i
    wv = rec["wave"].to_numpy()
    for fi, f in enumerate(fams):
        g = rec[f].to_numpy()
        for v in range(n_vars):
            base_idx[:, fi, v] = [first[(a, f, b, f"v{v}")] for a, b in zip(wv, g)]

    def contrib_fn(tok, records):
        n, k, nv = tok.shape
        idx = base_idx[:, None, :, :] + tok[:, :, None, :].astype(np.int64)   # [N,K,F,V]
        idx = np.sort(idx.reshape(n * k, -1), axis=1)
        R, m = idx.shape
        return sp.csr_matrix((np.ones(R * m), idx.reshape(-1), np.arange(0, R * m + 1, m)),
                             shape=(R, len(cells)))

    A_truth = contrib_fn(truth[:, None, :], rec)
    cells["target"] = A_truth.T @ rec["weight"].to_numpy()
    return draws, rec, cells, contrib_fn


def selftest(outdir):
    os.makedirs(outdir, exist_ok=True)
    results = {}
    # 1. Feasible by construction (the truth is one of the draws).
    draws, rec, cells, cf = fabricate(seed=1, include_truth=True)
    w, lam, rep = calibrate(draws, rec, cells, contrib_fn=cf)
    print_summary("feasible (truth among draws)", rep)
    ok1 = (rep["converged"] and rep["feasible_max_rel_error"] < 1e-11
           and rep["record_total_max_rel_error"] < 1e-13
           and rep["status_counts"]["unreachable"] == 0
           and rep["status_counts"]["zero_relaxed"] == 0
           and rep["status_counts"]["joint"] == 0)
    results["feasible"] = {"pass": bool(ok1), **{k: rep[k] for k in SUMMARY_KEYS}}
    # Independent check of the target error from scratch, on the CPU.
    A = cf(draws, rec)
    ach = A.T @ w.reshape(-1)
    act = np.array(rep["cell_status"]) == "active"
    t = cells["target"].to_numpy()
    results["feasible"]["cpu_check_max_rel_error"] = float((np.abs(ach - t)[act] / t[act]).max())
    results["feasible"]["zero_cells_weight"] = float(ach[t == 0].sum())

    # 2. Infeasible pieces: no truth draw (tail cells unreachable, some
    # records with every draw in a zero cell), plus two doctored targets.
    draws, rec, cells, cf = fabricate(seed=2, include_truth=False)
    t = cells["target"].to_numpy().copy()
    pos = np.where(t > 0)[0]
    rng = np.random.default_rng(5)
    doctored = rng.choice(pos, 2, replace=False)
    t[doctored[0]] *= 50.0          # far more than any record set can supply
    t[doctored[1]] *= 1.3
    cells["target"] = t
    w, lam, rep = calibrate(draws, rec, cells, contrib_fn=cf)
    print_summary("infeasible pieces (no truth draw, doctored targets)", rep)
    st = np.array(rep["cell_status"])
    results["infeasible"] = {"pass": bool(rep["feasible_max_rel_error"] < 1e-11
                                          and rep["record_total_max_rel_error"] < 1e-13),
                             "doctored_status": [st[d] for d in doctored],
                             **{k: rep[k] for k in SUMMARY_KEYS}}
    # 3. Small K with the truth as draw 0: still feasible, but most of the
    # solution sits on the boundary (many draws must get weight exactly
    # zero). Uses the index-array path of Contrib.
    draws, rec, cells, cf = fabricate(seed=1, include_truth=True)
    cf_idx = lambda tok, r: np.asarray(cf(tok, r).indices.reshape(tok.shape[0] * tok.shape[1], -1))
    for k in (2, 4):
        w, lam, rep = calibrate(draws[:, :k], rec, cells, contrib_fn=cf_idx)
        print_summary(f"feasible, K={k} (truth is draw 0)", rep)
        okk = (rep["converged"] and rep["feasible_max_rel_error"] < 1e-11
               and rep["status_counts"]["joint"] == 0)
        results[f"feasible_k{k}"] = {"pass": bool(okk), **{k2: rep[k2] for k2 in SUMMARY_KEYS}}
    with open(os.path.join(outdir, "selftest.json"), "w") as fh:
        json.dump(results, fh, indent=1, default=str)
    log("self-test: " + ", ".join(f"{k} pass={v.get('pass')}" for k, v in results.items()))


SUMMARY_KEYS = ["converged", "status_counts", "feasible_max_abs_error", "feasible_max_rel_error",
                "record_total_max_rel_error", "kl_nats_per_unit_weight",
                "effective_draws_weighted_mean", "share_records_below4_effective_draws_weighted",
                "newton_iters_total", "cg_iters_total", "seconds_total",
                "positive_cells_within_tolerance", "gpu_peak_memory_gb"]


def print_summary(name, rep):
    log(f"== {name}")
    for k in SUMMARY_KEYS:
        log(f"   {k}: {rep[k]}")
    if "status_by" in rep and "by_family" in rep["status_by"]:
        log(f"   by family: {rep['status_by']['by_family']}")


# --------------------------------------------------------------------------
# CLI
# --------------------------------------------------------------------------

def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--selftest", action="store_true")
    ap.add_argument("--a-config", default=None, choices=[None, "full"],
                    help="'full': call a_common.configure_full() (dataset full, $P/full/a)")
    ap.add_argument("--a-ckpt", default=None, help="checkpoint passed to configure_full")
    ap.add_argument("--target-set", default=None,
                    help="target set name (e.g. core, full); passed to A's loaders and "
                         "contrib_index when they accept it; default file targets_<set>.parquet")
    ap.add_argument("--draws", default=None, help="default: <A dir>/ref_draws.npz")
    ap.add_argument("--records", default=None, help="default: <A dir>/records.parquet")
    ap.add_argument("--targets", default=None, help="default: <A dir>/targets[_<set>].parquet")
    ap.add_argument("--k", type=int, nargs="+", default=[None],
                    help="use the first k draws of each record (several values allowed)")
    ap.add_argument("--tag", default="ref")
    ap.add_argument("--outdir", default=os.path.join(P, "b"))
    ap.add_argument("--save-weights", action="store_true",
                    help="write weights_<tag>[_K<k>].npz")
    ap.add_argument("--marker", default=None, help="write this DONE marker at the end")
    ap.add_argument("--mus", type=float, nargs="+", default=None, help="ridge path (debugging)")
    ap.add_argument("--max-rounds", type=int, default=20)
    ap.add_argument("--no-polish", action="store_true")
    ap.add_argument("--method", default="raking", choices=["raking", "newton"])
    args = ap.parse_args()
    if args.selftest:
        selftest(os.path.join(P, "dev", "b"))
        return
    import inspect
    import pandas as pd
    sys.path.insert(0, HERE)
    import a_common as A
    if args.a_config == "full":
        A.configure_full(ckpt=args.a_ckpt)
    adir = str(A.PA)
    ts = args.target_set
    draws = args.draws or os.path.join(adir, "ref_draws.npz")
    rec_path = args.records or os.path.join(adir, "records.parquet")
    tgt_path = args.targets or os.path.join(adir, f"targets_{ts}.parquet" if ts else "targets.parquet")
    os.makedirs(args.outdir, exist_ok=True)
    d = np.load(draws)
    tokens = d["tokens"]
    records = pd.read_parquet(rec_path)
    targets = pd.read_parquet(tgt_path)
    cfn = A.contrib_index
    if ts and "target_set" in inspect.signature(cfn).parameters:
        cfn = lambda tok, recs, _f=A.contrib_index: _f(tok, recs, target_set=ts)
    log(f"A dir {adir}; draws {draws}: tokens {tokens.shape}; records {rec_path} ({len(records)}); "
        f"targets {tgt_path} ({len(targets)} cells)")
    for k in args.k:
        kk = tokens.shape[1] if k is None else k
        if kk > tokens.shape[1]:
            log(f"skip K={kk}: only {tokens.shape[1]} draws")
            continue
        name = args.tag if k is None else f"{args.tag}_K{kk}"
        opts = {"max_rounds": args.max_rounds, "polish": not args.no_polish, "method": args.method}
        if args.mus:
            opts.update(mus=args.mus, mu_final=min(args.mus))
        w, lam, rep = calibrate(tokens[:, :kk], records, targets, contrib_fn=cfn, **opts)
        rep.update({"draws_file": draws, "records_file": rec_path, "targets_file": tgt_path,
                    "target_set": ts, "a_config": args.a_config, "tag": name})
        print_summary(name, rep)
        cid = targets["cell"].to_numpy() if "cell" in targets.columns else np.arange(len(targets))
        if args.save_weights or k is None:
            np.savez(os.path.join(args.outdir, f"weights_{name}.npz"), w=w, **{"lambda": lam},
                     cell_id=cid, cell_status=np.array(rep["cell_status"]))
        with open(os.path.join(args.outdir, f"report_{name}.json"), "w") as fh:
            json.dump(rep, fh, indent=1, default=float)
        log(f"wrote report_{name}.json")
    if args.marker:
        open(os.path.join(args.outdir, args.marker), "w").write(time.strftime("%Y-%m-%d %H:%M:%S\n"))


if __name__ == "__main__":
    main()
