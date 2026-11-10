"""The integrated solve for a past year: the national weights and the state weights, solved together.

Each record gets one weight in each area: the 50 states, DC, the IRS's Other Areas, and Puerto Rico from 2018. A
record's national weight is the sum of its area weights. The solve moves the area weights from their start (the
record's starting weight times its starting share in each area) so that the year's national targets and the state
targets of state_targets.py are met together. It uses solver.py's cost: the misses from the targets in standard
errors, plus tau times the distance the weights move from the start, plus a penalty on weights above a soft cap.

A state target is a share. For an area, an AGI group and an item, the condition is: the area's amount of the item among
the group's records, less the IRS's share of the area times the file's national amount of the item in the group (the
sum over every area), is zero. The national amount is the file's own, at the weights the solve finds. The non-filer
targets by state and age band have the same form, with one group for the non-filers.

As in solver.py, the solve searches over one multiplier per target rather than over the weights themselves. Given the
multipliers, record i's weight in area s is

    w_is = w0_i x p0_is x exp(x_i' lambda_national + z_i' (lambda_s - mu_g)),

where w0_i is the record's starting weight, p0_is its starting share in area s, x_i and z_i its amounts of the items
of the national and the state targets, lambda_national the national targets' multipliers, lambda_s area s's
multipliers (zero for a cell with no target), and mu_g the group's K numbers, one per item: the sum over the areas of
the area's share times its multiplier. mu_g enters every area's exponent with a minus sign because every area's amount
is part of the national amount that each share condition subtracts. Newton's method finds the multipliers. Each Newton
step solves a linear system with one row per multiplier: about 1,300 national multipliers and about 21,000 state ones.

Without the mu_g terms, a state multiplier for one area and AGI group would move only the weights of that group's
records in that area, and the state part of the system would break into about 580 small blocks: one for each area and
AGI group, and one for each state's non-filers. The mu_g terms tie the areas of a group together. The tie is carried
by 2K extra unknowns for each group, added to the dense part of the system: the change in mu_g, and the change in the
group's national amounts of the K items. With these unknowns written out, each area's block depends only on its own
multipliers and on the group's extra unknowns, and the blocks are again small and independent. The blocks are solved
one at a time, which leaves a dense system the size of the national targets plus the extra unknowns (about 2,300
rows). That system is symmetric but not positive definite (the rows of the extra unknowns have zeros on the diagonal),
and it is factored by LU decomposition, not Cholesky. Records above their soft cap tie their areas together in a second
way; their terms are added afterward by the Woodbury identity. See FEATURES.md, "State weights".

When every area of a group has a target for an item and the shares sum to one, the item's conditions are not
independent: their sum is zero whatever the weights. The system stays solvable because each condition's standard
error adds a positive amount to its diagonal; the common part of the item's multipliers is then set by the standard
errors alone.
"""
import argparse
import json
import os
import pickle
import sys
import time

import numpy as np
import pandas as pd
import pyarrow as pa
import pyarrow.parquet as pq
import scipy.linalg as sl
import scipy.sparse as sp

from shared import common
from reweight import solver as SV
from reweight import sparse_table
from reweight import state_targets as ST
from reweight import state_weights as SW


# The gradient is the vector of the cost's slopes with respect to the multipliers; the solve stops when its largest
# entry is below the stopping size. A year that runs out of Newton steps with its largest gradient within NEAR times the
# stopping size is used and named in the log. Beyond that, the year's files are written and the job fails.
NEAR = 10.0
# At most this many records above their soft cap enter one Newton step's system: those whose cap changes the system
# most. The line search still checks every step against the full cost.
MAX_CAPPED = 20000
# The block that links national and state multipliers is built this many items at a time. This bounds the size of a
# temporary array of records x items x areas.
BORDER_ITEMS = 12
say = SW.say


# ---- Setting up the problem ----

def national_setup(w0, F, T, se, identity, averages, fixed, rho):
    """The national targets in the form the solve uses. Records that are held fixed (the Forbes list) are taken out:
    their amounts are subtracted from the targets, as solver.solve does. Each target's record amounts are divided by
    the target's size, on the same scale as solver._solve. Returns the scaled record amounts X (free records by
    targets), the scaled targets t, the standard errors, and what the targets that are averages need. F and X are
    sparse matrices, which store only the entries that are not zero (sparse_table.py); X is in the layout by row."""
    F = sp.csc_matrix(F)
    fixed = np.asarray(fixed, bool)
    free = ~fixed
    m = len(T)
    averages = list(averages) if averages is not None else [None] * m
    if fixed.any():
        wf = w0[fixed]
        T_free = T - sparse_table.records(F, fixed).toarray(order='C').T @ wf   # the few fixed records, written out in full
        offset, extra = np.zeros(m), np.full(m, np.nan)
        av_free = [None] * m
        for j, a in enumerate(averages):
            if a is None:
                continue
            mem, v, mm = a
            mem, v = np.asarray(mem, bool), np.asarray(v, float)
            offset[j] = -float((wf * mem[fixed] * (v[fixed] - mm)).sum())
            extra[j] = float(wf[mem[fixed]].sum())
            av_free[j] = (mem[free], v[free], mm)
        size_of = np.abs(T).astype(float)
    else:
        T_free, offset, extra, av_free, size_of = T, None, None, averages, None
    G, Tl, size, member = SV._conditions(w0[free], sparse_table.records(F, free), T_free, av_free, offset, extra, size_of)
    W = float(w0[free].sum())
    X = sparse_table.scale_columns(G, W / size).tocsr()
    del G
    is_avg = np.array([mm is not None for mm in member])
    return dict(free=free, W=W, X=X, t=Tl / size, size=size, member=member, is_avg=is_avg, extra=extra, se=np.asarray(se, float),
                rho=np.asarray(rho, float), free_target=~np.asarray(identity, bool))


# ---- The dual problem and Newton's method ----

class Joint:
    """The integrated solve's dual problem: its value, its gradient, and Newton's method on it.

    The dual problem is a function of the multipliers alone. Given the multipliers, every area weight follows from the
    formula in the module docstring, and the dual's gradient for a target is where the target lands less its value
    (plus an allowance for the target's standard error). For a share condition, where the target lands is the area's
    amount less the share times the national amount, and its value is zero apart from the fixed records' amounts.
    Newton's method looks for the multipliers at which every gradient is zero. All amounts are on the solver's scale:
    weights are shares of the free records' total starting weight W, and each target is divided by its size.

    Inputs. w0_free: the free records' starting weights. P0 [records, areas]: their starting shares. nat: the national
    targets from national_setup; each group keeps its records' rows of X, with the targets its records reach, written
    out in full (a group reaches about half the targets, and the groups' tables together take about 2 GB). groups: one dict for each AGI group of filers and one for the non-filers, holding rows
    (the group's records among the free records), Z [records in the group, items] (the state items), T, se, share and
    fixed [areas, items] (the state targets at the starting weights, NaN where there is none; their standard errors;
    the IRS's shares; the amounts that fixed records already put there), and size [items] (the scale of each item's
    targets). tau: the weight on the move from the start. tol: the stopping size of the gradient. soft: (U, beta), each
    free record's soft cap and the cap's strength."""

    def __init__(self, w0_free, P0, nat, groups, tau, tol, soft):
        self.W, self.tau, self.tol, self.nat = nat['W'], float(tau), float(tol), nat
        self.mN = nat['X'].shape[1]
        self.A = P0.shape[1]
        self.p_rec = w0_free / self.W
        self.t_N = nat['t']
        self.groups = []
        n_state, n_aux = 0, 0
        self.dropped_no_holder = 0
        U, beta = soft
        U_s = U / self.W
        c_all = beta / (self.tau * U_s ** 2)      # the cap's strength c and the cap U per free record, on the solver's scale
        for g in groups:
            r = g['rows']
            Xg = nat['X'][r]
            cols = np.flatnonzero(sparse_table.reached(Xg))
            scale = self.W / g['size']
            Z = g['Z'] * scale[None, :]
            have = (Z != 0).any(0)
            mask = np.isfinite(g['T']) & have[None, :]
            self.dropped_no_holder += int((np.isfinite(g['T']) & ~have[None, :]).sum())
            # An item with no target in the group has no multiplier there. Each target with a multiplier gets a place
            # pos in the vector of state multipliers. A target with standard error se adds 0.5 x nu x lambda^2 to the
            # dual, with nu = tau x (se / size)^2: the larger the standard error, the further the target may be missed.
            keep = mask.any(0)
            Z, mask = Z[:, keep], mask[:, keep]
            g = {**g, 'T': g['T'][:, keep], 'se': g['se'][:, keep], 'fixed': g['fixed'][:, keep], 'size': g['size'][keep],
                 'share': g['share'][:, keep]}
            K = Z.shape[1]
            pos = np.full((self.A, K), -1, dtype=np.int64)
            pos[mask] = n_state + np.arange(int(mask.sum()))
            n_state += int(mask.sum())
            share = np.where(mask, np.nan_to_num(g['share']), 0.0)
            # The value of a share condition. The fixed records' amounts do not move: the condition on the free
            # records is the share times the fixed records' national amount of the item, less their amount in the
            # area, divided by the item's size.
            t = np.where(mask, (share * g['fixed'].sum(0)[None, :] - g['fixed']) / g['size'][None, :], 0.0)
            nu = np.where(mask, self.tau * (np.nan_to_num(g['se']) / g['size'][None, :]) ** 2, 0.0)
            assert (nu[mask] > 0).all(), 'a state target with no positive standard error'
            iu, ju = np.triu_indices(K)
            # The group's 2K extra unknowns in the dense part of the Newton system: yrows for the change in mu_g,
            # zrows for the change in the group's national amounts of the items. dense lists the dense rows the
            # group's blocks touch: its national targets, then its extra unknowns.
            yrows, zrows = n_aux + np.arange(K), n_aux + K + np.arange(K)
            n_aux += 2 * K
            dense = np.concatenate([cols, self.mN + yrows, self.mN + zrows])
            self.groups.append(dict(rows=r, X=Xg[:, cols].toarray(order='C'), cols=cols, Z=Z, Z2=Z[:, iu] * Z[:, ju], iu=iu, ju=ju, P=(w0_free[r, None] / self.W) * P0[r],
                                    p=self.p_rec[r], mask=mask, pos=pos, t=t, nu=nu, share=share, c=c_all[r], U=U_s[r],
                                    yrows=yrows, zrows=zrows, dense=dense))
        self.mS, self.mA = n_state, n_aux
        self.t = np.zeros(self.mN + self.mS)                # the targets: national first, then the state targets in order of pos
        self.t[:self.mN] = self.t_N
        for g in self.groups:
            self.t[self.mN + g['pos'][g['mask']]] = g['t'][g['mask']]
        self.nu = np.zeros_like(self.t)                      # each target's nu, in the same order
        for g in self.groups:
            self.nu[self.mN + g['pos'][g['mask']]] = g['nu'][g['mask']]
        self.steps, self.calls = 0, 0
        self.timing = {}

    # --- The national targets' standard errors. A target that is an average has its standard error on the average, so
    #     on the solver's scale it depends on the weight of the group the average is over. ---
    def scaled_se(self, w_free):
        n = self.nat
        gw = SV._group_weight(w_free, n['member'], n['extra'])
        return np.where(n['is_avg'], n['se'] * np.nan_to_num(gw), n['se']) / n['size'] * n['free_target']

    def set_national_penalty(self, s):
        n = self.nat
        self.nu_N = self.tau * s ** 2 / n['rho'] * n['free_target']
        self.nu[:self.mN] = self.nu_N

    # --- The vector of multipliers holds the national multipliers first, then the state multipliers in order of pos. ---
    def split(self, lam):
        """The national multipliers, and for each group a matrix [areas, items] of its state multipliers (0 where a
        cell has no target)."""
        out = []
        for g in self.groups:
            L = np.zeros_like(g['t'])
            L[g['mask']] = lam[self.mN + g['pos'][g['mask']]]
            out.append(L)
        return lam[:self.mN], out

    @staticmethod
    def exponents(g, L):
        """What each area's records get in their exponent from the state multipliers L [areas, items] of the group:
        the area's multipliers less mu_g, the sum over the areas of the share times the multiplier. Every area gets the
        mu_g term, including an area with no target for the item."""
        return L - (g['share'] * L).sum(0)[None, :]

    @staticmethod
    def by_condition(g, R):
        """Amounts by area and item, R [areas, items], as the group's share conditions see them: each cell's amount
        less its share of the sum over every area, at the cells with a target."""
        return (R - g['share'] * R.sum(0)[None, :])[g['mask']]

    def evaluate(self, lam):
        """The dual's value at the multipliers lam, and for each group the area weights E [records, areas] and the
        national weights e [records], on the solver's scale. The soft cap acts on a record's national weight; the
        record's area weights are then scaled to sum to it. Returns infinity if a weight overflows."""
        lN, ls = self.split(lam)
        val, fields = 0.0, []
        with np.errstate(over='ignore', invalid='ignore', divide='ignore', under='ignore'):
            for g, L in zip(self.groups, ls):
                zN = g['X'] @ lN[g['cols']]
                E = g['P'] * np.exp(zN[:, None] + g['Z'] @ self.exponents(g, L).T)
                Asum = E.sum(1)
                zt = np.maximum(np.log(Asum / g['p']), -700.0)
                if not np.isfinite(zt).all():
                    return np.inf, None
                e = SV._soft_weights(zt, g['p'], g['c'], g['U'])
                val += SV._soft_value(zt, e, g['p'], g['c'], g['U'])
                E *= (e / np.where(Asum > 0, Asum, 1.0))[:, None]
                if not np.isfinite(val):
                    return np.inf, None
                fields.append((E, e))
        val += -float(self.t @ lam) + 0.5 * float((self.nu * lam ** 2).sum())
        return val, fields

    def gradient(self, lam, fields):
        """For each target: where it lands under the weights in fields, less its value, plus nu x its multiplier. A
        share condition lands at the area's amount less the share times the national amount."""
        lN, ls = self.split(lam)
        out = np.zeros(self.mN + self.mS)
        for g, (E, e) in zip(self.groups, fields):
            np.add.at(out, g['cols'], g['X'].T @ e)
            R = (g['Z'].T @ E).T                            # [areas, items]: the free records' amounts by area
            out[self.mN + g['pos'][g['mask']]] = self.by_condition(g, R) - g['t'][g['mask']]
        out[:self.mN] -= self.t_N
        return out + self.nu * lam

    # --- The Newton step. The Hessian is the matrix of the dual's second derivatives with respect to the multipliers.
    #     A Newton step solves Hessian x step = -gradient. The Hessian is never formed in full: it is factored in parts
    #     (factor), and the parts are combined whenever the step needs the Hessian's inverse on a vector (inverse). ---
    @staticmethod
    def cholesky(M, attempts, first, grow, what):
        """The Cholesky factor of M, a lower triangular matrix L with L L' = M. Where M is not quite positive definite
        (rounding can cause this), a small amount is added to its diagonal (the "jitter") and the factor is tried
        again."""
        jitter = 0.0
        for attempt in range(attempts):
            try:
                return sl.cholesky(M + jitter * np.eye(len(M)), lower=True, check_finite=False), jitter
            except sl.LinAlgError:
                jitter = max(first, jitter * grow)
        raise RuntimeError(what)

    def capped(self, g, e):
        """The records of the group whose national weight e is above their soft cap, and for each the amount kappa =
        e x ce / (1 + ce) that the cap takes off the Hessian's curvature in that record's direction."""
        idx = np.flatnonzero(e > g['U'])
        ce = g['c'][idx] * e[idx]
        return idx, e[idx] * ce / (1.0 + ce)

    def factor(self, fields):
        """Factor the Hessian for one Newton step, in five steps.

        1. Assemble the Hessian without the soft cap's terms, with each group's 2K extra unknowns written out. The
           unknowns of the system are the national multipliers, then every group's extra unknowns (together the dense
           head, H), then the state multipliers. For a group with area blocks M_a (the curvature of the group's records
           in area a, items by items) and national amounts M_e = sum over the areas of M_a, the rows are:
           - a state multiplier of area a, item k: its block M_a plus nu on the diagonal (the state blocks); minus
             M_a's row k against the group's y unknowns; minus the cell's share against the group's z unknown of item k;
             and the border against the national multipliers, B;
           - the group's y unknowns (the change in mu_g): M_e against themselves; the identity against the z unknowns;
             minus the national-by-item curvature against the national multipliers;
           - the group's z unknowns (the change in the national amounts): zero against themselves.
           Eliminating y and z from this system gives back the Hessian of the multipliers.
        2. Scale every row and column so that the diagonal is 1 (a y unknown by the square root of M_e's diagonal, a z
           unknown by the inverse of that).
        3. Factor each state block and subtract its part from the dense head; this eliminates the state multipliers.
        4. Factor what remains of the dense head by LU decomposition; it is symmetric but not positive definite.
        5. Add the soft cap's terms (woodbury)."""
        t0 = time.time()
        mN, mS, mA = self.mN, self.mS, self.mA
        nD = mN + mA
        H = np.zeros((nD, nD))
        B = np.zeros((nD, mS))
        dS, dA = np.zeros(mS), np.zeros(mA)
        blocks = []
        for g, (E, e) in zip(self.groups, fields):
            X, Z, cols, mask, pos, nu, share = g['X'], g['Z'], g['cols'], g['mask'], g['pos'], g['nu'], g['share']
            yr, zr = mN + g['yrows'], mN + g['zrows']
            # The national part of the Hessian, X' diag(e) X, computed as Y'Y as in solver.py.
            Y = X * np.sqrt(e)[:, None]
            H[np.ix_(cols, cols)] += Y.T @ Y
            K = Z.shape[1]
            # the border between national and state multipliers, built a few items at a time to bound memory
            for k0 in range(0, K, BORDER_ITEMS):
                k1 = min(k0 + BORDER_ITEMS, K)
                m = mask[:, k0:k1].T
                if m.any():
                    R = (Z[:, k0:k1, None] * E[:, None, :]).reshape(len(E), -1)
                    B[np.ix_(cols, pos[:, k0:k1].T[m])] = (X.T @ R)[:, m.ravel()]
            # The group's extra unknowns: the national-by-item curvature on the national weights, M_e, and the
            # identity between the y and z unknowns.
            eZ = Z * e[:, None]
            XeZ = X.T @ eZ
            H[np.ix_(cols, yr)] -= XeZ
            H[np.ix_(yr, cols)] -= XeZ.T
            Me = Z.T @ eZ
            H[np.ix_(yr, yr)] += Me
            H[yr, zr] = 1.0
            H[zr, yr] = 1.0
            dA[g['yrows']] = np.sqrt(np.diag(Me)) + 1e-300
            dA[g['zrows']] = 1.0 / dA[g['yrows']]
            R = g['Z2'].T @ E
            full = np.zeros((K, K))
            for s in range(self.A):
                ks = np.flatnonzero(mask[s])
                if not len(ks):
                    continue
                full[g['iu'], g['ju']] = R[:, s]
                full[g['ju'], g['iu']] = R[:, s]
                D = full[np.ix_(ks, ks)].copy()
                D[np.diag_indices(len(ks))] += nu[s, ks]
                p = pos[s, ks]
                blocks.append(dict(g=g, s=s, ks=ks, p=p, D=D))
                dS[p] = np.diag(D)
                B[np.ix_(yr, p)] = -full[:, ks]
                B[zr[ks], p] = -share[s, ks]
        t1 = time.time()
        dN = np.sqrt(np.diag(H)[:mN] + self.nu_N) + 1e-300
        dD = np.concatenate([dN, dA])
        dS = np.sqrt(dS) + 1e-300
        H = H / dD[:, None] / dD[None, :]
        H[np.diag_indices(mN)] += self.nu_N / dN ** 2 + 1e-12
        B = B / dD[:, None] / dS[None, :]
        by_group = {}
        for b in blocks:
            D = b['D'] / dS[b['p'], None] / dS[b['p']][None, :]
            D[np.diag_indices(len(b['p']))] += 1e-12
            b['L'] = sl.cholesky(D, lower=True, check_finite=False)
            del b['D']
            rows = b['g']['dense']
            C = sl.solve_triangular(b['L'], B[np.ix_(rows, b['p'])].T, lower=True, check_finite=False)
            by_group.setdefault(id(b['g']), (rows, []))[1].append(C)
        for rows, Cs in by_group.values():
            C = np.vstack(Cs)
            H[np.ix_(rows, rows)] -= C.T @ C
        lu = sl.lu_factor(H, check_finite=False)
        assert np.isfinite(lu[0]).all() and (np.abs(np.diag(lu[0])) > 0).all(), 'the dense part of the Newton system is singular'
        t2 = time.time()
        self.F = dict(B=B, blocks=blocks, lu=lu, dN=dN, dA=dA, dS=dS)
        self.F['wood'] = self.woodbury(fields)
        self.timing = {'assemble': t1 - t0, 'eliminate': t2 - t1, 'woodbury': time.time() - t2}

    def block_solve(self, r):
        """The state blocks' inverse times r, block by block."""
        out = np.zeros_like(r)
        for b in self.F['blocks']:
            out[b['p']] = sl.cho_solve((b['L'], True), r[b['p']], check_finite=False)
        return out

    def arrow(self, r):
        """The inverse of the scaled Hessian without the soft cap's terms, times a vector r [national and state
        multipliers]. The system with the extra unknowns has the shape of an arrow: the dense head, the border, and
        the state blocks along the diagonal. The extra unknowns' right-hand sides are zero. Solve for the dense head
        first, then for the state part, and return the national and state parts of the answer."""
        F, mN, nD = self.F, self.mN, self.mN + self.mA
        rN, rS = r[:mN], r[mN:]
        v = self.block_solve(rS)
        rD = np.zeros(nD)
        rD[:mN] = rN
        xD = sl.lu_solve(F['lu'], rD - F['B'] @ v, check_finite=False)
        xS = self.block_solve(rS - F['B'].T @ xD)
        return np.concatenate([xD[:mN], xS])

    def woodbury(self, fields):
        """The soft cap's terms. A record above its cap ties its areas together: its term in the Hessian is not within
        one state block. These terms are few (one per capped record), so they are added by the Woodbury identity, a
        formula for the inverse of a matrix plus a correction of low rank. Returns the factor of
        I - sqrt(kappa) Y H^-1 Y' sqrt(kappa), where H is the scaled Hessian without the cap's terms and Y holds the
        capped records' directions. A capped record's direction for the state multiplier of area s and item k is its
        item amount times (its share in area s less the cell's share): the record's area shares as the share conditions
        see them."""
        F, mN, nD = self.F, self.mN, self.mN + self.mA
        recs = []
        for gi, (g, (E, e)) in enumerate(zip(self.groups, fields)):
            idx, kappa = self.capped(g, e)
            recs += [(float(kk), gi, int(i)) for kk, i in zip(kappa, idx)]
        r_all = len(recs)
        if r_all > MAX_CAPPED:
            recs = sorted(recs, reverse=True)[:MAX_CAPPED]
        by = {}
        for kk, gi, i in recs:
            by.setdefault(gi, []).append((i, kk))
        r = len(recs)
        if r == 0:
            return dict(r=0, r_all=r_all)
        at, parts = 0, []
        CY = np.zeros((nD, r))
        Gd = []
        blocks_of = {}
        for b in F['blocks']:
            blocks_of.setdefault(id(b['g']), []).append(b)
        for gi in sorted(by):
            g = self.groups[gi]
            E, e = fields[gi]
            idx = np.array([i for i, _ in by[gi]], dtype=np.int64)
            sk = np.sqrt(np.array([k for _, k in by[gi]]))
            q = E[idx] / e[idx, None]
            Zc, Xc = g['Z'][idx], g['X'][idx]
            rows = np.arange(at, at + len(idx))
            CY[np.ix_(g['cols'], rows)] = (Xc / F['dN'][g['cols']][None, :] * sk[:, None]).T
            GG = np.zeros((len(idx), len(idx)))
            for b in blocks_of.get(id(g), []):
                y = (q[:, b['s'], None] - g['share'][b['s'], b['ks']][None, :]) * Zc[:, b['ks']] / F['dS'][b['p']][None, :] * sk[:, None]    # [r_g, k]
                V = sl.cho_solve((b['L'], True), y.T, check_finite=False)                                                                # [k, r_g]
                GG += y @ V
                CY[np.ix_(g['dense'], rows)] -= F['B'][np.ix_(g['dense'], b['p'])] @ V
            Gd.append((rows, GG))
            parts.append(dict(gi=gi, idx=idx, sk=sk, q=q, rows=rows))
            at += len(idx)
        W = -(CY.T @ sl.lu_solve(F['lu'], CY, check_finite=False))
        del CY
        W = 0.5 * (W + W.T)
        for rows, GG in Gd:
            W[np.ix_(rows, rows)] -= GG
        W[np.diag_indices(r)] += 1.0
        LW, jitter = self.cholesky(W, 8, 1e-10, 10, 'the soft cap\'s system is not positive definite')
        return dict(r=r, r_all=r_all, parts=parts, LW=LW, jitter=jitter)

    def inverse(self, r):
        """The inverse of the scaled Hessian, with the soft cap's terms, times a scaled vector r (the Woodbury identity
        applied to arrow)."""
        F, wood = self.F, self.F['wood']
        v = self.arrow(r)
        if wood['r'] == 0:
            return v
        d = np.concatenate([F['dN'], F['dS']])
        lN, ls = self.split(v / d)
        a = np.zeros(wood['r'])
        for part in wood['parts']:
            g, idx = self.groups[part['gi']], part['idx']
            dz = (g['X'][idx] @ lN[g['cols']])[:, None] + g['Z'][idx] @ self.exponents(g, ls[part['gi']]).T
            a[part['rows']] = part['sk'] * (part['q'] * dz).sum(1)
        u = sl.cho_solve((wood['LW'], True), a, check_finite=False)
        back = np.zeros(self.mN + self.mS)
        for part in wood['parts']:
            g, idx = self.groups[part['gi']], part['idx']
            c = part['sk'] * u[part['rows']]
            np.add.at(back, g['cols'], g['X'][idx].T @ c)
            R = (g['Z'][idx].T @ (c[:, None] * part['q'])).T
            back[self.mN + g['pos'][g['mask']]] = self.by_condition(g, R)
        return v + self.arrow(back / d)

    def hess_vec(self, v, fields):
        """The full Hessian, unscaled and with every term, times a vector v. Used to check a Newton step."""
        lN, ls = self.split(v)
        out = np.zeros(self.mN + self.mS)
        for g, L, (E, e) in zip(self.groups, ls, fields):
            dz = (g['X'] @ lN[g['cols']])[:, None] + g['Z'] @ self.exponents(g, L).T
            de = E * dz
            idx, kappa = self.capped(g, e)
            if len(idx):
                q = E[idx] / e[idx, None]
                de[idx] -= (kappa * (q * dz[idx]).sum(1))[:, None] * q
            np.add.at(out, g['cols'], g['X'].T @ de.sum(1))
            R = (g['Z'].T @ de).T
            out[self.mN + g['pos'][g['mask']]] = self.by_condition(g, R)
        return out + self.nu * v

    def newton_step(self, g_vec, fields):
        """The Newton step for the gradient g_vec, and the size of the remaining error in solving for it."""
        self.factor(fields)
        d = np.concatenate([self.F['dN'], self.F['dS']])
        r0 = -g_vec / d
        x = self.inverse(r0)
        # Eliminating the state blocks subtracts nearly equal numbers, because a national total is the sum of its
        # areas' amounts. A few refinement steps correct the step: each solves again for the part of the system the
        # step still misses.
        res_size = np.inf
        for _ in range(3):
            res = r0 - self.hess_vec(x / d, fields) / d
            size = float(np.abs(res).max())
            if size <= 1e-8 * float(np.abs(r0).max()) or size >= res_size:
                break
            res_size = size
            x = x + self.inverse(res)
        return x / d, size / max(float(np.abs(r0).max()), 1e-300)

    # --- Newton's method, with solver._newton's stopping rule and line search. ---
    def newton(self, lam, budget):
        """Newton steps from the multipliers lam, at most budget of them. Stops when the largest gradient is below the
        stopping size, or when it is below 1e-7 and has not fallen below half its smallest value in five steps. Each
        step is halved, as often as needed, until the dual falls enough; this is the line search. Returns the
        multipliers, the weights and a status."""
        self.calls += 1
        D, fields = self.evaluate(lam)
        best, stuck = np.inf, 0
        for _ in range(budget):
            g = self.gradient(lam, fields)
            size_g = float(np.abs(g).max())
            if size_g < self.tol:
                return lam, fields, 'converged'
            stuck = stuck + 1 if size_g > 0.5 * best else 0
            best = min(best, size_g)
            if stuck >= 5 and size_g < 1e-7:
                return lam, fields, 'converged'
            t0 = time.time()
            self.steps += 1
            step, residual = self.newton_step(g, fields)
            t1 = time.time()
            a = 1.0
            gs = float(g @ step)
            while True:
                D_new, f_new = self.evaluate(lam + a * step)
                if np.isfinite(D_new) and D_new <= D + 1e-4 * a * gs:
                    break
                if np.isfinite(D_new) and D_new <= D + 1e-13 * max(1.0, abs(D)) and \
                        np.abs(self.gradient(lam + a * step, f_new)).max() < size_g:
                    break
                a /= 2
                if a < 1e-12:
                    return lam, fields, 'converged' if size_g < 100 * self.tol else 'stalled'
            lam, D, fields = lam + a * step, D_new, f_new
            w = self.F['wood']
            gN = float(np.abs(g[:self.mN]).max())
            say(f'step {self.steps}: largest gradient {size_g:.3e} (national {gN:.3e}), step size {a:.3g}, residual {residual:.1e}, above the cap {w["r_all"]}, '
                f'cap jitter {w.get("jitter", 0.0):.0e}, {t1 - t0:.1f} s for the step ({self.timing["assemble"]:.1f} assemble, '
                f'{self.timing["eliminate"]:.1f} eliminate, {self.timing["woodbury"]:.1f} cap), {time.time() - t1:.1f} s for the line search')
        g = self.gradient(lam, fields)
        return lam, fields, 'converged' if np.abs(g).max() < self.tol else 'out of steps'

    def solve(self, budget, avg_change):
        """The whole solve, as solver._solve does it. The standard errors of averages are first set at the starting
        weights of their groups. After Newton's method converges, they are set again at the new weights and the solve
        continues, at most three rounds in all, until no group's weight moves by more than avg_change."""
        n = self.nat
        lam = np.zeros(self.mN + self.mS)
        wf = self.p_rec * self.W
        group0 = SV._group_weight(wf, n['member'], n['extra'])
        self.set_national_penalty(self.scaled_se(wf))
        rounds, notes = 0, []
        while True:
            rounds += 1
            lam, fields, status = self.newton(lam, max(budget - self.steps, 0))
            if status != 'converged' or not n['is_avg'][n['free_target']].any() or rounds >= 3:
                break
            w_now = self.national_weights(fields)
            group = SV._group_weight(w_now, n['member'], n['extra'])
            change = float(np.nanmax(np.abs(group / group0 - 1)))
            notes.append(f'average groups moved by up to {100 * change:.2f}% (round {rounds})')
            say(notes[-1])
            if change < avg_change or self.steps >= budget:
                break
            self.set_national_penalty(self.scaled_se(w_now))
            group0 = group
        g = self.gradient(lam, fields)
        return lam, fields, dict(status=status, newton_steps=self.steps, newton_calls=self.calls, rounds=rounds, notes=notes, largest_gradient=float(np.abs(g).max()),
                                 converged=status == 'converged')

    def national_weights(self, fields):
        """Each free record's national weight, in units of weight (not on the solver's scale)."""
        w = np.zeros(len(self.p_rec))
        for g, (E, e) in zip(self.groups, fields):
            w[g['rows']] = e * self.W
        return w


# ---- Reading the year and building the state targets ----

def load_year(run, year, start_dir):
    """The year's file from the inputs step (tilt.py prepare), with every weight at its start; the saved inputs of the
    national targets; the year's Historic Table 2; the year's areas; which records are filers; and each record's
    starting chances of the 51 states (state_start.py)."""
    path = f'{run}/{year}/tilt/y{year}/file_{year}.parquet'
    df = ST.read_file(path, extra=['weight_start'])
    inp = pickle.load(open(f'{run}/{year}/solve_inputs.pkl', 'rb'))
    assert np.array_equal(inp['record_id'], df['record_id'].to_numpy()), 'the solve\'s records are not the file\'s, in order'
    assert np.array_equal(inp['w0'], df['weight_start'].to_numpy(float)), 'the solve\'s starting weights are not the file\'s'
    assert np.array_equal(df['weight'].to_numpy(float), df['weight_start'].to_numpy(float)), \
        f'{path}: the weights are not at their start; the integrated solve reads the inputs step\'s file'
    ht2 = ST.read_ht2(year)
    areas = ST.areas(year)
    filer = df['filer'].to_numpy() == 1
    pi = SW.read_start(f'{start_dir}/start_{year}.npz', df)
    return df, inp, ht2, areas, filer, pi


def state_problem(df, inp, ht2, areas, filer, pi, year, items, tolerance, nonfiler_tolerance, data_dir):
    """Each record's starting shares across the areas [records, areas], and the state targets, one group for each AGI
    group of filers and one for the non-filers. A record that is held fixed enters as a fixed amount.

    A target's value in dollars or returns, here and in the tables written, is the share times the file's national
    amount at the starting weights. The solve meets the share of the final national amount; the value at the start
    sets the standard error and decides which cells have a target.

    Each state target's standard error is divided by sqrt(h), where h is the AGI group's share of the year's weight.
    The reason: tau applies to the move of the whole file, so a group holding share h of the weight effectively has
    tau x h on its own move. The division gives each group the same balance between missing its targets and moving its
    weights that it would have if it were reweighted alone. Example: the $50-75k group holds about 11% of 2019's
    units, so a 2% tolerance becomes 6.0%."""
    # 1. The starting shares. A filer starts at its chances of the 51 states, scaled to leave room for Other Areas and
    #    Puerto Rico (state_weights.cps_start). A Forbes record with a known state is put wholly in that area. A
    #    non-filer starts at its chances of the 51 states.
    f = df[filer].reset_index(drop=True)
    nf = df[~filer].reset_index(drop=True)
    group = ST.agi_group(f['calc_agi'])
    pin = SW.pins_of(f, year, areas)
    p_f = SW.cps_start(pi[filer], group, ht2, areas)
    free_pin = pin < 0
    p_f[~free_pin] = 0.0
    p_f[np.flatnonzero(~free_pin), pin[~free_pin]] = 1.0
    P0 = np.zeros((len(df), len(areas)))
    P0[filer] = p_f
    P0[~filer, :51] = pi[~filer]
    fixed = np.asarray(inp['fixed'], bool)
    w0 = np.asarray(inp['w0'], float)
    say(f'{year}: {int(fixed.sum())} records held fixed nationally; {int((~free_pin).sum())} filers pinned to an area; '
        f'fixed and not pinned {int((fixed[filer] & free_pin).sum())}, pinned and not fixed {int((~fixed[filer] & ~free_pin).sum())}')
    # 2. The filer targets and their standard errors. Items with no target this year are dropped from the list.
    tg, notes = ST.filer_targets(f, group, ht2, items, year)
    items = [v for v in items if v in set(tg['item'])]
    tg['se'] = SW.tolerances(tg, tolerance)
    w_total = float(w0.sum())
    h = pd.Series(w0[filer]).groupby(group).sum() / w_total
    tg['se'] = tg['se'] / np.sqrt(tg['group'].map(h).to_numpy(float))
    say(f'{year}: each AGI group\'s share of the weight: {h.round(4).to_dict()}')
    x = ST.item_vectors(f, items)
    X = np.column_stack([x[v] for v in items])
    apos, ipos = {a: k for k, a in enumerate(areas)}, {v: k for k, v in enumerate(items)}
    tg['a'], tg['k'] = tg['area'].map(apos), tg['item'].map(ipos)
    frow = np.flatnonzero(filer)
    wf = w0[filer]
    fixf = fixed[filer]
    specs, fits = [], []
    A, K = len(areas), len(items)
    # 3. For each AGI group: the targets, standard errors and shares as [areas, items] matrices, the amounts the fixed
    #    records put in each cell, and each item's size (its national amount in the group at the start).
    for g in range(1, 11):
        rows = np.flatnonzero(group == g)
        c = tg[tg['group'] == g]
        T = np.full((A, K), np.nan)
        se = np.full_like(T, np.nan)
        share = np.zeros((A, K))
        T[c['a'].to_numpy(), c['k'].to_numpy()] = c['target'].to_numpy()
        se[c['a'].to_numpy(), c['k'].to_numpy()] = c['se'].to_numpy()
        share[c['a'].to_numpy(), c['k'].to_numpy()] = c['share'].to_numpy()
        fx_rows = rows[fixf[rows]]
        fixed_amt = ((wf[fx_rows, None] * X[fx_rows]).T @ p_f[fx_rows]).T if len(fx_rows) else np.zeros((A, K))
        # A cell where the Forbes records already hold 90% or more of the target has no target.
        full = np.isfinite(T) & (np.abs(fixed_amt) >= SW.LIST_FULL * np.abs(np.nan_to_num(T))) & (fixed_amt != 0)
        if full.any():
            note = pd.DataFrame({'year': year, 'item': [items[k] for k in np.nonzero(full)[1]], 'group': g, 'note': 'the list records hold 90% or more of the target, no target'})
            notes = pd.concat([notes, note], ignore_index=True)
            T[full] = np.nan
            share[full] = 0.0
            drop = pd.Series(list(zip(c['a'], c['k'])), index=c.index).isin(list(zip(*np.nonzero(full))))
            c = c[~drop.to_numpy()]
        free_rows = rows[~fixf[rows] & (wf[rows] > 0)]
        total = (c['target'] / c['share']).abs()
        size = np.ones(K)
        size[c['k'].to_numpy()] = total.to_numpy()
        specs.append(dict(all_rows=frow[free_rows], Z=X[free_rows], T=T, se=se, share=share, fixed=fixed_amt, size=size))
        fits.append(c.assign(fixed_amount=fixed_amt[c['a'].to_numpy(), c['k'].to_numpy()]))
    fit = pd.concat(fits, ignore_index=True)
    # 4. The non-filers: adults by state and age band. Their standard error is the non-filer tolerance times the
    #    target at the start, divided by the square root of the non-filers' share of the weight.
    tgn, table, Xn = ST.nonfiler_targets(nf, ht2, year, data_dir)
    Tn51 = tgn.pivot(index='state', columns='band', values='target').loc[ST.STATES, ST.AGE_BAND_NAMES].to_numpy(float)
    Tn = np.full((A, len(ST.AGE_BAND_NAMES)), np.nan)
    Tn[:51] = Tn51
    share_n = np.zeros_like(Tn)
    share_n[:51] = tgn.pivot(index='state', columns='band', values='share').loc[ST.STATES, ST.AGE_BAND_NAMES].to_numpy(float)
    nrow = np.flatnonzero(~filer)
    wn = w0[~filer]
    fn = ~fixed[~filer] & (wn > 0)
    assert not fixed[~filer].any(), 'a non-filer held fixed'
    size = np.abs(Tn51).sum(0)
    nf_scale = 1.0 / np.sqrt(wn.sum() / w_total)
    specs.append(dict(all_rows=nrow[fn], Z=Xn[fn], T=Tn, se=nf_scale * nonfiler_tolerance * np.abs(Tn), share=share_n, fixed=np.zeros_like(Tn), size=size))
    return P0, specs, fit, tgn, table, notes, items


# ---- Solving ----

def joint_problem(w0, fixed, inp, P0, specs, tau, cfg, year, t_all):
    """The Joint problem: the national targets on the records that are not held fixed (the free records), and the
    state targets by group, with each group's rows renumbered among the free records. Returns the problem, the
    national targets and which records are free."""
    nat = national_setup(w0, inp['F'], np.asarray(inp['T'], float), inp['se'], inp['identity'], inp['averages'], fixed, inp['rho'])
    free = nat['free']
    where_free = -np.ones(len(w0), dtype=np.int64)
    where_free[free] = np.arange(int(free.sum()))
    groups = [{**sp, 'rows': where_free[sp['all_rows']]} for sp in specs]
    assert all((g['rows'] >= 0).all() for g in groups)
    n_in = sum(len(g['rows']) for g in groups)
    assert n_in == int((free & (w0 > 0)).sum()), f'{n_in} records in the groups, {int((free & (w0 > 0)).sum())} free with a weight'
    say(f'{year}: {int((free & (w0 == 0)).sum())} free records with no starting weight stay at zero')
    assert inp.get('soft_upper') is not None, 'the saved inputs have no soft cap'
    soft = (np.asarray(inp['soft_upper'], float)[free], float(inp['soft_beta']))
    J = Joint(w0[free], P0[free], nat, groups, tau, tol=float(cfg.get('gradient_tolerance', 1e-8)), soft=soft)
    say(f'{year}: {int(free.sum()):,} free records, {J.mN} national multipliers, {J.mS} state multipliers, {J.mA} extra unknowns, '
        f'{sum(int((g["mask"].sum(1) > 0).sum()) for g in J.groups)} state blocks, largest {max(int(g["mask"].sum(1).max()) for g in J.groups)} rows; '
        f'{J.dropped_no_holder} state targets no free record holds; {(time.time() - t_all) / 60:.1f} minutes to here')
    return J, nat, free


def run_solve(J, c, cfg, year):
    """Solve, and log how it ended. A solve that stops short within NEAR times the stopping size is used."""
    t_solve = time.time()
    lam, fields, info = J.solve(budget=c['newton_budget'], avg_change=float(cfg.get('average_weight_change', 1e-3)))
    info['solve_minutes'] = (time.time() - t_solve) / 60
    within_near = info['largest_gradient'] <= NEAR * J.tol
    say(f'{year}: {info["status"]} in {info["newton_steps"]} Newton steps ({info["rounds"]} rounds), {info["solve_minutes"]:.1f} minutes, largest gradient {info["largest_gradient"]:.2e}')
    if not info['converged']:
        say(f'{year}: WARNING: the solve stopped short of the stopping size {J.tol:.0e}, largest gradient {info["largest_gradient"]:.2e}: ' + ('within 10 times it, the result is used' if within_near else 'beyond 10 times it, this job will fail with the files written'))
    return lam, fields, info, within_near


# ---- The answer ----

def area_weights(J, fields, w0, P0, free, fixed, year):
    """Every record's area weights wis [records, areas], its national weight w1, and its shares across the areas Q.
    Free records take theirs from the solve; fixed records keep their starting weight and shares. Also each record's
    relative gap between the sum of its area weights and its national weight."""
    wis = w0[:, None] * P0
    w1 = w0.copy()
    for g, (E, e) in zip(J.groups, fields):
        r = np.flatnonzero(free)[g['rows']]
        wis[r] = E * J.W
        w1[r] = e * J.W
    assert np.array_equal(w1[fixed], w0[fixed]), 'a fixed weight moved'
    # The solve can take a record's weight so close to zero that every area's part underflows to zero. Such a record
    # gets a national weight of zero and keeps its starting shares.
    lost = wis.sum(1) == 0
    say(f'{year}: {int(lost.sum())} records whose areas all underflow to zero, largest national weight among them {w1[lost].max() if lost.any() else 0.0:.2e}; {int((w1 == 0).sum())} national weights of zero')
    w1[lost] = 0.0
    Q = np.where(lost[:, None], P0, wis / np.where(lost, 1.0, wis.sum(1))[:, None])
    assert np.isfinite(Q).all() and np.isfinite(w1).all(), 'a missing share or weight'
    Q = Q / Q.sum(1, keepdims=True)
    sum_err = np.abs(wis.sum(1) - w1) / np.where(w1 > 1e-300, w1, np.inf)
    return wis, w1, Q, sum_err


def move_by_area(J, wis, w0, P0, free):
    """How far the free records' area weights moved from their start: the Kullback-Leibler measure, in nats, on
    weights divided by the free records' total starting weight."""
    wf_is, pf_is = wis[free] / J.W, (w0[:, None] * P0)[free] / J.W
    on = wf_is > 0
    return float((wf_is[on] * np.log(wf_is[on] / pf_is[on])).sum() - wf_is.sum() + pf_is.sum())


def state_fit(df, filer, w1, wis, specs, fit, Q, ht2, areas, tgn, od):
    """Where each state target lands, and the held-out items' scores by cell. Writes state_fit.csv.gz and
    heldout_cells.csv.gz. In the fit table, target is the share times the national amount at the starting weights,
    target_final the share times the national amount at the final weights, and the misses are from target_final.
    Returns the filers with their new weights, the fit table, the held-out scores, and each state's relative miss on
    its non-filer targets by age band."""
    f = df[filer].reset_index(drop=True).copy()
    f['weight'] = w1[filer]
    reach = [(sp['Z'].T @ wis[sp['all_rows']]).T + sp['fixed'] for sp in specs]     # where each target lands, [areas, items] per group
    national = [rc.sum(0) for rc in reach]                                           # each item's national amount, [items] per group
    fit['final'] = [reach[int(g) - 1][int(a_), int(k)] for g, a_, k in zip(fit['group'], fit['a'], fit['k'])]
    fit['target_final'] = [s * national[int(g) - 1][int(k)] for g, k, s in zip(fit['group'], fit['k'], fit['share'])]
    fit['miss_se'] = (fit['final'] - fit['target_final']) / fit['se']
    fit['miss_rel'] = (fit['final'] / fit['target_final'] - 1).abs()
    fit.to_csv(f'{od}/state_fit.csv.gz', index=False)
    group = ST.agi_group(f['calc_agi'])
    cells = SW.heldout_cells(f, group, Q[filer], ht2, areas)
    assert not cells.isna().any().any(), 'a missing value in the held-out cells'
    cells.to_csv(f'{od}/heldout_cells.csv.gz', index=False)
    s = SW.score(cells)
    nr = reach[-1][:51]
    share_n = tgn.pivot(index='state', columns='band', values='share').loc[ST.STATES, ST.AGE_BAND_NAMES].to_numpy(float)
    nfr = share_n * national[-1][None, :]
    miss_n = np.abs(nr / nfr - 1)
    return f, fit, s, miss_n


def write_year_files(a, year, od, df, w1, Q, areas, info, within_near):
    """The year's file with the new national weights; the state weights file (one row per record and area: id, state,
    weight); each record's shares in full, for the projection years; and the check of the state weights file."""
    tb = pq.read_table(f'{a.run}/{year}/tilt/y{year}/file_{year}.parquet')
    tb = tb.set_column(tb.schema.get_field_index('weight'), 'weight', pa.array(w1))
    fd = f'{od}/tilt/y{year}'
    os.makedirs(fd, exist_ok=True)
    pq.write_table(tb, f'{fd}/file_{year}.parquet', compression='zstd')
    del tb
    ids = df['record_id'].to_numpy()
    path = f'{od}/state_weights_{year}.parquet'
    SW.write_long(path, ids, w1, Q, areas)
    # The state weights file leaves out a record's areas where its weight is zero, so the shares are saved in full.
    os.makedirs(f'{a.out}/shares', exist_ok=True)
    np.savez(f'{a.out}/shares/shares_{year}.npz', record_id=ids, areas=np.array(areas), P=Q)
    df['weight'] = w1
    chk = SW.check_written(path, df, ST.item_vectors(df, list(ST.ITEMS))) | {'year': year, 'newton_largest_group': info['newton_steps'], 'newton_all_groups': info['newton_steps'],
                                                                             'all_converged': bool(info['converged']), 'within_near': bool(within_near), 'largest_gradient': info['largest_gradient']}
    say(json.dumps(chk))
    json.dump(chk, open(f'{od}/check.json', 'w'), indent=1)


def write_solve_info(od, info, mv, mv_free, tau, budget, J):
    """solve_info.json: the description of the solve that the reweighting's summary (tilt.py finish) prints, with the
    keys solver.solve gives."""
    how = (f'one cost, tau = {tau:g}: {info["status"]} in {info["newton_steps"]} Newton steps of a budget of {budget} '
           f'({info["rounds"]} round(s)); largest gradient {info["largest_gradient"]:.2e}'
           + ('; ' + '; '.join(info['notes']) if info['notes'] else '')
           + f'; the integrated solve of the national and the state targets together ({J.mS} state multipliers, state_joint.py)')
    out = dict(tau=tau, converged=bool(info['converged']), how=how, status=info['status'], newton_steps=info['newton_steps'],
               newton_calls=info['newton_calls'], newton_budget=budget, soft_cap=True, largest_gradient=info['largest_gradient'],
               rounds=info['rounds'], notes=info['notes'], solve_minutes=info['solve_minutes'], kl_move=mv['kl_move'],
               effective_sample_kept=mv['effective_sample_kept'], kl_move_free=mv_free['kl_move'],
               effective_sample_kept_free=mv_free['effective_sample_kept'], national_multipliers=J.mN, state_multipliers=J.mS)
    json.dump(out, open(f'{od}/solve_info.json', 'w'), indent=1)


# ---- The whole run ----

def parse_args():
    ap = argparse.ArgumentParser()
    ap.add_argument('--run', required=True, help='the inputs step\'s folders: <run>/<year>/tilt/y<year>/file_<year>.parquet (every weight at its start) and <run>/<year>/solve_inputs.pkl (tilt.py prepare)')
    ap.add_argument('--out', required=True, help='the output folder; the year\'s results go to <out>/integrated/y<year>')
    ap.add_argument('--year', type=int, required=True)
    ap.add_argument('--start-dir', required=True, help='the folder of start_<year>.npz (state_start.py)')
    return ap.parse_args()


def main():
    c = SW.common_settings()
    a = parse_args()
    year = a.year
    tolerance, tau = c['tolerance'], float(c['tau'])
    t_all = time.time()
    common.return_large_blocks()

    # 1. Read the year's file, the saved inputs of the national targets, Historic Table 2 and the starting chances of
    #    the states.
    df, inp, ht2, areas, filer, pi = load_year(a.run, year, a.start_dir)
    w0 = np.asarray(inp['w0'], float)
    fixed = np.asarray(inp['fixed'], bool)

    # 2. Set the starting shares and build the state targets. Write the cells with no target and the non-filer targets.
    P0, specs, fit, tgn, table, notes, items = state_problem(df, inp, ht2, areas, filer, pi, year, ST.FINAL_ITEMS, tolerance, c['nonfiler_tolerance'], f'{a.run}/{year}/data')
    od = f'{a.out}/integrated/y{year}'
    os.makedirs(od, exist_ok=True)
    notes.to_csv(f'{od}/no_target.csv', index=False)
    table.to_csv(f'{od}/nonfiler_states.csv')
    tgn.to_csv(f'{od}/nonfiler_targets.csv', index=False)

    # 3. Set up the joint problem. The table of national conditions is kept aside for the fit table. The scaled
    #    record amounts are released from the problem, which holds its own copy by group.
    cfg = inp['reweighting']
    J, nat, free = joint_problem(w0, fixed, inp, P0, specs, tau, cfg, year, t_all)
    F = inp.pop('F')
    nat['X'] = None
    common.release_memory()

    # 4. Solve.
    lam, fields, info, within_near = run_solve(J, c, cfg, year)

    # 5. The weights: each record's area weights, national weight and shares across the areas.
    wis, w1, Q, sum_err = area_weights(J, fields, w0, P0, free, fixed, year)
    # The solve's own arrays are not needed again: the weights by group, the factored Newton system and each group's
    # record amounts.
    del fields
    J.F, J.groups = None, None
    common.release_memory()

    # 6. The national fit (solver.py's table, written to fit.csv) and how far the weights moved.
    nfit = SV.fit_table(w0, w1, F, np.asarray(inp['T'], float), np.asarray(inp['se'], float), inp['averages'], inp['rows'], np.asarray(inp['identity'], bool))
    nfit.to_csv(f'{od}/fit.csv', index=False)
    mv = SV.move(w0, w1)
    miss_se = np.nan_to_num(nfit['miss_in_se'].to_numpy(float))
    D_joint = move_by_area(J, wis, w0, P0, free)
    write_solve_info(od, info, mv, SV.move(w0[free], w1[free]), tau, c['newton_budget'], J)

    # 7. The state fit and the held-out items' scores.
    f, fit, s, miss_n = state_fit(df, filer, w1, wis, specs, fit, Q, ht2, areas, tgn, od)

    # 8. The summary of the year: the solve, the fits and the weights. Write it and the multipliers.
    r = dict(year=year, tag='integrated', items='final', tolerance=tolerance, tau=tau, national_multipliers=J.mN, state_multipliers=J.mS, records=len(w0), status=info['status'],
             converged=info['converged'], within_near=within_near, newton_steps=info['newton_steps'], rounds=info['rounds'],
             solve_minutes=info['solve_minutes'], largest_gradient=info['largest_gradient'], notes=info['notes'], largest_share_sum_error=float(sum_err.max()),
             targets_state=len(fit), within_2pct=100 * float((fit['miss_rel'] <= 0.02).mean()), plain_targeted=100 * float(fit['miss_rel'].mean()),
             largest_miss_se_state=float(fit['miss_se'].abs().max()), nonfiler_mean_miss=100 * float(miss_n.mean()),
             national_over_1se=int((np.abs(miss_se) > 1).sum()), national_over_2se=int((np.abs(miss_se) > 2).sum()), national_largest_miss_se=float(np.abs(miss_se).max()),
             national_miss_cost=float((0.5 * np.asarray(inp['rho'], float) * miss_se ** 2).sum()), kl_national=mv['kl_move'], effective_sample_kept=100 * mv['effective_sample_kept'],
             smallest_weight_factor=mv['smallest_weight_factor'], largest_weight_factor=mv['largest_weight_factor_overall'], kl_record_by_state=D_joint,
             income_tax_after_credits_filers=float((f['weight'] * f['calc_liab_after_credits']).sum()),
             **SW.quality(f, Q[filer], areas), **{f'heldout {k}': v for k, v in s.items()})
    say(json.dumps({k: (round(v, 4) if isinstance(v, float) else v) for k, v in r.items() if not k.startswith('heldout plain ') and not k.startswith('heldout weighted ')}))
    json.dump(r, open(f'{od}/summary.json', 'w'), indent=1)
    np.savez_compressed(f'{od}/multipliers.npz', lam=lam)

    # 9. Write the year's file, the state weights file and the shares, and check the state weights file.
    write_year_files(a, year, od, df, w1, Q, areas, info, within_near)
    say(f'all done in {(time.time() - t_all) / 60:.1f} minutes')
    if not info['converged'] and not within_near:
        sys.exit(3)


if __name__ == '__main__':
    main()
