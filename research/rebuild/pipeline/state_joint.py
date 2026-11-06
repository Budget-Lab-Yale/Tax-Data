"""The joint solve of national and state weights (FEATURES.md, "State weights"). One solve over record-by-state weights:

    w_is = w0_i x p0_is x exp(x_i' lambda_national + x_i' lambda_s),

with the year's national targets (tilt.py's list, each with its standard error) and the state targets of state_targets.py. A record's
national weight is the sum of its weights over areas, so the file's national totals are those weights' totals by construction.

The rule. The same cost as solver.py:

    minimize  sum over targets of 0.5 x importance x (miss / standard error)^2  +  tau x D(w),
    D(w) = sum over records and areas of [w_is log(w_is / (w0_i p0_is)) - w_is + w0_i p0_is],  on weights divided by the sum of w0, in nats,

with solver.py's soft cap on each record's national weight, and national identities exact. Tau is the one number of the national solve,
applied to the whole file. Part 1's tilt applied tau to each AGI group's own move, so a group holding 10% of the returns has one tenth of
that tilt's tau here: the same 2% tolerance is held more tightly in the smaller groups.

Why the size is small. The multipliers minimize a smooth convex function by Newton's method, and there are about 1,300 national multipliers and
11,000 to 23,000 state multipliers. The Hessian has a form that makes the step cheap. A state multiplier (area s, AGI group g, item k) touches
only record-by-state weights in s and g, so two state multipliers meet only when they share s and g. The state-by-state part is therefore made
of about 580 blocks of at most 21 (or 55) rows, and only the national part is dense. Eliminating the blocks leaves a dense system of the size of
the national targets, about 1,300 rows. No matrix of size 12,000 or 24,000 is factored.

The one exception is the soft cap. A record above its cap has a weight that does not respond one for one, which ties its areas together: the
Hessian loses kappa_i y_i y_i' for each such record, where y_i is the record's average condition across areas. This is a small number of terms
(2,400-3,500 records in run7) and is handled by the Woodbury identity on top of the blocks. If more than --max-capped records are above their cap at
an iterate, only the largest terms are kept for that step; the step is then a slightly overcautious one, and the line search still holds.

Worked example of the record's weight with the soft cap. Below its cap a record's weight is the sum over areas of w0 p0_s exp(z_s). Above it, with
A the same sum, the national weight E solves log(E / A) + c (E - U) = 0, the one-variable equation of solver._soft_weights, and each area keeps its
share of A: a record with p0 exp(z) of 30, 10 and 0 in three areas has shares 75%, 25% and 0 whatever E is.
"""
import argparse
import json
import os
import pickle
import time

import numpy as np
import pandas as pd
import scipy.linalg as sl

import solver as SV
import state_targets as ST
import state_weights as SW


def say(*a):
    print(*a, flush=True)


# ---- the problem ---------------------------------------------------------------------------------------------------------
def national_setup(w0, F, T, se, identity, averages, fixed, rho):
    """solver.solve's treatment of records held fixed, and solver._conditions: the conditions on the free records. Returns a dict of the free
    records' conditions on the solver's scale, the same scale as solver._solve, so its multipliers can start this solve."""
    fixed = np.asarray(fixed, bool)
    free = ~fixed
    m = len(T)
    averages = list(averages) if averages is not None else [None] * m
    if fixed.any():
        wf = w0[fixed]
        T_free = T - F[fixed].T @ wf
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
    G, Tl, size, member = SV._conditions(w0[free], F[free], T_free, av_free, offset, extra, size_of)
    W = float(w0[free].sum())
    X = G * (W / size)
    del G
    is_avg = np.array([mm is not None for mm in member])
    return dict(free=free, W=W, X=X, t=Tl / size, size=size, member=member, is_avg=is_avg, extra=extra, se=np.asarray(se, float),
                rho=np.asarray(rho, float), free_target=~np.asarray(identity, bool))


class Joint:
    """The dual and its Newton step. nat: national_setup's dict. P0 [n, A]: the free records' starting shares. groups: one dict per AGI group and
    one for the non-filers: rows (into the free records), Z [n_g, K] the items in the targets' own units, T, se, fixed [A, K] targets (NaN: none),
    standard errors and fixed amounts on the left side, size [K] the item's national total in the group, the scale of its targets."""

    def __init__(self, w0_free, P0, nat, groups, tau, tol=1e-8, soft=None, max_capped=20000):
        self.W, self.tau, self.tol, self.nat, self.max_capped = nat['W'], float(tau), float(tol), nat, int(max_capped)
        self.mN = nat['X'].shape[1]
        self.A = P0.shape[1]
        self.p_rec = w0_free / self.W
        self.t_N = nat['t']
        self.groups = []
        n_state = 0
        self.dropped_no_holder = 0
        for g in groups:
            r = g['rows']
            Xg = nat['X'][r]
            cols = np.flatnonzero((Xg != 0).any(0))
            K = g['Z'].shape[1]
            scale = self.W / g['size']
            Z = g['Z'] * scale[None, :]
            have = (Z != 0).any(0)
            mask = np.isfinite(g['T']) & have[None, :]
            self.dropped_no_holder += int((np.isfinite(g['T']) & ~have[None, :]).sum())
            # an item with no target in the group has no multiplier there
            keep = mask.any(0)
            Z, mask, g = Z[:, keep], mask[:, keep], {**g, 'T': g['T'][:, keep], 'se': g['se'][:, keep], 'fixed': g['fixed'][:, keep], 'size': g['size'][keep]}
            K = Z.shape[1]
            pos = np.full((self.A, K), -1, dtype=np.int64)
            pos[mask] = n_state + np.arange(int(mask.sum()))
            n_state += int(mask.sum())
            t = np.where(mask, (np.nan_to_num(g['T']) - g['fixed']) / g['size'][None, :], 0.0)
            nu = np.where(mask, self.tau * (np.nan_to_num(g['se']) / g['size'][None, :]) ** 2, 0.0)
            assert (nu[mask] > 0).all(), 'a state target with no positive standard error'
            iu, ju = np.triu_indices(K)
            self.groups.append(dict(rows=r, X=Xg[:, cols].copy(), cols=cols, Z=Z, Z2=Z[:, iu] * Z[:, ju], iu=iu, ju=ju, P=(w0_free[r, None] / self.W) * P0[r],
                                    p=self.p_rec[r], mask=mask, pos=pos, t=t, nu=nu, name=g.get('name', '')))
        self.mS = n_state
        self.soft = None
        if soft is not None:
            U, beta = soft
            U_s = U / self.W
            self.soft = (beta / (self.tau * U_s ** 2), U_s)      # c and U per free record, on shares of the starting total
            for g in self.groups:
                g['c'], g['U'] = self.soft[0][g['rows']], self.soft[1][g['rows']]
        self.nu_N = np.zeros(self.mN)
        self.steps, self.calls = 0, 0
        self.timing = {}

    # --- the national standard errors depend on the average groups' weights ---
    def scaled_se(self, w_free):
        n = self.nat
        gw = SV._group_weight(w_free, n['member'], n['extra'])
        return np.where(n['is_avg'], n['se'] * np.nan_to_num(gw), n['se']) / n['size'] * n['free_target']

    def set_national_penalty(self, s):
        n = self.nat
        self.nu_N = self.tau * s ** 2 / n['rho'] * n['free_target']

    # --- the vector of multipliers: national first, then the state targets in order of pos ---
    def split(self, lam):
        out = []
        for g in self.groups:
            L = np.zeros_like(g['t'])
            L[g['mask']] = lam[self.mN + g['pos'][g['mask']]]
            out.append(L)
        return lam[:self.mN], out

    def evaluate(self, lam):
        """The dual's value, and per group the weights on the solver's scale E [n_g, A] and the record's national weight e [n_g]."""
        lN, ls = self.split(lam)
        val, fields = 0.0, []
        with np.errstate(over='ignore', invalid='ignore', divide='ignore', under='ignore'):
            for g, L in zip(self.groups, ls):
                zN = g['X'] @ lN[g['cols']]
                E = g['P'] * np.exp(zN[:, None] + g['Z'] @ L.T)
                Asum = E.sum(1)
                if self.soft is not None:
                    zt = np.maximum(np.log(Asum / g['p']), -700.0)
                    if not np.isfinite(zt).all():
                        return np.inf, None
                    e = SV._soft_weights(zt, g['p'], g['c'], g['U'])
                    val += SV._soft_value(zt, e, g['p'], g['c'], g['U'])
                    E *= (e / np.where(Asum > 0, Asum, 1.0))[:, None]
                else:
                    e = Asum
                    val += float(e.sum())
                if not np.isfinite(val):
                    return np.inf, None
                fields.append((E, e))
        val += -float(self.t_all(lam)) + 0.5 * float((self.nu_all() * lam ** 2).sum())
        return val, fields

    def t_all(self, lam):
        t = np.zeros(self.mN + self.mS)
        t[:self.mN] = self.t_N
        for g in self.groups:
            t[self.mN + g['pos'][g['mask']]] = g['t'][g['mask']]
        return t @ lam

    def nu_all(self):
        nu = np.zeros(self.mN + self.mS)
        nu[:self.mN] = self.nu_N
        for g in self.groups:
            nu[self.mN + g['pos'][g['mask']]] = g['nu'][g['mask']]
        return nu

    def gradient(self, lam, fields):
        lN, ls = self.split(lam)
        out = np.zeros(self.mN + self.mS)
        for g, (E, e) in zip(self.groups, fields):
            np.add.at(out, g['cols'], g['X'].T @ e)
            R = (g['Z'].T @ E).T - g['t']
            out[self.mN + g['pos'][g['mask']]] = R[g['mask']]
        out[:self.mN] -= self.t_N
        return out + self.nu_all() * lam

    # --- the Newton step ---
    def capped(self, g, e):
        """Which records of the group lie above their cap, and kappa, the curvature the cap takes away: E x cE / (1 + cE)."""
        if self.soft is None:
            return np.zeros(0, dtype=np.int64), np.zeros(0)
        idx = np.flatnonzero(e > g['U'])
        ce = g['c'][idx] * e[idx]
        return idx, e[idx] * ce / (1.0 + ce)

    def factor(self, fields):
        """The Hessian without the soft cap's terms (national block, border and state blocks), scaled to a unit diagonal, its blocks eliminated;
        then the soft cap's terms by the Woodbury identity."""
        t0 = time.time()
        mN, mS = self.mN, self.mS
        HN = np.zeros((mN, mN))
        B = np.zeros((mN, mS))
        dS = np.zeros(mS)
        blocks = []
        for g, (E, e) in zip(self.groups, fields):
            X, Z, cols, mask, pos, nu = g['X'], g['Z'], g['cols'], g['mask'], g['pos'], g['nu']
            HN[np.ix_(cols, cols)] += X.T @ (X * e[:, None])
            K = Z.shape[1]
            for k in range(K):
                ss = np.flatnonzero(mask[:, k])
                if len(ss):
                    B[np.ix_(cols, pos[ss, k])] = X.T @ (Z[:, k:k + 1] * E[:, ss])
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
                blocks.append(dict(g=g, s=s, ks=ks, p=pos[s, ks], D=D))
                dS[pos[s, ks]] = np.diag(D)
        t1 = time.time()
        dN = np.diag(HN) + self.nu_N
        dN = np.sqrt(dN) + 1e-300
        dS = np.sqrt(dS) + 1e-300
        HN = HN / dN[:, None] / dN[None, :]
        HN[np.diag_indices(mN)] += self.nu_N / dN ** 2 + 1e-12
        B = B / dN[:, None] / dS[None, :]
        S = HN
        by_group = {}
        for b in blocks:
            D = b['D'] / dS[b['p'], None] / dS[b['p']][None, :]
            D[np.diag_indices(len(b['p']))] += 1e-12
            b['L'] = sl.cholesky(D, lower=True, check_finite=False)
            del b['D']
            cols = b['g']['cols']
            C = sl.solve_triangular(b['L'], B[np.ix_(cols, b['p'])].T, lower=True, check_finite=False)
            by_group.setdefault(id(b['g']), (cols, []))[1].append(C)
        for cols, Cs in by_group.values():
            C = np.vstack(Cs)
            S[np.ix_(cols, cols)] -= C.T @ C
        del by_group
        jitter = 0.0
        for attempt in range(6):
            try:
                LS = sl.cholesky(S + jitter * np.eye(mN), lower=True, check_finite=False)
                break
            except sl.LinAlgError:
                jitter = max(1e-11, jitter * 100)
        else:
            raise RuntimeError('the national part of the Newton system is not positive definite')
        t2 = time.time()
        self.F = dict(B=B, blocks=blocks, LS=LS, dN=dN, dS=dS, jitter=jitter)
        self.F['wood'] = self.woodbury(fields)
        self.timing = {'assemble': t1 - t0, 'eliminate': t2 - t1, 'woodbury': time.time() - t2}

    def block_solve(self, r):
        """The state blocks' inverse on a vector."""
        out = np.zeros_like(r)
        for b in self.F['blocks']:
            out[b['p']] = sl.cho_solve((b['L'], True), r[b['p']], check_finite=False)
        return out

    def arrow(self, r):
        """The inverse of the scaled Hessian without the soft cap's terms, on a vector [mN + mS]."""
        F, mN = self.F, self.mN
        rN, rS = r[:mN], r[mN:]
        v = self.block_solve(rS)
        xN = sl.cho_solve((F['LS'], True), rN - F['B'] @ v, check_finite=False)
        xS = self.block_solve(rS - F['B'].T @ xN)
        return np.concatenate([xN, xS])

    def woodbury(self, fields):
        """The records above their cap: the factor of I - sqrt(kappa) Y H^-1 Y' sqrt(kappa), H the scaled Hessian without the cap's terms."""
        if self.soft is None:
            return None
        F, mN = self.F, self.mN
        recs = []
        for gi, (g, (E, e)) in enumerate(zip(self.groups, fields)):
            idx, kappa = self.capped(g, e)
            recs += [(float(kk), gi, int(i)) for kk, i in zip(kappa, idx)]
        r_all = len(recs)
        if r_all > self.max_capped:
            recs = sorted(recs, reverse=True)[:self.max_capped]
        by = {}
        for kk, gi, i in recs:
            by.setdefault(gi, []).append((i, kk))
        r = len(recs)
        if r == 0:
            return dict(r=0, r_all=r_all)
        at, parts = 0, []
        CY = np.zeros((mN, r))
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
                y = q[:, b['s'], None] * Zc[:, b['ks']] / F['dS'][b['p']][None, :] * sk[:, None]    # [r_g, k]
                V = sl.cho_solve((b['L'], True), y.T, check_finite=False)                           # [k, r_g]
                GG += y @ V
                CY[np.ix_(g['cols'], rows)] -= F['B'][np.ix_(g['cols'], b['p'])] @ V
            Gd.append((rows, GG))
            parts.append(dict(gi=gi, idx=idx, sk=sk, q=q, rows=rows))
            at += len(idx)
        M = sl.solve_triangular(F['LS'], CY, lower=True, check_finite=False)
        del CY
        W = -(M.T @ M)
        del M
        for rows, GG in Gd:
            W[np.ix_(rows, rows)] -= GG
        W[np.diag_indices(r)] += 1.0
        jitter = 0.0
        for attempt in range(8):
            try:
                LW = sl.cholesky(W + jitter * np.eye(r), lower=True, check_finite=False)
                break
            except sl.LinAlgError:
                jitter = max(1e-10, jitter * 10)
        else:
            raise RuntimeError('the soft cap\'s system is not positive definite')
        return dict(r=r, r_all=r_all, parts=parts, LW=LW, jitter=jitter)

    def inverse(self, r):
        """The inverse of the scaled Hessian with the soft cap's terms on a scaled vector."""
        F, wood = self.F, self.F['wood']
        v = self.arrow(r)
        if wood is None or wood['r'] == 0:
            return v
        d = np.concatenate([F['dN'], F['dS']])
        lN, ls = self.split(v / d)
        a = np.zeros(wood['r'])
        for part in wood['parts']:
            g, idx = self.groups[part['gi']], part['idx']
            dz = (g['X'][idx] @ lN[g['cols']])[:, None] + g['Z'][idx] @ ls[part['gi']].T
            a[part['rows']] = part['sk'] * (part['q'] * dz).sum(1)
        u = sl.cho_solve((wood['LW'], True), a, check_finite=False)
        back = np.zeros(self.mN + self.mS)
        for part in wood['parts']:
            g, idx = self.groups[part['gi']], part['idx']
            c = part['sk'] * u[part['rows']]
            np.add.at(back, g['cols'], g['X'][idx].T @ c)
            R = (g['Z'][idx].T @ (c[:, None] * part['q'])).T
            back[self.mN + g['pos'][g['mask']]] = R[g['mask']]
        return v + self.arrow(back / d)

    def hess_vec(self, v, fields):
        """The Hessian (unscaled, every term) on a vector."""
        lN, ls = self.split(v)
        out = np.zeros(self.mN + self.mS)
        for g, L, (E, e) in zip(self.groups, ls, fields):
            dz = (g['X'] @ lN[g['cols']])[:, None] + g['Z'] @ L.T
            de = E * dz
            idx, kappa = self.capped(g, e)
            if len(idx):
                q = E[idx] / e[idx, None]
                de[idx] -= (kappa * (q * dz[idx]).sum(1))[:, None] * q
            np.add.at(out, g['cols'], g['X'].T @ de.sum(1))
            R = (g['Z'].T @ de).T
            out[self.mN + g['pos'][g['mask']]] = R[g['mask']]
        return out + self.nu_all() * v

    def newton_step(self, g_vec, fields):
        self.factor(fields)
        d = np.concatenate([self.F['dN'], self.F['dS']])
        r0 = -g_vec / d
        x = self.inverse(r0)
        # the elimination subtracts nearly equal numbers, since a national target on a group's total is the sum of its state targets: clean the answer
        res_size = np.inf
        for _ in range(3):
            res = r0 - self.hess_vec(x / d, fields) / d
            size = float(np.abs(res).max())
            if size <= 1e-8 * float(np.abs(r0).max()) or size >= res_size:
                break
            res_size = size
            x = x + self.inverse(res)
        return x / d, size / max(float(np.abs(r0).max()), 1e-300)

    # --- Newton's method: solver._newton's stopping rule and line search ---
    def newton(self, lam, budget, log=say):
        self.calls += 1
        D, fields = self.evaluate(lam)
        if not np.isfinite(D):
            lam = np.zeros_like(lam)
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
            log(f'step {self.steps}: largest gradient {size_g:.3e} (national {gN:.3e}), step size {a:.3g}, residual {residual:.1e}, above the cap {w["r_all"] if w else 0}, '
                f'jitter {self.F["jitter"]:.0e}/{w.get("jitter", 0.0) if w else 0.0:.0e}, {t1 - t0:.1f} s for the step ({self.timing["assemble"]:.1f} assemble, '
                f'{self.timing["eliminate"]:.1f} eliminate, {self.timing["woodbury"]:.1f} cap), {time.time() - t1:.1f} s for the line search')
        g = self.gradient(lam, fields)
        return lam, fields, 'converged' if np.abs(g).max() < self.tol else 'out of steps'

    def solve(self, budget=150, avg_change=1e-3, lam0=None, log=say):
        """solver._solve's outer loop: the average targets' standard errors are taken at the start's group weights, then again at the answer's."""
        n = self.nat
        lam = np.zeros(self.mN + self.mS)
        if lam0 is not None:
            lam[:self.mN] = lam0
        wf = self.p_rec * self.W
        group0 = SV._group_weight(wf, n['member'], n['extra'])
        self.set_national_penalty(self.scaled_se(wf))
        rounds, notes = 0, []
        while True:
            rounds += 1
            lam, fields, status = self.newton(lam, max(budget - self.steps, 0), log)
            if status != 'converged' or not n['is_avg'][n['free_target']].any() or rounds >= 3:
                break
            w_now = self.national_weights(fields)
            group = SV._group_weight(w_now, n['member'], n['extra'])
            change = float(np.nanmax(np.abs(group / group0 - 1)))
            notes.append(f'average groups moved by up to {100 * change:.2f}% (round {rounds})')
            log(notes[-1])
            if change < avg_change or self.steps >= budget:
                break
            self.set_national_penalty(self.scaled_se(w_now))
            group0 = group
        g = self.gradient(lam, fields)
        return lam, fields, dict(status=status, newton_steps=self.steps, newton_calls=self.calls, rounds=rounds, notes=notes, largest_gradient=float(np.abs(g).max()),
                                 converged=status == 'converged')

    def national_weights(self, fields):
        w = np.zeros(len(self.p_rec))
        for g, (E, e) in zip(self.groups, fields):
            w[g['rows']] = e * self.W
        return w


# ---- the year ------------------------------------------------------------------------------------------------------------
def load_year(run, year, start_dir, share_year=None):
    """The control's file (its weights set the state targets' national totals), the solve's inputs, the starting shares."""
    path = f'{run}/{year}/tilt/y{year}/file_{year}.parquet'
    df = ST.read_file(path, extra=['weight_start'])
    inp = pickle.load(open(f'{run}/{year}/solve_inputs.pkl', 'rb'))
    assert np.array_equal(inp['record_id'], df['record_id'].to_numpy()), 'the solve\'s records are not the file\'s, in order'
    assert np.array_equal(inp['w0'], df['weight_start'].to_numpy(float)), 'the solve\'s starting weights are not the file\'s'
    ht2 = ST.read_ht2(share_year or year)
    areas = ST.areas(share_year or year)
    filer = df['filer'].to_numpy() == 1
    pi = SW.read_start(f'{start_dir}/start_{year}.npz', df) if start_dir else None
    return df, inp, ht2, areas, filer, pi


def state_problem(df, inp, ht2, areas, filer, pi, year, items, tolerance, nonfiler_tolerance, trust, by_group_move=False, totals_weight=None, P0_given=None, share_year=None):
    """The starting shares [n, A] and the state targets, as part 1's tilt_filers and tilt_nonfilers make them: targets from the file's (control's)
    national totals. A record the national solve holds fixed keeps its starting weight and shares and enters as a fixed amount.
    by_group_move: part 1's tilt charges tau on each AGI group's own move; here the move is on the whole file, so a group holding share h of the weight has
    tau x h on its own move. With by_group_move each state target's standard error is multiplied by 1 / sqrt(h), which gives each group part 1's balance between
    its misses and its move. Worked example: the group $50-75k holds about 11% of 2019's units, so a 2% tolerance becomes 2% / sqrt(0.11) = 6.0%. Only tau x tolerance^2 matters.
    totals_weight: national weights [all records] that set the state targets' national totals, in place of the file's: an earlier joint solve's, so the targets follow the joint solve's own totals.
    P0_given, share_year (a future year): the starting shares [n, A] carried from the prior file, in place of the network's; ht2 is share_year's table, and the non-filers keep share_year's state and age shares. The target of a filer cell is then the year's national total x the IRS's share in share_year."""
    f = df[filer].reset_index(drop=True)
    nf = df[~filer].reset_index(drop=True)
    group = ST.agi_group(f['calc_agi'])
    pin = SW.pins_of(f, year, areas)
    p_f = SW.cps_start(pi[filer], group, ht2, areas) if P0_given is None else P0_given[filer].copy()
    free_pin = pin < 0
    p_f[~free_pin] = 0.0
    p_f[np.flatnonzero(~free_pin), pin[~free_pin]] = 1.0
    P0 = np.zeros((len(df), len(areas)))
    P0[filer] = p_f
    if P0_given is None:
        P0[~filer, :51] = pi[~filer]
    else:
        P0[~filer] = P0_given[~filer]
    fixed = np.asarray(inp['fixed'], bool)
    w0 = np.asarray(inp['w0'], float)
    say(f'{year}: {int(fixed.sum())} records held fixed nationally; {int((~free_pin).sum())} filers pinned to an area; '
        f'fixed and not pinned {int((fixed[filer] & free_pin).sum())}, pinned and not fixed {int((~fixed[filer] & ~free_pin).sum())}')
    tg, notes = ST.filer_targets(f, group, ht2, items, year, weight=None if totals_weight is None else totals_weight[filer], trust=trust)
    items = [v for v in items if v in set(tg['item'])]
    tg['se'] = SW.tolerances(tg, tolerance)
    w_total = float(w0.sum())
    if by_group_move:
        h = pd.Series(wf_all := w0[filer]).groupby(group).sum() / w_total
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
    for g in range(1, 11):
        rows = np.flatnonzero(group == g)
        c = tg[tg['group'] == g]
        T = np.full((A, K), np.nan)
        se = np.full_like(T, np.nan)
        T[c['a'].to_numpy(), c['k'].to_numpy()] = c['target'].to_numpy()
        se[c['a'].to_numpy(), c['k'].to_numpy()] = c['se'].to_numpy()
        fx_rows = rows[fixf[rows]]
        fixed_amt = ((wf[fx_rows, None] * X[fx_rows]).T @ p_f[fx_rows]).T if len(fx_rows) else np.zeros((A, K))
        # part 1's rule: a cell the list records already fill has no target
        full = np.isfinite(T) & (np.abs(fixed_amt) >= SW.LIST_FULL * np.abs(np.nan_to_num(T))) & (fixed_amt != 0)
        if full.any():
            note = pd.DataFrame({'year': year, 'item': [items[k] for k in np.nonzero(full)[1]], 'group': g, 'note': 'the list records hold 90% or more of the target, no target'})
            notes = pd.concat([notes, note], ignore_index=True)
            T[full] = np.nan
            drop = pd.Series(list(zip(c['a'], c['k'])), index=c.index).isin(list(zip(*np.nonzero(full))))
            c = c[~drop.to_numpy()]
        free_rows = rows[~fixf[rows] & (wf[rows] > 0)]
        total = (c['target'] / c['share']).abs()
        size = np.ones(K)
        size[c['k'].to_numpy()] = total.to_numpy()
        specs.append(dict(name=ST.GROUP_NAMES[g - 1], all_rows=frow[free_rows], Z=X[free_rows], T=T, se=se, fixed=fixed_amt, size=size))
        fits.append(c.assign(fixed_amount=fixed_amt[c['a'].to_numpy(), c['k'].to_numpy()]))
    fit = pd.concat(fits, ignore_index=True)
    # the non-filers
    tgn, table, Xn = ST.nonfiler_targets(nf if totals_weight is None else nf.assign(weight=totals_weight[~filer]), ht2, share_year or year)
    Tn51 = tgn.pivot(index='state', columns='band', values='target').loc[ST.STATES, ST.AGE_BAND_NAMES].to_numpy(float)
    Tn = np.full((A, len(ST.AGE_BAND_NAMES)), np.nan)
    Tn[:51] = Tn51
    nrow = np.flatnonzero(~filer)
    wn = w0[~filer]
    fn = ~fixed[~filer] & (wn > 0)
    assert not fixed[~filer].any(), 'a non-filer held fixed'
    size = np.abs(Tn51).sum(0)
    nf_scale = 1.0 / np.sqrt(wn.sum() / w_total) if by_group_move else 1.0
    specs.append(dict(name='non-filers', all_rows=nrow[fn], Z=Xn[fn], T=Tn, se=nf_scale * nonfiler_tolerance * np.abs(Tn), fixed=np.zeros_like(Tn), size=size))
    return P0, specs, fit, tgn, table, notes, items


def prior_shares(df, prior_long, areas, year):
    """A future year's starting shares [n, A]: each record's shares in the prior file (id, state, weight), part 1's carry. A list record's id is its rank on the year's list, so it does
    not follow a person: list records are pinned by forbes_uri, and one with no state on any list takes the distribution of the year's pinned list records. A record with no weight in the prior file
    has no shares to carry: it takes the prior file's overall shares, which multiply nothing. A record that is neither on the list nor in the prior file stops the run."""
    d = pd.read_parquet(prior_long)
    assert not d.isna().any().any(), f'{prior_long}: a missing value'
    ids0 = pd.Index(d['id'].unique())
    W = np.zeros((len(ids0), len(areas)))
    W[ids0.get_indexer(d['id'].to_numpy()), pd.Index(areas).get_indexer(d['state'].astype(str).to_numpy())] = d['weight'].to_numpy(float)
    tot = W.sum(1)
    overall = W.sum(0) / W.sum()
    S = np.where(tot[:, None] > 0, W / np.where(tot > 0, tot, 1.0)[:, None], overall[None, :])
    say(f'{year}: prior file {prior_long}: {len(ids0):,} records, {int((tot == 0).sum())} with no weight, which take the overall shares')
    idx = ids0.get_indexer(df['record_id'].to_numpy())
    lst = df['billionaire'].to_numpy() == 1
    n_new = int(((idx < 0) & ~lst).sum())
    assert n_new == 0, f'{year}: {n_new} records neither on the list nor in the prior file'
    P = np.zeros((len(df), len(areas)))
    P[~lst] = S[idx[~lst]]
    pin = SW.pins_of(df, year, areas)
    pinned = lst & (pin >= 0)
    P[np.flatnonzero(pinned), pin[pinned]] = 1.0
    unpinned = lst & (pin < 0)
    if unpinned.any():
        P[unpinned] = np.bincount(pin[pinned], minlength=len(areas)) / pinned.sum()
    say(f'{year}: list records {int(lst.sum())}, pinned {int(pinned.sum())}, on the pinned distribution {int(unpinned.sum())}; non-list records not in the prior file: 0')
    return P


def share_floor(P, df, filer, ht2, areas, share_year, eps):
    """A future year's starting shares with a floor. Carried shares can be extremely small: a record with a large item in a year's tilt ends with shares such as 1e-238 in some areas, and a cell whose holders all
    have such shares has almost no curvature, so Newton's step on it is enormous and the solve stalls. Each record's shares become (1 - eps) x carried + eps x the IRS's shares: for a filer the IRS's share of returns in
    the record's AGI group, for a non-filer the state's share of the residual of Census adults less adults on returns. Worked example, eps = 0.001: a record with a share of 1e-238 in Wyoming, where the IRS has 0.18% of
    the group's returns, starts at 0.001 x 0.0018 = 1.8e-6; a share of 20% in California with the IRS at 11% starts at 0.999 x 0.20 + 0.001 x 0.11 = 19.99%. Pinned list records are set after this, by state_problem."""
    out = P.copy()
    group = ST.agi_group(df.loc[filer, 'calc_agi'])
    out[filer] = (1 - eps) * P[filer] + eps * SW.irs_return_shares(ht2, areas)[group - 1]
    t, _ = ST.nonfiler_state_table(share_year, ht2)
    r = t['residual'].clip(lower=0).to_numpy(float)
    b = np.zeros(len(areas))
    b[:51] = r / r.sum()
    out[~filer] = (1 - eps) * P[~filer] + eps * b
    return out


def read_back(specs, wis_all, areas):
    """Where each target lands: [(spec, reached [A, K])]."""
    out = []
    for sp in specs:
        out.append(((sp['Z'].T @ wis_all[sp['all_rows']]).T + sp['fixed']))
    return out


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument('--run', required=True, help='the control\'s run folder: <run>/<year>/tilt/y<year>/file_<year>.parquet and <run>/<year>/solve_inputs.pkl')
    ap.add_argument('--out', required=True, help='the output folder')
    ap.add_argument('--year', type=int, required=True)
    ap.add_argument('--start-dir', default=None)
    ap.add_argument('--items', default='comparison')
    ap.add_argument('--tolerance', type=float, default=0.02)
    ap.add_argument('--nonfiler-tolerance', type=float, default=0.02)
    ap.add_argument('--tau', type=float, default=1000.0)
    ap.add_argument('--budget', type=int, default=150)
    ap.add_argument('--tag', required=True)
    ap.add_argument('--warm', action='store_true', help='start from the national solve\'s multipliers (its steps then count too)')
    ap.add_argument('--no-trust', action='store_true')
    ap.add_argument('--by-group-move', action='store_true', help='state standard errors divided by the square root of the AGI group\'s share of the weight (state_problem)')
    ap.add_argument('--max-capped', type=int, default=20000)
    ap.add_argument('--totals-from', default=None, help='the file of an earlier solve whose national weights set the state targets\' national totals')
    ap.add_argument('--prior-long', default=None, help='a future year: the prior file of id, state, weight whose shares start this solve')
    ap.add_argument('--share-year', type=int, default=None, help='a future year: the year of the IRS\'s shares, and of the non-filers\' state and age shares')
    ap.add_argument('--share-floor', type=float, default=0.0, help='a future year: each record\'s carried shares mixed with this much of the IRS\'s shares (share_floor)')
    ap.add_argument('--no-state', action='store_true', help='no state targets: the national solve\'s weights x the prior shares; every state number is only measured')
    a = ap.parse_args()
    year = a.year
    t_all = time.time()
    df, inp, ht2, areas, filer, pi = load_year(a.run, year, a.start_dir, a.share_year)
    P_given = prior_shares(df, a.prior_long, areas, year) if a.prior_long else None
    if a.share_floor > 0:
        P_given = share_floor(P_given, df, filer, ht2, areas, a.share_year, a.share_floor)
    items = {'comparison': ST.ISELIN_21, 'final': ST.FINAL_ITEMS,
             'wider': ST.ISELIN_21 + [v for v in ST.EXTRA_ITEMS if v != 'kg_amt']}.get(a.items) or a.items.split(',')
    trust = a.items != 'comparison' and not a.no_trust
    totals_weight = None
    if a.totals_from:
        tf = pd.read_parquet(a.totals_from, columns=['record_id', 'weight'])
        assert np.array_equal(tf['record_id'].to_numpy(), df['record_id'].to_numpy())
        totals_weight = tf['weight'].to_numpy(float)
    P0, specs, fit, tgn, table, notes, items = state_problem(df, inp, ht2, areas, filer, pi, year, items, a.tolerance, a.nonfiler_tolerance, trust, a.by_group_move, totals_weight, P_given, a.share_year)
    w0 = np.asarray(inp['w0'], float)
    fixed = np.asarray(inp['fixed'], bool)
    cfg = inp['reweighting']
    assert float(cfg['tau']) == 1000.0 or a.tau != 1000.0
    nat = national_setup(w0, inp['F'], np.asarray(inp['T'], float), inp['se'], inp['identity'], inp['averages'], fixed, inp['rho'])
    free = nat['free']
    where_free = -np.ones(len(w0), dtype=np.int64)
    where_free[free] = np.arange(int(free.sum()))
    groups = []
    for sp in specs:
        r = where_free[sp['all_rows']]
        assert (r >= 0).all()
        groups.append(dict(rows=r, Z=sp['Z'], T=sp['T'], se=sp['se'], fixed=sp['fixed'], size=sp['size'], name=sp['name']))
    n_in = sum(len(g['rows']) for g in groups)
    assert n_in == int((free & (w0 > 0)).sum()), f'{n_in} records in the groups, {int((free & (w0 > 0)).sum())} free with a weight'
    say(f'{year}: {int((free & (w0 == 0)).sum())} free records with no starting weight stay at zero')
    soft = None
    if inp.get('soft_upper') is not None:
        soft = (np.asarray(inp['soft_upper'], float)[free], float(inp['soft_beta']))
    J = Joint(w0[free], P0[free], nat, groups, a.tau, tol=float(cfg.get('gradient_tolerance', 1e-8)), soft=soft, max_capped=a.max_capped)
    lam0 = None
    warm_steps = 0
    if a.warm:
        z = np.load(f'{a.run}/{year}/solve_inputs.pkl.answer.npz')
        lam0, warm_steps = z['multipliers'], int(z['newton_steps'])
    say(f'{year}: {int(free.sum()):,} free records, {J.mN} national multipliers, {J.mS} state multipliers, '
        f'{sum(int((g["mask"].sum(1) > 0).sum()) for g in J.groups)} state blocks, largest {max(int(g["mask"].sum(1).max()) for g in J.groups)} rows; '
        f'{J.dropped_no_holder} state targets no free record holds; {(time.time() - t_all) / 60:.1f} minutes to here')
    del_F = inp.pop('F')
    nat['X'] = None
    t_solve = time.time()
    if a.no_state:
        lam, fields = None, None
        info = dict(status='converged', newton_steps=0, newton_calls=0, rounds=0, notes=['no state targets: the national solve\'s weights x the prior shares'], largest_gradient=0.0, converged=True)
    else:
        lam, fields, info = J.solve(budget=a.budget - warm_steps, avg_change=float(cfg.get('average_weight_change', 1e-3)), lam0=lam0)
    info['solve_minutes'] = (time.time() - t_solve) / 60
    say(f'{year}: {info["status"]} in {info["newton_steps"]} Newton steps ({info["rounds"]} rounds), {info["solve_minutes"]:.1f} minutes, largest gradient {info["largest_gradient"]:.2e}')
    # the weights: free records from the solve, fixed records at their start
    n, A = len(w0), len(areas)
    wis = w0[:, None] * P0
    w1 = w0.copy()
    if a.no_state:
        w1 = df['weight'].to_numpy(float)
        wis = w1[:, None] * P0
    else:
        for g, (E, e) in zip(J.groups, fields):
            r = np.flatnonzero(free)[g['rows']]
            wis[r] = E * J.W
            w1[r] = e * J.W
    assert np.array_equal(w1[fixed], w0[fixed]), 'a fixed weight moved'
    sum_err = np.abs(wis.sum(1) - w1) / np.where(w1 > 1e-300, w1, np.inf)
    # The solve can take a record's weight to a number so small that every area's part underflows to zero (run7's own file has national weights
    # of the order of 1e-316). Such a record keeps its starting shares, which multiply nothing. A missing value here would drop whole cells from the scores.
    lost = wis.sum(1) == 0
    say(f'{year}: {int(lost.sum())} records whose areas all underflow to zero, largest national weight among them {w1[lost].max() if lost.any() else 0.0:.2e}; {int((w1 == 0).sum())} national weights of zero')
    w1[lost] = 0.0
    Q = np.where(lost[:, None], P0, wis / np.where(lost, 1.0, wis.sum(1))[:, None])
    assert np.isfinite(Q).all() and np.isfinite(w1).all(), 'a missing share or weight'
    Q = Q / Q.sum(1, keepdims=True)
    sum_err = np.abs(wis.sum(1) - w1) / np.where(w1 > 1e-300, w1, np.inf)
    # the national fit, solver.py's table
    od = f'{a.out}/{a.tag}/y{year}'
    os.makedirs(od, exist_ok=True)
    F, T, se, identity, averages, rows = del_F, np.asarray(inp['T'], float), np.asarray(inp['se'], float), np.asarray(inp['identity'], bool), inp['averages'], inp['rows']
    nfit = SV.fit_table(w0, w1, F, T, se, averages, rows, identity)
    nfit.to_csv(f'{od}/fit.csv', index=False)
    mv = SV.move(w0, w1)
    rho = np.asarray(inp['rho'], float)
    miss_se = np.nan_to_num(nfit['miss_in_se'].to_numpy(float))
    wf_is, pf_is = wis[free] / J.W, (w0[:, None] * P0)[free] / J.W     # the move in record-by-state weights, on the free records, in nats
    on = wf_is > 0
    D_joint = float((wf_is[on] * np.log(wf_is[on] / pf_is[on])).sum() - wf_is.sum() + pf_is.sum())
    # the state fit, the same columns as part 1's
    f = df[filer].reset_index(drop=True).copy()
    f['weight'] = w1[filer]
    reach = read_back(specs, wis, areas)
    fit['final'] = [reach[int(g) - 1][int(a_), int(k)] for g, a_, k in zip(fit['group'], fit['a'], fit['k'])]
    fit['miss_se'] = (fit['final'] - fit['target']) / fit['se']
    fit['miss_rel'] = (fit['final'] / fit['target'] - 1).abs()
    fit.to_csv(f'{od}/state_fit.csv.gz', index=False)
    notes.to_csv(f'{od}/no_target.csv', index=False)
    group = ST.agi_group(f['calc_agi'])
    cells = SW.heldout_cells(f, group, Q[filer], ht2, areas)
    assert not cells.isna().any().any(), 'a missing value in the held-out cells'
    cells.to_csv(f'{od}/heldout_cells.csv.gz', index=False)
    s = SW.score(cells)
    nr = reach[-1][:51]
    nfr = tgn.pivot(index='state', columns='band', values='target').loc[ST.STATES, ST.AGE_BAND_NAMES].to_numpy(float)
    miss_n = np.abs(nr / nfr - 1)
    r = dict(year=year, tag=a.tag, items=a.items, tolerance=a.tolerance, tau=a.tau, warm=a.warm, national_multipliers=J.mN, state_multipliers=J.mS, records=n, status=info['status'],
             converged=info['converged'], newton_steps=info['newton_steps'], warm_steps=warm_steps, newton_total=info['newton_steps'] + warm_steps, rounds=info['rounds'],
             solve_minutes=info['solve_minutes'], largest_gradient=info['largest_gradient'], notes=info['notes'], largest_share_sum_error=float(sum_err.max()),
             targets_state=len(fit), within_2pct=100 * float((fit['miss_rel'] <= 0.02).mean()), plain_targeted=100 * float(fit['miss_rel'].mean()),
             largest_miss_se_state=float(fit['miss_se'].abs().max()), nonfiler_mean_miss=100 * float(miss_n.mean()),
             national_over_1se=int((np.abs(miss_se) > 1).sum()), national_over_2se=int((np.abs(miss_se) > 2).sum()), national_largest_miss_se=float(np.abs(miss_se).max()),
             national_miss_cost=float((0.5 * rho * miss_se ** 2).sum()), kl_national=mv['kl_move'], effective_sample_kept=100 * mv['effective_sample_kept'],
             smallest_weight_factor=mv['smallest_weight_factor'], largest_weight_factor=mv['largest_weight_factor_overall'], kl_record_by_state=D_joint,
             income_tax_after_credits_filers=float((f['weight'] * f['calc_liab_after_credits']).sum()),
             **SW.quality(f, Q[filer], areas), **{f'heldout {k}': v for k, v in s.items()})
    say(json.dumps({k: (round(v, 4) if isinstance(v, float) else v) for k, v in r.items() if not k.startswith('heldout plain ') and not k.startswith('heldout weighted ')}))
    json.dump(r, open(f'{od}/summary.json', 'w'), indent=1)
    if lam is not None:
        np.savez_compressed(f'{od}/multipliers.npz', lam=lam)
    # the file for the held-out national score, the national weights only, and the year's file of id, state, weight
    import pyarrow.parquet as pq
    src = f'{a.run}/{year}/tilt/y{year}/file_{year}.parquet'
    tb = pq.read_table(src)
    i = tb.schema.get_field_index('weight')
    import pyarrow as pa
    tb = tb.set_column(i, 'weight', pa.array(w1))
    fd = f'{od}/tilt/y{year}'
    os.makedirs(fd, exist_ok=True)
    pq.write_table(tb, f'{fd}/file_{year}.parquet', compression='zstd')
    del tb
    ids = df['record_id'].to_numpy()
    path = f'{od}/state_weights_{year}.parquet'
    SW.write_long(path, ids, w1, Q, areas)
    d0 = df.copy()
    d0['weight'] = w1
    chk = SW.check_written(path, d0, ST.item_vectors(d0, list(ST.ITEMS)))
    say(json.dumps(chk))
    json.dump(chk, open(f'{od}/check.json', 'w'), indent=1)
    say(f'all done in {(time.time() - t_all) / 60:.1f} minutes')


if __name__ == '__main__':
    main()
