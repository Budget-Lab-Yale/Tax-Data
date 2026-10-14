"""Piece C: test the tilt fitting on a fabricated problem with a known answer.

A small hand-made autoregressive "reference model" draws three variables per
record in a fixed order (not the variables' own order), with an
allowed-code restriction that differs by wave. Records have a wave and
groups in three families, laid out in cells exactly as A's layout (block =
wave, family, group; within a block the variables' coarse bins).

Case 1 (answer known): targets are the exact expected cell totals under a
known tilt lambda*, computed by enumerating every possible record (no Monte
Carlo), with one cell forced to zero. The fit should (a) hit the targets
exactly on its own draws, (b) on exact enumeration under the fitted lambda,
miss them only by Monte Carlo noise that shrinks with the number of draws,
and (c) recover lambda*'s tilt up to the redundant directions.

Case 2 (like the real problem): targets are the cells of one "true" draw per
record. The fit should still hit them exactly on its own draws.

Writes $P/dev/c/toy_report.json.
"""
import itertools
import json
import os
import sys

import numpy as np
import torch

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import c_tilt as CT   # noqa: E402

OUT = "/nfs/roberts/scratch/pi_nrs36/jar335/posttrain/dev/c"
torch.set_default_dtype(torch.float64)


class Toy:
    def __init__(self, seed=0, N=300):
        rng = np.random.default_rng(seed)
        self.N = N
        self.waves = 2
        self.groups = [3, 2, 4]
        self.F = len(self.groups)
        self.vocab = [6, 7, 5]
        self.tok2bin = [np.array([0, 1, 1, 2, 2, 2]),
                        np.array([0, 0, 1, 2, 3, 3, 1]),
                        np.array([0, 1, 1, 2, 2])]
        self.nbins = [3, 4, 3]
        self.V = 3
        self.vb_start = np.concatenate([[0], np.cumsum(self.nbins)[:-1]])
        self.VB = int(sum(self.nbins))
        self.order = [1, 0, 2]
        gsum = sum(self.groups)
        goff = np.concatenate([[0], np.cumsum(self.groups)[:-1]])
        self.n_blocks = self.waves * gsum
        self.C = self.n_blocks * self.VB
        self.wave = rng.integers(0, 2, N)
        self.g = np.stack([rng.integers(0, G, N) for G in self.groups], axis=1)
        self.blk = self.wave[:, None] * gsum + goff[None, :] + self.g
        self.W = np.exp(rng.normal(0, 1, N))
        # allowed codes by wave: wave 1 cannot draw token 2 of variable 0, nor
        # token 0 of variable 2 (so variable 2's bin 0 is impossible in wave 1)
        self.allowed = [[np.ones(n, bool) for n in self.vocab] for _ in range(2)]
        self.allowed[1][0][2] = False
        self.allowed[1][2][0] = False
        # reference logits
        self.base = []
        for v in range(self.V):
            n = self.vocab[v]
            a = rng.normal(0, 1, n)
            bt = rng.normal(0, 0.7, (2, n))
            gf = [rng.normal(0, 0.7, (G, n)) for G in self.groups]
            lg = a[None, :] + bt[self.wave]
            for f in range(self.F):
                lg = lg + gf[f][self.g[:, f]]
            self.base.append(torch.as_tensor(lg))
        self.M = {}
        for i, v in enumerate(self.order):
            for u in self.order[:i]:
                self.M[(v, u)] = torch.as_tensor(rng.normal(0, 0.9, (self.vocab[u], self.vocab[v])))
        self.allowed_t = [[torch.as_tensor(self.allowed[w][v]) for v in range(self.V)]
                          for w in range(2)]

    def logp(self, v, rec, tok):
        """Reference log-probabilities of variable v [R, vocab_v] (float32),
        given earlier tokens tok [R, V] of records rec [R]."""
        lg = self.base[v][rec].clone()
        i = self.order.index(v)
        for u in self.order[:i]:
            lg = lg + self.M[(v, u)][tok[:, u]]
        al = torch.stack([self.allowed_t[w][v] for w in range(2)])[torch.as_tensor(self.wave)[rec]]
        lg = lg.masked_fill(~al, float("-inf"))
        return torch.log_softmax(lg, dim=1).float()

    def geometry(self, target):
        return CT.Geometry(self.blk, self.VB, self.vb_start, self.nbins, self.tok2bin,
                           target, self.W, "cpu")


class ToyStepper:
    def __init__(self, toy, K):
        self.toy, self.K = toy, K
        self.R = toy.N * K
        self.rec = torch.arange(toy.N).repeat_interleave(K)
        self.tok = torch.zeros(self.R, toy.V, dtype=torch.long)
        self.order = list(toy.order)

    def logp_all(self, v):
        return self.toy.logp(v, self.rec, self.tok)

    def set(self, v, tok):
        self.tok[:, v] = tok


def exact(toy, geom, lam):
    """Exact expected cell totals (normalized) and KL per record under the
    tilt lam (zero cells of geom forbidden), by enumerating all records."""
    trajs = torch.as_tensor(list(itertools.product(*[range(n) for n in toy.vocab])))
    M = len(trajs)
    N = toy.N
    rec = torch.arange(N).repeat_interleave(M)
    tok = trajs.repeat(N, 1)
    lp_new = torch.zeros(N * M)
    lp_ref = torch.zeros(N * M)
    for v in toy.order:
        lp = toy.logp(v, rec, tok)
        bo = geom.tok2bin[v]
        logq = CT.bin_logq(lp, bo, toy.nbins[v])
        forb = geom.forbidden(v).repeat_interleave(M, dim=0)
        logq = logq.masked_fill(forb, float("-inf"))
        s = geom.tilt(lam, v).repeat_interleave(M, dim=0)
        logP = torch.log_softmax(logq + s, dim=1)
        t = tok[:, v]
        b = bo[t]
        lr = lp.gather(1, t[:, None]).squeeze(1).double()
        ln = logP.gather(1, b[:, None]).squeeze(1) - logq.gather(1, b[:, None]).squeeze(1) + lr
        lp_ref += lr
        lp_new += ln
    p = torch.exp(lp_new)                       # 0 where impossible
    p = torch.nan_to_num(p, nan=0.0)
    pr = p.view(N, M)
    assert torch.allclose(pr.sum(1), torch.ones(N), atol=1e-9), pr.sum(1)[:5]
    E = torch.zeros(geom.C + geom.VB)          # with the dummy block
    for v in range(toy.V):
        b = geom.tok2bin[v][tok[:, v]]
        cells = geom.cells(v).repeat_interleave(M, dim=0)           # [N*M, F, nb]
        c = cells.gather(2, b[:, None, None].expand(-1, toy.F, 1)).squeeze(2)
        w = (geom.W.repeat_interleave(M) * p)[:, None].expand(-1, toy.F)
        E.index_add_(0, c.reshape(-1), w.reshape(-1))
    term = torch.where(p > 0, p * (lp_new - lp_ref), torch.zeros_like(p)).view(N, M).sum(1)
    return E[:geom.C], term


def centered_tilt_gap(toy, geom, lam_a, lam_b):
    """Weighted RMS difference of the two tilts, centred over each record's
    possible bins (the redundant directions drop out), and the RMS of the
    second tilt for scale."""
    num, den, wsum = 0.0, 0.0, 0.0
    wave = torch.as_tensor(toy.wave)
    for v in range(toy.V):
        possible = torch.zeros(2, toy.nbins[v], dtype=torch.bool)
        for w in range(2):
            toks = torch.as_tensor(toy.allowed[w][v]).nonzero().squeeze(1)
            possible[w, geom.tok2bin[v][toks]] = True
        ok = possible[wave] & ~geom.forbidden(v)
        sa = geom.tilt(lam_a, v)
        sb = geom.tilt(lam_b, v)
        okf = ok.double()
        n = okf.sum(1, keepdim=True)
        ca = sa - (sa * okf).sum(1, keepdim=True) / n
        cb = sb - (sb * okf).sum(1, keepdim=True) / n
        d2 = (((ca - cb) ** 2) * okf).sum(1) / n.squeeze(1)
        b2 = ((cb ** 2) * okf).sum(1) / n.squeeze(1)
        num += float((geom.W * d2).sum())
        den += float((geom.W * b2).sum())
        wsum += 1
    return float(np.sqrt(num / wsum)), float(np.sqrt(den / wsum))


def main():
    os.makedirs(OUT, exist_ok=True)
    toy = Toy()
    rep = {}
    # ---- Case 1: targets from a known tilt
    rng = np.random.default_rng(5)
    lam_star = torch.as_tensor(rng.normal(0, 0.7, toy.C))
    # force one cell to zero: wave 0, family 1, group 0, variable 1, bin 3
    gsum = sum(toy.groups)
    blk0 = 0 * gsum + toy.groups[0] + 0
    zc = blk0 * toy.VB + toy.vb_start[1] + 3
    T_dummy = np.ones(toy.C)
    T_dummy[zc] = 0
    # cells impossible from the allowed codes are zero too; find them by
    # an exact pass with everything allowed except the forced cell
    g0 = toy.geometry(T_dummy)
    E_star, kl_star = exact(toy, g0, lam_star)
    T = E_star.numpy() * g0.Wtot
    T[T < 1e-300] = 0.0
    geom = toy.geometry(T)
    rep["n_cells"] = int(toy.C)
    rep["n_zero_cells"] = int((T == 0).sum())
    rep["true_kl"] = float((geom.W * kl_star).sum())
    CT.log(f"case 1: {toy.C} cells, {rep['n_zero_cells']} zero; KL of true tilt {rep['true_kl']:.4f}")
    case1 = {}
    for K in (8, 64, 512):
        lam = torch.zeros(toy.C)
        out = CT.run_pass(ToyStepper(toy, K), geom, lam, seed=11, fit=True,
                          solve_opts=dict(tol=1e-11))
        ins, _ = CT.target_error(geom, out["expected"])
        rea, _ = CT.target_error(geom, out["realized"])
        E_hat, kl_hat = exact(toy, geom, lam)
        exa, _ = CT.target_error(geom, E_hat)
        fresh = CT.run_pass(ToyStepper(toy, K), geom, lam, seed=12, fit=False)
        fre, _ = CT.target_error(geom, fresh["expected"])
        gap, scale = centered_tilt_gap(toy, geom, lam, lam_star)
        kls = CT.kl_summary(geom, K, fresh["logp_new_var"].sum(1), fresh["logp_ref_var"].sum(1),
                            fresh["kl_step"].sum(1))
        # rerun: same seed gives the same lambda
        lam2 = torch.zeros(toy.C)
        CT.run_pass(ToyStepper(toy, K), geom, lam2, seed=11, fit=True, solve_opts=dict(tol=1e-11))
        case1[K] = dict(
            in_sample_expected=ins, in_sample_realized=rea, exact_under_fit=exa,
            fresh_expected=fre, tilt_gap_rms=gap, true_tilt_rms=scale,
            kl_exact_fit=float((geom.W * kl_hat).sum()),
            kl_fresh_sample=kls["kl_sample"], kl_fresh_sample_se=kls["kl_sample_se"],
            kl_fresh_stepwise=kls["kl_stepwise"], kl_fresh_stepwise_se=kls["kl_stepwise_se"],
            zero_cell_mass_fresh_realized=float(fresh["realized"][geom.T == 0].sum()),
            same_lambda_on_rerun=bool(torch.equal(lam, lam2)),
            newton_iters=[out["infos"][v]["newton"] for v in toy.order],
            max_rel_err_steps=[out["infos"][v]["max_rel_err"] for v in toy.order])
        CT.log(f"K={K}: {json.dumps(case1[K], indent=1)}")
    rep["case1"] = case1

    # ---- Case 2: targets from one true draw per record (point masses)
    truth = ToyStepper(toy, 1)
    lam_t = torch.as_tensor(rng.normal(0, 1.0, toy.C))
    o = CT.run_pass(truth, g0, lam_t, seed=99, fit=False)
    T2 = o["realized"].numpy() * g0.Wtot
    T2[T2 < 1e-300] = 0.0
    geom2 = toy.geometry(T2)
    case2 = {}
    for K in (16, 128):
        lam = torch.zeros(toy.C)
        out = CT.run_pass(ToyStepper(toy, K), geom2, lam, seed=21, fit=True,
                          solve_opts=dict(tol=1e-11))
        ins, _ = CT.target_error(geom2, out["expected"])
        E_hat, kl_hat = exact(toy, geom2, lam)
        exa, _ = CT.target_error(geom2, E_hat)
        case2[K] = dict(n_zero_cells=int((T2 == 0).sum()), in_sample_expected=ins,
                        exact_under_fit=exa, kl_exact_fit=float((geom2.W * kl_hat).sum()),
                        max_abs_lambda=float(lam.abs().max()),
                        steps={v: {k: out["infos"][v][k] for k in ("newton", "max_rel_err", "n_not_reached_1e-6")}
                               for v in toy.order})
        CT.log(f"case 2 K={K}: {json.dumps(case2[K], indent=1)}")
    rep["case2"] = case2

    # ---- Case 3: 30% of records have no group in family 1 (like unknown
    # age on the PUF); targets exact under lambda* for that geometry.
    miss = np.random.default_rng(8).random(toy.N) < 0.3
    blk_save = toy.blk.copy()
    toy.blk = toy.blk.copy()
    toy.blk[miss, 1] = -1
    g3 = toy.geometry(T_dummy)
    E3, _ = exact(toy, g3, lam_star)
    T3 = E3.numpy() * g3.Wtot
    T3[T3 < 1e-300] = 0.0
    geom3 = toy.geometry(T3)
    lam = torch.zeros(toy.C)
    out = CT.run_pass(ToyStepper(toy, 64), geom3, lam, seed=31, fit=True, solve_opts=dict(tol=1e-11))
    ins, _ = CT.target_error(geom3, out["expected"])
    E_hat, _ = exact(toy, geom3, lam)
    exa, _ = CT.target_error(geom3, E_hat)
    gap, scale = centered_tilt_gap(toy, geom3, lam, lam_star)
    rep["case3_missing_family1_30pct"] = dict(in_sample_expected=ins, exact_under_fit=exa,
                                              tilt_gap_rms=gap, true_tilt_rms=scale)
    CT.log(f"case 3: {json.dumps(rep['case3_missing_family1_30pct'], indent=1)}")

    # ---- Case 4: inconsistent targets (family 2's cells for variable 0
    # scaled so its bin totals disagree with the other families), fitted with
    # the ridge.
    toy.blk = blk_save
    T4 = T.copy()
    for blk_ in range(toy.n_blocks):
        fam = (blk_ % sum(toy.groups))
        if fam >= toy.groups[0] + toy.groups[1]:          # family 2
            c0 = blk_ * toy.VB + toy.vb_start[0]
            sl = slice(c0, c0 + toy.nbins[0])
            x = T4[sl].copy()
            x[1] *= 1.10                                  # 10% more in bin 1
            T4[sl] = x * T[sl].sum() / x.sum()            # group total unchanged
    geom4 = toy.geometry(T4)
    case4 = {}
    for ridge in (0.0, 1e-3, 1e-2):
        lam = torch.zeros(toy.C)
        out = CT.run_pass(ToyStepper(toy, 64), geom4, lam, seed=41, fit=True,
                          solve_opts=dict(tol=1e-11, ridge=ridge))
        ins, _ = CT.target_error(geom4, out["expected"])
        inf0 = out["infos"][0]
        case4[str(ridge)] = dict(in_sample_expected=ins, var0_stop=inf0["stop"],
                                 var0_max_abs_lambda=inf0["max_abs_lambda"], var0_at_cap=inf0["n_at_cap"],
                                 var1_max_rel_err=out["infos"][1]["max_rel_err"])
    rep["case4_inconsistent"] = case4
    CT.log(f"case 4: {json.dumps(case4, indent=1)}")
    with open(os.path.join(OUT, "toy_report.json"), "w") as f:
        json.dump(rep, f, indent=1, default=str)
    CT.log("wrote toy_report.json")


if __name__ == "__main__":
    main()
