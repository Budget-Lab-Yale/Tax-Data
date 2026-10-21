"""Probe Q: targeted and untargeted wealth statistics of each run's draws
against the SCF truth, before and after the tilt, on training and test
records. Writes aggregate tables only ($Q/eval/).

Truth is each record's true wealth tokens decoded to dollars the same way
as the draws (position within a token's rank band drawn at random), so the
comparison is not affected by the decoding.

    python q_eval.py best ep01 ep02 ...
"""
import json
import sys
from pathlib import Path

import numpy as np
import pandas as pd

HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(HERE))
import q_common as QC          # noqa: E402

A = QC.setup()
L = A.layout()
WEALTH = list(A.WEALTH)
AD = QC.ASSETS + QC.DEBTS                      # 20 asset and debt variables
NI = len(A.INC_LABELS)
OUT = QC.Q / "eval"
OUT.mkdir(parents=True, exist_ok=True)
SPLITS = ("train", "test")


def wq(x, w, q):
    o = np.argsort(x)
    c = np.cumsum(w[o])
    return x[o][np.searchsorted(c, q * c[-1])]


def wspearman(x, y, w):
    rx = QC.wrank(x, w)
    ry = QC.wrank(y, w)
    mx, my = (w * rx).sum() / w.sum(), (w * ry).sum() / w.sum()
    cov = (w * (rx - mx) * (ry - my)).sum()
    return cov / np.sqrt((w * (rx - mx) ** 2).sum() * (w * (ry - my) ** 2).sum())


def stats(D, tok, recs, K, sel=None):
    """D [N, K, 24] dollars, tok [N, K, 24] tokens. sel: boolean [N] subset
    of records (for the truth's half-sample yardstick). Returns dict of
    named arrays."""
    w0 = recs["weight"].to_numpy(float)
    if sel is not None:
        w0 = np.where(sel, w0, 0.0)
    N = len(recs)
    w = np.repeat(w0 / K, K)
    D = D.reshape(N * K, -1)
    tok = tok.reshape(N * K, -1)
    keep = w > 0
    w, D, tok = w[keep], D[keep], tok[keep]
    rr = lambda c: np.repeat(recs[c].to_numpy(), K)[keep]     # noqa: E731
    gi, ga, gm, gd, wave = rr("g_inc"), rr("g_age"), rr("g_mar"), rr("g_dep"), rr("wave_i")
    nw = A.nw_from(D)
    hold = D != 0
    s = {}
    # targeted: coarse-bin shares within (wave, family, group), as in the targets
    cb = A.coarse_bin(tok)                                    # [R, 24]
    tg = []
    for f, g in (("inc", gi), ("age", ga), ("mar", gm), ("dep", gd)):
        ng = len(L.groups[f])
        blk = wave * ng + g
        for k in range(24):
            nb = int(L.nbins[k])
            num = np.bincount(blk * nb + cb[:, k], weights=w, minlength=2 * ng * nb)
            den = np.bincount(blk, weights=w, minlength=2 * ng)
            tg.append((num.reshape(2 * ng, nb) / np.maximum(den, 1e-300)[:, None]).ravel())
    s["targeted_bin_share"] = np.concatenate(tg)
    # holding rate by income group (targeted: the zero bin of each variable)
    den_i = np.bincount(gi, weights=w, minlength=NI)
    s["hold_by_inc"] = np.stack([np.bincount(gi, weights=w * hold[:, k], minlength=NI)
                                 for k in range(24)], 1) / den_i[:, None]
    # untargeted 1: holding rates by income group x age range
    na = len(L.groups["age"])
    c = gi * na + ga
    den_c = np.bincount(c, weights=w, minlength=NI * na)
    s["cell_w"] = den_c
    s["hold_inc_age"] = np.stack([np.bincount(c, weights=w * hold[:, k], minlength=NI * na)
                                  for k in range(24)], 1) / np.maximum(den_c, 1e-300)[:, None]
    # untargeted 2: mean net worth by income group x age range, and by income group
    s["nw_inc_age"] = np.bincount(c, weights=w * nw, minlength=NI * na) / np.maximum(den_c, 1e-300)
    s["nw_mean_inc"] = np.bincount(gi, weights=w * nw, minlength=NI) / den_i
    s["nw_median_inc"] = np.array([wq(nw[gi == g], w[gi == g], 0.5) for g in range(NI)])
    s["inc_w"] = den_i
    # untargeted 3: joint holding of pairs of asset and debt variables
    ia = [WEALTH.index(v) for v in AD]
    H = hold[:, ia].astype(float)
    J = (H * w[:, None]).T @ H / w.sum()
    iu = np.triu_indices(len(ia), 1)
    s["pair_joint"] = J[iu]
    s["pair_joint_by_inc"] = np.stack([((H[gi == g] * w[gi == g, None]).T @ H[gi == g]
                                        / w[gi == g].sum())[iu] for g in range(NI)])
    hv = lambda v: hold[:, WEALTH.index(v)]                   # noqa: E731
    s["named_conditional"] = np.array([
        (w * hv("primary_mortgage") * hv("primary_home")).sum() / (w * hv("primary_home")).sum(),
        (w * hv("equities") * hv("pass_throughs")).sum() / (w * hv("pass_throughs")).sum(),
        (w * hv("kg_pass_throughs") * hv("pass_throughs")).sum() / (w * hv("pass_throughs")).sum(),
        (w * hv("other_mortgage") * hv("other_home")).sum() / (w * hv("other_home")).sum()])
    # untargeted 4: rank correlation of net worth with each income
    cr = []
    for v in QC.SCF_INCOMES:
        y = rr("d_" + v).astype(float)
        ok = ~np.isnan(y)          # farm: unobserved for households filing both C and F
        cr.append(wspearman(nw[ok], y[ok], w[ok]))
    s["corr_nw_income"] = np.array(cr)
    # untargeted 5: top 1 percent's share of net worth within each income group
    top = []
    for g in range(NI):
        m = gi == g
        x, ww = nw[m], w[m]
        o = np.argsort(-x)
        cw = np.cumsum(ww[o])
        t = cw <= 0.01 * cw[-1]
        top.append((x[o][t] * ww[o][t]).sum() / (x * ww).sum())
    s["top1_share_inc"] = np.array(top)
    # untargeted 6: mean net worth by household type (filing status x children)
    kid = (gd > 0).astype(int)
    ht = gm * 2 + kid
    nht = len(L.groups["mar"]) * 2
    den_h = np.bincount(ht, weights=w, minlength=nht)
    s["ht_w"] = den_h
    s["nw_mean_ht"] = np.bincount(ht, weights=w * nw, minlength=nht) / np.maximum(den_h, 1e-300)
    return s


def compare(m, t):
    """Error summaries of model statistics m against truth t."""
    e = {}
    e["targeted_mean_abs_pp"] = 100 * np.nanmean(np.abs(m["targeted_bin_share"] - t["targeted_bin_share"]))
    e["targeted_hold_by_inc_pp"] = 100 * np.abs(m["hold_by_inc"] - t["hold_by_inc"]).mean()
    cw = t["cell_w"] / t["cell_w"].sum()
    d = np.abs(m["hold_inc_age"] - t["hold_inc_age"]).mean(1)
    e["hold_inc_age_pp"] = 100 * (cw * d).sum()
    e["hold_inc_age_pp_by_inc"] = (100 * np.array([
        (cw.reshape(NI, -1)[g] * d.reshape(NI, -1)[g]).sum() / cw.reshape(NI, -1)[g].sum()
        for g in range(NI)])).tolist()
    # mean net worth by income x age: abs error as a share of the income group's true mean
    dn = np.abs(m["nw_inc_age"] - t["nw_inc_age"]).reshape(NI, -1)
    cwi = cw.reshape(NI, -1)
    by = np.array([(cwi[g] * dn[g]).sum() / cwi[g].sum() for g in range(NI)])
    e["nw_inc_age_abs_dollars_by_inc"] = by.tolist()
    e["nw_inc_age_pct_of_group_mean_by_inc"] = (100 * by / t["nw_mean_inc"]).tolist()
    e["nw_inc_age_pct"] = float((t["inc_w"] / t["inc_w"].sum() * 100 * by / t["nw_mean_inc"]).sum())
    e["nw_mean_inc_pct"] = (100 * (m["nw_mean_inc"] / t["nw_mean_inc"] - 1)).tolist()
    e["nw_median_inc_pct"] = (100 * (m["nw_median_inc"] / t["nw_median_inc"] - 1)).tolist()
    e["pair_joint_pp"] = 100 * np.abs(m["pair_joint"] - t["pair_joint"]).mean()
    e["pair_joint_pp_by_inc"] = (100 * np.abs(m["pair_joint_by_inc"] - t["pair_joint_by_inc"]).mean(1)).tolist()
    e["named_conditional_pp"] = (100 * (m["named_conditional"] - t["named_conditional"])).tolist()
    e["corr_abs_diff"] = float(np.abs(m["corr_nw_income"] - t["corr_nw_income"]).mean())
    e["top1_share_pp"] = float(100 * np.abs(m["top1_share_inc"] - t["top1_share_inc"]).mean())
    e["top1_share_pp_by_inc"] = (100 * (m["top1_share_inc"] - t["top1_share_inc"])).tolist()
    hw = t["ht_w"] / t["ht_w"].sum()
    ok = (t["ht_w"] > 0) & (m["ht_w"] > 0)       # household types present in both
    e["nw_ht_pct"] = float((hw[ok] * 100 * np.abs(m["nw_mean_ht"][ok] / t["nw_mean_ht"][ok] - 1)).sum()
                           / hw[ok].sum())
    return {k: (float(v) if np.isscalar(v) else v) for k, v in e.items()}


def levels(s):
    return {k: s[k].tolist() for k in ("nw_mean_inc", "nw_median_inc", "top1_share_inc",
                                       "corr_nw_income", "named_conditional", "nw_mean_ht",
                                       "hold_by_inc")}


def main(names):
    recs = {sp: QC.records(sp) for sp in SPLITS}
    truth, res = {}, {}
    for sp in SPLITS:
        r = recs[sp]
        tt = A.true_tokens(r)[:, None, :]
        Dt = A.decode_dollars(tt, r, seed=5)
        truth[sp] = stats(Dt, tt, r, 1)
        # yardstick 1: the truth's own sampling error (two random halves of
        # households; |half A - half B| / 2 is about one standard error)
        hh = r["year"].to_numpy() * 10**9 + r["hh_id"].to_numpy()
        u = np.unique(hh)
        half = np.isin(hh, np.random.default_rng(3).choice(u, len(u) // 2, replace=False))
        sa, sb = stats(Dt, tt, r, 1, half), stats(Dt, tt, r, 1, ~half)
        res[f"yardstick_truth_halves_{sp}"] = {k: (np.array(v) / 2).tolist() if isinstance(v, list) else v / 2
                                               for k, v in compare(sa, sb).items()}
        res[f"truth_levels_{sp}"] = levels(truth[sp])
    for nm in names:
        z = np.load(QC.Q / "runs" / nm / "draws.npz")
        rep = json.load(open(QC.Q / "runs" / nm / "report.json"))
        out = dict(report={k: rep.get(k) for k in ("ckpt_epoch", "ckpt_val_loss_all",
                                                   "completion_score", "hidden_inputs")})
        out["kl_train"] = rep["train_after"]["kl_stepwise"]
        out["kl_test"] = rep["test_after"]["kl_stepwise"]
        out["kl_train_quantiles"] = rep["train_after"]["kl_per_record_quantiles"]
        out["fit_expected_error"] = rep["fit"].get("expected_error")
        out["train_after_expected_error"] = rep["train_after"].get("expected_error")
        for sp in SPLITS:
            r = recs[sp]
            tr = truth[sp]
            if len(z[f"rec_{sp}"]) != len(r):            # development runs on a subset
                r = r.iloc[z[f"rec_{sp}"]].reset_index(drop=True)
                tt = A.true_tokens(r)[:, None, :]
                tr = stats(A.decode_dollars(tt, r, seed=5), tt, r, 1)
            K = int(z["K"])
            klr = z[f"{sp}_kl_record"].astype(float)
            w = r["weight"].to_numpy(float)
            gi = r["g_inc"].to_numpy()
            out[f"kl_by_inc_{sp}"] = [float((w[gi == g] * klr[gi == g]).sum() / w[gi == g].sum())
                                      for g in range(NI)]
            for when in ("before", "after"):
                tk = z[f"{sp}_{when}"].astype(np.int64)
                D = A.decode_dollars(tk, r, seed=6)
                s = stats(D, tk, r, K)
                out[f"{sp}_{when}"] = compare(s, tr)
                out[f"{sp}_{when}_levels"] = levels(s)
                if when == "after" and sp == "train" and K >= 2:
                    h = K // 2
                    s1 = stats(D[:, :h], tk[:, :h], r, h)
                    s2 = stats(D[:, h:], tk[:, h:], r, K - h)
                    out["yardstick_draw_noise_train"] = {
                        k: (np.array(v) / 2).tolist() if isinstance(v, list) else v / 2
                        for k, v in compare(s1, s2).items()}
        res[nm] = out
        print(nm, json.dumps({f"{k}_{w}": out[f"train_{w}"][k] for w in ("before", "after")
                              for k in ("targeted_mean_abs_pp", "hold_inc_age_pp")}), flush=True)
    tag = "_".join(names) if len(names) < 3 else "all"
    with open(OUT / f"eval_{tag}.json", "w") as f:
        json.dump(res, f, indent=1, default=float)
    print("wrote", OUT / f"eval_{tag}.json")


if __name__ == "__main__":
    main(sys.argv[1:])
