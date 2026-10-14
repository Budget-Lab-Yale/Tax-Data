"""Test 3, piece F: untargeted measures for the from-scratch arms, with E's
`frame_stats` (e_evaluate.py) applied to F's draws through a small adapter.

    python f_eval.py stats            # $P/f/eval/<label>.json for every ready input
    python f_eval.py tables           # $P/f/eval/tables.md (aggregate tables only)

Labels: truth (reported dollars), truth_decoded (true tokens through the
decoder), s0_ref (S0 draws, each carrying 1/K of its record's weight),
s0_m1 (S0 calibrated), s1_ref_m<k> / s1_m1_m<k> (S1 draws at k times the
real count, equal starting weights / calibrated).

For draws, the income group is the draw's own (its drawn incomes' total
against the real wave's thresholds), and the income rank used in the
correlations is the drawn total's weighted rank in the real wave's
distribution (all SCF training units, split-offs included, as for A's
records).
"""
import json
import os
import sys
import time
from types import SimpleNamespace

import numpy as np
import pandas as pd

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, HERE)
import f_common as F                                         # noqa: E402
from f_common import A, WEALTH, INCOMES, WAVES, SHARED_CAT, VARS, log  # noqa: E402
import e_evaluate as E                                       # noqa: E402
from c_common import token_info, classifier_test, wrank      # noqa: E402

PF = str(F.PF)
OUT = os.path.join(PF, "eval")
SEED = 20261231


def real_cdf():
    """Per wave: sorted totals and mid-rank of each, over every SCF training
    unit of the wave (split-offs included)."""
    rec = pd.read_parquet(A.DATA / "records_train.parquet")
    src = rec["source"].astype(str).to_numpy()
    yr = rec["year"].to_numpy().astype(int)
    tot = rec[INCOMES].to_numpy(dtype=float).sum(axis=1)
    wt = rec["weight"].to_numpy(dtype=float)
    out = []
    for w in WAVES:
        ii = np.flatnonzero((src == "SCF") & (yr == w))
        r = wrank(tot[ii], wt[ii])
        o = np.argsort(tot[ii], kind="mergesort")
        out.append((tot[ii][o], r[o]))
    return out


def rank_in_real(total, wave_i, cdf):
    r = np.zeros(len(total))
    for wi in range(len(WAVES)):
        m = wave_i == wi
        xs, rs = cdf[wi]
        r[m] = np.interp(total[m], xs, rs)
    return r


def base_like(inc5, age, incd, inc_rank):
    meta = A.meta()
    vm = {v["name"]: v for v in meta["variables"]}
    tinfo = {v: token_info(vm[v]) for v in WEALTH + INCOMES}
    ages = list(range(len(vm["age_range"]["tokens"])))
    labels = {i: str(t.get("meaning", i)) for i, t in enumerate(vm["age_range"]["tokens"])}
    return SimpleNamespace(tinfo=tinfo, inc5=np.asarray(inc5), age=np.asarray(age), ages=ages,
                           age_labels=labels, incd=incd, inc_rank=np.asarray(inc_rank))


def class_frame(hh, w, demo, inc_tok, wealth_tok):
    Fm = pd.DataFrame({"hh": hh, "w": w})
    for j, v in enumerate(SHARED_CAT):
        Fm["t_" + v] = demo[:, j]
    for j, v in enumerate(INCOMES):
        Fm["t_" + v] = inc_tok[:, j]
    for j, v in enumerate(WEALTH):
        Fm["t_" + v] = wealth_tok[:, j]
    return Fm


def truth_frames(recs):
    W = recs["weight"].to_numpy(float)
    tok = A.true_tokens(recs)
    incd = {v: recs["d_" + v].to_numpy(float) for v in INCOMES}
    b = base_like(recs["g_inc"].to_numpy(), recs["tok_age_range"].to_numpy(), incd, recs["inc_rank"].to_numpy())
    hh = recs["year"].to_numpy(np.int64) * 10**9 + recs["hh_id"].to_numpy(np.int64)
    demo = np.stack([recs["tok_" + v].to_numpy() for v in SHARED_CAT], axis=1)
    inc_tok = np.stack([recs["tok_" + v].to_numpy() for v in INCOMES], axis=1)
    return b, tok, W, hh, demo, inc_tok


def stats_for(label, b, tok, dol, w, cls=None):
    t0 = time.time()
    st = E.frame_stats(b, np.arange(len(w)), tok, dol, w, with_patterns=False)
    out = {"label": label, "rows": int(len(w)), "total_weight": float(np.sum(w)), "stats": st}
    if cls is not None:
        c, imp = classifier_test(cls[0], cls[1], SEED)
        c["top_variables"] = imp.head(6).to_dict("records")
        out["classifier"] = c
    os.makedirs(OUT, exist_ok=True)
    with open(os.path.join(OUT, f"{label}.json"), "w") as fh:
        json.dump(E.jsonable(out) if hasattr(E, "jsonable") else out, fh, indent=1, default=float)
    log(f"{label}: {len(w):,} rows in {time.time() - t0:.0f}s"
        + (f", classifier AUC {out['classifier']['auc']:.3f}" if cls is not None else ""))


def pick_one(w2d, rng):
    """One draw per record, chosen with probability w_ij / W_i."""
    q = w2d / w2d.sum(1, keepdims=True)
    u = rng.random(len(q))[:, None]
    return np.minimum((np.cumsum(q, 1) < u).sum(1), q.shape[1] - 1)


def run_stats():
    recs = A.load_records()
    cdf = real_cdf()
    b, tok, W, hh, demo, inc_tok = truth_frames(recs)
    truth_cls = class_frame(hh, W, demo, inc_tok, tok)
    if not os.path.exists(os.path.join(OUT, "truth.json")):
        dol = np.stack([recs["d_" + v].to_numpy(float) for v in WEALTH], axis=1)
        stats_for("truth", b, tok, dol, W)
        dd = np.stack([recs["dec_" + v].to_numpy(float) for v in WEALTH], axis=1)
        stats_for("truth_decoded", b, tok, dd, W)
    rng = np.random.default_rng(SEED)
    # S0
    fn = os.path.join(PF, "s0", "draws.npz")
    if os.path.exists(fn):
        z = np.load(fn)
        names = list(z["vars"])
        N, K, _ = z["tokens"].shape
        tokf = z["tokens"].reshape(N * K, -1).astype(np.int64)
        dolf = z["dollars"].reshape(N * K, -1)
        wave_i = np.repeat(recs["wave_i"].to_numpy(), K)
        ii = [names.index(v) for v in INCOMES]
        iw = [names.index(v) for v in WEALTH]
        tot = dolf[:, ii].sum(1)
        g = F.income_group(tot, wave_i, F.income_thresholds())
        incd = {v: dolf[:, names.index(v)] for v in INCOMES}
        bs = base_like(g, np.repeat(recs["tok_age_range"].to_numpy(), K), incd, rank_in_real(tot, wave_i, cdf))
        demo_s = np.repeat(demo, K, axis=0)
        variants = [("s0_ref", np.repeat(W / K, K))]
        wf = os.path.join(PF, "s0", "weights.npz")
        if os.path.exists(wf):
            variants.append(("s0_m1", np.load(wf)["w"].reshape(-1)))
        for label, w in variants:
            pick = pick_one(w.reshape(N, K), rng)
            r = np.arange(N) * K + pick
            cls = (truth_cls, class_frame(hh, W, demo, tokf[r][:, ii], tokf[r][:, iw]))
            stats_for(label, bs, tokf[:, iw], dolf[:, iw], w, cls)
    # S1
    for mult in (8, 1, 4, 32, 64):
        wf = os.path.join(PF, "s1", f"weights_m{mult}.npz")
        if not os.path.exists(wf) and mult != 8:
            continue
        import f_run
        z = f_run.load_s1(mult)
        names = VARS["s1"]
        tokf = z["tokens"].astype(np.int64)
        dolf = z["dollars"]
        wave_i = z["wave_i"].astype(np.int64)
        ii = [names.index(v) for v in INCOMES]
        iw = [names.index(v) for v in WEALTH]
        idm = [names.index(v) for v in SHARED_CAT]
        tot = dolf[:, ii].sum(1)
        g = F.income_group(tot, wave_i, F.income_thresholds())
        incd = {v: dolf[:, names.index(v)] for v in INCOMES}
        bs = base_like(g, tokf[:, names.index("age_range")], incd, rank_in_real(tot, wave_i, cdf))
        Ww = recs.groupby("wave_i")["weight"].sum().to_numpy()
        nw = np.bincount(wave_i, minlength=len(WAVES))
        w0 = Ww[wave_i] / nw[wave_i]
        todo = [(f"s1_ref_m{mult}", w0)]
        if os.path.exists(wf):
            todo.append((f"s1_m1_m{mult}", np.load(wf)["w"]))
        for label, w in todo:
            if mult != 8 and label.startswith("s1_ref"):
                continue
            # classifier: resample as many synthetic records as real ones, with
            # probability proportional to weight within wave
            cls = None
            if mult == 8:
                pick = []
                for wi in range(len(WAVES)):
                    m = np.flatnonzero(wave_i == wi)
                    n_real = int((recs["wave_i"] == wi).sum())
                    pick.append(rng.choice(m, n_real, replace=True, p=w[m] / w[m].sum()))
                pick = np.concatenate(pick)
                cls = (truth_cls, class_frame(pick + 10**12, np.full(len(pick), W.sum() / len(pick)),
                                              tokf[pick][:, idm], tokf[pick][:, ii], tokf[pick][:, iw]))
            stats_for(label, bs, tokf[:, iw], dolf[:, iw], w, cls)


# --------------------------------------------------------------------------
# Tables
# --------------------------------------------------------------------------

def load(label):
    fn = os.path.join(OUT, f"{label}.json")
    if os.path.exists(fn):
        return json.load(open(fn))
    fe = os.path.join(A.P, "e", label, "stats.json")          # E's completion-arm labels
    if os.path.exists(fe):
        return json.load(open(fe))
    return None


def money(x):
    if x is None:
        return "-"
    a = abs(x)
    if a >= 1e6:
        return f"{x / 1e6:,.2f}m"
    if a >= 1e3:
        return f"{x / 1e3:,.0f}k"
    return f"{x:,.0f}"


def mean_abs_diff(a, b):
    a, b = np.asarray(a, float), np.asarray(b, float)
    m = ~(np.isnan(a) | np.isnan(b))
    return float(np.abs(a[m] - b[m]).mean())


def make_tables():
    cols = [("truth", "Truth"), ("truth_decoded", "Truth, decoded"),
            ("ref", "Completion, unweighted"), ("m1", "Completion, method 1"),
            ("s0_ref", "S0, unweighted"), ("s0_m1", "S0, method 1"),
            ("s1_ref_m8", "S1 8x, unweighted"), ("s1_m1_m8", "S1 8x, method 1")]
    S = {k: load(k) for k, _ in cols}
    cols = [(k, n) for k, n in cols if S[k] is not None]
    st = {k: S[k]["stats"] for k, _ in cols}
    L = []
    head = "| " + " | ".join(["Income group"] + [n for _, n in cols]) + " |"
    sep = "|" + "---|" * (len(cols) + 1)
    for what, key, fmt in (("Mean net worth", "mean", money), ("Median net worth", "median", money),
                           ("Share of all net worth", "nw_share", lambda x: f"{100 * x:.1f}%"),
                           ("Share of population weight", "pop_share", lambda x: f"{100 * x:.2f}%")):
        L += [f"### {what}, by income group", "", head, sep]
        for gi in range(5):
            row = [st["truth"]["nw_inc"][gi]["group"]]
            row += [fmt(st[k]["nw_inc"][gi][key]) for k, _ in cols]
            L.append("| " + " | ".join(row) + " |")
        L.append("")
    L += ["### Whole population", "", "| Measure | " + " | ".join(n for _, n in cols) + " |", sep]
    L.append("| Mean net worth | " + " | ".join(money(st[k]["nw_mean"]) for k, _ in cols) + " |")
    for q in ("p10", "p25", "p50", "p75", "p90", "p99"):
        L.append(f"| Net worth {q} | " + " | ".join(money(st[k]["nw_q"][q]) for k, _ in cols) + " |")
    for t in st["truth"]["nw_top"]:
        L.append(f"| Net worth share, {t} | " + " | ".join(f"{100 * st[k]['nw_top'][t]:.1f}%" for k, _ in cols) + " |")
    L.append("| Share with negative net worth | " + " | ".join(f"{100 * st[k]['share_negative_nw']:.1f}%" for k, _ in cols) + " |")
    L.append("")
    L += ["### Conditional holding rates (untargeted)", "", "| Rate | " + " | ".join(n for _, n in cols) + " |", sep]
    for i, c in enumerate(st["truth"]["cond"]):
        L.append(f"| {c['words']} | " + " | ".join(
            (f"{100 * st[k]['cond'][i]['rate']:.1f}%" if st[k]['cond'][i]['rate'] is not None else "-") for k, _ in cols) + " |")
    L.append("")
    # summaries of distances from the truth
    L += ["### Average distance from the truth over many untargeted statistics", "",
          "| Measure | " + " | ".join(n for _, n in cols[1:]) + " |", "|" + "---|" * len(cols)]
    T = st["truth"]
    def pair_vals(s):
        return [s["pair_corr"][k] if s["pair_corr"][k] is not None else np.nan for k in T["pair_corr"]]
    def inc_vals(s):
        return [s["income_corr"][v] if s["income_corr"][v] is not None else np.nan for v in WEALTH]
    def hold_ia(s):
        keys = [k for k in T["inc_age"]["hold"] if k in s["inc_age"]["hold"]]
        return np.concatenate([T["inc_age"]["hold"][k] for k in keys]), np.concatenate([s["inc_age"]["hold"][k] for k in keys])
    rows = [("Joint holding of two components (mean absolute difference, percentage points, 276 pairs)",
             lambda s: 100 * mean_abs_diff(np.array(T["M11"]), np.array(s["M11"]))),
            ("Holding rate by income group x age (mean absolute difference, points, 30 cells x 24)",
             lambda s: 100 * mean_abs_diff(*hold_ia(s))),
            ("Rank correlation between two components, among holders of both (mean absolute difference)",
             lambda s: mean_abs_diff(pair_vals(T), pair_vals(s))),
            ("Rank correlation of each component with total income, among holders (mean absolute difference)",
             lambda s: mean_abs_diff(inc_vals(T), inc_vals(s)))]
    for name, f in rows:
        L.append(f"| {name} | " + " | ".join(f"{f(st[k]):.3f}" for k, _ in cols[1:]) + " |")
    cl = []
    for k, n in cols:
        c = S[k].get("classifier") or (S[k].get("classifier_test") if isinstance(S[k].get("classifier_test"), dict) else None)
        cl.append(f"{c['auc']:.3f}" if c and "auc" in c else "-")
    L.append("| Classifier test, area under the curve (0.5 = indistinguishable from the truth) | "
             + " | ".join(cl[1:]) + " |")
    L.append("")
    txt = "\n".join(L)
    with open(os.path.join(OUT, "tables.md"), "w") as fh:
        fh.write(txt)
    print(txt)


def kl(p, q):
    p, q = np.asarray(p, float), np.asarray(q, float)
    m = p > 0
    return float((p[m] * np.log(p[m] / q[m])).sum()) if (q[m] > 0).all() else float("inf")


def splitoff():
    """How far the split-off units sit from the rest on the demographics x
    income-group table, and where the S1 draws start and end on it."""
    rec = pd.read_parquet(A.DATA / "records_train.parquet")
    names = [v["name"] for v in A.meta()["variables"]]
    truth = np.load(A.DATA / "truth_train.npy")
    src = rec["source"].astype(str).to_numpy()
    yr = rec["year"].to_numpy().astype(int)
    scf = src == "SCF"
    wo = rec["wealth_observed"].fillna(False).to_numpy().astype(bool)
    tot = rec[INCOMES].to_numpy(float).sum(1)
    wt = rec["weight"].to_numpy(float)
    thr = F.income_thresholds()
    out = {}
    recs = A.load_records()
    for wi, w in enumerate(WAVES):
        ii = np.flatnonzero(scf & (yr == w))
        dm = np.stack([truth[ii, names.index(v)] for v in SHARED_CAT], 1).astype(np.int64)
        if (dm < 0).any():
            raise ValueError("missing demographic tokens on SCF rows")
        g = F.income_group(tot[ii], np.full(len(ii), wi), thr)
        cell = ((dm[:, 0] * 2 + dm[:, 1]) * 4 + dm[:, 2]) * 5 + g
        p_all = np.bincount(cell, weights=wt[ii], minlength=240)
        p_obs = np.bincount(cell[wo[ii]], weights=wt[ii][wo[ii]], minlength=240)
        p_all, p_obs = p_all / p_all.sum(), p_obs / p_obs.sum()
        o = {"splitoff_weight_share": float(wt[ii][~wo[ii]].sum() / wt[ii].sum()),
             "kl_observed_vs_all_units": kl(p_obs, p_all)}
        wf = os.path.join(PF, "s1", "weights_m8.npz")
        if os.path.exists(os.path.join(PF, "s1", "draws_p1.npz")):
            import f_run
            z = f_run.load_s1(8)
            nm = VARS["s1"]
            m = z["wave_i"] == wi
            dmd = z["tokens"][m][:, [nm.index(v) for v in SHARED_CAT]].astype(np.int64)
            gd = F.income_group(z["dollars"][m][:, [nm.index(v) for v in INCOMES]].sum(1), np.full(m.sum(), wi), thr)
            cd = ((dmd[:, 0] * 2 + dmd[:, 1]) * 4 + dmd[:, 2]) * 5 + gd
            p_draw = np.bincount(cd, minlength=240) / m.sum()
            if os.path.exists(wf):
                w1 = np.load(wf)["w"][m]
                p_cal = np.bincount(cd, weights=w1, minlength=240) / w1.sum()
                o["kl_calibrated_vs_draws"] = kl(p_cal, p_draw)
            o.update({"kl_observed_vs_draws": kl(p_obs, p_draw), "kl_all_units_vs_draws": kl(p_all, p_draw),
                      "draw_share_by_income_group": np.bincount(gd, minlength=5).tolist(),
                      "total_variation_draws_vs_all_units": float(0.5 * np.abs(p_draw - p_all).sum()),
                      "total_variation_draws_vs_observed": float(0.5 * np.abs(p_draw - p_obs).sum())})
        out[str(w)] = o
    print(json.dumps(out, indent=1))
    os.makedirs(OUT, exist_ok=True)
    json.dump(out, open(os.path.join(OUT, "splitoff.json"), "w"), indent=1)


if __name__ == "__main__":
    if sys.argv[1] == "stats":
        run_stats()
    elif sys.argv[1] == "splitoff":
        splitoff()
    else:
        make_tables()
