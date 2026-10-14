"""Test 3, piece F: draws, targets and method 1 calibration for the
from-scratch arms. See f_common.py for the definitions.

    python f_run.py selftest            # B's calibrate_matrix on fabricated data,
                                        # incl. the one-big-group (S1) case
    python f_run.py check               # GPU: F's sampler reproduces A's dev draws;
                                        # thresholds, layouts and targets
    python f_run.py draw s0             # GPU: K = 32 per record
    python f_run.py draw s1 --part 1 --mult 8    # GPU: parts nest (1: 8x, 2: +24x, 3: +32x)
    python f_run.py prep s0 | prep s1   # decode dollars, groups, targets
    python f_run.py calib s0 [--k 8 16 32]
    python f_run.py calib s1 --mult 1 4 8 32 64

Outputs under $P/f/<arm>/ (see SPEC.md, contract change of 20:25).
"""
import argparse
import json
import os
import sys
import time

import numpy as np
import pandas as pd

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, HERE)
import f_common as F                    # noqa: E402
from f_common import A, WEALTH, INCOMES, WAVES, FAMILIES, VT, SHARED_CAT, VARS, log  # noqa: E402

PF = str(F.PF)
SEEDS = {"s0": 20261201, "s1_p1": 20261211, "s1_p2": 20261212, "s1_p3": 20261213,
         "dollars": 20261221}
S1_PARTS = {1: 8, 2: 24, 3: 32}          # multiples of the wave's real count, nested


def jdump(obj, path):
    with open(path, "w") as fh:
        json.dump(obj, fh, indent=1, default=lambda x: x.item() if hasattr(x, "item") else str(x))


# --------------------------------------------------------------------------
# Self-test of B's calibrate_matrix, including the one-big-group case
# --------------------------------------------------------------------------

def selftest():
    import b_calibrate as B
    out = {}
    rng = np.random.default_rng(7)
    # One group holding all draws (S1's layout): N = 1 record, K draws.
    K = 20000
    sizes = [6, 4, 5]
    p_draw = [rng.dirichlet(np.ones(s) * 20) for s in sizes]   # every type present
    X = np.stack([rng.choice(s, K, p=p) for s, p in zip(sizes, p_draw)], axis=1)
    # Truth: another distribution on the same types (all 120 types present
    # among the draws, so the targets are feasible).
    types = np.array(np.meshgrid(*[np.arange(s) for s in sizes], indexing="ij")).reshape(3, -1).T
    q_true = rng.dirichlet(np.ones(len(types)) * 0.7)
    present = {tuple(x) for x in X}
    ok_types = all(tuple(t) in present for t in types)
    # cells: a x b, a x c, b x c, and the total
    def cells_of(Z):
        ab = Z[:, 0] * 4 + Z[:, 1]
        ac = 24 + Z[:, 0] * 5 + Z[:, 2]
        bc = 54 + Z[:, 1] * 5 + Z[:, 2]
        tot = np.full(len(Z), 74)
        return np.stack([ab, ac, bc, tot], axis=1).astype(np.int32)
    C = 75
    Wtot = 1.0e8
    t = np.bincount(cells_of(types).reshape(-1), weights=np.repeat(q_true * Wtot, 4), minlength=C)
    meta = pd.DataFrame({"wave": 0, "family": ["ab"] * 24 + ["ac"] * 30 + ["bc"] * 20 + ["total"],
                         "group": 0, "variable": "x"})
    w, lam, rep = B.calibrate_matrix(cells_of(X), np.array([Wtot]), t, K, cell_meta=meta, verbose=False)
    ach = np.bincount(cells_of(X).reshape(-1), weights=np.repeat(w.reshape(-1), 4), minlength=C)
    act = np.array(rep["cell_status"]) == "active"
    out["one_group"] = {"all_types_present": ok_types, "converged": rep["converged"],
                        "status_counts": rep["status_counts"],
                        "max_rel_error_independent": float((np.abs(ach - t)[act] / t[act]).max()),
                        "weights_sum_rel_error": float(abs(w.sum() - Wtot) / Wtot),
                        "kl": rep["kl_nats_per_unit_weight"],
                        "effective_draws": rep["effective_draws_weighted_mean"],
                        "newton": rep["newton_iters_total"]}
    log(f"one group: {out['one_group']}")
    # An infeasible piece: a cell no draw reaches.
    X2 = X.copy()
    X2[(X2[:, 0] == 0) & (X2[:, 1] == 0), 1] = 1       # no draw has a=0, b=0
    w2, _, rep2 = B.calibrate_matrix(cells_of(X2), np.array([Wtot]), t, K, cell_meta=meta, verbose=False)
    out["one_group_unreachable"] = {"status_counts": rep2["status_counts"], "converged": rep2["converged"],
                                    "feasible_max_rel_error": rep2["feasible_max_rel_error"]}
    log(f"one group with an unreachable cell: {out['one_group_unreachable']}")
    # Many records (S0's layout): B's own fabricated problem.
    draws, rec, cells, cf = B.fabricate(seed=1, include_truth=True)
    w3, _, rep3 = B.calibrate(draws, rec, cells, contrib_fn=cf, verbose=False)
    out["records_feasible"] = {k: rep3[k] for k in ("converged", "feasible_max_rel_error",
                                                     "record_total_max_rel_error", "status_counts")}
    log(f"records: {out['records_feasible']}")
    os.makedirs(os.path.join(PF, "dev"), exist_ok=True)
    jdump(out, os.path.join(PF, "dev", "selftest.json"))


# --------------------------------------------------------------------------
# Checks: F's sampler against A's
# --------------------------------------------------------------------------

def check():
    import torch
    recs = A.load_records()
    out = {}
    model, G = F.load_model("cuda")
    F.set_units(G, "c", recs)
    out["completion_order"] = G.order_names
    dev = A.load_draws("dev")
    Kd = int(dev["K"]) if "K" in dev else dev["tokens"].shape[1]
    seed = int(dev["seed"])
    gen = torch.Generator(device=G.device)
    gen.manual_seed(seed)
    t0 = time.time()
    tok, lpv = F.sample(model, G, np.repeat(np.arange(G.n_units), Kd), gen)
    out["reproduce_dev_seconds"] = time.time() - t0
    tok = tok.reshape(G.n_units, Kd, -1)
    lpv = lpv.reshape(G.n_units, Kd, -1)
    out["dev_tokens_identical_share"] = float((tok == dev["tokens"]).all(axis=2).mean())
    out["dev_logp_var_max_abs_diff"] = float(np.abs(lpv - dev["logp_ref_var"]).max())
    # log-probabilities of A's dev tokens through F's scorer (first 4000 records)
    n = 4000
    ri = np.repeat(np.arange(n), Kd)
    lp2 = F.logprob(model, G, ri, dev["tokens"][:n].reshape(n * Kd, -1))
    out["dev_logprob_max_abs_diff"] = float(np.abs(lp2 - dev["logp_ref_var"][:n].reshape(n * Kd, -1)).max())
    log(f"sampler check: {out}")
    # S0 and S1 orders
    F.set_units(G, "s0", recs)
    out["s0_order"] = G.order_names
    F.set_units(G, "s1", wave_i=np.zeros(4, dtype=np.int64))
    out["s1_order"] = G.order_names
    out["allowed_notes"] = G.allowed_notes
    # thresholds
    thr = F.income_thresholds()
    g = F.income_group(recs["inc_total"].to_numpy(), recs["wave_i"].to_numpy(), thr)
    out["thresholds"] = thr.tolist()
    out["inc_group_agree_with_A"] = float((g == recs["g_inc"].to_numpy()).mean())
    # layouts and targets
    tA = A.load_targets()
    for arm in ("s0", "s1"):
        FL = F.FLayout(arm)
        tab, _ = F.targets(FL, recs)
        wc = tab[tab["a_cell"] >= 0]
        diff = np.abs(wc["target"].to_numpy() - tA["target"].to_numpy()[wc["a_cell"].to_numpy()]).max()
        out[f"{arm}_cells"] = {"n_cells": FL.n_cells, "per_draw": FL.per_draw,
                               "wealth_cells_max_abs_diff_vs_A": float(diff),
                               "n_zero_cells": int(tab["zero_cell"].sum()),
                               "by_kind": tab.groupby("var_kind").size().to_dict()}
    log(json.dumps(out, indent=1, default=str))
    os.makedirs(os.path.join(PF, "dev"), exist_ok=True)
    jdump(out, os.path.join(PF, "dev", "checks.json"))


# --------------------------------------------------------------------------
# Draws
# --------------------------------------------------------------------------

def draw(arm, part=None, mult=None, k=32, batch_size=16384):
    import torch
    recs = A.load_records()
    model, G = F.load_model("cuda")
    os.makedirs(os.path.join(PF, arm), exist_ok=True)
    t0 = time.time()
    if arm == "s0":
        F.set_units(G, "s0", recs)
        gen = torch.Generator(device=G.device)
        gen.manual_seed(SEEDS["s0"])
        unit = np.repeat(np.arange(G.n_units), k)
        tok, lpv = F.sample(model, G, unit, gen, batch_size=batch_size)
        wave_i = recs["wave_i"].to_numpy()[unit]
        fn = os.path.join(PF, "s0", "draws.npz")
        seed = SEEDS["s0"]
    else:
        n_real = np.bincount(recs["wave_i"].to_numpy(), minlength=len(WAVES))
        seed = SEEDS[f"s1_p{part}"]
        toks, lps, wvs = [], [], []
        for wi in range(len(WAVES)):
            n = int(mult * n_real[wi])
            wv = np.full(n, wi, dtype=np.int64)
            F.set_units(G, "s1", wave_i=wv)
            gen = torch.Generator(device=G.device)
            gen.manual_seed(seed * 10 + wi)
            tk, lp = F.sample(model, G, np.arange(n), gen, batch_size=batch_size)
            toks.append(tk), lps.append(lp), wvs.append(wv)
            log(f"  wave {WAVES[wi]}: {n:,} synthetic records")
        tok, lpv, wave_i = np.concatenate(toks), np.concatenate(lps), np.concatenate(wvs)
        fn = os.path.join(PF, "s1", f"draws_p{part}.npz")
    secs = time.time() - t0
    log(f"{arm}: {len(tok):,} draws in {secs:.1f}s")
    # dollars of incomes and wealth
    names = VARS[arm]
    dol = F.decode(tok, names, wave_i, SEEDS["dollars"] + (part or 0)).to_numpy(np.float64)
    shape = (G.n_units, k) if arm == "s0" else (len(tok),)
    np.savez(fn, tokens=tok.reshape(*shape, -1), logp_var=lpv.reshape(*shape, -1),
             logp=lpv.astype(np.float64).sum(-1).astype(np.float32).reshape(shape),
             dollars=dol.reshape(*shape, -1), wave_i=wave_i.reshape(shape),
             vars=np.array(names), seed=np.int64(seed), dollars_seed=np.int64(SEEDS["dollars"] + (part or 0)),
             seconds=np.float64(secs), mult=np.int64(mult or 0), K=np.int64(k if arm == "s0" else 1))
    log(f"wrote {fn}")


def load_s1(mult):
    """S1 draws for a multiple of each wave's real count: the first
    mult * n_real draws of each wave across the nested parts."""
    recs = A.load_records()
    n_real = np.bincount(recs["wave_i"].to_numpy(), minlength=len(WAVES))
    parts, have = [], 0
    for p, m in S1_PARTS.items():
        if have >= mult:
            break
        fn = os.path.join(PF, "s1", f"draws_p{p}.npz")
        if not os.path.exists(fn):
            raise FileNotFoundError(f"{fn} needed for mult {mult}")
        z = np.load(fn)
        parts.append({k: z[k] for k in ("tokens", "logp_var", "logp", "dollars", "wave_i")})
        have += m
    if have < mult:
        raise ValueError(f"only {have}x drawn, {mult}x asked")
    out = {k: [] for k in parts[0]}
    for wi in range(len(WAVES)):
        need = int(mult * n_real[wi])
        for z in parts:
            m = z["wave_i"] == wi
            take = np.flatnonzero(m)[:need]
            for k in out:
                out[k].append(z[k][take])
            need -= len(take)
            if need == 0:
                break
        if need:
            raise ValueError("not enough draws")
    return {k: np.concatenate(v) for k, v in out.items()}


# --------------------------------------------------------------------------
# Groups, contribution index, targets
# --------------------------------------------------------------------------

def draw_arrays(arm, z, recs):
    """(wave_i [R], vt tokens [R, 31], demo tokens [R, 3], income total
    [R], income group [R]) for flattened draws."""
    names = list(VARS[arm])
    tok = z["tokens"].reshape(-1, len(names)).astype(np.int64)
    dol = z["dollars"].reshape(-1, len(names))
    wave_i = z["wave_i"].reshape(-1).astype(np.int64)
    vt = tok[:, [names.index(v) for v in VT]]
    if arm == "s0":
        K = z["tokens"].shape[1]
        dm = np.repeat(np.stack([recs["tok_" + v].to_numpy() for v in SHARED_CAT], axis=1), K, axis=0)
    else:
        dm = tok[:, [names.index(v) for v in SHARED_CAT]]
    tot = dol[:, [names.index(v) for v in INCOMES]].sum(axis=1)
    g = F.income_group(tot, wave_i, F.income_thresholds())
    return wave_i, vt, dm.astype(np.int64), tot, g


def prep(arm, mult=None):
    recs = A.load_records()
    FL = F.FLayout(arm)
    tab, _ = F.targets(FL, recs)
    tab.to_parquet(os.path.join(PF, arm, "targets.parquet"))
    log(f"{arm}: {FL.n_cells:,} cells, {int(tab['zero_cell'].sum()):,} zero cells")


# --------------------------------------------------------------------------
# Calibration
# --------------------------------------------------------------------------

def kl_parts(w, W, K, groups):
    """Split the KL of the weights from the starting weights into the move
    between the categories of `groups` (one category per draw, [N, K]) and
    the move within them (chain rule)."""
    q = w / W[:, None]
    p0 = np.full_like(q, 1.0 / K)
    wq = W[:, None] * q
    w0 = W[:, None] * p0
    a = pd.Series(wq.reshape(-1)).groupby(groups.reshape(-1)).sum()
    b = pd.Series(w0.reshape(-1)).groupby(groups.reshape(-1)).sum()
    a, b = a / W.sum(), b / W.sum()
    between = float((a[a > 0] * np.log(a[a > 0] / b[a > 0])).sum())
    return between


def calib_s0(ks):
    import b_calibrate as B
    recs = A.load_records()
    tab = pd.read_parquet(os.path.join(PF, "s0", "targets.parquet"))
    FL = F.FLayout("s0")
    z = np.load(os.path.join(PF, "s0", "draws.npz"))
    z = {k: z[k] for k in z.files}
    N, Kall = z["tokens"].shape[:2]
    W = recs["weight"].to_numpy(np.float64)
    t = tab["target"].to_numpy(np.float64)
    for k in ks:
        zk = {kk: (v[:, :k] if kk in ("tokens", "dollars", "wave_i") else v) for kk, v in z.items()}
        wave_i, vt, dm, tot, g = draw_arrays("s0", zk, recs)
        idx = FL.index(wave_i, vt, dm, g)
        log(f"s0 K={k}: index {idx.shape}")
        w, lam, rep = B.calibrate_matrix(idx, W, t, k, cell_meta=tab.drop(columns=["bin"]))   # no block check: draws of one record span groups
        rep["arm"], rep["K"] = "s0", int(k)
        rep["kl_between_income_groups"] = kl_parts(w, W, k, g.reshape(N, k))
        # share of draws whose income group differs from the record's real one
        rep["share_draws_income_group_differs_unweighted"] = float(
            (g.reshape(N, k) != recs["g_inc"].to_numpy()[:, None]).mean())
        rep["share_weight_income_group_differs_calibrated"] = float(
            (w * (g.reshape(N, k) != recs["g_inc"].to_numpy()[:, None])).sum() / W.sum())
        name = "" if k == Kall else f"_K{k}"
        st = np.array(rep.pop("cell_status"))
        rep.pop("reach_any", None), rep.pop("forced", None)
        np.savez(os.path.join(PF, "s0", f"weights{name}.npz"), w=w, **{"lambda": lam},
                 cell_status=st, inc_group=g.reshape(N, k).astype(np.int8))
        rep["infeasible_cells_by_family_and_kind"] = infeasible_table(tab, st)
        jdump(rep, os.path.join(PF, "s0", f"report{name}.json"))
        B.print_summary(f"s0 K={k}", rep)
    open(os.path.join(PF, "DONE_s0"), "w").write(time.strftime("%Y-%m-%d %H:%M:%S\n"))


def infeasible_table(tab, st):
    bad = np.isin(st, ["unreachable", "above_reach", "below_forced", "joint", "zero_relaxed"])
    d = tab.assign(bad=bad, st=st, tbad=np.where(bad, tab["target"], 0.0))
    fam = d.groupby(["var_kind", "family"]).agg(cells=("cell", "size"), infeasible=("bad", "sum"),
                                               target=("target", "sum"), target_infeasible=("tbad", "sum"))
    return {f"{a}|{b}": r._asdict() if hasattr(r, "_asdict") else r
            for (a, b), r in fam.to_dict("index").items()}


def calib_s1(mults):
    import b_calibrate as B
    recs = A.load_records()
    tab = pd.read_parquet(os.path.join(PF, "s1", "targets.parquet"))
    FL = F.FLayout("s1")
    Wwave = recs.groupby("wave_i")["weight"].sum().to_numpy()
    for mult in mults:
        z = load_s1(mult)
        wave_i, vt, dm, tot, g = draw_arrays("s1", z, recs)
        idx = FL.index(wave_i, vt, dm, g)
        R = len(idx)
        w_all = np.zeros(R)
        lam_all = np.zeros(FL.n_cells)
        st_all = np.array(["active"] * FL.n_cells, dtype=object)
        reps = {}
        for wi in range(len(WAVES)):
            rows = np.flatnonzero(wave_i == wi)
            cw = np.flatnonzero(tab["wave_i"].to_numpy() == wi)
            remap = np.full(FL.n_cells, -1, dtype=np.int64)
            remap[cw] = np.arange(len(cw))
            iw = remap[idx[rows]]
            if (iw < 0).any():
                raise ValueError("draw touches a cell of the other wave")
            meta = tab.iloc[cw].reset_index(drop=True)
            log(f"s1 {mult}x wave {WAVES[wi]}: {len(rows):,} draws, {len(cw):,} cells")
            w, lam, rep = B.calibrate_matrix(iw.astype(np.int32), np.array([Wwave[wi]]),
                                             meta["target"].to_numpy(), len(rows), cell_meta=meta.drop(columns=["bin"]))
            w_all[rows] = w.reshape(-1)
            lam_all[cw] = lam
            st_all[cw] = rep.pop("cell_status")
            rep.pop("reach_any", None), rep.pop("forced", None)
            # effective sample size of the wave and KL split by demographics
            q = w.reshape(-1) / Wwave[wi]
            rep["ess"] = float(1.0 / (q ** 2).sum())
            rep["ess_share_of_draws"] = rep["ess"] / len(rows)
            rep["ess_over_real_records"] = rep["ess"] / int((recs["wave_i"] == wi).sum())
            demo = (dm[rows, 0] * 2 + dm[rows, 1]) * 4 + dm[rows, 2]
            rep["kl_between_demographic_cells"] = kl_parts(w, np.array([Wwave[wi]]), len(rows), demo[None, :])
            rep["kl_between_demo_x_income_group"] = kl_parts(
                w, np.array([Wwave[wi]]), len(rows), (demo * 5 + g[rows])[None, :])
            reps[str(WAVES[wi])] = rep
            B.print_summary(f"s1 {mult}x wave {WAVES[wi]}", rep)
        kl = sum(reps[str(WAVES[wi])]["kl_nats_per_unit_weight"] * Wwave[wi] for wi in range(len(WAVES))) / Wwave.sum()
        out = {"arm": "s1", "mult": mult, "n_draws": R, "kl_nats_per_unit_weight": kl,
               "ess_by_wave": {k: v["ess"] for k, v in reps.items()},
               "converged": all(v["converged"] for v in reps.values()),
               "feasible_max_rel_error": max(v["feasible_max_rel_error"] for v in reps.values()),
               "status_counts": {s: int((st_all == s).sum()) for s in B.STATUSES},
               "infeasible_cells_by_family_and_kind": infeasible_table(tab, st_all),
               "by_wave": reps}
        name = f"_m{mult}"
        np.savez(os.path.join(PF, "s1", f"weights{name}.npz"), w=w_all, **{"lambda": lam_all},
                 cell_status=st_all.astype(str), inc_group=g.astype(np.int8), wave_i=wave_i.astype(np.int8))
        jdump(out, os.path.join(PF, "s1", f"report{name}.json"))
        log(f"s1 {mult}x: KL {kl:.4f}, ESS {out['ess_by_wave']}, status {out['status_counts']}")
    open(os.path.join(PF, "DONE_s1"), "w").write(time.strftime("%Y-%m-%d %H:%M:%S\n"))


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("stage")
    ap.add_argument("arm", nargs="?")
    ap.add_argument("--part", type=int)
    ap.add_argument("--mult", type=int, nargs="+")
    ap.add_argument("--k", type=int, nargs="+", default=[32])
    a = ap.parse_args()
    os.makedirs(PF, exist_ok=True)
    if a.stage == "selftest":
        selftest()
    elif a.stage == "check":
        check()
    elif a.stage == "draw":
        draw(a.arm, part=a.part, mult=(a.mult or [None])[0])
    elif a.stage == "prep":
        prep(a.arm)
    elif a.stage == "calib":
        if a.arm == "s0":
            calib_s0(a.k)
        else:
            calib_s1(a.mult)
    else:
        raise SystemExit(a.stage)


if __name__ == "__main__":
    main()
