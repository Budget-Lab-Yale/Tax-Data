"""Test 3, piece A: build the shared files.

    python a_build.py common     records.parquet, targets.parquet, cells.json,
                                 checks (CPU is enough)            -> DONE_common
    python a_build.py dev        dev_draws.npz (K = 4) and the log-probability
                                 checks against test 2 (GPU)       -> DONE_dev
    python a_build.py ref        ref_draws.npz (K = 32) + ref_dollars.parquet
                                 -> DONE_ref; then ref_draws_b.npz (second,
                                 independent K = 32) + ref_dollars_b.parquet
                                 -> DONE_ref_b; then ref_draws_k128.npz
                                 -> DONE_ref_k128 (GPU)
Outputs in $P/a/. Seeds are fixed below and written into each file.
"""

import json
import os
import sys
import time
from pathlib import Path

import numpy as np
import pandas as pd

sys.path.insert(0, str(Path(__file__).resolve().parent))
import a_common as A                                   # noqa: E402
from a_common import WEALTH, INCOMES, FAMILIES, log    # noqa: E402

SEEDS = dict(decode_true=20261110, dev=20261101, ref=20261102, ref_b=20261103,
             ref_k128=20261104, decode_ref=20261112, decode_ref_b=20261113)
PA = A.PA


def marker(name, info):
    with open(PA / f"DONE_{name}", "w") as f:
        f.write(json.dumps(dict(info, time=time.strftime("%Y-%m-%d %H:%M:%S"),
                                slurm_job=os.environ.get("SLURM_JOB_ID")), indent=1) + "\n")


def stage_common():
    PA.mkdir(parents=True, exist_ok=True)
    recs, notes = A.build_records()
    log(f"records: {len(recs):,}; notes {notes}")
    # true tokens decoded to dollars, for like-with-like comparisons
    dec = A.decode_dollars(A.true_tokens(recs), recs, SEEDS["decode_true"])
    for k, v in enumerate(WEALTH):
        recs["dec_" + v] = dec[:, k]
    recs["nw_dec"] = A.nw_from(dec)
    recs.to_parquet(PA / "records.parquet", index=False)
    log("wrote records.parquet")

    tab = A.targets(recs)
    L = A.layout()
    checks = {}

    # 1. Independent computation of the targets (pandas group-by, no layout code).
    W = recs["weight"].to_numpy()
    wb = A.wealth_bins()
    rows = []
    for f in FAMILIES:
        for v in WEALTH:
            labels, t2b = wb[v]
            df = pd.DataFrame({"wave": recs["year"].to_numpy(), "group": recs["g_" + f].to_numpy(),
                               "bin_label": np.array(labels)[t2b[recs["tok_" + v].to_numpy()]],
                               "w": W})
            g = df.groupby(["wave", "group", "bin_label"], as_index=False).agg(t2=("w", "sum"), n2=("w", "size"))
            g["family"], g["variable"] = f, v
            rows.append(g)
    alt = pd.concat(rows, ignore_index=True)
    m = tab.merge(alt, on=["wave", "family", "group", "variable", "bin_label"], how="left")
    if len(m) != len(tab):
        raise RuntimeError("merge changed the number of cells")
    m["t2"] = m["t2"].fillna(0.0)      # cells with no record in the group-by are zero cells
    m["n2"] = m["n2"].fillna(0).astype(np.int64)
    n_alt_unmatched = len(alt) - int(m["n2"].gt(0).sum())
    checks["independent_targets_max_abs_diff"] = float(np.abs(m["target"] - m["t2"]).max())
    checks["independent_counts_equal"] = bool((m["n_records"] == m["n2"]).all())
    checks["groupby_cells_not_in_layout"] = int(n_alt_unmatched)

    # 2. Family sums: within (wave, family, variable) cells sum to the wave total.
    wave_tot = recs.groupby("year")["weight"].sum()
    s = tab.groupby(["wave", "family", "variable"])["target"].sum().reset_index()
    s["wave_total"] = s["wave"].map(wave_tot)
    s["rel"] = (s["target"] - s["wave_total"]).abs() / s["wave_total"]
    checks["family_sum_max_rel_diff"] = float(s["rel"].max())
    checks["wave_totals"] = {int(k): float(v) for k, v in wave_tot.items()}
    checks["wave_records"] = {int(k): int(v) for k, v in recs.groupby("year").size().items()}

    # 3. Contribution matrix on the true tokens with the true weights.
    Acsr = A.contrib(A.true_tokens(recs), recs, dtype=np.float64)
    tA = Acsr.T @ W
    checks["contrib_true_max_abs_diff"] = float(np.abs(tA - tab["target"].to_numpy()).max())
    checks["contrib_row_nnz"] = sorted(set(np.diff(Acsr.indptr).tolist()))
    checks["contrib_distinct_cells_per_row"] = int(
        min(len(np.unique(Acsr.indices[Acsr.indptr[i]:Acsr.indptr[i + 1]])) for i in range(0, Acsr.shape[0], 997)))

    # 4. Counts.
    feas = tab[~tab.zero_cell]
    by_fam = tab.groupby("family").agg(cells=("cell", "size"), zero=("zero_cell", "sum"),
                                       lt5_records=("n_records", lambda x: int(((x > 0) & (x < 5)).sum())),
                                       lt5_households=("n_households", lambda x: int(((x > 0) & (x < 5)).sum())))
    counts = dict(cells=int(len(tab)), zero_cells=int(tab.zero_cell.sum()),
                  positive_cells=int(len(feas)),
                  positive_cells_lt5_records=int((feas.n_records < 5).sum()),
                  positive_cells_lt5_households=int((feas.n_households < 5).sum()),
                  positive_cells_1_household=int((feas.n_households == 1).sum()),
                  by_family={f: {k: int(v) for k, v in r.items()} for f, r in by_fam.iterrows()},
                  zero_cells_by_bin=tab[tab.zero_cell].groupby("bin_label").size().astype(int).to_dict(),
                  zero_cells_by_wave=tab[tab.zero_cell].groupby("wave").size().astype(int).to_dict())
    # zero cells that are structural: negative bin in a wave with no negative quantile function
    al = A.allowed_codes()
    struct = 0
    for wi, w in enumerate(A.WAVES):
        for k, v in enumerate(WEALTH):
            labels, t2b = wb[v]
            if "neg" in labels and not al[wi, k, :len(t2b)][t2b == 0].any():
                struct += int(((tab.wave == w) & (tab.variable == v) & (tab.bin_label == "neg")).sum())
    counts["zero_cells_structural_no_negative_code_in_wave"] = struct
    log(f"checks {json.dumps(checks)}")
    log(f"counts {json.dumps(counts, default=int)}")
    ok = (checks["independent_targets_max_abs_diff"] < 1e-9 * max(wave_tot)
          and checks["independent_counts_equal"] and checks["groupby_cells_not_in_layout"] == 0
          and checks["family_sum_max_rel_diff"] < 1e-12 and checks["contrib_true_max_abs_diff"] < 1e-3
          and checks["contrib_row_nnz"] == [A.PER_DRAW]
          and checks["contrib_distinct_cells_per_row"] == A.PER_DRAW)
    if not ok:
        raise RuntimeError(f"checks failed: {checks}")
    tab.to_parquet(PA / "targets.parquet", index=False)

    cj = dict(
        description="Test 3 target cells. cell = block * VB + vb_start[var_i] + bin. See a_common.py.",
        waves=A.WAVES, families=FAMILIES, wealth=WEALTH, incomes=INCOMES,
        groups=L.groups, wealth_bins={v: wb[v][0] for v in WEALTH},
        VB=L.VB, vb_start=L.vb_start.tolist(), n_blocks=L.n_blocks, n_cells=L.n_cells,
        per_draw=A.PER_DRAW,
        blocks=L.blocks[["block", "wave", "family", "group", "group_label"]].to_dict(orient="records"),
        records=int(len(recs)), record_notes=notes, checks=checks, counts=counts,
        seeds=SEEDS, checkpoint=str(A.CKPT), data=str(A.DATA))
    with open(PA / "cells.json", "w") as f:
        json.dump(cj, f, indent=1, default=int)
    marker("common", dict(records=len(recs), cells=len(tab)))
    log("wrote targets.parquet, cells.json, DONE_common")


def nll_checks(model, ctx, recs, n=512):
    """ref_logprob vs test 2's completion_nll on the first n records of each
    wave (true tokens)."""
    import torch
    from b_model import completion_nll
    out = {}
    pick = np.concatenate([np.flatnonzero(recs["year"].to_numpy() == w)[:n // 2] for w in A.WAVES])
    tt = A.true_tokens(recs.iloc[pick])
    lp_free = A.ref_logprob(model, ctx, pick, tt, allowed=False)
    lp_allw = A.ref_logprob(model, ctx, pick, tt, allowed=True)
    tok_in = ctx.tok_in[pick].cpu().numpy()
    truth = tok_in.copy()
    truth[:, ctx.wslot] = tt
    src = ctx.src[pick].cpu().numpy()
    yr = ctx.year[pick].cpu().numpy()
    for amp in (False, True):
        nll, sc = completion_nll(model, tok_in, truth, src, yr, ctx.order, 4096, ctx.device, amp=amp)
        if not sc[:, ctx.wslot].all() or sc.sum() != len(pick) * A.NV:
            raise RuntimeError("completion_nll scored an unexpected set of slots")
        ref = -nll[:, ctx.wslot]
        key = "bf16" if amp else "float32"
        out[f"completion_nll_{key}_vs_ref_logprob_unrestricted_max_abs"] = float(np.abs(ref - lp_free).max())
        out[f"completion_nll_{key}_vs_ref_logprob_unrestricted_mean_abs_per_record_sum"] = float(
            np.abs(ref.sum(1) - lp_free.sum(1)).mean())
    out["allowed_restriction_effect_max"] = float((lp_allw - lp_free).max())
    out["allowed_restriction_effect_mean_per_record"] = float((lp_allw - lp_free).sum(1).mean())
    out["true_tokens_mean_logp_per_record"] = float(lp_allw.sum(1).mean())
    out["true_tokens_neg_inf"] = int(np.isinf(lp_allw).sum())
    out["n_records_checked"] = int(len(pick))
    return out


def stage_dev():
    import torch
    recs = A.load_records()
    model, ctx = A.load_reference("cuda", recs)
    log(f"order: {ctx.order_names}; dropped {ctx.dropped}; allowed notes {ctx.allowed_notes}; "
        f"GPU {torch.cuda.get_device_name(0)}")
    checks = nll_checks(model, ctx, recs)
    log(f"log-probability checks {json.dumps(checks)}")
    d = A.draw_set(model, ctx, 4, SEEDS["dev"])
    # sampled log-probabilities equal teacher-forced ones on the same tokens
    sub = np.arange(0, ctx.N, 50)
    lp_tf = A.ref_logprob(model, ctx, sub, d["tokens"][sub, 0].astype(np.int64))
    checks["sample_vs_teacher_forced_max_abs"] = float(np.abs(lp_tf - d["logp_ref_var"][sub, 0]).max())
    checks["draws_neg_inf"] = int(np.isinf(d["logp_ref_var"]).sum())
    # contributions of the draws
    idx = A.contrib_index(d["tokens"], recs)
    w = np.repeat(recs["weight"].to_numpy() / 4, 4)
    tot = A.cell_totals(idx, w)
    tgt = A.load_targets()["target"].to_numpy()
    checks["dev_draws_total_weight_rel_diff"] = float(abs(tot.sum() / A.PER_DRAW - w.sum()) / w.sum())
    checks["dev_draws_weight_in_zero_cells_share"] = float(tot[tgt == 0].sum() / tot.sum())
    np.savez(PA / "dev_draws.npz", **d)
    with open(PA / "dev_checks.json", "w") as f:
        json.dump(dict(checks=checks, order=ctx.order_names, allowed_notes=ctx.allowed_notes,
                       gpu=torch.cuda.get_device_name(0)), f, indent=1)
    log(f"checks {json.dumps(checks)}")
    if checks["sample_vs_teacher_forced_max_abs"] > 1e-3 or checks["draws_neg_inf"]:
        raise RuntimeError("draw log-probabilities inconsistent")
    marker("dev", dict(K=4, seed=SEEDS["dev"], checks=checks))
    log("wrote dev_draws.npz, DONE_dev")


def dollars_frame(d, recs, seed):
    dol = A.decode_dollars(d["tokens"], recs, seed)               # [N, K, 24]
    N, K, _ = dol.shape
    df = pd.DataFrame({"rec": np.repeat(np.arange(N), K).astype(np.int32),
                       "draw": np.tile(np.arange(K), N).astype(np.int16)})
    flat = dol.reshape(N * K, A.NV)
    for k, v in enumerate(WEALTH):
        df[v] = flat[:, k]
    df["nw"] = A.nw_from(flat)
    return df


def stage_ref(which):
    import torch
    recs = A.load_records()
    model, ctx = A.load_reference("cuda", recs)
    log(f"GPU {torch.cuda.get_device_name(0)}")
    for name in which:
        K = 128 if name == "ref_k128" else 32
        d = A.draw_set(model, ctx, K, SEEDS[name])
        if np.isinf(d["logp_ref_var"]).any():
            raise RuntimeError(f"{name}: -inf log-probabilities")
        fn = {"ref": "ref_draws.npz", "ref_b": "ref_draws_b.npz", "ref_k128": "ref_draws_k128.npz"}[name]
        np.savez(PA / fn, **d)
        log(f"wrote {fn}")
        info = dict(K=K, seed=SEEDS[name], file=fn)
        if name in ("ref", "ref_b"):
            df = dollars_frame(d, recs, SEEDS["decode_" + name])
            dn = "ref_dollars.parquet" if name == "ref" else "ref_dollars_b.parquet"
            df.to_parquet(PA / dn, index=False)
            info.update(dollars=dn, decode_seed=SEEDS["decode_" + name])
            log(f"wrote {dn}")
        marker(name, info)


if __name__ == "__main__":
    st = sys.argv[1]
    if st == "common":
        stage_common()
    elif st == "dev":
        stage_dev()
    elif st == "ref":
        stage_ref(sys.argv[2].split(",") if len(sys.argv) > 2 else ["ref", "ref_b", "ref_k128"])
    else:
        raise SystemExit(f"unknown stage {st}")
