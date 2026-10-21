"""Test 3, piece D: fabricated development data for the PUF arm (arm P), to
test d_tune.py's PUF code path (SCF source tag, year 2015, missing age left
unobserved, records with no block in a family) before the full model's
files exist. Pilot model (expB) only. Nothing here is a result.

Records: every n-th PUF training record of expB. Families inc, age, mar,
dep (P_core's four), one wave (2015); a record with unknown age has block
-1 in the age family. Targets: the reference network's own expected cells
on these records, each table's cells multiplied by exp(0.3 z) with z
standard normal and rescaled to the table's total, so the targets differ
from the reference by a known, feasible amount. Reference draws (K = 8) and
their method-1 weights (piece B's calibrate_matrix) for variant 1.

Writes $P/dev/d_puf/: records.parquet, targets.parquet, draws.npz,
weights.npz, calib.json.
"""

import json
import sys
from pathlib import Path

import numpy as np
import pandas as pd
import torch

HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(HERE.parent / "scf_joint"))
sys.path.insert(0, str(HERE))

import a_common as A  # noqa: E402

OUT = A.P / "dev" / "d_puf"
FAMS = ["inc", "age", "mar", "dep"]


def main(n=4000, K=8):
    OUT.mkdir(parents=True, exist_ok=True)
    m = A.meta()
    names = [v["name"] for v in m["variables"]]
    rec = pd.read_parquet(A.DATA / "records_train.parquet")
    tok = np.load(A.DATA / "tokens_train.npy").astype(np.int64)
    puf = np.flatnonzero(rec["source"].astype(str).to_numpy() == "PUF")
    tot_all = rec[A.INCOMES].to_numpy(float)[puf].sum(1)
    w_all = rec["weight"].to_numpy(float)[puf]
    rank = A.wrank(tot_all, w_all)
    ginc_all = np.searchsorted(np.array(A.INC_EDGES), rank, side="right")
    sel = np.arange(0, len(puf), len(puf) // n)[:n]
    rows = puf[sel]
    out = pd.DataFrame({"rec": np.arange(len(rows)), "row": rows, "source": "PUF",
                        "year": 2015, "weight": w_all[sel], "wave_i": 0})
    groups = {"inc": ginc_all[sel],
              "age": tok[rows, names.index("age_range")],
              "mar": tok[rows, names.index("married")],
              "dep": tok[rows, names.index("n_dep")]}
    for f in ("mar", "dep"):
        if (groups[f] < 0).any():
            raise ValueError(f"{f}: {(groups[f] < 0).sum()} PUF records without a token")
    L = A.layout()
    ng = {"inc": 5, "age": 6, "mar": 2, "dep": 4}
    off, b0 = {}, 0
    blocks = []
    for f in FAMS:
        off[f] = b0
        for g in range(ng[f]):
            blocks.append((b0 + g, f, g))
        b0 += ng[f]
    for f in FAMS:
        g = groups[f]
        out["g_" + f] = g
        out["blk_" + f] = np.where(g >= 0, off[f] + g, -1)
    print(f"{len(out)} PUF records; unknown age {(groups['age'] < 0).mean():.3f}", flush=True)
    out.to_parquet(OUT / "records.parquet", index=False)
    # cell table
    VB = L.VB
    vb_var = np.repeat(np.arange(A.NV), L.nbins)
    vb_bin = np.concatenate([np.arange(k) for k in L.nbins])
    nb = len(blocks)
    tab = pd.DataFrame({"cell": np.arange(nb * VB),
                        "block": np.repeat([b[0] for b in blocks], VB),
                        "wave": 2015,
                        "family": np.repeat([b[1] for b in blocks], VB),
                        "group": np.repeat([b[2] for b in blocks], VB),
                        "variable": np.tile(np.array(A.WEALTH)[vb_var], nb),
                        "var_i": np.tile(vb_var, nb),
                        "bin": np.tile(vb_bin, nb)})
    tab["target"] = 1.0
    tab.to_parquet(OUT / "targets.parquet", index=False)

    import d_tune
    d_tune.setup_torch()
    S = d_tune.Setup(torch.device("cuda"), records_path=OUT / "records.parquet",
                     targets_path=OUT / "targets.parquet", arm="puf")
    print("unfilled hidden slots:", S.ctx.unfilled_hidden, flush=True)
    E = d_tune.evaluate(S, S.ref, 2, 99, 16384)          # warm-up / sanity only
    gen = torch.Generator(device="cuda")
    gen.manual_seed(5)
    ec = torch.zeros(S.n_cells, dtype=torch.float64, device="cuda")
    allidx = torch.arange(S.N, device="cuda").repeat_interleave(4)
    for s0 in range(0, len(allidx), 16384):
        ri = allidx[s0:s0 + 16384]
        ec += d_tune.sample(S, S.ref, ri, gen, ref=False, w=S.W[ri] / 4)["cells"]
    e = ec.cpu().numpy()
    rng = np.random.default_rng(3)
    tid = tab["block"].to_numpy() * A.NV + tab["var_i"].to_numpy()
    pert = e * np.exp(0.3 * rng.standard_normal(len(e)))
    s0_ = np.bincount(tid, weights=e)
    s1_ = np.bincount(tid, weights=pert)
    tab["target"] = pert * (s0_[tid] / np.where(s1_[tid] > 0, s1_[tid], 1))
    tab.to_parquet(OUT / "targets.parquet", index=False)
    print(f"targets written; reference misplaced share before perturbation (sanity) "
          f"{E['misplaced_expected']:.4f}", flush=True)
    # reference draws and method-1 weights
    d = A.draw_set(S.ref, S.ctx, K, seed=11)
    np.savez(OUT / "draws.npz", **d)
    S2 = d_tune.Setup(torch.device("cuda"), records_path=OUT / "records.parquet",
                      targets_path=OUT / "targets.parquet", arm="puf")
    from b_calibrate import calibrate_matrix
    idx = S2.cell_index_np(d["tokens"])
    w, lam, rep = calibrate_matrix(idx, out["weight"].to_numpy(float),
                                   tab["target"].to_numpy(float), K, cell_meta=tab)
    np.savez(OUT / "weights.npz", w=w, **{"lambda": lam})
    rep.pop("cell_status", None)
    with open(OUT / "calib.json", "w") as f:
        json.dump(rep, f, indent=1, default=float)
    print(f"method-1 weights: KL {rep['kl_nats_per_unit_weight']:.4f}, effective draws "
          f"{rep['effective_draws_weighted_mean']:.2f}, statuses {rep['status_counts']}", flush=True)


if __name__ == "__main__":
    main()
