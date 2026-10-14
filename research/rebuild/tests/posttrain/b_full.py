"""Test 3, piece B, full model: method 1 (reweight draws) on arm P.

Arm P: PUF training records (2015) at their own weights, the 24 wealth
variables drawn K times each from the reference model; targets are piece A's
SCF wealth tables carried to the PUF (a_full.py: target set `wcore` = income
group, age, married, dependents; `wfull` = wcore plus each wealth variable by
the band of each income item both sources measure). Each record keeps its
weight across its draws; only the wealth targets bind.

    python b_full.py --version full_v2 --set wcore [--k 8 16 32]
    python b_full.py --version pilot --set wcore     # A's pilot on expB

Outputs in $P/<version>/b/P_<core|full>/ (pilot: $P/full/b/pilot_P_<..>):
weights_P.npz (w [N, K], lambda, cell_id, cell_status), report_P.json,
report_P_K<k>.json for subsets, marker DONE_P.
"""
import argparse
import json
import os
import sys
import time
from pathlib import Path

import numpy as np
import pandas as pd

HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(HERE))
import a_common as A          # noqa: E402
import a_full as AF           # noqa: E402
from b_calibrate import calibrate_matrix, print_summary, log, P   # noqa: E402

SETNAME = {"wcore": "core", "wfull": "full"}


def build_index(ts, recs, G, tokens, fill, chunk=20000):
    vp = AF.truth_view(recs)
    N, K = tokens.shape[:2]
    out = np.empty((N * K, ts.width), dtype=np.int32)
    for s0 in range(0, N, chunk):
        sl = slice(s0, min(N, s0 + chunk))
        vs = AF.View(sl.stop - sl.start)
        vs.tok = {k: a[sl] for k, a in vp.tok.items()}
        vs.val = {k: a[sl] for k, a in vp.val.items()}
        out[sl.start * K: sl.stop * K] = ts.contrib_index(G[sl], vs, tokens[sl], fill)
    return out


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--version", default="full_v2", help="full, full_v2 or pilot")
    ap.add_argument("--set", default="wcore", choices=["wcore", "wfull"])
    ap.add_argument("--draws", default=None, help="default <A dir>/ref_draws_P.npz")
    ap.add_argument("--k", type=int, nargs="+", default=[None])
    ap.add_argument("--outdir", default=None)
    ap.add_argument("--marker", default="DONE_P")
    ap.add_argument("--fake-draws", type=int, default=0,
                    help="code test: K random valid tokens per record instead of a draws file")
    args = ap.parse_args()
    pilot = args.version == "pilot"
    if pilot:
        AF.configure_pilot()
    else:
        A.configure_full(args.version)
    outdir = args.outdir or (os.path.join(P, "full", "b", f"pilot_P_{SETNAME[args.set]}") if pilot
                             else os.path.join(P, args.version, "b", f"P_{SETNAME[args.set]}"))
    os.makedirs(outdir, exist_ok=True)
    recs = AF.load_puf_records()
    ts = AF.rebuild_set(args.set, pilot)
    cells = pd.read_parquet(A.PA / f"targets_{args.set}.parquet")
    if len(cells) != ts.n_cells or not np.array_equal(cells["cell"].to_numpy(), np.arange(ts.n_cells)):
        raise SystemExit("targets file does not match the rebuilt target set")
    G = np.stack([recs[f"g_{args.set}_{fam.name}"].to_numpy() for fam, _ in ts.tables], axis=1)
    fill = AF.Spec(pilot).wealth
    if args.fake_draws:
        rng = np.random.default_rng(0)
        draws = f"fake (K={args.fake_draws}, uniform over valid tokens)"
        tokens_all = np.empty((len(recs), args.fake_draws, len(fill)), dtype=np.int16)
        for j, x in enumerate(fill):
            ok = np.where(np.asarray(ts.bins[x][1]) >= 0)[0]
            tokens_all[:, :, j] = rng.choice(ok, size=(len(recs), args.fake_draws))
        if args.marker == "DONE_P":
            args.marker = None
    else:
        draws = args.draws or str(A.PA / "ref_draws_P.npz")
        tokens_all = np.load(draws)["tokens"]
    log(f"A dir {A.PA}; draws {draws} {tokens_all.shape}; records {len(recs)}; "
        f"set {args.set}: {ts.n_cells} cells, {ts.width} per draw; fill {len(fill)} variables")
    if tokens_all.shape[0] != len(recs) or tokens_all.shape[2] != len(fill):
        raise SystemExit("draws do not match records x fill variables")
    W = recs["weight"].to_numpy(np.float64)
    t = cells["target"].to_numpy(np.float64)
    for k in args.k:
        K = tokens_all.shape[1] if k is None else k
        if K > tokens_all.shape[1]:
            log(f"skip K={K}")
            continue
        name = "P" if k is None else f"P_K{K}"
        idx = build_index(ts, recs, G, tokens_all[:, :K], fill)
        w, lam, rep = calibrate_matrix(idx, W, t, K, cell_meta=cells)
        del idx
        rep.update({"draws_file": draws, "target_set": args.set, "version": args.version,
                    "tag": name, "a_dir": str(A.PA)})
        print_summary(name, rep)
        if k is None:
            np.savez(os.path.join(outdir, f"weights_{name}.npz"), w=w, **{"lambda": lam},
                     cell_id=cells["cell"].to_numpy(), cell_status=np.array(rep["cell_status"]))
        with open(os.path.join(outdir, f"report_{name}.json"), "w") as fh:
            json.dump(rep, fh, indent=1, default=float)
        log(f"wrote {outdir}/report_{name}.json")
    if args.marker:
        with open(os.path.join(outdir, args.marker), "w") as fh:
            fh.write(time.strftime("%Y-%m-%d %H:%M:%S") + f" job {os.environ.get('SLURM_JOB_ID')}\n")


if __name__ == "__main__":
    main()
