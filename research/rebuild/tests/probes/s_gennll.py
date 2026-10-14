"""Probe S: how well each checkpoint scores real PUF records under the
generation procedure itself, in several orders.

For each PUF validation record (and a 20,000-record sample of the training
records), every variable is hidden; the variables are then revealed one at
a time in a given order, and the score is the sum over the order of
-log p(true token of variable k | true tokens of variables 1..k-1), in nats
per record, averaged with population weights. This is the log-likelihood
that generation in that order assigns to the real record. A model whose
answers to different questions agree with one another gives the same score
in every order; the spread across orders measures how much the model's
generation depends on the order.

Orders: agifirst (AGI, then the demolast order), demolast (test 1's order,
demographics after all PUF amounts), and two random orders (fixed
permutations, seeds 1 and 2, the same for every record and every
checkpoint).

Also the same scores for records whose true AGI is in the top 1% band
(token 68 and above, as in t_diag.py).

Usage: python s_gennll.py --out <json> CKPT ...
Aggregates only.
"""
import argparse
import json
import sys
from pathlib import Path

import numpy as np
import torch

HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(HERE.parent / "scf_joint"))
from b_model import (completion_nll, load_checkpoint, load_meta,  # noqa: E402
                     load_split, slot_variables)

DATA = Path("/nfs/roberts/scratch/pi_nrs36/jar335/scf_joint/model/full_v2/data")
DEMO = ["AGERANGE", "GENDER", "EARNSPLIT"]


def orders(meta, names, puf_vars):
    v = {x["name"]: x for x in meta["variables"]}
    o = [n for n in meta["generation_order"] if v[n].get("group") != "scf_only"]
    o = [n for n in o if n not in DEMO] + DEMO
    missing = [n for n in puf_vars if n not in o]
    if missing:
        print(f"PUF variables not in the generation order, appended: {missing}", flush=True)
        o = o + missing
    o = [n for n in o if n in puf_vars]
    out = {"agifirst": ["E00100"] + [n for n in o if n != "E00100"], "demolast": o}
    for s in (1, 2):
        rng = np.random.default_rng(s)
        out[f"random{s}"] = [o[i] for i in rng.permutation(len(o))]
    return {k: [names.index(n) for n in x] for k, x in out.items()}, out


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--out", required=True)
    ap.add_argument("ckpts", nargs="+")
    a = ap.parse_args()
    meta = load_meta(DATA / "meta.json")
    sv = slot_variables(meta)
    names = [x["name"] for x in sv]
    device = torch.device("cuda")
    sets = {}
    for sp in ("val", "train"):
        d = load_split(DATA, sp, meta, ["PUF", "SCF"])
        m = d["src"] == 0
        tok = d["tok"][m]
        w = d["records"]["weight"].to_numpy(float)[m]
        idx = np.arange(len(w))
        if sp == "train":
            idx = np.random.default_rng(7).choice(len(w), 20000, replace=False)
        sets[sp] = dict(tok=tok[idx], src=d["src"][m][idx],
                        year=d["year"][m][idx].astype(np.float32), w=w[idx])
    puf_vars = [names[j] for j in np.flatnonzero((sets["val"]["tok"] >= 0).any(0))]
    oidx, onames = orders(meta, names, puf_vars)
    res = {"orders": onames}
    for ck in a.ckpts:
        model, _ = load_checkpoint(ck, meta, device)
        out = {}
        for sp, d in sets.items():
            tin = np.full_like(d["tok"], -1)
            for k, order in oidx.items():
                with torch.no_grad():
                    nll, sc = completion_nll(model, tin, d["tok"], d["src"], d["year"],
                                             order, 8192, device)
                per = nll.sum(1)
                top = d["tok"][:, names.index("E00100")] >= 68  # top 1% band (t_diag.py)
                jA = names.index("E00100")
                out[f"{sp}:{k}"] = dict(
                    nats_per_record=float(np.average(per, weights=d["w"])),
                    nats_agi=float(np.average(nll[:, jA], weights=d["w"])),
                    n_records=int(len(per)),
                    top1_nats_per_record=float(np.average(per[top], weights=d["w"][top])),
                    top1_nats_agi=float(np.average(nll[top, jA], weights=d["w"][top])),
                    top1_n_records=int(top.sum()))
        res[ck] = out
        print(ck, json.dumps(out, indent=1), flush=True)
    json.dump(res, open(a.out, "w"), indent=2)


if __name__ == "__main__":
    main()
