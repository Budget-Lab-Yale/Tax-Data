"""Probe T, step 1: where does the thin top enter?

For each checkpoint given, on the PUF training (and validation) records:
the model's predicted distribution of the AGI token (E00100), averaged over
records with their population weights, when the model sees only some of the
record's true values (everything else hidden). Compared with the weighted
distribution of the records' true AGI tokens. Also the expected AGI dollars
implied (each token valued at the weighted mean true AGI of the training
records holding it).

Conditions:
  none        nothing visible (PUF tag, year 2015)
  first6      MARS, DSI, XTOT, N24, EIC, FDED true (generation with
              demographics last sees exactly this when it draws AGI)
  first6+demo the same plus AGERANGE, GENDER, EARNSPLIT where the record
              has them (generation with age first sees this, with the
              demographics always present)
  allbutAGI   every other PUF variable true
  gen6        MARS..FDED taken from a generated file (demographics last),
              equal weights: reproduces what generation actually drew
Also: the six first variables' joint distribution in the generated file
against the truth.

Usage: python t_diag.py --out <json> --gen <generated parquet> CKPT ...
Aggregates only.
"""
import argparse
import json
import sys
from pathlib import Path

import numpy as np
import pandas as pd
import torch

HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(HERE.parent / "scf_joint"))
from b_model import load_checkpoint, load_meta, load_split, slot_variables  # noqa: E402

DATA = Path("/nfs/roberts/scratch/pi_nrs36/jar335/scf_joint/model/full_v2/data")
FIRST6 = ["MARS", "DSI", "XTOT", "N24", "EIC", "FDED"]
DEMO = ["AGERANGE", "GENDER", "EARNSPLIT"]


@torch.no_grad()
def avg_pred(model, tok_in, src, year, j, w, device, bs=8192):
    P = np.zeros(model.max_v if hasattr(model, "max_v") else 128)
    per = []
    for s in range(0, len(tok_in), bs):
        t = torch.as_tensor(tok_in[s:s + bs], device=device)
        h = t < 0
        with torch.autocast("cuda", dtype=torch.bfloat16):
            lg = model(t, h, torch.as_tensor(src[s:s + bs], device=device),
                       torch.as_tensor(year[s:s + bs], device=device))
        p = torch.softmax(lg[:, j, :].float(), dim=-1).cpu().numpy()
        per.append(p)
    p = np.concatenate(per)
    return p


def summarize(p, w, mean_d, nv):
    pw = (p[:, :nv] * w[:, None]).sum(0) / w.sum()
    exp_agi = float((p[:, :nv] @ mean_d * w).sum() / 1e12)
    pos = np.clip(mean_d, 0, None)
    top = float((p[:, 68:nv] @ pos[68:] * w).sum() / (p[:, :nv] @ pos * w).sum())
    top01 = float((p[:, 77:nv] @ pos[77:] * w).sum() / (p[:, :nv] @ pos * w).sum())
    return dict(rank_ge_090=float(pw[59:].sum()), rank_ge_099=float(pw[68:].sum()),
                rank_ge_0999=float(pw[77:].sum()), rank_ge_09999=float(pw[78:].sum()),
                expected_total_agi_tn=exp_agi, expected_share_rank_ge_099=top,
                expected_share_rank_ge_0999=top01,
                top_bands=[float(x) for x in pw[59:]]), pw


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--out", required=True)
    ap.add_argument("--gen", default=None)
    ap.add_argument("ckpts", nargs="+")
    a = ap.parse_args()
    meta = load_meta(DATA / "meta.json")
    sv = slot_variables(meta)
    names = [v["name"] for v in sv]
    j = names.index("E00100")
    nv = int(sv[j]["vocab_size"])
    device = torch.device("cuda")
    res = {}
    splits = {}
    for sp in ("train", "val"):
        d = load_split(DATA, sp, meta, ["PUF", "SCF"])
        m = d["src"] == 0
        w = d["records"]["weight"].to_numpy(float)[m]
        splits[sp] = dict(tok=d["tok"][m], src=d["src"][m], year=d["year"][m].astype(np.float32),
                          w=w, agi=d["records"]["E00100"].to_numpy(float)[m])
    tr = splits["train"]
    tA = tr["tok"][:, j]
    mean_d = np.array([np.average(tr["agi"][tA == k], weights=tr["w"][tA == k]) if (tA == k).any()
                       else 0.0 for k in range(nv)])
    for sp, d in splits.items():
        oh = np.zeros((len(d["w"]), nv))
        oh[np.arange(len(d["w"])), d["tok"][:, j]] = 1
        res[f"truth_{sp}"], _ = summarize(oh, d["w"], mean_d, nv)
    # Generated file: the six first variables against the truth.
    gen = None
    if a.gen:
        g = pd.read_parquet(a.gen)
        gen = np.stack([g[f"tok_{n}"].to_numpy() for n in FIRST6], 1)
        cmp6 = {}
        for k, n in enumerate(FIRST6):
            tt = tr["tok"][:, names.index(n)]
            cmp6[n] = {int(c): [float(tr["w"][tt == c].sum() / tr["w"].sum()),
                                float((gen[:, k] == c).mean())] for c in np.unique(tt)}
        # Joint cells MARS x FDED x EIC>0
        tm, tf = tr["tok"][:, names.index("MARS")], tr["tok"][:, names.index("FDED")]
        joint = {}
        for c1 in np.unique(tm):
            for c2 in np.unique(tf):
                joint[f"MARS{c1}_FDED{c2}"] = [
                    float(tr["w"][(tm == c1) & (tf == c2)].sum() / tr["w"].sum()),
                    float(((gen[:, 0] == c1) & (gen[:, 5] == c2)).mean())]
        res["generated_first6_vs_truth(token: [truth share, generated share])"] = dict(
            marginals=cmp6, MARS_by_FDED=joint)
    for ck in a.ckpts:
        model, _ = load_checkpoint(ck, meta, device)
        model.eval()
        out = {}
        for sp in ("train", "val"):
            d = splits[sp]
            V = d["tok"].shape[1]
            conds = {"none": [], "first6": FIRST6, "first6+demo": FIRST6 + DEMO}
            if sp == "train":
                conds["allbutAGI"] = [n for n in names if n != "E00100"]
            for cn, vis in conds.items():
                tin = np.full_like(d["tok"], -1)
                for n in vis:
                    tin[:, names.index(n)] = d["tok"][:, names.index(n)]
                p = avg_pred(model, tin, d["src"], d["year"], j, d["w"], device)
                out[f"{sp}:{cn}"], pw = summarize(p, d["w"], mean_d, nv)
                if sp == "train" and cn == "first6":
                    # by MARS x FDED cell: predicted vs true share at rank >= 0.99
                    tm = d["tok"][:, names.index("MARS")]
                    tf = d["tok"][:, names.index("FDED")]
                    cell = {}
                    for c1 in np.unique(tm):
                        for c2 in np.unique(tf):
                            mm = (tm == c1) & (tf == c2)
                            if d["w"][mm].sum() <= 0:
                                continue
                            cell[f"MARS{c1}_FDED{c2}"] = dict(
                                weight_share=float(d["w"][mm].sum() / d["w"].sum()),
                                true_rank_ge_099=float(d["w"][mm & (d["tok"][:, j] >= 68)].sum()
                                                       / d["w"][mm].sum()),
                                pred_rank_ge_099=float((p[mm, 68:nv].sum(1) * d["w"][mm]).sum()
                                                       / d["w"][mm].sum()))
                    out["train:first6_by_cell"] = cell
        if gen is not None:
            tin = np.full((len(gen), tr["tok"].shape[1]), -1, dtype=np.int64)
            for k, n in enumerate(FIRST6):
                tin[:, names.index(n)] = gen[:, k]
            wg = np.full(len(gen), tr["w"].sum() / len(gen))
            p = avg_pred(model, tin, np.zeros(len(gen), np.int64),
                         np.full(len(gen), 2015, np.float32), j, wg, device)
            out["gen6"], _ = summarize(p, wg, mean_d, nv)
        res[ck] = out
        print(ck, json.dumps({k: {kk: vv for kk, vv in v.items() if kk != "top_bands"}
                              for k, v in out.items() if "by_cell" not in k}, indent=1), flush=True)
    res["mean_dollars_per_token"] = [float(x) for x in mean_d]
    json.dump(res, open(a.out, "w"), indent=2)
    print(json.dumps({k: v for k, v in res.items() if k.startswith("truth") or k.startswith("gen")},
                     indent=1))


if __name__ == "__main__":
    main()
