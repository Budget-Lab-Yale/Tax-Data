"""Calibration of the model's top-wealth probabilities (piece B diagnostic).

On target records whose true wealth is known (experiment A: half 1;
`wealth_observed`), every wealth variable is hidden and the model is asked,
in one pass, for each wealth variable's distribution given the shared
variables and
  (a) the record's true SCF weight code, or
  (b) no weight (as in completion, where the weight is drawn).
For each wealth variable, the population-weighted average of the predicted
probability of (i) holding it (a code other than zero) and (ii) a code in
the top 1% of holders (positive rank band starting at 0.99 or above) is
compared with the weighted share in the truth. Under (a) a calibrated model
should match the weighted truth, because the sample's over-representation
of the wealthy is fully described by the weight. Under (b) the model
describes the SCF sample, so it should exceed the weighted truth by about
what the weight correction has to remove.

Usage: python b_calib_top.py --dataset expA_clean --checkpoint <ckpt> [--as-source SCF]
Writes $S/model/<dataset>/calib_top_<checkpoint stem>[_asSCF].csv.
"""

import argparse
import json
from pathlib import Path

import numpy as np
import pandas as pd
import torch

from b_model import load_checkpoint, load_meta, load_split, slot_columns, slot_variables

S = Path("/nfs/roberts/scratch/pi_nrs36/jar335/scf_joint")


@torch.no_grad()
def probs(model, tok, src, year, device, bs=8192):
    out = []
    for i in range(0, len(tok), bs):
        t = torch.as_tensor(tok[i:i + bs], device=device)
        with torch.autocast("cuda", dtype=torch.bfloat16, enabled=device.type == "cuda"):
            lg = model(t, t < 0, torch.as_tensor(src[i:i + bs], device=device),
                       torch.as_tensor(year[i:i + bs], device=device))
        out.append(torch.softmax(lg.float(), -1).cpu().numpy())
    return np.concatenate(out)


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--dataset", required=True)
    ap.add_argument("--checkpoint", required=True)
    ap.add_argument("--as-source", default=None)
    ap.add_argument("--split", default="test")
    ap.add_argument("--target-sources", default=None)
    a = ap.parse_args()
    data = S / "data" / a.dataset
    meta = load_meta(data / "meta.json")
    device = torch.device("cuda" if torch.cuda.is_available() else "cpu")
    model, ck = load_checkpoint(a.checkpoint, meta, device)
    sources = ck["sources"]
    sp = load_split(data, a.split, meta, sources)
    rec = sp["records"]
    keep = rec["wealth_observed"].astype(bool).to_numpy()
    if a.target_sources:
        keep &= rec["source"].astype(str).isin(a.target_sources.split(",")).to_numpy()
    truth = np.load(data / f"truth_{a.split}.npy")[:, slot_columns(meta)][keep].astype(np.int64)
    svars = slot_variables(meta)
    names = [v["name"] for v in svars]
    src = sp["src"][keep].copy()
    if a.as_source:
        src[:] = sources.index(a.as_source)
    year = sp["year"][keep].astype(np.float32)
    w = rec["weight"].to_numpy(float)[keep]
    scf_only = np.array([v["group"] == "scf_only" for v in svars])
    jw = [j for j, v in enumerate(svars) if v["kind"] == "weight"][0]
    if (truth[:, jw] < 0).any():
        raise ValueError("true weight code missing for some target records")
    tok_a = truth.copy()
    tok_a[:, scf_only] = -1
    tok_a[:, jw] = truth[:, jw]
    tok_b = truth.copy()
    tok_b[:, scf_only] = -1
    pa = probs(model, tok_a, src, year, device)
    pb = probs(model, tok_b, src, year, device)
    rows = []
    for j, v in enumerate(svars):
        if v["group"] != "scf_only" or v["kind"] != "amount":
            continue
        toks = v["tokens"]
        zero = np.array([t.get("kind") == "zero" for t in toks])
        top = np.array([t.get("kind") == "pos" and float(t["lo"]) >= 0.99 - 1e-9 for t in toks])
        n = len(toks)
        t = truth[:, j]
        rows.append(dict(
            variable=v["name"],
            true_hold=np.average(~zero[t], weights=w),
            pred_hold_given_weight=np.average(1 - pa[:, j, :n][:, zero].sum(1), weights=w),
            pred_hold_no_weight=np.average(1 - pb[:, j, :n][:, zero].sum(1), weights=w),
            true_top1=np.average(top[t], weights=w),
            pred_top1_given_weight=np.average(pa[:, j, :n][:, top].sum(1), weights=w),
            pred_top1_no_weight=np.average(pb[:, j, :n][:, top].sum(1), weights=w),
            true_top1_unweighted=float(np.mean(top[t])),
            pred_top1_no_weight_unweighted=float(np.mean(pb[:, j, :n][:, top].sum(1)))))
    df = pd.DataFrame(rows)
    tot = df.drop(columns="variable").mean().to_dict()
    tot["variable"] = "MEAN over components"
    df = pd.concat([df, pd.DataFrame([tot])], ignore_index=True)
    stem = Path(a.checkpoint).stem + ("_asSCF" if a.as_source == "SCF" else "")
    df.to_csv(S / "model" / a.dataset / f"calib_top_{stem}.csv", index=False)
    pd.set_option("display.width", 250)
    print(f"{a.checkpoint} as {a.as_source or 'own source'}; {int(keep.sum())} records")
    print(df.round(4).to_string(index=False))


if __name__ == "__main__":
    main()
