"""Probe O: a fixed generation order chosen greedily from the model's own
predictions on PUF training records.

Start with every PUF variable hidden. At each step, for every variable still
hidden, compute the population-weighted mean log-probability the model gives
the true value given the variables already revealed; reveal (put next in the
order) the variable with the highest mean log-probability, i.e. the one the
model predicts best at that point. Records: a random subset of the PUF
training records. The order is then scored on validation records and used to
generate (o_orders.py name `greedy`).

Usage: python o_greedy.py --checkpoint CK --out JSON [--n 40000]
"""
import argparse
import json
import sys
from pathlib import Path

import numpy as np
import torch
import torch.nn.functional as F

HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(HERE.parent / "scf_joint"))
from b_model import load_checkpoint, load_meta, load_split, slot_variables  # noqa: E402

DATA = Path("/nfs/roberts/scratch/pi_nrs36/jar335/scf_joint/model/full_v2/data")
sys.path.insert(0, str(HERE))
from o_orders import fixed_orders  # noqa: E402


@torch.no_grad()
def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--checkpoint", required=True)
    ap.add_argument("--out", required=True)
    ap.add_argument("--n", type=int, default=40000)
    ap.add_argument("--seed", type=int, default=7)
    a = ap.parse_args()
    meta = load_meta(DATA / "meta.json")
    svars = slot_variables(meta)
    names = [v["name"] for v in svars]
    dev = torch.device("cuda")
    model, ck = load_checkpoint(a.checkpoint, meta, dev)
    sp = load_split(DATA, "train", meta, ck["sources"])
    rec = sp["records"]
    puf = np.flatnonzero(rec["source"].astype(str).to_numpy() == "PUF")
    rng = np.random.default_rng(a.seed)
    rows = np.sort(rng.choice(puf, size=min(a.n, puf.size), replace=False))
    truth = sp["tok"][rows]
    w = rec["weight"].to_numpy(float)[rows]
    src = sp["src"][rows]
    year = sp["year"][rows].astype(np.float32)
    pufvars = fixed_orders()[0]["test3"]
    fill = np.isin(names, pufvars)
    revealed = np.zeros(len(names), bool)
    order, trace = [], []
    bs = 8192
    for step in range(len(pufvars)):
        tot = np.zeros(len(names))
        cnt = np.zeros(len(names))
        for s0 in range(0, len(rows), bs):
            tt = torch.as_tensor(truth[s0:s0 + bs], device=dev)
            hid = (tt < 0) | torch.as_tensor(fill & ~revealed, device=dev)[None, :]
            cur = torch.where(hid, torch.full_like(tt, -1), tt)
            with torch.autocast("cuda", dtype=torch.bfloat16):
                lg = model(cur, hid, torch.as_tensor(src[s0:s0 + bs], device=dev),
                           torch.as_tensor(year[s0:s0 + bs], device=dev))
            lp = F.log_softmax(lg.float(), dim=-1)
            obs = tt >= 0
            v = lp.gather(2, tt.clamp(min=0)[:, :, None]).squeeze(2)
            ww = torch.as_tensor(w[s0:s0 + bs], device=dev, dtype=torch.float32)[:, None]
            tot += (torch.where(obs, v, torch.zeros_like(v)) * ww).sum(0).double().cpu().numpy()
            cnt += (obs.float() * ww).sum(0).double().cpu().numpy()
        cand = np.flatnonzero(fill & ~revealed)
        mean = tot[cand] / np.maximum(cnt[cand], 1e-9)
        j = cand[int(np.argmax(mean))]
        order.append(names[j])
        trace.append(dict(step=step, chosen=names[j], mean_logprob=float(mean.max()),
                          worst=names[cand[int(np.argmin(mean))]],
                          worst_mean_logprob=float(mean.min())))
        revealed[j] = True
        print(trace[-1], flush=True)
    json.dump(dict(order=order, trace=trace, n=int(len(rows)),
                   checkpoint=a.checkpoint), open(a.out, "w"), indent=1)
    print(",".join(order))


if __name__ == "__main__":
    main()
