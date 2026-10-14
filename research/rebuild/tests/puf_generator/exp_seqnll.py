"""Sequential negative log-likelihood per validation record (nats) of a
checkpoint, under a chosen order and optionally on a subset of records
(night-of-2026-10-08 experiments, EXPERIMENTS.md).
Usage: python exp_seqnll.py <checkpoint> <label> [--order a,b,..] [--drop-token VAR=TOK]
Prints aggregate values only."""
import argparse
import numpy as np
import torch
from model import generation_order, load_checkpoint, load_meta, sequential_nll

S = "/nfs/roberts/scratch/pi_nrs36/jar335/puf_generator"
ap = argparse.ArgumentParser()
ap.add_argument("checkpoint"); ap.add_argument("label")
ap.add_argument("--order", default=None); ap.add_argument("--drop-token", default=None)
a = ap.parse_args()
meta = load_meta(f"{S}/data/meta.json")
names = [v["name"] for v in meta["variables"]]
dev = torch.device("cuda")
model, ck = load_checkpoint(a.checkpoint, meta, dev)
va = np.load(f"{S}/data/tokens_val.npy")
if a.drop_token:
    var, tok = a.drop_token.split("=")
    va = va[va[:, names.index(var)] != int(tok)]
order = [names.index(x) for x in a.order.split(",")] if a.order else generation_order(meta)
seq = sequential_nll(model, va, order, 4096, dev, amp=True)
print(f"RESULT {a.label}: records {len(va)}, sequential NLL per record {seq.mean():.4f} "
      f"(non-finite {int((~np.isfinite(seq)).sum())})")
