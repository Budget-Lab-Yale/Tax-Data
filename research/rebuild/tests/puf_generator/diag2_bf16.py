"""Does reduced-precision arithmetic in generation matter? (test 1, diag2)

generate.py runs the model in bfloat16 (a 16-bit number format with about
three significant digits) on the GPU; the teacher-forced checks in
diag2_calib.py ran in full 32-bit precision. This script
  1. compares the one-step predicted distributions in the two precisions
     on the validation records (base model, default order);
  2. generates a full-size file from the base model in 32-bit precision
     with the seed of the original base file (20261009), and reports the
     same aggregate measures as diag2_calib2 / diag2_ageagi for it.
Aggregates only; the generated tokens stay in $S/diag2/.
"""
import importlib.util
from pathlib import Path

import numpy as np
import pandas as pd
import torch

from diag2_calib import AGI_EDGES, AGI_LABELS, coarse_map, tvd, wdist
from model import generation_order, load_checkpoint, load_meta, sample_records

S = Path("/nfs/roberts/scratch/pi_nrs36/jar335/puf_generator")
OUT = S / "diag2"
CODE = Path(__file__).resolve().parent


@torch.no_grad()
def tf_probs(model, tokens, order, device, amp, bs=4096):
    V = model.n_vars
    rank = np.empty(V, dtype=int)
    rank[order] = np.arange(V)
    vocab = model.vocab.cpu().numpy()
    probs = {j: [] for j in range(V)}
    for start in range(0, tokens.shape[0], bs):
        tok = torch.as_tensor(tokens[start:start + bs], device=device).long()
        b = tok.shape[0]
        for k, j in enumerate(order):
            hid = torch.as_tensor(rank >= k, device=device)[None, :].expand(b, V)
            with torch.autocast("cuda", dtype=torch.bfloat16, enabled=amp):
                logits = model(tok, hid)
            probs[j].append(torch.softmax(logits[:, j, :vocab[j]].float(), -1).cpu().numpy())
    return {j: np.concatenate(v) for j, v in probs.items()}


def main():
    device = torch.device("cuda")
    meta = load_meta(S / "data" / "meta.json")
    vars_ = meta["variables"]
    names = [v["name"] for v in vars_]
    ix = {n: i for i, n in enumerate(names)}
    K = [int(v["vocab_size"]) for v in vars_]
    V = len(names)
    tv = np.load(S / "data" / "tokens_val.npy").astype(np.int64)
    tt = np.load(S / "data" / "tokens_train.npy").astype(np.int64)
    w = pd.read_parquet(S / "data" / "values_val.parquet", columns=["weight"])["weight"].to_numpy()
    wt = pd.read_parquet(S / "data" / "values_train.parquet", columns=["weight"])["weight"].to_numpy()
    cmap = [coarse_map(vars_[j], tt[:, j]) for j in range(V)]
    order = generation_order(meta)
    model, _ = load_checkpoint(S / "model" / "checkpoint_base.pt", meta, device)

    P32 = tf_probs(model, tv, order, device, amp=False)
    P16 = tf_probs(model, tv, order, device, amp=True)
    rows = []
    for j in range(V):
        a32 = (w[:, None] * P32[j]).sum(0) / w.sum()
        a16 = (w[:, None] * P16[j]).sum(0) / w.sum()
        rows.append(dict(variable=names[j],
                         mean_record_tvd_32_vs_16=np.average(0.5 * np.abs(P32[j] - P16[j]).sum(1), weights=w),
                         tvd_of_averages=tvd(a32, a16)))
    r = pd.DataFrame(rows)
    r.to_csv(OUT / "bf16_vs_fp32_teacher_forced.csv", index=False)
    pd.set_option("display.width", 200, "display.max_rows", 100)
    print("== one-step predictions, 32-bit vs bfloat16 (base, default order)")
    print(r.round(5).to_string())
    j0 = ix["S006"]
    for lab, P in (("fp32", P32), ("bf16", P16)):
        p0 = P[j0].mean(0)
        mw = np.array([wt[tt[:, j0] == k].mean() if (tt[:, j0] == k).any() else 0 for k in range(K[j0])])
        print(f"implied total weight ratio, {lab}: {(p0 * mw).sum() * len(tt) / wt.sum():.4f}")
    del P32, P16

    # Full-size generation in 32-bit precision.
    n = len(tt)
    g = sample_records(model, n, order, 8192, 20261009, device, amp=False)
    np.save(OUT / "synthetic_base_fp32_tokens.npy", g)
    spec = importlib.util.spec_from_file_location("tokenizer", CODE / "tokenizer.py")
    tok_mod = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(tok_mod)
    df = tok_mod.decode(g, meta, np.random.default_rng(20261009))
    df.to_parquet(OUT / "synthetic_base_fp32.parquet", index=False)
    gw = df["weight"].to_numpy()
    print(f"\nfp32 base file: total weight ratio to train {gw.sum() / wt.sum():.4f}")
    old = df[(df.DSI == 0) & (df.AGERANGE == 6)]
    print(f"fp32 base file: oldest age range (non-dependent) with wages "
          f"{100 * np.average(old.E00200 != 0, weights=old.weight):.1f}%")

    def coarse(p, j):
        return np.bincount(cmap[j], weights=p, minlength=cmap[j].max() + 1)
    obs = [wdist(tv[:, j], w, K[j]) for j in range(V)]
    files = {"fp32": (g, gw)}
    for st in ("synthetic_base", "synthetic_base_s20261010", "synthetic_base_s20261011",
               "synthetic_base_s20261012"):
        files[st] = (np.load(S / "model" / f"{st}_tokens.npy").astype(np.int64),
                     pd.read_parquet(S / "model" / f"{st}.parquet", columns=["weight"])["weight"].to_numpy())
    jg, ja = ix["E00100"], ix["AGERANGE"]
    agi_lo = np.array([float(t["lo"]) for t in vars_[jg]["tokens"]])
    agi_hi = np.array([float(t["hi"]) for t in vars_[jg]["tokens"]])
    tok_band = pd.cut(pd.Series(np.where(agi_lo == agi_hi, agi_lo, 0.5 * (agi_lo + agi_hi))),
                      AGI_EDGES, labels=AGI_LABELS, right=False).cat.codes.to_numpy()

    def agetab(tok, wts):
        t = np.zeros((K[ja], len(AGI_LABELS)))
        np.add.at(t, (tok[:, ja], tok_band[tok[:, jg]]), wts)
        return t / t.sum()
    age_obs = agetab(tv, w)
    sm = []
    for lab, (gg, ww) in files.items():
        d = [tvd(coarse(wdist(gg[:, j], ww, K[j]), j), coarse(obs[j], j)) for j in range(1, V)]
        sm.append(dict(file=lab, total_weight_ratio=ww.sum() / wt.sum(),
                       mean_coarse_tvd_vs_val=np.mean(d),
                       agexagi_tvd_vs_val=tvd(agetab(gg, ww), age_obs),
                       agerange_tvd=d[ja - 1] if ja > 0 else np.nan))
    sm = pd.DataFrame(sm)
    sm.to_csv(OUT / "bf16_vs_fp32_generation.csv", index=False)
    print(sm.round(4).to_string())
    print("done", flush=True)


if __name__ == "__main__":
    main()
