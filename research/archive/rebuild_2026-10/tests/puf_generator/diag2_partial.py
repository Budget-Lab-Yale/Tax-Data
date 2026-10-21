"""Where along the generation order does the error build up? (test 1, diag2)

Start generation from validation records with the first k variables of the
default order set to their TRUE values, and let the model draw the rest
exactly as in generate.py (bfloat16, temperature 1). k = 0 is ordinary
generation; larger k fixes more of the record. Each validation record is
used R times (R independent continuations). Measures against validation:
  age range x AGI band TVD (all records, AGI band from tokens);
  oldest age range (non-dependent) share with wages and with AGI < $25k;
  mean coarse TVD of each variable jointly with AGI band (variables after
  wages);
  mean coarse TVD of the 39 variables' own distributions.
Aggregates only.
"""
from pathlib import Path

import numpy as np
import pandas as pd
import torch

from diag2_calib import AGI_EDGES, AGI_LABELS, coarse_map, tvd, wdist
from model import generation_order, load_checkpoint, load_meta

S = Path("/nfs/roberts/scratch/pi_nrs36/jar335/puf_generator")
OUT = S / "diag2"
R = 4


@torch.no_grad()
def continue_records(model, tokens, order, k, seed, device, bs=8192):
    V = model.n_vars
    gen = torch.Generator(device=device)
    gen.manual_seed(seed)
    out = np.empty_like(tokens)
    for start in range(0, tokens.shape[0], bs):
        tok = torch.as_tensor(tokens[start:start + bs], device=device).long().clone()
        b = tok.shape[0]
        hid = torch.ones(b, V, dtype=torch.bool, device=device)
        hid[:, order[:k]] = False
        for j in order[k:]:
            with torch.autocast("cuda", dtype=torch.bfloat16):
                logits = model(tok, hid)
            probs = torch.softmax(logits[:, j, :].float(), dim=-1)
            tok[:, j] = torch.multinomial(probs, 1, generator=gen).squeeze(1)
            hid[:, j] = False
        out[start:start + b] = tok.cpu().numpy()
    return out


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
    cmap = [coarse_map(vars_[j], tt[:, j]) for j in range(V)]
    nG = [cmap[j].max() + 1 for j in range(V)]
    order = generation_order(meta)
    jg, ja, jw, jd = ix["E00100"], ix["AGERANGE"], ix["E00200"], ix["DSI"]
    agi_lo = np.array([float(t["lo"]) for t in vars_[jg]["tokens"]])
    agi_hi = np.array([float(t["hi"]) for t in vars_[jg]["tokens"]])
    tok_band = pd.cut(pd.Series(np.where(agi_lo == agi_hi, agi_lo, 0.5 * (agi_lo + agi_hi))),
                      AGI_EDGES, labels=AGI_LABELS, right=False).cat.codes.to_numpy()
    nB = len(AGI_LABELS)
    low_agi = np.array([t["hi"] <= 25000 for t in vars_[jg]["tokens"]])
    zero_w = [t["token"] for t in vars_[jw]["tokens"] if t["lo"] == 0 and t["hi"] == 0][0]
    dsi0 = [t["token"] for t in vars_[jd]["tokens"] if t["value"] == 0][0]
    age6 = [t["token"] for t in vars_[ja]["tokens"] if t["value"] == 6][0]
    after = order[order.index(jw) + 1:]

    def agetab(tok, wts):
        t = np.zeros((K[ja], nB))
        np.add.at(t, (tok[:, ja], tok_band[tok[:, jg]]), wts)
        return t / t.sum()

    def joint(tok, wts, j):
        t = np.zeros((nG[j], nB))
        np.add.at(t, (cmap[j][tok[:, j]], tok_band[tok[:, jg]]), wts)
        return t / t.sum()

    def coarse_d(tok, wts, j):
        return np.bincount(cmap[j][tok[:, j]], weights=wts, minlength=nG[j]) / wts.sum()

    def measures(tok, wts):
        old = (tok[:, jd] == dsi0) & (tok[:, ja] == age6)
        return dict(agexagi_tvd=tvd(agetab(tok, wts), agetab(tv, w)),
                    oldest_with_wages=np.average(tok[old, jw] != zero_w, weights=wts[old]),
                    oldest_agi_under_25k=np.average(low_agi[tok[old, jg]], weights=wts[old]),
                    joint_with_agi_mean_tvd=np.mean([tvd(joint(tok, wts, j), joint(tv, w, j))
                                                     for j in after]),
                    own_dist_mean_tvd=np.mean([tvd(coarse_d(tok, wts, j), coarse_d(tv, w, j))
                                               for j in range(1, V)]))

    rows = [dict(model="observed", k=V, first_true="all", **measures(tv, w))]
    rep = np.tile(tv, (R, 1))
    wrep = np.tile(w, R)
    for m in ("base", "large"):
        model, _ = load_checkpoint(S / "model" / f"checkpoint_{m}.pt", meta, device)
        for k in (0, 1, 7, 8, 10, 11, 12):
            g = continue_records(model, rep, order, k, 20261100 + k, device)
            # the weight is generated when k = 0, so use the generated
            # weight's value; otherwise the true weight is kept
            if k == 0:
                wv = np.array([float(t.get("value", t.get("lo"))) for t in vars_[ix["S006"]]["tokens"]])
                wk = wv[g[:, ix["S006"]]]
                wk = wk * (wrep.sum() / wv[rep[:, ix["S006"]]].sum())  # same scale as weight column
            else:
                wk = wrep
            half = len(g) // 2
            mc = measures(g[:half], wk[:half])["agexagi_tvd"]
            r = dict(model=m, k=k, first_true="+".join(names[j] for j in order[:k]) or "(none)",
                     **measures(g, wk), agexagi_tvd_half=mc)
            rows.append(r)
            print(r, flush=True)
        del model
        torch.cuda.empty_cache()
    res = pd.DataFrame(rows)
    res.to_csv(OUT / "partial_generation.csv", index=False)
    pd.set_option("display.width", 250)
    print(res.drop(columns="first_true").round(4).to_string())
    print("done", flush=True)


if __name__ == "__main__":
    main()
