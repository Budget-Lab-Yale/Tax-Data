"""Hypothesis 2 diagnostics (test 1): are the model's conditionals calibrated?

For a checkpoint and a generation order, on the validation records:
  * teacher-forced step for each variable j: show the true values of the
    variables before j in the order, hide j and everything after it, and
    average the model's predicted distribution of j over records (weighted
    by record weight). Compare with the observed weighted distribution of j
    (total variation distance, TVD = half the sum of absolute differences
    in shares). Yardstick: draw one value per record from the model's own
    predicted distribution and compute the same TVD; this is the size of
    the gap a perfectly calibrated model would show from sampling alone.
  * calibration of the top choice: weighted average predicted probability
    of the most likely token versus how often that token is the true one.
  * age range x AGI band tables built from the teacher-forced predictions.
  * free-running: weighted marginal of each variable in the generated token
    files versus validation (and train versus validation as the yardstick).

Writes aggregate CSVs only, to $S/diag2/. Prints aggregate tables only.
"""
import argparse
import json
from pathlib import Path

import numpy as np
import pandas as pd
import torch

from model import generation_order, load_checkpoint, load_meta

S = Path("/nfs/roberts/scratch/pi_nrs36/jar335/puf_generator")
OUT = S / "diag2"
OUT.mkdir(exist_ok=True)

AGI_EDGES = [-np.inf, 0, 10e3, 25e3, 50e3, 75e3, 100e3, 200e3, 500e3, 1e6, np.inf]
AGI_LABELS = ["<0", "0-10k", "10-25k", "25-50k", "50-75k", "75-100k", "100-200k",
              "200-500k", "500k-1m", "1m+"]


def wdist(tok, w, K):
    d = np.bincount(tok, weights=w, minlength=K)
    return d / d.sum()


def tvd(p, q):
    return 0.5 * np.abs(p - q).sum()


def coarse_map(v, train_tok_col):
    """Map an amount variable's tokens to about 12 groups: negative, zero
    (and point tokens at zero), and 10 groups of positive tokens of about
    equal training count. Categoricals and the weight map to themselves."""
    K = int(v["vocab_size"])
    if v["kind"] != "amount":
        return np.arange(K)
    toks = v["tokens"]
    lo = np.array([float(t["lo"]) for t in toks])
    hi = np.array([float(t["hi"]) for t in toks])
    cnt = np.bincount(train_tok_col, minlength=K).astype(float)
    g = np.zeros(K, dtype=int)
    neg = hi <= 0
    zero = (lo == 0) & (hi == 0)
    g[neg & ~zero] = 0
    g[zero] = 1
    pos = np.where(~neg & ~zero)[0]
    pos = pos[np.argsort(lo[pos])]
    c = np.cumsum(cnt[pos])
    if len(pos):
        q = np.minimum((10 * (c - 0.5 * cnt[pos]) / max(c[-1], 1)).astype(int), 9)
        g[pos] = 2 + q
    return g


@torch.no_grad()
def teacher_forced(model, tokens, order, device, bs=4096, keep=()):
    """For each variable in `order`, the model's predicted distribution given
    the true values of the variables before it. Returns dict j -> probs
    [N, K_j] (float32 numpy) for every variable."""
    V = model.n_vars
    rank = np.empty(V, dtype=int)
    rank[order] = np.arange(V)
    probs = {j: [] for j in range(V)}
    vocab = model.vocab.cpu().numpy()
    for start in range(0, tokens.shape[0], bs):
        tok = torch.as_tensor(tokens[start:start + bs], device=device).long()
        b = tok.shape[0]
        for k, j in enumerate(order):
            hid = torch.as_tensor(rank >= k, device=device)[None, :].expand(b, V)
            logits = model(tok, hid)
            p = torch.softmax(logits[:, j, :vocab[j]].float(), dim=-1)
            probs[j].append(p.cpu().numpy())
    return {j: np.concatenate(v) for j, v in probs.items()}


@torch.no_grad()
def leave_one_out(model, tokens, js, device, bs=4096):
    """Predicted distribution of each variable in js given all the others."""
    V = model.n_vars
    vocab = model.vocab.cpu().numpy()
    out = {j: [] for j in js}
    for start in range(0, tokens.shape[0], bs):
        tok = torch.as_tensor(tokens[start:start + bs], device=device).long()
        b = tok.shape[0]
        for j in js:
            hid = torch.zeros(b, V, dtype=torch.bool, device=device)
            hid[:, j] = True
            logits = model(tok, hid)
            out[j].append(torch.softmax(logits[:, j, :vocab[j]].float(), dim=-1).cpu().numpy())
    return {j: np.concatenate(v) for j, v in out.items()}


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--models", default="base,large")
    ap.add_argument("--orders", default="default,age_after_agi_wages")
    ap.add_argument("--nsub", type=int, default=0,
                    help="use a random subsample of this many validation records (0: all)")
    ap.add_argument("--tag", default="")
    a = ap.parse_args()
    global OUT
    if a.tag:
        OUT = OUT / a.tag
        OUT.mkdir(exist_ok=True)
    torch.set_num_threads(max(1, int(__import__("os").environ.get("SLURM_CPUS_PER_TASK", "4"))))
    device = torch.device("cuda" if torch.cuda.is_available() else "cpu")
    print("device", device, flush=True)
    meta = load_meta(S / "data" / "meta.json")
    vars_ = meta["variables"]
    names = [v["name"] for v in vars_]
    K = [int(v["vocab_size"]) for v in vars_]
    V = len(names)
    ix = {n: i for i, n in enumerate(names)}
    tv = np.load(S / "data" / "tokens_val.npy").astype(np.int64)
    tt = np.load(S / "data" / "tokens_train.npy").astype(np.int64)
    vv = pd.read_parquet(S / "data" / "values_val.parquet")
    vt = pd.read_parquet(S / "data" / "values_train.parquet", columns=["weight"])
    if a.nsub:
        keep = np.sort(np.random.default_rng(7).choice(len(vv), a.nsub, replace=False))
        print(f"subsample: {a.nsub} of {len(vv)} validation records (seed 7)")
        tv = tv[keep]
        vv = vv.iloc[keep].reset_index(drop=True)
    w = vv["weight"].to_numpy()
    wt = vt["weight"].to_numpy()
    assert len(w) == tv.shape[0] and len(wt) == tt.shape[0]
    print(f"validation records {len(w)}, training records {len(wt)}", flush=True)
    cmap = [coarse_map(vars_[j], tt[:, j]) for j in range(V)]
    rng = np.random.default_rng(20261008)

    obs = [wdist(tv[:, j], w, K[j]) for j in range(V)]
    obs_tr = [wdist(tt[:, j], wt, K[j]) for j in range(V)]

    def coarse(p, j):
        return np.bincount(cmap[j], weights=p, minlength=cmap[j].max() + 1)

    default = generation_order(meta)
    on = [names[j] for j in default]
    # Order variant for part C: age range, gender, earnings split moved to
    # just after AGI and wages.
    demo = ["AGERANGE", "GENDER", "EARNSPLIT"]
    rest = [n for n in on if n not in demo]
    k = rest.index("E00200") + 1
    age_after = rest[:k] + demo + rest[k:]
    orders = {"default": default, "age_after_agi_wages": [ix[n] for n in age_after]}
    orders = {k: orders[k] for k in a.orders.split(",")}

    agi_band = pd.cut(vv["E00100"], AGI_EDGES, labels=AGI_LABELS, right=False).cat.codes.to_numpy()
    nondep = vv["DSI"].to_numpy() == 0
    age_tok_vals = [t["value"] for t in vars_[ix["AGERANGE"]]["tokens"]]
    agi_coarse = None
    # map AGI tokens to AGI bands: token's (lo+hi)/2 band; bins straddling a
    # band edge are rare because bands are wide; use lo for assignment.
    agi_lo = np.array([float(t["lo"]) for t in vars_[ix["E00100"]]["tokens"]])
    agi_hi = np.array([float(t["hi"]) for t in vars_[ix["E00100"]]["tokens"]])
    agi_mid = np.where(agi_lo == agi_hi, agi_lo, 0.5 * (agi_lo + agi_hi))
    tok_band = pd.cut(pd.Series(agi_mid), AGI_EDGES, labels=AGI_LABELS, right=False).cat.codes.to_numpy()
    # check token-band assignment against true values on validation
    agree = np.average(tok_band[tv[:, ix["E00100"]]] == agi_band, weights=w)
    print(f"AGI band from token agrees with band from value for {100*agree:.2f}% of weight", flush=True)

    rows, calib_rows, age_rows = [], [], []
    rel_rows = []
    for m in a.models.split(","):
        model, ck = load_checkpoint(S / "model" / f"checkpoint_{m}.pt", meta, device)
        for oname, order in orders.items():
            P = teacher_forced(model, tv, order, device)
            for pos, j in enumerate(order):
                p = P[j]
                avg = (w[:, None] * p).sum(0) / w.sum()
                # one draw per record from the model's own conditional
                u = rng.random(len(w))[:, None]
                draw = (p.cumsum(1) < u).sum(1).clip(max=K[j] - 1)
                dd = wdist(draw, w, K[j])
                top = p.argmax(1)
                rows.append(dict(model=m, order=oname, position=pos + 1, variable=names[j],
                                 tvd_tf=tvd(avg, obs[j]), tvd_tf_noise=tvd(avg, dd),
                                 tvd_tf_coarse=tvd(coarse(avg, j), coarse(obs[j], j)),
                                 tvd_tf_coarse_noise=tvd(coarse(avg, j), coarse(dd, j)),
                                 mean_top_prob=np.average(p.max(1), weights=w),
                                 top_correct=np.average(top == tv[:, j], weights=w),
                                 mean_prob_true=np.average(p[np.arange(len(w)), tv[:, j]], weights=w)))
                if names[j] in ("AGERANGE", "MARS", "XTOT", "E00100", "E00200"):
                    pm = p.max(1)
                    edges = np.array([0, .2, .3, .4, .5, .6, .7, .8, .9, .95, 1.0001])
                    bi = np.digitize(pm, edges) - 1
                    for b in range(len(edges) - 1):
                        s = bi == b
                        if s.sum() == 0:
                            continue
                        rel_rows.append(dict(model=m, order=oname, variable=names[j],
                                             top_prob_lo=edges[b], top_prob_hi=min(edges[b + 1], 1),
                                             records=int(s.sum()), weight_share=w[s].sum() / w.sum(),
                                             mean_top_prob=np.average(pm[s], weights=w[s]),
                                             top_correct=np.average(top[s] == tv[s, j], weights=w[s])))
                    # weighted predicted vs observed shares, all categories
                    for t in (range(K[j]) if K[j] <= 8 else []):
                        calib_rows.append(dict(model=m, order=oname, variable=names[j], token=t,
                                               value=vars_[j]["tokens"][t].get("value"),
                                               observed_share=obs[j][t], predicted_share=avg[t]))
            # Age range x AGI band, non-dependent filers.
            ja, jg = ix["AGERANGE"], ix["E00100"]
            sel = nondep
            nA, nB = K[ja], len(AGI_LABELS)
            obs_tab = np.zeros((nA, nB))
            np.add.at(obs_tab, (tv[sel, ja], agi_band[sel]), w[sel])
            tot = obs_tab.sum()
            # (a) age given the variables before it, grouped by true AGI band
            pa = P[ja][sel]
            tab_a = np.zeros((nA, nB))
            for b in range(nB):
                s = agi_band[sel] == b
                tab_a[:, b] = (w[sel][s, None] * pa[s]).sum(0)
            # (b) AGI band given the variables before AGI (incl. true age when
            # age precedes AGI), grouped by true age
            pg = P[jg][sel]
            pgb = np.zeros((pg.shape[0], nB))
            for b in range(nB):
                pgb[:, b] = pg[:, tok_band == b].sum(1)
            tab_b = np.zeros((nA, nB))
            for t in range(nA):
                s = tv[sel, ja] == t
                tab_b[t] = (w[sel][s, None] * pgb[s]).sum(0)
            for lab, tab in (("age_given_earlier_grouped_by_true_agi", tab_a),
                             ("agi_given_earlier_grouped_by_true_age", tab_b)):
                age_rows.append(dict(model=m, order=oname, table=lab,
                                     tvd_vs_observed=tvd(tab / tab.sum(), obs_tab / tot)))
                pd.DataFrame(100 * (tab / tab.sum() - obs_tab / tot),
                             index=[f"age{v}" for v in age_tok_vals], columns=AGI_LABELS).round(3) \
                    .to_csv(OUT / f"agexagi_diff_{m}_{oname}_{lab}.csv")
            del P
        # (c) age given all other variables (sees AGI), grouped by true AGI
        L = leave_one_out(model, tv, [ix["AGERANGE"]], device)
        pa = L[ix["AGERANGE"]][nondep]
        tab_c = np.zeros((nA, nB))
        for b in range(nB):
            s = agi_band[nondep] == b
            tab_c[:, b] = (w[nondep][s, None] * pa[s]).sum(0)
        age_rows.append(dict(model=m, order="any", table="age_given_all_others",
                             tvd_vs_observed=tvd(tab_c / tab_c.sum(), obs_tab / tot)))
        p = L[ix["AGERANGE"]]
        top = p.argmax(1)
        calib_rows.append(dict(model=m, order="age_given_all_others", variable="AGERANGE",
                               token=-9, value=None,
                               observed_share=np.average(top == tv[:, ix["AGERANGE"]], weights=w),
                               predicted_share=np.average(p.max(1), weights=w)))
        # (d) noise yardstick for the age x AGI tables: draw age from the
        # model's own conditional given all others, tabulate against true AGI
        u = rng.random(nondep.sum())[:, None]
        draw = (pa.cumsum(1) < u).sum(1).clip(max=nA - 1)
        dtab = np.zeros((nA, nB))
        np.add.at(dtab, (draw, agi_band[nondep]), w[nondep])
        age_rows.append(dict(model=m, order="any", table="noise: one draw from age_given_all_others",
                             tvd_vs_observed=tvd(tab_c / tab_c.sum(), dtab / dtab.sum())))
        del model
        torch.cuda.empty_cache()

    res = pd.DataFrame(rows)
    res.to_csv(OUT / "teacher_forced_by_variable.csv", index=False)
    pd.DataFrame(rel_rows).to_csv(OUT / "reliability.csv", index=False)
    pd.DataFrame(calib_rows).to_csv(OUT / "category_shares.csv", index=False)
    pd.DataFrame(age_rows).to_csv(OUT / "age_agi_teacher_forced.csv", index=False)

    # Free-running: generated token files versus validation.
    gen = {
        "base_s09": "synthetic_base", "base_s10": "synthetic_base_s20261010",
        "base_s11": "synthetic_base_s20261011", "base_s12": "synthetic_base_s20261012",
        "large_s09": "synthetic", "large_s10": "synthetic_large_s20261010",
        "large_s11": "synthetic_large_s20261011", "large_s12": "synthetic_large_s20261012",
        "small_s09": "synthetic_small", "small_s10": "synthetic_small_s20261010",
        "base_demoglast": "synthetic_base_demoglast", "base_gibbs2": "synthetic_base_gibbs2",
    }
    fr = []
    for lab, stem in gen.items():
        f = S / "model" / f"{stem}_tokens.npy"
        if not f.exists():
            print("missing", f)
            continue
        g = np.load(f).astype(np.int64)
        gw = pd.read_parquet(S / "model" / f"{stem}.parquet", columns=["weight"])["weight"].to_numpy()
        for j in range(V):
            d = wdist(g[:, j], gw, K[j])
            fr.append(dict(file=lab, variable=names[j], tvd_vs_val=tvd(d, obs[j]),
                           tvd_vs_val_coarse=tvd(coarse(d, j), coarse(obs[j], j)),
                           tvd_vs_train=tvd(d, obs_tr[j]),
                           tvd_vs_train_coarse=tvd(coarse(d, j), coarse(obs_tr[j], j))))
    for j in range(V):
        fr.append(dict(file="train_records", variable=names[j], tvd_vs_val=tvd(obs_tr[j], obs[j]),
                       tvd_vs_val_coarse=tvd(coarse(obs_tr[j], j), coarse(obs[j], j)),
                       tvd_vs_train=0.0, tvd_vs_train_coarse=0.0))
    fr = pd.DataFrame(fr)
    fr.to_csv(OUT / "free_running_by_variable.csv", index=False)

    pd.set_option("display.width", 250, "display.max_rows", 500, "display.max_columns", 40)
    print("\n== Teacher-forced, by variable")
    print(res.round(4).to_string())
    print("\n== Age x AGI from teacher-forced predictions")
    print(pd.DataFrame(age_rows).round(4).to_string())
    print("\n== Reliability")
    print(pd.DataFrame(rel_rows).round(3).to_string())
    print("\n== Category shares")
    print(pd.DataFrame(calib_rows).round(4).to_string())
    print("\n== Free running, coarse TVD vs validation (rows variables)")
    print(fr.pivot(index="variable", columns="file", values="tvd_vs_val_coarse")
          .reindex(on).round(4).to_string())
    print("done", flush=True)


if __name__ == "__main__":
    main()
