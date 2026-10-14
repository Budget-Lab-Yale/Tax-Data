"""Test 3, piece F, on the full model (SPEC.md, "Contract changes": main
session 20:45 and F 20:40): arms PS0 and PS1 for PUF records.

The dataset directory, checkpoint and tokenizer module are parameters, so
the same code runs on the pilot model (expB) and on the full model.

PS0: each PUF training record keeps its categorical variables (its
demographics: in the full model MARS, DSI, XTOT, N24, EIC, FDED, AGERANGE,
GENDER, EARNSPLIT, whichever the dataset carries; in the pilot age_range,
married, n_dep). A kept variable that is unobserved on the record stays
unobserved: it is shown to the network as masked and never drawn. Every PUF
amount is drawn with the PUF source tag, then the 24 wealth variables with
the SCF tag, year 2015. K draws per record (default 16, drawn in two nested
parts of 8), each record keeping its weight.

PS1: whole records from nothing but year 2015: every PUF variable with the
PUF tag, then wealth with the SCF tag. N synthetic records = a multiple of
the PUF training count (parts of 4x), equal starting weights summing to the
PUF total weight.

Order: fill_order(meta) without dropped variables, PUF variables first and
wealth after (each in fill_order's sequence). Allowed codes: b_complete's
rule for (PUF, 2015) on PUF variables and (SCF, 2015) on wealth.
Log-probabilities in float32 (no autocast). Dollars: PUF variables decoded
as PUF 2015, wealth as SCF at 2015 (the tokenizer interpolates the 2013 and
2016 quantile functions).

    python f_full.py draw ps0 --part 1 [--data DIR --ckpt FILE --out DIR]
    python f_full.py draw ps1 --part 1 --mult 4
    python f_full.py calib ps0 --k 8 16 --set core full
    python f_full.py calib ps1 --mult 4 8 --set core full
    python f_full.py pilot      # code test on the pilot model (small)
"""
import argparse
import importlib
import json
import os
import sys
import time
from pathlib import Path

import numpy as np
import pandas as pd

HERE = Path(__file__).resolve().parent
T2CODE = HERE.parent / "scf_joint"
for p in (str(HERE), str(T2CODE)):
    if p not in sys.path:
        sys.path.insert(0, p)
from c_common import WEALTH                      # noqa: E402

T2 = Path("/nfs/roberts/scratch/pi_nrs36/jar335/scf_joint")
P = Path("/nfs/roberts/scratch/pi_nrs36/jar335/posttrain")
DEFAULTS = {"data": str(T2 / "data" / "full"),
            "ckpt": str(T2 / "model" / "full" / "checkpoint_full_w1_seed01.pt"),
            "out": str(P / "full" / "f"), "tokenizer": str(HERE / "g_tokenizer.py")}
PILOT = {"data": str(T2 / "data" / "expB"),
         "ckpt": str(T2 / "model" / "expB" / "checkpoint_joint_w1_seed01.pt"),
         "out": str(P / "dev" / "f_full"), "tokenizer": "a_tokenizer"}
YEAR = 2015
SEED0 = {"ps0": 20261401, "ps1": 20261411, "dollars": 20261421}
K_PART = 8          # PS0 draws per record per part
M_PART = 4          # PS1 multiple per part


def log(msg, t0=[time.time()]):
    print(f"[{time.time() - t0[0]:7.1f}s] {msg}", flush=True)


def jdump(obj, path):
    with open(path, "w") as fh:
        json.dump(obj, fh, indent=1, default=lambda x: x.item() if hasattr(x, "item") else str(x))


def tokenizer(name):
    if os.path.sep in name or name.endswith(".py"):
        spec = importlib.util.spec_from_file_location("f_tok", name)
        mod = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(mod)
        return mod
    return importlib.import_module(name)


# --------------------------------------------------------------------------
# Model and variables
# --------------------------------------------------------------------------

class G:
    pass


def load(data, ckpt, tok_name, device="cuda"):
    import torch
    from b_model import fill_order, load_checkpoint, slot_columns, slot_variables
    from b_complete import allowed_code_tables
    torch.backends.cuda.matmul.allow_tf32 = False
    torch.backends.cudnn.allow_tf32 = False
    torch.set_float32_matmul_precision("highest")
    g = G()
    g.data = Path(data)
    g.meta = json.load(open(g.data / "meta.json"))
    g.tok = tokenizer(tok_name)
    g.device = torch.device(device)
    model, ck = load_checkpoint(ckpt, g.meta, g.device)
    for p in model.parameters():
        p.requires_grad_(False)
    g.svars = slot_variables(g.meta)
    g.snames = [v["name"] for v in g.svars]
    g.scols = slot_columns(g.meta)
    g.allnames = [v["name"] for v in g.meta["variables"]]
    g.dropped = [x for x in (ck.get("args") or {}).get("drop_vars", "").split(",") if x]
    g.sources = list(ck["sources"])
    missing = [v for v in WEALTH if v not in g.snames]
    if missing:
        raise ValueError(f"wealth variables missing from the model: {missing}")
    g.is_w = np.array([n in WEALTH for n in g.snames])
    g.is_drop = np.array([n in g.dropped for n in g.snames])
    g.is_cat = np.array([v.get("kind") == "categorical" for v in g.svars])
    g.is_puf = ~g.is_w & ~g.is_drop
    fo = [j for j in fill_order(g.meta, g.svars) if not g.is_drop[j]]
    g.fo = [j for j in fo if g.is_puf[j]] + [j for j in fo if g.is_w[j]]
    keys = pd.DataFrame({"source": ["PUF", "SCF"], "year": [YEAR, YEAR]})
    tab, gidx, notes = allowed_code_tables(g.tok, g.meta, g.data, keys, None, g.svars, model.max_v)
    al = np.where(g.is_w[:, None], tab[gidx[1]], tab[gidx[0]])
    g.allowed = torch.as_tensor(al, device=g.device)
    g.allowed_notes = notes
    g.ckpt = str(ckpt)
    return model, g


def puf_records(g):
    rec = pd.read_parquet(g.data / "records_train.parquet")
    src = rec["source"].astype(str).to_numpy()
    rows = np.flatnonzero(src == "PUF")
    yrs = set(rec["year"].to_numpy()[rows].astype(int).tolist())
    if yrs != {YEAR}:
        raise ValueError(f"PUF training years {yrs}")
    W = rec["weight"].to_numpy(float)[rows]
    if np.isnan(W).any() or (W <= 0).any():
        raise ValueError(f"PUF weights: {np.isnan(W).sum()} missing, {(W <= 0).sum()} not positive")
    return rows, W


def setup(g, arm, rows=None, n=None):
    """Inputs and order for arm ps0 (rows: PUF training rows) or ps1 (n
    synthetic records)."""
    import torch
    V = len(g.snames)
    if arm == "ps0":
        tok_all = np.load(g.data / "tokens_train.npy")
        tok_in = tok_all[rows][:, g.scols].astype(np.int64)
        keep = g.is_puf & g.is_cat
        tok_in[:, ~keep] = -1
        g.unobserved_kept = {g.snames[j]: int((tok_in[:, j] < 0).sum()) for j in np.flatnonzero(keep)
                             if (tok_in[:, j] < 0).any()}
        drawn = (g.is_puf & ~g.is_cat) | g.is_w
    else:
        tok_in = np.full((n, V), -1, dtype=np.int64)
        g.unobserved_kept = {}
        drawn = g.is_puf | g.is_w
    g.drawn = drawn
    g.order = [j for j in g.fo if drawn[j]]
    puf, scf = g.sources.index("PUF"), g.sources.index("SCF")
    g.src_of = {j: (scf if g.is_w[j] else puf) for j in g.order}
    g.tok_in = torch.as_tensor(tok_in, device=g.device)
    g.n = len(tok_in)
    g.year = torch.full((g.n,), float(YEAR), device=g.device)
    return g


def sample(model, g, unit, gen, batch=16384, start=None, order=None):
    """Draws for each entry of unit (index into g.tok_in). Returns tokens
    int16 [n, V] (kept and unobserved slots as given) and float32
    log-probabilities [n, V] (0 where not drawn). start [n, V]: partly drawn
    rows to continue from (slots >= 0 are visible); order: the steps to run
    (default g.order)."""
    import torch
    unit = np.asarray(unit, dtype=np.int64)
    n, V = len(unit), len(g.snames)
    order = g.order if order is None else order
    tok_out = np.zeros((n, V), dtype=np.int16)
    lp_out = np.zeros((n, V), dtype=np.float32)
    with torch.no_grad():
        for s0 in range(0, n, batch):
            ui = torch.as_tensor(unit[s0:s0 + batch], device=g.device)
            if start is None:
                cur = g.tok_in[ui].clone()
            else:
                cur = torch.as_tensor(np.asarray(start[s0:s0 + batch], dtype=np.int64), device=g.device)
            hid = cur < 0
            res = torch.zeros(len(ui), V, device=g.device)
            for j in order:
                src = torch.full((len(ui),), g.src_of[j], dtype=torch.int64, device=g.device)
                lj = model(cur, hid, src, g.year[ui])[:, j, :].float()
                lj = lj.masked_fill(~g.allowed[j, :lj.shape[1]][None, :], float("-inf"))
                lp = torch.log_softmax(lj, dim=-1)
                d = torch.multinomial(lp.exp(), 1, generator=gen).squeeze(1)
                res[:, j] = lp.gather(1, d[:, None]).squeeze(1)
                cur[:, j] = d
                hid[:, j] = False
            tok_out[s0:s0 + batch] = cur.cpu().numpy().astype(np.int16)
            lp_out[s0:s0 + batch] = res.cpu().numpy()
    return tok_out, lp_out


def rescore(model, g, unit, tokens, batch=16384):
    """Float32 log-probability of the drawn slots of given token rows, in the
    same order and with the same source tags."""
    import torch
    unit = np.asarray(unit, dtype=np.int64)
    tokens = np.asarray(tokens, dtype=np.int64)
    n, V = tokens.shape
    out = np.zeros((n, V), dtype=np.float32)
    with torch.no_grad():
        for s0 in range(0, n, batch):
            ui = torch.as_tensor(unit[s0:s0 + batch], device=g.device)
            cur = torch.as_tensor(tokens[s0:s0 + batch], device=g.device)
            hid = g.tok_in[ui] < 0
            res = torch.zeros(len(ui), V, device=g.device)
            for j in g.order:
                src = torch.full((len(ui),), g.src_of[j], dtype=torch.int64, device=g.device)
                lj = model(cur, hid, src, g.year[ui])[:, j, :].float()
                lj = lj.masked_fill(~g.allowed[j, :lj.shape[1]][None, :], float("-inf"))
                res[:, j] = torch.log_softmax(lj, -1).gather(1, cur[:, j:j + 1]).squeeze(1)
                hid[:, j] = False
            out[s0:s0 + batch] = res.cpu().numpy()
    return out


def decode(g, tokens, seed, chunk=400_000):
    """Dollars [n, V] (slot order): PUF variables as PUF 2015, wealth as SCF
    at 2015; categoricals decoded to their values; NaN where unobserved."""
    q = pd.read_parquet(g.data / "quantiles.parquet")
    cache = g.tok._qf_cache(q)
    rng = np.random.default_rng(seed)
    t = np.asarray(tokens, dtype=np.int64)
    out = np.full(t.shape, np.nan)
    cols = np.array([g.allnames.index(n) for n in g.snames])
    for src, sel in (("PUF", g.is_puf), ("SCF", g.is_w)):
        js = np.flatnonzero(sel)
        for s0 in range(0, len(t), chunk):
            blk = t[s0:s0 + chunk]
            full = np.full((len(blk), len(g.allnames)), -1, dtype=np.int64)
            full[:, cols[js]] = blk[:, js]
            r = pd.DataFrame({"source": src, "year": np.full(len(blk), YEAR)})
            d = g.tok.decode(full, g.meta, r, q, rng, year=None, cache=cache)
            out[s0:s0 + chunk][:, js] = d[[g.snames[j] for j in js]].to_numpy(float)
    bad = np.isnan(out) & (t >= 0) & (g.is_puf | g.is_w)[None, :]
    if bad.any():
        raise ValueError(f"NaN dollars for observed tokens: {bad.sum()}")
    return out


# --------------------------------------------------------------------------
# Draws
# --------------------------------------------------------------------------

def draw(args):
    import torch
    out = Path(args.out) / args.arm
    out.mkdir(parents=True, exist_ok=True)
    model, g = load(args.data, args.ckpt, args.tokenizer)
    rows, W = puf_records(g)
    if args.n_records:
        rows, W = rows[:args.n_records], W[:args.n_records]
    t0 = time.time()
    if args.arm == "ps0":
        setup(g, "ps0", rows=rows)
        K = args.k or K_PART
        unit = np.repeat(np.arange(g.n), K)
        seed = SEED0["ps0"] * 10 + args.part
    else:
        mult = args.mult[0] if args.mult else M_PART
        n = int(round(mult * len(rows))) if not args.n_synthetic else args.n_synthetic
        setup(g, "ps1", n=n)
        unit = np.arange(n)
        seed = SEED0["ps1"] * 10 + args.part
    log(f"{args.arm} part {args.part}: order {[g.snames[j] for j in g.order]}; "
        f"kept unobserved: {g.unobserved_kept}")
    gen = torch.Generator(device=g.device)
    gen.manual_seed(seed)
    tok, lp = sample(model, g, unit, gen, batch=args.batch)
    secs = time.time() - t0
    log(f"{len(tok):,} draws in {secs:.0f}s")
    # checks: kept slots unchanged, drawn slots allowed, rescoring agrees
    kept = ~g.drawn
    tin = g.tok_in.cpu().numpy()[unit]
    if not np.array_equal(tok[:, kept], tin[:, kept]):
        raise ValueError("kept variables changed")
    chk = slice(0, min(len(tok), 20000))
    lp2 = rescore(model, g, unit[chk], tok[chk])
    checks = {"rescore_max_abs_diff": float(np.abs(lp2 - lp[chk]).max()),
              "draws": int(len(tok)), "seconds": secs, "seed": seed,
              "order": [g.snames[j] for j in g.order], "kept_unobserved": g.unobserved_kept,
              "data": str(args.data), "ckpt": str(args.ckpt), "allowed_notes": g.allowed_notes}
    log(f"rescoring check: largest difference {checks['rescore_max_abs_diff']:.2e} nats")
    dol = decode(g, tok, SEED0["dollars"] * 10 + args.part + (0 if args.arm == "ps0" else 50))
    if args.arm == "ps0":
        shp = (g.n, K)
        extra = {"row": rows, "weight": W}
    else:
        shp = (len(tok),)
        extra = {"puf_total_weight": np.float64(W.sum()), "n_puf": np.int64(len(rows))}
    fn = out / f"draws_p{args.part}.npz"
    np.savez(fn, tokens=tok.reshape(*shp, -1), logp_var=lp.reshape(*shp, -1),
             logp=lp.astype(np.float64).sum(-1).astype(np.float32).reshape(shp),
             dollars=dol.reshape(*shp, -1), slot_names=np.array(g.snames), drawn=g.drawn,
             seed=np.int64(seed), **extra)
    jdump(checks, out / f"draws_p{args.part}_checks.json")
    open(out / f"DONE_draws_p{args.part}", "w").write(time.strftime("%Y-%m-%d %H:%M:%S\n"))
    log(f"wrote {fn}")


def load_parts(out, arm, size):
    """PS0: the first `size` draws per record (K); PS1: the first size x
    n_puf synthetic records."""
    parts, have = [], 0
    p = 1
    while True:
        fn = Path(out) / arm / f"draws_p{p}.npz"
        if not fn.exists():
            break
        z = np.load(fn)
        parts.append({k: z[k] for k in z.files})
        p += 1
    if not parts:
        raise FileNotFoundError(f"no draws under {out}/{arm}")
    if arm == "ps0":
        tok = np.concatenate([z["tokens"] for z in parts], axis=1)[:, :size]
        dol = np.concatenate([z["dollars"] for z in parts], axis=1)[:, :size]
        if tok.shape[1] < size:
            raise ValueError(f"only {tok.shape[1]} draws per record")
        return dict(tokens=tok, dollars=dol, row=parts[0]["row"], weight=parts[0]["weight"],
                    slot_names=parts[0]["slot_names"])
    n = int(round(size * int(parts[0]["n_puf"])))
    tok = np.concatenate([z["tokens"] for z in parts])[:n]
    dol = np.concatenate([z["dollars"] for z in parts])[:n]
    if len(tok) < n:
        raise ValueError(f"only {len(tok)} synthetic records, {n} needed")
    return dict(tokens=tok, dollars=dol, total=float(parts[0]["puf_total_weight"]),
                slot_names=parts[0]["slot_names"])


# --------------------------------------------------------------------------
# Calibration against A's full-model targets
# --------------------------------------------------------------------------

def a_targets(data, set_name, tok, dol, names):
    """A's full-model targets (a_full.py) for one of F's sets, and the cell
    ids of each row computed from the row's own values. F's sets: 'core' =
    A's PUF ground truth ('puf') + SCF wealth core ('wcore'); 'full' = 'puf'
    + 'wfull'. Returns (idx int64 [R, width] with -1 where the row touches
    no cell, targets frame with a 'target' column (NaN = no target; zero =
    zero cell) and a 'set' column)."""
    import a_common as A
    import a_full as AF
    A.configure_full(Path(data).name)
    v = AF.view_from_tokens(np.asarray(tok, dtype=np.int64), list(names), dol)
    idxs, tabs, off = [], [], 0
    for nm in ("puf", "wcore" if set_name == "core" else "wfull"):
        ts = AF.rebuild_set(nm)
        if nm != "puf":
            # a_full.rebuild_set sets the measure to the notes' text when the
            # SCF lacks E00100; rebuild the list as a_full.build does
            info = json.load(open(A.PA / "cells_full.json"))
            msr = info["notes"]["income_group_measure_wealth_tables"]
            if isinstance(msr, str) and msr.startswith("sum of"):
                msr = [x for x in info["notes"]["common_income_items"] if x in AF.MEASURE_ITEMS]
            for fam, _ in ts.tables:
                if fam.kind == "rank":
                    fam.measure = msr
        cells = AF.load_set(nm).copy()
        if len(cells) != ts.n_cells or not (cells["cell"].to_numpy() == np.arange(ts.n_cells)).all():
            raise ValueError(f"{nm}: targets file does not match the rebuilt layout")
        G = ts.groups(v)
        idx = ts.contrib_index(G, v).astype(np.int64)
        idxs.append(np.where(idx == ts.n_cells, -1, idx + off))
        cells["set"] = nm
        cells["family"] = nm + ":" + cells["family"].astype(str)
        tabs.append(cells)
        off += ts.n_cells
    return np.concatenate(idxs, axis=1), pd.concat(tabs, ignore_index=True)


def calib(args):
    import b_calibrate as B
    out = Path(args.out) / args.arm
    sizes = args.k if args.arm == "ps0" else args.mult
    for size in sizes:
        z = load_parts(args.out, args.arm, size)
        V = z["tokens"].shape[-1]
        tok = z["tokens"].reshape(-1, V)
        dol = z["dollars"].reshape(-1, V)
        if args.arm == "ps0":
            W = z["weight"]
            N, K = z["tokens"].shape[:2]
        else:
            W = np.array([z["total"]])
            N, K = 1, len(tok)
        for s in args.set:
            idx, tab = a_targets(args.data, s, tok, dol, [str(x) for x in z["slot_names"]])
            t = tab["target"].to_numpy(np.float64)
            C = len(t)
            n_unassigned = int((idx < 0).sum())
            idx, tt, meta = with_spares(idx, t, tab)
            log(f"{args.arm} size {size} set {s}: {len(tok):,} draws, {C:,} cells, "
                f"{n_unassigned:,} unassigned entries")
            w, lam, rep = B.calibrate_matrix(idx.astype(np.int32), W, tt, K, cell_meta=meta.drop(columns=["bin"], errors="ignore"))   # no block check (see SPEC.md, F 20:45)
            st = np.array(rep.pop("cell_status"))[:C]
            rep.pop("reach_any", None), rep.pop("forced", None)
            rep["status_counts_targets"] = {x: int((st == x).sum()) for x in B.STATUSES}
            rep["unassigned_entries"] = n_unassigned
            rep.update({"arm": args.arm, "set": s, "size": size, "K": int(K)})
            if args.arm == "ps1":
                q = w.reshape(-1) / W.sum()
                rep["ess"] = float(1 / (q ** 2).sum())
                rep["ess_share_of_draws"] = rep["ess"] / K
            name = f"{s}_{'K' if args.arm == 'ps0' else 'm'}{size:g}"
            np.savez(out / f"weights_{name}.npz", w=w, **{"lambda": lam[:C]}, cell_status=st.astype(str))
            jdump(rep, out / f"report_{name}.json")
            B.print_summary(f"{args.arm} {name}", rep)
    open(Path(args.out) / f"DONE_{args.arm}", "w").write(time.strftime("%Y-%m-%d %H:%M:%S\n"))


# --------------------------------------------------------------------------
# PS1 with the top oversampled ("ps1s"): strata = income groups
# --------------------------------------------------------------------------
#
# Phase 1 draws a large pool of synthetic records only as far as the income
# measure (E00100 in the full model; the seven incomes in the pilot). Each
# unit's income group is computed from its drawn income against the PUF
# training records' thresholds. Phase 2 keeps every unit of the top three
# groups and a random subset of the bottom two (as many as a QUOTA x n_puf
# sample would hold), and draws the rest of each kept record. Within a group
# the kept records are a random sample from the model's distribution given
# the group, so calibrating each group to its own (targeted) total weight,
# with equal starting weights inside the group, is the same minimum-KL
# problem as calibrating the whole pool: the group totals are targets in
# any case. KL from the model = the move between groups (model share of the
# group in the pool against the target share) + the weighted move within.

POOL = 16          # pool size, multiple of the PUF training count
QUOTA = 4          # bottom two groups kept as in a QUOTA x sample


def income_steps(g):
    """Steps up to and including the income measure's variables."""
    from c_common import INCOMES
    need = ["E00100"] if "E00100" in g.snames else INCOMES
    idx = [g.snames.index(v) for v in need]
    last = max(g.order.index(j) for j in idx)
    return g.order[:last + 1], need


def puf_income_thresholds(g):
    from c_common import wrank as _wrank, INCOMES
    rec = pd.read_parquet(g.data / "records_train.parquet")
    puf = rec["source"].astype(str).to_numpy() == "PUF"
    cols = ["E00100"] if "E00100" in rec.columns else INCOMES
    tot = rec.loc[puf, cols].to_numpy(float).sum(1)
    w = rec.loc[puf, "weight"].to_numpy(float)
    gr = np.searchsorted(np.array([0.5, 0.9, 0.99, 0.999]), _wrank(tot, w), side="right")
    return np.array([tot[gr >= k + 1].min() for k in range(4)]), np.bincount(gr, weights=w, minlength=5)


def draw_strat(args):
    import torch
    out = Path(args.out) / "ps1s"
    out.mkdir(parents=True, exist_ok=True)
    model, g = load(args.data, args.ckpt, args.tokenizer)
    rows, W = puf_records(g)
    pool = int(round((args.mult[0] if args.mult else POOL) * len(rows)))
    setup(g, "ps1", n=pool)
    steps1, need = income_steps(g)
    steps2 = [j for j in g.order if j not in steps1]
    seed = SEED0["ps1"] * 10 + 90 + args.part
    gen = torch.Generator(device=g.device)
    gen.manual_seed(seed)
    t0 = time.time()
    tok1, lp1 = sample(model, g, np.arange(pool), gen, batch=args.batch, order=steps1)
    log(f"phase 1: {pool:,} units through {len(steps1)} steps in {time.time() - t0:.0f}s")
    thr, wg = puf_income_thresholds(g)
    d1 = decode(g, tok1, seed + 7)
    inc = d1[:, [g.snames.index(v) for v in need]].sum(1)
    grp = np.searchsorted(thr, inc, side="right")
    cnt = np.bincount(grp, minlength=5)
    quota = np.array([QUOTA * len(rows) * s for s in (0.5, 0.4)] + [np.inf] * 3)
    rng = np.random.default_rng(seed)
    keep = []
    for s in range(5):
        m = np.flatnonzero(grp == s)
        k = int(min(len(m), quota[s]))
        keep.append(np.sort(rng.choice(m, k, replace=False)) if k < len(m) else m)
    keep = np.concatenate(keep)
    log(f"pool counts by income group {cnt.tolist()}; kept {np.bincount(grp[keep], minlength=5).tolist()}")
    t1 = time.time()
    tok, lp2 = sample(model, g, np.arange(len(keep)), gen, batch=args.batch, start=tok1[keep], order=steps2)
    lp = lp1[keep] + lp2
    log(f"phase 2: {len(keep):,} units through {len(steps2)} steps in {time.time() - t1:.0f}s")
    dol = decode(g, tok, seed + 8)
    dol[:, steps1] = d1[keep][:, steps1]      # the stratum was set from these dollars: keep them
    np.savez(out / f"draws_p{args.part}.npz", tokens=tok, logp_var=lp,
             logp=lp.astype(np.float64).sum(-1).astype(np.float32), dollars=dol,
             stratum=grp[keep].astype(np.int8), pool_counts=cnt, kept_counts=np.bincount(grp[keep], minlength=5),
             group_target_weight=wg, thresholds=thr, slot_names=np.array(g.snames), drawn=g.drawn,
             seed=np.int64(seed), puf_total_weight=np.float64(W.sum()), n_puf=np.int64(len(rows)))
    jdump({"pool": pool, "pool_counts": cnt.tolist(), "kept": np.bincount(grp[keep], minlength=5).tolist(),
           "income_measure": need, "phase1_steps": len(steps1), "phase2_steps": len(steps2),
           "seconds": time.time() - t0, "data": str(args.data), "ckpt": str(args.ckpt)},
          out / f"draws_p{args.part}_checks.json")
    open(out / f"DONE_draws_p{args.part}", "w").write(time.strftime("%Y-%m-%d %H:%M:%S\n"))


def with_spares(idx, t, tab):
    """Entries a row cannot be assigned to (-1) point at one extra cell per
    column slot, with no target (NaN: B leaves it out). One cell per slot
    keeps every cell in a single slot, as B's raking needs."""
    idx = np.asarray(idx, dtype=np.int64).copy()
    C, m = len(t), idx.shape[1]
    r, c = np.nonzero(idx < 0)
    idx[r, c] = C + c
    tt = np.concatenate([np.asarray(t, float), np.full(m, np.nan)])
    meta = pd.concat([tab, pd.DataFrame({"family": ["(unassigned)"] * m, "variable": "-", "group": -1})],
                     ignore_index=True)
    return idx, tt, meta


def calib_strata(idx, stratum, t, Wg, pool_counts, meta):
    """Method 1 with the income groups as the 'records': group s holds its
    kept draws (padded to a common count with rows masked out through B's
    draw_mask) and its targeted total weight Wg[s]. idx [R, m] cell ids of
    the kept draws (-1 = unassigned). Returns per-draw weights [R], lambda,
    cell statuses and a report with the KL from the model split into the
    move between income groups and the move within them."""
    import b_calibrate as B
    R, m = idx.shape
    C = len(t)
    idx, tt, mt = with_spares(idx, t, meta)
    ns = np.bincount(stratum, minlength=len(Wg))
    K = int(ns.max())
    order = np.argsort(stratum, kind="stable")
    pos = np.concatenate([np.arange(n) for n in ns])
    # Padding rows fall only in one zero-target cell per slot, so B drops
    # them as draws in zero cells (B's draw_mask path fails when a group has
    # no draw clear of zero cells: its relaxation step picks a masked row).
    big = np.tile(C + m + np.arange(m, dtype=np.int64), (len(Wg) * K, 1))
    big[stratum[order] * K + pos] = idx[order]
    tt = np.concatenate([tt, np.zeros(m)])
    mt = pd.concat([mt, pd.DataFrame({"family": ["(padding)"] * m, "variable": "-", "group": -2})],
                   ignore_index=True)
    w, lam, rep = B.calibrate_matrix(big.astype(np.int32), Wg.astype(float), tt, K,
                                     cell_meta=mt.drop(columns=["bin"], errors="ignore"))
    wd = np.zeros(R)
    wd[order] = w.reshape(-1)[stratum[order] * K + pos]
    S = Wg.sum()
    p_model = pool_counts / pool_counts.sum()
    between = float(sum((Wg[s] / S) * np.log((Wg[s] / S) / p_model[s]) for s in range(len(Wg)) if Wg[s] > 0))
    within = 0.0
    for s in range(len(Wg)):
        q = wd[stratum == s] / Wg[s]
        q = q[q > 0]
        within += (Wg[s] / S) * float((q * np.log(ns[s] * q)).sum())
    w0 = (Wg / np.maximum(ns, 1))[stratum]
    rep["kl_between_income_groups"] = between
    rep["kl_within_income_groups"] = within
    rep["kl_from_model_total"] = between + within
    rep["ess_final"] = float(wd.sum() ** 2 / (wd ** 2).sum())
    rep["ess_start"] = float(w0.sum() ** 2 / (w0 ** 2).sum())
    rep["draws"] = int(R)
    rep["draws_by_group"] = ns.tolist()
    rep.pop("reach_any", None), rep.pop("forced", None)
    st = np.array(rep.pop("cell_status"))[:C]
    rep["status_counts_targets"] = {x: int((st == x).sum()) for x in B.STATUSES}
    return wd, lam[:C], st, rep


def calib_strat(args):
    out = Path(args.out) / "ps1s"
    z = np.load(out / "draws_p1.npz")
    for s in args.set:
        idx, tab = a_targets(args.data, s, z["tokens"], z["dollars"], [str(x) for x in z["slot_names"]])
        t = tab["target"].to_numpy(np.float64)
        wd, lam, st, rep = calib_strata(idx, z["stratum"].astype(np.int64), t, z["group_target_weight"],
                                        z["pool_counts"], tab)
        rep.update({"arm": "ps1s", "set": s})
        np.savez(out / f"weights_{s}.npz", w=wd, **{"lambda": lam}, cell_status=st.astype(str))
        jdump(rep, out / f"report_{s}.json")
        log(f"ps1s {s}: KL from model {rep['kl_from_model_total']:.4f} (between groups "
            f"{rep['kl_between_income_groups']:.4f}), ESS {rep['ess_final']:,.0f}, statuses {rep['status_counts_targets']}")
    open(Path(args.out) / "DONE_ps1s", "w").write(time.strftime("%Y-%m-%d %H:%M:%S\n"))


# --------------------------------------------------------------------------
# Untargeted measures (net worth levels by income group first) against the
# SCF 2013 and 2016 training records and their interpolation to 2015
# --------------------------------------------------------------------------

AGI_NAMES = ["E00100"]          # income-group variable in the full model
INCOME_ALIASES = {"business": ["E00900", "E26270", "business"], "rent": ["E25850", "-E25860", "rent"]}


def _income_measure(names, dol):
    """Income total used for the income groups: AGI where the model has it,
    else the sum of the seven pilot incomes."""
    if "E00100" in names:
        return dol[:, names.index("E00100")], "E00100 (AGI)"
    from c_common import INCOMES
    return dol[:, [names.index(v) for v in INCOMES]].sum(1), "seven-income total"


def _incd(names, dol):
    out = {}
    for k, al in INCOME_ALIASES.items():
        if k in names:
            out[k] = dol[:, names.index(k)]
            continue
        s = np.zeros(len(dol))
        for a in al:
            sign = -1.0 if a.startswith("-") else 1.0
            a = a.lstrip("-")
            if a in names:
                s = s + sign * np.nan_to_num(dol[:, names.index(a)])
        out[k] = s
    return out


def evaluate(args):
    """Stats for every draws/weights set of an arm, and tables.md."""
    from types import SimpleNamespace
    import e_evaluate as E
    from c_common import token_info, wrank as _wrank
    out = Path(args.out)
    ev = out / "eval"
    ev.mkdir(parents=True, exist_ok=True)
    meta = json.load(open(Path(args.data) / "meta.json"))
    vm = {v["name"]: v for v in meta["variables"]}
    tinfo = {v: token_info(vm[v]) for v in WEALTH}
    # PUF training records: income-group thresholds and rank function
    rec = pd.read_parquet(Path(args.data) / "records_train.parquet")
    puf = rec["source"].astype(str).to_numpy() == "PUF"
    names_all = list(rec.columns)
    if "E00100" in names_all:
        tot_r = rec.loc[puf, "E00100"].to_numpy(float)
    else:
        from c_common import INCOMES
        tot_r = rec.loc[puf, INCOMES].to_numpy(float).sum(1)
    w_r = rec.loc[puf, "weight"].to_numpy(float)
    r_r = _wrank(tot_r, w_r)
    g_r = np.searchsorted(np.array([0.5, 0.9, 0.99, 0.999]), r_r, side="right")
    thr = np.array([tot_r[g_r >= k + 1].min() for k in range(4)])
    o = np.argsort(tot_r, kind="mergesort")
    xs, rs = tot_r[o], r_r[o]
    age_name = "AGERANGE" if "AGERANGE" in vm else "age_range"
    ages = list(range(len(vm[age_name]["tokens"])))
    labels = {i: str(t.get("meaning", t.get("value", i))) for i, t in enumerate(vm[age_name]["tokens"])}
    results = {}
    for arm in ("ps0", "ps1", "ps1s"):
        if arm == "ps1s":
            fz = out / "ps1s" / "draws_p1.npz"
            if not fz.exists():
                continue
            zz = np.load(fz)
            ns = np.bincount(zz["stratum"], minlength=5)
            z = {"tokens": zz["tokens"], "dollars": zz["dollars"], "slot_names": zz["slot_names"],
                 "w0": (zz["group_target_weight"] / np.maximum(ns, 1))[zz["stratum"]]}
        else:
          try:
            z = load_parts(args.out, arm, (args.k or [16])[-1] if arm == "ps0" else (args.mult or [8])[-1])
          except (FileNotFoundError, ValueError):
            try:
                z = load_parts(args.out, arm, K_PART if arm == "ps0" else M_PART)
            except (FileNotFoundError, ValueError) as e:
                log(f"{arm}: no draws ({e})")
                continue
        names = [str(x) for x in z["slot_names"]]
        V = len(names)
        tok = z["tokens"].reshape(-1, V).astype(np.int64)
        dol = z["dollars"].reshape(-1, V)
        if arm == "ps0":
            N, K = z["tokens"].shape[:2]
            w0 = np.repeat(z["weight"] / K, K)
            size = K
        elif arm == "ps1s":
            w0 = z["w0"]
        else:
            w0 = np.full(len(tok), z["total"] / len(tok))
            size = round(len(tok) / (z["total"] and 1) / 1)
        iw = [names.index(v) for v in WEALTH]
        inc, inc_name = _income_measure(names, dol)
        g = np.searchsorted(thr, inc, side="right")
        b = SimpleNamespace(tinfo=tinfo, inc5=g, age=tok[:, names.index(age_name)], ages=ages,
                            age_labels=labels, incd=_incd(names, dol), inc_rank=np.interp(inc, xs, rs))
        sets = [(f"{arm}_ref", w0)]
        for fn in sorted((out / arm).glob("weights_*.npz")):
            wz = np.load(fn)["w"].reshape(-1)
            if len(wz) == len(w0):
                sets.append((f"{arm}_m1_{fn.stem[8:]}", wz))
        for label, w in sets:
            # age groups with weight in this set only (frame_stats fails on empty groups)
            b.ages = [a for a in ages if w[b.age == a].sum() > 0]
            if any(w[g == k].sum() <= 0 for k in range(5)):
                log(f"{label}: an income group is empty; skipped")
                continue
            st = E.frame_stats(b, np.arange(len(w)), tok[:, iw], dol[:, iw], w, with_patterns=False)
            res = {"label": label, "income_measure": inc_name, "stats": st}
            json.dump(E.jsonable(res) if hasattr(E, "jsonable") else res, open(ev / f"{label}.json", "w"),
                      indent=1, default=float)
            results[label] = st
            log(f"{label}: done")
    # SCF side: E's statistics of the SCF 2013 and 2016 training records
    scf = {}
    for y in (2013, 2016):
        fn = P / "e" / "puf" / f"scf{y}" / "stats.json"
        if fn.exists():
            scf[y] = json.load(open(fn))["stats"]
    write_tables(results, scf, ev / "tables.md")


def _interp(a, b, log_scale):
    if log_scale and a > 0 and b > 0:
        return float(np.exp(np.log(a) / 3 + 2 * np.log(b) / 3))
    return a / 3 + 2 * b / 3


def write_tables(results, scf, path):
    def money(x):
        return "-" if x is None else (f"{x / 1e6:,.2f}m" if abs(x) >= 1e6 else f"{x / 1e3:,.0f}k")
    cols = list(results)
    L = []
    have_scf = 2013 in scf and 2016 in scf
    head = ["Income group"] + (["SCF 2013", "SCF 2016", "SCF interpolated 2015"] if have_scf else []) + cols
    for what, key, fmt, lg in (("Mean net worth", "mean", money, True), ("Median net worth", "median", money, True),
                               ("Share of all net worth", "nw_share", lambda x: f"{100 * x:.1f}%", False)):
        L += [f"### {what}, by income group", "", "| " + " | ".join(head) + " |", "|" + "---|" * len(head)]
        for gi in range(5):
            row = [str(gi)]
            if have_scf:
                a, b = scf[2013]["nw_inc"][gi][key], scf[2016]["nw_inc"][gi][key]
                row = [scf[2013]["nw_inc"][gi]["group"], fmt(a), fmt(b), fmt(_interp(a, b, lg))]
            row += [fmt(results[c]["nw_inc"][gi][key]) for c in cols]
            L.append("| " + " | ".join(row) + " |")
        L.append("")
    L += ["### Whole population", "", "| Measure | " + " | ".join(head[1:]) + " |", "|" + "---|" * len(head)]
    rows = [("Mean net worth", lambda s: s["nw_mean"], money, True)]
    rows += [(f"Net worth {q}", (lambda q: lambda s: s["nw_q"][q])(q), money, True) for q in ("p25", "p50", "p75", "p90", "p99")]
    keys = list(next(iter(results.values()))["nw_top"]) if results else []
    rows += [(f"Share of net worth, {t}", (lambda t: lambda s: s["nw_top"][t])(t), lambda x: f"{100 * x:.1f}%", False) for t in keys]
    for name, f, fmt, lg in rows:
        row = [name]
        if have_scf:
            try:
                a, b = f(scf[2013]), f(scf[2016])
                row += [fmt(a), fmt(b), fmt(_interp(a, b, lg))]
            except KeyError:
                row += ["-", "-", "-"]
        row += [fmt(f(results[c])) for c in cols]
        L.append("| " + " | ".join(row) + " |")
    L.append("")
    txt = "\n".join(L)
    open(path, "w").write(txt)
    print(txt)


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("stage")
    ap.add_argument("arm", nargs="?")
    ap.add_argument("--part", type=int, default=1)
    ap.add_argument("--k", type=int, nargs="+")
    ap.add_argument("--mult", type=float, nargs="+")
    ap.add_argument("--set", nargs="+", default=["core", "full"])
    ap.add_argument("--data", default=DEFAULTS["data"])
    ap.add_argument("--ckpt", default=DEFAULTS["ckpt"])
    ap.add_argument("--tokenizer", default=DEFAULTS["tokenizer"])
    ap.add_argument("--out", default=DEFAULTS["out"])
    ap.add_argument("--adir", default=str(P / "full" / "a"))
    ap.add_argument("--n-records", type=int)
    ap.add_argument("--n-synthetic", type=int)
    ap.add_argument("--batch", type=int, default=16384)
    a = ap.parse_args()
    if a.stage == "pilot":
        for k, v in PILOT.items():
            setattr(a, k, v)
        a.arm, a.n_records, a.k, a.part = "ps0", 3000, 2, 1
        draw(a)
        a.arm, a.n_synthetic, a.part = "ps1", 5000, 1
        draw(a)
    elif a.stage == "draw":
        draw(a)
    elif a.stage == "calib":
        calib_strat(a) if a.arm == "ps1s" else calib(a)
    elif a.stage == "draw_strat":
        draw_strat(a)
    elif a.stage == "pilot_strat":
        for k, v in PILOT.items():
            setattr(a, k, v)
        draw_strat(a)
    elif a.stage == "eval":
        evaluate(a)
    elif a.stage == "pilot_eval":
        for k, v in PILOT.items():
            setattr(a, k, v)
        evaluate(a)
    else:
        raise SystemExit(a.stage)


if __name__ == "__main__":
    main()
