"""Test 3, piece F: the from-scratch arms (SPEC.md, contract change of the
main session, 9 October 20:25).

Arm S0 ("close to scratch"): each real record keeps its year, age_range,
married and n_dep; the seven incomes and the 24 wealth variables are drawn,
K per record, in the fixed order (incomes in meta.json order, then the 24
wealth variables in A's order). Each record keeps its weight across its
draws.

Arm S1 ("from scratch"): whole SCF records drawn from nothing but source SCF
and the year: age_range, married, n_dep, the seven incomes, then wealth.
N synthetic records per wave with equal starting weights summing to the
wave's total weight.

Arm "c" (completion) is A's setup, used only to check that this sampler
reproduces A's draws and log-probabilities.

This file holds the sampler (same allowed-code restriction and float32
log-probabilities as a_common.sample), the cell layout of F's targets, the
income-group thresholds, the contribution index and the targets.

Variable order in every F array: VARS[arm] (S0: INCOMES + WEALTH; S1:
SHARED_CAT + INCOMES + WEALTH).

Cells. A block is A's (wave, family, group); the 96 blocks are numbered as
in a_common.Layout. Within a block, cells run over the 31 target variables
(INCOMES + WEALTH) and their coarse bins (same bin rule for incomes as for
wealth: negative, zero, twelve positive rank bands), except that an income
is not crossed with its own x_ family. For S1 there are also the joint
age x married x dependents cells (48 per wave) and one wave-total cell per
wave. A draw touches 11*24 + 10*7 = 334 cells (S0) or 336 (S1).
"""
import json
import os
import sys
import time
from pathlib import Path

import numpy as np
import pandas as pd

HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(HERE))
import a_common as A                                   # noqa: E402
from a_common import WEALTH, INCOMES, WAVES, FAMILIES  # noqa: E402
from c_common import wrank                             # noqa: E402

SHARED_CAT = ["age_range", "married", "n_dep"]
VT = INCOMES + WEALTH                                  # target variables
VARS = {"c": list(WEALTH), "s0": INCOMES + WEALTH, "s1": SHARED_CAT + INCOMES + WEALTH}
P = A.P
PF = P / "f"


def log(msg, t0=[time.time()]):
    print(f"[{time.time() - t0[0]:7.1f}s] {msg}", flush=True)


# ---------------------------------------------------------------------------
# Model and inputs
# ---------------------------------------------------------------------------

class Gen:
    """Reference network plus everything needed to draw for one arm.
    tok_in [n_units, V] slot tokens with the drawn slots hidden (-1);
    order: slot indices drawn, in order; col[s]: column of VARS[arm] filled
    at step s; allowed [n_waves, V, max_v] bool over all slots."""


def load_model(device="cuda"):
    import torch
    from b_model import fill_order, load_checkpoint, slot_columns, slot_variables
    from b_complete import allowed_code_tables
    import a_tokenizer
    torch.backends.cuda.matmul.allow_tf32 = False
    torch.backends.cudnn.allow_tf32 = False
    torch.set_float32_matmul_precision("highest")
    device = torch.device(device)
    m = A.meta()
    model, ck = load_checkpoint(A.CKPT, m, device)
    for p in model.parameters():
        p.requires_grad_(False)
    svars = slot_variables(m)
    snames = [v["name"] for v in svars]
    dropped = [x for x in (ck.get("args") or {}).get("drop_vars", "").split(",") if x]
    drop_j = [snames.index(x) for x in dropped]
    G = Gen()
    G.device = device
    G.svars, G.snames, G.scols = svars, snames, slot_columns(m)
    G.dropped, G.drop_j = dropped, drop_j
    G.full_order = [j for j in fill_order(m, svars) if j not in drop_j]
    G.sources = list(ck["sources"])
    wave_recs = pd.DataFrame({"source": ["SCF"] * len(WAVES), "year": WAVES})
    tab, gidx, notes = allowed_code_tables(a_tokenizer, m, A.DATA, wave_recs, None, svars, model.max_v)
    G.allowed_np = np.stack([tab[gidx[i]] for i in range(len(WAVES))])     # [W, V, max_v]
    G.allowed = torch.as_tensor(G.allowed_np, device=device)
    G.allowed_notes = notes
    G.max_v = model.max_v
    return model, G


def set_units(G, arm, recs=None, wave_i=None):
    """Inputs for the units being drawn. arm 'c' / 's0': A's records (recs);
    arm 's1': synthetic units, one per entry of wave_i."""
    import torch
    V = len(G.snames)
    hide_scf = np.array([v.get("group") == "scf_only" for v in G.svars])
    if arm in ("c", "s0"):
        tok_all = np.load(A.DATA / "tokens_train.npy")
        tok = tok_all[recs["row"].to_numpy()][:, G.scols].astype(np.int64)
        hide = hide_scf.copy()
        if arm == "s0":
            hide |= np.isin(G.snames, INCOMES)
        tok_in = tok.copy()
        tok_in[:, hide] = -1
        if not np.array_equal((tok_in < 0).any(axis=0), hide):
            bad = [G.snames[j] for j in np.flatnonzero((tok_in < 0).any(axis=0) & ~hide)]
            raise ValueError(f"unexpected unobserved slots: {bad}")
        wave_i = recs["wave_i"].to_numpy()
    elif arm == "s1":
        wave_i = np.asarray(wave_i, dtype=np.int64)
        tok_in = np.full((len(wave_i), V), -1, dtype=np.int64)
        hide = np.ones(V, dtype=bool)
    else:
        raise ValueError(arm)
    order = [j for j in G.full_order if hide[j]]
    names = [G.snames[j] for j in order]
    if sorted(names) != sorted(VARS[arm]):
        raise ValueError(f"arm {arm}: fill order {names} is not {VARS[arm]}")
    G.arm = arm
    G.order = order
    G.order_names = names
    G.col = [VARS[arm].index(G.snames[j]) for j in order]
    G.tok_in = torch.as_tensor(tok_in, device=G.device)
    G.wave_i_np = np.asarray(wave_i, dtype=np.int64)
    G.wave_i = torch.as_tensor(G.wave_i_np, device=G.device)
    G.src = torch.full((len(wave_i),), G.sources.index("SCF"), dtype=torch.int64, device=G.device)
    G.year = torch.as_tensor(np.array(WAVES, dtype=np.float32)[G.wave_i_np], device=G.device)
    G.n_units = len(wave_i)
    return G


def _step(model, G, cur, hid, ui, j, c, bias_fn):
    import torch
    src = G.src[ui]
    if getattr(G, "src_col", None) is not None:          # PUF arms: source tag per drawn variable
        src = torch.full_like(src, int(G.src_col[c]))
    logits = model(cur, hid, src, G.year[ui])            # float32, no autocast
    lj = logits[:, j, :].float()
    lj = lj.masked_fill(~G.allowed[G.wave_i[ui], j, :lj.shape[1]], float("-inf"))
    if bias_fn is not None:
        lj = bias_fn(c, ui, lj)
    return torch.log_softmax(lj, dim=-1)


def sample(model, G, unit_idx, gen, batch_size=16384, bias_fn=None, step_hook=None):
    """Draw VARS[G.arm] for each entry of unit_idx in the fixed order.
    Returns (tokens int16 [n, nv], logp float32 [n, nv]) in VARS order.
    bias_fn(c, unit_idx_batch, logits) adjusts the masked logits of
    VARS[arm][c]; step_hook(c, unit_idx_batch, s0, logp) sees every step."""
    import torch
    unit_idx = np.asarray(unit_idx, dtype=np.int64)
    n, nv = len(unit_idx), len(VARS[G.arm])
    tok_out = np.zeros((n, nv), dtype=np.int16)
    lp_out = np.zeros((n, nv), dtype=np.float32)
    with torch.no_grad():
        for s0 in range(0, n, batch_size):
            ui = torch.as_tensor(unit_idx[s0:s0 + batch_size], device=G.device)
            cur = G.tok_in[ui].clone()
            hid = cur < 0
            tk = torch.zeros(len(ui), nv, device=G.device, dtype=torch.int64)
            res = torch.zeros(len(ui), nv, device=G.device, dtype=torch.float32)
            for j, c in zip(G.order, G.col):
                lp = _step(model, G, cur, hid, ui, j, c, bias_fn)
                if step_hook is not None:
                    step_hook(c, ui, s0, lp)
                d = torch.multinomial(lp.exp(), 1, generator=gen).squeeze(1)
                tk[:, c] = d
                res[:, c] = lp.gather(1, d[:, None]).squeeze(1)
                cur[:, j] = d
                hid[:, j] = False
            tok_out[s0:s0 + batch_size] = tk.cpu().numpy().astype(np.int16)
            lp_out[s0:s0 + batch_size] = res.cpu().numpy()
    return tok_out, lp_out


def logprob(model, G, unit_idx, tokens, batch_size=16384, bias_fn=None):
    """Float32 log-probability of given tokens (VARS order) under the
    network in the fixed order. Returns [n, nv]."""
    import torch
    unit_idx = np.asarray(unit_idx, dtype=np.int64)
    tokens = np.asarray(tokens, dtype=np.int64)
    n, nv = len(unit_idx), len(VARS[G.arm])
    out = np.zeros((n, nv), dtype=np.float32)
    with torch.no_grad():
        for s0 in range(0, n, batch_size):
            ui = torch.as_tensor(unit_idx[s0:s0 + batch_size], device=G.device)
            tk = torch.as_tensor(tokens[s0:s0 + batch_size], device=G.device)
            cur = G.tok_in[ui].clone()
            hid = cur < 0
            for j, c in zip(G.order, G.col):
                cur[:, j] = tk[:, c]
            res = torch.zeros(len(ui), nv, device=G.device, dtype=torch.float32)
            for j, c in zip(G.order, G.col):
                lp = _step(model, G, cur, hid, ui, j, c, bias_fn)
                res[:, c] = lp.gather(1, tk[:, c:c + 1]).squeeze(1)
                hid[:, j] = False
            out[s0:s0 + batch_size] = res.cpu().numpy()
    return out


# ---------------------------------------------------------------------------
# Dollars and income groups
# ---------------------------------------------------------------------------

def decode(tokens, names, wave_i, seed, chunk=400_000):
    """tokens [n, len(names)] (names among the slot variables) -> DataFrame of
    dollars (categoricals decoded to their values), with a_tokenizer.decode
    at each unit's own wave; position within a band from default_rng(seed)."""
    import a_tokenizer
    m = A.meta()
    allnames = [v["name"] for v in m["variables"]]
    cols = [allnames.index(v) for v in names]
    q = pd.read_parquet(A.DATA / "quantiles.parquet")
    cache = a_tokenizer._qf_cache(q)
    rng = np.random.default_rng(seed)
    t = np.asarray(tokens, dtype=np.int64)
    yrs = np.array(WAVES)[np.asarray(wave_i)]
    out = []
    for s0 in range(0, len(t), chunk):
        full = np.full((len(t[s0:s0 + chunk]), len(allnames)), -1, dtype=np.int64)
        full[:, cols] = t[s0:s0 + chunk]
        r = pd.DataFrame({"source": "SCF", "year": yrs[s0:s0 + chunk]})
        d = a_tokenizer.decode(full, m, r, q, rng, year=None, cache=cache)
        out.append(d[names])
    out = pd.concat(out, ignore_index=True)
    nan = out.isna().sum()
    if nan.any():
        raise ValueError(f"decoded dollars contain NaN: {nan[nan > 0].to_dict()}")
    return out


def income_thresholds():
    """Total-income dollar thresholds of the four income-group edges per
    wave, from every SCF training unit of the wave (split-offs included), as
    in A's records. thr [n_waves, 4]: group = number of thresholds <= total."""
    rec = pd.read_parquet(A.DATA / "records_train.parquet")
    src = rec["source"].astype(str).to_numpy()
    yr = rec["year"].to_numpy().astype(int)
    tot = rec[INCOMES].to_numpy(dtype=float).sum(axis=1)
    wt = rec["weight"].to_numpy(dtype=float)
    thr = np.zeros((len(WAVES), len(A.INC_EDGES)))
    for wi, w in enumerate(WAVES):
        ii = np.flatnonzero((src == "SCF") & (yr == w))
        if np.isnan(tot[ii]).any() or np.isnan(wt[ii]).any():
            raise ValueError("NA income or weight on SCF rows")
        g = np.searchsorted(np.array(A.INC_EDGES), wrank(tot[ii], wt[ii]), side="right")
        for k in range(len(A.INC_EDGES)):
            thr[wi, k] = tot[ii][g >= k + 1].min()
    return thr


def income_group(total, wave_i, thr):
    total = np.asarray(total, dtype=float)
    g = np.zeros(len(total), dtype=np.int64)
    for wi in range(len(WAVES)):
        m = np.asarray(wave_i) == wi
        g[m] = np.searchsorted(thr[wi], total[m], side="right")
    return g


# ---------------------------------------------------------------------------
# Cell layout, contribution index, targets
# ---------------------------------------------------------------------------

class FLayout:
    def __init__(self, arm):
        L = A.layout()
        self.arm = arm
        self.A = L
        self.blocks = L.blocks
        self.vbins = {v: A._coarse(A.vmeta(v), A.WEALTH_POS_EDGES, A.WEALTH_POS_LABELS) for v in VT}
        for v in WEALTH:
            if not np.array_equal(self.vbins[v][1], L.wbins[v][1]):
                raise ValueError(f"{v}: coarse bins differ from A's")
        self.xbins = A.income_bins()
        self.pairs = [(fi, vi) for fi, f in enumerate(FAMILIES) for vi, v in enumerate(VT)
                      if f != "x_" + v]
        self.pf = np.array([p[0] for p in self.pairs])
        self.pv = np.array([p[1] for p in self.pairs])
        rows, base = [], np.full((L.n_blocks, len(VT)), -1, dtype=np.int64)
        for b in L.blocks.itertuples():
            for vi, v in enumerate(VT):
                if b.family == "x_" + v:
                    continue
                base[b.block, vi] = len(rows)
                labs = self.vbins[v][0]
                for k, lab in enumerate(labs):
                    a_cell = (b.block * L.VB + L.vb_start[WEALTH.index(v)] + k) if v in WEALTH else -1
                    rows.append((len(rows), b.block, b.wave, b.wave_i, b.family, b.group,
                                 b.group_label, v, "income" if v in INCOMES else "wealth", k, lab, a_cell))
        self.base = base
        self.demo_start = self.total_start = None
        if arm == "s1":
            self.ndemo = [len(A.vmeta(v)["tokens"]) for v in SHARED_CAT]     # 6, 2, 4
            nd = int(np.prod(self.ndemo))
            self.demo_start = np.zeros(len(WAVES), dtype=np.int64)
            self.total_start = np.zeros(len(WAVES), dtype=np.int64)
            for wi, w in enumerate(WAVES):
                self.demo_start[wi] = len(rows)
                for g in range(nd):
                    a_, r_ = divmod(g, self.ndemo[1] * self.ndemo[2])
                    m_, d_ = divmod(r_, self.ndemo[2])
                    rows.append((len(rows), -1, w, wi, "demo", g, f"age{a_}|mar{m_}|dep{d_}",
                                 "age_range*married*n_dep", "demo", 0, "-", -1))
            for wi, w in enumerate(WAVES):
                self.total_start[wi] = len(rows)
                rows.append((len(rows), -1, w, wi, "total", 0, "all", "weight", "total", 0, "-", -1))
        self.cells = pd.DataFrame(rows, columns=["cell", "block", "wave", "wave_i", "family", "group",
                                                 "group_label", "variable", "var_kind", "bin",
                                                 "bin_label", "a_cell"])
        self.n_cells = len(self.cells)
        self.per_draw = len(self.pairs) + (2 if arm == "s1" else 0)
        off = np.zeros((len(WAVES), len(FAMILIES)), dtype=np.int64)
        for r in L.blocks.itertuples():
            if r.group == 0:
                off[r.wave_i, r.family_i] = r.block
        self.off = off
        maxv = max(len(self.vbins[v][1]) for v in VT)
        self.tok2bin = np.full((len(VT), maxv), -1, dtype=np.int64)
        for vi, v in enumerate(VT):
            self.tok2bin[vi, :len(self.vbins[v][1])] = self.vbins[v][1]

    def groups(self, vt_tokens, demo_tokens, inc_group):
        """Group of each family for each row: [R, 11]. vt_tokens [R, 31]
        (VT order), demo_tokens [R, 3] (age_range, married, n_dep tokens)."""
        R = len(vt_tokens)
        g = np.zeros((R, len(FAMILIES)), dtype=np.int64)
        g[:, 0] = inc_group
        g[:, 1:4] = demo_tokens
        for k, v in enumerate(INCOMES):
            g[:, 4 + k] = self.xbins[v][1][vt_tokens[:, VT.index(v)]]
        return g

    def index(self, wave_i, vt_tokens, demo_tokens, inc_group, chunk=200_000):
        """Cell ids touched by each row: int32 [R, per_draw]."""
        vt = np.asarray(vt_tokens, dtype=np.int64)
        dm = np.asarray(demo_tokens, dtype=np.int64)
        wave_i = np.asarray(wave_i, dtype=np.int64)
        R = len(vt)
        out = np.empty((R, self.per_draw), dtype=np.int32)
        for s in range(0, R, chunk):
            e = min(R, s + chunk)
            g = self.groups(vt[s:e], dm[s:e], np.asarray(inc_group)[s:e])
            blk = self.off[wave_i[s:e]] + g                                  # [r, 11]
            bins = self.tok2bin[np.arange(len(VT))[None, :], vt[s:e]]       # [r, 31]
            if (bins < 0).any():
                raise ValueError("token outside a variable's vocabulary")
            b = self.base[blk[:, self.pf], self.pv]
            if (b < 0).any():
                raise ValueError("cell base missing")
            out[s:e, :len(self.pairs)] = b + bins[:, self.pv]
            if self.arm == "s1":
                d = (dm[s:e, 0] * self.ndemo[1] + dm[s:e, 1]) * self.ndemo[2] + dm[s:e, 2]
                out[s:e, len(self.pairs)] = self.demo_start[wave_i[s:e]] + d
                out[s:e, len(self.pairs) + 1] = self.total_start[wave_i[s:e]]
        return out


def record_arrays(recs):
    """True VT tokens [N, 31], demographic tokens [N, 3], A's income group."""
    vt = np.stack([recs["tok_" + v].to_numpy() for v in VT], axis=1).astype(np.int64)
    dm = np.stack([recs["tok_" + v].to_numpy() for v in SHARED_CAT], axis=1).astype(np.int64)
    return vt, dm, recs["g_inc"].to_numpy().astype(np.int64)


def targets(FL, recs):
    """F's targets from the real records: total weight per cell, number of
    records and of distinct households."""
    vt, dm, gi = record_arrays(recs)
    idx = FL.index(recs["wave_i"].to_numpy(), vt, dm, gi)
    W = recs["weight"].to_numpy(dtype=np.float64)
    tab = FL.cells.copy()
    tab["target"] = np.bincount(idx.reshape(-1), weights=np.repeat(W, idx.shape[1]), minlength=FL.n_cells)
    tab["n_records"] = np.bincount(idx.reshape(-1), minlength=FL.n_cells)
    hh = recs["year"].to_numpy(np.int64) * 10**9 + recs["hh_id"].to_numpy(np.int64)
    pairs = pd.DataFrame({"cell": idx.reshape(-1), "hh": np.repeat(hh, idx.shape[1])}).drop_duplicates()
    tab["n_households"] = np.bincount(pairs["cell"].to_numpy(), minlength=FL.n_cells)
    tab["zero_cell"] = tab["target"] == 0
    return tab, idx
