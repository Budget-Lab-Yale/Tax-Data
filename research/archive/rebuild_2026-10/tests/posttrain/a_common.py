"""Test 3 (post-training), piece A: shared records, target cells, the
contribution matrix, reference log-probabilities and decoding to dollars.

Every other piece imports this file; nobody re-implements the mapping from
tokens to target cells. Contract: SPEC.md in this folder.

Quick use
---------
    import a_common as A
    recs  = A.load_records()             # $P/a/records.parquet, one row per record
    cells = A.load_targets()             # $P/a/targets.parquet, one row per target cell
    d     = A.load_draws("dev")          # dict: tokens [N,K,24], logp_ref [N,K], logp_ref_var [N,K,24]

    idx = A.contrib_index(d["tokens"], recs)   # int32 [N*K, 264]: the 264 cells each draw touches
    Acsr = A.contrib(d["tokens"], recs)        # scipy CSR [N*K, n_cells], entries 1.0
    # rows are record-major: row = i*K + k.

    # On the GPU, without building a matrix:
    idx_t = torch.as_tensor(idx, device="cuda")
    tot = A.cell_totals_torch(idx_t, w_t, A.n_cells())   # A^T w (w: one weight per row)
    s   = A.draw_scores_torch(idx_t, lam_t)              # A lam (one number per row)

    model, ctx = A.load_reference("cuda")      # reference network and its record inputs
    tok, lpv = A.sample(model, ctx, rec_idx, gen)            # float32 draws and log-probs
    lpv = A.ref_logprob(model, ctx, rec_idx, tokens)          # [n, 24] float32, WEALTH order
    dollars = A.decode_dollars(tokens, recs, seed)            # float64 [..., 24], WEALTH order

Definitions
-----------
Records: expB training rows with source SCF and wealth_observed true, in the
order they appear in records_train.parquet. `rec` is 0..N-1; `row` is the
row in expB's records_train.parquet / tokens_train.npy.

Wealth variables are always in `WEALTH` order (c_common.py) in every array
this file reads or writes. The model fills them in `ctx.order`, which is
fill_order(meta) with the dropped survey weight removed (the 24 wealth
variables in meta.json's generation order). The survey weight is never an
input: the checkpoint was trained with it dropped, so it stays hidden.

Coarse bins of a wealth variable, in this order: 'neg' (any negative value;
only for variables with negative tokens), 'zero', then twelve positive rank
bands p00_10, p10_20, ..., p80_90, p90_99, p99_999, p999_100 (rank among
the wave's positive holders, from the token's band). So 14 bins with
negatives, 13 without (see "Contract changes" in SPEC.md: the spec text
said 13 and 12 but listed 14 and 13).

Families and groups (group numbers are 0-based, in this order):
    inc     income group by weighted rank of the seven-income total within
            wave over all SCF training units of the wave, split-offs
            included: <50, 50-90, 90-99, 99-99.9, top 0.1 (5 groups)
    age     age_range token (6)       mar  married token (2)
    dep     n_dep token (4)
    x_<inc> the income's own coarse bin: 'neg' (only if the income has
            negative tokens), 'zero', p00_50, p50_90, p90_100 (4 or 5)

Cell layout: a block is one (wave, family, group); blocks are numbered wave
first, then family, then group. Within a block the cells run over WEALTH
variables and, within a variable, over its coarse bins. So
    cell = block * VB + VB_START[v] + bin,   VB = 319 cells per block.
Every cell exists for both waves even when its target is zero (for example
a negative home value in 2016); such cells are zero cells.
"""

import json
import os
import sys
import time
from pathlib import Path

import numpy as np
import pandas as pd

HERE = Path(__file__).resolve().parent
T2CODE = HERE.parent / "scf_joint"
if str(T2CODE) not in sys.path:
    sys.path.insert(0, str(T2CODE))

from c_common import WEALTH, ASSETS, DEBTS, INCOMES, wrank   # noqa: E402

T2 = Path("/nfs/roberts/scratch/pi_nrs36/jar335/scf_joint")
DATA = T2 / "data" / "expB"
CKPT = T2 / "model" / "expB" / "checkpoint_joint_w1_seed01.pt"
P = Path("/nfs/roberts/scratch/pi_nrs36/jar335/posttrain")
PA = P / "a"

WAVES = [2013, 2016]
FAMILIES = ["inc", "age", "mar", "dep"] + ["x_" + v for v in INCOMES]
INC_EDGES = [0.5, 0.9, 0.99, 0.999]
INC_LABELS = ["<50", "50-90", "90-99", "99-99.9", "top0.1"]
# Positive coarse-bin edges for wealth (rank among positive holders).
WEALTH_POS_EDGES = [0.0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 0.99, 0.999]
WEALTH_POS_LABELS = ["p00_10", "p10_20", "p20_30", "p30_40", "p40_50", "p50_60",
                     "p60_70", "p70_80", "p80_90", "p90_99", "p99_999", "p999_100"]
INCOME_POS_EDGES = [0.0, 0.5, 0.9]
INCOME_POS_LABELS = ["p00_50", "p50_90", "p90_100"]

NV = len(WEALTH)                 # 24
NF = len(FAMILIES)               # 11
PER_DRAW = NF * NV               # 264 cells touched by each draw


def log(msg, t0=[time.time()]):
    print(f"[{time.time() - t0[0]:7.1f}s] {msg}", flush=True)


# ---------------------------------------------------------------------------
# Metadata and coarse bins
# ---------------------------------------------------------------------------

_META = None


def meta():
    global _META
    if _META is None:
        with open(DATA / "meta.json") as f:
            _META = json.load(f)
    return _META


def vmeta(name):
    for v in meta()["variables"]:
        if v["name"] == name:
            return v
    raise KeyError(name)


def _coarse(vm, edges, pos_labels):
    """Map every token of an amount variable to a coarse bin. Returns
    (labels, tok2bin int array over the vocabulary)."""
    toks = vm["tokens"]
    kinds = np.array([t["kind"] for t in toks])
    has_neg = bool((kinds == "neg").any())
    labels = (["neg"] if has_neg else []) + ["zero"] + list(pos_labels)
    off = 2 if has_neg else 1
    out = np.full(len(toks), -1, dtype=np.int64)
    e = np.asarray(edges + [1.0])
    for i, t in enumerate(toks):
        if t["kind"] == "neg":
            out[i] = 0
        elif t["kind"] == "zero":
            out[i] = off - 1
        elif t["kind"] == "pos":
            lo, hi = float(t["lo"]), float(t["hi"])
            b = int(np.searchsorted(e, lo + 1e-12, side="right") - 1)
            if not (e[b] - 1e-9 <= lo and hi <= e[b + 1] + 1e-9):
                raise ValueError(f"{vm['name']} token {i} band [{lo},{hi}] straddles a coarse edge")
            out[i] = off + b
        else:
            raise ValueError(f"{vm['name']} token {i}: unexpected kind {t['kind']}")
    if (out < 0).any():
        raise ValueError(f"{vm['name']}: tokens without a coarse bin")
    return labels, out


def wealth_bins():
    """{variable: (labels, tok2bin)} for the 24 wealth variables."""
    return {v: _coarse(vmeta(v), WEALTH_POS_EDGES, WEALTH_POS_LABELS) for v in WEALTH}


def income_bins():
    """{income: (labels, tok2group)} for the seven x_ families."""
    return {v: _coarse(vmeta(v), INCOME_POS_EDGES, INCOME_POS_LABELS) for v in INCOMES}


def _cat_labels(name):
    return [str(t["meaning"]) for t in vmeta(name)["tokens"]]


def family_groups():
    """{family: list of group labels}."""
    out = {"inc": list(INC_LABELS), "age": _cat_labels("age_range"),
           "mar": _cat_labels("married"), "dep": _cat_labels("n_dep")}
    ib = income_bins()
    for v in INCOMES:
        out["x_" + v] = ib[v][0]
    return out


class Layout:
    """Cell numbering (see the module docstring)."""

    def __init__(self):
        self.wbins = wealth_bins()
        self.groups = family_groups()
        self.nbins = np.array([len(self.wbins[v][0]) for v in WEALTH])
        self.vb_start = np.concatenate([[0], np.cumsum(self.nbins)[:-1]])
        self.VB = int(self.nbins.sum())
        # block table
        rows = []
        for wi, w in enumerate(WAVES):
            for fi, f in enumerate(FAMILIES):
                for g, lab in enumerate(self.groups[f]):
                    rows.append((len(rows), w, wi, f, fi, g, lab))
        self.blocks = pd.DataFrame(rows, columns=["block", "wave", "wave_i", "family",
                                                  "family_i", "group", "group_label"])
        self.block_of = {(r.wave, r.family, r.group): r.block for r in self.blocks.itertuples()}
        self.n_blocks = len(self.blocks)
        self.n_cells = self.n_blocks * self.VB
        # token -> position within a block, per wealth variable [24, max_vocab]
        maxv = max(len(self.wbins[v][1]) for v in WEALTH)
        self.tok2vb = np.full((NV, maxv), -1, dtype=np.int64)
        for k, v in enumerate(WEALTH):
            t2b = self.wbins[v][1]
            self.tok2vb[k, :len(t2b)] = self.vb_start[k] + t2b

    def cell_table(self):
        """One row per cell: cell, block, wave, family, group, group_label,
        variable, bin, bin_label."""
        vb_var = np.repeat(np.arange(NV), self.nbins)
        vb_bin = np.concatenate([np.arange(n) for n in self.nbins])
        vb_lab = np.concatenate([self.wbins[v][0] for v in WEALTH])
        b = self.blocks
        nb, VB = self.n_blocks, self.VB
        return pd.DataFrame({
            "cell": np.arange(self.n_cells),
            "block": np.repeat(b["block"].to_numpy(), VB),
            "wave": np.repeat(b["wave"].to_numpy(), VB),
            "family": np.repeat(b["family"].to_numpy(), VB),
            "group": np.repeat(b["group"].to_numpy(), VB),
            "group_label": np.repeat(b["group_label"].to_numpy(), VB),
            "variable": np.tile(np.array(WEALTH)[vb_var], nb),
            "var_i": np.tile(vb_var, nb),
            "bin": np.tile(vb_bin, nb),
            "bin_label": np.tile(vb_lab, nb),
        })


_LAYOUT = None


def layout():
    global _LAYOUT
    if _LAYOUT is None:
        _LAYOUT = Layout()
    return _LAYOUT


def n_cells():
    return layout().n_cells


def coarse_bin(tokens):
    """Coarse bin (within-variable index) of wealth tokens [..., 24]."""
    L = layout()
    t = np.asarray(tokens, dtype=np.int64)
    return L.tok2vb[np.arange(NV), t] - L.vb_start


# ---------------------------------------------------------------------------
# Records
# ---------------------------------------------------------------------------

BLOCK_COLS = ["blk_" + f for f in FAMILIES]
GROUP_COLS = ["g_" + f for f in FAMILIES]


def build_records():
    """Assemble the records frame from expB's training files (no model).
    Returns (records DataFrame, notes dict)."""
    m = meta()
    names = [v["name"] for v in m["variables"]]
    rec = pd.read_parquet(DATA / "records_train.parquet")
    tok = np.load(DATA / "tokens_train.npy").astype(np.int64)
    truth = np.load(DATA / "truth_train.npy").astype(np.int64)
    if not (len(rec) == len(tok) == len(truth)):
        raise ValueError("records, tokens and truth differ in length")
    notes = {"train_rows": len(rec)}
    src = rec["source"].astype(str).to_numpy()
    yr = rec["year"].to_numpy()
    if np.isnan(yr.astype(float)).any():
        raise ValueError("NA year in records_train")
    yr = yr.astype(int)
    wo_raw = rec["wealth_observed"]
    notes["wealth_observed_na_all_sources"] = int(wo_raw.isna().sum())
    notes["wealth_observed_na_scf"] = int(wo_raw[src == "SCF"].isna().sum())
    if notes["wealth_observed_na_scf"]:
        raise ValueError(f"{notes['wealth_observed_na_scf']} SCF rows with NA wealth_observed")
    scf = src == "SCF"
    wo = scf & wo_raw.fillna(False).to_numpy().astype(bool)   # NA only on non-SCF rows (checked)
    notes["scf_train_rows"] = int(scf.sum())
    notes["scf_train_rows_wealth_unobserved"] = int((scf & ~wo).sum())
    for w in WAVES:
        notes[f"scf_train_rows_{w}"] = int((scf & (yr == w)).sum())
        notes[f"records_{w}"] = int((wo & (yr == w)).sum())
    if sorted(set(yr[scf].tolist())) != WAVES:
        raise ValueError(f"SCF waves {sorted(set(yr[scf].tolist()))}, expected {WAVES}")

    # Income group, ranked over every SCF training row of the wave.
    inc = rec[INCOMES].to_numpy(dtype=float)
    if np.isnan(inc[scf]).any():
        raise ValueError(f"NA incomes on SCF rows: {np.isnan(inc[scf]).sum(axis=0).tolist()}")
    wt = rec["weight"].to_numpy(dtype=float)
    if np.isnan(wt[scf]).any() or (wt[scf] <= 0).any():
        raise ValueError("SCF rows with NA or non-positive weight")
    tot = inc.sum(axis=1)
    rank = np.full(len(rec), np.nan)
    for w in WAVES:
        ii = np.flatnonzero(scf & (yr == w))
        rank[ii] = wrank(tot[ii], wt[ii])
    g_inc = np.searchsorted(np.array(INC_EDGES), rank, side="right")

    keep = np.flatnonzero(wo)
    N = len(keep)
    full = np.where(tok >= 0, tok, truth)
    col = {n: j for j, n in enumerate(names)}
    out = pd.DataFrame({"rec": np.arange(N), "row": keep.astype(np.int64),
                        "source": src[keep], "year": yr[keep]})
    for c in ["hh_id", "implicate", "unit"]:
        if c in rec.columns:
            out[c] = rec[c].to_numpy()[keep]
    out["weight"] = wt[keep]
    out["wave_i"] = np.searchsorted(WAVES, out["year"].to_numpy())
    out["inc_total"] = tot[keep]
    out["inc_rank"] = rank[keep]
    groups = {"inc": g_inc[keep],
              "age": full[keep, col["age_range"]],
              "mar": full[keep, col["married"]],
              "dep": full[keep, col["n_dep"]]}
    ib = income_bins()
    for v in INCOMES:
        t = full[keep, col[v]]
        if (t < 0).any():
            raise ValueError(f"{v}: {(t < 0).sum()} records without an income token")
        groups["x_" + v] = ib[v][1][t]
    for f in ["age", "mar", "dep"]:
        if (groups[f] < 0).any():
            raise ValueError(f"{f}: {(groups[f] < 0).sum()} records without a token")
    L = layout()
    for f in FAMILIES:
        out["g_" + f] = groups[f].astype(np.int64)
        ng = len(L.groups[f])
        if (out["g_" + f] >= ng).any():
            raise ValueError(f"family {f}: group out of range")
    wave_i = out["wave_i"].to_numpy()
    # block id = offset of (wave, family) + group
    off = {}
    for r in L.blocks.itertuples():
        off.setdefault((r.wave_i, r.family), r.block - r.group)
    for f in FAMILIES:
        o = np.array([off[(wi, f)] for wi in range(len(WAVES))])
        out["blk_" + f] = (o[wave_i] + out["g_" + f].to_numpy()).astype(np.int64)
    for v in ["age_range", "married", "n_dep"] + INCOMES:
        out["tok_" + v] = full[keep, col[v]].astype(np.int64)
    for v in INCOMES:
        out["d_" + v] = inc[keep, INCOMES.index(v)]
    wt_tok = np.stack([full[keep, col[v]] for v in WEALTH], axis=1)
    bad = (wt_tok < 0).sum(axis=0)
    if bad.any():
        raise ValueError(f"records with missing true wealth tokens: {dict(zip(WEALTH, bad.tolist()))}")
    for k, v in enumerate(WEALTH):
        out["tok_" + v] = wt_tok[:, k].astype(np.int64)
    dol = rec[WEALTH].to_numpy(dtype=float)[keep]
    nan = np.isnan(dol).sum(axis=0)
    if nan.any():
        raise ValueError(f"records with NA true wealth dollars: {dict(zip(WEALTH, nan.tolist()))}")
    for k, v in enumerate(WEALTH):
        out["d_" + v] = dol[:, k]
    out["nw"] = nw_from(dol)
    notes["records"] = N
    return out, notes


def nw_from(d):
    """Net worth from a [..., 24] dollar array in WEALTH order: assets minus
    debts (the four unrealized-gain variables are not part of it)."""
    d = np.asarray(d, dtype=np.float64)
    ia = [WEALTH.index(v) for v in ASSETS]
    idb = [WEALTH.index(v) for v in DEBTS]
    return d[..., ia].sum(axis=-1) - d[..., idb].sum(axis=-1)


def load_records():
    return pd.read_parquet(PA / "records.parquet")


def true_tokens(recs):
    return np.stack([recs["tok_" + v].to_numpy() for v in WEALTH], axis=1).astype(np.int64)


def load_targets():
    return pd.read_parquet(PA / "targets.parquet")


def load_draws(name="ref"):
    """name: 'dev' (K=4), 'ref' (K=32), 'ref_b' (second K=32), 'ref_k128'."""
    fn = {"dev": "dev_draws.npz", "ref": "ref_draws.npz", "ref_b": "ref_draws_b.npz",
          "ref_k128": "ref_draws_k128.npz"}.get(name, name)
    z = np.load(PA / fn if not os.path.isabs(fn) else fn, allow_pickle=False)
    return {k: z[k] for k in z.files}


# ---------------------------------------------------------------------------
# Contribution matrix
# ---------------------------------------------------------------------------

def contrib_index(tokens, recs):
    """Cell ids touched by each draw. tokens: [N, 24] or [N, K, 24] wealth
    tokens (WEALTH order), N = len(recs) and in records order. Returns int32
    [N*K, 264] (K = 1 for 2-D input), rows record-major (row = i*K + k);
    columns family-major (family f, variable v at f*24 + v)."""
    L = layout()
    t = np.asarray(tokens, dtype=np.int64)
    if t.ndim == 2:
        t = t[:, None, :]
    N, K, V = t.shape
    if V != NV or N != len(recs):
        raise ValueError(f"tokens {t.shape} do not match {len(recs)} records x 24")
    tb = L.tok2vb[np.arange(NV)[None, None, :], t]             # [N, K, 24]
    if (tb < 0).any():
        raise ValueError("token outside a variable's vocabulary")
    blk = recs[BLOCK_COLS].to_numpy(dtype=np.int64)             # [N, 11]
    idx = blk[:, None, :, None] * L.VB + tb[:, :, None, :]      # [N, K, 11, 24]
    return idx.reshape(N * K, PER_DRAW).astype(np.int32)


def contrib(tokens, recs, dtype=np.float32):
    """Sparse contribution matrix A (scipy CSR), [N*K, n_cells], entry 1 where
    a draw falls in a cell. Row = i*K + k."""
    import scipy.sparse as sp
    idx = contrib_index(tokens, recs)
    R = idx.shape[0]
    indptr = np.arange(0, R * PER_DRAW + 1, PER_DRAW, dtype=np.int64)
    # Within a row the 264 cells are distinct (different family or variable),
    # so no duplicates; sort the column indices per row for a canonical CSR.
    ind = np.sort(idx, axis=1).reshape(-1)
    data = np.ones(R * PER_DRAW, dtype=dtype)
    return sp.csr_matrix((data, ind, indptr), shape=(R, n_cells()))


def contrib_torch(idx, device=None, dtype=None):
    """torch sparse CSR [rows, n_cells] from contrib_index output (numpy or
    torch)."""
    import torch
    idx_t = torch.as_tensor(idx, device=device)
    R = idx_t.shape[0]
    crow = torch.arange(0, R * PER_DRAW + 1, PER_DRAW, device=idx_t.device, dtype=torch.int64)
    col = torch.sort(idx_t.long(), dim=1).values.reshape(-1)
    val = torch.ones(R * PER_DRAW, device=idx_t.device, dtype=dtype or torch.float32)
    return torch.sparse_csr_tensor(crow, col, val, size=(R, n_cells()))


def cell_totals_torch(idx_t, w_t, ncell=None):
    """A^T w on the GPU: total weight in each cell. idx_t [R, 264] (int32 or
    int64), w_t [R]."""
    import torch
    ncell = ncell or n_cells()
    out = torch.zeros(ncell, device=w_t.device, dtype=w_t.dtype)
    out.index_add_(0, idx_t.reshape(-1).long(),
                   w_t[:, None].expand(-1, idx_t.shape[1]).reshape(-1))
    return out


def draw_scores_torch(idx_t, lam_t):
    """A lam on the GPU: sum of the cell numbers each draw touches. [R]."""
    return lam_t[idx_t.long()].sum(dim=1)


def cell_totals(idx, w, ncell=None):
    """A^T w in numpy (float64)."""
    w = np.asarray(w, dtype=np.float64)
    return np.bincount(np.asarray(idx).reshape(-1),
                       weights=np.repeat(w, idx.shape[1]), minlength=ncell or n_cells())


# ---------------------------------------------------------------------------
# Targets
# ---------------------------------------------------------------------------

def targets(recs):
    """Table of cells with their targets: total population weight of records
    (true tokens) in each cell, number of records (implicate rows) and of
    distinct households. Zero cells have target 0."""
    L = layout()
    tab = L.cell_table()
    idx = contrib_index(true_tokens(recs), recs)
    W = recs["weight"].to_numpy(dtype=np.float64)
    tab["target"] = cell_totals(idx, W)
    tab["n_records"] = np.bincount(idx.reshape(-1), minlength=L.n_cells).astype(np.int64)
    # distinct households per cell
    hh = (recs["year"].to_numpy(np.int64) * 10**9 + recs["hh_id"].to_numpy(np.int64))
    pairs = pd.DataFrame({"cell": idx.reshape(-1), "hh": np.repeat(hh, PER_DRAW)}).drop_duplicates()
    tab["n_households"] = np.bincount(pairs["cell"].to_numpy(), minlength=L.n_cells).astype(np.int64)
    tab["zero_cell"] = tab["target"] == 0
    return tab


# ---------------------------------------------------------------------------
# Reference model
# ---------------------------------------------------------------------------

class RefContext:
    """The reference network's inputs for every record, on one device.

    tok_in [N, V] int64 slot tokens; the variables to fill and every slot the
    record does not observe are -1. Unobserved slots that are not in `fill`
    (for example a PUF record's missing age) stay hidden throughout: they
    are never drawn. src [N], year [N] float32; order: slot indices filled,
    in order; fill: names of the filled variables (the token arrays' column
    order); wslot[k]: slot index of fill[k]; step_var[s]: index in `fill`
    filled at step s; allowed [n_groups, nfill, max_v] bool and agroup [N]
    (the record's (source, year) group for the allowed-code table); wave_i
    is kept as an alias of agroup for the SCF arm."""


TOKENIZER = None          # None: ../scf_joint/a_tokenizer.py
_TOKMOD = None


def tokenizer():
    """The tokenizer module (decode, _qf_cache, _rank_basis): test 2's
    a_tokenizer by default, or the file set by configure(tokenizer=...)
    (G's g_tokenizer.py for dataset `full`)."""
    global _TOKMOD
    if _TOKMOD is None:
        if TOKENIZER is None:
            import a_tokenizer
            _TOKMOD = a_tokenizer
        else:
            import importlib.util
            spec = importlib.util.spec_from_file_location(Path(TOKENIZER).stem, TOKENIZER)
            mod = importlib.util.module_from_spec(spec)
            spec.loader.exec_module(mod)
            _TOKMOD = mod
    return _TOKMOD


def configure(data=None, ckpt=None, out=None, tokenizer=None):
    """Point the module at another dataset, checkpoint, output folder and
    tokenizer file (the full model: configure_full()). Resets cached
    metadata. The SCF-arm defaults are expB, checkpoint_joint_w1_seed01.pt,
    $P/a and test 2's a_tokenizer."""
    global DATA, CKPT, PA, _META, _LAYOUT, _ALLOWED, TOKENIZER, _TOKMOD
    if tokenizer is not None:
        TOKENIZER = str(tokenizer)
        _TOKMOD = None
    if data is not None:
        DATA = Path(data)
    if ckpt is not None:
        CKPT = Path(ckpt)
    if out is not None:
        PA = Path(out)
    _META = None
    _LAYOUT = None
    _ALLOWED = None


def configure_full(version="full", ckpt=None):
    """Use dataset `full` or `full_v2` (piece G), its seed-1 checkpoint
    (piece H: $T2/model/<version>/checkpoint_<version>_w1_seed01.pt) and
    $P/<version>/a."""
    configure(T2 / "data" / version,
              ckpt or T2 / "model" / version / f"checkpoint_{version}_w1_seed01.pt",
              P / version / "a", HERE / "g_tokenizer.py")


def allowed_table(recs, fill=None, decode_year=None, max_v=None):
    """Allowed codes (b_complete's rule) for each distinct (source, year) of
    recs. Returns (table [n_groups, nfill, max_v] bool, group index per
    record [N], groups list, notes)."""
    from b_complete import allowed_code_tables
    from b_model import slot_variables
    a_tokenizer = tokenizer()
    fill = list(fill or WEALTH)
    m = meta()
    svars = slot_variables(m)
    snames = [v["name"] for v in svars]
    max_v = max_v or max(int(v["vocab_size"]) for v in svars)
    keys = list(zip(recs["source"].astype(str), recs["year"].astype(int)))
    groups = sorted(set(keys))
    gi = {k: i for i, k in enumerate(groups)}
    gidx = np.array([gi[k] for k in keys], dtype=np.int64)
    gr = pd.DataFrame({"source": [g[0] for g in groups], "year": [g[1] for g in groups]})
    tab, gg, notes = allowed_code_tables(a_tokenizer, m, DATA, gr, decode_year, svars, max_v)
    ws = [snames.index(v) for v in fill]
    table = np.stack([tab[gg[i]][ws] for i in range(len(groups))])
    return table, gidx, groups, notes


def load_reference(device="cuda", recs=None, fill=None, split="train", as_source=None,
                   decode_year=None):
    """Load the checkpoint (CKPT) and every record's inputs.

    recs: frame with `row` (row of the split's token file), `source`, `year`
    (default: load_records()). fill: variables to draw (default the 24 wealth
    variables). Every observed value of a record is kept except the fill
    variables, which are hidden and drawn; any other unobserved slot stays
    hidden and is never drawn. as_source: present every record with this
    source tag (e.g. 'SCF' for PUF records). decode_year: year whose quantile
    functions decide the allowed codes for variables the record's source does
    not observe (2015 for PUF records)."""
    import torch
    from b_model import fill_order, load_checkpoint, slot_columns, slot_variables
    torch.backends.cuda.matmul.allow_tf32 = False
    torch.backends.cudnn.allow_tf32 = False
    torch.set_float32_matmul_precision("highest")
    device = torch.device(device)
    fill = list(fill or WEALTH)
    m = meta()
    model, ck = load_checkpoint(CKPT, m, device)
    for p in model.parameters():
        p.requires_grad_(False)
    svars = slot_variables(m)
    snames = [v["name"] for v in svars]
    scols = slot_columns(m)
    dropped = [x for x in (ck.get("args") or {}).get("drop_vars", "").split(",") if x]
    bad = [v for v in fill if v in dropped or v not in snames]
    if bad:
        raise ValueError(f"fill variables are not model slots: {bad}")
    recs = load_records() if recs is None else recs
    tok_all = np.load(DATA / f"tokens_{split}.npy")
    tok = tok_all[recs["row"].to_numpy()][:, scols].astype(np.int64)
    wslot = [snames.index(v) for v in fill]
    tok_in = tok.copy()
    tok_in[:, wslot] = -1
    # Variables dropped in training are never inputs (as b_complete.py).
    drop_j = [snames.index(x) for x in dropped if x in snames]
    tok_in[:, drop_j] = -1
    order = [j for j in fill_order(m, svars) if j in wslot]
    sources = ck["sources"]
    src_names = recs["source"].astype(str).to_numpy()
    if as_source is not None:
        src_names = np.full(len(recs), as_source)
    src = np.array([sources.index(s) for s in src_names], dtype=np.int64)
    year = recs["year"].to_numpy().astype(np.float32)
    allowed, agroup, agroups, notes = allowed_table(recs, fill, decode_year, model.max_v)
    other_hidden = (tok_in < 0).sum(axis=0)
    other_hidden[wslot] = 0
    other_hidden[drop_j] = 0
    ctx = RefContext()
    ctx.device = device
    ctx.model_args = ck.get("args")
    ctx.dropped = dropped
    ctx.snames = snames
    ctx.fill = fill
    ctx.nfill = len(fill)
    ctx.order = order
    ctx.order_names = [snames[j] for j in order]
    ctx.wslot = wslot
    ctx.step_var = [wslot.index(j) for j in order]
    ctx.unfilled_hidden = {snames[j]: int(c) for j, c in enumerate(other_hidden) if c}
    ctx.tok_in = torch.as_tensor(tok_in, device=device)
    ctx.src = torch.as_tensor(src, device=device)
    ctx.year = torch.as_tensor(year, device=device)
    ctx.agroup = torch.as_tensor(agroup, device=device)
    ctx.wave_i = ctx.agroup
    ctx.agroups = agroups
    ctx.allowed = torch.as_tensor(allowed, device=device)
    ctx.allowed_np = allowed
    ctx.agroup_np = agroup
    ctx.allowed_notes = notes
    ctx.max_v = model.max_v
    ctx.N = len(recs)
    ctx.as_source = as_source
    ctx.decode_year = decode_year
    return model, ctx


def _step_logits(model, ctx, cur, hid, ri, j, k, allowed, bias_fn):
    import torch
    logits = model(cur, hid, ctx.src[ri], ctx.year[ri])          # float32, no autocast
    lj = logits[:, j, :].float()
    if allowed:
        lj = lj.masked_fill(~ctx.allowed[ctx.agroup[ri], k, :lj.shape[1]], float("-inf"))
    if bias_fn is not None:
        lj = bias_fn(k, ri, lj)
    return torch.log_softmax(lj, dim=-1)


def ref_logprob(model, ctx, rec_idx, tokens, allowed=True, batch_size=16384, bias_fn=None):
    """Float32 log-probability of given tokens of the fill variables under the
    network, in the fixed order: for each draw, log p(x_v | observed, earlier
    x) for each fill variable. rec_idx [n] (record of each draw), tokens
    [n, nfill] in ctx.fill order (WEALTH order by default). allowed: restrict
    to the allowed codes (the reference distribution). bias_fn(k,
    rec_idx_batch, logits) -> logits, optional, applied to the masked logits
    of variable fill[k] before normalizing (for method 2's tilt). Returns
    numpy float32 [n, nfill]."""
    import torch
    rec_idx = np.asarray(rec_idx, dtype=np.int64)
    tokens = np.asarray(tokens, dtype=np.int64)
    n, nf = len(rec_idx), ctx.nfill
    out = np.zeros((n, nf), dtype=np.float32)
    ws = torch.as_tensor(ctx.wslot, device=ctx.device)
    with torch.no_grad():
        for s0 in range(0, n, batch_size):
            ri = torch.as_tensor(rec_idx[s0:s0 + batch_size], device=ctx.device)
            tk = torch.as_tensor(tokens[s0:s0 + batch_size], device=ctx.device)
            cur = ctx.tok_in[ri].clone()
            cur[:, ws] = tk
            hid = ctx.tok_in[ri] < 0
            res = torch.zeros(len(ri), nf, device=ctx.device, dtype=torch.float32)
            for j, k in zip(ctx.order, ctx.step_var):
                lp = _step_logits(model, ctx, cur, hid, ri, j, k, allowed, bias_fn)
                res[:, k] = lp.gather(1, tk[:, k:k + 1]).squeeze(1)
                hid[:, j] = False
            out[s0:s0 + batch_size] = res.cpu().numpy()
    return out


def sample(model, ctx, rec_idx, gen, allowed=True, batch_size=16384, bias_fn=None,
           step_hook=None, temperature=1.0):
    """Draw the fill variables in the fixed order, in float32, for each entry
    of rec_idx (repeat an index for several draws). gen: a torch.Generator on
    ctx.device. Returns (tokens int16 [n, nfill], logp float32 [n, nfill]) in
    ctx.fill order; logp is the float32 log-probability of each drawn token
    at temperature 1 under the (possibly tilted) model, i.e. equal to
    ref_logprob of the same tokens. step_hook(k, rec_idx_batch, s0, logp
    [B, max_v]) is called at every step (e.g. for method 2's expected
    cells)."""
    import torch
    rec_idx = np.asarray(rec_idx, dtype=np.int64)
    n, nf = len(rec_idx), ctx.nfill
    tok_out = np.zeros((n, nf), dtype=np.int16)
    lp_out = np.zeros((n, nf), dtype=np.float32)
    with torch.no_grad():
        for s0 in range(0, n, batch_size):
            ri = torch.as_tensor(rec_idx[s0:s0 + batch_size], device=ctx.device)
            cur = ctx.tok_in[ri].clone()
            hid = cur < 0
            tk = torch.zeros(len(ri), nf, device=ctx.device, dtype=torch.int64)
            res = torch.zeros(len(ri), nf, device=ctx.device, dtype=torch.float32)
            for j, k in zip(ctx.order, ctx.step_var):
                lp = _step_logits(model, ctx, cur, hid, ri, j, k, allowed, bias_fn)
                if step_hook is not None:
                    step_hook(k, ri, s0, lp)
                pr = torch.softmax(lp / temperature, dim=-1) if temperature != 1.0 else lp.exp()
                d = torch.multinomial(pr, 1, generator=gen).squeeze(1)
                tk[:, k] = d
                res[:, k] = lp.gather(1, d[:, None]).squeeze(1)
                cur[:, j] = d
                hid[:, j] = False
            tok_out[s0:s0 + batch_size] = tk.cpu().numpy().astype(np.int16)
            lp_out[s0:s0 + batch_size] = res.cpu().numpy()
    return tok_out, lp_out


def draw_set(model, ctx, K, seed, batch_size=16384, **kw):
    """K draws per record for every record. Returns dict in the draws-file
    layout: tokens [N, K, nfill] int16, logp_ref [N, K] float32 (sum over
    the fill variables, accumulated in float64), logp_ref_var [N, K, nfill]
    float32, seed, K, rec, wealth (the fill variable names)."""
    import torch
    N = ctx.N
    gen = torch.Generator(device=ctx.device)
    gen.manual_seed(int(seed))
    rec_idx = np.repeat(np.arange(N), K)
    t0 = time.time()
    tok, lpv = sample(model, ctx, rec_idx, gen, batch_size=batch_size, **kw)
    log(f"  {N:,} records x {K} draws in {time.time() - t0:.1f}s")
    tok = tok.reshape(N, K, ctx.nfill)
    lpv = lpv.reshape(N, K, ctx.nfill)
    return dict(tokens=tok, logp_ref=lpv.astype(np.float64).sum(axis=2).astype(np.float32),
                logp_ref_var=lpv, seed=np.int64(seed), K=np.int64(K),
                rec=np.arange(N), wealth=np.array(ctx.fill))


# ---------------------------------------------------------------------------
# Dollars
# ---------------------------------------------------------------------------

def decode_dollars(tokens, recs, seed, chunk=400_000, names=None, year=None):
    """Tokens of variables `names` (default WEALTH) [N, nv] or [N, K, nv],
    records order, to dollars [same shape] float64 with a_tokenizer.decode:
    each record at its own source and year's quantile functions, or, for a
    variable its source does not observe, the SCF's at `year` (default the
    record's year; pass 2015 for PUF records: 2013 and 2016 interpolated in
    log dollars, as test 2). Position within a token's rank band drawn from
    np.random.default_rng(seed). Raises if a token has no quantile function
    (a code the allowed-code rule excludes)."""
    a_tokenizer = tokenizer()
    names = list(names or WEALTH)
    nv = len(names)
    m = meta()
    allnames = [v["name"] for v in m["variables"]]
    t = np.asarray(tokens, dtype=np.int64)
    shp = t.shape
    if t.ndim == 2:
        t = t[:, None, :]
    N, K, _ = t.shape
    flat = t.reshape(N * K, nv)
    al, agi, _, _ = allowed_table(recs, names, year)
    ag = np.repeat(agi, K)
    bad = ~al[ag[:, None], np.arange(nv)[None, :], flat]
    if bad.any():
        raise ValueError(f"tokens with no quantile function: "
                         f"{dict(zip(names, bad.sum(axis=0).tolist()))}")
    q = pd.read_parquet(DATA / "quantiles.parquet")
    cache = a_tokenizer._qf_cache(q)
    rng = np.random.default_rng(seed)
    cols = [allnames.index(v) for v in names]
    yrs = np.repeat(recs["year"].to_numpy(), K)
    srcs = np.repeat(recs["source"].astype(str).to_numpy(), K)
    out = np.empty((N * K, nv), dtype=np.float64)
    for s0 in range(0, N * K, chunk):
        sl = slice(s0, s0 + chunk)
        full = np.full((len(flat[sl]), len(allnames)), -1, dtype=np.int64)
        full[:, cols] = flat[sl]
        r = pd.DataFrame({"source": srcs[sl], "year": yrs[sl]})
        d = a_tokenizer.decode(full, m, r, q, rng, year=year, cache=cache)
        out[sl] = d[names].to_numpy(dtype=np.float64)
    if np.isnan(out).any():
        raise ValueError("decoded dollars contain NaN")
    return out.reshape(shp)


_ALLOWED = None


def allowed_codes():
    """Allowed codes [n_waves, 24, max_v] bool for the SCF arm (CPU)."""
    global _ALLOWED
    if _ALLOWED is None:
        wave_recs = pd.DataFrame({"source": ["SCF"] * len(WAVES), "year": WAVES})
        _ALLOWED = allowed_table(wave_recs, WEALTH)[0]
    return _ALLOWED
