"""Probe Q: the SCF arm on the version 2 (full_v2) model, with four target
families (income group, age range, filing status, children under 17).

Reuses test 3's piece A module (../posttrain/a_common.py) by pointing it at
the full_v2 data and a chosen checkpoint and replacing its families. Piece
A's SCF arm was written for test 2's dataset; nothing in ../posttrain is
edited, the module is configured and its family globals are replaced in
memory for this process only.

Records: SCF rows of a split with wealth observed (split-off units out), in
the order of records_<split>.parquet. Income group: the sum of the eleven
income amounts the SCF measures in full_v2, against dollar thresholds at
weighted ranks 0.5, 0.9, 0.99, 0.999 among all SCF *training* rows of the
wave (split-offs included, as piece A), so test records are grouped by the
training thresholds.
"""
import os
import sys
from pathlib import Path

import numpy as np
import pandas as pd

HERE = Path(__file__).resolve().parent
PT = HERE.parent / "posttrain"
sys.path.insert(0, str(PT))
sys.path.insert(0, str(HERE.parent / "scf_joint"))
import a_common as A          # noqa: E402
from c_common import wrank, ASSETS, DEBTS, KG, WEALTH   # noqa: E402

T2 = Path("/nfs/roberts/scratch/pi_nrs36/jar335/scf_joint")
DATA = T2 / "model" / "full_v2" / "data"
Q = Path("/nfs/roberts/scratch/pi_nrs36/jar335/probes/q")
QA = Q / "a"
BEST = T2 / "model" / "full_v2" / "checkpoint_full_v2_w1_seed01.pt"

# The eleven income amounts the SCF measures in full_v2 (meta.json, group "shared").
SCF_INCOMES = ["E00200", "E00900", "E02000", "P23250", "E00600", "E00300", "E01700",
               "E02400", "E02100", "E00400", "E02300"]
INCOME_LABELS = {"E00200": "wages", "E00900": "Schedule C", "E02000": "Schedule E",
                 "P23250": "long-term gains", "E00600": "dividends", "E00300": "taxable interest",
                 "E01700": "taxable pensions", "E02400": "Social Security", "E02100": "farm",
                 "E00400": "tax-exempt interest", "E02300": "unemployment"}
FAM_VAR = {"age": "AGERANGE", "mar": "MARS", "dep": "N24"}
FAMILIES = ["inc", "age", "mar", "dep"]


def setup(ckpt=None):
    """Point piece A's module at full_v2, the checkpoint and QA, with the four
    families."""
    A.configure(data=DATA, ckpt=Path(ckpt or BEST), out=QA, tokenizer=PT / "g_tokenizer.py")
    A.FAMILIES = list(FAMILIES)
    A.NF = len(FAMILIES)
    A.PER_DRAW = A.NF * A.NV
    A.BLOCK_COLS = ["blk_" + f for f in FAMILIES]
    A.GROUP_COLS = ["g_" + f for f in FAMILIES]
    A.family_groups = family_groups
    return A


def _labels(name):
    return [str(t.get("meaning", t.get("value", t.get("token")))) for t in A.vmeta(name)["tokens"]]


def family_groups():
    out = {"inc": list(A.INC_LABELS)}
    for f, v in FAM_VAR.items():
        out[f] = _labels(v)
    return out


def _split_frames(split):
    m = A.meta()
    names = [v["name"] for v in m["variables"]]
    rec = pd.read_parquet(DATA / f"records_{split}.parquet")
    tok = np.load(DATA / f"tokens_{split}.npy").astype(np.int64)
    truth = np.load(DATA / f"truth_{split}.npy").astype(np.int64)
    assert len(rec) == len(tok) == len(truth)
    return names, rec, tok, truth


FARM = "E02100"


def income_total(inc, mask_desc):
    """Sum of the eleven incomes. Farm (E02100) is unobserved for SCF
    households that filed both Schedules C and F (g_crosswalk.md): their
    combined amount is in Schedule C (E00900), so the farm term is counted
    as 0 for them, once, not dropped. Any other NA is an error."""
    nan = np.isnan(inc)
    j = SCF_INCOMES.index(FARM)
    other = np.delete(nan, j, axis=1)
    if other.any():
        raise ValueError(f"NA incomes other than farm ({mask_desc}): {other.sum(0).tolist()}")
    print(f"income total ({mask_desc}): farm unobserved on {int(nan[:, j].sum())} rows, "
          "counted inside Schedule C", flush=True)
    return np.where(nan, 0.0, inc).sum(axis=1)


def income_thresholds():
    """Dollar thresholds of the income groups per wave, from SCF training rows."""
    names, rec, tok, truth = _split_frames("train")
    scf = rec["source"].astype(str).to_numpy() == "SCF"
    inc = rec[SCF_INCOMES].to_numpy(dtype=float)
    tot = np.full(len(rec), np.nan)
    tot[scf] = income_total(inc[scf], "SCF training rows")
    yr = rec["year"].to_numpy()
    wt = rec["weight"].to_numpy(float)
    out = {}
    for w in A.WAVES:
        ii = np.flatnonzero(scf & (yr == w))
        r = wrank(tot[ii], wt[ii])
        o = np.argsort(r)
        out[w] = [float(tot[ii][o][np.searchsorted(r[o], e, side="left")]) for e in A.INC_EDGES]
    return out


def build_records(split, thr=None):
    thr = thr or income_thresholds()
    names, rec, tok, truth = _split_frames(split)
    src = rec["source"].astype(str).to_numpy()
    yr = rec["year"].to_numpy().astype(int)
    scf = src == "SCF"
    wo_raw = rec["wealth_observed"]
    if wo_raw[scf].isna().any():
        raise ValueError("SCF rows with NA wealth_observed")
    wo = scf & wo_raw.fillna(False).to_numpy().astype(bool)   # NA only on PUF rows (checked)
    keep = np.flatnonzero(wo)
    N = len(keep)
    full = np.where(tok >= 0, tok, truth)
    col = {n: j for j, n in enumerate(names)}
    inc = rec[SCF_INCOMES].to_numpy(dtype=float)[keep]
    tot = income_total(inc, f"SCF {split} rows with wealth observed")
    y = yr[keep]
    g_inc = np.zeros(N, dtype=np.int64)
    for w in A.WAVES:
        m = y == w
        # a unit exactly at a threshold dollar value goes to the upper group,
        # as a weighted rank equal to the edge does in piece A
        g_inc[m] = np.searchsorted(np.array(thr[w]), tot[m], side="right")
    out = pd.DataFrame({"rec": np.arange(N), "row": keep.astype(np.int64),
                        "source": src[keep], "year": y})
    for c in ["hh_id", "implicate", "unit"]:
        out[c] = rec[c].to_numpy()[keep]
    out["weight"] = rec["weight"].to_numpy(float)[keep]
    if not np.all(out["weight"] > 0):
        raise ValueError("non-positive weights")
    out["wave_i"] = np.searchsorted(A.WAVES, y)
    out["inc_total"] = tot
    groups = {"inc": g_inc}
    for f, v in FAM_VAR.items():
        t = full[keep, col[v]]
        if (t < 0).any():
            raise ValueError(f"{v}: {(t < 0).sum()} SCF records without a token")
        groups[f] = t
    L = A.layout()
    off = {}
    for r in L.blocks.itertuples():
        off.setdefault((r.wave_i, r.family), r.block - r.group)
    wave_i = out["wave_i"].to_numpy()
    for f in FAMILIES:
        out["g_" + f] = groups[f].astype(np.int64)
        o = np.array([off[(wi, f)] for wi in range(len(A.WAVES))])
        out["blk_" + f] = (o[wave_i] + out["g_" + f].to_numpy()).astype(np.int64)
    for k, v in enumerate(SCF_INCOMES):
        out["d_" + v] = inc[:, k]
    wt_tok = np.stack([full[keep, col[v]] for v in WEALTH], axis=1)
    if (wt_tok < 0).any():
        raise ValueError("records with missing true wealth tokens")
    for k, v in enumerate(WEALTH):
        out["tok_" + v] = wt_tok[:, k]
    dol = rec[WEALTH].to_numpy(dtype=float)[keep]
    if np.isnan(dol).any():
        raise ValueError("records with NA true wealth dollars")
    for k, v in enumerate(WEALTH):
        out["d_" + v] = dol[:, k]
    out["nw"] = A.nw_from(dol)
    return out


def records(split):
    """Cached records frame of a split."""
    fn = QA / f"records_{split}.parquet"
    if fn.exists():
        return pd.read_parquet(fn)
    QA.mkdir(parents=True, exist_ok=True)
    r = build_records(split)
    r.to_parquet(fn)
    return r


def targets(split):
    fn = QA / f"targets_{split}.parquet"
    if fn.exists():
        return pd.read_parquet(fn)
    t = A.targets(records(split))
    t.to_parquet(fn)
    return t


# Inputs to hide from the model, for the deliberately weak variants.
HIDE = {
    "none": [],
    "noincome": SCF_INCOMES,
    "nothing": None,      # every observed slot except source and year
}


def hide_inputs(ctx, which):
    """Set chosen observed slots of every record to unobserved (-1)."""
    import torch
    if which == "none":
        return []
    names = ctx.snames
    if HIDE[which] is None:
        cols = [j for j, n in enumerate(names) if n not in WEALTH]
    else:
        cols = [names.index(n) for n in HIDE[which]]
    ctx.tok_in[:, torch.as_tensor(cols, device=ctx.device)] = -1
    return [names[j] for j in cols]
