"""Piece C: one interface to the records, targets, reference model and cell
mapping of an arm, for c_fit.py and c_post.py.

Arms:
  scf        the pilot: SCF training records with their own tables
             (a_common, test 2's expB dataset and current checkpoint).
  P_core,    arm P of the full model: PUF training records' wealth, targets
  P_full     A's wealth target sets 'wcore' / 'wfull' (a_full.py: SCF
             wealth tables carried to the PUF as shares within groups).
             Dataset 'full' or 'full_v2' (A.configure_full), or the pilot
             of a_full on expB (--dataset pilot_expB).

For the wealth target sets every table covers the same wealth variables,
so a cell is  block * VB + vb_start[v] + bin  with block = table offset /
VB + group, exactly the layout c_tilt.Geometry uses; a record with no
group in a table (age unknown) has block -1.
"""
import os
import sys

import numpy as np
import pandas as pd

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, HERE)

ARM_SETS = {"P_core": "wcore", "P_full": "wfull"}


class Arm:
    """Fields: name, A (a_common), recs (frame with `weight`), cells
    (targets frame in cell order with `target`, `family`), T [C],
    fam_of_cell [C], blk [N, F] (-1: no group), VB, vb_start, nbins,
    tok2bin (lists over the targeted variables), var_names, fill (the
    variables drawn, in tokens order), has_truth, load_reference(),
    contrib_index(tokens [N, K, V], recs) -> int [N*K, m] (-1: no cell),
    ref_kwargs."""


def make_arm(arm, dataset=None, checkpoint=None):
    import a_common as A
    o = Arm()
    o.name, o.A = arm, A
    if arm in (None, "scf"):
        if dataset or checkpoint:
            A.configure(data=dataset if dataset and os.path.isabs(dataset) else None,
                        ckpt=checkpoint)
        L = A.layout()
        o.recs = A.load_records()
        o.cells = A.load_targets()
        o.T = o.cells["target"].to_numpy(np.float64)
        o.fam_of_cell = o.cells["family"].to_numpy()
        o.blk = o.recs[A.BLOCK_COLS].to_numpy(np.int64)
        o.VB, o.vb_start, o.nbins = L.VB, list(L.vb_start), list(L.nbins)
        o.tok2bin = [L.wbins[v][1] for v in A.WEALTH]
        o.var_names = list(A.WEALTH)
        o.fill = list(A.WEALTH)
        o.has_truth = True
        o.ref_kwargs = {}
        o.contrib_index = A.contrib_index
        o.group_cols = {"wave": "year", "inc": "g_inc"}
        return o

    import a_full as AF
    set_name = ARM_SETS[arm]
    pilot = dataset == "pilot_expB"
    if pilot:
        AF.configure_pilot()
    else:
        A.configure_full(dataset or "full", checkpoint)
    ts = AF.rebuild_set(set_name, pilot)
    o.ts = ts
    o.recs = AF.load_puf_records()
    o.cells = AF.load_set(set_name)
    if not (o.cells["cell"].to_numpy() == np.arange(ts.n_cells)).all():
        raise ValueError("targets not in cell order")
    o.T = o.cells["target"].to_numpy(np.float64)
    o.fam_of_cell = o.cells["family"].to_numpy()
    vs0 = ts.tables[0][1]
    for fam, vs in ts.tables:
        if list(vs) != list(vs0):
            raise ValueError(f"table {fam.name} covers other variables; method 2 needs "
                             "the same variables in every table")
    VB = ts.VB[0]
    if any(v != VB for v in ts.VB) or any(o_ % VB for o_ in ts.offsets):
        raise ValueError("tables differ in cells per block")
    G = np.stack([o.recs[f"g_{set_name}_{fam.name}"].to_numpy() for fam, _ in ts.tables], axis=1)
    base = np.array([off // VB for off in ts.offsets], dtype=np.int64)
    o.blk = np.where(G >= 0, base[None, :] + G, -1).astype(np.int64)
    o.VB = VB
    o.vb_start = list(ts.vb_start[0])
    o.var_names = list(vs0)
    o.nbins = [len(ts.bins[x][0]) for x in vs0]
    o.tok2bin = [ts.bins[x][1] for x in vs0]
    o.fill = list(vs0)
    o.has_truth = False
    o.ref_kwargs = dict(fill=o.fill, as_source="SCF", decode_year=AF.PUF_YEAR)
    o.group_cols = {"wave": "year"}
    inc_col = [c for c in o.recs.columns if c == f"g_{set_name}_inc"]
    if inc_col:
        o.group_cols["inc"] = inc_col[0]

    def contrib_index(tokens, recs):
        tokens = np.asarray(tokens)
        if tokens.ndim == 2:
            tokens = tokens[:, None, :]
        Gr = np.stack([recs[f"g_{set_name}_{fam.name}"].to_numpy() for fam, _ in ts.tables], axis=1)
        idx = ts.contrib_index(Gr, AF.View(len(recs)), tokens, o.fill).astype(np.int64)
        idx[idx == ts.n_cells] = -1
        return idx
    o.contrib_index = contrib_index
    return o


def nan_report(T):
    """Count of cells with no target (NaN): left unconstrained."""
    return int(np.isnan(T).sum())
