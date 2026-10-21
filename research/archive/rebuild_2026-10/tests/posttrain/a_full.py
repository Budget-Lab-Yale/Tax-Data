"""Test 3, piece A, full model: target sets for PUF records (SPEC.md,
"Main session, 9 October 20:45", item 4).

A target set is a list of tables. A table is one *family* (a way of putting
a record in a group) crossed with a list of *variables*; each variable's
values are put in coarse bins (amounts: 'neg', 'zero' and the twelve
positive rank bands of a_common.py; categoricals: one bin per category).
A cell is (table, group, variable, bin); its target is a population weight.
Every draw touches at most one cell per (table, variable): none when its
group is undefined (for example age unknown) or the variable is unobserved.

Target sets, all on PUF training records (2015, own weights):

  puf    PUF ground truth: each PUF variable's coarse bins crossed with AGI
         group (E00100 weighted rank: <50, 50-90, 90-99, 99-99.9, top 0.1),
         filing status, age range and number of dependents; plus two-way
         crosses among the main income items (each item's bins by each other
         item's own band: zero, negative, positive rank [0,.5), [.5,.9),
         [.9,1]). Targets are the PUF records' own weighted counts.
  wcore  SCF wealth carried to the PUF as shares within groups: each wealth
         variable's coarse bins by income group, age range, married, number
         of dependents. s_2013 and s_2016 are the SCF training records'
         (wealth observed) weight share of the bin within the group in each
         wave, s_2015 = s_2013 / 3 + 2 s_2016 / 3, target = s_2015 x the PUF
         group's weight. Groups on each side from that side's own values.
  wfull  wcore plus each wealth variable by the band of each income item that
         both sources measure.

Usage (CPU; dataset from a_common.configure / configure_full):
    python a_full.py build [--pilot]     -> records_puf.parquet, targets_<set>.parquet,
                                            scf_shares.parquet, cells_full.json, DONE_targets
    python a_full.py draws --k 32        -> ref_draws_P.npz, ref_dollars_P.parquet, DONE_ref_P (GPU)
--pilot runs the same code on test 2's expB dataset and current checkpoint
(the seven summed incomes stand in for the PUF variables), writing to
$P/full/a/pilot_expB/, to test the code before dataset `full` exists.

Python use:
    import a_common as A, a_full as AF
    A.configure_full()                      # or AF.configure_pilot()
    spec = AF.Spec()                        # family and table definitions for the dataset
    ts = AF.load_set("wcore")               # TargetSet with .cells (targets) and layout
    recs = AF.load_puf_records()
    v = AF.truth_view(recs)                 # values of the PUF records
    G = ts.groups(v)                        # [N, n_tables] group per table (-1: none)
    idx = ts.contrib_index(G, v, wealth_tokens)   # [N*K, width] cell ids, ts.n_cells = none
"""

import argparse
import json
import os
import sys
import time
from pathlib import Path

import numpy as np
import pandas as pd

HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(HERE))
import a_common as A                                      # noqa: E402
from a_common import WEALTH, log                          # noqa: E402
from c_common import wrank, INCOMES                       # noqa: E402

RANK_EDGES = [0.5, 0.9, 0.99, 0.999]
RANK_LABELS = ["<50", "50-90", "90-99", "99-99.9", "top0.1"]
PUF_YEAR = 2015
SEEDS = dict(draws_P=20261121, decode_P=20261122)

# Test 1's PUF variables (S006, the weight, is not a model variable).
PUF_VARS = ["MARS", "DSI", "XTOT", "N24", "EIC", "FDED", "AGERANGE", "GENDER", "EARNSPLIT",
            "E00100", "E00200", "E00900", "E26270", "P23250", "P22250", "E00600", "E00650",
            "E00300", "E01500", "E01700", "E01400", "E02400", "E25850", "E25860", "E02100",
            "E00400", "E26390", "E26400", "E01100", "E02300", "E03150", "E03210", "E03270",
            "E17500", "E18400", "E18500", "E19200", "E19800", "E20100", "E02000"]
# Main income items for the two-way crosses of the PUF ground-truth set.
MAIN_INCOME = ["E00200", "E00900", "E26270", "P23250", "P22250", "E00600", "E00300",
               "E01500", "E02400", "E25850", "E02000"]
# Income items whose sum ranks units into income groups for the wealth tables
# when the SCF does not measure AGI (E00100): the AGI-type items both sources
# measure (tax-exempt interest, qualified dividends, total pensions, losses
# reported as positive amounts and deductions are left out).
MEASURE_ITEMS = ["E00200", "E00900", "E26270", "P23250", "P22250", "E00600", "E00300",
                 "E01700", "E01400", "E02400", "E25850", "E02100", "E26390", "E01100",
                 "E02300", "E02000"]


# ---------------------------------------------------------------------------
# Views: values of a set of rows (records or draws)
# ---------------------------------------------------------------------------

class View:
    """tok[name]: int token array (-1 unobserved); val[name]: dollars for
    amounts, category value for categoricals (NaN unobserved)."""

    def __init__(self, n):
        self.n = n
        self.tok = {}
        self.val = {}


def _cat_label(t):
    return str(t.get("meaning", t.get("value")))


def _cat_values(name):
    vm = A.vmeta(name)
    return np.array([float(t["value"]) for t in vm["tokens"]])


def view_from_tokens(tok_full, names, dollars=None):
    """View from a [n, len(names)] token array (and optional dollars for the
    amounts, same layout; NaN where not decoded)."""
    v = View(tok_full.shape[0])
    for j, nm in enumerate(names):
        t = np.asarray(tok_full[:, j], dtype=np.int64)
        v.tok[nm] = t
        vm = A.vmeta(nm)
        if vm["kind"] == "categorical":
            cv = _cat_values(nm)
            v.val[nm] = np.where(t >= 0, cv[np.maximum(t, 0)], np.nan)
        elif dollars is not None:
            v.val[nm] = np.where(t >= 0, dollars[:, j], np.nan)
    return v


def truth_view(recs, split="train"):
    """View of the records' own values: tokens from tokens_<split>.npy (truth
    where the token file hides an observed value), dollars from the records
    file."""
    m = A.meta()
    names = [x["name"] for x in m["variables"]]
    tok = np.load(A.DATA / f"tokens_{split}.npy")[recs["row"].to_numpy()].astype(np.int64)
    tf = A.DATA / f"truth_{split}.npy"
    if tf.exists():
        tr = np.load(tf)[recs["row"].to_numpy()].astype(np.int64)
        tok = np.where(tok >= 0, tok, tr)
    raw = pd.read_parquet(A.DATA / f"records_{split}.parquet")
    keep = [j for j, x in enumerate(m["variables"]) if x.get("group") != "context"]
    names = [names[j] for j in keep]
    tok = tok[:, keep]
    dol = np.full(tok.shape, np.nan)
    for j, nm in enumerate(names):
        if nm in raw.columns and A.vmeta(nm)["kind"] == "amount":
            dol[:, j] = raw[nm].to_numpy(dtype=float)[recs["row"].to_numpy()]
    v = view_from_tokens(tok, names, dol)
    # an amount observed as a token must have dollars
    for nm in names:
        if A.vmeta(nm)["kind"] == "amount" and nm in v.val:
            miss = (v.tok[nm] >= 0) & np.isnan(v.val[nm])
            if miss.any():
                raise ValueError(f"{nm}: {miss.sum()} rows with a token but no dollars")
    return v


# ---------------------------------------------------------------------------
# Families
# ---------------------------------------------------------------------------

class Family:
    """kind 'rank': weighted rank of a dollar measure (a variable or a sum)
    within the population, edges RANK_EDGES; thresholds are fitted on the
    population's own records (fit) and then applied to any view, so draws
    are grouped against the truth's thresholds. kind 'cat': a categorical's
    token, optionally restricted/merged by `mapping` (token value -> group).
    kind 'band': an amount's own band (neg, zero, p00_50, p50_90, p90_100)
    from its token. kind 'fn': fn(view) -> group array, with labels."""

    def __init__(self, name, kind, var=None, labels=None, mapping=None, fn=None, measure=None):
        self.name, self.kind, self.var = name, kind, var
        self.labels, self.mapping, self.fn, self.measure = labels, mapping, fn, measure
        self.thr = None
        if kind == "cat":
            vals = _cat_values(var)
            if mapping is None:
                self.labels = [_cat_label(t) for t in A.vmeta(var)["tokens"]]
                self.tok2g = np.arange(len(vals))
            else:
                self.tok2g = np.array([mapping.get(float(x), -1) for x in vals])
        elif kind == "band":
            self.labels, self.tok2g = A._coarse(A.vmeta(var), A.INCOME_POS_EDGES, A.INCOME_POS_LABELS)
        elif kind == "rank":
            self.labels = list(RANK_LABELS)

    def _measure(self, v):
        names = self.measure if isinstance(self.measure, list) else [self.measure]
        x = np.zeros(v.n)
        for nm in names:
            x = x + v.val[nm]
        return x                       # NaN if any component unobserved

    def fit(self, v, w):
        """Fit rank thresholds on a population view with weights w. Returns
        the groups of the population (exact weighted mid-rank groups)."""
        if self.kind != "rank":
            return self(v)
        x = self._measure(v)
        ok = ~np.isnan(x)
        g = np.full(v.n, -1, dtype=np.int64)
        r = wrank(x[ok], w[ok])
        g[ok] = np.searchsorted(np.array(RANK_EDGES), r, side="right")
        # threshold k = smallest value with group >= k (groups are monotone in x)
        self.thr = np.array([x[ok][g[ok] >= k].min() if (g[ok] >= k).any() else np.inf
                             for k in range(1, len(RANK_LABELS))])
        self.n_unobserved = int((~ok).sum())
        return g

    def __call__(self, v):
        if self.kind == "rank":
            if self.thr is None:
                raise RuntimeError(f"family {self.name}: thresholds not fitted")
            x = self._measure(v)
            g = (x[:, None] >= self.thr[None, :]).sum(axis=1)
            return np.where(np.isnan(x), -1, g).astype(np.int64)
        if self.kind in ("cat", "band"):
            t = v.tok[self.var]
            return np.where(t >= 0, self.tok2g[np.maximum(t, 0)], -1).astype(np.int64)
        return np.asarray(self.fn(v), dtype=np.int64)


def var_bins(name):
    """(labels, tok2bin) of a target variable."""
    vm = A.vmeta(name)
    if vm["kind"] == "categorical":
        return [_cat_label(t) for t in vm["tokens"]], np.arange(int(vm["vocab_size"]))
    return A._coarse(vm, A.WEALTH_POS_EDGES, A.WEALTH_POS_LABELS)


# ---------------------------------------------------------------------------
# Target sets
# ---------------------------------------------------------------------------

class TargetSet:
    """tables: list of (Family, [variables]). Cell numbering: tables in
    order; within a table, group-major, then variable, then bin."""

    def __init__(self, name, tables):
        self.name = name
        self.tables = tables
        self.vars = []                                   # all target variables, first-seen order
        for _, vs in tables:
            for x in vs:
                if x not in self.vars:
                    self.vars.append(x)
        self.bins = {x: var_bins(x) for x in self.vars}
        self.offsets, self.vb_start, self.VB = [], [], []
        off = 0
        rows = []
        for ti, (fam, vs) in enumerate(tables):
            nb = [len(self.bins[x][0]) for x in vs]
            vbs = np.concatenate([[0], np.cumsum(nb)[:-1]]).astype(np.int64)
            VB = int(sum(nb))
            self.offsets.append(off)
            self.vb_start.append(vbs)
            self.VB.append(VB)
            for g, gl in enumerate(fam.labels):
                for k, x in enumerate(vs):
                    for b, bl in enumerate(self.bins[x][0]):
                        rows.append((off + g * VB + vbs[k] + b, ti, fam.name, g, gl, x, b, bl))
            off += len(fam.labels) * VB
        self.n_cells = off
        self.layout = pd.DataFrame(rows, columns=["cell", "table", "family", "group",
                                                  "group_label", "variable", "bin", "bin_label"])
        assert (self.layout["cell"].to_numpy() == np.arange(self.n_cells)).all()
        self.width = sum(len(vs) for _, vs in tables)

    def groups(self, v):
        return np.stack([fam(v) for fam, _ in self.tables], axis=1)

    def contrib_index(self, G, v, tokens=None, tok_names=None):
        """Cell ids per row. G [N, n_tables] groups (from groups()); v: view
        of the N rows (tokens of the target variables, unless given in
        `tokens` [N, K, len(tok_names)] for drawn variables, e.g. wealth).
        Returns int32 [N*K, width] with self.n_cells where the row touches no
        cell (group undefined or variable unobserved). Rows record-major."""
        N = G.shape[0]
        K = 1 if tokens is None else tokens.shape[1]
        cols = []
        for ti, (fam, vs) in enumerate(self.tables):
            g = np.repeat(G[:, ti], K)
            for k, x in enumerate(vs):
                if tokens is not None and x in tok_names:
                    t = np.asarray(tokens[:, :, tok_names.index(x)], dtype=np.int64).reshape(-1)
                else:
                    t = np.repeat(v.tok[x], K)
                t2b = self.bins[x][1]
                b = np.where(t >= 0, t2b[np.maximum(t, 0)], -1)
                c = self.offsets[ti] + g * self.VB[ti] + self.vb_start[ti][k] + b
                cols.append(np.where((g >= 0) & (b >= 0), c, self.n_cells))
        return np.stack(cols, axis=1).astype(np.int32)

    def totals(self, idx, w):
        """A'w (float64) from contrib_index output and one weight per row."""
        tot = np.bincount(idx.reshape(-1), weights=np.repeat(np.asarray(w, float), idx.shape[1]),
                          minlength=self.n_cells + 1)
        return tot[:self.n_cells]


def contrib_csr(idx, n_cells, dtype=np.float32):
    """scipy CSR [rows, n_cells] from contrib_index output, dropping the
    'no cell' entries."""
    import scipy.sparse as sp
    R, W = idx.shape
    keep = idx < n_cells
    indptr = np.concatenate([[0], np.cumsum(keep.sum(axis=1))]).astype(np.int64)
    ind = np.sort(np.where(keep, idx, np.iinfo(np.int32).max), axis=1)
    ind = ind[ind < n_cells]
    return sp.csr_matrix((np.ones(len(ind), dtype=dtype), ind, indptr), shape=(R, n_cells))


# ---------------------------------------------------------------------------
# Dataset specifics
# ---------------------------------------------------------------------------

class Spec:
    """Families and tables for the configured dataset. For `full`: PUF
    variable names; for the expB pilot: the ten shared variables."""

    def __init__(self, pilot=False):
        m = A.meta()
        self.names = [x["name"] for x in m["variables"]]
        self.pilot = pilot
        if pilot:
            self.puf_vars = ["age_range", "married", "n_dep"] + INCOMES
            self.main_income = list(INCOMES)
            self.agi = INCOMES                       # sum of the seven incomes
            self.age, self.mars, self.dep = "age_range", "married", "n_dep"
        else:
            self.puf_vars = [x for x in PUF_VARS if x in self.names]
            missing = [x for x in PUF_VARS if x not in self.names]
            if missing:
                log(f"PUF variables not in dataset: {missing}")
            self.main_income = [x for x in MAIN_INCOME if x in self.names]
            self.agi = "E00100"
            self.age, self.mars, self.dep = "AGERANGE", "MARS", None
        self.wealth = [x for x in WEALTH if x in self.names]

    # families -------------------------------------------------------------
    def fam_agi(self, name="agi", measure=None):
        return Family(name, "rank", measure=measure or self.agi)

    def fam_age(self):
        if self.pilot:
            return Family("age", "cat", self.age)
        # AGERANGE codes 1..6 (filers who are not dependents); 0 ("not
        # present") and dependent filers' codes are no group.
        vals = _cat_values(self.age)
        mapping = {float(c): int(c) - 1 for c in range(1, 7) if float(c) in vals}
        f = Family("age", "cat", self.age, mapping=mapping,
                   labels=["under 26", "26-34", "35-44", "45-54", "55-64", "65+"])
        if "DSI" in self.names:
            base = f.__call__
            dsi_vals = _cat_values("DSI")

            def fn(v, base=base):
                g = base(v)
                t = v.tok["DSI"]
                dep_filer = (t >= 0) & (dsi_vals[np.maximum(t, 0)] == 1)
                return np.where(dep_filer, -1, g)
            return Family("age", "fn", labels=f.labels, fn=fn)
        return f

    def fam_mars(self):
        return Family("mars", "cat", self.mars)

    def fam_married(self):
        if self.pilot:
            return Family("mar", "cat", self.mars)
        return Family("mar", "cat", self.mars, mapping={1.0: 0, 2.0: 1, 3.0: 0, 4.0: 0},
                      labels=["not married filing jointly", "married filing jointly"])

    def fam_dep(self):
        if self.pilot:
            return Family("dep", "cat", self.dep)
        # dependents = exemptions (XTOT) minus filer and spouse, 0..3+
        def fn(v):
            x, ms = v.val["XTOT"], v.val["MARS"]
            d = x - 1 - (ms == 2)
            d = np.clip(d, 0, 3)
            return np.where(np.isnan(x) | np.isnan(ms), -1, d)
        return Family("dep", "fn", labels=["0", "1", "2", "3 or more"], fn=fn)

    def fam_band(self, x):
        return Family("x_" + x, "band", x)

    # sets ----------------------------------------------------------------
    def puf_set(self):
        T = [(self.fam_agi(), self.puf_vars), (self.fam_mars(), self.puf_vars),
             (self.fam_age(), self.puf_vars), (self.fam_dep(), self.puf_vars)]
        for x in self.main_income:
            T.append((self.fam_band(x), self.main_income))
        return TargetSet("puf", T)

    def wealth_set(self, full, common_income):
        T = [(self.fam_agi("inc"), self.wealth), (self.fam_age(), self.wealth),
             (self.fam_married(), self.wealth), (self.fam_dep(), self.wealth)]
        if full:
            for x in common_income:
                T.append((self.fam_band(x), self.wealth))
        return TargetSet("wfull" if full else "wcore", T)


def configure_pilot():
    A.configure(out=A.P / "full" / "a" / "pilot_expB")


# ---------------------------------------------------------------------------
# Records
# ---------------------------------------------------------------------------

def split_records(split="train"):
    rec = pd.read_parquet(A.DATA / f"records_{split}.parquet")
    out = pd.DataFrame({"row": np.arange(len(rec), dtype=np.int64),
                        "source": rec["source"].astype(str).to_numpy(),
                        "year": rec["year"].to_numpy().astype(int),
                        "weight": rec["weight"].to_numpy(dtype=float)})
    for c in ["hh_id", "implicate", "unit", "wealth_observed"]:
        if c in rec.columns:
            out[c] = rec[c].to_numpy()
    return out


def load_puf_records():
    return pd.read_parquet(A.PA / "records_puf.parquet")


def load_set(name):
    """(TargetSet layout re-created from the dataset, targets frame)."""
    return pd.read_parquet(A.PA / f"targets_{name}.parquet")


# ---------------------------------------------------------------------------
# Build
# ---------------------------------------------------------------------------

def fit_population(ts, v, w):
    """Fit rank thresholds of every rank family on this population; return
    groups [N, n_tables]."""
    cols = []
    for fam, _ in ts.tables:
        cols.append(fam.fit(v, w))
    return np.stack(cols, axis=1)


def observed_share(v, w, name):
    t = v.tok[name]
    return float(w[t >= 0].sum() / w.sum())


def build(pilot=False):
    if pilot:
        configure_pilot()
    A.PA.mkdir(parents=True, exist_ok=True)
    spec = Spec(pilot)
    allrec = split_records("train")
    notes = {}
    # NA checks on the columns used
    for c in ["source", "year", "weight"]:
        n = int(allrec[c].isna().sum())
        if n:
            raise ValueError(f"{n} NA values in records_train {c}")
    puf = allrec[(allrec.source == "PUF") & (allrec.year == PUF_YEAR)].reset_index(drop=True)
    scf = allrec[allrec.source == "SCF"].reset_index(drop=True)
    notes["puf_records"] = len(puf)
    notes["scf_rows"] = len(scf)
    if "wealth_observed" in scf.columns:
        notes["scf_wealth_observed_na"] = int(scf["wealth_observed"].isna().sum())
        if notes["scf_wealth_observed_na"]:
            raise ValueError("NA wealth_observed on SCF rows")
    vp = truth_view(puf)
    wp = puf["weight"].to_numpy()
    if (wp <= 0).any() or np.isnan(wp).any():
        raise ValueError("PUF records with NA or non-positive weight")

    # Which income items both sources measure (SCF observation share > 90%).
    vs_all = truth_view(scf)
    ws_all = scf["weight"].to_numpy()
    obs = {x: (observed_share(vp, wp, x), observed_share(vs_all, ws_all, x)) for x in spec.puf_vars}
    notes["observed_share_puf_scf"] = {k: [round(a, 4), round(b, 4)] for k, (a, b) in obs.items()}
    common = [x for x in spec.main_income if obs[x][1] > 0.9 and obs[x][0] > 0.99]
    if not pilot:
        # every amount the SCF measures counts as an income item for the x_ families
        common = [x for x in spec.puf_vars if A.vmeta(x)["kind"] == "amount"
                  and obs[x][1] > 0.9 and obs[x][0] > 0.99 and x != "E00100"]
    notes["common_income_items"] = common
    agi_measure = spec.agi
    if not pilot and obs.get("E00100", (0, 0))[1] < 0.9:
        agi_measure = [x for x in common if x in MEASURE_ITEMS]
        notes["income_group_measure_wealth_tables"] = f"sum of {agi_measure} (SCF lacks E00100)"
    else:
        notes["income_group_measure_wealth_tables"] = agi_measure

    # ---------------- PUF ground truth ----------------
    sets = {}
    ts_puf = spec.puf_set()
    G = fit_population(ts_puf, vp, wp)
    idx = ts_puf.contrib_index(G, vp)
    cells = ts_puf.layout.copy()
    cells["target"] = ts_puf.totals(idx, wp)
    cells["n_records"] = np.bincount(idx.reshape(-1), minlength=ts_puf.n_cells + 1)[:ts_puf.n_cells]
    cells["zero_cell"] = cells["target"] == 0
    sets["puf"] = (ts_puf, cells, G)

    # ---------------- wealth sets ----------------
    for full in (False, True):
        ts = spec.wealth_set(full, common)
        for fam, _ in ts.tables:
            if fam.kind == "rank":
                fam.measure = agi_measure
        # SCF side, per wave: thresholds from all SCF rows of the wave
        # (split-offs included), shares over rows with wealth observed.
        sh = {}
        for wave in A.WAVES:
            sw = scf[scf.year == wave].reset_index(drop=True)
            v = truth_view(sw)
            w = sw["weight"].to_numpy()
            Gs = fit_population(ts, v, w)
            wo = sw["wealth_observed"].fillna(False).to_numpy().astype(bool) \
                if "wealth_observed" in sw.columns else np.ones(len(sw), bool)
            sub = np.flatnonzero(wo)
            wt = np.stack([v.tok[x][sub] for x in spec.wealth], axis=1)
            if (wt < 0).any():
                raise ValueError(f"SCF {wave}: wealth-observed rows with missing wealth tokens")
            vsub = View(len(sub))
            vsub.tok = {k: a[sub] for k, a in v.tok.items()}
            vsub.val = {k: a[sub] for k, a in v.val.items()}
            idx_s = ts.contrib_index(Gs[sub], vsub, wt[:, None, :], spec.wealth)
            tot = ts.totals(idx_s, w[sub])
            n = np.bincount(idx_s.reshape(-1), minlength=ts.n_cells + 1)[:ts.n_cells]
            gt = pd.DataFrame({"cell": ts.layout.cell, "tot": tot})
            gt["key"] = list(zip(ts.layout.table, ts.layout.group, ts.layout.variable))
            gw = gt.groupby("key")["tot"].transform("sum").to_numpy()
            sh[wave] = (np.where(gw > 0, tot / np.where(gw > 0, gw, 1), np.nan), n, gw)
        # PUF side: thresholds refitted on the PUF
        for fam, _ in ts.tables:
            fam.thr = None
        Gp = fit_population(ts, vp, wp)
        cells = ts.layout.copy()
        # PUF group weight of every cell = weight of PUF records in (table, group)
        gw_p = np.zeros(ts.n_cells)
        lay = ts.layout
        for ti, (fam, vs) in enumerate(ts.tables):
            gwt = np.bincount(np.maximum(Gp[:, ti], 0), weights=wp * (Gp[:, ti] >= 0),
                              minlength=len(fam.labels))
            m = (lay.table == ti).to_numpy()
            gw_p[m] = gwt[lay.group.to_numpy()[m]]
        s13, n13, g13 = sh[2013]
        s16, n16, g16 = sh[2016]
        both = ~np.isnan(s13) & ~np.isnan(s16)
        s15 = np.where(both, s13 / 3 + 2 * s16 / 3, np.where(np.isnan(s13), s16, s13))
        cells["s_2013"], cells["s_2016"], cells["s_2015"] = s13, s16, s15
        cells["n_scf_2013"], cells["n_scf_2016"] = n13, n16
        cells["scf_group_weight_2013"], cells["scf_group_weight_2016"] = g13, g16
        cells["puf_group_weight"] = gw_p
        cells["one_wave_only"] = np.isnan(s13) ^ np.isnan(s16)
        cells["no_scf_group"] = np.isnan(s15)
        cells["target"] = np.where(np.isnan(s15), np.nan, s15 * gw_p)
        cells["zero_cell"] = cells["target"] == 0
        # PUF records per (table, group)
        npg = np.zeros(ts.n_cells, dtype=np.int64)
        for ti, (fam, vs) in enumerate(ts.tables):
            c = np.bincount(np.maximum(Gp[:, ti], 0), weights=(Gp[:, ti] >= 0).astype(float),
                            minlength=len(fam.labels))
            m = (lay.table == ti).to_numpy()
            npg[m] = c[lay.group.to_numpy()[m]].astype(np.int64)
        cells["n_puf_records_in_group"] = npg
        sets[ts.name] = (ts, cells, Gp)

    # ---------------- records file ----------------
    rp = puf.copy()
    rp.insert(0, "rec", np.arange(len(rp)))
    for nm, (ts, cells, G) in sets.items():
        for ti, (fam, _) in enumerate(ts.tables):
            col = f"g_{nm}_{fam.name}"
            rp[col] = G[:, ti]
    for x in spec.puf_vars:
        rp["tok_" + x] = vp.tok[x]
        if A.vmeta(x)["kind"] == "amount":
            rp["d_" + x] = vp.val[x]
    rp.to_parquet(A.PA / "records_puf.parquet", index=False)

    # ---------------- checks ----------------
    checks = {}
    for nm, (ts, cells, G) in sets.items():
        c = {}
        t = cells["target"].to_numpy()
        # within each (table, variable): targets over groups and bins sum to
        # the PUF weight of records with a group (and the variable observed,
        # for the PUF set)
        if nm == "puf":
            # within each (table, variable): targets over groups and bins sum
            # to the PUF weight of records with a group and the variable observed
            sums = cells.groupby(["table", "variable"])["target"].sum()
            rel = 0.0
            for ti, (fam, vs) in enumerate(ts.tables):
                has = G[:, ti] >= 0
                for x in vs:
                    e = wp[has & (vp.tok[x] >= 0)].sum()
                    rel = max(rel, abs(sums[(ti, x)] - e) / max(e, 1))
            c["family_sum_max_rel_diff"] = float(rel)
        else:
            # within each (table, group, variable) with an SCF counterpart:
            # targets over bins sum to the PUF group's weight; and each
            # table's groups add up to the PUF weight of records with a group
            ok = ~cells["no_scf_group"]
            g = cells[ok].groupby(["table", "group", "variable"]).agg(
                t=("target", "sum"), w=("puf_group_weight", "first"))
            c["group_sum_max_rel_diff"] = float(((g.t - g.w).abs() / g.w.clip(lower=1)).max())
            c["puf_weight_with_group_by_table"] = {
                ts.tables[ti][0].name: float(wp[G[:, ti] >= 0].sum()) for ti in range(len(ts.tables))}
            c["puf_weight_total"] = float(wp.sum())
        c["cells"] = int(len(cells))
        c["zero_cells"] = int(cells["zero_cell"].sum())
        c["no_scf_group_cells"] = int(cells.get("no_scf_group", pd.Series(False, index=cells.index)).sum())
        c["one_wave_only_cells"] = int(cells.get("one_wave_only", pd.Series(False, index=cells.index)).sum())
        pos = t > 0
        if nm == "puf":
            c["positive_cells_lt5_records"] = int((pos & (cells["n_records"] < 5)).sum())
        else:
            c["positive_cells_lt5_scf_records_both_waves"] = int(
                (pos & ((cells["n_scf_2013"] + cells["n_scf_2016"]) < 5)).sum())
            # interpolation check, recomputed from the stored shares
            ok = ~cells["no_scf_group"] & ~cells["one_wave_only"]
            s15 = cells["s_2013"] / 3 + 2 * cells["s_2016"] / 3
            c["interpolation_max_abs_diff"] = float((s15[ok] - cells["s_2015"][ok]).abs().max())
            c["shares_sum_to_one_max_abs_diff"] = float(
                (cells[ok].groupby(["table", "group", "variable"])["s_2015"].sum() - 1).abs().max())
        c["by_family"] = {f: dict(cells=int(len(g)), zero=int(g.zero_cell.sum()))
                          for f, g in cells.groupby("family")}
        checks[nm] = c
        cells.to_parquet(A.PA / f"targets_{nm}.parquet", index=False)
    # The PUF set reproduced by the PUF truth exactly (contribution check)
    ts, cells, G = sets["puf"]
    idx = ts.contrib_index(G, vp)
    csr = contrib_csr(idx, ts.n_cells, np.float64)
    checks["puf"]["contrib_true_max_abs_diff"] = float(np.abs(csr.T @ wp - cells["target"].to_numpy()).max())
    info = dict(dataset=str(A.DATA), pilot=pilot, notes=notes, checks=checks,
                sets={nm: dict(n_cells=ts.n_cells, width=ts.width,
                               tables=[dict(family=f.name, kind=f.kind, labels=f.labels,
                                            variables=vs, offset=int(ts.offsets[i]),
                                            VB=int(ts.VB[i]),
                                            thresholds=(f.thr.tolist() if f.thr is not None else None))
                                       for i, (f, vs) in enumerate(ts.tables)])
                      for nm, (ts, _, _) in sets.items()},
                wealth=spec.wealth, seeds=SEEDS)
    with open(A.PA / "cells_full.json", "w") as f:
        json.dump(info, f, indent=1, default=float)
    log(json.dumps(dict(notes=notes, checks=checks), indent=1, default=float))
    with open(A.PA / "DONE_targets", "w") as f:
        f.write(time.strftime("%Y-%m-%d %H:%M:%S") + f" job {os.environ.get('SLURM_JOB_ID')}\n")
    return sets


def rebuild_set(name, pilot=False):
    """Re-create a TargetSet (layout and fitted thresholds) from cells_full.json
    for use on draws."""
    spec = Spec(pilot)
    with open(A.PA / "cells_full.json") as f:
        info = json.load(f)
    common = info["notes"]["common_income_items"]
    ts = spec.puf_set() if name == "puf" else spec.wealth_set(name == "wfull", common)
    for (fam, _), t in zip(ts.tables, info["sets"][name]["tables"]):
        assert fam.name == t["family"]
        if fam.kind == "rank":
            fam.thr = np.array(t["thresholds"])
            if name != "puf":
                fam.measure = info["notes"]["income_group_measure_wealth_tables"]
    return ts


# ---------------------------------------------------------------------------
# Reference draws for arm P
# ---------------------------------------------------------------------------

def draws_P(K, pilot=False):
    import torch
    if pilot:
        configure_pilot()
    recs = load_puf_records()
    model, ctx = A.load_reference("cuda", recs, fill=Spec(pilot).wealth, as_source="SCF",
                                  decode_year=PUF_YEAR)
    log(f"order {ctx.order_names}; unobserved and not drawn: {ctx.unfilled_hidden}; "
        f"GPU {torch.cuda.get_device_name(0)}")
    d = A.draw_set(model, ctx, K, SEEDS["draws_P"])
    if np.isinf(d["logp_ref_var"]).any():
        raise RuntimeError("draws with log-probability -inf")
    sub = np.arange(0, ctx.N, 97)
    lp_tf = A.ref_logprob(model, ctx, sub, d["tokens"][sub, 0].astype(np.int64))
    chk = dict(sample_vs_teacher_forced_max_abs=float(np.abs(lp_tf - d["logp_ref_var"][sub, 0]).max()),
               unobserved_not_drawn=ctx.unfilled_hidden, order=ctx.order_names)
    np.savez(A.PA / "ref_draws_P.npz", **d)
    dol = A.decode_dollars(d["tokens"], recs, SEEDS["decode_P"], names=ctx.fill, year=PUF_YEAR)
    N = len(recs)
    df = pd.DataFrame({"rec": np.repeat(np.arange(N), K).astype(np.int32),
                       "draw": np.tile(np.arange(K), N).astype(np.int16)})
    flat = dol.reshape(N * K, len(ctx.fill))
    for k, x in enumerate(ctx.fill):
        df[x] = flat[:, k]
    df["nw"] = A.nw_from(flat) if ctx.fill == WEALTH else np.nan
    df.to_parquet(A.PA / "ref_dollars_P.parquet", index=False)
    # coverage of the wealth target sets by the draws
    vp = truth_view(recs)
    for nm in ("wcore", "wfull"):
        ts = rebuild_set(nm, pilot)
        cells = pd.read_parquet(A.PA / f"targets_{nm}.parquet")
        G = np.stack([recs[f"g_{nm}_{fam.name}"].to_numpy() for fam, _ in ts.tables], axis=1)
        tot = np.zeros(ts.n_cells)
        reach = np.zeros(ts.n_cells, dtype=np.int64)
        W = recs["weight"].to_numpy()
        for s0 in range(0, N, 20000):                    # in chunks of records (memory)
            sl = slice(s0, s0 + 20000)
            vs = View(len(recs.iloc[sl]))
            vs.tok = {k: a[sl] for k, a in vp.tok.items()}
            vs.val = {k: a[sl] for k, a in vp.val.items()}
            idx = ts.contrib_index(G[sl], vs, d["tokens"][sl], ctx.fill)
            tot += ts.totals(idx, np.repeat(W[sl] / K, K))
            reach += np.bincount(idx.reshape(-1), minlength=ts.n_cells + 1)[:ts.n_cells]
        t = cells["target"].to_numpy()
        pos = t > 0
        grp = cells["puf_group_weight"].to_numpy()
        err = np.abs(tot - np.nan_to_num(t)) / np.where(grp > 0, grp, np.nan)
        chk[nm] = dict(positive_cells_reached_by_no_draw=int((pos & (reach == 0)).sum()),
                       positive_cells_reached_by_fewer_than_5_draws=int((pos & (reach < 5)).sum()),
                       draw_weight_share_in_zero_cells=float(tot[t == 0].sum() / tot.sum()),
                       mean_abs_err_share_by_family={f: float(np.nanmean(err[(cells.family == f).to_numpy()]))
                                                     for f in cells.family.unique()})
    with open(A.PA / "ref_P_checks.json", "w") as f:
        json.dump(chk, f, indent=1)
    log(json.dumps(chk, indent=1))
    with open(A.PA / "DONE_ref_P", "w") as f:
        f.write(json.dumps(dict(K=K, seed=SEEDS["draws_P"], decode_seed=SEEDS["decode_P"],
                                checkpoint=str(A.CKPT), job=os.environ.get("SLURM_JOB_ID"),
                                time=time.strftime("%Y-%m-%d %H:%M:%S"))) + "\n")


if __name__ == "__main__":
    p = argparse.ArgumentParser()
    p.add_argument("stage", choices=["build", "draws"])
    p.add_argument("--pilot", action="store_true")
    p.add_argument("--k", type=int, default=32)
    p.add_argument("--ckpt", default=None)
    p.add_argument("--dataset", default="full", help="full or full_v2")
    a = p.parse_args()
    if not a.pilot:
        A.configure_full(a.dataset, a.ckpt)
    if a.stage == "build":
        build(a.pilot)
    else:
        draws_P(a.k, a.pilot)
