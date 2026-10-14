"""Test 3, piece F: the PUF arms PS0 and PS1 (SPEC.md, contract change of
the main session, 9 October 20:30).

PS0: each PUF training record keeps its demographics (age range where the
PUF has it, married, dependents); the seven incomes are drawn with the PUF
source tag, then the 24 wealth variables with the SCF tag, year 2015
throughout. K = 16 draws per record (memory: 2.6 million draws x 334 cells),
each record keeping its weight. Where the age range is missing (about 42% of
PUF records) it stays unobserved: the slot is shown to the network as
masked at every step and is never drawn (it is not in the fill order).

PS1: whole records from nothing but year 2015: age range, married,
dependents and the seven incomes with the PUF tag, then wealth with the SCF
tag. N synthetic records = a multiple of the PUF training count, equal
starting weights summing to the PUF total weight.

Targets (one wave, 2015):
  - incomes x families: the PUF training records' own tables (income coarse
    bins by income group, age, married, dependents and the other incomes'
    bands), from their true tokens and weights;
  - wealth x families: arm P's targets: the SCF share of each bin within the
    group, interpolated s_2015 = s_2013/3 + 2 s_2016/3, times the PUF
    group's weight. Computed here from A's SCF targets; checked against
    $P/a_puf/ when it exists. Two sets: full (all 11 families) and core
    (wealth crossed with income group, age, married and dependents only;
    incomes keep all families);
  - PS1 also: age x married x dependents (PUF records with a known age,
    scaled to the total weight) and the total weight.
Age family: over PUF records with a known age. In PS0 the draws of records
without an age fall in no age cell (their entries point at one placeholder
cell that is kept out of the solve). In PS1 every draw has an age, so the
age-family targets (and the joint demographic table) are the known-age
tables scaled up by total weight / known-age weight.

    python f_puf.py targets
    python f_puf.py draw ps0 | draw ps1 --part 1 --mult 4
    python f_puf.py calib ps0 --set core full
    python f_puf.py calib ps1 --mult 4 --set core full
"""
import argparse
import json
import os
import sys
import time

import numpy as np
import pandas as pd

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, HERE)
import f_common as F                     # noqa: E402
from f_common import A, WEALTH, INCOMES, WAVES, FAMILIES, VT, SHARED_CAT, VARS, log  # noqa: E402
from c_common import wrank               # noqa: E402

PF = str(F.PF)
YEAR = 2015
SEEDS = {"ps0": 20261301, "ps1_p1": 20261311, "ps1_p2": 20261312, "dollars": 20261321}
PS1_PARTS = {1: 4, 2: 4}
A13, A16 = 1.0 / 3.0, 2.0 / 3.0
CORE = ["inc", "age", "mar", "dep"]


def jdump(obj, path):
    with open(path, "w") as fh:
        json.dump(obj, fh, indent=1, default=lambda x: x.item() if hasattr(x, "item") else str(x))


# --------------------------------------------------------------------------
# PUF records
# --------------------------------------------------------------------------

def puf_records():
    m = A.meta()
    names = [v["name"] for v in m["variables"]]
    rec = pd.read_parquet(A.DATA / "records_train.parquet")
    tok = np.load(A.DATA / "tokens_train.npy").astype(np.int64)
    src = rec["source"].astype(str).to_numpy()
    keep = np.flatnonzero(src == "PUF")
    yrs = set(rec["year"].to_numpy()[keep].astype(int).tolist())
    if yrs != {YEAR}:
        raise ValueError(f"PUF years {yrs}")
    out = pd.DataFrame({"rec": np.arange(len(keep)), "row": keep})
    out["weight"] = rec["weight"].to_numpy(float)[keep]
    if np.isnan(out["weight"]).any() or (out["weight"] <= 0).any():
        raise ValueError("PUF weights missing or not positive")
    for v in SHARED_CAT + INCOMES:
        out["tok_" + v] = tok[keep, names.index(v)]
    for v in INCOMES:
        out["d_" + v] = rec[v].to_numpy(float)[keep]
    nan = {v: int(out["d_" + v].isna().sum()) for v in INCOMES if out["d_" + v].isna().any()}
    if nan:
        raise ValueError(f"PUF incomes missing: {nan}")
    miss = {v: int((out["tok_" + v] < 0).sum()) for v in SHARED_CAT + INCOMES}
    if any(miss[v] for v in ["married", "n_dep"] + INCOMES):
        raise ValueError(f"PUF tokens missing: {miss}")
    out["age_known"] = out["tok_age_range"] >= 0
    out["inc_total"] = out[["d_" + v for v in INCOMES]].sum(axis=1)
    out["inc_rank"] = wrank(out["inc_total"].to_numpy(), out["weight"].to_numpy())
    out["g_inc"] = np.searchsorted(np.array(A.INC_EDGES), out["inc_rank"].to_numpy(), side="right")
    out["wave_i"] = 0
    return out, miss


def puf_thresholds(recs):
    tot, g = recs["inc_total"].to_numpy(), recs["g_inc"].to_numpy()
    return np.array([[tot[g >= k + 1].min() for k in range(len(A.INC_EDGES))]])


# --------------------------------------------------------------------------
# Layout: F's layout restricted to one wave (block numbers of wave 2013 are
# reused as the 2015 blocks), plus a placeholder cell for unknown age
# --------------------------------------------------------------------------

class PLayout:
    def __init__(self, arm):
        self.arm = arm
        self.FL = F.FLayout("s0" if arm == "ps0" else "s1")
        c = self.FL.cells
        keep = np.flatnonzero(c["wave_i"].to_numpy() == 0)
        self.remap = np.full(self.FL.n_cells + 1, -1, dtype=np.int64)
        self.remap[keep] = np.arange(len(keep))
        cells = c.iloc[keep].reset_index(drop=True).copy()
        cells["cell"] = np.arange(len(cells))
        cells["wave"] = YEAR
        self.placeholder = None
        if arm == "ps0":
            self.placeholder = len(cells)
            ph = pd.DataFrame([{"cell": len(cells), "block": -1, "wave": YEAR, "wave_i": 0, "family": "age",
                                "group": -1, "group_label": "age unknown (placeholder)", "variable": "-",
                                "var_kind": "placeholder", "bin": 0, "bin_label": "-", "a_cell": -1}])
            cells = pd.concat([cells, ph], ignore_index=True)
        self.cells = cells
        self.n_cells = len(cells)
        self.age_cols = np.flatnonzero(self.FL.pf == FAMILIES.index("age"))

    def index(self, vt, dm, g):
        dm = np.asarray(dm, dtype=np.int64).copy()
        unknown = dm[:, 0] < 0
        if unknown.any() and self.arm != "ps0":
            raise ValueError("unknown age in a drawn record")
        dm[unknown, 0] = 0
        idx = self.FL.index(np.zeros(len(vt), dtype=np.int64), vt, dm, g)
        out = self.remap[idx]
        if (out < 0).any():
            raise ValueError("cell outside wave 0")
        if unknown.any():
            out[np.ix_(np.flatnonzero(unknown), self.age_cols)] = self.placeholder
        return out.astype(np.int32)


# --------------------------------------------------------------------------
# Targets
# --------------------------------------------------------------------------

def scf_shares():
    """SCF bin shares within (family, group, variable), per wave, from A's
    targets. Returns DataFrame keyed by family, group, variable, bin with
    s2013, s2016, group weights."""
    t = A.load_targets()
    t["gw"] = t.groupby(["wave", "family", "group", "variable"])["target"].transform("sum")
    t["s"] = np.where(t["gw"] > 0, t["target"] / t["gw"].where(t["gw"] > 0, 1), np.nan)
    p = t.pivot_table(index=["family", "group", "variable", "bin"], columns="wave", values=["s", "gw"])
    out = pd.DataFrame({"s2013": p[("s", 2013)], "s2016": p[("s", 2016)],
                        "gw2013": p[("gw", 2013)], "gw2016": p[("gw", 2016)]}).reset_index()
    return out


def build_targets(arm, recs, PL):
    """Targets for both sets; returns DataFrame with target_full and
    target_core (NaN = not a target in that set) and notes."""
    notes = {}
    W = recs["weight"].to_numpy(float)
    known = recs["age_known"].to_numpy()
    vt = np.stack([recs["tok_" + v].to_numpy() for v in VT if v in INCOMES], 1)
    tab = PL.cells.copy()
    # PUF truth for income cells (wealth tokens unknown: use zeros, then
    # keep only income cells, demographic and total cells)
    vt_full = np.zeros((len(recs), len(VT)), dtype=np.int64)
    zero_tok = {v: int(np.flatnonzero(np.array([t["kind"] for t in A.vmeta(v)["tokens"]]) == "zero")[0]) for v in WEALTH}
    for j, v in enumerate(VT):
        vt_full[:, j] = recs["tok_" + v].to_numpy() if v in INCOMES else zero_tok[v]
    dm = np.stack([recs["tok_" + v].to_numpy() for v in SHARED_CAT], 1)
    g = recs["g_inc"].to_numpy()
    if arm == "ps0":
        idx = PL.index(vt_full, dm, g)
        tot = np.bincount(idx.reshape(-1), weights=np.repeat(W, idx.shape[1]), minlength=PL.n_cells)
    else:
        # known-age records give the age family and the joint demographic
        # table, scaled to the total weight; all records give the rest
        idx_k = PL.index(vt_full[known], dm[known], g[known])
        tot_k = np.bincount(idx_k.reshape(-1), weights=np.repeat(W[known], idx_k.shape[1]), minlength=PL.n_cells)
        dm0 = dm.copy()
        dm0[~known, 0] = 0
        idx_a = PL.index(vt_full, dm0, g)
        tot = np.bincount(idx_a.reshape(-1), weights=np.repeat(W, idx_a.shape[1]), minlength=PL.n_cells)
        scale = W.sum() / W[known].sum()
        notes["age_scale_total_over_known"] = float(scale)
        agefam = (tab["family"] == "age").to_numpy() | (tab["family"] == "demo").to_numpy()
        tot[agefam] = tot_k[agefam] * scale
    kind = tab["var_kind"].to_numpy()
    t_inc = np.where(np.isin(kind, ["income", "demo", "total"]), tot, np.nan)
    # wealth cells: SCF shares x PUF group weight (group weight = PUF
    # weight of the group; age family over known ages, PS1 scaled)
    sh = scf_shares()
    w = tab[kind == "wealth"][["cell", "family", "group", "variable", "bin"]].merge(
        sh, on=["family", "group", "variable", "bin"], how="left")
    s13, s16 = w["s2013"].to_numpy(), w["s2016"].to_numpy()
    s = A13 * s13 + A16 * s16
    only13 = np.isnan(s16) & ~np.isnan(s13)
    only16 = np.isnan(s13) & ~np.isnan(s16)
    s = np.where(only13, s13, np.where(only16, s16, s))
    notes["wealth_groups_in_one_scf_wave_only"] = int(only13.sum() + only16.sum()) // 1
    nogroup = np.isnan(s)
    notes["wealth_cells_without_scf_group"] = int(nogroup.sum())
    # PUF group weight per (family, group): from the first wealth variable's
    # cells of the PUF truth tables (zero bin holds every record there)
    gw = {}
    for f in FAMILIES:
        fi = FAMILIES.index(f)
        if f == "age":
            for a in range(len(A.layout().groups["age"])):
                m = known & (recs["tok_age_range"].to_numpy() == a)
                gw[(f, a)] = W[m].sum() * (notes.get("age_scale_total_over_known", 1.0) if arm == "ps1" else 1.0)
            continue
        if f == "inc":
            gg = g
        elif f in ("mar", "dep"):
            gg = recs["tok_" + {"mar": "married", "dep": "n_dep"}[f]].to_numpy()
        else:
            v = f[2:]
            gg = A.income_bins()[v][1][recs["tok_" + v].to_numpy()]
        for k in range(len(A.layout().groups[f])):
            gw[(f, k)] = W[gg == k].sum()
    pg = np.array([gw[(f, k)] for f, k in zip(w["family"], w["group"])])
    tw = np.where(nogroup, 0.0, s) * pg
    notes["wealth_cells_without_scf_group_puf_weight"] = float(pg[nogroup].sum())
    t_w = np.full(len(tab), np.nan)
    t_w[w["cell"].to_numpy()] = tw
    full = np.where(kind == "wealth", t_w, t_inc)
    core = full.copy()
    core[(kind == "wealth") & ~tab["family"].isin(CORE).to_numpy()] = np.nan
    if PL.placeholder is not None:
        full[PL.placeholder] = core[PL.placeholder] = np.nan
    tab["target_full"], tab["target_core"] = full, core
    tab["scf_s2013"] = np.nan
    tab.loc[kind == "wealth", "scf_s2013"] = s13
    tab.loc[kind == "wealth", "scf_s2016"] = s16
    notes["n_cells"] = int(len(tab))
    notes["zero_cells_full"] = int((full == 0).sum())
    return tab, notes


def targets_stage():
    recs, miss = puf_records()
    os.makedirs(os.path.join(PF, "ps0"), exist_ok=True)
    os.makedirs(os.path.join(PF, "ps1"), exist_ok=True)
    recs.to_parquet(os.path.join(PF, "ps0", "records.parquet"))
    out = {"n_records": len(recs), "total_weight": float(recs["weight"].sum()),
           "age_unknown_records": int((~recs["age_known"]).sum()),
           "age_unknown_weight_share": float(recs.loc[~recs["age_known"], "weight"].sum() / recs["weight"].sum()),
           "thresholds": puf_thresholds(recs).tolist()}
    for arm in ("ps0", "ps1"):
        PL = PLayout(arm)
        tab, notes = build_targets(arm, recs, PL)
        tab.to_parquet(os.path.join(PF, arm, "targets.parquet"))
        out[arm] = notes
        # checks: within every (family, group, variable) of a set the bins
        # sum to the group weight; every family sums to the total
        for col in ("target_full", "target_core"):
            d = tab[tab[col].notna() & tab["var_kind"].isin(["income", "wealth"])]
            fs = d.groupby(["family", "variable"])[col].sum()
            out[arm][f"{col}_family_sum_rel_range"] = [float(fs.min() / recs["weight"].sum() - 1),
                                                       float(fs.max() / recs["weight"].sum() - 1)]
            out[arm][f"{col}_n_targets"] = int(tab[col].notna().sum())
    # compare with piece A's arm P targets when present
    ap = os.path.join(A.P, "a_puf")
    out["a_puf_present"] = os.path.isdir(ap)
    log(json.dumps(out, indent=1, default=str))
    jdump(out, os.path.join(PF, "ps0", "targets_notes.json"))


# --------------------------------------------------------------------------
# Draws
# --------------------------------------------------------------------------

def set_units_puf(G, arm, recs=None, n=None):
    import torch
    from b_complete import allowed_code_tables
    import a_tokenizer
    V = len(G.snames)
    hide_scf = np.array([v.get("group") == "scf_only" for v in G.svars])
    if arm == "ps0":
        tok_all = np.load(A.DATA / "tokens_train.npy")
        tok_in = tok_all[recs["row"].to_numpy()][:, G.scols].astype(np.int64)
        hide = hide_scf | np.isin(G.snames, INCOMES)
        tok_in[:, hide] = -1
        unobs = (tok_in < 0).any(axis=0) & ~hide
        notes = {"unobserved_not_drawn": [G.snames[j] for j in np.flatnonzero(unobs)]}
        n = len(tok_in)
    else:
        tok_in = np.full((n, V), -1, dtype=np.int64)
        hide = np.ones(V, dtype=bool)
        notes = {}
    order = [j for j in G.full_order if hide[j]]
    names = [G.snames[j] for j in order]
    vars_arm = VARS["s0" if arm == "ps0" else "s1"]
    if sorted(names) != sorted(vars_arm):
        raise ValueError(f"{arm}: order {names}")
    G.arm = "s0" if arm == "ps0" else "s1"
    G.order, G.order_names = order, names
    G.col = [vars_arm.index(G.snames[j]) for j in order]
    puf, scf = G.sources.index("PUF"), G.sources.index("SCF")
    G.src_col = [scf if v in WEALTH else puf for v in vars_arm]
    m = A.meta()
    keys = pd.DataFrame({"source": ["PUF", "SCF"], "year": [YEAR, YEAR]})
    tab, gidx, an = allowed_code_tables(a_tokenizer, m, A.DATA, keys, None, G.svars, G.max_v)
    al = np.where(hide_scf[:, None], tab[gidx[1]], tab[gidx[0]])[None]          # [1, V, max_v]
    G.allowed_np = al
    G.allowed = torch.as_tensor(al, device=G.device)
    notes["allowed_notes"] = an
    G.tok_in = torch.as_tensor(tok_in, device=G.device)
    G.wave_i_np = np.zeros(n, dtype=np.int64)
    G.wave_i = torch.zeros(n, dtype=torch.int64, device=G.device)
    G.src = torch.full((n,), scf, dtype=torch.int64, device=G.device)
    G.year = torch.full((n,), float(YEAR), dtype=torch.float32, device=G.device)
    G.n_units = n
    return notes


def decode_puf(tok, names, seed):
    """Incomes and demographics decoded as PUF 2015; wealth as SCF at 2015
    (the 2013 and 2016 quantile functions interpolated, as in test 2)."""
    import a_tokenizer
    m = A.meta()
    allnames = [v["name"] for v in m["variables"]]
    q = pd.read_parquet(A.DATA / "quantiles.parquet")
    cache = a_tokenizer._qf_cache(q)
    rng = np.random.default_rng(seed)
    out = {}
    t = np.asarray(tok, dtype=np.int64)
    for src, vs in (("PUF", [v for v in names if v not in WEALTH]), ("SCF", [v for v in names if v in WEALTH])):
        res = []
        for s0 in range(0, len(t), 400_000):
            full = np.full((len(t[s0:s0 + 400_000]), len(allnames)), -1, dtype=np.int64)
            for v in vs:
                full[:, allnames.index(v)] = t[s0:s0 + 400_000, names.index(v)]
            r = pd.DataFrame({"source": src, "year": np.full(len(full), YEAR)})
            res.append(a_tokenizer.decode(full, m, r, q, rng, year=None, cache=cache)[vs])
        res = pd.concat(res, ignore_index=True)
        for v in vs:
            out[v] = res[v].to_numpy(float)
    d = np.column_stack([out[v] for v in names])
    if np.isnan(d).any():
        raise ValueError(f"NaN dollars: {np.isnan(d).sum(axis=0)}")
    return d


def draw(arm, part=None, mult=None, k=16, batch_size=16384):
    import torch
    recs = pd.read_parquet(os.path.join(PF, "ps0", "records.parquet"))
    model, G = F.load_model("cuda")
    t0 = time.time()
    if arm == "ps0":
        notes = set_units_puf(G, "ps0", recs)
        seed = SEEDS["ps0"]
        unit = np.repeat(np.arange(G.n_units), k)
        fn = os.path.join(PF, "ps0", "draws.npz")
    else:
        n = int(mult * len(recs))
        notes = set_units_puf(G, "ps1", n=n)
        seed = SEEDS[f"ps1_p{part}"]
        unit = np.arange(n)
        fn = os.path.join(PF, "ps1", f"draws_p{part}.npz")
    log(f"{arm}: order {G.order_names}; {notes}")
    gen = torch.Generator(device=G.device)
    gen.manual_seed(seed)
    tok, lpv = F.sample(model, G, unit, gen, batch_size=batch_size)
    secs = time.time() - t0
    log(f"{arm}: {len(tok):,} draws in {secs:.1f}s")
    names = list(VARS[G.arm])
    dol = decode_puf(tok, names, SEEDS["dollars"] + (part or 0))
    shape = (G.n_units, k) if arm == "ps0" else (len(tok),)
    np.savez(fn, tokens=tok.reshape(*shape, -1), logp_var=lpv.reshape(*shape, -1),
             logp=lpv.astype(np.float64).sum(-1).astype(np.float32).reshape(shape),
             dollars=dol.reshape(*shape, -1), vars=np.array(names), seed=np.int64(seed),
             seconds=np.float64(secs))
    jdump(notes, fn.replace(".npz", "_notes.json"))
    log(f"wrote {fn}")


def load_ps1(mult, n_puf):
    out, have = None, 0
    for p, m in PS1_PARTS.items():
        if have >= mult:
            break
        z = np.load(os.path.join(PF, "ps1", f"draws_p{p}.npz"))
        z = {k: z[k] for k in ("tokens", "logp", "dollars")}
        out = z if out is None else {k: np.concatenate([out[k], z[k]]) for k in out}
        have += m
    n = int(mult * n_puf)
    if len(out["tokens"]) < n:
        raise ValueError(f"{len(out['tokens'])} draws, {n} needed")
    return {k: v[:n] for k, v in out.items()}


# --------------------------------------------------------------------------
# Calibration
# --------------------------------------------------------------------------

def arrays(arm, z, recs, thr):
    names = list(VARS["s0" if arm == "ps0" else "s1"])
    tok = z["tokens"].reshape(-1, len(names)).astype(np.int64)
    dol = z["dollars"].reshape(-1, len(names))
    vt = tok[:, [names.index(v) for v in VT]]
    if arm == "ps0":
        K = z["tokens"].shape[1]
        dm = np.repeat(np.stack([recs["tok_" + v].to_numpy() for v in SHARED_CAT], 1), K, axis=0)
    else:
        dm = tok[:, [names.index(v) for v in SHARED_CAT]]
    tot = dol[:, [names.index(v) for v in INCOMES]].sum(1)
    g = np.searchsorted(thr[0], tot, side="right")
    return vt, dm.astype(np.int64), tot, g


def calib(arm, sets, mults=None):
    import b_calibrate as B
    from f_run import infeasible_table, kl_parts
    recs = pd.read_parquet(os.path.join(PF, "ps0", "records.parquet"))
    thr = puf_thresholds(recs)
    tab = pd.read_parquet(os.path.join(PF, arm, "targets.parquet"))
    PL = PLayout(arm)
    W = recs["weight"].to_numpy(float)
    runs = [None] if arm == "ps0" else mults
    for mult in runs:
        if arm == "ps0":
            z = np.load(os.path.join(PF, "ps0", "draws.npz"))
            z = {k: z[k] for k in ("tokens", "dollars")}
            N, K = z["tokens"].shape[:2]
        else:
            z = load_ps1(mult, len(recs))
            N, K = 1, len(z["tokens"])
        vt, dm, tot, g = arrays(arm, z, recs, thr)
        idx = PL.index(vt, dm, g)
        log(f"{arm} {mult}: index {idx.shape}")
        Wc = W if arm == "ps0" else np.array([W.sum()])
        for s in sets:
            t = tab[f"target_{s}"].to_numpy()
            use = ~np.isnan(t)
            # cells not in this set: compact them away; draws' entries in
            # them point at one extra column that is unconstrained
            remap = np.full(len(t) + 1, -1, dtype=np.int64)
            remap[np.flatnonzero(use)] = np.arange(use.sum())
            spare = int(use.sum())
            ii = remap[idx]
            ii[ii < 0] = spare
            tt = np.concatenate([t[use], [0.0]])
            meta = pd.concat([tab[use], pd.DataFrame([{"family": "(not targeted)", "variable": "-",
                                                       "group": -1, "wave": YEAR, "var_kind": "spare"}])],
                             ignore_index=True)
            # the spare column: target set above any reach so the presolve
            # leaves it out of the solve ("above_reach"); reported apart
            tt[spare] = 1e3 * W.sum() * idx.shape[1]
            w, lam, rep = B.calibrate_matrix(ii.astype(np.int32), Wc, tt, K, cell_meta=meta.drop(columns=["bin"]))
            st = np.array(rep.pop("cell_status"))
            rep.pop("reach_any", None), rep.pop("forced", None)
            st_t = st[:-1]
            rep["status_counts_targets"] = {x: int((st_t == x).sum()) for x in B.STATUSES}
            rep["spare_column_status"] = str(st[-1])
            rep["infeasible_cells_by_family_and_kind"] = infeasible_table(tab[use].reset_index(drop=True), st_t)
            rep["arm"], rep["set"], rep["mult"], rep["K"] = arm, s, mult, int(K)
            if arm == "ps1":
                q = w.reshape(-1) / W.sum()
                rep["ess"] = float(1 / (q ** 2).sum())
                rep["ess_share_of_draws"] = rep["ess"] / K
                rep["ess_over_puf_records"] = rep["ess"] / len(recs)
                demo = (np.maximum(dm[:, 0], 0) * 2 + dm[:, 1]) * 4 + dm[:, 2]
                rep["kl_between_demographic_cells"] = kl_parts(w, Wc, K, demo[None, :])
                rep["kl_between_demo_x_income_group"] = kl_parts(w, Wc, K, (demo * 5 + g)[None, :])
            else:
                rep["kl_between_income_groups"] = kl_parts(w, Wc, K, g.reshape(N, K))
                rep["share_draws_income_group_differs"] = float((g.reshape(N, K) != recs["g_inc"].to_numpy()[:, None]).mean())
            name = s if arm == "ps0" else f"{s}_m{mult}"
            np.savez(os.path.join(PF, arm, f"weights_{name}.npz"), w=w, **{"lambda": lam[:-1]},
                     cell_status=st_t.astype(str), inc_group=g.reshape(N, K).astype(np.int8))
            jdump(rep, os.path.join(PF, arm, f"report_{name}.json"))
            B.print_summary(f"{arm} {name}", rep)
    open(os.path.join(PF, f"DONE_{arm}"), "w").write(time.strftime("%Y-%m-%d %H:%M:%S\n"))


def calib_ps1s_pilot(sets, ddir):
    """Code test of f_full's oversampled PS1 on the pilot model, against this
    file's PS1 targets (pilot: SCF shares and PUF income tables)."""
    import f_full
    recs = pd.read_parquet(os.path.join(PF, "ps0", "records.parquet"))
    thr = puf_thresholds(recs)
    tab = pd.read_parquet(os.path.join(PF, "ps1", "targets.parquet"))
    PL = PLayout("ps1")
    z = np.load(os.path.join(ddir, "ps1s", "draws_p1.npz"))
    names = [str(x) for x in z["slot_names"]]
    tok = z["tokens"].astype(np.int64)
    dol = z["dollars"]
    vt = tok[:, [names.index(v) for v in VT]]
    dm = tok[:, [names.index(v) for v in SHARED_CAT]]
    g = np.searchsorted(thr[0], dol[:, [names.index(v) for v in INCOMES]].sum(1), side="right")
    if not np.array_equal(g, z["stratum"]):
        raise ValueError("strata differ from income groups recomputed from dollars")
    idx = PL.index(vt, dm, g)
    Wg = np.bincount(recs["g_inc"].to_numpy(), weights=recs["weight"].to_numpy(), minlength=5)
    if not np.allclose(Wg, z["group_target_weight"]):
        raise ValueError("group weights differ")
    for s in sets:
        t = tab[f"target_{s}"].to_numpy()
        use = ~np.isnan(t)
        remap = np.full(len(t), -1, dtype=np.int64)
        remap[np.flatnonzero(use)] = np.arange(use.sum())
        ii = remap[idx]
        wd, lam, st, rep = f_full.calib_strata(ii, z["stratum"].astype(np.int64), t[use], Wg, z["pool_counts"],
                                               tab[use].reset_index(drop=True))
        from f_run import infeasible_table
        rep["infeasible_cells_by_family_and_kind"] = infeasible_table(tab[use].reset_index(drop=True), st)
        rep.update({"arm": "ps1s_pilot", "set": s, "pool_counts": z["pool_counts"].tolist(),
                    "kept_counts": z["kept_counts"].tolist()})
        np.savez(os.path.join(ddir, "ps1s", f"weights_{s}.npz"), w=wd, cell_status=st.astype(str))
        jdump(rep, os.path.join(ddir, "ps1s", f"report_{s}.json"))
        log(f"pilot ps1s {s}: KL {rep['kl_from_model_total']:.4f} (between {rep['kl_between_income_groups']:.4f}), "
            f"ESS {rep['ess_final']:,.0f} of {rep['draws']:,}, statuses {rep['status_counts_targets']}")


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("stage")
    ap.add_argument("arm", nargs="?")
    ap.add_argument("--part", type=int)
    ap.add_argument("--mult", type=int, nargs="+")
    ap.add_argument("--set", nargs="+", default=["core", "full"])
    ap.add_argument("--k", type=int, default=16)
    a = ap.parse_args()
    if a.stage == "targets":
        targets_stage()
    elif a.stage == "draw":
        draw(a.arm, part=a.part, mult=(a.mult or [None])[0], k=a.k)
    elif a.stage == "calib":
        calib(a.arm, a.set, a.mult)
    elif a.stage == "pilot_ps1s":
        calib_ps1s_pilot(a.set, "/nfs/roberts/scratch/pi_nrs36/jar335/posttrain/dev/f_full")
    else:
        raise SystemExit(a.stage)


if __name__ == "__main__":
    main()
