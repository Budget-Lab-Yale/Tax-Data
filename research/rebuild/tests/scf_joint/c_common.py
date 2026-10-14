"""Part C shared helpers: reading a dataset in the SPEC.md layout, weighted
statistics, and the comparison of one set of wealth records with another.

Every statistic is weighted by the records' population weights. A record
with several completion draws counts each draw at 1 / (number of draws).
"""
import json
import os
import re
import time

import numpy as np
import pandas as pd

S = "/nfs/roberts/scratch/pi_nrs36/jar335/scf_joint"

ASSETS = ['cash', 'equities', 'bonds', 'dc', 'db', 'life_ins', 'annuities',
          'trusts', 'other_fin', 'pass_throughs', 'primary_home', 'other_home',
          're_fund', 'other_nonfin']
DEBTS = ['primary_mortgage', 'other_mortgage', 'credit_lines', 'credit_cards',
         'installment_debt', 'other_debt']
KG = ['kg_primary_home', 'kg_other_re', 'kg_pass_throughs', 'kg_other']
WEALTH = ASSETS + DEBTS + KG
INCOMES = ['wages', 'business', 'int_div', 'capital_gains', 'rent', 'ss_pens',
           'ui_other']
SHARED_CAT = ['age_range', 'married', 'n_dep']

# Income groups: weighted rank of the total of the seven incomes, within
# source and year.
INC_EDGES = [0.5, 0.9, 0.99]
INC_LABELS = ['Bottom 50%', '50-90%', '90-99%', 'Top 1%']

QS = [0.10, 0.25, 0.50, 0.75, 0.90]
MIN_CELL = 30          # fewest unweighted holder records for a quantile cell
MIN_CORR = 200         # fewest unweighted joint holders for a correlation

# Conditional holding rates shown individually: (holds X, given holds Y).
# Y may be a shared income variable ("income nonzero").
COND_PAIRS = [
    ('primary_home', 'equities'),
    ('primary_mortgage', 'primary_home'),
    ('kg_primary_home', 'primary_home'),
    ('other_home', 'primary_home'),
    ('equities', 'dc'),
    ('bonds', 'equities'),
    ('credit_cards', 'primary_home'),
    ('pass_throughs', 'business'),
    ('equities', 'int_div'),
    ('kg_other', 'capital_gains'),
    ('other_home', 'rent'),
    ('db', 'ss_pens'),
]

T0 = time.time()


def log(msg):
    print(f"[{time.time() - T0:7.1f}s] {msg}", flush=True)


# --------------------------------------------------------------------------
# Reading the dataset
# --------------------------------------------------------------------------

def _get(d, keys, default=None):
    for k in keys:
        if k in d:
            return d[k]
    return default


def token_info(vm):
    """For one variable's metadata, arrays over tokens: is_zero, is_neg, and
    a signed rank score (positive tokens: midpoint of the rank band among
    positive values, in (0, 1]; negative tokens: -1 + midpoint of the band
    among negative values, so more negative values score closer to -1)."""
    toks = vm.get("tokens")
    V = int(vm["vocab_size"])
    is_zero = np.zeros(V, bool)
    is_neg = np.zeros(V, bool)
    score = np.full(V, np.nan)
    if isinstance(toks, dict):          # {"0": {...}, ...}
        toks = [dict(t, token=int(k)) if "token" not in t else t for k, t in toks.items()]
    for i, t in enumerate(toks or []):
        k = int(_get(t, ["token", "id", "index"], i))
        kind = t.get("kind")
        z = bool(_get(t, ["zero", "is_zero"], False)) or kind == "zero"
        n = bool(_get(t, ["negative", "is_negative", "neg"], False)) or kind == "neg"
        lo = _get(t, ["lo", "rank_lo"])
        hi = _get(t, ["hi", "rank_hi"])
        is_zero[k], is_neg[k] = z, n
        if z:
            score[k] = 0.0
        elif lo is not None and hi is not None:
            mid = (float(lo) + float(hi)) / 2
            score[k] = (-1 + mid) if n else mid
    return {"is_zero": is_zero, "is_neg": is_neg, "score": score}


class Dataset:
    def __init__(self, name, split="test", root=None):
        self.name = name
        self.dir = os.path.join(root or os.path.join(S, "data"), name)
        self.meta = json.load(open(os.path.join(self.dir, "meta.json")))
        self.vars = [v["name"] for v in self.meta["variables"]]
        self.vmeta = {v["name"]: v for v in self.meta["variables"]}
        self.split = split
        self.truth = np.load(os.path.join(self.dir, f"truth_{split}.npy"))
        self.records = pd.read_parquet(os.path.join(self.dir, f"records_{split}.parquet"))
        assert len(self.truth) == len(self.records), "truth and records differ in length"
        self.tinfo = {v: token_info(self.vmeta[v]) for v in WEALTH + INCOMES if v in self.vmeta}
        missing = [v for v in WEALTH + INCOMES + SHARED_CAT if v not in self.vmeta]
        if missing:
            raise SystemExit(f"meta.json lacks variables: {missing}")

    def col(self, v):
        return self.truth[:, self.vars.index(v)]

    def age_labels(self):
        vm = self.vmeta["age_range"]
        toks = vm.get("tokens") or []
        out = {}
        for i, t in enumerate(toks):
            k = int(_get(t, ["token", "id", "index"], i))
            out[k] = str(_get(t, ["label", "value", "category"], k))
        return out


def income_groups(df):
    """Weighted rank of the seven-income total within each source-year."""
    tot = df[INCOMES].to_numpy().sum(axis=1)
    g = np.zeros(len(df), dtype=np.int8)
    for key, idx in df.groupby(["source", "year"]).indices.items():
        r = wrank(tot[idx], df["weight"].to_numpy()[idx])
        g[idx] = np.searchsorted(np.array(INC_EDGES), r, side="right")
    return g


def truth_frame(ds, keep):
    """The true records (rows `keep` of the split) as an evaluation frame."""
    rec = ds.records
    F = pd.DataFrame({"row": np.arange(len(rec))})
    F["source"] = rec["source"].to_numpy()
    F["year"] = rec["year"].to_numpy()
    F["hh"] = rec["year"].astype(np.int64).to_numpy() * 10**9 + rec["hh_id"].astype(np.int64).to_numpy()
    F["w"] = rec["weight"].to_numpy().astype(float)
    F["inc_group"] = income_groups(rec)
    F["inc_total"] = rec[INCOMES].to_numpy().sum(axis=1)
    for v in SHARED_CAT + INCOMES:
        F["t_" + v] = ds.col(v)
    for v in INCOMES:
        F["d_" + v] = rec[v].to_numpy()
    for v in WEALTH:
        F["t_" + v] = ds.col(v)
        F["d_" + v] = rec[v].to_numpy().astype(float)
    keep = np.asarray(keep, bool).copy()
    n_unobs = 0
    if "wealth_observed" in rec.columns:
        wo = rec["wealth_observed"].fillna(False).to_numpy().astype(bool)
        n_unobs = int((keep & ~wo).sum())
        w_unobs = float(rec["weight"].to_numpy()[keep & ~wo].sum())
        if n_unobs:
            log(f"  {n_unobs:,} records excluded from grading: wealth unobserved (split-off SCF "
                f"units, no true wealth); their weight {w_unobs:,.0f} of "
                f"{rec['weight'].to_numpy()[keep].sum():,.0f}")
        keep &= wo
    F = F.loc[keep].reset_index(drop=True)
    F.attrs["n_excluded_wealth_unobserved"] = n_unobs
    bad = {v: int((F["t_" + v] < 0).sum()) for v in WEALTH}
    if any(bad.values()):
        raise SystemExit(f"true wealth tokens missing (-1) in the evaluation set: {bad}")
    nan = {v: int(F["d_" + v].isna().sum()) for v in WEALTH if F["d_" + v].isna().any()}
    if nan:
        raise SystemExit(f"true wealth dollars missing in the evaluation set: {nan}")
    F["draw"] = 0
    F["ndraw"] = 1
    return add_nw(F)


def add_nw(F):
    F["nw"] = F[["d_" + v for v in ASSETS]].sum(axis=1) - F[["d_" + v for v in DEBTS]].sum(axis=1)
    return F


def completed_frame(ds, truth, path, encode=None):
    """A completed file joined to the true frame's shared variables. The
    completed frame keeps the true weight, income group and shared
    variables of each record and replaces the 24 wealth amounts."""
    C = pd.read_parquet(path)
    n0 = len(C)
    if "source" in C.columns:
        puf = (C["source"] == "PUF").to_numpy()
        if puf.any():
            log(f"  {puf.sum():,} PUF rows in {os.path.basename(path)} set aside "
                "(evaluated by c_evaluate_puf.py)")
            C = C.loc[~puf].reset_index(drop=True)
    if "row" in C.columns:
        rows = C["row"].to_numpy().astype(np.int64)
    else:
        keys = ["source", "year", "hh_id", "implicate", "unit"]
        rk = ds.records[keys].copy()
        rk["row"] = np.arange(len(rk))
        if rk.duplicated(keys).any():
            raise SystemExit("records keys are not unique; completed file needs a `row` column")
        m = C[keys].merge(rk, on=keys, how="left")
        if m["row"].isna().any():
            raise SystemExit(f"{int(m['row'].isna().sum())} completed rows do not match a record")
        rows = m["row"].to_numpy().astype(np.int64)
    pos = pd.Series(np.arange(len(truth)), index=truth["row"].to_numpy())
    miss = ~np.isin(rows, truth["row"].to_numpy())
    if miss.any():
        unobs = 0
        if "wealth_observed" in ds.records.columns:
            unobs = int((~ds.records["wealth_observed"].fillna(False).to_numpy().astype(bool)[rows[miss]]).sum())
        if unobs != miss.sum():
            raise SystemExit(f"{miss.sum()} completed rows are not in the evaluation set "
                             f"({unobs} of them have wealth unobserved)")
        log(f"  {miss.sum():,} completed rows set aside: records with wealth unobserved (no truth to grade)")
        C = C.loc[~miss].reset_index(drop=True)
        rows = rows[~miss]
    idx = pos.loc[rows].to_numpy()
    if "draw" not in C.columns:
        C["draw"] = 0
    F = truth.iloc[idx].reset_index(drop=True).copy()
    F["draw"] = C["draw"].to_numpy()
    nd = pd.Series(F["draw"]).groupby(F["row"]).transform("nunique").to_numpy()
    F["ndraw"] = nd
    F["w"] = F["w"] / nd
    covered = np.isin(truth["row"].to_numpy(), rows)
    if not covered.all():
        log(f"  WARNING: {int((~covered).sum()):,} of {len(truth):,} evaluation records "
            "have no completion; the truth is restricted to the completed records")
    nan_d = [v for v in WEALTH if v not in C.columns or C[v].isna().any()]
    if nan_d:
        if not all("tok_" + v in C.columns for v in WEALTH):
            raise SystemExit(f"completed dollars missing for {nan_d} and no tokens to decode")
        log(f"  completed dollars missing for {len(nan_d)} wealth variables; decoding the "
            "completed tokens with a_tokenizer.decode (seed 20261009)")
        dec = decode_wealth(ds, C, seed=20261009)
        for v in WEALTH:
            C[v] = dec[v].to_numpy()
        C.attrs["decoded_by_C"] = {"neg_fallback": dec.attrs.get("neg_fallback", {})}
    for v in WEALTH:
        d = C[v].to_numpy().astype(float)
        if np.isnan(d).any():
            raise SystemExit(f"completed dollars missing for {v}: {int(np.isnan(d).sum())}")
        F["d_" + v] = d
        if "tok_" + v in C.columns:
            F["t_" + v] = C["tok_" + v].to_numpy()
        elif encode is not None:
            F["t_" + v] = encode(v, d, F)
        else:
            raise SystemExit(f"completed file lacks tok_{v} and no encoder is available")
        if (F["t_" + v] < 0).any():
            raise SystemExit(f"completed tokens negative (missing) for {v}")
    log(f"  completed file: {n0:,} rows, {F['row'].nunique():,} records, "
        f"draws per record {sorted(set(nd.tolist()))}")
    F = add_nw(F)
    F.attrs["decoded_by_C"] = C.attrs.get("decoded_by_C")
    return F, covered


def decode_wealth(ds, C, seed, year=None):
    """Decode the completed wealth tokens of C (tok_<variable> columns) into
    dollars with piece A's tokenizer and the dataset's quantile functions.

    A completed negative token for a source-year whose SCF wave has no
    negative values of that component (seen: primary_home and pass_throughs
    in 2016) has no quantile function to decode it; those draws are decoded
    with the nearest SCF wave that has negatives, and counted (returned in
    the frame's attrs['neg_fallback'])."""
    import importlib.util
    here = os.path.dirname(os.path.abspath(__file__))
    spec = importlib.util.spec_from_file_location("a_tokenizer", os.path.join(here, "a_tokenizer.py"))
    tk = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(tk)
    q = pd.read_parquet(os.path.join(ds.dir, "quantiles.parquet"))
    cache = tk._qf_cache(q)
    rng = np.random.default_rng(seed)
    out = pd.DataFrame(index=range(len(C)))
    fallback = {}
    yr = C["year"].to_numpy().astype(int) if year is None else np.full(len(C), int(year))
    for v in WEALTH:
        k = ds.vars.index(v)
        tok_v = C["tok_" + v].to_numpy().astype(np.int64)
        kinds = np.array([tt.get("kind", "") for tt in ds.vmeta[v]["tokens"]])
        negyears = sorted({key[1] for key in cache if key[0] == "SCF" and key[2] == v and key[3] == "neg"})
        waves = sorted({key[1] for key in cache if key[0] == "SCF" and key[2] == v})

        def needs(y):
            # the SCF waves a.decode reads for year y (its own, or the two around it)
            if y in waves or y < waves[0] or y > waves[-1]:
                return [min(waves, key=lambda n: abs(n - y))]
            return [max(n for n in waves if n < y), min(n for n in waves if n > y)]
        ok_year = {y: all(n in negyears for n in needs(y)) for y in np.unique(yr)}
        bad = (kinds[tok_v] == "neg") & ~np.array([ok_year[y] for y in yr], bool)
        res = np.full(len(C), np.nan)
        for sel, yy in ((~bad, year), (bad, None)):
            if not sel.any():
                continue
            tok = np.full((sel.sum(), len(ds.vars)), -1, dtype=np.int64)
            tok[:, k] = tok_v[sel]
            rec = C.loc[sel, ["source", "year"]].reset_index(drop=True).copy()
            if yy is None and bad[sel].all():
                if not negyears:
                    raise SystemExit(f"negative tokens for {v} but no SCF wave has negatives")
                rec["year"] = [min(negyears, key=lambda n: abs(n - y)) for y in yr[sel]]
                fallback[v] = int(sel.sum())
            d = tk.decode(tok, ds.meta, rec, q, rng, year=yy, cache=cache)
            res[sel] = d[v].to_numpy()
        out[v] = res
    if fallback:
        log(f"  negative draws with no negative quantile function in their wave, decoded with the "
            f"nearest wave that has negatives: {fallback}")
    out.attrs["neg_fallback"] = fallback
    return out


# --------------------------------------------------------------------------
# Weighted statistics
# --------------------------------------------------------------------------

def wquantile(x, w, qs):
    order = np.argsort(x, kind="mergesort")
    xs, cw = x[order], np.cumsum(w[order])
    cw = cw / cw[-1]
    idx = np.searchsorted(cw, np.asarray(qs), side="left")
    return xs[np.minimum(idx, len(xs) - 1)]


def wrank(x, w):
    order = np.argsort(x, kind="mergesort")
    xs, ws = x[order], w[order] / w.sum()
    uniq, start = np.unique(xs, return_index=True)
    block_w = np.add.reduceat(ws, start)
    below = np.concatenate([[0.0], np.cumsum(block_w)[:-1]])
    mid = below + block_w / 2
    r = np.empty(len(x))
    r[order] = np.repeat(mid, np.diff(np.append(start, len(xs))))
    return r


def wcorr(x, y, w):
    w = w / w.sum()
    mx, my = (w * x).sum(), (w * y).sum()
    vx, vy = (w * (x - mx) ** 2).sum(), (w * (y - my) ** 2).sum()
    if vx <= 0 or vy <= 0:
        return np.nan
    return float((w * (x - mx) * (y - my)).sum() / np.sqrt(vx * vy))


def wspearman(x, y, w):
    return wcorr(wrank(x, w), wrank(y, w), w)


def top_shares(x, w, tops=(0.10, 0.01, 0.001)):
    order = np.argsort(-x, kind="mergesort")
    xs, ws = x[order], w[order] / w.sum()
    cw = np.cumsum(ws)
    tot = (xs * ws).sum()
    out = {}
    for t in tops:
        k = np.searchsorted(cw, t, side="left")
        # include the fraction of the boundary record needed to reach t
        part = (xs[:k] * ws[:k]).sum()
        if k < len(xs):
            prev = cw[k - 1] if k > 0 else 0.0
            part += xs[k] * (t - prev)
        out[t] = float(part / tot)
    return out


def top_mask(x, w, t):
    """Records in the weighted top share t by x."""
    order = np.argsort(-x, kind="mergesort")
    cw = np.cumsum(w[order]) / w.sum()
    k = np.searchsorted(cw, t, side="left") + 1
    m = np.zeros(len(x), bool)
    m[order[:k]] = True
    return m


# --------------------------------------------------------------------------
# Statistics of one frame
# --------------------------------------------------------------------------

def frame_stats(F, ds_tinfo, age_keys):
    """Every statistic of one set of records, as plain tables. Comparisons
    are differences of these."""
    w = F["w"].to_numpy()
    w = w / w.sum()
    H = np.column_stack([F["d_" + v].to_numpy() != 0 for v in WEALTH])
    # holding defined on tokens too; the two must agree (zero token = $0)
    Ht = np.column_stack([ds_tinfo[v]["is_zero"][F["t_" + v].to_numpy()] == False  # noqa: E712
                          for v in WEALTH])
    disagree = (H != Ht).mean(axis=0)
    st = {"n_rows": int(len(F)), "hold_token_dollar_disagree":
          {v: float(d) for v, d in zip(WEALTH, disagree) if d > 0}}
    inc = F["inc_group"].to_numpy()
    age = F["t_age_range"].to_numpy()
    groups = {"all": np.zeros(len(F), int), "inc": inc, "age": age}
    glabels = {"all": [0], "inc": list(range(4)), "age": age_keys}

    # 1. holding shares by group
    hold = {}
    neg = {}
    for gname, g in groups.items():
        for k in glabels[gname]:
            m = g == k
            sw = w[m].sum()
            hold[(gname, k)] = (w[m] @ H[m]) / sw if sw > 0 else np.full(len(WEALTH), np.nan)
            Nm = np.column_stack([F["d_" + v].to_numpy()[m] < 0 for v in WEALTH])
            neg[(gname, k)] = (w[m] @ Nm) / sw if sw > 0 else np.full(len(WEALTH), np.nan)
    st["hold"] = hold
    st["neg"] = neg

    # 2. amount quantiles among positive holders, dollars and rank scores
    qd, qr, ncell = {}, {}, {}
    for gname, g in groups.items():
        for k in glabels[gname]:
            m0 = g == k
            for j, v in enumerate(WEALTH):
                d = F["d_" + v].to_numpy()
                m = m0 & (d > 0)
                ncell[(gname, k, v)] = int(m.sum())
                if m.sum() == 0:
                    continue
                qd[(gname, k, v)] = wquantile(d[m], w[m], QS)
                sc = ds_tinfo[v]["score"][F["t_" + v].to_numpy()[m]]
                qr[(gname, k, v)] = wquantile(sc, w[m], QS)
    st["qd"], st["qr"], st["ncell"] = qd, qr, ncell
    # the top of each component's distribution, in rank: share of positive
    # holders whose token lies in the top 1% (0.1%) rank band of positives
    tb = {}
    for v in WEALTH:
        d = F["d_" + v].to_numpy()
        m = d > 0
        sc = ds_tinfo[v]["score"][F["t_" + v].to_numpy()[m]]
        sw = w[m].sum()
        tb[v] = (float((w[m] * (sc > 0.99)).sum() / sw) if sw > 0 else np.nan,
                 float((w[m] * (sc > 0.999)).sum() / sw) if sw > 0 else np.nan,
                 # unweighted: the share among sample records, which shows how
                 # strongly the SCF's sample over-represents the top
                 float((sc > 0.99).mean()) if m.any() else np.nan,
                 float((sc > 0.999).mean()) if m.any() else np.nan)
    st["top_band"] = tb

    # 3. joint holding
    bits = (H.astype(np.int64) << np.arange(len(WEALTH), dtype=np.int64)).sum(axis=1)
    pat = pd.Series(w).groupby(bits).sum()
    st["patterns"] = pat
    st["n_patterns"] = int(len(pat))
    p1 = w @ H
    M11 = (H * w[:, None]).T @ H
    st["p1"], st["M11"] = p1, M11
    cond = {}
    for x, y in COND_PAIRS:
        hx = H[:, WEALTH.index(x)]
        hy = H[:, WEALTH.index(y)] if y in WEALTH else (F["d_" + y].to_numpy() != 0)
        cond[(x, y)] = float((w * hx * hy).sum() / (w * hy).sum()) if (w * hy).sum() > 0 else np.nan
    st["cond"] = cond

    # 4. rank correlations among holders: amount with income; pairs
    inc_tot = F["inc_total"].to_numpy()
    icorr, ni = {}, {}
    for j, v in enumerate(WEALTH):
        m = H[:, j]
        ni[v] = int(m.sum())
        if m.sum() >= MIN_CORR:
            icorr[v] = wspearman(F["d_" + v].to_numpy()[m], inc_tot[m], w[m])
    st["icorr"], st["icorr_n"] = icorr, ni
    pcorr, pn = {}, {}
    for a in range(len(WEALTH)):
        for b in range(a + 1, len(WEALTH)):
            m = H[:, a] & H[:, b]
            pn[(a, b)] = int(m.sum())
            if m.sum() >= MIN_CORR:
                pcorr[(a, b)] = wspearman(F["d_" + WEALTH[a]].to_numpy()[m],
                                          F["d_" + WEALTH[b]].to_numpy()[m], w[m])
    st["pcorr"], st["pcorr_n"] = pcorr, pn

    # 5. net worth
    nw = F["nw"].to_numpy()
    st["nw_q"] = dict(zip([0.10, 0.25, 0.50, 0.75, 0.90, 0.99],
                          wquantile(nw, w, [0.10, 0.25, 0.50, 0.75, 0.90, 0.99])))
    st["nw_mean"] = float((w * nw).sum())
    st["nw_top"] = top_shares(nw, w)
    nwg = {}
    for k in range(4):
        m = inc == k
        nwg[k] = {"mean": float((w[m] * nw[m]).sum() / w[m].sum()),
                  "median": float(wquantile(nw[m], w[m], [0.5])[0])}
    st["nw_inc"] = nwg

    # 6. cloning at the top: records in the top 1% by net worth whose
    # combination of 24 wealth tokens also appears, in the top 1%, for a
    # record of a different household.
    tm = top_mask(nw, w, 0.01)
    T = np.column_stack([F["t_" + v].to_numpy() for v in WEALTH]).astype(np.int32)
    keys = pd.Series([r.tobytes() for r in T[tm]])
    hh = F["hh"].to_numpy()[tm]
    nh = pd.DataFrame({"k": keys, "h": hh}).groupby("k")["h"].nunique()
    rep = keys.map(nh).to_numpy() >= 2
    wt = w[tm]
    st["clone_share"] = float((wt * rep).sum() / wt.sum())
    st["clone_max_households"] = int(nh.max()) if len(nh) else 0
    st["top1_records"] = int(tm.sum())
    return st


# --------------------------------------------------------------------------
# Comparing two frames
# --------------------------------------------------------------------------

def tvd_series(a, b):
    u = a.index.union(b.index)
    return 0.5 * float(np.abs(a.reindex(u, fill_value=0).to_numpy() -
                              b.reindex(u, fill_value=0).to_numpy()).sum())


def compare_stats(A, B):
    """Gaps of B (candidate) against A (reference). Returns (headline dict,
    detail tables dict)."""
    out, det = {}, {}
    # holding shares
    rows = []
    for (g, k), ha in A["hold"].items():
        hb = B["hold"][(g, k)]
        for j, v in enumerate(WEALTH):
            rows.append({"by": g, "group": k, "variable": v, "ref": ha[j], "cand": hb[j],
                         "gap": hb[j] - ha[j],
                         "ref_neg": A["neg"][(g, k)][j], "cand_neg": B["neg"][(g, k)][j]})
    hold = pd.DataFrame(rows)
    det["hold"] = hold
    for g in ["all", "inc", "age"]:
        x = hold.loc[hold["by"] == g, "gap"].abs()
        out[f"hold_{g}_mae_pp"] = 100 * float(x.mean())
        out[f"hold_{g}_max_pp"] = 100 * float(x.max())
    # amount quantiles
    rows = []
    for key, qa in A["qd"].items():
        if key not in B["qd"]:
            continue
        if A["ncell"][key] < MIN_CELL or B["ncell"][key] < MIN_CELL:
            continue
        g, k, v = key
        qb = B["qd"][key]
        ra, rb = A["qr"][key], B["qr"][key]
        for i, q in enumerate(QS):
            rows.append({"by": g, "group": k, "variable": v, "q": q,
                         "ref_dollars": qa[i], "cand_dollars": qb[i],
                         "log_gap": np.log(qb[i] / qa[i]),
                         "ref_rank": ra[i], "cand_rank": rb[i], "rank_gap": rb[i] - ra[i]})
    amt = pd.DataFrame(rows)
    det["amounts"] = amt
    for g in ["all", "inc", "age"]:
        sub = amt[amt["by"] == g]
        out[f"amt_{g}_rank_mae_pctile"] = 100 * float(sub["rank_gap"].abs().mean())
        out[f"amt_{g}_dollar_mae_pct"] = 100 * float(np.exp(sub["log_gap"].abs().mean()) - 1)
        out[f"amt_{g}_cells"] = int(sub[["group", "variable"]].drop_duplicates().shape[0])
    tb = pd.DataFrame([{"variable": v, "ref_top1": A["top_band"][v][0], "cand_top1": B["top_band"][v][0],
                        "ref_top01": A["top_band"][v][1], "cand_top01": B["top_band"][v][1],
                        "ref_top1_unweighted": A["top_band"][v][2],
                        "ref_top01_unweighted": A["top_band"][v][3]} for v in WEALTH])
    det["top_band"] = tb
    ok = tb["ref_top1"] > 0
    out["topband1_ratio_mean"] = float((tb.loc[ok, "cand_top1"] / tb.loc[ok, "ref_top1"]).mean())
    ok = tb["ref_top01"] > 0
    out["topband01_ratio_mean"] = float((tb.loc[ok, "cand_top01"] / tb.loc[ok, "ref_top01"]).mean())
    # joint holding
    out["pattern_tvd"] = tvd_series(A["patterns"], B["patterns"])
    out["n_patterns_ref"], out["n_patterns_cand"] = A["n_patterns"], B["n_patterns"]
    tv = []
    pa, pb = A["p1"], B["p1"]
    for a in range(len(WEALTH)):
        for b in range(a + 1, len(WEALTH)):
            ca = np.array([A["M11"][a, b], pa[a] - A["M11"][a, b], pa[b] - A["M11"][a, b],
                           1 - pa[a] - pa[b] + A["M11"][a, b]])
            cb = np.array([B["M11"][a, b], pb[a] - B["M11"][a, b], pb[b] - B["M11"][a, b],
                           1 - pb[a] - pb[b] + B["M11"][a, b]])
            tv.append({"a": WEALTH[a], "b": WEALTH[b], "tvd": 0.5 * np.abs(ca - cb).sum(),
                       "ref_both": ca[0], "cand_both": cb[0]})
    tv = pd.DataFrame(tv)
    det["pairs2x2"] = tv
    out["pair2x2_tvd_mean"] = float(tv["tvd"].mean())
    out["pair2x2_tvd_max"] = float(tv["tvd"].max())
    out["pair2x2_tvd_max_pair"] = f"{tv.loc[tv['tvd'].idxmax(), 'a']} x {tv.loc[tv['tvd'].idxmax(), 'b']}"
    cond = pd.DataFrame([{"holds": x, "given": y, "ref": A["cond"][(x, y)],
                          "cand": B["cond"][(x, y)]} for x, y in COND_PAIRS])
    cond["gap"] = cond["cand"] - cond["ref"]
    det["cond"] = cond
    out["cond_mae_pp"] = 100 * float(cond["gap"].abs().mean())
    # correlations
    ic = pd.DataFrame([{"variable": v, "ref": A["icorr"].get(v, np.nan),
                        "cand": B["icorr"].get(v, np.nan), "n_ref": A["icorr_n"][v]}
                       for v in WEALTH])
    ic["gap"] = ic["cand"] - ic["ref"]
    det["income_corr"] = ic
    out["income_corr_mae"] = float(ic["gap"].abs().mean())
    out["income_corr_n"] = int(ic["gap"].notna().sum())
    pc = pd.DataFrame([{"a": WEALTH[a], "b": WEALTH[b], "ref": r, "cand": B["pcorr"].get((a, b), np.nan),
                        "n_ref": A["pcorr_n"][(a, b)]} for (a, b), r in A["pcorr"].items()])
    if len(pc):
        pc["gap"] = pc["cand"] - pc["ref"]
        out["pair_corr_mae"] = float(pc["gap"].abs().mean())
        out["pair_corr_n"] = int(pc["gap"].notna().sum())
    else:
        out["pair_corr_mae"], out["pair_corr_n"] = np.nan, 0
    det["pair_corr"] = pc
    # net worth
    nwq = pd.DataFrame([{"stat": f"p{int(round(100 * q))}", "ref": A["nw_q"][q], "cand": B["nw_q"][q]}
                        for q in A["nw_q"]] +
                       [{"stat": "mean", "ref": A["nw_mean"], "cand": B["nw_mean"]}] +
                       [{"stat": f"top{t * 100:g}%_share", "ref": A["nw_top"][t], "cand": B["nw_top"][t]}
                        for t in A["nw_top"]])
    det["nw"] = nwq
    for q in [0.5, 0.9, 0.99]:
        out[f"nw_p{int(round(100 * q))}_ratio"] = float(B["nw_q"][q] / A["nw_q"][q])
    out["nw_mean_ratio"] = float(B["nw_mean"] / A["nw_mean"])
    for t in A["nw_top"]:
        out[f"nw_top{t * 100:g}_ref"] = 100 * A["nw_top"][t]
        out[f"nw_top{t * 100:g}_cand"] = 100 * B["nw_top"][t]
        out[f"nw_top{t * 100:g}_gap_pp"] = 100 * (B["nw_top"][t] - A["nw_top"][t])
    nwg = pd.DataFrame([{"group": INC_LABELS[k], "ref_mean": A["nw_inc"][k]["mean"],
                         "cand_mean": B["nw_inc"][k]["mean"], "ref_median": A["nw_inc"][k]["median"],
                         "cand_median": B["nw_inc"][k]["median"]} for k in range(4)])
    det["nw_inc"] = nwg
    out["nw_inc_mean_mape"] = 100 * float((np.abs(nwg["cand_mean"] / nwg["ref_mean"] - 1)).mean())
    out["nw_inc_median_mape"] = 100 * float((np.abs(nwg["cand_median"] / nwg["ref_median"] - 1)).mean())
    # cloning
    out["clone_ref"] = 100 * A["clone_share"]
    out["clone_cand"] = 100 * B["clone_share"]
    out["clone_gap_pp"] = out["clone_cand"] - out["clone_ref"]
    return out, det


# --------------------------------------------------------------------------
# Classifier test
# --------------------------------------------------------------------------

def classifier_test(A, B, seed):
    """A gradient-boosted classifier that tries to tell B's records from
    A's, using the shared variables and the 24 wealth tokens. Fitted and
    scored with the population weights (each class scaled to equal total
    weight). Households are kept whole on one side of the fit/score split."""
    from sklearn.ensemble import HistGradientBoostingClassifier
    from sklearn.inspection import permutation_importance
    from sklearn.metrics import roc_auc_score

    feats = ["t_" + v for v in SHARED_CAT + INCOMES + WEALTH]
    rng = np.random.default_rng(seed)
    X = np.vstack([A[feats].to_numpy(), B[feats].to_numpy()]).astype(float)
    y = np.concatenate([np.zeros(len(A)), np.ones(len(B))])
    w = np.concatenate([A["w"].to_numpy() / A["w"].sum(), B["w"].to_numpy() / B["w"].sum()])
    hh = np.concatenate([A["hh"].to_numpy(), B["hh"].to_numpy()])
    uh = np.unique(hh)
    test_h = set(rng.choice(uh, size=int(0.3 * len(uh)), replace=False).tolist())
    te = np.array([h in test_h for h in hh])
    clf = HistGradientBoostingClassifier(max_iter=300, learning_rate=0.1, early_stopping=True,
                                         validation_fraction=0.15, n_iter_no_change=20,
                                         random_state=seed)
    clf.fit(X[~te], y[~te], sample_weight=w[~te] / w[~te].mean())
    p = clf.predict_proba(X[te])[:, 1]
    auc = roc_auc_score(y[te], p, sample_weight=w[te])
    pi = permutation_importance(clf, X[te], y[te], scoring="roc_auc", n_repeats=3,
                                random_state=seed, sample_weight=w[te], n_jobs=1)
    imp = pd.DataFrame({"variable": [f[2:] for f in feats], "auc_drop": pi.importances_mean}
                       ).sort_values("auc_drop", ascending=False)
    return {"auc": float(auc), "n_fit": int((~te).sum()), "n_score": int(te.sum()),
            "iterations": int(clf.n_iter_)}, imp


# --------------------------------------------------------------------------
# Helpers
# --------------------------------------------------------------------------

def household_halves(F, seed):
    uh = np.unique(F["hh"].to_numpy())
    rng = np.random.default_rng(seed)
    pick = set(rng.choice(uh, size=len(uh) // 2, replace=False).tolist())
    m = np.array([h in pick for h in F["hh"].to_numpy()])
    return F.loc[m].reset_index(drop=True), F.loc[~m].reset_index(drop=True)


def shuffled_frame(T, seed, draws=5):
    """Every record keeps its shared variables and weight, and takes the whole
    wealth vector of a random record (drawn by weight) from the same set:
    wealth patterns kept, link to income and age broken."""
    rng = np.random.default_rng(seed)
    p = T["w"].to_numpy() / T["w"].sum()
    wcols = [c for c in T.columns if c[2:] in WEALTH and c[:2] in ("d_", "t_")]
    parts = []
    for d in range(draws):
        donor = rng.choice(len(T), size=len(T), p=p)
        F = T.copy()
        for c in wcols:
            F[c] = T[c].to_numpy()[donor]
        F["draw"] = d
        F["ndraw"] = draws
        F["w"] = T["w"] / draws
        parts.append(F)
    return add_nw(pd.concat(parts, ignore_index=True))


def label_group(label):
    return re.sub(r"_seed\d+$", "", label)


def jsonable(x):
    if isinstance(x, dict):
        return {str(k): jsonable(v) for k, v in x.items()}
    if isinstance(x, (list, tuple)):
        return [jsonable(v) for v in x]
    if isinstance(x, (np.floating, float)):
        return None if not np.isfinite(x) else float(x)
    if isinstance(x, np.integer):
        return int(x)
    return x
