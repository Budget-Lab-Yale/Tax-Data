"""Probe Y: does D24's translation recover wealth given tax-concept incomes?

For each experiment A variant of test 2: half 1 (incomes distorted, true
wealth known) is the reference "file"; half 2 (undistorted incomes, wealth
observed) is the survey. Half 2 is put through:
  none        as it is;
  translated  y_translate.translate (likelihood-proportional switching, the
              default), plus the same with switching by rank and at random;
  reweighted  entropy raking of half 2's weights to half 1's income
              distribution on the same cells;
  oracle      half 2 with the same distortion applied directly (a fresh
              draw): what a perfect translation would give, apart from
              sampling differences between the two halves.
Each is compared with the truth (half 1's distorted incomes with its true
wealth) in weighted tables by income group.

Usage: python y_run.py <variant> [<variant> ...]
variants: clean noise recat unrep unrep_poor unrep_rich
Writes $Q/y_tables_<variant>.csv, $Q/y_movement_<variant>.json.
"""
import json
import os
import sys
import time

import numpy as np
import pandas as pd

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import y_translate as yt  # noqa: E402

DATA = "/nfs/roberts/scratch/pi_nrs36/jar335/scf_joint/data"
Q = "/nfs/roberts/scratch/pi_nrs36/jar335/probes/y"

INC = ["wages", "business", "int_div", "capital_gains", "rent", "ss_pens", "ui_other"]
ASSETS = ['cash', 'equities', 'bonds', 'dc', 'db', 'life_ins', 'annuities',
          'trusts', 'other_fin', 'pass_throughs', 'primary_home', 'other_home',
          're_fund', 'other_nonfin']
DEBTS = ['primary_mortgage', 'other_mortgage', 'credit_lines', 'credit_cards',
         'installment_debt', 'other_debt']
KG = ['kg_primary_home', 'kg_other_re', 'kg_pass_throughs', 'kg_other']
WEALTH = ASSETS + DEBTS + KG
GROUPS = [(0, .2, "Bottom 20%"), (.2, .4, "20-40%"), (.4, .6, "40-60%"), (.6, .8, "60-80%"),
          (.8, .9, "80-90%"), (.9, .99, "90-99%"), (.99, 1.0001, "Top 1%")]
ALTER_SEED_ORACLE = 777
ALTER_SEED_NEW = 20261099


def log(*a):
    print(time.strftime("%H:%M:%S"), *a, flush=True)


def load(dataset):
    meta = json.load(open(os.path.join(DATA, dataset, "meta.json")))
    names = meta["variable_names"]
    parts = []
    for p in ("train", "val", "test"):
        r = pd.read_parquet(os.path.join(DATA, dataset, f"records_{p}.parquet"))
        t = np.load(os.path.join(DATA, dataset, f"truth_{p}.npy"))
        for c in ("age_range", "married", "n_dep"):
            k = names.index(c)
            v = meta["variables"][k]
            vals = {tok["token"]: tok["value"] for tok in v["tokens"]}
            col = t[:, k]
            if (col < 0).any():
                raise ValueError(f"{c} unobserved in {dataset} {p}")
            r[c] = pd.Series(col).map(vals).astype(float).to_numpy()
        r["part"] = p
        parts.append(r)
    df = pd.concat(parts, ignore_index=True)
    h1 = df[df.part == "test"].reset_index(drop=True)
    h2 = df[(df.part != "test") & (df.half == 2)].reset_index(drop=True)
    assert (h1.half == 1).all() and h1.source.eq("SCF_H1").all()
    return h1, h2


def add_columns(d):
    d = d.copy()
    d["age3"] = np.select([d.age_range <= 3, d.age_range <= 5], [0, 1], 2).astype(float)
    d["networth"] = d[ASSETS].sum(axis=1, min_count=1) - d[DEBTS].sum(axis=1, min_count=1)
    d.loc[~d.wealth_observed.astype(bool), "networth"] = np.nan
    d["f_networth"] = np.arcsinh(d.networth / 1000)
    for c in WEALTH:
        d["f_" + c] = np.arcsinh(d[c] / 1000)
    return d


def alter(d, variant, seed):
    """Apply a distortion to a table's incomes (as test 2's a_prepare.alter)."""
    d = d.copy()
    for c in INC:
        d[c] = d["orig_" + c]
    if variant == "clean":
        return d
    rng = np.random.default_rng(seed)
    key = d[["hh_id", "unit"]].drop_duplicates().sort_values(["hh_id", "unit"]).reset_index(drop=True)
    if variant == "noise":
        for c in INC:
            key["z_" + c] = rng.normal(0, 0.3, len(key))
        m = d[["hh_id", "unit"]].merge(key, on=["hh_id", "unit"], how="left")
        for c in INC:
            x = d[c].to_numpy()
            d[c] = np.where(x != 0, x * np.exp(m["z_" + c].to_numpy()), 0.0)
    elif variant == "recat":
        sel = d.business > 0
        move = 0.3 * d.loc[sel, "business"]
        d.loc[sel, "wages"] += move
        d.loc[sel, "business"] -= move
    elif variant in ("unrep", "unrep_poor", "unrep_rich"):
        key["u_id"] = rng.random(len(key))
        key["u_kg"] = rng.random(len(key))
        if variant == "unrep":
            key["p"] = 0.25
        else:
            # Non-reporting depends on wealth: 40% against 10% for units
            # below and above the weighted median net worth (unit's mean over
            # implicates); 25% where wealth is unobserved (split-off units).
            nw = d.groupby(["hh_id", "unit"]).networth.mean()
            ww = d.groupby(["hh_id", "unit"]).weight.sum()
            ok = nw.notna()
            med = yt.weighted_quantile_map(nw[ok].to_numpy(), ww[ok].to_numpy(), np.array([0.5]))[0]
            low = (nw < med).reindex(pd.MultiIndex.from_frame(key[["hh_id", "unit"]])).to_numpy()
            obs = nw.notna().reindex(pd.MultiIndex.from_frame(key[["hh_id", "unit"]])).to_numpy()
            hi_p, lo_p = (0.10, 0.40) if variant == "unrep_poor" else (0.40, 0.10)
            key["p"] = np.where(~obs, 0.25, np.where(low, lo_p, hi_p))
        m = d[["hh_id", "unit"]].merge(key, on=["hh_id", "unit"], how="left")
        x = d.int_div.to_numpy()
        d["int_div"] = np.where((x != 0) & (m.u_id.to_numpy() < m.p.to_numpy()), 0.0, x)
        x = d.capital_gains.to_numpy()
        d["capital_gains"] = np.where((x != 0) & (m.u_kg.to_numpy() < m.p.to_numpy()), 0.0, x)
    else:
        raise ValueError(variant)
    return d


# ---------------------------------------------------------------------------
# Tables
# ---------------------------------------------------------------------------

def wmed(x, w):
    return float(yt.weighted_quantile_map(x, w, np.array([0.5]))[0])


def tables(d, method):
    w = d.weight.to_numpy(float)
    tot = d[INC].sum(axis=1).to_numpy()
    r = yt.weighted_rank(tot, w, np.random.default_rng(3))
    obs = d.networth.notna().to_numpy()
    nw = d.networth.to_numpy()
    rows = []

    def stats(mask, table, group):
        m = mask & obs
        ww = w[m]
        out = {
            "pop_share": w[mask].sum() / w.sum(),
            "mean_nw": np.average(nw[m], weights=ww),
            "median_nw": wmed(nw[m], ww),
            "share_equities": np.average(d.equities.to_numpy()[m] > 0, weights=ww),
            "share_business": np.average(d.pass_throughs.to_numpy()[m] > 0, weights=ww),
            "share_bonds": np.average(d.bonds.to_numpy()[m] > 0, weights=ww),
            "mean_equities": np.average(d.equities.to_numpy()[m], weights=ww),
            "n_rows": int(m.sum()),
        }
        for k, v in out.items():
            rows.append((method, table, group, k, float(v)))

    for lo, hi, lab in GROUPS:
        stats((r >= lo) & (r < hi), "income_group", lab)
    for v in INC:
        x = d[v].to_numpy()
        stats(x == 0, "status_" + v, "zero")
        stats(x != 0, "status_" + v, "nonzero")
    top = (r >= 0.99) & obs
    rows.append((method, "top1", "Top 1% by income", "share_of_networth",
                 float((nw[top] * w[top]).sum() / (nw[obs] * w[obs]).sum())))
    rows.append((method, "all", "All", "mean_nw", float(np.average(nw[obs], weights=w[obs]))))
    rows.append((method, "all", "All", "median_nw", wmed(nw[obs], w[obs])))
    return rows




def run(variant, design="halves"):
    """design "halves": reference = half 1 (test 2's set-up). design "self":
    reference = half 2 itself with the distortion applied (the oracle), so
    the truth is known household by household and the two tables differ
    only in measurement, not in sampling."""
    log(f"=== {variant} ({design})")
    if variant in ("clean", "noise", "recat", "unrep"):
        h1, h2 = load(f"expA_{variant}")
        h1, h2 = add_columns(h1), add_columns(h2)
    else:
        h1, h2 = load("expA_clean")
        h1, h2 = add_columns(h1), add_columns(h2)
        if not variant.startswith("svy_"):
            h1 = alter(h1, variant, ALTER_SEED_NEW)
    # half 2's incomes are undistorted in every dataset
    for c in INC:
        assert np.array_equal(h2[c].to_numpy(), h2["orig_" + c].to_numpy()), c
    log(f"half 1 rows {len(h1)} households {h1.hh_id.nunique()}; half 2 rows {len(h2)} households {h2.hh_id.nunique()}")
    if variant.startswith("svy_"):
        # The other direction: the SURVEY under-reports (as the SCF does
        # against the PUF for dividends) and the reference is right.
        # Survey = half 2 with the distortion; reference and truth = half 2
        # as measured (same households).
        h1 = h2.copy()
        h2 = alter(h2, variant[4:], ALTER_SEED_ORACLE)
        oracle = h1.copy()
        variant = variant + "_self"
    else:
        oracle = alter(h2, variant, ALTER_SEED_ORACLE)
        if design == "self":
            h1 = oracle.copy()
            variant = variant + "_self"

    rows = tables(h1, "truth") + tables(h2, "none") + tables(oracle, "oracle")
    movement = {}
    feats = ["f_networth"] + ["f_" + c for c in WEALTH]
    for rule in ("proportional", "rank", "random", "likely_on_random_off"):
        spec = yt.TranslateSpec(amounts=INC, cell_vars=["married", "age3"], flip_features=feats,
                                n_draws=5, flip_rule=rule, total_groups=3, seed=11)
        t0 = time.time()
        out, diag = yt.translate(h2, h1, spec, log=log)
        log(f"translate ({rule}) took {time.time() - t0:.0f}s; cells per amount {diag['cells_per_amount']}")
        name = "translated" if rule == "proportional" else f"translated_{rule}"
        rows += tables(out, name)
        movement[name] = diag
        # draw 0, for retraining test 2's model on translated data (step 4)
        d0 = out[out.draw == 0][["hh_id", "implicate", "unit"] + INC]
        assert not d0.duplicated(["hh_id", "implicate", "unit"]).any()
        suffix = "" if rule == "proportional" else "_" + rule
        d0.to_parquet(os.path.join(Q, f"y_translated_draw0_{variant}{suffix}.parquet"), index=False)

    # Reweighting: raking half 2's weights to half 1 on the same cells
    def m_cat(d):
        return d.married.astype(int).astype(str) + "_" + d.age3.astype(int).astype(str) + "_" + d.n_dep.astype(int).astype(str)
    margins = [m_cat]
    for v in INC:
        x1 = h1[v].to_numpy()
        pos = x1 > 0
        cuts = yt.weighted_cutpoints(x1[pos], h1.weight.to_numpy()[pos], 5)

        def f(d, v=v, cuts=cuts):
            x = d[v].to_numpy()
            b = np.where(x < 0, "neg", np.where(x == 0, "zero", "p" + pd.Series(np.searchsorted(cuts, x, side="right")).astype(str).to_numpy()))
            return d.married.astype(int).astype(str).to_numpy() + "_" + d.age3.astype(int).astype(str).to_numpy() + "_" + b
        margins.append(f)
    t1 = h1[INC].sum(axis=1).to_numpy()
    tcuts = yt.weighted_quantile_map(t1, h1.weight.to_numpy(), np.array([.1, .2, .3, .4, .5, .6, .7, .8, .9, .99]))
    margins.append(lambda d: np.searchsorted(tcuts, d[INC].sum(axis=1).to_numpy(), side="right"))
    wr = yt.rake(h2, h1, margins, log=log)
    rw = h2.copy()
    rw["weight"] = wr
    rows += tables(rw, "reweighted")
    w0 = h2.weight.to_numpy()
    movement["reweighted"] = {
        "weight_ratio_p1": float(np.percentile(wr / w0, 1)), "weight_ratio_p50": float(np.percentile(wr / w0, 50)),
        "weight_ratio_p99": float(np.percentile(wr / w0, 99)), "weight_ratio_max": float(np.max(wr / w0)),
        "ess_new": float(wr.sum() ** 2 / (wr ** 2).sum()), "ess_old": float(w0.sum() ** 2 / (w0 ** 2).sum()),
        "mean_nw_all_new": float(np.average(rw.networth[rw.networth.notna()], weights=wr[rw.networth.notna()])),
    }
    # How much does the noise scramble order? Weighted rank correlation of
    # half 2's true and distorted amounts among nonzero values (oracle draw).
    scramble = {}
    for v in INC:
        x, y = h2[v].to_numpy(), oracle[v].to_numpy()
        nz = (x != 0) & (y != 0)
        ww = h2.weight.to_numpy()[nz]
        rng = np.random.default_rng(5)
        a, b = yt.weighted_rank(x[nz], ww, rng), yt.weighted_rank(y[nz], ww, rng)
        scramble[v] = {
            "rank_corr": float(np.corrcoef(a, b)[0, 1]),
            "mean_abs_rank_move_points": float(np.average(np.abs(a - b), weights=ww) * 100),
            "share_switched": float(np.average((x != 0) != (y != 0), weights=h2.weight.to_numpy())),
        }
    movement["oracle_distortion_of_half2"] = scramble

    tab = pd.DataFrame(rows, columns=["method", "table", "group", "stat", "value"])
    tab.insert(0, "variant", variant)
    tab.to_csv(os.path.join(Q, f"y_tables_{variant}.csv"), index=False)
    json.dump(movement, open(os.path.join(Q, f"y_movement_{variant}.json"), "w"), indent=1)
    log(f"wrote tables and movement for {variant}")


if __name__ == "__main__":
    design = "halves"
    for v in sys.argv[1:]:
        if v == "--self":
            design = "self"
            continue
        run(v, design)
