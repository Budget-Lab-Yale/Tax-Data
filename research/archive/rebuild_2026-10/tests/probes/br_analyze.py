"""Probe BR: compare the version 2 model with the bridge model (full_v3br).

  python br_analyze.py --v2-seeds 1 2 --br-seeds 1 2

Reads completions written by h_complete.py (version 2: $T2/model/full_v2/;
bridge: $Q/model/full_v3br/) and the records files, prints and writes
markdown tables to $Q/analysis/br_tables.md. Weighted aggregates only.
"""
import argparse, json, os
import numpy as np
import pandas as pd

T2 = "/nfs/roberts/scratch/pi_nrs36/jar335/scf_joint"
Q = "/nfs/roberts/scratch/pi_nrs36/jar335/probes/br"
V2 = f"{T2}/model/full_v2"
BR = f"{Q}/model/full_v3br"
BRC = f"{Q}/model/full_v3brc"
ASSETS = ['cash', 'equities', 'bonds', 'dc', 'db', 'life_ins', 'annuities', 'trusts', 'other_fin',
          'pass_throughs', 'primary_home', 'other_home', 're_fund', 'other_nonfin']
DEBTS = ['primary_mortgage', 'other_mortgage', 'credit_lines', 'credit_cards', 'installment_debt', 'other_debt']
WEALTH = ASSETS + DEBTS + ['kg_primary_home', 'kg_other_re', 'kg_pass_throughs', 'kg_other']
INC7 = ["wages", "business", "int_div", "capital_gains", "rent", "ss_pens", "ui_other"]
GROUPS = [(0, .5, "Bottom 50%"), (.5, .9, "50-90%"), (.9, .99, "90-99%"), (.99, .999, "99-99.9%"), (.999, 1.01, "Top 0.1%")]
OUTL = []


def out(s=""):
    print(s, flush=True)
    OUTL.append(s)


def wmed(x, w):
    o = np.argsort(x, kind="stable"); c = np.cumsum(w[o]); return x[o][np.searchsorted(c, c[-1] / 2)]


def wrank(x, w):
    o = np.argsort(x, kind="stable"); c = np.cumsum(w[o]) / w.sum(); r = np.empty(len(x)); r[o] = c - w[o] / w.sum() / 2
    return r


def money(v):
    a = abs(v)
    if a >= 1e6: return f"${v / 1e6:.2f}m"
    return f"${v / 1e3:.0f}k"


def nw(df):
    return df[ASSETS].sum(axis=1) - df[DEBTS].sum(axis=1)


def table(title, rows, cols):
    out(f"\n**{title}**\n")
    out("| " + " | ".join(cols) + " |")
    out("|" + "---|" * len(cols))
    for r in rows:
        out("| " + " | ".join(str(x) for x in r) + " |")


def load_comp(d, tag, seed, cols):
    f = f"{d}/h_{tag}_seed{seed:02d}.parquet"
    if not os.path.exists(f):
        return None
    import pyarrow.parquet as pq
    have = pq.read_schema(f).names
    return pd.read_parquet(f, columns=[c for c in cols if c in have])


def info(d, tag, seed):
    f = f"{d}/h_{tag}_seed{seed:02d}_info.json"
    return json.load(open(f)) if os.path.exists(f) else None


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--v2-seeds", type=int, nargs="+", default=[1, 2])
    ap.add_argument("--br-seeds", type=int, nargs="+", default=[1])
    ap.add_argument("--brc-seeds", type=int, nargs="*", default=[])
    ap.add_argument("--puf", action="store_true", help="also the PUF tables (slow)")
    a = ap.parse_args()
    os.makedirs(f"{Q}/analysis", exist_ok=True)

    # ---------------- scores on held-out SCF records ----------------
    out("## Wealth completion score, SCF test records (nats per record, lower is better)\n")
    wealth_names = None
    rows = []
    for s in a.v2_seeds:
        i = info(V2, "scf_test", s)
        if i:
            sc = i["scores"]; bv = sc["by_variable"]
            wealth_names = [v for v in bv if v in WEALTH]
            rows.append([f"Version 2, seed {s}", f"{sum(bv[v]['nll'] for v in WEALTH if v in bv):.2f} "
                         f"(per record: {sc['nll_per_record']:.2f})", "", "",
                         f"{bv['primary_home']['nll']:.3f}", f"{bv['primary_mortgage']['nll']:.3f}"])
    for lab, D, seeds in (("Bridge model", BR, a.br_seeds), ("Bridge + charity model", BRC, a.brc_seeds)):
      for s in seeds:
        i = info(D, "scf_test", s); j = info(D, "scf_test_nobr", s)
        if i:
            sc = i["scores"]; bv = sc["by_variable"]
            r = [f"{lab}, seed {s}", "", f"{sum(bv[v]['nll'] for v in WEALTH if v in bv):.2f}"]
            if j:
                bj = j["scores"]["by_variable"]
                r.append(f"{sum(bj[v]['nll'] for v in WEALTH if v in bj):.2f}")
                r += [f"{bv['primary_home']['nll']:.3f} (hidden: {bj['primary_home']['nll']:.3f})",
                      f"{bv['primary_mortgage']['nll']:.3f} (hidden: {bj['primary_mortgage']['nll']:.3f})"]
            else:
                r += ["", f"{bv['primary_home']['nll']:.3f}", f"{bv['primary_mortgage']['nll']:.3f}"]
            rows.append(r)
            out(f"({lab} seed {s}: records scored {sc['n_records_scored']}; bridge observed: "
                f"E19200 {bv.get('E19200', {}).get('n', 'not scored')})")
    table("Completion score of the 24 wealth values", rows,
          ["Model", "Version 2 (no bridge)", "Bridge observed", "Bridge hidden",
           "Home value (nats)", "Home mortgage (nats)"])

    # ---------------- SCF test: levels by income group ----------------
    rt = pd.read_parquet(f"{BR}/data/records_test.parquet") if os.path.exists(f"{BR}/data/records_test.parquet") \
        else pd.read_parquet(f"{Q}/data/full_v3br/records_test.parquet")
    scf = rt.loc[(rt.source == "SCF") & rt.wealth_observed].copy()
    inc = scf[["t2_" + c for c in INC7]].sum(axis=1).to_numpy()
    scf["g"] = ""
    for y in scf.year.unique():
        m = (scf.year == y).to_numpy()
        r = wrank(inc[m], scf.weight.to_numpy()[m])
        lab = np.array([next(n for lo, hi, n in GROUPS if lo <= x < hi) for x in r])
        scf.loc[m, "g"] = lab
    scf["nw"] = nw(scf)
    variants = []
    for s in a.v2_seeds:
        variants.append((f"Version 2, seed {s}", V2, "scf_test", s))
    for s in a.br_seeds:
        variants.append((f"Bridge, seed {s}", BR, "scf_test", s))
        variants.append((f"Bridge hidden, seed {s}", BR, "scf_test_nobr", s))
    for s in a.brc_seeds:
        variants.append((f"Bridge + charity, seed {s}", BRC, "scf_test", s))
    rowsets = {k: [] for k in ["own", "home", "mort", "nwmean", "nwmed"]}
    comp = {}
    for name, d, tag, s in variants:
        c = load_comp(d, tag, s, ["row", "draw", "weight", "year"] + WEALTH)
        if c is None:
            continue
        c = c.loc[c.row.isin(np.flatnonzero(((rt.source == "SCF") & rt.wealth_observed).to_numpy()))]
        c["g"] = scf.g.reindex(c.row.to_numpy()).to_numpy()
        c["nw"] = nw(c)
        comp[name] = c
    names = list(comp)
    def grp_stat(df, f):
        return [f(df.loc[df.g == n]) for _, _, n in GROUPS] + [f(df)]
    stats = {
        "Share owning a home (%)": lambda d: f"{100 * np.average(d.primary_home > 0, weights=d.weight):.1f}",
        "Mean home value": lambda d: money(np.average(d.primary_home, weights=d.weight)),
        "Mean home mortgage debt": lambda d: money(np.average(d.primary_mortgage, weights=d.weight)),
        "Mean net worth": lambda d: money(np.average(d.nw, weights=d.weight)),
        "Median net worth": lambda d: money(wmed(d.nw.to_numpy(), d.weight.to_numpy())),
    }
    out("\n## SCF test records with wealth observed: truth against completed wealth, by income group\n")
    out("Income groups: test 2's seven-income total ranked within wave over these records; both waves pooled at "
        "their weights, each in its own dollars; five draws per record.")
    for title, f in stats.items():
        rows = []
        tr = grp_stat(scf, f)
        cols = {n: grp_stat(comp[n], f) for n in names}
        for k, (_, _, gname) in enumerate(GROUPS + [(0, 0, "All")]):
            rows.append([gname, tr[k]] + [cols[n][k] for n in names])
        table(title, rows, ["Income group", "Truth"] + names)

    if a.puf:
        puf_tables(a)
    with open(f"{Q}/analysis/br_tables.md", "w") as fh:
        fh.write("\n".join(OUTL) + "\n")


def puf_tables(a):
    rv2 = pd.read_parquet(f"{V2}/data/records_train.parquet",
                          columns=["source", "year", "hh_id", "weight", "FDED", "E19200", "E18500", "E00100", "MARS"])
    m = (rv2.source == "PUF").to_numpy()
    rows_puf = np.flatnonzero(m)
    p = rv2.loc[m].copy()
    allr = pd.concat([pd.read_parquet(f"{V2}/data/records_{s}.parquet") for s in ("train", "val", "test")])
    sc = allr.loc[(allr.source == "SCF") & allr.wealth_observed].copy()
    sc["nw"] = nw(sc)
    # SCF bridge amounts on SCF units (from the bridge dataset, all parts) for the reference columns
    brr = pd.concat([pd.read_parquet(f"{Q}/data/full_v3br/records_{s}.parquet") for s in ("train", "val", "test")])
    sb = brr.loc[(brr.source == "SCF") & brr.wealth_observed].copy()
    sb["nw"] = nw(sb)
    comp = {}
    for s in a.v2_seeds:
        comp[f"Version 2, seed {s}"] = load_comp(V2, "puf_wealth", s, ["row", "draw", "weight"] + ASSETS + DEBTS)
    for s in a.br_seeds:
        c = load_comp(BR, "puf_wealth", s, ["row", "draw", "weight"] + ASSETS + DEBTS)
        if c is not None:
            comp[f"Bridge, seed {s}"] = c
    for s in a.brc_seeds:
        c = load_comp(BRC, "puf_wealth", s, ["row", "draw", "weight"] + ASSETS + DEBTS)
        if c is not None:
            comp[f"Bridge + charity, seed {s}"] = c
    comp = {k: v for k, v in comp.items() if v is not None}
    for k, c in comp.items():
        assert set(np.unique(c.row)) <= set(rows_puf), k
        c["FDED"] = rv2.FDED.to_numpy()[c.row]
        c["E19200"] = rv2.E19200.to_numpy()[c.row]
        c["E18500"] = rv2.E18500.to_numpy()[c.row]
        c["agi"] = rv2.E00100.to_numpy()[c.row]
        c["nw"] = nw(c)
    names = list(comp)
    out("\n## PUF training records: completed wealth by itemized deduction band\n")
    out("Itemizers only (FDED = 1). SCF column: SCF filer units (both waves, all parts) whose constructed amount "
        "falls in the band, all units whether or not they would itemize. Version 2 completions as written by test 3.")
    for var, edges, lab, bl in [("E19200", [0, 1, 5e3, 1e4, 2e4, 4e4, np.inf], "Mortgage interest deducted (E19200)",
                                 ["0", "under $5k", "$5k-10k", "$10k-20k", "$20k-40k", "$40k and over"]),
                                ("E18500", [0, 1, 2.5e3, 5e3, 1e4, 2e4, np.inf], "Real estate tax deducted (E18500)",
                                 ["0", "under $2.5k", "$2.5k-5k", "$5k-10k", "$10k-20k", "$20k and over"])]:
        def band(x):
            b = np.digitize(x, edges[1:-1], right=False)
            return b
        for title, f in [("Share owning a home (%)", lambda d: f"{100 * np.average(d.primary_home > 0, weights=d.weight):.0f}"),
                         ("Mean home value", lambda d: money(np.average(d.primary_home, weights=d.weight))),
                         ("Mean home mortgage debt", lambda d: money(np.average(d.primary_mortgage, weights=d.weight))),
                         ("Median net worth", lambda d: money(wmed(d.nw.to_numpy(), d.weight.to_numpy())))]:
            rows = []
            for b in range(len(bl)):
                r = [bl[b]]
                c0 = comp[names[0]]; it = c0.loc[(c0.FDED == 1) & (c0.draw == 0)]
                r.append(f"{100 * it.weight[band(it[var].to_numpy()) == b].sum() / it.weight.sum():.0f}")
                for n in names:
                    c = comp[n]; d = c.loc[(c.FDED == 1) & (band(c[var].to_numpy()) == b)]
                    r.append(f(d) if len(d) else "")
                d = sb.loc[band(sb[var].to_numpy()) == b]
                r.append(f(d) if len(d) else "")
                rows.append(r)
            table(f"{lab}: {title}", rows, ["Band", "Itemizers in band (%)"] + names + ["SCF units in band"])
    out("\n## PUF training records: overall completed wealth\n")
    for title, f in [("Share owning a home (%)", lambda d: f"{100 * np.average(d.primary_home > 0, weights=d.weight):.1f}"),
                     ("Mean home value", lambda d: money(np.average(d.primary_home, weights=d.weight))),
                     ("Mean home mortgage debt", lambda d: money(np.average(d.primary_mortgage, weights=d.weight))),
                     ("Mean net worth", lambda d: money(np.average(d.nw, weights=d.weight))),
                     ("Median net worth", lambda d: money(wmed(d.nw.to_numpy(), d.weight.to_numpy())))]:
        rows = []
        for gname, sel in [("Itemizers", lambda d: d.FDED == 1), ("Non-itemizers", lambda d: d.FDED != 1),
                           ("All returns", lambda d: np.ones(len(d), bool))]:
            rows.append([gname] + [f(comp[n].loc[sel(comp[n])]) for n in names])
        table(title, rows, ["PUF returns"] + names)
    rows = []
    for y in (2013, 2016):
        d = sc.loc[sc.year == y]
        rows.append([f"SCF {y} filer units (own dollars)", f"{100 * np.average(d.primary_home > 0, weights=d.weight):.1f}",
                     money(np.average(d.primary_home, weights=d.weight)), money(np.average(d.primary_mortgage, weights=d.weight)),
                     money(np.average(d.nw, weights=d.weight)), money(wmed(d.nw.to_numpy(), d.weight.to_numpy()))])
    table("SCF reference", rows, ["", "Own home (%)", "Mean home value", "Mean home mortgage", "Mean net worth", "Median net worth"])
    # by AGI group
    out("\n## PUF training records: mean and median completed net worth by AGI group\n")
    c0 = comp[names[0]]; d0 = c0.loc[c0.draw == 0]
    r = wrank(d0.agi.to_numpy(), d0.weight.to_numpy())
    gmap = dict(zip(d0.row.to_numpy(), [next(n for lo, hi, n in GROUPS if lo <= x < hi) for x in r]))
    for n in names:
        comp[n]["g"] = comp[n].row.map(gmap)
    for title, f in [("Mean net worth", lambda d: money(np.average(d.nw, weights=d.weight))),
                     ("Median net worth", lambda d: money(wmed(d.nw.to_numpy(), d.weight.to_numpy()))),
                     ("Mean home mortgage debt", lambda d: money(np.average(d.primary_mortgage, weights=d.weight)))]:
        rows = []
        for _, _, gname in GROUPS:
            rows.append([gname] + [f(comp[n].loc[comp[n].g == gname]) for n in names])
        table(f"{title} by AGI group (PUF training records)", rows, ["AGI group"] + names)


if __name__ == "__main__":
    main()
