"""Probe C (test 4): build dataset full_v3c, all of test 4's data fixes together.

Starting point: test 3's version 2 (full_v2). Rows, split, weights and wealth
are unchanged; the three source datasets below have identical rows (checked).

1. Bridge variables and the itemizer fix (probe BR, full_v3br): mortgage
   interest (E19200) and real estate tax (E18500) observed on SCF rows; on
   PUF non-itemizers (FDED != 1) these two are unknown (token -1), not zero.
   Here the same is done for the other four itemized deductions in the
   model (E17500, E18400, E19800, E20100), which BR left at zero.
2. SCF shared variables translated into PUF terms (probe X, full_v3x2, final
   rule: switch on by likelihood, switch off at random within cells): SCF
   rows' shared columns (demographics and the eleven shared incomes) taken
   from X, records and tokens.
3. D25, every total replaced by its parts plus a remainder, in the total's
   column (c3_identities.py): AGI -> AGI_REMAINDER (tokens from probe AS's
   full_v2s), E02000 -> SCHE_REMAINDER, E00600 -> DIV_NONQUAL,
   E01500 -> PENS_NONTAX.
   SCF rows. The SCF measures total dividends and total Schedule E only. X's
   translated totals (PUF 2015 amounts) are split into the PUF's parts with a
   donor: a PUF training record drawn with probability proportional to its
   weight among PUF records with the same token of the total (the same
   weighted-rank band of the total, PUF 2015 function). Dividends: qualified =
   SCF total x donor's qualified share; non-qualified the rest. Schedule E:
   every piece and the remainder = donor's amount x (SCF total / donor's
   total), so the pieces add to the SCF total exactly and keep the donor's
   mix. A zero total gives zero parts. The split is therefore drawn from the
   PUF's distribution given the total, and does not use the SCF household's
   wealth. SCF pensions: taxable pensions (E01700) stay as X translated
   them; the non-taxable part is unobserved, as total pensions were.
   The new SCF parts are encoded with the PUF 2015 rank functions, as X did.

    python c3_prepare.py

Diagnostic variant (C3ZERO=1 C3HIDESCHE=1, dataset full_v3czs): as full_v3cz,
but the SCF rows' Schedule E pieces and remainder are unobserved (the donor
split is not used), to see whether the split weakens completed wealth.

Variant (environment C3ZERO=1, dataset full_v3cz): the six itemized
deductions are zero for PUF non-itemizers, as in version 2 (BR's two bridge
variables put back to zero there), instead of unknown. Added after a first
generation test showed that with the unknown coding the any-order model's
form of deduction depends on whether the deductions are hidden.
"""
import json
import os
import shutil
import sys
import time
import numpy as np
import pandas as pd

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, os.path.join(HERE, "..", "posttrain"))
sys.path.insert(0, HERE)
import g_tokenizer as tk  # noqa: E402
import as_agi  # noqa: E402
import c3_identities as ci  # noqa: E402

V2 = "/nfs/roberts/scratch/pi_nrs36/jar335/scf_joint/data/full_v2"
BR = "/nfs/roberts/scratch/pi_nrs36/jar335/probes/br/data/full_v3br"
XD = "/nfs/roberts/scratch/pi_nrs36/jar335/probes/x/data/full_v3x2"
AS = "/nfs/roberts/scratch/pi_nrs36/jar335/probes/as/data/full_v2s"
ZERO = os.environ.get("C3ZERO") == "1"
HIDE_SCHE = os.environ.get("C3HIDESCHE") == "1"
OUT = os.environ.get("C3OUT", "/nfs/roberts/scratch/pi_nrs36/jar335/probes/c3/data/" +
                     ("full_v3c" + ("z" if ZERO else "") + ("s" if HIDE_SCHE else "")))
SEED = 20261011 + 333
PARTS = ("train", "val", "test")
ITEMIZED = ["E17500", "E18400", "E18500", "E19200", "E19800", "E20100"]
X_SHARED = ["MARS", "XTOT", "N24", "AGERANGE", "GENDER", "E00200", "E00900", "E02000", "P23250",
            "E00600", "E00300", "E01700", "E02400", "E02100", "E00400", "E02300", "DSI"]
NEWDESC = {
    "AGI_REMAINDER": "AGI less the income items in the model plus the adjustments in the model (probes/as_agi.py)",
    "SCHE_REMAINDER": "Schedule E total (E02000) less partnership and S corporation, rent and royalty, estate and trust "
                      "(probes/c3_identities.py)",
    "DIV_NONQUAL": "Non-qualified ordinary dividends: E00600 less E00650",
    "PENS_NONTAX": "Non-taxable pensions and annuities: E01500 less E01700",
}


def log(*a):
    print(time.strftime("%H:%M:%S"), *a, flush=True)


def wq_table(x, w, name):
    rows = []
    for side, m in (("pos", x > 0), ("neg", x < 0)):
        if not m.any():
            continue
        t = tk.quantile_table(x[m], w[m], side)
        t.insert(0, "side", side); t.insert(0, "variable", name)
        t.insert(0, "year", 2015); t.insert(0, "source", "PUF")
        t["weighted"] = True
        rows.append(t)
    return rows


def main():
    t0 = time.time()
    os.makedirs(OUT, exist_ok=True)
    if os.path.exists(os.path.join(OUT, "DONE")):
        os.remove(os.path.join(OUT, "DONE"))
    meta = json.load(open(os.path.join(BR, "meta.json")))
    meta_as = json.load(open(os.path.join(AS, "meta.json")))
    names = list(meta["variable_names"])
    assert names == json.load(open(os.path.join(XD, "meta.json")))["variable_names"]
    rec, tok = {}, {}
    for p in PARTS:
        rb = pd.read_parquet(os.path.join(BR, f"records_{p}.parquet"))
        rx = pd.read_parquet(os.path.join(XD, f"records_{p}.parquet"))
        ra = pd.read_parquet(os.path.join(AS, f"records_{p}.parquet"))
        r2 = pd.read_parquet(os.path.join(V2, f"records_{p}.parquet"))
        for c in ("source", "year", "hh_id", "implicate", "unit", "weight"):
            for o in (rx, ra, r2):
                assert (rb[c].astype(str).to_numpy() == o[c].astype(str).to_numpy()).all(), (p, c)
        tb = np.load(os.path.join(BR, f"tokens_{p}.npy")).astype(np.int64)
        tx = np.load(os.path.join(XD, f"tokens_{p}.npy")).astype(np.int64)
        ta = np.load(os.path.join(AS, f"tokens_{p}.npy")).astype(np.int64)
        scf = (rb.source.astype(str) == "SCF").to_numpy()
        r = rb.copy()
        # 2. X's translation on SCF rows
        for c in X_SHARED:
            j = names.index(c)
            tb[scf, j] = tx[scf, j]
            r.loc[scf, c] = rx.loc[scf, c].to_numpy()
        # AGI remainder (dollars and tokens) from AS
        r["AGI_REMAINDER"] = ra["AGI_REMAINDER"].to_numpy()
        tb[:, names.index("E00100")] = ta[:, names.index("E00100")]
        assert (tb[scf, names.index("E00100")] < 0).all()
        # 1. itemizer fix for all six itemized deductions
        puf = ~scf
        nonit = puf & (r.FDED.to_numpy() != 1)
        for c in ITEMIZED:
            j = names.index(c)
            nz = (r.loc[nonit, c].fillna(0).to_numpy(float) != 0)
            wsum = r.loc[nonit, "weight"].to_numpy(float)
            log(f"{p} {c}: non-itemizer PUF rows with a nonzero amount {nz.sum()} "
                f"({wsum[nz].sum()/1e6:.2f}m weighted); set {'zero' if ZERO else 'unknown'}")
            if ZERO:
                r.loc[nonit, c] = 0.0
                tb[nonit, j] = meta["variables"][j]["binning"]["zero_token"]
            else:
                r.loc[nonit, c] = np.nan
                tb[nonit, j] = -1
        rec[p], tok[p] = r, tb
        log(p, "rows", len(r), "SCF", scf.sum(), "PUF non-itemizers", nonit.sum())

    # 3. D25 parts on PUF rows
    allr = pd.concat([rec[p].assign(_part=p) for p in PARTS], ignore_index=True)
    puf = (allr.source.astype(str) == "PUF").to_numpy()
    P = allr.loc[puf]
    newvals = {
        "SCHE_REMAINDER": P["E02000"].to_numpy(float) - ci.sche_pieces(P),
        "DIV_NONQUAL": P["E00600"].to_numpy(float) - P["E00650"].to_numpy(float),
        "PENS_NONTAX": P["E01500"].to_numpy(float) - P["E01700"].to_numpy(float),
    }
    w_all = P["weight"].to_numpy(float)
    for k, v in newvals.items():
        v = np.round(v, 2)
        newvals[k] = v
        log(f"PUF {k}: zero {np.mean(v == 0):.4f} of records; negative {np.sum(v < 0)} records "
            f"({w_all[v < 0].sum()/1e6:.3f}m weighted, ${np.sum(w_all*np.minimum(v,0))/1e9:.2f}bn); "
            f"total ${np.sum(w_all*v)/1e9:.1f}bn")
        allr[k] = np.nan
        allr.loc[puf, k] = v

    # quantile tables for the new variables (PUF 2015 training records)
    q = pd.read_parquet(os.path.join(V2, "quantiles.parquet"))
    qa = pd.read_parquet(os.path.join(AS, "quantiles.parquet"))
    q = q[~q.variable.isin(["E00100", "E02000", "E00600", "E01500"])]
    tr = allr.loc[puf & (allr._part == "train").to_numpy()]
    newq = [qa[qa.variable == "AGI_REMAINDER"]]
    for k in newvals:
        newq += wq_table(tr[k].to_numpy(float), tr["weight"].to_numpy(float), k)
    q = pd.concat([q] + newq, ignore_index=True)

    # meta entries: each new variable takes the total's column and place
    vmeta = {}
    for old, new in ci.REPLACE.items():
        k = names.index(old)
        if new == "AGI_REMAINDER":
            nv = meta_as["variables"][k]
            assert nv["name"] == "AGI_REMAINDER"
        else:
            hasneg = (newvals[new] < 0).any()
            tmpl = meta["variables"][names.index("E02000")] if hasneg else meta["variables"][k]
            nv = {kk: vv for kk, vv in tmpl.items() if kk not in ("name", "description")}
            nv["column"] = k
            nv["group"] = "puf_only"
            nv["observed_in"] = ["PUF", "SCF"] if new != "PENS_NONTAX" else ["PUF"]
            nv["scf_definition"] = {"SCHE_REMAINDER": "split from X's translated SCF Schedule E total by a PUF donor (c3_prepare.py)",
                                    "DIV_NONQUAL": "split from X's translated SCF dividends by a PUF donor (c3_prepare.py)",
                                    "PENS_NONTAX": "not measured by the SCF: unobserved"}[new]
        nv = dict(nv)
        nv["name"] = new
        nv["description"] = NEWDESC[new]
        vmeta[new] = nv
        meta["variables"][k] = nv
        meta["variable_names"][k] = new
        meta["generation_order"] = [new if n == old else n for n in meta["generation_order"]]
    for c in ["E00650", "E26270", "E25850", "E25860", "E26390", "E26400"]:
        v = meta["variables"][names.index(c)]
        v["group"] = "shared"
        v["observed_in"] = ["PUF", "SCF"]
        v["scf_definition"] = "split from X's translated SCF total by a PUF donor (c3_prepare.py)"
    q.to_parquet(os.path.join(OUT, "quantiles.parquet"), index=False)
    q = pd.read_parquet(os.path.join(OUT, "quantiles.parquet"))
    cache = tk._qf_cache(q)
    rng = np.random.default_rng(SEED)

    # SCF donor split of dividends and Schedule E
    scf = ~puf
    S = allr.loc[scf]
    trp = allr.loc[puf & (allr._part == "train").to_numpy()].reset_index(drop=True)
    trk = {p: tok[p] for p in PARTS}
    # PUF training tokens of the totals (unchanged from version 2)
    v2tok = np.load(os.path.join(V2, "tokens_train.npy")).astype(np.int64)
    v2rec = pd.read_parquet(os.path.join(V2, "records_train.parquet"))
    pufm = (v2rec.source.astype(str) == "PUF").to_numpy()
    # SCF rows' tokens of the totals: X's (PUF 2015 function)
    scf_tok_tot = {}
    for p in PARTS:
        tx = np.load(os.path.join(XD, f"tokens_{p}.npy")).astype(np.int64)
        rp = rec[p]
        m = (rp.source.astype(str) == "SCF").to_numpy()
        for c in ("E00600", "E02000"):
            scf_tok_tot.setdefault(c, []).append(tx[m, names.index(c)])
    for c in scf_tok_tot:
        scf_tok_tot[c] = np.concatenate(scf_tok_tot[c])
    # order of SCF rows in allr equals concatenation by part
    assert len(scf_tok_tot["E00600"]) == scf.sum()
    split = {}
    for total, parts in (("E00600", ["E00650", "DIV_NONQUAL"]),
                         ("E02000", [c for c, _ in ci.SCHE_PIECES] + ["SCHE_REMAINDER"])):
        jt = names.index(total)
        dtok = v2tok[pufm, jt]
        dT = trp[total].to_numpy(float)
        dw = trp["weight"].to_numpy(float)
        dparts = {c: trp[c].to_numpy(float) if c in trp else None for c in parts}
        for c in parts:
            if c in newvals:
                dparts[c] = trp[c].to_numpy(float)
        sT = S[total].to_numpy(float)
        st = scf_tok_tot[total]
        out = {c: np.full(len(S), np.nan) for c in parts}
        obs = ~np.isnan(sT)
        assert ((st >= 0) == obs).all()
        z = obs & (sT == 0)
        for c in parts:
            out[c][z] = 0.0
        nz = obs & (sT != 0)
        miss = 0
        for t in np.unique(st[nz]):
            m = nz & (st == t)
            cand = np.flatnonzero((dtok == t) & (dT != 0))
            if len(cand) == 0:
                miss += m.sum()
                cand = np.flatnonzero(np.sign(dT) == np.sign(sT[m][0]))
                cand = cand[np.argsort(np.abs(dT[cand] - sT[m][0]))[:50]]
            pr = dw[cand] / dw[cand].sum()
            d = rng.choice(cand, size=m.sum(), p=pr)
            ratio = sT[m] / dT[d]
            for c in parts:
                out[c][m] = np.round(dparts[c][d] * ratio, 2)
        # make the identity exact after rounding: put the rounding into the last part
        last = parts[-1]
        if total == "E00600":
            out[last][obs] = np.round(sT[obs] - out["E00650"][obs], 2)
        else:
            s = sum(sg * out[c] for c, sg in ci.SCHE_PIECES)
            out[last][obs] = np.round(sT[obs] - s[obs], 2)
        ws = S["weight"].to_numpy(float)
        log(f"SCF split of {total}: observed {obs.sum()}, zero {z.sum()}, nonzero {nz.sum()}, "
            f"rows whose band had no PUF donor (nearest 50 by amount used) {miss}")
        for c in parts:
            log(f"   SCF {c}: weighted total ${np.nansum(ws*out[c])/1e9:.1f}bn (SCF dollars sum, both waves); "
                f"share nonzero among observed {np.mean(out[c][obs] != 0):.3f}")
        split.update(out)
    for c, v in split.items():
        allr.loc[scf, c] = v
    if HIDE_SCHE:
        # diagnostic variant full_v3czs: the SCF's Schedule E is not used at all
        for c in [c for c, _ in ci.SCHE_PIECES] + ["SCHE_REMAINDER"]:
            allr.loc[scf, c] = np.nan
        log("SCF Schedule E pieces and remainder set unobserved (C3HIDESCHE=1)")
    allr.loc[scf, "PENS_NONTAX"] = np.nan

    # identities on every PUF record (dollars) and on SCF rows (split totals)
    chk = allr.copy()
    keep = {c: chk[c].to_numpy(float).copy() for c in ("E00100", "E02000", "E00600", "E01500")}
    ci.add_totals(chk)
    for c, v in keep.items():
        d = np.abs(chk[c].to_numpy(float) - v)
        for lab, m in (("PUF", puf), ("SCF", scf)):
            ok = ~np.isnan(v[m])
            if ok.sum() == 0:
                continue
            log(f"identity {c} on {lab} rows: {ok.sum()} rows with the total; within $1: {np.mean(d[m][ok] <= 1.0):.6f}; "
                f"max abs diff {np.nanmax(d[m][ok]):.4f}")
            if lab == "PUF":
                assert np.all(d[m][ok] <= 1.0), c

    # encode the new and newly observed columns
    enc_cols = list(ci.REPLACE.values())[1:] + ["E00650", "E26270", "E25850", "E25860", "E26390", "E26400"]
    mini = {"variables": [meta["variables"][names.index(c)] if c in names else vmeta[c] for c in enc_cols]}
    # names of the new variables in the meta list
    enc_in = allr[["source", "year"] + enc_cols].copy()
    enc_in.loc[scf, "source"] = "PUF"
    enc_in.loc[scf, "year"] = 2015
    enc_in["source"] = enc_in["source"].astype(str)
    enc = tk.encode(enc_in, mini, q, np.random.default_rng(SEED + 1), cache=cache).astype(np.int64)
    newnames = meta["variable_names"]
    off = 0
    for p in PARTS:
        m = (allr._part == p).to_numpy()
        n = m.sum()
        t = tok[p]
        e = enc[off:off + n]
        assert (allr.loc[m, "_part"] == p).all()
        sc = (allr.loc[m, "source"].astype(str) == "SCF").to_numpy()
        for i, c in enumerate(enc_cols):
            j = newnames.index(c)
            if c in ("E00650", "E26270", "E25850", "E25860", "E26390", "E26400"):
                # PUF rows keep version 2's tokens; SCF rows get the split's
                assert (t[~sc, j] >= 0).all()
                t[sc, j] = e[sc, i]
            else:
                t[:, j] = e[:, i]
            assert ((t[:, j] < 0) == allr.loc[m, c].isna().to_numpy()).all(), (p, c)
        off += n
        tk_dt = np.int16 if max(v["vocab_size"] for v in meta["variables"]) < 32000 else np.int32
        t = t.astype(tk_dt)
        for j, v in enumerate(meta["variables"]):
            assert t[:, j].max() < v["vocab_size"], v["name"]
        np.save(os.path.join(OUT, f"tokens_{p}.npy"), t)
        np.save(os.path.join(OUT, f"truth_{p}.npy"), t)
        r = allr.loc[m].drop(columns=["_part"]).reset_index(drop=True)
        r.to_parquet(os.path.join(OUT, f"records_{p}.parquet"), index=False)
        obs = (t >= 0).mean(axis=0)
        log(p, t.shape, "observed share SCF rows, new columns:",
            {c: round(float((t[sc, newnames.index(c)] >= 0).mean()), 3) for c in enc_cols})
    meta["dataset"] = os.path.basename(OUT)
    meta["description"] = ("Probe C (test 4): version 2 with all of test 4's data fixes: BR bridge variables and "
                           "itemized deductions unknown for non-itemizers; X's translated SCF shared variables; "
                           "D25 totals replaced by parts plus remainders (c3_prepare.py, c3_identities.py). "
                           + meta["description"])
    meta["c3_identities"] = ci.__doc__
    json.dump(meta, open(os.path.join(OUT, "meta.json"), "w"), indent=1)
    with open(os.path.join(OUT, "DONE"), "w") as f:
        f.write(f"{time.strftime('%Y-%m-%d %H:%M:%S')} job {os.environ.get('SLURM_JOB_ID')} c3_prepare.py\n")
    log(f"done in {time.time()-t0:.0f}s")


if __name__ == "__main__":
    main()
