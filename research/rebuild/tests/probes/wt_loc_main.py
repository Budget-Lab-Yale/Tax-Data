"""Probe WT (locate step): where does the shortfall of completed PUF wealth at
the top enter in probe C's combined model? No training; reads probe C's and
probe X's files. Weighted aggregates only.

    python wt_loc_main.py

Income concept for groups ("shared income"): the sum of the income items both
the PUF and the translated SCF carry, in PUF 2015 dollars: wages (E00200),
Schedule C (E00900), total Schedule E (E02000), capital gains (P23250, plus
P22250 short-term gains on PUF rows; the translated SCF puts all gains in
P23250), ordinary dividends (E00600), taxable interest (E00300), taxable
pensions (E01700), gross Social Security (E02400), farm (E02100),
unemployment compensation (E02300). Groups are weighted ranks within each SCF
wave and within the PUF (training records, all units); test records use the
training thresholds of their wave. PUF records are also grouped by AGI, as
probe C did.

Sections, written to $L/locate.md and locate.json:
  A  SCF records' own net worth by group (training and test), against the
     PUF records' completed net worth (C's three seeds, X's two)
  B  SCF test records: completed net worth (C seeds 1-3, bridge observed and
     hidden; X seeds 1-2) against their truth, same records
  C  income mix and support: PUF top groups against SCF top groups
  D  rank-band decoding of the wealth variables
  E  PUF completions with the bridge hidden / only shared variables (if the
     wt_loc_comp jobs have finished)
"""
import glob
import json
import os
import sys

import numpy as np
import pandas as pd

sys.path.insert(0, "/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data/research/rebuild/tests/posttrain")
import g_tokenizer as gt  # noqa: E402

C3 = "/nfs/roberts/scratch/pi_nrs36/jar335/probes/c3"
XQ = "/nfs/roberts/scratch/pi_nrs36/jar335/probes/x"
L = "/nfs/roberts/scratch/pi_nrs36/jar335/probes/wt/locate"
D = f"{C3}/data/full_v3c"
ASSETS = ['cash', 'equities', 'bonds', 'dc', 'db', 'life_ins', 'annuities', 'trusts', 'other_fin',
          'pass_throughs', 'primary_home', 'other_home', 're_fund', 'other_nonfin']
DEBTS = ['primary_mortgage', 'other_mortgage', 'credit_lines', 'credit_cards', 'installment_debt', 'other_debt']
WEALTH = ASSETS + DEBTS + ['kg_primary_home', 'kg_other_re', 'kg_pass_throughs', 'kg_other']
EDGES = [0.5, 0.9, 0.99, 0.999]
GL = ["Bottom 50%", "50-90%", "90-99%", "99-99.9%", "Top 0.1%"]
SHOW = [2, 3, 4]  # 90-99, 99-99.9, top 0.1
OUT = []


def say(s=""):
    print(s, flush=True)
    OUT.append(s)


def nw(f):
    return f[ASSETS].to_numpy(float).sum(1) - f[DEBTS].to_numpy(float).sum(1)


def shared_income(f):
    s = np.zeros(len(f))
    for c in ["E00200", "E00900", "E02000", "P23250", "E00600", "E00300", "E01700", "E02400", "E02100", "E02300"]:
        s += np.nan_to_num(f[c].to_numpy(float))
    if "P22250" in f:
        s += np.where(f.source.to_numpy() == "PUF", np.nan_to_num(f["P22250"].to_numpy(float)), 0.0)
    return s


def wq(x, w, q):
    if len(x) == 0:
        return np.nan
    o = np.argsort(x, kind="mergesort")
    cw = np.cumsum(w[o]) / w.sum()
    return float(x[o][min(np.searchsorted(cw, q), len(x) - 1)])


def thresholds(x, w):
    return [wq(x, w, q) for q in EDGES]


def grp(x, th):
    return np.searchsorted(th, x, side="right")


def interp(a13, a16, share=False):
    if share or not (a13 > 0 and a16 > 0):
        return a13 / 3 + 2 * a16 / 3
    return float(np.exp(np.log(a13) / 3 + 2 * np.log(a16) / 3))


def m_(x, w):
    return float(np.sum(x * w) / w.sum()) if w.sum() > 0 else np.nan


def fm(x):
    if x is None or not np.isfinite(x):
        return "n/a"
    a = abs(x)
    if a >= 1e6:
        return f"${x / 1e6:.1f}m"
    if a >= 1e3:
        return f"${x / 1e3:.0f}k"
    return f"${x:.0f}"


def table(head, rows):
    say("| " + " | ".join(head) + " |")
    say("|" + "---|" * len(head))
    for r in rows:
        say("| " + " | ".join(str(c) for c in r) + " |")
    say()


meta = json.load(open(f"{D}/meta.json"))
V = {v["name"]: v for v in meta["variables"]}
tr = pd.read_parquet(f"{D}/records_train.parquet")
te = pd.read_parquet(f"{D}/records_test.parquet")
tr["inc"] = shared_income(tr)
te["inc"] = shared_income(te)
res = {}

# ---- check that the SCF rows carry the translated incomes ----
xtr = pd.read_parquet(f"{XQ}/model/full_v3x2/data/records_train.parquet",
                      columns=["source", "E00200", "orig_E00200", "E00900", "orig_E00900"])
ms = (tr.source == "SCF").to_numpy()
for c in ("E00200", "E00900"):
    a, b, o = tr[c].to_numpy(float)[ms], xtr[c].to_numpy(float)[ms], xtr["orig_" + c].to_numpy(float)[ms]
    say(f"check {c} on SCF rows: equal to X's translated {np.mean(np.isclose(a, b, equal_nan=True)):.4f}, "
        f"equal to X's untranslated {np.mean(np.isclose(a, o, equal_nan=True)):.4f}")
say()

# ---- groups ----
TH = {}
for y in (2013, 2016):
    m = ((tr.source == "SCF") & (tr.year == y)).to_numpy()
    TH[("SCF", y)] = thresholds(tr.inc.to_numpy()[m], tr.weight.to_numpy(float)[m])
mp = (tr.source == "PUF").to_numpy()
TH[("PUF", "inc")] = thresholds(tr.inc.to_numpy()[mp], tr.weight.to_numpy(float)[mp])
TH[("PUF", "agi")] = thresholds(tr.E00100.to_numpy(float)[mp], tr.weight.to_numpy(float)[mp])
for f in (tr, te):
    g = np.full(len(f), -1)
    for y in (2013, 2016):
        m = ((f.source == "SCF") & (f.year == y)).to_numpy()
        g[m] = grp(f.inc.to_numpy()[m], TH[("SCF", y)])
    m = (f.source == "PUF").to_numpy()
    g[m] = grp(f.inc.to_numpy()[m], TH[("PUF", "inc")])
    f["g_inc"] = g
    ga = np.full(len(f), -1)
    ga[m] = grp(f.E00100.to_numpy(float)[m], TH[("PUF", "agi")])
    f["g_agi"] = ga
say("Thresholds of shared income (99th, 99.9th percentile): " + "; ".join(
    f"{k[0]} {k[1]}: {fm(v[2])}, {fm(v[3])}" for k, v in TH.items()))
say()

# ================= A =================
say("## A. Net worth by shared-income group: SCF records' own, and PUF records' completed")
say()
rows = []
for gi in SHOW + ["top1"]:
    lab = "Top 1%" if gi == "top1" else GL[gi]
    row = [lab]
    for f, nm in ((tr, "train"), (te, "test")):
        st = {}
        for y in (2013, 2016):
            m = ((f.source == "SCF") & (f.year == y) & f.wealth_observed.astype(bool)).to_numpy()
            m &= (f.g_inc >= 3).to_numpy() if gi == "top1" else (f.g_inc == gi).to_numpy()
            w = f.weight.to_numpy(float)[m]
            x = nw(f[m])
            st[y] = (m_(x, w), wq(x, w, 0.5), int(m.sum()), m_(f.inc.to_numpy()[m], w))
        row += [fm(interp(st[2013][0], st[2016][0])), fm(interp(st[2013][1], st[2016][1])),
                f"{st[2013][2]}/{st[2016][2]}"]
        if nm == "train":
            inc_scf = interp(st[2013][3], st[2016][3])
    m = mp & ((tr.g_inc >= 3).to_numpy() if gi == "top1" else (tr.g_inc == gi).to_numpy())
    row += [fm(inc_scf), fm(m_(tr.inc.to_numpy()[m], tr.weight.to_numpy(float)[m])), int(m.sum())]
    rows.append(row)
table(["Shared-income group", "SCF train mean", "SCF train median", "SCF train rows (2013/2016)",
       "SCF test mean", "SCF test median", "SCF test rows", "SCF mean shared income",
       "PUF mean shared income", "PUF records"], rows)

# PUF completions: C (3 seeds), X (2 seeds), C zero-coded (3 seeds)
files = {}
for f in sorted(glob.glob(f"{C3}/model/h_puf_wealth_seed*.parquet")):
    files.setdefault("C (unknown coding)", []).append(f)
for f in sorted(glob.glob(f"{C3}/model_cz/h_puf_wealth_seed*.parquet")):
    files.setdefault("C (zero coding)", []).append(f)
for f in sorted(glob.glob(f"{XQ}/model/full_v3x2/h_puf_wealth_seed*.parquet")):
    files.setdefault("X (translated)", []).append(f)
for tag, lab in (("puf_nobr", "C, bridge hidden"), ("puf_shared", "C, only shared variables visible"),
                 ("puf_sharednobr", "C, only shared visible and bridge hidden")):
    for f in sorted(glob.glob(f"{L}/comp/h_{tag}_seed*.parquet")):
        files.setdefault(lab, []).append(f)
p = tr[mp].copy()
p["row"] = np.flatnonzero(mp)
pw = p.set_index("row")
PUFNW = {}
for lab, fl in files.items():
    for f in fl:
        C = pd.read_parquet(f, columns=["row", "draw"] + WEALTH)
        K = C.draw.nunique()
        x = nw(C)
        # mean over draws per record
        per = pd.Series(x).groupby(C.row.to_numpy()).mean()
        PUFNW.setdefault(lab, []).append(per)
        say(f"read {lab}: {os.path.basename(f)} draws {K}")
say()
rows = []
for gname in ("g_inc", "g_agi"):
    for gi in SHOW + ["top1"]:
        lab = ("Top 1%" if gi == "top1" else GL[gi]) + (" (shared income)" if gname == "g_inc" else " (AGI)")
        g = pw[gname].to_numpy()
        m = (g >= 3) if gi == "top1" else (g == gi)
        idx = pw.index.to_numpy()[m]
        w = pw.weight.to_numpy(float)[m]
        row = [lab]
        for mlab in files:
            vals = [m_(per.loc[idx].to_numpy(), w) for per in PUFNW[mlab]]
            row.append(fm(np.mean(vals)) + " [" + ", ".join(fm(v) for v in vals) + "]")
            res.setdefault("A_puf", {}).setdefault(mlab, {})[lab] = vals
        rows.append(row)
table(["PUF group"] + list(files), rows)

# ================= B =================
say("## B. SCF test records: completed net worth against their own truth")
say()
te_r = te.copy()
te_r["row"] = np.arange(len(te))
comp = {}
for sd in (1, 2, 3):
    for d_, tag, lab in (("model", "scf_test", "C, bridge observed"), ("model", "scf_test_nobr", "C, bridge hidden"),
                         ("model_cz", "scf_test", "C zero coding"),
                         ("model_czs", "scf_test", "C zero coding, SCF Schedule E unobserved")):
        f = f"{C3}/{d_}/h_{tag}_seed{sd:02d}.parquet"
        if os.path.exists(f):
            comp.setdefault(lab, []).append(f)
xte = pd.read_parquet(f"{XQ}/model/full_v3x2/data/records_test.parquet", columns=["source", "year", "hh_id", "implicate", "unit"])
same = bool((xte.astype(str).values == te[["source", "year", "hh_id", "implicate", "unit"]].astype(str).values).all())
say(f"X's test records in the same row order as C's: {same}")
for sd in (1, 2):
    f = f"{C3}/xscore/h_scf_test_seed{sd:02d}.parquet"
    if os.path.exists(f) and same:
        comp.setdefault("X (translated)", []).append(f)
rows = []
for gi in SHOW + ["top1"]:
    lab = "Top 1%" if gi == "top1" else GL[gi]
    sel = te_r[(te_r.source == "SCF") & te_r.wealth_observed.astype(bool)]
    sel = sel[(sel.g_inc >= 3) if gi == "top1" else (sel.g_inc == gi)]
    row = [lab, f"{(sel.year == 2013).sum()}/{(sel.year == 2016).sum()}"]
    # truth, interpolated
    st = {}
    for y in (2013, 2016):
        s = sel[sel.year == y]
        st[y] = (m_(nw(s), s.weight.to_numpy(float)), wq(nw(s), s.weight.to_numpy(float), 0.5))
    row += [fm(interp(st[2013][0], st[2016][0])), fm(interp(st[2013][1], st[2016][1]))]
    tot_true = np.sum(nw(sel) * sel.weight.to_numpy(float))
    for clab, fl in comp.items():
        means, ratios = [], []
        for f in fl:
            Cc = pd.read_parquet(f, columns=["row", "draw", "year", "weight"] + WEALTH)
            Cc = Cc[Cc.row.isin(sel.row)]
            K = Cc.draw.nunique()
            x = nw(Cc)
            w = Cc.weight.to_numpy(float) / K
            sy = {}
            for y in (2013, 2016):
                my = (Cc.year == y).to_numpy()
                sy[y] = m_(x[my], w[my])
            means.append(interp(sy[2013], sy[2016]))
            ratios.append(np.sum(x * w) / tot_true)
        row.append(fm(np.mean(means)) + f" (ratio {np.mean(ratios):.2f}: " + ", ".join(f"{r:.2f}" for r in ratios) + ")")
        res.setdefault("B", {}).setdefault(clab, {})[lab] = dict(means=means, ratios=ratios)
    rows.append(row)
table(["Shared-income group", "SCF test rows 2013/2016", "Truth mean", "Truth median"] +
      [f"{k} mean (completed/true, pooled waves)" for k in comp], rows)

# ================= C =================
say("## C. Income mix and support: PUF top records against SCF top records (training records)")
say()
COMP = {"wages": ["E00200"], "Schedule C": ["E00900"], "partnership and S corp": ["E26270"],
        "other Schedule E": ["E02000", "-E26270"], "capital gains": ["P23250", "P22250"],
        "dividends": ["E00600"], "taxable interest": ["E00300"], "pensions and Social Security": ["E01700", "E02400"],
        "farm and unemployment": ["E02100", "E02300"],
        "(bridge) interest paid E19200": ["E19200"], "(bridge) real estate tax E18500": ["E18500"]}


def comp_val(f, cols):
    s = np.zeros(len(f))
    for c in cols:
        sg = -1.0 if c.startswith("-") else 1.0
        c = c.lstrip("-")
        v = np.nan_to_num(f[c].to_numpy(float))
        if c == "P22250":
            v = np.where(f.source.to_numpy() == "PUF", v, 0.0)
        s += sg * v
    return s


for gi, lab in ((4, "Top 0.1%"), (3, "99-99.9%")):
    rows = []
    sp = tr[mp & (tr.g_inc == gi).to_numpy()]
    ss = tr[(~mp) & (tr.g_inc == gi).to_numpy()]
    wp, wsc = sp.weight.to_numpy(float), ss.weight.to_numpy(float)
    for k, cols in COMP.items():
        a, b = comp_val(sp, cols), comp_val(ss, cols)
        rows.append([k, fm(m_(a, wp)), fm(m_(b, wsc)), f"{m_(a != 0, wp):.0%}", f"{m_(b != 0, wsc):.0%}"])
        res.setdefault("C_mix", {}).setdefault(lab, {})[k] = [m_(a, wp), m_(b, wsc), m_(a != 0, wp), m_(b != 0, wsc)]
    for k in ("E19200", "E18500"):
        a, b = sp[k].to_numpy(float), ss[k].to_numpy(float)
        oa, ob = np.isfinite(a), np.isfinite(b)
        rows.append([f"{k} among records observing it", fm(m_(a[oa], wp[oa])), fm(m_(b[ob], wsc[ob])),
                     f"{m_(np.nan_to_num(a) != 0, wp):.0%}", f"{m_(np.nan_to_num(b) != 0, wsc):.0%}"])
    rows.append(["shared income", fm(m_(sp.inc.to_numpy(), wp)), fm(m_(ss.inc.to_numpy(), wsc)), "", ""])
    say(f"### {lab} of shared income (PUF and SCF each ranked on their own records; SCF waves pooled)")
    say()
    table(["Item", "PUF mean", "SCF mean", "PUF share nonzero", "SCF share nonzero"], rows)

# support in tokens
tok = np.load(f"{D}/tokens_train.npy")
KEY = ["E00200", "E00900", "E26270", "P23250", "E00650", "DIV_NONQUAL", "E00300", "E02400", "E01700",
       "E25850", "E19200", "E18500", "SCHE_REMAINDER"]
rows = []
scf_obs = (~mp) & tr.wealth_observed.astype(bool).to_numpy()
for gi, glab in ((4, "Top 0.1%"), (3, "99-99.9%")):
    mpg = mp & (tr.g_inc == gi).to_numpy()
    msg = scf_obs & (tr.g_inc == gi).to_numpy()
    wp = tr.weight.to_numpy(float)[mpg]
    for k in KEY:
        c = V[k]["column"]
        tp, ts = tok[mpg, c], tok[msg, c]
        tall = tok[scf_obs, c]
        mx = ts.max() if (ts >= 0).any() else -1
        mxall = tall.max()
        # top three positive bands (rank 0.999 and above) of the PUF's own rank grid
        top3 = [t["token"] for t in V[k]["tokens"] if t["kind"] == "pos" and t["lo"] >= 0.999 - 1e-9]
        rows.append([glab, k, f"{m_((tp >= 0), wp):.0%}", f"{m_(np.isin(tp, top3), wp):.1%}",
                     f"{np.mean(np.isin(ts[ts >= 0], top3)) if (ts >= 0).any() else 0:.1%}",
                     f"{m_(tp > mx, wp):.1%}", f"{m_(tp > mxall, wp):.2%}"])
say("Rank-band support of the shared amounts (tokens are PUF 2015 rank bands for both sources). "
    "'Top three bands' = rank among PUF positives 0.999 and above.")
say()
table(["PUF group", "Variable", "PUF share observed", "PUF share in top three bands (weighted)",
       "SCF same group, share in top three bands (unweighted rows)",
       "PUF share above highest band of any SCF record in group", "PUF share above highest band of any SCF record"], rows)

# nearest-neighbour support on six key incomes
K6 = ["E00200", "E00900", "E26270", "P23250", "E00650", "E00300"]
cols6 = [V[k]["column"] for k in K6]
S6 = tok[scf_obs][:, cols6].astype(np.int32)
S6 = np.where(S6 < 0, -100, S6)
rows = []
for gi, glab in ((4, "Top 0.1%"), (3, "99-99.9%")):
    mpg = mp & (tr.g_inc == gi).to_numpy()
    P6 = tok[mpg][:, cols6].astype(np.int32)
    P6 = np.where(P6 < 0, -100, P6)
    wp = tr.weight.to_numpy(float)[mpg]
    for tol in (1, 2, 4):
        cnt = np.zeros(len(P6), dtype=np.int64)
        for i in range(0, len(P6), 500):
            d = np.abs(P6[i:i + 500, None, :] - S6[None, :, :]).max(axis=2)
            cnt[i:i + 500] = (d <= tol).sum(axis=1)
        rows.append([glab, f"within {tol} band(s)", f"{m_(cnt == 0, wp):.1%}", f"{m_(cnt < 25, wp):.1%}",
                     f"{np.median(cnt):.0f}"])
        res.setdefault("C_nn", {})[f"{glab} tol {tol}"] = [m_(cnt == 0, wp), m_(cnt < 25, wp), float(np.median(cnt))]
    # benchmark: SCF test records of the same group against the SCF training records
    tt = np.load(f"{D}/tokens_test.npy")
    mst = ((te.source == "SCF") & te.wealth_observed.astype(bool) & (te.g_inc == gi)).to_numpy()
    T6 = tt[mst][:, cols6].astype(np.int32)
    T6 = np.where(T6 < 0, -100, T6)
    wt_ = te.weight.to_numpy(float)[mst]
    for tol in (1, 2, 4):
        cnt = np.zeros(len(T6), dtype=np.int64)
        for i in range(0, len(T6), 500):
            d = np.abs(T6[i:i + 500, None, :] - S6[None, :, :]).max(axis=2)
            cnt[i:i + 500] = (d <= tol).sum(axis=1)
        rows.append([glab + ", benchmark: SCF test records", f"within {tol} band(s)", f"{m_(cnt == 0, wt_):.1%}",
                     f"{m_(cnt < 25, wt_):.1%}", f"{np.median(cnt):.0f}"])
say("SCF training records (wealth observed, both waves, all implicates) whose rank bands on six incomes "
    "(wages, Schedule C, partnership and S corp, long-term gains, qualified dividends, interest) are all "
    "within a tolerance of a PUF record's")
say()
table(["PUF group (shared income)", "Tolerance", "PUF share with no SCF neighbour", "PUF share with fewer than 25 SCF rows (about 5 households)",
       "Median SCF rows per PUF record"], rows)

# C vs X by dominant income source within the PUF top 0.1% (AGI), and SCF truth by the same split
say("### Top 0.1% split by the largest income item")
say()


def dominant(f):
    parts = {k: comp_val(f, COMP[k]) for k in ("wages", "partnership and S corp", "capital gains")}
    other = f.inc.to_numpy() - sum(parts.values())
    arr = np.vstack([parts["wages"], parts["partnership and S corp"], parts["capital gains"], other])
    return np.array(["wages", "partnership and S corp", "capital gains", "other"])[arr.argmax(0)]


pt = pw[pw.g_inc == 4]
dp = dominant(pt)
st_ = tr[scf_obs & (tr.g_inc == 4).to_numpy()]
ds = dominant(st_)
rows = []
for dname in ("wages", "partnership and S corp", "capital gains", "other"):
    m = dp == dname
    w = pt.weight.to_numpy(float)[m]
    idx = pt.index.to_numpy()[m]
    row = [dname, f"{w.sum() / pt.weight.sum():.0%}"]
    ms_ = ds == dname
    ws = st_.weight.to_numpy(float)[ms_]
    row += [f"{ws.sum() / st_.weight.sum():.0%}", fm(m_(nw(st_[ms_]), ws)), int(ms_.sum())]
    for mlab in files:
        vals = [m_(per.loc[idx].to_numpy(), w) for per in PUFNW[mlab]]
        row.append(fm(np.mean(vals)))
    rows.append(row)
table(["Largest item (top 0.1% of shared income)", "PUF weight share", "SCF weight share",
       "SCF own mean net worth (waves pooled)", "SCF rows"] + [f"PUF completed, {k}" for k in files], rows)

# ================= D =================
say("## D. Rank-band decoding of the wealth variables")
say()
q = pd.read_parquet(f"{D}/quantiles.parquet")
cache = gt._qf_cache(q)
ntop99 = sum(1 for t in V["equities"]["tokens"] if t["kind"] == "pos" and t["lo"] >= 0.99 - 1e-9)
ntop999 = sum(1 for t in V["equities"]["tokens"] if t["kind"] == "pos" and t["lo"] >= 0.999 - 1e-9)
say(f"Positive bands at or above the 99th percentile of holders: {ntop99} (67-token variables); at or above "
    f"the 99.9th: {ntop999}. 80-token variables: " + str(
        sum(1 for t in V["pass_throughs"]["tokens"] if t["kind"] == "pos" and t["lo"] >= 0.99 - 1e-9)) +
    " and " + str(sum(1 for t in V["pass_throughs"]["tokens"] if t["kind"] == "pos" and t["lo"] >= 0.999 - 1e-9)))
say()
s = tr[scf_obs].reset_index(drop=True)
ts = tok[scf_obs]
rng = np.random.default_rng(7)
# decode the SCF training records' own tokens (their own wave)
dec = gt.decode(ts, meta, s, q, rng, cache=cache)
rows = []
for k in ["equities", "pass_throughs", "primary_home", "other_home", "bonds", "trusts", "cash", "other_fin",
          "re_fund", "dc", "kg_pass_throughs", "kg_other"]:
    c = V[k]["column"]
    t = ts[:, c]
    w = s.weight.to_numpy(float)
    truev, decv = s[k].to_numpy(float), dec[k].to_numpy(float)
    toptok = [tt["token"] for tt in V[k]["tokens"] if tt["kind"] == "pos" and tt["lo"] >= 0.999 - 1e-9]
    m = np.isin(t, toptok)
    m2 = np.isin(t, [tt["token"] for tt in V[k]["tokens"] if tt["kind"] == "pos" and tt["lo"] >= 0.99 - 1e-9])
    rows.append([k, int(m.sum()), fm(m_(truev[m], w[m])), fm(m_(decv[m], w[m])), fm(np.mean(truev[m])) if m.any() else "n/a",
                 fm(np.mean(decv[m])) if m.any() else "n/a",
                 f"{np.sum((decv * w)[m2]) / np.sum((truev * w)[m2]):.3f}" if m2.any() else "n/a",
                 f"{np.sum(decv * w) / np.sum(truev * w):.3f}"])
table(["Wealth item", "SCF rows in top three bands", "True mean, weighted", "Decoded mean, weighted",
       "True mean, unweighted", "Decoded mean, unweighted", "Decoded/true total, holders above 99th pct",
       "Decoded/true total, all"], rows)
rows = []
for gi in SHOW + ["top1"]:
    lab = "Top 1%" if gi == "top1" else GL[gi]
    m = (s.g_inc >= 3).to_numpy() if gi == "top1" else (s.g_inc == gi).to_numpy()
    w = s.weight.to_numpy(float)[m]
    a, b = nw(s[m]), nw(dec[m])
    rows.append([lab, fm(m_(a, w)), fm(m_(b, w)), f"{m_(b, w) / m_(a, w):.3f}", fm(wq(a, w, .5)), fm(wq(b, w, .5))])
say("SCF training records' net worth from their own tokens decoded back to dollars (own wave), waves pooled")
say()
table(["Shared-income group", "True mean", "Decoded mean", "Ratio", "True median", "Decoded median"], rows)

os.makedirs(L, exist_ok=True)
open(f"{L}/locate.md", "w").write("\n".join(OUT) + "\n")
json.dump(res, open(f"{L}/locate.json", "w"), indent=1, default=float)
say("written " + f"{L}/locate.md")
