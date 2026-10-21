"""Diagnose why the model's age-range relationships are off (test 1).

Compares test records, training records and generated files on the
demographic variables. Prints aggregate tables only.
"""
import numpy as np
import pandas as pd

S = "/nfs/roberts/scratch/pi_nrs36/jar335/puf_generator"
files = {
    "test": f"{S}/data/values_test.parquet",
    "train": f"{S}/data/values_train.parquet",
    "small": f"{S}/model/synthetic_small.parquet",
    "base": f"{S}/model/synthetic_base.parquet",
}
import sys
for arg in sys.argv[1:]:
    k, v = arg.split("=", 1)
    files[k] = v
d = {k: pd.read_parquet(v) for k, v in files.items()}
pd.set_option("display.width", 200, "display.max_columns", 30)


def wshare(df, by):
    w = df.groupby(by, observed=True)["weight"].sum()
    return w / w.sum()


def table(by, cond=None, label=""):
    cols = {}
    for k, df in d.items():
        x = df if cond is None else df[cond(df)]
        cols[k] = wshare(x, by)
    t = pd.DataFrame(cols).fillna(0)
    print(f"\n== {label or by} (weighted shares)")
    print((100 * t).round(2))
    for k in [k for k in d if k != "test"]:
        print(f"   TVD vs test, {k}: {0.5 * (t[k] - t['test']).abs().sum():.4f}")


# 1. Marginal, and within records that have demographics
table("AGERANGE")
table("AGERANGE", lambda x: x.AGERANGE >= 0, "AGERANGE among records with demographics")
table("DSI")
table(["DSI", "AGERANGE"], lambda x: x.AGERANGE >= 0, "DSI x AGERANGE (records with demographics)")

# 2. The missing-demographics rule: real data have AGERANGE missing iff weight < 200
print("\n== Missing demographics vs weight < 200 (record counts)")
for k, df in d.items():
    low = df.weight < 200
    miss = df.AGERANGE == -1
    print(f"{k:6s} n={len(df):7d}  low&miss={np.sum(low & miss):7d}  low&present={np.sum(low & ~miss):6d}  "
          f"high&miss={np.sum(~low & miss):6d}  high&present={np.sum(~low & ~miss):7d}")

# 3. Do the three demographic variables go missing together?
print("\n== Missing pattern across AGERANGE, GENDER, EARNSPLIT (record counts)")
for k, df in d.items():
    pat = (df.AGERANGE == -1).astype(int).astype(str) + (df.GENDER == -1).astype(int).astype(str) \
        + (df.EARNSPLIT == -1).astype(int).astype(str)
    print(k, pat.value_counts().to_dict())

# 4. Earnings split by filing status
table(["MARS", "EARNSPLIT"], label="MARS x EARNSPLIT")

# 5. Age profiles of wages, Social Security and pensions (weighted)
print("\n== Profiles by age range: share with wages / mean wages / share with Social Security / share with pensions")
rows = []
for k, df in d.items():
    for a, g in df.groupby("AGERANGE"):
        w = g.weight
        rows.append({"file": k, "AGERANGE": a, "pop_share": w.sum() / df.weight.sum(),
                     "has_wages": np.average(g.E00200 != 0, weights=w),
                     "mean_wages": np.average(g.E00200, weights=w),
                     "has_SS": np.average(g.E02400 != 0, weights=w),
                     "has_pension": np.average(g.E01500 != 0, weights=w),
                     "has_EITC_kids": np.average(g.EIC > 0, weights=w),
                     "joint": np.average(g.MARS == 2, weights=w)})
p = pd.DataFrame(rows).set_index(["AGERANGE", "file"]).sort_index()
print(p.round(3).to_string())

# 6. Where in the weight distribution is the age-range misfit?
print("\n== AGERANGE TVD vs test, within weight bands (records with demographics)")
bands = [200, 500, 1000, 2000, 1e9]
for k in [k for k in d if k != "test"]:
    out = []
    for lo, hi in zip(bands[:-1], bands[1:]):
        def sh(df):
            x = df[(df.weight >= lo) & (df.weight < hi) & (df.AGERANGE >= 0)]
            s = x.groupby("AGERANGE")["weight"].sum()
            return s / s.sum(), x.weight.sum() / df.weight.sum()
        a, pa = sh(d["test"])
        b, pb = sh(d[k])
        tv = 0.5 * (a.reindex(a.index.union(b.index), fill_value=0)
                    - b.reindex(a.index.union(b.index), fill_value=0)).abs().sum()
        out.append(f"[{lo:.0f},{hi:.0g}) pop {100*pa:.1f}%/{100*pb:.1f}% TVD {tv:.3f}")
    print(k, " | ".join(out))
