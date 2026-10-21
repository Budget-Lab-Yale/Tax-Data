"""Collect the headline measures of the noise/order/training-set batch
(evaluate.py outputs in $S/eval/<label>/) into one table. Aggregates only.
Prints markdown tables and writes $S/diag2/batch_table.csv."""
import json
from pathlib import Path

import pandas as pd

S = Path("/nfs/roberts/scratch/pi_nrs36/jar335/puf_generator")
E = S / "eval"
files = [f"{m}_seed{s:02d}" for m in ("small", "base", "large") for s in (9, 10, 11, 12)]
files += ["base_demoglast", "base_gibbs2"]
labels = []
for f in files:
    labels += [(f, "test", f), (f, "train", f + "_vstrain")]
labels += [(f"{m}_s0_vs_s1", "seed 09 file", f"{m}_s0_vs_s1") for m in ("small", "base", "large")]
TOPV = {"E00100": "agi", "E00200": "wages", "P23250": "ltcg", "E00600": "div"}

rows = []
refs = {}
for f, ref, lab in labels:
    p = E / lab / "summary.json"
    if not p.exists():
        continue
    s = json.load(open(p))
    r = s[lab]
    top = pd.read_csv(E / lab / "top_shares.csv")
    t01 = top[top.group == "top0.1"].set_index("variable")
    row = dict(file=f, against=ref, pair_med=r["pair_tvd_median"], pair_p90=r["pair_tvd_p90"],
               triple_mean=r["triple_tvd_mean"], patterns=r["pattern_tvd"],
               auc=r["classifier_auc"],
               weight_ratio=s["total_weight_ratio_to_train"][lab])
    for v, n in TOPV.items():
        row[f"top01_{n}"] = t01.loc[v, lab]
    rows.append(row)
    # the reference itself and the floor (real versus real) once per reference
    if ref not in refs:
        fl = s["floor"]
        rr = dict(file="floor (train vs test records)", against=ref, pair_med=fl["pair_tvd_median"],
                  pair_p90=fl["pair_tvd_p90"], triple_mean=fl["triple_tvd_mean"],
                  patterns=fl["pattern_tvd"], auc=fl["classifier_auc"], weight_ratio=float("nan"))
        rf = dict(file=f"reference records ({ref})", against=ref)
        for v, n in TOPV.items():
            rr[f"top01_{n}"] = t01.loc[v, "floor"]
            rf[f"top01_{n}"] = t01.loc[v, "test"]
        refs[ref] = (rr, rf)

for ref, (rr, rf) in refs.items():
    # in --reference runs the "floor" is the training records against the
    # reference, so it is only a real-versus-real floor when the reference
    # is the test records
    if ref == "test":
        rows.append(rr)
    if ref in ("test", "train"):
        rows.append(rf)
df = pd.DataFrame(rows)
df.to_csv(S / "diag2" / "batch_table.csv", index=False)
pd.set_option("display.width", 250, "display.max_rows", 200)
print(df.round(4).to_markdown(index=False) if hasattr(df, "to_markdown") else df.round(4).to_string())
# spread across the four seeds per model
num = [c for c in df.columns if c not in ("file", "against")]
d2 = df[df.file.str.contains("_seed")].copy()
d2["model"] = d2.file.str.split("_").str[0]
g = d2.groupby(["model", "against"])[num]
print("\nMean over the four seeds")
print(g.mean().round(4).to_string())
print("\nRange (max minus min) over the four seeds")
print((g.max() - g.min()).round(4).to_string())
print("\nseeds per group", g.size().to_dict())
