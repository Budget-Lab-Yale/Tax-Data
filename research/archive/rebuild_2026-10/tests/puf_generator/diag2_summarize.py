"""Summaries of the diag2 aggregate CSVs (no record-level data)."""
from pathlib import Path

import pandas as pd

D = Path("/nfs/roberts/scratch/pi_nrs36/jar335/puf_generator/diag2")
pd.set_option("display.width", 250, "display.max_rows", 200)
tf = pd.read_csv(D / "teacher_forced_by_variable.csv")
fr = pd.read_csv(D / "free_running_by_variable.csv")
tf = tf[(tf.order == "default") & (tf.variable != "S006")]
fr = fr[fr.variable != "S006"]
fr["model"] = fr.file.str.replace(r"_s\d\d$", "", regex=True)
frm = fr.groupby(["model", "variable"])["tvd_vs_val_coarse"].mean().unstack(0)
t = tf.pivot(index="variable", columns="model", values=["tvd_tf_coarse", "tvd_tf_coarse_noise"])
t.columns = [f"{a.replace('tvd_tf_coarse', 'tf')}_{b}" for a, b in t.columns]
out = t.join(frm.add_prefix("free_"))
order = tf[tf.model == "base"].sort_values("position").variable.tolist()
out = out.reindex(order)
print("Coarse TVD vs validation, by variable (default order). free_ = mean over seeds")
print(out.round(4).to_string())
print("\nMean over the 39 variables after the weight")
print(out.mean().round(4).to_string())
groups = {"categoricals (MARS..FDED)": order[:6], "demographics": order[6:9],
          "AGI and wages": order[9:11], "other incomes": order[11:30],
          "adjustments and deductions": order[30:]}
print("\nMean by group")
print(pd.DataFrame({g: out.loc[v].mean() for g, v in groups.items()}).T.round(4).to_string())
cal = tf[["model", "variable", "mean_top_prob", "top_correct"]]
cal = cal[cal.variable.isin(["MARS", "XTOT", "AGERANGE", "E00100", "E00200", "GENDER", "EARNSPLIT"])]
print("\nTop-choice calibration (default order, given earlier variables)")
print(cal.round(3).to_string())
tfa = pd.read_csv(D / "teacher_forced_by_variable.csv")
print("\nAGERANGE and E00200 in the age-after-AGI-and-wages order")
print(tfa[tfa.variable.isin(["AGERANGE", "E00200", "E00100"])].round(4).to_string())
