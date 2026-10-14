"""Probe Q: markdown tables from $Q/eval/eval_all.json (aggregates only)."""
import json
import sys
from pathlib import Path

Q = Path("/nfs/roberts/scratch/pi_nrs36/jar335/probes/q")
# arguments: run names; each run's evaluation is in $Q/eval/eval_<name>.json
d = {}
for nm in sys.argv[1:]:
    e = json.load(open(Q / "eval" / f"eval_{nm}.json"))
    for k, v in e.items():
        if k == nm or k not in d:
            d[k] = v
names = sorted([k for k in d if not k.startswith(("yardstick", "truth"))],
               key=lambda n: d[n]["kl_train"])
INC = ["Bottom 50%", "50-90%", "90-99%", "99-99.9%", "Top 0.1%"]

COLS = [("targeted_mean_abs_pp", "Targeted cells (pp)", 2),
        ("hold_inc_age_pp", "Holding rates, income x age (pp)", 2),
        ("nw_inc_age_pct", "Mean net worth, income x age (% of group mean)", 1),
        ("pair_joint_pp", "Joint holding of asset pairs (pp)", 2),
        ("top1_share_pp", "Top 1% share within income group (pp)", 1),
        ("nw_ht_pct", "Mean net worth by household type (%)", 1),
        ("corr_abs_diff", "Rank correlation of net worth with incomes", 3)]


def label(n):
    r = d[n]["report"]
    hid = {"best_noincome": " (incomes hidden)", "best_nothing": " (all inputs hidden)"}.get(n, "")
    return f"{n}{hid}", r["ckpt_epoch"]


for sp in ("train", "test"):
    print(f"\n### {sp} records\n")
    hdr = ["Model", "Epoch", "Completion score, validation (nats/record)", "Tilt size (nats)"] + \
          [c[1] + ": before / after" for c in COLS]
    print("| " + " | ".join(hdr) + " |")
    print("|" + "---|" * len(hdr))
    for n in names:
        x = d[n]
        lab, ep = label(n)
        sc = x["report"]["completion_score"]["val"]["nats_per_record_weighted"]
        row = [lab, str(ep), f"{sc:.2f}", f"{x[f'kl_{sp}']:.2f}"]
        for k, _, p in COLS:
            row.append(f"{x[f'{sp}_before'][k]:.{p}f} / {x[f'{sp}_after'][k]:.{p}f}")
        print("| " + " | ".join(row) + " |")
    ys = d[f"yardstick_truth_halves_{sp}"]
    print("| Yardstick: truth's own sampling error | | | | " +
          " | ".join(f"{ys[k]:.{p}f}" for k, _, p in COLS) + " |")
    if sp == "train" and "best" in d and "yardstick_draw_noise_train" in d["best"]:
        yn = d["best"]["yardstick_draw_noise_train"]
        print("| Yardstick: draw noise (best model, 8 draws) | | | | " +
              " | ".join(f"{yn[k]:.{p}f}" for k, _, p in COLS) + " |")

for sp in ("train", "test"):
    print(f"\n### Mean and median net worth by income group, {sp} records ($ thousands; truth, then before / after)\n")
    tl = d[f"truth_levels_{sp}"]
    print("| Model | " + " | ".join(INC) + " |")
    print("|---|" + "---|" * 5)
    print("| Truth, mean | " + " | ".join(f"{v / 1e3:,.0f}" for v in tl["nw_mean_inc"]) + " |")
    for n in names:
        b = d[n][f"{sp}_before_levels"]["nw_mean_inc"]
        a = d[n][f"{sp}_after_levels"]["nw_mean_inc"]
        print(f"| {label(n)[0]}, mean | " + " | ".join(f"{x / 1e3:,.0f} / {y / 1e3:,.0f}" for x, y in zip(b, a)) + " |")
    print("| Truth, median | " + " | ".join(f"{v / 1e3:,.0f}" for v in tl["nw_median_inc"]) + " |")
    for n in names:
        b = d[n][f"{sp}_before_levels"]["nw_median_inc"]
        a = d[n][f"{sp}_after_levels"]["nw_median_inc"]
        print(f"| {label(n)[0]}, median | " + " | ".join(f"{x / 1e3:,.0f} / {y / 1e3:,.0f}" for x, y in zip(b, a)) + " |")

for sp in ("train", "test"):
    print(f"\n### By income group, {sp} records: tilt size (nats), holding-rate error by income x age (pp), mean net worth error by income x age ($ thousands), before / after\n")
    print("| Model | Measure | " + " | ".join(INC) + " |")
    print("|---|---|" + "---|" * 5)
    for n in names:
        x = d[n]
        print(f"| {label(n)[0]} | tilt size | " + " | ".join(f"{v:.2f}" for v in x[f"kl_by_inc_{sp}"]) + " |")
        b, a = x[f"{sp}_before"]["hold_inc_age_pp_by_inc"], x[f"{sp}_after"]["hold_inc_age_pp_by_inc"]
        print(f"| | holding rates (pp) | " + " | ".join(f"{u:.1f} / {v:.1f}" for u, v in zip(b, a)) + " |")
        b, a = x[f"{sp}_before"]["nw_inc_age_abs_dollars_by_inc"], x[f"{sp}_after"]["nw_inc_age_abs_dollars_by_inc"]
        print(f"| | mean net worth ($000) | " + " | ".join(f"{u / 1e3:,.0f} / {v / 1e3:,.0f}" for u, v in zip(b, a)) + " |")
        b, a = x[f"{sp}_before"]["top1_share_pp_by_inc"], x[f"{sp}_after"]["top1_share_pp_by_inc"]
        print(f"| | top 1% share, model minus truth (pp) | " + " | ".join(f"{u:+.1f} / {v:+.1f}" for u, v in zip(b, a)) + " |")
    print("| Truth top 1% share (%) | | " + " | ".join(f"{100 * v:.1f}" for v in d[f'truth_levels_{sp}']['top1_share_inc']) + " |")

print("\n### Named conditional holding rates, train records (model minus truth, pp; before / after)\n")
print("Mortgage among homeowners; equities among pass-through owners; unrealized gain among pass-through owners; other mortgage among other-home owners. Truth: "
      + ", ".join(f"{100 * v:.1f}%" for v in d["truth_levels_train"]["named_conditional"]) + "\n")
for n in names:
    b, a = d[n]["train_before"]["named_conditional_pp"], d[n]["train_after"]["named_conditional_pp"]
    print(f"- {label(n)[0]}: " + "; ".join(f"{u:+.1f} / {v:+.1f}" for u, v in zip(b, a)))
print("\n### KL quantiles per record (train): median / 90th / 99th\n")
for n in names:
    q = d[n]["kl_train_quantiles"]
    print(f"- {label(n)[0]}: {q['0.5']:.2f} / {q['0.9']:.2f} / {q['0.99']:.2f}; train after, expected error mean {d[n]['train_after_expected_error']['mean_share_err_pos'] * 100:.3f}% max {d[n]['train_after_expected_error']['max_share_err_pos'] * 100:.2f}%")
