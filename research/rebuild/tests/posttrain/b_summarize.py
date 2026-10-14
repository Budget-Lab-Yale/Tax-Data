"""Piece B: summarise method-1 reports as Markdown tables (aggregates only).

    python b_summarize.py report_a.json [report_b.json ...] > summary.md
"""
import json
import sys

BAD = ["unreachable", "above_reach", "block", "below_forced", "joint", "missed", "zero_relaxed"]


def load(p):
    with open(p) as fh:
        return json.load(fh)


def main(paths):
    reps = [(p, load(p)) for p in paths]
    print("| Run | K | KL (nats per unit weight) | Effective draws (weighted mean) | "
          "Weight on records with < 4 effective draws | Cells hit (status active) | "
          "Largest relative error, active cells | Positive cells within 1e-6 / 1e-3 / 1e-2 of target | "
          "Target weight within 1e-3 | Sum of absolute misses, share of all target weight | Solve minutes |")
    print("|" + "---|" * 11)
    for p, r in reps:
        tol = r["positive_cells_within_tolerance"]
        sc = r["status_counts"]
        print(f"| {r.get('tag', p)} | {r['K']} | {r['kl_nats_per_unit_weight']:.3f} | "
              f"{r['effective_draws_weighted_mean']:.2f} | "
              f"{r['share_records_below4_effective_draws_weighted']:.1%} | "
              f"{sc['active']:,} of {r['n_cells'] - sc['zero'] - sc.get('no_target', 0):,} | "
              f"{r['feasible_max_rel_error']:.1e} | "
              f"{tol['1e-06']['share_of_cells']:.1%} / {tol['0.001']['share_of_cells']:.1%} / "
              f"{tol['0.01']['share_of_cells']:.1%} | {tol['0.001']['share_of_target_weight']:.1%} | "
              f"{r['total_abs_miss_share_of_weight'] * r['total_weight'] / sum(r['target_weight_by_status'].values()):.2%} | {r['seconds_total'] / 60:.0f} |")
    print()
    print("Cell status counts:")
    print()
    st = ["active", "zero", "zero_relaxed", "unreachable", "above_reach", "block",
          "below_forced", "joint", "missed", "no_target"]
    print("| Run | " + " | ".join(st) + " |")
    print("|" + "---|" * (len(st) + 1))
    for p, r in reps:
        print(f"| {r.get('tag', p)} | " + " | ".join(f"{r['status_counts'].get(s, 0):,}" for s in st) + " |")
    for p, r in reps:
        sb = r.get("status_by", {})
        for col in ("by_family", "by_variable", "by_bin_label"):
            if col not in sb:
                continue
            print()
            print(f"{r.get('tag', p)}: cells not hit, {col[3:].replace('_', ' ')} "
                  "(counts; share of the positive target weight not hit)")
            print()
            print("| " + col[3:] + " | hit | " + " | ".join(BAD) + " | target weight not hit |")
            print("|" + "---|" * (len(BAD) + 3))
            for k, d in sb[col].items():
                tot = d.get("target_weight_total", 0.0)
                share = d.get("target_weight_infeasible", 0.0) / tot if tot else 0.0
                print(f"| {k} | {d.get('active', 0):,} | " + " | ".join(f"{d.get(s, 0):,}" for s in BAD)
                      + f" | {share:.1%} |")


if __name__ == "__main__":
    main(sys.argv[1:])
