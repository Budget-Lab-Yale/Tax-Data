"""Probe S: collect every finished run into markdown tables (one row per
trained model and seed). Reads t_compare.py output (generated files against
the PUF), t_diag.py output (step-1 AGI questions), s_gennll.py output (real
PUF records scored under generation in four orders), training logs (PUF
any-order validation loss) and run files (SCF wealth completion score).

Usage: python s_summary.py <compare json> [<compare json> ...]
Aggregates only.
"""
import csv
import glob
import json
import sys
from pathlib import Path

M = "/nfs/roberts/scratch/pi_nrs36/jar335/scf_joint/model/full_v2"
T = "/nfs/roberts/scratch/pi_nrs36/jar335/probes/t"
S = "/nfs/roberts/scratch/pi_nrs36/jar335/probes/s"


def runs():
    out = []
    for s in (1, 2, 3):
        out.append(dict(v="base", s=s, ck=f"{M}/checkpoint_full_v2_w1_seed0{s}.pt",
                        diag=f"{T}/base/diag_base{'' if s == 1 else f'_s{s}'}.json",
                        run=f"{M}/run_full_v2_w1_seed0{s}.json", log=None))
    out.append(dict(v="sched", s=1, ck=f"{T}/sched/checkpoint_sched_seed01.pt",
                    diag=f"{T}/sched/diag_sched_kept.json", run=f"{T}/sched/run_sched_seed01.json",
                    log=f"{T}/sched/train_log_sched_seed01.csv"))
    for s in (1, 2, 3):
        out.append(dict(v="agi50", s=s, ck=f"{T}/mixagi60/checkpoint_mixagi60_seed0{s}.pt",
                        diag=f"{T}/mixagi60/diag_mixagi60_s{s}.json",
                        run=f"{T}/mixagi60/run_mixagi60_seed0{s}.json",
                        log=f"{T}/mixagi60/train_log_mixagi60_seed0{s}.csv"))
    for v, seeds in (("agi10", (1, 2, 3)), ("agi25", (1, 2, 3, 4, 5)), ("agi50", (4, 5)), ("agi100", (1, 2, 3)),
                     ("rnd50", (1, 2, 3)), ("rnd100", (1, 2, 3)), ("two50", (1, 2))):
        for s in seeds:
            out.append(dict(v=v, s=s, ck=f"{S}/{v}/checkpoint_{v}_seed0{s}.pt",
                            diag=f"{S}/{v}/diag_{v}_s{s}.json", gennll=f"{S}/{v}/gennll_{v}_s{s}.json",
                            run=f"{S}/{v}/run_{v}_seed0{s}.json",
                            log=f"{S}/{v}/train_log_{v}_seed0{s}.csv"))
    return out


def jload(p):
    try:
        return json.load(open(p))
    except (FileNotFoundError, json.JSONDecodeError):
        return None


def ck_entry(d, ck):
    if d is None:
        return None
    if ck in d:
        return d[ck]
    ks = [k for k in d if k.endswith(".pt")]
    return d[ks[0]] if len(ks) == 1 else None


def val_puf(log, epoch):
    """PUF any-order validation loss (population weights) at the kept epoch."""
    if not log or not Path(log).exists() or epoch is None:
        return None
    rows = [r for r in csv.DictReader(open(log)) if int(r["epoch"]) == int(epoch)]
    return float(rows[0]["val_pop_PUF"]) if rows and "val_pop_PUF" in rows[0] else None


def f(x, fmt):
    return "-" if x is None else fmt.format(x)


def main():
    comp = {}
    for p in sys.argv[1:]:
        c = jload(p)
        if c:
            comp.update(c)
    gn_prev = jload(f"{S}/tprev/gennll_tprev.json") or {}
    R = runs()
    print("## Step-1 questions, validation loss, wealth completion\n")
    print("| Model | seed | top 1% band given nothing | given first six | given all but AGI | "
          "PUF any-order validation loss | SCF wealth completion (nats/record) | epoch kept |")
    print("|---|---|---|---|---|---|---|---|")
    for r in R:
        dg = ck_entry(jload(r["diag"]), r["ck"])
        rn = jload(r["run"])
        wc = rn["val_completion"]["nll_per_record"] if rn and rn.get("val_completion") else None
        ep = rn["val_completion"].get("epoch") if rn and rn.get("val_completion") else None
        g = lambda k: dg[k]["rank_ge_099"] if dg and k in dg else None  # noqa: E731
        print(f"| {r['v']} | {r['s']} | {f(g('train:none'), '{:.2%}')} | "
              f"{f(g('train:first6'), '{:.2%}')} | {f(g('train:allbutAGI'), '{:.2%}')} | "
              f"{f(val_puf(r['log'], ep), '{:.4f}')} | {f(wc, '{:.2f}')} | {ep if ep else '-'} |")
    for pre, title in (("", "all"), ("top1_", "true AGI in the top 1% band")):
        print(f"\n## Real PUF validation records ({title}) scored under generation "
              "(nats per record; AGI step in brackets)\n")
        print("| Model | seed | AGI first | demographics last | random order 1 | random order 2 | "
              "largest minus smallest |")
        print("|---|---|---|---|---|---|---|")
        for r in R:
            e = ck_entry(jload(r["gennll"]), r["ck"]) if "gennll" in r else gn_prev.get(r["ck"])
            ks = ["agifirst", "demolast", "random1", "random2"]
            if not e or f"{pre}nats_per_record" not in e["val:agifirst"]:
                print(f"| {r['v']} | {r['s']} | - | - | - | - | - |")
                continue
            vals = [e[f"val:{k}"][f"{pre}nats_per_record"] for k in ks]
            print(f"| {r['v']} | {r['s']} | " + " | ".join(
                f"{e[f'val:{k}'][pre + 'nats_per_record']:.3f} "
                f"({e[f'val:{k}'][pre + 'nats_agi']:.3f})" for k in ks)
                + f" | {max(vals) - min(vals):.3f} |")
    print("\n## Whole generated files against the PUF\n")
    print("| Model | seed | order | total AGI | weight above true 99th pct | above 99.9th | "
          "top 1% share | top 0.1% share | bottom-half capital gains $bn | bottom-half "
          "partnership $bn | top 0.1% components $bn | top 0.1% AGI $bn |")
    print("|---|---|---|---|---|---|---|---|---|---|---|---|")
    comps = ["wages_bn", "cap_gains_bn", "partnership_scorp_bn", "dividends_bn", "interest_bn",
             "business_schC_bn"]
    pr = comp.get("PUF training records")
    rows = [("PUF", "", "", pr)] if pr else []
    for r in R:
        for o in ("agifirst", "demolast", "givenagi"):
            rows.append((r["v"], r["s"], o, comp.get(f"{r['v']}_s{r['s']}_{o}")))
    extra = sorted(k for k in comp if k.startswith("noise_"))
    rows += [(k, "", "", comp[k]) for k in extra]
    for v, s, o, c in rows:
        if c is None:
            continue
        gt = c["groups"]["top 0.1"]
        gb = c["groups"]["bottom 50"]
        print(f"| {v} | {s} | {o} | ${c['total_agi_tn']:.2f}tn | {c['w_above_p99']:.2%} | "
              f"{c['w_above_p999']:.3%} | {c['share_above_p99']:.1%} | {c['share_above_p999']:.1%} | "
              f"{gb['cap_gains_bn']:,.0f} | {gb['partnership_scorp_bn']:,.0f} | "
              f"{sum(gt[k] for k in comps):,.0f} | {gt['agi_bn']:,.0f} |")
    draws_tables(comp, R, comps)


def metrics(c, comps):
    gt, gb = c["groups"]["top 0.1"], c["groups"]["bottom 50"]
    return dict(agi=c["total_agi_tn"], w99=c["w_above_p99"], w999=c["w_above_p999"],
                s1=c["share_above_p99"], s01=c["share_above_p999"], cgb=gb["cap_gains_bn"],
                ptb=gb["partnership_scorp_bn"], ct=sum(gt[k] for k in comps), at=gt["agi_bn"])


def draws_tables(comp, R, comps):
    """Every sampling draw of each checkpoint and order: the first draw (label
    <v>_s<s>_<order>) and the repeat draws (noise_<order>4x_<v>_s<s>_g<k>_...)."""
    import numpy as np
    keys = ["agi", "w99", "w999", "s1", "s01", "cgb", "ptb", "ct", "at"]
    fmt = dict(agi="${:.2f}tn", w99="{:.2%}", w999="{:.3%}", s1="{:.1%}", s01="{:.1%}",
               cgb="{:,.0f}", ptb="{:,.0f}", ct="{:,.0f}", at="{:,.0f}")
    per = {}
    for r in R:
        for o in ("agifirst", "demolast"):
            ds = [comp[f"{r['v']}_s{r['s']}_{o}"]] if f"{r['v']}_s{r['s']}_{o}" in comp else []
            ds += [comp[k] for k in sorted(comp)
                   if k.startswith(f"noise_{o}4x_{r['v']}_s{r['s']}_g")]
            if ds:
                per[(r["v"], r["s"], o)] = np.array([[metrics(c, comps)[k] for k in keys]
                                                     for c in ds])
    print("\n## Mean over sampling draws for each checkpoint (draws; range in brackets)\n")
    print("| Model | seed | order | draws | " + " | ".join(
        ["total AGI", "weight above 99th", "above 99.9th", "top 1% share", "top 0.1% share",
         "bottom-half cap gains $bn", "bottom-half partnership $bn",
         "top 0.1% components $bn", "top 0.1% AGI $bn"]) + " |")
    print("|---" * 13 + "|")
    for (v, s, o), a in per.items():
        print(f"| {v} | {s} | {o} | {len(a)} | " + " | ".join(
            f"{fmt[k].format(a[:, i].mean())} ({fmt[k].format(a[:, i].min())} to "
            f"{fmt[k].format(a[:, i].max())})" for i, k in enumerate(keys)) + " |")
    print("\n## By recipe: mean over seeds of checkpoint means; spread between seeds "
          "against sampling noise\n")
    print("Sampling SD: standard deviation across draws of one checkpoint, pooled over "
          "checkpoints. Seed SD: standard deviation across seeds of the checkpoint means, "
          "less the part sampling noise contributes to a mean of that many draws "
          "(0 if negative).\n")
    print("| Recipe | order | seeds | total AGI | sampling SD | seed SD | top 1% share | "
          "sampling SD | seed SD | top 0.1% share | sampling SD | seed SD | "
          "bottom-half cap gains $bn | bottom-half partnership $bn | top 0.1% components $bn |")
    print("|---" * 15 + "|")
    for v in dict.fromkeys(r["v"] for r in R):
        for o in ("agifirst", "demolast"):
            arrs = [a for (vv, s, oo), a in per.items() if vv == v and oo == o]
            if not arrs:
                continue
            means = np.array([a.mean(0) for a in arrs])
            multi = [a for a in arrs if len(a) > 1]
            samp_var = (np.mean([a.var(0, ddof=1) for a in multi], axis=0) if multi
                        else np.full(len(keys), np.nan))
            ndraw = np.mean([len(a) for a in arrs])
            if len(arrs) > 1:
                seed_var = np.clip(means.var(0, ddof=1) - samp_var / ndraw, 0, None)
            else:
                seed_var = np.full(len(keys), np.nan)
            m = means.mean(0)

            def cell(i, f):
                return (f.format(m[i]), "-" if np.isnan(samp_var[i]) else f.format(np.sqrt(samp_var[i])),
                        "-" if np.isnan(seed_var[i]) else f.format(np.sqrt(seed_var[i])))
            out = []
            for i, f in ((0, "{:.2f}"), (3, "{:.2%}"), (4, "{:.2%}")):
                out += cell(i, f)
            out += [f"{m[5]:,.0f}", f"{m[6]:,.0f}", f"{m[7]:,.0f}"]
            print(f"| {v} | {o} | {len(arrs)} | " + " | ".join(out) + " |")


if __name__ == "__main__":
    main()
