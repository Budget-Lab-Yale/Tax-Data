---
title: "The latest run of the minimal example: step 34 without dropout, tilted with business targets on file units"
role: status
workstream: rebuild
status: current
updated: 2026-10-19
sot: research/rebuild/design/plan.md
supersedes: []
superseded_by: null
---

# The latest run: step 34 without dropout, tilted with business targets

This file names the current run and says where every piece of it is. When
a newer run replaces it, edit this file to point at the new one.

John chose this on 19 October:
- the model trained without dropout (D0, with D0s2 and D0s3 as the other
  seeds);
- the 2019 tilt with the SOI business targets in six AGI groups, the
  starting weights pre-scaled by filing status;
- the wealth count targets on the file's units (IRS returns plus
  non-filers), not the SCF's own counts (plan decision 17, changed by John
  on 19 October).

**The business-target rule is provisional.** John will settle it "based on
vibes and judgment later" (`36_RESULTS.md`, open questions).

Write-ups:
- `34_RESULTS.md`: the business income split, and why rare holders came out
  short (dropout), sections 3–5.
- `36_RESULTS.md`: the business targets.
- `37_real_vs_gen.py` and `$R/step37/real_vs_gen.md`: the real PUF tilted
  to the same targets, as a yardstick.
- `HANDOFF_2026-10-18.md`, section 7: the day's summary.

**What changed since step 31** (18 October, `31_RESULTS.md`), in order:
1. Table E33, then E34: more PUF items. Business income split into its
   eighteen PUF parts on the PUF side, with the total computed from them
   (steps 33 and 34). The SCF side is unchanged: still the cell method of
   step 31.
2. Training without dropout (`MINIMAL_DROPOUT=0`). With dropout 0.1, rare
   items came out 10–20% low (`34_RESULTS.md` 4.3).
3. The 2019 tilt with the SOI 2019 business targets: six families, net
   income and net loss, returns and amount, in six AGI groups, 140 targets,
   exact (`MINIMAL_BUSINESS_TARGETS=coarse`).
4. The tilt's starting weights for filers are pre-scaled within joint and
   non-joint returns to the 2019 counts. This is the default since
   19 October (`MINIMAL_PRESCALE`).
5. The wealth count targets are on the file's 187 million units: the SCF's
   share above each line times the file's units. Step 31 used the SCF's own
   143 million units.

## What it is

Scratch = `/nfs/roberts/scratch/pi_nrs36/jar335/minimal/`.

| Piece | What | Where |
|---|---|---|
| Training table | E34: the 2015 PUF with capital gains in four pieces, business income in eighteen parts, more AGI items, and the crude non-filers; SCF waves 2013, 2016, 2019, 2022 | scratch `data/stacked_E34.parquet`, `data/band_edges_E34.json`; `34_tables.py` |
| Settings | the cell method's sheet (step 33's, plus a block that only records the parts); shared settings in `34_env.sh` | `34_settings_scf.json`, `34_env.sh` |
| Model | the transformer of `02_model.py` (4 layers, width 128), dropout 0, label on, 80 passes, fill-ins from pass 3; seeds 20261016–18 | scratch `model/step34/D0` (main), `D0s2`, `D0s3`; `31_train.py` via `34_run.sbatch` |
| Generated file | 400,000 PUF-style records per seed: PUF variables first, business income and line-13 capital gains computed from their parts, then net worth under the SCF label; oversampled at the top of wages, capital gains, S corp nonpassive income and net worth | scratch `gen/gen_puf_stack_s34_d0.parquet` (and `_d0s2`, `_d0s3`); `03_stack.py` |
| Dollars | 2019 dollars: the 2015 PUF cut points grown by the IRS growth per return with each item | scratch `map/V34/` |
| Post-training | the tilt to the 136 IRS targets (exact), the non-filer count (exact), the 140 business targets (exact), and the eight SCF 2019 wealth targets (2%), counts on the file's units, start pre-scaled by filing status | scratch `tilt/step34/s34_d0_file_bus_ps/` (and `s34_d0s2_file_bus_ps/`, `s34_d0s3_file_bus_ps/`); `07_baseline.py` via `34_tilt.sbatch <run> file_units bus_ps`. The same tilt with counts on the SCF's units, for comparison: `s34_<run>_bus_ps/` |
| Report | business parts, combinations, the tilt, net worth before and after, against step 33 | scratch `step34/nodropout/report.md` |

## Headline numbers

Mean of three seeds, range in brackets.

| | This run | Step 31 (cell method, one business item) | Benchmark |
|---|---:|---:|---|
| Total net worth before the tilt | $99.3T (95.1–102.3) | $96.7T | SCF 2019: $96.1T |
| Mean net worth, units with none of interest, dividends, gains, before the tilt | $120,900 | $102,500 | SCF: $220,900 |
| Partnership nonpassive income holders, before the tilt | 1.24% | | PUF: 1.33% |
| Mean business income per unit, before the tilt, ratio to PUF | 0.90 (0.85–0.93) | | |
| Move, 136 IRS targets alone | 0.0076 nats | 0.0063 | |
| Move, 136 + business + wealth, counts on file units | 0.049–0.055 nats (D0, D0s3) | 0.0447 (136 + wealth) | |
| Effective sample kept, same | 90.1% (89.5–90.9) | 95.6% on SCF units | |
| Median after the tilt | $90,800 | $38,100 on SCF units, $90,900 on file units | SCF: $91,100 |
| Top 1% share of net worth after the tilt | 38.4% | | SCF: 38.4% |
| Mean net worth, wages $200k and over, after the tilt | $3.47m | | SCF: $4.36m |
| Mean net worth, units with none of interest, dividends, gains, after the tilt | $108,100 | | SCF: $220,900 |

With counts on the SCF's units instead, the same tilt keeps 93.1% of the
effective sample, and the median falls to $39,400.

The IRS and business targets are met exactly for D0 and D0s3. **D0s2 is
not used.** One cell, estate and trust losses at AGI of $1m and over, is
out of its reach: its generated losses there are 0.03 of SOI's dollars.
On file units its tilt does not settle even with the 2% tolerance
(`36_RESULTS.md`).

## Known gaps

**Untargeted business patterns.** The tilt cannot reach these; they are
the model's job:
- too many units hold both partnership and S corporation items, 1.14–1.33
  of the PUF's share;
- nonpassive losses are low;
- the passive share of partnership and S corp income is a little high;
- section 179 appears too often on loss units.

**Rare holders before the tilt.** Even without dropout, 5–8% too few,
because the model's answers do not fit one joint distribution exactly. A
resampling pass after generation was tried and failed (`34_RESULTS.md`
4.5–4.6). The tilt absorbs this.

**Seed noise.** Mean business income before the tilt is 0.85–0.93 of the
PUF's; total net worth before the tilt is $95–102 trillion.

**Carried over from step 31:**
- the SCF side is still the cell method, which John wants replaced (the
  redesign is on hold);
- the combinations of interest, dividends and gains are off;
- earners of $200,000 and over hold too little net worth before the tilt;
- the non-filers are crude.

**Net worth by group after the tilt.**
- Earners of $200,000 and over: $3.47 million against the SCF's $4.36
  million.
- Units with none of interest, dividends and gains: $108,000 against the
  SCF's $221,000.

**Not run for this model:** the fake survey.

## What comes next

Open, for John:
- **The business-target rule:** the level of detail, thin cells, and
  constructed targets where SOI publishes nothing (`36_RESULTS.md`).
- **Real PUF records in place of synthetic PUF variables.** On the same
  targets the real PUF costs no more to tilt, and it gets the untargeted
  business patterns right (`$R/step37/real_vs_gen.md`). The proposed next
  test is the real PUF with model-drawn net worth, against this run. It
  changes the rebuild's premise, so it is John's decision.
- **Pre-scaling by age as well as filing status,** once records carry age.
  It matters more for distant target years.
- The translation redesign (on hold) and the real non-filer work.
