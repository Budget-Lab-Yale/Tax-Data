---
title: "The latest run of the minimal example: v4c"
role: status
workstream: rebuild
status: current
updated: 2026-10-16
sot: research/rebuild/design/plan.md
supersedes: []
superseded_by: null
---

# The latest run: v4c

This file names the run that is current and says where every piece of it
is. When a newer run replaces it, edit this file to point at the new one.
The full write-up, with v4b beside it, is the last section of
`15_BASELINE_V4.md` ("v4c: PUF variables first, one base for the wealth
counts").

v4c is v4b with John's two decisions of 16 October: every PUF variable is
drawn before net worth (plan decision 9), and every wealth count target is
on one base (decision 17). The model and the maps are v4b's.

## What it is

| Piece | What | Where (scratch = `/nfs/roberts/scratch/pi_nrs36/jar335/minimal/`) |
|---|---|---|
| Training table | E4: 2015 PUF and 2016 SCF wave, seven variables, the SCF's wages, interest, dividends and capital gains placed on the PUF's dollar scale (plan decision 15) | scratch `data/stacked_E4.parquet`, `data/band_edges_E4.json`; built by `01_variants.py` |
| Model | Transformer, 4 layers, width 128, trained on all records except a 2% held-back slice, 80 passes; the same as v4 and v4b, not retrained | scratch `model/V4/`; `02_model.py` with `MINIMAL_TABLE=E4 MINIMAL_RUN=V4 MINIMAL_ALL_RECORDS=1` |
| Generated file | 400,000 PUF-style records in four files of 100,000 (seeds 31 to 34), each oversampling the top 1% of wages, capital gains, other income or net worth tenfold. Order: married, dependents and the five incomes first, in a random order for each record, under the PUF label, with no net worth slot; then net worth under the SCF label with other income left out (decisions 9 and 16) | scratch `gen/gen_puf_stack_v4c.parquet`, `gen/gen_meta_v4c.json`; `03_stack.py` with `STACK_VERSION=v4c MINIMAL_STACK_LABEL=scf MINIMAL_STACK_FOREIGN=absent MINIMAL_ORDER=own_first MINIMAL_STACK_DIR=<scratch>/gen` |
| Plain files for the checks | 200,000 records, seed 3, and the same with seed 35, same rule, no oversampling | scratch `gen/V4c/gen_puf_with_net_worth_ownerlabel_absent_ownfirst.parquet`, `gen/gen_plain_seed35_v4c.parquet` |
| Dollars | Positions turned into 2019 dollars with the 2015 PUF cut points grown by the IRS growth in each item | scratch `map/V4/`; `15_maps.py`, `04_dollars.py` |
| Post-training | Reweighting (the tilt) to 136 IRS targets for 2019, exact, and eight SCF wealth targets for 2019 in dollars, each to about 2%. Every count target is the SCF's own weighted count of units above the line (`WEALTH_COUNT_BASE=scf_units`, the default since 2026-10-16; plan decision 17) | scratch `tilt/v4c_scfbase/`; `07_baseline.py` with `STACK_VERSION=v4c TILT_TAG=v4c_scfbase WEALTH_UNITS=all WEALTH_COUNT_BASE=scf_units` |
| Checks | Stack against plain file, redraw, untargeted relationships, side-by-side tables against v4b | scratch `checks/V4c/`; `15_checks.py`, `15_v4c_report.py` (`checks/V4c/report_tables.md`) |

**The count base: SCF units (John, 2026-10-16).** Every wealth count
target is the SCF's own weighted count above the line, on its 143 million
units. This assumes the 15 million returns the IRS counts beyond the SCF's
units all sit below every line (for example, dependents filing their own
returns). It is a stopgap: once the SCF's tax units are built the way the
IRS's are, the two counts should agree and the question goes away (plan,
decision 17). The same stack tilted with every count on the IRS return
count is kept for comparison: scratch `tilt/v4c_allunits/`
(`WEALTH_COUNT_BASE=irs_returns`).

## Headline numbers

| | v4c | v4b | Benchmark |
|---|---:|---:|---|
| Net worth before the tilt | $91.2 trillion | $88.2 trillion | SCF 2019: $89.1 trillion |
| Top 1% share before the tilt | 40.0% | 37.0% | SCF: 38.4% |
| Median net worth before the tilt | $68,400 | $78,500 | SCF: $84,500 |
| Move, 136 IRS targets alone | 0.0068 nats | 0.0093 nats | |
| Move, 136 IRS plus wealth targets | 0.0139 nats | 0.0243 nats | |
| Records effectively kept by the tilt | 97.1% | 95.7% | |
| IRS targets hit after the tilt | 136 of 136, exactly | 136 of 136 | |
| IRS targets within 2 standard errors on a redrawn equal-weight file | 134 of 136 | 134 of 136 | |
| Wealth targets | all eight within 0.01% | two at ±1.7%, the rest exact | |
| Returns with net worth between $928,000 and $992,000 (2015 dollars) | 0.594% | 0% | SCF: 0.66% of units |
| Non-joint returns with two or more dependents, after the tilt | 11.6% | 8.7% | PUF: 11.6% |
| Median net worth after the tilt | $62,200 | $84,500 | SCF: $84,500 (of 143 million units) |
| Mean net worth above the SCF's 99th percentile line | $23.9 million | $21.7 million | SCF: $23.9 million |

The median falls below the SCF's because the SCF base places the 15 million
extra returns below every line. With the IRS base instead (kept for
comparison): move 0.0182 nats, 96.3% kept, median $84,500, mean above the
99th percentile line $21.7 million.

## Known gaps

- Returns with no wages hold a mean of $1.12 million against $706,000 in
  the SCF. Probably in part a population mismatch: the SCF's no-wage
  units include many who would not file. Not checked.
- Earners above $200,000 hold a mean of $3.89 million after the tilt
  against the SCF's $4.50 million (v4b: $4.04 million; within the
  seed-to-seed range measured in `20_SEPARATE_MODELS.md`).
- The top 0.1% holds 13.0% after the tilt against 14.7%. Not targeted.
- Before the tilt the file has too much wealth at the top and too little at
  the bottom (top 1% share 40.0%, median $68,400). The tilt fixes both.
- The base of the wealth count targets is a stopgap until the SCF's tax
  units are built to match the IRS's (above).
- The stack ran on a different GPU type from v4b's (RTX A5000 against
  H200), which adds some noise to the v4b comparison.

## What comes next

Nonfilers (`README.md`, "What comes next"). The choice between the two
count bases connects to it: it turns on who the 15 million returns beyond
the SCF's units are.
