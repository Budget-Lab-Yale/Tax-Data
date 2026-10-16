---
title: "Minimal example: does the joint model add anything over separate models plus calibration?"
role: evidence
workstream: rebuild
status: current
updated: 2026-10-15
sot: research/rebuild/design/plan.md
supersedes: []
superseded_by: null
---

# Does the joint model add anything over separate models plus calibration?

John asked on 15 October whether the whole process reduces to "the PUF,
plus separate models for each source trained independently, plus ordinary
calibration." In v4b the PUF variables are drawn under the PUF label, net
worth is drawn under the SCF label with only the SCF's slots visible, and
then the file is tilted. That is statistical matching followed by
calibration. This note tests whether training one model on both sources
changes the result.

## Short answer

1. **For the PUF variables, the joint model adds nothing measurable.** A
   model trained on the PUF records alone predicts held-back PUF records
   exactly as well as V4, and generates the same holder shares, joint
   share, dependents and dividend and capital gains relationships.
2. **For net worth on PUF records, the joint model does differ, by more
   than training noise.** Net worth drawn from a model trained on the SCF
   alone puts 1.25 to 1.37 times the SCF's total wealth on the PUF-style
   file before the tilt. The joint model, drawn in the same order (C),
   puts 1.04 to 1.12 times. The gap is two to three times the difference
   between two training seeds. After the tilt the totals match by
   construction, but the separate model leaves wealth too flat across
   wage groups: earners above $200,000 hold a mean of $2.7 to $3.8
   million, against $3.9 to $4.2 million with the joint model and $4.5
   million in the SCF. The joint model also predicts held-back
   SCF records better, by 0.09 to 0.14 nats per record.
3. **The draw order explains the dependents gap.** Drawing married and
   dependents before net worth (C) instead of after (A, v4b) raises
   non-joint returns with two or more dependents from 8.7% to 11.4%
   (second seed: 9.4% to 11.9%). The PUF has 11.6%. Wealth is about the
   same either way.

So in arm C's order the procedure is matching plus calibration in form,
with one model doing both halves of the matching. What the joint training
buys is a better net worth model on PUF-style records, where the SCF alone
is thin. Two separate SCF-only models were tried, and both were worse than
the joint one. A separate SCF model tuned more carefully (for example,
stopped at its best held-back pass) was not tried.

## What was run

All arms use table E4, the V4 settings (4 layers, width 128, every record
except the same 2% held-back slice, keep the last pass), the same
four-file stack (100,000 records each, seeds 31 to 34, tenfold
oversampling of the top 1% of wages, capital gains, other_agi and net
worth, with weights that undo it exactly), the same V4 maps to dollars,
and the same tilt: the 136 IRS targets for 2019 (exact) plus the eight
all-units 2019 SCF wealth targets as dollar lines (each soft at 2%).

| Arm | PUF variables drawn from | Net worth drawn from | Order |
|---|---|---|---|
| A (v4b) | V4, PUF label | V4, SCF label, other_agi left out | incomes, **net worth**, married, dependents |
| B | B1: a model trained on the PUF records of E4 only, 80 passes | B2: a model trained on the SCF records of E4 only, 80 passes | incomes, married, dependents, **net worth** |
| B long | B1 | B2 trained for 516 passes, the same number of gradient steps as V4 | as B |
| C | V4, PUF label | V4, SCF label, other_agi left out | as B |

In B and C the PUF variables are drawn exactly as a PUF training record
looks: the record has no net worth slot at all. Net worth is then drawn
from a record that looks exactly like an SCF training record: SCF label,
married, dependents, wages, interest, dividends and capital gains
visible, other_agi absent. (In A, the income draws see a masked net worth
slot on a PUF record, and married and dependents are drawn after net
worth, under the PUF label, with net worth visible. PUF records never have
net worth, so that is a combination the model never saw in training.)

**Why "B long".** With the same 80 passes, B2 takes 9 gradient steps per
pass against V4's 58, so about one seventh as many in all. B2's training
loss on SCF net worth was 3.941 nats against V4's 3.893 on the same
records, so it had not fitted its own data as well. B long gives B2 the
same number of steps as V4. It then overfits: its held-back loss is
lowest at pass 72 (second seed: 53) and rises steadily after that. Both are
reported. The held-back records were not used to choose either.

**A second training seed.** Every model (B1, B2, B2 long, V4) was trained
again with a second seed, and every arm was rebuilt from it. "A seed 2"
is v4b's exact procedure applied to V4 seed 2. All training ran on the
same GPU type as V4 (NVIDIA A40), so the seed-to-seed differences below
are training noise alone, not differences between GPU types.

Code (all new, nothing existing was edited): `20_tables.py` (the PUF-only
and SCF-only tables), `20_train.py` (02_model.py with a training-seed
setting), `20_generate.py` (the stacks), `20_tilt.py` (07_baseline.py
pointed at these stacks), `20_heldback.py`, `20_diag.py`, `20_report.py`,
`20_run_gpu.sbatch`, `20_run_cpu.sbatch`. Outputs under
`/nfs/roberts/scratch/pi_nrs36/jar335/minimal/`: tables
`data/stacked_E4puf.parquet`, `data/stacked_E4scf.parquet` (with copies
of E4's band edges); models `model/S20_PUF`, `model/S20_SCF`,
`model/S20_SCF_long` and their `_seed2` versions, `model/S20_V4_seed2`;
stacks `gen/S20/{seed1,seed2,long_seed1,long_seed2}`,
`gen/C20/{seed1,seed2,A_seed2}`; tilts under `tilt/S20/` and `tilt/C20/`
in matching folders; all tables in `checks/S20/report_tables.md`
(A against C only: `checks/C20/order_tables.md`), held-back scores in
`checks/S20/heldback.csv` and `heldback_differences.csv`, the diagnostic in
`checks/S20/diag_expected_net_worth.csv`. Slurm jobs: GPU 29706027,
29706028, 29708338 (26 GPU-minutes in all); tilts 29706870, 29706871,
29706883 to 29706885, 29708339, 29708340; held-back 29708369; diagnostic
29708370; tables 29708371.

In every table, "seed 2" is the same arm built from the second training
seed. Net worth is in 2015 dollars. The SCF column is the 2019 wave, all
units, with wages put in 2015 dollars for the wage groups.

## 1. Held-back loss: does training jointly help predict each source's own variables?

Cross-entropy in nats on the same held-back records (4,124 PUF, 710 SCF,
about 140 SCF households), lower is better. "Whole record" is the record's
joint log-probability in the order of generation (incomes in random order,
then married, then dependents, then net worth on SCF records; four orders
averaged). Standard errors are paired and treat households as the
sampling unit.

| | V4 | separate | separate minus V4 (standard error) | V4 seed 2 | separate seed 2 | separate minus V4, seed 2 (standard error) |
|---|---:|---:|---:|---:|---:|---:|
| PUF records, whole record (separate = B1) | 11.128 | 11.128 | −0.000 (0.003) | 11.127 | 11.130 | +0.004 (0.003) |
| SCF records, whole record (separate = B2) | 10.528 | 10.664 | **+0.136 (0.057)** | 10.547 | 10.632 | +0.085 (0.054) |
| SCF records, whole record (separate = B2 long) | 10.528 | 11.207 | +0.680 (0.101) | 10.547 | 11.217 | +0.669 (0.107) |
| SCF net worth, everything else visible (B2) | 3.673 | 3.676 | +0.003 (0.039) | 3.696 | 3.668 | −0.028 (0.038) |
| SCF net worth, everything else visible (B2 long) | 3.673 | 4.024 | +0.351 (0.060) | 3.696 | 4.040 | +0.344 (0.062) |

- **PUF.** No variable differs by more than about 0.005 nats, and the whole
  record by less than the seed-to-seed change of V4 itself. The SCF adds
  nothing to the PUF's own relationships, which is expected: the PUF has
  six times as many records and the SCF covers the same shared variables.
- **SCF.** The joint model is better on the SCF's shared variables:
  married (+0.03 for B2), dependents (+0.04), interest (+0.03) and
  dividends (+0.02). These are the relationships the PUF has plenty of
  data on. For net worth given everything else, the two are equal within a
  standard error of 0.04, so the held-back slice is too small to tell
  them apart on wealth itself.
- **B long** fits its training records more closely and does much worse on
  held-back records (+0.68 nats per record). Its training loss falls to
  1.45 while its held-back loss rises to 1.61: 516 passes over the five
  implicates of each SCF household fit the training households too
  closely. The joint model takes as many gradient steps without this,
  because most of its steps are spent on the PUF.

## 2. Wealth before the tilt

| | SCF 2019 | A | B | B long | C | A seed 2 | B seed 2 | B long seed 2 | C seed 2 |
|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| total ($ trillion) | 89.1 | 88.2 | 115 | 111 | 92.4 | 95.6 | 122 | 113 | 100 |
| ratio to the SCF | 1.00 | 0.99 | **1.29** | 1.25 | **1.04** | 1.07 | **1.37** | 1.26 | **1.12** |
| mean | 623,000 | 559,000 | 726,000 | 705,000 | 586,000 | 606,000 | 773,000 | 714,000 | 635,000 |
| median | 84,500 | 78,500 | 78,800 | 71,100 | 68,600 | 71,800 | 73,100 | 70,500 | 66,100 |
| top 1% share (%) | 38.4 | 37.0 | 41.1 | 40.9 | 40.7 | 38.8 | 43.1 | 42.7 | 40.9 |
| top 0.1% share (%) | 14.7 | 14.6 | 14.2 | 17.3 | 16.9 | 15.8 | 17.0 | 18.9 | 17.0 |
| bottom 50%, miss in rank form (%) | | −18.9 | −9.0 | −39.1 | −41.9 | −26.1 | −22.9 | −39.2 | −43.4 |

The generated file has 158 million returns and the SCF 143 million units,
nonfilers included, so the ratio compares different populations and is a
rough guide only.

Training noise: within each arm, the two seeds differ by 0.08 in the ratio
(A, B and C alike; B long 0.01). The gap between separate and joint, in
the same order, is 0.25 in both seeds (B against C), or 0.21 and 0.14
with B long. That is two to three times the seed-to-seed difference, and
more than the 0.1 known between GPU types.

## 3. Wealth by wage group

Mean net worth ($):

| wages | SCF 2019 | A | B | B long | C | A seed 2 | B seed 2 | B long seed 2 | C seed 2 |
|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| **before the tilt** | | | | | | | | | |
| 0 | 706,000 | 1,190,000 | 1,380,000 | 1,360,000 | 1,200,000 | 1,330,000 | 1,610,000 | 1,420,000 | 1,380,000 |
| 25-50k | 148,000 | 207,000 | 356,000 | 298,000 | 204,000 | 219,000 | 340,000 | 284,000 | 228,000 |
| 50-100k | 333,000 | 403,000 | 674,000 | 561,000 | 408,000 | 420,000 | 628,000 | 553,000 | 430,000 |
| 100-200k | 922,000 | 887,000 | 1,310,000 | 1,210,000 | 902,000 | 971,000 | 1,350,000 | 1,150,000 | 990,000 |
| 200k+ | 4,500,000 | 3,710,000 | 3,290,000 | 4,200,000 | 4,220,000 | 4,300,000 | 3,990,000 | 4,740,000 | 4,150,000 |
| **after the tilt** | | | | | | | | | |
| 0 | 706,000 | 1,190,000 | 1,070,000 | 1,060,000 | 1,100,000 | 1,180,000 | 1,120,000 | 1,110,000 | 1,150,000 |
| 1-25k | 203,000 | 200,000 | 219,000 | 210,000 | 214,000 | 202,000 | 205,000 | 204,000 | 211,000 |
| 25-50k | 148,000 | 190,000 | 254,000 | 228,000 | 192,000 | 190,000 | 232,000 | 216,000 | 198,000 |
| 50-100k | 333,000 | 372,000 | 497,000 | 435,000 | 382,000 | 364,000 | 454,000 | 414,000 | 383,000 |
| 100-200k | 922,000 | 844,000 | 1,010,000 | 940,000 | 874,000 | 888,000 | 998,000 | 916,000 | 881,000 |
| 200k+ | 4,500,000 | 4,040,000 | 2,710,000 | 3,720,000 | 4,220,000 | 4,000,000 | 3,050,000 | 3,780,000 | 3,870,000 |

Median net worth after the tilt ($):

| wages | SCF 2019 | A | B | B long | C | A seed 2 | B seed 2 | B long seed 2 | C seed 2 |
|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| 0 | 109,000 | 232,000 | 194,000 | 233,000 | 236,000 | 236,000 | 204,000 | 241,000 | 239,000 |
| 25-50k | 26,000 | 38,300 | 42,600 | 37,500 | 36,700 | 40,400 | 42,500 | 40,400 | 37,400 |
| 50-100k | 109,000 | 135,000 | 135,000 | 132,000 | 126,000 | 131,000 | 132,000 | 129,000 | 130,000 |
| 200k+ | 1,680,000 | 1,430,000 | 1,000,000 | 1,350,000 | 1,340,000 | 1,430,000 | 1,040,000 | 1,330,000 | 1,330,000 |

Before the tilt, C's mean is closer to the SCF's than B's in all six wage
groups, in both seeds. The separate model puts too much wealth on earners
from $25,000 to $200,000 and, in B, too little above $200,000. The tilt
fixes totals and the top dollar lines, not the wage gradient, so B's
pattern survives it. B long is between B and C. The zero-wage group is
too rich in every arm ($1.06 to $1.19 million against $706,000), so that
problem does not come from joint training or from draw order.

Share of units above $1 million after the tilt, earners above $200,000:
SCF 66.7%; A 63.3% and 63.5%; B 50.1% and 51.4%; B long 61.7% and 60.6%;
C 60.9% and 62.0%.

## 4. The move and the effective sample kept

| | A | B | B long | C | A seed 2 | B seed 2 | B long seed 2 | C seed 2 |
|---|---:|---:|---:|---:|---:|---:|---:|---:|
| move, 136 IRS targets alone (nats) | 0.0093 | 0.0069 | 0.0069 | 0.0061 | 0.0144 | 0.0074 | 0.0074 | 0.0075 |
| move, 136 plus wealth (nats) | 0.0243 | 0.0244 | 0.0311 | **0.0208** | 0.0331 | 0.0272 | 0.0341 | **0.0244** |
| of which wealth (nats) | 0.0151 | 0.0175 | 0.0242 | 0.0147 | 0.0187 | 0.0198 | 0.0267 | 0.0169 |
| effective sample kept, 136 alone (%) | 98.0 | 98.4 | 98.6 | 98.7 | 96.9 | 98.1 | 98.4 | 98.3 |
| effective sample kept, 136 plus wealth (%) | 95.7 | 95.3 | 94.4 | 96.3 | 93.8 | 94.5 | 93.7 | 95.5 |

All tilts hit the 136 IRS targets (largest gap 0.00009%, in B seed 1,
where the solver stopped just short of its one-in-ten-million tolerance)
and the wealth dollar lines as in v4b. In both seeds, C has the smallest
move with the wealth targets. A's move for the 136 targets alone is the
largest, because A has too many joint returns before the tilt (36.8% and
38.0%, against 35.2% and 36.1% in C; IRS 34.7%).

## 5. The redraw (200,000 equal-weight records)

| | A | B | B long | C | A seed 2 | B seed 2 | B long seed 2 | C seed 2 |
|---|---:|---:|---:|---:|---:|---:|---:|---:|
| IRS targets within 2 standard errors (of 136) | 134 | 128 | 132 | 130 | 129 | 129 | 133 | 132 |
| largest \|z\| | 2.87 | 3.20 | 2.61 | 2.27 | 3.03 | 2.92 | 2.32 | 2.72 |
| total net worth, miss (%) | +0.1 | +0.9 | +1.1 | +0.9 | −1.7 | −2.0 | −0.6 | −1.1 |
| top 1% net worth, miss (%) | +0.0 | +1.0 | +2.1 | +2.5 | −3.8 | −2.1 | −1.1 | −3.3 |

No pattern beyond noise: the same arm moves by up to five targets
between seeds.

## 6. Holder shares and joint returns (% of returns, before the tilt)

| | A | B | B long | C | A seed 2 | B seed 2 | B long seed 2 | C seed 2 | PUF 2015 | IRS 2019 |
|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| interest | 27.4 | 27.9 | 27.9 | 28.0 | 26.5 | 27.4 | 27.4 | 27.6 | 28.3 | 30.8 |
| dividends | 17.6 | 17.7 | 17.7 | 17.9 | 16.6 | 17.2 | 17.2 | 17.6 | 18.4 | 18.4 |
| capital gains | 12.8 | 12.9 | 12.9 | 13.0 | 12.1 | 12.8 | 12.8 | 12.8 | 13.3 | 13.8 |
| joint returns | 36.8 | 36.0 | 36.0 | 35.2 | 38.0 | 35.4 | 35.4 | 36.1 | | 34.7 |

After the tilt every arm matches the IRS's 2019 holder counts and joint
share exactly (they are targets). B and C are within seed noise of each
other. A is a little further from the PUF, most clearly in its second
seed. A's income draws see a masked net worth slot that no PUF training
record has, and its joint share is drawn after net worth.

## 7. Relationships no target covers (% of returns)

| | A | B | B long | C | A seed 2 | B seed 2 | B long seed 2 | C seed 2 | PUF 2015 |
|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| non-joint with 2+ dependents, before the tilt | 8.7 | 11.1 | 11.1 | **11.4** | 9.4 | 11.5 | 11.5 | **11.9** | **11.6** |
| non-joint with 2+ dependents, after the tilt | 8.7 | 10.8 | 10.7 | 11.0 | 9.0 | 11.2 | 11.1 | 11.3 | 11.6 |
| joint with no dependents, after | 54.0 | 49.7 | 49.2 | 49.9 | 53.3 | 49.7 | 49.1 | 50.1 | 49.7 |
| non-joint with no dependents, after | 77.5 | 74.9 | 75.2 | 74.1 | 76.9 | 74.2 | 74.4 | 73.9 | 73.3 |
| joint with 3+ dependents, after | 10.3 | 12.0 | 12.1 | 12.0 | 10.8 | 12.3 | 12.5 | 12.3 | 12.3 |
| 17 dividend and capital gains cells: mean gap to the PUF, before (points) | 1.5 | 1.0 | 1.1 | 1.0 | 2.1 | 1.2 | 1.2 | 1.0 | |
| the same, after (points) | 1.6 | 1.4 | 1.5 | 1.3 | 1.2 | 1.1 | 1.2 | 1.0 | |

The dependents shortfall of v4b is caused by the draw order. When married
and dependents are drawn before net worth, as a PUF record (B and C), the
dependents distribution matches the PUF to within 0.8 points in every
cell for C and 1.6 points for B (A: up to 4.3 points). When they are drawn after net worth with net worth visible under the
PUF label (A), joint returns without dependents rise 4 points and
non-joint returns with two or more dependents fall 3 points. The
difference between the two orders is four to five times the seed-to-seed
difference.

## 8. Why the separate net worth model gives more wealth

The PUF variables of B and C are nearly the same (sections 6 and 7), so
the gap is in the net worth draw. `20_diag.py` asks each net worth model
for the expected net worth of the same records: the real SCF records of
table E4 (on which the models were trained) and the real 2015 PUF records
(survey weights), each seen as an SCF record. Mean expected net worth, in
thousands of 2015 dollars:

| records | group | share of units (%) | actual SCF | V4 | V4 seed 2 | B2 | B2 seed 2 | B2 long | B2 long seed 2 |
|---|---|---:|---:|---:|---:|---:|---:|---:|---:|
| SCF | all | 100 | 614 | 659 | 671 | 663 | 712 | 645 | 659 |
| SCF | wages 25-50k | 19.1 | 181 | 169 | 175 | 270 | 244 | 212 | 210 |
| SCF | wages 200k+ | 3.8 | 5,119 | 5,826 | 5,671 | 3,429 | 4,260 | 5,138 | 5,326 |
| SCF | any interest, dividends or capital gains | 20.3 | 2,320 | 2,514 | 2,540 | 2,185 | 2,499 | 2,428 | 2,510 |
| SCF | none of the three | 79.7 | 180 | 186 | 195 | 275 | 257 | 191 | 188 |
| PUF | all | 100 | | 598 | 633 | 740 | 798 | 715 | 720 |
| PUF | wages 25-50k | 21.2 | | 212 | 225 | 353 | 324 | 315 | 302 |
| PUF | wages 50-100k | 18.0 | | 407 | 431 | 673 | 642 | 568 | 529 |
| PUF | wages 200k+ | 2.7 | | 4,301 | 4,181 | 3,340 | 4,142 | 4,263 | 4,441 |
| PUF | any interest, dividends or capital gains | 35.0 | | 1,481 | 1,565 | 1,716 | 1,914 | 1,800 | 1,818 |
| PUF | none of the three | 65.0 | | 122 | 132 | 214 | 197 | 132 | 129 |

Two things show here.

- **B2 at 80 passes does not reproduce the SCF even on its own training
  records.** Its wealth is too flat across wage groups: $270,000 for wages
  of $25,000 to $50,000 against an actual $181,000, and $3.4 million for
  wages above $200,000 against $5.1 million. This is underfitting, as the
  training loss in "Why B long" showed. Most of B's excess comes from this.
- **B2 long reproduces the SCF on its own records about as well as V4
  does, but still puts 13% to 20% more wealth on the PUF records.** The
  excess sits on PUF records with some investment income. That is 35% of
  PUF records but only 20% of SCF units, because the SCF misses many small
  holders. On those records B2 long gives $1.80 million against V4's
  $1.48 to $1.57 million. On records with no investment income the two
  agree. An SCF-only model has seen investment income only as SCF units
  report it, mostly by larger holders, and it treats the PUF's many small
  holders like those. The joint model gives them less wealth.

The second point is the part that is joint training proper. Both models
draw net worth only from what they learned on SCF records, so neither
answer for these PUF records is checked directly by data. The available
evidence favours the joint model's answer. Its file's wealth by wage
group is closer to the SCF's before the tilt, its tilt costs less, and it
predicts held-back SCF records better.

## 9. The stack against its plain file

In every new arm the importance weights sum to 100,000 per file. The gaps
between the stack and a plain file of the same rule (seed 3) are of the
same size as the gap between two plain files (seeds 3 and 35) for holder
shares, the joint share, dependents and the median
(`checks/S20/report_tables.md`, section 7). As in v4b, plain files are
themselves noisy at the top: C's two plain files differ by 2.2 points in
the top 1% share.

## What it shows

1. **Joint against separate, for the PUF variables:** no difference.
   Held-back loss, generated relationships and the IRS-only move are the
   same within seed noise. A PUF-only model would do.
2. **Joint against separate, for net worth on PUF records:** the joint
   model is better by more than the training noise. The total before the
   tilt is 1.04 and 1.12 times the SCF's, against 1.29 and 1.37 for a
   separate SCF model trained the same way, and 1.25 and 1.26 for one
   given as many gradient steps. The two seeds of any one arm differ by
   0.08. After the tilt, the separate model's wealth stays too flat
   across wage groups: earners above $200,000 have $2.7 to $3.1 million
   in B and $3.7 to $3.8 million in B long, against $3.9 to $4.2 million
   in C and $4.5 million in the SCF. Part of the gain is that an SCF-only
   model of this size, trained this way, either underfits (80 passes) or
   overfits (516 passes). The joint model avoids both, because the PUF
   keeps training the shared parts of the network. The other part is how
   net worth is assigned to PUF records whose small investment income the
   SCF does not report.
3. **Draw order:** drawing every PUF variable first and net worth last (C)
   instead of v4b's order (A) fixes the dependents shortfall (non-joint
   returns with two or more dependents: 8.7% and 9.4% in A, 11.4% and
   11.9% in C, 11.6% in the PUF) and the excess of joint returns without
   dependents. It also lowers the move (0.021 and 0.024 nats against 0.024
   and 0.033). Wealth totals and the wage profile are the same within
   noise. The bottom half of the wealth distribution is further short
   before the tilt in C (−42% against −19%), and the tilt fixes it at no
   extra cost. The zero-wage excess is unchanged.
4. **So the process is matching plus calibration in form**, once the draw
   order is C's. The joint model is the better donor model for the
   borrowed variable, and for the PUF variables it is no better than a
   PUF-only model.

Not tested: a separately tuned SCF-only model (early stopping on held-back
households, a smaller network, more regularisation), which might close
part of the net worth gap; and whether C's order should replace v4b's.
Whether to change the order is John's decision. C's order puts married
and dependents in block 1 with the incomes, which differs from decision 9
(demographics last).
