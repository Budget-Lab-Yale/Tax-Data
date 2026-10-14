---
title: "Test 3, piece F — records drawn from (close to) scratch: report"
role: report
workstream: rebuild
status: current
updated: 2026-10-09
sot: research/rebuild/tests/posttrain/SPEC.md
supersedes: []
superseded_by: null
---

# Piece F: drawing whole records, and calibrating them

Two sets of results. Sections 1 to 3 use the **pilot model** (test 2's joint
model, which knows only the seven summed incomes, age range, married and
dependents on the PUF side), on SCF records whose true wealth is known.
Section 4 has the first draws from the **full model** (H's v1, test 1's PUF
variables plus wealth). The full-model calibrations are queued (section 5).

## 1. What ran on the pilot model

- **Arm S0 (close to scratch).** Each of the 42,057 SCF training records
  kept its year, age range, married and dependents and its own weight; the
  model drew the seven incomes and then the 24 wealth variables, 32 times
  per record. Targets: A's 30,624 wealth cells, each draw grouped by its
  own drawn incomes, plus 8,188 cells on the incomes (each income's coarse
  bins by income group, age, married, dependents and the other six
  incomes' bands), all from the real records.
- **Arm S1 (from scratch).** Whole records drawn from nothing but source
  SCF and the year: 1, 4, 8, 32 and 64 times each wave's real record count
  (nested samples). Equal starting weights summing to the wave total. Added
  targets: age x married x dependents and the wave total.
- **Checks.** F's sampler gives A's float32 log-probabilities of A's draws
  to within 5e-6 nats (on a different GPU type from A's). The wealth
  targets equal A's exactly. The income-group thresholds in dollars
  reproduce A's income groups on every record. The real records' own
  income tokens, decoded as draws are, fall in A's income groups 97.9% of
  the time with the same group shares.
- **Calibration** used B's `calibrate_matrix` as it stood at 20:20 (S1)
  and 20:42 (S0). B's "block check" wrongly drops cells whenever a record's
  draws fall in different groups, which always happens once incomes are
  drawn (SPEC.md, F 20:45); F's calls skip it.

## 2. Pilot numbers

### Mean net worth by income group (SCF training records, both waves)

| Income group | Truth | Completion arm, unweighted | S0, unweighted | S1 8x, unweighted | S1 8x, method 1 (not exact) |
|---|---|---|---|---|---|
| Bottom 50% | 112k | 163k | 123k | 79k | 118k |
| 50-90% | 366k | 446k | 468k | 402k | 389k |
| 90-99% | 2.09m | 2.29m | 2.02m | 2.03m | 2.13m |
| 99-99.9% | 11.90m | 11.23m | 11.32m | 11.31m | 10.67m |
| Top 0.1% | 52.40m | 30.28m | 32.16m | 31.12m | 33.73m |

Share of all net worth held by each income group:

| Income group | Truth | Completion arm | S0 | S1 8x | S1 8x, method 1 |
|---|---|---|---|---|---|
| Bottom 50% | 8.1% | 10.9% | 14.7% | 22.3% | 8.9% |
| 50-90% | 26.9% | 30.4% | 32.4% | 30.7% | 29.8% |
| 90-99% | 35.0% | 35.7% | 35.4% | 30.9% | 37.2% |
| 99-99.9% | 20.1% | 17.6% | 13.6% | 12.2% | 18.6% |
| Top 0.1% | 9.9% | 5.3% | 4.0% | 3.9% | 5.5% |

### The drawn incomes are too low, and worse the less the model is shown

Population share by income group (thresholds from all real SCF units):

| Income group | Truth | S0 | S1 8x |
|---|---|---|---|
| Bottom 50% | 44.0% | 57.6% | 75.2% |
| 50-90% | 44.7% | 33.4% | 20.4% |
| 90-99% | 10.2% | 8.4% | 4.1% |
| 99-99.9% | 1.03% | 0.58% | 0.29% |
| Top 0.1% | 0.12% | 0.06% | 0.03% |

| Share with zero | Real | S0 draws | S1 draws |
|---|---|---|---|
| Wages | 22.9% | 13.8% | 8.2% |
| Business | 88.1% | 92.7% | 96.0% |
| Interest and dividends | 78.0% | 85.0% | 92.0% |
| Capital gains | 90.9% | 94.0% | 96.8% |
| Rent | 89.1% | 93.2% | 96.4% |
| Pensions and Social Security | 70.1% | 77.9% | 85.7% |
| Unemployment and other | 90.0% | 96.1% | 98.3% |

Asked for incomes with little or nothing else shown, the pilot model
produces too many wage-only, lower-income records. A likely cause is
training: under any-order masking the share hidden is drawn uniformly, so
a record with all 34 variables hidden is seen about once in 35 records, and
the early steps of a from-scratch draw are poorly trained. The full model
shows the same pattern at the top (section 4).

### Calibration (method 1): exactness is out of reach for the from-scratch arm

**S1.** The solver did not converge at any sample size. What it reached:

| Sample | KL from the starting weights (nats per unit of weight) | Effective sample size (2013 / 2016) | Cells with a positive target and no draw |
|---|---|---|---|
| 1x (20,595 / 21,462 draws) | 2.41 | 1,235 / 1,244 (6%) | 5,848 |
| 8x (164,760 / 171,696 draws) | 3.69 | 2,411 / 2,452 (1.5%) | 2,184 |

At 8x, 32,810 cells were still being targeted and nearly all were off by
more than the tolerance; the worst miss was 459 times its target. These are
not exact solutions. Of the 3.7 nats, about 0.8 is the move on the age x
married x dependents x income-group table alone (0.66 on age x married x
dependents).

The cause is the number of draws in small groups. A from-scratch sample
holds records in proportion to the model's population shares, which are
too low at the top, while the real SCF oversamples the wealthy:

| Sample | Top 0.1% income group, 2013: draws | Positive target cells there | Real SCF records there |
|---|---|---|---|
| 1x | 12 | 298 | 1,035 |
| 4x | 30 | 298 | 1,035 |
| 8x | 53 | 298 | 1,035 |
| 32x | 257 | 298 | 1,035 |

At 1x, 36 of the 96 (wave, family, group) blocks have fewer draws than
positive target cells; at 8x and 32x, two (the top 0.1% in each wave); at
64x, none.

Even inexact, the reweighting brings the drawn records much closer to the
truth on things that were not targeted (distance from the truth; smaller is
better):

| Measure | Completion arm | S0 | S1 8x | S1 8x, method 1 |
|---|---|---|---|---|
| Joint holding of two components (mean absolute difference, percentage points) | 0.38 | 1.40 | 3.49 | 0.18 |
| Holding by income group x age (mean absolute difference, points) | 4.94 | 5.47 | 6.05 | 5.76 |
| Rank correlation between components (mean absolute difference) | 0.081 | 0.082 | 0.073 | 0.076 |
| Rank correlation with total income (mean absolute difference) | 0.056 | 0.044 | 0.038 | 0.019 |
| Classifier test, area under the curve (0.5 = indistinguishable) | 0.596 | 0.743 | 0.852 | 0.533 |

The cost is concentration: an effective sample of about 2,400 per wave out
of 165,000 draws, and the top 0.1% still has only half the true mean net
worth because 53 draws cannot represent it.

**S0.** No usable solution (job 28902234, B's solver of 20:42, 72
minutes). Before the solve: 631 cells with a positive target and no draw,
462 needing more weight than every record that reaches them holds, 3 below
the weight forced into them. The first round left 31,498 of the remaining
33,805 cells missed by more than a millionth of their target; that version
of the solver then classed every missed cell as jointly infeasible and
re-solved with the one cell left, so the final weights hit almost nothing
and stay close to the starting weights (0.07 nats, mostly from dropping
draws in zero cells). In the draws, 56% fall in a different income group
from their record's real one, so a record's weight can no longer stand in
for the rich: drawing incomes from demographics alone discards the SCF's
oversampling of the top. B's later raking solver was not tried on S0
(pilot runs were stopped at the reorientation).

**Split-off units.** They are 11.7% (2013) and 13.9% (2016) of the SCF's
weight; removing them moves the demographics x income-group table by 0.034
and 0.039 nats. That is small next to the 0.77 to 0.80 nats the S1
calibration spent on the same table, so they are not what the calibration
mainly has to fix.

## 3. A remedy built from this: oversampling the top (PS1s)

`f_full.draw_strat` draws a pool of 16 times the PUF count only as far as
the income measure (AGI, the 10th step in the full model), keeps every unit
in the top three income groups and a 4x-sized random subset of the bottom
two, and completes only the kept records. Method 1 then treats each income
group as one "record" holding its targeted total weight, which is the same
minimum-KL problem because group totals are targets anyway; the KL from the
model is reported as the move between income groups plus the move within
them. A pilot calibration ran for 30 minutes (3,170 cells still off target
under B's new raking solver) and was stopped to free job slots for the
full model.

## 4. First draws from the full model (v1)

All drawn on the full model with the PUF tag for PUF variables and the SCF
tag for wealth; a draw's recomputed log-probability equals the sampled one
exactly. PS0 keeps each PUF record's categorical variables; 70,615 records
lack AGERANGE (and about as many GENDER and EARNSPLIT), which stay
unobserved and are never drawn.

The full model also under-produces high incomes when drawing from scratch.
In a pool of 2,658,496 synthetic PUF records (PS1s), by AGI group against
the PUF's own thresholds:

| AGI group | Target share | Pool share | Pool records |
|---|---|---|---|
| Bottom 50% | 50% | 51.3% | 1,364,971 |
| 50-90% | 40% | 39.8% | 1,057,861 |
| 90-99% | 9% | 8.5% | 227,087 |
| 99-99.9% | 0.9% | 0.31% | 8,286 |
| Top 0.1% | 0.1% | 0.011% | 291 |

Net worth by income group, before calibration, against the SCF interpolated
to 2015 (the PUF side groups by AGI, the SCF side by its seven-income
total, so the groups are not defined identically):

| Income group | SCF 2013 | SCF 2016 | SCF interpolated 2015 | PS1 4x, unweighted | PS1s (groups at target weight), unweighted |
|---|---|---|---|---|---|
| Bottom 50% | 109k | 115k | 113k | 213k | 213k |
| 50-90% | 339k | 390k | 372k | 514k | 516k |
| 90-99% | 1.82m | 2.34m | 2.15m | 2.52m | 2.54m |
| 99-99.9% | 9.78m | 13.85m | 12.33m | 12.00m | 11.46m |
| Top 0.1% | 41.17m | 62.94m | 54.64m | 25.42m | 30.68m |

Whole population: mean net worth 628k (SCF, 2015) against 570k (PS1) and
675k (PS1s); top 1% share of net worth 37.8% against 34.9% and 37.8%.

The v2 model (Schedule E total added) draws the top better in wealth but
not in numbers: its pool holds 234 top-0.1% records (0.009%) and 7,740 in
the 99-99.9% group (0.29%). Before calibration:

| Income group | SCF interpolated 2015 | v2 PS1 8x, unweighted | v2 PS1s, unweighted |
|---|---|---|---|
| Bottom 50% | 113k | 310k | 314k |
| 50-90% | 372k | 571k | 577k |
| 90-99% | 2.15m | 2.29m | 2.28m |
| 99-99.9% | 12.33m | 11.96m | 12.35m |
| Top 0.1% | 54.64m | 53.35m | 56.52m |

The bottom half's mean net worth is nearly three times the SCF's in v2
(twice in v1), while the bottom half's median is half the SCF's (8k
against 17k), so the excess sits in a minority of low-income records with
high wealth.

## 5. Calibrating the full-model draws (method 1, core targets)

Targets: A's PUF ground-truth tables (each PUF variable's coarse bins by AGI
group, filing status, age range and dependents, and two-way crosses of the
main income items) plus A's SCF wealth "core" tables carried to the PUF,
each draw grouped by its own drawn values. B's solver as of 21:50 to 22:20
(raking, then Newton), at most about 45 minutes per run. Results at 23:00:

| Model | Arm | Synthetic records | Exact? | Average miss per cell, as a share of the average positive target | KL from the model (nats per unit of weight) | Effective sample size |
|---|---|---|---|---|---|---|
| v1 | PS1, 4x | 664,624 | no | 2.5% | 1.11 | 38,100 (5.7%) |
| v1 | PS1, 8x | 1,329,248 | no | 2.2% | 1.09 | 51,145 (3.8%) |
| v1 | PS1s, top oversampled | 833,825 | no | 2.2% | 1.36 | 33,648 |
| v2 | PS1, 4x | 664,624 | no | 2.3% | 1.10 | 36,600 (5.5%) |
| v2 | PS1, 8x | 1,329,248 | no | 2.4% | 1.21 | 54,172 (4.1%) |
| v2 | PS1s, top oversampled | 830,163 | no | 2.4% | 1.90 | 15,817 |

- None reached an exact solution. Before solving, 1,100 to 1,550 cells
  (depending on the run) have a positive target and no synthetic record at
  all, and 1,350 to 1,430 are zero cells (no PUF record there), which remove
  every draw that falls in them. With those set aside, the solver still could not
  hit the rest together and ended with a compromise: the solver labels
  almost every cell "not hit jointly", and the average miss is a little over
  2% of an average cell.
- Doubling the sample (4x to 8x) did not shrink the misses or the move.
  Oversampling the top (PS1s) did not either: almost none of its move is
  between income groups (0.005 nats); the rest is within them.
- PS0 (keep each PUF record's demographics) and the `full` target sets
  (wealth by the income items both sources measure) were still running at
  23:00.

## 6. Unresolved, and what is queued

- **Full-model calibrations still running at 23:00**: PS0 (K = 8 and 16)
  and every `full` target set, v1 and v2; then the evaluation jobs (v1
  28902520, v2 28904039) write `$P/<version>/f/eval/tables.md` with the
  calibrated columns. `python f_summary.py` (through `f_cpu.sbatch`) prints
  the table of section 5 for every report present. Chains, outputs and how
  to finish by hand: STATUS.md, F entries 20:45, 20:58 and 21:17. Targets are
  A's `a_full.py` sets (`core` = PUF ground truth + SCF wealth core,
  `full` = PUF ground truth + SCF wealth with income bands), with a
  work-around for one bug in `a_full.rebuild_set` (SPEC.md, F 21:15).
- **Exactness.** On this target detail no from-scratch arm hit every target
  exactly. Whether to (a) oversample small groups at drawing time (PS1s),
  (b) draw far more records, (c) coarsen the targets in thin groups, or
  (d) accept small misses is for John.
- **Method 2 on the from-scratch arms** was not attempted. C's sequential
  fit needs each record's groups to be known before wealth is drawn; with
  drawn incomes the cells of an income step depend on incomes drawn later,
  so it needs another design.
- **Pretraining.** Both models under-produce high incomes from scratch
  (pilot: the top 0.1% at about a quarter of its share; full v1: about a ninth). This
  bears on H's and E's pretraining checks.
