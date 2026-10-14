---
title: "Test 4, probe UW — training on the PUF sample as drawn, with the sampling weight as a model variable"
role: report
workstream: rebuild
status: current
updated: 2026-10-11
sot: research/rebuild/tests/probes/SPEC.md
supersedes: []
superseded_by: null
---

# Training on the PUF sample as drawn, with the sampling weight as a variable

## Summary

- **On its own, training on the sample as drawn does not fix generation.**
  The top 0.1% mix improves: given each record's true AGI and weight, the
  six components there add to $769–776bn against the PUF's $702bn (earlier
  models: $400–537bn). But whole generated records get worse in two ways.
  Without ordered questions in training (arm A, defined below), the top
  1% share of AGI is 25.2% against 19.4% in the PUF. With them (arm B),
  the top is right (20.1%) but total AGI is $6.99tn against $8.09tn, with
  AGI in the bottom half at -$281bn against +$830bn.
- **Why: mistakes on records drawn with a large weight are multiplied by
  that weight.** About half of the PUF sample has weights above 1,000. Such
  records have AGI below -$100,000 in 0.09% of cases, and none has AGI in
  the top 0.1%. Generated records with weights above 1,000 have AGI below
  -$100,000 in 0.41% of cases, which adds about -$650bn (arm A) or
  -$1,100bn (arm B) of negative AGI to the population against -$32bn in the
  PUF. In arm A, 0.016% of them land in the top 0.1%, which adds about
  $250bn there. Errors this small barely affect the training loss, but each
  such record stands for about 1,400 returns.
- **Restricting each draw to values seen in the PUF sample among records with
  similar weights removes most of this.** The weights come from the IRS's
  sampling strata, which are income classes, so a record's weight largely
  fixes its income range. Arm B with this restriction gives total AGI of
  $8.02tn, a top 1% share of 19.3%, a top 0.1% share of 8.8%, and $679bn
  of the six components in the top 0.1% (PUF $8.09tn, 19.4%, 8.8%, $702bn).
  Bottom-half capital gains are $16bn (PUF $13bn), partnership and S
  corporation income -$28bn (PUF -$31bn), and dividends $14bn (PUF $14bn).
  Every one of these is closer to the PUF than S's 25% recipe, the best
  earlier recipe. It covers three seeds with one sampling draw each.
- **The SCF side is unchanged.** SCF wealth completion is 35.46–35.58
  nats per record in every arm (test 3: 35.50–35.56).
- **The generated weights are as informative as the sample's.** The
  effective sample size of the generated weights is 52–54% of the records,
  against 54% for the PUF sample itself. The drawn weights follow the
  sample's distribution closely: the share of records at each weight value
  differs from the sample's by 0.017 to 0.028 in total (half the sum of
  absolute differences).

## Terms used

- **Sampling weight.** The PUF record's weight, S006/100: the number of
  returns the record stands for. The 166,156 PUF training records carry
  only 73 distinct weights, from 1.01 for the top income classes, which are
  sampled at about 100%, to 1,703.
- **Training on the sample as drawn.** Every PUF record counts equally in
  the training loss, instead of in proportion to its sampling weight (test
  3's version 2). The model then learns the sample, in which the top 0.1%
  of AGI is 20,339 records (12.2%), not 0.1% of the signal. The weight is
  a model variable, so each generated record draws a weight, and population
  statistics use those weights.
- **Weight-group restriction.** At generation, once a record's weight is
  drawn, each later variable may take only values (tokens) seen for that
  variable among PUF training records with a similar weight. "Similar":
  the 73 weights sorted and adjacent ones merged until each group has at
  least 300 records, giving 45 groups. This is a fixed table built from the
  training records. It is not a change to the model.
- **Ordered questions.** As in probe S: for a share of each batch, the
  model sees the first k variables of a fixed order and is scored on
  variable k+1. These are the questions generation asks.
- **Effective sample size.** (sum of weights)² / (sum of squared weights):
  the number of equally weighted records that would give the same
  precision.
- **Nats.** Units of log-likelihood; lower is better.

## What was run

All jobs ran in Slurm on 10–11 October. Outputs are in
`/nfs/roberts/scratch/pi_nrs36/jar335/probes/uw/`, with every job number in
`jobs.txt` there. Every number below comes from these jobs. The full tables
are in `tables_final.md` (collector job 29102652), with logs under
`/nfs/roberts/scratch/pi_nrs36/jar335/probes/logs/uw_*`.

**Data (`uw_prepare.py`, job 29094078).** `full_v2w` is test 3's
`full_v2` plus one variable, PUFWT: the PUF sampling weight, observed on
PUF rows and unobserved on SCF rows. Because there are only 73 distinct
weights, each weight is its own token and decodes back to exactly itself.
The round trip on the PUF training records, decoding each record's weight
from its token, changes nothing. Total AGI ($8.094tn), the top 1% and top
0.1% shares (19.42%, 8.85%), and every component in every AGI group are
identical to the last digit. The script also tested bands of 50 to 200
equal-count groups. Those shifted some components in some AGI groups by
10% to 31%, so exact tokens are clearly better.

**Loss scaling (`uw_train.py`, a copy of S's `s_train.py` with
`--loss-weight uw` and `uwsqrt`).** In version 2 each record's loss weight
was its population weight. The SCF rows then carried 61.55% of the total
record weight, and 60.24% of the weighted variable slots scored. Now:

- PUF rows: loss weight 1 for every record (arms A and B), or the square
  root of its sampling weight (arms C and D).
- SCF rows: the survey weight times one constant, chosen so that the SCF
  rows again carry 61.55% of the total record weight. All loss weights are
  then rescaled to average 1.
- With PUFWT now scored on PUF rows, the SCF share of weighted scored slots
  moves from 60.24% to 60.36%.

The validation loss used for early stopping is weighted the same way.

**Arms** (three seeds each for A and B, two for C and D):

| Arm | PUF loss weight | Ordered questions | Schedule | Training jobs |
|---|---|---|---|---|
| A | equal | none | test 3's recipe (learning rate not decayed, early stopping) | 29094106, 29094108, 29095404 |
| B | equal | 25% of each batch; order: weight, AGI, then S's AGI-first order | S's 25% recipe (60 epochs, decaying rate) | 29094107, 29094109, 29095405 |
| C | square root of the sampling weight | none | as A | 29097237, 29097245 |
| D | square root of the sampling weight | as B | as B | 29097241, 29097249 |

Arms C and D were added after the first results. They are the middle
course in T's report: the top counts for more than its population share,
but not as much as in A and B. Their generated records are weighted by the
square root of the drawn weight, which is exact in principle by the same
argument as for A and B.

**Generation (`uw_gen.sbatch`, using `uw_complete.py`, a copy of probe O's
`o_complete.py` with the restriction added).** Each run makes 664,624 whole
PUF records in the order: weight, AGI, then S's AGI-first order
(categoricals, other amounts, demographics last). There are two sampling
draws per trained model, and one more with the weight-group restriction
(table from `uw_support.py`, job 29097322). Arm A was also run with a
fresh random order for each record (one draw per seed). Population
statistics weight each generated record by its drawn weight (its square
root in C and D), scaled so the weights add up to the PUF's 120.66 million
returns.

**Mix given the true AGI (`uw_given.sbatch`).** As in T and S, each PUF
training record keeps its true AGI, its weight and its first six variables
(filing status, dependent status, exemptions, children for the child
credit, children for the EITC, form of deduction). The rest are drawn, and
records carry their true weights. This was run with and without the
restriction.

**Comparisons (`uw_compare.py`, a copy of T's `t_compare.py`;
`uw_summary.py`; `uw_collect.sbatch`).** These use the PUF training
records' AGI thresholds: 50th, 90th, 99th and 99.9th weighted percentiles.
The test 3 and probe S columns are copied from `S_REPORT.md`.

**Failures, all rerun.** The first restricted runs crashed when a weight
group allowed no token for a variable. This happened only for the
demographic variables (age range, gender, earnings split), because the
PUF leaves them blank for most top-income records. Those draws now fall
back to unrestricted, which happened for 42% of records in the
given-AGI runs (counts for each run are in its log), in those three
variables only. Two given-AGI runs ran out of GPU memory on a node whose
GPU was held by another process (a1127u05n01, the node S reported); they
were rerun with that node excluded.

## Results: whole records, weight drawn first, no restriction

Means over seeds and sampling draws, with the range in brackets. Arms A
and B: three seeds × two draws. Arm A in random order: three seeds × one
draw. $bn unless stated.

| | PUF | test 3, AGI first | S 25% | A | B | A, random order |
|---|---|---|---|---|---|---|
| Total AGI | $8.09tn | $7.88tn | $7.97tn | $8.28tn (7.97–8.44) | $6.99tn (6.80–7.18) | $9.43tn (8.72–10.79) |
| Weight above true 99th pct | 1.00% | | | 1.22% | 1.00% | 1.22% |
| Weight above true 99.9th pct | 0.100% | | | 0.140% | 0.103% | 0.192% |
| Top 1% share of AGI | 19.4% | 17.7% | 19.6% | 25.2% (24.7–25.6) | 20.1% (19.3–20.8) | 28.6% (24.9–35.6) |
| Top 0.1% share of AGI | 8.8% | 7.3% | 8.9% | 13.5% (12.4–14.6) | 9.4% (8.9–9.8) | 19.3% |
| AGI in top 0.1% | 731 | 589 | 728 | 1,239 | 780 | 2,105 |
| Six components in top 0.1% | 702 | 401 | 412 | 1,034 | 794 | 1,092 |
| AGI, bottom 50% | 830 | | | 59 | -281 | -364 |
| Capital gains, bottom 50% | 13 | 104 | 70 | 128 | 122 | 97 |
| Partnership and S corp., bottom 50% | -31 | 41 | 6 | -213 | -184 | -297 |
| Dividends, bottom 50% | 14 | 47 | 27 | 67 | 60 | 145 |
| Wages, 99th–99.9th pct | 455 | 594 | 548 | 598 | 500 | 497 |

Blank cells: not reported in `S_REPORT.md`. The six components are wages,
capital gains (long plus short term), partnership and S corporation
income, ordinary dividends, taxable interest and Schedule C income.

## Results: whole records with the weight-group restriction

The same trained models, one draw each (three seeds for A and B).

| | PUF | S 25% | A, restricted | B, restricted |
|---|---|---|---|---|
| Total AGI | $8.09tn | $7.97tn | $8.40tn (8.27–8.47) | $8.02tn (7.94–8.13) |
| Weight above true 99th pct | 1.00% | | 1.10% | 0.97% |
| Weight above true 99.9th pct | 0.100% | | 0.114% | 0.101% |
| Top 1% share of AGI | 19.4% | 19.6% | 20.9% (20.2–21.2) | 19.3% (18.9–19.7) |
| Top 0.1% share of AGI | 8.8% | 8.9% | 9.7% | 8.8% (8.6–8.9) |
| AGI in top 0.1% | 731 | 728 | 837 | 718 |
| Six components in top 0.1% | 702 | 412 | 769 | 679 (656–717) |
| Six components, 99th–99.9th pct | 829 | | 902 | 832 |
| AGI, bottom 50% | 830 | | 765 | 858 |
| Capital gains, bottom 50% | 13 | 70 | 16 | 16 |
| Partnership and S corp., bottom 50% | -31 | 6 | -57 | -28 |
| Dividends, bottom 50% | 14 | 27 | 20 | 14 |
| Wages, 99th–99.9th pct | 455 | 548 | 549 | 474 |
| Wages, top 0.1% | 211 | | 233 | 189 |

Arm B with the restriction, components by AGI group ($bn; PUF / arm B
restricted, mean of three seeds):

| AGI group | AGI | Wages | Capital gains | Partnership and S corp. | Dividends | Interest | Schedule C |
|---|---|---|---|---|---|---|---|
| Bottom 50 | 830 / 858 | 810 / 860 | 13 / 16 | -31 / -28 | 14 / 14 | 9 / 8 | 59 / 65 |
| 50–90 | 3,415 / 3,406 | 2,654 / 2,762 | 30 / 27 | 38 / 22 | 41 / 31 | 16 / 12 | 83 / 74 |
| 90–99 | 2,245 / 2,183 | 1,590 / 1,577 | 84 / 70 | 129 / 115 | 55 / 45 | 16 / 14 | 82 / 74 |
| 99–99.9 | 873 / 857 | 455 / 474 | 104 / 91 | 186 / 193 | 39 / 34 | 12 / 11 | 33 / 30 |
| Top 0.1 | 731 / 718 | 211 / 189 | 245 / 253 | 175 / 172 | 45 / 40 | 18 / 18 | 10 / 8 |

## Results: the mix of income given the true AGI (the key test)

Each PUF training record keeps its true AGI, weight and first six
variables; the rest are drawn. $bn, means over seeds (test 3 and S: from
`S_REPORT.md`).

| Component and AGI group | PUF | test 3 | S 25% | A | B | A, restricted | B, restricted |
|---|---|---|---|---|---|---|---|
| Six components, top 0.1% | 702 | 486 | 419 | 769 (727–810) | 776 (768–788) | 693 (657–728) | 692 (681–705) |
| Six components, 99th–99.9th | 829 | | | 891 | 893 | 827 | 849 |
| Wages, 99th–99.9th | 455 | 627 | 561 | 516 | 504 | 498 | 486 |
| Wages, top 0.1% | 211 | 167 | 119 | 225 | 213 | 207 | 189 |
| Capital gains, bottom 50% | 13 | 70 | 43 | 66 | 20 | 10 | 14 |
| Capital gains, top 0.1% | 245 | 160 | 157 | 276 | 288 | 248 | 261 |
| Partnership and S corp., bottom 50% | -31 | 40 | 18 | -123 | -107 | -38 | -29 |
| Partnership and S corp., top 0.1% | 175 | 114 | 105 | 196 | 202 | 173 | 175 |
| Dividends, bottom 50% | 14 | | | 40 | 42 | 18 | 16 |
| Dividends, top 0.1% | 45 | 30 | 24 | 41 | 44 | 37 | 40 |

T's and S's figure for the PUF's six components in the top 0.1% is $704bn
in this table and $702bn in their generation tables. Ours is $702bn in
both, from the same records.

Training on the sample as drawn is what fixes the top 0.1% mix: $769–776bn
without the restriction, $692–693bn with it, against $702bn. Before it,
every recipe gave $400–537bn. The restriction then removes the
false bottom-half income that leaks in from records with large weights.

## Where the remaining error comes from

By band of the drawn weight, for whole generated records without the
restriction (means over files; "negative AGI" is the population-weighted
sum of AGI over records with negative AGI, $bn).

| Weight band | Measure | PUF sample | A | B | B, restricted |
|---|---|---|---|---|---|
| 20–200 | share with AGI above the true 99.9th pct | 0.004% | 0.758% | 0.239% | 0.103% |
| 20–200 | negative AGI, $bn | -47 | -130 | -93 | -60 |
| 200–1,000 | share with AGI above the true 99th pct | 0.01% | 1.22% | 0.55% | 0.13% |
| 1,000 and over (47% of records) | share with AGI above the true 99.9th pct | 0.000% | 0.016% | 0.000% | 0.000% |
| 1,000 and over | share with AGI below -$100,000 | 0.088% | 0.408% | 0.407% | 0.060% |
| 1,000 and over | negative AGI, $bn | -32 | -652 | -1,102 | -26 |
| 1,000 and over | AGI above the true 99.9th pct, $bn | 0 | 253 | 12 | 0 |

The weights and the income given the weight are each close to the sample.
The problem is in the extreme tails of income given a large weight: a few
hundredths of a percent of records drawn in the wrong place. In a sample
drawn this way, those records stand for over a thousand returns each. The
restriction encodes the fact that the IRS strata are income classes,
which the model learns only approximately.

## The weight variable

| | PUF sample | A | B | A, restricted | B, restricted |
|---|---|---|---|---|---|
| Records | 166,156 | 664,624 | 664,624 | 664,624 | 664,624 |
| Effective sample size | 89,270 | 344,314 | 357,841 | 343,949 | 357,607 |
| as a share of records | 0.537 | 0.518 | 0.538 | 0.518 | 0.538 |
| Mean weight | 726 | 699 | 728 | 698 | 728 |
| Difference from the sample's distribution over the 73 weights (half the sum of absolute differences in shares) | | 0.028 | 0.017 | 0.029 | 0.017 |
| Records with weight under 2 | 5.3% | 5.4% | 5.2% | 5.4% | 5.2% |
| Records with weight 1,000 and over | 47.1% | 45.3% | 47.3% | 45.2% | 47.2% |
| Records in the top 0.1% of AGI | 12.2% | 13.1% | 12.3% | 13.0% | 12.3% |
| Mean weight of records in the top 0.1% | 5.9 | 7.5 | 6.1 | 6.1 | 6.0 |

The generated file has the sample's design: about 12% of its records
describe the top 0.1%, so a file of 664,624 records has about 81,000 top
records. Under population weighting there would be about 660. Arm A in
random order draws the weight poorly (difference 0.187). That order fails
in general here.

## SCF wealth completion

H's score on 6,112 SCF validation records (nats per record, lower is
better), at the kept epoch of each model. Test 3: 35.50, 35.52, 35.56.

| Arm | Seed 1 | Seed 2 | Seed 3 | Kept epochs |
|---|---|---|---|---|
| A | 35.49 | 35.56 | 35.58 | 14, 16, 15 |
| B | 35.54 | 35.56 | 35.52 | 53, 60, 52 |
| C | 35.51 | 35.46 | | 15, 14 |
| D | 35.46 | 35.54 | | 52, 55 |

## Square-root arms (C and D)

The PUF loss is weighted by the square root of the sampling weight, and
generated records by the square root of their drawn weight. Two seeds:
two unrestricted draws and one restricted draw per seed. $bn unless
stated (table `tables_final.md`).

| | PUF | C | D | C, restricted | D, restricted | B, restricted |
|---|---|---|---|---|---|---|
| Total AGI | $8.09tn | $8.47tn | $8.02tn | $8.31tn | $8.05tn | $8.02tn |
| Top 1% share of AGI | 19.4% | 23.2% | 21.4% | 20.2% | 18.8% | 19.3% |
| Top 0.1% share of AGI | 8.8% | 11.5% | 10.9% | 9.1% | 8.6% | 8.8% |
| Six components in top 0.1% | 702 | 1,055 | 731 | 776 | 564 | 679 |
| Six components, 99th–99.9th | 829 | 1,352 | 1,026 | 1,072 | 811 | 832 |
| Capital gains, bottom 50% | 13 | 295 | 188 | 29 | 21 | 16 |
| Partnership and S corp., bottom 50% | -31 | -11 | -20 | -24 | -14 | -28 |
| Dividends, bottom 50% | 14 | 68 | 44 | 18 | 13 | 14 |
| Wages, 99th–99.9th | 455 | 736 | 513 | 674 | 469 | 474 |
| Six components in top 0.1%, given true AGI and weight | 702 | 888 | 683 | 784 | 593 | 692 |
| Effective sample size, share of records | 0.537 (PUF sample) | 0.910 | 0.915 | 0.910 | 0.915 | 0.538 |

The square root makes the generated file closer to population weighting:
1.3% of records are in the top 0.1% instead of 12%, and the weights are
more even (effective sample size 91% of records). It does not make the
income mix better. Given the true AGI, D puts too little of the six
components in the top 0.1% ($593bn restricted, against $702bn), and C
puts too much wage income just below it ($674bn restricted, against
$455bn). Arm B with the restriction is the closest of the restricted columns on
every row except two. On total AGI and on wages in the 99th–99.9th
percentiles, D restricted is closer, by $0.03tn and $5bn.

## What this means for the plan

1. **Train the PUF on the sample as drawn, with the weight as a variable.**
   This is the first change in test 4 that fixes the mix of income in the
   top 0.1% given AGI. It costs nothing on the SCF side. The
   generated file keeps the sample's design: many records at the top, few
   at the bottom.
2. **Generate with the weight drawn first and later draws restricted to
   values seen in the weight's group.** The restriction is a small table
   taken from the training records. It stands in for the known design of
   the sample: strata are income classes. Without it, rare errors on
   records with large weights dominate the population totals.
3. **Keep the 25% of ordered questions (arm B).** With the restriction, B
   matches the PUF closely on AGI, the top shares and the top and
   bottom income mix. Some middle groups still differ: partnership
   income in the 50th–90th percentiles is $22bn against $38bn, and
   dividends there are $31bn against $41bn. A still overshoots the top
   (top 1% share 20.9%).
4. **Future years.** The weight can be the variable that targets act on
   (the generated file is a weighted sample), which fits a later
   reweighting step to future-year targets.
5. **Not yet tested:** more than one restricted draw per model (the spread
   between seeds is 18.9% to 19.7% of top 1% share); a restriction table
   built from coarser or finer weight groups; whether a model can learn
   the strata exactly, so the table is not needed.

## Unfinished

Nothing is running. Every job listed in
`/nfs/roberts/scratch/pi_nrs36/jar335/probes/uw/jobs.txt` have ended. The
final tables are in `tables_final.md` (collector job 29102652), and every
number above matches that file. The failures (restricted runs before the
fix, two runs out of GPU memory) were rerun and are listed in `jobs.txt`.
Not run:

- More than one restricted draw per trained model, and more seeds for the
  restricted arm B (three seeds, one draw each: top 1% share 18.9% to
  19.7%).
- The weight-group restriction with groups other than "at least 300
  records".
- The restricted given-AGI completion scored as a log-likelihood (the
  restriction is applied only to draws).
- Arm A in random order with the restriction (not possible: the
  restriction needs the weight drawn first).
- A PUF any-order validation loss comparable with test 3's. Each run's
  log has the population-weighted PUF validation loss, but it now includes
  the weight variable, so it is not comparable.

The whole-file levels compare generated records with the PUF training
records the model saw. Reports carry weighted aggregates only. The
restriction table (`support_min300.npz`) holds token sets by weight
group, not records, and stays in scratch.

## To finish

No job is pending. If more draws are wanted:
`sbatch uw_gen.sbatch <checkpoint> <out dir> wfirstsup_uwB_s<seed>_g<n> wfirst <seed> <sampling seed> 664624 /nfs/roberts/scratch/pi_nrs36/jar335/probes/uw/support_min300.npz`.
Then `sbatch uw_collect.sbatch <name>` writes
`/nfs/roberts/scratch/pi_nrs36/jar335/probes/uw/tables_<name>.md` with
every table above. The headline rows are the first table. The columns
are "uwB wfirstsup" for arm B restricted and "uwB givensup" for the
given-AGI test. `uw_train.py` stays in this folder for probe C's
combined arm (`--loss-weight uw` or `uwsqrt`, data
`/nfs/roberts/scratch/pi_nrs36/jar335/probes/uw/full_v2w`).
