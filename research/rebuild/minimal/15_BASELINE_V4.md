---
title: "Minimal example: baseline v4, the fixed model, oversampled stack tilted to 2019 IRS and filer-only SCF targets"
role: evidence
workstream: rebuild
status: current
updated: 2026-10-16
sot: research/rebuild/design/plan.md
supersedes: [research/archive/minimal_07_BASELINE_2026-10-14_superseded.md]
superseded_by: null
---

# Minimal example: baseline v4

This is the baseline. It replaces the v3 baseline of `07_BASELINE.md` (now `research/archive/minimal_07_BASELINE_2026-10-14_superseded.md`), which
was built on the model from before the wealth fix. Later changes are compared
against this one.

**The latest run is v4c** (the last section), and `LATEST.md` says where
each piece of it is.

Two sections at the end follow this one. "v4b: net worth drawn under the
SCF label with other income left out" repeats the build with only the net
worth draw changed (John's decision of 15 October) and sets its results
beside v4's. "v4c: PUF variables first, one base for the wealth counts"
changes the draw order and the base of the wealth count targets (John's
decisions of 16 October) and sets its results beside v4b's.

## What the baseline is

- **The model.** The step 2 transformer trained on table E4: the 2015 PUF
  and the 2016 SCF wave, with the SCF's wages, interest, dividends and
  capital gains placed on the PUF's dollar scale (the translation rule,
  `14_TRANSLATION_RULE.md`). It is trained on every record except a 2%
  held-back slice. Run tag V4.
- **The file.** 400,000 generated PUF-style records with net worth, in four
  parts that oversample the top of wages, capital gains, other_agi and net
  worth.
- **The tilt.** The file reweighted to the 136 IRS targets for 2019, exactly,
  and to six SCF wealth targets for 2019, softly. The wealth targets now come
  only from the 2019 SCF units that would file a return.

Code: `02_model.py`, `02_evaluate.py`, `03_generate.py` (run by
`09_run_one.sbatch`), `03_stack.py`, `15_maps.py`, `07_baseline.py`,
`15_checks.py` (the CPU steps run by `15_run_cpu.sbatch`). Outputs under
`/nfs/roberts/scratch/pi_nrs36/jar335/minimal/`: `model/V4/`, `gen/V4/`,
`gen/gen_puf_stack_v4.parquet`, `map/V4/`, `tilt/v4/`, `checks/V4/`.
Slurm jobs: training 29689487, stack 29690085, maps 29690052, copy check
29690086, stack check 29690089, tilt 29690090, all-units tilt 29691347,
follow-up checks 29693187 (`checks/V4/tilt_more.md`).

## How it was built

**Step 1. The model, trained on all records.** A new switch in
`02_model.py`, `MINIMAL_ALL_RECORDS=1` (off by default), trains on every
record except a 2% held-back slice. The slice is drawn by household with a
fixed seed: on the PUF a household is one record; on the SCF all units and
all five implicates of a household fall on the same side, so no copy of a
held-back SCF household is in training. That leaves 203,568 PUF and 33,667
SCF records for training and 4,124 PUF and 710 SCF records (about 140
households) held back. Training runs all 80 passes and keeps the last; the
held-back loss is watched each pass but never used to choose anything.
Training took 4.4 minutes on one GPU. The same job scored the held-back slice
(`02_evaluate.py`) and generated the plain files, including 200,000
PUF-style records with net worth drawn under the PUF label and the same
under the SCF label.

**Step 2. The stack.** `03_stack.py`, pointed at the V4 model, writes four
files of 100,000 PUF-style records. In each, the bands holding the top 1% of
one variable (wages, capital gains, other_agi, net worth) are drawn ten times
as often as the model would draw them, and each record carries the weight
that undoes this exactly (the model's probability over the sampling
probability). In the net worth file the incomes are drawn first, then net
worth from the boosted distribution given those incomes, then filing status
and dependents. Net worth is drawn under the PUF label throughout, as in run
E12 of `09_RESULTS.md`. Each file's weights are scaled to sum to 100,000.

**Step 3. Wealth targets for the people in the file.** The six 2019 wealth
targets were built from every 2019 SCF tax unit, nonfilers included; the
generated file holds filers only. The targets are now rebuilt from the SCF
units that would file, by this rule:

- Gross income is the sum of the income items the SCF units file has:
  wages, interest, dividends, and capital gains if positive (a loss does not
  lower gross income).
- A unit files if gross income is at least $24,400 for a married couple
  filing jointly, or $12,200 otherwise (the 2019 thresholds).
- The units file has no age and no self-employment income, so the extra
  amount for people 65 or over and the $400 self-employment test are not
  applied. No separate head-of-household threshold is used.
- The incomes in the 2019 wave are 2018 amounts in 2018 dollars; they are
  compared with the 2019 thresholds as given. With the 2018 thresholds
  ($24,000 and $12,000) the rule drops 0.7 points fewer units.

The training data is unchanged: the nonfiler question for training waits.

**Step 4. The tilt.** `07_baseline.py` with `STACK_VERSION=v4`,
`WEALTH_UNITS=filers` and the V4 maps. Starting weights are the importance
weights scaled to SOI's 157.8 million returns for 2019. Incomes get dollars
from the step 4 map grown to 2019; net worth gets dollars from the 2015 SCF
map. The maps are rebuilt from table E4 (`15_maps.py`); they turn out
identical to the v2 maps, record for record, because table E4 bands the
PUF's incomes and the SCF's net worth exactly as the v2 table did. The 136
IRS targets are exact; the six wealth targets are soft, each with a standard
deviation of 2% of its value. The same file is also tilted to the 136
targets alone, and a redraw of 200,000 equal-weight records checks the
result.

**A second tilt, for comparison.** The filer-only targets turned out to
conflict with the IRS return count (section 4 below). So the same file was
also tilted with the six targets built from all 2019 SCF units, as in the v3
baseline and run E12. Its outputs are in `tilt/v4_allunits/` (job 29691347).
Both are reported. The filer-only tilt is the baseline as specified; the
all-units tilt is the one whose numbers can be trusted.

**Which form of the wealth targets was used.** In both tilts the rank
iteration of step 6 did not settle in 50 passes. With filer-only targets the
top 1% missed its target by about 11% after every pass, and the group
membership kept shifting. With all-units targets the passes still changed the
statistics by 0.07% at pass 50, against the stopping rule of 0.01%. So both
tilts used the fallback, the threshold form of `09_score.py`: units and net
worth above the SCF's 50th, 90th and 99th percentiles, total net worth, and
units above $1 million, each soft at 2%. The v3 baseline settled in 14
passes; run E12 used the threshold form too.

## Results

### 1. The model

**Held-back loss.** Cross-entropy in nats on the held-back records, with only
that variable hidden, about half the slots hidden, and all hidden. The last
column is a model that knows nothing but each variable's overall
distribution. Lower is better.

| source | variable | one hidden | half hidden | all hidden | overall distribution only |
|---|---|---:|---:|---:|---:|
| PUF | married | 0.445 | 0.532 | 0.658 | 0.657 |
| PUF | n_dep | 0.914 | 0.962 | 1.022 | 1.022 |
| PUF | wages | 3.380 | 3.540 | 3.806 | 3.806 |
| PUF | interest | 1.285 | 1.363 | 1.541 | 1.541 |
| PUF | dividends | 0.890 | 0.966 | 1.148 | 1.148 |
| PUF | capital_gains | 0.633 | 0.690 | 0.833 | 0.833 |
| PUF | other_agi | 2.782 | 2.961 | 3.228 | 3.228 |
| SCF | married | 0.558 | 0.600 | 0.650 | 0.653 |
| SCF | n_dep | 0.868 | 0.909 | 0.971 | 0.972 |
| SCF | wages | 2.983 | 3.069 | 3.278 | 3.280 |
| SCF | interest | 0.892 | 0.926 | 1.058 | 1.058 |
| SCF | dividends | 0.597 | 0.661 | 0.834 | 0.832 |
| SCF | capital_gains | 0.384 | 0.422 | 0.554 | 0.553 |
| SCF | net_worth | 3.673 | 3.783 | 4.093 | 4.096 |

The gain over the overall distribution is about the same as E4's on its own
test split (wages, half hidden: 0.27 nats here, 0.25 for E4). The numbers
themselves are not comparable with E4's, because the held-back records are
different records.

**Training loss against held-back loss**, under the training's own random
hiding (`train_meta.json`). If the model had memorised its training records,
the training loss would be clearly lower.

| variable | PUF, training | PUF, held back | SCF, training | SCF, held back |
|---|---:|---:|---:|---:|
| married | 0.567 | 0.576 | 0.604 | 0.622 |
| n_dep | 0.974 | 0.984 | 0.972 | 0.934 |
| wages | 3.603 | 3.626 | 3.241 | 3.199 |
| interest | 1.447 | 1.419 | 0.756 | 0.993 |
| dividends | 1.085 | 1.044 | 0.728 | 0.738 |
| capital_gains | 0.745 | 0.751 | 0.531 | 0.462 |
| other_agi | 3.084 | 3.055 | — | — |
| net_worth | — | — | 3.893 | 3.912 |

The PUF columns agree to within about 0.04 nats either way. The SCF slice is only
about 140 households, so its column is noisy: SCF interest is 0.24 worse held
back, while SCF capital gains and n_dep are better held back.

**Label swap.** How much worse the model predicts a variable on PUF records
when told they are SCF records (nats, one hidden): interest 0.174, wages
0.176, dividends 0.031, capital gains 0.051. E4 had 0.174 for interest. The
model still uses the source label to learn the SCF's larger share of zeros.

**Copy check.** The 200,000 generated PUF-style records against the training
records and the held-back records, matched exactly on all seven PUF slots
(filing status, dependents, and the bands of the five incomes).

| generated records | match a training record | match a held-back record | match a training slice the size of the held-back slice | matches per record, training ÷ matches per record, held back |
|---|---:|---:|---:|---:|
| all 200,000 | 65.7% | 36.2% | 36.6% | 0.96 |
| the 3,085 with an amount in the top 1% bands | 1.62% | 0.03% (1 record) | 0.03% (1 record) | too few matches to read |

- The first column is higher than the second only because the training set
  is 49 times larger. Against a training slice of the same size as the
  held-back slice, the share is the same (36.6% against 36.2%).
- The last column divides each set's matches by its number of records. A
  model that copied would score well above 1. It scores 0.96.
- At the top, generated records almost never repeat any real record exactly
  (50 of 3,085 match a training record), and there are too few matches to
  compare the two sets.
- On the 40,000 SCF-style records the last column is 1.01. The held-back
  share there (9.4%) is below the same-size training slice (18.2%) only
  because the held-back slice is drawn by household, and the five
  implicates of a household are near copies of each other.

The model does not copy.

### 2. The stack

The importance weights sum to exactly 100,000 in each of the four files and
400,000 in all. Compared with the plain V4 file of 200,000 records, and with a
second plain file drawn with another seed as the measure of noise (2015
dollars):

| statistic | plain file | plain file, second seed | stack |
|---|---:|---:|---:|
| has interest | 27.4% | 27.6% | 27.5% |
| has dividends | 17.8% | 17.8% | 17.5% |
| has capital gains | 12.8% | 12.8% | 12.7% |
| joint returns | 36.1% | 36.2% | 36.2% |
| net worth, mean | $595,000 | $594,000 | $598,000 |
| net worth, median | $74,400 | $74,400 | $74,700 |
| top 10% share | 77.6% | 77.6% | 77.8% |
| top 1% share | 40.0% | 39.4% | 40.4% |
| top 0.1% share | 14.1% | 13.8% | 15.8% |
| mean net worth, no wages | $1,320,000 | $1,260,000 | $1,290,000 |
| mean net worth, wages over $200,000 | $3,330,000 | $3,580,000 | $3,610,000 |

Every band-level distance between the stack and the plain file is within the
distance between the two plain files (`gen/03_stack_tables_v4.md`). Two
gaps are a little larger than the seed-to-seed gap: dividend holders 0.23
points lower, and the top 0.1% share 1.7 points higher. The top 0.1% rests on
about 170 records in a plain file and about 900 in the stack, so the stack's
figure is the better estimate. The stack has 5 to 8 times as many records as
the plain file in the top 1% bands of each oversampled variable.

### 3. What the filing rule drops

| 2019 SCF units | units (millions) | would file (millions) | share of units dropped | net worth ($ trillion, 2015 dollars) | share of net worth dropped |
|---|---:|---:|---:|---:|---:|
| all | 143.2 | 91.1 | 36.4% | 89.1 | 18.6% |
| married filing jointly | 56.7 | 41.2 | 27.4% | 68.0 | 15.4% |
| everyone else | 86.5 | 49.9 | 42.3% | 21.1 | 29.0% |

The rule drops 9,810 of the 31,830 unit records. Units with no wages fall
from 29.8% of SCF units to 2.4% of filers. The few units with no wages that
remain pass only on investment income, so they are very rich: their mean
net worth is $7.2 million and their median $3.6 million. On the PUF, 17% of
returns have no wages.

The six targets before and after (trillions of 2015 dollars; units in
millions):

| target | all 2019 SCF units | 2019 SCF units that would file |
|---|---:|---:|
| total net worth | 89.1 | 72.6 |
| top 1% | 34.2 | 28.3 |
| next 9% | 35.1 | 29.9 |
| next 40% | 18.9 | 13.7 |
| bottom 50% | 0.956 | 0.677 |
| units above $1 million | 15.3 | 11.5 |

### 4. The tilt: move and targets

| tilt | move (nats) | effective sample size kept | records below 0.1× their start weight | records at weight zero |
|---|---:|---:|---:|---:|
| 136 IRS targets alone | 0.0080 | 98.0% | 0 | 0 |
| 136 + six wealth targets, filers only (the baseline as specified) | 0.131 | 79.2% | 13,652 | 9,546 |
| 136 + six wealth targets, all units (comparison) | 0.0184 | 96.3% | 1,911 | 0 |

For reference, the v3 baseline moved 0.0093 nats for the 136 alone and 0.077
with the wealth targets, and run E12 moved 0.0113 and 0.0245.

**The 136 IRS targets** are met exactly in both tilts: the largest gap is
0.0000000001%.

**The six wealth targets, threshold form, after the tilt** (miss in %; each
has a standard deviation of 2%):

| target | filers only: target | filers only: miss | all units: target | all units: miss |
|---|---:|---:|---:|---:|
| units above the SCF's 99th percentile | 1.58 million | −0.00% | 1.58 million | −0.00% |
| net worth above the 99th percentile | $28.2 trillion | −0.00% | $34.2 trillion | +0.00% |
| units above the SCF's 90th percentile | 15.8 million | **−17.7%** | 15.8 million | −1.7% |
| net worth above the 90th percentile | $58.2 trillion | −0.06% | $69.3 trillion | −0.01% |
| units above the SCF's median | 78.9 million | −0.02% | 78.9 million | −0.00% |
| net worth above the median | $71.9 trillion | +0.05% | $88.2 trillion | +0.02% |
| total net worth | $72.6 trillion | +0.03% | $89.1 trillion | −0.02% |
| units above $1 million (2019 dollars) | 11.5 million | **+12.9%** | 15.3 million | +1.7% |

With all-units targets every wealth target is met within one standard
deviation. With filer-only targets two targets contradict each other and
cannot both be met. Among SCF filers the 90th percentile is $1.26 million.
Scaled to the 157.8 million returns, 15.8 million units should be above it,
but only 11.5 million units should be above $1 million, a lower line. The
tilt settles between the two, at 13.0 million, and misses both by many
standard deviations. The cause is that the filer rule keeps 91 million SCF
units, while the IRS counts 158 million returns.

**The redraw** (200,000 equal-weight records drawn in proportion to the
tilted weights). Filer-only tilt: 131 of the 136 IRS targets within 2
standard errors, worst |z| 2.95. All-units tilt: 124 of 136, worst |z| 3.21
(dividends and capital gains of returns with no AGI, +50% and +70%). The
v3 baseline had 134 of 136. The largest miss in percent in both is capital
gains in the $1 to $5,000 AGI bracket, an amount near zero where gains and
losses cancel. On the redraw, the filer-only wealth statistics in rank form
miss total net worth by +0.4% and the top 1% by +0.5%.

### 5. Holder shares (% of returns)

| item | pretrain | posttrain (either tilt) | PUF 2015 | IRS 2015 | IRS 2019 |
|---|---:|---:|---:|---:|---:|
| interest | 27.5 | 30.8 | 28.3 | 28.3 | 30.8 |
| dividends | 17.5 | 18.4 | 18.4 | 18.3 | 18.4 |
| capital gains | 12.7 | 13.8 | 13.3 | 13.3 | 13.8 |

Before the tilt the generated file is 0.6 to 0.8 points below the 2015 PUF.
After the tilt the shares equal the 2019 IRS shares exactly, because the
counts are targets.

### 6. Wealth against the actual SCF

All net worth in 2015 dollars. The 2019 SCF's wages are put in 2015 dollars
for the groupings (the v3 tables left them in 2018 dollars). Generated
records are grouped by their 2015-dollar draws. "Posttrain, filers" is the
baseline as specified; "posttrain, all units" is the comparison tilt.

**Net worth, all units**

| | 2019 SCF, all units | 2019 SCF, filers | pretrain | posttrain, filers | posttrain, all units |
|---|---:|---:|---:|---:|---:|
| units (millions) | 143 | 91.1 | 158 | 158 | 158 |
| total ($ trillion) | 89.1 | 72.6 | 94.4 | 72.6 | 89.1 |
| mean | $623,000 | $797,000 | $598,000 | $460,000 | $565,000 |
| median | $84,500 | $89,200 | $74,700 | $88,900 | $84,500 |
| share negative | 10.7% | 12.4% | 10.3% | 10.4% | 9.6% |
| top 10% share | 77.7% | 80.2% | 77.8% | 82.5% | 78.0% |
| top 1% share | 38.4% | 38.9% | 40.4% | 38.9% | 38.4% |
| top 0.1% share | 14.7% | 14.9% | 15.8% | 6.8% | 12.6% |
| 90th percentile | $992,000 | $1,260,000 | $1,110,000 | $481,000 | $902,000 |
| 99th percentile | $9,400,000 | $12,300,000 | $9,290,000 | $12,200,000 | $9,390,000 |
| 99.9th percentile | $39,100,000 | $49,800,000 | $39,700,000 | $24,400,000 | $37,700,000 |

**Mean net worth by wage group ($)**

| wages | 2019 SCF, all units | 2019 SCF, filers | pretrain | posttrain, filers | posttrain, all units |
|---|---:|---:|---:|---:|---:|
| 0 | 706,000 | 7,210,000 | 1,290,000 | 963,000 | 1,180,000 |
| 1-25k | 203,000 | 217,000 | 248,000 | 172,000 | 231,000 |
| 25-50k | 148,000 | 148,000 | 246,000 | 171,000 | 226,000 |
| 50-100k | 333,000 | 333,000 | 424,000 | 314,000 | 395,000 |
| 100-200k | 922,000 | 922,000 | 873,000 | 648,000 | 816,000 |
| 200k+ | 4,500,000 | 4,500,000 | 3,610,000 | 3,110,000 | 3,370,000 |

**Median net worth by wage group ($)**

| wages | 2019 SCF, all units | 2019 SCF, filers | pretrain | posttrain, filers | posttrain, all units |
|---|---:|---:|---:|---:|---:|
| 0 | 109,000 | 3,590,000 | 285,000 | 192,000 | 256,000 |
| 1-25k | 11,300 | 7,980 | 8,340 | 9,080 | 9,400 |
| 25-50k | 26,000 | 26,000 | 27,900 | 30,400 | 29,900 |
| 50-100k | 109,000 | 109,000 | 106,000 | 105,000 | 111,000 |
| 100-200k | 362,000 | 362,000 | 280,000 | 181,000 | 269,000 |
| 200k+ | 1,680,000 | 1,680,000 | 1,120,000 | 722,000 | 1,020,000 |

**Share of units in each wage group**

| wages | 2019 SCF, all units | 2019 SCF, filers | pretrain | posttrain, filers | posttrain, all units |
|---|---:|---:|---:|---:|---:|
| 0 | 29.8% | 2.4% | 17.1% | 17.8% | 17.8% |
| 1-25k | 17.4% | 14.7% | 32.2% | 30.7% | 30.7% |
| 25-50k | 18.6% | 29.3% | 21.4% | 22.1% | 22.1% |
| 50-100k | 19.5% | 30.7% | 17.8% | 18.1% | 17.9% |
| 100-200k | 10.4% | 16.3% | 9.0% | 8.7% | 8.7% |
| 200k+ | 4.2% | 6.6% | 2.5% | 2.7% | 2.8% |

**Share of units with net worth above $1 million, by wage group**

| wages | 2019 SCF, all units | 2019 SCF, filers | pretrain | posttrain, filers | posttrain, all units |
|---|---:|---:|---:|---:|---:|
| 0 | 10.9% | 79.2% | 24.3% | 18.9% | 21.5% |
| 1-25k | 3.7% | 3.6% | 4.2% | 2.8% | 3.7% |
| 25-50k | 2.1% | 2.1% | 4.3% | 2.5% | 3.5% |
| 50-100k | 4.5% | 4.5% | 8.1% | 5.2% | 6.9% |
| 100-200k | 18.9% | 18.9% | 18.3% | 13.3% | 16.2% |
| 200k+ | 66.7% | 66.7% | 53.0% | 49.0% | 50.5% |

The full tables, including filing status, dividend groups and the 2016 wave,
are in `tilt/v4/tables_v4.md` and `tilt/v4_allunits/tables_v4.md`.

### 7. Relationships no target covers (% of returns)

"Actual 2015 PUF" is the real PUF with its survey weights and its dollars
grown to 2019.

| relationship | group | actual 2015 PUF | pretrain | posttrain, filers | posttrain, all units |
|---|---|---:|---:|---:|---:|
| any dividends | no wages, joint | 43.4 | 43.8 | 45.3 | 44.8 |
| any dividends | no wages, not joint | 31.1 | 27.5 | 28.3 | 28.6 |
| any dividends | wages 1-25k, joint | 22.9 | 23.2 | 22.5 | 23.7 |
| any dividends | wages 1-25k, not joint | 5.2 | 4.5 | 5.0 | 4.9 |
| any dividends | wages 25-50k, joint | 15.3 | 15.9 | 15.0 | 15.8 |
| any dividends | wages 25-50k, not joint | 5.9 | 5.5 | 5.7 | 5.6 |
| any dividends | wages 50-100k, joint | 18.9 | 19.9 | 19.6 | 20.3 |
| any dividends | wages 50-100k, not joint | 13.2 | 12.2 | 13.5 | 12.9 |
| any dividends | wages 100-200k, joint | 30.3 | 29.6 | 31.3 | 31.4 |
| any dividends | wages 100-200k, not joint | 28.8 | 27.1 | 29.3 | 29.0 |
| any dividends | wages 200k+, joint | 61.1 | 57.8 | 62.5 | 61.9 |
| any dividends | wages 200k+, not joint | 57.7 | 56.3 | 60.2 | 60.7 |
| any capital gains | no dividends | 3.7 | 3.6 | 3.9 | 4.0 |
| any capital gains | dividends 1-1k | 38.7 | 37.7 | 39.4 | 39.1 |
| any capital gains | dividends 1k-10k | 67.5 | 67.4 | 70.7 | 69.2 |
| any capital gains | dividends 10k-100k | 87.0 | 86.4 | 89.0 | 88.0 |
| any capital gains | dividends 100k+ | 93.5 | 87.6 | 91.7 | 90.3 |
| no dependents | joint | 49.7 | 54.3 | 53.3 | 54.5 |
| no dependents | not joint | 73.3 | 78.5 | 78.0 | 78.5 |
| 1 dependent | joint | 18.1 | 17.7 | 18.0 | 17.6 |
| 1 dependent | not joint | 15.2 | 13.4 | 13.6 | 13.3 |
| 2 dependents | joint | 19.8 | 17.8 | 18.2 | 17.8 |
| 2 dependents | not joint | 8.2 | 5.5 | 5.7 | 5.5 |
| 3+ dependents | joint | 12.3 | 10.2 | 10.5 | 10.1 |
| 3+ dependents | not joint | 3.4 | 2.6 | 2.7 | 2.6 |

The tilt moves none of these by more than 4.7 points (filers only) or 4.4
points (all units); the largest moves are dividends among wage earners above
$200,000.

## What it shows and what still looks wrong

1. The fixed model works before any tilt. Its PUF-style file holds $94.4
   trillion of net worth for 158 million returns, against $89.1 trillion
   for 143 million SCF units, so the mean is 4% below the SCF's and the
   median 12% below. The top 1% holds 40.4% against 38.4%. The v3 stack held
   $171 trillion.
2. Training on all records does no harm. The model does not copy its
   training records, and it scores the held-back slice about as well as
   its training records.
3. With the all-units wealth targets, wealth costs 0.010 nats on top of the
   136 IRS targets (0.008), and 96% of the effective sample is kept. In the
   v3 baseline wealth cost 0.068. Mean, median and top 1% share then match
   the 2019 SCF.
4. The filing rule as specified does not describe who files. It keeps 91
   million SCF units against the IRS's 158 million returns, and it keeps
   almost no one without wages (2.4% of filers against 17% of PUF returns).
   This is because the SCF units file has no Social Security, pension,
   business or rental income, and because many returns are filed below the
   threshold, for refunds. The filer-only targets then contradict the return
   count. The tilt to them costs 0.131 nats, puts 9,546 records at zero
   weight, misses two wealth targets by 13% and 18%, and leaves the top 0.1%
   with 6.8% of net worth against 14.9%. The filer-only tilt should not be
   used until the rule is fixed; that is John's call.
5. The very top is still thin after any tilt. Even with all-units targets
   the top 0.1% holds 12.6% against 14.7%.
6. Wealth by wage group is still off at both ends. Returns with no wages
   hold a mean of $1.18 million after the all-units tilt, against $706,000
   for SCF units with no wages, and their median is 2.3 times the SCF's.
   Earners above $200,000 hold $3.37 million against $4.50 million. Part of
   the first gap is the population difference: 30% of SCF units have no
   wages, against 17% of returns.
7. Dependents are short of the PUF before and after the tilt: joint returns
   with no dependents are 54% against 50%, and non-joint returns with two or
   more dependents 8.1% against 11.6%. No target touches them.
8. The redraw is a little worse than in v3. With all-units targets 124 of
   the 136 IRS targets are within 2 standard errors, against 134 in v3. The
   misses sit in thin cells (no AGI, $5,000 to $10,000, $500,000 to $1
   million).

## v4b: net worth drawn under the SCF label with other income left out

### What changed

John decided on 15 October that a borrowed variable (net worth on a PUF
record) is drawn under the label of the survey that owns it (the SCF), with
the record's slots that the owner never has (other_agi) left out entirely.
The model then sees, at the moment it draws net worth, exactly what an SCF
record looks like. The v4 stack drew net worth under the PUF label with
other_agi visible, a combination the model never saw in training
(`16_MODEL_SIDE_FIXES.md`, section 2, problem 2).

v4b repeats the v4 build with that one change and nothing else:

- The same trained model, `model/V4`, not retrained.
- The same four files of 100,000 records, the same seeds (31 to 34) and the
  same tenfold oversampling of the top 1% of wages, capital gains,
  other_agi and net worth. In all four files net worth is drawn under the
  SCF label with other_agi left out. Every draw before net worth is
  identical to v4 (the three income-stratified files have exactly the same
  number of top records from their own file as in v4); married and
  dependents, drawn after net worth, differ.
- The plain file for the stack check (200,000 records, seed 3) and the
  second-seed file (seed 35) are drawn under the same rule.
- The tilt to the 136 IRS targets alone, and to the 136 plus the six
  all-units 2019 SCF wealth targets, exactly as `tilt/v4_allunits`. As in
  v4, the rank iteration did not settle in 50 passes and the threshold
  form was used. The filer-only tilt was not run: John has set it aside
  until the filing rule is fixed.

Code changes, each a new environment setting whose default keeps the v4
behaviour: `03_stack.py` takes `MINIMAL_STACK_FOREIGN` (`absent` for v4b)
and `MINIMAL_STACK_PLAIN` (the plain file, generated if missing);
`15_checks.py` takes `MINIMAL_CHECKS_DIR`, `MINIMAL_STACK_PLAIN` and
`MINIMAL_TILT_TAGS`; `15_run_cpu.sbatch` passes `STACK_VERSION` through.
Outputs under `/nfs/roberts/scratch/pi_nrs36/jar335/minimal/`:
`gen/gen_puf_stack_v4b.parquet`, `gen/gen_meta_v4b.json`,
`gen/03_stack_tables_v4b.md`, `gen/gen_plain_seed35_v4b.parquet`,
`gen/V4b/gen_puf_with_net_worth_ownerlabel_absent.parquet`,
`tilt/v4b_allunits/`, `checks/V4b/`. No v4 output was changed. Slurm
jobs: stack 29701513 (GPU), tilt 29701516, stack check 29701517, follow-up
checks 29701518.

In the tables below, "v4" is the v4 stack and its all-units tilt, and
"v4b" the new stack and its all-units tilt. The SCF columns are those of
section 6 above (2019 wave, all units, wages put in 2015 dollars for the
groupings). All net worth is in 2015 dollars.

### 1. Wealth before and after the tilt

| | 2019 SCF, all units | v4 pretrain | v4b pretrain | v4 posttrain | v4b posttrain |
|---|---:|---:|---:|---:|---:|
| units (millions) | 143 | 158 | 158 | 158 | 158 |
| total ($ trillion) | 89.1 | 94.4 | 88.2 | 89.1 | 89.1 |
| mean | $623,000 | $598,000 | $559,000 | $565,000 | $565,000 |
| median | $84,500 | $74,700 | $78,500 | $84,500 | $84,500 |
| share negative | 10.7% | 10.3% | 12.2% | 9.6% | 11.8% |
| top 10% share | 77.7% | 77.8% | 74.4% | 78.0% | 78.0% |
| top 1% share | 38.4% | 40.4% | 37.0% | 38.4% | 38.4% |
| top 0.1% share | 14.7% | 15.8% | 14.6% | 12.6% | 13.2% |
| 90th percentile | $992,000 | $1,110,000 | $1,080,000 | $902,000 | $902,000 |
| 99th percentile | $9,400,000 | $9,290,000 | $8,020,000 | $9,390,000 | $9,390,000 |
| 99.9th percentile | $39,100,000 | $39,700,000 | $36,900,000 | $37,700,000 | $38,900,000 |

Before the tilt, v4b holds $88.2 trillion against the SCF's $89.1 trillion
(1% short); v4 held $94.4 trillion (6% over). But the shape moved the other
way at the top: v4b's top 1% holds 37.0% against the SCF's 38.4%, where
v4 held 40.4%, and its top 10% holds 74.4% against 77.7%. The six wealth
targets in rank form before the tilt:

| wealth target | v4 pretrain miss | v4b pretrain miss | v4 after the 136 alone | v4b after the 136 alone |
|---|---:|---:|---:|---:|
| total net worth | +5.9% | −1.1% | +16.0% | +7.4% |
| top 1% | +11.3% | −4.8% | +22.4% | +4.4% |
| next 9% | +0.6% | −5.8% | +11.4% | +3.0% |
| next 40% | +7.8% | +15.1% | +15.2% | +22.0% |
| bottom 50% | −35.4% | −18.9% | −27.4% | −8.6% |
| units above $1 million | +17.9% | +19.3% | +25.8% | +26.5% |

So v4b's file is closer to the SCF at the top and at the bottom, and
further from it in the middle: the next 40% (units between the median and
the 90th percentile) hold 15% too much.

After the tilt, the threshold-form wealth targets are met as in v4: every
target within 0.02%, except units above the SCF's 90th percentile (−1.7%)
and units above $1 million (+1.7%), the same pair and the same misses as in
v4. The 136 IRS targets are exact (largest gap 0.000000000008%).

### 2. The move and the effective sample kept

| tilt | v4 move (nats) | v4b move (nats) | v4 effective sample kept | v4b effective sample kept | v4 records below 0.1× start weight | v4b records below 0.1× start weight | records at weight zero (both) |
|---|---:|---:|---:|---:|---:|---:|---:|
| 136 IRS targets alone | 0.0080 | 0.0093 | 98.0% | 98.0% | 0 | 11 | 0 |
| 136 + six all-units wealth targets | 0.0184 | 0.0243 | 96.3% | 95.7% | 1,911 | 2,276 | 0 |

Wealth costs 0.015 nats on top of the 136 targets in v4b, against 0.010 in
v4. The 136 targets alone cost a little more too (0.0093 against 0.0080).
The likely reason is the joint share: married is drawn after net worth, and
v4b's file has 36.8% joint returns before the tilt against v4's 36.2%,
further from the IRS's 34.7%.

### 3. Wealth by wage group against the SCF

**Mean net worth ($)**

| wages | 2019 SCF, all units | v4 pretrain | v4b pretrain | v4 posttrain | v4b posttrain |
|---|---:|---:|---:|---:|---:|
| 0 | 706,000 | 1,290,000 | 1,190,000 | 1,180,000 | 1,190,000 |
| 1-25k | 203,000 | 248,000 | 206,000 | 231,000 | 200,000 |
| 25-50k | 148,000 | 246,000 | 207,000 | 226,000 | 190,000 |
| 50-100k | 333,000 | 424,000 | 403,000 | 395,000 | 372,000 |
| 100-200k | 922,000 | 873,000 | 887,000 | 816,000 | 844,000 |
| 200k+ | 4,500,000 | 3,610,000 | 3,710,000 | 3,370,000 | 4,040,000 |

**Median net worth ($)**

| wages | 2019 SCF, all units | v4 pretrain | v4b pretrain | v4 posttrain | v4b posttrain |
|---|---:|---:|---:|---:|---:|
| 0 | 109,000 | 285,000 | 262,000 | 256,000 | 232,000 |
| 1-25k | 11,300 | 8,340 | 8,330 | 9,400 | 8,710 |
| 25-50k | 26,000 | 27,900 | 36,100 | 29,900 | 38,300 |
| 50-100k | 109,000 | 106,000 | 144,000 | 111,000 | 135,000 |
| 100-200k | 362,000 | 280,000 | 387,000 | 269,000 | 316,000 |
| 200k+ | 1,680,000 | 1,120,000 | 1,400,000 | 1,020,000 | 1,430,000 |

**Share of units with net worth above $1 million**

| wages | 2019 SCF, all units | v4 pretrain | v4b pretrain | v4 posttrain | v4b posttrain |
|---|---:|---:|---:|---:|---:|
| 0 | 10.9% | 24.3% | 23.3% | 21.5% | 21.1% |
| 1-25k | 3.7% | 4.2% | 3.7% | 3.7% | 3.1% |
| 25-50k | 2.1% | 4.3% | 3.3% | 3.5% | 2.6% |
| 50-100k | 4.5% | 8.1% | 7.4% | 6.9% | 5.9% |
| 100-200k | 18.9% | 18.3% | 22.4% | 16.2% | 19.3% |
| 200k+ | 66.7% | 53.0% | 64.7% | 50.5% | 63.3% |

The share of units in each wage group is the same in both files to within
0.3 points (wages are drawn before net worth, and the tilt sets the
counts).

**The zero-wage problem does not change.** Returns with no wages hold a
mean of $1.19 million after the v4b tilt, against $1.18 million in v4 and
$706,000 for SCF units with no wages. Their median falls from $256,000 to
$232,000, still more than twice the SCF's $109,000, and 21% of them are
above $1 million against 11% in the SCF. Leaving other_agi out of the net
worth draw does not touch this group.

**Earners above $200,000 move most of the way to the SCF.** Their mean
rises from $3.37 million to $4.04 million (SCF $4.50 million), their median
from $1.02 million to $1.43 million (SCF $1.68 million), and the share
above $1 million from 50.5% to 63.3% (SCF 66.7%). The mean net worth of
wage earners from $1 to $100,000 also moves closer to the SCF's.

**The middle moves away.** The median of wage earners from $25,000 to
$100,000 rises above the SCF's: $38,300 against $26,000, and $135,000
against $109,000. This is the same excess as the next-40% target before
the tilt (+15%).

### 4. Holder shares (% of returns)

| item | v4 pretrain | v4b pretrain | posttrain (both) | PUF 2015 | IRS 2019 |
|---|---:|---:|---:|---:|---:|
| interest | 27.5 | 27.4 | 30.8 | 28.3 | 30.8 |
| dividends | 17.5 | 17.6 | 18.4 | 18.4 | 18.4 |
| capital gains | 12.7 | 12.8 | 13.8 | 13.3 | 13.8 |

Unchanged, as expected: the incomes are drawn before net worth.

### 5. The redraw

200,000 equal-weight records drawn in proportion to the all-units tilted
weights, same seed as v4.

| | v4 | v4b |
|---|---:|---:|
| IRS targets within 2 standard errors | 124 of 136 | 134 of 136 |
| largest \|z\| | 3.21 | 2.87 |
| largest miss in % (capital gains, AGI $1 to $5,000) | 136% | 89% |
| total net worth, miss | +4.1% | +0.1% |
| top 1% net worth, miss | +9.1% | +0.0% |
| distinct records | 155,113 | 154,760 |

The two misses beyond 2 standard errors in v4b are capital gains in the
$75,000 to $100,000 AGI bracket (+15%, z 2.87) and the count of joint
returns (−0.7%, z −2.14). The redraw is one draw, so part of the
improvement can be luck; but v4's redraw missed the top 1% of net worth by
9%, which points to a few heavy-weight records at the top of the v4 file,
and v4b does not show this.

### 6. Dependents (% of returns)

| relationship | actual 2015 PUF | v4 pretrain | v4b pretrain | v4 posttrain | v4b posttrain |
|---|---:|---:|---:|---:|---:|
| non-joint returns with 2 or more dependents | 11.6 | 8.1 | 8.7 | 8.1 | 8.7 |
| joint returns with no dependents | 49.7 | 54.3 | 54.0 | 54.5 | 54.0 |

A small improvement, about half a point, from drawing dependents after a
net worth drawn under the SCF label. Most of the gap to the PUF remains.

### 7. The stack against its plain file

The importance weights sum to 100,000 in each file and 400,000 in all.
Most gaps between the v4b stack and the v4b plain file are about the size
of the gap between two plain files drawn with different seeds
(`checks/V4b/stack_check.md`). The largest relative to that gap: the
capital gains holder share (stack 0.22 points above the plain file,
against 0.05 between seeds), the joint share (0.22 against 0.10), and the
mean net worth of earners above $200,000 (stack $3.71 million, plain file
$4.15 million, second seed $3.88 million). The seed-3 plain file is itself
high at the top (top 0.1% share 16.1%, against 14.7% for the second seed
and 14.6% for the stack). In the net worth file 6,742 records fall in the
top 1% of net worth, against 8,832 in v4: under the SCF label the model
puts less weight on those ranges.

### What v4b shows

1. Drawing net worth as an SCF record would be drawn brings the total
   before the tilt to within 1% of the SCF (v4: 6% over) and cuts the
   bottom-50% shortfall from 35% to 19%.
2. It fixes much of the shortfall among earners above $200,000: mean $4.04
   million after the tilt, against $3.37 million in v4 and $4.50 million in
   the SCF.
3. It does not change the zero-wage problem: $1.19 million against $706,000
   in the SCF, the same as v4.
4. It overstates the middle: the next 40% hold 15% too much before the
   tilt, and wage earners from $25,000 to $100,000 have medians above the
   SCF's after it.
5. The tilt costs a little more (0.0243 nats against 0.0184; 95.7% of the
   effective sample kept against 96.3%), and the redraw is better (134 of
   136 targets within 2 standard errors against 124).

## v4c: PUF variables first, one base for the wealth counts

### What changed

John made two decisions on 16 October (`design/plan.md`, decisions 9 and
17).

1. **Draw order.** On a PUF record, every PUF variable is drawn first under
   the PUF label: married, dependents, wages, interest, dividends, capital
   gains and other income, in a random order for each record. Net worth is
   drawn last, under the SCF label with other income left out (decision
   16). While the PUF variables are drawn the record has no net worth slot
   at all, as on a PUF training record. In v4b the incomes came first, then
   net worth, then married, then dependents, so married and dependents were
   drawn under the PUF label with net worth visible, a combination no PUF
   record has. Arm C of `20_SEPARATE_MODELS.md` used the new order, except
   that it drew married and dependents after the incomes rather than in
   the random order with them.
2. **One base for every wealth count target.** v4b's eight wealth targets
   put the count above $1 million on the SCF's own base (15.25 million of
   its 143 million units) and the counts above the SCF's 50th, 90th and
   99th percentile lines on the IRS return count (the SCF's share times
   157.8 million returns). The $1 million line ($927,884 in 2015 dollars)
   is below the 90th percentile line ($991,815), but the targets asked for
   fewer units above it (15.25 million) than above the 90th percentile
   (15.77 million). No weights can meet both, and the v4b tilt emptied the
   band between the two lines (`19_DIRECT.md`, section 9). Now every count
   is on one base.

The coordinator then asked for the base to be a setting and for v4c to be
tilted both ways, because the right base is not obvious:

- **IRS base** (`WEALTH_COUNT_BASE=irs_returns`, the default for now):
  every count is the SCF's share of units above the line times the IRS
  return count. This assumes the 15 million returns the IRS counts beyond
  the SCF's units are spread across the wealth distribution as SCF units
  are.
- **SCF base** (`WEALTH_COUNT_BASE=scf_units`): every count is the SCF's own
  weighted count. This assumes those 15 million returns all sit below every
  line, as they would if they were mostly dependents filing their own
  returns with little wealth. The counts at the top are then about 10%
  lower than on the IRS base.

Dollar targets (net worth above each line, and total net worth) are the
SCF's own amounts on both bases. Which base becomes the rule is John's
decision.

The eight targets (2015 dollars):

| target | v4b (mixed base) | IRS base | SCF base |
|---|---:|---:|---:|
| units above the SCF 50th percentile ($84,484), millions | 78.89 | 78.89 | 71.59 |
| units above $1 million in 2019 dollars ($927,884), millions | **15.25** | **16.81** | 15.25 |
| units above the SCF 90th percentile ($991,815), millions | **15.77** | 15.77 | 14.31 |
| units above the SCF 99th percentile ($9,403,546), millions | 1.58 | 1.58 | 1.43 |
| net worth above the 50th percentile, $ trillion | 88.19 | 88.19 | 88.19 |
| net worth above the 90th percentile, $ trillion | 69.27 | 69.27 | 69.27 |
| net worth above the 99th percentile, $ trillion | 34.21 | 34.21 | 34.21 |
| total net worth, $ trillion | 89.14 | 89.14 | 89.14 |

On both new bases the counts fall as the line rises, the net worth above a
line falls as the line rises, the mean net worth above each line is above
the line, and the mean net worth of the units between two lines lies
between the lines. On v4b's mixed base the one check that fails is the
first: 15.25 million above $927,884 against 15.77 million above $991,815.
`07_baseline.py` now runs these checks before every tilt and stops if one
fails (`consistency_<v>.csv`).

Mean net worth of the units above each line, as the targets imply it:

| line | IRS base | SCF base |
|---|---:|---:|
| above $84,484 | $1,120,000 | $1,230,000 |
| above $991,815 | $4,390,000 | $4,840,000 |
| above $9,403,546 | $21,700,000 | $23,900,000 |

Every tilt hits these exactly (the counts and amounts are targets).

### What ran

- The same trained model, `model/V4`, not retrained, and the same V4 maps.
- The same four files of 100,000 records, seeds 31 to 34, tenfold
  oversampling of the top 1% of wages, capital gains, other income and net
  worth, weights that undo it exactly. The one change in the stack is the
  order. In the three income-stratified files the stratifier is drawn first
  from the model's distribution for a PUF record with no net worth slot;
  then the other six PUF variables in a random order; then net worth. In
  the net worth file the seven PUF variables come first in a random order,
  then net worth from the boosted conditional.
- The plain file (200,000 records, seed 3) and the second-seed file (seed
  35) under the same rule.
- The tilt to the 136 IRS targets alone, and to the 136 plus the eight
  wealth targets, once on each base. The 136 alone are the same on both.
- **One extra tilt:** the v4b stack tilted to the IRS-base targets
  ("v4b, IRS base" below). It changes only the targets, so comparing it with
  v4b shows what the target fix does, and comparing it with v4c shows what
  the order does.
- The checks: stack against plain file, the redraw, the untargeted
  relationships. The copy check is a property of the model, which did not
  change, so it was not rerun.

One difference from v4b that is not a choice: v4b's stack ran on an NVIDIA
H200, v4c's on an RTX A5000 (the first try failed on a hardware memory error
on its GPU, and no H200 was free). The same seed gives different draws on different GPU
types; `20_SEPARATE_MODELS.md` measured that difference at about 0.1 in the
ratio of total wealth to the SCF's.

Code changes, each a setting whose default keeps earlier runs reproducible
unless noted:

- `03_generate.py`: `MINIMAL_ORDER` = `block` (default, v4 and v4b) or
  `own_first` (v4c on). A test on 3,000 records confirmed that the default
  gives exactly the records of the code before the change, and that under
  `own_first` the net worth probabilities equal those of arm C's code
  (`20_generate.draw_net_worth`) on the same records, to the last digit.
- `03_oversample.py`: under `own_first`, the stratifier's starting
  distribution is taken without a net worth slot, to match how generation
  then draws it.
- `03_stack.py`: passes the order through, records it in the meta file,
  and adds `_ownfirst` to the default plain-file name.
- `07_baseline.py`: `WEALTH_COUNT_BASE` = `irs_returns` (default),
  `scf_units`, or `mixed_v4b` (the old mixture, to reproduce v4 and v4b
  only). **This changes the default**: it is the bug fix. The base is
  written in `summary_<v>.csv`. New outputs: `wealth_lines_<v>.csv` (the
  eight targets as tilted: before, after the 136 alone, after, on the
  redraw) and `consistency_<v>.csv`.
- `15_run_cpu.sbatch`: `MARKER_PREFIX` (default `STEP15_`; v4c used
  `STEP_V4C_` and `STEP_V4C_SCFBASE_`), and a `v4creport` step.
- New: `15_v4c_report.py`, the side-by-side tables below
  (`checks/V4c/report_tables.md`).

Outputs under `/nfs/roberts/scratch/pi_nrs36/jar335/minimal/`:
`gen/gen_puf_stack_v4c.parquet`, `gen/gen_meta_v4c.json`,
`gen/03_stack_tables_v4c.md`, `gen/gen_plain_seed35_v4c.parquet`,
`gen/V4c/gen_puf_with_net_worth_ownerlabel_absent_ownfirst.parquet`,
`tilt/v4c_allunits/` (IRS base), `tilt/v4c_scfbase/` (SCF base),
`tilt/v4b_onebase/` (v4b stack, IRS base), `checks/V4c/` (and
`checks/V4c/scfbase/`). No v4 or v4b output was changed. Slurm jobs: test
29739101; stack 29739269; tilts 29739270 (IRS base), 29739271 (SCF base),
29738934 (v4b stack, IRS base); stack check 29739272; follow-up checks
29739273, 29739274; tables 29739275.

In the tables, the SCF column is the 2019 wave, all units, with wages put
in 2015 dollars for the wage groups. Net worth is in 2015 dollars. "v4c" is
the IRS base unless the column says SCF base.

### 1. Wealth before and after the tilt

Before the tilt the two v4b columns are one file, and so are the two v4c
columns.

| | SCF 2019 | v4b before | v4c before | v4b after | v4b, IRS base, after | v4c after | v4c, SCF base, after |
|---|---:|---:|---:|---:|---:|---:|---:|
| units (millions) | 143 | 158 | 158 | 158 | 158 | 158 | 158 |
| total ($ trillion) | 89.1 | 88.2 | 91.2 | 89.1 | 89.1 | 89.1 | 89.2 |
| mean | $623,000 | $559,000 | $578,000 | $565,000 | $565,000 | $565,000 | $565,000 |
| median | $84,500 | $78,500 | $68,400 | $84,500 | $84,500 | $84,500 | **$62,200** |
| share negative | 10.7% | 12.2% | 12.8% | 11.8% | 11.8% | 11.9% | 13.2% |
| top 10% share | 77.7% | 74.4% | 76.7% | 78.0% | 77.7% | 77.7% | 79.2% |
| top 1% share | 38.4% | 37.0% | 40.0% | 38.4% | 38.4% | 38.4% | 39.9% |
| top 0.1% share | 14.7% | 14.6% | 16.5% | 13.2% | 13.2% | 13.0% | 15.3% |
| 90th percentile | $992,000 | $1,080,000 | $1,050,000 | $902,000 | $991,000 | $991,000 | $888,000 |
| 99th percentile | $9,400,000 | $8,020,000 | $8,700,000 | $9,390,000 | $9,390,000 | $9,390,000 | $9,140,000 |
| 99.9th percentile | $39,100,000 | $36,900,000 | $40,600,000 | $38,900,000 | $38,900,000 | $39,600,000 | $40,400,000 |

Before the tilt, v4c has 2% more wealth than the SCF (v4b: 1% less), more
at the top (top 1% share 40.0% against 37.0% and the SCF's 38.4%) and less
at the bottom (median $68,400 against $78,500 and $84,500). This is what
arm C showed (top 1% share 40.7%, median $68,600).

After the tilt on the IRS base, v4c matches the SCF's shares and
percentiles as v4b did, and the 90th percentile now sits at the SCF's
$991,000; in v4b it was at $902,000, below the $1 million line, because the
band between the lines was empty.

On the SCF base the file's median net worth falls to $62,200 and the
share with negative net worth rises to 13.2%. This follows from the
targets: only 71.6 million of the 157.8 million returns may be above the
SCF's median line, so 86.2 million returns must be below it. The 15
million extra returns land at the bottom, as the SCF base assumes. The top
shares are higher for the same reason (the same dollars spread over fewer
units at the top).

### 2. The move and the effective sample kept

| tilt | v4b | v4b, IRS base | v4c | v4c, SCF base |
|---|---:|---:|---:|---:|
| move, 136 IRS targets alone (nats) | 0.0093 | 0.0093 | 0.0068 | 0.0068 |
| move, 136 plus the eight wealth targets (nats) | 0.0243 | 0.0206 | **0.0182** | 0.0139 |
| of which wealth (nats) | 0.0151 | 0.0113 | 0.0114 | 0.0071 |
| effective sample kept, 136 alone | 98.0% | 98.0% | 98.5% | 98.5% |
| effective sample kept, 136 plus wealth | 95.7% | 95.9% | 96.3% | 97.1% |
| records below 0.1 times their start weight | 2,276 | 8 | 9 | 0 |

Of v4b's 0.0243 nats, the target fix removes 0.0037 (v4b to v4b, IRS base)
and the order a further 0.0024 (to v4c). The order alone lowers the move
for the 136 IRS targets from 0.0093 to 0.0068, mainly because the joint
share before the tilt is closer to the IRS's (35.1% against 36.8%; IRS
34.7%). The 2,276 v4b records cut to almost nothing were the records in
the emptied band; with one base they keep their weight. The SCF base costs
the least of the four.

All four tilts hit the 136 IRS targets (largest gap 0.000008%, in v4b, IRS
base).

### 3. The eight wealth targets and their misses (%)

| target | v4b before | v4b after | v4c before, IRS base | v4c after, IRS base | v4c before, SCF base | v4c after, SCF base |
|---|---:|---:|---:|---:|---:|---:|
| units above the 50th percentile | −2.5 | −0.0 | −6.4 | −0.0 | +3.1 | +0.0 |
| units above $1 million | +19.3 | **+1.65** | +4.9 | −0.0 | +15.6 | −0.0 |
| units above the 90th percentile | +9.7 | **−1.71** | +6.5 | +0.0 | +17.4 | +0.0 |
| units above the 99th percentile | −21.3 | −0.0 | −14.1 | −0.0 | −5.4 | −0.0 |
| net worth above the 50th percentile | −1.1 | +0.0 | +2.4 | +0.0 | +2.4 | −0.0 |
| net worth above the 90th percentile | −3.0 | −0.0 | +2.6 | −0.0 | +2.6 | −0.0 |
| net worth above the 99th percentile | −13.4 | +0.0 | +0.7 | +0.0 | +0.7 | +0.0 |
| total net worth | −1.1 | −0.0 | +2.4 | −0.0 | +2.4 | +0.0 |

"Before" is against the targets of the same column's base; v4b's before
and after are against its own mixed-base targets. Each target has a
standard deviation of 2%. In v4b the contradictory pair settled 1.7% on
either side; on either new base every target is met to within 0.03%.

Before the tilt, v4c is much closer to the SCF at the top than v4b: the
net worth above the 99th percentile line is 0.7% over (v4b: 13.4% short),
though the count above it is still 14% short on the IRS base, so the units
above that line are on average richer than in the SCF.

### 4. The band between $928,000 and $992,000

Net worth between $927,884 and $991,815 (2015 dollars). The SCF puts 0.66%
of its units there.

| | records in the band | share before the tilt | share after the tilt | smallest weight factor in the band | median weight factor in the band |
|---|---:|---:|---:|---:|---:|
| v4b | 2,268 | 0.57% | **0.00%** | 3 × 10⁻¹⁸⁷ | 1 × 10⁻¹⁸⁶ |
| v4b, IRS base | 2,268 | 0.57% | 0.655% | 0.29 | 1.13 |
| v4c | 2,118 | 0.53% | **0.655%** | 0.25 | 1.25 |
| v4c, SCF base | 2,118 | 0.53% | 0.594% | 0.24 | 1.14 |

The band is populated again, at 0.655% of returns on the IRS base against
the SCF's 0.66%. On the SCF base it holds 0.59% of returns, which is the
SCF's 0.94 million units in the band spread over 158 million returns
(0.94 / 158 = 0.59%). The fix comes from the targets, not the order: the
v4b stack tilted to the IRS base fills the band too. Arm C, which used the
old targets, emptied it.

### 5. Wealth by wage group against the SCF

**Mean net worth ($)**

| wages | SCF 2019 | v4b before | v4c before | v4b after | v4c after | v4c after, SCF base |
|---|---:|---:|---:|---:|---:|---:|
| 0 | 706,000 | 1,190,000 | 1,140,000 | 1,190,000 | 1,120,000 | 1,130,000 |
| 1-25k | 203,000 | 206,000 | 228,000 | 200,000 | 219,000 | 214,000 |
| 25-50k | 148,000 | 207,000 | 211,000 | 190,000 | 200,000 | 192,000 |
| 50-100k | 333,000 | 403,000 | 403,000 | 372,000 | 384,000 | 374,000 |
| 100-200k | 922,000 | 887,000 | 940,000 | 844,000 | 909,000 | 915,000 |
| 200k+ | 4,500,000 | 3,710,000 | 3,990,000 | 4,040,000 | 3,890,000 | 4,000,000 |

**Median net worth ($)**

| wages | SCF 2019 | v4b before | v4c before | v4b after | v4c after | v4c after, SCF base |
|---|---:|---:|---:|---:|---:|---:|
| 0 | 109,000 | 262,000 | 257,000 | 232,000 | 239,000 | 236,000 |
| 1-25k | 11,300 | 8,330 | 7,920 | 8,710 | 9,180 | 7,470 |
| 25-50k | 26,000 | 36,100 | 31,500 | 38,300 | 37,000 | 29,400 |
| 50-100k | 109,000 | 144,000 | 120,000 | 135,000 | 124,000 | 107,000 |
| 100-200k | 362,000 | 387,000 | 361,000 | 316,000 | 307,000 | 317,000 |
| 200k+ | 1,680,000 | 1,400,000 | 1,330,000 | 1,430,000 | 1,330,000 | 1,280,000 |

**Share of units with net worth above $1 million**

| wages | SCF 2019 | v4b after | v4c after | v4c after, SCF base |
|---|---:|---:|---:|---:|
| 0 | 10.9% | 21.1% | 21.1% | 19.5% |
| 1-25k | 3.7% | 3.1% | 3.4% | 3.0% |
| 25-50k | 2.1% | 2.6% | 2.7% | 2.3% |
| 50-100k | 4.5% | 5.9% | 6.4% | 5.6% |
| 100-200k | 18.9% | 19.3% | 19.5% | 17.5% |
| 200k+ | 66.7% | 63.3% | 61.5% | 57.9% |

- Returns with no wages are still too rich: $1.12 million against the
  SCF's $706,000, median $239,000 against $109,000. The order does not
  touch this.
- The middle is a little closer to the SCF before the tilt (median for
  wages $50,000 to $100,000: $120,000 against v4b's $144,000 and the SCF's
  $109,000), and about the same after it.
- Earners above $200,000 end slightly further from the SCF: mean $3.89
  million after the tilt against v4b's $4.04 million and the SCF's $4.50
  million; median $1.33 million against $1.43 million and $1.68 million.
  Arm C had $4.22 million, and two training seeds of arm C differed by
  $350,000, so this is within the noise measured there.
- On the SCF base the middle medians come down to the SCF's ($29,400 and
  $107,000 for wages $25,000 to $100,000) and the top earners' share above
  $1 million falls to 57.9%, because the base puts fewer returns above
  every line.

### 6. Holder shares and joint returns (% of returns, before the tilt)

| | v4b | v4c | PUF 2015 | IRS 2019 |
|---|---:|---:|---:|---:|
| interest | 27.38 | 27.80 | 28.33 | 30.85 |
| dividends | 17.59 | 17.89 | 18.37 | 18.37 |
| capital gains | 12.76 | 12.97 | 13.33 | 13.76 |
| joint returns | 36.83 | 35.09 | | 34.73 |

v4c is a little closer to the PUF on all three holder shares and on the
joint share. After the tilt both match the IRS exactly (they are targets),
on both bases.

### 7. The redraw (200,000 equal-weight records)

| | v4b | v4b, IRS base | v4c | v4c, SCF base |
|---|---:|---:|---:|---:|
| IRS targets within 2 standard errors (of 136) | 134 | 126 | 134 | 135 |
| largest \|z\| | 2.87 | 3.09 | 2.48 | 2.04 |
| total net worth, miss | +0.1% | +0.1% | −1.0% | −0.6% |
| net worth above the 99th percentile line, miss | | −0.2% | −2.1% | −2.9% |
| distinct records | 154,760 | 154,785 | 155,091 | 155,254 |

The two v4c cells beyond 2 standard errors are capital gains in the
$25,000 to $30,000 AGI bracket (count +9.9%, z 2.48) and $40,000 to
$50,000 (amount −21.6%, z −2.43). The redraw is one draw, so a few targets
either way is noise: v4b's own stack gave 134 and 126 under the two target
sets.

### 8. Dependents (% of returns)

| relationship | actual 2015 PUF | v4b before | v4c before | v4b after | v4c after | v4c after, SCF base |
|---|---:|---:|---:|---:|---:|---:|
| non-joint returns with 2 or more dependents | 11.6 | 8.7 | **11.6** | 8.7 | **11.2** | 11.6 |
| joint returns with no dependents | 49.7 | | | 54.0 | 50.0 | 49.4 |
| non-joint returns with no dependents | 73.3 | | | 77.5 | 74.0 | 73.3 |
| joint returns with 3 or more dependents | 12.3 | | | 10.3 | 11.9 | 12.1 |

The dependents shortfall is gone: 11.6% before the tilt against the PUF's
11.6% (arm C: 11.4%). Every dependents cell is within 0.7 points of the PUF
after the tilt (v4b: up to 4.3 points).

### 9. The untargeted relationships

Seventeen cells no target covers: the share of returns with any dividends
by wage group and joint status (12 cells), and the share with any capital
gains by dividend group (5 cells), against the 2015 PUF
(`untargeted_<v>.csv`):

| | v4b | v4c | v4c, SCF base |
|---|---:|---:|---:|
| mean gap before the tilt (points) | 1.5 | 0.8 | 0.8 |
| largest gap before the tilt (points) | 5.3 | 3.3 | 3.3 |
| mean gap after the tilt (points) | 1.6 | 0.8 | 0.9 |
| largest gap after the tilt (points) | 6.7 | 2.4 | 2.8 |

Every cell is in `checks/V4c/untargeted_by_arm.csv`. The relationships
the tilt does not target are about twice as close to the PUF as in v4b,
and the tilt does not pull them away.

### 10. The stack against its plain file

The importance weights sum to 100,000 in each file. The gaps between the
v4c stack and its plain file are about the size of the gap between two
plain files drawn with different seeds, for every statistic in
`checks/V4c/stack_check.md`: for example the top 1% share 40.0% in the
stack, 40.2% and 40.6% in the two plain files; non-joint returns with two
or more dependents 11.6%, 11.7% and 11.8%. The v4b plain files differed by
1.2 points in the top 1% share; v4c's differ by 0.4.

### What v4c shows

1. Drawing every PUF variable first fixes the dependents shortfall (11.6%
   against the PUF's 11.6%; v4b 8.7%), brings the joint share and holder
   shares closer to the PUF before the tilt, and halves the gap on the
   untargeted dividend and capital gains relationships.
2. With one base for the counts, the eight wealth targets agree with each
   other, every one is met to within 0.03%, and the band between $928,000
   and $992,000 holds 0.655% of returns against the SCF's 0.66% (v4b:
   empty).
3. The move falls from 0.0243 to 0.0182 nats and the effective sample kept
   rises from 95.7% to 96.3%. About 60% of the fall comes from the target
   fix and 40% from the order.
4. What did not improve: returns with no wages are still too rich ($1.12
   million against $706,000), earners above $200,000 are a little short
   ($3.89 million against $4.50 million), and the top 0.1% holds 13.0%
   after the tilt against the SCF's 14.7%. Before the tilt v4c has too much
   at the top (top 1% share 40.0%) and too little at the bottom (median
   $68,400); the tilt fixes both at the cost shown above.
5. The SCF base costs less (0.0139 nats) but sets the median return's net
   worth at $62,200 against the SCF's $84,500, because it puts the 15
   million extra returns below every line. Which base to use turns on who
   those returns are, which is John's call and connects to the nonfiler
   work.
