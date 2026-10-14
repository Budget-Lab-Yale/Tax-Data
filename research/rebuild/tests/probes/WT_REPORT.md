---
title: "Test 4, probe WT — why completed PUF wealth is too low at the top"
role: report
workstream: rebuild
status: current
updated: 2026-10-14
sot: research/rebuild/tests/probes/SPEC.md
supersedes: []
superseded_by: null
---

# Why completed PUF wealth is too low at the top

## Summary

- **Where the shortfall enters.** It enters in training on the SCF. Probe
  C's model falls short of the SCF's top even on held-out SCF records,
  where it knows each household's own translated incomes. In the top 0.1%
  by income, it completes a mean net worth of $31m against a true $48m.
  That is about the same proportion as the PUF shortfall ($38m against
  $55m). Two other explanations were checked and ruled out. The PUF's top
  returns do not lie outside what the SCF's top covers: after translation
  the two have nearly the same income thresholds and income mix. And the
  rank-band coding of wealth loses less than 1% at the top.
- **Why the training falls short.** The SCF's top 0.1% by income is 1,948
  training rows, 4.1% of the SCF rows, but carries 0.1% of the SCF weight,
  because the SCF's list sample deliberately oversamples wealthy
  households. Population-weighted training gave those rows about one
  fortieth of the influence their numbers would give them. This is the same
  mechanism that starved the PUF top.
- **Training the SCF rows on the sample as drawn fixes the top 0.1% median
  but overshoots everywhere else.** The setup: SCF rows counted equally in
  training; the SCF weight a model variable; several weight-and-wealth
  draws per record, resampled in proportion to the drawn weight. Results
  (three seeds): completed PUF top 0.1% mean $70m and median $32m, against
  the SCF's $55m and $33m. But group means are too high at every income
  level; the bottom half's mean is $253k against $113k. The cause is the
  one probe UW found for the PUF. Records drawn with a large weight rarely
  get a very large wealth draw, but each such draw stands for thousands of
  households: 0.24% of bottom-half returns get more than $10m of net worth,
  against 0.02% in the SCF.
- **Limiting each wealth draw to values seen among SCF rows of similar
  weight brings every group mean close to the SCF, but leaves top medians
  low.** This is UW's weight-group limit, applied to the SCF weight. On PUF
  records the mean is $54m in the top 0.1% (SCF $55m), $129k in the bottom
  half ($113k), $368k at the 50th–90th percentiles ($370k) and $10.9m at
  the 99th–99.9th ($12.3m). On held-out SCF records the group means are also
  close to the truth. The top medians fall, however: $22m in the top 0.1%
  against the SCF's $33m. Why the limit lowers the medians is not known.
- **A middle course helps the top, but not the lower groups.** In this
  version SCF rows are weighted by the square root of their weight in
  training, and draws are resampled by the square root of the drawn weight
  (two seeds). Completed PUF top 0.1% mean $59m and median $29m (SCF $55m
  and $33m). But the bottom half's mean is still $202k against $113k. With
  the weight-group limit added, the top falls too low (mean $41m).
- **Bottom line.** The cause is found: it is the population weighting of the
  SCF in training. Training on the sample as drawn gets the top right. No
  version tried yet gets the top and the lower groups right at the same
  time.

## What was run

All jobs ran in Slurm on 14 October. Job numbers are in
`/nfs/roberts/scratch/pi_nrs36/jar335/probes/wt/jobs.txt`, logs in
`../logs/wt_*`, and the tables in `$Q/WT_TABLES.md` and `$Q/wt_wealth.json`
(`$Q = /nfs/roberts/scratch/pi_nrs36/jar335/probes/wt`). The locating step
(no training) is in `$Q/locate/locate.md`.

| Step | Scripts (this folder) | Jobs |
|---|---|---|
| Locating the shortfall from probe C's files, plus completions of PUF records by C's model with the bridge variables or the PUF-only variables hidden | `wt_loc_main.py`, `wt_loc_shared.py`, `wt_loc_comp.sbatch`, `wt_loc_cpu.sbatch`, `wt_loc_inspect.py` | 29534342, 29534794–96, 29537684 |
| Data `full_v3cwt`: probe C's `full_v3c` plus the SCF weight as a model variable | `wt_prepare.py`, `wt_cpu.sbatch` | 29534366 |
| Training, SCF rows counted equally, seeds 1–3 | `wt_train.py` (copy of `uw_train.py` with `--loss-weight wt`), `wt_train.sbatch` | 29534367, 29534368, 29534801 |
| Completions: PUF training records, SCF test records with the weight drawn, SCF test records with their own weight given | `wt_chain.sbatch` (`../posttrain/h_complete.py` unchanged) | 29534728, 29534729, 29534802 |
| The same completions with the weight-group limit | `wt_complete.py` (copy of `h_complete.py` with `--weight-groups`), `wt_rchain.sbatch` | 29537936–38 |
| Tables | `wt_wealth.py` | 29539012 (final; earlier passes superseded) |
| Square-root arm: training seeds 1–2, completions without and with the limit | `wt_train.py --loss-weight wtsqrt`, `wt_sqchain.sbatch` (uses `wt_complete.py`) | 29538986–89 |

**The data.** `full_v3cwt` is `full_v3c` (probe C's dataset with
non-itemizers' deductions coded unknown, the one C's wealth results come
from) plus one variable, the SCF weight. It is observed on SCF rows and
unobserved on PUF rows. It is coded as 200 equal-count bands of the weight,
each decoded to the band's mean weight. Decoding the SCF training records'
weights by band changes mean net worth by income group by at most 2.6% (the
top 0.1% in 2016); 50 bands changed it by up to 17%. The weight is drawn
first in every completion.

**The training.** The training script is probe UW's, with one new option
(`--loss-weight wt`). PUF rows keep their population weight, as in probe C,
so the PUF side is unchanged. SCF rows each count the same. One constant
keeps the SCF's share of the total training weight at version 2's 61.6%.
Everything else follows UW's arm A (test 3's recipe, early stopping). Each
run stopped after 31 epochs, with the best checkpoint at epoch 16.

**Resampling.** Each record gets 10 draws of (SCF weight, 24 wealth
variables). The model is trained on the SCF sample as drawn, so it learns
how weight and wealth occur together in the sample. In the sample, a kind
of household turns up in proportion to its sampling rate, which is one over
its weight. Weighting each draw by its drawn weight undoes that sampling
rate, so the weighted draws follow the population's distribution of wealth
given the record's other variables, the same way survey weights correct a
survey's own statistics. In the tables each draw counts for the record's
weight times its drawn weight over the sum of the drawn weights of that
record's draws; this is resampling in proportion to the drawn weight,
without the extra noise of an actual resample. The "plain" rows weight the
draws equally, to show the size of the correction.

**The square-root arm** (`--loss-weight wtsqrt`). Each SCF row's training
weight is the square root of its survey weight, with one constant keeping
the SCF's share at 61.6%. The model then learns the population distribution
divided by the square root of the weight, so draws are resampled by the
square root of the drawn weight. Runs stopped at epochs 30–31.

**The weight-group limit** (`wt_complete.py --weight-groups 1000`). Adjacent
weight bands are merged until each group has at least 1,000 SCF training
rows, which gives 40 groups. Once a record's weight is drawn, each wealth
variable may only take values (bands) seen for it among the SCF training
rows in that weight group. The model itself is unchanged.

## Where the shortfall enters

From the locating step: probe C's three unknown-coded models, probe X's two
models, and the SCF records. Groups are by the sum of ten translated
incomes that both files share, ranked within each SCF wave and within the
PUF. Means are interpolated to 2015 as in probe C.

**The top groups are comparable across the two files.** The thresholds
nearly coincide: the 99th percentile is $467k in the PUF and $491–498k in
the SCF, and the 99.9th is $2.1m and $2.1–2.3m. In the top 0.1%, mean
amounts per return:

| Income item, top 0.1% | PUF | SCF |
|---|---:|---:|
| Wages | $1.7m | $1.3m |
| Partnership and S corporation | $1.5m | $1.7m |
| Capital gains | $2.1m | $2.1m |
| Dividends | $366k | $371k |
| Interest | $151k | $157k |
| Schedule C | $85k | $83k |

No PUF top 0.1% return has an income in a rank band above the highest band
of any SCF top record, except interest paid. On that item 11% of PUF top
returns are above every SCF top record; the likely cause is that the PUF
item includes investment interest.

**The model falls short on the SCF's own records.** SCF test records,
completed with their shared variables known, against their true wealth
(test-record totals, both waves):

| Model | Completed over true net worth, 99th–99.9th | Completed over true, top 0.1% | Mean, top 0.1% (true $50.0m) |
|---|---:|---:|---:|
| C, unknown coding (three seeds) | 0.93 | 0.75 [0.68–0.83] | $37.4m |
| C, bridge variables hidden | 0.97 | 0.72 | $35.8m |
| C, zero coding | 0.96 | 0.77 | $38.3m |
| C, SCF Schedule E unobserved | 0.86 | 0.61 | $29.8m |
| X, translated (two seeds) | 1.03 | 0.88 | $43.5m |

On PUF records, C reaches 71% of the SCF figure in the top 0.1% ($37.8m
against $53.0m on these groups), close to the 75% it reaches on the SCF's
own test records. So the PUF shortfall is mostly the SCF shortfall carried
over.

**The bridge variables do not explain it.** Completing PUF records with C's
model with mortgage interest and real estate tax hidden gives a top 0.1%
mean of $33.3m. Showing only the variables the SCF also observes gives
$34.6m, and both together give $32.5m. All three are lower than C's own
completion, not higher.

**Rank-band decoding does not explain it.** Each wealth variable has 12
bands at or above its holders' 99th percentile and 3 at or above the
99.9th. Decoding the SCF training records' own bands back to dollars gives
a top 0.1% mean net worth of $50.7m, against a true $50.9m.

**Why X did better than C (0.88 against 0.75) is not settled.** The seed
ranges are wide, and hiding the SCF's Schedule E made C worse (0.61), not
better.

**The SCF top's share of training.** From the data job (29534366), SCF
training rows grouped by the seven-income total, as probe X and C grouped
the SCF:

| Income group | SCF training rows | Share of rows | Share of SCF weight | Median weight |
|---|---:|---:|---:|---:|
| Bottom half | 20,021 | 42.2% | 50.0% | 4,685 |
| 50th–90th | 16,771 | 35.4% | 40.0% | 4,575 |
| 90th–99th | 6,115 | 12.9% | 9.0% | 2,687 |
| 99th–99.9th | 2,589 | 5.5% | 0.9% | 308 |
| Top 0.1% | 1,948 | 4.1% | 0.1% | 30 |

## Results of the fix

The tables use probe C's grouping: PUF records by true AGI, and the SCF by
the seven-income total within each wave, interpolated to 2015. Each cell is
the mean over seeds, with the range in brackets. "Limited" means the
weight-group limit was applied. The "SCF training records, pooled" column is
the 2013 and 2016 training records together, without interpolation. It is
there for the rows C's benchmark does not have. Source: collector job
29539012, `$Q/WT_TABLES.md` (written 11:39).

### Completed PUF wealth (PUF training records)

| | SCF 2015 | SCF training records, pooled | C, unknown coding (3 seeds) | Sample as drawn, resampled (3) | Sample as drawn, limited, resampled (3) | Sample as drawn, not resampled (3) | Square root, resampled (2) | Square root, limited, resampled (2) |
|---|---|---|---|---|---|---|---|---|
| All returns, mean | $628k | $608k | $559k | $808k [$739k–$908k] | $533k [$501k–$564k] | $1.1m | $712k | $480k |
| All returns, median | $91k | $90k | $56k | $62k | $57k | $62k | $56k | $53k |
| Bottom half, mean | $113k | $112k | $153k | $253k [$224k–$281k] | $129k [$122k–$135k] | $428k | $202k | $122k |
| 50th–90th, mean | $370k | $363k | $423k | $568k [$493k–$675k] | $368k [$340k–$405k] | $767k | $516k | $352k |
| 90th–99th, mean | $2.1m | $2.1m | $2.0m | $2.7m [$2.4m–$3.2m] | $1.9m [$1.7m–$2.1m] | $3.6m | $2.5m | $1.7m |
| 90th–99th, median | $1.1m | $1.0m | $924k | $974k | $834k | $1.1m | $1.0m | $855k |
| 99th–99.9th, mean | $12.3m | $11.9m | $10.2m | $15.5m [$14.9m–$16.0m] | $10.9m [$10.6m–$11.1m] | $21.4m | $13.6m | $9.1m |
| 99th–99.9th, median | $8.0m | $7.4m | $5.6m | $6.4m [$6.2m–$6.8m] | $4.7m [$4.4m–$5.0m] | $8.3m | $6.4m | $4.6m |
| Top 0.1%, mean | $54.7m | $52.5m | $37.8m [$34.0m–$44.5m] | $70.5m [$66.6m–$74.2m] | $53.5m [$50.0m–$58.2m] | $86.3m | $59.1m [$55.1m–$63.2m] | $40.7m [$37.3m–$44.1m] |
| Top 0.1%, median | $32.7m | $27.8m | $20.7m | $32.0m [$31.1m–$33.4m] | $22.5m [$20.7m–$25.1m] | $42.4m | $29.2m [$28.3m–$30.1m] | $18.5m |
| Bottom half, share above $10m | | 0.02% | | 0.24% | 0.08% | 0.46% | 0.20% | 0.08% |
| 50th–90th, share above $10m | | 0.09% | | 0.51% | 0.17% | 0.87% | 0.41% | 0.14% |
| 99th–99.9th, share above $10m | | 36.7% | | 36.1% | 27.3% | 44.7% | 34.7% | 24.7% |
| Top 0.1%, share above $10m | | 84.3% | | 82.1% | 70.6% | 87.8% | 82.0% | 68.3% |

The median drawn SCF weight for PUF records follows the SCF's own pattern.
It is 34–37 in the top 0.1% by AGI (SCF top 0.1% rows: 30), 406–523 at the
99th–99.9th (SCF: 308) and about 4,700 in the bottom half (SCF: 4,685). So
the model places PUF top returns in the list sample, as it should.

### SCF test records: completed against true

SCF test records with wealth observed, grouped by the seven-income total
within each wave among the test records; 397 rows (about 80 households) in
the top 0.1%, 907 at the 99th–99.9th.

| | Truth | C, unknown coding (3) | Sample as drawn, weight drawn, resampled (3) | Limited, weight drawn, resampled (3) | Sample as drawn, own weight given (3) | Square root, weight drawn, resampled (2) |
|---|---|---|---|---|---|---|
| All, mean | $661k | $688k | $935k | $634k | $923k | $849k |
| All, median | $85k | $93k | $97k | $91k | $97k | $93k |
| Bottom half, mean | $131k | $161k | $243k | $131k [$127k–$138k] | $252k | $199k |
| 50th–90th, mean | $463k | $605k | $735k | $488k | $703k | $690k |
| 90th–99th, mean | $2.4m | $2.4m | $3.2m | $2.2m | $3.0m | $2.9m |
| 90th–99th, median | $1.1m | $984k | $998k | $855k | $962k | $1.0m |
| 99th–99.9th, mean | $16.1m | $13.1m | $20.2m | $14.6m [$13.9m–$15.1m] | $21.1m | $17.9m |
| 99th–99.9th, median | $9.0m | $7.2m | $8.0m | $6.1m | $8.7m | $8.4m |
| Top 0.1%, mean | $47.5m | $31.4m [$27.8m–$36.1m] | $53.0m [$48.2m–$60.4m] | $45.8m [$43.4m–$48.0m] | $55.1m | $51.5m [$47.0m–$55.9m] |
| Top 0.1%, median | $14.4m | $15.5m | $19.0m | $17.9m | $18.8m | $21.6m |
| Bottom half, share above $10m | 0.06% | 0.10% | 0.21% | 0.05% | 0.20% | 0.15% |
| Top 0.1%, share above $10m | 83.3% | 63.6% | 66.5% | 61.3% | 66.8% | 69.0% |

Giving the model each test record's own weight, instead of drawing it,
leaves the bottom-half overshoot ($252k against $131k). So the overshoot
comes from the wealth draws at a given weight, not from drawing the wrong
weight.

## What it means

- The low top in probe C is a training-weight problem on the SCF side, the
  same one that probe UW found on the PUF side. The SCF's top income
  households are 4% of its rows but 0.1% of its weight. A model trained
  on population weights learns their wealth poorly, and that carries over
  to every PUF return completed from them.
- Training on the sample as drawn, with the weight as a variable and draws
  resampled by the drawn weight, gives the top what it was missing. It also
  brings back UW's problem: a rare error on a record drawn with a large
  weight is multiplied by that weight. The weight-group limit removes most
  of the excess, as it did for UW's generation. With the limit, every group
  mean, on PUF records and on held-out SCF records, is within about 15% of
  the SCF. That is closer than probe C at the top and in the bottom half, and about as close in between.
- Neither version is right yet. Without the limit the means are too high.
  With it, the medians in the top 1% are too low: on PUF records $4.7m
  against $8.0m at the 99th–99.9th, and $22m against $33m in the top 0.1%.
  On held-out SCF records too few of the top 0.1% have more than $10m (61%
  against 83%), but the same is true of probe C's model and of every
  version here. The square-root arm sits between the two: the top is close
  (PUF top 0.1% mean $59m, median $29m) but the bottom half is still too
  high ($202k against $113k), and adding the limit to it pulls the top too
  low. A finer limit (smaller weight groups, or a limit on net worth as a whole
  rather than on each of the 24 variables separately) would be another.
- For the plan: the rule from the history file ("train on the sample as
  drawn, with the weight as a variable") applies to the SCF as well as the
  PUF. It needs the weight-group limit or something like it at completion.

## To finish

Nothing is pending. Every job has finished, including the square-root arm
(training 29538986–87, completions 29538988–89, collector 29539012 at
11:39). The tables above are from that collector's `$Q/WT_TABLES.md`, which
also has every seed separately and the share of each group's net worth
coming from draws with a drawn weight above 1,000.

Not done, for a next run: a limit applied to total net worth instead of to
each of the 24 wealth variables separately, or smaller weight groups;
finding why the limit lowers top medians much more on PUF records than on
SCF records; and wealth completion scores in nats (the training runs report
one, but it is conditional on the true SCF weight, so it is not comparable
to probe C's).
