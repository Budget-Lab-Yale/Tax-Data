---
title: "Test 4, probe BL — a standard baseline: sequential tree synthesis of PUF records"
role: report
workstream: rebuild
status: current
updated: 2026-10-11
sot: research/rebuild/tests/probes/SPEC.md
supersedes: []
superseded_by: null
---

# A standard baseline: sequential tree synthesis of PUF records

## Summary

- **What was run.** This probe built the standard method for synthetic tax
  and census microdata, the one in the R package synthpop. It used the same
  40 PUF variables as probe AS, with AGI computed as items less adjustments
  plus AS's remainder. The variables are drawn one at a time, and each value
  is copied from a real record in the matching leaf of a decision tree. The
  fitted synthesizers generated 664,624 records per file, the same size as
  AS's files, and AS's tables scored them. Each run took about two
  minutes on one CPU, against about 18 GPU minutes to train each
  transformer.
- **Weighted, it is worse than the transformer at the top.** The plain
  version uses population weights in the trees and the draws. It
  overshoots the top: the top 1% share of AGI is 22.2% (PUF 19.4%), the top
  0.1% share 10.5% (PUF 8.8%), and top 0.1% capital gains $359bn
  (PUF $245bn). It uses the same order as AS's size-first arm, which gives
  20.4%, 8.8% and $313bn.
- **Unweighted, with the weight drawn first, it matches or beats the
  transformer on most rows.** This is the variant probe UW tried for the
  transformer. Total AGI is $8.16tn (PUF $8.09tn), the top 1% share 20.1%
  and the top 0.1% share 9.1%. In the top 0.1%, wages are $212bn
  (PUF $211bn) and partnership and S corporation income $174bn (PUF $175bn).
  Bottom-half capital gains are $13bn (PUF $13bn) and bottom-half
  partnership income −$31bn (PUF −$31bn). AS's transformer with a random
  order gives $180bn, $142bn, −$1bn and −$8bn for these four. The tree
  version overshoots the 99th to 99.9th percentile slightly: $918bn of AGI
  against $873bn, where the transformer gives $885bn.
- **Pure copying scores best of all.** A file of training records drawn
  with replacement in proportion to weight matches the PUF within sampling
  noise on every row. These tables reward copying. On this evidence alone
  they cannot tell a model that has learned the distribution from one that
  repeats the training records.
- **Copying.** No tree file contains an exact copy of a top 0.1% training
  record. Over all records, 17–27% of tree records equal a training record
  on all 40 variables. Among real PUF validation records, which no model
  saw, the rate is 14.5%, so many simple returns repeat by coincidence. The
  transformer's rate is 0.05%, because it draws each dollar amount at random
  within a rank band. Partial copying at the top is higher for the trees
  than for real records. With minimum leaf 5, 0.5–3% of top 1% tree records
  carry five or more of one real taxpayer's exact non-zero amounts. No
  real validation record does. A minimum leaf of 20 brings this to zero
  without making the tables worse.

## What was run

All jobs ran in Slurm on 11 October between 00:15 and 00:41. Code is in
this folder. Outputs are under
`/nfs/roberts/scratch/pi_nrs36/jar335/probes/bl/`, and logs are under
`../logs/bl_*`.

| Step | Script | Jobs |
|---|---|---|
| Tree synthesizer, four settings, two sampling seeds each | `bl_synth.py`, `bl_cpu.sbatch` | 29098157, 29098158, 29098159, 29098182, 29098887 |
| Copy of the training records (reference) | `bl_boot.py` | 29098884 |
| Levels tables against the PUF training records, and copying | `bl_collect.sh` (runs AS's `as_compare.py` and `as_table.py` unchanged, `bl_copies.py`, `bl_nearest.py`), `bl_cpush.sbatch` | 29100009 (final; 29098177 and 29098885 were earlier passes) |
| Levels tables against the PUF validation records | `bl_collect_val.sh`, `bl_compare_val.py` (a copy of `as_compare.py` with the validation records as the truth) | 29100309 |

The 29098157-series jobs replace a first submission (29097869–71) that
stopped at once on missing demographics; see the data section below.

### Method

The method is sequential synthesis with classification and regression
trees, as in synthpop's default `cart` method:

1. Order the 40 variables.
2. Draw the first variable from its weighted distribution: each generated
   record takes the first variable's value from a training record chosen
   with probability proportional to its weight.
3. For each later variable, fit a tree on the PUF training records. Use a
   regression tree for amounts and a classification tree for categoricals,
   with the variables before it as predictors and the population weight as
   the sample weight. Send each generated record down the tree using its
   own generated values of the earlier variables. Then copy the value of a
   training record in the leaf where it lands, chosen with probability
   proportional to weight.
4. AGI is not drawn. It is computed afterwards as items less adjustments
   plus the remainder (`as_agi.py`), as in probe AS.

Each generated record carries an equal weight: total training weight
divided by 664,624.

**Settings.** The trees come from scikit-learn 1.6.1. They have no depth
limit, and they stop splitting where a leaf would have fewer than the
minimum number of training records. That number is 5, synthpop's default
for its trees, in the main runs and 20 in a check. Synthpop also sets a
near-zero complexity penalty, so its trees grow this deep too. With a
minimum of 5, the median leaf holds 6 records. On average, 60% of training
records sit in leaves where every record has the same value of the
variable being drawn (`bl_tables.md`, from the info files). A minimum leaf
of 5 is the convention, not a choice tuned here. The copying results below
favour 20.

**Orders.**

- *Size first.* This is probe AS's size-first order, taken from its info
  file. It runs through all 31 amounts, largest weighted total of absolute
  dollars first: wages, total pensions, Schedule E, partnership, the
  remainder, long-term gains, and so on. The nine categoricals follow.
  This is the main arm, and it is directly comparable with AS's size-first
  transformer arm.
- *Categoricals first.* The nine categoricals, then the amounts in the
  same size order.
- The transformer's best arm draws a fresh random order for each record.
  A tree synthesizer cannot do that without a tree for every order, so it
  was not tried.

**Unweighted variant.** This is the variant probe UW tried for the
transformer. The training records are treated as unweighted. The PUF
weight is drawn first, uniformly over training records, and it is a
predictor in every later tree. Leaf donors are drawn uniformly. Each
generated record keeps its drawn weight, scaled so that the file adds up to
the PUF's 120.7 million returns. Because the PUF oversamples high incomes,
82,000 of the 664,624 records in these files sit in the top 0.1%, against
about 700 in an equal-weight file.

**Data.** The data are the 166,156 PUF training records of AS's `full_v2s`.
The three demographics from the PUF demographic file (age range, gender,
earnings split) are missing on about 42% of these records (70,415–70,615
records). The synthesizer treats "missing" as a value and copies it like
any other value, so generated files carry the same pattern. AS's
transformer instead draws a value for every record. No other variable has
missing values; the script stops if one does. The trees are fitted once,
so the two sampling seeds share them. The ranges for tree arms below cover
sampling only. The ranges for AS's transformer cover four separately
trained models.

**Reference: a copy of the training records.** The reference file draws
664,624 training records with replacement, with probability proportional
to weight (`bl_boot.py`). A generator that had memorized the PUF would
score like this. Its spread over two seeds shows the sampling noise of a
file this size.

**Generated files, for probe V's scorecard.** They are in
`/nfs/roberts/scratch/pi_nrs36/jar335/probes/bl/gen/` and have the same
columns as AS's files: `gen_id`, `source`, `year`, `weight`, `draw`, the 40
variables in dollars, and `tok_<name>`, each value's token under the
`full_v2s` tokenizer. The files are `bl_sizefirst_s{1,2}.parquet`,
`bl_catfirst_s{1,2}`, `bl_unweighted_s{1,2}`, `bl_sizefirst_leaf20_s{1,2}`,
`bl_unweighted_leaf20_s{1,2}` and `bl_bootstrap_s{1,2}` (the copy of the
training records). Each has an `_info.json` with the order, settings,
timings and per-tree leaf counts. In the unweighted files, `weight`
differs from record to record.

## Results

### Levels against the PUF training records

These tables come from job 29100009 (`bl/bl_tables.md`), produced by AS's
own comparison code. The truth is the PUF training records, grouped by
their own AGI at the PUF's percentiles (50th $36,200, 90th $133,700, 99th
$464,600, 99.9th $2.131m). Amounts are weighted totals in $bn unless
marked otherwise. Tree columns give the mean over the two sampling seeds,
with the range in brackets where it is wide. Transformer columns are AS's
summed-AGI models: the mean over four model seeds at sampling seed 1, with
the range over seeds.

| | PUF | Copy of training records | Tree, weighted, size first | Tree, weighted, categoricals first | Tree, unweighted, size first | Tree, unweighted, leaf 20 | Transformer, random order | Transformer, size first |
|---|---|---|---|---|---|---|---|---|
| Total AGI ($tn) | 8.09 | 8.11 | 8.45 | 8.38 | 8.16 | 8.16 | 7.99 [7.89–8.11] | 8.00 [7.89–8.26] |
| Weight above true 99th pct (%) | 1.00 | 1.00 | 1.14 | 1.10 | 1.05 | 1.05 | 1.00 | 1.08 |
| Weight above true 99.9th pct (%) | 0.100 | 0.100 | 0.113 | 0.123 | 0.102 | 0.102 | 0.103 | 0.106 |
| Top 1% share of AGI (%) | 19.4 | 19.5 [19.2–19.9] | 22.2 [21.7–22.7] | 22.1 | 20.1 | 20.1 | 19.4 [18.8–20.2] | 20.4 [19.2–21.7] |
| Top 0.1% share of AGI (%) | 8.8 | 8.9 [8.6–9.3] | 10.5 [9.8–11.1] | 10.8 | 9.1 | 9.0 | 8.6 [7.7–9.5] | 8.8 [7.7–9.4] |
| AGI, bottom 50 | 830 | 837 | 805 | 806 | 823 | 817 | 802 | 787 |
| AGI, 50–90 | 3,415 | 3,414 | 3,390 | 3,406 | 3,400 | 3,397 | 3,354 | 3,332 |
| AGI, 90–99 | 2,245 | 2,241 | 2,333 | 2,275 | 2,268 | 2,270 | 2,243 | 2,203 |
| AGI, 99–99.9 | 873 | 877 | 1,011 | 965 | 918 | 923 | 885 | 954 |
| AGI, top 0.1 | 731 | 736 [704–768] | 906 [844–967] | 923 | 755 | 751 | 703 [624–778] | 722 [626–781] |
| Wages, 99–99.9 | 455 | 454 | 465 | 467 | 462 | 462 | 431 | 458 |
| Wages, top 0.1 | 211 | 217 | 211 | 222 | 212 | 212 | 180 [156–220] | 164 |
| Capital gains, bottom 50 | 13 | 7 [−4–19] | 20 | 8 | 13 | 11 | −1 | −1 |
| Capital gains, 99–99.9 | 104 | 107 | 145 | 121 | 110 | 112 | 110 | 130 |
| Capital gains, top 0.1 | 245 | 218 [200–236] | 359 [311–407] | 362 | 255 | 255 | 249 [196–300] | 313 |
| Partnership and S corp, bottom 50 | −31 | −32 | −31 | −32 | −31 | −31 | −8 [−28–29] | 1 |
| Partnership and S corp, 50–90 | 38 | 37 | 35 | 37 | 37 | 36 | 36 | 47 |
| Partnership and S corp, top 0.1 | 175 | 200 | 165 [133–197] | 190 | 174 | 173 | 142 [125–158] | 128 |
| Dividends, bottom 50 | 14 | 15 | 17 | 14 | 15 | 15 | 13 | 14 |
| Dividends, top 0.1 | 45 | 49 | 87 [60–113] | 73 | 48 | 48 | 47 | 47 |
| Schedule C, bottom 50 | 59 | 59 | 56 | 55 | 57 | 57 | 47 | 67 |
| Schedule C, 99–99.9 | 33 | (not tabulated) | 64 | 46 | 36 | (not tabulated) | 40 | 52 |
| Schedule E total, top 0.1 | 190 | 211 | 180 | 203 | 190 | 189 | 188 | 134 |
| Six components, 99–99.9 | 829 | 833 | 943 | 902 | 857 | 857 | 820 | 902 |
| Six components, top 0.1 (PUF $702bn) | 702 | 710 [680–741] | 875 [815–935] | 896 | 719 | 717 | 642 [541–730] | 683 [599–723] |
| AGI less six components, top 0.1 | 28 | 26 | 30 | 27 | 35 | 35 | 61 | 39 |
| Remainder, 5th pct, top 0.1 ($) | −366,600 | (not tabulated) | −401,890 | −377,130 | −377,550 | (not tabulated) | −165,456 | −231,960 |
| Remainder, 95th pct, top 0.1 ($) | 540,800 | (not tabulated) | 572,180 | 454,765 | 576,815 | (not tabulated) | 249,242 | 420,287 |
| Itemizers, top 0.1 (% of group) | 92.8 | 92.7 | 95.7 | 92.5 | 94.3 | 94.4 | 85.6 | 87.4 |
| Joint filers, top 0.1 (% of group) | 82.5 | 83.0 | 74.4 | 81.5 | 82.3 | 82.5 | 74.4 | 82.4 |
| Both wages and capital gains, top 0.1 (%) | 75.7 | 75.4 | 77.5 | 77.6 | 77.9 | 78.5 | 65.9 | 70.3 |

"Six components" are wages, capital gains (long plus short term,
unlimited), partnership and S corporation income, ordinary dividends,
taxable interest and Schedule C, as in AS. "Not tabulated" marks rows
left out of this summary; the full table in `bl/bl_tables.md` has every
component in every group for every arm. The weighted tree with minimum
leaf 20 is also in that file. It falls between the weighted and the
unweighted arms: top 1% share 20.6% [19.9–21.3], top 0.1% share 9.6%,
top 0.1% capital gains $293bn.

### Levels against the PUF validation records

The table above rewards copying training records, as the copy column
shows. Job 29100309 (`bl/bl_tables_val.md`) therefore repeats it with
the 20,768 PUF validation records as the truth, their weights scaled to
the training total. No model was trained on these records. Selected rows:

| | Validation records | Copy of training records | Tree, weighted, size first | Tree, unweighted | Transformer, random | Transformer, size first |
|---|---|---|---|---|---|---|
| Total AGI ($tn) | 8.03 | 8.11 | 8.45 | 8.16 | 7.99 | 8.00 |
| Top 1% share (%) | 19.4 | 19.5 | 22.2 | 20.1 | 19.4 | 20.4 |
| Top 0.1% share (%) | 8.7 | 8.9 | 10.5 | 9.1 | 8.6 | 8.8 |
| AGI, 99–99.9 | 877 | 877 | 1,011 | 918 | 885 | 954 |
| AGI, top 0.1 | 716 | 736 | 906 | 755 | 703 | 722 |
| Wages, top 0.1 | 220 | 217 | 211 | 212 | 180 | 164 |
| Capital gains, top 0.1 | 238 | 218 | 359 | 255 | 249 | 313 |
| Partnership and S corp, top 0.1 | 167 | 200 | 165 | 174 | 142 | 128 |
| Partnership and S corp, bottom 50 | −27 | −32 | −31 | −31 | −8 | 1 |
| Capital gains, bottom 50 | 12 | 7 | 20 | 13 | −1 | −1 |
| Six components, top 0.1 | 694 | 710 | 875 | 719 | 642 | 683 |

The validation records sit close to the training records on these rows.
The held-out comparison leads to the same reading as the first table.

### Exact copies of training records

This table comes from `bl_copies.py`, in job 29100009. A record counts as a
copy when it equals some PUF training record on all 40 variables, on the
31 amounts alone, or on all 40 tokens. Tokens are rank bands; the
transformer draws a token and then a dollar amount inside its band. Shares
are weighted, in percent, for sampling seed 1. Seed 2 and the other
transformer seeds are within 1 point in `bl/bl_tables.md`. "Top 0.1%" means
records with AGI at or above the PUF's 99.9th percentile.

| File | All records: 40 variables | All records: 31 amounts | All records: 40 tokens | Top 0.1%: 40 variables | Top 0.1%: training records copied |
|---|---|---|---|---|---|
| PUF validation records (real, unseen) | 14.5 | 32.4 | 28.1 | 0.00 | 0 |
| Copy of training records | 100 | 100 | 100 | 100 | 665 |
| Tree, weighted, size first | 22.0 | 37.8 | 32.2 | 0.00 | 0 |
| Tree, weighted, categoricals first | 27.0 | 34.2 | 33.0 | 0.00 | 0 |
| Tree, unweighted | 21.7 | 37.0 | 31.9 | 0.00 | 2 (of 81,763 records) |
| Tree, weighted, leaf 20 | 16.9 | 34.4 | 29.3 | 0.00 | 0 |
| Tree, unweighted, leaf 20 | 16.9 | 33.9 | 29.2 | 0.00 | 0 |
| Transformer, random order | 0.05 | 0.54 | 27.5 | 0.00 | 0 |
| Transformer, size first | 0.07 | 0.65 | 28.9 | 0.00 | 0 |

One in seven real validation records equals a training record on every
variable. These are presumably simple returns with few non-zero items and
rounded amounts; that was not checked. Against that rate, the trees add
2–12 points of whole-record copies, all below the top 1%. The transformer's
dollar-level rate is near zero because of the random draw within a band. At
the token level it equals the real-record rate.

### Partial copying at the top

A tree synthesizer copies every value from some real record, so a generated
top record can carry several of one real taxpayer's exact amounts even when
it is not a whole copy. `bl_nearest.py` (job 29100009) took up to 1,500
generated records per group, drawn in proportion to weight. For each, it
found the training record (among the 78,279 with AGI at or above the 90th
percentile) that shares the most of its non-zero amounts exactly: the same
variable with the same dollar value. Shares are percent of records
examined, sampling seed 1. A generated top record from the trees has
13–16 non-zero amounts.

| File | Top 1%: mean most shared | Top 1%: 3 or more shared | Top 1%: 5 or more shared | Top 0.1%: 3 or more shared | Top 0.1%: 5 or more shared |
|---|---|---|---|---|---|
| PUF validation records (real, unseen) | 1.98 | 11.9 | 0.0 | 5.5 | 0.0 |
| Tree, weighted, size first | 2.43 | 33.2 | 2.5 | 24.8 | 1.4 |
| Tree, weighted, categoricals first | 2.35 | 30.3 | 2.8 | 19.2 | 1.0 |
| Tree, unweighted | 2.21 | 23.6 | 0.5 | 18.3 | 0.7 |
| Tree, weighted, leaf 20 | 2.07 | 15.5 | 0.0 | 6.4 | 0.1 |
| Tree, unweighted, leaf 20 | 1.97 | 10.4 | 0.0 | 5.8 | 0.0 |
| Transformer, random order (model seed 1) | 1.16 | 1.7 | 0.0 | 0.6 | 0.0 |
| Transformer, size first (model seed 1) | 1.10 | 1.7 | 0.0 | 0.8 | 0.0 |

The same count on tokens, also in `bl/bl_tables.md`, does not separate the
methods. Every file, including the real validation records, shares five or
more bands with some training record for 46–90% of top records. The bands
are too broad for that test to detect copying.

## What it means for whether the transformer is competitive

1. **On AS's levels tables, a two-minute standard method matches the
   transformer once it is fitted the way probe UW fitted the transformer.**
   The unweighted tree synthesizer gets the top 0.1%'s wages, partnership
   income, capital gains, Schedule E and joint-filer share right. It also
   gets the bottom half's capital gains and partnership losses right. AS's
   best transformer arm misses on wages and partnership income at the top
   and on capital gains and partnership losses at the bottom. The
   transformer's random-order arm is closer on the 99th to 99.9th
   percentile ($885bn against $918bn; PUF $873bn) and on the top 1%
   share. The tree method's figures vary less, partly because its ranges
   cover sampling only and partly because the unweighted file has 82,000
   records in the top 0.1%.
2. **The weighted tree synthesizer is worse than the transformer at the top.**
   It puts too much income in the top 1%. Most of the excess is in capital
   gains, dividends, interest and Schedule C above the 99th percentile.
   Two changes remove most of it: drawing the weight first as a variable,
   or using larger leaves. The cause was not investigated. A likely one is
   that in the weighted trees the many heavily weighted middle-income
   records decide where the splits go, so leaves at the top mix very
   different high-income records. The weight marks the PUF's sampling
   stratum, which is defined by income, so using it as a predictor would
   separate them.
3. **The tables cannot separate learning from copying.** The copy of the
   training records beats every model on nearly every row, and the tree
   synthesizer is close to a copy, record by record. At the top it carries
   about two exact amounts from one real record on average, and up to
   five or more in a few percent of records. Any plan to release or share
   generated PUF records would need a disclosure test, not only these
   tables. The transformer has the advantage on disclosure: it draws
   dollar amounts that are not real values, and it repeats no more rank
   bands than an unseen real record does.
4. **For the plan.** The transformer's value has to come from what trees
   do poorly, not from AS's levels tables, where it is not ahead. Two
   candidates are completing PUF records with SCF wealth, which a tree
   synthesizer fitted on one file cannot do, and staying close to the
   targets when post-trained. The tree arms are a cheap benchmark that
   probe V's scorecard can now score beside the transformer. A sensible
   next comparison is the transformer trained unweighted (probe UW's arms)
   against the unweighted tree file.

## Caveats

- Ranges are not comparable across methods. Tree ranges cover two
  sampling seeds of one fitted synthesizer; transformer ranges cover four
  trained models. A tree analogue of a model seed would be refitting on a
  resample of the training records, which was not run.
- Only the levels in AS's tables, and copying, were scored. Joint
  relationships beyond those rows, impossible records, tax liability and
  a classifier test are for probe V's scorecard, which can read these files
  directly.
- The weighted trees use scikit-learn's minimum leaf in records, not in
  weight. Larger leaves (20) reduced copying and improved the weighted
  arm. Other values were not tried.
- The demographics are left missing where the PUF has them missing. The
  transformer fills them. The levels tables do not use them, but a
  classifier test would see the difference.

## To finish

Every job of this probe has finished, and every table above is filled from
files that exist now. Nothing is pending. For the next session:

| Job | Writes | Fills |
|---|---|---|
| 29098157, 29098158, 29098159, 29098182, 29098887 | `bl/gen/bl_{sizefirst,catfirst,unweighted,sizefirst_leaf20,unweighted_leaf20}_s{1,2}.parquet` and `_info.json` | The generated files for probe V |
| 29098884 | `bl/gen/bl_bootstrap_s{1,2}.parquet` | The copy column |
| 29100009 (`bl_collect.sh`) | `bl/bl_tables.md`, `bl_compare.json`, `bl_compare_detail.md`, `bl_copies.json`, `bl_nearest.json` | The levels table against the training records and both copying tables |
| 29100309 (`bl_collect_val.sh`) | `bl/bl_tables_val.md`, `bl_compare_val.json` | The levels table against the validation records |

To score probe V's scorecard on the tree files, point it at the
`bl/gen/*.parquet` files. Compute AGI with `as_agi.agi_from_parts`, and
use the `weight` column, which varies in the unweighted files. To rerun
every table after adding a file, run `sbatch bl_cpush.sbatch bl_collect.sh`
and `sbatch bl_cpush.sbatch bl_collect_val.sh`. Each takes a few minutes.
