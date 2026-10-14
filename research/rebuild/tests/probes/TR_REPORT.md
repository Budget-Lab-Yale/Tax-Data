---
title: "Test 4, probe TR — sequential trees on the multi-survey job"
role: report
workstream: rebuild
status: current
updated: 2026-10-14
sot: research/rebuild/tests/probes/SPEC.md
supersedes: []
superseded_by: null
---

# Sequential trees on the multi-survey job

## Summary

- **What was built.** Sequential trees for the whole multi-survey job, on
  probe C's dataset `full_v3cz`: tax variables by probe BL's method on the
  PUF rows, then the 24 wealth variables one at a time from trees fitted on
  the SCF rows, conditioned on the shared tax variables in rank space. It
  generates whole joint records (tax and wealth) and completes wealth on
  PUF records and on SCF test records, in probe C's file formats, for probe
  EV to score. Every job finished within 25 minutes of submission, on CPUs only.
- **Tax side: as good as the PUF-only trees, better than probe C's
  transformer on most rows** (training-record tables, which reward
  copying; probe V's classifier is the fairer check). Total AGI $8.19tn and
  $8.17tn (PUF $8.09tn; C's seed-1 file $7.81tn), top 1% share 20.3% (PUF
  19.4%, C 18.1%), top 0.1% wages $212bn (PUF $211bn, C $147bn),
  bottom-half partnership income −$34bn (PUF −$31bn, C −$52bn). The
  99th–99.9th percentile is overshot ($939bn of AGI against $873bn), as in
  BL. On probe V's scorecard: classifier 0.56 (C 0.81), largest gap in
  average tax rate by group 0.2 points (C 2.7), records breaking a rule real
  records never break 0.30% (C 0.93%).
- **Wealth: how the SCF weights enter the fitting decides the top.** Trees
  fitted with the SCF weights (as specified) put the top 0.1% of PUF
  returns (by the seven-income total) at $27m of mean net worth, against
  the SCF's $55m and the transformer's $35–39m. The SCF oversamples the
  wealthy and gives them small weights, so in a weighted fit they barely
  move the splits and the top leaves mix them with less wealthy
  households. Fitting the trees on the sample as drawn, and still drawing
  each donor in proportion to its weight, raises this to $37m (the
  transformer's level); adding the SCF weight as a predictor gives $40m.
  None reaches the SCF or probe X's translated transformer ($51–57m).
- **The itemizer coding fails for trees exactly as for the transformer.**
  Reading the zero mortgage interest and real estate tax of PUF
  non-itemizers as "none paid" gives 28% home ownership (SCF 67%; C's
  zero-coded transformer 28%). Using trees without the bridge variables
  for non-itemizers gives 61% (C's unknown-coded transformer 56–58%). So
  that failure is in the data coding, not the model.
- **No copying found.** Under 0.002% of generated records carry five or
  more non-zero tax amounts from one PUF record, and under 0.03% carry five
  or more non-zero wealth amounts from one SCF household (minimum leaf 50
  rows; 0.06% with leaf 20).

## Method and settings

All code is `tr_synth.py` in this folder. Data: probe C's dataset
`full_v3cz` (PUF 2015 plus the SCF 2013 and 2016 waves with their shared
variables translated into PUF terms by probe X; every total replaced by its
parts plus a remainder; the bridge variables mortgage interest and real
estate tax on SCF rows; the six itemized deductions zero for PUF
non-itemizers). The same training rows the transformer used: 166,156 PUF
training records and 42,057 SCF training rows with wealth observed (7,739
households, five implicates each; split-off units, whose wealth was not
asked, are left out).

**Tax variables** (generation only). Probe BL's method unchanged on the PUF
training rows of `full_v3cz`: the PUF records treated as unweighted, the
sampling weight drawn first (from a training record chosen uniformly) and
used as a predictor of every later variable; then each of the 40 PUF
variables in probe AS's size-first order (with the three totals replaced by
the pieces that complete them) gets a classification or regression tree on
the variables before it, minimum leaf 20 records; a generated record's value
is copied in dollars from a training record chosen uniformly in the leaf it
lands in. Totals (AGI, total Schedule E, ordinary dividends, total
pensions) are computed afterwards from their parts (`c3_totals.py`, probe
C's script, unchanged). Each generated record carries its drawn weight,
rescaled so the file has the PUF training population total.

**Wealth variables.** For each of the 24 wealth variables in the dataset's
order (most to least commonly held), a regression tree fitted on the SCF
training rows, survey-weighted: the SCF implicate weight is the sample
weight in fitting, and a donor in a leaf is chosen in proportion to it.
Minimum leaf 50 rows (about ten households, since each household appears as
five implicates; a leaf of 20 rows can be four households). Predictors: the
23 shared variables (filing status, dependent status, exemptions,
children, age range, sex, eleven incomes as their parts, and the two bridge
variables) plus the wealth variables already drawn.

**Footing of the shared variables: rank space.** Trees work on the
dataset's rank tokens (each amount's weighted rank band within its own
source and year, with the SCF's shared variables translated into PUF terms
first), which is exactly what the transformer sees; a monotone change of a
predictor does not change a tree, so this differs from working in
translated dollars only inside a band. The wealth tree's target is the
wealth variable's token; the drawn token is decoded to dollars by the
dataset's own decoder (`g_tokenizer.decode`): an SCF record at its own
wave, a PUF record at 2015 by interpolating the 2013 and 2016 waves in log
dollars, as probe C's completions were. One addition: a donor from one wave
can carry a negative band that the other wave does not have (e.g. a
negative home value); such a band is decoded with the wave that has it.

**Missing age and sex on PUF records.** About 42% of PUF records are not in
the demographic file. Before wealth is drawn, age range and then sex are
drawn for them from SCF trees on the other shared variables (weighted, same
leaf rule). They are used only to condition wealth; output files keep them
missing, as probe C's do.

**Itemizers.** With deductions coded zero for non-itemizers, a zero
mortgage interest on a PUF non-itemizer means "not itemized", while on an
SCF row it means "none paid". Two sets of wealth trees are fitted, with and
without the bridge variables among the predictors. Arm *split* (the main
one): PUF itemizers use the first set, non-itemizers the second. Arm
*zero*: every record uses the first set and reads the zeros as the data
code them, which is what the transformer trained on `full_v3cz` saw.
Generated records use the split rule, with the form of deduction they
drew.

**SCF weight as a variable** (tried, completions only). Probe UW's idea
applied to the SCF: wealth trees fitted unweighted on the sample as drawn,
with the log SCF weight as a predictor and donors chosen uniformly in a
leaf; each target record first draws an SCF weight from a tree on its
shared variables, with donors chosen in proportion to weight (so the drawn
weight follows its population distribution given the shared variables).

## What ran

All jobs ran in Slurm on 14 October between 10:25 and 10:50 (CPU, 4 cores,
partitions `day,scavenge`; logs `../logs/tr_cpu_<job>.out`). Outputs under
`$Q = /nfs/roberts/scratch/pi_nrs36/jar335/probes/tr/`.

| Step | Script | Job | Output |
|---|---|---|---|
| Whole joint records, survey-weighted SCF trees, two sampling seeds; totals from parts | `tr_synth.py --mode generate`, `c3_totals.py` (`tr_run_gen.sh`) | 29535124 | `gen/tr_joint_leaf50_s{1,2}.parquet` |
| Completions, survey-weighted: PUF training records (split and zero arms), SCF test records with the bridge variables observed and hidden; five draws each | `tr_synth.py` (`tr_run_comp.sh`) | 29535125 | `comp/tr_puf_wealth_{split,zero}_leaf50.parquet`, `comp/tr_scf_test_split_leaf50.parquet`, `comp/tr_scf_test_nobr_split_leaf50.parquet` |
| SCF fitted on the sample as drawn (`_uf`), and with the SCF weight as a predictor (`_wv`): the same three completions, plus generated records (uf two seeds, wv one) | `tr_run_uf.sh` | 29535801 | `comp/*_uf.parquet`, `comp/*_wv.parquet`, `gen/tr_joint_leaf50_uf_s{1,2}.parquet`, `gen/tr_joint_leaf50_wv_s1.parquet` |
| Sensitivity: minimum wealth leaf 20 rows | `tr_run_leaf20.sh` | 29535722 | `comp/tr_puf_wealth_split_leaf20.parquet`, `comp/tr_scf_test_split_leaf20.parquet`, `gen/tr_joint_leaf20_s1.parquet` |
| Collector: AGI tables (`c3_compare.py`, `c3_table.py`), completed wealth (`tr_wealth.py`, a copy of `c3_wealth.py`), SCF test records against truth (`tr_scftest.py`), probe V's scorecard (`v_score.py`, `v_collect.py`, unchanged), copying by AGI group (`tr_copies.py`) | `tr_collect.sh`, `tr_collect2.sh` | 29535126, 29535802 | `tables/TR_TABLES.md` and the files beside it |

A first weight-as-variable run (job 29535114) drew donors uniformly in a
leaf, which over-weights the oversampled wealthy wherever a leaf mixes
weights (top 0.1% mean net worth $53m, but every middle group 20–40% too
high); its files were overwritten by job 29535801's, which draw in
proportion to weight. Two earlier submissions (29534778–79) stopped on a
decoding error, fixed as described above.

Every generated file shares the same tax side for a given sampling seed
(the tax draws come first with the same random stream), so the arms
differ only in wealth. Trees give no completion log-likelihood, so probe
C's nats-per-record score has no counterpart here.

## Results

Every number below is from `tables/TR_TABLES.md` (job 29535802) or the
files it was built from. The real PUF column is the PUF training records
with their weights; AGI groups use the real PUF's thresholds (50th
percentile $36,200, 90th $133,700, 99th $464,600, 99.9th $2.131m). C is
probe C's zero-coded transformer, model seed 1, sampling seed 1 (one of
its six files; their mean was $7.85tn, top 1% share 19.0%). BL is probe
BL's unweighted, leaf-20 tree file on the PUF alone.

### Whole records: the tax side

| | PUF | Trees, seed 1 | Trees, seed 2 | C transformer | BL (PUF only) |
|---|---:|---:|---:|---:|---:|
| Total AGI ($tn) | 8.09 | 8.19 | 8.17 | 7.81 | 8.15 |
| Top 1% share of AGI (%) | 19.4 | 20.3 | 20.3 | 18.1 | 20.1 |
| Top 0.1% share of AGI (%) | 8.8 | 9.1 | 9.1 | 7.7 | 9.0 |
| AGI, 99th–99.9th ($bn) | 873 | 939 | 938 | 829 | 920 |
| AGI, top 0.1% ($bn) | 731 | 764 | 758 | 616 | 750 |
| Wages, top 0.1% ($bn) | 211 | 212 | 213 | 147 | 212 |
| Partnership and S corp, top 0.1% ($bn) | 175 | 174 | 173 | 195 | 174 |
| Six main items, top 0.1% ($bn) | 702 | 725 | 721 | 585 | 716 |
| Capital gains, bottom half ($bn) | 13 | 12 | 11 | 2 | 10 |
| Partnership and S corp, bottom half ($bn) | −31 | −34 | −34 | −52 | −32 |
| Dividends, 99th–99.9th ($bn) | 39 | 48 | 48 | 34 | 42 |
| Pensions and IRA, 99th–99.9th ($bn) | 22 | 38 | 40 | 25 | 36 |
| Itemizers, all returns (%) | 30 | 30.0 | 29.9 | | |

Probe V's scorecard (`v_score.py`, seed-1 files):

| Check | Real | C transformer | Trees |
|---|---:|---:|---:|
| Sum of absolute dollar misses, 30 amounts × 5 groups ($bn) | 0 | 860 | 383 |
| Filing status by group, largest gap (points) | 0 | 6.1 | 1.0 |
| Co-holding of 28 pairs of items × 5 groups, largest gap (points) | 0 | 12.2 | 3.0 |
| Records breaking a rule real records never break (%) | 0 | 0.93 | 0.30 |
| Net income tax, all returns ($bn; simplified 2015 law) | 1,095 | 1,077 | 1,134 |
| Net income tax, top 0.1% ($bn) | 204 | 189 | 215 |
| Largest gap in average tax rate by AGI group (points) | 0 | 2.71 | 0.20 |
| Classifier telling generated from real (0.5 = cannot) | 0.50 | 0.81 | 0.56 |
| Same, within the top 0.1% | 0.50 | 0.80 | 0.65 |

These are scored against the training records, which favours a method
that copies; the copying check below is the guard.

### Completed wealth on PUF training records

Five draws per record, mean net worth in $ thousands. Groups are the
seven-income total (test 2's grouping, the one the SCF can also form), so
the SCF column is comparable; the SCF is its training records with
wealth observed, interpolated to 2015 (one third 2013, two thirds 2016).
X is probe X's translated transformer (`full_v3x2`), C the combined
transformers (two model seeds each).

| Seven-income group | SCF | Trees, weighted fit | Trees, sample as drawn | Trees, weight as predictor | Trees, zero arm | C zero coding | C unknown coding | X translated |
|---|---:|---:|---:|---:|---:|---:|---:|---:|
| Bottom 50% | 113 | 106 | 108 | 112 | 65 | 91–93 | 124–128 | 96–102 |
| 50–90% | 370 | 423 | 426 | 434 | 336 | 370–371 | 427–434 | 352–365 |
| 90–99% | 2,133 | 1,890 | 1,927 | 1,936 | 1,868 | 1,906–2,002 | 1,939–2,012 | 2,139–2,167 |
| 99–99.9% | 12,317 | 8,782 | 9,181 | 10,094 | 8,610 | 10,047–10,668 | 9,944–10,299 | 13,802–13,910 |
| Top 0.1% | 54,693 | 26,667 | 37,032 | 40,413 | 25,830 | 38,778–38,842 | 34,716–35,663 | 50,691–56,816 |
| All returns, mean | 628 | 498 | 517 | 535 | 439 | 495–509 | 537–542 | 561–569 |
| All returns, median | 91 | 62 | 62 | 65 | 22 | 22–25 | 56 | 32–50 |
| Owns a home (%) | 66.5 | 60.7 | 60.3 | 60.6 | 28.3 | 28.1 | 55.7–58.1 | 44.9–55.3 |
| Top 1% share of net worth (%) | 37.8 | 35.7 | 36.0 | 38.2 | 40.0 | 40.4–41.9 | 37.5–38.4 | 43.9–44.4 |

The SCF median ($91k) leaves out split-off units, whose wealth was not
asked; counting them at zero gives $56–57k (probe C's caveat). The trees'
"split" arm is used in the first three tree columns. Minimum leaf 20
instead of 50 (weighted fit): top 0.1% $30.9m, home ownership 60.0%.

Generated joint records give the same picture as completion (by AGI
group, top 0.1% mean net worth $25.9m weighted fit, $37.0m sample as
drawn, $40.4m weight as predictor; completed PUF records by AGI group
$26.2m, $36.4m, $39.7m).

### Completed wealth on SCF test records

The SCF test records' shared variables known (bridge variables observed),
five draws, against the records' own wealth. Mean net worth in $
thousands, groups by the seven-income total within the wave. The test set
has 6,591 (2013) and 6,824 (2016) rows, each household appearing as five
implicates, so the top 0.1% group is a handful of households and its row
is noisy. C is model seed 1.

| | 2013 truth | C | Trees weighted | Trees as drawn | Trees weight predictor | 2016 truth | C | Trees weighted | Trees as drawn | Trees weight predictor |
|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| Mean | 568 | 583 | 562 | 572 | 591 | 756 | 821 | 764 | 825 | 837 |
| Median | 63 | 79 | 87 | 91 | 92 | 108 | 109 | 117 | 116 | 115 |
| Owns a home (%) | 66.9 | 66.7 | 67.4 | 67.4 | 67.4 | 66.5 | 65.6 | 66.9 | 66.6 | 66.5 |
| Bottom 50% | 112 | 139 | 123 | 123 | 138 | 152 | 204 | 172 | 164 | 174 |
| 50–90% | 379 | 439 | 441 | 419 | 430 | 439 | 643 | 547 | 599 | 582 |
| 90–99% | 1,730 | 1,850 | 1,710 | 2,000 | 1,990 | 2,630 | 2,470 | 2,810 | 2,570 | 2,890 |
| 99–99.9% | 13,100 | 10,600 | 11,600 | 9,830 | 10,400 | 16,300 | 15,000 | 13,400 | 17,300 | 16,100 |
| Top 0.1% | 39,400 | 30,300 | 19,600 | 26,900 | 29,900 | 51,500 | 35,100 | 19,100 | 41,000 | 36,300 |

With the bridge variables hidden (drawn after wealth), the trees fitted
on the sample as drawn give top 0.1% $29.9m (2013) and $36.4m (2016), home
ownership 65.3% and 67.4%; C gives $24.7m and $32.5m, 62.2% and 62.9%.
Probe EV scores these files properly; this table is only a first look.

### What it means

- On the tax side the multi-survey dataset changes nothing for the trees:
  the PUF rows are what they were in BL's test, and the result matches
  BL's. The transformer's case has to come from the wealth side or from
  stretching to other years.
- On the wealth side the trees, fitted on the SCF sample as drawn, are
  about level with the combined transformer on PUF completion and on SCF
  test records (first look; EV's scores decide). Both fall short of the
  SCF in the top 1% of income. Since two different methods on the same
  dataset fall short together while probe X's transformer on its own
  dataset gets there, this suggests the cause of probe C's low top lies in
  what `full_v3cz` adds (the bridge variables, the totals as parts, the
  Schedule E donor split), not in the model. Not tested here. Probe WT is on that
  question.
- The weighting lesson of probe UW carries to the SCF: fit on the sample
  as drawn so the oversampled top shapes the splits; draw in proportion
  to the population weight. Drawing uniformly, as UW's PUF method does,
  over-weights the oversampled records wherever a leaf mixes weights.

## Copying

A tree copies every value from some real record, and `tr_synth.py`
records which one. The check counts, for each record, the largest number
of its non-zero amounts that came from one real record: one PUF training
record for the 31 tax amounts, one SCF household (any of its five
implicates) for the 24 wealth amounts. Weighted shares, from
`tr_copies.py` and the info files (job 29535802). Probe BL's check, which
looks for equal dollar amounts in any training record rather than the
actual donor, found 0.0% for real unseen records and for leaf-20 trees.

| File | Group | Tax: 3 or more (%) | Tax: 5 or more (%) | Wealth: 3 or more (%) | Wealth: 5 or more (%) |
|---|---|---:|---:|---:|---:|
| Generated, weighted fit, seed 1 | all | 0.27 | 0.000 | 0.57 | 0.002 |
| | top 0.1% | 0.87 | 0.000 | 2.50 | 0.013 |
| Generated, sample as drawn, seed 1 | all | 0.27 | 0.000 | 0.55 | 0.002 |
| | top 0.1% | 0.87 | 0.000 | 2.14 | 0.021 |
| Generated, wealth leaf 20, seed 1 | all | 0.27 | 0.000 | 2.38 | 0.061 |
| | top 0.1% | 0.87 | 0.000 | 4.04 | 0.076 |
| Completed PUF records, weighted fit | top 0.1% | | | 2.59 | 0.034 |
| Completed PUF records, sample as drawn | top 0.1% | | | 2.17 | 0.018 |

No record carries eight or more wealth amounts from one household in the
leaf-50 files. The decoded wealth dollars are not the donor's own dollars
(each drawn rank band is decoded to a fresh amount, as the transformer's
draws are), so on the wealth side the check is on rank bands from one
household; exact dollar copies of SCF amounts do not occur. The tax side
copies dollars, as BL's trees do.

## To finish

Nothing is pending: every job has finished and every table above is
filled. For probe EV, the files to score (also in `STATUS.md`):

- Trees' best arm (SCF fitted on the sample as drawn, non-itemizers
  without the bridge variables): `$Q/comp/tr_puf_wealth_split_leaf50_uf.parquet`,
  `$Q/comp/tr_scf_test_split_leaf50_uf.parquet`,
  `$Q/comp/tr_scf_test_nobr_split_leaf50_uf.parquet`,
  `$Q/gen/tr_joint_leaf50_uf_s{1,2}.parquet`.
- The arm as specified (survey-weighted fit): the same names without
  `_uf`; the weight-as-predictor arm with `_wv`; the zero arm
  `$Q/comp/tr_puf_wealth_zero_leaf50.parquet`.

Not done: probe V's scorecard on the validation records alongside (it
reports training-record gaps only here); the transformer trained on the
sample as drawn for the SCF side, which would say whether its top gap
closes the same way; and the trees on the dataset with deductions split
into claimed and paid (probe DD's), which would replace the split arm.
