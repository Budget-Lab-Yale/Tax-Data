---
title: "Test 2, piece A: data preparation report"
role: results
workstream: rebuild
status: current
updated: 2026-10-09
sot: research/rebuild/tests/scf_joint/SPEC.md
supersedes: []
superseded_by: null
---

# Test 2, piece A: data preparation report

This note describes how the SCF tax units for 2013 and 2016 and the 2015 PUF
were turned into the five token datasets of test 2, what was checked, and
what John needs to decide. The contract is `SPEC.md`, section A, with the
changes listed at its end; the income definitions are argued in
`a_crosswalk.md`. Every number comes from the Slurm runs of 9 October
(logs in `$S/logs/`, `$S` = `/nfs/roberts/scratch/pi_nrs36/jar335/scf_joint`).
Tables hold weighted aggregates only.

Two terms. A **token** is the integer code a variable takes in the model's
input. A **weighted rank** of an amount is the share of the population (using
survey or PUF weights) with a smaller amount of the same sign, within the
record's own source and year.

## Summary

- The production tax-unit builder runs on the 2013 and 2016 waves without
  changes to its logic. Units add up exactly to their households: net worth
  over all units equals the SCF summary file's to within 28 cents per household.
- Filer tax units: 140.2 million in SCF 2016 (tax year 2015) against 141.6
  million PUF 2015 returns without dependent filers; 132.3 million in SCF
  2013. Filers hold 98.5% of SCF net worth.
- **The SCF summary files on the cluster are in 2022 dollars** for every wave
  (2016 wealth is 1.2334 times its 2016-dollar value, 2016 income 1.2493 times
  its 2015-dollar value, the same factor for every record). Incomes here come
  from the raw interview items (tax-year dollars), wealth from the summary
  file divided by the factor (survey-year dollars).
- Split-off units (adults in the household who are financially independent)
  carry no filing answer of their own; a rule is used (below), affecting 18.8
  million of the 140.2 million 2016 filer units. These units also have zero
  wealth in every component by construction, because the SCF does not ask
  about their assets. **John decided (9 October) to keep them with their
  wealth unobserved**: their incomes, demographics and weight are used, their
  wealth is hidden (token -1) and never graded.
- Two errors in the production builder's diagnostic income columns (not used
  by `wealth.R`): `rent_scf` is rent *paid*, and `business_scf` includes
  Schedule E income. Details in `a_crosswalk.md`.
- Rank tokens lose little: decoded totals are within 1% for most variables
  and source-years, off by up to 2 to 3% systematically for the worst
  (life insurance, other debt, `ui_other`), with draw-to-draw noise up to 5%
  for the thinnest (installment debt and credit lines in some source-years).

## Files

| File | Contents |
|---|---|
| `a_scf_tax_units.R` | Copy of `build_scf_tax_units()` (git ba48f97) with three marked additions: filing answers and split shares carried per unit; the seven incomes from raw interview items; messages without "2022". Tax-unit structure, weights, ages, children and wealth unchanged. |
| `a_build_units.R`, `a_build_units.sbatch` | Builds the units for given waves, applies the filing rule, writes `$S/data/scf_units/scf_units_<wave>.csv.gz` and `checks_<wave>.csv`. |
| `a_scfp_dollars.R`, `.sbatch` | Measures the dollar factor of each SCF summary file (`$S/data/scf_units/scfp_dollar_factors.csv`). |
| `a_prepare.py`, `a_prepare.sbatch` | Builds the datasets and the crosswalk tables. |
| `a_tokenizer.py` | `load_meta`, `load_quantiles`, `encode`, `decode`. |
| `a_verify.py`, `a_summarize.py` (+ `.sbatch`) | Consistency checks and the tables of this report. |
| `a_crosswalk.md` | The PUF-to-SCF income crosswalk, for review. |
| `$S/data/<dataset>/` | `meta.json`, `tokens_*.npy`, `truth_*.npy`, `records_*.parquet`, `quantiles.parquet`, `roundtrip.csv`, `prep_summary.json`, `DONE`. |

To rebuild everything from scratch takes about 10 minutes of Slurm time; the
commands are in `STATUS.md` (13:07 entry).

## 1. SCF tax units, 2013, 2016 and 2019

The builder needed no changes to run on the older waves. Six of its nine
optional retirement-account allocation flags do not exist in the 2016 file (they feed
only a portfolio-mix flag that this test does not use).

| | 2013 | 2016 | 2019 |
|---|---|---|---|
| Households in the file | 6,015 | 6,248 | 5,777 |
| Tax-unit rows (all five implicates) | 37,323 | 39,562 | 36,295 |
| Units per household | 1.24 | 1.27 | 1.26 |
| Households with more than one unit (weighted) | 21.3% | 23.1% | 23.9% |
|   of which a couple split into two units | 10.7% | 10.8% | 11.3% |
|   of which with split-off adults | 11.6% | 13.4% | 13.5% |
| Households, millions | 122.5 | 126.0 | 128.6 |
| Tax units, millions | 155.3 | 163.1 | 166.7 |
| Filer units, millions | 132.3 | 140.2 | 142.0 |
| Net worth, all units (summary-file dollars, trillions) | 82.41 | 107.14 | 111.37 |
| Net worth, summary file (same dollars) | 82.41 | 107.14 | 111.37 |
| Largest household difference, units vs summary file | $0.28 | $0.23 | $0.08 |
| Filers' share of net worth | 98.5% | 98.5% | 98.8% |

In each wave's own dollars, filers' net worth is $63.8 trillion (2013) and
$85.6 trillion (2016).

Income: the units of each household add up exactly to the household's raw
interview amounts (ratio 1.0000 for each of the five categories checked:
wages, business, capital gains, Schedule E, Social Security and pensions), plus the separately
reported income of split-off units (2016: $348 billion of wages, $41 billion
of Social Security and pensions). Totals by category, against the PUF, are in
`a_crosswalk.md`.

**Dollars of the summary files.** For every record, summary-file wages divided
by the raw interview wages are 1.2921 (2013), 1.2493 (2016), 1.1802 (2019) and
1.0809 (2022); summary-file home value divided by the raw home value is
1.2728, 1.2334, 1.1592 and 1.0000. So every summary file is in 2022 dollars,
with income inflated from the tax year and wealth from the survey year. The
survey weight in the summary file is the raw weight divided by 5 for every
record, as the builder assumes.

## 2. Filers

The household's filing answers are X5744 (did you, or you and your spouse or
partner, file: 1 yes, 5 no, 6 not yet but will) and X5746 (1 jointly, 2
separately, 3 only the respondent, 4 only the spouse or partner). The rule:

| Unit | Files if | 2016 filer share (weighted) |
|---|---|---|
| Household kept as one unit | X5744 is 1 or 6 | 87.1% |
| Respondent's unit of a split couple | X5744 is 1 or 6 and X5746 is not 4 | 86.8% |
| Spouse's or partner's unit | X5744 is 1 or 6 and X5746 is not 3 | 85.8% |
| Split-off adult (no filing question in the SCF) | gross income other than Social Security and welfare at or above the 2015 single filing threshold ($10,300; $11,850 at 65 or over), or self-employment income of $400 or more, **or any wages** | 80.4% |

So split couples do carry their own filing answer through X5746; split-off
adults do not. For them, 11.1 million (2016, weighted) are required to file by
the income test, and another 7.7 million have wages but are below the
threshold; the rule counts both as filers (18.8 million). The reasoning: most
low-wage workers have tax withheld and file to get it back or to claim the
earned income credit. Counting only those required to file would give 132.4
million SCF 2016 filers against 141.6 million PUF returns; the rule used gives
140.2 million. Thresholds for 2012 ($9,750 / $11,200) and 2018 apply to the
other waves.

Dropped as non-filers (2016): 4,748 unit rows of 39,562 (22.9 million
weighted units): 2,863 households kept as one unit, 823 units of split
couples, 1,062 split-off adults. 2013: 4,569 rows (23.0 million).

Couples split into two units when they file separately or are unmarried
partners. Married couples where only one spouse filed (X5746 = 3 or 4) are
kept as one unit by the builder and count as married filing jointly here.

**Split-off adults have zero wealth.** The SCF asks about their income in a
short section but not about their assets, and the builder gives them none. In
2016 they are 4,123 of the 34,814 filer rows (18.8 million of 140.2 million
weighted filers: 13.4%), all with every wealth component exactly zero. The
model will learn that some low-income young filers have no wealth at all, and
on the PUF it cannot tell these units apart from others with the same
incomes. **John's decision (9 October): keep them, with wealth unobserved.**
Their rows, incomes, demographics and `scf_weight` stay; all 24 wealth
variables are -1 in both the token and truth files and NaN in the records
files, which carry a column `wealth_observed` (False for these units and for
every PUF record). **Wealth ranks and the wealth quantile functions are
computed over units with wealth observed only.** So the model does not learn
false zeros from them, and the SCF still covers all 140 million filers for
the income and demographic variables. Rows affected: 4,123 filer rows in 2016
(18.8 million weighted) and 3,255 in 2013 (14.9 million).

## 3. The PUF side

2015 PUF records without the four aggregate records, test 1's split (joined
on RECID). Dependent filers (DSI = 1: 6,526 records, 9.3 million weighted)
are removed, because the SCF builder never makes a financially dependent
household member into a tax unit, so the SCF filers include no dependent
filers. That leaves 201,166 records (141.6 million returns): 160,943 train,
20,093 val, 20,130 test. Age range is unobserved (-1) for 87,950 records
absent from the demographic file (the high-income strata, as in test 1) and
224 coded 0, so 43.8% of PUF records have no age range.

## 4. The datasets

All five have the same 36 token columns: `source` (context), `age_range`,
`married`, `n_dep`, the seven incomes, `scf_weight`, then the 24 wealth
components in order of how common holding is among SCF training records
(cash, vehicles and other nonfinancial assets, home, home capital gain,
retirement accounts, ... trusts last). `meta.json` lists every token's
meaning.

| Dataset | Train | Val | Test | Notes |
|---|---|---|---|---|
| `expA_clean`, `_noise`, `_recat`, `_unrep` | 31,299 (half 1: 15,653) | 3,515 (half 1: 1,790) | 17,443 (all of half 1) | SCF 2016 filers; 2,852 households in each half |
| `expB` | 208,387 (PUF 160,943; SCF 47,444) | 26,802 (6,709 SCF) | 33,545 (PUF 20,130; SCF 13,415) | SCF households 70/10/20 per wave |

Rows are tax unit by implicate; a household's units and implicates are
always in the same part (checked).

Rows with true wealth, which are the ones that can be graded (split-off
units have none):

| Dataset | Train | Val | Test |
|---|---|---|---|
| Experiment A (all four) | 27,583 of 31,299 | 3,108 of 3,515 | 15,380 of 17,443 (2,063 split-off rows of half 1 are completed but not graded) |
| `expB`, SCF rows | 42,057 of 47,444 | 6,112 of 6,709 | 12,021 of 13,415 (1,394 split-off rows) | Seeds: split 20261009, alterations
20261010, rank tie-breaking 20261011, round-trip decoding 20261012.

**Ranks.** Each amount's token is its weighted rank band among the positive
(or negative) values of its own source and year, computed from that
source-year's training records. Ties (common round amounts) are spread at
random over the rank bands they span. Positive bands: steps of 0.02 to 0.90,
0.01 to 0.99, 0.001 to 0.999, then 0.9999, 0.99999 and 1 (66 bands). Negative
bands, most negative first: 0.001, 0.01, 0.05, then tenths (13 bands); finer
at the large-loss end because plain tenths put the largest losses in one wide
band and made decoded loss totals noisy (PUF Schedule E losses were off by
31%). `scf_weight`: 64 bands of the weight's unweighted rank within the SCF
source-year.

**Decoding to dollars.** A rank is drawn uniformly inside the band and read
off the source-year's quantile function, interpolated in log dollars between
neighbouring values; values held by at least 0.2% of a side come back
exactly. Wealth of a record from a source that does not observe it is decoded
with the SCF's function for the same year, or for 2015 by interpolating 2013
and 2016 in log dollars (weights 1/3 and 2/3). As a check, the same SCF 2016
test tokens decoded with the 2013, interpolated 2015 and 2016 functions give,
for equities, $1.84, $2.30 and $2.55 trillion (2013 and 2016 in their own
dollars), and for homes $4.19, $4.56 and $4.77 trillion: the 2015 values lie
between, nearer 2016.

**Experiment A variants** (half 1 only, in dollars before ranking, one draw
per tax unit shared by its five implicates). Effect on half 1's tokens
compared with `expA_clean`:

| Variant | Income | Tokens unchanged | Share zero, clean | Share zero, variant | Rank correlation among positives |
|---|---|---|---|---|---|
| noise | wages | 35% | 22.6% | 22.6% | 0.969 |
| noise | int_div | 79% | 70.4% | 70.4% | 0.995 |
| noise | ss_pens | 77% | 73.3% | 73.3% | 0.944 |
| recat | wages | 45% | 22.6% | 16.6% | 0.988 |
| recat | business | 100% | 83.5% | 83.5% | 1.000 |
| unrep | int_div | 82% | 70.4% | 78.2% | 0.999 |
| unrep | capital_gains | 88% | 82.2% | 87.2% | 0.999 |

(The noise variant changes every income; the table shows three.) Moving 30%
of business income to wages leaves business tokens untouched, as expected:
scaling every positive amount by 0.7 keeps their order, so ranks absorb it;
what the variant tests is the change in wages (6 points fewer units with no
wages, and wage ranks shifted). The unreported-income variant sets 1,365
records' interest and dividends and 874 records' capital gains to zero.
Unaltered incomes have identical tokens in every variant.

## 5. Round trip (dollars to tokens to dollars)

Training records of every source-year with the variable observed (for
wealth, units with `wealth_observed`), true tokens decoded with five seeds.
Shares at zero are reproduced exactly (zero is its own token). Full tables:
`$S/data/<dataset>/roundtrip.csv`; columns `total_rel_err_mean_5_seeds` and
`_sd_5_seeds` separate systematic error from decoding noise.

| Source-year | Variables | Median absolute error of the total (mean of 5 seeds) | Largest errors of a total, averaged over 5 seeds (standard deviation across seeds in brackets) | Largest draw-to-draw spread of a total (standard deviation) |
|---|---|---|---|---|
| PUF 2015 | 7 incomes | 0.10% | all within 0.3% | 0.6% (capital gains) |
| SCF 2013 | 32 | 0.23% | life insurance -2.2% (1.0%), capital gains -1.1% (2.7%), credit lines -1.1% (0.5%) | 2.7% (capital gains) |
| SCF 2016 | 32 | 0.28% | other debt +2.8% (1.1%), credit lines +2.4% (3.5%), annuities +1.7% (1.5%) | 3.5% (credit lines), 2.6% (bonds) |
| SCF 2016, experiment A half 2 | 32 | 0.50% | ui_other +3.2% (0.8%), trusts +1.9% (1.9%), other debt +1.7% (2.2%) | 4.7% (installment debt), 3.0% (other real-estate gains) |

Quantiles among nonzero holders: within 1% at the 10th to 90th percentiles for
the common variables; up to 8% for business and capital gains at the 99th
percentile; at the 99.9th percentile of rare variables (annuities, trusts,
credit lines, under 4% of units holding) one or two SCF records decide the
value, and decoding can move it by 50% or more. Re-encoding decoded values
gives the same token for 88% (home value) and 89% (wages), where many records
report the same round amount, and 93% to 99% for the other variables.

**A caution for the evaluation.** In experiment A, half 1's wealth is
decoded with half 2's quantile functions, since half 1 plays a source without
wealth. Even half 1's *true* tokens then decode to dollar totals that differ
from its true dollars by the sampling difference between the halves: credit
lines -43%, defined-benefit pensions +12%, real-estate funds +5%, other
financial assets -4% (means of 5 seeds) (rows with source `SCF_H1` in `roundtrip.csv`). Dollar
comparisons in experiment A should be made against the decoded truth, or in
rank tokens.

## 6. For John to decide

1. Decided: split-off adults kept with wealth unobserved (section 2).
2. **The filing rule for split-off adults** (section 2): wages count as filing.
3. **The crosswalk** (`a_crosswalk.md`): taxable pensions rather than total
   pensions; `ui_other` narrowed to unemployment, workers' compensation, child
   support and alimony; IRA withdrawals left out of both sides; large gaps in
   who has interest, dividends, capital gains and business income.
4. **Age**: the SCF side uses the unit head's age at the interview, about a
   year after the tax year; PLAN mentioned the older spouse's age.
5. The production builder's `rent_scf` and `business_scf` diagnostic columns
   are mislabelled (rent paid; business including Schedule E). Not used by
   `wealth.R`; worth fixing when `src/` is next touched.

## 7. `expB_multi` (added 15:10)

`expB` plus SCF 2019 and 2022 filers, at John's request, with the same builder
and rules. `expB`'s PUF rows and its 2013 and 2016 SCF households are in the
same parts; 2019 and 2022 households are split 70/10/20 by the same seeded
generator, continued. Within each part, `expB`'s rows come first and in
`expB`'s order, and their tokens and truth are identical to `expB`'s
(checked). Token columns are in `expB`'s order (completion order taken from
`expB`).

| SCF wave (tax year) | Households with filers | Filer units, millions | Filer net worth, wave's own dollars, $ trillion | Share of filer weight with wealth observed |
|---|---|---|---|---|
| 2013 (2012) | 5,484 | 132.3 | 63.8 | 88.7% |
| 2016 (2015) | 5,704 | 140.2 | 85.6 | 86.6% |
| 2019 (2018) | 5,252 | 142.0 | 94.9 | 86.9% |
| 2022 (2021) | 4,221 | 149.0 | 137.7 | 88.2% |

For 2022 the units again add up exactly to the summary file (net worth over
all units equals it to the cent; filers hold 99.0%); its summary file is in
its own dollars (factor 1.0000 for wealth, 1.0809 for income). One 2022
household reports -1, the codebook's "nothing" code, for Social Security and
pensions (and has a -1 defined-benefit value) in all five implicates; these
10 values are set to 0. No other wave has negatives in either variable.

| Part | Rows | PUF | SCF 2013 | SCF 2016 | SCF 2019 | SCF 2022 | SCF rows with wealth observed (graded) |
|---|---|---|---|---|---|---|---|
| Train | 248,671 | 160,943 | 22,963 | 24,481 | 22,231 | 18,053 | 77,916 |
| Val | 32,662 | 20,093 | 3,200 | 3,509 | 3,202 | 2,658 | 11,317 |
| Test | 45,134 | 20,130 | 6,591 | 6,824 | 6,449 | 5,140 | 22,335 |

Round trip: the median absolute error of a decoded total is 0.15% (2019) and
0.24% (2022). The largest are other debt +2.4% (2019) and unrealized gains on
other assets -2.7% (2022). Credit lines in 2019 vary by 16% from one decoding
seed to the next: a few very large balances in a thinly held variable.
