---
title: "Test 1, part A: data preparation report"
role: results
workstream: rebuild
status: current
updated: 2026-10-08
sot: research/rebuild/tests/puf_generator/SPEC.md
supersedes: []
superseded_by: null
---

# Test 1, part A: data preparation report

This note describes how the 2015 PUF was turned into the training, validation
and test files for the transformer test, and how much information the
conversion to tokens loses. The contract is `SPEC.md`, section A. Every number
here comes from the run of `prepare_data.py` through Slurm on 2026-10-08
(outputs in `/nfs/roberts/scratch/pi_nrs36/jar335/puf_generator/data/`). The
tables give counts and relative errors only; no record values appear here.

Two terms used below. A **token** is the integer code a variable takes in the
model's input: for a categorical, one token per category; for an amount, one
token per value range ("bin"), plus tokens for zero and for a few exact values.
A variable's **vocabulary** is its set of tokens.

## Files

| File | Contents |
|---|---|
| `prepare_data.py`, `prepare_data.sbatch` | The preparation script and its Slurm job (code folder). |
| `tokenizer.py` | `load_meta()`, `encode(df, meta)` and `decode(tokens, meta, rng)` (code folder). |
| `$S/data/meta.json` | Variable list in generation order, every variable's tokens with their values or bounds, seeds, counts, split strata. |
| `$S/data/tokens_{train,val,test}.npy` | int16 arrays, one row per record, one column per variable in `meta.json` order. |
| `$S/data/values_{train,val,test}.parquet` | Original values, same row order: `RECID`, `weight` (= S006/100), then each variable by PUF name. Categoricals are int64 codes with -1 for "not in the demographic file"; amounts are float64 dollars. `S006` itself is not a column; the weight appears only as `weight`. |
| `$S/data/bins_summary.csv`, `roundtrip.csv`, `roundtrip_quantiles.csv`, `split_strata.csv`, `prep_summary.json` | Supporting tables for this report. |

## Records

| | Records |
|---|---|
| PUF 2015, raw | 207,696 |
| Aggregate records set aside (`RECID >= 999996`) | 4 |
| Records kept | 207,692 |
| Of which in the demographic file | 119,675 |
| Of which absent from the demographic file (coded -1) | 88,017 |
| Training / validation / test | 166,156 / 20,768 / 20,768 |

The four aggregate records (RECIDs 999996 to 999999) have filing status coded
0, which no other record uses, and stand for groups of extreme-income returns
(three with very large positive AGI, one with a very large loss). They are
excluded, as `src/process_puf.R` also does. No other record was dropped.

**Missing values.** None of the 40 variables has a missing value in the 207,692
kept records. In the full raw file three columns not used here (E00800, E03500,
E87521) have missing values, and only on the aggregate records. The demographic
file joins one-to-one on RECID and every one of its records matches a PUF
record. The 88,017 records absent from it are not a random subset: the
demographic file covers only the PUF's sampling strata 7 to 13, the returns
sampled at 0.07 percent (demographic booklet, page 3). In this file that means
every record with weight of 200 or more has demographics and every record with
weight under 200 does not:

| Record weight | Records | Absent from demographic file |
|---|---|---|
| under 2 | 10,942 | 10,942 |
| 2 to 20 | 42,701 | 42,701 |
| 20 to 200 | 34,374 | 34,374 |
| 200 to 1,000 | 21,856 | 0 |
| 1,000 and over | 97,819 | 0 |

By the booklet's Table A, the records without demographics are those in the
strata with income of $250,000 or more in absolute value (positive or negative,
in the booklet's 1991 dollars), plus a 27-record stratum of very large Schedule C
businesses. So `missing` in AGERANGE, GENDER and EARNSPLIT stands for "large
positive or negative income". The model will learn that pattern; it is a
feature of the file, not an error.

## Split

80 / 10 / 10 within strata, seed 20261008. Within each stratum the records are
shuffled, 10 percent (rounded) go to validation, 10 percent to test, the rest to
training. Strata are record weight band crossed with AGI band:

- weight bands: under 2, 2 to 20, 20 to 200, 200 to 1,000, 1,000 and over;
- AGI bands: negative, 0 to 50k, 50k to 200k, 200k to 1m, 1m to 10m, 10m and over.

25 of the 30 cells are occupied (the PUF's sampling design leaves the others
empty). The five smallest occupied cells hold 9, 17, 20, 37 and 53 records; each still
puts at least one record in validation and test. The top is represented in every part:
for example the cell "weight under 2, AGI 10m and over" has 7,976 / 997 / 997
records. Full counts are in `meta.json` (`split.strata`) and `split_strata.csv`.

## Variables and generation order

40 variables, in this order (which is both the column order of the token
arrays and the generation order in `meta.json["order"]`):

1. weight (`S006`);
2. categoricals: `MARS`, `DSI`, `XTOT`, `N24`, `EIC`, `FDED`, `AGERANGE`, `GENDER`, `EARNSPLIT`;
3. AGI, `E00100`;
4. incomes, roughly from larger aggregate to smaller: `E00200` wages, `E00900` Schedule C, `E26270` partnership and S corporation, `P23250` long-term gains, `P22250` short-term gains, `E00600` ordinary dividends, `E00650` qualified dividends, `E00300` taxable interest, `E01500` total pensions, `E01700` taxable pensions, `E01400` taxable IRA distributions, `E02400` Social Security, `E25850` rent and royalty income, `E25860` rent and royalty loss, `E02100` farm, `E00400` tax-exempt interest, `E26390` estate and trust income, `E26400` estate and trust loss, `E01100` capital gain distributions, `E02300` unemployment compensation;
5. adjustments: `E03150` IRA contributions, `E03210` student loan interest, `E03270` self-employed health insurance;
6. itemized deductions: `E17500` medical, `E18400` state and local income or sales tax, `E18500` real estate tax, `E19200` interest paid, `E19800` cash charity, `E20100` non-cash charity.

### Categoricals

Category values found (all kept records); each value is one token.

| Variable | Values (records) |
|---|---|
| MARS | 1 (64,100), 2 (121,171), 3 (4,306), 4 (18,115) |
| DSI | 0 (201,166), 1 (6,526) |
| XTOT | 0 to 5 (0: 6,524; 5: 17,103) |
| N24 | 0 to 3 |
| EIC | 0 to 3 |
| FDED | 1 (116,965), 2 (82,966), 3 (7,761) |
| AGERANGE | -1 (88,017), 0 (252), 1 to 6 |
| GENDER | -1 (88,017), 1 (78,456), 2 (41,219) |
| EARNSPLIT | -1 (88,017), 0 (75,200), 1 to 3 |

Two coding points. AGERANGE 0 means "not present" in the booklet and is kept
as its own category (252 records). AGERANGE codes mean different age ranges for
dependent filers (DSI = 1: codes 1 to 3) and other filers (codes 1 to 6); the
token is the code, so the model has to learn this from DSI. EARNSPLIT 0 is "not
present" in the booklet (the split is shown only for joint returns) and is
distinct from -1, "not in the demographic file".

### The weight

`S006` takes only 73 distinct values in the kept records (one per sampling
stratum of the PUF), from 101 (record weight 1.01) to 170,280 (weight 1,702.8).
Since that is fewer than 256, each value is a point token and the weight
round-trips exactly. The vocabulary is built from all kept records, not only
training, so that validation and test never contain a weight value without a
token.

### Amounts

Every amount is a whole number of dollars in the file. Rules, following the
spec:

- **Zero** has its own token.
- **Point tokens.** A value gets its own token, decoded exactly, if it holds
  more than one bin's worth of the nonzero training records on its side of zero
  *and* at least 1 percent of them. The 1 percent floor is my addition: for rare
  variables one bin's worth is only about 20 records, and without the floor the
  PUF's rounding grid (amounts in multiples of 10 or 100) turned dozens of
  ordinary values into point tokens (64, the cap, for several variables).
- **Bins.** The remaining nonzero training values are cut at unweighted
  quantiles: up to 256 bins for positive values and 64 for negative values, at
  least 20 training records per bin. In the middle of the distribution the
  bins hold equal counts. At both ends the bins shrink toward 20 records (sizes
  20, 30, 45, 68, ... counting in from each end). The top end matters for
  totals: the largest amounts carry much of each aggregate, and with equal-count
  bins the top bin of AGI would hold the top 625 or so records. The bottom end
  matters for low quantiles that fall among the smallest nonzero amounts. Bins
  left with fewer than 20 records because of repeated values are merged into a
  neighbour.
- **Decoding.** A bin token is decoded by a draw that is uniform on the log
  scale between the bin's bounds, or uniform in dollars when the bin is narrow
  (upper bound under 1.1 times the lower), then rounded down to whole dollars.
  One exception: the **outermost bin on each side** (the largest amounts, and the
  largest losses) is decoded from a truncated Pareto distribution whose exponent
  is fitted by maximum likelihood to the training amounts in that bin. These
  bins are wide (a factor of 3 to 20 between bounds in the cases I checked)
  and their amounts bunch toward the lower bound; in a first run a log-uniform draw there moved
  the weighted total by 20 percent for capital gain distributions and by 1.8
  percent for farm income. The exponent is stored with the token
  (`decode: "pareto"`, `alpha`), so `tokenizer.decode` is fully described by
  `meta.json`.
- Tokens are numbered in value order (negative bins, zero, positive bins, with
  point tokens at their value), so a token's index is its position in the
  variable's order.

Validation and test values outside the training range are put in the end bin
on their side. This affects 1 validation record (E02400) and 7 test values
across 6 variables.

Bins per variable (training records; "points" are point tokens):

| Variable | Nonzero positive | Nonzero negative | Positive bins | Negative bins | Points | Vocabulary |
|---|---|---|---|---|---|---|
| E00100 AGI | 159,952 | 5,915 | 254 | 63 | 0 | 318 |
| E00200 wages | 132,344 | 0 | 256 | 0 | 0 | 257 |
| E00900 Sch. C | 30,175 | 10,553 | 254 | 63 | 0 | 318 |
| E26270 partnership/S corp | 38,784 | 16,563 | 252 | 60 | 0 | 313 |
| P23250 long-term gains | 56,745 | 9,934 | 251 | 61 | 0 | 313 |
| P22250 short-term gains | 17,359 | 35,410 | 246 | 58 | 8 | 313 |
| E00600 ordinary dividends | 77,332 | 0 | 244 | 0 | 3 | 248 |
| E00650 qualified dividends | 74,524 | 0 | 243 | 0 | 3 | 247 |
| E00300 taxable interest | 95,717 | 0 | 229 | 0 | 12 | 242 |
| E01500 total pensions | 40,064 | 0 | 253 | 0 | 0 | 254 |
| E01700 taxable pensions | 33,932 | 0 | 252 | 0 | 0 | 253 |
| E01400 taxable IRA | 21,532 | 0 | 252 | 0 | 0 | 253 |
| E02400 Social Security | 34,242 | 0 | 255 | 0 | 0 | 256 |
| E25850 rent income | 34,242 | 0 | 247 | 0 | 4 | 252 |
| E25860 rent loss | 20,779 | 0 | 252 | 0 | 3 | 256 |
| E02100 farm | 2,703 | 4,016 | 106 | 63 | 0 | 170 |
| E00400 tax-exempt interest | 36,972 | 0 | 250 | 0 | 3 | 254 |
| E26390 estate/trust income | 4,963 | 0 | 193 | 0 | 2 | 196 |
| E26400 estate/trust loss | 2,232 | 0 | 86 | 0 | 4 | 91 |
| E01100 cap. gain distributions | 4,978 | 0 | 145 | 0 | 9 | 155 |
| E02300 unemployment | 4,306 | 0 | 140 | 0 | 1 | 142 |
| E03150 IRA contributions | 4,190 | 0 | 26 | 0 | 12 | 39 |
| E03210 student loan interest | 7,463 | 0 | 162 | 0 | 3 | 166 |
| E03270 SE health insurance | 22,623 | 0 | 248 | 0 | 0 | 249 |
| E17500 medical | 7,650 | 0 | 237 | 0 | 0 | 238 |
| E18400 state/local income tax | 82,780 | 0 | 255 | 0 | 0 | 256 |
| E18500 real estate tax | 85,225 | 0 | 255 | 0 | 0 | 256 |
| E19200 interest paid | 72,908 | 0 | 251 | 0 | 0 | 252 |
| E19800 cash charity | 81,846 | 0 | 248 | 0 | 1 | 250 |
| E20100 non-cash charity | 46,081 | 0 | 222 | 0 | 11 | 234 |

Negative values occur only in AGI, Schedule C, partnership and S corporation
income, long- and short-term gains, and farm income. Losses on rent and on
estates and trusts are separate positive variables (E25860, E26400).

**Point tokens found.** Two kinds.

- Statutory or reporting thresholds: the $2,500 cap on the student loan interest
  deduction (1,262 training records, 17 percent of nonzero E03210); IRA
  contribution limits of $5,500 and $6,500 (age 50 and over) and their doubles
  for couples, $11,000, $12,000 and $13,000, plus round amounts from $1,000 to
  $10,000 (12 points in E03150, which leaves only 26 bins); the $25,000 special
  allowance for rental real estate losses (E25860); the $500 threshold above
  which non-cash gifts require Form 8283 (8,263 training records, 18 percent of
  nonzero E20100), with further spikes just below it at $450, $480 and $490 and
  at round amounts from $100 to $1,000; $500 in cash charity; $11,700 in
  unemployment compensation.
- Small round amounts: $2, $10, $20, $30 and so on in interest, dividends,
  rents, estates and trusts, capital gain distributions and short-term gains
  (including -$2 and -$10 for short-term losses). Taxable interest has 12 such
  points from $2 to $110. The PUF appears to round small amounts to multiples
  of 10, with a separate spike at $2 that I did not find explained in the
  booklet.

## Round-trip check

`decode(encode(train))` against the training records, weighted by the record
weight. Shares at zero and shares negative are exact by construction (zero has
its own token and bins never cross zero), and every categorical and the weight
come back exactly; these all pass. Re-encoding the decoded records gives back
the same token for at least 99.9 percent of records in every variable (a
decoded draw can land exactly on a point value).

The table gives, per amount: the relative error of the weighted total for the
reported seed (20261009), its mean and standard deviation over five decoding
seeds (20261009 to 20261013), and the largest relative error among the weighted
quantiles 10, 25, 50, 75, 90, 99 and 99.9 (quantiles that are zero are exact
and are counted as zero error).

| Variable | Total error | Mean, 5 seeds | Std. dev., 5 seeds | Largest quantile error (at) |
|---|---|---|---|---|
| E00100 | +0.09% | +0.09% | 0.01% | 0.12% (25th) |
| E00200 | +0.13% | +0.12% | 0.00% | 0.34% (25th) |
| E00900 | +0.10% | +0.12% | 0.02% | 1.20% (90th) |
| E26270 | +0.10% | +0.14% | 0.07% | 0.41% (99th) |
| P23250 | +0.16% | +0.22% | 0.05% | 0.29% (99th) |
| P22250 | -0.76% | -0.18% | 1.09% | 0.49% (99th) |
| E00600 | +0.22% | +0.32% | 0.08% | 0.63% (90th) |
| E00650 | +0.44% | +0.46% | 0.04% | 0.63% (99.9th) |
| E00300 | +0.60% | +0.43% | 0.10% | 3.00% (90th) |
| E01500 | -0.17% | +0.35% | 0.82% | 0.82% (99.9th) |
| E01700 | +0.19% | +0.20% | 0.01% | 0.29% (99th) |
| E01400 | +0.36% | +0.28% | 0.05% | 0.34% (99.9th) |
| E02400 | +0.19% | +0.20% | 0.00% | 0.26% (90th) |
| E25850 | +0.24% | +0.30% | 0.05% | 1.04% (99.9th) |
| E25860 | +0.08% | +0.08% | 0.09% | 0.50% (99th) |
| E02100 | -0.66% | +0.30% | 0.59% | 0.56% (99.9th) |
| E00400 | +0.30% | +0.34% | 0.04% | 1.37% (99.9th) |
| E26390 | +0.41% | +0.30% | 0.13% | 0.69% (99.9th) |
| E26400 | +0.28% | +3.35% | 2.25% | 0 (all quantiles zero) |
| E01100 | +1.29% | +7.92% | 8.85% | 0.76% (99th) |
| E02300 | +0.21% | +0.27% | 0.06% | 0.62% (99.9th) |
| E03150 | +0.25% | +0.33% | 0.10% | 0 (exact) |
| E03210 | +0.33% | +0.34% | 0.00% | 0 (exact) |
| E03270 | +0.25% | +0.24% | 0.01% | 0.49% (99.9th) |
| E17500 | +0.25% | +0.25% | 0.06% | 0.19% (99.9th) |
| E18400 | +0.20% | +0.23% | 0.03% | 0.48% (99th) |
| E18500 | +0.20% | +0.20% | 0.01% | 6.82% (75th) |
| E19200 | +0.27% | +0.25% | 0.01% | 0.29% (99.9th) |
| E19800 | +0.33% | +0.33% | 0.03% | 0.48% (99th) |
| E20100 | +0.17% | +0.34% | 0.74% | 0.10% (99.9th) |

Reading the table:

- For 28 of the 30 amounts the weighted total is within half a percent on
  average across seeds, and every listed quantile is within 1.5 percent except
  two. Most totals sit 0.1 to 0.4 percent high. That is the log-uniform draw
  inside bins: the true amounts in a bin are slightly more concentrated toward
  its lower end than a log-uniform spread assumes.
- The two larger quantile errors are both at quantiles that fall among very
  small amounts, where bins are wide relative to their values: the 75th
  percentile of real estate tax (E18500) falls just above its 74.9 percent share
  at zero, among the smallest positive values; the 90th percentile of taxable
  interest is a small dollar amount.
- **Capital gain distributions (E01100)** and **estate and trust losses
  (E26400)** have totals off by 8 percent and 3 percent on average, with large
  seed-to-seed variation. Both are rare variables (5,000 and 2,200 nonzero
  training records) whose top bins are wide because each must hold 20 records.
  For E01100 the top bin spans a factor of 20 and holds two heavily weighted
  records (weights about 1,400 and 330) near its lower bound; they account for
  most of the error, since any draw within that bin moves the weighted total by
  several percent. For E26400 the top bin spans a factor of 3, and most of the
  error comes from one record at its lower bound. This is
  a limit of binning a rare, skewed amount with mixed weights, not a coding
  error. It also bounds what the model can achieve on these two totals.
- Short-term gains, total pensions, farm income and non-cash charity have
  seed-to-seed standard deviations of 0.6 to 1.1 percent in the total, for the
  same reason on a smaller scale (a few heavily weighted records in wide bins);
  their mean errors are under 0.4 percent.

## Things worth knowing

- The PUF's weight is a stratum code as much as a weight: 73 values, and the
  demographic variables are present exactly for the records with weight of 200
  or more. Any model of this file will learn that AGERANGE, GENDER and EARNSPLIT are
  missing for high-income records.
- Many amounts bunch at statutory thresholds and at small round numbers; these
  are kept exact as point tokens.
- The 1 percent floor on point tokens is a choice beyond the spec; without it
  rare variables get dozens of point tokens from the rounding grid. The
  truncated-Pareto decode of the outermost bins is a second departure from the
  spec's "uniform on the log scale"; all other bins follow the spec.
- Seeds: split 20261008; round-trip decode 20261009 (and 20261010 to 20261013
  for the five-seed columns).
