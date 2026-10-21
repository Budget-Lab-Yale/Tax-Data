---
title: "Test 3, piece G: the dataset `full` (PUF variables plus SCF wealth) — data report"
role: results
workstream: rebuild
status: current
updated: 2026-10-09
sot: research/rebuild/tests/posttrain/SPEC.md
supersedes: []
superseded_by: null
---

# Test 3, piece G: the dataset `full`

This note describes the token dataset that the full joint model is trained
on: test 1's 39 PUF variables and the SCF's 24 wealth variables, with 2015
PUF returns and 2013 and 2016 SCF filer tax units in one file. The contract
is `SPEC.md`, entry "Main session, 9 October 20:45", item 1. Which PUF
variables the SCF observes, and how, is in `g_crosswalk.md`. Every number
here is a weighted aggregate from the Slurm runs of 9 October (logs in
`$P/logs/`, `$P` = `/nfs/roberts/scratch/pi_nrs36/jar335/posttrain`).

Two terms. A **token** is the integer code a variable takes in the model's
input. A variable is **unobserved** for a record when its token is -1: the
model never sees a value there and never scores one, and the record's
NaN in the records file means "not measured", not zero.

## What was built

| | `full` (version 1) | `full_v2` (version 2) |
|---|---|---|
| Folder | `$T2/data/full/` | `$T2/data/full_v2/` |
| PUF-side variables | test 1's 39 | 40: test 1's 39 plus total Schedule E (`E02000`) |
| PUF-side variables the SCF observes | 11 | 17 (adds `N24`, `GENDER`, `E02400`, `E01700`, `E02100`, `E02000`) |
| SCF unit file | test 2's (`$T2/data/scf_units/`) | rebuilt with extra columns (`$P/g/scf_units/`) |
| Status | `DONE` 20:42; piece H's first chain trains on it | `DONE` 20:54; **the main dataset** (coordinator, 9 October); piece H's chain for it starts on this marker |

(`$T2` = `/nfs/roberts/scratch/pi_nrs36/jar335/scf_joint`.)

Both have test 2's layout: `meta.json`, `tokens_{train,val,test}.npy`,
`truth_*.npy` (identical to the tokens: nothing is hidden on purpose),
`records_*.parquet`, `quantiles.parquet`, `roundtrip.csv`, `checks_*.csv`,
`g_tables.md`, `prep_summary.json`, `DONE`. Test 2's `b_model.py` and
`b_train.py` read them unchanged (a one-epoch run on version 1, CPU job
28902236, loaded, checked and trained without error).

**Variables.** 63 model variables plus the source tag (version 2: 64, with
total Schedule E after Schedule C): test 1's 39 PUF
variables in test 1's order (categoricals, AGI, incomes, adjustments,
itemized deductions), then the 24 wealth variables in test 2's expB order
(most to least commonly held). `meta.json` gives each variable a group:
`shared` (a PUF variable the SCF also measures), `puf_only` (always
unobserved for SCF records) or `scf_only` (wealth, always unobserved for PUF
records and for SCF split-off units). `generation_order` lists all 63 in the
order above, so test 2's `fill_order` fills them in that order. The year
(2015, 2013 or 2016) is a number in the records file, as in test 2.

**The weight is not a variable.** The PUF's `S006`/100 and the SCF unit's
implicate weight are the `weight` column of `records_*.parquet`, for
weighting the loss and for every table.

**Amounts** are rank tokens exactly as in test 2: the weighted rank among
positive (or negative) values within the record's own source and year, from
that source-year's training records; 66 positive bands (finer at the top),
13 negative bands for variables with any negative value, one token for exact
zero. Categoricals keep one token per value. Decoding to dollars uses
`g_tokenizer.py`, a copy of test 2's `a_tokenizer.py` with one change: a
PUF variable the SCF does not measure, when completed for an SCF record, is
decoded with the PUF 2015 quantile function (test 2's tokenizer would stop
with an error there, since it only knew how to borrow the SCF's functions).

## Records and split

| Part | All rows | PUF 2015 | SCF 2013 | SCF 2016 | SCF rows with wealth observed |
|---|---|---|---|---|---|
| Train | 213,600 | 166,156 | 22,963 | 24,481 | 42,057 |
| Val | 27,477 | 20,768 | 3,200 | 3,509 | 6,112 |
| Test | 34,183 | 20,768 | 6,591 | 6,824 | 12,021 |

- **PUF**: all of test 1's 207,692 returns (the four aggregate records were
  already set aside) and test 1's split. Unlike test 2, the 6,526 dependent
  filers (9.3 million returns) are kept, because `DSI` is now a model
  variable.
- **SCF**: test 2's filer tax units, all five implicates at one-fifth weight,
  and test 2's household split, reused by household id: the SCF row count in
  each part equals expB's exactly (checked in the script). Split-off units
  (financially independent adults in the household; 3,255 rows in 2013,
  4,123 in 2016) keep their incomes and demographics with wealth unobserved,
  as in test 2.
- **PUF demographic codes.** AGERANGE -1 (not in the demographic file:
  88,017 records, all of them high-income strata) and 0 ("not present": 252)
  are unobserved; GENDER -1 and EARNSPLIT -1 likewise (88,017 each). Test 1
  treated them as categories; here an age is something the model must infer,
  and the SCF, where every unit has an age, helps it do so at high incomes.

## Checks

### No unexpected missing values

The script stops if any column it uses has a missing value it did not
expect, or if the count of unobserved tokens per variable and source differs
from the design: PUF-only variables unobserved for every SCF row, wealth
unobserved for every PUF row and every SCF split-off row. Both checks pass.
The only other unobserved values (all reported, none dropped) are:

| Variable | Source | Rows unobserved | Why |
|---|---|---|---|
| AGERANGE | PUF | 88,269 | not in the demographic file (88,017) or coded 0 (252) |
| GENDER, EARNSPLIT | PUF | 88,017 | not in the demographic file |
| E00300, E00400, E00600 | SCF (version 1 only) | 36 (11 in 2013, 25 in 2016) | split-off units reporting interest or dividends in test 2's unit file, which does not say which; fixed in version 2 |
| GENDER | SCF (version 2) | 7,378 | split-off units (the builder does not carry their sex) |
| E02100 | SCF (version 2) | 634 | households that filed both Schedule C and F (480), split-off units with business income (154) |

Token -1 coincides exactly with NaN in the records files (asserted).

### Crosswalked items, PUF against SCF filers (version 1)

All parts together. "PUF all" is every return; "PUF no dep. filers" drops
dependent filers, the population the SCF's filer units correspond to. SCF
2013 amounts are tax-year 2012 dollars raised to 2015 by the CPI-U.

| Variable | PUF all total ($bn) | PUF no dep. filers total | SCF 2013 total | SCF 2016 total | PUF all nonzero | PUF no dep. filers nonzero | SCF 2013 nonzero | SCF 2016 nonzero | PUF no dep. filers negative | SCF 2013 negative | SCF 2016 negative |
|---|---|---|---|---|---|---|---|---|---|---|---|
| `E00200` wages | 7,145 | 7,086 | 6,948 | 7,941 | 82.9% | 82.2% | 80.4% | 79.9% | 0 | 0 | 0 |
| `E00900` Schedule C (SCF: C and F) | 332 | 331 | 410 | 576 | 16.4% | 17.2% | 9.0% | 11.6% | 4.1% | 1.1% | 0.9% |
| `P23250` long-term gains (SCF: all net gains) | 637 | 634 | 494 | 678 | 10.9% | 11.2% | 7.6% | 8.3% | 2.2% | 2.0% | 1.7% |
| `E00600` ordinary dividends | 241 | 238 | 210 | 250 | 18.4% | 18.9% | 12.0% | 11.9% | 0 | 0 | 0 |
| `E00300` taxable interest | 89 | 88 | 91 | 90 | 28.3% | 29.6% | 16.3% | 12.4% | 0 | 0 | 0 |
| `E00400` tax-exempt interest | 60 | 60 | 67 | 81 | 3.9% | 4.1% | 2.3% | 2.2% | 0 | 0 | 0 |
| `E02300` unemployment (SCF: plus workers' compensation) | 27 | 27 | 59 | 20 | 4.1% | 4.3% | 6.7% | 3.1% | 0 | 0 | 0 |

| Variable | Value | PUF all | PUF no dep. filers | SCF 2013 | SCF 2016 |
|---|---|---|---|---|---|
| `MARS` | 1 single | 47.2% | 43.8% | 41.9% | 44.9% |
| | 2 joint | 36.0% | 38.4% | 40.8% | 39.1% |
| | 3 separate | 1.9% | 2.1% | 2.2% | 2.7% |
| | 4 head of household | 14.8% | 15.8% | 15.1% | 13.2% |
| `DSI` | 1 dependent filer | 6.1% | 0 | 0 | 0 |
| `XTOT` | 0 | 6.1% | 0.0% | 0 | 0 |
| | 1 | 40.7% | 43.4% | 43.0% | 46.6% |
| | 2 | 27.7% | 29.5% | 30.7% | 29.6% |
| | 3 | 11.7% | 12.5% | 8.7% | 7.7% |
| | 4 | 9.3% | 9.9% | 11.0% | 10.4% |
| | 5 or more | 4.4% | 4.7% | 6.6% | 5.7% |
| `AGERANGE` (where known) | under 26 | 12.6% | 12.1% | 11.7% | 13.9% |
| | 26 to 34 | 22.9% | 19.3% | 16.8% | 15.9% |
| | 35 to 44 | 16.9% | 17.9% | 18.3% | 17.1% |
| | 45 to 54 | 16.4% | 17.5% | 18.9% | 17.7% |
| | 55 to 64 | 15.1% | 16.1% | 16.8% | 16.5% |
| | 65 and over | 16.1% | 17.1% | 17.6% | 19.0% |

What the comparison shows:

- **Population and filing status line up**: 141.6 million PUF returns
  without dependent filers against 140.2 million SCF 2016 filer units;
  shares single, joint, separate and head of household within 3 points.
- **Wages** match in who has them (82 against 80 percent); the SCF total is
  12 percent higher in 2016, a level difference the rank tokens absorb.
- **Who has capital income differs a lot**, as test 2 found: taxable
  interest 30 percent of PUF returns against 12 to 16 percent of SCF units,
  dividends 19 against 12, Schedule C 17 against 9 to 12 (and losses 4
  against 1), gains 11 against 8. Rank tokens do not absorb these, because
  zero is its own token: the model will learn that "SCF" means fewer small
  holders, and the source tag carries that difference.
- **Dollar totals** of interest, dividends and gains are close (within 15
  percent) despite the nonzero gaps: the SCF's holders are fewer and larger.
  The SCF's Schedule C total is 74 percent above the PUF's in 2016 (it
  includes farm, and probably some pass-through income respondents call
  "business").
- **Unemployment** in the SCF 2013 (tax year 2012) is twice the PUF 2015
  level: the SCF item adds workers' compensation, and unemployment
  compensation was much higher in 2012.
- **Exemptions**: the SCF has fewer units with three exemptions and more
  with four or five, as test 2 found for dependents (the PUF counts older
  students and other relatives; the SCF's builder counts children under 19).
- **Age**: the SCF has fewer units aged 26 to 34 than the PUF; SCF ages are
  measured at the interview, about a year after the tax year.

PUF totals of every PUF-only variable, and the SCF wealth totals, are in
`$T2/data/full/g_tables.md`.

### Round trip (dollars to tokens to dollars)

Training rows, each source-year with the variable observed, true tokens
decoded with five seeds (`roundtrip.csv`).

| Source-year | Amount variables | Median absolute error of the total (mean of 5 seeds) | Largest errors (mean of 5 seeds; standard deviation across seeds) |
|---|---|---|---|
| PUF 2015 | 30 | 0.05% | non-cash charity +0.9% (2.2%), total pensions +0.5% (0.3%), capital gain distributions +0.5% (1.0%) |
| SCF 2013 | 31 | 0.30% | other financial assets -3.0% (1.1%), life insurance -1.9% (0.4%), defined-benefit pensions +1.9% (1.0%) |
| SCF 2016 | 31 | 0.38% | annuities +1.8% (0.7%), credit lines +1.8% (6.8%), taxable interest +1.6% (0.6%) |

Shares at zero and shares negative are reproduced exactly (zero has its own
token and bands never cross zero). Re-encoding decoded values gives the same
token for 87 percent of home values and 88 percent of taxable interest
(common round amounts spread over several bands), 93 to 100 percent for
every other variable. On the PUF, rank tokens are coarser than test 1's
value bins (66 bands against up to 256), but the round trip of totals is
still within 1 percent for every PUF variable.

## Version 2 (`full_v2`)

Built by `g_prepare.py --units g` (CPU job 28903209), marker `DONE` at
20:54; same records, split, layout and checks as version 1 (all passed).
64 model variables: the 40 PUF-side variables, then the 24 wealth variables.
(An earlier build without `E02000`, job 28902903, wrote a `DONE` at 20:50
that was withdrawn two minutes later, before any job used it.)
The SCF unit file was rebuilt (`g_build_units.R` with `g_scf_tax_units.R`,
R job 28902493): copies of test 2's builder that only add output columns, so
filers, weights, wealth and test 2's seven incomes are unchanged. The SCF
now also observes:

- `N24`: children under 17 in the unit (builder's count, never above its
  dependents), top-coded at 3.
- `GENDER`: sex of the unit's head; unobserved for split-off units (7,378
  rows), whose sex the builder does not carry.
- `E02400` Social Security: current benefit of the respondent and spouse,
  annualized, each person's to their own unit when a couple is split;
  split-off units: their own Social Security.
- `E01700` taxable pensions: the SCF item for Social Security, pensions and
  annuities (X5722) less that Social Security amount, floored at zero,
  split by earnings share; split-off units: their pension income.
- `E02100` farm: the business amount of households that filed Schedule F but
  not Schedule C (X5749, X5747; 235 rows in 2016), which `E00900` then
  excludes; unobserved for households that filed both (255 rows in 2016)
  and for split-off units with business income (110).
- Split-off units' interest and dividends from their own answers, so the 36
  unobserved values of version 1 are gone.
- **`E02000` total Schedule E** (John, 9 October, option b in
  `g_crosswalk.md`): a 40th PUF-side variable, observed in both sources.
  PUF: `E02000` from the raw 2015 PUF. SCF: X5714 (rent, royalties,
  partnerships, S corporations, estates and trusts), split by earnings
  share, plus split-off units' rent income: test 2's `rent`. It comes right
  after `E00900` in the generation order; the five separate Schedule E
  pieces stay PUF-only.

| Variable | PUF no dep. filers total ($bn) | SCF 2013 total | SCF 2016 total | PUF no dep. filers nonzero | SCF 2013 nonzero | SCF 2016 nonzero |
|---|---|---|---|---|---|---|
| `E02000` total Schedule E | 704 | 980 | 1,142 | 12.4% (4.4% losses) | 9.6% (1.0% losses) | 10.2% (1.2% losses) |
| `E02400` Social Security | 603 | 552 | 654 | 19.7% | 20.6% | 21.6% |
| `E01700` taxable pensions (SCF: gross pensions and annuities) | 692 | 549 | 663 | 20.0% | 19.6% | 20.5% |
| `E00900` Schedule C (now without farm-only households) | 331 | 393 | 568 | 17.2% | 8.6% | 11.3% |
| `E02100` farm | -15 | 17 | 8 | 1.3% (0.9% losses) | 0.5% | 0.4% |

| Variable | Value | PUF no dep. filers | SCF 2013 | SCF 2016 |
|---|---|---|---|---|
| `N24` | 0 | 71.2% | 75.4% | 77.9% |
| | 1 | 14.4% | 13.5% | 12.7% |
| | 2 | 10.0% | 8.2% | 7.0% |
| | 3 or more | 4.4% | 2.9% | 2.5% |
| `GENDER` (where known) | 1 male | 62.8% | 67.9% | 68.7% |
| | 2 female | 37.2% | 32.1% | 31.3% |

Social Security and pensions line up well in both who receives them and the
totals. Total Schedule E does not line up in level: the SCF total is 39
percent (2013, in 2015 dollars) to 62 percent (2016) above the PUF's, with
somewhat fewer units reporting any amount (10 against 12 percent) and far
fewer losses (1 against 4 percent; the SCF masks some losses, see test 2's
`a_crosswalk.md`). The SCF's holders are fewer and larger, as for the other
capital incomes; rank tokens absorb the level difference but not the
difference in who has a loss. Farm does not: the PUF's farm sector is a net loss, the SCF's a
small net gain, and the SCF identifies far fewer farm units; it is a small
variable either way. The SCF has more male heads than the PUF has male
primary filers (69 against 63 percent): the SCF's respondent in a
mixed-sex couple is usually the man, while the PUF's primary filer on a
joint return is not always.

One caution on the pension amount. In the raw items (all households), X5722
totals $1,444 billion in 2016 and current Social Security annualized $812
billion, which is consistent with X5722 including Social Security. But
among households receiving Social Security, 31 percent report a current
annual benefit above their prior-year X5722 (benefits that started during
the survey year, or Social Security left out of X5722); for them the pension
amount is floored at zero.

Round trip: median absolute error of a total 0.06 percent (PUF), 0.25
percent (SCF 2013 and 2016); total Schedule E within 0.5 percent in every
source-year. The largest errors are SCF farm (-22 percent in 2013, -10
percent in 2016, with seed-to-seed spreads of 9 and 4 percent): few SCF farm
units, and a net total of gains and losses close to zero, so small dollar
errors are large relative to it. Otherwise, as in version 1, life
insurance, defined-benefit pensions, credit lines and other financial
assets (2 to 3 percent), and PUF short-term gains (+1.8 percent, also a net
of gains and losses).

## For John to decide

1. **Version 2 is the main dataset** (coordinator, 9 October): it adds
   Social Security, pensions, children under 17, sex, farm and total
   Schedule E to what the SCF observes. Version 1 stays on disk for piece
   H's first chain.
2. **Schedule E**: decided (option b): total Schedule E added as a 40th
   variable in version 2; the SCF's level is well above the PUF's (above).
3. **Dependent filers** are kept on the PUF side.
4. **AGERANGE, GENDER, EARNSPLIT outside the demographic file** are
   unobserved rather than a "missing" category.
