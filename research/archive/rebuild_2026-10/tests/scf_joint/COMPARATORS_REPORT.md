---
title: "Test 2 — the comparison methods (piece D)"
role: method
workstream: rebuild
status: current
updated: 2026-10-09
sot: research/rebuild/tests/scf_joint/SPEC.md
supersedes: []
superseded_by: null
---

# Test 2: the comparison methods

Piece D of test 2 (see `PLAN.md` and `SPEC.md`, section D). Two existing
kinds of method complete the same records as the model, from the same
training records, so that piece C can put all of them in the same tables:

1. **The production method** (`d_drf.R`): the wealth imputation in
   `src/imputations/wealth.R`, refit outside the pipeline, without its
   final calibration step.
2. **Sequential trees** (`d_seqtrees.py`): one gradient-boosted tree
   classifier per wealth variable, applied in a chain.

**Split-off SCF units.** The SCF tax-unit builder turns financially
independent adults in a household into their own tax units, and gives them
zero wealth by construction (the SCF records wealth for the household's
primary economic unit only). By John's decision of 9 October, their wealth
is treated as unobserved in this test (piece A, "Contract changes", 13:19).
Both methods therefore train only on units whose wealth is observed, and
still complete every target record, split-off units included. This differs
from production, which keeps these units in its donor pool with their zero
balance sheets, so in production a low-income single adult can receive an
all-zero balance sheet from such a donor.

Both are trained only on records whose wealth is observed: in experiment A,
half 2's training rows (half 2's validation rows are used only by the
sequential trees, to decide when to stop adding trees); in experiment B,
the SCF training rows of 2013 and 2016. They complete the target records:
in experiment A, half 1 (the "pretend PUF"); in experiment B, the held-out
SCF test units and, in addition, the PUF test records at 2015. Each record
gets five completions ("draws"), and each method is run with three seeds.

Outputs are in `$S/comp/<dataset>/` (`$S` is
`/nfs/roberts/scratch/pi_nrs36/jar335/scf_joint`):
`completed_<method>_seed<NN>.parquet` for the SCF or half-1 targets and
`completed_<method>-puf_seed<NN>.parquet` for the PUF targets of experiment
B, with `<method>` either `drf` or `seqtrees`. The layout is the one B uses
(`SPEC.md`, "Contract changes"): the target record's columns from
`records_test.parquet`, `row` (its row in that file), `draw` (0 to 4), the
completed wealth in dollars under each variable's own name, and the
completed rank tokens in `tok_<variable>`. The PUF files from the production
method have no token columns, because `a_tokenizer.encode` has no single
SCF rank scale for 2015 (it lies between the two waves); piece C reads their
dollars. Each run also writes `run_<method>_seed<NN>.json` with its
settings, counts and timings.

## 1. The production method, refit (`d_drf.R`)

### What production does

`run_wealth_imputation()` in `wealth.R` has two stages that matter here
(Stage 1, the split of SCF households into tax units, happens upstream and
is done for this test by piece A):

- **Stage 2, the forest and the donor draw.** The SCF tax units are split
  into eight income groups by their weighted rank of total income (0–20,
  20–40, 40–60, 60–80, 80–90, 90–99, 99–99.9 and 99.9–100 percent). In each
  group, 100,000 rows (150,000 in the top group) are drawn with replacement,
  with probability proportional to the survey weight; this "weighted
  bootstrap" is how the survey weight enters. A distributional random
  forest is fit to each group's bootstrap. A distributional random forest
  is a random forest whose splits are chosen to separate records with
  different joint distributions of all 24 wealth amounts at once, rather
  than different averages of one. Settings: 500 trees, at least 50 records
  per leaf, 10 candidate features per split, 50 random projections in the
  splitting criterion, "honest" trees (the records that choose the splits
  are not the records that sit in the leaves). Each PUF record is placed in
  an income group by its weighted rank within the PUF, picks one of the
  group's 500 trees at random, follows it down to a leaf, and receives the
  complete 24-variable balance sheet of one SCF record drawn at random from
  that leaf. The record's assets, debts and gains therefore always come
  together from one real SCF unit.
- **Stage 3, the calibration.** The donor probabilities are tilted so that
  totals by income group and age group match SCF totals, and the results
  are then rescaled category by category. This stage is left out, as
  `SPEC.md` asks: it forces aggregates to match the SCF, which would make
  the comparison of aggregates uninformative.

The features (wealth.R, lines 216–225) are: total income's weighted
percentile and its sign; married; the age of the older spouse and of the
younger spouse; the number of dependents; and, for wages, business income,
interest and dividends, capital gains, and Social Security and pensions,
each one's weighted percentile within the dataset (with finer steps in the
top 1 percent) and whether it is positive, zero or negative. Rent and the
"unemployment and other" category enter only through total income.

### What the refit does, and every deviation from production

The refit copies Stage 2 line by line (the feature construction, the income
groups, the bootstrap sizes, the forest settings, the tree walk and the
uniform draw from the leaf), with these differences:

1. **Ages.** Production uses the exact ages of the older and the younger
   spouse (on the PUF, these come from Tax-Data's own age imputation). The
   shared variables of this test carry only the age range of the head (the
   PUF demographic file's six `AGERANGE` groups), because that is all the
   PUF itself reports. The refit uses the age-range code in place of the
   older spouse's age and drops the younger spouse's age, so it has 19
   features instead of 20. The SCF records carry exact ages, but using them
   would give the comparator information the model does not have.
2. **PUF records without an age range** (experiment B only; 8,822 of the
   20,130 PUF test records, those absent from the
   demographic file). Production has an imputed age for every record. The
   refit draws an age range for each such record from the SCF training
   records in the same income group and marital status, with probability
   proportional to the survey weight (seeded; once per record, the same in
   all five draws).
3. **Draws and seeds.** Production draws one donor per record, keyed by the
   record's id. The refit makes five independent draws per record (each
   picks its own tree and its own donor from the leaf). The run seed (1, 2
   or 3) sets the bootstrap and the forest's own seed, so the three seeds
   are three independent refits; the draws are seeded from the run seed and
   the draw number.
4. **Ranks within the target set.** As production ranks PUF incomes within
   the PUF, the refit ranks the target records' incomes within the target
   set itself, separately for each source and year (half 1 in experiment A;
   the 2013 test units, the 2016 test units and the PUF test records
   separately in experiment B), using the records' population weights. The
   training records are ranked within all SCF units of the training split,
   **split-off units included** (their incomes are observed), and only then
   restricted to the units with wealth observed. This matters. A first run
   ranked the training records among wealth-observed units only, while the
   targets were ranked among all units, split-off units included. Split-off
   units are mostly low-income single adults, so the same income ranked
   higher among the targets than among the donors, and targets were matched
   to richer donors: mean net worth of half 1 came out at about $1.05
   million against a true $725,000. Ranking the targets on the donors' scale
   instead brought it to about $850,000 (seed 1, a diagnostic run). The
   production pipeline does not have this problem, because its donor table
   keeps the split-off units (with zero wealth) and the PUF contains
   everyone. The first run's files were deleted and replaced.
5. **Two SCF waves (experiment B).** Production pools SCF 2019 and 2022
   (`pool_scf_donors.R`): the older wave is rescaled to the newer wave's
   dollars, column by column, by the ratio of weighted totals, and each
   wave's weights are scaled to the same population and halved. The refit
   does the same with 2013 (rescaled to 2016 dollars) and 2016, including
   the seven income columns. A donor's wealth is then put back into the
   target's year: divided by the same per-column factor for 2013 test
   units, unchanged for 2016 test units, and multiplied by the factor to
   the power −1/3 for PUF records at 2015 (one third of the way back from
   2016 to 2013, in logs). The factors are in `run_drf_seed<NN>.json`.
6. **The $100 interest threshold.** Production sets PUF interest plus
   dividends of $100 or less to zero before building features, to match the
   share of SCF units reporting any. The refit does the same for PUF
   records; it does not apply to SCF records in either source.
7. **Not done:** the Stage 3 tilt and rescale, the zeroing of dependent
   filers' wealth (piece A's universe decision governs dependents), the
   removal of SCF units with more than $1 billion (production's Forbes
   step), and the derived basis and accrual columns. Production's per-cell
   minimum leaf size can be overridden by an environment variable; the
   refit uses the production default, 50.

The fitted forests are stored in `$S/comp/<dataset>/drf_cache/` and never in
`resources/cache/`, so no production fit can be overwritten. In the
experiment A variants only half 1's incomes change, so the training records
are identical to `expA_clean`'s; with `DRF_CACHE_FROM=expA_clean` the
variants reuse its forests after checking that the training inputs are
identical in every value.

The script is in three steps, run as Slurm jobs: `d_drf_prep.py` (reads the
dataset, checks that the income dollars in the records file are exactly what
the income tokens encode, so the production method sees the same incomes as
the model, and writes the inputs), `d_drf.R` (fits and draws, in dollars),
and `d_drf_finish.py` (encodes the completed dollars to rank tokens with
`a_tokenizer.encode` and writes the completed file).

Diagnostic switches in `d_drf.R`, used only for the checks described under
"First look" and never for the completed files: `D_TARGETS` and `D_OUT`
complete another target file and write elsewhere; `D_RANK=joint` ranks the
targets on the training records' scale; `D_MODE=sw` fits each forest to the
original records with the survey weight instead of the weighted bootstrap
and draws donors within the leaf in proportion to the weight. The
diagnostic runs and their scripts are under `$S/dev/d/`.

## 2. Sequential trees (`d_seqtrees.py`)

This is the standard "chained" imputation, done in the model's units:

- Everything is in rank tokens. The features are the shared variables (age
  range, married, number of dependents, and the seven incomes' rank tokens,
  ordered so a tree can split on "rank above x") and, in experiment B, the
  year as a number.
- For each wealth variable in the dataset's completion order (the same
  order the model uses: most to least commonly held), a gradient-boosted
  tree classifier predicts that variable's token from the features plus all
  wealth variables earlier in the order. A gradient-boosted tree classifier
  is a sum of many small decision trees, each fit to correct the errors of
  the ones before, whose output is a probability for each possible token.
  It is trained on the true values of the training records, weighted by the
  population weight, so it estimates the population's relationship rather
  than the oversampled sample's.
- Completion runs the chain: draw the first variable's token from its
  predicted probabilities, then the second given the first, and so on, five
  separate times per record. Tokens are turned into dollars with
  `a_tokenizer.decode`, as the model's are.
- PUF records without an age range (experiment B) get one drawn first, from
  a classifier of the same kind on the other shared variables.

Settings: scikit-learn's `HistGradientBoostingClassifier`, learning rate
0.1, at most 7 leaves per tree, at least 300 records per leaf, a ridge
penalty of 1 on leaf values (`l2_regularization`), at most 300 rounds. The
number of rounds is chosen per variable on the dataset's own validation
households: the weighted log loss (the average of minus the log of the
probability given to the true token) is checked every 10 rounds, the best
model is kept, and boosting stops after three checks without improvement.
In practice 10 to 50 rounds are kept; the SCF half has only about 2,600
training households, and each appears up to five times (the implicates),
so the classifiers overfit quickly. Three choices came out of trials on
`expA_clean`'s validation households (never on half 1), and are stated
because they matter:

- **The ridge penalty.** Without it the loss diverges within a few rounds:
  for rare tokens the weighted curvature of the loss is tiny and the leaf
  values computed from it explode.
- **How large a penalty.** The penalty acts in units of the survey weight.
  The oversampled wealthy SCF households have small weights, so a large
  penalty pulls their predictions back toward the population average and
  understates wealth at the top. On half 1, with the other settings fixed,
  mean net worth was about $0.54 million with a penalty of 1 (the setting
  used; best validation loss among the stable choices), $0.60 million with
  0.1, and $1.8 million with 0.01, where the chain became unstable; the
  truth is $0.725 million. A penalty of 10 had a slightly better
  validation loss than 1 but understated holding rates more (for example
  17.4% predicted against 18.2% observed for directly held stocks in the
  training data, measured on the data before the split-off change).
- **Early stopping on households.** scikit-learn's built-in early stopping
  holds out a random tenth of rows, which would put implicates of one
  household on both sides; the dataset's validation households are used
  instead.

In experiment B, a classifier trained on both waves can give a 2016 record
a negative token for a variable that is negative only in 2013 (for
example the primary home or a business), which the 2016 rank scale cannot
decode. Those tokens get probability zero for that record. The probability
removed was about one in 100,000 per record; it is reported in
`run_seqtrees_seed<NN>.json`.

## Runs and run times

All runs used the final datasets (after piece A's 13:19 change) and are in
`$S/comp/<dataset>/`: both methods, seeds 1, 2 and 3, five draws per
record, for `expA_clean`, `expA_noise`, `expA_recat`, `expA_unrep` and
`expB` (SCF test units, and the PUF test records in the `-puf` files).

| | Production method | Sequential trees |
|---|---|---|
| `expA_clean`, per seed | 22–25 minutes (16 cores; the eight forests take nearly all of it) | 1–2 minutes |
| Experiment A variants, per seed | under 30 seconds (the `expA_clean` forests are reused, after checking that the training inputs are identical) | about 1 minute |
| `expB`, per seed | 26–28 minutes | about 2 minutes |

Counts: experiment A trains on 13,803 half-2 rows with wealth observed
(half 2's 1,843 split-off rows are used only for ranking) and completes
17,443 half-1 rows, of which 2,063 are split-off units without true wealth.
Experiment B trains on 42,057 SCF rows (2013 and 2016) and completes 13,415
SCF test rows (1,394 of them split-off units) and 20,130 PUF test records.
In experiment B, 2,097 PUF test records had their interest and dividends of
$100 or less set to zero by the production rule, 8,822 PUF records received
a drawn age range, and between one and eight of about 67,000 completed SCF
values per seed were negative in a wave that has no negative values of that
variable (the primary home or a business), so they got the zero token
while keeping their dollars.

## First look at the results

Piece C evaluates the completed files properly (`EVAL_REPORT.md`). The
quick check below (`d_check.py`, and the same summary per seed) is only to
show what the two methods do to net worth: assets minus debts in dollars,
weighted by the population weight, over the target units whose true
wealth is known.

| Dataset | Truth: mean, median, top 1% share | Production method (3 seeds) | Sequential trees (3 seeds) |
|---|---|---|---|
| `expA_clean` (half 1) | $725k, $93k, 39% | $915k–$934k, $110k–$113k, 45% | $526k–$541k, $166k–$167k, 25%–27% |
| `expA_noise` | same | $948k–$955k, $106k–$110k, 45%–46% | $525k–$540k, $166k–$167k, 25%–27% |
| `expA_recat` | same | $902k–$917k, $110k–$114k, 44%–45% | $528k–$544k, $168k, 25%–27% |
| `expA_unrep` | same | $822k–$856k, $106k–$108k, 44%–45% | $495k–$510k, $161k–$162k, 26%–28% |
| `expB` (SCF test units) | $661k, $85k, 37% | $738k–$751k, $83k–$86k, 45% | $497k–$511k, $122k–$123k, 27% |

What this shows, without interpretation beyond the numbers:

- **The production method without its calibration step overstates net
  worth** in experiment A by about a quarter on the mean and a fifth on the
  median, and concentrates too much of it at the top (45% against 39% for
  the top 1%). Holding rates are close to the truth (for example cash 95.7%
  against 95.7%, a primary home 64.9% against 65.3%, directly held stocks
  21.5% against 20.1%, seed 1); the excess is in amounts. Two diagnostic
  runs, on seed 1 and under the first (mismatched) ranking, bear on the
  cause without settling it. Completing its own training records, the
  method reproduces their mean net worth to within 0.5%, which is the form
  of the check production's comments report (+0.4%); on half 2's held-out
  validation households it overstated. And a version without the weighted
  bootstrap, fitting each forest to the original records with the survey
  weight passed to the forest and drawing donors within the leaf in
  proportion to the weight, overstated less (half 1 mean $833k where the
  production version gave $1.06M under the same ranking). In production,
  the Stage 3 tilt then moves totals by income and age group to the
  SCF's.
- **The sequential trees compress the distribution:** the median is far
  too high ($167k against $93k), the mean too low, and the top 1% holds
  25%–27% instead of 39%. Holding rates of the rarer assets are too low
  (for example trusts 1.0% against 2.1%, seed 1). A likely reason: each
  variable is drawn with some independent noise around a conditional
  distribution that the regularisation pulls toward the population
  average, and sums of such draws bunch in the middle.
- **Seeds matter little for either method** compared with these gaps;
  piece C's training-noise yardstick will say how much.
- The `expA_unrep` variant (some capital income set to zero in half 1)
  lowers completed net worth for both methods, as expected when the
  income signals of wealth are removed.

### Third comparator: the survey-weighted forest (`drfsw`)

At the main session's request (9 October, 14:40), the production refit
was also run with one change and is written as a separate method,
`completed_drfsw_seed<NN>.parquet` (plus `-puf` files in `expB`). There is
no weighted bootstrap. Each income group's forest is fit to the original
records, with the survey weight passed to the forest, and the donor is
drawn within the leaf in proportion to the survey weight (`D_MODE=sw` in
`d_drf.R`). Everything else is as in the refit. Each run takes about a
minute. This is not production.

| Dataset | Truth: mean, median, top 1% share | Survey-weighted forest (3 seeds) |
|---|---|---|
| `expA_clean` | $725k, $93k, 39% | $749k–$771k, $107k–$110k, 40%–41% |
| `expA_noise` | same | $756k–$771k, $106k–$107k, 40%–41% |
| `expA_recat` | same | $745k–$765k, $106k–$108k, 40%–41% |
| `expA_unrep` | same | $694k–$719k, $105k–$107k, 40%–41% |
| `expB` (SCF test units) | $661k, $85k, 37% | $634k–$650k, $86k–$87k, 39%–40% |

On these net worth figures it is much closer to the truth than the
bootstrap version. Its median is still about 15% high in experiment A.

## Decisions

Settled by the main session (9 October, 14:40):

1. The production Stage 2 refit is kept as it is, and the survey-weighted
   forest has been added as a third comparator (above).
2. The sequential trees keep a ridge penalty of 1. A smaller penalty (0.1)
   moved half-1 mean net worth from $0.54M to $0.60M, at a small cost in
   validation loss (measured on the data before the split-off change).

Still open:

3. **PUF records without an age range** (44% of the PUF test records).
   The production refit and the survey-weighted forest draw an age range
   from donors in the same income group and marital status. The trees draw
   one from a classifier on all the shared variables, and the model
   completes it its own way. Results for PUF records depend on this
   choice.
