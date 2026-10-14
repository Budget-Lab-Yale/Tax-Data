---
title: "Tax-Data rebuild — imputation methods"
role: evidence
workstream: rebuild
status: current
updated: 2026-10-07
sot: research/rebuild/BRIEF.md
supersedes: []
superseded_by: null
---

# Imputation methods for the rebuild

Background paper 09. It inventories the current imputations, reviews the
methods that could replace them, and recommends a design for the imputation
layer that feeds the calibration described in
`research/rebuild/background/04_methods_first_principles.md` and
`04b_methods_first_principles_codex.md`. Statistical matching and the
conditional independence assumption are also covered, from the survey
statistics side, in `05_lit_survey_statistics.md` section 5; tabular
generators in `06_lit_ml_bayes.md` section 4. This paper does not repeat
those reviews.

## Summary

1. **About twenty imputation steps, in three families:** lookup tables and
   rules (ages, sex, earnings split, QBI, holding period); quantile forests
   with one random draw (tips, overtime, auto loan, childcare, mortgage
   share, total consumption, state and local tax rate); distributional
   forests drawing whole donors (consumption shares, wealth). Only wealth
   uses targets to choose among draws; elsewhere benchmarks are imposed by
   scaling after the draw.
2. **Two defects, found by reading the code at `ba48f97`.** The state and
   local tax module calls `predict_ranger_draw()` without the record-id
   arguments added on 13 September; it reached `main` on 6 October and a
   full run will stop there (R error confirmed in a Slurm test). In runs
   that load cached models, the consumption module's bootstrap no longer
   matches the cached forest, so category shares come from the wrong CEX
   households; default runs retrain and are unaffected.
3. **Survey weights do not reach the drawn values** in `quantregForest` and
   `ranger` (verified in their source): weights shape the trees, but leaf
   donors are picked with equal probability. This matters most for the
   SCF-trained forests. The weight-proportional bootstrap already used for
   consumption shares and wealth fixes it everywhere.
4. **Several modules condition on very little:** tip share on wages alone,
   exact age on the age band alone, childcare as half a family's expense per
   earner, holding period on the sign of the gain alone.
5. **Relationships across surveys are assumed.** Wealth (SCF) and
   consumption (CEX) are linked only through income and demographics, an
   assumption the two files cannot test. Wealth, auto loan interest and
   mortgage share all come from the SCF but in separate draws; they should
   come from one donor.
6. **Rank matching fixes a difference in scale only.** For differences in
   population or definition, harmonize income components, reweight donors
   toward the PUF's matching variables, and leave levels to calibration.
7. **A standard diagnostic battery** (section 7.4): proper scoring rules and
   interval coverage on a donor hold-out, a classifier two-sample test,
   donor–PUF overlap, tail cloning, seed stability, identity checks, and the
   effect on Tax-Simulator.
8. **Several draws per record is practical:** K clones per record at
   stratified quantiles of the record's own random number, weighted by the
   calibration. Report record-level effective sample size and clones used
   per record. Output one clone per record by balanced sampling at first;
   emitting all clones requires Tax-Simulator to key random numbers by
   (id, clone).
9. **The GPU belongs to calibration.** Imputation models are small; GPU
   forest libraries exist but do not give donor draws directly. Keep the R
   models, export candidates to a JAX calibrator, replace models only where
   diagnostics show weakness.
10. **Order of work:** fix the defects; weight the leaf draws; move in-module
    benchmarks (tips, overtime, PCE, wealth Step B, QBI factors) into the
    calibration as targets; draw auto loan and mortgage split from the
    wealth donor; adopt a common module interface (section 11).

## 1. Scope, sources and terms

I read `src/impute_variables.R`, every file in `src/imputations/`,
`src/cex.R`, `src/pce_benchmark.R`, `src/impute_nonfilers.R`,
`src/nonfiler_contract.R`, the age-band code in `src/process_puf.R`, the
Phase 3 section of `src/main.R`, the headers of `src/nonfilers/*.R`,
`src/mortality_ledger.R` and `src/forbes_splice.R`, the "Consumption
imputation" and "Non-filer population" sections of `CLAUDE.md`, and John's
project notes on the CEX, SCF wealth and state-and-local-tax imputations.
Code statements refer to commit `ba48f97` (6 October 2026). Package
behaviour was checked in package source or documentation, cited in
section 13. One Slurm job confirmed an R error message; nothing else was
run.

Terms used throughout:

- **Donor survey**: the survey on which an imputation model is trained (SCF,
  CEX, SIPP, CPS). **Recipient**: the PUF record or non-filer record that
  receives the value.
- **Matching variables**: variables measured in both files, used to predict
  the missing one.
- **Conditional distribution**: the distribution of the missing variable
  among donors with the same matching variables. An imputation should draw
  from it, not report its mean.
- **Quantile regression forest**: an ensemble of regression trees. A tree
  splits the donors into small groups ("leaves") with similar matching
  variables; the forest's prediction for a recipient is the set of donors
  that share its leaves, which approximates the conditional distribution
  (Meinshausen 2006).
- **Distributional random forest**: the same idea for several variables at
  once, with splits chosen to separate whole multivariate distributions
  (Ćevid et al. 2022). A draw is a whole donor record.
- **Two-part (hurdle) model**: one model for whether the amount is positive,
  a second for its size when positive.
- **Extrapolate**: produce values beyond the largest seen in the donor data.

## 2. Inventory of the current imputations

### 2.1 What each step produces, from what, and how

Steps are in pipeline order. P-steps run in `src/process_puf.R` before
reweighting; N is the non-filer append; steps 1–16 run in
`src/impute_variables.R` on the 2017 base (filers plus every non-filer
pool); step 17 runs in Phase 3 of `src/main.R` on the file materialized at
2022. "id-keyed" means the random number comes from `draw_by_id()` in
`src/imputations/rng.R`, so a record's draw does not depend on which other
records are present.

| Step | File | Variables | Donor data and year | Matching variables | Model | Draw |
|---|---|---|---|---|---|---|
| P1 | `process_puf.R` | `age_group` for records without the demographic file; older age band of dependent filers | SOI Table 1.6, 2015 (counts by filing status × AGI × age band) | filing status, AGI group | Residual allocation: each cell's age bands are drawn in proportion to the SOI count that records with known ages leave unfilled | Weighted pick, positional (fixed input, so deterministic) |
| P2 | `process_puf.R` | missing `dep_age_group1–3` | PUF records with exactly one dependent, 2015 | dependent type (CTC child, child at home, away, parent, other) | Lookup table | Weighted pick, positional |
| N | `impute_nonfilers.R`; builder in `src/nonfilers/`, `research/state_weights/nonfiler_pool/` | every variable of a non-filing unit | CPS ASEC tax years 2017–2023; ACS for group quarters; Mok (2017) filing probits estimated on a 2007 ASEC–IRS link; IRS Pub 5785 | the unit's own ASEC values | Rule-built tax units; probit filing model below the filing threshold, hazard above; calibrated to residual adults by age band | None: expected weights, weight × P(does not file) |
| 1 | `demographics.R` | `blind1`, `blind2` | 1040 line counts (year not stated in code) | none | Constant share | Bernoulli, id-keyed |
| 2 | `demographics.R` | `male1`, `male2` | PUF `GENDER` where present; DINA 2017 | unmarried: filer, has children, has wages | Cell probabilities chosen so reported plus imputed matches DINA; married 50/50 | Bernoulli, id-keyed; spouse opposite sex with probability 0.99 |
| 3 | `ages.R` | `age1`; `age2`; `dep_age1–3`; `dep_ctc1–3` | CPS ASEC extract `cps_00027` (all years in the file pooled) | `age1`: PUF age band only; `age2`: `age1` | Lookup tables; dependent ages uniform within band; CTC status by rule plus a 17-year-old ineligibility share estimated on the PUF | Weighted pick, id-keyed |
| 4 | `ssn.R` | `dep_ssn1–3` | ITEP estimate (1.1 million of 72 million children) | none | Constant share, same status for all dependents in a unit | Bernoulli, id-keyed |
| 5 | `earnings_split.R` | `wages1/2`; `sole_prop1/2`, `farm1/2`, `part_se1/2` | Saez tabulations, 2014 (`resources/wagesplit.csv`); PUF `E30400`/`E30500` | couple's wage percentile; PUF `EARNSPLIT` category where present | Seven-point distribution of the secondary earner's share; self-employment split deterministic from the PUF | Inverse CDF, id-keyed |
| 6 | `qbi.R` | `sstb_*`, `wagebill_*` by entity form | `resources/qbi.csv` (source not documented in the module) | entity form, SSTB status, net income | Rules with hand-set factors: SSTB probability ÷ 5; employer share + 0.2; wage bill half by count, half by profit, × Normal(1, 0.15) | Bernoulli and normal, id-keyed by form |
| 7 | `mobility.R` | `parent_rank` | Chetty et al. mobility matrix (`resources/mobility_matrix.csv`) | own income rank (module's own income definition) | Lookup | Weighted pick, id-keyed |
| 8 | `tips.R` | `tips1/2`, `tips_lh1/2` | SIPP, income years 2017–2022, wage workers, deflated to 2017 | receipt: wages, parent, married, age; share of wages: **wages only**; leisure/hospitality flag: share, wages, parent, married, age | `quantregForest`: classification for receipt and industry, regression for the share | Receipt Bernoulli at the forest probability; share = one ensemble member, id-keyed per earner |
| 9 | `overtime.R` | `ot1/2` | `resources/otdata2023.dta` (worker extract with weekly and overtime earnings and FLSA eligibility; comment says CPS/SIPP) | wage percentile, parent, married, age | `quantregForest` two-part | As tips |
| 10 | `auto_loan.R` | `auto_int_exp` | SCF 2022 plus an external auto-interest imputation (`resources/scf2022_carinterest.dta`) | wage percentile, income percentile, children, married, `age1` | `quantregForest` two-part | As tips |
| 11 | `childcare.R` | `care_exp` | CPS ASEC 2017 SPM childcare expenses, heads and spouses, halved for married | wages, married, children ≤ 4, dependents 5–24, age | `quantregForest` | One ensemble member per earner, summed; non-earners get zero |
| 12 | `mortgage.R` | `prim_mort_share` | SCF 2022 mortgagors: `MRTHEL / (MRTHEL + RESDBT)` | income percentile, children, married, `age1` | `quantregForest` | One member; only records with `int_exp > 0`, others set to 1 |
| 13 | `consumption.R`, `cex.R` | total consumption C; eight `c_*` categories | CEX interview survey 2022–2024, complete income reporters, CPI-deflated to 2017 | Stage A: `has_income`, income percentile, married, `age1`, `n_dep`, `male1`. Stage B: same plus C | Stage A: `ranger` quantile forest. Stage B: distributional random forest on a 200,000-row weight-proportional bootstrap | Stage A: one of 99 conditional quantiles at random. Stage B: random tree, uniform leaf member. Both id-keyed |
| 14 | `capital_gains.R` | `kg_lt_years_held`, `kg_lt_basis` | IRS Sales of Capital Assets panel 2013–2015 (holding-period bucket shares, basis-to-sales ratios); S&P 500 index | sign of `kg_lt` only | Bucket distribution with Weibull smoothing and an exponential tail past 20 years; basis from an OLS model of the basis-to-sales ratio | Inverse CDF, id-keyed; basis deterministic given holding period |
| 15 | `state_local_tax.R` | `dina_other_taxes_rate` | DINA 2019 tax units | fine income rank, `has_income`, married, filer, age bucket, dependents | `ranger` quantile forest on (state income + sales + property tax) / broad income, floored at 0, winsorized at P99.5 | One of 99 quantiles (call currently broken, section 2.4) |
| 16 | `placeholders.R` | prior-year earnings, employer pre-tax contributions, NOLs, mortgage detail, personal property tax and others | none | none | Zeros, or copies of `int_exp` | none |
| 17 | `wealth.R` (Phase 3, at 2022) | 20 asset and debt values, 5 bases, 7 accruals | SCF 2019 (reflated) and 2022 pooled; tax units from Moore's SCF-to-TAXSIM port; five implicates; billionaires removed when Forbes input exists | 19 features: income rank and sign flags; ranks and flags for wages, business, interest+dividends, capital gains, SS+pensions; older and younger age; married; children | Distributional random forest per income cell on a weighted bootstrap; donor probabilities exponentially tilted to SCF 2022 cell targets (8 income × 2 age buckets; category amounts and counts; net worth above the SCF top-1% and top-0.1% thresholds) | One donor per record from the tilted probabilities (top 300), id-keyed |
| 18 | `forbes_splice.R` | appended billionaire records | Forbes list | — | Weight-only splice | — |
| 19 | `mortality_ledger.R` | `q_death1/2` | SSA Trustees 2024, Chetty et al. 2016, Johnson et al. 2000 | age, sex, within-cell income rank, marital status (men) | Deterministic formula | none |

### 2.2 Adjustments after the draw, known weaknesses, and dependence on earlier steps

| Step | Adjustment after the draw | Known weaknesses (code comments, CLAUDE.md, project notes, my reading) | Conditions on earlier imputed values |
|---|---|---|---|
| P1 | none; the method itself fills SOI counts | none recorded | — |
| N | later modules overwrite `age1/2`, sex and wage split | later pools rank against 2017 cut points with their own nominal dollars (comment in `impute_nonfilers.R`) | — |
| 2 | — | DINA target is to be replaced (S14) | — |
| 3 | — | exact age ignores income and filing status; age feeds wealth, mortality and senior provisions | `age2` on `age1` |
| 5 | `EARNSPLIT` override | 2014 tabulation; TODO list in `impute_variables.R` names it | — |
| 6 | factors "calibrated to match QBI totals" by hand | runs on the filer file `puf` only, so non-filer records get missing values | — |
| 8 | counts and averages scaled to 2017 W-2 benchmarks by marital status; receipt rate × post/pre-pandemic SIPP ratio | share conditional on wages alone; benchmark applied inside the module | wages split (5), ages (3), dependents (3) |
| 9 | receipt probability rescaled to the training mean; amount capped at the training maximum `max_ot` | benchmark applied inside the module | as tips |
| 10 | capped at `max_ot`, the overtime maximum; ÷ 1.3886 to 2017 | survey weights not in leaf draw (SCF oversample); independent of the SCF wealth draw; the `max_ot` cap | `income` from step 7, dependents, `max_ot` from step 9 |
| 11 | — | a one-earner couple receives one half-family draw; non-earners get zero; trained on all adults, most with zero | wages split, ages, dependents |
| 12 | — | independent of the wealth draw's `primary_mortgage` and `other_mortgage`; ignores the PUF mortgage interest amount | `income` from step 7, dependents |
| 13 | top-tail multiplier (Y/Y_ref)^β above Y_ref (β ≈ 0.5); PCE benchmark in `project_puf.R` (category scale capped at 2×, remainder spread in proportion to C) | CEX income 22–48% below PUF at the same rank; pre-benchmark C = 58% of 2017 NIPA PCE; ≈ $200 billion missing from `c_other_nondurables`; multiplier up to 24× on one record; quantile grid never draws below the conditional 1st or above the 99th percentile; `ranger` leaf draw ignores weights, so zero-weight CEX rows are donors; PUF `n_dep` counts dependents living away and adult dependents, CEX counts household members; cache-load bootstrap defect (section 2.4) | sex (2), `age1` (3), C for Stage B |
| 14 | — | holding period unconditional on age or size of gain | — |
| 15 | floor at 0 | broken call; DINA's rates are themselves modelled; national only | `age1` |
| 17 | Step B rescales each variable in each cell to the SCF total; dependents' wealth set to zero; basis = value − unrealized gain; accruals at Z.1 rates; aged by DFA income bucket | bottom-decile ownership overstated (cash 69% PUF vs 46% SCF); top-tail donor cloning (pooling SCF 2019 addresses it); seed variation in net worth of about $4–5 trillion (April 2026 notes, before per-cell forests and pooling); `drf`'s `sample.weights` recorded as having no effect in the installed version; tilt result depends on which records are present (stated in code); Step B moves values outside donor support | ages, sex, dependents, every income component at 2022 values |
| 19 | — | — | age, sex, income rank at 2022 |

### 2.3 The chain

The order is: age bands (P1, P2) → non-filer append (N) → blindness and sex
(1–2) → ages (3) → SSN (4) → earnings split (5) → QBI (6) → parent rank (7)
→ tips (8) → overtime (9) → auto loan (10) → childcare (11) → mortgage
share (12) → consumption (13) → capital gains (14) → state and local tax
rate (15) → placeholders (16) → projection → wealth at 2022 (17) → Forbes
(18) → mortality (19).

Almost all dependence between steps runs through demographics and the wage
split. No module conditions on another module's imputed dollar amount,
except Stage B of consumption on Stage A, the tip industry flag on the tip
share, and the auto loan cap on the overtime maximum. So the joint
distribution of tips and overtime, childcare and consumption, or
consumption and wealth is whatever passes through the shared matching
variables. Section 4 discusses what that implies.

### 2.4 Defects found by reading the code

These are readings of the code, not results of a run.

**State and local tax rate: broken call.** `helpers.R` defines
`predict_ranger_draw(model, newdata, ids, stream)` and checks
`stopifnot(length(ids) == nrow(newdata))`. `state_local_tax.R` line 176
calls it with two arguments. The signature changed on 13 September (commit
`7d54d50`, S23). The state and local tax commit (`ccf5983`, 8 July) is not
an ancestor of that commit or of any first-parent merge on `main` before
`ba48f97`, so it arrived with the 6 October merge. A one-line Slurm test of
the same pattern under `R/4.4.1` returned
`argument "ids" is missing, with no default`. The last log I found that
reached this module (`main_17369215.out`) is from 8 July. Fix: pass
`puf$id` and a new stream name added to `RNG_STREAMS`.

**Consumption shares in cache-loading runs.** `consumption.R` line 48 draws
the 200,000-row bootstrap with `sample.int()` on the global random stream,
then builds `share_matrix` from it. The cached forest's leaves store row
numbers of the bootstrap it was trained on. The global stream at line 48
depends on whether `auto_loan.R` and `mortgage.R` ran their training
branches, which call `runif()` only when `estimate_models` is 1. So in a
run with `TAXDATA_ESTIMATE_MODELS=0`, the forest's row numbers index a
different bootstrap sample, and each record's shares come from an unrelated
CEX household. The default (`estimate_models = 1`, set in `configure.R`)
retrains on the same sample and is consistent. `wealth.R` avoids the
problem by calling `set.seed(100 + cell)` before its bootstrap. The `drf`
object stores its training responses in `Y.orig` (verified in the `drf`
source), so the fix is to read shares from `share_drf$Y.orig`. A check that
`all.equal(share_drf$Y.orig, share_matrix)` holds would catch the problem.
I do not know whether any published vintage was built with cached models.

**Auto loan cap.** `auto_loan.R` line 105 takes
`pmin(max_ot, …)`, where `max_ot` is the largest overtime amount in the
overtime training extract. If `max_ot` is large it has no effect, but it is
not an auto loan quantity.

### 2.5 Survey weights in the forest draws

The R documentation for `randomForest`, the engine under `quantregForest`,
says its `weights` are "used only in sampling data to grow each tree (not
used in any other calculation)". `quantregForest` then stores, for each
tree and leaf, one training response chosen by `sample.int(n)`, which is
uniform over training rows. `ranger`'s `case.weights` are "weights for
sampling of training observations", and its quantile set-up likewise
selects "one random obs per node and tree" with `sample(1:n, n)`. I read
both in the package source. So in both packages a leaf with one donor
weighted 50 and one weighted 5,000 offers each with probability one half.

Three consequences:

- For the SCF-trained forests (steps 10 and 12) the oversample of wealthy
  households is over-represented within leaves. Leaves are partly sorted by
  income rank, which limits the damage, but nothing removes it.
- In CEX Stage A, rows whose reference period falls outside the target year
  carry `WT_ANNUAL = 0` but remain in the leaves as donors.
- A second, minor point: because each (tree, leaf) stores one fixed
  response, all recipients that land in the same leaf of the same tree get
  the same value from it. The consumption Stage B and wealth code instead
  pick a fresh leaf member per recipient, which is better.

The consumption Stage B and wealth modules handle weights by drawing a
weight-proportional bootstrap before fitting, then sampling leaf members
uniformly. That reproduces the weighted distribution in expectation. The
same approach, or weight-proportional sampling of leaf members at
prediction time, should be standard for every forest module.

## 3. Conditional models

### 3.1 What tax data asks of a model

- **Exact zeros in large numbers.** Most units have no tips, overtime,
  business income or auto loan. A model must give a probability mass at
  zero, not small positive values.
- **Heavy upper tails.** A few records hold much of the total; the top
  0.1% matters for revenue. Donor surveys are thin there.
- **Mixed types.** Binary flags, counts, amounts, and share vectors that
  sum to one.
- **Identities.** Components that sum to a total, shares that sum to one,
  bases no larger than values, wages1 + wages2 = wages exactly.
- **Survey weights** that vary by a factor of hundreds (SCF).
- **Top-coding and blurring** in the PUF, and under-reporting in surveys.

### 3.2 Options

| Method | What it learns | How a value is drawn | Zeros | Tails | Several variables | Survey weights | Software, hardware |
|---|---|---|---|---|---|---|---|
| Quantile regression forest (Meinshausen 2006) | Conditional distribution as a weighted set of donor values | Pick a tree and a leaf member | Kept as point masses (draws are real donor values) | Cannot exceed the largest donor value | One at a time | Tree growth only in R packages (2.5) | R `quantregForest`, `ranger`, `grf`; CPU |
| Distributional random forest (Ćevid et al. 2022) | Joint conditional distribution of a vector | Pick a donor record | Kept | Cannot exceed donors; thin tops are cloned | Yes, whole donor vector | Recorded as not effective in the installed `drf` (project notes); bootstrap expansion works | R and Python `drf`; CPU |
| Generalized random forests (Athey, Tibshirani and Wager 2019) | Any quantity defined by local estimating equations; quantile forest is one case | As QRF | Kept | As QRF | Mostly one target | `grf::quantile_forest` has no weights argument (verified signature) | R `grf`; CPU |
| Gradient-boosted quantiles (LightGBM `quantile`, XGBoost `reg:quantileerror`) | One model per quantile level | Predict a grid, sort, interpolate, draw a uniform | Produces small positive values unless two-part | Weak beyond data | One at a time | In the loss, properly | Mature GPU training |
| Distributional boosting (NGBoost; XGBoostLSS, LightGBMLSS) | Parameters of a chosen distribution family as functions of X | Draw from the fitted family | Zero-inflated families available | Parametric tail can extrapolate, for better or worse | One at a time (some multivariate families) | In the loss | NGBoost CPU (scikit-learn); LSS on XGBoost/LightGBM, GPU |
| Two-part (hurdle) model (Cragg 1971; Duan et al. 1983) | P(y > 0 \| X) and the distribution of y given y > 0 | Bernoulli, then amount | Exact | Depends on the second part | One at a time | Any | Any of the above for each part |
| Extreme-value tail models: extremal random forests (Gnecco, Terefe and Engelke), gradient boosting for extreme quantiles (Velthoen et al.) | Generalized Pareto tail above a high threshold, with parameters depending on X | Draw from the fitted tail | — | Designed to extrapolate | One at a time | Not checked | R; CPU |
| Engression (Shen and Meinshausen) | A neural generator of the conditional distribution | Sample the generator | Needs handling | Abstract claims extrapolation under monotonicity assumptions | Yes | Not checked | R and Python (per abstract); GPU |
| Conditional normalizing flows (Papamakarios et al. 2021; Durkan et al. 2019) | An invertible neural map from a simple distribution to the conditional distribution | Sample base, transform | Needs a separate point mass | Base distribution sets tail behaviour | Yes | Weighted loss is straightforward | JAX (`flowjax`), PyTorch; GPU |
| Tabular diffusion (TabDDPM, TabSyn, TabDiff; TabCSDI for imputation); ForestFlow (tree-based) | Whole joint distribution of a table | Reverse a noising process, conditioning on observed columns | Mixed-type handling varies | No published tax evaluation | Yes | Not standard | GPU (neural); ForestFlow on CPU |
| Sequential CART synthesis (synthpop; Reiter's CART synthesis; the IRS–Urban synthetic files) | Each variable given all earlier ones, one tree each | Draw from the leaf | Kept | As QRF | Through the chain | Varies | R `synthpop`; CPU |

Notes on each, limited to what matters for Tax-Data:

**Quantile regression forests** are the model most used here.
A draw is a real donor value, so zeros, lumps (round-number reporting,
common amounts) and impossible combinations are handled by construction.
The costs: no extrapolation past the donors, one variable per forest, and
the weighting gap in section 2.5. Leaf size (`nodesize`, `min.node.size`)
controls the trade-off between fitting the conditional distribution closely
and having enough donors per leaf. Picking one ensemble member, as
`predict_qrf_draw_by_id()` does, is an exact draw from the forest's
distribution. Picking one of 99 interpolated quantiles, as
`predict_ranger_draw()` does, cuts off the bottom and top 1% of each
record's conditional distribution and invents values between donors; it
should be replaced by an ensemble-member pick.

**Distributional random forests** keep the joint structure of a donor
record, which is why both the wealth and the consumption-share modules use
them. A donor's 24 wealth variables or 8 shares arrive together, so sums
and shares hold. Two weaknesses are documented in the project notes: thin
SCF tops are cloned across many PUF records, and weights must be imposed by
bootstrap expansion.

**Generalized random forests** add little for imputation beyond honest
splitting (fitting the tree on one half of the data and filling its leaves
from the other). `grf`'s quantile forest does not take sample weights.

**Gradient-boosted quantile models** fit each quantile level separately.
XGBoost's documentation notes that quantile curves can cross (an estimated
90th percentile below the 80th); rearranging (sorting) the predicted
quantiles fixes this (Chernozhukov, Fernández-Val and Galichon 2010). They
take weights in the loss, and they train fast on GPUs. A draw is an
interpolated value, not a donor's value, so zeros need a two-part set-up.
They are a reasonable replacement for single-variable forests when GPU
training matters, which it does not here.

**Distributional boosting** assumes a distribution family (for example
zero-inflated lognormal). It gives smooth draws and a parametric tail.
The tail is useful for extrapolation and risky because it is an
assumption. NGBoost trains with a proper scoring rule (section 7).

**Two-part models** are already used informally for tips, overtime and auto
loans. They should be the default for any zero-heavy amount. The second
part can be a forest, a boosted model or a parametric family.

**Extreme-value tail models** fit a generalized Pareto distribution (the
standard limiting shape of a distribution's tail above a high threshold)
whose parameters depend on X. They are the principled replacement for the
consumption top-tail multiplier and a candidate for the wealth top. The
extremal random forest paper applies the method to U.S. wage data (per its
abstract). I found no application to tax or survey microdata.

**Engression, flows and diffusion models** are neural generators. They
handle several variables jointly and run natively on GPUs. Their
weaknesses for this problem: donor surveys are small (the SCF has about
29,000 tax-unit rows across five implicates), zeros and identities need
extra machinery, and none has a published evaluation on U.S. tax data.
`06_lit_ml_bayes.md` section 4 reviews the generators in detail.

**Sequential CART synthesis** is what the IRS–Urban synthetic PUF work and
the `synthpop` package use (Bowen et al. 2022; Nowok, Raab and Dibben
2016). Each variable is drawn from a single tree fitted on all earlier
variables. It is the same structure as Tax-Data's chain, with single trees
in place of forests.

### 3.3 Fit to the current modules

For single variables with many zeros and a donor survey of tens of
thousands of rows, a two-part model with a weighted quantile forest for
the positive part remains the best default. It needs no distributional
assumption and cannot produce impossible values. For blocks of variables
from one survey, a distributional forest with whole-donor draws is the
right tool, because identities hold automatically. The place where
something else is needed is the top tail, where donors are thin or absent:
there, an extreme-value tail model fitted on the upper donors and applied
above a threshold is the defensible way to extrapolate, and its parameters
can be targets of the calibration.

## 4. One variable at a time or jointly

**Chained imputation** draws variables in sequence, each conditional on the
matching variables and on everything imputed before it. The `mice` package
(van Buuren and Groothuis-Oudshoorn 2011) and sequential CART synthesis
work this way. **Joint block imputation** draws a set of variables together,
as the distributional forest does.

What the chain order does:

- A later variable can depend on an earlier one; the reverse dependence is
  represented only through the earlier variable's own model. If the
  conditional models are mutually inconsistent, different orders give
  different joint distributions. Liu, Gelman, Hill, Su and Kropko (2014)
  study when an iterated chain settles to a well-defined joint distribution
  (metadata verified; I did not read the paper).
- Putting the best-predicted variables first, and those with most
  structural meaning for later ones (age, earnings split) early, usually
  works best. The current order follows this.
- Within a single donor survey, a chain loses nothing if each step
  conditions on all earlier steps from the same donor. Drawing a whole
  donor record does the same thing more simply.

**Relationships across modules.** When two variables come from different
surveys, as wealth (SCF) and consumption (CEX) do, nothing in either survey
says how they relate beyond their common matching variables. The options:

1. **Accept and state the assumption.** The file then assumes that, for two
   households with the same income composition and demographics, wealth and
   consumption are unrelated. This understates the joint concentration
   Fisher, Johnson, Smeeding and Thompson report in their study of income,
   consumption and wealth together in the SCF (abstract read).
2. **Condition one on the other** using a donor that has both. Candidates:
   the PSID (wealth, income and spending); the SCF's own spending items,
   which Fisher et al. use; and asset questions in the CEX. I believe all
   three exist but did not verify their content.
3. **Calibrate to joint targets,** such as consumption by wealth group, if a
   published source exists. I did not find one in this review.
4. **Sensitivity analysis.** Impute consumption with an assumed rank
   correlation between its residual and wealth's residual given income, and
   report how outputs that depend on both (for example a consumption tax by
   wealth group) change as that correlation varies.

**Modules from the same survey should share a donor.** Auto loan interest
and mortgage share come from the SCF 2022, and wealth from the SCF 2019 and
2022, in three independent draws. The wealth vector already contains
`installment_debt`, `primary_mortgage` and `other_mortgage`. Taking auto
interest and the mortgage split from the same donor as wealth makes them
consistent by construction. This moves them to the 2022 base (step 17),
which raises the same issue for earlier years that wealth already has.

## 5. Matching variables measured differently

Two documented examples: CEX income runs 22–48% below PUF income at the
same rank (CLAUDE.md); and household surveys miss capital and business
income at the top (Meyer, Mok and Sullivan 2015 on survey accuracy in
general; `03_data_microdata.md` on coverage).

| Option | What it assumes | Right for | Wrong for |
|---|---|---|---|
| **Rank matching** (use each unit's percentile within its own file) | The survey's income is a monotone transformation of the PUF's: the order of units is right, only the scale differs | A level gap such as CEX income being uniformly lower | Different populations (filers vs. all households; survey non-coverage of the top); different income concepts; under-reporting that varies across people of the same true income |
| **Harmonize definitions** (rebuild each income concept from components present in both) | Components are measured comparably | Concept differences, e.g. the wealth module drops rent and unemployment insurance and thresholds interest and dividends at $100 | Components that do not exist in one file |
| **Reweight donors to the PUF's matching-variable distribution** (estimate how much more common each donor's X is in the PUF than in the survey, for example with a classifier, and weight donors by that ratio before fitting) | The relationship of Y to X is the same in both populations | Population differences in X, which the wealth notes identify as the main cause of the aggregate wealth gap | Differences in how Y relates to X |
| **Model the measurement error** (specify how reported income relates to true income, from a validation study such as linked survey–tax data; Bollinger et al. 2019 study earnings non-response and misreporting in the CPS) | The error model is right and transfers | Known, well-studied error (CPS earnings) | Most other cases; heavy |
| **Calibrate after imputing** (fit aggregate totals) | The model's shape is right, its level is not | Levels: PCE totals, wealth totals | Conditional shape; extensive margins |

Two points specific to Tax-Data:

- **Rank must be computed over the same population in both files.** The
  current code ranks PUF records over filers plus the 2017 non-filer pool,
  and CEX units over all CEX tax units, which roughly match. Later non-filer
  pools rank against 2017 cut points in their own year's dollars (known,
  recorded in `impute_nonfilers.R`).
- **Censored PUF variables can be matched by simulating the reporting
  rule.** The brief's approach to itemized deductions applies to matching
  as well. PUF mortgage interest is observed only for itemizers. To use it
  as a matching variable for the SCF, compute for each SCF unit whether it
  would itemize under 2015 law and what it would report, then match on the
  reported quantity. The same applies to other itemized items.

Recommended combination: harmonize components wherever both files have them,
reweight donors toward the PUF's X distribution, use rank only for the
remaining scale gap, and leave aggregate levels to the calibration.

## 6. The conditional independence assumption

Statistical matching joins a file with (X, Y) to one with (X, Z), with no
unit in both. The joint distribution of Y and Z given X is not identified
by the two files: any dependence between Y and Z given X is consistent with
both. Every model trained on one file and applied to the other assumes Y
and Z are independent given X, unless information from outside is added
(Kadane, reprinted 2001; Rässler 2002; D'Orazio, Di Zio and Scanu 2006). It
cannot be tested from the two files because no record shows Y and Z
together.

What can be done (detail in `05_lit_survey_statistics.md` section 5):

- **Add matching variables.** The assumption is weaker the more of the
  relevant variation X captures. The wealth module's move from a few
  features to 19 is this step.
- **Use an auxiliary file** that observes Y and Z together, even if small
  or old, to estimate the missing dependence or to test the assumption.
- **Bounds.** Without the assumption, the files identify only a range of
  joint distributions. Conti, Marella and Scanu (2016) estimate this range
  for complex survey data (metadata verified). Often it is wide.
- **Sensitivity analysis** as in section 4, option 4.
- **Joint targets** in the calibration, when published.

For Tax-Data, Y is the PUF-only detail (itemized deductions, specific income
lines) and Z is the survey-only variable (wealth, consumption, tips). The
assumption matters most where an output depends on both, such as wealth
taxes interacting with charitable deductions, or consumption taxes by
itemizer status.

## 7. Scoring an imputation

### 7.1 Proper scoring rules

A **scoring rule** assigns a number to a predicted distribution and the
value that actually occurred. It is **proper** if, on average, the score is
best when the predicted distribution is the true one (Gneiting and Raftery
2007). Mean squared error of a point prediction is not a test of an
imputation: it is minimized by predicting the conditional mean, which
removes all dispersion. Proper scores reward the right spread.

- **Continuous ranked probability score (CRPS).** For a predicted
  distribution F and outcome y, CRPS = E|Y − y| − ½ E|Y − Y′|, where Y and
  Y′ are independent draws from F. In words: the average distance from a
  draw to the truth, minus half the average distance between two draws. It
  is in the units of y (dollars), handles point masses at zero, and can be
  computed directly from K draws. It is twice the average of pinball losses
  over all quantile levels.
- **Pinball (quantile) loss** at level τ for predicted quantile q:
  (τ − 1{y < q})(y − q) (Koenker and Bassett 1978). It scores one quantile;
  τ = 0.9 or 0.99 focuses on the upper part.
- **Threshold-weighted CRPS** (Gneiting and Ranjan 2011) weights the score
  toward a region, such as amounts above a high threshold.
- **Energy score** for a vector: E‖Y − y‖ − ½ E‖Y − Y′‖, the multivariate
  CRPS (Gneiting and Raftery 2007). It is known to be weak at detecting
  wrong correlations between components; Scheuerer and Hamill (2015)
  propose the variogram score, which compares differences between pairs of
  components, for that purpose (metadata verified).
- **Brier score or log loss** for the probability of a positive amount.

Two practical points. In dollars, the score is dominated by the top; so
compute it twice, on dollars (with survey weights) and on a compressing
transform such as asinh(y / $1,000). And a score means little alone;
report it as a skill score, 1 − score / baseline score, against two
baselines: the unconditional donor distribution, and a hot deck within
cells of income decile × age band × marital status.

### 7.2 Calibration and coverage

A predicted distribution is **calibrated** if outcomes fall below its
predicted p-th percentile p% of the time (Gneiting, Balabdaoui and Raftery
2007). Checks:

- The **probability integral transform** (PIT): the predicted CDF evaluated
  at the true value. Its histogram should be flat. Peaks at the ends mean
  predicted distributions are too narrow; a hump in the middle means too
  wide. With point masses, use the randomized version.
- **Coverage** of central 50%, 80% and 95% intervals, overall and within 20
  to 40 cells (income group × age × marital status × children).
- **Reliability** of the positive-amount probability: observed share
  positive against predicted, in bins of predicted probability.

### 7.3 Comparing joint distributions

- **Classifier two-sample test** (Lopez-Paz and Oquab 2017): train a
  classifier to tell real held-out donor records (X, Y) from records with
  imputed Y. An accuracy or AUC near one half means the two are hard to
  tell apart. The classifier's feature importance shows where they differ.
- **Kernel and energy two-sample tests:** maximum mean discrepancy (Gretton
  et al. 2012) and energy distance (Székely and Rizzo 2013), with
  permutation p-values.

### 7.4 The standard battery

Every module would produce the same report, in four parts. The donor
hold-out is 20% of donor households, drawn by household and stratum, with
all five SCF implicates of a household kept together.

**A. Accuracy on the donor hold-out**

1. Weighted CRPS on dollars and on the asinh scale, with skill against the
   two baselines; pinball loss at τ = 0.5, 0.9, 0.99 by income group.
2. Brier score and reliability table for P(amount > 0).
3. PIT histogram and 50/80/95% coverage, overall and by cell.
4. For vector outputs: energy score and variogram score on scaled
   components, plus CRPS per component.
5. Classifier two-sample AUC, with the five most important features.

**B. Transfer to the PUF**

6. Overlap: a classifier's AUC for donor versus PUF on the matching
   variables, and the share of PUF weight in regions the donors barely
   cover (predicted PUF probability above 0.95).
7. Imputed conditional distributions on the PUF against the donor's, by
   cell (weighted quantiles, and weighted Kolmogorov–Smirnov statistics
   with ties handled correctly; the project notes record a tie bug in the
   current script).
8. Weighted totals by cell against the donor survey, and against external
   benchmarks in units of the benchmark's stated standard error, before
   calibration.

**C. Tail and stability**

9. Top 1% and 0.1% shares; largest value; number of distinct donors among
   the 1,000 highest-weighted top records; largest weight share carried by
   one donor (the cloning measure).
10. Standard deviation of key totals across 3–5 refits with different
    training seeds and different draw seeds.

**D. Integrity and stakes**

11. Count of identity violations (negative amounts, shares not summing to
    one, components above totals, wages1 + wages2 ≠ wages). Must be zero.
12. Effect on Tax-Simulator: the change in revenue and in a few
    distributional tables when the module's draws are replaced by the
    unconditional baseline. This ranks modules by how much their quality
    matters.

Parts A and B answer different questions. A tests the model on the survey
it was trained on. B tests whether applying it to the PUF is reasonable.
Neither tests the conditional independence assumption (section 6).

## 8. Several draws per record

`06_lit_ml_bayes.md` (prototype 2) and `04_methods_first_principles.md`
(section on imputed variables) both propose giving each record several
imputation draws and letting calibration weight them. This section works
out the details.

### 8.1 Relation to existing methods

- **Fractional imputation** (Kim and Fuller 2004; Kim 2011; review by Yang
  and Kim 2016) is the survey-statistics name for several imputed values
  per missing item, each with a fractional weight, in a single file. My
  understanding, from these papers' titles and my prior knowledge rather
  than a reading of them, is that the fractional weights can be calibrated.
- **Multiple imputation** (Rubin 1987) creates several complete files to
  measure uncertainty. That is a different goal; the brief wants one file.
- **Calibrated imputation** (Beaumont 2005, abstract read) changes imputed
  values, not weights, to satisfy constraints. It is the alternative when
  candidates cannot reach a target.
- The **wealth tilt** is this design already: each record has a probability
  vector over up to 300 donors, and the tilt reweights it toward targets.

### 8.2 Construction

Each record i gets K **clones**. A clone is a complete record: PUF values
identical across clones, imputed values drawn separately. Later modules
condition on the clone's own earlier imputed values, so clone k of
consumption sees clone k's ages and wage split.

**Stratified draws.** For a single variable, take clone k's value at the
record's conditional quantile u_ik = (k − 1 + v_i) / K, where v_i is the
record's id-keyed uniform for that stream. The K values then cover the
conditional distribution evenly, each standing for 1/K of it. For a
two-part variable with P(positive) = p, the same uniform maps to zero if
u < 1 − p and otherwise to the positive-part quantile at (u − (1 − p)) / p.
For whole-donor draws (distributional forests), draw K donors without
replacement from the record's donor probabilities, or keep the sparse
probability vector as the wealth module does.

Why stratify: with K independent draws, two clones can land close
together. With stratification, the most extreme clone sits on average at
the (K − 0.5)/K conditional quantile: 0.875 for K = 4, 0.94 for K = 8, 0.97
for K = 16. That is how far a target can push a record's value without
leaving the model's support.

**Random numbers.** Stratification needs only the one uniform v_i that the
current single draw already uses, so `rng.R` needs no new streams, and K = 1
with v_i reproduces today's file. Where independent clones are wanted
instead, the `sub` key can carry the clone index (for example sub = 10 × k
+ existing sub-key, which fits the 0–999 range for k ≤ 99).

**What stays invariant.** Candidates depend only on the record id and the
fitted model, so the S23 property ("adding records changes only the added
records") holds for the candidate file and can be tested there. The
calibrated clone weights depend on every record through the target
multipliers, as the wealth tilt does today (stated in `wealth.R`). That is
inherent in using targets.

### 8.3 How clones enter the calibration

With prior record weight w_i, clone prior probabilities π_ik (1/K for
stratified draws), and a_ik the clone's contributions to the targets, there
are two versions.

- **Record weight fixed.** Clone weight
  w_ik = w_i · π_ik exp(λ·a_ik) / Σ_l π_il exp(λ·a_il). Targets on imputed
  variables move only imputed values; PUF variables are untouched. This is
  exactly the wealth tilt, generalized.
- **Joint.** w_ik = w_i π_ik exp(λ·a_ik), the general entropy calibration on
  the expanded file. Record totals Σ_k w_ik also move, so a wealth target
  can shift weight between PUF records as well as between clones. PUF
  targets in the same problem hold the PUF variables in place.

The joint version matches the brief's framing. The fixed
version is simpler to prototype and isolates the imputation question. In
either, totals the module currently imposes itself (W-2 tip counts, PCE
categories, SCF cell totals) become targets with stated precision and are
removed from the module, so they are not used twice (the caution in
`04_methods_first_principles.md`).

### 8.4 How many clones

- K = 1 is today's file.
- K = 4 reaches the 0.875 quantile and suffices for moving aggregates by
  small amounts.
- K = 8 is a reasonable default for dollar amounts.
- K = 16 or more, or the donor-probability vector, for wealth and the
  consumption top tail, where targets on top shares need extreme values.

Choose by testing K ∈ {4, 8, 16} and taking the smallest K at which target
fit, the conditional-distribution checks of section 7.4 part B, and the
outputs stop changing. Memory grows with N × K × (imputed columns that
enter targets), not with all 190 columns. For illustration, 1.5 million
records × 8 clones × 50 columns in 32-bit floats is 2.4 GB.

### 8.5 Effective sample size

Two numbers, both per cell:

- **Record-level Kish effective sample size** (Σ_i W_i)² / Σ_i W_i², on
  record totals W_i = Σ_k w_ik. Computing it on clone rows would overstate
  the information, because clones of one record are not independent
  observations.
- **Within-record effective number of clones**, exp(H_i) with
  H_i = −Σ_k p_ik log p_ik and p_ik = w_ik / W_i. It runs from 1 (one clone
  carries all weight) to K. Report its mean and the share of records below
  1.5.

If the within-record number collapses in a cell, the targets want values
the imputation model considers unlikely. That is evidence against the
model or the target, and is the imputation analogue of the target
multipliers reported for the weights.

### 8.6 What Tax-Simulator receives

| Output | Totals | File size | Tax-Simulator change |
|---|---|---|---|
| Every clone with positive weight, as a row keyed by (id, clone); clones below a small share of their record's weight pruned and the rest rescaled | Exact | Up to K × rows | Key random numbers and sample membership by (id, clone) |
| One clone per record, chosen with probability w_ik / W_i using the record's id-keyed uniform | Off by sampling noise | Unchanged | None beyond reading weights |
| One clone per record, chosen by balanced sampling (the cube method of Deville and Tillé 2004, metadata verified), which selects so that chosen totals nearly match the calibrated totals | Nearly exact on balanced totals | Unchanged | None beyond reading weights |

`impute_nonfilers.R` records that Tax-Simulator binds precomputed random
numbers to records by row position (citing Tax-Simulator `run.R` lines
349–357, which I did not open). Any option that adds rows changes that
binding, so this is a joint decision with the Tax-Simulator side. A
reasonable start is balanced sampling to one clone per record, which
leaves Tax-Simulator untouched, with full clone output kept for
diagnostics.

## 9. GPU and JAX

| Model | Training on GPU | Prediction in JAX | Notes |
|---|---|---|---|
| Lookup tables and rules (steps 1–7, 14) | — | Direct | Vectorized arithmetic |
| Quantile and distributional forests (steps 8–13, 15, 17) | No mature GPU quantile or distributional forest that I know of. XGBoost trains random forests on GPU (`num_parallel_tree`, `device="cuda"`), and RAPIDS cuML has `RandomForestRegressor`; neither returns quantiles or leaf donors directly | Tree walking is possible with arrays of split variables and thresholds; I found no library | XGBoost's `pred_leaf` returns each row's leaf in each tree. Applied to donors and recipients, it gives the same-leaf donor lists needed for forest draws, with survey weights used at the leaf |
| Gradient-boosted quantiles and distributional boosting | LightGBM (`device_type` cpu, gpu, cuda) and XGBoost (`reg:quantileerror`, multiple levels since 2.0) | Tree walking as above | Weighted loss is native |
| Flows, engression, mixture density networks | Native | Native (`flowjax` provides conditional flows in JAX) | Small donor files limit them |
| Wealth tilt (step 17) | — | Straightforward | It is a calibration; it belongs in the JAX solver |

Training is not the bottleneck. The forests train on tens of thousands of
donor rows in seconds to minutes on CPU. I did not profile the pipeline,
so I cannot say which step dominates the two-hour run, but the wealth tilt's
dense donor-weight matrices are the most memory-intensive imputation step
by construction.

The realistic path for each module:

1. Keep the R models. Change each module's output from one draw to K
   candidates (or a sparse donor-probability vector for wealth), written as
   arrays.
2. The JAX calibrator reads candidates and target contributions.
3. Replace a model only where the battery shows weakness and a GPU-native
   or better-suited model scores better on the hold-out.

## 10. Recommendations for each imputation

| Step | Recommended model and draw | Evidence that would justify the switch | Risk |
|---|---|---|---|
| P1 age bands | Keep the residual method for now; later give records without ages a few candidate bands from a CPS model conditional on filing status, AGI and income sources, and let calibration fit SOI Table 1.6 | The calibration layer exists | Low |
| N non-filers | Keep; observed ASEC values are better than imputations. Pass observed age and sex through instead of redrawing them, as `nonfiler_contract.R` already flags as a future option | — | Low; changes draw counts, now harmless after S23 |
| 1, 4 blindness, SSN | Keep (low stakes); condition SSN status on income if a source exists (TODO in code) | — | Low |
| 2 sex | Keep; replace the DINA target per S14 | — | Low |
| 3 exact ages | CPS model of exact age given band, filing status, wage and Social Security receipt, income rank, dependents; whole-record draw from CPS units | Hold-out log loss better than the current band-only lookup; better wealth and mortality diagnostics by age | Low |
| 5 earnings split | CPS ASEC or SIPP two-earner model of the secondary share given couple wages, ages, children, with point masses at 0 and 1 | The PUF demographic file's three-way `EARNSPLIT` for about 120,000 records is a built-in hold-out: compare category frequencies | Low; identity holds by construction |
| 6 QBI | Replace hand-set factors with parameters fitted in calibration, if SOI publishes QBI deduction tabulations (`01_data_irs.md`); run on non-filers too | SOI totals by AGI matched without hand factors | Moderate |
| 7 parent rank | Keep | — | — |
| 8 tips | Two-part, weighted leaf sampling; condition the share on age, sex, marital status, children, not wages alone; W-2 benchmarks become targets | Hold-out CRPS improvement over the wages-only share model | Low |
| 9 overtime | Same as tips; drop the in-module rescaling | Hold-out scores; target fit | Low |
| 10 auto loan | Take from the SCF wealth donor (installment debt and car interest of the same donor) | Joint consistency of auto interest and installment debt; hold-out scores | Moderate: moves to 2022 base |
| 11 childcare | Unit-level two-part model (children under 13, earners, wages) from CPS ASEC SPM data, one draw per unit; child and dependent care credit totals as targets via simulated credit rules | Hold-out scores; credit totals | Low |
| 12 mortgage share | From the SCF wealth donor's primary and other mortgage; use the PUF mortgage interest amount as a matching variable through simulated itemization | Consistency with wealth debts | Moderate |
| 13 consumption | Fix the bootstrap defect; replace the 99-quantile draw with an ensemble-member draw; weighted leaf sampling in Stage A; replace the power-law multiplier with an extreme-value tail model fitted on the CEX upper tail, or keep β as a calibration parameter; NIPA PCE categories become targets with large stated errors (CEX and PCE concepts differ), replacing the 2× cap; align `n_dep` with the CEX household concept, as the wealth module did. Consider one joint draw of C and shares | Battery parts A–C before and after; the validated conditional checks must not worsen | Moderate: Stages A and B are validated and need re-validation |
| 14 capital gains | Condition the holding period on age and gain size if SOCA tabulations allow; otherwise keep | Data availability | Low |
| 15 state and local tax | Fix the call; ensemble-member draw | — | Low |
| 16 placeholders | Out of scope here; list them as known gaps (employer pre-tax contributions are zero) | — | — |
| 17 wealth | Keep per-cell distributional forests and the donor-probability representation; move the tilt into the global calibration with SCF/DFA targets carrying stated errors; drop Step B, which moves values outside donor support and repeats the targets; replace the top with an extreme-value tail or the Forbes splice, decided by the cloning measure | Target fit at least as good without Step B; cloning measure; seed stability | Moderate |
| 19 mortality | Keep (deterministic formula) | — | — |

## 11. Architecture for the imputation layer

### 11.1 A common module interface

Each module declares, in one place:

- **Name, version and base year** (the year at which its matching variables
  are evaluated).
- **Donor builder:** returns the donor table with weights that sum to the
  population, the survey year, the price deflator, and how implicates are
  handled.
- **Matching variables:** one function that computes X on both donor and
  recipient from a declared concept map, stating for each variable whether
  it is used as a level, a within-file rank, or a harmonized sum of
  components, and recording known concept gaps.
- **Outputs:** names and types (binary, zero-inflated amount, share
  vector, category) and the identities they must satisfy.
- **Model:** fit and predict. The cache key is a hash of donor data, code
  version, settings and seed, replacing today's name-keyed cache, which
  CLAUDE.md records as unable to detect changed training data.
- **Candidates:** given recipient X, ids and K, returns an array of
  N × K × outputs and prior probabilities π_ik, using id-keyed stratified
  draws, with survey weights applied when sampling leaf members.
- **Targets contributed** (optional): functions mapping candidates to
  target contributions, each with a stated error, for the calibration's
  target list.
- **Diagnostics:** the battery of section 7.4.

### 11.2 Order of blocks

Within a block, variables from one survey come from one donor draw.

1. **Population and demographics:** PUF, non-filer pool; age bands, exact
   ages, sex, dependents (PUF, CPS, SOI).
2. **Labour income detail:** earnings split, tips, overtime, QBI variables
   (CPS, SIPP, SOI). Conditions on block 1.
3. **Household balance sheet:** wealth, auto loan interest, mortgage split,
   bases and accruals (one SCF donor draw). Conditions on blocks 1–2 at the
   donor year.
4. **Spending:** consumption, childcare (CEX, CPS). Conditions on blocks 1–2;
   on block 3 only if an auxiliary file supports it (section 4).
5. **Tax history and rates:** state and local tax rate, capital gains
   holding period and basis, parent rank, mortality.

### 11.3 Hand-off to calibration

1. Modules produce raw model output as candidates, with no benchmarking of
   their own.
2. Benchmarks that modules impose today (W-2 tips, overtime receipt rate,
   PCE categories, SCF cell totals, QBI totals) move to the target list
   with stated precision.
3. The calibrator chooses clone weights (and record weights, in the joint
   version) and reports, per cell, record-level effective sample size,
   within-record effective clones, and each target's multiplier.
4. Output: one clone per record by balanced sampling at first; full clone
   output once Tax-Simulator keys random numbers by (id, clone).

### 11.4 Sequencing within the two weeks

1. Fix the two defects (section 2.4) and the auto loan cap.
2. Weighted leaf sampling for the `quantregForest` and `ranger` modules;
   ensemble-member draws in place of the 99-quantile grid.
3. Candidate output (K = 8, stratified) for the forest modules; the wealth
   donor-probability vector exported as a sparse matrix.
4. The diagnostic battery on three modules first: consumption, wealth,
   tips.
5. Move in-module benchmarks to the target list as the calibration
   prototype comes up.
6. Draw auto loan interest and mortgage split from the wealth donor.

Model changes beyond these (tail models, new conditioning, joint
consumption draws) should follow evidence from step 4.

## 12. What I could not verify or did not look for

- The session's web-search allowance was exhausted before this work began.
  I fetched only known URLs and used the arXiv and Crossref APIs. I did not
  search for further literature, so newer or better comparisons of
  imputation methods on economic microdata may exist.
- Content of the PSID, SCF and CEX modules for joint wealth–consumption
  information: believed, not checked.
- Reiter's CART synthesis paper (2005, *Journal of Official Statistics*):
  not found in Crossref; cited from memory, unverified. The IRS–Urban
  method description rests on the Bowen et al. metadata and on
  `06_lit_ml_bayes.md`.
- Fractional imputation papers, Liu et al. 2014, Conti et al. 2016,
  Deville and Tillé 2004: metadata only.
- The source of `resources/qbi.csv` and `resources/otdata2023.dta` is not
  documented in the modules.
- Whether any published vintage was built with
  `TAXDATA_ESTIMATE_MODELS=0`, which determines whether the consumption
  bootstrap defect reached an output.
- The claim that `drf`'s `sample.weights` has no effect comes from John's
  April 2026 notes. `helpers.R` documents the opposite. I did not test it.
- Tax-Simulator's row-position binding of random numbers: from a comment in
  `impute_nonfilers.R`; I did not open Tax-Simulator.
- The number of records in the non-filer union base; my memory example in
  section 8.4 is illustrative.
- Whether a JAX library for training or walking tree ensembles exists. I
  know of none.
- I did not profile the pipeline.

## 13. References

Labels: **Read** — I opened the document, page or source code and read the
relevant part. **Abstract** — I read the abstract (arXiv API, Crossref or
the publisher page). **Metadata** — I confirmed title, authors, venue and
year through arXiv or Crossref. **Believed** — not checked.

Forests and distributional regression

- Meinshausen, N. (2006). Quantile regression forests. *JMLR* 7(35):983–999.
  https://jmlr.org/papers/v7/meinshausen06a.html — Abstract.
- Ćevid, D., L. Michel, J. Näf, P. Bühlmann and N. Meinshausen (2022).
  Distributional random forests. *JMLR* 23(333):1–79.
  https://www.jmlr.org/papers/v23/21-0585.html — Abstract.
- Athey, S., J. Tibshirani and S. Wager (2019). Generalized random forests.
  *Annals of Statistics*. https://arxiv.org/abs/1610.01271 — Abstract (arXiv).
- Wright, M. N. and A. Ziegler (2017). ranger. *Journal of Statistical
  Software* 77(1). https://doi.org/10.18637/jss.v077.i01 — Metadata.
- `quantregForest` source, CRAN mirror:
  https://github.com/cran/quantregForest (files `R/quantregForest.R`,
  `R/predict.quantregForest.R`) — Read.
- `randomForest` reference manual, `weights` argument:
  https://cran.r-project.org/web/packages/randomForest/refman/randomForest.html — Read.
- `ranger` source and manual: https://github.com/imbs-hl/ranger (`R/ranger.R`,
  `R/predict.R`); https://cran.r-project.org/web/packages/ranger/refman/ranger.html — Read.
- `drf` package: https://cran.r-project.org/package=drf; source
  https://github.com/lorismichel/drf (`r-package/drf/R/drf.R`, `Y.orig`) — Read.
- `grf::quantile_forest` reference:
  https://grf-labs.github.io/grf/reference/quantile_forest.html — Read.
- Gnecco, N., E. M. Terefe and S. Engelke. Extremal random forests.
  https://arxiv.org/abs/2201.12865 — Abstract.
- Velthoen, J., C. Dombry, J.-J. Cai and S. Engelke. Gradient boosting for
  extreme quantile regression. https://arxiv.org/abs/2103.00808 — Abstract.
- Shen, X. and N. Meinshausen. Engression. https://arxiv.org/abs/2307.00835 — Abstract.

Boosting

- Duan, T. et al. NGBoost. https://arxiv.org/abs/1910.03225 — Abstract;
  https://github.com/stanfordmlgroup/ngboost — Read (README).
- März, A. XGBoostLSS. https://arxiv.org/abs/1907.03178 — Abstract.
- März, A. and T. Kneib. Distributional gradient boosting machines.
  https://arxiv.org/abs/2204.00778 — Abstract.
- LightGBM parameters (`objective = quantile`, `alpha`, `device_type`):
  https://lightgbm.readthedocs.io/en/latest/Parameters.html — Read.
- XGBoost parameters (`reg:quantileerror`, `quantile_alpha`, crossing note):
  https://xgboost.readthedocs.io/en/stable/parameter.html — Read.
- XGBoost random forests on GPU:
  https://xgboost.readthedocs.io/en/stable/tutorials/rf.html — Read.
- XGBoost `pred_leaf`:
  https://xgboost.readthedocs.io/en/stable/python/python_api.html — Read.
- RAPIDS cuML API (`RandomForestRegressor`):
  https://docs.rapids.ai/api/cuml/stable/api/ — Read (listing only).
- Chernozhukov, V., I. Fernández-Val and A. Galichon (2010). Quantile and
  probability curves without crossing. *Econometrica* 78(3):1093–1125.
  https://doi.org/10.3982/ECTA7880 — Metadata (authors from memory; Crossref
  record lacks them).

Two-part models

- Cragg, J. G. (1971). Some statistical models for limited dependent
  variables. *Econometrica* 39(5):829. https://doi.org/10.2307/1909582 — Metadata.
- Duan, N., W. Manning, C. Morris and J. Newhouse (1983). A comparison of
  alternative models for the demand for medical care. *JBES* 1(2):115–126.
  https://doi.org/10.1080/07350015.1983.10509330 — Metadata.
- Mullahy, J. (1986). Specification and testing of some modified count data
  models. *Journal of Econometrics* 33(3):341–365.
  https://doi.org/10.1016/0304-4076(86)90002-3 — Metadata.

Neural and generative models

- Papamakarios, G. et al. (2021). Normalizing flows for probabilistic
  modeling and inference. *JMLR* 22(57). https://arxiv.org/abs/1912.02762 — Abstract.
- Durkan, C. et al. Neural spline flows. https://arxiv.org/abs/1906.04032 — Abstract.
- `flowjax`: https://github.com/danielward27/flowjax — Read (README).
- Kotelnikov, A. et al. TabDDPM (ICML 2023). https://arxiv.org/abs/2209.15421 — Abstract.
- Zhang, H. et al. TabSyn. https://arxiv.org/abs/2310.09656 — Abstract.
- Shi, J. et al. TabDiff (ICLR 2025). https://arxiv.org/abs/2410.20626 — Abstract.
- Zheng, S. and N. Charoenphakdee. Diffusion models for missing value
  imputation in tabular data (TabCSDI). https://arxiv.org/abs/2210.17128 — Abstract.
- Jolicoeur-Martineau, A., K. Fatras and T. Kachman. ForestDiffusion /
  ForestFlow. https://arxiv.org/abs/2309.09968 — Abstract.
- PolicyEngine `microimpute`: https://github.com/PolicyEngine/microimpute — Read (README).

Imputation and synthesis

- Rubin, D. B. (1987). *Multiple Imputation for Nonresponse in Surveys*.
  Wiley. https://doi.org/10.1002/9780470316696 — Metadata.
- van Buuren, S. and K. Groothuis-Oudshoorn (2011). mice. *JSS* 45(3).
  https://doi.org/10.18637/jss.v045.i03 — Metadata.
- Liu, J., A. Gelman, J. Hill, Y.-S. Su and J. Kropko (2014). On the
  stationary distribution of iterative imputations. *Biometrika*
  101(1):155–173. https://doi.org/10.1093/biomet/ast044 — Metadata.
- Doove, L., S. van Buuren and E. Dusseldorp (2014). Recursive partitioning
  for missing data imputation in the presence of interaction effects. *CSDA*
  72:92–104. https://doi.org/10.1016/j.csda.2013.10.025 — Metadata.
- Andridge, R. and R. Little (2010). A review of hot deck imputation for
  survey non-response. *International Statistical Review* 78(1):40–64.
  https://doi.org/10.1111/j.1751-5823.2010.00103.x — Metadata.
- Little, R. (1988). Missing-data adjustments in large surveys. *JBES*
  6(3):287–296. https://doi.org/10.1080/07350015.1988.10509663 — Metadata.
- Kim, J. K. and W. Fuller (2004). Fractional hot deck imputation.
  *Biometrika* 91(3):559–578. https://doi.org/10.1093/biomet/91.3.559 — Metadata.
- Kim, J. K. (2011). Parametric fractional imputation for missing data
  analysis. *Biometrika* 98(1):119–132. https://doi.org/10.1093/biomet/asq073 — Metadata.
- Yang, S. and J. K. Kim (2016). Fractional imputation in survey sampling: a
  comparative review. *Statistical Science* 31(3).
  https://doi.org/10.1214/16-STS569 — Metadata.
- Beaumont, J.-F. (2005). Calibrated imputation in surveys under a
  quasi-model-assisted approach. *JRSS B* 67(3):445–458.
  https://doi.org/10.1111/j.1467-9868.2005.00511.x — Abstract.
- Deville, J.-C. and Y. Tillé (2004). Efficient balanced sampling: the cube
  method. *Biometrika* 91(4):893–912. https://doi.org/10.1093/biomet/91.4.893 — Metadata.
- Nowok, B., G. Raab and C. Dibben (2016). synthpop. *JSS* 74(11).
  https://doi.org/10.18637/jss.v074.i11 — Metadata.
- Drechsler, J. and J. Reiter (2011). An empirical evaluation of easily
  implemented, nonparametric methods for generating synthetic datasets.
  *CSDA* 55(12):3232–3243. https://doi.org/10.1016/j.csda.2011.06.006 — Metadata.
- Reiter, J. (2005). Using CART to generate partially synthetic public use
  microdata. *Journal of Official Statistics* 21(3). — Believed; not found
  in Crossref.
- Bowen, C. M. et al. (2022). Synthetic individual income tax data: promises
  and challenges. *National Tax Journal* 75(4):767–790.
  https://doi.org/10.1086/722094 — Metadata.
- Bowen, C. M. et al. (2022). Synthetic individual income tax data:
  methodology, utility, and privacy implications. *Privacy in Statistical
  Databases*, LNCS. https://doi.org/10.1007/978-3-031-13945-1_14 — Metadata.

Statistical matching and measurement

- Rubin, D. B. (1986). Statistical matching using file concatenation with
  adjusted weights and multiple imputations. *JBES* 4(1):87–94.
  https://doi.org/10.1080/07350015.1986.10509497 — Metadata.
- Moriarity, C. and F. Scheuren (2003). A note on Rubin's statistical
  matching using file concatenation. *JBES* 21(1):65–73.
  https://doi.org/10.1198/073500102288618766 — Metadata.
- Rässler, S. (2002). *Statistical Matching*. Springer Lecture Notes in
  Statistics. https://doi.org/10.1007/978-1-4613-0053-3 — Metadata.
- D'Orazio, M., M. Di Zio and M. Scanu (2006). *Statistical Matching:
  Theory and Practice*. Wiley. https://doi.org/10.1002/0470023554 — Metadata.
- Conti, P. L., D. Marella and M. Scanu (2016). Statistical matching analysis
  for complex survey data with applications. *JASA* 111(516):1715–1725.
  https://doi.org/10.1080/01621459.2015.1112803 — Metadata.
- Kadane, J. (1978; reprinted 2001 in *Journal of Official Statistics*).
  Some statistical problems in merging data files. — Believed; cited via
  `05_lit_survey_statistics.md`.
- Meyer, B., W. Mok and J. Sullivan (2015). Household surveys in crisis.
  *JEP* 29(4):199–226. https://doi.org/10.1257/jep.29.4.199 — Abstract.
- Bollinger, C., B. Hirsch, C. Hokayem and J. Ziliak (2019). Trouble in the
  tails? What we know about earnings nonresponse 30 years after Lillard,
  Smith, and Welch. *JPE* 127(5):2143–2185. https://doi.org/10.1086/701807 — Metadata.
- Fisher, J., D. Johnson, T. Smeeding and J. Thompson (2022; online 2021). Inequality in
  3-D: income, consumption, and wealth. *Review of Income and Wealth*
  68(1):16–42. https://doi.org/10.1111/roiw.12509 — Abstract.

Scoring and two-sample tests

- Gneiting, T. and A. Raftery (2007). Strictly proper scoring rules,
  prediction, and estimation. *JASA* 102(477):359–378.
  https://doi.org/10.1198/016214506000001437 — Metadata (content from prior
  knowledge).
- Gneiting, T., F. Balabdaoui and A. Raftery (2007). Probabilistic forecasts,
  calibration and sharpness. *JRSS B* 69(2):243–268.
  https://doi.org/10.1111/j.1467-9868.2007.00587.x — Metadata.
- Gneiting, T. and R. Ranjan (2011). Comparing density forecasts using
  threshold- and quantile-weighted scoring rules. *JBES* 29(3):411–422.
  https://doi.org/10.1198/jbes.2010.08110 — Metadata.
- Koenker, R. and G. Bassett (1978). Regression quantiles. *Econometrica*
  46(1):33. https://doi.org/10.2307/1913643 — Metadata.
- Scheuerer, M. and T. Hamill (2015). Variogram-based proper scoring rules
  for probabilistic forecasts of multivariate quantities. *Monthly Weather
  Review* 143(4):1321–1334. https://doi.org/10.1175/MWR-D-14-00269.1 — Metadata.
- Gretton, A. et al. (2012). A kernel two-sample test. *JMLR*
  13(25):723–773. https://jmlr.org/papers/v13/gretton12a.html — Abstract.
- Székely, G. and M. Rizzo (2013). Energy statistics. *JSPI*
  143(8):1249–1272. https://doi.org/10.1016/j.jspi.2013.03.018 — Metadata.
- Lopez-Paz, D. and M. Oquab (2017). Revisiting classifier two-sample tests.
  https://arxiv.org/abs/1610.06545 — Abstract.

Repository sources (commit `ba48f97`)

- `src/impute_variables.R`, `src/imputations/*.R`, `src/cex.R`,
  `src/pce_benchmark.R`, `src/process_puf.R`, `src/impute_nonfilers.R`,
  `src/nonfiler_contract.R`, `src/main.R`, `src/configure.R`,
  `src/nonfilers/*.R` (headers), `src/mortality_ledger.R` (header),
  `src/forbes_splice.R` (header), `CLAUDE.md`, and John's project notes on
  the CEX, SCF wealth and state-and-local-tax imputations — Read.
