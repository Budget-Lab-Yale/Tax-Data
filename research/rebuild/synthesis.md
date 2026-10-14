---
title: "Tax-Data rebuild — synthesis of the background research"
role: notes
workstream: rebuild
status: current
updated: 2026-10-07
sot: research/rebuild/BRIEF.md
supersedes: []
superseded_by: null
---

# Synthesis of the background research

This note pulls together the background research written overnight on 6–7
October 2026: twelve reports in `research/rebuild/background/` and seven
adversarial reviews in `research/rebuild/background/reviews/`. Every claim
below is stated in its reviewed form. Where a review corrected a report, the
correction is used, and the main corrections are listed in their own section.
The shared problem statement is `research/rebuild/BRIEF.md`.

## Summary

1. **The current reweighting distorts the top systematically.** The 2017
   linear program puts all but 114 of 207,692 records at a weight factor of
   0.34 or 1.66. Which end a record lands at is not arbitrary. Records with
   small sampling weights go to 0.34, and those are disproportionately
   high-income. All 188 targets are counts; dollar totals are matched
   afterwards by rescaling values within eight AGI bins.
2. **The replacement is well understood.** The standard method is
   calibration that keeps weights as close as possible to their starting
   values while hitting the targets. In the soft-target version, each target
   is weighted by its precision. The weights-only problem solved 1,000,000
   records and 1,000 targets in 1.3 seconds on one CPU core in a test. A
   GPU is not needed for this part.
3. **The first step can be simpler than the full framing.** Bounded
   calibration to exact or banded targets (Treasury's method) removes the
   corners. It needs no prior strength and no per-target standard errors.
   Precision weighting adds value mainly where sources conflict and in
   projection years.
4. **Two settings in the full framing are judgement calls.** How strongly
   the starting file resists the targets, and each target's standard error,
   cannot be derived from the Bayesian argument. They can be partly
   estimated from the published targets, source group by source group.
5. **Reweighting cannot change the shape of a distribution within groups of
   similar records.** The reports agree that values must also move. The
   strongest evidence-based route is to age each earner's wages by SSA's
   growth at that earner's percentile, plus growth factors by AGI bin for
   other income, estimated from SOI average amounts outside the solver.
6. **Surveys track SOI's wage growth in the middle of the distribution, but
   not at the bottom, the top, or for capital income.**
   - From 2015 to 2023, CPS and SSA mean wages grew 38.1% and 38.6%; CPS
     and SOI total wages grew 46.8% and 43.5%. The reviewer reproduced these
     figures from the raw files.
   - The CPS tracks SSA from the median to the 95th percentile, but misses
     at the 10th and 25th.
   - CPS capital income is unusable for measuring change.
   - CPS sampling error on cumulative growth is about 1–3 points, as large
     as the drift a borrowed-change method would need to detect.
   - So survey change is a weak signal: usable for composition and the
     middle of the wage distribution, with SOI and SSA governing the rest.
7. **Most of the IRS information for 2016–2023 is published and unused.**
   Every table the repository uses exists for every year through 2023. So
   do tables it does not use: income by age, percentile tables down to the
   top 0.001%, coefficients of variation, and information-return totals.
   Most are already downloaded in the shared IRS-Ind store.
8. **IRS microdata is going backwards.** SOI has withdrawn the 2012–2015
   PUFs. A synthetic 2016 PUF announced for November 2025 has not appeared.
   Whether the Budget Lab's 2015 license is affected is open.
9. **The censored-table approach has a precedent and a gap.** JCT targets
   itemizer counts each projected year by running its calculator. Every
   method can match the tables for itemizers. None can see the expenses of
   non-itemizers, which no published table reports; those must come from
   surveys or assumption.
10. **Imputation is where much of the remaining quality lies, and it has
    concrete defects.**
    - **Unweighted fits.** On any multi-core run, the quantile forests for
      tips, overtime, auto loans, childcare and mortgage share are fitted
      without their survey weights. CLAUDE.md says the opposite.
    - **Ignored weights.** The `drf` forests ignore their weights.
    - **Leaf draws.** Even where weights do reach the fit, each leaf's donor
      is drawn with equal probability.
    - **Separate SCF draws.** Three SCF-based variables are drawn from
      separate donors.
    - **Thin conditioning.** Several modules condition on very little.
    - **Bugs on `main`.** There are three, and one of them stops a full
      run.
11. **Evaluation needs to be built.** That means backtests against
    later-published SOI years, held-out tax tables, a noise floor from
    repeated seeds, a downstream panel on tax-simulator-gpu, and a scorecard
    on every run. None of this exists today.

## What the current pipeline does

The reviewers checked these points against the code and the cached
solution.

- **2017 reweighting (`src/create_2017_puf.R`, `src/reweight.R`).**
  - Weights are first scaled by cell (married × senior × AGI) to Pub 1304
    Table 1.6 counts.
  - A linear program then picks a factor between 0.34 and 1.66 for each
    record to hit 188 count targets. It minimizes the unweighted sum of the
    factors.
  - In the cached solution, 128,813 records sit at 0.34, 78,765 at 1.66
    and 114 in between.
  - Within each AGI bin and filing status, the share at 1.66 rises from 8%
    for the lowest-weight fifth of records to 71% for the highest-weight
    fifth. Low-weight records are high-income records in the PUF's design,
    so the method pushes weight away from them.
  - The four all-returns-by-filing-status rows each appear four times in
    `config/target_info/baseline.csv`.
- **Dollar totals.** These are matched after reweighting by rescaling
  values within eight AGI bins. The rebuild has to choose deliberately
  whether dollar alignment is done by values (as now) or by weights. Doing
  it by weights lets shifts in who is in the population stand in for
  growth in amounts.
- **Projection (`src/project_puf.R`).**
  - Weights grow by filing status × age group, from observed Table 1.6
    counts through 2023.
  - Each dollar variable grows by one factor for every record. Apart from
    wealth, which ages by DFA bucket, distribution shapes stay as they were
    in 2015.
  - Beyond CBO's window, the Macro-Projections vintage carries 2056 growth
    rates forward to 2099 at constant shares of GDP.
- **Top of the PUF.** `src/process_puf.R` drops the PUF's aggregate top
  records (record IDs 999996 and above). None of the reports had noticed.
- **Demographics.** Filer age bands come from the 2015 demographic file.
  That file covers 97.8% of weighted returns, including many above
  $250,000. Records without demographics are 2.2% of returns, holding 23%
  of AGI, and are filled from Table 1.6 by filing status and AGI group.

## Data

Sources: reports 01–03 and their reviews.

**IRS.**
- **Publication 1304** is published through tax year 2023 (released March
  2026). 2024 is expected in 2027.
- **Filing-season statistics** for 2024 exist by AGI class only.
- **Tables the repository uses** are published for every year 2016–2023.
  The repository uses them for 2017 targets and as growth rates.
- **Unused tables that fit the rebuild directly:**
  - Table 1.5: income sources by age.
  - Tables 1.2 and 1.3: by filing status.
  - Table 4.3: percentile groups down to the top 0.001%, with income by
    source. It is also published for 2014–2018 as "Table 3", which covers
    the base year.
  - Table 1.4CV: coefficients of variation, a measured precision for each
    target.
  - Table 2.5: the EITC.
  - The W-2 study: per-earner wages by age and sex.
  - The IRA study.
  - Publication 5385: information-return totals, which deduction rules do
    not censor.
- **Censoring after 2017.** Table 2.1 covers 15.1 million itemizers in
  2023 against 46.9 million in 2017. For those itemizers it still reports
  state and local taxes before the cap and medical expenses before the
  floor.

**Other aggregates.**
- **CBO's February 2026 revenue workbook** projects Form 1040 lines for
  2022–2036 (sheet 3). It includes return and itemizer counts, top AGI
  shares and capital-gains realizations (sheet 6). `resources/cbo_1040.csv`
  is drawn from it, but uses only eight columns, as growth rates.
- **SSA's W-2 wage statistics** are the only timely wage distribution. The
  latest confirmed bracket table is 2023.
- **JCT's JCX-45-25 Table 3** reports tax expenditures, not deduction
  dollars, and its counts are not itemizer counts, so it is not a ready
  target.
- **BEA no longer reconciles personal income with AGI.** The last
  reconciliation covers 2005. National-accounts totals serve better as
  growth signals than as level targets.
- **Some "outside" distributions are partly built from IRS data:** CBO's
  household income, Auten–Splinter, BEA's distribution and Giving USA.
  They should not be counted as independent evidence.
- **Little exists for the long run.** Beyond about 2056 there is almost no
  distributional information. CBO's share of earnings below the taxable
  maximum is flat after 2056.

**Microdata.**
- **The Budget Lab store** holds newer survey years than several
  imputations use: CPS ASEC through the 2026 survey, SIPP through 2024 data,
  CEX through 2024, SCF 2019 and 2022, and ACS through 2024.
- **Delays to come.** The 2025 SCF is promised for late 2026. The 2025 ACS
  is delayed by a Commerce order that also bans record swapping, which is
  how the CPS protects top incomes.
- **Restricted IRS routes yield tables, not microdata.** The next window
  for the IRS Joint Statistical Research Program is 2027.

## Method

Sources: reports 04, 04b, 05 and 06 and their reviews.

**What kind of problem this is.** It is an inverse problem. Many
populations are consistent with the evidence, so a rule is needed to choose
one. The standard rule is to change the starting file as little as
possible while fitting the targets. The Opus and Codex first-principles
agents reached this independently, as did the two literature reviews.

**The estimator.**
- **Distance.** With relative entropy as the measure of change, each new
  weight is the starting weight times an exponential of the record's
  contributions to the targets.
- **Size of the problem.** There is one unknown per target.
- **Uniqueness.** With all targets soft, the answer is unique even when
  targets are duplicated or nested.
- **Exact targets.** With exact targets that conflict, the solver can
  diverge while reporting success. Convergence must be checked, not
  assumed.

**What reweighting preserves.** Records that look the same on the targeted
quantities get the same adjustment. So the distribution of everything else,
given the targeted quantities, stays as it was in the prior. Count targets
by AGI × filing status leave the mix of everything else within each cell
exactly as it was in 2015. Amount targets tilt weight within a cell toward
records with more of the targeted amount. To change a relationship, you
target it. Reweighting cannot create combinations the 2015 file lacks.

**The simpler first step.**
- **Bounded calibration** (Treasury's x⁴ + x⁻⁴ − 2 penalty, logit
  calibration, or the Tax-Calculator team's bounded quadratic program) fits
  exact or banded targets and removes the corners.
- **Precision weighting** is the generalization. It is worth its tuning
  cost where targets come from conflicting sources and in projection years.
- **Recommendation.** Build the solver so it can do either, start with
  bounded calibration on the current targets, then add precision weights.

**Settings that are judgement calls.** The Bayesian argument fixes neither
the prior strength nor the direction of relative entropy. Taken literally,
it sets the prior strength at the population count, which would freeze the
file. The reviewers suggest estimating the prior strength and standard
errors partly, by fitting them to the published targets within each source
group.

**Value changes.** Weights alone make composition stand in for growth in
amounts. The reviewers' compromise:
- Estimate growth factors that vary across the distribution outside the
  solver: SSA percentile growth for wages, SOI average amounts by AGI bin
  for other income.
- Apply them to the prior, then calibrate.
- Moving values inside the optimizer makes it lose its single-optimum
  property, and is a later step.

**Several imputation draws per record.** Give each record several imputed
versions and let calibration weight them, so aggregate targets update
imputed variables. Report 09 suggests eight draws, keyed to the existing
record-id random numbers. Two cautions:
- Splitting records inflates the measured effective sample size, so report
  it at the level of original records.
- Start with wealth only, using the donor probabilities the tilt already
  computes.

**Censored targets.** With records fixed, "itemized deductions under year-t
law" is a fixed amount per record, so it is an ordinary target. Two
problems remain:
- The PUF records expenses only for itemizers, so 2015 non-itemizers'
  expenses must be imputed from surveys before any of this works.
- Running tax law on the PUF's blurred amounts is biased near thresholds.

**Shared errors.** All the methods reports treat each record's error as
independent. The errors that matter most here are shared across many
records: aging assumptions, imputation models and definitional gaps. A
precision weighting that ignores this will be overconfident.

## What others do

Sources: reports 07 and 08 and their review.

- **US scorers.**
  - JCT and Treasury use bounded calibration with the x⁴ + x⁻⁴ − 2 penalty,
    with more than 100 and 84 targets.
  - TPC, Penn Wharton, taxdata and the Tax Foundation use O'Hare's linear
    program.
  - The PSL tax-microdata file uses a quadratic program with ±0.5% bands.
  - PolicyEngine fits a soft penalty with no pull toward starting weights.
  - None sets target tolerances from measured precision.
- **PolicyEngine's failure.** The Enhanced CPS failed in mid-2026: total AGI
  reached $23.9 trillion against SOI's roughly $15.5 trillion, and
  charitable deductions about nine times SOI's. Its replacement, Populace,
  caps each weight at 5 times its start (since August 2026) and does not
  target projected incomes, keeping them as checks.
- **The agencies' advantage is data.** JCT's model rests on the
  confidential 2019 SOI sample, with information returns linked.
- **Shape changes in projection.** Realtime Inequality changes the wage
  distribution's shape using QCEW and the monthly CPS, and backtests every
  year. HMRC grows earnings in seven bands and publishes its projection
  errors: 2–4% one year ahead, 4–6% three years ahead.
- **Uniform growth is biased at both ends.** It overstates growth at the
  bottom and understates it at the top (Realtime Inequality's backtests).
- **Non-filers.** Piketty–Saez–Zucman found information returns show
  about twice as many non-filer wage earners as a CPS-based pool, each
  earning about twice as much. Their CPS pool assumed every unit with
  children and labour income files, so this is a test to run on the ASEC
  pool, not an estimate of its error.

## Imputation

Sources: report 09 and `reviews/review_09_10_11.md`.

- **Weights, three separate failures.**
  1. **Multi-threaded quantile forests drop weights.** `train_or_load_qrf()`
     and `mortgage.R` pass `nthreads = n_threads()`. With more than one
     thread, `quantregForest` 1.3-7.1 calls `parallelRandomForest()`, which
     accepts extra arguments but never uses them, so `weights` is silently
     discarded. Every multi-core run therefore fits the tips, overtime,
     auto-loan, childcare and mortgage-share forests unweighted, under R
     4.4.1 or 4.4.2.
     - The claim in CLAUDE.md that the R 4.4.2 rebuild "trains them
       weighted" is wrong, as is the comment in `tips.R`.
     - `randomForest` does have a `weights` argument from version 4.7-1;
       the claim that it lacks one is also wrong.
     - Verified in the package source.
  2. **`drf` ignores `sample.weights` entirely.** Fits with and without
     weights gave identical trees. The comment on `train_or_load_drf()` is
     wrong.
  3. **Leaf donors are drawn with equal probability.** Even in a
     single-threaded fit, where weights shape the trees, each leaf's donor
     is drawn with equal probability.

  This matters most for SCF-trained modules, where wealthy households are
  oversampled. The consumption-share and wealth modules already bootstrap
  in proportion to weight before fitting, which fixes all three failures.
  The same fix works elsewhere.
- **One SCF donor.** Wealth, auto-loan interest and mortgage share come
  from separate SCF draws, so a record can have auto interest with no
  installment debt. Draw them from one donor.
- **Thin conditioning.** The tip share depends on wages alone; exact age
  on the age band alone; childcare is half a family's expense per earner;
  the capital-gains holding period depends only on the sign of the gain.
- **Older data.** Several modules train on older data than the store
  holds: childcare on the 2017 ASEC (income year 2016, heads and spouses
  only), ages on 2015–2019, tips on SIPP through 2023.
- **Sex imputation.** The sex imputation does not hit its DINA target in
  cells where sex is partly reported. How many records this affects has
  not been measured.
- **A standard diagnostic battery** (report 09 lists twelve items) would
  replace one-off validation scripts. It includes held-out donor accuracy,
  fit on the PUF, tails, seed stability, identities and downstream effect.
- **Where the GPU belongs.** Imputation models are small. The GPU work is
  calibration. Keep the R models, export candidate draws to a JAX
  calibrator, and replace a model only where diagnostics show it is weak.

## Borrowing change from surveys

Sources: report 11 and `reviews/review_09_10_11.md`. The reviewer
reproduced every number from the raw files and found the code sound:
survey and income years align, weights are used correctly, and
not-in-universe codes are handled right. The conclusions go further than
the check supports, as noted below.

Every approach rests on one of two assumptions:
- **(a)** The 2015 relationships between PUF-only variables and the
  variables a survey shares with the PUF still hold.
- **(b)** A survey's measurement error at a given rank is stable over time,
  so its changes are right even where its levels are wrong.

Both can be tested by backtest.

The empirical check from 2015 to 2023:
- **Wages per earner, CPS against SSA.**
  - The two agree on growth in the mean, the median and the 75th, 90th and
    95th percentiles to within about 2 points.
  - They disagree at the bottom: at the 25th percentile, growth was +50.0%
    in the CPS against +57.9% in SSA. The 10th percentile also misses.
  - They disagree at the 99th percentile by about 15 points.
  - They disagree on the direction of the top 10% wage share.
- **Tax units, CPS against SOI.**
  - The CPS (built with the repository's rules) and SOI agree on growth in
    returns, wages and Social Security to within about 1–3 points.
  - The CPS "AGI" omits capital gains and taxable Social Security, so its
    match with SOI AGI is not like for like.
  - Counts above $200,000 grow far faster in the CPS.
- **The timing of the gap.** The CPS ran ahead of SSA by about 3 points in
  2016–2018, and by 4.7 points in 2019. With the pandemic-adjusted weights
  on disk, the 2019 gap falls to 3.5 points.
- **Sampling error.** CPS sampling error on cumulative mean-wage growth is
  about 1.0–1.2 points, and 2–3 points at the 90th percentile. A drift of
  that size cannot be told apart from noise.
- **Capital income.**
  - The public file's largest interest and dividend values jump at the
    2019 processing change.
  - CPS interest had already risen 78% from 2015 to 2017, against about 11%
    in SOI.
  - Survey change in capital income is not usable.

Recommendations, adjusted for the review:
1. **Age wages by SSA percentile growth.** SSA counts every W-2 earner,
   including non-filers, so a filer's percentile must be mapped onto a
   matching population. This is unresolved.
2. **Survey change targets for demographic composition.** Use them with
   precision from replicate weights. Treat them as weak evidence, given the
   sampling error.
3. **CPS shifts in wage rank** only in the middle of the distribution, not
   below the median or at the top.
4. **Driver-based record generation.** Prototype it only for the bulk of
   the distribution and for variables whose 2015 relationships look stable.
   Capital gains, dividends, interest, pass-through income, unemployment
   compensation and IRA distributions are not among them.
5. **Panel transitions** only for variables that depend on the prior year
   (the 2020–21 EITC lookback, carryforwards).

Report 11's description of Realtime Inequality's method is inaccurate in
three places. It adds a re-sorting step, calls the wage level
administrative when it averages QCEW and CPS, and applies the paper's
bottom-90% rule, which the paper uses with the monthly CPS, to the ASEC.
Read the paper rather than relying on report 11's account.

## Evaluation

Sources: report 10 and `reviews/review_09_10_11.md`.

- **Backtests.**
  - Fit only through a cutoff year (2017, 2019 or 2021) and score the
    following years against Pub 1304 tables published later.
  - Benchmark against the current method and uniform growth.
  - Use actual totals first, so the test isolates how the method
    distributes them.
- **Held-out targets.** Keep tax, credits and the top-percentile table out
  of every fit, since they are what Tax-Simulator computes. The reviewer
  found three gaps in report 10's version:
  - Tables 3.3 and 2.5 repeat targeted counts and AGI, so they are not
    fully independent.
  - Table 1.2 partly implies Table 1.6.
  - A level score is zero by construction when realized totals are fed in,
    so the backtest should score distribution, not levels.
- **Noise floor.** Rebuild with several seeds before reading any old
  against new comparison. A shifted random stream alone moved imputed
  totals by up to about 4% in September.
- **Downstream panel on tax-simulator-gpu.** Existing scenario folders
  cover a top-rate change, SALT, the CTC, capital gains, estate tax,
  mortgage interest repeal and TCJA extension. Calibrated values in both
  simulators are tied to a Tax-Data vintage. Run once with them held fixed,
  to see the data effect alone, and once re-fitted.
  - **Cost.** The model itself runs in 100–220 seconds, measured on
    7 September 2026. A Slurm job takes about 5–15 minutes, plus a 7–11
    minute data conversion per Tax-Data vintage.
  - **CBO comparison.** The line-by-line comparison with CBO's income tax
    detail is merged to `main` on GitHub (pull request 127), not only on a
    branch.
- **Known-answer simulation.** Use half the PUF as a "true" population and
  the other half, aged naively, as a deliberately wrong prior.
- **A 19-row scorecard** written by every run, with the current vintage
  alongside.

## A two-week plan

This assembles the reports' and reviewers' suggestions. It is a starting
point for discussion, not a commitment; the reviewers judged both
first-principles reports' two-week plans optimistic.

The reviewers also note that report 10's build order assumes three things
that do not yet exist: a new solver, a 2017-law itemization simulation, and
a converted candidate vintage.

**Week 1.**
1. Fix the three bugs on `main`, and make the quantile forests actually use
   their weights. Either bootstrap in proportion to weight before fitting,
   as consumption and wealth already do, or fit single-threaded. This
   changes imputed tips, overtime, auto loans, childcare and mortgage
   share, so it needs its own before-and-after comparison.
2. Remove the duplicated target rows.
3. Build the target registry: source, year, standard error (from Table
   1.4CV where it exists), source group and a held-out flag.
4. Write the calibration solver in JAX or Python. It should handle bounded
   exact targets and precision-weighted soft targets, and report
   convergence, effective sample size at the record level, standardized
   misses and multipliers.
5. Replace the 2017 linear program with it on the current count targets.
   Compare weights and Tax-Simulator outputs against today's file.
6. Measure the noise floor with five seeds.

**Week 2.**
1. Fit 2017–2023 from a common aged base, with Pub 1304 targets for each
   year, adding Tables 1.5 and 4.3.
2. Age wages by SSA percentile growth, and other income by SOI average
   amounts by AGI bin, both outside the solver.
3. Run the backtest from a 2017 cutoff, scored on 2018–2019, against the
   current method and uniform growth.
4. Run the downstream panel on tax-simulator-gpu.
5. If time allows: one censored target family (itemizer counts and amounts
   for 2018–2023) and several draws per record for wealth.

**Later.**
- Precision tuning from the data.
- Joint fitting across years.
- Driver-based record generation.
- Imputation model replacements.
- A full port of the pipeline.
- Profiling the current run, which has not been done.

## Decisions for John

1. **Start with bounded calibration or go straight to precision-weighted
   targets?** The reviewers lean towards bounded first, with a solver that
   can do both.
2. **Dollar alignment by values (as now) or by weights?** This decides
   whether the AGI-bin value rescaling survives.
3. **Should 2015 and 2016 be produced at all?** No source model covers
   them, and wealth starts in 2022.
4. **How far to trust surveys for change.** The empirical check supports
   using them for composition and for the middle of the wage distribution
   (the median to the 95th percentile), as weak evidence. It does not
   support using them at the bottom, at the very top, or for capital
   income.
5. **Which sources to hold out as checks rather than fit:** tax tables,
   projected incomes, CBO lines.
6. **Port to JAX now or later?** The calibrator is small enough to write in
   JAX alongside the R pipeline. A full port is a separate project.

## Corrections made by the reviews

Statements made in the reports, or relayed in chat on 6 October, that the
reviews found wrong or overstated:

- **The linear program's corners.** They are systematic against
  low-weight, high-income records, not arbitrary.
- **The current targets.** None of the 188 is a dollar amount.
- **The calibration check.** Standardized misses should not average one in
  mean square after fitting. They average less, because the fit uses up
  some of the noise. Report 06 had this wrong.
- **PolicyEngine.**
  - The ninefold miss was in charitable deductions, not itemized
    deductions.
  - Populace's cap is 5 times the starting weight, not 50.
  - The cited top-coding issue was fixed in September.
- **Simulating who itemizes.** It is not new: JCT targets itemizers by
  running its calculator each year.
- **"Every scorer treats targets as exact."** Overstated. Several use
  bands or soft penalties; none uses measured precision.
- **The 2015 demographic file.** It has many records above $250,000. The
  missing demographics are confined to the top 1–2% of returns.
- **The Saez–Zucman age tables.** Not a useful post-2015 target set.
- **JCX-45-25.** Tax expenditures, not deduction totals.
- **SSA's 2024 wage brackets.** Not confirmed as released.
- **The Fed's distributional accounts.** They rank households by realized
  income, not usual income. The project memory note has been corrected.
- **Canada's SPSD/M.** At version 34.0 (database year 2022), not 30.1.1.
- **Weighted forests.** The quantile forests are not trained weighted under
  R 4.4.2 on multi-core runs, and `drf` ignores its weights. Repository
  documentation states otherwise.
- **Survey change below the 90th percentile.** The CPS misses at the 10th
  and 25th percentiles, so "usable below the 90th percentile" was too
  broad.
- **The CBO comparison.** It is merged to `main` on GitHub, not only on a
  branch.

## Housekeeping

- **Bugs on `main`, confirmed by reading the code.**
  - `src/imputations/state_local_tax.R:176` calls `predict_ranger_draw()`
    without the required record-id and stream arguments, so a full run
    stops there.
  - `src/imputations/consumption.R` rebuilds its CEX bootstrap from the
    global random stream, so runs that load cached models get wrong
    category shares (default runs retrain and are unaffected).
  - `src/imputations/auto_loan.R:105` caps auto-loan interest at the
    largest overtime amount.
- **Weights dropped in the forests.**
  - Multi-threaded `quantregForest` fits drop their survey weights
    (`src/imputations/helpers.R:61-62` together with the package's
    `parallelRandomForest()`), and `drf` ignores `sample.weights`.
  - The "Model caches were trained unweighted" paragraph in CLAUDE.md, the
    comment in `tips.R` and the comment on `train_or_load_drf()` need
    correcting.
  - None of these has been edited.
- **A public API key.** A FRED API key is in old commits of the public
  Macro-Projections repository and should be revoked and reissued.
- **Tax-Data is public on GitHub,** including `research/`.
  `research/rebuild/` is untracked and has not been pushed.
- **`ipumsr` is not installed** in any R library this account can see, so
  `read_asec()` and `nonfiler_pool/01_build_units.R` cannot run as the
  environment stands.
- **The web-search limit.** The session's search allowance ran out
  partway through the night. The affected reports mark what they could not
  check.
- **Untracked files.** Nothing from this work has been committed.

## Files

| File | Topic |
|---|---|
| `research/rebuild/BRIEF.md` | Shared problem statement |
| `background/01_data_irs.md` | IRS sources |
| `background/02_data_aggregates.md` | Non-IRS aggregates and projections |
| `background/03_data_microdata.md` | Microdata for priors |
| `background/04_methods_first_principles.md` | Formulation (Opus) |
| `background/04b_methods_first_principles_codex.md` | Formulation (Codex, independent) |
| `background/05_lit_survey_statistics.md` | Survey statistics literature |
| `background/06_lit_ml_bayes.md` | Machine-learning and Bayesian literature |
| `background/07_practice_tax_models.md` | US tax models |
| `background/08_practice_other_fields.md` | Other fields and countries |
| `background/09_imputation_methods.md` | Imputation |
| `background/10_evaluation_design.md` | Evaluation |
| `background/11_borrowing_change_over_time.md` | Borrowing change from surveys |
| `background/reviews/*.md` | Seven adversarial reviews, covering every report |

## Revision history

- 2026-10-07: first version.
