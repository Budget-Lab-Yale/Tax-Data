---
title: "How US tax microsimulation models build and age their data"
role: evidence
workstream: rebuild
status: current
updated: 2026-10-07
sot: research/rebuild/BRIEF.md
supersedes: []
superseded_by: null
---

# How US tax microsimulation models build and age their data

## Summary

1. **Every documented scorer treats targets as exact constraints, and none
   weights a target by its precision.** They differ in how they measure
   distance from the starting weights. JCT and Treasury's Office of Tax
   Analysis (OTA) minimize a smooth, symmetric penalty in each record's
   weight ratio x, namely x⁴ + x⁻⁴ − 2, with about 100 and 84 targets a
   year. The Tax Policy Center (TPC), Penn Wharton (PWBM), PSL's taxdata
   and the Tax Foundation use John O'Hare's linear program, which minimizes
   the sum of absolute percentage weight changes inside a cap. O'Hare's own
   memo says most records end up unchanged and the rest move to the cap.
   The current Tax-Data uses a related linear program that minimizes the
   sum of the weight factors. When exact targets conflict, OTA and TPC drop
   or edit targets by hand. The statistical name for the brief's framing is
   "penalized" or "ridge" calibration. The nearest working code is TMD's
   2026 quadratic program with slack and Populace's chi-square option, and
   both still give every target the same tolerance or a hand-set loss
   weight.
2. **PolicyEngine's Enhanced CPS fitted weights with no term for distance
   from the starting weights, and it failed in production.** Between 19 May
   and 12 June 2026, new non-filer targets led its optimizer to put weight
   on "a handful of synthetic very-high-income records". Total AGI reached
   $23.9 trillion against an SOI level of about $15.5 trillion, and
   itemized deductions reached about nine times SOI. PolicyEngine retired
   the Enhanced CPS for a rebuild, Populace. Populace weights each target
   by the square root of its value, caps each record's weight at 50 times
   its start, and offers a chi-square pull toward the starting weights.
3. **PolicyEngine reverses the usual design.** The base file is the CPS
   ASEC, and the 2015 PUF serves only as training data for imputing tax
   variables. In the legacy pipeline the imputation used seven demographic
   predictors and no income variable. PolicyEngine's targets are computed
   by running the tax calculator on every record. For itemized deductions it
   targets JCT tax expenditures, using each record's change in tax when the
   deduction is repealed. That is a working example of matching a
   law-censored statistic by simulating it.
4. **The agencies' advantage is data, not method.** JCT's 2023 model is
   built on the confidential 2019 SOI sample (360,033 records), with 19
   kinds of information return linked by Social Security number. Its January
   2025 slides still describe the 2019 base. OTA documents a 2016 base and
   uses a 2019 base for the FY2024 budget. Both build non-filers from
   information returns and set the non-filer count as population (plus
   deaths) minus people on returns. None of the documents I found describes
   using SOI's published totals for the years between the base year and the
   present.
5. **No documented method fits post-2017 SOI itemized-deduction tables by
   simulating who itemizes.** The documented approaches are these. JCT's
   2023 model ages its 2017-based file to 2019 and matches it to actual 2019
   returns to impute deductions for non-itemizers. OTA uses an EM imputation
   for non-itemizers, calibrated to the American Housing Survey, Giving USA
   and Census government-finance totals. PolicyEngine targets JCT tax
   expenditures. PolicyEngine and TMD grow itemized deductions at an assumed
   2% a year, and TMD grows state and local taxes at 4.7%. TPC notes it
   cannot match low-AGI itemizer counts because its model itemizes only
   when doing so lowers tax.
6. **Projection follows one of three patterns.**
   - Re-solve the weights each year against projected targets. JCT, OTA,
     TPC and taxdata do this. JCT also "chains" each year to the year
     before.
   - Grow weights by demographics only and scale dollar amounts by
     aggregate factors. CBO, PWBM, TMD, Populace and the current Tax-Data do
     this.
   - Reweight to long-run SSA Trustees targets. PolicyEngine does this for
     2025–2100, with experimental "donor" records added for household types
     the base file lacks.

   Populace deliberately does not target projected income or program totals.
   Its reason is that a quantity the weights are forced to match "can no
   longer be checked."
7. **Public tax microdata is in transition.** On 22 May 2026 the IRS page
   said the 2012–2015 PUFs "are no longer available" and that a Synthetic
   Public Use File "will be available in the next few months". Every model
   outside government still runs on an old PUF:
   - TPC is moving from 2006 to 2015.
   - PWBM's public page says 2012.
   - The Tax Foundation and taxdata use 2011.
   - TMD, PolicyEngine and Tax-Data use 2015.
8. **Most outsiders import non-filers from the CPS.** TPC, taxdata, PWBM and
   CBO run a constrained statistical match: each file is sorted on predicted
   income within demographic cells, records are paired with weights split,
   and the CPS units left over become the non-filers. TMD and PolicyEngine
   append CPS units classed as non-filers by filing rules. ITEP uses the ACS.
9. **Little validation is published.** The best design I found is OTA's
   1989 backtest (Paper 62). It aged a 1981 file to 1983 using actual 1983
   SOI totals as targets, then scored how far each item's distribution
   across AGI classes landed from the truth. It found that adding exact
   targets beyond a point gives non-convergence rather than accuracy. Also
   published:
   - ITEP reports 97% of 3,264 state-by-income cells within 2% of IRS
     figures.
   - TMD tests against CBO at four horizons.
   - A July 2026 CRS report compares Tax-Simulator with Tax-Calculator.
10. **The Budget Lab's public description of Tax-Data is out of date.** The
    April 2024 methodology page and the four-line GitHub README describe the
    old method: non-filers from Piketty–Saez–Zucman, and no reweighting after
    2017. The repository's `research/` tree and decisions log are already
    public on GitHub.

## How this was researched

On 6–7 October 2026 I read the code of PolicyEngine's repositories myself,
from fresh clones:

- `policyengine-us-data` at commit `42ed5d4`, 2 July 2026. The repository
  is now archived.
- `populace` at `581b569`.
- `ledger` at `0f2cd96`.
- `microcalibrate` at `93e9c2a`.
- `microimpute` at `de68506`.

Four helper agents covered the other models. They opened the documents and
code cited below and reported page and section numbers.

- **"Verified"** means that I or a helper opened the source and read the
  passage.
- **"Not verified"** means a belief, a search snippet, or a citation taken
  from another document.
- **Re-checked by me:** the IRS PUF withdrawal notice, the PolicyEngine
  advisory, the CRS report and the TMD reweighting code.

Two limits on coverage:

- jct.gov, cbo.gov and taxpolicycenter.org block automated fetches. Their
  documents were read as direct PDF downloads, Internet Archive copies, or
  through a text-extraction service.
- The helpers' web-search budgets ran out partway. The gaps that leaves are
  listed in the last section.

## PolicyEngine US

PolicyEngine publishes an open-source tax-benefit calculator
(`policyengine-us`) and the microdata it runs on. The data work has had
three phases:

- The **Enhanced CPS**, from 2024 to mid-2026.
- A **geography-specific extension** of it, 2025–2026.
- Since June 2026, **Populace**, whose code namespace is "microcosm". It
  comes with a separate fact store, **Ledger**, which calls itself
  "Chronicle" in its README.

Sources (all verified):
- https://github.com/PolicyEngine/policyengine-us-data (README;
  `docs/methodology.md`; `docs/long_term_projections.md`; `paper/`;
  `datasets/cps/enhanced_cps.py`; `datasets/cps/extended_cps.py`;
  `calibration/puf_impute.py`; `datasets/puf/puf.py`;
  `datasets/puf/uprate_puf.py`; `utils/loss.py`; `utils/soi.py`;
  `utils/uprating.py`; `datasets/cps/long_term/README.md`)
- https://github.com/PolicyEngine/populace (`DESIGN.md`;
  `docs/static-aging.md`; `docs/calibration-l2-basis.md`;
  `docs/us-fact-to-target.md`;
  `packages/microcosm-calibrate/src/microcosm/calibrate/solve.py`;
  `packages/microcosm-build/src/microcosm/build/us/spec/imputation.yaml`)
- https://github.com/PolicyEngine/ledger (README)
- https://github.com/PolicyEngine/microcalibrate and
  https://github.com/PolicyEngine/microimpute (READMEs)
- Advisory, 19 June 2026:
  https://github.com/PolicyEngine/policyengine.py/discussions/429

### Base data

The base file is the CPS ASEC, because the CPS has households, states,
non-filers and benefits. The "2024" dataset is built from the March 2025
ASEC, which covers income year 2024 (`census_cps.py` sets the file year to
the dataset year plus one).

Non-filers need no separate construction, because they are already CPS
respondents. The calculator decides who files from the filing requirement.
Some units file without being required to. For those, a random draw uses a
table of voluntary-filing rates by number of children, wage band and age of
head, and units that claim the EITC are excluded (`cps.py`).

### PUF processing and imputation

- **The 2015 PUF and its top records.** The four aggregate records are
  split into about 120 synthetic records. An optional newer path rebuilds
  the $100 million-plus tail instead. It starts from the Forbes 400, matches
  those people to top-tail SCF donors, and then to top PUF records.
- **PUF demographics.** Records without the demographic file get imputed
  ages, gender, dependents' ages and earnings split. The model is a
  quantile regression forest trained on 10,000 records that have the file.
  A quantile regression forest is a random forest that estimates the whole
  conditional distribution, so values can be drawn from it.
  - Predictors: wages, filing status, whether the filer is someone's
    dependent, EITC children, and exemptions.
- **Aging 2015 to 2021.** Each item grows by its SOI aggregate growth
  divided by growth in returns.
  - Positive and negative parts of business income, capital gains and
    partnership income get separate factors.
  - About forty other items grow with AGI per return.
  - Weights grow with the count of returns.
  - Itemized deductions (medical, state income tax, real estate tax,
    interest paid, charity) are the exception. They grow at a flat 2% a
    year (`ITMDED_GROW_RATE = 0.02`). The code gives no reason. My
    inference is that post-2017 SOI totals fell because fewer people
    itemized, so their growth would understate growth in the underlying
    expenses.
- **Aging 2021 to 2024.** The PUF then moves to 2024 with PolicyEngine's
  per-capita uprating indices, which are CBO, SOI, CPI and SSA series
  stored as model parameters.
- **The doubled CPS.** Every CPS household appears twice.
  - Copy one keeps CPS values and receives PUF values only for variables
    the CPS lacks: itemized deductions, capital gains, dividend types and
    credits.
  - Copy two has its CPS incomes replaced by PUF-imputed values, and
    starts with zero weight.
- **The imputation model.** A sequential quantile regression forest:
  each variable is conditioned on the predictors and on every variable
  imputed before it.
  - Training data: a stratified sample of about 20,000 PUF records, with
    every record above the 99.5th AGI percentile kept.
  - Legacy predictors: seven person-level demographics. These are age, sex,
    joint filing, number of dependents, and head, spouse or dependent role
    (`DEMOGRAPHIC_PREDICTORS`). No income variable is a predictor.
  - The paper draft counts 72 imputed variables.
  - Populace's PUF imputation adds a within-file income rank and an
    earnings indicator to the predictors (`imputation.yaml`).
- **Other surveys.** Separate forests impute:
  - rent and real estate taxes from the ACS, with state as a predictor;
  - tips, liquid assets and vehicles from the SIPP;
  - net worth and its components from the SCF.

  Assets measured in both surveys take the SIPP or the SCF prediction by a
  household-level coin flip. SCF components are rescaled to an imputed SCF
  net-worth total.

### Reweighting the national Enhanced CPS

- **What is chosen.** The log of each household's weight. Working in logs
  keeps weights positive.
- **Starting weights.** CPS weights scaled by 0.95. The zero-weight PUF
  copies share the other 5% equally. This was added after a bug "starved"
  the copies.
- **The loss.** The mean squared relative error,
  ((estimate − target + 1)/(target + 1))².
  - National and state targets are each normalized to equal total weight.
  - Multipliers are hand-set: 1,000 on BEA wage and some national-accounts
    targets, and 100,000 on household counts.
- **No term for distance from the starting weights.** Closeness to the
  start comes only from beginning there and stopping after 1,000 Adam
  steps at learning rate 0.2. Adam is a common form of gradient descent
  that scales each step by recent gradient size.
  - A per-household "hard concrete" gate exists in the code. It is a smooth
    stand-in for switching a record off. Its penalty is set to zero.
  - The 2024 paper draft describes an earlier version that used "dropout":
    each step, 5% of weights were replaced by the mean of the others.
- **How targets are computed.** Each target is computed by running the
  tax-benefit calculator on every record and tabulating the result the way
  the source does (`build_loss_matrix`).
  - SOI rows (AGI, returns and income items by AGI band and filing status,
    mostly taxable returns) use the calculator's own filing decision and
    each year's law.
  - Other targets: Census single-year ages; CBO income and program totals;
    SNAP, SSI, Medicaid and ACA; EITC and CTC by AGI and children; state
    AGI and BEA wages; health spending by age; retirement contributions.
- **How many targets.** The count grew from 570 (November 2024 NTA slides,
  `presentations/nta_2024_11/main.tex`) to "over 7,000" (paper draft). The
  current documentation says "~3,000 in the legacy national calibration".
- **Targets for years without SOI data.** The latest SOI year not after the
  target year is grown with the uprating factors (`get_soi`).
- **Law-dependent targets.**
  - JCT tax-expenditure estimates for SALT, medical, charitable, mortgage
    interest and QBI. The code comment cites JCX-48-24; I did not check the
    figures. Each household contributes its change in income tax when the
    deduction is repealed.
  - Mortgage interest, targeted as the deductible amount on taxable returns.
- **Gates after fitting.**
  - Weighted households within 2% of the source total.
  - PUF copies hold at least 5% of the weight.
  - Person poverty rate between 5% and 25%.

### The June 2026 failure

PolicyEngine's 19 June 2026 advisory (discussion #429, re-checked by me)
states the following:

- **What changed.** New non-filer income targets were added "under a single
  flat-weighted loss".
- **What the optimizer did.** It resolved the conflict "by over-weighting a
  handful of synthetic very-high-income records".
- **How large the distortion was.**
  - Total AGI went from about $14 trillion to $23.9 trillion. SOI's level
    is about $15.5 trillion.
  - The $100 million-plus band went from $0.4 trillion to $7.5 trillion.
  - Itemized deductions reached about nine times SOI.
- **What it affected.** Reforms that cap or repeal deductions "scored far too
  high". The affected period was 19 May to 12 June 2026 for policyengine.org,
  and to 19 June for direct users.
- **The remedy.** Replace the Enhanced CPS with Populace, which weights each
  target's loss by the square root of its value and caps each record's
  weight at 50 times its starting value.

### Geography-specific calibration

This is the 2025–2026 extension, inside the same repository.

- **Shrinking the file.** The doubled CPS (about 400,000 person records) is
  cut to about 12,000 households by stratified sampling. Every household
  above the 99.5th AGI percentile is kept.
- **Copies by geography.** Each household is copied 430 times, and each
  copy gets a random census block drawn in proportion to population. Taxes
  and benefits are recomputed under each copy's state law, with benefit
  take-up redrawn.
- **The fit.** One weight per copy, about 5.2 million in all.
  - Targets: about 13,000 national, state and congressional-district
    targets, chosen from about 37,000 in a SQLite database.
  - Loss: the same squared relative error, plus an L0 penalty, which is a
    penalty on the number of nonzero weights.
  - Solver: Adam on GPUs, for 1,000 to 4,000 epochs.

### Projection

- **Budget window.** The legacy code can refit weights for each year
  against targets grown from the latest SOI year. Dollar inputs grow with
  per-capita indices through 2034.
- **Long run, 2025–2100** (`datasets/cps/long_term/README.md`). The 2024
  file is uprated each year and reweighted to:
  - SSA Trustees single-year ages, 0–84 and 85+;
  - total OASDI benefits;
  - taxable payroll;
  - revenue from taxing benefits.

  The recommended method is "positive entropy calibration". It finds the
  weights closest to the base weights, in the Kullback–Leibler sense, that
  meet the constraints exactly; weights stay positive. Raking and
  generalized regression (GREG) remain as options. GREG can produce
  negative weights far out.
- **Records the base file lacks.** For late years an experimental option
  adds tagged copies of real 2024 tax units that are nearest to the
  household types the base file lacks.
  - The README says simple nearest-neighbour donors "did not materially
    improve" the 2100 fit.
  - Composite donors hit 2100 targets exactly only under chosen settings.
- **Long-run tax law.** Core thresholds are assumed to be wage-indexed after
  2034, to approximate the Trustees.
- **Metadata.** Each year's file records its targets and assumptions.
- **Populace's static aging** (`docs/static-aging.md`).
  - **Weights.** For each year separately, weights are reweighted to SSA
    age-and-sex counts only, with each weight capped at five times its base
    value.
  - **Dollar columns.** Each is scaled so its weighted total grows exactly
    with its national series, with positive and negative parts scaled
    separately.
  - **Why program and income totals are not targets.** The document gives
    the reason in these words: "a weight vector forced to match one can no
    longer be wrong about it, so it can no longer be checked". Those totals
    are compared afterwards instead.

### Populace and Ledger

- **Records versus weights.** Each record carries its origin (CPS
  pass-through, synthetic, verbatim tail). The rule is "Calibration owns
  representation; generation owns support". In plain terms, the record set
  must already contain the needed kinds of people, and reweighting decides
  only how many of each.
- **The pool.** The CPS ASEC and the ACS, stacked at household level. Gaps
  are filled across the two surveys under a declared donor plan. Then one
  imputation pass from the 2015 PUF (uprated to 2024) runs over both.
- **Facts are kept apart from targets.** Ledger stores what each source
  published, with provenance. It does not age, reconcile or select.
  Publishers' projections (CBO, SSA Trustees, JCT and TPC scores) are stored
  as facts flagged as projections. Populace decides which facts bind, and
  to which variable and universe.
- **A support check before a target is switched on** (`us-fact-to-target.md`).
  - A target needing about a threefold stretch of the weight on records
    that carry the item was judged bindable.
  - The Keogh deduction needed 34 times ($0.92 billion modeled against
    $30.13 billion in SOI). It was put in an exclusion register instead.
- **Survey-measured taxes and benefits are never targets.** This includes
  anything derived from them, such as poverty rates. They are held out as
  checks.
- **The solver** (`solve.py`).
  - Loss: weighted mean absolute percentage error. Each target's miss is
    capped at 1,000% so that one target cannot dominate.
  - Method: Adam on log-weights.
  - Options: L0 or L1 sparsity penalties; a hard cap on each record's
    weight ratio; free or conserved total weight.
  - **The pull toward the starting weights.** The default penalty,
    mean((w/d)²), does not anchor to the starting weights d. Without
    targets it pushes weights toward d², that is, toward the square of the
    starting weights. A newer option uses the chi-square distance from d.
    That is GREG's distance, and the document calls it "penalized ('ridge')
    calibration, the soft form of GREG".
  - **Checks against an exact solver.** PolicyEngine compared its solver
    with an exact convex solver (CLARABEL). On some small problems where all
    targets push total weight the same way, the default method stalls
    measurably short of the true optimum.
- **Charter versus code.** The design charter says calibration should be
  "uncertainty-weighted evidence combination, not exact-hit". It also says
  that standard errors of cells from one survey are correlated, which
  should be allowed for. I found no code that sets a target's weight from
  its standard error. The weighting rule actually documented is the
  advisory's square root of the target's value.

### microcalibrate and microimpute

- **microcalibrate** wraps the gradient-based reweighting: PyTorch,
  optional L0 gates, dropout, hyperparameter search and holdout checks.
  Populace's calibration package succeeds it.
- **microimpute** puts five imputation methods behind one interface:
  - statistical matching;
  - linear regression;
  - quantile regression;
  - quantile regression forests;
  - mixture density networks.

  It fits with survey weights, and an "autoimpute" routine picks a method by
  cross-validated quantile loss. Its paper uses SCF-to-CPS net worth as the
  example.

### Validation

- **Comparisons with the CPS and the PUF.** The NTA 2024 slides report
  that the Enhanced CPS beats the CPS on 63% of targets and the PUF on 71%.
- **The paper draft (Woodruff and Ghenis).** The repository's
  `paper/response_to_reviewers.md` names the reviewers as Jon Bakija, Nora
  Lustig and Gijs Dekkers. The draft quotes statistics that I could not
  trace to scripts in the repository:
  - "QRF reduces mean absolute error by 34%";
  - an effective sample size of 68%;
  - a table of tax expenditures against JCT.

  The response to reviewers says placeholders were added "for metrics
  requiring full dataset generation". I treat the draft's numbers as
  unverified.
- **Populace** uses release gates (checks a build must pass before it is
  published) and held-out comparisons.

## PSL taxdata and its successor, TMD

Sources (verified in code, cloned 6 October 2026):
- https://github.com/PSLmodels/taxdata (commit `608bf99`)
- https://github.com/PSLmodels/tax-microdata-benchmarking (commit `4b2a4f9`;
  README calls it "tax-microdata")
- https://github.com/PSLmodels/Tax-Calculator (commit `54b782d`)
- O'Hare, "Extrapolation Methodology", 2009 memo:
  https://github.com/PSLmodels/taxdata/blob/master/docs/book/content/methods/Stage1_Stage2_Methodology.pdf

### taxdata `puf.csv`

- **Base data.** The 2011 PUF matched to the March 2016 CPS ASEC
  (`createpuf.py`). The file has 252,868 records including non-filers
  (`tests/conftest.py`).
- **Preparation.**
  - The four aggregate records are dropped.
  - Weights are multiplied by 1.03, with no reason given.
  - "Aged" means having Social Security income, because the 2011 PUF lacks
    age.
- **The match** (`taxdata/matching/statmatch.py`). A constrained
  predictive-mean match, with the PUF as host and CPS filers as donors.
  - Cells are formed from dependent status, filing status, aged,
    dependents and household size.
  - Within each cell, CPS weights are scaled to the PUF total.
  - A weighted regression of PUF taxable income on income items shared by
    both files gives a predicted value for each record in both files.
  - Records are paired in order of predicted value, splitting weights.
  - PUF values win wherever a variable exists in both files.
  - The memo's stated assumption is that PUF-only and CPS-only variables
    are independent given the shared ones.
- **Non-filers.** CPS units flagged as non-filers are appended unchanged.
- **Imputations.**
  - The head–spouse earnings split comes from matched CPS shares.
  - Pension deferrals come from W-2 aggregates in 128 cells.
  - An itemized-deduction imputation for non-itemizers, calibrated to
    JCX-75-15 Table 2, exists in the code but is not called.

### taxdata `cps.csv`

- **Base data.** March ASEC 2013–2015, pooled.
- **Tax units.** Built by taxdata's own rules.
- **Benefits.** From Urban's C-TAM transfer model.
- **Tax variables.** Imputed by logit and log-linear models with hand-set
  multipliers. Income is scaled to SOI 2014 state totals.
- **Purpose.** This is Tax-Calculator's bundled public file. Its tests check
  tax logic, not data accuracy.

### Aging in taxdata: stages 1–3

- **Stage 1: growth factors.** SOI aggregates to 2017, carried forward with
  CBO growth rates and divided by population growth, give per-person
  factors through 2036 (`growfactors.csv`).
- **Stage 2: a linear program** (`puf_stage2/solver.jl`). Each record's
  weight is multiplied by (1 + r − s), with r and s nonnegative and
  bounded.
  - Objective: minimize the sum of r + s over records.
  - Bounds: 0.30–0.40 for 2012–2016, 0.45 for 2017–2032, 0.50 thereafter.
  - Targets: 27 exact equalities. They cover returns by filing status,
    returns with Social Security, dependent exemptions, ten income totals,
    and wages in 12 AGI bins.
  - Target values: actual SOI through 2017, then CBO-grown.
  - Solver: Tulip (earlier Cbc, and before that CLP). Each year is solved
    separately; the README says the run takes "five or more hours".
- **Stage 3: interest.** Taxable interest is rescaled by AGI bin to SOI
  shares, with 2014 shares held for later years.
- **In Tax-Calculator.** Each simulated year it grows dollar items by the
  year's factors, takes that year's weight column (`WT2011`…`WT2036`), and
  applies the stage-3 ratios.

### TMD (tax-microdata-benchmarking)

- **History.**
  - Started in February 2024 by Nikhil Woodruff of PolicyEngine.
  - Phase 1 put PolicyEngine's 2022 Enhanced CPS into Tax-Calculator
    format.
  - From June 2024 it was a 2021-based file built from the 2015 PUF.
  - Since 2026 the base is 2022 (`TAXYEAR = 2022`). Tax-Calculator 6.5.0
    (29 March 2026) moved to it.
  - Funding: not verified.
- **Construction.**
  - The 2015 PUF is aged to 2022 by SOI growth per return. State and local
    taxes grow 4.7% a year and other itemized deductions 2% a year
    (`tmd/imputation_assumptions.py`).
  - There is no PUF–CPS match. Non-filers are CPS units judged non-filers
    under approximate Census filing rules, appended to the file.
  - Overtime and tips come from the 2024 SIPP, and auto-loan interest from
    the 2023 CEX.
  - The file has 215,406 records.
- **Reweighting history.** Each step is verified in git history; I
  re-checked the current code.
  - 2024: Adam, starting from multipliers of 1 clamped to [0.1, 10]. The
    loss is squared relative error. The penalty on weight change existed
    but was set to 0.
  - February 2026: L-BFGS with a small L2 penalty on weight change.
  - Since 28 February 2026 (`tmd/utils/reweight.py`): a quadratic program
    solved by Clarabel.
    - It minimizes Σ(x − 1)², the squared change in each multiplier, plus
      10⁶ × the squared slack.
    - Each target must be met within ±0.5%, or the slack is used.
    - Multipliers are bounded to [0.1, 10].
    - There are 550 SOI 2022 targets (AGI and returns by 19 AGI bins and 5
      filing statuses, nine income items by bin, and totals).
    - Every target has the same tolerance.
- **Later years.** No reweighting. Weights grow with CBO's projected return
  count, then with population; dollar items grow with taxdata-style
  factors through 2075. A comment in the code notes that population-only
  growth had left revenue 5.1% below CBO by 2035.
- **Area weights.**
  - Method: a quadratic program per state (about 175 targets) and per
    congressional district (about 107), within ±0.5%.
  - What the team reports in `AREA_WEIGHTING_LESSONS.md`:
    - The size of the penalty on weight change did not matter; only the
      cap on multipliers did.
    - The solver's shadow prices, the cost of each constraint, showed
      $1 million-plus filing-status counts costing six to eight orders of
      magnitude more than the other targets, so those targets were dropped.
    - Untargeted items drifted: student loan interest by −10.5% and the
      AMT by −10.3% in sums across states.
  - The 2024 version used ridge-type least squares, ‖Ax − b‖² + δ‖x − 1‖²,
    with JAX gradients.
- **Validation.**
  - Tests compare returns, AGI and income tax with CBO's February 2026
    projections at 2022, 2026, 2031 and 2036, within 1%.
  - Archived comparisons from March 2025 show calendar-2023 income tax of
    $2,237.6 billion against CBO's $2,512.3 billion.

## Tax Policy Center (Urban–Brookings)

Sources (verified):
- Brief description, updated 31 July 2026:
  https://taxpolicycenter.org/resources/tax-model-resources/brief-description-tax-model
- Earlier version (March 2022 text) and FAQ:
  http://web.archive.org/web/20260315170917/https://taxpolicycenter.org/resources/brief-description-tax-model
  and
  http://web.archive.org/web/20251008125101/https://taxpolicycenter.org/resources/tax-model-resources/tpcs-microsimulation-model-faq
- Rohaly, Carasso and Saleem (2005), model documentation:
  https://www.urban.org/sites/default/files/alfresco/publication-pdfs/411136-The-Urban-Brookings-Tax-Policy-Center-Microsimulation-Model.PDF
- Page (2021), technical update:
  https://www.urban.org/sites/default/files/publication/105278/tpc-model-technical-update.pdf
- Khitatrakun, Mermin and Francis (2016), state analysis:
  https://taxpolicycenter.org/sites/default/files/alfresco/publication-pdfs/2000697-Incorporating-State-Analysis-into-the-TPCs-Microsimulation-Model.pdf
- Khitatrakun, Mermin, Page and Rohaly (2023), race and ethnicity:
  https://taxpolicycenter.org/sites/default/files/publication/164920/a_new_approach_for_estimating_the_impact_of_tax_policies_by_race_and_ethnicity.pdf

### Base data

- **Two databases.** The July 2026 page describes a new 2015 PUF database
  (207,696 records). It also says the model "currently uses" a database
  built on the 2006 PUF: a 2011 "Look-Alike" file matched to the March
  2012 CPS. The page's aging section still describes the 2011-based file,
  so which one is in production is unclear.
- **Why 2006.** The 2006 PUF was chosen in 2015 as more representative
  than the boom year 2007 or the recession year 2008.

### Repairs to the 2015 PUF

- **Ages** are imputed by raking to SOI cross-tabulations. Raking scales
  weights to match one table at a time, repeatedly.
- **Capped dependent counts** are fixed with a Poisson model estimated on
  earlier PUFs.
- **The top tail.** The 2015 aggregate records, which hold 1,233 returns,
  are replaced by "several thousand of the most extreme records from the
  2001-2007 PUFs". Those records are grown to 2015 and reweighted to match
  "the counts and amounts for hundreds of variables" in the aggregate
  records.

### Aging and targets

1. **Uprating, 2015 to 2022.** Items grow by per-return growth from SOI
   tables.
2. **Reweighting** by linear programming to "hundreds of targets for counts,
   amounts, and select distributions". 2022 is the last year with detailed
   SOI tables.
   - The 2005 objective was the sum of absolute percentage weight changes,
     with tolerances of 0.10 to 0.30 (O'Hare's method). It had 17 targets
     and no distribution targets for future years.
   - Now the AGI distribution is matched to SOI for 2022–2023 and to CBO
     projections (CBO's revenue workbook, table 3) through the budget
     window.
3. **CPS match.** A constrained predictive-mean match to the March 2023
   CPS, within cells, with weight splitting. Unmatched CPS records become
   non-filers. Filing below the threshold uses Cilke's (1998) probits.

- **Projection targets:**
  - CBO income growth, revenue baseline and return counts;
  - JCT tax-unit counts and tax-expenditure distributions;
  - Census ages;
  - Department of Education projections;
  - CBO health costs.
- **Long run.** The same two stages are rerun every ten years from 2040 to
  2060 against DYNASIM3 and CBO targets.

### Imputations

- **Wealth.** From the SCF by probit and regression for 18 categories,
  iterated until net worth by size matches. The Forbes 400 are matched in
  separately.
- **Consumption.** From the CEX, in 25 categories.
- **Health coverage.** From Urban's HIPSM model.
- **Education.** From NPSAS.
- **Retirement.** From the SCF and SOI.
- **Benefits.** TRIM3 adjustments for SNAP, TANF and SSI.
- **Tips and overtime (2026).** From W-2 and BLS aggregates, allocated with
  the 2023 SIPP.
- **Vehicle loan interest.** Calibrated "closer to the Treasury estimates"
  where JCT and Treasury "differ markedly".
- **Race and ethnicity.** Each record is copied once per group and weighted
  to 2,812 targets with the Schirm–Zaslavsky method. That method makes each
  new weight an exponential function of the record's characteristics, the
  same functional form as raking. Only 3.6% of targets missed by more than
  2%.

### State weights and the censoring note

- **Method.** Schirm–Zaslavsky in nine AGI groups, with 22,308 SOI targets
  and a 5% tolerance.
  - Some targets were dropped to reach convergence.
  - Some "too extreme" targets were edited.
- **Results.** Simulated state income tax is within 3.8% of SOI in every
  state.
- **The censoring note.** Footnote 19 says the model cannot match "the
  relatively small number of returns itemizing deductions at the lowest AGI
  levels because it does not allow returns to itemize deductions if the
  standard deduction results in lower tax liability".

### The SOI–Urban synthetic data project

A fully synthetic file is one in which every record is generated from
fitted models and no real return appears. This project built its files with
sequential CART (classification and regression trees): each variable is
drawn from a tree fitted on the variables already synthesized, with
smoothed draws for continuous values.

Sources (verified):
- 2018 proposal:
  https://www.taxpolicycenter.org/sites/default/files/publication/156011/safely_expanding_research_access_to_administrative_tax_data_creating_a_synthetic_public_use_file_and_a_validation_server_0.pdf
- 2020 non-filer file:
  https://www.bea.gov/sites/default/files/2021-02/a-synthetic-supplemental-public-use-file-of-low-income-information-return-data.pdf
- NTA slides, May 2024:
  https://ntanet.org/wp-content/uploads/2024/05/Burman-Presentation_-Updated-NTA-54th-Annual-Spring-Symposium.pdf
- IRS PUF page, updated 22 May 2026, re-checked by me:
  https://www.irs.gov/statistics/soi-tax-stats-individual-public-use-microdata-files

What the sources say:

- **The 2012 supplemental file** synthesizes low-income non-filers from a
  0.1% sample of information returns.
- **The 2024 slides** say the 2015 synthetic PUF would not be public. They
  say a fully synthetic 2016 PUF would follow review, and that statistics
  from it "may be unreliable for some purposes".
- **The IRS page** says the 2012–2015 PUFs "are no longer available" and
  that a Synthetic Public Use File "will be available in the next few
  months".
- **Not verified:** the IRS "What's New" page listed 21 November 2025 for a
  tax-year-2016 synthetic file. I could not confirm that it was released.

## Penn Wharton Budget Model

Sources (verified; John is the author of the second, and only what it
states is reported):
- https://budgetmodel.wharton.upenn.edu/model/tax-module
- Ricco (2020), "Data Processing for PWBM's Tax Module":
  https://budgetmodel.wharton.upenn.edu/model-docs/DataProcessingForPWBMTaxModule.pdf
- Gokhale (2018), Working Paper 2018-1:
  https://budgetmodel.wharton.upenn.edu/working-papers/w2018-1.pdf

### Base data

The current page describes the 2012 PUF, aged through history, matched to
the CPS, and projected with PWBMsim demographics. Simulated taxpayers choose
the deductions and credits that minimize their tax.

### Aggregate records

The top records for each variable are copied. That gives 134 records
standing for 1,362 returns, which are reweighted to the aggregate records'
counts and totals.

### Historical aging, 2012 to 2016

Ricco (2020) describes "a modified version of the two-stage aging procedure
initially developed by John O'Hare".

- **First step.** Weights are scaled by growth in returns by AGI group.
- **Stage 1, a linear program.**
  - Objective: minimize Σ|z| with |z| < δ, where z is the percentage
    change in a record's weight and δ is the cap.
  - Targets: each count target must fall within a tolerance band.
  - Solver: lp_solve.
  - δ is lowered from 0.5 until the problem becomes infeasible; it solved at
    0.4. The lowest feasible δ rises with distance from the base year.
- **The targets.** 22 count targets, some by AGI group, with tolerances:
  - 0.1% for returns by filing status;
  - 1% for counts of medical, SALT, mortgage and charitable deductions;
  - 3% for interest and dividends;
  - 10% for capital losses.

  They were chosen by "a qualitative assessment of how important each is to
  correctly calculating tax liabilities".
- **Stage 2.** Amounts are rescaled to SOI totals by AGI group. The order is
  reversed from O'Hare's, which allows count targets to be matched more
  closely. Reweighting rather than zeroing values "maintains the joint
  distributions between variables".

### CPS match

A constrained predictive-mean match in 14 cells. Elective filers are chosen
by a probabilistic module, and CPS non-filers are appended.

### Projection

- **Weights** grow with PWBMsim growth in tax units by age and marital
  status.
- **Incomes** grow by per-capita aggregate growth times a distribution
  factor for three AGI groups, "following Perese (2016)" at CBO.
- **Where aging happens.** Inside each model run, so reported income can
  respond to policy.

### Estate data

From the SCF and the Forbes 400, with mortality adjusted by income and
marital status, then rescaled and reweighted. In this case the rescaling
comes before the linear program.

### Not found

Any PWBM document on targeting itemized deductions after the 2017 tax law
(TCJA) by simulating itemization, and any move past the 2012 PUF.

## Tax Foundation

Sources (verified):
- https://taxfoundation.org/wp-content/uploads/2025/03/ModelMeth25.pdf
- https://taxfoundation.org/wp-content/uploads/2026/07/ModelMeth26.pdf
- Fiscal Fact 429 (2014): https://files.taxfoundation.org/legacy/docs/FF429.pdf

What the documents say:

- **Base data.** The 2011 PUF, with "more than 150,000" returns. Earlier
  versions used the 2008 PUF with no aging.
- **Extrapolation** "follow[s] the extrapolation approach developed in
  TaxData… initially developed by John O'Hare". It takes three steps:
  growth to CBO forecasts; a linear program to return-group, income and
  12-group wage targets; and per-record factors that keep untargeted
  distributions at their base-year shape.
- **CPS match and non-filers.** These follow taxdata.
- **Itemizing** is the taxpayer's choice of the larger benefit.
- **Not found:** a Tax Foundation target list, or any validation against
  SOI or CBO.

## Joint Committee on Taxation

Sources (verified):
- JCX-75-15 (April 2015), *Description of the Individual Tax Model*:
  https://www.jct.gov/getattachment/0308b4da-ff48-4e9e-92f6-3592eb6a45ac/x-75-15-4776.pdf
- JCX-48-23 (30 October 2023), *Description of the Individual Tax Model for
  2023*:
  https://www.jct.gov/getattachment/3f7eb8cb-f024-4b37-814d-3050e47c5e43/x-48-23.pdf
- JCX-46-11 (2011), *Summary of Economic Models and Estimating Practices*:
  https://www.jct.gov/getattachment/8c7d67bd-1e93-4dbd-885b-be35e7077b1d/x-46-11-4373.pdf
- *Revenue Estimating Process*, January 2025:
  https://www.jct.gov/getattachment/29cf59ce-31ca-450b-adea-3848d0d33350/Revenue-Estimating-Process-January-2025.pdf

### Base data

- **The INSOLE file.** SOI's confidential edited sample, the file behind
  Publication 1304; not the PUF.
  - The 2023 model uses tax year 2019: 360,033 records with more than 2,900
    dollar fields.
  - The 2014 model used tax year 2011.
  - The January 2025 slides still describe the 2019 sample.
- **How often the base changes.** "Generally… every two years". A recession
  trough may be skipped as "an unsuitable base".
- **How current this is.** In 2023 JCT worked from 2019 data while the
  latest PUF was 2015.
- **Linked data.** Linked by Social Security number:
  - SSA birth year, death year and sex;
  - 19 kinds of information return (W-2, 1098, 1099 series, 1095, 5498,
    SSA-1099 and others);
  - employer Form 941, for firm size and industry.

### Non-filers

- **The count.** The 2019 resident population (329.2 million) plus deaths
  (2.9 million) gives 332.1 million. People on returns were 294.4 million;
  less 1.5 million overseas and 0.6 million "excess dependent children",
  that is 292.3 million. The difference is 39.8 million non-filers
  (Table 1).
- **The records** come from the Continuous Work History Sample. That is a
  1-in-1,000 sample fixed by Social Security number digits; JCT keeps the
  people with information returns but no return.
- **Tax units.** Couples are formed by pairing 8.1 million weighted men with
  8.1 million weighted women. Children are assigned to adults without
  wages.
- **History.** Before about 2009, non-filers came from an SOI–CPS match.

### Imputations

- **Statistical matches.** To MEPS for health and other items, and to the
  CEX for excise-tax spending. Both are within exact cells, then by
  closeness. The 2014 model matched to the CPS by rank within cells.
- **Itemized deductions for non-itemizers in the 2023 model.** The 2017-based
  model is extrapolated to 2019. Those "extrapolated taxpayers" are matched
  to actual 2019 returns in 385 bins, defined in part by sampling-weight
  group, and deductions are imputed from the match (JCX-48-23, pp. 19–20).

### Extrapolation

- **Growth factors.** 29 to over 30 of them. Wages get separate factors by
  wage class.
- **Reweighting.** About 100 targets per year.
  - Population by age.
  - Returns by filing status.
  - Credit claimants.
  - Mortgage-interest itemizers.
  - Capital gains, wages and dividends by income class.
  - IRA amounts by age.
  - Charitable giving.

  CBO supplies "more than 300 macroeconomic and tax-related values".
- **Income-class shares.** These come from equations estimated on past SOI
  files. The classes are defined on "modified total income", which leaves
  out capital gains and taxable Social Security because AGI's definition
  has changed over time. Interest targets by class were dropped when the
  equations lost predictive power.
- **The objective** (JCX-75-15, Appendix): minimize
  Σ K·S·[α(x_b⁴ + x_b⁻⁴ − 2) + β(x_c⁴ + x_c⁻⁴ − 2)], subject to every target
  being met exactly. The terms are these:
  - x_b is the ratio of the new weight to the base weight.
  - x_c is the ratio to the previous projected year's weight.
  - K = 0 freezes a record. JCT does this routinely for records with an
    original weight below 2. Frozen records still count toward targets,
    scaled by overall growth.
  - S raises the cost of moving records that carry a lot of a targeted
    item.

  The equations are solved by Newton–Raphson, a standard iterative method.
- **Chaining.** α falls from 1.0 to 0.2 across the window while β rises,
  so later years are anchored more to the previous year than to the base
  year.
- **Not found:** whether actual SOI totals for 2020–2022 replace forecasts
  in the years between the base and the present.

### Validation

Almost none is published. The documents say married-person counts are
"very close" to population figures and that deaths match the CDC total.

## Congressional Budget Office

Sources (verified; read through Internet Archive copies):
- *An Overview of CBO's Microsimulation Tax Model* (June 2018):
  https://www.cbo.gov/system/files/2018-06/54096-taxmodel.pdf
- Perese, *Projection and Alignment Methods for Static Microsimulation
  Models* (2016):
  https://www.cbo.gov/sites/default/files/114th-congress-2015-2016/presentation/52147-presentation.pdf
- Perese, *Statistically Matching Administrative Tax Data With Household
  Survey Data* (2017):
  https://www.cbo.gov/system/files/115th-congress-2017-2018/presentation/52914-presentation.pdf
- *The Distribution of Household Income in 2021* (2024), Appendix A:
  https://www.cbo.gov/system/files/2024-09/60341-income.pdf

### Purpose and base data

- **Not used to score legislation.** JCT scores legislation. CBO uses its
  model for baselines and distributional work.
- **Base data.** The SOI sample (more than 300,000 records), available
  "roughly 1.5 years after the end of the tax year". The 2021
  distribution file used 438,281 SOI records and 59,148 CPS households.

### Imputations

- **Exact matches.** W-2s and 1099s supply the spouse earnings split and
  untaxed Social Security.
- **CPS match.** Supplies transfers and non-filers.
- **SCF and CEX.** Supply potential itemized deductions for non-itemizers.

### The SOI–CPS match

1. Build tax units in the CPS.
2. Form 15 demographic cells.
3. In each cell, regress SOI total income on income items both files share.
4. Sort both files by predicted income and pair records with weight
   splitting.
5. The leftover low-income CPS units become non-filers.

In 2013 this gave 147 million filing and 97 million non-filing units. CBO
does not use the CPS race carried by the match, because the match may not
preserve income–race relationships.

### Projection

Five sequential adjustments, with no joint optimization:

1. Population by age, sex and marital status.
2. Employment count.
3. Presence of particular income types, assigned at random.
4. About 100 items scaled with 12 macro series.
5. The wage distribution: the top 1% share comes from a regression on SSA
   wage data, with offsetting growth lower down. Only wages change shape.

The 2016 presentation surveys other agencies:

- **The comparison it draws.** The linear-programming version "produces a
  trimodal distribution of weight changes", while the nonlinear version
  "produces a smooth distribution". It adds: "One is not necessarily better
  than the other."
- **What Treasury and JCT use.** It says they use the x⁴ + x⁻⁴ − 2 form.

## Treasury Office of Tax Analysis

Sources (verified):
- Technical Paper 12 (May 2023), *U.S. Treasury Individual Income Tax
  Model*: https://home.treasury.gov/system/files/131/TP-12.pdf
- Technical Paper 8 (2022), distribution methodology:
  https://home.treasury.gov/system/files/131/TP-8.pdf
- OTA Paper 62 (Gillette, 1989), goodness of fit for extrapolations:
  https://home.treasury.gov/system/files/131/WP-62.pdf
- Technical Paper 3 (2008), panel model:
  https://home.treasury.gov/system/files/131/TP-3.pdf
- Technical Paper 6 (2015), state reweighting:
  https://home.treasury.gov/system/files/131/TP-6.pdf

### Base data

- **Versions.** TP-12 documents the 2016-based model, used for the FY2021–23
  budgets. A 2019-based model was used for FY2024. There have been more
  than 20 versions since 1963.
- **The 2016 INSOLE.** 148.6 million returns, and over 338,000 unweighted
  nondependent returns.
- **Information returns.** These allocate amounts among people on a return,
  but "the 1040 amounts are assumed to represent the true values".

### Imputations

- **Itemized deductions for non-itemizers.** By the EM algorithm, which
  alternates between filling in missing values and re-estimating the model.
  - Shares of AGI are treated as a truncated multivariate normal.
  - Whether a deduction is present at all is hot-decked from itemizers. Hot
    decking copies values from a similar donor record.
  - Amounts are then forced to American Housing Survey, Giving USA and
    Census government-finance totals.
  - Each total is capped at a random value between half the standard
    deduction and the full standard deduction.
- **Wealth.** Linked to estate returns and hot-decked from estate returns
  and the SCF. OTA prefers hot decks and predictive mean matching because
  they "only use actual values".

### Non-filers

- **Records.** From the Continuous Work History Sample: 95,000 people, cut
  to 40,000 and formed into about 34,000 units using Form 1095 household
  information.
- **Weights.** Reweighted to match Census CPS data linked to returns,
  giving 39.5 million people in 31.5 million units.
- **The total checked against bounds.** The model's population of about
  321.2 million falls between bounds of 318.3 and 321.8 million taken from
  population tax data.

### Extrapolation

- **Two steps.** About 40 growth factors, some varying across the
  distribution, then reweighting.
- **Targets: 84.** 35 counts and 49 dollar amounts, including:
  - modified AGI in 15 classes;
  - capital gains in 9 classes;
  - non-filer income in 5 sources.

  The targets come from OMB's assumptions and SSA projections.
- **Objective.** Minimize Σ w·(x⁴ + x⁻⁴ − 2) subject to exact targets,
  solved through one Lagrange multiplier per target. "Five to ten
  iterations will be sufficient."
- **When it fails to converge.** The cause is "generally… the target
  forecast being inconsistent and alternative targets are chosen."
- **A stated caution.** "Adding more targets will eventually lead to
  non-convergence… The extrapolation process is akin to inflating a
  balloon… Adding targets leads the balloon to bulge in an unconstrained
  dimension."

### The 1989 backtest (Paper 62)

- **Design.** The 1981 file was extrapolated to 1983 with actual 1983 SOI
  totals as targets.
- **Scoring.** Percent errors, and Theil's "information gain" by AGI class.
  Information gain is the Kullback–Leibler divergence between extrapolated
  and true shares.
- **Findings.**
  - Reweighting alone produced "major changes in the weights" and was
    rejected. Uniform inflation of dollar amounts before reweighting fixed
    it.
  - Hitting totals did not reproduce distributions, so some items were
    targeted by AGI class.
  - "It has proven impossible to improve the predictive power… by simply
    adding more targets". The paper suggests the procedure "needs some
    'degrees of freedom'".

### Related work

- **The panel model (TP-3).** Ages an old panel by matching it within cells
  to a later file, with weight factors capped between 0.1 and 10.
- **State and race weights (TP-6 and TP-11).** Use log-linear models fitted
  on full-population data instead of raking.

## Other models

### NBER TAXSIM

TAXSIM is a calculator, not a data builder.

- **Marginal-rate tables.** Built on PUFs through 2010. "We no longer supply
  extrapolations beyond the actual data."
- **Old aging.** Uniform inflation of every amount, for example "an
  inflation rate of 2.5% and a real growth rate of 2.2%".
- **Successor.** PolicyEngine is building it "with my cooperation"
  (Feenberg).

Sources:
- https://taxsim.nber.org/marginal-tax-rates
- https://taxsim.nber.org/archive/mrates/mrates3.html
- https://taxsim.nber.org/
- https://github.com/PolicyEngine/policyengine-taxsim

### ITEP

Sources:
- https://itep.org/itep-tax-model/
- https://itep.org/who-pays-7th-edition-methodology-and-discussion/

What the sources say:

- **Base data.** IRS public-use returns paired with the ACS, which supplies
  non-filers and state detail. The base year is 2019, chosen over the
  COVID year 2020.
- **Imputations.** Consumption from the CEX by a fractional multinomial
  logit, a model that predicts a full set of budget shares summing to one.
  Renters' property tax comes from Zillow price-to-rent ratios.
- **Aging.** Incomes and weights are adjusted "to reflect targets published
  by the IRS, CBO, and other sources". The method is not described.
- **Validation.** Within 2% of IRS values in more than 97% of 3,264
  income-by-band-by-state cells.
- **Not found.** The PUF year, the matching method, and the code, which is
  not public.

### Urban TRIM3 and ATTIS

Sources:
- https://www.census.gov/content/dam/Census/library/working-papers/2016/demo/Effect-of-Different-Tax-Calculators-on-the-SPM.pdf
- https://www.nationalacademies.org/read/25246/chapter/17
- https://www.nationalacademies.org/read/29163/chapter/20
- https://www.urban.org/research/publication/analysis-transfers-taxes-and-income-security-attis-model

**TRIM3** runs on one year of the CPS ASEC.

- **Filing.** Every tax unit is simulated to file.
- **Tax variables.** Capital gains, IRA and Keogh contributions and
  itemizable expenses come from a PUF statistical match.
- **Calibration.** Taxes are compared with SOI but not adjusted. For 2021,
  returns with positive liability were 10% high and liability 11% short.
- **Aging.** As of 2019 it "does not currently include the ability to age
  the population".

**ATTIS** runs on the ACS.

- **Itemizing.** Assumes no one itemizes.
- **Calibration.** Compares with SOI but does not adjust. In New York, AGI
  was 10% short and EITC dollars were 57% of target.

## The Budget Lab's published description and the CRS comparison

Sources (the second re-checked by me):
- "Tax Microsimulation at The Budget Lab" (12 April 2024):
  https://budgetlab.yale.edu/research/tax-microsimulation-budget-lab
- CRS R49052, *A Comparison of Two Open-Source Tax Microsimulation Models*
  (30 July 2026): https://www.everycrsreport.com/reports/R49052.html

### The Budget Lab's document

- **Base data.** The 2015 PUF.
- **2015 to 2017.** O'Hare-style aging following Ricco (2020): a linear
  program on counts, then rescaling of amounts.
- **2018–2019.** No reweighting to SOI, because TCJA changed who itemizes.
- **2020 onward.** Weights grow with SSA population, and amounts are scaled
  to CBO 1040 aggregates, NIPA mortgage interest and Giving USA.
- **Non-filers.** From Piketty–Saez–Zucman's 2017 file.

**What outsiders see on GitHub.** The four-line README on
https://github.com/Budget-Lab-Yale/Tax-Data still lists "Running the LP
targeting algorithm for projected years" as outstanding. The `research/`
tree and the decisions log are public.

### The CRS report

- **What it compares.** Tax-Simulator with Tax-Calculator release 6.7.2.
  Both rest on "the IRS's 2015 Public Use File [PUF], supplemented and aged
  forward".
- **Where it expects the largest gaps from JCT.** Provisions that "require
  significant data imputation and aging". It names capital gains, the
  section 199A deduction and the net investment income tax.
- **Its baseline comparison.** It reports income tax about 9% below CBO in
  both models. (Read through a page summary; not verified in the full
  text.)

## Methodological literature

Opened, or bibliographic details checked by DOI:

- **Deville and Särndal (1992)**, "Calibration Estimators in Survey
  Sampling", *JASA* 87: 376–382,
  https://doi.org/10.1080/01621459.1992.10475217.
  - Weights are chosen as close as possible to the design weights, under a
    chosen distance, subject to exact totals.
  - Chi-square distance gives GREG; another choice gives raking.
- **Deville, Särndal and Sautory (1993)**, "Generalized Raking Procedures",
  *JASA* 88: 1013–1020, https://doi.org/10.1080/01621459.1993.10476369.
  Bounded distances that keep weights positive and limited.
- **Dekkers (2014)**, "The simulation properties of microsimulation models
  with static and dynamic ageing", *International Journal of
  Microsimulation* 8(1), https://microsimulation.pub/articles/00113.
  Static aging changes "the combination of characteristics, not the
  characteristics themselves". It fails for combinations not in the base
  data.
- **Cilke (1998)**, "A Profile of Non-Filers", OTA Paper 78,
  https://home.treasury.gov/system/files/131/WP-78.pdf. The classic
  evidence on who files below the threshold, from the CPS exactly matched
  to returns.
- **Wheaton and Stevens (2016)**, the Census working paper above. It shows
  that tax-unit rules, the PUF match and assumptions about who files all
  move survey-based tax results.
- **Bowen et al. (2022)**, *National Tax Journal* 75(4): 767–790,
  https://ideas.repec.org/a/ucp/nattax/doi10.1086-722094.html. Synthetic
  individual tax data. Abstract only.

Not opened; cited from memory or from other documents, so check before
citing:

- **Penalized ("ridge") calibration.** Bardsley and Chambers (1984); Rao and
  Singh (1997); Beaumont and Bocci (2008); Guggemos and Tillé (2010),
  "Penalized calibration in survey sampling", *Journal of Statistical
  Planning and Inference*. These replace exact constraints with a penalty
  per target, so each target can be met approximately, at a strength set
  target by target. This is the established form of the brief's framing.
- **Reweighting distances for tax models.** Creedy (2003), "Survey
  Reweighting for Tax Microsimulation Modelling", NZ Treasury WP 03/17.
  Merz on reweighting by "minimum information loss" (an entropy distance).
  Immervoll et al. (2005), EUROMOD EM7/05, on static aging.

## Comparison table

"LP" is O'Hare's linear program: minimize the sum of absolute percentage
weight changes, within caps. "x⁴ penalty" is the smooth objective
Σ w(x⁴ + x⁻⁴ − 2).

| Model | Base data (year) | Missing variables | Non-filers | Aging to the current year | Projection | Distance from the prior | Targets precision-weighted? | Public material |
|---|---|---|---|---|---|---|---|---|
| PolicyEngine Enhanced CPS (to mid-2026) | CPS ASEC 2025 (income 2024); 2015 PUF as training data | QRF from PUF (7 demographic predictors); ACS, SIPP, SCF | Already in the CPS; filing by rule plus random voluntary filing | Adam on log-weights, about 3,000 targets computed by the calculator | Refit per year; long run to 2100 by entropy calibration to SSA | None (start point and early stopping) | No; hand-set multipliers | Full code |
| PolicyEngine Populace (since June 2026) | CPS ASEC + ACS stack; 2015 PUF uprated to 2024 | QRF with income rank added | Already in the surveys | Weighted mean absolute percentage error, capped; weight cap; optional chi-square pull | Demographic reweighting plus factors; income totals deliberately untargeted | Optional chi-square; default does not anchor | No (square root of value) | Full code; facts in Ledger |
| taxdata (PSL) | 2011 PUF + March 2016 CPS | Constrained predictive-mean match; regressions | Unmatched CPS units appended | LP with 27 exact targets, caps 0.30–0.50 | Re-solve LP each year to CBO-grown SOI targets | L1 | No | Full code |
| TMD (PSL) | 2015 PUF aged to 2022 | Random-forest demographics; SIPP and CEX by chained equations | CPS units by Census-style filing rule | Quadratic program, 550 SOI 2022 targets, ±0.5% plus slack | No reweighting; weights grow with CBO returns | L2 | No (same tolerance for all) | Full code |
| TPC | 2006 PUF (2011 base); 2015 PUF (2022 base) in development | Predictive-mean match to CPS; SCF, CEX, HIPSM and others | Unmatched CPS records; Cilke probits | LP, "hundreds" of targets | LP per year to CBO and JCT; every 10 years to 2060 | L1 | No (tolerances by hand) | Documentation only |
| PWBM | 2012 PUF aged to 2016 | Predictive-mean match to CPS | CPS non-filers appended | LP with banded tolerances, then rescaling | PWBMsim demographics plus income growth in 3 AGI groups | L1 | Partly (hand tolerances 0.1–10%) | Documentation only |
| Tax Foundation | 2011 PUF | Follows taxdata | Follows taxdata | taxdata LP | taxdata LP | L1 | No | Overview only |
| JCT | 2019 INSOLE (confidential) | Linked information returns; matches to MEPS, CEX; aged-file match for deductions | Information-return sample; residual count | x⁴ penalty, about 100 exact targets | Per year with chaining toward the prior year | x⁴, base and previous year | No | Detailed documentation |
| CBO | SOI sample (confidential) | Exact matches; CPS rank match; SCF, CEX | Leftover CPS units from the match | Sequential scaling | Population, employment, income totals, wage shares | None (sequential) | No | Slides, papers |
| Treasury OTA | 2016 / 2019 INSOLE (confidential) | Linked returns; EM; hot deck | Information-return sample; population bounds | x⁴ penalty, 84 exact targets | Per budget exercise | x⁴ | No | Detailed technical paper |
| ITEP | PUF (year not stated) + ACS, base 2019 | CEX, ACS, SCF | ACS | Not described | Not described | Not described | Not described | Overview only |
| TRIM3 / ATTIS | CPS ASEC / ACS | PUF match (TRIM3); none (ATTIS) | All units in the survey | None | None (TRIM3 does not age) | None | No calibration of taxes | Documentation |
| Budget Lab (public description) | 2015 PUF | Several imputations | Piketty–Saez–Zucman 2017 file (now ASEC pool) | Linear program to 2017, then scaling | Demographic weights plus aggregate factors | Linear (sum of weight factors, bounded) | No | Code and research tree public |

## What Tax-Data could adopt or should avoid

This section draws on the material above.

### Could adopt

- **A smooth distance from the prior instead of the absolute-value linear
  program.** Options are chi-square (GREG's), entropy (raking's), or the
  agencies' x⁴ + x⁻⁴ − 2. All spread the adjustment across records rather
  than leaving most unchanged and the rest at a cap. CBO's 2016 review
  describes the three-peaked distribution of weight changes that the
  absolute-change linear program produces. Tax-Data's own linear program
  shows a related pattern: in its cached solution almost every record sits
  at one of the two bounds.
- **Soft targets with a stated precision per target.** No production model
  does this today. The pieces exist:
  - the penalized-calibration literature;
  - TMD's quadratic program with slack;
  - Populace's chi-square option and its charter;
  - OTA's 1989 call for "degrees of freedom";
  - TPC and OTA practice, which drop or edit conflicting targets by hand.

  PolicyEngine's incident shows that the loss must be measured in absolute
  terms scaled by each target's precision. A relative error averaged over
  thousands of targets let large aggregates be "drowned out".
- **Diagnostics that others found useful:**
  - the cost of each target to the solution, from TMD's shadow prices;
  - effective sample size, the number of equally weighted records the final
    weights are worth;
  - top-weight share;
  - drift in variables nobody targeted (TMD's "bystander" table);
  - a per-record weight-ratio cap, with a record of how often it binds;
  - Populace's check of how far weights must stretch before a target is
    switched on, and its exclusion register.
- **Matching law-dependent statistics by simulating them.** PolicyEngine's
  tax-expenditure targets give each record its change in tax under repeal.
  Because every record's contribution is computed once and is linear in the
  weights, the target fits an ordinary reweighting. JCT's 2023 method for
  deductions ages the older file and matches it to newer returns, which
  uses the aged file as a prior for the newer evidence.
- **A backtest in OTA's 1989 design.** Age the 2015 file to 2019, 2021 and
  2022 with actual SOI totals as targets. Hold some tables out, and score
  each item's distribution across AGI classes with Theil's information gain.
  Populace's rule that survey-measured tax and benefit quantities stay out
  of the fit and serve only as checks is a version of the same idea.
- **Separating published facts from the targets built on them.** Ledger
  stores each figure with its source and marks publishers' projections as
  projections. Ageing and reconciliation happen downstream, in the build.
- **Non-filer counts as an accounting residual with stated bounds.** JCT's
  calculation is population plus deaths, minus people on returns, minus
  overseas filers, minus excess dependents. OTA's lower and upper bounds
  supply a precision directly.
- **Income-class targets on an income concept that is stable over time.**
  JCT's modified total income leaves out capital gains and taxable Social
  Security.
- **Anchoring each year to the previous year as well as the base.** JCT
  moves the weight on the base year from 1.0 to 0.2 across the window and
  shifts the rest to the previous projected year.
- **Rebuilding the top tail.** TPC replaces the 2015 aggregate records with
  real extreme records from the 2001–2007 PUFs, reweighted to the aggregate
  records' totals. PolicyEngine builds the tail from the Forbes 400 through
  the SCF.

### Should avoid

- **A reweighting with no term for distance from the starting weights.**
  PolicyEngine's 2026 incident came from this, and TMD's 2024 penalty was
  set to zero.
- **A flat relative-error loss over many targets of very different size.**
- **Linear-program reweighting**, with its corner solutions. taxdata, TPC,
  PWBM and the Tax Foundation use the absolute-change version; the current
  Tax-Data uses a version that minimizes the sum of the weight factors.
- **Exact equality targets that conflict.** They force hand-dropping of
  targets, as at OTA and in TPC's state work.
- **Box bounds that dominate the prior term.** TMD found that only the cap
  mattered.
- **Imputing PUF incomes from a handful of demographics.** PolicyEngine's
  legacy predictor set had no income variable.
- **Uniform growth factors that freeze each income distribution at its
  base-year shape.** Only CBO's wage-share regression and JCT's wage-class
  factors relax this for wages.
- **Fitting survey-measured tax or benefit totals.** Populace keeps these
  out of the fit and treats them as checks instead.

## What I looked for and could not find

- Any agency document describing the use of actual SOI totals for the years
  between the base year and the present.
- Any JCT or OTA table of target misses for a current model. The only
  published goodness-of-fit study is OTA's from 1989.
- Any CBO document after 2018 on its projection method.
- OTA documentation of the 2019-based model.
- Any PWBM, TPC or Tax Foundation document on targeting post-2017 SOI
  itemized-deduction tables. TPC's footnote 19 is the only statement.
- ITEP's PUF year and matching method.
- TMD's funding source.
- Whether the IRS has released the synthetic 2016 PUF. It was scheduled for
  November 2025.
- A published comparison of revenue scores across models by Auerbach or
  Burman.
- National Tax Journal articles on how models build their data, and the
  Handbook of Microsimulation Modelling chapter on static aging. The search
  budget ran out.
- PolicyEngine's validation dashboard, which I did not open.
