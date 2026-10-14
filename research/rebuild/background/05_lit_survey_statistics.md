---
title: "Literature review: survey statistics, official statistics and econometrics for combining a sample with external totals"
role: evidence
workstream: rebuild
status: current
updated: 2026-10-07
sot: research/rebuild/BRIEF.md
supersedes: []
superseded_by: null
---

# Literature review: survey statistics, official statistics and econometrics

Background paper 05 for the Tax-Data rebuild. It covers the statistical and
econometric traditions. Machine learning and Bayesian computation are covered
by another paper.

## Summary

1. **The problem in the brief already has a name in survey statistics:
   calibration with uncertain control totals.** Survey statisticians call
   "adjust the weights as little as possible so that weighted totals match
   known totals" *calibration* (Deville and Särndal 1992). The version where
   the totals are themselves estimates with stated error, and are matched only
   approximately in proportion to their precision, has been worked out since
   the 1980s. It appears as ridge calibration (Bardsley and Chambers 1984),
   calibration to controls with an estimated covariance matrix (Isaki, Tsay
   and Fuller 2004), and penalized calibration (Guggemos and Tillé 2010).
   National accountants solved the same problem for aggregate tables in 1942
   (Stone, Champernowne and Meade). The brief's "Bayesian updating" framing
   is, at its single best estimate, one of these estimators.

2. **The current linear program is the textbook case of what not to do, and
   the literature explains the 62% / 38% split exactly.** A linear program's
   solution sits at a corner: at most as many records can have factors
   strictly between the bounds as there are binding constraints. With 188
   targets, 114 interior records is what theory predicts. The objective
   "minimize the sum of factors" also has no statistical meaning; it pushes
   every factor it can to the lower bound. Every smooth distance function in
   the calibration literature avoids this.

3. **The U.S. Treasury already uses smooth calibration; TPC and the Policy
   Simulation Library use linear programs.** Treasury's Office of Tax Analysis
   (Technical Paper 12, 2023) reweights to 84 targets while minimizing a
   symmetric penalty on weight ratios that grows without bound as a ratio
   approaches zero or infinity. The Tax Policy Center (2021 update) and PSL
   `taxdata` use linear programs. None of the U.S. documents I read treat
   targets as noisy.

4. **The choice of distance function matters less than the choice of bounds
   and of which targets are hard.** All standard distance functions give the
   same answer to first order in large samples (Deville and Särndal 1992).
   They differ when bounds are tight or targets are many relative to the
   records that carry them, which is the situation at the top of the PUF.
   The entropy (raking) distance and the bounded "logit" distance are the
   usual choices; entropy balancing in econometrics (Hainmueller 2012) is the
   same estimator as raking.

5. **Censored tables fit into calibration directly when only weights change.**
   If each record carries its underlying deductible expenses, then "total
   itemized deductions claimed under 2019 law" is a fixed number per record
   times its weight, so the target is linear in the weights. Survey
   statisticians call targets built from a model applied to each record
   *model calibration* (Wu and Sitter 2001). When record values change as
   well, the target becomes discontinuous, and the econometric tools are
   simulated method of moments and indirect inference.

6. **Reweighting alone cannot change the shape of a distribution within a
   group of similar records, and cannot create values that are not in the
   file.** The tax-model and official-statistics practice for projection is
   a hybrid: change values with several growth factors that differ across
   the distribution, then reweight. Treasury allows about 40 growth factors,
   including different wage growth at different points of the wage
   distribution. Blanchet, Saez and Zucman's real-time inequality series
   shifts each record's rank within its demographic cell and reassigns
   wages. Tax-Data today uses one growth factor per variable for every record.

7. **Statistical matching (data fusion) always rests on the conditional
   independence assumption, which cannot be tested from the two files.** Every
   survey-based imputation in Tax-Data assumes the imputed variable is
   independent of the PUF-only variables once the conditioning variables are
   fixed. The literature offers three remedies: auxiliary data where both are
   observed (Rubin 1986; Fosdick, DeYoreo and Reiter 2016), bounds on what the
   files can identify (D'Orazio, Di Zio and Scanu 2006), and imputing then
   calibrating the imputed totals to known aggregates (Beaumont 2005).

8. **Grouped-data methods turn SOI tabulations into a full distribution.**
   Generalized Pareto interpolation (Blanchet, Fournier and Piketty 2022)
   recovers a smooth distribution from bracket thresholds, counts and
   amounts, which is the format of Publication 1304. This gives a principled
   way to build 2016–2023 distributional targets and to spot inconsistent
   tables.

9. **Distributional national accounts reconcile sources mainly by scaling
   values, not weights.** Piketty, Saez and Zucman, BEA and others allocate
   each national-accounts total to records in proportion to a related
   variable, so totals match by construction and shapes come from the
   microdata. This is Tax-Data's step 4 ("rescale dollar amounts within AGI
   bins") and has the same weakness: if a gap is concentrated in one group
   (for example under-reported self-employment income), proportional scaling
   puts it in the wrong place.

10. **Four strands matter most for the rebuild** (section 13): soft-target
    calibration with a bounded entropy distance; targets defined by
    simulating the reporting process, with grouped-data interpolation to make
    tabulations usable; statistical-matching theory to discipline the
    imputations; and hybrid aging that changes values as well as weights,
    with smoothing across years.

## Verification labels

Each reference in the list at the end carries one of three labels.

- **[read]**: I opened the document or page and read the passage the claim
  rests on.
- **[record]**: I confirmed the bibliographic record (the DOI resolves to
  this title, authors and journal through the Crossref registry, or the
  publisher's or agency's catalogue page appeared in search results with this
  title). The description of the content is from my own knowledge of the
  paper and was not re-read for this note.
- **[unverified]**: I believe the reference exists as described but could not
  confirm it.

The shared web-search budget for this session ran out partway through the
work. Section 14 lists what I planned to check and could not.

## 1. The problem in survey-statistics terms

In survey language, the brief's problem has these parts.

- **Design weights.** The starting weights (the PUF weights scaled to the
  year) are the "design weights" d_i.
- **Auxiliary information.** Each target is a known or estimated population
  total T_j of some quantity x_ij that can be computed on each record. The
  weighted estimate is Σ_i w_i x_ij.
- **Calibration.** Choosing new weights w_i close to d_i such that the
  weighted estimates hit the totals.
- **Edits.** Accounting identities that must hold on each record (wages of
  primary plus spouse equal total wages) are called *edit rules* or *balance
  edits* in official statistics (de Waal, Pannekoek and Scholtus 2011). They
  are hard constraints on record values, not on weights.
- **Imputation.** Filling in variables a record does not have, either from a
  model or from a donor record in another file.
- **Statistical matching or data fusion.** Imputation where the donor is a
  different survey that never observed the recipients.

Two distinctions organize the rest of this note: whether a method changes
**weights** or **values**, and whether targets are **hard** (met exactly) or
**soft** (met approximately, with a stated tolerance or variance).

## 2. Calibration and reweighting

### 2.1 Raking (iterative proportional fitting)

- **What it solves.** Adjust weights so the weighted table matches known
  one-way (or lower-order) margins, for example counts by filing status and
  counts by age group, when the full cross-table is not known.
- **How it works.** Scale weights to match the first margin, then the
  second, and so on, cycling until all margins match. Deming and Stephan
  (1940) introduced it; Ireland and Kullback (1968) showed that the
  converged answer is the table closest to the starting table in the sense
  of relative entropy (a measure of how far one distribution is from
  another), subject to the margins. Fienberg (1970) gave its convergence
  theory.
- **Key assumption.** The starting table has the right interactions; only
  the margins are off. Raking never creates an interaction the starting data
  do not have.
- **Uncertain targets.** Classic raking treats margins as exact. Inconsistent
  margins (two sources that disagree on the same total) make it cycle without
  converging.
- **Scaling.** Each cycle costs one pass over the records per margin.
  Convergence can be slow when margins nearly conflict.
- **Fit to Tax-Data.** Raking is a special case of calibration with the
  entropy distance (section 2.2). It works for counts by cell but not for
  dollar targets or overlapping targets, which are most of the 188.
- **Where it breaks.** Cells with zero or very few records, and margins that
  conflict.

### 2.2 Generalized regression and Deville–Särndal calibration

- **What it solves.** The general version: choose w to minimize a distance
  Σ_i d_i G(w_i / d_i) subject to Σ_i w_i x_ij = T_j for every target j
  (Deville and Särndal 1992). G is a function equal to zero when the weight is
  unchanged and growing as it moves away.
- **Distance functions.** The main choices are:
  - *Linear (chi-square).* G(r) = (r − 1)² / 2. Gives the generalized
    regression (GREG) estimator, which has a closed form. Weights can go
    negative.
  - *Raking (multiplicative, entropy).* G(r) = r log r − r + 1. Weights are
    the design weight times an exponential of a linear function of the
    record's characteristics, so they are always positive. This reproduces
    raking.
  - *Bounded "logit".* A distance that is infinite outside chosen bounds
    L < r < U, so the ratio of new to old weight can never leave the bounds.
    Introduced by Deville, Särndal and Sautory (1993) as "generalized raking".
  - *Truncated linear.* The linear distance with hard bounds.
- **Key result.** All of these give the same estimates to first order in
  large samples (Deville and Särndal 1992). Singh and Mohl (1996) compared
  them numerically and found they agree when bounds are loose and diverge
  when bounds are tight.
- **Key assumption.** The design weights are a good starting point, and the
  target variables explain the departure of the sample from the population.
  In model terms, the calibrated estimate of any other variable is good to
  the extent that it is linearly related to the target variables
  (Särndal 2007).
- **Uncertain targets.** Exact calibration treats every target as known.
  See 2.4 for soft versions.
- **Scaling.** The solution is found by solving for one number per target
  (the "dual" problem: the multipliers that price each constraint), not one
  per record. Each iteration of Newton's method costs a pass over records
  times targets. This is why national statistical offices run calibration on
  millions of records with ordinary software. Standard implementations: the
  R `survey` package's `calibrate()` (Lumley 2004), CALMAR (INSEE), and
  Stata's `ipfraking` (Kolenikov 2014).
- **Fit to Tax-Data.** Direct replacement for the linear program. The weights
  depend smoothly on record characteristics, so two records with the same
  values get the same adjustment.
- **Where it breaks.** Exact calibration has no solution when targets
  conflict or when bounds are too tight for the targets. The linear distance
  gives negative weights. Many targets carried by few records (the top of the
  income distribution) produce extreme weights.

### 2.3 Bounds, extreme weights and how they are measured

- **The problem.** Calibration can make a handful of records very heavy.
  Heavy records inflate the variance of every estimate that touches them and
  make the file fragile: one high-income record with an unusual deduction can
  move a reform estimate.
- **Diagnostics.** The usual summary is Kish's effective sample size,
  (Σ w_i)² / Σ w_i², or equivalently the design effect from weighting,
  1 + (coefficient of variation of the weights)². Reviews: Haziza and
  Beaumont (2017), Chen, Elliott, Haziza and others (2017).
- **Remedies.** Bounded distances (2.2); weight trimming followed by
  re-calibration; range-restricted calibration through empirical likelihood
  (Chen, Sitter and Wu 2002); relaxing some constraints (2.4).
- **Fit to Tax-Data.** The effective sample size should be reported for the
  whole file and for the top AGI groups after each reweighting. The current
  LP output (every factor at 0.34 or 1.66) has a weight ratio of 4.9 between
  otherwise identical records, chosen by the solver rather than by the data.

### 2.4 Soft constraints: ridge, penalized and uncertain-target calibration

This strand is the closest to the brief's framing.

- **What it solves.** Targets that are estimates with error, targets that
  conflict, and too many targets for the data to meet exactly within bounds.
- **The estimator.** Minimize
  Σ_i d_i G(w_i / d_i) + ½ Σ_j (Σ_i w_i x_ij − T_j)² / σ_j²
  subject to the hard constraints, where σ_j is the stated error of target j.
  (With correlated targets the second term becomes a quadratic form in the
  inverse covariance matrix.) A target with tiny σ_j is effectively hard; a
  target with large σ_j barely moves the weights.
- **Literature.**
  - Bardsley and Chambers (1984) introduced *ridge calibration*: a penalty
    on missed targets, which is equivalent to ridge regression in the GREG
    case.
  - Rao and Singh (1997) used a ridge-type relaxation that loosens
    constraints one at a time until range restrictions on the weights are met.
  - Isaki, Tsay and Fuller (2004) built household weights by quadratic
    programming with size bounds and control totals, and extended it to
    "the situation where the controls are estimates with an estimated
    covariance matrix" (their abstract, which I read).
  - Guggemos and Tillé (2010) gave penalized calibration a model
    interpretation: it is the design-based counterpart of a mixed model in
    which some auxiliary variables have random rather than fixed effects.
  - Dever and Valliant (2016) and Berger, Muñoz and Rancourt (2009) derive
    variances when control totals are estimated rather than known.
  - Outside survey statistics, the transport population synthesizer
    PopulationSim relaxes each control by a factor penalized according to a
    user-set "importance", with very high importance making the control
    effectively hard (section 8; I read its configuration documentation).
  - Barratt, Angeris and Boyd (2021) state representative weighting as a
    general convex optimization with arbitrary losses on missed targets and
    a regularizer on the weights.
- **Bayesian reading.** If G is the entropy distance, the first term behaves
  like the log of a prior over the weights centred on the design weights, and
  the second is a normal measurement model for the targets. The minimizer is
  then the posterior mode. This is my reading of the formula; the closest
  explicitly Bayesian treatments I found are Si and Zhou (2021, "Bayes-raking")
  and Isaki–Tsay–Fuller's covariance formulation.
- **How much weight the prior gets.** The literature does not settle how to
  scale the distance term relative to the target term. In ridge and
  penalized calibration it is a tuning constant. One defensible anchor (my
  inference, not from a source I read): the entropy distance times the
  sample size approximates a log-likelihood ratio, so scaling the distance by
  the effective sample size puts it on the same footing as the
  inverse-variance target term.
- **Scaling.** Still a smooth convex problem with as many free quantities as
  records (or, in the dual, as targets). Well suited to gradient methods on
  a GPU.
- **Fit to Tax-Data.** This is the brief's framing, already published, with
  known variance theory. It also answers "projection is not a separate
  method": in 2060 the targets have large σ_j and the weights stay near the
  prior.
- **Where it breaks.** (a) Someone must state σ_j for every target; SOI
  sampling errors, CBO forecast errors and definitional mismatches are
  different kinds of error. (b) A definitional mismatch between two sources
  is a bias, not noise; treating it as noise splits the difference between
  two sources that are measuring different things. (c) Redundant targets
  (bracket counts that sum to a total that is also a target) count the same
  information twice unless their correlation is modelled.

### 2.5 Linear-programming reweighting and corner solutions

- **What it is.** Choose weight factors within fixed bounds to satisfy
  target constraints (here two-sided, within a tolerance), minimizing a
  linear objective. Tax-Data's `src/reweight.R` first searches for the
  smallest common bound ε for which the targets are feasible (a binary search
  over ε), then, within [1 − ε, 1 + ε], minimizes the sum of the factors.
- **Why corners.** A standard property of linear programs: an optimal
  solution can always be found at a vertex of the feasible region, and at a
  vertex the number of variables strictly between their bounds is at most
  the number of constraints that bind. Solvers return such vertices. With
  188 targets, at most 188 factors can be interior; the 2026-10-06 check
  found 114. Every other factor sits on a bound.
- **Why mostly at the lower bound.** Minimizing the sum of factors makes the
  solver lower as many factors as it can and compensate with the fewest
  possible factors at the upper bound. The 62% / 38% split follows from the
  objective, not the data.
- **Statistical consequence.** Which record gets 0.34 and which gets 1.66 is
  decided by the solver's path, not by any feature of the records. Any
  quantity not targeted (for example a deduction among married filers aged
  45–54 with AGI $200,000–$500,000) inherits arbitrary weight shifts.
- **Where else it is used.** The Tax Policy Center's 2021 technical update
  says it uses "a linear programming algorithm to adjust the weights on each
  record" in the second stage of extrapolation, for both the budget window
  and the long run (read). PSL `taxdata` stage 2 solves a linear program in
  Julia with JuMP and the Cbc solver (README read; objective not read).
- **Fit to Tax-Data.** Replace. A linear program is the right tool only when
  the objective has a meaning (for example minimizing total absolute
  deviation, which still produces corners).

### 2.6 Practice in tax and benefit models

- **U.S. Treasury, Office of Tax Analysis** (Technical Paper 12, May 2023;
  read). Extrapolation is a hybrid. Stage I applies about 40 growth factors,
  some differing across the wage distribution. Stage II grows weights by
  filing-status counts, recomputes derived variables under that year's law,
  then adjusts weights to hit 84 targets (35 counts, 49 dollar amounts,
  including capital gains in 9 income classes and modified AGI in 15),
  minimizing Σ_i w_i Φ(x_i), where x_i is the ratio of new to old weight. Φ
  is zero at 1, infinite at 0 and infinity, and symmetric in x and 1/x
  ("it is as distorting to multiply a weight by 4 as it is to divide it by
  4"). The PDF's text layer is garbled at the formula; it appears to be
  Φ(x) = x⁴ + x⁻⁴ − 2, which has those properties. Targets are hard. The
  paper warns that more targets do not necessarily improve the extrapolation
  and eventually prevent convergence: "adding targets leads the balloon to
  bulge in an unconstrained dimension" (citing Gillette 1989).
- **Treasury, state weights** (Fisher and Lin 2015, Technical Paper 6;
  read in part). Splits each record's weight into 52 state weights so that
  conditional distributions match the population file, following Schirm and
  Zaslavsky.
- **Tax Policy Center** (Page 2021; read). 2011 PUF base, CPS-matched,
  CPS non-filers added. Two stages: per-capita growth factors by income
  type, then linear-programming reweighting to aggregates and to the AGI
  distribution from SOI (2012–2018) and CBO (2019–2031). For 2040–2090 it
  targets DYNASIM3 projections of income components, age structure and the
  income distribution.
- **CBO.** Perese (2016) presented CBO's projection and alignment method and
  a survey of other models (page read through an archive; the slides
  themselves were blocked, so I cannot report the method).
- **Joint Committee on Taxation.** JCX-48-23 (2023) describes the
  individual tax model; I could not open it (access refused).
- **United Kingdom.** The Family Resources Survey is grossed with INSEE's
  CALMAR, a Deville–Särndal calibration with optional bounds, to 286
  age-sex-region categories plus controls for households with children,
  lone parents, council-tax band, tenure and regional household counts (ONS review by Lound and Broad, 2014; read). DWP's
  Policy Simulation Model projects the survey by uprating incomes from the
  forecast and calibrating weights to benefit caseload forecasts, holding
  employment states fixed (OBR Welfare Trends Report, January 2018, box 4.1;
  read).
- **Academic tax microsimulation.** Creedy (2004) reviews survey reweighting
  for tax microsimulation; Cai, Creedy and Kalb (2006) use it to impose
  projected population ageing. Immervoll and others (2005) set out a
  framework for static ageing in EUROMOD.

## 3. Entropy, empirical likelihood and exponential tilting

These are the same idea reached from information theory, nonparametric
likelihood and econometrics.

- **Minimum discrimination information (I-projection).** Among all
  distributions satisfying given moment constraints, choose the one closest
  in relative entropy (Kullback–Leibler divergence) to a reference
  distribution (Kullback and Leibler 1951; Csiszár 1975 proved existence and
  the geometry). With a uniform reference this is Jaynes's (1957) maximum
  entropy. The solution always has the exponential form: new weight = old
  weight × exp(λ′x_i). This is raking.
- **Entropy balancing** (Hainmueller 2012). Reweight a control group so its
  covariate moments match the treated group, minimizing entropy distance to
  base weights. Same estimator as calibration with the raking distance; Zhao
  and Percival (2017) showed it is consistent if either the selection model
  or the outcome model is linear in the balanced moments.
- **Empirical likelihood** (Owen 1988, 2001). Put probability p_i on each
  observation and maximize Σ log p_i subject to moment constraints. Chen and
  Qin (1993) applied it to finite populations with known auxiliary totals;
  Qin and Lawless (1994) to general estimating equations; Chen, Sitter and
  Wu (2002) used it to get calibration weights that stay within a range.
- **Exponential tilting and related estimators in econometrics.** Kitamura
  and Stutzer (1997) and Imbens, Spady and Johnson (1998) estimate moment
  condition models by minimizing an entropy distance; these are members of
  the family later called generalized empirical likelihood.
- **Combining micro and macro data.** Imbens and Lancaster (1994) showed how
  aggregate information (for example population means from a census) can be
  imposed on a micro sample to sharpen estimates. Hellerstein and Imbens
  (1999) did this by reweighting the micro sample with
  empirical-likelihood weights so it matches auxiliary moments, then running
  the regression of interest on the reweighted data. This is the econometric
  version of calibration.
- **Noisy constraints.** Golan, Judge and Miller (1996) extend maximum
  entropy to constraints with error by giving each constraint an error term
  with its own discrete support and maximizing joint entropy ("generalized
  maximum entropy"). Robilliard and Robinson (2003) applied cross-entropy with
  measurement error to reweight a household survey to national-accounts
  totals in Madagascar. This is a soft-target method from the same family.
- **Key assumption.** The reference distribution is right except for what
  the constraints force; the answer adds as little information as possible.
- **Fit to Tax-Data.** The entropy distance is the natural default for the
  calibration step: weights stay positive, are multiplicative in record
  characteristics, and have the information-theory and likelihood
  interpretations that support the Bayesian reading.
- **Where it breaks.** Unbounded entropy weights can still be extreme;
  combine with bounds or with the logit distance.

## 4. Reconciling aggregate accounts with stated precision

- **Stone, Champernowne and Meade (1942).** Given national-accounts
  estimates that should satisfy accounting identities but do not, adjust
  each by generalized least squares weighted by its stated variance so the
  identities hold. Precise estimates move little, rough ones move a lot.
  This is the brief's framing applied to aggregates rather than microdata.
  Byron (1978) scaled it to large social accounting matrices (record not
  verified).
- **Temporal benchmarking.** Denton (1971) adjusts a monthly or quarterly
  series to annual totals while keeping its period-to-period movement as
  smooth as possible; Chow and Lin (1971) do the same with a regression on
  related series. Dagum and Cholette (2006) is the standard reference. Both
  are used routinely by statistical agencies.
- **Fit to Tax-Data.** Two uses. First, before microdata calibration,
  reconcile the targets themselves (SOI, BEA, CBO) so that hard identities
  between targets hold and conflicts are resolved in proportion to
  precision. Second, smooth target paths across years (2016–2023 and the
  projection) so that annual files do not jump for reasons that are noise.
- **Where it breaks.** Requires variances for each source; definitional
  differences again need bias terms rather than variance.

## 5. Data fusion and statistical matching

- **What it solves.** File A (the PUF) has variables X and Y; file B (a
  survey) has X and Z; nobody is in both. Produce a file with X, Y and Z.
- **Methods.**
  - *Unconstrained matching*: each record in A gets Z from its nearest
    neighbour in B on X (hot deck), or from a model of Z given X fitted on B.
    Tax-Data's quantile regression forests and distributional random forests
    are model-based versions.
  - *Constrained matching*: every record in both files is used, and the
    weights of both files are preserved, so the marginal distribution of Z
    in the fused file equals the one in B. Rodgers (1984) evaluated it for
    tax and survey files; Blanchet, Saez and Zucman (2023) implement it as
    an optimal transport problem (a linear program that pairs records to
    minimize total distance on X while respecting both sets of weights),
    feasible at around 100,000 records per file with modern solvers (read).
  - *File concatenation* (Rubin 1986): stack the files, compute weights for
    the union, and multiply impute the missing parts; Moriarity and Scheuren
    (2003) examined it.
  - *Mass imputation* (Kim and Rao 2012; Kim, Park, Chen and Wu 2021):
    impute Z for every record in the large file from a model fitted on the
    small file, then estimate totals. Doubly robust versions combine this
    with weighting (Chen, Li and Wu 2020). Reviews: Yang and Kim (2020);
    Lohr and Raghunathan (2017); Rao (2021).
- **Key assumption: conditional independence.** Y and Z are independent once
  X is known. The two files contain no information about the joint
  distribution of Y and Z beyond what passes through X, so this cannot be
  tested from them (Kadane 1978, reprinted 2001; D'Orazio, Di Zio and Scanu
  2006, chapter 4; Rässler 2002).
- **What can be done instead.**
  - *Bounds.* Without the assumption, the files identify only a range of
    joint distributions (Fréchet bounds). D'Orazio and coauthors and Conti,
    Marella and Scanu (2016) estimate this "uncertainty region"; for many
    pairs it is wide.
  - *Auxiliary information.* A third file where Y and Z are observed
    together, or known margins of (Y, Z), pins down the joint. Fosdick,
    DeYoreo and Reiter (2016) build this into a Bayesian fusion model for
    categorical data. Rubin (1986) and the D'Orazio book discuss using prior
    values of the partial correlation.
  - *Calibrate after imputing.* Make the imputed totals match known totals
    (section 6).
- **How fusion is evaluated.** Rässler (2002) lists four levels of
  validity, from hardest to easiest: reproduce the true values for
  individuals (generally impossible); reproduce the true joint distribution;
  reproduce correlations and other joint moments; reproduce the marginal
  distributions of the imputed variables (my summary from memory of the
  book; I did not re-read it). In practice evaluation uses a held-out sample
  where all variables are observed, or a simulation where a file is split
  artificially.
- **Population differences.** Matching on X also assumes X is measured the
  same way in both files. Tax-Data's CEX consumption imputation already
  meets the case where it is not: at the same rank, CEX income runs 22–48%
  below PUF income (Tax-Data CLAUDE.md). Matching on rank rather than level
  is the usual response; Blanchet, Saez and Zucman (2023) report that their
  matching preserves ranks, and Blanchet, Flores and Morgan (2022) assume no
  re-ranking between survey and tax data.
- **Scaling.** Nearest-neighbour and model-based matching are cheap.
  Constrained matching as optimal transport is a large linear program; exact
  solutions at 200,000 × 100,000 are heavy, approximate (entropy-regularized)
  ones are routine on GPUs.
- **Fit to Tax-Data.** This is the theory behind every survey imputation in
  the pipeline. The assumption to examine for each one is which PUF-only
  variables the imputed variable might depend on beyond the conditioning set.
- **Where it breaks.** Variables whose joint distribution with PUF-only
  variables matters for tax (wealth with capital gains realizations, tips
  with wages in tipped occupations) are exactly where conditional
  independence is least credible.

## 6. Multiple imputation, sequential regression and imputation under constraints

- **Multiple imputation** (Rubin 1987; Little and Rubin 2019). Draw missing
  values several times from their predictive distribution and combine
  analyses to reflect imputation uncertainty. The brief wants a single best
  file, so the combining rules matter less; two lessons still apply.
  - *Draw, do not predict.* Filling in conditional means shrinks variance
    and breaks distributional statistics. Tax-Data already draws.
  - *Congeniality* (Meng 1994). The imputation model must include every
    relationship later analyses depend on. For a tax model this means any
    interaction the tax law uses (for example tips only for workers in
    tipped occupations, or wealth together with realized gains).
- **Sequential regression (chained equations).** Impute variables one at a
  time, each from a regression on all others, cycling until stable
  (Raghunathan, Lepkowski, Van Hoewyk and Solenberger 2001; van Buuren and
  others 2006; van Buuren and Groothuis-Oudshoorn 2011; van Buuren's open
  book *Flexible Imputation of Missing Data*). Raghunathan and coauthors
  build in restrictions to a subpopulation (impute a variable only where it
  applies) and bounds (draw from a truncated distribution), which is how
  nonnegativity and "not in universe" are handled. Tree-based versions:
  Burgette and Reiter (2010).
- **Calibrated imputation.** Beaumont (2005) adjusts imputed values so that
  imputed totals match known totals. Statistics Netherlands' handbook (de
  Waal, Pannekoek and Scholtus 2011) covers imputation that satisfies edit
  rules, including balance edits (parts summing to a total), which is the
  record-level version of Tax-Data's split-sum identities.
- **Model calibration** (Wu and Sitter 2001). Calibrate to totals of fitted
  values from a model rather than raw auxiliary variables. This is the
  survey-statistics name for "compute what each record would report, then
  calibrate to the reported total" (section 11).
- **When donors and recipients come from different populations.**
  - The standard condition is that the distribution of the imputed variable
    given the conditioning variables is the same in both populations
    (missing at random across files). If the X distribution differs but the
    conditional does not, the imputation transfers; checking means looking
    at regions of X where the donor file is thin.
  - Where the measurement differs, the template is a bridging study:
    Schenker and Raghunathan (2007) and Raghunathan, Xie, Schenker and others
    (2007) combine a large self-report survey with a smaller survey that has
    both self-report and clinical measures, model the relationship in the
    smaller one, and impute the better measure into the larger one.
  - Gelman (2007) and Si and Zhou (2021) treat weighting and calibration as
    special cases of a regression model with population cells; this is the
    Bayesian route to the same targets.
- **Fit to Tax-Data.** Keeps the current module-per-variable structure but
  says what to add: subpopulation restrictions and bounds inside the
  imputation, joint (or sequential) imputation for variables that interact
  in tax law, and a calibration step on imputed totals.
- **Where it breaks.** Chained equations need not correspond to any coherent
  joint distribution; with many variables and nonlinear models, convergence
  is checked by eye.

## 7. Small area estimation and benchmarking

- **Fay–Herriot model** (Fay and Herriot 1979). For each area a, a direct
  survey estimate y_a with known sampling variance is combined with a
  regression prediction from auxiliary data. The result is a weighted
  average, with weight on the direct estimate equal to its precision
  relative to the model's. The original application was income for small
  places, using IRS and census data. Unit-level version: Battese, Harter and
  Fuller (1988). Textbook: Rao and Molina (2015). Review: Pfeffermann
  (2013).
- **Benchmarking.** Force small-area estimates to add to a reliable national
  total. Bayesian benchmarking (Datta, Ghosh, Steorts and Maples 2011) and
  benchmarked best linear unbiased predictors (Bell, Datta and Ghosh 2013)
  minimize the loss from the benchmarking adjustment. Pfeffermann and Tiller
  (2006) benchmark state-space time-series models for monthly state
  employment to national totals.
- **Key assumption.** Areas share a common regression relationship; the
  sampling variances are known.
- **Uncertain targets.** Central to the method: each direct estimate is a
  noisy target with its own variance.
- **Fit to Tax-Data.** Not a microdata method, but the right logic for
  turning noisy year-by-year and state-by-state aggregates into the targets
  that calibration then uses, and for borrowing strength across years. The
  state-weights research workstream is a small area problem.
- **Where it breaks.** Needs variances for the direct estimates; produces
  aggregates, not records.

## 8. Synthetic population generation

- **IPF-based synthesis** (Beckman, Baggerly and McKay 1996). Fit a
  cross-table for each small area by raking a sample table to area margins,
  then draw households from the sample (U.S. census microdata) to fill it.
  Iterative proportional updating (Ye, Konduri, Pendyala and others 2009;
  link not verified) extends this to match household and person margins at
  once. Casati and others (2015) combine a hierarchical simulation with
  generalized raking.
- **Combinatorial optimization** (Williamson, Birkin and Rees 1998; Voas
  and Williamson 2000). Select an integer number of copies of each sample
  record, by simulated annealing or similar search, so the selection fits
  published small-area tables.
- **Simulation-based and sample-free synthesis.** Farooq, Bierlaire,
  Hurtubia and Flötteröd (2013) use Gibbs sampling from estimated
  conditional distributions to generate combinations not in the seed sample.
  Lenormand and Deffuant (2013) compare sample-free and sample-based methods.
- **Entropy list balancing** (PopulationSim; read). Maximum-entropy
  reweighting of a seed sample at several geographic levels at once. Each
  control can be relaxed by a factor penalized according to its
  "importance"; a cap on the ratio of final to initial weight
  (`max_expansion_factor`) bounds weights. A linear program then rounds
  weights to integers.
- **Software and reviews.** `simPop` (Templ and others 2017) simulates
  synthetic populations from survey data with calibration; Tanton (2013)
  reviews spatial microsimulation methods; Lovelace and Dumont (2016) is an
  open textbook covering IPF, GREGWT (the Australian Bureau of Statistics'
  generalized regression weighting program), simulated annealing and
  integerization; Lovelace and Ballas (2013) on integerization.
- **Fit to Tax-Data.** (a) Household–person consistency is the same problem
  as tax unit–individual consistency (spouses' ages, dependents): calibrate
  at the unit level using person-level targets aggregated to the unit
  (integrated weighting). (b) Soft controls with importance weights are a
  working, documented version of soft-target calibration. (c) Sample-free
  generation corresponds to the brief's "generate new records"; its known
  risk is implausible combinations, so generated records should be checked
  against constraints and against held-out data.
- **Where it breaks.** These methods target counts in categorical cells;
  continuous dollar amounts with long tails are not their home ground.
  Integerization is unnecessary for Tax-Simulator.

## 9. Distributional national accounts

- **Piketty, Saez and Zucman (2018)** (read). Start from tax microdata,
  add CPS-based non-filers weighted to match the adult population, then
  allocate each national-income component missing from tax data (fringe
  benefits, imputed rent, retained earnings, government spending) to
  individuals, mostly in proportion to a related reported variable, or from
  survey-estimated probabilities by cell. Wealth comes from capitalizing
  reported capital income. "By construction, the totals in these micro-files
  add up to the national accounts totals." Statistical matching to the CPS
  supplies demographic variables.
- **Auten and Splinter (2024).** Same overall frame, different allocation
  rules, notably for under-reported income and for the tax-unit concept;
  they find smaller top shares (record verified; method summary from my
  knowledge).
- **BEA distribution of personal income** (Fixler, Gindelsky and Johnson,
  2019 slides; read). CPS ASEC households are the base. Incomes of $500,000
  or more are replaced by draws from a Pareto distribution fitted to IRS
  Form 1040 microdata. Then each of about 70 personal-income components is
  scaled to its NIPA total in proportion to the related CPS variable. BEA
  examined record-level replacement with linked tax data and rejected it
  because survey and tax incomes differ substantially for the same
  households.
- **Real-time inequality** (Blanchet, Saez and Zucman 2023; read). For
  months after the last microdata year, rescale each income component to
  monthly totals, and change the wage distribution within components by
  shifting each worker's rank within cells (race × education × sex × age ×
  marital status) by the movement in average rank seen in the CPS, then
  assigning the wage for the new rank. Wage quantiles are projected from the
  Quarterly Census of Employment and Wages. Demographics come from
  optimal-transport matching to the CPS (section 5).
- **Survey plus tax tabulations** (Blanchet, Flores and Morgan 2022). Above
  an estimated "merging point" in income, reweight the survey by calibration
  so its distribution matches tax tabulations, then replace survey incomes
  with tax-consistent values, assuming no re-ranking.
- **Distributional Financial Accounts** (Batty, Bricker, Briggs and others
  2019). Allocate Financial Accounts household-sector totals using the SCF
  distribution, interpolated between survey years (content from my
  knowledge).
- **Fit to Tax-Data.** These are the reconciliations the brief's targets
  come from, and proportional scaling of values is already Tax-Data's step
  4. Two lessons: (a) proportional scaling is a modelling choice with a
  stated assumption (the gap is spread like the reported amount), and should
  be replaced by a calibrated value adjustment wherever there is evidence of
  where the gap sits; (b) rank-based value updates (real-time inequality) are
  a tested way to move shapes forward in years without microdata.
- **Where it breaks.** Allocation rules carry most of the result and differ
  across teams (the Piketty–Saez–Zucman versus Auten–Splinter debate is
  about allocation, not data).

## 10. Aging and projecting a microdata file

- **Static aging.** Keep the records, change weights to match projected
  demographics and aggregates, and uprate amounts by growth factors.
  Immervoll and others (2005) set out what static aging can and cannot
  represent: it cannot produce combinations of characteristics absent from
  the base year, and it represents changes in composition only through
  weights.
- **Hybrid aging.** Allow several growth factors that differ across the
  distribution, then reweight (Treasury, section 2.6). Treasury's stated
  reasons: reweighting preserves correlations between items on a return;
  differential growth lets the composition of income and the means of
  particular variables change over time.
- **Dynamic microsimulation.** Simulate each person's life course (births,
  deaths, marriages, labour force transitions, earnings) year by year. Li and
  O'Donoghue (2012) survey the models (U.S. examples include DYNASIM, MINT
  and CBO's long-term model). TPC uses DYNASIM3 projections as targets for
  2040–2090 rather than running dynamic simulation itself.
- **Fit to Tax-Data.** Tax-Data's projection is static aging with one growth
  factor per dollar variable. The literature supports (a) differential growth
  factors for wages and other incomes by position in the distribution, set
  from CBO's projected AGI distribution near term and from projected shares
  long term; (b) reweighting to demographic projections, with soft targets
  so far-future targets do not force extreme weights; (c) for 2040 and
  beyond, targets from a dynamic model or from long-run shares rather than
  more reweighting.
- **Where it breaks.** Over 80 years, reweighting a 2015 file to a very
  different age structure concentrates weight on a shrinking set of records
  (for example elderly records in 2060); effective sample size within age
  groups should be tracked by year.

## 11. Censored tables, coarse tabulations and deconvolution

### 11.1 Censored and truncated published data

- **Classical models.** Tobin (1958) for a variable observed only above a
  threshold; Heckman (1979) for selection into being observed; Amemiya
  (1984) surveys the family. These fit a parametric distribution to the
  observed part and infer the hidden part from the shape.
- **The itemized-deduction case.** Before and after the 2017 law, SOI
  tabulates itemized deductions only for itemizers. Even in the 2015 PUF,
  deductible expenses of non-itemizers (below the 2015 standard deduction)
  are not observed. So the lower part of the distribution of deductible
  expenses is censored in every IRS year; post-2017 tables censor more of
  it. Treasury's model handles non-itemizers by imputing amounts from
  surveys and then assigning each a total between half and the full
  standard deduction, scaling down imputed items pro rata if needed (TP-12;
  read). That is a constraint-respecting imputation, not an estimate from the
  tables.
- **Simulating the reporting process.** With latent deductible expenses on
  each record, the 2019 target "itemized deductions claimed" equals
  Σ_i w_i × D_i × 1{record i itemizes under 2019 law}. When only weights
  change, this is linear in the weights and enters calibration like any
  other target (model calibration, section 6). When the latent amounts D_i
  are also being adjusted, the indicator makes the target jump as a record
  crosses the itemization threshold. Econometrics handles this by simulated
  method of moments (McFadden 1989) and indirect inference (Gourieroux,
  Monfort and Renault 1993): choose model parameters so that simulated
  statistics, computed by running the reporting process, match the published
  ones. In practice the indicator can also be replaced by a smooth
  approximation inside the optimization.
- **Fit to Tax-Data.** The brief's plan matches this literature. The binding
  question is identification: the tables reveal the share and amount above
  the threshold, so the shape below it comes from surveys (CEX, SCF, ACS for
  housing costs) or from an assumed functional form, not from SOI.

### 11.2 Distributions from grouped data

- **Generalized Pareto interpolation** (Blanchet, Fournier and Piketty 2022;
  `gpinter` tools at wid.world). The "inverted Pareto coefficient" b(p) is
  the ratio of mean income above rank p to the income at rank p. It is
  constant for an exact Pareto tail and varies smoothly elsewhere. The method
  interpolates b(p) and the quantile function through bracket thresholds,
  population shares and bracket means, recovering a smooth full
  distribution, and was shown to beat standard interpolation on
  near-complete tax tabulations (search-result summary and record verified;
  method detail from my knowledge). Blanchet, Garbinti, Goupille-Lebret and
  Martínez-Toledano (2018) apply it.
- **Parametric fits to grouped data.** The generalized beta of the second
  kind (McDonald 1984) is the standard flexible income distribution.
  Chotikapanich, Griffiths and Rao (2007) and Hajargasht, Griffiths, Brice,
  Rao and Chotikapanich (2012) estimate such distributions from group
  shares and means by generalized method of moments, with standard errors.
  von Hippel, Scarpino and Holas (2016) compare methods for binned incomes.
- **Deconvolution.** Recovering a distribution from noisy observations of
  it. Efron (2016) gives an empirical Bayes method. Relevant to the PUF's
  disclosure blurring only if the noise process is known.
- **Fit to Tax-Data.** Publication 1304 tables are bracket counts and
  amounts. Interpolating them gives (a) quantile-based targets for each year
  2016–2023, (b) a check on internal consistency, and (c) a way to describe
  the top tail between published brackets. In calibration, bracket counts and
  amounts can be used directly as linear targets; interpolation adds value
  where brackets are wide or where tables change bracket boundaries between
  years.
- **Where it breaks.** Interpolation between brackets is an assumption about
  smoothness; it adds no information inside a bracket.

## 12. How the strands relate

The methods in this note sort into three operations:

| Operation | Changes | Methods |
|---|---|---|
| Reweight | weights | raking, calibration (any distance), entropy balancing, empirical likelihood, linear programs, population synthesis by list balancing |
| Revalue | record values | uprating, differential growth factors, pro-rata scaling to totals (distributional national accounts), rank shifting, calibrated imputation |
| Add information | new variables or records | statistical matching, mass and multiple imputation, synthetic records |

A single objective can cover reweighting and revaluing together: a distance
from the prior for weights, a distance from the prior for values, and a
precision-weighted penalty on missed targets, subject to the hard
constraints. The literature treats the weight part thoroughly. The combined
problem appears in pieces (calibrated imputation; the
reweight-then-replace method of Blanchet, Flores and Morgan; Treasury's
hybrid aging) but I did not find a single published formulation for tax
microdata.

## 13. Strands that matter most for Tax-Data

1. **Soft-target calibration with a bounded entropy distance** (sections
   2.2–2.4, 3, 4). It replaces the linear program, has a direct reading as
   the brief's Bayesian update, has variance theory for estimated targets
   (Isaki–Tsay–Fuller; Dever–Valliant), and is what national statistical
   offices and Treasury already do in its exact-target form. What Tax-Data
   must supply is a stated error for every target and a decision about which
   few targets are hard. Report effective sample size overall and at the
   top after each run.

2. **Targets defined by simulating the reporting process, with grouped-data
   interpolation** (sections 6, 11). Model calibration makes censored and
   law-dependent tables ordinary linear targets as long as only weights
   change; generalized Pareto interpolation turns SOI tabulations into
   consistent distributional targets for 2016–2023. Where record values also
   change, simulated method of moments is the fallback. Identification of
   the censored part comes from surveys or assumptions, and should be said
   so.

3. **Statistical-matching theory for the imputations** (sections 5, 6). For
   each imputed variable, name the conditional independence assumption,
   look for auxiliary data that observe the pair jointly, impute variables
   that interact in tax law jointly, restrict and bound imputations inside
   the model, and calibrate imputed totals to external aggregates. Evaluate
   at the level of joint distributions, not only margins.

4. **Hybrid aging with smoothing across years** (sections 4, 9, 10).
   Change values as well as weights: differential growth factors (Treasury)
   or rank shifting within cells (real-time inequality) let income
   distributions change shape after 2015; reweighting handles composition.
   Smooth target paths across years (Denton-type benchmarking or state-space
   models) and let far-future targets be soft. For the long run, take
   distributional targets from a dynamic model or projected shares, as TPC
   does with DYNASIM3.

## 14. What I looked for and could not find or verify

- **CBO's method.** The CBO page for Perese (2016) and the presentation PDF
  returned "forbidden" to both the fetch tool and direct download. I read
  only the archived summary, which says CBO was re-examining its method.
- **JCT's method.** JCX-48-23 could not be opened (access refused). JCX-75-15
  (2015) also exists; not opened.
- **A U.S. tax model that treats targets as noisy.** I found none in the
  documents I read (Treasury TP-12, TPC 2021, PSL `taxdata` README). A search
  snippet attributed a gradient-descent loss-based reweighting to a model
  HM Treasury uses (PolicyEngine UK); I did not open the source.
- **HMRC's projection of the Survey of Personal Incomes.** A search snippet
  (from a Scottish Government publication) said SPI data are rolled forward
  by uprating with no adjustment for demographic change. I did not open it.
- **Census SIPP Synthetic Beta**, a sequential-regression synthesis of SIPP
  linked to IRS and SSA records: the page I tried returned "not found".
- **Not checked after the web-search budget ran out:** iterative
  proportional updating (Ye and others 2009), Beaumont and Bocci (2008) on
  ridge calibration, Chambers (1996) on robust case weighting, Potter on
  weight trimming, Elliott and Little (2000) on alternatives to trimming,
  Kish (1965) as the source of effective sample size, Moriarity and
  Scheuren (2001) in the Journal of Official Statistics, Byron (1978)
  (the DOI I tried resolved to a different paper), and Cowell and Mehta
  (1982).
- **Rässler's four validity levels** are summarized from memory of the
  book.

## References

Calibration and weighting

- Bardsley, P. and R. Chambers (1984). Multipurpose estimation from
  unbalanced samples. *Applied Statistics* 33(3): 290–299.
  https://doi.org/10.2307/2347706 [record]
- Barratt, S., G. Angeris and S. Boyd (2021). Optimal representative sample
  weighting. *Statistics and Computing* 31.
  https://doi.org/10.1007/s11222-021-10001-1 [record]
- Berger, Y., J. Muñoz and E. Rancourt (2009). Variance estimation of survey
  estimates calibrated on estimated control totals. *Computational
  Statistics and Data Analysis* 53: 2596–2604.
  https://doi.org/10.1016/j.csda.2008.12.011 [record]
- Chen, J., R. Sitter and C. Wu (2002). Using empirical likelihood methods to
  obtain range restricted weights in regression estimators for surveys.
  *Biometrika* 89(1): 230–237. https://doi.org/10.1093/biomet/89.1.230
  [record]
- Chen, Q., M. Elliott, D. Haziza and others (2017). Approaches to improving
  survey-weighted estimates. *Statistical Science* 32(2).
  https://doi.org/10.1214/17-STS609 [record]
- Deming, W. E. and F. Stephan (1940). On a least squares adjustment of a
  sampled frequency table when the expected marginal totals are known.
  *Annals of Mathematical Statistics* 11(4): 427–444.
  https://doi.org/10.1214/aoms/1177731829 [record]
- Dever, J. and R. Valliant (2016). General regression estimation adjusted
  for undercoverage and estimated control totals. *Journal of Survey
  Statistics and Methodology* 4(3): 289–318.
  https://doi.org/10.1093/jssam/smw001 [record]
- Deville, J.-C. and C.-E. Särndal (1992). Calibration estimators in survey
  sampling. *Journal of the American Statistical Association* 87: 376–382.
  https://doi.org/10.1080/01621459.1992.10475217 [record]
- Deville, J.-C., C.-E. Särndal and O. Sautory (1993). Generalized raking
  procedures in survey sampling. *Journal of the American Statistical
  Association* 88: 1013–1020. https://doi.org/10.1080/01621459.1993.10476369
  [record]
- Fienberg, S. (1970). An iterative procedure for estimation in contingency
  tables. *Annals of Mathematical Statistics* 41(3): 907–917.
  https://doi.org/10.1214/aoms/1177696968 [record]
- Guggemos, F. and Y. Tillé (2010). Penalized calibration in survey
  sampling: design-based estimation assisted by mixed models. *Journal of
  Statistical Planning and Inference* 140: 3199–3212.
  https://doi.org/10.1016/j.jspi.2010.04.010 [record]
- Haziza, D. and J.-F. Beaumont (2017). Construction of weights in surveys:
  a review. *Statistical Science* 32(2). https://doi.org/10.1214/16-STS608
  [record]
- Ireland, C. T. and S. Kullback (1968). Contingency tables with given
  marginals. *Biometrika* 55(1): 179–188.
  https://doi.org/10.1093/biomet/55.1.179 [record]
- Isaki, C. T., J. H. Tsay and W. A. Fuller (2004). Weighting sample data
  subject to independent controls. *Survey Methodology* 30(1).
  https://www150.statcan.gc.ca/n1/en/catalogue/12-001-X20040016992 [read:
  abstract]
- Kolenikov, S. (2014). Calibrating survey data using iterative
  proportional fitting (raking). *Stata Journal* 14(1): 22–59.
  https://doi.org/10.1177/1536867X1401400104 [record]
- Kott, P. (2016). Calibration weighting in survey sampling. *WIREs
  Computational Statistics* 8(1): 39–53. https://doi.org/10.1002/wics.1374
  [record]
- Lumley, T. (2004). Analysis of complex survey samples. *Journal of
  Statistical Software* 9(8). https://doi.org/10.18637/jss.v009.i08 [record]
- Rao, J. N. K. and A. C. Singh (1997). A ridge-shrinkage method for
  range-restricted weight calibration in survey sampling. Statistics Canada
  working paper. https://publications.gc.ca/site/eng/9.841397/publication.html
  [record: catalogue entry seen in search results]
- Särndal, C.-E. (2007). The calibration approach in survey theory and
  practice. *Survey Methodology* 33(2): 99–119.
  https://www150.statcan.gc.ca/n1/en/catalogue/12-001-X200700210488 [record]
- Särndal, C.-E. and S. Lundström (2005). *Estimation in Surveys with
  Nonresponse*. Wiley. https://doi.org/10.1002/0470011351 [record]
- Si, Y. and P. Zhou (2021). Bayes-raking: Bayesian finite population
  inference with known margins. *Journal of Survey Statistics and
  Methodology* 9(4): 833–855. https://doi.org/10.1093/jssam/smaa008 [record]
- Singh, A. C. and C. A. Mohl (1996). Understanding calibration estimators in
  survey sampling. *Survey Methodology* 22(2).
  https://www150.statcan.gc.ca/n1/en/catalogue/12-001-X19960022973 [record]
- Valliant, R., J. Dever and F. Kreuter (2018). *Practical Tools for
  Designing and Weighting Survey Samples*, 2nd ed. Springer.
  https://doi.org/10.1007/978-3-319-93632-1 [record]
- Lound, C. and P. Broad (2014). Initial review of the Family Resources
  Survey weighting scheme. ONS Methodology Advisory Service for DWP.
  https://www.gov.uk/government/publications/initial-review-of-the-family-resources-survey-weighting-scheme/initial-review-of-the-family-resources-survey-weighting-scheme
  [read]

Entropy, empirical likelihood, econometrics of moment restrictions

- Chan, K. C. G., S. C. P. Yam and Z. Zhang (2016). Globally efficient
  non-parametric inference of average treatment effects by empirical
  balancing calibration weighting. *JRSS B* 78(3): 673–700.
  https://doi.org/10.1111/rssb.12129 [record]
- Chen, J. and J. Qin (1993). Empirical likelihood estimation for finite
  populations and the effective usage of auxiliary information. *Biometrika*
  80(1): 107–116. https://doi.org/10.1093/biomet/80.1.107 [record]
- Csiszár, I. (1975). I-divergence geometry of probability distributions and
  minimization problems. *Annals of Probability* 3(1).
  https://doi.org/10.1214/aop/1176996454 [record]
- Golan, A., G. Judge and D. Miller (1996). *Maximum Entropy Econometrics:
  Robust Estimation with Limited Data*. Wiley.
  https://are.berkeley.edu/~jperloff/gme/gjmbook.htm [record]
- Hainmueller, J. (2012). Entropy balancing for causal effects. *Political
  Analysis* 20(1): 25–46. https://doi.org/10.1093/pan/mpr025 [record]
- Hellerstein, J. and G. Imbens (1999). Imposing moment restrictions from
  auxiliary data by weighting. *Review of Economics and Statistics* 81(1):
  1–14. https://doi.org/10.1162/003465399557860 [record]
- Imbens, G. and T. Lancaster (1994). Combining micro and macro data in
  microeconometric models. *Review of Economic Studies* 61(4): 655–680.
  https://doi.org/10.2307/2297913 [record]
- Imbens, G., R. Spady and P. Johnson (1998). Information theoretic
  approaches to inference in moment condition models. *Econometrica* 66(2):
  333–357. https://doi.org/10.2307/2998561 [record]
- Jaynes, E. T. (1957). Information theory and statistical mechanics.
  *Physical Review* 106: 620–630. https://doi.org/10.1103/PhysRev.106.620
  [record]
- Kitamura, Y. and M. Stutzer (1997). An information-theoretic alternative
  to generalized method of moments estimation. *Econometrica* 65(4): 861–874.
  https://doi.org/10.2307/2171942 [record]
- Kullback, S. and R. Leibler (1951). On information and sufficiency.
  *Annals of Mathematical Statistics* 22(1): 79–86.
  https://doi.org/10.1214/aoms/1177729694 [record]
- Owen, A. (1988). Empirical likelihood ratio confidence intervals for a
  single functional. *Biometrika* 75(2): 237–249.
  https://doi.org/10.1093/biomet/75.2.237 [record]
- Owen, A. (2001). *Empirical Likelihood*. Chapman and Hall/CRC.
  https://doi.org/10.1201/9781420036152 [record]
- Qin, J. and J. Lawless (1994). Empirical likelihood and general estimating
  equations. *Annals of Statistics* 22(1).
  https://doi.org/10.1214/aos/1176325370 [record]
- Robilliard, A.-S. and S. Robinson (2003). Reconciling household surveys
  and national accounts data using a cross entropy estimation method.
  *Review of Income and Wealth* 49(3): 395–406.
  https://doi.org/10.1111/1475-4991.00094 [record]
- Wang, Y. and J. Zubizarreta (2020). Minimal dispersion approximately
  balancing weights: asymptotic properties and practical considerations.
  *Biometrika*.
  https://doi.org/10.1093/biomet/asz050 [record]
- Zhao, Q. and D. Percival (2017). Entropy balancing is doubly robust.
  *Journal of Causal Inference* 5(1). https://doi.org/10.1515/jci-2016-0010
  [record]
- Zubizarreta, J. (2015). Stable weights that balance covariates for
  estimation with incomplete outcome data. *JASA* 110: 910–922.
  https://doi.org/10.1080/01621459.2015.1023805 [record]

Accounts balancing and temporal benchmarking

- Chow, G. and A.-L. Lin (1971). Best linear unbiased interpolation,
  distribution, and extrapolation of time series by related series. *Review
  of Economics and Statistics* 53(4): 372–375.
  https://doi.org/10.2307/1928739 [record]
- Dagum, E. B. and P. Cholette (2006). *Benchmarking, Temporal
  Distribution, and Reconciliation Methods for Time Series*. Springer.
  https://doi.org/10.1007/0-387-35439-5 [record]
- Denton, F. (1971). Adjustment of monthly or quarterly series to annual
  totals: an approach based on quadratic minimization. *JASA* 66: 99–102.
  https://doi.org/10.1080/01621459.1971.10482227 [record]
- Stone, R., D. Champernowne and J. Meade (1942). The precision of national
  income estimates. *Review of Economic Studies* 9(2): 111–125.
  https://doi.org/10.2307/2967664 [record]
- Byron, R. (1978). The estimation of large social account matrices.
  *JRSS A* 141(3). No verified link. [unverified]

Statistical matching and data integration

- Blanchet, T., E. Saez and G. Zucman (2023). Real-time inequality. Working
  paper, November 2023. https://gabriel-zucman.eu/files/BSZ2023.pdf [read]
- Chen, Y., P. Li and C. Wu (2020). Doubly robust inference with
  nonprobability survey samples. *JASA* 115: 2011–2021.
  https://doi.org/10.1080/01621459.2019.1677241 [record]
- Conti, P. L., D. Marella and M. Scanu (2016). Statistical matching analysis
  for complex survey data with applications. *JASA* 111: 1715–1725.
  https://doi.org/10.1080/01621459.2015.1112803 [record]
- D'Orazio, M., M. Di Zio and M. Scanu (2006). *Statistical Matching:
  Theory and Practice*. Wiley. https://doi.org/10.1002/0470023554 [record]
- Fosdick, B., M. DeYoreo and J. Reiter (2016). Categorical data fusion
  using auxiliary information. *Annals of Applied Statistics* 10(4).
  https://doi.org/10.1214/16-AOAS925 [record]
- Kadane, J. (1978). Some statistical problems in merging data files. In
  Treasury, *Compendium of Tax Research 1978*; reprinted in *Journal of
  Official Statistics* 17 (2001): 423–433. Compendium:
  https://home.treasury.gov/system/files/131/Report-Compendium-1978-Part6.pdf
  [record: seen in search results, not opened]
- Kim, J. K. and J. N. K. Rao (2012). Combining data from two independent
  surveys: a model-assisted approach. *Biometrika* 99(1): 85–100.
  https://doi.org/10.1093/biomet/asr063 [record]
- Kim, J. K., S. Park, Y. Chen and C. Wu (2021). Combining non-probability
  and probability survey samples through mass imputation. *JRSS A* 184:
  941–963. https://doi.org/10.1111/rssa.12696 [record]
- Lohr, S. and T. Raghunathan (2017). Combining survey data with other data
  sources. *Statistical Science* 32(2). https://doi.org/10.1214/16-STS584
  [record]
- Moriarity, C. and F. Scheuren (2003). A note on Rubin's statistical
  matching using file concatenation with adjusted weights and multiple
  imputations. *JBES* 21(1): 65–73.
  https://doi.org/10.1198/073500102288618766 [record]
- Rao, J. N. K. (2021). On making valid inferences by integrating data from
  surveys and other sources. *Sankhya B* 83: 242–272.
  https://doi.org/10.1007/s13571-020-00227-w [record]
- Rässler, S. (2002). *Statistical Matching: A Frequentist Theory, Practical
  Applications, and Alternative Bayesian Approaches*. Lecture Notes in
  Statistics 168, Springer. https://doi.org/10.1007/978-1-4613-0053-3
  [record]
- Rodgers, W. (1984). An evaluation of statistical matching. *JBES* 2(1):
  91–102. https://doi.org/10.1080/07350015.1984.10509373 [record]
- Rubin, D. (1986). Statistical matching using file concatenation with
  adjusted weights and multiple imputations. *JBES* 4(1): 87–94.
  https://doi.org/10.1080/07350015.1986.10509497 [record]
- Yang, S. and J. K. Kim (2020). Statistical data integration in survey
  sampling: a review. *Japanese Journal of Statistics and Data Science* 3:
  625–650. https://doi.org/10.1007/s42081-020-00093-w [record]

Imputation

- Beaumont, J.-F. (2005). Calibrated imputation in surveys under a
  quasi-model-assisted approach. *JRSS B* 67(3): 445–458.
  https://doi.org/10.1111/j.1467-9868.2005.00511.x [record]
- Burgette, L. and J. Reiter (2010). Multiple imputation for missing data
  via sequential regression trees. *American Journal of Epidemiology* 172:
  1070–1076. https://doi.org/10.1093/aje/kwq260 [record]
- de Waal, T., J. Pannekoek and S. Scholtus (2011). *Handbook of Statistical
  Data Editing and Imputation*. Wiley. https://doi.org/10.1002/9780470904848
  [record]
- Gelman, A. (2007). Struggles with survey weighting and regression
  modeling. *Statistical Science* 22(2).
  https://doi.org/10.1214/088342306000000691 [record]
- Little, R. and D. Rubin (2019). *Statistical Analysis with Missing Data*,
  3rd ed. Wiley. https://doi.org/10.1002/9781119482260 [record]
- Meng, X.-L. (1994). Multiple-imputation inferences with uncongenial
  sources of input. *Statistical Science* 9(4).
  https://doi.org/10.1214/ss/1177010269 [record]
- Raghunathan, T., J. Lepkowski, J. Van Hoewyk and P. Solenberger (2001). A
  multivariate technique for multiply imputing missing values using a
  sequence of regression models. *Survey Methodology* 27(1): 85–95.
  https://www150.statcan.gc.ca/n1/en/catalogue/12-001-X20010015857 [record;
  search-result abstract describes the subpopulation and bounds features]
- Raghunathan, T., D. Xie, N. Schenker and others (2007). Combining
  information from two surveys to estimate county-level prevalence rates of
  cancer risk factors and screening. *JASA* 102: 474–486.
  https://doi.org/10.1198/016214506000001293 [record]
- Rubin, D. (1987). *Multiple Imputation for Nonresponse in Surveys*. Wiley.
  https://doi.org/10.1002/9780470316696 [record]
- Schenker, N. and T. Raghunathan (2007). Combining information from
  multiple surveys to enhance estimation of measures of health. *Statistics
  in Medicine* 26: 1802–1811. https://doi.org/10.1002/sim.2801 [record]
- van Buuren, S., J. Brand, C. Groothuis-Oudshoorn and D. Rubin (2006).
  Fully conditional specification in multivariate imputation. *Journal of
  Statistical Computation and Simulation* 76: 1049–1064.
  https://doi.org/10.1080/10629360600810434 [record]
- van Buuren, S. and K. Groothuis-Oudshoorn (2011). mice: multivariate
  imputation by chained equations in R. *Journal of Statistical Software*
  45(3). https://doi.org/10.18637/jss.v045.i03 [record]
- van Buuren, S. *Flexible Imputation of Missing Data*, 2nd ed., open online
  book. https://stefvanbuuren.name/fimd/ [read: contents]
- Wu, C. and R. Sitter (2001). A model-calibration approach to using
  complete auxiliary information from survey data. *JASA* 96: 185–193.
  https://doi.org/10.1198/016214501750333054 [record]

Small area estimation

- Battese, G., R. Harter and W. Fuller (1988). An error-components model for
  prediction of county crop areas using survey and satellite data. *JASA* 83:
  28–36. https://doi.org/10.1080/01621459.1988.10478561 [record]
- Bell, W., G. Datta and M. Ghosh (2013). Benchmarking small area
  estimators. *Biometrika* 100(1): 189–202.
  https://doi.org/10.1093/biomet/ass063 [record]
- Datta, G., M. Ghosh, R. Steorts and J. Maples (2011). Bayesian
  benchmarking with applications to small area estimation. *TEST* 20:
  574–588. https://doi.org/10.1007/s11749-010-0218-y [record]
- Fay, R. and R. Herriot (1979). Estimates of income for small places: an
  application of James-Stein procedures to census data. *JASA* 74: 269–277.
  https://doi.org/10.1080/01621459.1979.10482505 [record]
- Pfeffermann, D. (2013). New important developments in small area
  estimation. *Statistical Science* 28(1). https://doi.org/10.1214/12-STS395
  [record]
- Pfeffermann, D. and R. Tiller (2006). Small-area estimation with
  state–space models subject to benchmark constraints. *JASA* 101:
  1387–1397. https://doi.org/10.1198/016214506000000591 [record]
- Rao, J. N. K. and I. Molina (2015). *Small Area Estimation*, 2nd ed.
  Wiley. https://doi.org/10.1002/9781118735855 [record]

Synthetic populations

- Beckman, R., K. Baggerly and M. McKay (1996). Creating synthetic baseline
  populations. *Transportation Research A* 30(6): 415–429.
  https://doi.org/10.1016/0965-8564(96)00004-3 [record]
- Casati, D., K. Müller, P. Fourie, A. Erath and K. Axhausen (2015).
  Synthetic population generation by combining a hierarchical,
  simulation-based approach with reweighting by generalized raking.
  *Transportation Research Record* 2493: 107–116.
  https://doi.org/10.3141/2493-12 [record]
- Farooq, B., M. Bierlaire, R. Hurtubia and G. Flötteröd (2013). Simulation
  based population synthesis. *Transportation Research B* 58: 243–263.
  https://doi.org/10.1016/j.trb.2013.09.012 [record]
- Lenormand, M. and G. Deffuant (2013). Generating a synthetic population of
  individuals in households: sample-free vs sample-based methods. *JASSS*
  16(4). https://doi.org/10.18564/jasss.2319 [record]
- Lovelace, R. and D. Ballas (2013). "Truncate, replicate, sample": a method
  for creating integer weights for spatial microsimulation. *Computers,
  Environment and Urban Systems* 41: 1–11.
  https://doi.org/10.1016/j.compenvurbsys.2013.03.004 [record]
- Lovelace, R. and M. Dumont (2016). *Spatial Microsimulation with R*. CRC
  Press; open online edition.
  https://spatial-microsim-book.robinlovelace.net/ [read: contents]
- PopulationSim documentation (ActivitySim consortium).
  https://activitysim.github.io/populationsim/ and
  https://activitysim.github.io/populationsim/application_configuration.html
  [read]
- Tanton, R. (2013). A review of spatial microsimulation methods.
  *International Journal of Microsimulation* 7(1): 4–25.
  https://doi.org/10.34196/ijm.00092 [record]
- Templ, M., B. Meindl, A. Kowarik and O. Dupriez (2017). Simulation of
  synthetic complex data: the R package simPop. *Journal of Statistical
  Software* 79(10). https://doi.org/10.18637/jss.v079.i10 [record]
- Voas, D. and P. Williamson (2000). An evaluation of the combinatorial
  optimisation approach to the creation of synthetic microdata.
  *International Journal of Population Geography* 6: 349–366.
  https://doi.org/10.1002/1099-1220(200009/10)6:5%3C349::AID-IJPG196%3E3.0.CO;2-5
  [record]
- Williamson, P., M. Birkin and P. Rees (1998). The estimation of population
  microdata by using data from small area statistics and samples of
  anonymised records. *Environment and Planning A* 30: 785–816.
  https://doi.org/10.1068/a300785 [record]
- Ye, X., K. Konduri, R. Pendyala and others (2009). A methodology to match
  distributions of both household and person attributes in the generation
  of synthetic populations. Transportation Research Board annual meeting
  paper. No verified link. [unverified]

Distributional national accounts

- Auten, G. and D. Splinter (2024). Income inequality in the United States:
  using tax data to measure long-term trends. *Journal of Political Economy*
  132(7): 2179–2227. https://doi.org/10.1086/728741 [record]
- Batty, M., J. Bricker, J. Briggs and others (2019). Introducing the
  Distributional Financial Accounts of the United States. FEDS 2019-017.
  https://doi.org/10.17016/FEDS.2019.017 [record]
- Blanchet, T., I. Flores and M. Morgan (2022). The weight of the rich:
  improving surveys using tax data. *Journal of Economic Inequality* 20:
  119–150. https://doi.org/10.1007/s10888-021-09509-3 [record; abstract seen
  in search results]
- Chancel, L., I. Flores, R. Moshrif, G. Nievas and T. Piketty (coordinators)
  (2025). Distributional National Accounts (DINA) Guidelines 2025. World
  Inequality Lab.
  https://wid.world/document/distributional-national-accounts-dina-guidelines-2025-methods-and-concepts-used-in-the-world-inequality-database
  [record: seen in search results]
- Fixler, D., M. Gindelsky and D. Johnson (2019). Improving the measure of
  the distribution of personal income. BEA Advisory Committee slides, May 10,
  2019.
  https://www.bea.gov/sites/default/files/2019-05/Improving-the-Measure-of-Distribution-of-Personal-Income.pdf
  [read]
- Piketty, T., E. Saez and G. Zucman (2018). Distributional national
  accounts: methods and estimates for the United States. *Quarterly Journal
  of Economics* 133(2): 553–609. https://doi.org/10.1093/qje/qjx043 [read;
  author copy https://gabriel-zucman.eu/files/PSZ2018QJE.pdf]

Aging, projection and tax-model practice

- Cai, L., J. Creedy and G. Kalb (2006). Accounting for population ageing in
  tax microsimulation modelling by survey reweighting. *Australian Economic
  Papers* 45(1): 18–37. https://doi.org/10.1111/j.1467-8454.2006.00275.x
  [record]
- Creedy, J. (2004). Survey reweighting for tax microsimulation modelling.
  *Research on Economic Inequality* 12: 229–249.
  https://doi.org/10.1016/S1049-2585(04)12009-7 [record]
- Fisher, R. and E. Lin (2015). Re-weighting to produce state-level tax
  microsimulation estimates. Treasury OTA Technical Paper 6.
  https://home.treasury.gov/system/files/131/TP-6.pdf [read in part]
- Gillette, R., S. Anantham, W. Boning and others (2023). U.S. Treasury
  Individual Income Tax Model. Treasury OTA Technical Paper 12.
  https://home.treasury.gov/system/files/131/TP-12.pdf [read: sections 3–4]
- Immervoll, H., K. Lindström, E. Mustonen, M. Riihelä and H. Viitamäki
  (2005). Static data "ageing" techniques: accounting for population changes
  in tax-benefit microsimulation models. EUROMOD Working Paper EM7/05.
  https://www.iser.essex.ac.uk/research/publications/working-papers/euromod/em7-05.pdf
  [record]
- Joint Committee on Taxation (2023). Estimating changes in the federal
  individual income tax: a description of the individual tax model for 2023.
  JCX-48-23. https://www.jct.gov/publications/2023/jcx-48-23/ [record; not
  opened]
- Li, J. and C. O'Donoghue (2012). A survey of dynamic microsimulation
  models: uses, model structure and methodology. *International Journal of
  Microsimulation* 6(2): 3–55. https://doi.org/10.34196/ijm.00082 [record]
- Office for Budget Responsibility (2018). Welfare trends report, January
  2018, box "DWP's policy simulation model and the Family Resources Survey".
  https://obr.uk/box/dwps-policy-simulation-model-and-the-family-resources-survey/
  [read]
- Page, B. (2021). TPC's 2021 tax model technical updates. Tax Policy
  Center.
  https://www.taxpolicycenter.org/sites/default/files/publication/163341/tpc-model-technical-update.pdf
  [read]
- Perese, K. (2016). Projection and alignment methods for static
  microsimulation models. CBO presentation, November 2, 2016.
  https://www.cbo.gov/publication/52147 (archived copy read:
  https://webharvest.gov/congress114th/20161109023857/https://www.cbo.gov/publication/52147)
  [record; slides not opened]
- PSL `taxdata` repository README.
  https://github.com/PSLmodels/taxdata [read]
- Sutherland, H. and F. Figari (2012). EUROMOD: the European Union
  tax-benefit microsimulation model. *International Journal of
  Microsimulation* 6: 4–26. https://doi.org/10.34196/ijm.00075 [record]

Censored data, grouped data and simulation-based estimation

- Amemiya, T. (1984). Tobit models: a survey. *Journal of Econometrics* 24:
  3–61. https://doi.org/10.1016/0304-4076(84)90074-5 [record]
- Blanchet, T., J. Fournier and T. Piketty (2022). Generalized Pareto
  curves: theory and applications. *Review of Income and Wealth* 68(1):
  263–288. https://doi.org/10.1111/roiw.12510 [record]; tools at
  https://wid.world/gpinter [unverified: link given in the paper's abstract
  as summarized by search]
- Blanchet, T., B. Garbinti, J. Goupille-Lebret and C. Martínez-Toledano
  (2018). Applying generalized Pareto curves to inequality analysis. *AEA
  Papers and Proceedings* 108: 114–118.
  https://doi.org/10.1257/pandp.20181075 [record]
- Chotikapanich, D., W. Griffiths and D. S. P. Rao (2007). Estimating and
  combining national income distributions using limited data. *JBES* 25(1):
  97–109. https://doi.org/10.1198/073500106000000224 [record]
- Efron, B. (2016). Empirical Bayes deconvolution estimates. *Biometrika*
  103(1): 1–20. https://doi.org/10.1093/biomet/asv068 [record]
- Gourieroux, C., A. Monfort and E. Renault (1993). Indirect inference.
  *Journal of Applied Econometrics* 8: S85–S118.
  https://doi.org/10.1002/jae.3950080507 [record]
- Hajargasht, G., W. Griffiths, J. Brice, D. S. P. Rao and D. Chotikapanich
  (2012). Inference for income distributions using grouped data. *JBES*
  30(4): 563–575. https://doi.org/10.1080/07350015.2012.707590 [record]
- Heckman, J. (1979). Sample selection bias as a specification error.
  *Econometrica* 47(1): 153–161. https://doi.org/10.2307/1912352 [record]
- McDonald, J. (1984). Some generalized functions for the size distribution
  of income. *Econometrica* 52(3): 647–663. https://doi.org/10.2307/1913469
  [record]
- McFadden, D. (1989). A method of simulated moments for estimation of
  discrete response models without numerical integration. *Econometrica*
  57(5): 995–1026. https://doi.org/10.2307/1913621 [record]
- Tobin, J. (1958). Estimation of relationships for limited dependent
  variables. *Econometrica* 26(1): 24–36. https://doi.org/10.2307/1907382
  [record]
- von Hippel, P., S. Scarpino and I. Holas (2016). Robust estimation of
  inequality from binned incomes. *Sociological Methodology* 46: 212–251.
  https://doi.org/10.1177/0081175015599807 [record]
