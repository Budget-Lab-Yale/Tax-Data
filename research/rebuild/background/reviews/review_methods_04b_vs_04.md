---
title: "Review of the Codex first-principles report (04b), compared with report 04"
role: review
workstream: rebuild
status: current
updated: 2026-10-07
true_as_of: 2026-10-07
sot: research/rebuild/BRIEF.md
supersedes: []
superseded_by: null
---

# Review of report 04b, compared with report 04

Reviewed: `research/rebuild/background/04b_methods_first_principles_codex.md`
(written by OpenAI's model through the Codex CLI), compared with
`research/rebuild/background/04_methods_first_principles.md` (written by a
Claude agent). Code claims were checked against the repository at commit
`ba48f970acd005b1f1f224622bf4dfebc476075d`, the commit 04b cites, which was
also the checked-out HEAD. Mathematical claims were checked by derivation,
and five of them also by a small numerical test run as a Slurm job (method
and results in the section "Numerical checks"). A separate reviewer covers 04
on its own merits; 04 appears here only where the comparison needs it.

## Summary

**Claims graded in 04b: 59.** CONFIRMED 50, OVERSTATED 4, UNSUPPORTED 3,
UNCHECKABLE 2, WRONG 0. I found nothing false in 04b's mathematics or in its
description of the code. Its weaknesses are overstatement and vagueness.

1. **04b's mathematics is correct and more carefully conditioned than 04's.**
   Every derivation I checked holds: the dual (the smaller optimization in one
   unknown per target), the convexity table, the splitting of entropy on
   completed records into a weight part and a missing-variable part, and the
   identification criterion. The dual and the splitting were also confirmed
   numerically.
2. **Every repository statement in 04b is accurate.** That covers the LP
   objective, the 0.34–1.66 bounds, the 188 target rows with duplicates, filer
   weights from observed return counts through 2023, non-filer pools through
   2023, wealth from 2022 and missing before then, wealth top-share targets at
   fixed SCF thresholds, and the tilt solver's criterion. 04b also notes,
   correctly and unlike 04, that the itemized-deduction fields hold amounts for
   itemizers only. So the "simulate the reporting process" idea has no
   underlying expenses for 2015 non-itemizers to work with until a new
   imputation supplies them.
3. **The specific test questions.** The convexity claims are CONFIRMED, under
   conditions 04b states. "Positive weight on a zero-prior item has infinite
   cost" is CONFIRMED for forward relative entropy. In practice it bites in
   two places: records with zero prior weight, such as non-filer pool records
   outside their own year (04 notes this; 04b does not), and any new record
   added without a prior probability. The chi-square negative-weights claim is
   CONFIRMED and standard. The posterior-mode reading is CONFIRMED but close to
   true by definition; 04b says so, which is to its credit. The identification
   section is CONFIRMED, but its proposed "range consistent with targets" by
   linear programming is OVERSTATED as a practical tool. Without a
   distance-to-prior restriction those ranges come from corner solutions, which
   is the very failure of today's LP, and they will be uninformatively wide.
4. **The criticism of the wealth tilt solver is correct but matters less than
   both reports imply.** Both say the criterion in `tilt_solver.R` (squared
   scaled misses plus a ridge penalty on the coefficients) is not convex. I
   confirmed this: about 20% of random segments violate convexity. But the
   tilted totals are the gradient of a convex function. Wherever that
   function's curvature matrix is nonsingular, every point where the
   unpenalized criterion is flat is a global best fit, so there are no inferior
   local optima to get trapped in. Forty random starts found a single solution
   in every case. The real difference is elsewhere. The tilt solver weights
   misses by arbitrary relative scales and a ridge, not by stated
   precisions and a distance from the prior.
5. **The two-week plans are optimistic.** 04b's plan 1 is the more realistic of
   the two reports' plans, but it still puts in week 2 a censored-target family
   with historical reporting rules, all remaining annual priors, a 2097 check
   and sensitivity reports. It does not budget the non-itemizer expense
   imputation that the censored targets need, or the change to roughly fifteen
   imputation modules that multiple completions per record need. 04's plan A
   (value layer, imputed variants, forward chain and reporting calculator, all
   in two weeks) is less realistic still.
6. **The weights-only solver does not need a GPU.** A synthetic problem with
   1,000,000 records and 1,000 sparse targets solved in 1.3 seconds on one CPU
   core (scipy L-BFGS-B, 11 iterations). This supports 04b ("a GPU is not
   mathematically necessary") over the implicit case in 04 for porting the
   calibration step itself to JAX.
7. **Where the two reports genuinely disagree** (detail in "Points of real
   disagreement"):
   - how years are linked: 04 chains each year from the previous year's
     answer; 04b fits separate years around a common growth model;
   - whether a value layer belongs in the first build: 04 yes, 04b only after
     diagnostics call for it;
   - whether a joint all-years problem is convex: 04b is right that with weights
     only and linear links it is;
   - whether the Bayesian reading is "exact": 04b is right that it holds only
     under chosen assumptions;
   - which identification diagnostic to report.
8. **04 catches what 04b misses:**
   - why the LP lands on corners;
   - composition change standing in for growth in projection years;
   - weight degeneracy over long chains of years;
   - the lock-out of zero-prior records;
   - a cheap numerical measure of how much the targets pin down each output.

   **04b catches what 04 misses:**
   - the itemizer-only fields;
   - the missing 2015–2016 output and pre-2022 wealth;
   - counting rules for population size;
   - the history variables (basis, carryforwards) a record must carry;
   - that the mode of a density depends on the coordinates it is written in;
   - that the Kish effective sample size rises artificially when a record is
     split into copies (confirmed: splitting every record into 5 copies raises
     it fivefold);
   - that in block-wise GPU code, squared misses must be taken on the summed
     totals, not on block sums.
9. **Combined judgement.** Build first:
   - a per-year penalized entropy calibration on fixed records, replacing the
     LP, solved through the dual on CPU;
   - a prior whose dollar amounts are aged by growth factors that vary by
     income rank, estimated outside the solver from SOI average amounts by AGI
     bin (04's insight, kept convex in 04b's way);
   - multiple completions for wealth only at first, where the donor
     probabilities already exist;
   - historical years each built from a common aged base, and projection years
     built from the last fitted historical year.

   The main open questions are listed at the end:
   - how to set prior strength and target precisions with no second year of
     microdata to validate against;
   - which direction of relative entropy to use;
   - how to create candidate values for PUF-observed amounts;
   - where non-itemizer expenses come from.

## Claims table for 04b

Line numbers refer to 04b as it stood on 2026-10-07.

### Repository statements

| # | Claim (04b line) | Grade | Note |
|---|---|---|---|
| 1 | The existing method already changes both weights and values (29) | CONFIRMED | `create_2017_puf.R`: cell weight factors, then LP, then per-AGI-group dollar rescale over 8 groups. |
| 2 | Weight factors by demographic and income cell (29) | CONFIRMED | Married × senior (age group 6) × AGI group, against Pub 1304 Table 1.6 counts. |
| 3 | LP limits 0.34 and 1.66 (29) | CONFIRMED | `reweight_lp(..., e = 0.66)`. |
| 4 | `reweight.R` minimizes the sum of factors, not departures from one (29) | CONFIRMED | `set.objfn(lprw, rep(1, nrow(lhs)))`. |
| 5 | Target tolerances are hard intervals (29) | CONFIRMED | Two inequality constraints per row at `target × (1 ± tolerance)`. |
| 6 | 188 rows, including repeated filing-status rows (29) | CONFIRMED | 188 data rows, 176 distinct; the four all-AGI filing-status rows each appear four times. |
| 7 | Filer weights use observed return-count growth through 2023 (31) | CONFIRMED | `FILER_OBS_LAST = 2023L`, `resources/return_counts_2023.csv`. |
| 8 | Non-filers use annual pools through 2023, then grow the last pool (31) | CONFIRMED | `NONFILER_LAST_POOL_YEAR`, asserted equal to 2023; growth by age band after. |
| 9 | Filer ages are annual age slices, not individuals ageing (31) | CONFIRMED | Comment in `compute_weights_for_year`: ages stay at 2017 values; each record is an age slice. |
| 10 | Wealth has separate growth factors by income group (31) | CONFIRMED | `dfa_factors.R` bucketed factors by DFA income bucket. |
| 11 | Variable guide has 190 entries, not 190 independent variables (31) | CONFIRMED | 190 data rows; includes splits, derived fields, mortgage metadata. |
| 12 | Wealth begins in 2022; `materialize()` leaves it missing before then (31) | CONFIRMED | `base_year = 2022L`; `materialize.R` doc: modules with later base year remain NA. |
| 13 | Several fields are itemizer-only or capped (46) | CONFIRMED | e.g. `med_exp`, `salt_inc_sales`, `int_exp`, `job_exp` are described "for itemizers"; `char_cash` and `salt_prop` come from Schedule A lines, so are zero for non-itemizers. 04 does not mention this. |
| 14 | Wealth top-share targets use fixed SCF thresholds (97) | CONFIRMED | `wealth.R` ~l. 690: thresholds are "FIXED inputs to the tilt". |
| 15 | Consumption: quantile-forest total, distributional-forest share vector, top-tail extrapolation (175) | CONFIRMED | `consumption.R` Stage A/B and the `(Y/Y_ref)^beta` adjustment. |
| 16 | Wealth: tilt donor probabilities, fit cell totals and ownership counts, sample a donor, rescale components within cells (177) | CONFIRMED | `wealth.R` Stage 3 Steps A and B. |
| 17 | Tilt criterion is squared scaled misses plus a quadratic penalty on coefficients (177) | CONFIRMED | `tilt_solver.R` l. 202–205. |
| 18 | Its scaling constants are not measurement standard deviations (177) | CONFIRMED | `scale = max(|target|, 1e9)` for amounts, `max(target, 1)` for counts. |
| 19 | Existing output range starts in 2017 (189) | CONFIRMED | `write_outputs.R` loops 2017–2097. |

### Mathematical statements

| # | Claim (04b line) | Grade | Note |
|---|---|---|---|
| 20 | Generalized relative entropy is zero at the prior and positive elsewhere; with equal totals it is proportional to KL (63) | CONFIRMED | With Σw = Σb = N it equals N·KL(q‖p), so it is proportional after dividing by N*. |
| 21 | Forward relative entropy permits a share to fall to zero at finite cost (69) | CONFIRMED | Per-record cost at zero is b; the slope there is infinite, so the optimum never sits exactly at zero unless forced. |
| 22 | Positive weight on a zero-prior item has infinite cost (69) | CONFIRMED | q·log(q/0) = ∞. In the candidate-record design every candidate has positive prior probability, so this bites only for zero-prior-weight records (union-base pool records outside their year) and for added records given no prior mass. 04b does not make that connection; 04 does. |
| 23 | Chi-square approximates relative entropy for small changes and penalizes large expansions more (70) | CONFIRMED | Cost grows like r² versus r·log r. |
| 24 | Without nonnegativity, quadratic calibration can give negative weights (70) | CONFIRMED | Standard GREG property. With an explicit nonnegativity constraint, chi-square instead sets some weights exactly to zero, which entropy never does. |
| 25 | Reverse relative entropy makes eliminating a represented type infinitely costly; expansion into a rare type "comparatively less costly" (71) | CONFIRMED, vague | The second half commits to little. In the unnormalized reverse form, mass on a zero-prior record costs only its own size, linearly, so reverse KL lets weight escape to unsupported records cheaply. That is a practical difference worth stating. |
| 26 | Hellinger has no infinite barrier at zero (72) | CONFIRMED | Forward KL has none either; the contrast is only with reverse KL. |
| 27 | Total variation can have many optima (73) | CONFIRMED | It is piecewise linear, like the current LP, so it lands on corners. |
| 28 | Population-weighted entropy is unchanged by splitting a record proportionally; unweighted factor penalties are not (78) | CONFIRMED | Identical records get identical tilt factors at the optimum. |
| 29 | KL from a discrete file to a continuous density is not finite (80) | CONFIRMED | |
| 30 | With fixed records, additive targets, including law-censored totals and products like wages × wealth, are linear in weights (95) | CONFIRMED | |
| 31 | Ratios of fitted totals and shares of the fitted top 1% are nonlinear (97) | CONFIRMED | A count below a supplied threshold is linear (both reports agree). |
| 32 | Objective is strictly convex on positive-prior support, so the best weight vector is unique (129) | CONFIRMED | Second derivative 1/w > 0; existence holds because the entropy term grows faster than linearly. |
| 33 | A Huber penalty keeps convexity (131) | CONFIRMED | |
| 34 | Attainable target averages form the convex hull of record contributions (fixed N) or a cone (free N) (139) | CONFIRMED | |
| 35 | Weights × transformations are generally nonconvex (151) | CONFIRMED | |
| 36 | Entropy on completed records splits into a weight term plus a weighted conditional term; convex in the masses, including with separate coefficients (160–165) | CONFIRMED | Checked algebraically: the conditional term is a perspective of a convex function composed with a linear map. Checked numerically: the two sides agree exactly (check 4). |
| 37 | Squared misses after an exponential tilt need not be convex (177) | CONFIRMED | 391–443 of 2,000 random segments violated convexity (check 3). See row 46 for why this matters less than it sounds. |
| 38 | Joint fit across years with linear transitions and quadratic penalties stays convex (195) | CONFIRMED | Holds also for a relative-entropy link to a linearly aged previous year, because relative entropy is jointly convex in both arguments. 04 calls its joint formulation B nonconvex; that is only because B includes value transformations. |
| 39 | Posterior-mode reading holds under normal errors and a prior proportional to exp(−relative entropy), with a specified base measure (215–217) | CONFIRMED | Any penalized objective is the mode of exp(−objective) once normalized; exp(−κ·D) is integrable on w ≥ 0, so the prior is proper. The claim is close to true by definition, and 04b says so. Its point about the base measure is correct and needed: the exact constraints define a lower-dimensional set. |
| 40 | Special cases: entropy calibration, maximum entropy, GREG, penalized least squares (219) | CONFIRMED | |
| 41 | A density's mode depends on coordinates (223) | CONFIRMED | Change of variables adds a Jacobian term. 04 omits this. |
| 42 | Identification: an output is determined by exact targets if and only if its contribution vector lies in the span of target and constraint rows, at a strictly positive solution (231) | CONFIRMED | Standard linear algebra; correct statement of the converse. |
| 43 | "Range consistent with targets" endpoints are linear programs; very wide or unbounded ranges are informative (246) | OVERSTATED | True as stated, but those LPs inherit today's corner pathology: all weight on a few extreme records. With ~200,000 records and ~1,000 targets the space of weight changes that leave every target fixed has ~199,000 dimensions, so almost every output will show a very wide range, which says little. With a population constraint the range is bounded, not unbounded. The useful version adds a ball around the prior in relative entropy; that is a convex program, not an LP. 04b mentions the ball only in passing. |
| 44 | Dual: stationarity gives w = b·exp(−a′λ/κ); Φ(λ) = κΣb·exp(−a′λ/κ) + y′λ + ½λ′Σλ; gradient is −Aw + y + Σλ (268–276) | CONFIRMED | Derived independently; matches up to a constant. Numerically, primal and dual weights agree to 4×10⁻⁶ (check 1). One thing 04b misses: the dual needs Σ, not its inverse, so redundant or exact targets do not need to be removed first (compare 04b l. 125). |
| 45 | Storage arithmetic: 200,000 × 190 × 83 years ≈ 3.15 billion numbers, 12.6 / 25.2 GB; 1,000 × 200,000 dense ≈ 0.8 / 1.6 GB (252–254) | CONFIRMED | Arithmetic correct; the record count excludes the non-filer pools, as 04b says. |
| 46 | That the tilt criterion is nonconvex is the reason it is "a different method" (177, 278) | OVERSTATED | The tilted totals are the gradient of a convex log-sum-exp function. Without the ridge, wherever its curvature matrix is nonsingular, a point where the criterion is flat must fit exactly. In 40 random starts per case there were no inferior local optima (check 3). With reachable targets the tilt and the penalized entropy dual give practically the same answer (misses within 0.4%, same distance from the prior). The real differences: (a) how unreachable or conflicting targets are traded off: by relative scale and ridge versus stated precision; (b) the correct convex counterpart is a conditional entropy problem whose dual is log-sum-exp, not the weight dual 04b writes down, because the tilt normalizes within each record. |

### Computation, JAX and plans

| # | Claim (04b line) | Grade | Note |
|---|---|---|---|
| 47 | `jax.value_and_grad`, `jax.jit`, 32-bit default, zero gradients for step and order-based functions (284–290) | CONFIRMED | JAX FAQ section "Why are gradients zero for functions based on sort order?" opened 2026-10-07: https://docs.jax.dev/en/latest/faq.html. |
| 48 | Summing squared errors within record blocks solves a different problem (286) | CONFIRMED | A real pitfall; 04 does not mention it. |
| 49 | "A GPU is not mathematically necessary" (280) | CONFIRMED | 1,000,000 records × 1,000 sparse targets: 1.3 s on one CPU core (check 2). |
| 50 | Kish effective sample size can be inflated by splitting a donor into copies (300) | CONFIRMED | Splitting every record into 5 copies multiplied it by exactly 5 (check 5). |
| 51 | Plan 1 in two weeks: registry, error scales, candidate export, solver; then censored family, remaining annual priors, 2097 check, sensitivity reports (324) | OVERSTATED | The solver is 1–2 days. The rest is not two weeks: (a) the censored family needs underlying expenses for 2015 non-itemizers, which are absent (row 13); (b) candidate export needs joint completions across ~15 modules that now draw once per record by id; (c) a reporting calculator for 2018–2023 law through Tax-Simulator is an interface 04b admits it did not inspect; (d) 2015–2016 priors have no source. |
| 52 | Plan 2 in two weeks: plan 1 core plus one transformation family (332) | OVERSTATED | It inherits plan 1's scope plus a nonconvex layer with multiple starting points. |
| 53 | Plan 3: restricted experiment on one block with simulated known answers (338) | CONFIRMED (as a judgement) | Realistic scope. |
| 54 | "Add limited value transformations where diagnostics require them"; "add … amount scenarios where support is inadequate" (23, 322) | UNSUPPORTED | It names no diagnostic or threshold. It also gives no method for creating candidate values of PUF-observed amounts, which is where the post-2015 shape drift lives (see disagreement 2). |
| 55 | Prior strength "should be selected through historical prediction checks" (131) | UNSUPPORTED | There is no second year of microdata to check against. The only workable form is predicting held-out published tables, which 04b mentions only for noisy targets (l. 302). |
| 56 | Projection "requires explicit distributional assumptions and forecast uncertainty" (21, 197) | UNSUPPORTED (vague) | Correct, but it names no assumption or mechanism. 04's point that expected growth of values must sit in the prior, or reweighting will stand in for it, is the concrete version. |
| 57 | Boyd and Vandenberghe example 3.19 documents the convexity of relative entropy (76) | UNCHECKABLE | I did not reopen the PDF. From memory, example 3.19 is the perspective of the negative logarithm, which gives relative entropy; consistent. |
| 58 | Hainmueller abstract verifies entropy balancing's use of maximum-entropy reweighting (219) | UNCHECKABLE | Not reopened; 04b says only the abstract was seen. |
| 59 | Bissiri, Holmes and Walker distinguish loss-based updating and discuss the scaling problem (221) | CONFIRMED | Opened 2026-10-07 (https://pmc.ncbi.nlm.nih.gov/articles/PMC5082587/): JRSS B 2016; "The question is how to select w, noting that w controls the relative weight of loss to data to loss to prior." |

Tally: CONFIRMED 50 (rows 1–42, 44, 45, 47–50, 53, 59; row 25 is
confirmed but vague), OVERSTATED 4 (43, 46, 51, 52), UNSUPPORTED 3 (54–56),
UNCHECKABLE 2 (57, 58).

## Numerical checks

One Python script (numpy 1.26 and scipy from `SciPy-bundle/2024.05`), run as
a single-core Slurm job on the `day` partition. All data were synthetic. The
script is not committed; its logic is described here so it can be rebuilt.

1. **Primal and dual agree.** 2,000 records, 21 targets (a population count
   plus 20 amounts), targets up to 10% off the prior, 1% standard errors. The
   primal was solved over log weights and the dual over 21 coefficients. Weights
   agreed to a maximum relative difference of 3.7×10⁻⁶.
2. **CPU cost of the weights-only problem.** 1,000,000 records, 1,000 targets,
   each record contributing to 15 targets, targets within ±5% of the prior,
   0.5% standard errors. L-BFGS-B on the dual converged in 11 iterations, 1.3
   seconds, one core. Weight ratios ranged 0.16–3.27. Harder targets will take
   more iterations, but the order of magnitude does not call for a GPU.
3. **Tilt criterion versus penalized conditional entropy.** 400 records, 60
   donors each, 4 donor features (two amounts, two ownership flags). Two
   target sets were tested:
   - (A) reachable but conflicting. The tilt criterion with ridge 10⁻⁴ and the
     entropy dual with 3–10% standard errors gave misses under 0.4% and the
     same distance from the prior (mean KL 0.094–0.096).
   - (B) one ownership target set to 3.5 times its prior total, which is
     unreachable. Both methods left the same −22.4% miss on it. The entropy
     dual's coefficient grew to 1,738 at 1% standard errors, while the ridge
     kept the tilt's at 6.4.

   In both cases, 40 random starts of the tilt criterion reached the same
   objective value to six digits. Midpoint convexity failed on about 20% of
   random segments.
4. **Splitting of entropy on completed records.** The two sides of 04b's
   identity agreed exactly on random inputs.
5. **Effective sample size under splitting.** Splitting every record into 5
   equal copies multiplied the Kish effective sample size by 5.0.

## Comparison of 04b and 04

### Where they agree, and whether the agreement is earned

| Shared conclusion | Earned? |
|---|---|
| Relative entropy on fixed records is the core; the problem is convex; the dual has one unknown per target | Yes. Both derive it and both relate it to the current LP. 04 explains why the LP sits at corners. 04b explains that the LP objective depends on how records are split into rows. |
| Weights alone cannot create combinations or values the records lack | Yes. Both give the convex-hull argument and concrete examples. |
| With fixed records, censored and other nonlinear-looking targets are linear in weights | Yes, and checked against the itemization case. But both treat the post-2017 itemizer tables as the main censoring problem. On the data at hand the larger one is in the 2015 base: non-itemizers there have no recorded expenses (row 13). The post-2017 itemizer population is roughly a subset of the 2015 itemizer population, whose expenses the PUF does record, because the standard deduction roughly doubled. So the post-2017 tables mostly inform how expenses grew in the upper part of the distribution. Rules that reach non-itemizers, such as a charitable deduction for non-itemizers, depend on expenses no target observes. This is my reasoning, not something either report states or I measured. |
| Several imputed completions per record let targets adjust imputed variables without disturbing PUF variables | Yes in principle. 04b's splitting identity is the cleaner statement. Neither notes two practical points: completions must be drawn jointly across modules (five per module crossed over fifteen modules is unworkable), and the wealth module already holds the donor probability vectors, so wealth completions are nearly free. |
| Do not count imputation-internal calibration totals again as independent targets | Yes. |
| A learned generator of new records comes last | Partly a standard view. Both justify it by the burden of validating a 190-variable joint model; neither tests it against a narrow use, such as splitting heavy top records, which 04 at least suggests. |
| One file plus selected sensitivity files | Yes; it follows the brief's decision. |
| The wealth tilt's least-squares criterion is not the convex dual | Correct, but both overstate its practical importance (row 46, check 3). |
| Relative entropy in the forward direction is the natural default | Not checked against the problem by either. See disagreement 4: the prior-strength anchor 04 proposes (the PUF's effective sample size) belongs to a model that implies the reverse direction. |

### Points of real disagreement

1. **How years are linked.**
   - 04 recommends a forward chain: year t's prior is year t−1's answer, aged.
   - 04b recommends separate annual problems around a common growth model, with
     an optional smoothness penalty.
   - Assessment: neither is right for all years. For 2016–2023, a chain carries
     each year's fitting noise and weight concentration into the next. Separate
     fits from a common aged base are safer and can run in parallel. For 2024
     onward, a prior aged from 2015 would discard everything learned from the
     2016–2023 targets, so projection years should start from the last fitted
     year. The current code already does this: filer weights after 2023 start
     from the 2023 weights. 04b is ambiguous about what the 2024+ prior is
     built from, and 04 does not distinguish the two periods.
2. **Whether value changes belong in the first build.**
   - 04 includes multipliers that vary with a record's rank in each dollar
     variable.
   - 04b defers value changes until diagnostics demand them.
   - Assessment: 04 is closer to right on substance. The main post-2015
     evidence is SOI amounts by AGI bin. With amounts frozen, entropy
     calibration meets them by shifting weight toward high-amount records within
     each bin, so composition change stands in for growth in average amounts.
     04 states this mechanism; 04b does not. Today's pipeline already rescales
     amounts within 8 AGI groups for 2017, and dropping that would be a step
     back.
   - 04b is right that putting the multipliers inside the optimizer makes the
     problem nonconvex. The compromise is to estimate rank-varying growth
     factors outside the solver, directly from SOI average amount per return
     by AGI bin. That extends `grow_with` from one factor per variable to a
     schedule. The aged values then form the prior, and the weights are
     calibrated convexly. One re-estimation pass can follow if records move
     across bins.
3. **Convexity of a joint all-years problem.**
   - 04 lists formulation B as nonconvex.
   - 04b shows that weights with linear year-to-year links are convex.
   - Assessment: 04b is right. Relative entropy is jointly convex in its two
     arguments, so even 04's own relative-entropy link to the aged previous
     year is convex in the weights. B is nonconvex only because it adds value
     multipliers.
4. **The status of the Bayesian reading.**
   - 04 calls it "exact", via Sanov's theorem: the probability that N
     independent draws from the prior look like Q falls off as
     exp(−N·KL(Q‖P₀)).
   - 04b calls the file a posterior mode under chosen assumptions. It notes that
     modes depend on coordinates and that a loss-based update needs its scale
     chosen.
   - Assessment: 04b is right, and the problem with 04's argument is
     specific. Sanov's theorem makes the prior strength equal to the number of
     tax units, about 10⁸. That would make the prior nearly rigid against any
     target with a stated sampling error. 04 then proposes anchoring the
     strength to the PUF's effective sample size instead. That anchor belongs
     to a different model: uncertainty about the population because the prior
     came from a finite sample (a Dirichlet or Bayesian-bootstrap model). That
     model gives the reverse direction of relative entropy, the
     empirical-likelihood form. So 04's default (forward direction with a
     sample-size strength) mixes two models. Neither direction is "the"
     Bayesian answer.
5. **The relation of the wealth tilt to raking.**
   - 04 calls it "the same exponential tilt" and says it agrees with the dual
     when targets are met.
   - 04b calls it a different method.
   - Assessment: both are partly right. The functional form is the same
     exponential family. But the tilt normalizes within each record (a
     conditional problem, with a log-sum-exp dual) and holds record weights
     fixed, whereas raking moves record weights. In practice the two give the
     same answer when targets are reachable (check 3).
6. **What identification diagnostic to report.**
   - 04 proposes a "data-determined share": the R-squared from regressing each
     record's contribution to an output on its contributions to the targets. I
     checked this under 04's local Gaussian approximation. It holds with
     prior-weighted, uncentered R-squared. 04 does not say "uncentered"; the
     intercept enters only if the population count is a target.
   - 04b proposes a yes-or-no criterion (the output's contributions lie in the
     span of the targets' contributions) plus a min–max range by LP.
   - Assessment: 04's measure is a quantitative version of 04b's criterion. It
     is cheap and informative. 04b's LP range is mostly uninformative unless
     restricted by a distance-to-prior ball (row 43). Report 04's share plus the
     change in output when the prior is swapped. Both reports recommend
     prior-swap sensitivity.
7. **The cost of moving values.**
   - 04 proposes the weighted squared change in log value.
   - 04b lists options and warns that per-record edit penalties depend on how
     old records are paired with new ones.
   - Assessment: 04b is more careful. 04's formula is undefined for zeros and
     for negative amounts (business losses, net worth, capital gains), which
     04b explicitly notes can be negative. A sign-aware transform is needed if
     04's route is taken.
8. **Whether a GPU is needed.**
   - 04 implies a GPU makes the solve take seconds.
   - 04b says no GPU is needed.
   - Assessment: check 2 supports 04b for the weights-only problem. A GPU
     matters only for the nonconvex value layer or for running the full tax
     calculation inside the optimizer.

### What each catches that the other misses

**Only 04:**
- why the LP has at most as many interior factors as distinct binding targets;
- composition standing in for growth in projection years;
- weights becoming concentrated on few records over long chains of years, and
  splitting heavy records as the remedy;
- zero-prior lock-out of union-base pool records;
- the meaning of the dual coefficients as the first diagnostic;
- a goodness-of-fit check: with correctly stated errors, the sum of squared
  standardized misses should be close to the number of targets minus the
  number effectively fitted;
- prior strength varying by cell.

**Only 04b:**
- itemizer-only fields;
- no output for 2015–2016 and no wealth before 2022;
- counting tax units versus people;
- history variables (basis, carryforwards) that annual records must carry;
- the dependence of a density's mode on coordinates;
- the base-measure caveat for exact constraints;
- inflation of the effective sample size by splitting a record into copies;
- correlated and nested target errors stated as a covariance matrix;
- the Huber penalty for discrepant sources;
- block-sum pitfalls in GPU code and 64-bit arithmetic;
- that reusing a forecast in both the growth prior and the targets double
  counts it;
- that averaging completions into one record distorts nonlinear tax.

**Neither:**
- the prior's filer weights should come from the PUF weights scaled to cell
  counts, not from today's LP output, which already sits at its 0.34 and 1.66
  bounds for nearly every record;
- multiple completions per record multiply Tax-Simulator's record count and
  run time across 83 years, so output compression has to be planned (04b
  mentions compression only in general terms);
- the PUF's disclosure protection at the top is noted by both but given no
  treatment.

## Combined judgement

### First formulation to build

1. **Solver.** Penalized entropy calibration on fixed records, one year at a
   time, solved through the dual, replacing `reweight_lp`. Start in R or Python
   on CPU; JAX is optional at this stage. Exact constraints: the population
   partition only. Everything else is a soft target with a stated standard
   error. Run the reverse direction as a sensitivity case.
2. **Prior.**
   - Filer weights: PUF weights scaled to the Pub 1304 cell counts, not the LP
     output.
   - Dollar amounts: aged by growth factors that vary across the AGI
     distribution, estimated outside the solver from SOI average amounts by AGI
     bin.
   - Imputations: as today, with internal calibration to totals removed where
     those totals become targets.
3. **Completions.** Wealth first, using the donor probabilities the tilt
   already computes (several donors per record, conditional entropy term).
   Other modules stay at one draw until a target needs them adjusted.
4. **Time.** 2017–2023 each fitted from a common aged base. 2024 onward aged
   from the fitted 2023 file, with forecast targets given wide errors. A
   smoothness penalty is a later step.
5. **Targets.** A registry of Pub 1304 counts and amounts by AGI bin and filing
   status for 2017–2023, deduplicated, at the finest published cells. One
   censored family (itemizer counts and amounts, 2018–2023) uses a small
   reporting calculator applied to the 2015 itemizers' aged expenses. The
   missing non-itemizer expenses are stated as a known gap.
6. **Diagnostics.**
   - standardized misses and dual coefficients;
   - Kish effective sample size per cell, computed at the original-record level;
   - 04's data-determined share for a few headline outputs;
   - the change in those outputs when the prior is swapped.

This is a realistic two-week target. The value layer inside the optimizer, a
joint all-years problem, and a record generator follow only if these
diagnostics show weight concentration that the rank-varying prior does not
remove.

### Open questions neither report resolves

1. **Prior strength and target precisions.** How should they be set when the
   2015 PUF is the only microdata year? Holding out published tables is the
   only available test, and nobody has said which tables.
2. **Direction of relative entropy.** Forward or reverse? They behave
   differently at the top of the income distribution, where weights move most.
3. **Candidate values for PUF-observed amounts.** How should they be created
   (wages, business income, capital gains), as opposed to imputed variables?
   This is where the post-2015 shape of the income distribution lives.
4. **Non-itemizer expenses.** Where should underlying deductible expenses for
   2015 non-itemizers come from, and how would they be validated?
5. **Consumption and wealth.** What joint dependence beyond income and
   demographics should be assumed? No single source observes both well.
6. **Measurement function.** Can Tax-Simulator or its GPU port serve as the
   measurement function for reporting-process targets without a second tax
   engine?
7. **2015–2016.** Should the method produce these years at all, given that the
   non-filer pools and the wealth module do not reach them?
8. **History variables.** How do basis, carryforwards and the mortality ledger
   fit with records that need not persist across years?
