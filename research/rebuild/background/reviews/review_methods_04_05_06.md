---
title: "Review of the methods reports 04, 05 and 06"
role: review
workstream: rebuild
status: current
updated: 2026-10-07
true_as_of: 2026-10-07
sot: research/rebuild/BRIEF.md
supersedes: []
superseded_by: null
---

# Review of the methods reports 04, 05 and 06

This review covers:

- `04_methods_first_principles.md` (the mathematical formulation);
- `05_lit_survey_statistics.md` (the survey-statistics and econometrics literature);
- `06_lit_ml_bayes.md` (the machine-learning and Bayesian literature).

I re-derived the mathematics, read the code the reports describe, and ran
numerical checks as Slurm batch jobs. The scripts and their output are in
`/nfs/roberts/scratch/pi_nrs36/jar335/review_methods_chk_20261007/`:

- `a_lp_real.R`: the cached linear-program solution set against the 2015 PUF weights;
- `b_toys.R`: a toy linear program, a toy entropy calibration, the identification
  measure, and the residual check;
- `e_tilt.R`: the wealth tilt solver, run from several starting points.

Citations were checked against the Crossref and arXiv registries, the
publisher or author pages, and the source code on GitHub. Where a grade rests
on my own knowledge of a paper and not on reading it today, the table says so.

## Summary

**Claims checked: 60.**

| Grade | Count |
|---|---|
| Confirmed | 35 |
| Overstated | 13 |
| Wrong | 9 |
| Unsupported | 1 |
| Uncheckable | 2 |

**Errors and objections that would change the recommended approach.** The three
reports recommend the same thing: replace the linear program with entropy
calibration, where each target's miss is penalized in proportion to its
precision and the problem is solved through one multiplier per target. The core
mathematics of that recommendation is right. What it does not settle is listed
below.

1. **The Bayesian argument does not fix how strong the prior is or which
   direction of relative entropy to use.** "Relative entropy" is the standard
   measure of distance between two distributions; it is not symmetric, so
   "distance of the new weights from the old" and "distance of the old weights
   from the new" are two different choices.
   - Taken literally, the "population drawn from the prior" model gives the
     usual raking direction. But the prior's strength is then the population
     count, about 150 million tax units. A prior that strong lets targets move
     the weights by almost nothing.
   - Report 04 sets the strength α by the PUF's sample size, but applies α to
     relative entropy computed on unnormalized weights (weights that sum to the
     population). That combination is wrong by a factor equal to the
     population: α = 200,000 on that scale means a prior about 150 million times
     stronger than intended.
   - The Dirichlet argument for the other direction only works when every
     record's prior share times the concentration is much larger than one. At
     the natural concentration (the sample size), the Dirichlet prior is flat
     and pulls nothing toward the prior.

   The Bayesian label therefore adds no discipline beyond ordinary ridge
   calibration. The prior strength α and the per-target error sizes σ are
   tuning choices. They have to be set by judgment, by holding out targets, or
   by estimating them from the data (see Objections).

2. **The residual check that report 06 proposes would mis-set the precisions.**
   Report 06 says that after fitting, each miss divided by its σ should have a
   mean square of about one. After a penalized fit it should not: the misses
   shrink by the share of each target that the fit absorbs. In a toy with 20
   targets the expected sum was 6.2, not 20. Following 06's rule would lead you
   to shrink every σ by about a factor of 1.8. Report 04 states the rule
   correctly.

3. **Every target in the current linear program is a count; none is a dollar
   amount.**
   - 176 of the 188 rows count returns with a nonzero amount of some item in a
     cell. The other 12 count dependents by AGI bin.
   - Dollar totals are matched afterwards, by rescaling values within 8 AGI
     bins (`src/create_2017_puf.R`).

   So replacing the linear program alone fixes only the count step. Adding
   SOI dollar totals to the calibration, as all three reports envisage, would
   move the dollar alignment from values onto weights. That is the substitution
   of composition change for value change that report 04 warns against in
   projection years. The reports never connect the two.

4. **The linear program does not assign its two bounds arbitrarily.** Report 05
   says the solver's path decides which record gets 0.34 and which gets 1.66.
   It does not. Minimizing the unweighted sum of adjustment factors makes it
   cheapest to raise records whose prior weight is large, because each unit of
   factor buys the most weight there.
   - In the cached solution, the share of records at 1.66 rises from 8% in the
     lowest prior-weight fifth to 71% in the highest. The fifths are taken
     within each AGI bin and filing status.
   - High-income records carry low sampling weights, so they are pushed toward
     0.34.

   The distortion in today's file is systematic, and the before-and-after
   comparison for any replacement should be read by weight stratum.

5. **A smaller, standard alternative is not weighed.** That alternative is exact
   or banded calibration with a bounded distance: Deville and Särndal's
   "logit", Treasury's x⁴ + x⁻⁴ − 2, or the Tax-Calculator team's bounded
   quadratic program with slack. It removes the corner solutions. It needs no α
   and no σ for each target, and it keeps the existing tolerance bands. The
   precision-weighted soft penalty earns its extra tuning only where sources
   conflict or targets are rough: cross-source totals and projection years.
   None of the reports sets the two side by side.

6. **The proposed fix to the wealth tilt uses the wrong dual.** The tilt
   normalizes donor probabilities within each record. Its concave dual
   therefore has a per-record log-sum-exp term ("log of a sum of exponentials",
   the normalizing constant), not the whole-file exponential in report 04. Cells
   whose targets cannot be reached need soft targets, which changes the
   estimator. Report 04 is right that the current least-squares form can stop at
   different answers from different starting points. I reproduced that, but
   only for targets at or beyond what the cell can reach.

**Overall judgement of each report.**

- **04 (first principles).** The strongest of the three. Its reading of the
  code is accurate. Its derivations are right, including the closed form, the
  dual, the corner bound and the residual rule. Its errors are in the Bayesian
  section: the units of α, and the Dirichlet direction stated without its
  condition. Two further claims need repair: the identification measure is
  correct only with a particular, non-standard R-squared, and the analogy
  between the tilt's ridge penalty and a target's error variance is wrong.
- **05 (survey statistics).** A good and well-sourced map of the literature. The
  citations I checked exist and mostly say what it says. It misdescribes the
  code in three places:
  - it says the linear program binary-searches for its bound, which production
    does not;
  - it says most of the 188 targets are dollar amounts, when none are;
  - it says the corner assignment is arbitrary, when it is driven by prior
    weight.

  It also misapplies Wu and Sitter's "model calibration".
- **06 (machine learning and Bayesian).** Broad. Its reading of the PolicyEngine
  and Tax-Calculator code is accurate. Its main diagnostic rule is wrong, as is
  the χ² rule it borrows from maximum-entropy imaging. It overstates what the
  particle-filter and Census TopDown results imply for Tax-Data, and it
  misstates one paper's constraint count.

## Table of claims

Grades:

- **Confirmed**: true as stated.
- **Wrong**: false as stated.
- **Overstated**: true only under a condition the report does not state.
- **Unsupported**: no evidence given.
- **Uncheckable**: I could not check it.

"Toy" means a numerical check in `b_toys.R` or `e_tilt.R`. "Data" means
`a_lp_real.R` on the cached solution.

### Mathematics

| # | Report | Claim | Grade | What is true |
|---|---|---|---|---|
| M1 | 04, 05 | At a vertex of a linear program, at most as many variables lie strictly inside their bounds as there are binding constraints. | Confirmed | The exact bound is the rank of the binding constraint rows: the number of them that are not linear combinations of the others. In the toy, 44 interior records matched 44 binding rows of rank 44. |
| M2 | 04 | "188 rows, 176 distinct … at most 176 factors can be interior. The cached solution has 114 … what that theorem predicts." | Overstated | 176 distinct rows is right. The bound is lower, because the count targets are nested. The 16 AGI-bin counts and the 24 filing-status × age counts each sum to total returns, and the 4 filing-status and 6 age totals are sums of the status × age cells. That removes at least 11 more dimensions, so at most 165 records can be interior. I did not compute the exact rank. The theorem gives a ceiling; it does not predict 114. |
| M3 | 05 | "With 188 targets, at most 188 factors can be interior … 114 is what theory predicts." | Overstated | As in M2; 188 is a looser ceiling still. |
| M4 | 05 | Which record gets 0.34 and which 1.66 "is decided by the solver's path, not by any feature of the records." | Wrong | It is driven by prior weight (summary, item 4). Data: within AGI bin × filing status, the share at 1.66 is 8%, 17%, 38%, 55% and 71% by prior-weight fifth. Toy: 0% in the bottom three fifths, 73% in the top. |
| M5 | 05 | The 62% / 38% split "follows from the objective, not the data." | Overstated | The objective creates the corners. The split depends on the spread of prior weights within cells. The records at 1.66 hold 49% of prior weight while being 38% of records. |
| M6 | 04 | The objective treats a factor on a record of weight 5 the same as one of weight 5,000 and rewards pushing factors down. | Confirmed | True, but 04 does not draw the consequence in M4. |
| M7 | 04, 06 | Closed form: w_i = w⁰_i exp(Σ_k a_ik λ_k / α), with λ_k = (y_k − Σ_i w_i a_ik)/σ_k². | Confirmed | Derived from the first-order condition. Toy: solving the problem directly for the weights and solving it for the multipliers give the same weights to 4×10⁻⁵, and the multiplier identity holds to 1×10⁻⁶. 06 has α = 1. |
| M8 | 04, 06 | The dual is concave with one unknown per target. | Confirmed | Strictly concave whenever every σ_k > 0. With exact targets (σ = 0), it is strictly concave only if the binding target rows are linearly independent. |
| M9 | 04, 06 | One optimum; behaviour with collinear targets or an empty cell. | Confirmed, with conditions | With soft targets the answer is unique even with duplicated or nested rows. In the toy, duplicates split one multiplier equally, and a target whose cell has no records gets λ = y/σ² and leaves the weights alone. With exact targets, collinear rows leave λ undetermined, and inconsistent rows have no solution. In the toy, two nearly exact duplicates that disagreed by 1% drove λ to ±5 million while the optimizer reported success. Published tables whose cells do not add to their totals because of rounding create exactly this case. |
| M10 | 04 | The multinomial ("population drawn from the prior") model gives KL(new ‖ prior), and penalized entropy calibration is "exactly the most likely file". | Overstated | The direction is right. KL is the Kullback–Leibler divergence, the standard name for relative entropy. "Exactly" is too strong: Sanov's theorem gives the probability only up to its leading exponential term. The model also has no free strength: it is the population count, about 1.5×10⁸, which makes the prior almost immovable. |
| M11 | 04 | α is "the number of independent units' worth of information", anchored on the PUF's effective sample size. KL is written for unnormalized weights. | Wrong (units) | With weights summing to N, α·KL(w ‖ w⁰) = α·N·KL(shares ‖ prior shares) plus a term in the total. Read as a sample size, α must multiply KL between shares. On 04's scale the equivalent is α/N ≈ 0.0013 for 200,000 records. |
| M12 | 04 | A Dirichlet prior centred on the prior gives KL(prior ‖ new), the empirical-likelihood form. | Overstated | The Dirichlet log density is Σ(β p⁰_i − 1) log q_i, where β is the concentration and p⁰_i the prior share. It reduces to −β·KL(p⁰ ‖ q) only when β p⁰_i ≫ 1 for every record. At β = n with equal shares it is flat (Dirichlet(1,…,1)) and pulls nothing. With unequal PUF weights, records with β p⁰_i < 1, the high-income ones, are pushed toward zero. |
| M13 | 04 | The two directions agree to second order. | Confirmed | Both are ≈ ½ w⁰(r − 1)², where r is the ratio of new to prior weight. |
| M14 | 04 | Per-record costs and weight forms in the distance table. | Confirmed | Checked each first-order condition. |
| M15 | 04 | Choosing each record's most likely value collapses the spread. | Confirmed | By the law of total variance, the within-record conditional variance is lost. 04's own transport cost anchors to drawn values, so it does not collapse spread. |
| M16 | 04 | Posterior over prior variance of an output ≈ 1 − R², with R² from a weighted regression of record contributions on target contributions. | Overstated | Exact (linear, normal errors, σ = 0) only if R² is the uncentered R² (no mean subtracted), with prior weights as regression weights. Toy: the true ratio was 0.035, 1 − uncentered R² was 0.035, and 1 − ordinary R² was 0.113. With σ > 0 the noise enters as a ridge term, not a simple discount (ratio 0.036). The measure inherits the assumption that each record's weight error is independent, so it says nothing about the shared errors (aging, imputation) that dominate. |
| M17 | 06 | Particle-filter and importance-sampling theory implies weight collapse as targets grow. | Overstated | For an exponential tilt, the effective sample size falls roughly like exp(−2·KL). That is exponential in how far the targets pull the weights, not in the number of targets. Targets the prior already meets cost nothing. Snyder et al. (2008) concerns per-particle likelihoods of high-dimensional observations, not population totals. The qualitative warning, that many discrepant fine-cell targets produce degenerate weights, stands. |
| M18 | 06 §7.3 | After fitting, misses divided by σ should have mean square ≈ 1 (citing Desroziers et al. 2005). | Wrong | The expected sum after a penalized fit is tr(S C⁻¹): S is the diagonal matrix of target error variances σ², and C is S plus the variance of each target implied by the prior. This is below the number of targets. Toy: 6.2 for 20 targets, confirmed by 2,000 simulations. Desroziers' relations use the prior-predictive miss, or the cross-product of before-fit and after-fit misses, not the after-fit miss alone. |
| M19 | 06 §1.7 | Maximum-entropy imaging sets the data weight so that χ² equals the number of measurements. | Wrong (outdated) | That was the "historic" criterion. Gull (1989) replaced it with χ² ≈ N − G, where G is the number of well-determined measurements. Same flaw as M18. This rests on my knowledge; I did not re-read Gull. |
| M20 | 04 §9 | With correct errors, the sum of squared standardized misses ≈ number of targets minus number effectively fitted. | Confirmed | Equals tr(S C⁻¹); see M18. |
| M21 | 06 §6.5 | The dual's curvature is Σ w a aᵀ + diag(σ²), and inverting it gives approximate covariance for λ. | Overstated | The curvature formula is right. Its inverse is not the posterior covariance of λ in any stated model; the posterior covariance of the weights comes from the primal curvature (by the Woodbury identity). |
| M22 | 06 §1.2 | A tolerance band corresponds to an absolute-value term on λ in the dual. | Confirmed | The conjugate of an interval indicator is λy + τ\|λ\|. |
| M23 | 04 §6, 06 | Large multipliers mark targets the prior disagrees with; they are "the first diagnostic to read". | Overstated | λ_k is measured in units of 1/target, so a count target and a dollar target cannot be compared. Compare λ_k σ_k, the standardized miss. |
| M24 | 05 §2.2 | All distance functions give the same estimates to first order (Deville and Särndal 1992). | Confirmed | This is a large-sample result for estimated totals under repeated sampling. It does not say the weights agree when adjustments are large, as at the top of the PUF; 05 notes that they diverge there. |
| M25 | 04 §2(b) | Alternating between weights and value transformations reaches a stationary point, not necessarily the best one. | Confirmed | Standard property of block coordinate descent. |
| M26 | 06 §3.2 | TopDown's bias finding argues for a two-pass fit in Tax-Data. | Overstated | The TopDown bias comes from nonnegativity truncating many noisy near-zero cell counts, with no prior. Tax-Data's weights are kept positive by the exponential form and anchored by a prior. Fixing precise targets first is just σ → 0 for them, with the feasibility risks of exact targets (M9). |
| M27 | 04 §8 | Each dual step takes milliseconds on a GPU; the whole solve takes seconds. | Unsupported | No benchmark. 06 flags its timing statements as expectations; 04 does not. |

### Code

| # | Report | Claim | Grade | What is true |
|---|---|---|---|---|
| C1 | 04 | `reweight.R` minimizes the unweighted sum of factors in [1 − ε, 1 + ε], called with e = 0.66. | Confirmed | `set.objfn(lprw, rep(1, n))`; `reweight_lp(puf_2017, targets, do_lp, e = 0.66)`. |
| C2 | 05 §2.5 | `reweight.R` first binary-searches for the smallest feasible ε. | Wrong | `tune_epsilon` runs only when e is NULL. Production passes 0.66. |
| C3 | 05 §2.1 | Dollar or overlapping targets are "most of the 188". | Wrong | 176 rows are counts of returns with a nonzero item (val_flag = 0). The 12 val_flag = 1 rows are dependents totals by AGI bin. There are no dollar-amount targets; dollars are matched by the later value rescale. |
| C4 | 04 | The target contribution is mostly an indicator for a nonzero filer in a cell. | Confirmed | `build_lhs`. |
| C5 | 04 | Four "all returns by filing status" rows each appear four times, giving 176 distinct rows. | Confirmed | `sort \| uniq -cd` on `config/target_info/baseline.csv`. |
| C6 | Brief, 04, 05 | 114 interior records; 62% at 0.34 and 38% at 1.66. | Confirmed | Recount: 114 interior, 128,813 at 0.34, 78,765 at 1.66, out of 207,692. |
| C7 | 05 §2.3 | A 4.9 ratio between records at the two bounds. | Confirmed | 1.66 / 0.34 = 4.88. |
| C8 | 04 | The dollar rescale uses about 8 multipliers per variable. | Confirmed | 8 AGI groups in `rescaling_agi_groups`. |
| C9 | 04 | The variable guide has 190 data rows. | Confirmed | |
| C10 | 04 §8 | `tilt_solver.R` minimizes squared scaled misses plus a ridge on λ, rather than solving the concave dual. | Confirmed | `fn = sum(((T_hat_n - target_n)/scale_n)^2) + ridge_eta*sum(lambda^2)`, solved by BFGS. |
| C11 | 04 §8 | That form is not guaranteed a single optimum. | Confirmed (in the infeasible regime) | 41 random small cells with targets near or beyond the reachable range ended at different objective values from different starts, for example 0.56 versus 1.64. A restart from the worse end with 20,000 iterations moved it by less than 0.01%. I did not test comfortably reachable targets; the file's own test 6 passes there. |
| C12 | 04 §8 | "Using the dual form would make it convex at no cost." | Overstated | The tilt's dual is per-record log-sum-exp, Σ_i w_i log Σ_j p_ij exp(Z_jᵀλ), not 04's whole-file formula. Cells that `attainable_bounds` flags as infeasible make the exact dual unbounded, so soft targets are needed, which is a different estimator. |
| C13 | 04 §8 | The dual's ridge term "is what the wealth tilt adds as `ridge_eta`". | Wrong | `ridge_eta` (default 10⁻⁴, on normalized λ) is added to a least-squares objective. It is not a target error variance. The tilt objective is neither the primal nor the dual of a penalized entropy problem. |
| C14 | 04 Summary 2 | Entropy calibration is "the same exponential tilt the wealth module already uses". | Overstated | The wealth tilt reshapes donor probabilities within each record and leaves record weights alone (04's §5 says this correctly). It is a conditional tilt, not a whole-file reweighting. |
| C15 | 04 §4 | Union-base records with zero weight in a year cannot gain weight. | Uncheckable | Mathematically true of relative entropy. I did not re-read `materialize.R` or `project_puf.R` for this review. |

### Literature

| # | Report | Citation and claim | Grade | Evidence |
|---|---|---|---|---|
| L1 | 04, 05, 06 | Deville and Särndal (1992), *JASA* 87: 376–382; calibration with a family of distances. | Confirmed | Crossref record. Content from my knowledge. |
| L2 | 05 | Bardsley and Chambers (1984), *Applied Statistics* 33: 290; ridge-type relaxation of benchmark constraints. | Confirmed | Crossref record. Content from my knowledge. Calling it "ridge calibration" predates the calibration vocabulary, but describes it fairly. |
| L3 | 05 | Isaki, Tsay and Fuller (2004), *Survey Methodology* 30(1); quadratic programming, size bounds, controls with an estimated covariance. | Confirmed | Abstract read on the Statistics Canada page. |
| L4 | 05 | Guggemos and Tillé (2010), *JSPI* 140: 3199–3212; penalized calibration as the design-based counterpart of mixed models. | Confirmed | Crossref record; the title matches. The abstract is withheld by the publisher. |
| L5 | 05 | Wu and Sitter (2001) "model calibration" is the survey name for "compute what each record would report, then calibrate". | Wrong | Model calibration fits a model for the study variable and needs auxiliary values for every unit in the population (the title says "complete auxiliary information"). The censored-target idea is ordinary calibration on a derived per-record variable whose total is published. |
| L6 | 05 | Blanchet, Fournier and Piketty, generalized Pareto curves, *RIW* 68: 263–288. | Confirmed | Crossref record and abstract: better than common interpolation "for the top half". Issued online 2021, in the 2022 volume. |
| L7 | 04, 05, 06 | Hainmueller (2012), *Political Analysis* 20: 25–46; entropy balancing is the same estimator as raking. | Confirmed | Crossref record. The equivalence is to calibration with the raking distance. |
| L8 | 06 | Census TopDown (Abowd et al. 2022): inverse-variance least squares performed best; multi-pass; "concerning trend in the bias"; "uncertainty principle". | Confirmed | arXiv full text, sections 7.1–7.2, quotes found verbatim. For the transfer to Tax-Data see M26. |
| L9 | 04, 06 | PolicyEngine `microcalibrate`: Adam on log weights, loss = mean ((estimate − target + 1)/(target + 1))², no distance-to-prior term, optional L0 and dropout. | Confirmed | Source read (`reweight.py`, `utils/metrics.py`). Initial weights add uniform noise (default scale 10). |
| L10 | 05 | Treasury TP-12 (May 2023): 84 targets (35 counts, 49 dollar amounts), objective Σ w Φ(x) with Φ(x) = x⁴ + x⁻⁴ − 2, hard targets, about 40 growth factors, Gillette (1989), "balloon". | Confirmed | PDF text extracted, pp. 18–22. TP-12 also solves through one multiplier per target, and warns that more targets lead to non-convergence. |
| L11 | 05, 06 (04 could not find it) | Ireland and Kullback (1968), *Biometrika* 55: 179–188; raking reaches the minimum relative-entropy table. | Confirmed | Crossref record. Content from my knowledge. 04's "not found" is resolved. |
| L12 | 06 | TabDDPM, TabSyn, TabDiff, ForestDiffusion/ForestFlow, CTGAN; Stoian et al. constraint layer. | Confirmed | arXiv records and abstracts. For example, ForestFlow uses a 27-dataset benchmark, beats deep generators at generation, and trains on CPUs. |
| L13 | 06 | Smith, Diamant and Trippe (2025): results "with up to 99 simultaneous constraints". | Wrong | The current arXiv abstract (v4, ICML 2026) says "hundreds of simultaneous constraints". 06 may have read an earlier version. |
| L14 | 06 | Tax-microdata-benchmarking: quadratic program in Clarabel, Σ(x − 1)² + 10⁶ Σ s², ±0.5%; area lessons (6–8 orders of magnitude, −10.5%, +7.4%, −6.23% to −0.18%). | Confirmed | `tmd/utils/reweight.py` docstring and `AREA_WEIGHTING_LESSONS.md` read. |
| L15 | 06 | Snyder et al. (2008), *Monthly Weather Review* 136: 4629–4640; Agapiou et al. (2017), *Statistical Science*. | Confirmed (existence) | Crossref and arXiv. For their applicability see M17. |
| L16 | 04 | Schennach (2005), *Biometrika* 92: 31–46; a Bayesian derivation of exponential tilting. | Confirmed | Crossref record. |
| L17 | 06 §1.1 | Csiszár (1975) "formalized" that minimizing KL picks the most probable population. | Overstated | Csiszár (1975) gives the geometry of the minimum-relative-entropy projection. The "most probable population" result is the conditional limit theorem (Van Campenhout and Cover 1981; Csiszár 1984). From my knowledge. |
| L18 | 04 | SOI publishes coefficients of variation for Publication 1304 estimates. | Uncheckable | Flagged as unverified by 04; not checked here. |

## Objections and omissions

### Where the three reports agree because they share a blind spot

- **They all assume that each record's weight is uncertain independently of the
  others.** All three adopt the relative-entropy prior on weights, which treats
  each record's weight error as separate. Under that prior, any aggregate's prior
  uncertainty shrinks like one over the square root of the number of records.
  The errors that matter for Tax-Data are shared across many records:
  - the aging step: growth by income type and by position in the distribution;
  - the imputation models;
  - concept gaps between the PUF and the published tables.

  A prior that represents those errors has a few parameters per income type and
  year: the shape parameters in 04's formulation A and 06's prototype 3. The
  weight prior then becomes a secondary regularizer, not the main one. This also
  undercuts the identification measure (M16), which reports how much
  independent per-record noise the targets remove.
- **The current linear program is described without reading its targets.** None
  of the reports notes that the 188 targets are counts only, and that dollars
  are aligned by the value rescale (summary, item 3). None notes the systematic
  preference for high-weight records (M4).
- **The convergence on entropy rests on convention.** Positivity and the history
  of raking are good reasons for entropy. The Bayesian argument does not supply
  one (M10–M12), and the direction should be chosen on behaviour, as 04 says.
  The reverse direction (empirical likelihood) strongly resists driving a weight
  toward zero. That matters for the low-weight high-income records, which the
  usual raking direction lets go to near zero cheaply.

### Costs of soft targets when the precisions are unknown

- **IRS precisions are not the real error.** The published sampling error of SOI
  totals is small; near-exact counts get σ near zero and behave as exact
  targets. What is actually uncertain for IRS targets is:
  - how well PUF variables match the table definitions;
  - the effect of aging the 2015 file to the target year;
  - the age groups, which are imputed in `process_puf.R` and then targeted.

  Each of these errors has to be set by judgment. The answer then depends on the
  ratio of each σ to α, and with a few hundred targets the data say little about
  them.
- **The precisions can be partly estimated.** Under the linearized model the
  published values have a known prior-predictive distribution: normal, centred
  on the prior's aggregates, with variance equal to the sum of the prior-implied
  variance and S. Maximizing that likelihood over α and one scale factor per
  source group (IRS counts, IRS amounts, BEA, Financial Accounts, projections)
  is a standard empirical-Bayes step. It is the correct form of the check 06
  misstates (M18). It would also absorb any misspecification of the prior's
  correlation structure into the estimated scales, so it is a sanity check, not
  an oracle. Holding out targets (04) is the complementary check.
- **A band costs less than a penalty when the bound is all you can defend.** If
  only "within 1%" can be defended, a flat-bottomed penalty or a hard band says
  so honestly. 06 notes the correspondence; none of the reports recommends
  using it for IRS counts.

### Sampling error and disclosure protection in the 2015 PUF

- **The aggregate records are dropped.** `process_puf.R` removes the records with
  RECID ≥ 999996 ("Remove aggregate returns (for now)"). Those records stand in
  for the highest-income returns. The $10 million-plus AGI targets are therefore
  met by the remaining records. None of the reports mentions this, although all
  three discuss the top tail.
- **Blurred values make the reporting calculator noisy.** The PUF blurs most
  amounts. Running tax law on blurred components (who itemizes, AGI bin
  membership, thresholds) gives a nonlinear function of noisy inputs. That is
  biased near thresholds, even with weights fixed. 05 mentions deconvolution
  only "if the noise process is known"; the reports do not connect it to the
  censored-table plan.
- **The top of the prior is a small sample.** The PUF's top cells hold few
  records with small weights. A single prior strength treats those as being as
  reliable as the middle; 04 notes that the strength should vary by cell.
  Calibration does nothing about sampling noise in directions that are not
  targeted.

### Computational traps in the dual with thousands of targets

- **Mixed units make the problem badly conditioned.** Counts near 10⁶ and dollar
  totals near 10¹² need per-target rescaling. The tilt solver already learned
  this (its comment on "24 orders of magnitude").
- **Exact targets can make the dual diverge silently.** With exact or nearly
  exact targets, nested or rounded tables make the dual singular or unbounded,
  and the optimizer can still report success (M9 toy). Use soft or banded
  targets for anything nested, and check the rank of the target matrix.
- **Thin cells push multipliers large.** Large multipliers overflow the
  exponential and concentrate weight. Use log-domain sums, report effective
  sample size per cell, and use a bounded distance where a cell has few records.
- **Fine targets compound.** A record that falls in many fine cells multiplies
  its factors together, so fine cross-classifications compound into extreme
  weights faster than one cell at a time suggests. TP-12 reports the same
  experience with hard targets.

### An alternative the reports do not weigh

Keep the existing targets and tolerance bands, and replace the linear program's
objective with a bounded distance, solved through its dual. The candidates are
the Deville–Särndal–Sautory logit, Treasury's Φ, or the Tax-Calculator team's
quadratic program. This removes the corner solutions with no new parameters and
is the established practice at Treasury and in PSL. Precision-weighted soft
targets can then be added for the targets that need them: cross-source totals,
projection years and imputed variables. They should come with an explicit,
estimated scale per source group.
