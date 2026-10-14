---
title: "Literature review: machine learning, Bayesian statistics and scientific computing for fitting microdata to many targets"
role: evidence
workstream: rebuild
status: current
updated: 2026-10-07
sot: research/rebuild/BRIEF.md
supersedes: []
superseded_by: null
---

# Machine learning, Bayesian statistics and scientific computing: literature relevant to the Tax-Data rebuild

## Summary

1. **The brief's framing is standard in several fields, and they all solve it the same way.** Weather forecasting, radio astronomy, the Census Bureau's 2020 disclosure system, privacy-protected synthetic data and recent machine learning all minimize one objective: distance from the prior plus each target's squared miss divided by its variance (Section 1).
2. **If only weights change, the problem is small, convex and fast.** With the Kullback–Leibler divergence as the distance, each new weight is the prior weight times exp(a weighted sum of the record's target contributions). There is one unknown per target, not per record, a single optimum, no corner solutions, and a "price" per target that shows which targets strain the prior.
3. **Targets computed through tax law are linear in the weights.** A censored total such as itemized deductions claimed in 2019 is a sum over records of a fixed function of each record, so it needs no special handling when only weights change. Discontinuities matter only when record values change; smoothing and perturbation methods handle them.
4. **Reweighting alone degrades as targets tighten and years move away from 2015.** Importance-sampling and particle-filter theory says weight collapses onto few records. The remedies are to move values (parameterized aging) or add records (clones, generators), then reweight.
5. **PolicyEngine (code read)** fits log weights by gradient descent on a GPU against about 13,000 targets, using mean squared relative error and an optional sparsity penalty. It has **no prior-distance term**, so the result depends on optimizer settings. The Tax-Calculator team's file solves a quadratic program with ±0.5% bands, uses constraint prices to drop costly targets, and already targets capped SALT by running tax law.
6. **The closest technical precedents are privacy-protected synthetic data and the Census TopDown algorithm.** TopDown keeps invariants exact, weights the noisy targets by inverse variance, and fits precise totals in an earlier pass because fitting everything at once biased them. RAP adjusts record values by gradient descent in JAX.
7. **Data assimilation supplies checks.** Concept mismatch becomes extra variance on a target. After fitting, the normalized misses should average about one in mean square; if they do not, the stated precisions are wrong. A joint multi-year fit with a penalty on year-to-year change makes projection "a year with few targets".
8. **Deep tabular generators (TabDDPM, TabSyn, TabDiff, ForestFlow) are improving but untested on tax data.** Identities must be built in by construction. The Urban–IRS synthetic files used sequential regression trees. Use generators as a source of prior records, then reweight.
9. **Learning-from-aggregates methods can fit imputation parameters to SOI cross-tabulations,** but aggregates identify only what varies across cells. Physics "unfolding" shows the same limit for censored tables: where the filter hides the data, the answer is the prior.
10. **Optimal transport's role is narrow.** Sinkhorn's algorithm is raking, and optimal-transport matching (used by Blanchet, Saez and Zucman) is good for attaching survey variables. It is not the main fitting engine, because the targets are aggregates.
11. **Full Bayesian machinery is unnecessary for a single best estimate.** A cheap curvature-based approximation gives uncertainty if it is wanted.

**Recommended prototypes (Section 11):**
- (1) Maximum-entropy reweighting with an explicit standard error per target, replacing the linear program.
- (2) Several imputation draws per record, with the same reweighting choosing among them.
- (3) A joint multi-year fit with a few shape parameters per income type and a random-walk prior.
- (4) Later: one conditional generator as imputation prior and source of new records.

## How to read this document

**Scope.** This review covers machine learning, Bayesian statistics and scientific computing. Classical survey calibration, raking and data fusion are covered in a companion review; I touch on them only where the newer literature reinterprets them.

**Verification labels.** In the reference table at the end:
- **Read** means I opened the document, code or page and read the relevant part.
- **Abstract** means I read the abstract through the arXiv interface or the publisher page.
- **Metadata** means I confirmed title, authors, venue and year through the arXiv or Crossref registries but did not read the content.
- **Believed** means I did not check it.

Claims in the text about a paper's content go no further than its label allows. Where they do go further, I say so.

**Search limits.** Partway through the work, the shared web-search budget for this session ran out. After that I confirmed papers I already knew by direct lookup only, never by new searches. Section 12 lists what I could not look for as a result.

**Notation.** Throughout:
- *records* are rows of the file, indexed by i;
- *prior weights* d_i are the weights before fitting (for example, 2015 PUF weights aged to the target year);
- *targets* y_j are published aggregates, each with a *standard error* σ_j, meaning its stated precision;
- *a_ij* is record i's contribution to target j: its wages if the target is total wages in an AGI bracket it falls in, 1 if the target is a count of returns it belongs to, and 0 otherwise.

The file's estimate of target j is the weighted sum Σ_i w_i a_ij.

---

## 1. The common mathematical core

### 1.1 The objective

The brief's framing can be written as one minimization:

> minimize over weights w: **D(w, d)** + Σ_j (Σ_i w_i a_ij − y_j)² / (2 σ_j²)
> subject to the hard constraints.

The first term measures how far the new weights are from the prior. The second penalizes each target's miss in proportion to how precise that target is. A target with a small σ_j is fitted closely, and one with a large σ_j barely matters.

**Why this is "Bayesian".** If the published target equals the true aggregate plus normally distributed error with standard deviation σ_j, then the second term is minus the log-likelihood of the targets. If the first term is minus the log of a prior probability, then minimizing the sum gives the *maximum a posteriori* (MAP) estimate, meaning the most probable file given the prior and the targets. MAP estimation and penalized optimization are the same thing under this reading.

**Why Kullback–Leibler divergence is the natural first term.** Take D to be the Kullback–Leibler divergence, Σ_i [w_i log(w_i/d_i) − w_i + d_i]. Sanov's theorem in probability says that the chance a large population drawn from the prior would show the weighted composition w falls off like exp(−n × KL(w, d)). So minimizing the KL divergence subject to the targets picks the most probable population consistent with them. Csiszár formalized this in 1975 ([Csiszár 1975](https://doi.org/10.1214/aop/1176996454)), building on Jaynes's maximum-entropy principle ([Jaynes 1957](https://doi.org/10.1103/PhysRev.106.620)). For contingency tables, Ireland and Kullback showed in 1968 that raking computes exactly this projection ([Ireland and Kullback 1968](https://doi.org/10.1093/biomet/55.1.179)). Survey statisticians know it as the "raking" distance function in calibration ([Deville and Särndal 1992](https://doi.org/10.1080/01621459.1992.10475217)).

### 1.2 The solution and its "dual"

With the KL divergence and normally distributed target errors, the optimum has a closed form. I derived the form below myself; it matches the general results in [Dudík, Phillips and Schapire 2007](https://www.jmlr.org/papers/v8/dudik07a.html).

> w_i = d_i × exp( Σ_j λ_j a_ij )

There is one number λ_j per target. The λ's are found by minimizing a second, smaller function, the *dual* (an equivalent problem whose unknowns are one price per target):

> Σ_i d_i exp( Σ_j λ_j a_ij ) − Σ_j λ_j y_j + ½ Σ_j σ_j² λ_j²

Six properties follow.

- **Size.** The dual has as many unknowns as there are targets (188 today), not as many as records (207,692). It is smooth and convex, so it has one optimum, and standard quasi-Newton methods such as L-BFGS find it reliably.
- **Prices.** At the optimum, the miss on target j equals −σ_j² λ_j. Each λ_j is a shadow price: how much the prior would have to bend per unit of that target. A target with a very large price is fighting the prior or other targets.
- **No corners.** Every record's weight changes smoothly with its characteristics. Nothing piles up at bounds, unlike the current linear program, where 62% of records sit at 0.34 and 38% at 1.66 (brief, checked 2026-10-06).
- **Nonnegativity.** It is automatic, because exp is positive.
- **Hard targets as a limit.** As σ_j → 0 the target becomes an exact equality. As σ_j → ∞ it drops out. So "hard versus soft" is one dial per target, and the brief's explicit separation can be implemented either by setting σ_j = 0 or by imposing the constraint separately (Section 1.4).
- **Tolerance bands.** "Hit within ±0.5%" bands, which are what the current LP and the Tax-Calculator file use, correspond to replacing the squared penalty with a flat-bottomed one. In the dual this becomes an absolute-value term on λ_j. Dudík, Phillips and Schapire (2007) give the general correspondence between relaxed constraints and penalties on the dual; "approximately balancing weights" ([Wang and Zubizarreta 2020](https://arxiv.org/abs/1705.00998)) is the survey-statistics version.

This is the same solution form as "exponential tilting" and "entropy balancing" ([Hainmueller 2012](https://doi.org/10.1093/pan/mpr025)). Tax-Data's wealth imputation already uses an exponential tilt to correct aggregate drift, according to the project's notes. The proposal here applies the same form to the whole file at once.

**Other distances.** Replacing KL with a squared distance gives the generalized-regression (GREG) estimator family. That family can produce negative weights unless bounded, and the survey-statistics review covers those trade-offs. A distance on the logarithm of the adjustment factor corresponds to a log-normal prior on factors. All of these fit the same framework.

### 1.3 Relative-error losses are a special case

Suppose each target's standard error is proportional to its size, σ_j = r_j |y_j|. Then the penalty is ½ ((estimate − y_j)/y_j)² / r_j², which is squared relative error weighted by 1/r_j². PolicyEngine's loss is this with every r_j equal and no prior term (Section 9). Setting r_j per source is how the brief's "precision" enters. For example: near-exact IRS counts r ≈ 0.5%, rougher BEA totals r ≈ 5%, long-run projections r ≈ 10–20%. Adding a small absolute floor to σ_j avoids the instability that pure relative error has for targets near zero. PolicyEngine handles that instability by adding 1 to the denominator.

### 1.4 Ways to impose hard constraints

1. **By construction.** Generate or adjust total wages and the primary earner's share, so that wages1 + wages2 = wages holds exactly. Keep weights positive by working with log weights. Hold a population block's total fixed by writing its weights as a fixed total times shares that sum to one. Reweighting has a built-in advantage here: it never changes record values, so every within-record identity in the prior survives automatically.
2. **As exact equalities in the dual** (σ_j = 0). This requires the constraints to be jointly feasible on the existing records.
3. **By projection after each step.** Examples are the constraint layer for generative models in [Stoian et al. 2024](https://arxiv.org/abs/2402.04823), which clamps each generated variable within bounds implied by linear inequalities, and the DC3 method ([Donti, Rolnick and Kolter 2021](https://arxiv.org/abs/2104.12225)).
4. **In passes.** Fit and fix the most precise quantities first, then fit the rest subject to them. This is the Census Bureau's remedy described in Section 3.2.

### 1.5 Targets computed by applying tax law are still linear in the weights

The brief's censored-table idea computes, for each record, what the 2019 law would make it report. That means whether it itemizes and, if so, its itemized amount; the result is compared with SOI's 2019 total. Call record i's reported amount r_i(x_i; law_2019). The file's estimate is Σ_i w_i r_i, which is linear in w no matter how discontinuous r_i is in x_i. So censored targets slot straight into the reweighting of Section 1.2 as ordinary columns of a_ij. The Tax-Calculator file already does this for SALT (Section 9.2).

The difficulty with censored tables is identification, not computation. The table says nothing about deductible expenses of non-itemizers, so in that region the answer is whatever the prior says (Section 10).

### 1.6 Where reweighting stops working

Reweighting can only produce distributions supported on the existing records. Two results bound how far it can go.

- **Weight collapse.** In importance sampling and particle filtering, the number of samples needed for stable weights grows very fast, roughly exponentially, with how much information the observations carry relative to the prior. [Snyder, Bengtsson, Bickel and Anderson 2008](https://doi.org/10.1175/2008MWR2529.1) showed this for weather particle filters. [Agapiou, Papaspiliopoulos, Sanz-Alonso and Stuart (arXiv 2015)](https://arxiv.org/abs/1511.06196) unify the theory. In Tax-Data terms, collapse comes from more and tighter targets, finer cross-tabulations, and years further from 2015.
- **Shape.** If 2030's wage distribution is more unequal than 2015's in a way the 2015 records cannot represent by reweighting, no weights fix it. Today's pipeline has this limitation too: shapes are frozen at 2015 apart from weights (brief, step 7).

Two remedies appear in every field reviewed:
- *move values*, through a parameterized transformation of record values (Section 11, prototype 3) or a transport map (Section 5);
- *add records*: clones with new random draws, which PolicyEngine and sequential Monte Carlo use (Section 6.6), or draws from a generative model (Section 4).

Either way the final step is the same reweighting.

### 1.7 The same objective in other fields

| Field | Name | Prior term | Target term | Unknowns |
|---|---|---|---|---|
| Survey statistics | Calibration, raking | Distance to design weights | Exact or banded constraints | Weights |
| Weather forecasting | 3D-Var / 4D-Var | (x − x_b)ᵀ B⁻¹ (x − x_b) | (y − H(x))ᵀ R⁻¹ (y − H(x)) | Model state values |
| Radio astronomy | Maximum-entropy imaging ([Skilling and Bryan 1984](https://doi.org/10.1093/mnras/211.1.111)) | Entropy relative to a default image | χ² against noisy measurements | Nonnegative pixel intensities |
| Census 2020 | TopDown ([Abowd et al. 2022](https://doi.org/10.1162/99608f92.529e3cb9)) | None (invariants exact) | Inverse-variance-weighted least squares | Nonnegative cell counts |
| Differential privacy | Private-PGM ([McKenna, Sheldon and Miklau 2019](https://arxiv.org/abs/1901.09136)) | Maximum entropy among consistent distributions | Negative log-likelihood of noisy marginals | Graphical-model parameters |
| Machine learning | Posterior regularization ([Ganchev et al. 2010](https://jmlr.org/papers/v11/ganchev10a.html)); calibrating generative models ([Smith, Diamant and Trippe 2025](https://arxiv.org/abs/2510.10020)) | KL to the base model | Expectation constraints, exact or penalized | Model distribution |

The maximum-entropy imaging work is the closest match to weights-only reweighting: nonnegative unknowns, an entropy prior relative to a "default model", and noisy linear data with known σ. Its practical rule for the overall strength of the data term is to choose it so that the χ² statistic about equals the number of measurements (I know this rule from that literature generally; I did not reread it in the 1984 paper). That gives a check on whether the σ_j's are jointly sensible, and Section 7.3 has the data-assimilation version.

---

## 2. Learning from aggregate data

This literature fits *individual-level* models when only *group-level* outcomes are observed. For Tax-Data, the "groups" are the cells of SOI tables (AGI bracket × filing status and similar), and the individual-level models are the imputation models.

### 2.1 Learning from label proportions

**What it does.** It trains a classifier when each training group ("bag") reports only the share of members in each class, not who is in which class ([Quadrianto, Smola, Caetano and Le 2009](https://www.jmlr.org/papers/v10/quadrianto09a.html); [Yu et al. 2013, ∝SVM](https://arxiv.org/abs/1306.0886)).

**Key assumption.** The conditional model P(class | covariates) is the same in every bag. The bags differ in their covariate mix, and that difference identifies the model.

**Uncertainty and hard constraints.** Usually none in the classical form; the proportions are treated as exact.

**Scale and hardware.** Standard machine-learning losses that run on GPUs.

**Maturity.** Established for more than 15 years, with a steady stream of papers.

**Fit.** Moderate. Example: the share of returns claiming a new deduction (tips, overtime) by AGI bin could adjust an incidence model trained on SIPP or CPS. In practice the reweighting of Section 1 does the same job more simply, by adjusting weights instead of model parameters.

### 2.2 Learning from aggregate observations and bag-level losses

**What it does.** [Zhang, Charoenphakdee, Wu and Sugiyama 2020](https://arxiv.org/abs/2004.06316) give a general maximum-likelihood framework for groups that report a mean, a rank, or another summary. [Javanmard et al. 2024](https://arxiv.org/abs/2401.11081) compare two losses. The "bag-level" loss matches aggregated predictions to aggregate responses. The "instance-level" loss fits each member's prediction to its bag's aggregate. They show the instance-level loss is a regularized form of the bag-level one, and they propose an estimator that interpolates between the two.

**Key assumption.** As in 2.1.

**Fit.** Moderate, for imputation models. Their result supports a practical choice for Tax-Data: fit imputation parameters by matching *aggregated predictions to SOI aggregates* (bag-level), regularized toward the survey-trained model. That is the same prior-plus-targets structure, applied to model parameters instead of record weights.

### 2.3 Ecological inference and distribution regression

**What it does.** Ecological inference recovers individual-level relationships (for example, vote choice by demographic group) from area-level totals. [Flaxman, Wang and Smola 2015](https://doi.org/10.1145/2783258.2783300) and [Flaxman, Sutherland, Wang and Teh 2016](https://arxiv.org/abs/1611.03787) represent each area by the distribution of its residents' characteristics from census microdata. That approach is called *distribution regression*: the input is a whole distribution, summarized by an average of nonlinear features. They then fit a model whose aggregated predictions match the area totals. [Szabó et al. 2016](https://arxiv.org/abs/1411.2066) give the theory.

**Key assumption.** The individual-level relationship is common across areas, or varies smoothly. The *ecological fallacy* is the standing warning that aggregates can be consistent with very different individual-level patterns.

**Maturity.** Mature in political science; the machine-learning variants are research code.

**Fit.** Low to moderate. Tax-Data already has individual-level covariates on every record, so the full ecological-inference apparatus is unnecessary. The warning still applies: SOI cross-tabulations identify only what varies across their cells.

### 2.4 Disaggregation regression and Gaussian-process models of aggregates

**What it does.** Spatial epidemiology estimates fine-grained disease risk when cases are reported only by district. [Law et al. 2018](https://arxiv.org/abs/1805.08463) do this with Gaussian processes and variational approximations at the scale of a million observations. The R package *disaggregation* ([Nandi et al. 2020](https://arxiv.org/abs/2001.04847)) implements the Bayesian version.

**Key assumption.** Smoothness of the latent fine-scale surface; a Poisson model for counts.

**Uncertainty.** These are full Bayesian models that give posterior intervals.

**Fit.** Low. The structure (fine-scale model, coarse observations, explicit likelihood) matches ours, but the spatial-smoothness prior has no counterpart in tax data.

### 2.5 Collective graphical models

**What it does.** These models estimate the parameters of a graphical model of individual behaviour from aggregate counts of individuals in each state. Sheldon and Dietterich introduced them in 2011 at NeurIPS (I did not open that paper); a later approximation is in [Liu, Sheldon and Dietterich 2014](https://arxiv.org/abs/1405.5156). The same group later built Private-PGM (Section 3.1).

**Fit.** Low directly. The idea of a graphical model as a compact joint distribution fitted to marginals reappears in Section 3.

### 2.6 Posterior regularization and learning from measurements

**What it does.** These methods take a probabilistic model and require its predicted *expectations* to satisfy constraints, such as "on average 30% of these are class A". They do this by projecting the model onto the set of distributions that satisfy the constraints, in KL divergence ([Ganchev, Graça, Gillenwater and Taskar 2010](https://jmlr.org/papers/v11/ganchev10a.html); [Liang, Jordan and Klein 2009, "Learning from measurements in exponential families"](https://doi.org/10.1145/1553374.1553457); [Hu et al. 2018](https://arxiv.org/abs/1806.09764) for deep generative models). The projection is an exponential tilt, as in Section 1.2. Liang, Jordan and Klein treat aggregate statistics explicitly as noisy *measurements*.

**Key assumption.** The base model is a reasonable prior. The constraints are expectations, meaning population aggregates.

**Fit.** Conceptual support for the framing. These methods apply the Section 1 mathematics to a model's distribution rather than to a sample's weights.

---

## 3. Private synthetic data and official-statistics disclosure systems

This literature solves nearly our problem: produce microdata matching a large set of aggregate statistics, each observed with noise of *known variance*, with some aggregates held exact. The noise there is added deliberately for privacy rather than arising from survey sampling or projection, but the estimation problem is the same. The main difference is that these methods usually have only a weak prior, while Tax-Data has a strong one.

### 3.1 Graphical-model estimation from noisy marginals (Private-PGM)

**What it does.** [McKenna, Sheldon and Miklau 2019](https://arxiv.org/abs/1901.09136) estimate a high-dimensional distribution from noisy measurements of low-dimensional marginals (cross-tabulations). They minimize a loss between the model's marginals and the measurements; in the general case the loss is the negative log-likelihood of the measurements. The paper notes that the losses "are easily adapted to varying degrees of noise". Among all distributions with the fitted marginals, the estimate is the one with *maximum entropy*, which takes the form of a graphical model.

**Key assumption.** The measured marginals capture the important structure; the joint distribution is otherwise as uninformative as possible.

**Uncertainty and hard constraints.** Noise variance enters the loss. Exact constraints can be added.

**Scale and hardware.** Efficient when the marginals are low-dimensional. The AIM method ([McKenna et al. 2022](https://arxiv.org/abs/2201.12677)), which builds on it, is a strong performer in differential-privacy synthetic-data benchmarks (I believe this from the abstract and general knowledge; I did not check benchmark tables).

**Maturity.** Open-source research code, widely used in privacy research.

**Fit.** Moderate. It is the cleanest example of "maximum entropy given noisy targets", but it works on discrete domains. Continuous incomes would have to be binned, and binning heavy-tailed variables loses precision at the top.

### 3.2 The Census Bureau's 2020 TopDown algorithm

**What it does.** It turns noisy measurements of many census tabulations into a complete microdata file, one record per person ([Abowd et al. 2022](https://doi.org/10.1162/99608f92.529e3cb9); [arXiv version](https://arxiv.org/abs/2204.08986), sections read). *Invariants* chosen as a matter of policy, such as state population totals, are held exact. So are "edit constraints" and "structural zeros", meaning impossible combinations. The noisy measurements are fitted by **nonnegative least squares weighted by the inverse of each measurement's variance**. The paper reports that inverse-variance weighting performed best among the weightings tried. A second step then rounds to integers ("controlled rounding").

**A finding that applies directly to Tax-Data.** The authors observed a "concerning trend in the bias" of geographic totals. Even very precise totals came out less accurate than expected when fitted together with a query that has many cells. They trace this to the nonnegativity constraint and describe an "uncertainty principle" for inequality-constrained least squares. Their fix is a *multi-pass* estimator: fit the totals first, then fit the detail subject to the totals.

For Tax-Data this argues for a two-stage version of the soft-target fit:
- fit the precise IRS counts and totals first and treat them as hard;
- then fit the rough targets.

The single-pass version risks letting many rough targets drag the precise ones.

**Scale and hardware.** National scale, hundreds of millions of persons. The paper mentions commercial solvers (Gurobi); I did not check the hardware.

**Maturity.** Production system.

**Fit.** High as a design pattern: invariants versus noisy measurements, inverse-variance weights, passes. It has no prior microdata, so it is not a template for the prior side.

### 3.3 Gradient-based synthetic records: relaxed adaptive projection (RAP, RAP++)

**What it does.** [Aydore et al. 2021](https://arxiv.org/abs/2103.06641) relax a synthetic dataset to continuous values and use gradient descent to make its query answers match the noisy answers. They pick queries adaptively, wherever the synthetic data are currently worst. [Vietri et al. 2022 (RAP++)](https://arxiv.org/abs/2209.07400) handles numerical features without binning. The code ([GitHub](https://github.com/amazon-science/relaxed-adaptive-projection), read) is written in **JAX** and runs on CPU or GPU.

**Key assumption.** A synthetic dataset of fixed size can represent the distribution, and the queries define what matters.

**Uncertainty and hard constraints.** Equal treatment of queries in the basic form; no hard constraints.

**Scale and hardware.** Thousands of queries on census-sized data; GPU.

**Maturity.** Research code.

**Fit.** Moderate. This is the published example closest to "change record values by gradient descent in JAX to match many aggregates". Tax-Data would add a prior term to keep records near their 2015 values, and per-target σ_j.

### 3.4 Multiplicative weights and generator-based methods (MWEM, PEP, GEM)

**What it does.**
- [MWEM (Hardt, Ligett and McSherry 2012)](https://arxiv.org/abs/1012.4763) keeps a distribution over all possible records and repeatedly multiplies the probabilities by exp(step × query error). This is exponentiated-gradient ascent, and it converges to a maximum-entropy-type solution.
- [Liu, Vietri and Wu 2021](https://arxiv.org/abs/2106.07153) unify these iterative methods. They add an entropy-projection variant (PEP) and GEM, which trains a neural-network generator instead of a weight table. GEM "incorporates prior information from public data", which is the role the PUF plays for us.
- A later genetic-algorithm variant is [Liu et al. 2023](https://arxiv.org/abs/2306.03257).

**Fit.** Moderate, as background. GEM's use of public data as a prior for a generator is the closest published analogue to "prior from the PUF, adjusted to targets, with a generator" (Section 4).

### 3.5 The Urban Institute–IRS synthetic files

**What it does.** The Urban Institute and SOI built a fully synthetic supplemental public-use file of 2012 non-filers. It uses *sequential* CART: each variable is drawn from a regression tree fitted on the variables already synthesized, and continuous values are drawn from a Gaussian kernel density around the tree's predictions, so the synthetic values are smoothed and unbounded ([Burman et al. 2019 draft](https://www.irs.gov/pub/irs-soi/19conpanburmanpaper.pdf), read in part). The authors say CART "out-performed regression-based parametric" methods in their work. A fully synthetic PUF followed. Its quality was evaluated through tax-model simulations ([Bowen et al. 2022, *National Tax Journal*](https://doi.org/10.1086/722094), abstract only), and a 2024 article discusses the wider program ([Burman et al. 2024](https://doi.org/10.1086/732683), metadata only).

**Fit.** High as a reference point. It is the only synthetic tax microdata effort I found with published tax-specific evaluation. Its method choice, tree-based sequential synthesis rather than deep networks, is evidence on what works for tax variables.

---

## 4. Generative models for tabular data and synthetic populations

A *generative model* is a model from which new records can be drawn, as opposed to a model that predicts one variable.

### 4.1 Tabular generators: what exists

| Method | What it is | Notes |
|---|---|---|
| CTGAN and TVAE ([Xu et al. 2019](https://arxiv.org/abs/1907.00503)) | A generative adversarial network (two networks trained against each other) and a variational autoencoder (a network that compresses records to a low-dimensional code and back), adapted to mixed tables | First widely used deep tabular generators; often beaten by later methods |
| TabDDPM ([Kotelnikov et al. 2022](https://arxiv.org/abs/2209.15421)) | Diffusion model: learns to reverse a process that gradually adds noise to records | Strong benchmark results at the time |
| TabSyn ([Zhang et al. 2023](https://arxiv.org/abs/2310.09656)) | Diffusion in a learned low-dimensional code space | Reports large error reductions on column distributions and pairwise correlations versus earlier methods |
| CDTD ([Mueller, Gruber and Fok 2023](https://arxiv.org/abs/2312.10431)) | Continuous diffusion for mixed types | Metadata only |
| TabDiff ([Shi et al. 2024, ICLR 2025](https://arxiv.org/abs/2410.20626)) | Joint diffusion for numerical and categorical columns, transformer network | Supports imputing missing columns through "classifier-free guidance" |
| ForestDiffusion / ForestFlow ([Jolicoeur-Martineau, Fatras and Kachman 2023](https://arxiv.org/abs/2309.09968)) | Diffusion and flow matching with gradient-boosted trees (XGBoost) in place of neural networks | On a 27-dataset benchmark, reports beating deep generators at generation; does imputation; trains on CPUs; Python and R code |
| GReaT ([Borisov et al. 2022](https://arxiv.org/abs/2210.06280)) | Fine-tuned language model writing records as text | Interesting, but costly and awkward for numbers |
| TabPFN ([Hollmann et al. 2025, *Nature*](https://doi.org/10.1038/s41586-024-08328-6)) | A transformer pre-trained on millions of simulated small datasets | Mainly a predictor for datasets up to about 10,000 rows. Secondary sources say it also supports generation and density estimation (I did not verify) |
| Synthcity ([Qian, Cebere and van der Schaar 2023](https://arxiv.org/abs/2301.07573)) | Benchmarking library for synthetic-data methods | Useful for comparisons |

**Key assumption, all methods.** The training sample is representative and large enough. The model learns the joint distribution and nothing else, with no external targets.

**Uncertainty and hard constraints.** None natively. Constraints have been added in three ways:
- a constraint layer that clamps each variable within bounds implied by linear inequalities given the variables already generated ([Stoian et al. 2024](https://arxiv.org/abs/2402.04823), read in part). The authors report that unconstrained generators violated constraints for over 95% of samples in some cases. The layer handles inequalities, and equalities only as pairs of inequalities.
- an expert-specified dependency graph ([DATGAN, Lederrey, Hillel and Bierlaire 2022](https://arxiv.org/abs/2203.03489)).
- construction. Generate the total and the share and the identity holds exactly. For Tax-Data this is the simplest and safest option.

**Scale and hardware.** The neural methods train on GPUs in minutes to hours for samples of 10⁵ records. ForestFlow trains on CPUs in parallel.

**Maturity.** Active research. Rankings change between benchmarks. I found no published evaluation of these methods on U.S. tax microdata. The concerns specific to tax data are:
- point masses at zero (most records have zero for most income items);
- extreme upper tails;
- exact accounting identities;
- the fact that what matters downstream is tax liability, a nonlinear function of many variables at once.

Standard benchmarks score column distributions, pairwise correlations and the accuracy of a classifier trained on synthetic data. None of these tests tax-relevant joint structure.

**Fit.** Moderate, in two roles:
- **A joint imputation model.** One conditional generator that draws all non-PUF variables given the PUF variables, in place of about fifteen separate modules. This keeps the dependence among imputed variables, such as wealth and consumption, which separate modules do not model.
- **A source of new records.** Draw extra records where reweighting would otherwise collapse, for example many more elderly records for 2060.

In both roles the generator supplies the *prior*. The targets are then imposed by the reweighting of Section 1. This is the "generate, then tilt" pattern of the next subsection.

### 4.2 Adjusting a generator to match aggregates

- **Importance weights from a classifier.** [Grover et al. 2019](https://arxiv.org/abs/1906.09531) correct a generator's bias by weighting its samples. The weights come from a classifier trained to tell generated records from real ones; its odds estimate the ratio of the two densities. [Choi et al. 2020](https://arxiv.org/abs/1910.12008) use the same device to make a generator match a small reference dataset.
- **Calibrating generative models.** [Smith, Diamant and Trippe 2025 (ICML 2026)](https://arxiv.org/abs/2510.10020) (read in part) fine-tune a generator to be the closest model in KL divergence that satisfies a set of expectation constraints. They note the exact solution is the exponential tilt p(x) exp(αᵀh(x)), and they propose two training losses. The "relax" loss is a penalty on constraint violation plus a KL term, which is exactly the brief's soft-target form. They report results with up to 99 simultaneous constraints and models of up to 9 billion parameters. They also report limitations: performance degrades for events rarer than about 1 in 1,000, and one of the two losses "falls short" beyond about 30 constraints. Rare events matter for Tax-Data: the top 0.1% is exactly a rare event.

**Fit.** These papers support a specific design. Draw a large pool of records from the generator, attach the PUF records themselves, and tilt the weights of the pool to the targets. Retraining the generator is only worth it if the tilted weights become too uneven.

### 4.3 Population synthesis in transport research

Transport modellers build synthetic populations of individuals and households for agent-based travel models. They face the same pair of tasks: generate realistic records, and match census marginals.

- [Borysov, Rich and Pereira 2019](https://doi.org/10.1016/j.trc.2019.07.006) ([arXiv](https://arxiv.org/abs/1808.06910)) use a variational autoencoder on a Danish travel survey. They found that a classical Gibbs sampler "essentially replicates the agents from the original sample". The autoencoder generates plausible combinations not in the sample.
- [Garrido et al. 2020](https://arxiv.org/abs/1909.07689) study *sampling zeros*: combinations that exist in the population but by chance not in the sample. They separate these from *structural zeros*, combinations that are impossible. Deep generators recover sampling zeros at the cost of producing some structural zeros. On their measure the autoencoder did better than the adversarial network.
- [Kim and Bansal 2023](https://arxiv.org/abs/2208.01403) add regularization during training to suppress structural zeros while keeping sampling zeros.
- [Johnsen et al. 2020](https://arxiv.org/abs/2011.06851) use conditional generators to synthesize populations for new developments.

**Fit.** Moderate. The sampling-zero versus structural-zero distinction is the right vocabulary for judging a generator for Tax-Data. Inventing a married couple with Schedule F income in a state where none appeared in the PUF may be correct. Inventing a dependent filer with a spouse is not. The usual practice in this field, generate a pool and then fit it to census marginals with IPF or combinatorial selection, is the same "generate, then tilt" pattern. I could not search for recent papers combining deep generators with calibration in one step, because the search budget had run out (Section 12).

---

## 5. Maximum entropy, optimal transport and matching

### 5.1 Raking, Sinkhorn and entropic optimal transport are one algorithm family

*Optimal transport* (OT) finds the cheapest way to move one distribution of mass onto another, given a cost for moving a unit between any two points. *Entropic* OT adds an entropy term, which makes the problem smooth and solvable by Sinkhorn's algorithm. That algorithm alternately rescales the rows and columns of a matrix until both margins match ([Cuturi 2013](https://arxiv.org/abs/1306.0895); textbook: [Peyré and Cuturi 2019](https://arxiv.org/abs/1803.00567)). This is iterative proportional fitting, the raking algorithm ([Idel 2016](https://arxiv.org/abs/1609.06349) reviews the shared history). [Benamou et al. 2015](https://arxiv.org/abs/1412.5154) generalize it to "iterative Bregman projections", which handle more than two margins and other linear constraints. That is raking with many margins, seen as alternating KL projections.

**Fit.** Raking is a special case of Section 1.2 with exact targets that each partition the records. The dual solver of Section 1.2 handles overlapping, banded and soft targets, which raking does not.

### 5.2 Unbalanced and multi-marginal transport

- *Unbalanced OT* ([Chizat, Peyré, Schmitzer and Vialard 2018](https://arxiv.org/abs/1607.05816)) lets mass be created or destroyed at a cost, so total weight need not be preserved. It is used for growth and death in cell populations.
- *Multi-marginal OT* ([Pass 2015](https://arxiv.org/abs/1406.0026)) couples three or more distributions at once. One example would be the PUF, the CPS and the SCF linked into a single synthetic population with each file's distribution preserved.

**Maturity.** The theory is mature. Multi-marginal problems are expensive beyond a few marginals unless the cost has special structure.

**Fit.** Low to moderate. Multi-marginal matching of several surveys is conceptually right for the imputation stage, but I found no applied economic-microdata example.

### 5.3 Optimal-transport statistical matching

[Blanchet, Saez and Zucman (NBER w30229)](https://www.nber.org/papers/w30229) ([paper](https://eml.berkeley.edu/~saez/BSZ2022.pdf), sections read) bring race, education and age into the distributional national accounts microfiles. They match those files to the March CPS and the SCF with a *constrained* one-to-one statistical match, solved as an OT linear program with an L1 distance on common income variables. They describe it as preserving the multivariate distribution of the transferred variables. They note that a 100,000 × 100,000 cost matrix has 10 billion entries and that recent OT implementations make this feasible.

**Key assumption.** Conditional independence: given the common variables, the transferred variables are unrelated to the base file's other variables. All statistical matching shares this assumption.

**Fit.** Moderate to high for the imputation stage. A constrained match preserves the donor survey's joint distribution of the transferred variables and the base file's weights at the same time; the quantile-regression-forest draws do not guarantee the former. OTT-JAX ([Cuturi et al. 2022](https://arxiv.org/abs/2201.12324)) runs entropic and low-rank OT for large point clouds on GPUs.

### 5.4 Linking cross-sections over time without a panel

Single-cell biology faces Tax-Data's projection problem in another form. Cells are destroyed when measured, so each time point is a separate cross-section. [Schiebinger et al. 2019 (Waddington-OT)](https://doi.org/10.1016/j.cell.2019.01.006) infer how cells move between time points using unbalanced entropic OT, with growth rates for mass creation, across 315,000 profiles at 40 time points. [Lavenant et al. 2021](https://arxiv.org/abs/2102.09204) prove that trajectories can be recovered from cross-sections for a class of processes, by solving one smooth convex problem over all time points jointly. Conditional versions learn transport maps that depend on covariates ([Bunne, Krause and Cuturi 2022](https://arxiv.org/abs/2206.14262)); [Courty et al. 2017](https://arxiv.org/abs/1507.00504) apply OT to shifting one data distribution onto another.

**Key assumption.** Individuals move as little as possible between time points, given the cost.

**Fit.** Low for Tax-Data as specified. The brief allows separate files per year, so individual trajectories are not needed. The OT minimal-movement assumption is also a poor description of true income mobility. In one dimension the OT map between two income distributions is quantile matching, which is useful for aging the shape of a distribution (Section 11, prototype 3), and needs no OT machinery.

### 5.5 Classical econometrics of changing distributions

These two are outside the assigned traditions, but they are the economists' versions of the same tools:
- **DiNardo–Fortin–Lemieux reweighting** ([1996](https://doi.org/10.2307/2171954)) changes the covariate mix by reweighting with odds from a classifier, the same device as Grover et al. 2019.
- **Counterfactual distributions by distribution regression** ([Chernozhukov, Fernández-Val and Melly 2013](https://doi.org/10.3982/ECTA10582)) model the whole conditional distribution so that it can shift.

Together they match the two remedies in Section 1.6: change weights, or change the conditional distribution of values.

---

## 6. Bayesian approaches

### 6.1 Bayesian benchmarking

**What it does.** Small-area estimates are often forced to add up to a reliable national total ("benchmarking"). [Datta, Ghosh, Steorts and Maples 2011](https://doi.org/10.1007/s11749-010-0218-y) derive benchmarked Bayes estimators. These minimize posterior expected loss subject to the benchmark constraint. My understanding, which I did not check against the paper, is that under weighted squared-error loss the discrepancy is spread across areas in proportion to their posterior uncertainty. [Si, Trangucci, Gabry and Gelman 2017](https://arxiv.org/abs/1707.08220) give a Bayesian treatment of survey weighting itself; [Gelman 2007](https://doi.org/10.1214/088342306000000691) is the standard discussion of why weighting and modelling should be reconciled.

**Fit.** Principle only: when an aggregate is enforced, the less certain parts should absorb more of the adjustment. The σ_j weighting in Section 1 does this across targets. Within the file, the closest analogue is letting imputed variables move more than observed PUF variables.

### 6.2 Bayesian melding

**What it does.** A deterministic simulator often ends up with two priors on the same quantity, one on its inputs and one on its outputs, which conflict. [Poole and Raftery 2000](https://doi.org/10.1080/01621459.2000.10474324) combine them by "logarithmic pooling", a weighted geometric average of the two prior densities.

**Fit.** The situation will arise. The aging model implies an aggregate (total wages in 2030 from aged records), and CBO supplies another. Treating CBO's figure as a soft target with its own σ is a simple version of melding, and the melding literature is the reference if a formal treatment is wanted.

### 6.3 Bayesian population reconstruction

**What it does.** [Wheldon, Raftery, Clark and Gerland 2013](https://doi.org/10.1080/01621459.2012.737729) estimate population counts by age, fertility, mortality and migration jointly from fragmentary censuses with measurement error. They embed the demographic accounting identity (the cohort-component projection) exactly in a Bayesian hierarchical model.

**Key assumption.** Priors centred on initial estimates, with stated measurement-error variances; exact accounting identities.

**Scale.** Small: age groups × periods, fitted by Markov chain Monte Carlo (MCMC).

**Fit.** High as a conceptual precedent from demography for the brief's framing: exact accounting identities, priors on components, noisy observations of varying precision. It is not a computational template at microdata scale.

### 6.4 Approximate Bayesian computation and simulation-based inference

**What it does.** Both handle models whose likelihood cannot be written down, only simulated.
- *Approximate Bayesian computation* (ABC) keeps parameter draws whose simulated summaries land close to the observed ones ([Beaumont, Zhang and Balding 2002](https://doi.org/10.1093/genetics/162.4.2025)).
- *Simulation-based inference* (SBI) trains a neural network on (parameter, simulated output) pairs to approximate the posterior or likelihood ([Cranmer, Brehmer and Louppe 2020](https://arxiv.org/abs/1911.01429); [Papamakarios and Murray 2016](https://arxiv.org/abs/1605.06376); toolkit: [sbi](https://arxiv.org/abs/2007.09114); background on the normalizing flows these methods often use: [Papamakarios et al. 2021](https://arxiv.org/abs/1912.02762)).

**Key assumption.** Few parameters, roughly fewer than a few dozen. The simulator is a black box, possibly random.

**Scale.** Thousands to millions of simulator runs.

**Fit.** Low for the main problem. Tax-Data's "simulator" (tax law applied to records) is deterministic, cheap, and as Section 1.5 shows, enters linearly in the weights. Gradient-based MAP is far more efficient. SBI would be worth considering only for a few structural parameters fitted through the full Tax-Simulator as a black box.

### 6.5 Variational inference and probabilistic programming in JAX

**What it does.** Variational inference approximates a posterior with a simpler distribution fitted by optimization ([Blei, Kucukelbir and McAuliffe 2017](https://arxiv.org/abs/1601.00670)). It scales to large data with stochastic gradients ([Hoffman et al. 2013](https://arxiv.org/abs/1206.7051)). How to tell whether it worked: [Yao et al. 2018](https://arxiv.org/abs/1802.02538). JAX libraries: [NumPyro](https://arxiv.org/abs/1912.11554), [BlackJAX](https://arxiv.org/abs/2402.10797).

**Fit.** Low for the brief's single best estimate. If uncertainty is wanted later, a cheaper route exists. The curvature of the dual in Section 1.2 at its optimum is Σ_i w_i a_i a_iᵀ + diag(σ_j²), and inverting it gives an approximate covariance for the target prices λ. That converts into approximate uncertainty for any weighted statistic of the file. This is a *Laplace approximation*: treating the posterior as normal around its peak. Separately, individual imputation models could be made Bayesian hierarchical models in NumPyro.

### 6.6 Sequential Monte Carlo and particle filters as a frame for year-by-year updating

**What it does.** A *particle filter* represents a distribution by weighted samples ("particles") and updates it through time in a cycle:
1. **Move** each particle with the dynamics: aging.
2. **Reweight** by the new observations: the year's targets.
3. **Resample** when the weights become too uneven, duplicating heavy particles and dropping light ones.
4. **Rejuvenate**, jittering the duplicates so they are not identical.

**Fit.** Tax-Data's records are the particles. The cycle names the steps that a multi-year design needs, and Snyder et al. 2008 (Section 1.6) is the warning about how quickly reweighting alone degenerates. "Resample and rejuvenate" is the formal version of "clone records and give the clones fresh draws".

### 6.7 State-space models for distributions over time

**What it does.** A *state-space model* has a hidden state that evolves over time (the transition equation) and noisy observations of it (the measurement equation). The *Kalman filter* ([Kalman 1960](https://doi.org/10.1115/1.3662552)) and its smoother are the linear-Gaussian solutions. [Chang, Chen and Schorfheide 2024 (*Journal of Political Economy*)](https://cowles.yale.edu/research/cfp-1913-heterogeneity-and-aggregate-fluctuations) apply this to the cross-sectional earnings distribution. Their state stacks macroeconomic aggregates and the coefficients of a flexible approximation to the log density, which evolve as a vector autoregression. Their measurement equation allows for sampling error in the density estimated from each year's cross-section. (I saw only a search-result summary of this paper and did not open it.)

**Fit.** Moderate to high, as a structure for multi-year fitting. Summarize each year's file by a modest parameter vector θ_t: the tilt prices λ_t and the shape parameters of Section 11, prototype 3. Give θ_t a random-walk-with-drift prior across years and let each year's targets be noisy observations. Then:
- data-rich years inform neighbouring data-poor years;
- projection years with only CBO and SSA aggregates inherit their shapes from the last data year, with the prior variance growing over the horizon;
- "projection is not a separate method" (brief) holds in a precise sense: a projection year is a year whose measurement equation contains few, imprecise observations.

Solving all years at once gives the smoother's answer, using later years' information as well as earlier ones.

### 6.8 Related applied work: nowcasting distributions

Two applied systems age microdata with aggregates; I note them because they are concrete precedents.
- [Blanchet, Saez and Zucman](https://www.nber.org/papers/w30229) rescale the latest annual microfiles to monthly national-accounts totals component by component. They then change the *within-component* wage distribution using monthly CPS and quarterly QCEW data by demographic cell. They back-test by projecting year t+1 from year t's file and comparing with the actual t+1 file, and they compare against rescaling alone (sections read). The same back-test is the right validation for Tax-Data's aging.
- Eurostat's flash estimates of poverty and inequality age EU-SILC microdata to labour-force survey employment changes and simulate current tax-benefit policy with EUROMOD ([methodological note](https://ec.europa.eu/eurostat/documents/d/experimental-statistics/methodological-note-flash-estimates-2025); I saw only a search summary).

The Federal Reserve's Distributional Financial Accounts combine triennial SCF microdata with quarterly Financial Accounts totals ([Batty et al. 2019](https://doi.org/10.17016/FEDS.2019.017), metadata only), a direct analogue for Tax-Data's wealth variables.

---

## 7. Data assimilation in weather forecasting

### 7.1 The correspondence

*Data assimilation* combines a forecast model's prior estimate of the atmosphere (the "background") with millions of observations of different types and precisions to produce the "analysis", the best estimate of the current state. Three-dimensional variational assimilation (3D-Var) minimizes

> J(x) = ½ (x − x_b)ᵀ B⁻¹ (x − x_b) + ½ (y − H(x))ᵀ R⁻¹ (y − H(x)),

where x_b is the background, B its error covariance, y the observations, H the "observation operator" that maps a model state to what an instrument would report, and R the observation-error covariance ([Lorenc 1986](https://doi.org/10.1002/qj.49711247414); reviews: [Carrassi et al. 2018](https://arxiv.org/abs/1709.02798); [Law, Stuart and Zygalakis 2015](https://arxiv.org/abs/1506.07825)). 4D-Var fits a time window of observations through the forecast model, and uses the model's adjoint to get gradients; an adjoint is the reverse-mode derivative that automatic differentiation now provides. The ensemble Kalman filter ([Evensen 2003](https://doi.org/10.1007/s10236-003-0036-9)) estimates B from an ensemble of forecasts instead; [Iglesias, Law and Stuart 2013](https://doi.org/10.1088/0266-5611/29/4/045001) turn it into a derivative-free optimizer.

| Weather | Tax-Data |
|---|---|
| Background state x_b | Aged prior file (2015 PUF + imputations, aged) |
| Background error covariance B | How the prior may move: the KL distance on weights; prior variances on shape parameters |
| Observations y | Targets |
| Observation operator H | Aggregation, including applying a year's tax law to records |
| Observation error R | σ_j², including concept mismatch |
| Analysis | The calibrated file |
| Forecast model | Aging step |
| Assimilation cycle | Year-by-year update |
| 4D-Var window | Joint multi-year fit |
| Model error ("weak-constraint" 4D-Var) | Allowing the aging rule to be wrong each year |

### 7.2 What the analogy contributes

1. **Covariances between variables come from the prior.** In weather, B decides how an observation of temperature at one place changes wind elsewhere. In reweighting, that role is played by the records themselves. Raising the weight of a record moves all of its variables together, so the prior's correlations carry through without being modelled. Changing record values loses this property unless the value changes are built to respect the joint structure.
2. **Observation operators can be nonlinear and complex.** Satellite radiances pass through a radiative-transfer model before they can be compared with the model state. Running tax law to compare with SOI's censored tables is the same practice.
3. **Representativeness error.** Meteorologists add variance to an observation when it measures something the model cannot represent exactly, such as a point measurement versus a grid-box average. For Tax-Data this is the place to account for concept mismatch: NIPA personal income versus AGI, Financial Accounts household wealth versus the SCF concept. It is better to widen σ_j for mismatched targets than to pretend they are exact.

### 7.3 Checks for whether the stated precisions are right

- **Normalized-residual (innovation) checks.** [Desroziers, Berre, Chapnik and Poli 2005](https://doi.org/10.1256/qj.05.108) show how to estimate observation and background error variances from the statistics of the misses before and after assimilation. The simplest version for Tax-Data: after fitting, the misses divided by σ_j should have mean square about 1 within each source group (IRS counts, BEA totals, projections). If IRS targets show a mean square of 10, their σ's are too small or the prior is badly off. This is the same idea as the maximum-entropy imaging rule in Section 1.7.
- **Quality control.** Assimilation systems reject or down-weight observations whose misses are many σ away from the background. A heavy-tailed error model, such as Student-t errors in place of normal ones, does this automatically. The Tax-Calculator file's use of constraint prices to find costly targets (Section 9.2) does the same by hand.

**Fit.** High as a conceptual and diagnostic framework. The standard algorithms (3D-Var, ensemble Kalman) are not needed as such: weights-only reweighting has its own convex dual, and Tax-Data needs no ensemble.

---

## 8. Differentiable programming and discontinuities

### 8.1 What JAX provides

JAX is a Python library that differentiates numerical code automatically. It compiles that code for CPUs and GPUs, and it vectorizes a function written for one record across all records. Relevant libraries:
- *Optax* for gradient optimizers;
- *Optimistix* for root finding, least squares and minimization, including quasi-Newton methods ([Rader, Lyons and Kidger 2024](https://arxiv.org/abs/2402.09983));
- *OTT-JAX* for optimal transport, including differentiable soft ranks and quantiles ([Cuturi et al. 2022](https://arxiv.org/abs/2201.12324));
- *NumPyro* and *BlackJAX* for Bayesian inference.

[Agrawal et al. 2019](https://arxiv.org/abs/1910.12430) show how to differentiate through convex optimization problems, which would allow a reweighting step to sit inside a larger fitted model. I found no published differentiable implementation of a tax-benefit microsimulation model in JAX or PyTorch. The search returned only conventional microsimulation papers.

### 8.2 Where discontinuities arise and what to do

- **Weights-only fitting.** No issue (Section 1.5). Targets based on AGI brackets, itemization status or any other tax-law classification are fixed columns of a_ij.
- **Changing record values.** A record whose AGI is being changed can cross a bracket boundary, and a record whose deductions are changed can cross the itemization threshold. The fitted total then jumps, and its gradient with respect to the change is zero almost everywhere and undefined at the jump. Standard remedies:
  - *Smooth relaxation.* Replace the indicator 1[itemized > standard] with a logistic curve σ((itemized − standard)/τ), and max(a, b) with a smooth maximum, using a "temperature" τ that is lowered over the fitting run. This is simple, and it works when many records lie near each boundary, which is true for population aggregates.
  - *Perturbation smoothing.* Average the discontinuous function over small random perturbations of the input. This gives an unbiased gradient of a smoothed objective ([Berthet et al. 2020](https://arxiv.org/abs/2002.08676); survey: [Petersen 2022](https://arxiv.org/abs/2209.00616)).
  - *Discrete choices in generators.* The Gumbel-softmax or Concrete relaxations ([Jang, Gu and Poole 2017](https://arxiv.org/abs/1611.01144); [Maddison, Mnih and Teh 2017](https://arxiv.org/abs/1611.00712)) and their "straight-through" variants.
  - *Targets defined on ranks.* Targets such as "share of income going to the top 1%" use differentiable sorting ([Grover et al. 2019](https://arxiv.org/abs/1903.08850); soft ranks in OTT-JAX).
- **Scale precedent.** Differentiable agent-based models with millions of agents on GPUs exist in epidemiology ([Chopra et al. 2023](https://arxiv.org/abs/2207.09714); [Chopra et al. 2024](https://arxiv.org/abs/2409.10568)). They calibrate a few parameters of a population simulator to aggregate time series by gradient descent.

**Fit.** Tax-Data needs only a small piece of tax law inside the fitting loop: the standard deduction, the itemization choice and the SALT cap for the censored targets. For weights-only fitting that piece can be any implementation, including a call to the existing simulator. A differentiable JAX version is needed only if record values are fitted against tax-law-filtered targets.

---

## 9. Gradient-based reweighting systems in practice

### 9.1 PolicyEngine (verified in code and documentation)

**What it does.** PolicyEngine builds U.S. microdata from the CPS with variables imputed from the PUF, and reweights it to administrative targets. Sources: [microcalibrate package](https://github.com/PolicyEngine/microcalibrate) (code read) and [policyengine-us-data documentation](https://github.com/PolicyEngine/policyengine-us-data) (methodology and calibration pages read).

- **Parameters and optimizer.** Log weights, optimized with Adam, a stochastic gradient method, in PyTorch. Initial weights are the original weights plus random noise. Optional "dropout" randomly removes weights during training.
- **Loss.** The mean over targets of ((estimate − target + 1)/(target + 1))². This is squared relative error with every target equally important; the "+1" protects small targets. **There is no term for distance from the original weights.** The original weights influence the result only as the starting point, so the answer depends on the learning rate and number of steps; a comment in the code says results are "pretty sensitive to learning rates". The physics unfolding literature makes the same observation: stopping early acts as an unstated regularizer (Section 10).
- **Sparsity.** An optional second stage multiplies each weight by a "Hard Concrete" gate, a smooth approximation of an on/off switch ([Louizos, Welling and Kingma 2018](https://arxiv.org/abs/1712.01312)), with a penalty on the number of gates that are on. The documentation's national preset keeps about 50,000 records, and the local-area preset 3–4 million. They give computational speed as the reason: fewer records with nonzero weight make simulation faster. They chose L0 over an L1 penalty because L1 also shrinks the surviving weights.
- **Records generated by cloning.** In the current production pipeline, each of about 12,000 stratified households is cloned 430 times, and each clone is assigned a random census block. With the PUF-imputed copies this gives about 10.3 million columns. The tax and benefit rules of each clone's state are simulated, so the optimizer chooses which clones to weight up in each geography. About 13,000 targets are used out of about 37,000 in their database, chosen as "the largest number of targets that converge stably given the number of clones". Fitting takes 5–20 minutes on a GPU (A100 or T4) with Adam at learning rate 0.15 over 1,000–1,500 epochs.
- **Long-run projection.** Their documentation describes uprating income categories by CBO projections and reweighting to SSA age projections through 2100 with IPF or GREG. Social Security benefit totals are an optional target.

**Fit.** Their system shows that gradient reweighting of millions of records against thousands of targets is routine on one GPU. The cloning idea is directly useful (Section 11, prototype 2). Their loss has no explicit prior and treats all targets as equally precise, so it does not implement the brief's framing. Their sparsity penalty serves a compute goal that Tax-Data may not share, since Tax-Simulator already runs on GPUs. If a smaller file is wanted, drawing a weighted sample from the calibrated file afterwards achieves it without distorting the fit.

### 9.2 PSL Tax-Calculator microdata (tax-microdata-benchmarking; verified in code)

The national reweighting in [tax-microdata-benchmarking](https://github.com/PSLmodels/tax-microdata-benchmarking) (`tmd/utils/reweight.py`, read) is a quadratic program solved with the Clarabel solver:

> minimize Σ_i (x_i − 1)² + M Σ_j s_j², with each target within ±0.5% of its value except for a slack s_j,

with M = 10⁶. Their area-weighting notes (`tmd/areas/AREA_WEIGHTING_LESSONS.md`, read) report three findings:
- **Constraint prices.** The solver's prices showed that filing-status counts in the $1 million-and-over AGI bin cost "6–8 orders of magnitude" more than any other target. Dropping them removed almost all weight distortion.
- **Bystander variables.** Untargeted variables move as a side effect of reweighting. Examples: student loan interest −10.5% and tax-exempt interest +7.4% when summed across states.
- **Censored SALT.** They target SALT after the cap, as computed by Tax-Calculator, rather than uncapped SALT. This cut SALT aggregation error from −6.23% to −0.18%.

**Fit.** All three are directly transferable checks. The constraint-price diagnostic is free with the dual method of Section 1.2.

### 9.3 Practical issues across these systems

- **Weight sparsity.** The current LP gives the most extreme form of sparsity in the adjustment factors (every record at a bound). Entropy or chi-square priors give dense, smooth adjustments, and L0 gives deliberate sparsity. For a file meant to represent a population, dense is the safer default.
- **Regularization.** An explicit prior term (Section 1) makes the answer well-defined and independent of the optimizer. Implicit regularization through early stopping does not.
- **Convergence.** The dual problem is smooth and convex with few unknowns, so quasi-Newton methods converge to the unique answer in seconds to minutes (my expectation from the problem size, not measured). The primal problem in log weights with Adam has millions of unknowns and converges slowly. Its endpoint depends on the step settings.
- **Choice of loss.** Relative squared error is the normal-error likelihood with σ proportional to the target. Squared log ratio is the log-normal version, with symmetric treatment of over- and under-shooting. Student-t or Huber losses give robustness to a target that conflicts with the others. Absolute floors on σ keep near-zero targets from dominating.
- **Target selection.** Both PolicyEngine and the Tax-Calculator file drop targets that destabilize the fit. With explicit σ's the equivalent is to widen σ for targets with large normalized misses or large prices and record the decision. This keeps every source in the file's audit trail.

---

## 10. Correcting for a known reporting filter: unfolding in particle physics

Particle physicists see collisions only through a detector that blurs, censors and misses some events. They estimate the true distribution by simulating the detector on candidate truth distributions and comparing with what was observed. This is called *unfolding*.
- *Iterative Bayesian unfolding* ([D'Agostini 1995](https://doi.org/10.1016/0168-9002(95)00274-X)) is an expectation–maximization method. It starts from a prior truth distribution and repeatedly reweights it so that its simulated detector output matches the data.
- *OmniFold* ([Andreassen et al. 2020](https://doi.org/10.1103/PhysRevLett.124.182001); [arXiv](https://arxiv.org/abs/1911.09107)) does the same on individual simulated events, using classifiers to estimate the reweighting at each step. It works without binning and in many dimensions at once.

**The correspondence.** The detector corresponds to tax law's reporting filter: who itemizes, the SALT cap, phase-outs. "Truth" is the underlying deductible expenses. The simulated events are the prior records.

**What carries over.**
- The method recovers truth only where the detector sees something. Where events are invisible, the answer equals the prior. Deductible expenses of non-itemizers after 2017 are such a region.
- The number of iterations acts as the regularizer, the same implicit regularization as in PolicyEngine (Section 9.1). An explicit prior term is the alternative.
- OmniFold's reweighting of a simulated sample to match observed distributions, through a known filter, is close to what the brief proposes for censored tables. The difference is that Tax-Data's observations are tabulated totals rather than event samples, so the reweighting of Section 1.2 replaces the classifiers.

**Fit.** High as a conceptual precedent for the censored-table idea and its limits.

---

## 11. The most promising approaches for Tax-Data, and first prototypes

The four prototypes build on one another, and each can be stopped after without wasting the earlier work. Effort figures are my guesses and have not been checked against the codebase.

### Prototype 1: maximum-entropy reweighting with explicit target precisions

**What.** Replace the linear program in `src/reweight.R` with the problem in Section 1.2. Solve it through its dual: one unknown per target, L-BFGS, in JAX, or in R for a first test, since the problem is small.

**Inputs.**
- The 207,692 records with prior weights from `src/create_2017_puf.R`.
- The 188 targets in `config/target_info/baseline.csv`.
- A new column giving each target's σ_j as a relative precision by source, with an absolute floor.

**Hard constraints.** Population partition totals as exact equalities (σ = 0) or by construction. Optional bounds on the adjustment factor, through the bounded "logit" distance in the survey-calibration literature, if extreme factors appear.

**Two-pass variant.** Following the Census Bureau (Section 3.2): fit the near-exact IRS count targets first and fix them, then fit everything else.

**Outputs and checks.**
- Target misses divided by σ_j, with the mean square by source group (should be near 1; Section 7.3).
- The price λ_j of each target, to flag costly targets as the Tax-Calculator team did.
- The distribution of adjustment factors, compared with the current 62% at 0.34 and 38% at 1.66.
- Kish effective sample size, overall and by AGI bracket.
- Bystander checks on untargeted variables.
- Tax-Simulator revenue and distribution tables, old weights versus new.

**First extension.** Add one censored target, such as the number of itemizers or total itemized deductions for a post-2017 year computed under that year's law, to show the mechanics of Section 1.5.

**Effort.** My guess is days, not weeks.

### Prototype 2: imputation clones, so targets update imputed variables

**What.** Give each record K draws of its imputed variables, for example K = 5 to 20 from the existing forest models with different record-keyed seeds, and split its prior weight equally among its clones. Run prototype 1 on the clone file with additional targets on imputed variables. Candidates: Financial Accounts and Distributional Financial Accounts wealth totals and shares, NIPA consumption categories, and SOI totals of any newly reported deductions.

The reweighting then shifts weight toward the draws that agree with the targets. This is importance sampling with the imputation model as the prior. It is PolicyEngine's cloning idea, but with a KL prior and explicit σ's. It would partly replace the separate post-hoc adjustments in individual modules, such as the wealth tilt and the consumption benchmarking.

**Checks.**
- How far each record's clone weights move from equal (entropy of the clone weights per record).
- Effective sample size.
- Conditional distributions of imputed variables before and after, against the source survey. Targets should move aggregates without destroying the conditional patterns that were validated (for example, consumption by income and demographics).

**Risk.** File size grows K-fold, and so does the need for record-keyed seeding. The prototype can run on a subset of variables first.

### Prototype 3: multi-year joint fit with distribution shapes that can change

**What.** For each income type k and year t, introduce a smooth monotone growth curve g_kt(p) over the within-type percentile p, with a handful of spline knots. Record values become x_ik,t = x_ik,2015 × g_kt(p_ik). This replaces today's single growth factor per variable, which freezes 2015 shapes.

Each year's weights are the tilt of Section 1.2. The shape parameters θ_t follow a random walk with drift across years, a penalty on Σ_t ||θ_t − θ_{t−1} − drift||², as in the state-space structure of Section 6.7. Targets:
- SOI totals and cross-tabulations through 2023, each with its σ;
- CBO and SSA aggregates for projection years, with σ's that widen with the horizon.

Fit all years jointly by gradient methods in JAX. AGI-bracket membership now depends on changed values, so it uses the logistic smoothing of Section 8.2.

**Checks.**
- A back-test in the style of Blanchet, Saez and Zucman: fit through 2019, project 2020–2023 using only the aggregates a projection would have, and compare the projected cross-tabulations with SOI's actual tables.
- The same back-test with today's single-factor aging, as the baseline.

**Risk.** The joint problem is no longer convex. Starting it from prototype 1's per-year solutions and fitting the shape parameters alternately with the weights should keep it stable; this is untested.

### Prototype 4 (later, higher risk): one conditional generator as imputation prior and record source

**What.** Train one conditional generative model for the non-PUF variables given PUF-overlap variables. ForestFlow is the first candidate, since it is tree-based, trains on CPU, handles imputation and has R code; TabDiff is the second. Build identities in by construction: generate totals and shares.

Use it in two places:
- as the draw source for prototype 2's clones, in place of fifteen separate modules;
- to generate additional records when the effective sample size in a projection year falls below a threshold, for example records for an older population, conditioned on SSA demographic cells, followed by prototype 1's reweighting.

**Checks.**
- The existing per-module validation diagnostics, re-run on the generator's draws.
- Sampling-zero versus structural-zero rates (Section 4.3), defined using tax-law feasibility rules.
- Tax-Simulator outputs.

**Why last.** The published evidence that deep generators beat sequential trees on economic microdata is thin, and the Urban–IRS work chose trees. The benefit is a coherent joint imputation, and its value can be judged only after prototypes 1–3 show where the prior is the binding limit.

### What not to prioritize

- Full Bayesian posteriors over files: variational inference, MCMC, SBI.
- Optimal transport as the main fitting engine.
- Neural generators trained end-to-end on targets.

Each answers a question the brief does not ask, or costs more than the alternatives above for the same result.

---

## 12. What I looked for and could not find, and what is unverified

- **Search budget.** The shared web-search budget ran out partway through. I could not search for:
  - recent papers (2023–2026) combining deep generative population synthesis with calibration to marginals in one step;
  - diffusion-model guidance toward aggregate (population-level) constraints rather than per-record constraints;
  - published Bayesian or variational treatments of survey calibration weights beyond Si et al. 2017;
  - applications of multi-marginal OT to linking economic surveys.

  These may exist.
- **Differentiable tax calculators.** One search found no differentiable (JAX or PyTorch) implementation of a tax-benefit microsimulation model. I did not check whether tax-simulator-gpu is itself written in a differentiable framework.
- **Not opened (metadata or search summary only).** Chang, Chen and Schorfheide 2024; Eurostat flash-estimate methodology; Bowen et al. 2022 beyond the abstract; Sheldon and Dietterich 2011; the Datta et al. 2011 result as I described it. TabPFN's generation capability is from secondary sources.
- **Not measured.** Every statement about run time for the proposed prototypes is an expectation from problem size, not a benchmark.

---

## References

Status: **Read** = relevant parts read; **Abstract** = abstract read; **Metadata** = title, authors and venue confirmed through arXiv or Crossref; **Search** = seen only in a search-result summary; **Believed** = not checked.

| Reference | URL | Status |
|---|---|---|
| Abowd et al. 2022, 2020 Census TopDown Algorithm, *Harvard Data Science Review* | https://doi.org/10.1162/99608f92.529e3cb9 ; https://arxiv.org/abs/2204.08986 | Read (sections 7.1–7.2) |
| Agapiou et al., Importance sampling: intrinsic dimension and computational cost | https://arxiv.org/abs/1511.06196 | Abstract |
| Agrawal et al. 2019, Differentiable convex optimization layers | https://arxiv.org/abs/1910.12430 | Metadata |
| Andreassen et al. 2020, OmniFold, *Physical Review Letters* 124, 182001 | https://doi.org/10.1103/PhysRevLett.124.182001 ; https://arxiv.org/abs/1911.09107 | Abstract |
| Aydore et al. 2021, Private query release through adaptive projection (RAP) | https://arxiv.org/abs/2103.06641 ; code https://github.com/amazon-science/relaxed-adaptive-projection | Abstract; code README and requirements read |
| Batty et al. 2019, Distributional Financial Accounts, FEDS 2019-017 | https://doi.org/10.17016/FEDS.2019.017 | Metadata |
| Beaumont, Zhang and Balding 2002, ABC in population genetics, *Genetics* | https://doi.org/10.1093/genetics/162.4.2025 | Metadata |
| Benamou et al. 2015, Iterative Bregman projections | https://arxiv.org/abs/1412.5154 | Metadata |
| Berthet et al. 2020, Differentiable perturbed optimizers | https://arxiv.org/abs/2002.08676 | Metadata |
| Blanchet, Saez and Zucman, Real-Time Inequality, NBER w30229 | https://www.nber.org/papers/w30229 ; https://eml.berkeley.edu/~saez/BSZ2022.pdf | Read (sections 3–4) |
| Blei, Kucukelbir and McAuliffe 2017, Variational inference review | https://arxiv.org/abs/1601.00670 | Metadata |
| Borisov et al. 2022, GReaT | https://arxiv.org/abs/2210.06280 | Metadata |
| Borysov, Rich and Pereira 2019, *Transportation Research C* | https://doi.org/10.1016/j.trc.2019.07.006 ; https://arxiv.org/abs/1808.06910 | Abstract |
| Bowen et al. 2022, Synthetic individual income tax data, *National Tax Journal* 75(4) | https://doi.org/10.1086/722094 | Abstract |
| Bunne, Krause and Cuturi 2022, Conditional Monge maps | https://arxiv.org/abs/2206.14262 | Metadata |
| Burman et al. 2019 draft, Synthetic supplemental PUF of low-income information returns | https://www.irs.gov/pub/irs-soi/19conpanburmanpaper.pdf | Read (method sections) |
| Burman et al. 2024, *National Tax Journal* | https://doi.org/10.1086/732683 | Metadata |
| Carrassi et al. 2018, Data assimilation in the geosciences | https://arxiv.org/abs/1709.02798 | Metadata |
| Chang, Chen and Schorfheide 2024, Heterogeneity and aggregate fluctuations, *JPE* 132(12) | https://cowles.yale.edu/research/cfp-1913-heterogeneity-and-aggregate-fluctuations | Search |
| Chernozhukov, Fernández-Val and Melly 2013, *Econometrica* | https://doi.org/10.3982/ECTA10582 | Metadata |
| Chizat et al. 2018, Unbalanced transport | https://arxiv.org/abs/1607.05816 | Abstract |
| Choi et al. 2020, Fair generative modeling via weak supervision | https://arxiv.org/abs/1910.12008 | Metadata |
| Chopra et al. 2023, Differentiable agent-based epidemiology | https://arxiv.org/abs/2207.09714 | Metadata |
| Chopra et al. 2024, Limits of agency in agent-based models | https://arxiv.org/abs/2409.10568 | Metadata |
| Courty et al. 2017, OT for domain adaptation | https://arxiv.org/abs/1507.00504 | Metadata |
| Cranmer, Brehmer and Louppe 2020, Frontier of simulation-based inference | https://arxiv.org/abs/1911.01429 | Metadata |
| Csiszár 1975, I-divergence geometry, *Annals of Probability* | https://doi.org/10.1214/aop/1176996454 | Metadata |
| Cuturi 2013, Sinkhorn distances | https://arxiv.org/abs/1306.0895 | Metadata |
| Cuturi et al. 2022, OTT-JAX | https://arxiv.org/abs/2201.12324 | Abstract |
| D'Agostini 1995, Unfolding based on Bayes' theorem, *NIM A* | https://doi.org/10.1016/0168-9002(95)00274-X | Metadata |
| Datta, Ghosh, Steorts and Maples 2011, Bayesian benchmarking, *TEST* 20(3) | https://doi.org/10.1007/s11749-010-0218-y | Metadata (content as described is believed) |
| Desroziers et al. 2005, Diagnosis of error statistics in observation space, *QJRMS* | https://doi.org/10.1256/qj.05.108 | Metadata |
| Deville and Särndal 1992, Calibration estimators, *JASA* | https://doi.org/10.1080/01621459.1992.10475217 | Metadata |
| DiNardo, Fortin and Lemieux 1996, *Econometrica* | https://doi.org/10.2307/2171954 | Metadata |
| Donti, Rolnick and Kolter 2021, DC3 | https://arxiv.org/abs/2104.12225 | Metadata |
| Dudík, Phillips and Schapire 2007, Maximum entropy with generalized regularization, *JMLR* 8 | https://www.jmlr.org/papers/v8/dudik07a.html | Search (abstract via search summary) |
| Eurostat, Flash estimates methodological note | https://ec.europa.eu/eurostat/documents/d/experimental-statistics/methodological-note-flash-estimates-2025 | Search |
| Evensen 2003, Ensemble Kalman filter, *Ocean Dynamics* | https://doi.org/10.1007/s10236-003-0036-9 | Metadata |
| Flaxman, Wang and Smola 2015, Who supported Obama in 2012?, KDD | https://doi.org/10.1145/2783258.2783300 | Metadata |
| Flaxman, Sutherland, Wang and Teh 2016, 2016 election ecological inference | https://arxiv.org/abs/1611.03787 | Abstract |
| Ganchev et al. 2010, Posterior regularization, *JMLR* 11 | https://jmlr.org/papers/v11/ganchev10a.html | Search |
| Garrido et al. 2020, Rare feature combinations in population synthesis | https://arxiv.org/abs/1909.07689 | Abstract |
| Gelman 2007, Struggles with survey weighting, *Statistical Science* | https://doi.org/10.1214/088342306000000691 | Metadata |
| Grover et al. 2019, Bias correction with likelihood-free importance weighting | https://arxiv.org/abs/1906.09531 | Abstract |
| Grover et al. 2019, Sorting networks via continuous relaxations | https://arxiv.org/abs/1903.08850 | Metadata |
| Hainmueller 2012, Entropy balancing, *Political Analysis* | https://doi.org/10.1093/pan/mpr025 | Metadata |
| Hardt, Ligett and McSherry 2012, MWEM | https://arxiv.org/abs/1012.4763 | Metadata |
| Hoffman et al. 2013, Stochastic variational inference | https://arxiv.org/abs/1206.7051 | Metadata |
| Hollmann et al. 2025, TabPFN, *Nature* | https://doi.org/10.1038/s41586-024-08328-6 | Metadata |
| Hu et al. 2018, Deep generative models with learnable knowledge constraints | https://arxiv.org/abs/1806.09764 | Metadata |
| Idel 2016, Matrix scaling and Sinkhorn's normal form | https://arxiv.org/abs/1609.06349 | Metadata |
| Iglesias, Law and Stuart 2013, Ensemble Kalman methods for inverse problems | https://doi.org/10.1088/0266-5611/29/4/045001 ; https://arxiv.org/abs/1209.2736 | Abstract |
| Ireland and Kullback 1968, Contingency tables with given marginals, *Biometrika* | https://doi.org/10.1093/biomet/55.1.179 | Metadata |
| Jang, Gu and Poole 2017, Gumbel-softmax | https://arxiv.org/abs/1611.01144 | Metadata |
| Javanmard et al. 2024, Learning from aggregate responses | https://arxiv.org/abs/2401.11081 | Abstract |
| Jaynes 1957, Information theory and statistical mechanics, *Physical Review* | https://doi.org/10.1103/PhysRev.106.620 | Metadata |
| Johnsen et al. 2020, Population synthesis for urban resident modeling | https://arxiv.org/abs/2011.06851 | Abstract |
| Jolicoeur-Martineau, Fatras and Kachman 2023, ForestDiffusion/ForestFlow | https://arxiv.org/abs/2309.09968 | Abstract |
| Kalman 1960, Linear filtering and prediction | https://doi.org/10.1115/1.3662552 | Metadata |
| Kim and Bansal 2023, Feasible and diverse population synthesis | https://arxiv.org/abs/2208.01403 | Abstract |
| Kotelnikov et al. 2022, TabDDPM | https://arxiv.org/abs/2209.15421 | Metadata |
| Lavenant et al. 2021, Theory of trajectory inference | https://arxiv.org/abs/2102.09204 | Abstract |
| Law et al. 2018, Variational learning on aggregate outputs with GPs | https://arxiv.org/abs/1805.08463 | Abstract |
| Law, Stuart and Zygalakis 2015, Data assimilation: a mathematical introduction | https://arxiv.org/abs/1506.07825 | Metadata |
| Lederrey, Hillel and Bierlaire 2022, DATGAN | https://arxiv.org/abs/2203.03489 | Abstract |
| Liang, Jordan and Klein 2009, Learning from measurements in exponential families, ICML | https://doi.org/10.1145/1553374.1553457 | Metadata |
| Liu, Sheldon and Dietterich 2014, Gaussian approximation of collective graphical models | https://arxiv.org/abs/1405.5156 | Metadata |
| Liu, Vietri and Wu 2021, Iterative methods for private synthetic data (PEP, GEM) | https://arxiv.org/abs/2106.07153 | Abstract |
| Liu et al. 2023, Private synthetic data with genetic algorithms | https://arxiv.org/abs/2306.03257 | Metadata |
| Lorenc 1986, Analysis methods for numerical weather prediction, *QJRMS* | https://doi.org/10.1002/qj.49711247414 | Metadata |
| Louizos, Welling and Kingma 2018, L0 regularization | https://arxiv.org/abs/1712.01312 | Metadata |
| Maddison, Mnih and Teh 2017, Concrete distribution | https://arxiv.org/abs/1611.00712 | Metadata |
| McKenna, Sheldon and Miklau 2019, Private-PGM | https://arxiv.org/abs/1901.09136 | Read (sections 1–3) |
| McKenna et al. 2022, AIM | https://arxiv.org/abs/2201.12677 | Metadata |
| Mueller, Gruber and Fok 2023, CDTD | https://arxiv.org/abs/2312.10431 | Metadata |
| Nandi et al. 2020, disaggregation R package | https://arxiv.org/abs/2001.04847 | Metadata |
| Papamakarios and Murray 2016, ε-free inference | https://arxiv.org/abs/1605.06376 | Metadata |
| Papamakarios et al. 2021, Normalizing flows | https://arxiv.org/abs/1912.02762 | Metadata |
| Pass 2015, Multi-marginal optimal transport | https://arxiv.org/abs/1406.0026 | Metadata |
| Petersen 2022, Learning with differentiable algorithms | https://arxiv.org/abs/2209.00616 | Metadata |
| Peyré and Cuturi 2019, Computational optimal transport | https://arxiv.org/abs/1803.00567 | Metadata |
| PolicyEngine, microcalibrate | https://github.com/PolicyEngine/microcalibrate | Read (README, loss and reweighting code) |
| PolicyEngine, policyengine-us-data documentation | https://github.com/PolicyEngine/policyengine-us-data | Read (methodology, calibration, long-term projections pages) |
| Poole and Raftery 2000, Bayesian melding, *JASA* | https://doi.org/10.1080/01621459.2000.10474324 | Metadata |
| PSL tax-microdata-benchmarking | https://github.com/PSLmodels/tax-microdata-benchmarking | Read (`tmd/utils/reweight.py` header, `tmd/areas/AREA_WEIGHTING_LESSONS.md`) |
| Qian, Cebere and van der Schaar 2023, Synthcity | https://arxiv.org/abs/2301.07573 | Metadata |
| Quadrianto et al. 2009, Estimating labels from label proportions, *JMLR* 10 | https://www.jmlr.org/papers/v10/quadrianto09a.html | Abstract |
| Rader, Lyons and Kidger 2024, Optimistix | https://arxiv.org/abs/2402.09983 | Metadata |
| Schiebinger et al. 2019, Waddington-OT, *Cell* | https://doi.org/10.1016/j.cell.2019.01.006 | Metadata and search summary |
| Shi et al. 2024, TabDiff (ICLR 2025) | https://arxiv.org/abs/2410.20626 | Abstract |
| Si, Trangucci, Gabry and Gelman 2017, Bayesian hierarchical weighting | https://arxiv.org/abs/1707.08220 | Metadata |
| Skilling and Bryan 1984, Maximum entropy image reconstruction, *MNRAS* | https://doi.org/10.1093/mnras/211.1.111 | Metadata |
| Smith, Diamant and Trippe 2025, Calibrating generative models to distributional constraints (ICML 2026) | https://arxiv.org/abs/2510.10020 | Read (formulation and limitations) |
| Snyder, Bengtsson, Bickel and Anderson 2008, Obstacles to high-dimensional particle filtering, *Monthly Weather Review* | https://doi.org/10.1175/2008MWR2529.1 | Metadata |
| Stoian et al. 2024, Constraining deep generative models for tabular data | https://arxiv.org/abs/2402.04823 | Read (constraint layer) |
| Szabó et al. 2016, Learning theory for distribution regression | https://arxiv.org/abs/1411.2066 | Metadata |
| Tejero-Cantero et al. 2020, sbi toolkit | https://arxiv.org/abs/2007.09114 | Metadata |
| Phan, Pradhan and Jankowiak 2019, NumPyro | https://arxiv.org/abs/1912.11554 | Metadata |
| Cabezas et al. 2024, BlackJAX | https://arxiv.org/abs/2402.10797 | Metadata |
| Vietri et al. 2022, RAP++ | https://arxiv.org/abs/2209.07400 | Abstract |
| Wang and Zubizarreta 2020, Approximately balancing weights | https://arxiv.org/abs/1705.00998 | Metadata |
| Wheldon, Raftery, Clark and Gerland 2013, Bayesian population reconstruction, *JASA* | https://doi.org/10.1080/01621459.2012.737729 | Metadata and search summary |
| Xu et al. 2019, CTGAN | https://arxiv.org/abs/1907.00503 | Metadata |
| Yao et al. 2018, Evaluating variational inference | https://arxiv.org/abs/1802.02538 | Metadata |
| Yu et al. 2013, ∝SVM | https://arxiv.org/abs/1306.0886 | Metadata |
| Zhang et al. 2023, TabSyn | https://arxiv.org/abs/2310.09656 | Abstract |
| Zhang, Charoenphakdee, Wu and Sugiyama 2020, Learning from aggregate observations | https://arxiv.org/abs/2004.06316 | Abstract |
