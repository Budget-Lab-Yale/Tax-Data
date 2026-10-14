---
title: "What kind of problem this is: an independent first-principles formulation"
role: evidence
workstream: rebuild
status: current
updated: 2026-10-07
sot: research/rebuild/BRIEF.md
supersedes: []
superseded_by: null
---

## Summary

1. This is an inverse problem: estimate a population distribution from incomplete records and aggregate measurements. Many distributions fit the evidence.
2. The object is each year's joint distribution of tax-unit characteristics, including population size. Record identities are a representation choice.
3. The prior supplies relationships the targets do not determine. Its strength and permitted characteristic combinations are substantive assumptions.
4. Relative entropy penalizes proportional weight changes. With linear targets it gives a convex problem, without inferior local optima.
5. Weights cannot create missing types or values outside the records' range. Additional records or value changes are then necessary.
6. Apply each year's reporting rules to underlying expenses, including those of nonitemizers. With fixed records, even law-censored totals remain linear in weights.
7. Imputation should supply variation, not just predicted means. Separate consumption and wealth models do not establish their joint relationship.
8. Annual estimates can share growth assumptions without tracking individuals. Projection to 2097 requires explicit distributional assumptions and forecast uncertainty.
9. A best file can be a Bayesian posterior mode under stated assumptions. It supplies neither uncertainty intervals nor identification of untargeted relationships.
10. First build entropy calibration over expanded candidate records. Add limited value transformations where diagnostics require them. A general record generator exceeds the two-week production scope.

## Evidence

I read `research/rebuild/BRIEF.md` first, then the requested code and configuration. I did not read other background papers or run R or the pipeline. The base source is the IRS Public Use File (PUF). Code statements below refer to the local checkout at commit `ba48f970acd005b1f1f224622bf4dfebc476075d`; linked repository URLs identify those files. Mathematical conclusions are deductions from the formulations below. Build schedules and performance expectations are judgments, not measured results.

The existing method already changes both weights and values. `src/create_2017_puf.R` applies demographic and income-cell weight factors, calls the linear program with adjustment limits of 0.34 and 1.66, then rescales amounts within income groups. `src/reweight.R` minimizes the sum of adjustment factors, rather than their departures from one. Its target tolerances define hard admissible intervals. The configuration has 188 target rows, including repeated filing-status rows; these are not 188 independent measurements. The brief's cached boundary-weight frequencies were not independently recomputed. [Construction code](https://github.com/Budget-Lab-Yale/Tax-Data/blob/ba48f970acd005b1f1f224622bf4dfebc476075d/src/create_2017_puf.R), [reweighting code](https://github.com/Budget-Lab-Yale/Tax-Data/blob/ba48f970acd005b1f1f224622bf4dfebc476075d/src/reweight.R), [target configuration](https://github.com/Budget-Lab-Yale/Tax-Data/blob/ba48f970acd005b1f1f224622bf4dfebc476075d/config/target_info/baseline.csv).

The current architecture retains a base file, annual weights, cumulative growth factors, and imputations attached at their source year. Filer weights use observed return-count growth through 2023. Nonfilers use distinct annual pools through 2023, then grow the last pool. Filer ages represent annual age groups rather than individuals aging through time. Wealth has separate growth factors by income group. Thus the current implementation is more varied than uniform aging of one immutable file. The variable guide has 190 entries, including derived quantities and source metadata; these are not 190 independent economic variables. Wealth begins in 2022, and `materialize()` leaves those module fields missing before then. [Entry point](https://github.com/Budget-Lab-Yale/Tax-Data/blob/ba48f970acd005b1f1f224622bf4dfebc476075d/src/main.R), [projection](https://github.com/Budget-Lab-Yale/Tax-Data/blob/ba48f970acd005b1f1f224622bf4dfebc476075d/src/project_puf.R), [materialization](https://github.com/Budget-Lab-Yale/Tax-Data/blob/ba48f970acd005b1f1f224622bf4dfebc476075d/src/materialize.R), [variable guide](https://github.com/Budget-Lab-Yale/Tax-Data/blob/ba48f970acd005b1f1f224622bf4dfebc476075d/config/variable_guide/baseline.csv).

## Estimand

The estimand is the economic object to be estimated. Here it is the number of tax units with each possible combination of characteristics, separately for every year. Let a record's vector \(x\) contain demographics, incomes, underlying expenses, assets, and any history needed by the simulator. A discrete representation is

\[
\mu_t=\sum_{i=1}^{n_t}w_{it}\,\delta_{x_{it}},
\qquad N_t=\sum_i w_{it},\qquad F_t=\mu_t/N_t.
\]

Each record represents \(w_{it}\geq0\) tax units with characteristics \(x_{it}\); \(\delta_x\) places all that record's weight at \(x\). The weights sum to the number of tax units, \(N_t\). Dividing by that total gives the probability distribution \(F_t\) of a randomly selected tax unit. We need both population size and its composition.

This distinction matters when fitting counts. A probability distribution alone cannot distinguish 150 million units from 170 million units. Nor does a count of tax units equal a count of people: married units, dependents, and dependent filers require explicit counting rules. The accounting partition must hold exactly, while measured population totals can remain uncertain.

Underlying quantities should be distinguished from what a return reports. A nonitemizer can have mortgage interest and charitable giving. A statutory cap on a credit's eligible expenses is not a cap on actual expenditure. Several current fields explicitly describe itemizer-only or capped amounts. Reinterpreting those columns as uncensored expenses would require new imputations and a documented mapping to Tax-Simulator's input definitions.

Annual distributions suffice for annual static revenue and distributional estimates. They do not determine individual mobility, lifetime incidence, or transitions into retirement. Where a tax rule depends on history, the annual record must include a sufficient history variable, such as basis or a carryforward. The permission to change record identities between years does not eliminate those economic requirements.

I retain the brief's unified estimation framing, with one distinction: constructing plausible candidate records and calibrating their representation are separate mathematical choices. A single objective does not require all unknowns to be free parameters in one optimization.

## Prior distance

A reference distribution is the initial economic population constructed from the PUF, nonfiler sources, imputations, and growth assumptions. Call its record weights \(b_i\). It describes plausible people before the particular targets being fitted are applied. It should preserve survey design weights and known within-record relationships, without treating disclosure-protected amounts as exact population truth.

For fixed records, a useful distance is generalized relative entropy:

\[
D(w,b)=\frac{1}{N_*}\sum_i
\left[w_i\log\left(\frac{w_i}{b_i}\right)-w_i+b_i\right].
\]

This charges for changing each record's population representation relative to its starting weight. \(N_*>0\) is a fixed reference population used to set the scale, not the number of sample rows. The expression is zero at the starting weights and positive for any departure. With equal population totals it is proportional to Kullback–Leibler divergence, usually abbreviated KL, a measure of how much one probability distribution differs from a reference distribution. Its direction matters.

The natural alternatives express different economic judgments. In this table, \(p_i\) and \(q_i\) are reference and fitted population shares on the same candidate records.

| Distance | Meaning and implications |
|---|---|
| Relative entropy, \(\sum_i q_i\log(q_i/p_i)\) | Penalizes expansion relative to the prior and permits a share to fall to zero at finite cost. Giving positive weight to something assigned zero prior probability has infinite cost. It need not preserve any variable's separate distribution unless that distribution is constrained. |
| Chi-square distance, \(\frac12\sum_i(q_i-p_i)^2/p_i\) | Penalizes squared changes relative to initial representation. It approximates relative entropy for small changes and penalizes very large expansions more strongly. Without nonnegativity constraints, a quadratic calibration formula can produce negative weights. |
| Reverse relative entropy, \(\sum_i p_i\log(p_i/q_i)\) | Makes eliminating an initially represented type infinitely costly. Expansion into a rare prior type is comparatively less costly. This is a different prior judgment, despite the similar name. |
| Squared Hellinger distance, \(\sum_i(\sqrt{q_i}-\sqrt{p_i})^2\) | Compares square roots of shares. It treats large relative departures less severely than forward relative entropy and does not impose an infinite barrier at zero fitted shares. |
| Total variation, \(\frac12\sum_i\lvert q_i-p_i\rvert\) | Measures how much population share must be reassigned among types. It does not distinguish a small income change from a large one. Its piecewise linear objective may have many best solutions. |
| Transport distance | Charges for changing characteristics as well as representation, using a chosen cost between two records. A small wage change can cost less than a large wage change. Costs for age, dollars, family structure, and ownership must be specified economically. |

The relative-entropy convexity used here is documented in Boyd and Vandenberghe, example 3.19. Transport distances and their dependence on the cost between observations are developed by Peyré and Cuturi. I verified those passages in the linked texts. [Convex Optimization](https://web.stanford.edu/~boyd/cvxbook/bv_cvxbook.pdf), [Computational Optimal Transport](https://arxiv.org/html/1803.00567).

Distances between distributions and penalties on individual edits are not identical. A penalty on each record's wage change depends on which old record is paired with which new record. A distributional distance does not care about record labels. Likewise, unweighted squared changes in weight factors depend on how records were split into rows. Population-weighted relative entropy is unchanged if an identical record and its prior weight are split proportionally. Adding duplicate rows must not manufacture more prior information.

For movable records, ordinary KL against a discrete PUF forbids every new characteristic vector. Comparing a discrete fitted file with a continuous density also does not give a finite ordinary KL divergence. The practical choices are an expanded discrete candidate set, an explicit continuous distribution model, or a transport/edit penalty. A density evaluated at records is not by itself a distance between the two population distributions.

Relative entropy is a reasonable default for weights. It is not a complete definition of plausibility, and it does not prevent extreme weights when strong targets require them.

## Measurements

A target is a measurement of a defined population under a defined reporting process. For a published count or amount, write

\[
y_{kt}=T_{kt}(\mu_t)+\epsilon_{kt},\qquad
T_{kt}(\mu_t)=\sum_iw_{it}\,h_{kt}\!\left(R_t\(x_{it}\)\right).
\]

The published value \(y_{kt}\) equals the modeled population total plus an error \(\epsilon_{kt}\). The reporting rule \(R_t\) applies the relevant year's law and reporting conventions. The function \(h_{kt}\) selects the table's population and computes that unit's contribution. A return count contributes one for an eligible filer; a wage amount contributes that filer's wages; a dependent count contributes the eligible number of dependents.

The general target can be any specified function \(T_{kt}(\mu_t)\), including a mean, percentile, or ratio. The displayed sum is the especially useful class of additive totals. Once records and reporting rules are fixed, every such total is linear in weights, even if its calculation involves brackets, caps, tax liability, or an itemization decision. Products of record variables, such as wages times wealth, are also linear in weights when those variables are fixed.

Ratios of two fitted totals are generally nonlinear. A mean with a known fixed denominator is linear after rescaling. A reported mean can sometimes be expressed as an amount-minus-mean-times-count condition, but its error model must reflect the uncertain denominator. A percentile at a supplied dollar threshold can be targeted through a count below that threshold, with attention to ties. A share belonging to the fitted top 1 percent requires finding a threshold from the fitted distribution and is generally nonlinear. The wealth module uses fixed Survey of Consumer Finances (SCF) wealth thresholds; matching dollars above those thresholds does not alone match the fitted file's own top-percentile share.

Every target needs its source and vintage, year, unit of observation, population coverage, amount definition, price basis, reporting rule, precision, and links to other targets. A Bureau of Economic Analysis (BEA) household-sector total and a tax-unit cash amount require an explicit reconciliation. Different concepts should not be forced to agree simply by assigning small errors.

A target's precision should include sampling error, reporting error, revisions, and uncertainty in that reconciliation. A standard deviation is the typical size of an error under the stated measurement model. A current 1 percent admissible tolerance is not automatically a 1 percent standard deviation. Near-exact administrative counts can be precise about their own universe without precisely measuring a differently defined population.

Targets derived from the same table are correlated: their errors move together. A covariance matrix records their error variances and these shared movements. Totals, subtotals, repeated rows, and annual observations from the same forecast cannot be treated as independent confirmations. Remove exact duplicate information or represent its dependence; for exact adding-up relationships, use independent contrasts or a covariance model restricted to the independent measurements.

### Tax-law censoring

For itemized expenses, retain the underlying expense vector for all relevant units. Apply the year's eligibility rules, limits, and reporting definitions, then model whether the unit reports itemized deductions. The contribution to a published medical deduction total, for example, is the reportable medical amount multiplied by the itemization indicator and the table's filer and income-group indicators.

The standard-versus-itemized choice must use the actual relevant rules and a stated reporting assumption. Always choosing the tax-minimizing option may not reproduce observed elections or compliance. Those differences can enter a reporting model or target error. A post-2017 fall in reported deductions therefore need not imply a comparable fall in underlying spending. The accounting alone also does not distinguish a reporting change from a behavioral response in spending.

Computationally, fixed candidate records allow \(R_t\) to run once before calibration. Changing values requires rerunning it, including the income-group assignments. Exact thresholds create jumps or kinks; automatic derivatives do not capture the effect of crossing a discrete reporting threshold. A smooth approximation can help optimization, but final diagnostics must use the exact reporting rules. The same selection issue applies to expenses unobserved among nonitemizers in the original PUF.

Tax-Data would need a versioned measurement interface to Tax-Simulator or its graphics processing unit (GPU) port for these calculations. That interface should reproduce the published concept without duplicating a second, independently maintained tax engine. I have not inspected either simulator's relevant interfaces or verified that all required reporting outputs exist.

## Objective

For fixed records, a concrete annual estimator is

\[
\widehat w_t=\underset{w_t\geq0,\ C_tw_t=d_t}{\arg\min}
\left\{\alpha_tD(w_t,b_t)
+\frac12(A_tw_t-y_t)'\Sigma_t^{-1}(A_tw_t-y_t)\right\}.
\]

The first term penalizes departure from the prior. The second penalizes target errors according to their precision and dependence. Each row of \(A_t\) contains one target's precomputed per-record contributions. \(C_tw_t=d_t\) contains only exact accounting restrictions, and \(\alpha_t>0\) controls prior strength. The inverse covariance expression assumes redundant measurements have been removed so that the covariance matrix can be inverted.

Population partitions belong among the exact relationships. An uncertain externally measured population level belongs among the soft targets. Identities within records, such as primary and spouse wages summing to total wages, should be enforced when constructing records. Positivity applies to weights and inherently nonnegative quantities; net business income, net worth, and capital gains can be negative.

This is regularized calibration: adjust representation to fit measured totals, while a penalty selects among distributions the measurements cannot distinguish. A convex problem has no inferior local optimum; with positive prior weights, relative entropy gives a strictly convex weight objective and hence at most one best weight vector on a nonempty convex feasible set. That uniqueness is conditional on the supplied records and penalties.

Outlying target errors can instead receive a Huber penalty, which is quadratic for small errors and grows linearly for large errors. This retains convexity for linear targets while limiting one discrepant source's influence. It does not resolve a mistaken population definition. Prior strength should be selected through historical prediction checks and sensitivity analysis, with choices recorded; it cannot be inferred from the count of synthetic rows.

## Decision variables

### Fixed records

With weights alone, the set of possible characteristic combinations remains fixed. Reweighting can change distributions, percentile positions, correlations, and within-cell means by changing representation. Every positive-weight record's values and internal relationships remain unchanged.

There is an exact feasibility test in principle. With a fixed population \(N\), attainable vectors of target averages are weighted averages of the records' contribution vectors. This set is their convex hull, meaning all mixtures with nonnegative shares summing to one. A target vector outside that set cannot be matched by any weights. Without a population constraint, attainable totals instead form the cone of nonnegative combinations. Bounds on weights make the attainable set smaller.

For example, if all candidate wage values are at most $150,000, no weighted mean wage can reach $200,000. Even targets inside each separate variable's range can fail jointly: if every candidate has wages or pensions but none has both, a positive target count of units receiving both is impossible. Missing nonfilers present the same issue if every candidate is marked as a filer. These are proofs of insufficient support, not failures of a particular optimizer. Support is the set of values or combinations the model allows.

Soft targets always permit a compromise when the hard restrictions are feasible and the objective is well behaved. They do not make an unsupported target reachable. Moreover, matching every available target does not prove adequate support for an untargeted reform.

### Transformations

Let records change through a specified transformation \(x_{it}=g_t(x^0_i;\theta_t)\), where \(\theta_t\) controls changes such as wage growth by age and income group. This can create new dollar values, change dispersion, and allow movement across brackets. It is often sufficient to use a few economically interpretable transformations rather than a parameter for every amount on every record.

Positive multipliers preserve zeros and signs. They cannot create new wage recipients, homeowners, or nonfilers. Those changes require an explicit ownership or participation model, alternative candidate records, or categorical edits. Recompute derived quantities after every transformation. For example, transforming total wages and a spouse share permits both wage components to be derived exactly.

Joint estimation of weights and transformation parameters is generally nonconvex, meaning the optimizer may settle on different local solutions. Totals contain products of weights and changed values, and reporting decisions change discontinuously. Alternating a weight fit with a value fit can be useful, but it does not certify a global optimum or identify whether a target discrepancy should change people or their incomes. Separate prior penalties express that distinction.

### Candidate records

New records need not come from a general machine-learning generator. One can create several plausible completions of an incomplete record, include observed nonfiler records, or add justified alternatives for thin cells. Each completion has internally consistent demographics and amounts, plus an initial probability from its imputation model. Freeze those candidates before calibration.

Suppose completion \(j\) for original record \(i\) has prior probability \(p_{ij}\). Assign it population mass \(m_{ij}\), and let \(w_i=\sum_jm_{ij}\) and \(q_{ij}=m_{ij}/w_i\). Then

\[
\sum_{ij}\left[m_{ij}\log\frac{m_{ij}}{b_ip_{ij}}-m_{ij}+b_ip_{ij}\right]
=\sum_i\left[w_i\log\frac{w_i}{b_i}-w_i+b_i\right]
+\sum_iw_i\sum_jq_{ij}\log\frac{q_{ij}}{p_{ij}}.
\]

The distance on completed records separates into changing how many units the original record represents and changing the distribution of its missing characteristics. The formula uses the usual zero-mass limit. Optimizing the joint masses \(m_{ij}\) keeps additive targets linear and the entropy problem convex. The two penalties can also receive different positive coefficients without losing convexity, allowing different confidence in population weights and imputations. Original-record totals can be fixed where appropriate or allowed to change with a penalty.

The output can retain several completions with fractional weights. These describe population variation within an observed type, not uncertainty draws from a posterior. Selecting just one completion per record creates sampling error and can destroy the fitted totals. If output size requires compression, recalibrate and check the compressed file, including policy outputs. Averaging completions into one record can also distort taxes because tax liability is nonlinear in characteristics.

A continuous generator that learns entirely new records would require a model of mixed discrete and continuous variables, zeros, tails, accounting identities, and source selection. Optimizing its parameters against targets is generally nonconvex. The candidate approach is also bounded by its expanded support, but that limitation is explicit and testable.

## Imputation

An imputation model supplies a conditional distribution: the range of plausible missing values and their probabilities for a unit with specified observed characteristics. It should supply variation as well as an expected value. Replacing every missing value with its conditional mean suppresses dispersion and changes the number of units exposed to thresholds in the tax system.

The consumption module already follows this principle. It draws a total consumption value from a quantile forest, a model that estimates different points of the outcome distribution conditional on predictors. It then draws a vector of expenditure shares from a distributional forest, a model that assigns probabilities to observed donor outcomes conditional on predictors. It also extrapolates consumption at incomes beyond the survey's coverage. The eight categories therefore retain some donor dependence rather than being imputed independently. These are verified implementation choices, not validation of their fit. [Consumption code](https://github.com/Budget-Lab-Yale/Tax-Data/blob/ba48f970acd005b1f1f224622bf4dfebc476075d/src/imputations/consumption.R).

The wealth module draws joint asset, debt, and gain vectors conditional on income and demographics. It tilts donor probabilities, fits selected cell totals and ownership counts, samples a donor, and rescales individual wealth components within cells. In `src/imputations/tilt_solver.R`, the fitted criterion is squared scaled target residuals plus a quadratic penalty on tilt coefficients. Its exponential form does not make that criterion the convex dual of the entropy estimator above. Squared errors after a nonlinear exponential mapping need not be convex. Nor are its relative-error scaling constants established measurement standard deviations. [Wealth code](https://github.com/Budget-Lab-Yale/Tax-Data/blob/ba48f970acd005b1f1f224622bf4dfebc476075d/src/imputations/wealth.R), [tilt solver](https://github.com/Budget-Lab-Yale/Tax-Data/blob/ba48f970acd005b1f1f224622bf4dfebc476075d/src/imputations/tilt_solver.R).

That module nevertheless provides useful components: conditional donor probabilities, support diagnostics, calibrated expectations, and separate diagnostics after donor sampling and value rescaling. A unified estimator should retain those distinctions. Rescaling after fitting can change other targets and donor relationships, so all final targets must be recomputed.

Consumption and wealth drawn separately conditional on income and age impose an additional assumption: after conditioning on those predictors, the two draws have no remaining association. That is conditional independence. Their separate survey distributions do not establish it. Assets, debt, and consumption jointly affect many distributional outputs; alternative residual associations should be included in sensitivity checks or learned from a source observing them together. A model trained on one survey also requires an assumption that its conditional relationships transfer to the PUF population and year.

Prior construction must retain genuine zeros, distinguish them from unobserved amounts, and preserve joint donor variables where available. Empty donor support may justify broader matching or new candidates, with the change recorded. Repeated copies of one donor do not establish a well-observed tail. A model for protected PUF amounts can similarly allow plausible underlying values, but I have not verified a usable disclosure-error model for this file.

Avoid counting the same source twice. Survey totals used to train or benchmark an imputation are not independent new evidence when reused as calibration targets. Likewise, a growth prior built to reproduce a forecast should not receive that forecast again as independent confirmation. A coherent statistical model would represent this dependence; a practical calibration implementation should document reused information and avoid claiming the resulting precision is independently supported.

## Time

There are 83 years from 2015 through 2097. The existing output range starts in 2017; extending estimation to 2015 and 2016 requires explicit source-year priors and nonfiler coverage. The current 2017 base and 2022 wealth module do not themselves provide those earlier distributions.

Separate annual problems are a valid first formulation. Construct \(b_t,x_t^0\) from an explicit growth model, then update that year's distribution with its available targets. Using the same economic growth assumptions across annual priors provides consistency without requiring the same record identities. However, independent fitting ignores shared measurement errors and may produce unstable year-to-year distributional changes.

A joint formulation adds shared parameters or a penalty on changes through time. For example, penalize the difference between a fitted vector of cell means and shares in year \(t\) and its predicted value from year \(t-1\). Smoothness means preferring small unexplained changes between years. Apply it to deviations from expected nominal growth and demographic change, rather than to raw dollar levels. Permit known shocks and reporting breaks explicitly.

If the selected summaries are linear in weights and their transition is a fixed linear map, a quadratic transition penalty preserves convexity across years. A fixed nonnegative transition map on population masses can also accommodate growth and demographic replacement. Transitions that depend on fitted ranks, unknown behavioral parameters, or freely changing records generally do not preserve convexity. No longitudinal population model is implied by smoothing annual distributions.

Projection to 2097 uses the same estimation machinery but a different information basis. Forecast totals constrain selected aggregates; they do not establish future wage dispersion, asset ownership, or the income-wealth relationship. Specify those distributional assumptions in the prior. Future targets should have uncertainty and shared forecast errors across years, with alternative macroeconomic and distributional scenarios. The absence of observed targets does not mean zero growth: the growth prior supplies it.

The precision of a target and the strength of a prior are separate choices. Both may weaken with horizon. A deterministic prior with a very large penalty can create spurious long-run certainty even when target errors are large. Conversely, weak penalties on both do not select a stable forecast without additional restrictions.

Later survey evidence can inform earlier missing variables through a stated model of change. Backcasting 2022 wealth to 2015 is such a model, not a consequence of the optimizer. Similarly, a joint historical fit lets later data revise earlier estimates; that should be distinguished from a forecast made using only information available at the earlier date.

## Bayesian interpretation

A Bayesian prior assigns probabilities to possible unknown populations before using the evidence being fitted. The reference microdata are a central population around which that prior can be concentrated; they are not themselves a complete prior over possible populations. Prior strength specifies how much departure is plausible.

A likelihood specifies how probable the observed targets would be if a particular candidate population were true. A posterior is the resulting distribution over possible populations after combining that likelihood with the prior. In a finite parameterization, write

\[
\pi(\theta\mid y)\ \propto
\underbrace{\exp[-L(y,T(\theta))]}_{\text{likelihood}}
\underbrace{\exp[-P(\theta)]\,\mathbf 1\{\theta\in\mathcal C\}}_{\text{prior}}.
\]

Here \(\theta\) collects the unknown weights, transformations, or population masses. \(L\) measures disagreement with the targets, \(P\) penalizes departure from prior beliefs, and \(\mathcal C\) allows only populations satisfying the hard identities. With an explicitly specified underlying measure and proper normalization, the most probable parameter value minimizes \(L+P\). This is the maximum a posteriori estimate, or posterior mode.

The displayed annual weight objective has this interpretation if target errors have a multivariate normal distribution with specified covariance, and the prior density on the admissible weight coordinates is proportional to the exponential of negative relative entropy. A normal error distribution puts most probability near zero error, with symmetric declining probabilities for larger errors. It can be a useful approximation for aggregate estimates; it is not automatically the correct sampling model for administrative counts or forecasts.

Several familiar methods appear as special cases. Relative entropy with exact feasible linear moments, meaning specified averages such as means or ownership rates, gives entropy calibration, also called entropy balancing. With a fixed population total, a uniform reference gives maximum entropy on the specified support. A chi-square weight penalty with exact linear constraints gives the usual quadratic calibration associated with generalized regression estimation, where admissibility constraints do not bind. A quadratic parameter prior and a linear normal measurement model give penalized least squares; in that unrestricted Gaussian case, the posterior mean and mode coincide. The published entropy-balancing abstract verifies its use of maximum-entropy reweighting and moment constraints; the equivalences here follow from the stated objectives. [Hainmueller, Entropy Balancing](https://www.cambridge.org/core/journals/political-analysis/article/abs/entropy-balancing-for-causal-effects-a-multivariate-reweighting-method-to-produce-balanced-samples-in-observational-studies/220E4FC838066552B53128E647E4FAA7).

If precision weights are judgments about acceptable discrepancies rather than a probability model of measurement error, the objective is still well-defined calibration. Calling the exponential of that loss a generalized Bayesian update requires choosing its scale; it does not supply a validated likelihood. Bissiri, Holmes, and Walker explicitly distinguish updating by a loss from updating by a conventional likelihood and discuss the relative scaling problem. Their distribution over beliefs should not be confused with the distribution of tax units. [A general framework for updating belief distributions](https://pmc.ncbi.nlm.nih.gov/articles/PMC5082587/).

A mode supplies no probabilities for alternative populations. It omits parameter uncertainty, uncertainty in imputations and projections, and the possibility of substantially different distributions fitting equally well. A density's mode also depends on its coordinates: the density over log weights can have a different mode from the density over weights. Specify the statistical penalty in economic coordinates; using log weights merely to optimize that same objective does not redefine it.

John's choice of one file is compatible with this formulation. It still warrants selected sensitivity fits. A posterior mean revenue estimate generally differs from revenue computed at a posterior mode population. Moreover, repeated imputation draws alone measure imputation variation, not all uncertainty. Full posterior inference would require a defensible measurement and prior model in addition to more computation.

## Identification

Identification means that the evidence distinguishes the economic quantity of interest from other possible values. A unique optimizer can result from the prior even when the evidence leaves many alternatives. Hundreds or thousands of totals cannot generally identify a joint distribution over approximately 190 characteristics.

For fixed records and exact linear targets, consider a change in weights \(v\) that leaves both measured totals and hard constraints unchanged: \(Av=0\) and \(Cv=0\). If a policy output has fixed per-record contributions \(g_i\), its total changes by \(g'v\). Whenever that is nonzero for an admissible change, those data do not identify the output. If \(g\) is a linear combination of the target and constraint rows, the output is determined by them. At a strictly positive feasible solution this is also the relevant converse; binding bounds can remove otherwise possible changes.

That is identification conditional on the candidate records. A support restriction can make an output appear determined simply because plausible missing types were excluded. Expanding support is consequently part of identification analysis, not only a numerical repair. No observed cross-tabulation can reveal every within-cell correlation, and a censored total usually says little about the distribution below the reporting threshold.

Report identification per economic output, not per file. An accompanying output table should contain:

| Field | Interpretation |
|---|---|
| Output and population | State the exact reform, tax measure, group, and year. |
| Direct evidence | Name targets measuring the output or close components, and show their residuals in dollars/counts and error-standard-deviation units. |
| Supporting records | Report candidate and original-donor counts, weight concentration, and concentration of the output's dollar contribution. |
| Dependence on assumptions | Show changes under alternative priors, prior strengths, donor associations, support expansions, and relevant projection scenarios. |
| Range consistent with targets | Minimize and maximize the output over an explicitly defined region of plausible target errors and hard constraints. State whether prior-distance or support restrictions narrow that range. |
| Assessment | Describe which components are directly constrained, indirectly informed, or mainly supplied by the prior. |

A range consistent with targets is not a posterior probability interval. It answers how far an output can move while retaining acceptable data fit. On fixed support with linear outputs and interval constraints, its endpoints are linear programs. Very wide or unbounded ranges are informative. A narrow range produced only by tight prior restrictions should be labeled accordingly.

For example, matching total wages directly constrains that aggregate; it does not necessarily constrain revenue from an additional bracket within a broad income cell. A tax on wealth among high-wage units depends on the joint distribution, even if wage and wealth totals are both tightly matched. For an output in 2097, the report should name the forecast and distributional assumptions rather than describe the result as identified by historical targets.

## Computation

At 200,000 records and 190 numeric fields, one year's values contain 38 million numbers: approximately 152 MB in 32-bit storage or 304 MB in 64-bit storage. Across 83 years, that is 3.154 billion numbers, about 12.6 or 25.2 GB, before nonfilers, alternate completions, strings, derivatives, solver state, and intermediate arrays. These are arithmetic storage estimates, not measurements of the repository. The brief reports a 141 GB peak for its last full run; I did not reproduce it.

A dense matrix of 1,000 target contributions for 200,000 records contains 200 million numbers, about 0.8 GB in 32-bit or 1.6 GB in 64-bit storage per year. Many contributions are zero. Grouped sums, sparse storage, or computing contributions in blocks can reduce memory. Retaining dense target matrices and differentiated tax computations for every year simultaneously is unnecessary. Five candidate completions multiply record-dependent work roughly fivefold; full donor-by-recipient matrices can be much larger.

| Decision variables | Convexity under the stated assumptions |
|---|---|
| Weights on fixed records | Convex with a convex prior distance, linear accounting restrictions, and convex losses on linear totals. Relative entropy is strictly convex on positive-prior support. |
| Masses on fixed completed records | The same result holds. Allowing several discrete completions does not itself create nonconvexity. |
| Weights with linear time restrictions or quadratic penalties on fixed linear transitions | A joint convex problem remains possible. |
| Weights and freely transformed amounts | Generally nonconvex, even before discrete tax-law decisions. Special restricted parameterizations may remain convex. |
| Learned record generator or discrete record selection | Generally nonconvex; discrete choices additionally require search or a relaxation. |

Nonconvexity here comes from the chosen finite parameterization. An optimization over all probability distributions with a suitable full-support reference and linear measurements can itself be convex. Representing and solving that infinite-dimensional problem is the practical difficulty. A freely moving finite set of records does not inherit its convexity.

### Weight solver

The fixed-record entropy problem often permits a smaller optimization with one coefficient per target. This is the dual problem: an equivalent optimization in the coefficients that price target discrepancies. In the interior, with only soft targets and no extra hard constraints, stationarity gives \(w_i=b_i\exp(-a_i'\lambda/\kappa)\), where \(a_i\) is record \(i\)'s contribution vector and \(\kappa=\alpha/N_*\). Hard linear constraints add their own coefficients; weight bounds require corresponding modifications.

For this simple case, minimize

\[
\Phi(\lambda)=\kappa\sum_i b_i\exp(-a_i'\lambda/\kappa)
+y'\lambda+\frac12\lambda'\Sigma\lambda,
\qquad \nabla\Phi=-Aw+y+\Sigma\lambda.
\]

The gradient is the discrepancy between the fitted totals and the observed totals, allowing the error implied by their covariance. The objective is convex. It solves the same entropy-plus-target-error problem, using hundreds or thousands of coefficients rather than hundreds of thousands of weights. This equation follows directly by minimizing the weight objective conditional on a target-error multiplier. It also explains why minimizing squared residuals of exponentially tilted totals is a different method.

Normalize target columns and transform their covariance consistently before optimization. Use stable exponential calculations, and check the hard constraints, objective, gradient, and target discrepancies in original units. A limited-memory quasi-Newton optimizer, which uses successive gradients to approximate curvature, is a reasonable starting solver. A central-processing-unit implementation of the same convex problem provides a useful comparison; a GPU is not mathematically necessary.

### JAX implementation

JAX provides automatic differentiation, which applies calculus rules to program operations to compute derivatives, and compilation of array functions for accelerator execution. A prototype can use `jax.value_and_grad` on the dual objective and `jax.jit` to compile repeated evaluations. The official documentation verifies these capabilities. [Differentiation](https://docs.jax.dev/en/latest/automatic-differentiation.html), [compilation](https://docs.jax.dev/en/latest/jit-compilation.html).

The proposed computation is a repeated sequence: calculate weights from current coefficients, sum record contributions in fixed-size blocks, calculate the loss and gradient, and update the coefficients with a step that reduces the objective. Accumulate global target totals before applying nonlinear residual penalties. Summing squared errors separately within record blocks would solve a different problem. Keep input arrays on the device when memory permits; otherwise stream blocks and retain only the necessary summaries.

For value transformations, differentiate through the transformation and smooth portions of the measurement calculation. Hold random imputation draws fixed during fitting. For exact categorical alternatives, optimize completion masses rather than attempting to differentiate a donor draw. Bracket indicators have zero derivatives away from their boundaries; a GPU cannot infer missing threshold-crossing information. JAX's documentation explicitly illustrates this behavior for step functions. [JAX differentiation FAQ](https://docs.jax.dev/en/latest/faq.html#why-are-gradients-zero-for-functions-based-on-sort-order).

Enable and compare 64-bit arithmetic for large totals, signed amounts, and tight tolerances. JAX defaults to 32-bit array creation unless its 64-bit option is enabled. This is a numerical validation choice; the speed and memory tradeoff needs measurement on the available GPU. [JAX numeric types](https://docs.jax.dev/en/latest/101/default_dtypes.html).

Retain the existing separation between source preparation, annual materialization, and output writing. R-trained imputation models can initially export candidate records or donor probabilities to the new solver. A JAX calibration prototype does not require porting every forest and source transformation. All implementation benchmarks and substantive computation on this cluster would run through Slurm. I have not benchmarked GPU speed, compilation cost, memory use, or the installed JAX environment.

## Failure diagnostics

**Contradictory evidence.** Check definitions, duplicate rows, units, and adding-up relationships before fitting. A total smaller than a required nonnegative subtotal cannot be reconciled by more flexible records. Soft targets produce a compromise; report the conflicting measurements and their residuals. Infeasible hard restrictions require correcting the restrictions or population construction, with no purported fitted solution.

**Insufficient support.** Check separate attainable ranges, then joint feasibility where important. Passing separate ranges is necessary but not sufficient. A linear feasibility problem can identify whether exact targets are jointly attainable; a separating combination of target rows can explain why they are not. Diagnose near-infeasibility as well: a target near the boundary may require almost all weight on a few records. Expand support only with an economic or source-data justification.

**Extreme weights.** Report weight ratios, concentration by cell, and concentration of each important policy output. A common concentration summary is the effective sample size, \((\sum_iw_i)^2/\sum_iw_i^2\), the number of equally weighted records with the same weight concentration. It is not a complete sampling-variance estimate for the PUF, and it can be inflated by splitting one donor into copies. Report concentration at the original-record or donor level too. Large original design weights are not necessarily mistakes; substantial changes relative to them need explanation. Weight bounds are optional modeling restrictions whose effect on data fit should be visible.

**Noisy targets.** Fitting every noisy detail precisely can move the population away from its true distribution. Use measurement errors, correlated-source treatment, and checks against held-out tables or historical years that were not already used in the prior. Residual size alone cannot choose prior strength: a flexible model can reduce it by sacrificing unsupported relationships. Compare relevant tax outputs as well.

**Thin cells and tails.** Report independent donor counts and the presence of positive and zero outcomes, not just the number of completed records. Pool information across neighboring cells through a stated model where appropriate; do not disguise a sparse cell with many donor copies. A tail extrapolation should retain its source and sensitivity assumptions. Suppression in a published table represents incomplete information, not a zero target.

**Numerical or postprocessing errors.** Distinguish solver termination from acceptable fit. Check the objective's optimality conditions and exact identities, then recompute all targets on the written records. Repeat those checks after sampling, compression, rounding, or rescaling. For a nonconvex formulation, compare multiple starting points and exact-law results. A small gradient on a smoothed reporting model does not establish a good fit under the actual law.

## Verification limits

I verified the repository statements by reading the linked local source files, not by executing their behavior. The external sources cited above were opened and their relevant text or abstract inspected. Hainmueller's full paper was not retrieved from the attempted author-hosted PDF; only the publisher's abstract was verified. The transport PDF download failed, but the arXiv HTML text was available and inspected. No claim here relies on an unread background paper.

I did not inspect the underlying PUF, published target tables, forecast inputs, or simulator implementation. Consequently, target precisions, source reconciliation errors, disclosure-error distributions, empirical support adequacy, and historical predictive performance remain unverified. No search for new target datasets was undertaken. The numerical dimensions above are planning arithmetic; the implementation schedules below are estimates conditional on usable existing inputs and access to the simulator's reporting calculations.

## Revision history

- 2026-10-07: Independent first-principles formulation from the shared brief, specified repository code, and the external sources cited above.

## Candidate formulations

### 1. Entropy calibration with expanded candidate records

Estimate annual population masses on fixed, plausible completed records, using relative entropy and precision-weighted target errors. Retain current imputations and growth factors as documented starting distributions. Add selected alternative imputations, participation states, or amount scenarios where support is inadequate. This is the first choice for the two-week window because its optimum and failures can be assessed separately from the quality of its prior.

In the first week, define the population and target registry, remove duplicate information, choose provisional error scales, export candidate records, and implement the convex annual solver. In the second, connect one censored target family to historical reporting rules, compare an observed pre-change year with a post-change year, materialize the remaining annual priors, and produce support, fit, concentration, and output-sensitivity reports. Check a long-run year such as 2097 explicitly. A source model is still needed for 2015–2016 and for wealth before 2022; any provisional backcast must be labeled.

The deliverable is a working estimator and evaluated prototype files, with every target and prior assumption recorded. Full source reconciliation and validation of all 190 fields are not a credible two-week promise. The principal limitation is that fitted outcomes remain restricted to the candidate set. The next decision would be based on measured support failures and output sensitivity.

### 2. Entropy calibration with a small transformation model

Estimate weights jointly with a limited set of amount and distribution-shape parameters. Penalize changes in those parameters according to source reliability, preserve identities by construction, and optionally smooth their deviations from expected growth across years. Use candidate alternatives for changes in categorical participation. This is the second choice when important targets cannot be reached by a manageable fixed candidate set.

Two weeks could build the first formulation's core plus one transformation family, such as wage levels and dispersion within broad demographic groups, or underlying deductible expenses. It would require recomputing measurement contributions, validating derivatives away from thresholds, comparing multiple starting points, and evaluating the final exact reporting rules. The deliverable would be a controlled comparison showing whether value flexibility improves fit and policy-output stability. It would not establish a globally optimal joint file or validate unrestricted changes to every amount.

### 3. Joint population model with generated records

Estimate a probabilistic model of demographics, participation, incomes, expenses, wealth, and consumption, with parameters shared across years. Train it on the source microdata while fitting aggregate measurements and their reporting rules. Generate weighted output records from the fitted model, then check their aggregate and policy behavior. Such a model can represent values and combinations absent from the original PUF, but those additions reflect assumptions unless supported by further evidence.

In two weeks, a feasible deliverable is a restricted experiment on one variable block and a few years, using simulated populations with known answers to test recovery and identification. A production replacement would additionally require coherent cross-survey dependence, mixed-variable distributions, valid tails and identities, selection models, temporal assumptions, and controlled sampling error in the output. This is ranked third for the stated window; its flexibility does not reduce those evidentiary requirements.
