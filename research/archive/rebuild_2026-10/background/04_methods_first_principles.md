---
title: "Tax-Data rebuild — the estimation problem stated from first principles"
role: evidence
workstream: rebuild
status: current
updated: 2026-10-07
sot: research/rebuild/BRIEF.md
supersedes: []
superseded_by: null
---

# The estimation problem stated from first principles

This note states mathematically what Tax-Data estimates, what the choices
are, and what each choice implies. It is grounded in the current code
(`src/reweight.R`, `src/create_2017_puf.R`, `src/project_puf.R`,
`src/materialize.R`, `src/imputations/consumption.R`,
`src/imputations/wealth.R`, `src/imputations/tilt_solver.R`) and in
`research/rebuild/BRIEF.md`. It is not a literature review; the few
references are there so the reader can find the standard treatment of a
result.

## Summary

1. **The object is one probability distribution per year** over the
   characteristics of a tax unit, plus the number of tax units. A microdata
   file is a discrete version of it: each record is a point, each weight is
   the mass on that point. "Close to the prior" means a small distance
   between two such distributions.

2. **With weights as the only decision variable and relative entropy as the
   distance, the answer has a closed form:** each record's new weight is its
   prior weight times the exponential of a weighted sum of its contributions
   to the targets. This is raking, entropy calibration, and the same
   exponential tilt the wealth module already uses (`tilt_solver.R`). The
   unknowns in the solver are one number per target, not one per record.

3. **The current linear program is not a distance-to-prior problem.** It
   minimizes the unweighted sum of adjustment factors inside a box. A linear
   objective over a box always lands on a corner, and a corner solution can
   have at most as many records strictly inside the box as there are
   binding targets (188 rows, 176 distinct). The 114 interior records the
   brief reports are what that theorem predicts. Any strictly convex
   distance removes this behaviour.

4. **Weights alone cannot create a combination of characteristics that no
   record has, and they cannot change the shape of a distribution within a
   record's neighbourhood.** They can only re-mix existing records. When the
   target requires something outside that reach, reweighting either fails or
   puts enormous weight on a few records. The current pipeline already
   changes values (the per-bin dollar rescale in `create_2017_puf.R`, the
   one-factor-per-variable growth in `project_puf.R`); the real question is
   how flexible the value changes should be, not whether to allow them.

5. **Most targets that look nonlinear become linear in the weights once
   record values are fixed.** A quantile with a published value becomes a
   count below that value; a share becomes a difference of two totals.
   Targets become genuinely hard (discontinuous) when values move, because
   records then cross bin edges and tax-law thresholds.

6. **Censored targets need a reporting calculator inside the estimation.**
   Matching post-2017 itemized deductions means computing, for every record
   and year, whether it itemizes under that year's law. With values fixed
   this is computed once and the target stays linear in the weights. With
   values moving, the itemization switch needs a smooth approximation or an
   alternating scheme.

7. **The Bayesian reading is exact for the weights-only problem:** the best
   single file under a "population drawn from the prior" model plus normally
   distributed target errors is penalized entropy calibration. The strength
   of the prior (how many records' worth of information it carries) is a
   tuning number that must be chosen; it does not come out of the data.
   The choice between the two directions of relative entropy is not settled
   by the Bayesian argument; both are defensible and agree for small
   adjustments.

8. **When values are decision variables, the prior must be placed on the
   distribution, not on each record's value.** Choosing each record's "most
   likely" value collapses dispersion; a file where every record holds its
   conditional median wealth is wrong even if every median is right.

9. **What the targets pin down can be measured.** Under a linear
   approximation, the share of a quantity's prior uncertainty removed by the
   targets equals the R-squared from regressing each record's contribution to
   that quantity on its contributions to the targets. This gives a per-output
   "data-determined share" that can be reported next to every result.

10. **The computation is small for a GPU.** The weights-only problem for one
    year is a smooth concave maximization in a few thousand unknowns; each
    step is one pass over the records. A joint all-years problem with value
    transformations has tens of millions of unknowns and is not convex, but
    automatic differentiation and first-order methods handle it in minutes
    to hours. Memory, not arithmetic, is the binding constraint.

11. **Recommended order:** first, per-year penalized entropy calibration on
    weights, with a small layer of rank-dependent value transformations and
    multiple imputed variants per record (section 10, formulation A); then,
    if it earns its keep, one joint problem across years (formulation B).
    A full generative model of the population (formulation C) is the most
    flexible and the riskiest, and should be confined to known gaps.

## 1. The object being estimated

For each year $t$, let $\mathcal{X}$ be the space of possible tax-unit
characteristics: filing status, ages, dependents, about 190 income,
deduction, wealth and consumption variables, and filer status. The object is

$$
\mu_t \;=\; N_t \, P_t ,
$$

where $N_t$ is the number of tax units in year $t$ and $P_t$ is a probability
distribution on $\mathcal{X}$.

In words: a year's population is a headcount times a description of how
those units are spread over all possible combinations of characteristics.

A microdata file approximates $\mu_t$ by finitely many points:

$$
\hat\mu_t \;=\; \sum_{i=1}^{n} w_{it}\, \delta_{x_{it}},
$$

where $x_{it}$ is record $i$'s vector of characteristics in year $t$,
$w_{it} \ge 0$ is its weight, and $\delta_x$ is a point mass at $x$.

In words: the population is represented as $n$ archetypal tax units, each
standing in for $w_{it}$ real ones. The sum of weights is the population
count. Every published aggregate is a weighted sum over records.

Three things are being chosen: how many records ($n$), where they sit
($x_{it}$), and how much each counts ($w_{it}$). The three options in the
brief — weights only, weights plus values, new records — correspond to
freeing $w$; $w$ and $x$; and $w$, $x$ and $n$.

The prior is a file of the same form, $\hat\mu^0_t = \sum_i w^0_{it}
\delta_{x^0_{it}}$: the 2015 PUF with survey imputations and the non-filer
pool, aged to year $t$ by growth factors and demographic factors. The
answer is a file "close" to the prior that fits the targets in proportion to
their precision.

### Distances between distributions

"Close" needs a distance. When the records are fixed and only weights move,
all the standard choices compare the new weight $w_i$ to the prior weight
$w^0_i$ record by record and add up. Writing $r_i = w_i / w^0_i$ for the
adjustment ratio:

| Distance | Per-record cost | Resulting weights | Plain meaning |
|---|---|---|---|
| Relative entropy of new from old, "$\mathrm{KL}(w\Vert w^0)$" | $w^0_i\,(r_i \log r_i - r_i + 1)$ | $w^0_i \exp(\text{linear in record's target contributions})$ | Multiplicative adjustment. Weights stay positive. Cost of doubling a weight is larger than cost of halving it. Can drive some weights close to zero cheaply. This is raking. |
| Relative entropy of old from new, "$\mathrm{KL}(w^0\Vert w)$" | $w^0_i\,(r_i - 1 - \log r_i)$ | $w^0_i / (1 + \text{linear})$ | Strongly resists shrinking a weight toward zero (cost goes to infinity), tolerates large increases. This is the empirical-likelihood form. |
| Chi-square | $w^0_i\,(r_i-1)^2/2$ | $w^0_i (1 + \text{linear})$ | Symmetric in absolute changes. Closed-form solution. Weights can go negative, which a microdata file cannot have. This is linear (regression) calibration. |
| Bounded variants (Deville and Särndal's "logit" family) | as above but infinite outside $[L, U]$ | ratio confined to $[L,U]$ smoothly | What the current box $[0.34, 1.66]$ tries to do, but with a smooth interior instead of corners. |

All four agree to second order: for small adjustments each cost is
approximately $w^0_i (r_i - 1)^2 / 2$. They differ only when targets force
large moves, and then the differences are about which records are allowed to
absorb the change. Relative entropy (first row) is the natural default: it
is what raking computes, what the wealth tilt computes, and it never
produces a negative weight.

The relative-entropy rows are written in the form for unnormalized weights,
$\sum_i [w_i \log(w_i/w^0_i) - w_i + w^0_i]$, so that the total population
can move as well as its composition.

**Once values move, these distances break.** Relative entropy between two
distributions on different points is infinite, because the new distribution
puts mass where the old one had none. A distance that counts how far values
were moved is needed instead. The natural one is a transport cost: the
weighted average squared distance each record was moved, for example

$$
\sum_i w_i \big(\log x^{\text{new}}_{iv} - \log x^{0}_{iv}\big)^2 .
$$

In words: the cost of a value change is the weighted average squared
percentage change in each record's value. For a rank-preserving change of one
variable, this equals the squared Wasserstein-2 distance (the standard
"earth mover" distance) between the old and new distributions of the log of
that variable. A combined problem then has two kinds of cost — reweighting
cost and moving cost — and their relative size decides, for example, whether
a rise in total wages is met by more high earners or by higher wages per
earner. That split is a modelling choice the targets usually cannot make
(section 7).

### What the current linear program is

`src/reweight.R` solves, for adjustment factors $r_i$,

$$
\min_r \sum_i r_i \quad \text{subject to} \quad
(1-\tau_k)\,y_k \le \sum_i a_{ik} w^0_i r_i \le (1+\tau_k)\,y_k ,\qquad
1-\varepsilon \le r_i \le 1+\varepsilon,
$$

with $\varepsilon = 0.66$, $a_{ik}$ the record's contribution to target $k$
(mostly an indicator for being a nonzero filer in a cell), and $\tau_k$ the
tolerance from `config/target_info/baseline.csv`.

In words: within the tolerance bands, make the unweighted sum of factors as
small as possible.

This has no term for staying near the prior. The objective treats a factor
on a record of weight 5 the same as on a record of weight 5,000, and it
rewards pushing every factor down. A linear objective over a region bounded
by flat faces is minimized at a vertex, and at a vertex every factor is at a
bound except for at most one per binding target constraint. With 188 target
rows, of which 176 are distinct (the four "all returns by filing status"
rows each appear four times; checked with `sort | uniq -d` on 2026-10-07),
at most 176 factors can be interior. The cached solution has 114. The corner
behaviour is a property of the formulation, not of the solver or the
tolerance.

## 2. Decision variables

### (a) Weights only, on fixed records

What it buys: the problem is convex (any local optimum is the global one),
the dual has one unknown per target, accounting identities inside a record
(wages of primary plus spouse equal total wages) hold automatically, and
the joint distribution within each record is exactly the prior's.

What it costs: the answer can only re-mix existing records. Formally, the
set of points with positive mass — the support — cannot grow:

$$
\operatorname{supp}(\hat\mu_t) \subseteq \{x^0_{1t}, \dots, x^0_{nt}\}.
$$

In words: every tax unit in the answer is a copy of some prior record.

Weights only is provably insufficient in three situations:

- **Infeasibility by range.** A cell average that lies outside the range of
  the records in the cell cannot be reached by any nonnegative weights. If
  every record in a cell has wages below \$80,000, no reweighting gives the
  cell a mean of \$90,000.
- **Absent combinations.** A target that needs mass on a combination no
  record has (for example, a 2023 count of units with both tip income and a
  characteristic that never co-occurs with tips in the prior) cannot be met.
- **Shape changes in thin regions.** Feasible in principle but degenerate in
  practice: if the top 0.1 percent's share of wages rises, reweighting meets
  it by inflating the handful of records at the very top. The file then has
  the right total and a few records with enormous weight. This is the
  common case, and it gets worse each year away from 2015.

A useful warning sign is the Kish effective sample size, $(\sum_i
w_i)^2/\sum_i w_i^2$ — the number of equally weighted records that would
carry the same statistical information. Reported per cell and per year, a
fall in it shows where weights are doing work they cannot do well.

### (b) Weights plus value transformations

The current pipeline already does this in two restricted forms: the per-AGI
bin dollar rescale in `create_2017_puf.R` (about 8 multipliers per
variable), and the projection in `project_puf.R` (one multiplier per
variable per year, applied to every record, divided by an "extensive factor"
for the change in the count of holders). So for every dollar variable except
wealth, the shape of each distribution after 2015 is fixed and only its
scale and the weights move.

A natural generalization is a smooth, rank-dependent multiplier:

$$
x_{iv,t} = x^0_{iv}\,\exp\!\big(\phi_{v,t}(\rho_{iv})\big),
$$

where $\rho_{iv}$ is record $i$'s rank (percentile) in variable $v$ in the
prior and $\phi_{v,t}$ is a smooth function with a handful of knots.

In words: each variable's values grow at a rate that may differ along its
distribution — top wages may grow faster than median wages — but records
keep their order. A constant $\phi$ is today's method.

What it buys: the shape of income distributions can drift with evidence
(SOI tables by AGI bin through 2023, top-share series), without inflating a
few weights. Identities can be kept exactly by transforming parents and
expressing children as shares (wages1 = $s_i \times$ wages, wages2 =
$(1-s_i) \times$ wages), so the split variables never get their own
multiplier.

What it costs: the problem is no longer convex jointly in weights and
values (target contributions are products $w_i \times x_{iv}$). It is
convex in each block with the other held fixed if targets are totals and
$\phi$ enters linearly, so alternating between the two converges to a
stationary point but not necessarily the best one. And cell membership
moves: a record whose AGI rises crosses an AGI bin edge, so bin counts
become step functions of the values (section 3).

### (c) Generating new records

What it buys: the support can grow. This is the only option that can fill a
combination absent from the prior, and the only clean fix for weight
degeneracy (split a heavy record into several nearby records with smaller
weights).

What it costs: a model of the joint distribution in the region where records
are added, which can invent structure the data do not support. The top of
the income distribution, where tax liability concentrates, is exactly where
the PUF is thinnest and most disclosure-altered, so a generative model is
weakest where it would matter most.

The repository already does (c) in three bounded places: the non-filer pool
(whole records from the CPS ASEC), the Forbes splice (top-wealth records),
and the donor draws in the imputations (new values for existing records,
which is (c) for the imputed coordinates). In the sequential Monte Carlo
literature the combination of reweighting with occasional record splitting
plus small perturbation is called "resample-move"; it is the standard answer
to weights that degenerate over many steps (section 4).

## 3. Targets

### General form

A target is a function of the distribution compared with a published
number:

$$
y_k = g_k(\mu_t) + e_k, \qquad e_k \sim \text{Normal}(0, \sigma_k^2).
$$

In words: published number $k$ equals the true population quantity plus an
error whose typical size is $\sigma_k$. The error covers sampling error in
the published table, definitional gaps between the source and our variables,
and, for projections, forecast error.

Hard constraints are the case $\sigma_k = 0$. They should be reserved for
true identities (the population partition by age band, accounting
identities), because a set of hard constraints can be infeasible and then
the problem has no answer. A soft target is never infeasible; a
contradiction between two soft targets is resolved in proportion to their
precisions.

The general fitting term is

$$
\tfrac12 \sum_k \Big(\frac{g_k(\hat\mu_t) - y_k}{\sigma_k}\Big)^2 ,
$$

the sum of squared standardized misses. In words: a miss of one standard
error costs the same for every target, whether it is a count of returns or
a dollar total.

Two practical consequences:

- **Duplicated or nested targets count more than once.** The four rows that
  appear four times in `config/target_info/baseline.csv` are harmless in the
  current linear program but would receive four times the weight in a
  penalized objective. Nested targets (AGI-bin counts and their total)
  carry correlated errors; treating them as independent overstates the
  information in the total. The cleanest rule is to target the finest
  published cells and not their sums, or to give the sum the precision of
  the residual it adds.
- **$\sigma_k$ has to be stated per target.** SOI publishes coefficients of
  variation for many Publication 1304 estimates (I believe so from past use;
  I did not check this for this note), which gives sampling error. The
  definitional component (how well our variable matches the published
  concept) must be set by judgment and is often larger.

### Which targets are linear in the weights

With record values fixed, every target that is a weighted sum of a
per-record quantity is linear in the weights:

$$
g_k(\hat\mu_t) = \sum_i w_{it}\, a_{ik}, \qquad a_{ik} = h_k(x_{it}).
$$

In words: each record contributes a fixed amount $a_{ik}$ to target $k$ —
its wages if it is in the cell, 1 for a count, 0 otherwise — and the target
is the weighted sum.

This covers more than it first seems:

| Target type | Rewritten as | Linear in weights? |
|---|---|---|
| Total or count in a cell | $\sum_i w_i\, x_{iv}\,1[\text{cell}]$ | Yes |
| Quantile with published value $q$ at probability $p$ | $\sum_i w_i\,1[x_i \le q] - p\sum_i w_i = 0$ | Yes |
| Ratio or mean $y_1/y_2 = c$ | $\sum_i w_i(a_{i1} - c\,a_{i2}) = 0$ | Yes |
| Top share with published threshold | $\sum_i w_i x_i 1[x_i > q] - s\sum_i w_i x_i = 0$ | Yes |
| Top share with unpublished threshold | add the threshold as an unknown | No (one extra unknown) |
| Variance, Gini, concentration | sums over pairs of records | No (quadratic) |
| Anything after values move | contributions depend on values | No; bin edges make it a step function |

The rewriting changes the error model: the standard error of a ratio is not
the standard error of its rewritten difference. That has to be carried
through when setting $\sigma_k$.

Under option (b), all cell-based targets become piecewise constant in the
values: a small change in a record's AGI either changes nothing or moves the
record across a bin edge. Gradient methods see a zero gradient almost
everywhere. Two standard fixes: replace the indicator by a smooth ramp a few
hundred dollars wide (a "soft bin"), or alternate — fix cell memberships,
solve, recompute memberships, repeat until they stop changing.

### Censored targets

Some published numbers are the underlying quantity passed through that
year's tax law. Itemized deductions after 2017 are the main case:

$$
y^{\text{item}}_{t} = \sum_i w_{it}\; D_{it}\; 1\!\big[D_{it} > S_t(\text{status}_i, \text{age}_i)\big],
\qquad D_{it} = d_t(\text{deductible expenses}_i,\ \text{AGI}_i).
$$

In words: the published total is the sum, over units that itemize, of their
allowable deductions. Allowable deductions $D_{it}$ come from the record's
underlying expenses through that year's rules (the state and local tax cap,
the medical expense floor, mortgage interest limits, the pre-2018
limitation on itemized deductions for high incomes), and a unit itemizes when
they exceed its standard deduction $S_t$.

What this requires of the computation:

- **A reporting calculator per year.** For each record and each historical
  year, compute $D_{it}$ and the itemization switch. This is a small piece of
  tax law, not the full Tax-Simulator. For some published items (taxable
  income bins, alternative minimum tax counts, credits) the reporting
  function is most of the tax calculator; for those, either call the
  GPU Tax-Simulator's functions or do not target them.
- **With values fixed, the target is still linear in the weights.** Compute
  $a_{ik} = D_{it}1[\cdot]$ once per record per year and treat it like any
  other contribution. This is the easy case and covers pure reweighting.
- **With values moving, the switch is discontinuous.** A smooth approximation
  (replace the indicator by a logistic function of $D_{it} - S_t$ with a
  width of a few hundred dollars) or the alternating scheme above.
- **Behaviour is part of the reporting process.** Itemization is a choice;
  some units that would gain by itemizing do not, and in years with the
  alternative minimum tax the choice depends on it. A deterministic switch
  is an approximation to what produced the published table.

The identification consequence is in section 7: after 2017 the itemizer
tables describe only the part of the deductible-expense distribution above
a threshold that varies by filing status. The part below comes from the
prior alone.

## 4. Time

### Separate years or one problem

**Separate per year.** Each year's prior is built independently (the 2015
base aged to year $t$), and each year is calibrated on its own targets.
Simple and parallel. Adjacent years can differ in ways nothing in the data
supports, because nothing ties them together.

**Forward chain (a "filter").** Year $t$'s prior is year $t-1$'s answer aged
one year. Information flows forward only: the 2019 file reflects 2015–2019
targets but not 2023 ones.

**One joint problem (a "smoother").**

$$
\min_{\{\hat\mu_t\}} \;\; D(\hat\mu_{2015}\,\Vert\,\mu^0_{2015})
\;+\; \sum_{t>2015} \kappa_t\, D\!\big(\hat\mu_t \,\Vert\, A_t\hat\mu_{t-1}\big)
\;+\; \sum_t \tfrac12\sum_{k\in K_t}\Big(\frac{g_k(\hat\mu_t)-y_k}{\sigma_k}\Big)^2 .
$$

In words: the 2015 file should be close to the 2015 prior; each later year
should be close to the previous year's file after aging it one year
($A_t$ is the aging step: growth factors, demographic change, mortality);
and every year should fit its targets. $\kappa_t$ sets how strongly
consecutive years are tied.

Every year's file then uses every year's targets. A 2023 Publication 1304
table informs 2019 because the path between them must be plausible. This is
the same structure as a state-space model in time-series econometrics, with
the whole distribution as the state.

If the records are shared across years (the current union-base design, in
which every record exists in every year with a possibly zero weight), the
link term is simply relative entropy between year-$t$ weights and year
$t-1$ weights grown by the aging factors. A record with zero prior weight in
a year can never gain weight under relative entropy, which is a property to
be aware of: under the current Block E design, a non-filer pool record is
locked out of every year it does not represent.

### Projection to 2097

After 2023 the targets are few and imprecise: CBO and SSA aggregates and
population by age, sex and marital status. In the joint formulation those
years are simply years with large $\sigma_k$, and the file follows the aging
step.

That makes the aging step carry nearly all the content. If the aging step
does not grow values and an aggregate wage target says wages doubled, the
estimation must meet it by reweighting toward high earners — composition
change standing in for real wage growth. So the expected growth of values
belongs in the prior ($A_t$), and the targets should correct deviations from
it, not supply it. "Projection is not a separate method" holds only with
that division of labour.

Long chains of reweighting degenerate: multiplying weights by year-specific
factors for 80 years concentrates mass on fewer records, as in any long
sequence of reweightings. Two remedies: re-centre the prior on the current
answer each year (the forward chain does this automatically), and split
heavy records when the effective sample size falls below a set level
(option (c), "resample-move").

## 5. Imputation models as priors for missing variables

Split each record's characteristics into the observed part $x'$ (PUF
variables) and the missing part $z$ (wealth, consumption, tips, and so on).
An imputation model trained on a survey supplies a conditional distribution
$p(z \mid x')$. The prior over the full record is

$$
P^0(x', z) = P^0_{\text{PUF}}(x')\; p_{\text{survey}}(z \mid x').
$$

In words: the joint prior is the PUF distribution of what the IRS observes,
times the survey model's distribution of the rest given what the IRS
observes. Everything the survey says about the relationship between $z$ and
$x'$ enters here and nowhere else.

There are three ways to represent the conditional in a microdata file:

1. **One draw per record** (the current default for most modules). The
   conditional is represented only in aggregate, across records with similar
   $x'$.
2. **Several draws per record**, each a separate row carrying $w_i/M$ of the
   weight. The conditional is represented within each record.
3. **A probability vector over donors** for each record, $p_{ij}$ — what the
   wealth module carries before sampling (`tilt_solver.R`).

**Adjusting an imputed conditional** means changing $p(z\mid x')$ while
leaving the distribution of $x'$ alone. The wealth tilt does exactly this:

$$
q_{ij} \;=\; \frac{p_{ij}\exp(Z_j^\top\lambda)}{\sum_l p_{il}\exp(Z_l^\top\lambda)} ,
$$

where $Z_j$ is donor $j$'s vector of wealth target features.

In words: within each PUF record, donors with more of a characteristic the
targets want more of become more likely, by a common exponential factor;
each record's donor probabilities still sum to one, so the record's own
weight, and hence the distribution of IRS variables, is untouched. This is
the relative-entropy projection of the imputed conditional onto the targets,
holding the IRS-variable distribution fixed as a hard constraint.

The general framework makes this a special case: expand the file to records
× imputed variants, and solve the entropy problem with one hard constraint
per record (its variants' weights sum to its weight). With one draw per
record (representation 1), there is no within-record freedom, so a target on
an imputed variable can only be met by reweighting whole records, which
also moves the IRS variables. **For imputed variables to be adjustable
without disturbing what the PUF pins, records need several imputed variants
each.** Memory grows linearly with the number of variants; 5 to 20 is
typical in multiple imputation.

Two cautions.

- **Do not use the same number twice.** The current imputations calibrate
  internally (wealth to SCF cell totals; consumption to NIPA personal
  consumption expenditures). In a single estimation, those totals belong in
  the target list, applied once, and the prior should be the uncalibrated
  model output. Applying them in the imputation and again as targets is
  harmless if both match exactly, but it double-counts their information in
  any statement about precision.
- **Sequential versus joint.** Imputing, then calibrating, fixes the imputed
  conditionals before the IRS targets are applied. In a joint problem the
  imputed conditionals respond to IRS targets too (if the income distribution
  shifts, consumption shifts with it through the conditional). The joint
  version is more coherent; the sequential version is easier to debug.

## 6. The Bayesian reading

**Prior.** A probability statement about what the population's distribution
could be before seeing the targets. Here: the true year-$t$ population looks
like $N$ independent draws from the aged, imputed PUF distribution $P^0_t$.

**Likelihood.** How probable the published numbers are, given a candidate
population. Here: each $y_k$ is the candidate's value of that quantity plus
a normal error with standard deviation $\sigma_k$.

**Posterior.** The prior updated by the likelihood: which populations remain
plausible once the published numbers are known.

**Best single estimate.** The single most probable population under the
posterior (the posterior mode). For the weights-only problem it is the
solution of

$$
\min_{w\ge 0}\;\; \alpha\, \mathrm{KL}(w \Vert w^0)
\;+\; \tfrac12 \sum_k \Big(\frac{\sum_i w_i a_{ik} - y_k}{\sigma_k}\Big)^2 .
$$

In words: stay close to the prior in relative entropy, fit each target to
within its standard error, with $\alpha$ setting how much the prior resists.

Why relative entropy: if a population of $\alpha$ units is drawn
independently from $P^0$, the probability that its empirical distribution
looks like $Q$ falls off as $\exp(-\alpha\,\mathrm{KL}(Q\Vert P^0))$ (a
standard large-deviations result, Sanov's theorem). So relative entropy is
minus the log prior, and $\alpha$ is the number of independent units' worth
of information the prior carries. Minimizing it plus the squared standardized
misses is maximizing the posterior. Penalized entropy calibration is
therefore exactly the most likely file under this prior and normal target
errors. With exact targets ($\sigma_k \to 0$) it becomes ordinary entropy
calibration (raking), and $\alpha$ no longer matters.

**The Bayesian argument does not pick the direction of relative entropy.** A
Dirichlet prior centred on $P^0$ (another standard prior over
distributions) gives $\mathrm{KL}(P^0\Vert Q)$ instead, whose solution is the
empirical-likelihood form $w^0_i/(1+\text{linear})$. Schennach (2005) shows
a related nonparametric Bayesian derivation of exponential tilting. Both
directions are defensible; they agree to second order and differ only for
large adjustments. The choice should be made on behaviour (which records are
allowed to absorb large changes), not on the Bayesian story.

**The prior strength $\alpha$ is a choice.** It does not come out of the
data. Reasonable anchors: the PUF's effective sample size in the relevant
domain (a prior built from 200,000 records carries less information in the
top 0.1 percent than overall), or a value chosen by holding out a subset of
targets and checking how well the calibrated file predicts them. A single
$\alpha$ for the whole file says the prior is equally trustworthy
everywhere; it is not, and a per-cell $\alpha$ is a natural extension.

**The Lagrange multipliers have a meaning.** At the solution,

$$
w_i = w^0_i \exp\!\Big(\tfrac1\alpha\sum_k a_{ik}\lambda_k\Big), \qquad
\lambda_k = \frac{y_k - \sum_i w_i a_{ik}}{\sigma_k^2}.
$$

In words: each record's weight is its prior weight times an exponential of
its contributions to the targets, and each target's multiplier is its
remaining miss divided by its error variance. A large multiplier marks a
target that the prior strongly disagrees with. They are the first diagnostic
to read.

**When values are decision variables, the prior must stay on the
distribution.** It is tempting to give each imputed value its own prior
density and pick the most probable value for each record. That drives every
record to its conditional mode and wipes out dispersion: a file where every
record holds the conditional median of wealth given income has the wrong
distribution of wealth even if each median is correct. The mode that matters
is the mode over distributions, which is what relative entropy and transport
costs measure.

**What is lost by not computing the full posterior.**

- **Error bars.** The posterior says how uncertain any output is (for
  example, revenue from a reform) because of data construction. One file
  gives a point estimate with no measure of how much it would move under
  other plausible files. For the weights-only problem a cheap approximation
  is available: the curvature of the objective at the solution (a "Laplace
  approximation") gives an approximate posterior covariance of the weights.
- **Imputation uncertainty.** Multiple imputation's between-draw variance is
  discarded. A file built from one imputation understates uncertainty in
  anything that depends on imputed variables.
- **Averaging.** When the posterior is lopsided, its mean and its mode
  differ. With a strong prior and many records this is minor for weights; it
  matters more for value transformations.

A middle course that fits the brief's decision to produce one file: publish
the single best file, and alongside it a small number of alternative files
from different priors (different imputation models, different $\alpha$) as
a sensitivity check.

## 7. Identification

Each part of the joint distribution is pinned by data, by the prior, or by
both. Roughly:

| Part of the distribution | 2015 | 2016–2023 | 2024 onward |
|---|---|---|---|
| Joint distribution of PUF variables among filers | PUF microdata (blurred at the top) | Margins and two-way cells from SOI tables; joint shape from prior | Prior (aged), plus CBO aggregates |
| Filer counts by status, age group, AGI bin | PUF | SOI tables, tight | SSA/CBO demographics; prior for income composition |
| Imputed variables (wealth, consumption, tips) given PUF variables | Survey models only | Survey models; a few aggregates (Financial Accounts, NIPA) | Prior |
| Underlying deductible expenses below the itemization threshold | Partly PUF (more itemized pre-2018) | Prior only after 2017 | Prior |
| Non-filers | CPS ASEC pool, calibrated to administrative residuals | Same, per year to 2023 | Prior (grown by band) |

A cleaner statement: with weights only and the relative-entropy prior, the
targets move the file only along the directions spanned by the target
contributions $a_{\cdot k}$. Anything orthogonal to them stays at the prior.

That can be turned into a number for any output. Let $f_i$ be record $i$'s
contribution to a quantity of interest (its tax under a reform, say). Under
a linear approximation around the prior,

$$
\frac{\operatorname{Var}_{\text{posterior}}(F)}{\operatorname{Var}_{\text{prior}}(F)}
\;\approx\; 1 - R^2_f ,
$$

where $F=\sum_i w_i f_i$ and $R^2_f$ is the R-squared from a weighted
regression of $f_i$ on the target contributions $a_{i1},\dots,a_{iK}$
(with each target's contribution discounted by its noise).

In words: a quantity is pinned by the targets to the extent that each
record's contribution to it can be predicted from its contributions to the
targets. Revenue from a reform that changes rates by AGI bin is nearly
pinned if AGI-bin totals are targeted. Revenue from a reform that taxes
wealth among the top 0.1 percent is not pinned by any IRS target and
depends almost entirely on the prior.

How to report it: next to each headline output, a "data-determined share"
($R^2_f$) and the change in the output when the prior is swapped for an
alternative. The first says how much the targets constrain the output; the
second says how much the unconstrained part matters. Both are cheap.

Two limits: this measures only uncertainty about weights. It does not
capture error in the prior's values themselves (a biased imputation model
remains biased however the weights are set), and it relies on a linear
approximation that is accurate only when the targets move the file modestly.

## 8. Computation

### Size

- **Records.** About 208,000 filer records (207,692 in the cached 2017
  solution), plus the non-filer pool records in the union base; I did not
  count the latter. With several imputed variants per record, one to a few
  million rows.
- **Variables.** About 190 (`config/variable_guide/baseline.csv` has 190
  data rows).
- **Years.** 2015 to 2097: 83.
- **Targets.** 188 rows today for one year. A full use of Publication 1304
  (AGI bins × filing status × line items, counts and amounts) and the other
  sources plausibly gives several thousand per historical year, and a few
  dozen per projected year.

Memory: one year at 1 million rows × 190 variables in 32-bit floats is about
0.76 GB; all 83 years held at once would be about 63 GB. Years should be
processed in sequence or derived from a shared base on the fly, as
`materialize.R` does now.

### Convexity under each choice

- **(a) Weights only:** convex with any of the distances in section 1 and
  squared-miss target penalties. A strictly convex distance gives a unique
  answer.
- **(b) Weights and value transformations:** not convex jointly; convex in
  each block when the other is held fixed if the transformation enters target
  totals linearly. Cell and threshold targets add discontinuities.
- **(c) Generating records:** fitting a generative model is a nonconvex
  machine-learning problem; calibrating its output afterwards by weights is
  convex again.

### The dual for entropy-type problems

For the weights-only problem, the solution is determined by the $K$
multipliers. They solve

$$
\max_{\lambda\in\mathbb{R}^K}\;\; \sum_k \lambda_k y_k
\;-\; \alpha \sum_i w^0_i \exp\!\Big(\tfrac1\alpha\sum_k a_{ik}\lambda_k\Big)
\;-\; \tfrac12 \sum_k \sigma_k^2 \lambda_k^2 .
$$

In words: a smooth function of one number per target, whose slope with
respect to $\lambda_k$ is target $k$'s remaining miss. Hard constraints are
targets with $\sigma_k = 0$. A noisy target's multiplier is held toward zero
by the last term — a ridge penalty, which is what the wealth tilt adds as
`ridge_eta`.

This function is concave, so there is one maximum and standard methods
(Newton's method, or L-BFGS, a memory-light quasi-Newton method) find it
reliably. Each evaluation is: compute $\eta_i = \sum_k a_{ik}\lambda_k$ for
every record (one sparse matrix–vector product), exponentiate, and add up
by target (a second product). On a GPU with a million records and several
thousand targets each step takes milliseconds; the whole solve takes seconds.
The target contributions need not be stored as a matrix: each target is a
sum over a cell, which a GPU computes by grouping records by cell id.

One note on the existing wealth tilt: as I read `tilt_solver.R`, it
minimizes the squared scaled misses of the tilted totals plus a ridge on
$\lambda$, using the tilted form of the solution. That agrees with the dual
above when the targets are met exactly, but it is a nonlinear least-squares
problem in $\lambda$ rather than the concave dual, so it is not guaranteed to
have a single optimum. Using the dual form would make it convex at no cost.

### A GPU gradient method for the general problem

For (b), and for the joint all-years problem, write the whole objective as
one differentiable function and let automatic differentiation (JAX computes
gradients of any program built from its operations) supply gradients:

1. Parameters: log adjustment ratios $u_{it}$ for weights
   ($w_{it} = w^0_{it}e^{u_{it}}$, which keeps weights positive), and the
   knot values of each $\phi_{v,t}$.
2. Forward pass: transform values, compute derived quantities (AGI,
   itemization switch with a smooth step), compute each target by grouped
   sums, compute the objective (relative entropy in $u$, transport cost in
   $\phi$, squared standardized misses).
3. Backward pass: gradients with respect to all parameters.
4. Update with L-BFGS or Adam (a first-order method with per-parameter step
   sizes, standard in machine learning).

The joint problem has about 1 million × 83 ≈ 80 million weight parameters
and perhaps 300,000 transformation parameters — small by GPU standards. A
practical scheme is to alternate: dual solve for weights (convex, fast), then
gradient steps for transformations, then recompute cell memberships.

PolicyEngine's `microcalibrate` package (README checked 2026-10-07) is a
working precedent for gradient-based weight calibration in PyTorch. It
regularizes with L0 sparsity and dropout; the README does not describe a
distance-to-prior term, so its answers are not "closest to the prior" in the
sense used here.

## 9. Failure modes

**Contradictory targets.** SOI, BEA and Financial Accounts figures differ by
definition, not only by error. With soft targets the estimate splits the
difference in proportion to the stated precisions, which is right only if
the precisions include the definitional gap. Symptoms: two large multipliers
of opposite sign on overlapping targets; standardized misses that stay
large. Fix by reconciling definitions before targeting, or by widening
$\sigma_k$ to cover the gap.

**Infeasible hard constraints.** Any set of equality constraints may have no
nonnegative solution. Keep the hard set to identities that the prior already
satisfies or that have dedicated free parameters (the non-filer weights for
the population partition).

**Extreme weights.** Large multipliers produce ratios like $e^{5}\approx 150$.
Monitor the effective sample size per cell; bound ratios by a bounded
divergence; or let the targets give way (larger $\alpha$). Persistent extreme
weights in one cell mean the decision variables are too narrow there, and
the remedy is (b) or (c), not tighter tuning.

**Overfitting noisy targets.** If $\sigma_k$ is understated, the file chases
noise in the published numbers. Check: with correctly stated errors, the sum
of squared standardized misses should be of the order of the number of
targets minus the number effectively fitted; a value far below suggests the
errors were overstated, far above that they were understated or the targets
conflict. Holding out a random tenth of targets and predicting them is the
direct test.

**Too few records in a cell.** A target on a cell with five records forces
those five to carry the cell. The prior strength helps only if it is set per
cell. Otherwise split records (c) or merge the cell into a coarser target.

**Double counting.** Duplicated target rows, nested targets with independent
errors, and numbers applied both inside an imputation and as targets all
overstate the information in the data.

**Composition standing in for growth.** In projection years, aggregate
targets met by reweighting rather than by the aging step (section 4).

**Zero prior weight.** Under relative entropy a record with zero prior
weight stays at zero. In the union-base design that is intended; elsewhere it
silently removes records from consideration.

## 10. Candidate formulations, ranked

### A. Per-year penalized entropy calibration with a small value layer (recommended first)

- **Decision variables.** Weights on a shared set of records; a rank-dependent
  multiplier per dollar variable per year with five to ten knots (generalizing
  `grow_with` from one number to a shape); five to twenty imputed variants per
  record for imputed variables, so their conditionals can tilt without moving
  the IRS variables.
- **Objective.** Relative entropy of weights from the aged prior with a
  stated prior strength; a transport cost on the multipliers measured from
  the prior growth path, with a smoothness penalty across knots; squared
  standardized target misses.
- **Hard constraints.** Identities by construction (split variables as shares
  of parents; nonnegativity by multiplicative transforms); the population
  partition as exact constraints.
- **Time.** Forward chain: year $t$'s prior is year $t-1$'s answer aged.
- **Censored targets.** A per-year reporting calculator for itemization,
  recomputed between alternating steps.
- **Solve.** Alternate the concave dual for weights with gradient steps on the
  multipliers.
- **To build.** A machine-readable target registry (cell definition, source,
  value, $\sigma_k$, reporting function); a JAX target evaluator using
  grouped sums; the dual solver; the transformation layer; diagnostics
  (effective sample size per cell, standardized misses, multipliers,
  data-determined shares). A prototype covering filers 2015–2023 against
  Publication 1304 tables fits in two weeks. Replacing the linear program in
  `reweight.R` with the dual solver alone is a day or two and is useful by
  itself.

### B. One joint problem across all years

- Same pieces as A, plus a relative-entropy link between consecutive years'
  weights and a penalty on year-to-year changes in the multipliers; solved as
  one objective with automatic differentiation.
- Gains: later data inform earlier years; smooth paths by construction; one
  prior strength and one set of link strengths govern all years.
- Costs: nonconvex, harder to diagnose (a bad target in 2021 moves 2017);
  more memory; link strengths are extra tuning numbers.
- To build: A first, then the link terms and a joint optimizer. Sensible only
  after A's diagnostics are trusted.

### C. A generative model as the prior, calibrated by tilting

- Fit a joint model of the full characteristic vector (for example a
  sequence of conditional models, one variable at a time, or a neural density
  model) to the PUF and surveys; sample a large synthetic file; calibrate it
  with A or B.
- Gains: the support is no longer limited to 2015 records; thin cells can be
  filled; weight degeneracy can be removed by sampling more records.
- Costs: the model can invent joint structure the data do not support,
  especially at the top; validation of a 190-variable joint model is a
  project in itself; disclosure-protection artefacts in the PUF are learned
  along with everything else.
- To build: months, not weeks. Better confined to the known gaps — the
  non-filer pool, the top tail, and splitting heavy records — inside A or B.

## Sources

Verified means I opened the page and saw the item.

- Csiszár, I. (1975), "I-Divergence Geometry of Probability Distributions
  and Minimization Problems," *Annals of Probability* 3(1), 146–158. Shows
  that minimum-relative-entropy problems are projections onto convex sets and
  characterizes the solution. Verified (Project Euclid abstract page):
  https://projecteuclid.org/euclid.aop/1176996454
- PolicyEngine, `microcalibrate` (gradient-based survey weight calibration in
  PyTorch with L0 and dropout regularization). Verified (GitHub README):
  https://github.com/PolicyEngine/microcalibrate
- Deville, J.-C. and C.-E. Särndal (1992), "Calibration Estimators in Survey
  Sampling," *Journal of the American Statistical Association* 87, 376–382.
  The standard treatment of calibration with a family of distance functions,
  including the bounded "logit" form. Citation details seen in search
  listings; the publisher page refused access, so not opened:
  https://doi.org/10.1080/01621459.1992.10475217
- Hainmueller, J. (2012), "Entropy Balancing for Causal Effects,"
  *Political Analysis* 20, 25–46, doi:10.1093/pan/mpr025. Entropy
  reweighting with a dual in as many unknowns as moment conditions. Citation
  seen in search listings; the PDF at the author's site could not be read
  by my tools: https://www.mit.edu/~jhainm/Paper/eb.pdf
- Schennach, S. M. (2005), "Bayesian Exponentially Tilted Empirical
  Likelihood," *Biometrika* 92(1), 31–46, doi:10.1093/biomet/92.1.31.
  Derives exponential tilting from a nonparametric Bayesian prior. Citation
  seen in search listings, paper not opened:
  https://ideas.repec.org/a/oup/biomet/v92y2005i1p31-46.html

Believed but not verified for this note: that Ireland and Kullback (1968,
*Biometrika*) showed that iterative proportional fitting (raking) minimizes
relative entropy subject to given margins. I searched and did not find the
article page; a search turned up a related 1969 *JASA* paper by Ireland, Ku
and Kullback instead. Also believed but not checked: that SOI publishes
coefficients of variation for Publication 1304 estimates. Sanov's theorem is
a textbook result in large-deviations theory; I did not look up a specific
source.

Repository facts were checked against the code on 2026-10-07: the linear
program's objective and box (`src/reweight.R`, called with `e = 0.66` in
`src/create_2017_puf.R`), the duplicated target rows
(`config/target_info/baseline.csv`), the projection by single growth factors
and extensive factors (`src/project_puf.R`), the union-base zero-weight
records (`src/project_puf.R`, `compute_weights_for_year`), and the tilt
objective (`src/imputations/tilt_solver.R`, header and `solve_tilt_block`).
