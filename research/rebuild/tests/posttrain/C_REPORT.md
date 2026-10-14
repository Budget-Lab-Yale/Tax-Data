---
title: "Test 3, piece C — method 2: tilt the model's probabilities"
role: report
workstream: rebuild
status: current
updated: 2026-10-09
sot: research/rebuild/tests/posttrain/SPEC.md
supersedes: []
superseded_by: null
---

# Method 2: tilt the model's probabilities

Code: `c_tilt.py` (the tilted model and the fitting), `c_fit.py` (fit,
fresh draws, checks), `c_arm.py` (records, targets and model for the SCF
pilot or arm P of the full model), `c_post.py` (residual calibration with
B's `calibrate()`, comparison with B, KL by income group), `c_resid.py`
(C's fallback residual calibration), `c_dev_toy.py` (a fabricated test
with a known answer), `c_chain.sh` (waits for markers, then submits a job),
`c_gpu.sbatch`, `c_py.sbatch`. Outputs in
`/nfs/roberts/scratch/pi_nrs36/jar335/posttrain/c/` (SCF pilot) and
`.../posttrain/<dataset>/c/<arm>/` (full model).

## What the method is

The reference network draws a record's 24 wealth variables one at a time in
the fixed order. At each step it gives a probability for every possible
token (amount band) of that variable, given the record's observed incomes
and demographics and the wealth variables already drawn.

The tilted model is the same network, with one change at each step. When
it draws variable v for a record, it adds a number to the log-probability
of every token in coarse bin b, and then rescales the probabilities so that
they again sum to one. The number added is the sum, over the target
families (11 in the SCF pilot),
of one multiplier per target cell: the cell for the record's wave, its
group in that family, variable v and bin b. So there is one multiplier per
target cell (30,624 in the SCF pilot), in the same numbering as A's cells and B's
multipliers. The added number is the same for every token in a bin, so
within a coarse bin the network's own split across fine amount bands is
unchanged. Tokens the decoder cannot turn into dollars for the record's
wave stay at probability zero, as in the reference. Bins that fall in a
zero cell for any of the record's groups get probability zero (a zero cell
is a cell no true record falls in).

Draws from the tilted model are unweighted: every record keeps its own
population weight, split equally across its K draws.

## How the multipliers are fitted

**The key fact.** The multipliers for variable v enter only the step that
draws v. They do not change anything drawn before v. So the variables can
be fitted one at a time, in the fill order, in a single pass:

1. All earlier variables of every draw have already been drawn from the
   tilted model with their final multipliers.
2. The reference network gives each draw's probabilities for variable v,
   computed in float32 with the allowed-code restriction. These are summed
   within each coarse bin.
3. The multipliers for v are solved so that the **expected** cell totals for
   v equal the targets exactly. The expected total of a cell is the sum over
   records in the cell's group of (record weight / K) times the tilted
   step probability of the bin, summed over the record's draws. It uses the
   step probabilities, not the drawn tokens, which removes the noise of
   the final draw of v itself. The earlier variables are drawn, so their
   noise remains.
4. Variable v is then drawn from the tilted step, and the pass moves to the
   next variable.

Because nothing fitted later changes anything drawn earlier, there is no
need to iterate the whole pass. (The SPEC suggested an outer iteration with
fixed random numbers. The pass above makes it unnecessary: at the end, every
variable's multipliers are fitted on draws whose earlier variables came
from the final tilted model.)

**The problem solved at each step.** Given the drawn earlier variables, the
tilt that hits the step's targets with the smallest change in the step
distributions (smallest KL divergence from the reference step, summed over
draws with record weights) has exactly the exponential-tilt form above.
Its multipliers minimize a smooth convex function (the dual) whose
gradient is expected totals minus targets. The unknowns for one variable
are the cells of that variable with a positive target, about 1,300 to
1,400 (blocks of wave, family and group, times bins). I solve it by
Newton's method with the exact Hessian (the matrix of second derivatives,
formed from each record's covariance of bin probabilities). Three
safeguards:

- **Redundant directions.** Adding the same number to every bin of one
  group's cells changes nothing, because the step's probabilities are
  rescaled anyway. Shifting one bin's multipliers up in one family and
  down in another also changes nothing, because every record belongs to
  exactly one group of each family. Levenberg–Marquardt damping (adding a
  small multiple of the Hessian's diagonal before solving) keeps the Newton
  step finite along these directions.
- **Step limit.** No multiplier moves by more than 5 in one Newton step, and
  a step is accepted only if the dual does not increase.
- **Cap.** Multipliers are held within ±40. A bin pushed down by 40 has its
  probability cut by a factor of e^40 (about 2 × 10^17), which is zero for
  every practical purpose.

The stopping rule is that every positive cell of the variable is within
10^-10 of its group's total weight, or 60 Newton steps. If the exact solve
stops short or puts a multiplier at the cap, the variable is refitted from
zero with a small ridge: a penalty on the size of the multipliers, of
strength 0.001, each multiplier scaled by its group's weight. It then
trades KL against the squared cell errors, and each cell's error as a share of its group total is 0.001 times
its multiplier. The report lists which variables needed it. In the SCF
pilot none did; in arm P all did (see below).

**Fixed random numbers.** Every draw uses two fixed uniform random numbers
per step: one picks the bin by inverting the tilted bin probabilities, the
other picks the token inside the bin by inverting the reference's
within-bin probabilities. They depend only on the seed, the variable and the
row, so a rerun with the same seed gives the same multipliers and draws
exactly (checked on the fabricated problem).

**Zero cells.** A bin that falls in a zero cell for any of the record's 11
groups gets probability zero at that step. Its multiplier is not a free
unknown. The record's true bin is never in a zero cell (the record itself
puts weight there), so every record always has at least one possible bin;
the code checks this for every record and variable.

**Infeasible cells (SCF arm).** For the tilted model there are no infeasible cells in
B's sense (no draw reaching a cell). Every bin with an allowed token keeps a
positive probability, and the true records themselves satisfy every
target, so the targets are always attainable at each step. What can
happen is that a target is attainable only in the limit: the combination
of a record's groups forces it to put all its probability in one bin. Then
some multipliers run to the cap, and their cells may stay slightly off.
These are counted and listed in the report (`n_at_cap`, cells not reached).
In arm P the targets do not come from the records, and they can be
mutually inconsistent; that case is described under arm P.

**Two sets of draws.** The fitting pass uses K = 32 draws per record (seed
3101). The fit is exact on those draws by construction, so they cannot show
how well the tilted model hits the targets. A second, fresh set of K = 32
draws (seed 3202) is drawn from the tilted model with the fitted
multipliers. These are the delivered draws (`draws.npz`). Their expected
and realized target errors are the honest measure of fit before the
residual calibration.

**KL from the reference.** Two estimates, in nats per unit of population
weight:

- the sample estimate the SPEC asks for: the weighted mean over draws of
  log p_tilted(draw) − log p_reference(draw), both in float32 from the same
  network evaluation;
- a step-wise estimate with less noise: for each draw, the sum over the 24
  steps of the exact KL divergence of the tilted step from the reference
  step given the draw's earlier variables. It has the same expected value.

**Residual calibration.** B's `calibrate()` on the fresh draws, starting
from equal weights per draw, gives the reweighting that makes them hit every
feasible target exactly. Its KL and effective number of draws measure how
far the tilted model still is from the targets. The weights are in
`weights_resid.npz` for E. While B's solver does not converge, C's
fallback (`c_resid.py`, below) writes `weights_resid_fallback.npz`.

## Tilted model versus B's reweighting

B's method reweights whole draws: draw j of record i gets weight
proportional to exp(sum of the multipliers of all 264 cells the draw
touches), rescaled over the record's draws. That is an exponential tilt of
the reference's joint distribution of all 24 variables, normalized once per
record. The tilted model here normalizes at every step. Even with the same
multipliers the two distributions differ: in the step-wise version, the tilt
on an early variable does not account for how that choice changes the
probabilities of later variables. So the fitted multipliers need not agree
with B's. The comparison is made on the tilt each record actually receives
on each bin of each variable, centred over the record's possible bins
(which removes the redundant directions, where the raw multipliers are
arbitrary in both methods).

## Test on a fabricated problem (`c_dev_toy.py`)

A small hand-made model draws three variables (6, 7 and 5 tokens; 3, 4
and 3 coarse bins) for 300 records, in an order different from the
variables' own order, with an allowed-code restriction that differs by wave.
Records have a wave and a group in each of three families, laid out in
cells as A lays them out (180 cells). Targets are the exact expected cell
totals under a known set of multipliers, computed by enumerating all 210
possible records for each record (no Monte Carlo), with one cell forced to
zero (10 zero cells in all, counting those the allowed codes rule out).

| Draws per record in the fit | 8 | 64 | 512 |
|---|---|---|---|
| Largest error on the fitting draws, share of group total | 7e-15 | 7e-15 | 7e-15 |
| Largest error, exact enumeration under the fitted multipliers | 0.031 | 0.0098 | 0.0032 |
| Mean error, exact enumeration | 0.0029 | 0.0012 | 0.0005 |
| Gap between fitted and true tilt (RMS; true tilt RMS 0.90) | 0.063 | 0.023 | 0.009 |
| KL, exact (true multipliers: 0.682) | 0.688 | 0.683 | 0.683 |
| KL, sample estimate on fresh draws | 0.650 ± 0.033 | 0.680 ± 0.012 | 0.686 ± 0.004 |
| KL, step-wise estimate on fresh draws | 0.688 ± 0.005 | 0.682 ± 0.002 | 0.681 ± 0.001 |

The fit is exact on its own draws; the error that remains under exact
enumeration is Monte Carlo noise and falls by about the square root of 8 for
each eightfold increase in draws. The fitted tilt converges to the true one.
No draw ever falls in a zero cell. The same seed gives identical
multipliers. A second case, with targets made from one drawn "true" record
each (like the real problem, where each record's true values are a single
point), is also hit exactly on the fitting draws (error 6e-16).

## Pilot results: SCF training records, current model

On 9 October at 20:45 the main session made the current model (test 2's
joint model, which knows only 10 PUF-side variables) a pilot for checking
code. The model used for the real answer will cover test 1's 40 PUF
variables plus the 24 wealth variables. The numbers below are for the
current model and the SCF arm: the 42,057 SCF training records (implicate
rows, wealth observed), targeted with their own tables. That is 30,624
cells, of which 3,034 are zero cells.

**What ran.** Job 28899409 on one H200: the fitting pass (32 draws per
record, 6 minutes), then a fresh set of 32 draws per record from the tilted
model (6 minutes). Every one of the 24 variables' fits converged. The
largest remaining error on the fitting draws is 7 × 10^-11 of a group's
total weight, with 9 to 29 Newton steps per variable. No multiplier reached
the cap. The largest multiplier in absolute value is 39 (on a cell of
`credit_cards`), and 36 on an `annuities` cell; in 17 of the 24 variables
the largest is below 8.

**Checks.** The float32 log-probabilities computed during the passes agree
with A's `ref_logprob` exactly for the reference. With the tilt applied
through A's `bias_fn` hook they agree to within 2 × 10^-6. Cell totals
computed by my code agree with A's contribution builder to 10^-13. No draw
fell in a zero cell.

**Target fit before the residual calibration** (fresh draws; error in a
cell's weight as a share of its group's total weight, over the 27,590
positive cells):

| Family | Expected: mean | Expected: largest | Realized: mean | Realized: largest |
|---|---|---|---|---|
| Income group | 0.018% | 0.65% | 0.079% | 1.44% |
| Age | 0.009% | 0.33% | 0.038% | 0.39% |
| Married | 0.005% | 0.13% | 0.020% | 0.16% |
| Dependents | 0.010% | 0.27% | 0.036% | 0.36% |
| Wages group | 0.008% | 0.28% | 0.033% | 0.45% |
| Business income group | 0.026% | 0.88% | 0.094% | 1.26% |
| Interest and dividends group | 0.012% | 0.47% | 0.054% | 0.59% |
| Capital gains group | 0.024% | 1.08% | 0.103% | 1.50% |
| Rent group | 0.025% | 1.52% | 0.094% | 1.69% |
| Pensions and Social Security group | 0.012% | 0.72% | 0.049% | 0.86% |
| Unemployment and other income group | 0.023% | 0.97% | 0.088% | 1.01% |
| All | 0.016% | 1.52% | 0.065% | 1.69% |

"Expected" uses the tilted model's step probabilities for each variable;
"realized" counts the drawn values. Four cells have an expected error
above 1 percent of their group's weight, and 32 have a realized error above
1 percent. For comparison, the reference model before any tilt misses by
0.3 to 1.3 percent on average by family, and by 26 percent in its worst
cell (realized, 32 draws per record; A's `ref_checks.json`). The
realized error of the fresh draws (mean 0.065%) is about the same as the realized error of the fitting draws
themselves (0.061%), where the expected totals are exact by construction.
So almost all of the realized error is the noise of drawing 32 values per
record, not a miss by the tilted model.

**Size of the move.** KL divergence of the tilted model from the reference:
1.313 nats per unit of population weight (sample estimate 1.313 ± 0.002;
step-wise estimate 1.313 ± 0.0004). It is spread fairly evenly over the 24
variables, from 0.04 (trusts) to 0.08 (cash) nats each. It is very uneven
across records (counted equally, not weighted): the median record's KL is
0.99 nats, the 90th percentile 5.9 and the 99th percentile 12.7. A KL of 1.3 nats means that, on average over
the tilted model's draws, the log of (tilted probability / reference
probability) of the drawn record is 1.3: in the geometric-mean sense the
tilted model gives its draws about e^1.3 ≈ 3.7 times the probability the
reference gives them.

The move is concentrated at the top of the income distribution
(population-weighted mean KL per record, nats, by income group):

| Income group | 2013 | 2016 |
|---|---|---|
| Bottom 50% | 0.82 | 0.70 |
| 50–90% | 1.44 | 1.30 |
| 90–99% | 2.99 | 2.91 |
| 99–99.9% | 5.98 | 4.87 |
| Top 0.1% | 9.16 | 7.82 |

The reference model is furthest from its own training records' wealth
tables among high-income units. That is where the SCF oversamples and
where the tables cut finest (the top bands of each wealth variable).

**Sensitivity to the number of fitting draws.** Fitting on 8 draws per
record instead of 32 gives KL 1.316 and a mean expected error on fresh draws
of 0.026% instead of 0.016% (largest 2.2%). The size of the move does not
depend on the number of fitting draws. Fitting on 64 draws per record
gives KL 1.313 again and a mean expected error of 0.014% (largest 1.2%):

| Draws per record in the fit | 8 | 32 | 64 |
|---|---|---|---|
| KL from the reference (nats per unit of weight) | 1.316 | 1.313 | 1.313 |
| Fresh draws, expected error: mean | 0.026% | 0.016% | 0.014% |
| Fresh draws, expected error: largest | 2.2% | 1.5% | 1.2% |
| Cells with expected error above 1% | 21 | 4 | 5 |

The remaining expected error falls slowly with more fitting draws. Part
of it is the noise in the fresh draws' own earlier variables (32 per
record in every column), which more fitting draws do not reduce.

**Residual calibration and comparison with B.** B's `calibrate()` (code
as of about 20:50) did not converge on these draws (job 28901936, 35
minutes): it set aside 26,979 of the 27,590 positive cells as "joint"
failures and kept 469. Its KL of 0.0002 nats and 32.0 effective draws
therefore do not describe a calibration to the targets. B's own run on the
reference draws (K = 8) shows the same pattern, so this looks like a
solver problem rather than a property of these draws, which are already
within 0.016 percent of the targets on average. The chain reruns B's
function automatically when B marks its reference run done (waiter
28901918). In the meantime `c_resid.py` solves the same reweighting problem
with a simpler solver: L-BFGS (a standard quasi-Newton method) on the same
convex dual, with a small ridge (1e-5) that absorbs combinations of cells
that cannot be met together. Before solving, it sets aside cells that the
draws cannot reach, using two simple screens. One: no draw falls in the
cell. Two: the target exceeds the total weight of the records that have
any draw in the cell. (B's solver applies more screens.)

**Residual calibration of the tilted draws** (`c_resid.py`, job 28914945;
`$P/c/weights_resid_fallback.npz`, `report_resid_fallback.json`):

| | |
|---|---|
| KL of the calibrated weights from equal weights | 0.009 nats per unit of weight |
| Effective draws per record (of 32), weighted mean | 31.45 |
| Share of weight in records with fewer than 4 effective draws | 0.0004% |
| Active cells (27,516): largest error, share of group total | 0.017% |
| Active cells: mean error | 0.0001% |
| Cells set aside: no draw reaches them | 62 |
| Cells set aside: target above what the records can supply | 12 |
| Largest error among all positive cells | 0.10% |

The 74 set-aside cells are mostly in the income-item families (21
capital gains, 12 business income, 9 rent) and the top income groups (16).
So after the tilt, almost no reweighting is needed: the calibration moves
the draws' weights by 0.009 nats, against 1.313 nats for the tilt itself,
and keeps nearly all 32 draws of every record in use.

**Method 1 on the same footing.** For comparison, the same fallback
solver was run on A's reference draws (32 per record, `$P/dev/c/ref_fallback/`,
job 28916552). This is method 1 solved by C's simpler solver, not B's
result. It did not reach the targets: after 4,000 iterations the mean error
is 0.13 percent of the group total and the largest 8.8 percent. Its
multipliers keep growing (largest 8,760), which means some combinations of
cells cannot be met by reweighting these draws. Its screens set aside 288
cells. Where it stopped, the weights are already far from equal:

| | Method 1 (reweight reference draws; not converged) | Method 2 (tilt), then residual calibration |
|---|---|---|
| KL from the reference, nats per unit of weight | 2.62 (at most log 32 = 3.47 per record) | 1.313 (tilt) + 0.009 (calibration) |
| Effective draws per record, of 32 | 2.85 | 31.45 |
| Weight in records with fewer than 4 effective draws | 83% | 0.0004% |
| Largest error in a cell kept in the solve | 8.8% | 0.017% |

The two KL numbers are not the same kind of quantity. Method 1's is the
distance of a reweighting of 32 fixed draws from equal weights. Method 2's
is the distance of a new distribution from the reference, estimated from
its own draws. Reweighting can only choose among each record's 32 draws,
and here it ends up loading most of a record's weight onto two or three of
them; the tilt changes the draws themselves.

**Comparison of the multipliers with method 1.** The comparison in
`c_post.py` uses the tilt each record gets on each bin, centred over the
record's possible bins (the raw multipliers are arbitrary along the
redundant directions in both methods). Against the non-converged fallback
solution above, the centred tilts correlate at 0.29, and method 1's are two
orders of magnitude larger (root mean square 265 against 1.5). This mostly
reflects multipliers running off along directions that cannot be met, so
it is not a fair comparison. The comparison with B's converged multipliers
runs automatically when B's reference run is done (waiter 28901918).

A note for that comparison: B's KL on 32 draws per record cannot exceed
log 32 ≈ 3.47 nats for any record, because reweighting can at most put all
of a record's weight on one of its draws. The tilted model has no such
ceiling, and 10 percent of records need more than 5.9 nats.

## Arm P: completing PUF records' wealth

**Pilot on the current model** (A's `a_full.py` pilot on test 2's
dataset: the current model's PUF training records, wealth drawn with the
SCF source tag at year 2015, target set `wcore`; run `$P/dev/c/pilotP_core/`,
8 draws per record for fitting and 8 fresh). The point of the run is to
check the code before the full model exists; the numbers come from the
model that is being replaced.

- 160,943 PUF training records; 5,423 cells (income group, age, married,
  dependents, each crossed with the 24 wealth variables' coarse bins), 321
  of them zero cells.
- 70,536 records (44 percent) have no age. Their age stays unobserved at
  every step: the model is given it as a hidden slot and never asked to
  fill it (A's reference context draws only the 24 wealth variables). They
  are in no group of the age family, so the age cells constrain only
  records with a known age, as A's targets are defined.
- **The targets cannot all be met at once.** Every one of the 24 variables
  needed the ridge. The targets are SCF shares within groups multiplied by
  the PUF's group weights, so the total weight each family implies for a
  wealth bin differs from family to family whenever the PUF's mix across
  groups differs from the SCF's. No distribution of wealth over the PUF
  records can satisfy all four families exactly.
- With the ridge (1e-3), the fresh draws' expected cell totals miss by
  0.09 percent of the group total on average; the largest error is 2.4 percent and 39 cells are off
  by more than 1 percent (the largest are in `primary_home`,
  `kg_primary_home`, `dc` and `primary_mortgage`). The errors are the same
  on the fitting draws and on fresh draws, so they are the compromise
  among inconsistent targets, not noise.
- KL from the reference: 0.447 nats per unit of weight (records counted
  equally: median 0.69, 90th percentile 3.8). By PUF income group: 0.34 (bottom half), 0.44
  (50–90), 1.03 (90–99), 1.06 (99–99.9), 2.48 (top 0.1 percent). It is
  smaller than in the SCF pilot (1.31 nats), which had 11 families of
  targets instead of 4, on different records.

- Residual calibration of these draws with C's fallback solver, using the
  same ridge (1e-3), because an exact reweighting does not exist (job
  28918617, `$P/dev/c/pilotP_core/report_resid_fallback.json`): KL 0.014
  nats per unit of weight, 7.85 effective draws of 8. The errors barely
  change: mean 0.084 percent of the group total, largest 2.0 percent
  (against 0.09 and 2.4 percent before). Reweighting cannot remove a
  disagreement among the targets themselves. 27 cells were set aside as
  unreachable with 8 draws, 20 of them in the age family.

`c_fit.py` now also reports how far the families' targets disagree (the
spread, across families that cover every record, of the total weight each
implies for a variable's bin); that spread is a floor for every method.

**Full model: results.** The full models' checkpoints and A's targets
existed by 21:22, so the chains ran during the session. Each run: 166,156
PUF training records, 32 draws per record for fitting and 32 fresh,
about 40 minutes per pass on one GPU. 75,749 records have no age; they
stay unobserved and outside the age family. In every run all 24 variables
needed the ridge (the targets disagree across families). The float32
checks against A's functions pass (reference log-probabilities identical;
tilted ones within 2 × 10^-6; cell totals within 10^-10).

| | full, P_core | full_v2, P_core | full, P_full | full_v2, P_full |
|---|---|---|---|---|
| Cells (zero cells) | 5,423 (328) | 5,423 (325) | 14,993 (1,030) | 20,735 (1,907) |
| Disagreement between families: largest, share of all weight | 3.5% | 3.5% | 6.5% | 6.9% |
| Disagreement: mean over variable bins | 0.25% | 0.26% | 0.61% | 0.63% |
| KL from the reference, nats per unit of weight | 0.672 | 0.688 | 1.563 | 2.029 |
| KL per record (counted equally): median / 90th / 99th percentile | 1.04 / 3.8 / 6.6 | 0.97 / 3.8 / 6.6 | 3.1 / 7.3 / 12.8 | 4.0 / 9.6 / 18.6 |
| Fresh draws, expected error: mean, share of group total | 0.081% | 0.091% | 0.140% | 0.139% |
| Fresh draws, expected error: largest | 2.0% | 2.2% | 5.7% | 7.7% |
| Cells off by more than 1% | 23 | 30 | 266 | 333 |

"Disagreement between families" is, for each wealth variable and bin, the
spread between the largest and smallest total weight that the families
covering every record (income group, married, dependents, and in P_full
the income-item families) imply for that bin, as a share of all PUF
weight. It is a floor: no completion of the PUF records can hit all
families' targets better than that. In P_core the largest disagreements
are in `primary_home` (the lowest positive band), `kg_primary_home` and
`dc` (defined-contribution accounts, the zero bin); in P_full, `dc`,
`primary_home` and `equities`.

Adding the income-item families (P_full) more than doubles the move away
from the reference (KL 1.56 to 2.03 against 0.67 to 0.69 for P_core) and
roughly doubles the target disagreement. In P_full the worst-fitting
family is interest income (`x_E00300`: largest error 5.7 percent in
`full`, 7.7 percent in `full_v2`), the item the SPEC flags as measured differently in the two
sources. Versions 1 and 2 of the data give nearly the same P_core results.

Still running at the end of the session (STATUS.md, 23:15): C's fallback
residual calibrations and `c_post.py` (B's `calibrate()`, comparison with
B's multipliers, KL by income group) for these four runs. Their results
land in `report_resid_fallback.json` and `report.json` in
`$P/<dataset>/c/<arm>/`.
