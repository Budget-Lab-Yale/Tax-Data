---
title: "Test 3, piece D — method 3: tuning the network's weights"
role: report
workstream: rebuild
status: current
updated: 2026-10-09
sot: research/rebuild/tests/posttrain/SPEC.md
supersedes: []
superseded_by: null
---

# Method 3: tuning the network's weights

Contract: `SPEC.md`, section D, and the contract changes of 20:30, 20:45
and 20:55 (arm P first; the full model; data version 2). Code:
`d_tune.py` (training, fresh draws, evaluation, residual calibration),
`d_direct.py` (variant 2's objective), `d_report.py` (report.json and
curves), `d_train.sh` / `d_finish.sh` (the two Slurm steps of a run),
`d_full_chain.sh` / `d_wait.sbatch` (the queued chains), and the code tests
`d_dev*.py` / `d_dev*_run.sh`. `$P` is
`/nfs/roberts/scratch/pi_nrs36/jar335/posttrain`.

## Where things stand (9 October, 23:05)

- **No final method-3 result yet.** The runs that count are on the full
  model's arm P (PUF training records, wealth completed, targets = SCF
  wealth tables carried to the PUF as shares within income group, age,
  married and dependents: target set `wcore`, called P_core in SPEC). They
  are queued or running as Slurm jobs and write their own `report.json`;
  the table below is to be filled from those files.
- **Variant 2 (direct target loss with a KL penalty) has trained** on both
  data versions, P_core, β = 0.1: jobs 28906187 (`full_v2`, the main
  model) and 28906192 (`full`, v1), 80 minutes each. Each is followed
  automatically by 32 fresh draws per record, the residual
  calibration and the report (jobs 28906189 and 28906194).
- **Variant 1 (weighted distillation) is training on `full` (v1)**, job
  28918406 (finish 28918407), on B's P_core weights. For `full_v2` it waits
  for A's v2 reference draws and B's v2 weights. Two waiter jobs (28914943 for `full`, 28914944 for `full_v2`)
  submit it when B's P_core weights and A's reference draws exist, and
  variant 1 on P_full (`wfull`) if B's P_full weights appear.
- The current (pilot) model was used only to test the code, as the 20:45
  entry asks, plus one short variant-1 run on B's SCF-arm weights (below)
  to check that distillation behaves on real calibrated weights.

### Numbers on the full model so far (P_core)

The share of PUF population weight that sits in the wrong coarse wealth bin
(misplaced share, defined below), from the network's own fresh draws (one
per record, before any residual calibration), and the KL divergence of the
tuned network from the reference:

| Run | Reference network | Tuned network | KL from reference (nats per unit weight) | Status at 23:05 |
|---|---|---|---|---|
| Variant 2, `full_v2` (job 28906187) | 4.10% | 2.25% after 501 steps (80 minutes) | 0.167 | training done; finish job 28906189 queued |
| Variant 2, `full` v1 (job 28906192) | 4.16% | 2.21% after 548 steps (80 minutes) | 0.208 | training done; finish job 28906194 queued |
| Variant 1, `full` v1 (job 28918406) | 4.16% | (training) | | finish job 28918407 queued |
| Variant 1, `full_v2` | | | | waits for A's and B's v2 files |

For scale: piece B's method 1 on the same targets (`full`, P_core,
32 reference draws per record) moved the draws by KL 1.21 nats per unit
weight, leaving 9.6 effective draws of 32 (`$P/full/b/P_core/report_P.json`).
Variant 2 was still improving at every evaluation when its 80 minutes ran
out, so these are not converged values: halving the misplaced share cost
about 0.17 to 0.21 nats so far, roughly a sixth of what method 1 spends,
but method 1 also hits most cells far more closely. The fresh-draw
fit, residual calibration KL and effective draws will be in each run's
`report.json` after the finish jobs.

Variant 1 on the pilot model's SCF arm with B's real calibrated weights
(5.2 effective draws of 32; job 28918265, a code test): after 6 minutes the
misplaced share of the network's own draws fell from 2.25% to 1.49% at KL
0.24 nats, against B's method-1 KL of 1.93 on those draws.

The squared-error measure that variant 2 minimizes fell faster than the
misplaced share: to 0.41 of the reference's within 50 steps. The squared
error is dominated by large relative errors in small groups (the top income
groups), which are fixed first; the misplaced share weights every cell by
population and moves more slowly. Whoever picks this up should read the
final values from `$P/full_v2/d/P_core/direct/report.json` (fields named in
"Outputs" below).

## What was run

Both variants start from the reference network and change all of its
weights. The model's distribution of a record's 24 wealth variables is, as
everywhere in test 3, the product of its 24 conditional probabilities in
the one fixed generation order, restricted to the codes the decoder can
turn into dollars. On arm P the PUF records are presented with the SCF
source tag and year 2015 (test 2's better choice), allowed codes come from
the 2015 quantile functions, and a record's missing age range stays
unobserved: the network is never asked to draw it (piece A's
`load_reference` keeps every unobserved slot that is not a wealth variable
hidden), and such a record contributes nothing to the age family's cells.

### Variant 1: weighted distillation

Piece B reweights the 32 reference draws of each record so that every
feasible target is hit exactly, staying as close as possible (in KL) to
equal weights. Variant 1 trains the network to reproduce that weighted set
of draws:

- A training example is one draw of one record. Draws are picked with
  probability proportional to B's weight, which in expectation is the same
  as weighting each example's loss by that weight.
- For each example one position k in the generation order is chosen at
  random. The record's observed variables and the draw's first k − 1
  wealth values are shown, the remaining wealth values are hidden, and the
  loss is minus the log-probability the network gives the draw's k-th value
  (among the allowed codes). Averaged over k this is the log-likelihood of
  the whole draw in the fixed order, divided by 24, so the network is
  trained on exactly the conditional probabilities that define its
  distribution.
- 20 per cent of each step's loss is the network's original training task
  (random hiding of observed variables on every training record of the
  dataset, records picked in proportion to population weight), to keep the
  parts of the network that the wealth task does not touch from drifting.
- AdamW, learning rate 5e-5 after 200 warm-up steps, gradient norm clipped
  at 1, a moving average of the weights (decay 0.998) used for every
  evaluation and for the saved checkpoint, bf16 arithmetic for training.

### Variant 2: direct target loss with a KL penalty

The objective is the squared target error of the network's own expected
cell totals, scaled so that the reference's error is 1, plus β times the KL
divergence of the tuned network from the reference (nats per unit of
population weight). β is the KL, in nats, the method would give up to
remove all of the reference's squared target error. Nothing is reweighted;
the network itself moves.

- The squared error is a function of the expected cell totals, which need
  a pass over every record. Every 5 steps the current network completes
  every record once and the expected cell totals are recomputed (each
  step's bin probabilities in place of the drawn bin). Between these
  refreshes the squared error is replaced by its linear approximation,
  which turns it into a cost on each drawn wealth value: the sum over the
  families of the error gradient at the cell that value falls in.
- Each training step completes 256 records 4 times each. For each of the 24
  generation steps, the exact expected cost of that step (target cost plus
  the step's KL from the reference, summed over every possible value) is
  differentiated directly; the effect of the drawn value on later steps is
  handled by the standard policy-gradient term, with the other three draws
  of the same record as the baseline. This is KL-penalized reinforcement
  learning with the target error as the cost, the counterpart of RLHF in
  `rlhf_analogy.md`.
- Learning rate 1e-4, β = 0.1, no mix-in of the original task.

**A failure and its fix.** With refreshes every 25 steps and learning rate
2e-4, the network overshot: within the 25 steps between refreshes the
linear approximation kept pushing weight into the same cells long after
they had reached their targets, and the squared error rose to 37 times the
reference's within 150 steps (pilot model, SCF arm; jobs 28900074 and
28900076, cancelled). With refreshes every 5 steps the error falls steadily
(pilot job 28901195: misplaced share 2.25% → 2.10% in 24 minutes at
learning rate 3e-5, KL 0.002; full model above at 1e-4).

### Stopping, and what is measured

Neither variant stops on its training loss. Every 10 minutes (6 on the
pilot) the moving average of the network completes every record once (twice
on the pilot) in float32, and two numbers are logged:

- **Misplaced share.** The sum over all target cells of |expected cell
  weight − target|, divided by twice the sum of the targets. Each table of
  coarse bins (one group of one family, one wealth variable) sums to the
  same total in the targets and in the draws, so this is the share of
  population weight that sits in the wrong coarse bin, averaged over the
  tables. It is reported from expected cells (less noise) and from the
  drawn cells; on the full model the two agree to about 0.01 percentage points.
- **KL from the reference**, nats per unit of population weight, by the
  chain rule: for each draw, the sum over the 24 steps of the exact KL
  between the tuned and the reference step distributions given the drawn
  earlier values. A second estimate, the mean of log p(tuned) − log
  p(reference) of the drawn values, is logged too; the two agree.

The checkpoint kept is the one with the smallest misplaced share. If no
evaluation beats the reference, the reference itself is kept and reported
as such. Training stops after 80 minutes or after 3 (variant 1) or 4
(variant 2) evaluations without improvement.

Then 32 fresh draws per record come from the kept checkpoint (float32,
seed 31337), with the float32 log-probability of each drawn value under the
tuned and under the reference network. On every test, D's reference
log-probabilities matched piece A's `ref_logprob` on 4,096 draws (largest
difference 3e-6 nats on the SCF arm, 0 on the PUF arm). Piece B's
`calibrate_matrix` then reweights these fresh draws to hit every feasible
target exactly (the residual calibration). Cells whose target is missing
(no SCF group to carry a share from) are left out of every measure and of
the residual calibration, and their count is logged.

## Outputs

Per run, in `$P/<version>/d/<arm>/<variant>/` (version `full` or
`full_v2`, arm `P_core` or `P_full`, variant `distill` or `direct`):

| File | Content |
|---|---|
| `checkpoint.pt` | tuned network (moving average of weights at the best evaluation) |
| `curve.csv`, `curves.png` | training curves: misplaced share (expected and drawn cells), KL, training losses, against minutes |
| `train.json` | settings, every evaluation, stopping reason |
| `draws.npz` | 32 fresh draws per record: `tokens` [N, 32, 24], `logp_new`, `logp_ref` [N, 32], per-variable versions, `kl_chain` |
| `weights_resid.npz`, `report_resid.json` | residual calibration (B's layout and report) |
| `report.json` | summary: `fresh_draws.target_fit_before_calibration_*`, `fresh_draws.kl_from_reference_*`, `residual_calibration.kl_nats_per_unit_weight`, `residual_calibration.effective_draws_weighted_mean`, cell statuses, and B's method-1 report for comparison |

Marker `$P/<version>/d/DONE_<arm>_<variant>`. How the chains run and how to
finish by hand: `STATUS.md`, piece D's entries.

## Code tests on the pilot model (not results)

| Test | What it checked | Outcome |
|---|---|---|
| SCF arm, 3,000 records with their own targets (job 28899058) | both variants end to end | ran; float32 log-probabilities match A's to 3e-6 |
| Fabricated PUF-arm data, 4 families, 44% unknown age (job 28902231) | arm P inputs, records without an age group | ran end to end, both variants |
| A's pilot of the full-model target layout (job 28906174) | a_full target sets, residual calibration with B's current solver | ran end to end |

One observation from the first test is worth keeping in mind for variant 1.
With degenerate calibrated weights (on 3,000 records the method-1 weights
put almost all of each record's weight on one draw: 1.04 effective draws of
8), distillation made the fit worse while the KL grew quickly (misplaced
share 3.7% → 6.0%, KL 3.8 nats in 5 minutes): the network learns the
individual surviving draws rather than a smooth distribution. Variant 1 is
only as good as the effective number of draws in B's weights; the fit-based
stopping rule keeps the reference in that case.

## Unresolved

- Final variant 1 and variant 2 numbers on the full model (queued; see
  above).
- Whether β = 0.1 is the right point on the curve: only one β is queued per
  version. More β values (for example 1 and 0.01) are a one-line change in
  the commands in `STATUS.md`.
- Variant 2 trains slowly (about 7 seconds per step on the full model: each
  step runs the network 72 times, 24 steps × 3 passes), so 80 minutes, less the
  evaluations, is about 500 steps.
