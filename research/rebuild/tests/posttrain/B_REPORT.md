---
title: "Test 3, piece B — method 1, reweighting the reference draws"
role: report
workstream: rebuild
status: draft
updated: 2026-10-09
sot: research/rebuild/tests/posttrain/SPEC.md
supersedes: []
superseded_by: null
---

# Method 1: reweighting the reference draws

## What this method does

The reference model completes each record K times (K draws of its 24 wealth
variables). Method 1 leaves the model alone and gives each draw a weight.
The weights of a record's draws add up to the record's own population
weight, so every table of the observed variables (incomes, demographics) is
unchanged. Among all weightings that hit the wealth targets, it picks the
one closest to equal weights (1/K of the record's weight on each draw),
closeness measured by the KL divergence (relative entropy). The solution has
the exponential-tilt form of `rlhf_analogy.md`: draw j of record i gets
weight W_i x exp(sum of the multipliers of the cells the draw falls in),
rescaled within the record. There is one multiplier per target cell.

Numbers reported for each run:

- **KL, in nats per unit of weight**: the average over records (weighted by
  record weight) of sum_j q_ij log(K q_ij), where q_ij is draw j's share of
  record i's weight. Zero means equal weights. log K is the most it can be
  (all of a record's weight on one draw): 2.08 nats at K = 8, 3.47 at
  K = 32.
- **Effective draws per record**: 1 / sum_j q_ij^2. K when the weights are
  equal, 1 when one draw carries the record. Reported as a weighted mean and
  as the share of weight on records with fewer than 4.
- **Target fit**: for cells in the solve, the largest error relative to the
  cell's target; for all positive cells, the share of cells and of target
  weight that end within 1e-9, 1e-6, 1e-3, 1e-2 and 10% of target.
- **Cells that cannot be hit**, by reason, family and variable.

## Which cells cannot be hit, and how they are found

Before the solve:

- **Zero cells** (no true record there): every draw that falls in one gets
  weight zero. If a record has no draw left, the zero cells of its draw
  that touches the fewest of them are released ("zero_relaxed") and kept in
  the solve with target zero, so the solver keeps as little weight there as
  it can.
- **unreachable**: positive target, no remaining draw falls in the cell.
- **above_reach**: the target is larger than the total weight of all
  records with at least one draw in the cell.
- **below_forced**: the target is smaller than the weight of records whose
  every draw is in the cell.
- **block**: an exact supply-and-demand check on each wave x family x group
  x variable block on its own. A block's bins can all be hit if and only if,
  for every set S of its bins, the records whose draws all fall in S weigh
  no more than the target of S, and the target of S is no more than the
  weight of records with a draw in S. With at most 14 bins all 16,384 sets
  are checked. Where the check fails, bins are released one at a time (the
  bin involved in the largest total shortfall first) until it passes.

During the solve:

- **joint**: a cell whose multiplier passes 30 in absolute value (a tilt of
  e^30, about 10^13) is released: it cannot be hit together with the other
  cells using these draws. Typical case: a record whose every draw has some
  rare value (a top-0.1% bin of some component) in a group where the SCF
  has almost no weight there; the record's weight has to land in one of
  those small cells.
- **missed**: still in the solve but off target by more than 1e-6 of its
  target when the time budget ran out. Not proven impossible.

Released cells are left out of the solve (unconstrained), as SPEC asks. The
set of released cells is not unique: a different order of release could
release different cells. The block check and the joint release are greedy.

## Solver, and why

Three designs were tried on fabricated data with known answers
(`b_calibrate.py --selftest`; 3,000 records, 1,760 cells with exact
duplicates, zero cells, infeasible cells and doctored targets) and on the
SCF arm's own draws:

1. **Newton's method on the multipliers** (conjugate gradients with
   Hessian-vector products on the GPU; the Hessian is never formed). On a
   problem that can be hit exactly with every draw keeping some weight it
   converges in about 10 to 15 iterations to a relative error of 1e-13.
   It fails when the targets cannot all be hit, or can be hit only by
   giving some draws exactly zero weight: the multipliers run off to
   infinity and the steps stall.
2. **Newton along a path of ridge penalties** (each target may be missed by
   a little, the allowance shrunk step by step) and an L1-type penalty on
   misses. Better behaved, but on the SCF arm's real targets it stalled with
   thousands of cells running away at once.
3. **Raking, then Newton** (the default). Raking (iterative proportional
   fitting) takes one family x variable at a time, moves each of its cells'
   multipliers by log(target / achieved), which puts those cells exactly on
   target, and then rescales each record's draws back to the record's
   weight. It never diverges, and a cell that cannot be hit shows up as a
   multiplier drifting steadily, which is what the 'joint' release uses.
   Raking converges slowly near the end, so after each round of 400
   raking sweeps Newton is tried from where raking stopped; close to the
   answer it converges in a few steps. Its result is kept only if it lowers
   both the total and the largest miss (far from the answer its multipliers
   run off). Rounds repeat (at most 20, and 45 minutes of solving) until
   the cells in the solve are within 1e-9.

   The real-data runs below used an intermediate version that tried Newton
   only once every cell was within 0.1% of target; that never happened, so
   those results are raking alone. The final version (above) was settled at
   23:00 after the self-test showed the intermediate one doing worse on
   fabricated data; jobs that started after 22:55 use it.

Self-test (fabricated data, final version):

| Case | Cells hit to 1e-6 | Largest relative error, cells hit | Cells released or impossible | KL (nats) |
|---|---|---|---|---|
| Feasible (true values among the 8 draws) | all 1,568 | 1.1e-12 | 0 | 1.009 |
| Infeasible pieces (no true values, two doctored targets) | 1,042 | 1.3e-12 | 552 (both doctored ones among them) | 1.255 |
| True value as one of 4 draws | 1,422 | 8.9e-12 | 146 | 0.897 |
| True value as one of 2 draws | 90 | 9.3e-7 | 169 released, 1,309 missed | 0.640 |

Record totals hold to 1e-14 relative in every case. The last two rows show
the known weakness: when the only exact solution puts zero weight on most
draws (here: all weight on the true draw), the method releases or misses
cells that are in fact attainable. That degenerate case is far from K = 32
model draws, but some of the 'joint' cells on real data may be of this kind.

## Results, SCF arm (pilot on the current model)

What ran: the reference draws of piece A (42,057 SCF training records,
30,624 cells of which 27,603 have a positive target at K = 32), first K
draws of each record for K = 8, 16, 32 and the first 64 of the K = 128 set.
Each run had a 45-minute solving budget (K = 8 on a CPU node, the others
on one GPU); the K = 16 run stopped earlier, after its 20 rounds of raking
(20 minutes), so it is a little less converged than the others. **None of the runs converged**: the numbers below are
where each run stood when the time ran out, not an exact calibration.

| K | KL (nats per unit of weight; the most possible is log K) | Effective draws per record (weighted mean) | Weight on records with fewer than 4 effective draws | Cells that cannot be hit, found before the solve | Cells released during the solve | Positive cells within 0.1% / 1% of target | Target weight in cells within 0.1% | Sum of all misses, share of all target weight |
|---|---|---|---|---|---|---|---|---|
| 8 | 1.73 (of 2.08) | 1.4 | 98.6% | 1,893 | 10,651 | 6.2% / 34.4% | 32.1% | 1.15% |
| 16 | 1.91 (of 2.77) | 2.5 | 84.5% | 1,028 | 8,346 | 14.1% / 51.9% | 59.9% | 0.57% |
| 32 | 1.93 (of 3.47) | 5.2 | 57.8% | 562 | 5,997 | 30.8% / 71.6% | 80.3% | 0.27% |
| 64 | 1.85 (of 4.16) | 11.8 | 37.3% | 248 | 3,963 | 36.0% / 76.9% | 86.6% | 0.16% |

"Cells that cannot be hit, found before the solve" adds the unreachable,
above_reach, below_forced and block cells (at K = 32: 65, 224, 12 and
261). Hardly any cell reached the 1e-6 tolerance (19 at K = 32); the rest
of the cells still in the solve are 'missed'.

What the table says:

- **The move is large.** At K = 32 the weights carry 1.93 nats of KL per
  unit of weight. That is what concentrating each record's weight on about
  e^(3.47 - 1.93), roughly 5, of its 32 draws would give, and the effective
  number of draws agrees (5.2). Most of each record's draws are being
  switched off. Because the solve had not finished, the KL of an exact
  solution would be at least this large.
- **More draws help the fit but not the size of the move.** From K = 8 to
  K = 64 the cells that cannot be hit at all fall by a factor of 8 (1,893 to
  248), the cells released fall from 10,651 to 3,963, and the total miss
  falls from 1.15% to 0.16% of target weight. The KL stays near 1.8 to 1.9
  nats; the effective draws grow roughly in proportion to K (16% of K at
  32, 18% at 64).
- **Where the misses are (K = 32).** Most cells that cannot be hit are in
  the extreme bins: 1,254 of the 5,997 released cells are a variable's top
  0.1% bin and 1,032 its 99 to 99.9% bin. By family, the income-item
  families and the income group miss most: cells within 0.1% of target are
  19% to 21% for income group, business, capital gains and rent, against
  38% to 52% for age, dependents and married. The largest single error, as
  a share of the group's total, is 24% (capital-gains family) and 23%
  (income group), in small top groups.

Target fit by family at K = 32 (positive cells; the error as a share of the
group total is the error in the cell divided by the weight of all records
in that wave and group):

| Family | Positive cells | Within 0.1% of target | Within 1% | Target weight within 0.1% | Mean error, share of group total | Largest |
|---|---|---|---|---|---|---|
| age | 3,339 | 38.5% | 83.1% | 75.8% | 0.017% | 1.6% |
| dependents | 2,438 | 40.3% | 81.6% | 85.4% | 0.014% | 0.6% |
| income group | 2,785 | 20.8% | 59.7% | 76.8% | 0.49% | 22.7% |
| married | 1,264 | 52.1% | 87.3% | 90.1% | 0.005% | 0.1% |
| business | 2,903 | 20.7% | 61.9% | 77.8% | 0.36% | 17.0% |
| capital gains | 2,832 | 18.8% | 57.1% | 78.7% | 0.50% | 24.3% |
| interest and dividends | 2,326 | 32.8% | 76.1% | 81.6% | 0.077% | 4.8% |
| rent | 2,909 | 19.4% | 58.9% | 69.1% | 0.43% | 16.2% |
| Social Security and pensions | 2,436 | 37.2% | 78.3% | 83.9% | 0.048% | 3.0% |
| unemployment and other | 1,893 | 38.5% | 73.3% | 85.8% | 0.27% | 12.5% |
| wages | 2,465 | 36.9% | 83.0% | 78.7% | 0.015% | 0.6% |

The K = 128 run (job 28910159) was still running when this was written;
`$P/b/report_ref_K128.json` will hold it.

## Results, arm P on the full model (version 1)

What ran: P_core (income group, age, married, dependents; 5,423 cells, 96
per draw) on piece A's K = 32 reference draws for the 166,156 PUF training
records, same 45-minute budget, one GPU (peak memory 24 GB). Not converged.

| Target set | KL (of 3.47) | Effective draws | Weight on records with fewer than 4 | Cannot be hit before the solve | Released during the solve | Positive cells within 0.1% / 1% | Target weight within 0.1% | Sum of all misses, share of target weight |
|---|---|---|---|---|---|---|---|---|
| P_core | 1.21 | 9.6 | 28.7% | 27 (11 unreachable, 2 above reach, 14 block) | 1,211 | 25.1% / 69.7% | 40.8% | 1.62% |

By family (positive cells within 0.1% / 1%; largest error as a share of the
group total): age 27.6% / 81.0%, 10%; dependents 28.2% / 65.3%, 15%; income
group 20.8% / 61.5%, 38%; married 21.9% / 66.5%, 3.7%.

The move is smaller than on the SCF arm (1.21 nats, 9.6 effective draws)
because P_core has 96 cells per draw instead of 264, but the fit is worse:
the PUF targets are SCF shares carried to differently composed PUF groups,
not tables of the same records, so they need not be reachable from the
model's draws at all. P_full ran out of memory on a 32 GB GPU (job
28913729) and was resubmitted on 80 GB or larger GPUs (28919769); it,
the K = 8 and 16 subsets (28913730, 28913731) and all of version 2
(28918412, 28918413, 28916147, 28916148) were queued or running when this
was written. P_full needs about 30 GB or more of GPU memory at N = 166,156
and K = 32; P_core peaked at 24 GB.

## What is unresolved

- **Exactness was not reached.** The spec asks for every feasible target to
  be hit exactly. On fabricated data where an exact answer exists the solver
  reaches 1e-13; on the real targets it did not get there in 45 minutes,
  and the released ('joint') and 'missed' cells are not proven impossible.
  Many target cells hold one to five households, and the exact answer for
  them sits on the boundary (some draws must get exactly zero weight),
  which neither raking nor Newton reaches quickly. Options, for John to
  choose: coarser target cells (merge the top three rank bands, or require
  a minimum number of households per cell); a stated tolerance instead of
  exactness; or a solver built for boundary solutions (for example finding
  the draws that must be zero with a linear program first).
- **The release threshold** (multiplier beyond 30) and the 45-minute budget
  are choices, not derived; results depend on them.
- **D's distillation** uses these weights as given; they are not exact.

## Files

- Code: `b_calibrate.py` (solver, reusable `calibrate` and
  `calibrate_matrix`, CLI, self-test), `b_full.py` (arm P on the full
  model), `b_chain.sh` / `b_wait.sbatch` (waiting chain), `b_gpu.sbatch`,
  `b_cpu.sbatch`.
- Outputs (record level, scratch only): `$P/b/weights_ref.npz`,
  `$P/b/report_ref.json`, `report_ref_K<k>.json`; `$P/<version>/b/P_core/`,
  `P_full/`.
