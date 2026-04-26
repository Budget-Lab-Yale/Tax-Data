# Stage 3 Step A: swap vs tilt experiment

**Status:** results pending — SLURM job 9523306 (`slurm_swap_compare.sh`) running on `2026-04-25`.
This file will be filled in once the run completes; the structure below
mirrors the validation table in the implementation plan
(`~/.claude/plans/nimble-swapping-otter.md`).

## What changed

A new Stage 3 Step A backend (`src/imputations/swap_solver.R`) was wired
into `wealth.R` as a randomized swap-based donor reassignment, behind a
`stage3_method` argument (env: `WEALTH_STAGE3_METHOD`). Default is still
`'tilt'` so no caller's behavior changes silently. A second knob,
`min_node_size` (env: `WEALTH_MIN_NODE_SIZE`), was added to let us
re-test with bigger forest leaves if swap reports too many `stuck`
buckets.

The legacy tilt code (`solve_tilt`, the convex Newton dual) was kept
intact so the comparison harness can run both arms back-to-back on the
same forest cache.

### Files added / modified

- `src/imputations/swap_solver.R` (new) — pure-function `solve_swap_bucket`.
- `src/imputations/wealth.R` — method dispatch + min_node_size knob.
- `src/eda/swap_vs_tilt_compare.R` (new) — comparison harness.
- `slurm_swap_compare.sh` (new) — 45 min day-partition job wrapper.

## Algorithm summary (swap)

For each (cell_income × cell_age) bucket independently:

1. Init: each PUF record gets a uniform-random donor from its DRF leaf
   (the existing `pre_tilt_donors`, no change).
2. Iterate: pick a random movable record, propose a different random
   donor from its leaf, accept iff
   `Σ_k |count_k − target_k| / max(target_k, 1)` strictly decreases.
3. Stop on:
   - **converged** — every category within `tol_rel = 5%`.
   - **stuck** — `n_trials_stuck = 5000` consecutive non-improving
     proposals (leaf donor pool is the binding constraint; not a soft
     fail like tilt's ESS cap).
   - **cap** — `max_iters = 500000` proposals tried.

Per-bucket RNG seeds (`init_seed = 1000 + bucket_idx`) make each bucket
bit-reproducible regardless of caller RNG state. The solver saves and
restores `.Random.seed` on entry/exit so it's safe under parallel
dispatch.

## Validation table

| Metric                                        | Tilt (legacy) | Swap (new) | Plan target | Δ |
|-----------------------------------------------|---------------|------------|-------------|---|
| % of (cell × age × cat) within ±5% count      | _pending_     | _pending_  | ≥ 80%       |   |
| % within ±10% count                           | _pending_     | _pending_  | ≥ 90%       |   |
| % within ±25% count                           | _pending_     | _pending_  | (gut check) |   |
| Aggregate (post-Step-B) match                 | exact         | _pending_  | exact       |   |
| Top-1 NW share vs SCF                         | _pending_     | _pending_  | ≤ +1.5pp    |   |
| Top-0.1 NW share vs SCF                       | _pending_     | _pending_  | ≤ +0.8pp    |   |
| Joint Y corr matrix L1 distance to SCF        | _pending_     | _pending_  | ≤ tilt      |   |
| Buckets STUCK (out of 16)                     | n/a           | _pending_  | ≤ 5         |   |
| Step A solver runtime                         | _pending_     | _pending_  | ≤ tilt + 1m |   |

Baseline numbers from the prior `slurm_target_verify.out`
(2026042315 / tilt only): **42.7% within 5%, 57.3% within 10%, 80.2%
within 25%**, with worst-case overshoot at `pct00to20/senior/bonds`
(+135%) and `pct99.9to100/senior/*` (+58–93%).

## Worst buckets (count error, side-by-side)

_Pending — top 15 buckets by max(tilt_err, swap_err)._

## Top-share comparison

_Pending — top-0.1 / top-1 / top-5 / top-10 / Gini for SCF / pre / tilt / swap._

## Joint Y correlation matrices

_Pending — entrywise (tilt − SCF) and (swap − SCF) heat-tables, plus L1
distances. Confirms whether swap preserves the joint Y structure as
designed (it should by construction — every assignment is a whole donor
row, no Frankenstein records)._

## Per-bucket convergence status (swap)

_Pending — table of (cell × age) → (status, swaps_accepted, total_proposals, final_max_rel)._

## Decisions

- **Run B verdict** (swap vs tilt at `min_node_size = 20`): _pending_.
- **Run C needed?** (swap with `min_node_size = 50`): trigger if Run B
  reports > 5 stuck buckets.
- **Default Step A method**: still `'tilt'` until the user reviews these
  numbers and flips it manually.

## Out of scope (per plan)

- Forbes-400 top-tail patch (separate effort).
- Stage 1 refactor (Sabelhaus alignment, 18-24 dependents).
- TPC-style Step A → Step B → recheck iteration loop.
- Forest architecture changes beyond the `min_node_size` knob.
