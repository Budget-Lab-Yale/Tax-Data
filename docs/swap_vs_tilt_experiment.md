# Stage 3 Step A: swap vs tilt — experiment results

**Date:** 2026-04-25 / 2026-04-26 (overnight + morning follow-up)
**Branch:** `donor-year-wealth`
**SLURM jobs:** 9523788 (mns20 head-to-head), 9523789 (mns50), 9524323 (variant sweep), 9532223 (mns50 + warm-start follow-up)
**Saved artifacts:**
- `2026042315/baseline/swap_vs_tilt_results.rds`
- `2026042315/baseline/swap_vs_tilt_results_mns50.rds`
- `2026042315/baseline/swap_variants_results.rds`
- `2026042315/baseline/mns50_warmstart_results.rds`

## TL;DR

The swap idea **works**, but only after one specific knob is turned: **min.node.size = 50** (bigger DRF leaves, more donor variety). At default leaves (mns20) swap loses to tilt on the headline ±5% count metric (37.5% vs 42.7%) but already wins on top shares and joint Y consistency. At mns50 swap **beats tilt on every count-match tolerance** while keeping the joint-structure advantage and running 10× faster.

The variant sweep at mns20 isolates exactly *why* swap struggles there: 14 of 16 buckets get stuck because the leaf donor pool can't reach the count target, not because the optimizer is finding bad local minima. Multi-restart, annealing, longer runs, smarter proposals — none move the needle. Only **warm-starting from tilt** delivers a meaningful within-mns20 improvement (44.8% within ±5%, 5 buckets converge), and that's mostly because it lands closer to the donor-feasible frontier before swap takes over.

**The combination of both effective tweaks (mns50 + warm-start from tilt_mns50) is the count-match champion**: 52.1% within ±5%, 28.1% within ±1%, 7 of 16 buckets converge with 0 cap-outs, in 16 seconds total Step A time. It also runs faster than uniform-init swap because tilt landed close. The cost is a top-share regression vs uniform-init swap (+1.87pp top-1 vs +1.35pp): warm-start inherits tilt's slightly inflated top-shares, and the swap solver only optimizes count match so doesn't undo them.

**Recommendation:** the choice is a Pareto frontier with two clean points.

- For **count-match-first** production (matches the original Stage 3 design intent): use `stage3_method='swap', min_node_size=50, init_donors_override=tilt_mns50`. 52.1% ±5%, but +1.87pp top-1.
- For **top-shares-first** production: use `stage3_method='swap', min_node_size=50` (uniform init). 45.8% ±5%, but +1.35pp top-1 — tightest top-share preservation we have.

Both arms are clear improvements over the current production `tilt_mns20`. A natural follow-up that isn't yet tested: warm-start *only* the stuck buckets and leave the converging ones alone — that should recover both top shares (from uniform init's wins on easy buckets) and count match (from warm start's wins on hard ones).

## Validation table

Headline metric is "% of (cell × age × cat) buckets within ±5% count error" (96 → 75 kept after QC). Top-share Δ is in pp vs SCF actual (SCF top1 = 0.384, top0.1 = 0.162). Joint corr L1 is the entrywise L1 distance between the 5×5 (equities, bonds, homes, other, debt) correlation matrix and SCF's. Step A runtime is the per-cell solver only, excluding forest training and Step B rescale.

| Metric                      | Tilt mns20 (prod) | Tilt mns50 | Swap mns20 | Swap mns50 | **Swap mns50+ws** | Plan target |
|-----------------------------|-------------------|------------|------------|------------|---------------|-------------|
| % within ±1% count          | 15.6%             | 19.8%      | 17.7%      | 24.0%      | **28.1%**     | —           |
| % within ±5% count          | 42.7%             | 47.9%      | 37.5%      | 45.8%      | **52.1%**     | ≥ 80%       |
| % within ±10% count         | 57.3%             | 64.6%      | 56.2%      | 66.7%      | **68.8%**     | ≥ 90%       |
| % within ±25% count         | 80.2%             | 79.2%      | 82.3%      | 82.3%      | **82.3%**     | —           |
| % within ±50% count         | 89.6%             | 92.7%      | 88.5%      | 90.6%      | 91.7%         | —           |
| Aggregate (post-Step-B)     | exact             | exact      | exact      | exact      | exact         | exact       |
| Top-1 NW share Δ vs SCF     | +1.63 pp          | +1.98      | +1.13      | **+1.35**  | +1.87         | ≤ +1.5 pp   |
| Top-0.1 NW share Δ vs SCF   | +1.18 pp          | +0.72      | +1.25      | **+0.40**  | +0.83         | ≤ +0.8 pp   |
| Gini Δ vs SCF               | +1.48 pp          | +1.49      | +1.36      | **+1.28**  | +1.44         | —           |
| Joint corr 5×5 L1 dist      | 1.156             | 1.173      | 0.935      | **1.051**  | 1.131         | ≤ tilt      |
| Buckets converged / stuck   | n/a               | n/a        | 1 / 14     | 4 / 11     | **7 / 9**     | ≤ 5 stuck   |
| Step A runtime              | 293 s             | 271 s      | 29 s       | 43 s       | **16 s**      | —           |

The first plan target (≥ 80% within ±5%) is still missed by every arm. That target was set under the optimistic theory that the donor pool would generally support the SCF count distribution if the optimizer were strong enough; the data show that's not the case at the production target granularity (96 buckets after the 99.9 split). The honest read is that *no* solver can hit ±5% on the buckets where the leaf pool genuinely doesn't contain enough donors of the right type — that's the senior top-tail cluster which persists across every variant including mns50 and warm-start.

**The Pareto frontier between count match and top shares is real and visible above.** No single arm dominates: `swap_mns50_ws` is the count-match leader, `swap_mns50` (uniform init) is the top-shares + joint-structure leader. Both beat current production `tilt_mns20` overall.

## Variant sweep — does the swap idea have headroom at mns20?

Eight algorithmic variants, run on the same forest cache (mns20). Each row is a different hypothesis about what was holding swap back. The "Δ ±5%" column is the change vs `swap_default` (positive = better than default).

| Variant            | ±1%   | ±5%   | ±10%  | ±25% | Δ ±5% | Conv | Stuck | Cap | Hypothesis tested |
|--------------------|-------|-------|-------|------|-------|------|-------|-----|-------------------|
| tilt (baseline)    | 15.6% | 42.7% | 57.3% | 80.2%|  ―    |   ―  |   ―   |  ― | legacy comparison |
| swap_default       | 17.7% | 37.5% | 56.2% | 82.3%| 0     |  1   | 14    | 1  | reproduce Run B   |
| **swap_warmstart** | 19.8% | **44.8%** | 55.2% | 82.3%| **+7.3pp** | **5**  | 11    | 0  | uniform init was the wrong starting point |
| swap_anneal        | 17.7% | 36.5% | 56.2% | 82.3%| −1.0pp |  1  | 11    | 4  | local-minima escape (*didn't help*) |
| swap_l2            | 19.8% | 43.8% | **59.4%** | 82.3%| **+6.3pp** |  5  | 9 | 2  | smoother L2 surface (*helped*) |
| swap_guided        | 15.6% | 39.6% | **60.4%** | **83.3%**| +2.1pp | 4 | 12  | 0  | bias proposals toward over/under-target records |
| swap_long          | 19.8% | 36.5% | 56.2% | 82.3%| −1.0pp |  1  | 15    | 0  | "just give up too soon" — *no* |
| swap_multistart    | 17.7% | 37.5% | 55.2% | 82.3%| 0     |  1   | 13    | 2  | local minima — *no, gold-standard test* |
| swap_combo (W+G+A) | 16.7% | 41.7% | 57.3% | 82.3%| +4.2pp |  5  | 11    | 0  | stack the winners |

What this tells us, mns20:

- **`swap_long` and `swap_multistart` are both essentially flat** vs default. That's the single most diagnostic result in this whole experiment. If the failure were "stuck in a local minimum of the L1 objective," multi-restart with 5 independent random seeds would find a better basin in at least some buckets — it doesn't. If the failure were "ran out of iterations," 5M iters and 50k stuck would punch through — they don't. Both fail in essentially the same buckets, with essentially the same residuals. The barrier is structural, not algorithmic.
- **`swap_warmstart` and `swap_l2` are the two real wins** — both push ±5% past tilt's 42.7%, both lift converged-buckets from 1 → 5. They work for different reasons (init position vs surface smoothness) but the gains aren't additive: `swap_combo` (warmstart + guided + anneal) only hits 41.7%, *worse than warmstart alone*. The runtime is also an order of magnitude bigger (689s for combo vs 19s for warmstart) without payoff.
- **The worst per-bucket residuals are essentially constant across all variants.** `pct99.9to100/senior` is at 0.71–0.99 max-rel under every solver; `pct80to90/senior` is at 0.53–0.56; `pct00to20/senior/bonds` is at 1.09–1.35. These are *the same 4–5 buckets every time*, regardless of objective, init, proposals, or iteration budget. Their leaves don't contain enough donors of the right positivity pattern, and no amount of optimizer cleverness summons more.
- **`swap_warmstart` is also the fastest** (18.5 s) — it inherits tilt's already-decent assignment and only fixes the buckets where tilt was wrong. This is essentially free.

So the variant sweep gives a clean answer to the user's question: **the swap algorithm is not the problem at mns20.** The problem is donor-pool feasibility in specific cells. That's an *X-distribution* property of the per-cell forest, not an optimizer hyperparameter.

## What mns50 changes

Tripling `min.node.size` (20 → 50) widens each leaf's donor pool by ≈ 2.5×. That single change:

| Outcome at mns50              | vs mns20 swap | vs tilt mns20 |
|-------------------------------|---------------|---------------|
| ±5% count match               | +8.3 pp       | +3.1 pp       |
| ±10% count match              | +10.5 pp      | +9.4 pp       |
| Top-0.1 share gap to SCF      | shrinks to +0.4 pp | tilt was +1.18 pp |
| Buckets converged             | 1 → 4         | n/a           |
| Joint corr L1 dist            | 0.935 → 1.051 | tilt 1.156    |

Joint correlation slightly degrades at mns50 (0.935 → 1.051) because bigger leaves blur the X-conditional Y distribution. It's still **better than tilt's 1.156**, but it's the one place mns50 trades off against mns20.

## Per-arm worst buckets

Every variant shares the same hard-to-hit cluster:

| Bucket                          | tilt err | swap_default | swap_warmstart | swap_combo | mns50 swap |
|---------------------------------|---------|---------------|----------------|------------|------------|
| pct00to20 / senior / bonds      | 1.350   | 1.119         | 1.350          | 1.350      | (improves) |
| pct99.9to100 / senior / equities| 0.934   | 0.982         | 0.951          | 0.934      | persists   |
| pct99.9to100 / senior / nw      | 0.714   | 0.714         | 0.714          | 0.714      | persists   |
| pct99.9to100 / senior / homes   | 0.703   | 0.704         | 0.703          | 0.703      | persists   |
| pct99.9to100 / senior / debt    | 0.584   | 0.586         | 0.573          | 0.584      | persists   |
| pct80to90  / senior / *         | 0.49–0.53 | unchanged    | unchanged      | 0.479–0.536| improves   |
| pct00to20  / nonsenior / homes  | 0.427   | **0.104**     | 0.108          | 0.426      | (improves) |

`pct99.9to100/senior` is the canonical hard cell — top-0.1% income, age ≥ 65 — only ~1100 SCF rows feed it, and the seniors among those have a wealth profile (high homes/debt-free) that's so tightly concentrated that the leaf-level positivity vectors aren't varied enough to hit SCF's count distribution exactly. No amount of swap-solver tuning fixes this within the existing leaf pool.

The standout *positive* result: `pct00to20/nonsenior/homes`. Tilt overshoots SCF by 43%; swap_default cuts that to 10%, and warm-start preserves the gain (10.8%). This is the kind of bucket where the swap mechanic actually does the right thing — bigger initial overshoot, lots of donor variety in a low-income cell, easy to swap out homeowner donors.

## Joint Y correlation (5×5 to SCF)

```
           | mns20 |   |
pre  − SCF |  0.77 | (uniform draw — random)
tilt − SCF |  1.16 |  ←  baseline
swap − SCF |  0.93 |     mns20 wins by 0.22
swap − SCF |  1.05 |     mns50 (still beats tilt by 0.10)
```

Swap preserves joint Y better than tilt at both leaf sizes. The mechanism is exactly what the design predicted: tilt mixes donors probabilistically per record, blurring the joint structure inherent in any single donor row; swap always assigns one whole donor row, so the SCF cross-category correlations transfer cleanly.

## Recommendations

1. **Production switch worth seriously considering**: `stage3_method='swap', min_node_size=50, init_donors_override=tilt_mns50` (count-match-first) OR uniform-init `swap_mns50` (top-shares-first). Both clearly beat `tilt_mns20`. The choice depends on which dimension Tax-Simulator downstream cares more about — worth checking with the consumer before flipping the default.
2. **Untested follow-up that should give the best of both**: warm-start *only the stuck buckets*, leave the converging ones at uniform init. Implementation is small: run swap_mns50 first, identify stuck buckets, re-run those with tilt-init. Expected to recover swap_mns50's top shares while keeping swap_mns50_ws's count match on the hard cells.
3. **For the senior top-tail (pct80to90/senior, pct99.9to100/senior, pct99.9to100/nonsenior):** bigger leaves only partially resolve these. The pct99.9to100/senior cluster sits at ~0.7–0.9 max-rel under every variant tested — it's a genuine SCF-side thin-sample issue (~570 senior rows in that cell). Three options worth a separate experiment:
   - (a) Coarsen the cell grid back to a single `pct99to100` bucket for seniors (gives up the 99.9 resolution we added but unblocks the targets).
   - (b) Allow donor pool augmentation by *age* within the same income cell: a senior in `pct99.9to100` could draw from `pct99.9to100` non-senior donors when leaf is donor-poor. Trades age-conditional precision for count match.
   - (c) Targeted trim for stuck buckets only: when swap stalls > 25% off, randomly demote some over-target donors. Sacrifices joint consistency on those buckets only.
4. **Hybrid solver as a clean fallback.** Swap on buckets where it converges or comes within 5%; tilt on the rest. Best-of-both, no algorithmic surprise. The dispatch is one `if`.

## What this experiment ruled out

Things this run definitively showed are *not* the cause of swap-mns20's poor count match:

- ❌ Local minima in the L1 objective (multistart × 5 changes nothing).
- ❌ Insufficient iteration budget (5M iters × 50k stuck-tolerance changes nothing).
- ❌ Greedy acceptance plateaus (annealing changes nothing or makes it worse).
- ❌ Bad initialization on its own (warm-start helps a bit, but ceiling is still hit).
- ❌ Wrong objective curvature (L2 helps modestly but caps at the same buckets).

What remains: **the per-cell DRF leaf is the binding feasibility set**, and the only effective lever is widening that set (mns50 helped; mns100 untested but likely adds little since the residual stuck buckets are SCF-side thin-sample issues).

## Files added / modified

- `src/imputations/swap_solver.R` (new) — `solve_swap_bucket` + `solve_swap_bucket_multistart`. Supports `objective ∈ {l1, l2}`, optional annealing (`anneal_T0`, `anneal_iters`), and `proposal_strategy ∈ {uniform, guided}`. Best-seen-state restore so anneal can't degrade results. 7 unit tests pass.
- `src/imputations/wealth.R` — adds `stage3_method`, `min_node_size`, `swap_options`, `n_restarts`, `init_donors_override` arguments to `run_wealth_imputation`. The legacy tilt path is preserved unchanged. `step_a_donors` and `pre_step_a_donors` exposed in the result for warm-start orchestration.
- `src/eda/swap_vs_tilt_compare.R` (new) — head-to-head harness, optional CLI arg for `min_node_size`.
- `src/eda/swap_variants_compare.R` (new) — sweep over 8 swap variants vs tilt.
- `src/eda/mns50_warmstart_test.R` (new) — focused 4-arm comparison (tilt_mns20, tilt_mns50, swap_mns50, swap_mns50_ws).
- `slurm_swap_compare.sh`, `slurm_swap_compare_mns50.sh`, `slurm_swap_variants.sh`, `slurm_mns50_warmstart.sh` (new) — SLURM wrappers.

The default `stage3_method` is `'tilt'` so existing pipeline runs (`src/main.R`, all other harnesses) are unchanged.

## Out of scope (not changed by this experiment)

- Forbes-400 top-tail patch (separate effort).
- Stage 1 Sabelhaus alignment / 18-24 dependents.
- TPC-style Step A → Step B → recheck iteration loop.
- Forest architecture changes beyond the `min_node_size` knob (mtry, num.features, num.trees).
- Step B intensive rescale (still hits aggregate exactly under both methods).
