# Stage 3 Calibration — Exponential Tilt at Donor Selection

**Design memo, 2026-04-22.**

## TL;DR

For each PUF record, Stage 2 picks a donor uniformly from its DRF leaf. Stage 3 replaces the uniform draw with a donor probability proportional to `exp(λᵀ Y_j)`, where `λ` is a vector of tilt parameters solved per calibration cell to hit target moments (DFA aggregates, SCF conditional moments, top shares, participation rates). This is **exponential tilting of the donor distribution**, equivalently **maximum-entropy calibration**. It generalizes the rank-tilt idea we discussed, handles multi-target calibration natively, is symmetric (up and down), preserves Stage 2's joint Y structure, respects donor support, and adds ~10–60 s to the pipeline.

This memo lays out: theory, implementation, performance, benchmarking, failure modes, and open design questions. Decisions marked **[OPEN]** are the ones we need to discuss before implementation.

---

## 1. The framework

### 1.1 Setup

Stage 2 produces, for each PUF record `i`, a leaf `L_i` in some DRF tree, containing SCF donors `{j : j ∈ L_i}` with 23-dim wealth vectors `Y_j ∈ ℝ²³`. The current draw is

    j* | i  ~  Uniform({j ∈ L_i})

and record `i` inherits `Y_{j*}`.

### 1.2 Proposed generalization

Replace uniform with an exponentially tilted probability over donors within the same leaf:

    P(j | i) = exp(λᵀ f(Y_j)) / Z_i(λ),    Z_i(λ) = Σ_{k ∈ L_i} exp(λᵀ f(Y_k))

where `f : ℝ²³ → ℝ^k` is a vector of *target statistics* (e.g., `f(Y) = Y` itself for mean targets; `f(Y) = 1{Y > 0}` for participation; `f(Y) = Y · 1{Y > p99}` for top-share × total; see §4). `λ ∈ ℝ^k` is solved so that cell-level aggregates match targets `T_c`:

    Σ_{i ∈ c} w_i  Σ_{j ∈ L_i} P(j | i) · f(Y_j)  =  T_c                                  (★)

`λ_c = 0` recovers the current uniform draw. `λ_c ≠ 0` distorts donor selection away from uniform by the minimum amount needed to hit `T_c`.

### 1.3 Why this is the right object

Rank tilt (Beta(α, β) on within-leaf ranks) and "donor-rank weighting" (`p_ij ∝ rank_j^γ`) are both **special cases** of this with scalar `f` and particular parametric shapes. Exponential tilting is the superset because:

- it handles multivariate `Y` natively;
- it hits nonlinear target functionals (top shares, participation) by choosing `f`;
- it has a clean information-theoretic optimality (§2.1);
- it is the standard object in the survey-calibration literature (Deville & Särndal 1992; Hainmueller 2012).

I want to commit to this formulation and treat "rank tilt" as its univariate-linear instance.

---

## 2. Theoretical properties

### 2.1 Maximum-entropy optimality

Among all distributions `p` on a donor pool satisfying `E_p[f(Y)] = t`, the one minimizing KL divergence from the uniform reference is

    p*(j) ∝ exp(λᵀ f(Y_j))

with `λ` chosen so the constraint holds. This is standard (Kullback 1959; Csiszár 1975). **Interpretation: among all ways to perturb Stage 2's donor draw to hit the target, the exponential tilt is the one that alters the conditional distribution least.** That is the honest answer to "how do I hit DFA without fucking with the data too much" — you can't hit DFA without fucking with *something*; this fucks with the least-surprising thing (donor-selection probabilities) by the least-surprising amount (minimum KL).

### 2.2 Convexity, uniqueness, and the solver

The calibration constraint (★) is equivalent to the gradient condition of a convex dual problem. Define

    ℓ(λ)  =  Σ_{i ∈ c} w_i log Z_i(λ)  −  λᵀ T_c
    ∇ℓ    =  Σ_{i ∈ c} w_i E_{P(·|i)}[f(Y)]  −  T_c              (gradient is constraint residual)
    ∇²ℓ   =  Σ_{i ∈ c} w_i Cov_{P(·|i)}[f(Y)]                     (PSD; PD under mild donor-pool rank conditions)

`ℓ` is convex; `λ*` is unique whenever `T_c` is in the relative interior of the reachable moment polytope. Newton's method with backtracking converges quadratically near `λ*`; typical convergence in <10 iterations regardless of `k`.

### 2.3 Reachability

The maximum achievable aggregate under tilt `λ → +∞ · e_m` (tilt up on dimension `m`) is

    T_c^max(m) = Σ_{i ∈ c} w_i max_{j ∈ L_i} f_m(Y_j)

i.e., each record's leaf contributes its argmax donor. Likewise for `−∞`. **If `T_c,m` falls outside `[T_c^min(m), T_c^max(m)]`, the cell is infeasible** — no donor reshuffling within existing leaves can reach it. This is a diagnostically useful signal, not a bug: it says "your leaves are too narrow for this target; expand them or accept a residual."

### 2.4 Donor support respected

Every imputed value remains a Y-vector that existed in the SCF. No synthetic values. Compare:

- **Rescaling** (`Y ← c · Y`): trivially produces values that no SCF donor had, which can be severe at the top tail where rescaling compounds against already-extreme donors.
- **Exponential tilt**: stays on the empirical SCF support for every record. If a donor combination (pass-throughs, other_home, debt) has never appeared in SCF, it never appears in calibrated PUF either.

This is the single cleanest property the tilt has over rescaling.

### 2.5 Extensive-margin movement

Rescaling cannot move a zero to a positive (`c · 0 = 0`). Tilt can: if record `i`'s leaf contains any donors with positive `Y_m`, `λ_m > 0` raises their selection probability, and record `i`'s expected participation on category `m` rises. This is directly relevant to two of our three Stage-2 residuals — bottom-decile cash/installment_debt overstatement (too many positives) and top-decile pass-throughs understatement (too few positives). Rescaling cannot touch either.

Caveat: tilt can only move participation *within the range supported by the leaf*. A leaf with 100% positive donors can't decrease participation; a leaf with 100% zero donors can't increase it. Leaves need nonzero dispersion in the target feature for tilt to move it.

### 2.6 Within-cell rank preservation

Tilt does **not** preserve within-cell rank of imputed values (different records redraw independently from tilted distributions). Rescaling does preserve rank. If within-cell rank is a property we care about (e.g., for inheriting plausible co-movement across categories), this is a cost of the tilt. I don't think we care, but flag it.

### 2.7 Joint Y structure preserved

Because each record still draws **one donor** under the tilt — just with non-uniform probability — the full 23-dim `Y_j` is inherited as a unit. Correlations across categories (e.g., someone who holds pass-throughs is more likely to hold other_home) are preserved exactly at the donor level. This is essential and is the reason multivariate tilt beats per-category independent calibration (§5).

### 2.8 Symmetry

`λ > 0` tilts up, `λ < 0` tilts down, `λ = 0` is uniform. Overshoot/undershoot recovery is continuous in `λ`-space. Contrast with the keep-max scheme we discussed: keep-max is asymmetric; keep-min is a different operator; alternating does not return you to the uniform distribution.

### 2.9 Effective sample size (ESS)

Under tilt with coefficient magnitude `‖λ‖`, the effective number of donors per leaf is

    ESS_i(λ) = 1 / Σ_j P(j|i)²

At `λ = 0`, `ESS_i = |L_i|` (typically ~20 at `min.node.size = 20`). At `‖λ‖ → ∞`, `ESS_i → 1`. **Donor-seed variance scales roughly as `√(|L_i| / ESS_i)`** relative to the uniform baseline. If the calibration drives ESS from 20 down to 5, expect donor-seed SD to roughly double from Stage 2's `$4T` floor.

**This is the quiet cost of the tilt** and the thing I'd most want to measure. Report median ESS, 10th-percentile ESS, and the seed-variance inflation factor alongside every calibrated run.

---

## 3. Comparison against alternatives

| Property                          | Rescaling (`c·Y`) | Keep-max redraw | Exponential tilt |
|-----------------------------------|:---:|:---:|:---:|
| Stays on SCF donor support        | ✗  | ✓  | ✓  |
| Symmetric (tilt up & down)        | ✓  | ✗  | ✓  |
| Moves extensive margin            | ✗  | ~  | ✓  |
| Preserves within-cell rank        | ✓  | ✗  | ✗  |
| Preserves joint Y structure       | ✓ scalar / ✗ vector | ✗  | ✓  |
| Hits multiple targets jointly     | ✗  | ✗  | ✓  |
| Unique closed-form solution       | ✓ trivial | n/a (iterative) | ✓ convex |
| Max-entropy optimal               | ✗  | ✗  | ✓  |
| Iteration-free                    | ✓  | ✗  | ✓ (one Newton solve) |
| Interpretable output              | ✓  | ✗  | ✓ (λ per target) |
| Cheap (adds <1 min)               | ✓  | ✗ (many passes) | ✓ |

---

## 4. Target statistics `f(Y)` — what we can actually hit

The flexibility of the tilt is in choosing `f`. Several useful cases:

- **Per-category totals** (linear): `f_m(Y) = Y_m`. Standard. One `λ_m` per category hits its aggregate.
- **Participation rates** (linear on indicators): `f_m(Y) = 1{Y_m > 0}`. Hits fraction-with-positive-holdings per category.
- **Intensive-margin conditional means** (awkward linear): harder. Can do `f(Y) = Y · 1{Y > 0}` to hit "sum among positives," then combine with participation constraint.
- **Top shares** (linear given cutoff): `f_m,top(Y) = Y_m · 1{Y_m > p_m}`. Hits "aggregate above threshold," which combined with the total pins top share.
- **Quantile targets** (not linear in `Y` — use indicator linearization): `f(Y) = 1{Y > q}` to pin `P(Y > q) = target`.

**Recommendation:** start with per-category totals (linear, trivial) and participation rates (linear on indicators, still trivial). Add top shares if needed after reviewing diagnostics. Avoid quantile targets in the first pass — they inflate `k` quickly.

---

## 5. Multi-dimensional `Y` handling

This is the most consequential design choice. Four options considered:

**(A) Independent tilts per category.** Solve a separate `λ_m` per category, apply per-category. Requires one donor draw per category per record → destroys joint structure. **Reject.**

**(B) Single scalar tilt on aggregate NW.** One `λ` on `f(Y) = Σ_m Y_m`. Preserves joint structure (one donor per record). Only hits aggregate NW; category totals are byproducts. Useful as a sanity check but too coarse.

**(C) Separate donor per category, rescale within.** Hybrid. Reintroduces rescaling. **Reject.**

**(D) Multivariate exponential tilt, one donor per record.** `λ ∈ ℝ^k`, `P(j|i) ∝ exp(λᵀ f(Y_j))`, single donor draw per record. Preserves joint structure, hits multiple targets, is the framework above. **Adopt.**

Option D is the right answer. The solver cost is linear in `k` (really `k²` for Hessian assembly, still negligible up to `k=50+`).

---

## 6. Calibration cells

### 6.1 What a cell is

A calibration cell `c` is a partition of PUF records sharing one `λ_c` vector. Cells partition the record set; each record belongs to exactly one cell; each cell has its own target vector `T_c`.

### 6.2 Candidate cell definitions

| Scheme | Cells | Pros | Cons |
|---|---|---|---|
| Global | 1 | simplest; hits aggregate DFA cleanly | can't fix distributional residuals |
| Income decile | 10 | matches a lot of our Stage-2 diagnostics | doesn't capture age effects |
| Income decile × age bracket | 30–50 | fine-grained; hits DFA's age × wealth cuts | small cells at extremes |
| Wealth decile (post-Stage-2) | 10 | aligns with DFA | **circular**: tilt changes wealth → changes percentile membership |
| SCF-matched cells | varies | maps directly onto SCF targets | re-benchmarks to SCF, not DFA |

### 6.3 The circularity problem

Wealth-based cells are the most natural for DFA (which publishes by wealth percentile), but wealth is what we're imputing. Two workarounds:

1. **Use Stage-2 (pre-tilt) wealth to define cells**, solve tilt, accept that post-tilt wealth percentiles drift slightly. Fast. Probably acceptable.
2. **Iterate**: tilt → recompute wealth → redefine cells → re-tilt. Fixed-point iteration. Converges if the tilt is not too aggressive. Adds 2–3× cost.

I'd start with (1). If diagnostics show large percentile-boundary drift, try (2).

### 6.4 DFA-aggregate vs SCF-conditional targets — [OPEN]

An important honest accounting: DFA publishes two flavors of number.

- **DFA aggregates (national, quarterly, by category).** Sourced from the Financial Accounts / Z.1 — *independent* of SCF.
- **DFA distributional (category × wealth-percentile × age).** Allocated *using SCF shares* — so calibrating PUF to distributional DFA is partly calibrating back to SCF.

The clean design:
- **Stage 3a** — SCF-conditional rebalancing. Hit per-cell moments derived from SCF directly (not DFA-distributional). Fixes `P_PUF(X | decile) ≠ P_SCF(X | decile)` as a within-conditional issue.
- **Stage 3b** — DFA-aggregate rescale. Hit per-category DFA totals at the global-cell level. Fixes the 5–10% gap between SCF aggregates and Z.1.

These are two passes of the same tilt machinery with different target sources. They compose naturally: 3a solves for one set of `λ`; 3b adds a residual scalar tilt on top. **[OPEN: confirm that's the right decomposition — it's my pitch, not yours.]**

---

## 7. Solver

### 7.1 The program

For cell `c` with records `R_c`, weights `w_i`, per-record donor pool `L_i ⊆ {1, …, n_SCF}`, donor features `f(Y_j) ∈ ℝ^k`:

    minimize over λ ∈ ℝ^k:
        ℓ_c(λ) = Σ_{i ∈ R_c} w_i log Z_i(λ)  −  λᵀ T_c
        Z_i(λ) = Σ_{j ∈ L_i} exp(λᵀ f(Y_j))

### 7.2 Vectorization in R

Precompute once: for each cell `c`, a stacked matrix `F_c ∈ ℝ^{N_donor_c × k}` concatenating `f(Y_j)` across all donors used by any record in the cell, plus an index `leaf_ptr_i` giving the row range of record `i`'s donors in `F_c`.

Per Newton iteration:
1. Compute `u = F_c · λ` (one matrix-vector product, `O(N_donor · k)`).
2. Compute `exp(u)`, normalize within each record's leaf slice (segmented softmax).
3. Form `∇ℓ = (P_c · F_c)ᵀ w − T_c` where `P_c` is the sparse per-record tilted probability matrix (also one matrix-vector).
4. Form `∇²ℓ = Σ_i w_i · [F_i' diag(p_i) F_i − (F_iᵀ p_i)(F_iᵀ p_i)ᵀ]`, accumulated across records.
5. Newton step `λ ← λ − (∇²ℓ)⁻¹ ∇ℓ`, with backtracking if `‖∇ℓ‖` does not decrease.

`Rcpp` would speed up step 4 but isn't necessary at our scale — 50 cells × 10 iterations × 100k records × 20 donors × 23² features ≈ 230M ops per Hessian pass ≈ a few seconds in base R with `crossprod`. Vectorize or loop, doesn't matter.

### 7.3 Starting point

Warm-start `λ = 0` gives uniform donor weights and `∇ℓ = T_c^{Stage 2} − T_c`. Typical tilt magnitudes should be small (log-target / donor-sd units), so Newton usually converges in 5–8 iterations with `‖∇ℓ‖/‖T_c‖ < 1e-4` tolerance.

---

## 8. Implementation plan

### 8.1 Pipeline hook

Stage 3 is a post-processor on `tax_units` after `wealth.R`. Cleanest modular structure:

```
src/imputations/
├── stage1_scf_tax_units.R    # existing
├── wealth.R                  # existing (Stage 2)
├── stage3_calibrate_wealth.R # NEW
```

Signature:

```r
stage3_calibrate_wealth(tax_units,
                        dfa_targets,       # list of per-category DFA totals
                        scf_cond_targets,  # list of per-cell SCF moments (Stage 3a)
                        cells_by,          # character vector of cell-defining cols
                        f_spec,            # character: which target functionals
                        leaf_membership,   # per-record leaf donor indices (from wealth.R)
                        Y_mat)             # SCF donor Y matrix
-> tax_units with updated wealth columns
```

### 8.2 What Stage 2 needs to export

Currently `wealth.R` calls `walk_to_leaf(t, X_puf[i,])` and forgets the leaf. We need to persist `leaves[[t]][[nd]]` per record. Options:

- **Recompute in Stage 3**: cheap (~5s to re-walk). Preferred for modularity. Requires re-loading the cached DRF — already fast.
- **Persist from Stage 2**: add a `leaf_donors` list column (`list(integer)`) to `tax_units`. ~35 MB for 220k × ~20 donors.

Prefer recompute. Keeps Stage 2 output clean.

### 8.3 DFA data ingestion

A new static input at `resources/dfa/` (suggest adding that subdir):

- `dfa_levels_2017q4.csv` — category × total, for the calibration year.
- `dfa_by_wealth_pct_2017q4.csv` — optional, for Stage 3a's within-cell targets if we use DFA-distributional rather than SCF-conditional.

Source: Federal Reserve's DFA tables, publicly downloadable. Parsing is CSV; schema drift is slow.

Map DFA categories to our 23-dim Y schema — this is the non-trivial curated step. SCF→DFA asset-class crosswalks exist in Batty et al. (2019); I'd port their mapping rather than invent. **[OPEN: confirm the crosswalk treatment.]**

### 8.4 Phased rollout

- **3a prototype** (`src/eda/stage3_prototype.R`): scalar global tilt on total NW. End-to-end sanity check; should match a known DFA number.
- **3b multivariate aggregate**: vector `λ` on all 23 category totals, one global cell. Hits DFA aggregates.
- **3c within-cell**: income-decile cells, SCF-conditional targets on category totals. Fixes within-decile structural mismatch.
- **3d participation**: add `f = 1{Y > 0}` targets. Addresses extensive-margin residuals.
- **3e production**: promote to `src/imputations/stage3_calibrate_wealth.R`; wire into pipeline.
- **Phase 4 — Forbes top-tail augmentation** (deferred). SCF explicitly excludes Forbes-listed individuals from its high-wealth sampling frame, so DFA (which uses SCF shares) inherits the gap. Fabricate ~400 synthetic tax units from the Forbes 400 list with plausible Y-schema composition (mostly pass_throughs + equities + other_home), assign small weights, append to the SCF donor pool before Stage 2 bootstrap. Open design questions: Forbes NW → 23-var composition rule; weight convention; interaction with Stage 3 calibration targets (Forbes donors push aggregates above both SCF and DFA totals, so targets may need Forbes-adjusted versions); longitudinal consistency as the Forbes list churns. See task #18.

Each phase is its own eda script, its own diagnostics, its own go/no-go with you.

---

## 9. Performance

Budget estimate on 16 cores, 220k PUF, 29k SCF donors, 23-dim Y:

| Step | Cost |
|---|---|
| Re-walk leaves (if not persisted) | ~5 s |
| Build per-cell donor blocks | ~1 s |
| Solve λ per cell (10 cells, Newton × 8 iter) | ~5–30 s |
| Redraw donors under tilt | ~5 s |
| Diagnostics (ESS, λ magnitudes, residuals) | ~2 s |
| **Total Stage 3 overhead** | **~20–50 s** |

Stage 2 is ~10 min end-to-end (training + draw). Stage 3 is a rounding error on top.

Memory: peak extra ~50 MB for the stacked donor-feature matrices. Irrelevant.

---

## 10. Benchmarking plan

### 10.1 Metrics to compute pre/post calibration

**Aggregate match** (should be exact post-3b):
- Total NW vs DFA aggregate.
- Each of 23 categories vs DFA aggregate.

**Distributional match** (the interesting output):
- NW by PUF income decile — vs SCF and vs DFA-distributional.
- Participation (% positive) by category × income decile.
- Top 1%, top 10%, Gini on NW — vs SCF benchmarks (from memory: Gini 0.875–0.88 currently, target ~0.863).
- Wealth-tax counter at $50M threshold (currently 1.3× over; target-dependent).

**Known residuals from Stage 2** (the motivations):
- Bottom-decile cash positivity — currently +23 pp vs SCF. After participation tilt at 3d?
- Bottom-decile installment_debt positivity — currently +18 pp. Same.
- Dec-10 pass_throughs / other_home participation — currently −6 pp. Does tilt narrow the gap?
- Dec-10 kg_pass_throughs participation — same.

### 10.2 Calibration quality

- Per-cell residual `‖∇ℓ‖/‖T_c‖`: should be ≤1e-4 at convergence.
- λ magnitude distribution: median, p95. Large values flag near-infeasibility.
- ESS per record: median, p10. Large drops flag variance inflation.
- Infeasibility count: cells where solver hits its iteration limit without convergence.

### 10.3 Seed variance (critical)

Repeat Stage 3 with 5 different donor-draw seeds (Stage 2 fixed). Compute SD of aggregate NW and top shares. Compare to Stage 2's `$4T` donor-seed SD floor. Expect inflation proportional to `√(|L|/ESS)`. If Stage 3 inflates donor-seed SD above the train-seed SD (`$5T`), it becomes the dominant noise source — worth knowing.

### 10.4 Comparison runs

Run three Stage 3 variants on the same Stage 2 output:
1. Exponential tilt (proposed).
2. Flat rescaling (per-category scale factor).
3. Uncalibrated Stage 2 baseline.

Produce the same diagnostic table for all three. Expected finding: tilt wins on distribution-preserving properties (top shares, donor support), rescaling wins on simplicity and rank preservation, both hit aggregates by construction.

---

## 11. Failure modes & mitigations

1. **Infeasible cell** (`T_c` outside reachable range). Detect via solver divergence. Mitigation: merge adjacent cells; if still infeasible, log and fall back to partial rescaling residual. Never silently suppress.
2. **ESS collapse** (`λ` large, donor draw concentrates on 1–2 donors per leaf). Mitigation: expand leaf definition — use all donors from the cell's DRF predictive distribution, not just the single-tree leaf. Tradeoff: loses conditional sharpness.
3. **Over-determined targets** (more target moments than effective donor dimension). Mitigation: L2-regularize `λ` ("elastic" calibration à la Chan, Yam, Zhang 2016). Gives soft hit on targets; report residuals.
4. **Tiny cells**. Cells with <100 records or <500 unique donors: solver unreliable. Mitigation: merge with neighbor cell, or fall back to the next-coarser partition.
5. **Negative aggregate targets** (possible for net debt positions). Tilt handles negative `T_c` as long as some donors have negative `f(Y)`. Guard: check that `T_c` is in `[T_c^min, T_c^max]` before solving.
6. **Wealth-cell circularity**. Detect by comparing pre-tilt and post-tilt wealth-percentile membership. If ≥5% of records cross cell boundaries, trigger one iteration.
7. **DFA vintage mismatch**. DFA is published quarterly and revised retroactively. Pin a specific vintage (e.g., 2017Q4 as of 2026-XX release) and record it in `config/interfaces/` per the usual upstream-dependency pattern.

---

## 12. Open questions (to discuss tomorrow)

Prioritized — the first three block implementation.

1. **Which DFA targets matter?** All 23 categories, or a curated subset (real estate, equities, pensions, the big four)? Tradeoff: more targets ⇒ tighter fit but more `λ`-dimensions to solve.
2. **Cell granularity.** Global only? Income decile? Income decile × age? I'd start global → income-decile and reserve cross-cuts for Phase 3c+.
3. **DFA-distributional vs SCF-conditional.** Is the "SCF distributional shape is right, aggregate is wrong" premise something you agree with? If yes, §6.4's 3a/3b decomposition is the move. If you'd rather calibrate to DFA-distributional directly (despite its SCF-derivation), say so.
4. **How to handle the bottom-decile extensive-margin overstatement.** Participation tilt (Phase 3d) helps but can't push to zero if the leaf has no zero-donors. Worth accepting the residual, or worth a separate donor-swap (Phase 3e)?
5. **DFA-category crosswalk.** Port Batty et al. (2019) Z.1→SCF crosswalk, or curate our own? The categories don't align 1-to-1 (e.g., SCF "vehicles" splits across multiple Z.1 lines).
6. **Seed variance tolerance.** If Stage 3 inflates donor-seed SD from `$4T` to `$8T`, is that acceptable given the aggregate-match gain? If not, we'd cap `‖λ‖` (regularize).
7. **Iterate wealth-percentile cells?** Probably no for V1, yes eventually.

---

## 13. Suggested next step

If you sign off on the general approach:

**Kick off Phase 3a as an eda prototype** — `src/eda/stage3_prototype.R`, scalar global tilt on total NW, end-to-end. One day of work. Output: a pre/post table showing the total-NW gap closed to zero, the category-level byproducts, ESS distribution, and the seed-variance number. From there we'd know whether the machinery holds and whether ESS is the binding constraint. Decide Phase 3b/3c scope from the prototype's behavior rather than guessing in advance.

Nothing in this design is committed; it's all a pitch. Review whatever looks wrong.
