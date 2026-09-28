---
title: "Code review of the filer-refit branch: what was cleaned, what is proposed, what needs a decision"
role: review
workstream: state_weights
status: current
updated: 2026-09-28
true_as_of: "phase3b-filer-refit @ 29f65fd (the 22 commits since main @ 8577cfe)"
sot: research/state_weights/plan.md
supersedes: []
superseded_by: null
---

# Code review of the filer-refit branch

For John Ricco. On 2026-09-28, before fast-forwarding `main` to the
filer-refit branch, JI ran a four-angle cleanup review (reuse, simplification,
efficiency, altitude) over the 22 commits that carry S40 (the per-year filer
refit), its diagnostics and the top-tail notes. The result sorted into three
tiers. **Tier A** — result-identical cleanups — was applied the same day and
is in the fast-forward (§1). **Tier B** — result-preserving restructuring of
the S40 loop in `project_puf.R` — and **Tier C** — changes that alter fitted
numbers, delete infrastructure the repo still documents, or need a decision
record — are proposed here for your review (§2, §3). Nothing in B or C is
built.

Glossary for cold reading: **S40** is the 2026-09-27 decision to calibrate
TY2017–2023 filer weights to Pub 1304 counts by AGI class (`src/refit_filers.R`,
`src/project_puf.R` lines ~640–716) instead of scaling by filing status × age.
**S30** (2026-09-21) says state-weight construction reads a pinned
Tax-Simulator AGI rather than a Tax-Data proxy. **The extensive factor** is the
year-y ÷ 2019 growth in the weighted count of records holding a variable, which
`build_factor_rows_2020plus()` divides the SOI/CBO income growth by to get a
per-holder factor; it reads the weights, which is why the refit iterates.
Timings quoted are from the v2 production log (`td_filer_refit-27709129.out`,
3h33m total) and from the reviewer's login-node measurements.

## 1. Applied (Tier A, commit after 29f65fd)

- Dates: three notes stamped 2026-09-29 corrected to 2026-09-28.
- `09_bucket_aging_prototype.R` reads the IRS side of Table 4.3 from 08's
  committed `table43_2017_2023.csv` instead of a second, weaker copy of the
  sheet reader; the per-iteration recomputation of an unchanged table hoisted;
  a dead variable removed.
- `tail_common.R` (new): one `read_filers()` (fread of the ~60 columns used,
  via the existing `model_data_path()` with its vintage guard) and one
  `top_slice()` (weighted top-n with the fractional marginal record) replace
  three hand-rolled copies each in 07/08/09.
- `src/agi_proxy.R`: keeps `part_scorp_net` in its output (08 and 09 had
  re-derived the 10-term formula); `TUITION_LIMIT` written as a limit plus a
  last year, the idiom `DPAD_LAST_YEAR` already used; `AGI_PROXY_INPUTS`
  names the columns the function reads. Output values unchanged.
- `src/refit_filers.R`: `refit_target_matrix()` no longer returns two fields
  no caller read.
- Five near-identical sbatch files replaced by `run_rscript.sbatch` taking
  the script path; job name, memory and time on the command line.
- `plan.md` Phase 3b step 3 and status row F brought up to the code (entropy
  calibration including TY2017, v2 accepted); `CLAUDE.md` step 4 no longer
  lists the LP as a pipeline stage and gains a "Filer weights" block.

Verification: 07, 08 and 09 re-run through the new runner; outputs diffed
against the committed results (see the commit message for the outcome).

## 2. Proposed, result-preserving (Tier B) — apply, then certify with one caches-loaded rebuild diffed against v2

**B1. The S40 loop carries 2018–2019 through every round although they are
fit once.** `project_puf.R` ~660–680: the year list `if (it == 1) REFIT_YEARS
else REFIT_YEARS[>= 2020]`, the class-total carry-forward for `year < 2020`,
and the `is.null(refit_class_prev)` branch exist only because 2018–2019
share the loop with the weight-dependent years; `2020L` appears five times.
Fit 2018–2019 before the loop, name `REFIT_ITER_YEARS = 2020L:FILER_OBS_LAST`
once, loop over it.

**B2. The in-loop ledger rebuild runs before the convergence test**, so on
the final round it is computed and discarded by the post-loop rebuild. Move
it below `if (moved < REFIT_CLASS_TOL) break`.

**B3. `agi_proxy()` runs twice per year-round on the same data** (inside
`refit_filer_year()` and again for the class tally). Proxy once, call
`refit_classed()` directly. The class comparison's `full_join` + `coalesce`
handles empty classes that `group_by(agi_lo, .drop = FALSE)` on the `cut()`
factor already handles; the break vector is a constant of `refit_targets`
(hoist as `REFIT_CLASS_BREAKS`).

**B4. The pre-loop ledgers are built for 2017–2097 and discarded.**
`weight_ledger` (181 s) and `factor_ledger` 2020–2097 (128 s) are built
before the loop, of which only 2017–2023 / 2020–2023 are read inside it, then
rebuilt in full after. Build through 2023 before, 2024+ after: ~285 s saved
per run, and the 74-year `compute_weights_for_year(2023)` recursion
(~20 s) hoisted.

**B5. `compute_weights_for_year()` is documented as pure and is not.** It
branches on the global `refit_filer_weights` that the loop mutates 300 lines
later, and `build_factor_rows_2020plus()` reads the global `weight_ledger`
the loop reassigns. Pass the refit table and the ledger as arguments. Same
numbers; removes an order-dependence that breaks the first time someone
sources the file in pieces.

**B6. `build_factor_rows_2020plus()` rescans the 113M-row ledger twice per
call** (the year-y slice and the identical 2019 slice) and re-derives the
invariant nonzero mask over 207k × ~110 columns; 168 calls per run ≈ 275 s.
Hoist `w19` and the mask; the extensive factor becomes two `crossprod`s.

**B7. The in-loop `bind_rows` + `arrange` of the full ledger** (~7.5 s and
~8 GB churn per iteration) shrinks 11× once B4 keeps a 2018–2023 ledger
inside the loop.

Together: same weights and factors in principle, about 10 minutes off a
3h33m run, one global-state hazard removed. **Certification:** one Slurm run
with caches loaded, every emitted year compared column-for-column with v2 —
the E2-tripwire discipline (`nonfiler_federal_validation.md`). Not assumed.

## 3. Proposed, needs your call (Tier C)

**C1. `calibrate_weights()` solves 207,692 record variables when the problem
has 1,007.** Stationarity of the objective gives `u_i = −W0·λ·Σ_j X_ij r_j / b_j`
(clipped), so every record with the same target-membership row gets the same
log-ratio; the solve collapses exactly to one variable per distinct row —
1,007 in TY2022 — with prior `Σ w0` per cell. One objective evaluation falls
from 6.3 ms to 0.08 ms and L-BFGS-B's own O(n) bookkeeping shrinks 200×.
The loop's 45,885 evaluations (~1,010 s of its 1,036 s) become seconds; the
two TY2017 fits likewise. Fitted weights change at solver-tolerance level
(not bit-identical), and the currently capped fits (C2) are not at a
stationary point, so their record-level ratios may vary within a cell today.
*Algorithmic change: your decision.* It would also make the bake-off's
"cal1" arm cheap to re-run under variants.

**C2. L-BFGS-B hits its 5,000-iteration cap without converging** for TY2018,
TY2019 and TY2023 in every round (`convergence = 1`; warm starts buy nothing
in 2023), because `factr = 1e5` (relative tolerance ~2e-11) is far tighter
than the ±0.5%/500-return band the targets are held to. All capped fits still
report 100% within band. ~580 s of the loop. Loosen to `optim`'s default
`factr = 1e7` and compare `within_band` and the ratio quantiles. Subsumed by
C1, but the stopping rule is mismatched either way.

**C3. The 2017 LP is dead code with live scaffolding.** `reweight_lp()`,
`build_lhs()`, `run_lp()` have no caller; `config/target_info/baseline.csv`
is read at `configure.R:178` and consumed nowhere; `TAXDATA_DO_LP` is set in
four entry points and read by none; `resources/cache/lp` is created for
nothing except the bake-off's `refit_common.R`, which still reads the
historical `weight_deltas.rds` that no code regenerates. `tune_epsilon()` has
a test and a memory note but is unreachable. Delete all of it and have
`refit_common.R` read its prior from a committed pre-LP weight file — or
keep it, and say why in `CLAUDE.md`. Your call: the LP is still described as
the pipeline in the README's tech-debt list.

**C4. `src/agi_proxy.R` contradicts S30 on its face.** S30: "do not pursue a
closer proxy … approximating above-the-line deductions, the
taxable-Social-Security formula and loss limitations inside Tax-Data —
reimplementing the tax calculator in the repo whose CLAUDE.md puts tax
calculation in Tax-Simulator." The proxy does those three things. The
reviewer judged it defensible — S30's pinned-vintage AGI would be stale
inside the run that ages the incomes it classes — but nothing records the
reconciliation: `grep S30` over the proxy, the design note and the refit
scripts finds nothing, and the S40 row does not mention the proxy. Proposed:
an S42 ("the per-year refit classes by a Tax-Data proxy because the aged
incomes exist only in this run; S30 stands for the state fit"), cited from
the proxy's header; and promote `02_agi_proxy_check.R`'s 99% same-class
agreement into `src/tests/` as the drift tripwire, since the proxy already
departs from Tax-Simulator on one line (net partnership + S-corp vs
gain-only) and any change to Tax-Simulator's `agi.R` will silently widen
that.

**C5. Stage 2 of the TY2017 fit is justified by an argument, not a number.**
TY2017 is fit before the value rescaling (by `E00100`) and again after the
imputations (by the proxy). Stage 2's effect exists only in a log line
(weight ratios 0.89–1.09 at the 1st/99th percentile relative to stage 1, one
more target reached). A stage-1-vs-stage-2 row in `05_vintage_diagnostics.R`
— ratio quantiles, class-total movement, Table 1.4 amount drift — would show
whether the second fit earns its keep or is dead weight. New diagnostic, not
a cleanup.

## 4. Reviewed and left alone

- The weights↔incomes iteration in `project_puf.R`: the right depth. A
  prior-weight extensive factor would make the refit single-pass but misstate
  2020+ aggregates by the holder-count ratio; the measured movement
  (0.1095% → 0.0265% of returns, rounds 1→2→3) contracts geometrically. The
  `REFIT_CLASS_TOL` stop rule replaced a max-single-class criterion that thin
  classes (3–92 returns) made unreachable; recorded in the code.
- `06_puf_soi_gaps.R` evaluates `configure.R` expression-by-expression to
  skip its two writes to the shared output root. Fragile, but the
  alternatives (a named scratch vintage under the shared root; a one-use env
  switch in `configure.R`) are not clearly better.
- The bake-off scripts `01`–`04` and `refit_common.R` carry pre-promotion
  copies of the target matrix, the objective and the tolerance constants.
  They are the frozen evidence behind S40's method choice; re-pointing them
  at `src/refit_filers.R` (which has since gained warm starts and a different
  band basis) would make a re-run stop reproducing the committed results.
  Left as the historical arm; the drift is a known choice.
- `07`'s direct reads of the IRS-Ind panel bypass `read_bysize_series()`'s
  filters; verified harmless on today's panel at the top classes.
- `AGE_GROUP_AGE` in `create_2017_puf.R`: a representative age per band so
  the age-range matcher works on a record that has only `age_group`; the
  bands align exactly with groups 1–6.

## Revision history

- **2026-09-28** — Written; Tier A applied the same day.
