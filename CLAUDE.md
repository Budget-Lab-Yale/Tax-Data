# Tax-Data

Produces historical and projected tax microdata for the Budget Lab microsimulation stack. The output of this repo is the input to **Tax-Simulator**, which is the only consumer that matters. If you are reasoning about tax liability, policy parameters, or behavioral response, you are in the wrong repo — those live downstream.

**Contributors:** John Ricco (lead), Joshua Kendall.

## What this repo does, in order

`src/main.R` is the single entry point. It is executed from the repo root and sources, in order:

1. `src/configure.R` — read the runscript (`config/runscripts/baseline.yaml`), set the output path, load the variable guide and target info.
2. `src/process_targets.R` — ingest SOI / CBO / IRS targets.
3. `src/process_puf.R` — read the raw PUF, standardize names, derive variables.
4. `src/reweight.R` → `src/summary.R` → `src/create_2017_puf.R` — produce the 2017 base-year file.
5. `src/impute_nonfilers.R` — attach non-filing units.
6. `src/impute_variables.R` — dispatch individual imputations in `src/imputations/*.R` (one file per imputed variable).
7. `src/project_puf.R` — age the 2017 file forward year by year using `variable_guide.grow_with` and macro projection series.

Read the actual files before editing. This sequence changes — don't trust this summary over the code.

## Directory contract

| Path                      | What lives here                                                                                       | Editing posture             |
|---------------------------|-------------------------------------------------------------------------------------------------------|-----------------------------|
| `src/`                    | Pipeline code.                                                                                        | Yes.                        |
| `src/imputations/`        | One file per imputed variable.                                                                        | Yes.                        |
| `src/eda/`                | Diagnostic and validation scripts. **Not part of the pipeline.**                                      | Freely.                     |
| `config/runscripts/`      | Scenario parameters (`write_locally`, `user_id`, upstream dependency vintages).                       | Yes.                        |
| `config/variable_guide/`  | One row per output variable: PUF name, type, `grow_with`, description.                                | Curated by hand.            |
| `config/target_info/`     | Reweighting target schema.                                                                            | Curated by hand.            |
| `config/interfaces/`      | Version pointers to upstream models (`Macro-Projections`, etc.).                                      | Bump per vintage.           |
| `resources/`              | Small, static, human-curated inputs committed to git (CEX dictionaries, PCE targets, crosswalks, mortality tables). | Carefully.                  |
| `resources/cache/`        | Cached model fits (ranger QRF, DRF). **Gitignored, rebuildable from code.**                           | Pipeline writes here.       |
| `src/nonfilers/`          | The ASEC non-filer builder's shared modules (`asec_tax_units.R`, `filing_model.R`, `state_weights.R`), moved from Tax-Simulator 2026-09-11. Sourced by `research/state_weights/` scripts; not yet by `main.R`. | Yes.                        |
| `research/`               | Population-construction research corpus (non-filer pool, residual anchors, split state weights): plans, method of record, decisions log, numbered build scripts with their committed gate CSVs. Moved from Tax-Simulator 2026-09-11; `research/README.md` is the index. Large intermediates under `**/results/` are gitignored. | Per `research/CONVENTIONS.md`. |
| `plots/`                  | Diagnostic plot output. **Gitignored.**                                                               | Freely.                     |

**Rule:** if it is large, changes frequently, or comes from another model, it belongs in `config/interfaces/` as a version pointer — *not* in `resources/`.

## Coding rules

**Inherit from Tax-Simulator.** For general R style (tidyverse idioms, naming, function shape, comment blocks), see `../Tax-Simulator/CLAUDE.md`. Only the rules below are Tax-Data–specific.

### Never drop NAs silently

No `na.rm = T`, no bare `drop_na()`, no `filter(!is.na(x))` buried in a pipe without a comment justifying the universe. In this pipeline NAs encode *not-in-universe* or *not-yet-imputed*, and dropping them corrupts weights.

**Weak:** `mean(income, na.rm = T)`
**Strong:** `stopifnot(!any(is.na(income)))`, or an explicit `filter()` whose comment states the universe being restricted to.

If you hit NAs you did not expect, surface the count and the cause to the user. Do not paper over.

### Trained models are cached, not retrained

Imputations use `train_or_load_*` helpers in `src/imputations/helpers.R` that hash their inputs and stash fits in `resources/cache/`. Don't bypass. If a cache is stale, delete the file — don't re-engineer the helper to "force" retraining.

### Year literals are load-bearing

2015 and 2017 appear as literals throughout. Before editing a number, check whether it is a base-year constant or a magic number. The README calls this out as tech debt; don't "clean it up" casually — confirm with the user first.

## Validation philosophy

Validation in Tax-Data is **bespoke, interactive, and vibes-based**. The user is the ultimate arbiter. There is no CI and no automated test suite for imputation correctness.

Expected workflow for any non-trivial change:

- Produce diagnostic plots (typically into `plots/`).
- Run targeted `src/eda/*.R` scripts.
- Compare imputed aggregates against NIPA / SOI / external benchmarks.
- Hand the user numbers and plots, wait for a thumbs-up.

**Weak:** "It runs without error and aggregates match NIPA within 2%, so I called it done."
**Strong:** "Here are the diagnostics — continuity at the boundary, sensitivity to the tuning knob, aggregate match, and conditional distributions versus source. What do you think?"

Necessary, not sufficient: a script running, an aggregate matching, a marginal distribution matching. Always hand the user diagnostics and defer on the call.

## Consumption imputation (as of 2026-04)

Total consumption `C` and 8 BEA-PCE categories (prefixed `c_*`: `c_clothing`, `c_motor_vehicles`, `c_durables`, `c_other_nondurables`, `c_food_off_premises`, `c_gasoline`, `c_housing_utilities`, `c_other_services_health`) are imputed from CEX into the PUF in two stages plus a top-tail adjustment, then benchmarked to NIPA PCE.

- **Stage A — total C.** Ranger quantile regression forest trained on CEX, predicting conditional quantiles of total consumption from income and demographics. A single draw from the estimated distribution is matched to each PUF unit by within-dataset income rank.
- **Stage B — category shares.** Distributional random forest (DRF) on the 8 collapsed PCE categories, drawing a share vector conditional on income and total C.
- **Top-tail adjustment.** Above a reference income `Y_ref` (≈ P80 of PUF income), CEX is thin and attenuated, so the Stage A draw is multiplied by a power-law factor `(Y / Y_ref)^β` with `β` estimated from the upper CEX tail (production `β ≈ 0.55`).
- **PCE benchmarking.** Per-category scale factors are applied with a cap (`sf_cap = 2`); any residual deficit relative to NIPA is redistributed across households proportional to post–Stage-A total C. No category can fabricate more than 2× household-level consumption to hit NIPA.

**Validation status.** Stages A, B, and the top-tail adjustment have all been validated:

- Stage A / B under rank-matching — conditional `F(C | pctile, demographics)` and `F(share_k | ⋯)` match CEX tightly across 81 demographic-income cells.
- Top tail — continuity at `Y_ref`, `β` sensitivity across CEX floors (range 0.47–0.57), aggregate match to 2017 NIPA PCE within rounding post-benchmark.

Diagnostic scripts: `src/eda/validate_top_tail.R`, `src/eda/verify_pce_bench.R`, `src/eda/conditional_diagnostic.R`.

**Known caveat — PUF-Y vs CEX-Y definitional gap.** PUF income and CEX income are defined differently. At the same within-dataset rank, CEX income runs 22–48% lower than PUF income. The imputation is correct on the numerator (C distributions reproduce CEX at rank), but **any `C / Y` analysis inherits this gap.** Do not report C/Y ratios without flagging it.

## Non-filer population (as of 2026-09)

The DINA append is replaced by a pool built from the CPS ASEC: tax units constructed rule-by-rule, a two-model filing decision (Mok probits below the filing threshold, a Pub 5785 hazard above it), group quarters from the ACS, calibrated so emitted adults plus claimed-dependent netting equal the administrative residual (population adults minus Pub 1304 filing adults) per age band, per year 2017–2023. The build lives in `research/state_weights/nonfiler_pool/01`–`16` with modules in `src/nonfilers/`; it publishes to the `ASEC-Nonfilers` interface, which `impute_nonfilers.R` reads through the contract in `nonfiler_contract.R`. Decisions are the S-series in `research/decisions_log.md` (S13–S26); the method of record is `research/state_weights/nonfiler_residual_design.md`; the plan is `research/state_weights/plan.md`; the review brief that framed Block E is `research/state_weights/NONFILER_STATE_WEIGHTS_REVIEW_2026-08-30.md`.

**Block E is built.** Every published pool year sits in one base (S22, design A), each record tagged with `base_year` and carrying weight only in the year(s) it represents; `materialize.R` divides by the base-year factor, and `project_puf.R` asserts the S21 partition (filing adults + pool adults + netting = Social Security area adults) for every pool year. Pool ids live in disjoint blocks of 1e6 per year; Forbes synthetic ids sit in a reserved block from 9e6 (S25). `write_outputs.R` emits only each year's live records, `weight > 0` (S24, design C; `TAXDATA_EMIT_LIVE_ONLY=0` restores the union-base emit for A/B reproduction only). Consequence for the consumer: the record set changes at seven year boundaries, so Tax-Simulator's sample membership is a rule on the record (its `design-c` branch), not a 2017 id list. The two repos merge as one coordinated vintage.

**Random draws are keyed by record id** (S23, `src/imputations/rng.R`): `draw_by_id()` for per-record draws and `with_model_seed()` around every forest fit, so a change in the record count moves no filer column (wealth excepted, by design: the tilt is a joint fit per bucket). Do not reintroduce `runif(nrow(.))`, `sample_n()` or an unseeded fit sized by `tax_units`. The E2 tripwire in `research/state_weights/nonfiler_federal_validation.md` is the exact-equality test of this property.

**Model caches were trained unweighted.** Every `quantregForest` fit cached before 2026-09 was written under R 4.4.1, whose randomForest lacked a `weights` argument, so the tips, overtime, auto-loan and childcare forests on disk ignored their survey weights. The first rebuild under R 4.4.2 (`TAXDATA_ESTIMATE_MODELS=1`) trains them weighted and moves those imputations; treat it as a modelling change with its own before/after, not a cache refresh.

## Out of scope for this repo

- Tax calculation — lives in **Tax-Simulator**.
- Reweighting *target generation* for the filer LP — upstream; consumed here as SOI / CBO target inputs. (Non-filer anchors and state-weight targets are built here, in `research/`.)
- Public data release cleaning.
- Policy scenarios and behavioral responses.

A request that touches any of the above is a wrong-repo redirect. Say so.

## SLURM

There is no orchestrated SLURM entry point analogous to Tax-Simulator's `slurm_run.sh`. SLURM is used ad-hoc to run individual long-running scripts (imputations, EDA) via `sbatch`; artifacts land as `slurm_*.out` / `slurm_*.err` at the repo root and are gitignored.
