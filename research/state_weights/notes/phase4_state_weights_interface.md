---
title: "Phase 4 plumbing: the State-Weights interface, its producer entry point, and Tax-Simulator's reader"
role: notes
workstream: state_weights
status: open
updated: 2026-09-29
sot: research/state_weights/plan.md
supersedes: []
superseded_by: null
---

# Phase 4 plumbing: the State-Weights interface, its producer entry point, and Tax-Simulator's reader

The design for `plan.md` §5a Phase 4 (group G, the swap-in): how per-year
state weights get from the fitting engine in Tax-Data to `run_one_year()` in
Tax-Simulator as a pinned, versioned interface. Written 2026-09-28 from a
read of both repos at Tax-Data `c965523` and Tax-Simulator `main a7dc6c604`.
It is plumbing — none of it depends on the *values* of the weights — so it
can be built while the Phase 3 re-run waits on the top-tail decision
(`notes/top_tail_base_proposal.md`). Nothing here is built. §7 lists the
decisions.

Glossary: **HT2** — IRS Historic Table 2, returns and income by state × AGI
stub, the source of the filer state targets. **The split** — for each record
i, a vector over jurisdictions s with Σ_s W[i,s] = w_i, its national weight.
**Design C** (S24) — each year's file carries only that year's live records;
non-filer pool ids sit in blocks of 1e6 per year, Forbes synthetics from 9e6.
**Arm C_10_warmB** (S39) — the tentatively chosen fit: one joint 2018–2023
gradient fit with a temporal penalty, warm-started from a year-by-year chain.

## 1. What exists, and what the plan gets wrong

**Tax-Simulator already has the socket.** `build_state_weights(tax_units,
year, method, states)` in `src/data/state_weights.R:66-88` returns a long
`(id, state, weight)` table and is called at `src/sim/run.R:433-438` with
`method = 'placeholder'` hard-coded (every requested state gets `weight / 53`).
Its header already says the next method "READS `state_weights_{year}.csv`
from the Tax-Data interface". The consumer, `get_state_totals()`
(`summary_stats.R:316`), does `filter(state == st) %>% left_join(by = 'id')`
and `replace_na(0)` — so a missing id is silently zero-weighted, and the
reader must validate coverage itself. Interfaces are declared by one yaml key
each (`config/interfaces/interface_versions.yaml`: `type, version,
default_vintage, default_id`); the runscript then accepts
`dep.<Name>.vintage` / `dep.<Name>.ID`, `dependencies.csv` records them, and
`scenario_info$interface_paths$<Name>` resolves to
`<prod>/model_data/<Name>/v<ver>/<vintage>/<ID>`. Every declared path must
exist (`config_parser.R:214-220`) — unconditionally.

**Tax-Data has the engine but not the chain.** `build_split_weights()`
(`src/nonfilers/sw_fit.R:394-424`) fits one year cold and returns the long
table; the invariant is asserted on the share matrix P (row sums within
1e-8), not on emitted W against the published weight. The A → B → C_10_warmB
chain — id-matched warm starts, the PR column appearing in 2018, Forbes ids
starting cold from 2022, the non-filer IPF per year — exists only in
`research/state_weights/scripts/phase3_fit.R`, and the Phase 3 fits saved
filers only. `build_weight_inputs()` needs baseline-calculated `agi` and
`eitc` (its docstring, `sw_fit.R:196-204`); the caller joins them from a
Tax-Simulator detail file passed as a free-form directory argument, and
nothing checks that the detail's own `dependencies.csv` pins the Tax-Data
vintage being split.

**Three corrections to `plan.md` §5a Phase 4.**
1. The `sample_ids` hazard it cites (`config_parser.R:219-225`, "the first
   runscript row's Tax-Data vintage picks the sample") no longer exists:
   Design C replaced it with `in_subsample(id, pct)`, a per-id rule
   (`src/misc/rng.R:109`). Sample membership is already coherent across
   vintages; the reader needs no special handling for it.
2. The dispatcher is no longer in Tax-Data (`src/nonfilers/state_weights.R`
   is a 40-line loader; the copy was deleted, finding 3 resolved). Only the
   Tax-Simulator copy needs the new method.
3. The split is 53 jurisdictions — `STATE_JURISDICTIONS` (51, with DC) plus
   `NONTAX_BUCKETS = c('PR', 'OA')` — and **52 in TY2017**, because HT2 2017
   has no Puerto Rico row and the engine takes jurisdictions from
   `sort(unique(ht2$state))`.

## 2. The two interfaces

Both point *down* the stack once each, which is what S30 anticipated: the
build graph stays acyclic because each is a pinned vintage, not a live call.

### 2.1 `Tax-Simulator-Baseline` (consumed by Tax-Data)

A pinned Tax-Simulator vintage whose `baseline/static/detail/{year}.csv`
supplies `id, agi, eitc` (81 columns; `agi` is column 34, `eitc` 65). Declared
in Tax-Data's `interface_versions.yaml` **under the key `Tax-Simulator`** —
the name Tax-Simulator publishes to (`model_data/Tax-Simulator/v1`) — so the
path resolver needs no special case; "Tax-Simulator-Baseline" in this note is
the prose name for that pin. `config/runscripts/state_weights.yaml` pins its
`vintage` and `scenario: baseline` (P2, done 2026-09-28).

**Producer runscript** (Tax-Simulator, precedent `config/runscripts/tests/
weights_2022.csv` and `phase0_baseline.csv`): baseline only, no behaviour,
no MTRs, `years` = the fit years, `dist_years` empty, `pct_sample = 1`,
`delete_detail = 0` (`purge_detail()` would remove the files), explicit
`dep.Tax-Data.vintage/ID`, and a **named** vintage — `sw_base_<taxdata
vintage>` — so the pair is legible on disk. About 67 s per year at full
sample; the S39 inputs came from such a run (`phase0_s33`, 2017–2026).

**Contract, checked in Tax-Data** (`src/state_weights_contract.R`, the
`nonfiler_contract.R` pattern): the vintage's `dependencies.csv` row for
Tax-Data must equal the Tax-Data vintage being split (this is the check that
does not exist today); every year requested has a detail file; the detail's
`id` set equals the year's live Tax-Data ids (not a subset — `weight` in the
detail is already divided by `pct_sample`, so the run must be full-sample);
`agi` and `eitc` non-NA. Caveat to carry: `filer` in the detail is
post-`do_taxes` and can flip; the split uses Tax-Data's `filer`.

### 2.2 `State-Weights` (produced by Tax-Data, consumed by Tax-Simulator)

`<prod>/model_data/State-Weights/v1/<vintage>/baseline/`:

| File | Contents |
|---|---|
| `state_weights_{year}.csv.gz`, one per emitted year | long `id, state, weight`, `weight > 0`, sorted by `id, state`; `state` is the 2-letter code incl. `DC`, `PR`, `OA` |
| `manifest.csv` | one row per year: `interface, version, vintage, scenario, tax_year, file, n_records, n_states, sum_weight, fitted` (TRUE for fit years, FALSE for carried years), `arm`, `source_year` (the fit year a carried year's shares come from) |
| `dependencies.csv` | `interface, version, vintage, scenario` for Tax-Data and Tax-Simulator-Baseline, plus the HT2 and ACS inputs as rows (§5) |
| `README.md` | method of record pointer, the arm, the jurisdiction set per year, the carry-forward rule, known asymmetries |

Long beats wide because the consumer's join is `filter(state == st) %>%
left_join(by = 'id')` (`research/state_tax/plan.md:50-62` in Tax-Simulator
already specifies this contract). Gzip because `fread` reads it directly and
the dense softmax shares make the long table large (§7, D2).

**Producer invariants, asserted before anything is written:** for every
emitted year, the id set equals the year's live Tax-Data ids exactly (no
missing, no extra — Design C makes this a per-year set); Σ over all
jurisdictions of W[i,s] equals the record's national weight within 1e-8
relative, **checked on W, not P**; states ⊆ the 53; weights finite and > 0
after the zero-drop; the manifest's `sum_weight` equals the file's. A
non-empty target directory refuses to publish unless overridden (the
`configure.R:86-95` guard; `05_emit_pool.R` publishes with `overwrite = TRUE`
and no guard, which is the precedent *not* to follow).

## 3. The producer entry point in Tax-Data

`src/main_state_weights.R`, a sibling of `main.R` — S30: a single entry point
cannot express a dependency on its own output, so both passes are visible in
the vintage record (precedent: `main_phase12_snapshot.R`, `main_placeholder.R`,
`main_accruals_validate.R`).

**Configuration.** `config/runscripts/state_weights.yaml` with
`dependency_info` for `Tax-Data` (vintage, scenario) and
`Tax-Simulator-Baseline` (vintage, scenario), `runtime_options` for the arm
and the fit years. `configure.R` cannot be sourced as-is: it hard-codes the
output path to `Tax-Data/v1/<vintage>/baseline`, mints a Tax-Data vintage
directory, writes a Tax-Data `dependencies.csv`, and requires every Tax-Data
dependency to exist. The clean cut is to **extract its interface resolution
into a pure function** — `resolve_interface_paths(runscript,
interface_versions, output_roots)`, the map at `configure.R:129-139` plus the
two guards at 158–175 — sourced by both entry points, with each entry point
building its own output path and writing its own `dependencies.csv`. `main.R`'s
behaviour is unchanged; certify by diffing its `dependencies.csv` before and
after.

**The fit moves into `src/`.** `src/nonfilers/sw_fit.R` gains the chain
`phase3_fit.R` runs today: `fit_state_weights_chain(inputs_by_year, arm)`
covering A (2017 cold), B (2017→2023 warm-started, id-matched, PR mass from
the record's prior where the column is new, new ids cold), and C_γ warm from
B (`fit_gradient_panel()`); the non-filer IPF per year, which Phase 3 fitted
but never saved; and `emit_split(P, w, ids, jurisdictions)` producing the long
table with the W-level invariant. `phase3_fit.R` then sources `src/` (one
definition; the reuse finding in `FILER_REFIT_CODE_REVIEW_2026-09-28.md`
applies here too). **Acceptance: the moved code reproduces the S39
`C_10_warmB/fit_{year}.rds` matrices `identical()`-ly from the same hardened
inputs**, the way Phase 1 reproduced config 7's golden fit before refactoring.

**Years.** Fit years are those with HT2, a Tax-Data year and a baseline
detail: 2017–2023 today (HT2 through TY2023; 2013 missing, irrelevant). For
2024–2097 the rule from Tax-Simulator's `research/state_tax/plan.md` step 5 —
hold the last calibrated `p(st | x)` fixed — is well-defined under Design C
because every live record in 2024+ is a 2023 record (the 2023 pool grown, the
filers): `W_y = w_y × P_2023`. The producer emits every year 2017–2097 so the
reader stays a reader (§7, D2 weighs the storage).

**TY2017's 52 jurisdictions.** PR has no HT2 row in 2017, so the fit has no PR
column; emit 2017 with PR's mass in OA (both are no-tax buckets, so no state
computation changes), and say so in the manifest. Arm C runs 2018–2023 for
exactly this reason; 2017 is arm A/B.

**Cost.** The S39 chain was A(2017) ~1.3 h, B ~6 h / 20 GB, C_10_warmB ~11 h /
39 GB — a `day`-partition job with `--mem=64G`+ for the panel, not a login
job. Input preparation (`prep_fit_inputs.R`, ~5 min/year, ~850 MB each,
rebuilds ACS margins from the raw extract) stays as a first stage; its
outputs go under `TAXDATA_SCRATCH`, which is not in `~/.Renviron` and is set
per job — worth adding.

## 4. The reader in Tax-Simulator

1. **Interface declaration.** `State-Weights: {type: model_data, version: 1,
   default_vintage, default_id: baseline}` in `interface_versions.yaml`. The
   loop at `config_parser.R:130-153` then supplies `dep.State-Weights.*`.
2. **Make the existence check optional for it.** `config_parser.R:214-220`
   stops on any missing declared path, which would fail every federal-only
   run until a default vintage exists. Skip the check for `State-Weights` on
   rows with no `states`, or require a default vintage to be published
   before the key lands. (D5.)
3. **The method.** `build_state_weights(..., method = 'interface', root)`:
   `fread(file.path(root, sprintf('state_weights_%d.csv.gz', year)))`;
   `semi_join` to `tax_units$id` (the subsample); `weight / globals$pct_sample`;
   then — **before** filtering to the requested states — assert every
   `tax_units` id is present and Σ over all jurisdictions equals
   `tax_units$weight × pct_sample` within 1e-6; then `filter(state %in%
   states)`. A missing year file stops. Drop the dead `calibration` /
   `gradient` method names.
4. **Call site** (`run.R:433-438`): `method` from `scenario_info` — `interface`
   when the row declares a State-Weights vintage, else `placeholder` with a
   warning; `root = scenario_info$interface_paths$'State-Weights'`. No
   signature change, so no `slurm/worker.R` edit.
5. **Coherence check.** The State-Weights vintage's `dependencies.csv` Tax-Data
   row must equal the runscript row's `dep.Tax-Data.vintage`; stop otherwise.
   This is the id-coherence guarantee, and it is what makes the
   `replace_na(0)` in `get_state_totals()` safe.
6. **Frozen across scenarios.** Extend `validate_runscript_states()`
   (`config_parser.R:465-507`): every row with `states` uses the baseline
   row's State-Weights vintage. Weights are a baseline product (S30); a
   counterfactual with different weights would score geography movement as
   policy.
7. **Test** beside `src/tests/state/test_state_calc.R`: a two-record,
   three-state fixture exercising coverage failure, Σ failure, pct_sample
   rescale, and the state filter.

## 5. Provenance the interface must carry

Two inputs are read from unversioned raw stores with no pin: HT2
(`raw_data/IRS-Ind/state/HT2/ht2_{year}.csv.gz`, via `ht2_path()`) and the
ACS extracts (`raw_data/ACS/acs_common_v2/us{year}a`). The non-filer anchors
and netting are committed research CSVs (`nonfiler_residual/results/`,
`nonfiler_pool/results/`) that must stay consistent with the pinned
ASEC-Nonfilers vintage, and nothing checks that. For now the producer writes
each HT2 file's md5 and the ACS extract's directory listing hash into
`dependencies.csv`, and the repo commit of the anchor CSVs into the manifest;
the durable fix — a versioned IRS-Ind interface — is out of Phase 4's scope
and belongs to the revamp.

## 6. Build order, and what waits on the top-tail decision

| Step | What | Depends on values? | Acceptance |
|---|---|---|---|
| **P1 — done 2026-09-28** | Tax-Simulator baseline runscript for the split (`config/runscripts/tests/state_weights_baseline.csv` there, commit `1a067ed9d`), run on `2026092720_filer_refit_v2`, years 2017–2023, full sample, detail kept; vintage **`sw_base_2026092720_filer_refit_v2`**, published to the shared root (1.0 GB; Slurm 27789830, 5 min) | no | ✅ seven detail files; `dependencies.csv` pins v2; per year the detail's id set and weights are identical to the live Tax-Data file's, no NA `agi`/`eitc`. Caveat: built on Tax-Simulator `a7dc6c604`, four commits behind `origin/main` (the childless-EITC fix); P6's baseline is rebuilt anyway |
| **P2 — done 2026-09-28** | Tax-Data: the `Tax-Simulator` interface key, `config/runscripts/state_weights.yaml`, `src/state_weights_contract.R` (+ `src/tests/test_state_weights_contract.R`), `resolve_interfaces()` in `src/resolve_interfaces.R` extracted from `configure.R`. One deliberate semantic change: "declared but unpinned" is now judged against all runscripts in `config/runscripts/`, not the one being run | no | ✅ old and new `configure.R` evaluated with writes stubbed: `output_path`, `interface_paths` (8) and the `dependencies.csv` table (8 rows) identical; the contract test's 10 checks pass, including P1's baseline accepted and `phase0_s33` refused for its s33 pin; the new runscript resolves both pins |
| **P3 — built 2026-09-29, certification running** | `warm_start_theta()`, `fit_filers_chain_B()`, `fit_filers_chain_C()`, `assert_split_shares()` and `emit_split()` (the W-level invariant; `build_split_weights()` now emits through it) in `src/nonfilers/sw_fit.R`; `phase3_fit.R` is a driver over them (arm B now fits 2017 itself instead of copying A's file). `scripts/phase3_compare_fits.R` compares two fit directories `identical()`-ly; `run_phase3_certify.sbatch` runs the driver into `phase3_cert/` and compares | no | Slurm 27854870 (B, ~6 h) then 27854871 (C_10_warmB, ~11 h): every year's P, theta, ids and jurisdictions must be `identical()` to the S39 fits |
| **P4** | `main_state_weights.R`, its yaml, the writer (files, manifest, dependencies, README), the invariants; a **dry run** publishing a provisional vintage from arm B on 2022–2023 only (~2 h) | no for the plumbing; the *published weights* are provisional | invariants hold; manifest sums match files; publish guard refuses a second write |
| **P5** | Tax-Simulator reader, config, coherence and frozen-vintage checks, test; dispatcher off `placeholder` | no | a state run on P4's vintage completes; state totals for the two years equal Σ `W × x` computed independently in R |
| **P6** | The production publish: the full chain on the final national weights — **this is the Phase 3 re-run** — then the Phase 1 battery per state (targeted within 2%, held-out HT2 MARD, Kish ESS) as acceptance | **yes** | waits on the top-tail decision; the plan's S39 metrics are the bar |

P1–P5 are this week's work and leave Phase 4 as a run, not a build. P6 is
one Slurm submission once the weights are settled.

## 7. Decisions for JI — all seven accepted as recommended (JI, 2026-09-28; S42)

| # | Decision | Options | Recommendation |
|---|---|---|---|
| D1 | Where the weights live | (a) their own `State-Weights` interface with its own vintage · (b) inside the Tax-Data vintage next to `tax_units_{year}.csv` | **(a)**: S30 wants both passes visible; (b) hides the baseline dependency and ties the weights' vintage to the file's |
| D2 | Which years the producer emits | all 81 years (2024+ by `w_y × P_2023`) · fit years only, reader carries forward | **all years** if storage allows — dense softmax shares make ~30M rows/year long, ~150 MB gz, ~12 GB a vintage, comparable to the Tax-Data vintage itself; otherwise fit years + a reader rule, which puts logic in the reader |
| D3 | TY2017's missing PR column | PR mass into OA · fit 2017 with a zero PR column | **into OA** (no state computation changes) |
| D4 | The dry-run arm for P4/P5 | B on 2022–2023 (~2 h) · the full chain | **B on two years**: exercises every code path at 10% of the cost |
| D5 | The existence check for a not-yet-published interface | skip for rows without `states` · publish a default vintage first | **skip**: a federal run must not depend on state weights existing |
| D6 | File format | `.csv.gz` · parquet | **`.csv.gz`**: `fread` reads it natively; the repo's Arrow build lacks zstd |
| D7 | `configure.R` | extract `resolve_interface_paths()` · duplicate the path logic in the new entry point | **extract**, certified by `main.R`'s unchanged `dependencies.csv` |

## 8. Found on the way (housekeeping, not Phase 4)

- Four research scripts read `interface_versions.yaml$Tax-Data$default_vintage`,
  a key that no longer exists: `scripts/build_state_weights.R:42-46`,
  `nonfiler_pool/06_acceptance.R:106`, `scripts/f1_adult_target_check.R:31`,
  `nonfiler_residual/03_diagnose_current_nonfilers.R:55`. All four fail on
  first use.
- `src/main_placeholder.R` and `src/main_accruals_validate.R` source neither
  `nonfiler_contract.R` nor `refit_filers_2017.R`, the omission that crashed
  `main_phase12_snapshot.R` once; `main_phase12_snapshot.R:75` writes to an
  absolute path under another user's home.
- `TAXDATA_SCRATCH` is unset in `~/.Renviron`; every Phase 3 job set it by hand.

## Revision history

- **2026-09-29** — P3 built; certification against the S39 fits submitted.
- **2026-09-28 (later)** — D1–D7 accepted as recommended (S42); P1 run, verified
  and published; P2 built and certified.
- **2026-09-28** — Written (JI's session), from a two-repo read at Tax-Data
  `c965523` / Tax-Simulator `a7dc6c604`.
