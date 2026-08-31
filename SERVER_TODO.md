# Server-side todo — non-filer handoff, Tier 0/1 verification and the annual rebuild

**Written** 2026-08-31, off-cluster.
**Repos** `Tax-Data` @ `asec-nonfiler-pool` (981a695 + uncommitted), `Tax-Simulator` @ `state-tax` (064b54f67 + uncommitted).
**Nothing is committed in either repo.** Review the diffs before you push anything.

**Executed on-cluster 2026-08-31** (both branches since committed and pulled). Status:
- **A1, A2, A3 PASS.** Contract test 20/20 on R 4.4.2; config resolution clean. A3 (job
  24303124, ~3.5h on 16cpu/384G, cached fits): every checkpoint green — configure.R
  resolves vintage 2026090101, non-zero append (166,938 records / 31.19M units / 81
  zero-filled), drop line printed (now incl. wages1/2, sole_prop1/2 — see below), past
  demographics.R, ran to completion, wrote Tax-Data vintage **2026083111** (97 files).
  Output spot-check: non-filer interest receipt 13.9% / dividends 3.8% (DINA: exactly
  0.0%), wages $281.8B = the emitted pool exactly, male1 observed (0.482 vs filer
  0.618, no NAs). Two failed attempts first, both real findings: quantregForest/drf
  missing from the 4.4.2 module bundle (installed to ~/r_libs_4.4), and the pool's
  observed wages1/2, sole_prop1/2 are owned by earnings_split.R AFTER the append —
  added to the declared-and-dropped list (the contract test's original comment
  believed they existed at append time; corrected). **Block B skipped** — all six
  2020–21 intermediates survive in `results/`.
  Quality note for A4/A5 review: the wealth tilt's pct80to90 × senior cell exits at
  the iteration cap (max_rel 0.159) rather than converged; check against a main-branch
  log before attributing it to this branch.
- **C2: 2020 and 2021 are INFEASIBLE, stop rule honored.** Band 18_25's fixed
  contributions (above-threshold hazard at the held Pub 5785 level + GQ) exceed the
  residual target: 2.090M vs 1.621M in 2020 (−0.469M), 2.298M vs 2.202M in 2021
  (−0.096M). The stimulus filing spike collapsed the young-adult residual faster than
  the held hazard allows. Feasible years' slack in that band was already thin
  (+0.139M in 2019, +0.129M in 2022). Substantive C1 answer, not the benign one;
  unblocking them needs a decision on the hazard's identifying restriction (JI).
- **Block D DONE** — S19 scale applied in `05_emit_pool.R` at the 2023 emit, gated
  pre/post against the alignment table (residuals ~1e-8 adults); audit written and
  published; `06_acceptance.R` detects and divides the scale back out for its
  PEP-basis comparisons. Doc fixes (wedge range, "merge time") landed.
- **C3/C4 DONE for the five feasible years**: acceptance 2023 passes (4/5, wage mass
  untestable); vintage **2026083101** published with 2017–2019, 2022, 2023 under
  `{vintage}/historical` + manifest + S19 tables, `div_pref` schema; `baseline.yaml`
  pin updated. Three stragglers of the qual_div→div_pref rename fixed in
  `06_acceptance.R`, `11_…contamination.R`, `12_aging_check.R`.
- **2020–21 RESOLVED via S20** (2026-08-31, later the same day): the hazard level is
  deflated per band by observed pandemic excess filing
  (`16_pandemic_filing_adjustment.R` → 02 → 04; decision S20 in the log). Both years
  calibrate, pass acceptance (pool better on every testable dimension), and are
  published in vintage **2026090101** with all seven years — the five non-pandemic
  files byte-identical to 2026083101. Pin updated to 2026090101. The aging check now
  runs for 2020–21: aged-from-2017 misses the built 2020 by −23.3% adults / −33.5%
  wages, the strongest S18(c) evidence yet.

Full evaluation and the deferred tiers: `~/.claude/plans/find-the-document-entitled-atomic-codd.md`.
Cross-repo map: `Tax-Simulator/research/state_weights/handoff.md`. Branch review: `NONFILER_BRANCH_NOTES.md`.

---

## Read this first — the ordering constraint

`configure.R` now hard-fails if any declared interface path does not exist. The currently
published pool (`ASEC-Nonfilers/v1/2026082801`) has **no scenario subdirectory**, because the
producer wrote `.../v1/{vintage}` while the consumer resolves
`.../v1/{vintage}/{scenario}`. That mismatch is fixed on the producer side, which means:

> **You cannot run Tax-Data's `main.R` until the pool is republished under the new layout.**

So the order is forced: **republish (Block C or the C-lite shortcut) → update the pin → run
Tax-Data**. Block A's steps 1–2 do not depend on this and can go first.

If you want a fast smoke test before committing to the full rebuild, do **C-lite**: republish
2017 alone under a new vintage, then run Block A step 3. That exercises every Tier 0/1 fix
without waiting on 2020–21.

---

## Environment

- R is not on `PATH`. Load the module **in the same command** as `Rscript`:
  `module load R/4.4.2-gfbf-2024a` (the version `run_gq_backfill.sbatch` uses; local dev was
  4.5.2, so watch for version-sensitive behaviour).
- Submit from the repo root. `sbatch` pattern with logs:
  `sbatch --output=$HOME/slurm-logs/%x-%j.out <script> [args]`
- A "completed" job is not a successful one. Check the exit code **and** the success sentinel
  in the log (the existing sbatch prints `GQ_BACKFILL_SUCCESS`; add an equivalent to anything
  new you write).
- Do **not** create a project-local `.Renviron` — it masks `~/.Renviron` entirely.
- ⚠ Scratch was reclaimed and verified empty 2026-08-27
  (`/nfs/roberts/scratch/pi_nrs36/ji252/state_weights_tmp/`). Four scripts still point at that
  dead path (`sweep_state_weights.R:37`, `validate_state_weights.R:31`, `eitc_takeup_test.R:7`,
  `research/STATUS.md:437-438`). Anything you re-run there must be staged again, and this time
  preserve accepted artifacts **in-repo**, not in scratch.

---

## Block A — test the fixes we made

### A1. Contract test (no cluster data needed) — `Tax-Data`

```
module load R/4.4.2-gfbf-2024a && Rscript src/tests/test_nonfiler_contract.R
```

- [ ] Expect `20 passed, 0 failed` then `PASS`.
- [ ] It passed on R 4.5.2 locally. If it fails on 4.4.2, that is a real finding — note which
      assertion and whether it is the `tidyverse`/`data.table` version, not the contract.

This exercises `validate_nonfiler_pool()` in `src/nonfiler_contract.R` — the same function
`impute_nonfilers.R` calls. Negative cases include the `qual_div`-for-`div_pref` rename and the
empty-pool silent no-op, i.e. the two bugs that made this branch unrunnable.

### A2. Config resolution (no cluster data needed) — `Tax-Data`

- [ ] Confirm every declared interface resolves to exactly one path and that
      `ASEC-Nonfilers` is among them. Quick check:

```
module load R/4.4.2-gfbf-2024a && Rscript -e '
y  <- yaml::yaml.load_file("config/runscripts/baseline.yaml")
iv <- yaml::yaml.load_file("config/interfaces/interface_versions.yaml")
u  <- setdiff(setdiff(names(iv), "Tax-Data"), names(y$dependency_info))
cat("unresolved:", if (length(u)) paste(u, collapse=", ") else "<none>", "\n")'
```

- [ ] Expect `<none>`. `CPS-ASEC-Panel` was removed from `interface_versions.yaml` (declared,
      never given a vintage, read by no code) — if someone re-adds it without a vintage, this
      is what catches it.

### A3. Full pipeline run — `Tax-Data` (needs Block C or C-lite done first)

```
sbatch --output=$HOME/slurm-logs/%x-%j.out <your wrapper> ; # Rscript src/main.R
```

Verify in order — each is a specific fix:

- [ ] **Gets past `configure.R`.** If it stops with `Interface paths do not exist`, the pool
      has not been republished under `{vintage}/{scenario}` yet, or the pin in
      `baseline.yaml` does not match what you published. This check is *stricter than before*
      — if it flags a **legacy** interface (DINA, IRS-PUF, SIPP…), that is a pre-existing
      broken path my change surfaced, not a regression I introduced. Record which.
- [ ] **Gets past `impute_nonfilers.R`** and prints a **non-zero** record count:
      `impute_nonfilers.R: N records, X.XXM weighted units, K columns zero-filled`.
      Zero records here used to be the silent failure — it is now impossible (`nrow > 0` guard).
- [ ] **Prints the drop line**: `dropping observed male1, male2, age1, age2`. Expected and
      documented — see the `age1` decision below.
- [ ] **Does NOT stop on missing economic variables.** A stop mentioning
      `silently zero-filled` means a producer-side rename slipped through; fix the name in
      `05_emit_pool.R`, never by adding it to the drop list.
- [ ] **Gets past `demographics.R:28`** — this is the `dina_2017` fix. The old branch died here
      with `object 'dina_2017' not found`. The read now lives in `demographics.R` itself.
- [ ] Runs to completion.

### A4. Reproduce `male1` / `male2` against `main` — `Tax-Data`

The branch notes name this as the pre-merge gate, and it is still open. `male1`/`male2` route
EITC and CDCTC.

- [ ] Compare `male1`/`male2` distributions on this branch vs `main`.
- [ ] Filer-side values should move **only** by commits 1–2 (extensive-factor mask, filer
      weights). Any non-filer-side movement needs an explanation.
- [ ] Note: `demographics.R:94-99` still redraws `male2` unconditionally with a 1% same-sex
      rule, and `male1` is rebuilt from `GENDER`. That is **Tier 2 item 9, deliberately not
      fixed here** — so expect non-filer `male2` to be drawn, not observed. Do not "fix" it in
      this run; it changes the RNG stream.

### A5. E2 tripwire — the strongest check that the swap is non-filer-only

- [ ] Under current law, `totals/1040.csv` and `supplemental/cbo_comparison.csv` must be
      **identical** between baseline and the pool swap, because every 1040 dollar aggregate is
      summed `* weight * filer`. Any movement falsifies "non-filer only".
- [ ] Run this with commits 1–2 held fixed so their (expected) filer movement is not confused
      with the swap's.

---

## Block B — prerequisites for the annual rebuild

`04_calibrate.R` reads three inputs, and **two of them are gitignored**, so they exist only in
the cluster working tree:

| Input | From | Tracked? |
|---|---|---|
| `scored_units_{yr}.rds` | 02 | **no** |
| `units_{yr}.rds` | 01 | **no** |
| `gq_persons_{yr}.csv.gz` | 03 | **no** |
| `gq_backfill_summary_{yr}.csv` | 03 | yes |

Committed gates show stages 01–03 *did* run for 2020 and 2021 (`unit_gates_2020/2021.csv`,
`filing_model_gates_2020/2021.csv`, `gq_backfill_summary_2020/2021.csv`), but the microdata
may have been cleaned up since.

- [ ] Check what survives:
      `ls -la research/state_weights/nonfiler_pool/results/{units,scored_units}_202{0,1}.rds \
      research/state_weights/nonfiler_pool/results/gq_persons_202{0,1}.csv.gz`
- [ ] **If present** → skip to Block C.
- [ ] **If absent** → re-run for 2020 and 2021, in order. 01 and 02 are login-node safe; **03
      is not** (the ACS read OOMs the ~5G cap, hence 48G in the sbatch):

```
module load R/4.4.2-gfbf-2024a && \
  Rscript research/state_weights/nonfiler_pool/01_build_units.R 2020 2021 && \
  Rscript research/state_weights/nonfiler_pool/02_filing_model.R 2020 2021

sbatch --output=$HOME/slurm-logs/%x-%j.out \
  research/state_weights/nonfiler_pool/run_gq_backfill.sbatch "2020 2021"
```

- [ ] Re-running 01/02 must **reproduce** the committed `unit_gates_2020/2021.csv` and
      `filing_model_gates_2020/2021.csv` exactly. If they move, stop — something upstream
      changed (the `SCHLCOLL` extract addition is the obvious candidate) and that is a finding
      in its own right, not a nuisance.

---

## Block C — calibrate and emit 2020–21, then publish 2017–2023

### C1. The open question, before you touch anything

Nothing in the repo records **why** `04_calibrate.R` was never run for 2020 and 2021. Two
possibilities and they lead different places:

1. Benign — the `c(2017L, 2022L)` default was simply never overridden.
2. Substantive — 2020 is genuinely hard. The observed residual count of non-filing adults is
   **40.77M in 2020** against 48.47M in 2017, a ~16% drop no demographic factor produces, and
   `aging_check_summary.csv` has no 2020/2021 rows at all.

Note that "2020–21 refuse a projection outright" (`handoff.md:78`) is an argument **for**
rebuilding them directly, which is what makes the omission odd.

- [ ] **If calibration does not converge for either year, stop and report. Do not loosen a
      tolerance to force it.** A non-converging 2020 is a result about the pandemic year, not
      a bug to tune away.

### C2. Calibrate

```
module load R/4.4.2-gfbf-2024a && \
  Rscript research/state_weights/nonfiler_pool/04_calibrate.R 2020 2021
```

- [ ] Produces `calibration_2020.csv`, `calibration_2021.csv`.
- [ ] Both close the anchor identity (`pool adults + claimed-dependent netting = residual
      anchor`, per age band) to the **same tolerance the other five years meet** — compare
      against `calibration_2017/2018/2019/2022/2023.csv`.
- [ ] Sanity-check the 2020 level against the 40.77M residual, not against 2017.

### C3. Acceptance and distributions for the new years

```
Rscript research/state_weights/nonfiler_pool/06_acceptance.R 2020 2021 2023
Rscript research/state_weights/nonfiler_pool/09_distributions.R 2020 2021 2023
```

- [ ] `acceptance_{2020,2021,2023}.csv` written. Note 2023 was calibrated but never
      acceptance-tested.
- [ ] Pool should still beat DINA on every dimension. If 2020 does not, say so — that is the
      pandemic year and an honest negative result.

### C4. Publish 2017–2023 under one pinned vintage

Pick the vintage deliberately — it goes into `baseline.yaml` verbatim.

```
Rscript research/state_weights/nonfiler_pool/05_emit_pool.R \
  --publish --vintage=<YYYYMMDDNN> 2017 2018 2019 2020 2021 2022 2023
```

- [ ] Lands at `model_data/ASEC-Nonfilers/v1/<VINTAGE>/historical/` — **check the
      `historical/` level exists**; that is the path fix.
- [ ] `manifest.csv` present, with one row per year, plus row counts and adults.
- [ ] Seven `nonfiler_pool_{year}.csv.gz` files.
- [ ] **Update `Tax-Data/config/runscripts/baseline.yaml`** — `ASEC-Nonfilers.vintage` to
      `<VINTAGE>`. It currently pins `2026082801`, which predates the layout fix.
- [ ] Fix the README text while you are here: it still says *"TY2017 is the vintage Tax-Data
      consumes ... TY2022 is the validation artifact"*, which is the two-year posture S18(c)
      replaces.

### C-lite (shortcut, if you want to test Block A first)

```
Rscript research/state_weights/nonfiler_pool/05_emit_pool.R \
  --publish --vintage=<YYYYMMDDNN> 2017
```

Then update the pin and run Block A3. Exercises every Tier 0/1 fix; leaves the rebuild for later.

---

## Block D — apply S19 upstream to the 2023 handoff pool

S19's per-band ssArea scale (**1.0609–1.1337**) is computed by `15_ssarea_alignment.R`, written
to two diagnostic CSVs, and **applied nowhere**. Grepping both repos for `ssarea_alignment`,
`scale_to_ssarea`, `ssarea_wedge` returns zero code references. `handoff.md:89` says the pool
"takes [it] at merge time" — there is no merge step in code.

Consuming annual pools through 2023 makes the 2023 pool the handoff base, so this stops being
deferrable. Agreed approach: **apply upstream, in `05_emit_pool.R`, for the 2023 emit only** —
upstream keeps the "applied exactly once" property a downstream merge step cannot guarantee.

- [ ] Read `results/ssarea_alignment_2023.csv` (`scale_to_ssarea` by band) and apply per band
      to the 2023 pool's weights at emit time.
- [ ] Apply **exactly once** — assert it cannot double-apply (e.g. a flag column or a recorded
      pre/post total the emitter checks).
- [ ] Write a pre/post audit table: pre-scale count, post-scale count, scale factor, target,
      residual, per band and total.
- [ ] Every band and the total close to a predeclared tolerance.
- [ ] Add a downstream assertion in Tax-Data that filer + non-filer adults close to the
      handoff universe by band and in total.
- [ ] Minor doc fix: S19's stated wedge range "5.6–11.7%" is the **5-band** range; the 18_25
      band is 2.65%. Either say 2.7–11.7% or say explicitly which bands the range covers.

---

## Block E — consume annual pools in Tax-Data

Currently `impute_nonfilers.R` hardcodes `nonfiler_pool_2017.csv.gz`, and `project_puf.R` only
**reweights** those 2017 records for every later year — `project_puf.R:376-378` says outright
that ages stay at 2017 values. So composition is frozen and only six band scalars move. That
is what S18(c) says not to do.

- [ ] Add `load_nonfiler_pool(tax_year)` in Tax-Data, reading the year's file from the pinned
      interface and running it through `validate_nonfiler_pool()` (already factored out for
      exactly this).
- [ ] For years **through 2023**: replace the non-filer cross-section with that year's pool
      after projecting the filer population.
- [ ] For years **after 2023**: project the 2023 pool forward with documented factors.
- [ ] **Random draws must be deterministic and keyed by `(record id, tax_year)`** — *not*
      positional. `run.R:348-357` binds precomputed random numbers positionally, and a
      changing annual cross-section has exactly the silent re-randomisation hazard D4 exists
      to catch. This is the single highest-risk item in this block.
- [ ] Ids unique within a year and unambiguous across years. Do **not** imply longitudinal
      identity — these are independent ASEC cross-sections, not a panel.
- [ ] Reconcile the ceiling in docs while you are here: `decisions_log.md:56` (S18(c)) says
      "through 2022"; `handoff.md` says "2023 national, 2022 with state products";
      `14_nonfiler_weight_targets.R:57` sets `LAST_OBS <- 2023L`. Intent is coherent (2023
      nationally, 2022 where HT2 binds) but S18(c)'s headline sentence needs amending to say so.

---

## Block F — the annual accounting table

- [ ] For every year 2017–2023, producer vs consumer: rows, weighted units, adults,
      dependents, income totals by source, and age-band distribution.
- [ ] **The test that would have caught the 2017-base bug:** changing the annual input file
      must change **composition**, not only total weights. Compare mean age, `pct_65p` and
      income shares across years. If only the weights move, Block E did not land.

---

## Deferred — do not start these here

On the plan, with file:line evidence, but explicitly out of scope for this pass:

- **Tier 2 item 7** — `must_file` fires on business *losses*
  (`asec_tax_units.R:698`, `abs(se_income) >= 400` against IRS positive-net-earnings). The
  same `abs()` is duplicated in the sensitivity at `:709`, so the existing sensitivity cannot
  surface it. **Quantify before fixing**: one-line diff, then diff `must_file_M` against the
  committed 2017 value of **124.498M**.
- **Tier 2 item 8** — the filing model's eighth constraint (achieved mean income) is computed
  and never asserted; `solve_level()` runs *after* `tilt_to_mean()` and moves the mean back off
  target. `hazard_margins_{year}.csv` has seven rows and no mean-income row.
- **Tier 2 item 9** — observed sex destroyed (`male2` redraw, `male1` rebuilt from `GENDER`).
- **Tier 2 item 10** — `summary_stats.R:144` `n_adults` gives joint 1 / single 0, propagating
  to `n_people`. Fix before the federal A/B leans on these.
- **Tier 4** — state-weight KL weighting, the unreproducible config-7 bake-off (~80 min
  sbatch), the uniform 1/53 production placeholder.
- **`dep_age_group` quirk** — codes 1–4 joined as ages, so dependents match the age-1–4
  population factor. Pre-existing on `main`, affects filers too, owner's call
  (`NONFILER_BRANCH_NOTES.md`).

---

## Decisions I did not make for you

**The `age1` question.** Observed exact age currently reaches the model only as `age_group`;
`ages.R` redraws position within band from the CPS. Band membership is exact and the cut points
match `ages.R` (<26/<35/<45/<55/<65/65+), so the tax-relevant 65+ boundary is preserved.

Taking exact observed age instead means guarding `ages.R` to skip records that already have it
— which **changes its RNG draw count and therefore shifts filer ages**, contaminating the E2
tripwire. That is why the pool's `age1`/`age2`/`male1`/`male2` are dropped rather than
preserved, and the loss is documented in both repos rather than hidden. If you want exact ages,
the RNG-neutral route is to let `ages.R` draw for everyone as it does now and then *overwrite*
non-filer ages afterward — same stream, observed values kept.

**`src/tests/` is new in this repo.** `CLAUDE.md` says validation here is "bespoke,
interactive, and vibes-based" with no automated suite. My argument for the exception is narrow:
this is a cross-repo **schema contract**, not imputation correctness, and it is the class of
failure that has now bitten this interface twice. Your call whether it stays.
