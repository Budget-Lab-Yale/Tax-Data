# `asec-nonfiler-pool` — draft for review, not for merge

Three thematic commits, reviewable independently and in order. Raised from
Tax-Simulator branch `state-tax`, where the replacement non-filer population
is built, calibrated and tested (`research/state_weights/nonfiler_pool/`);
the map of that work is `research/state_weights/handoff.md` there.

## Commit 1 — Restrict the extensive-factor mask to filers

A standing defect on `main`, independent of the swap: the per-variable
extensive factor ran over all records, so non-filers reached filer growth
factors. DINA carries wages and pensions, so those masks are contaminated
*today* (+0.34% / +0.39% factor bias by 2025, +1.34% / +1.57% by 2055).

**Review question:** accept that filer totals move relative to `main` by
exactly this correction. Diagnostic:
Tax-Simulator `research/state_weights/nonfiler_pool/11_extensive_factor_contamination.R`.

## Commit 2 — Filer weights on observed IRS counts through 2023; S18(b) non-filers

- `resources/return_counts_2023.csv`: Pub 1304 T1.6 by status × age,
  2017–2023. Gated: 2017–2019 reproduce `return_counts_2019.csv` exactly
  (all 72 cells). Returns per adult .5991→.6101→**.6314 (2020)**→.6142→
  .6118→**.6008 (2023)** — the demographic handoff now lands after the
  pandemic filing spike has reverted, not before or inside it.
- `resources/nonfiler_weight_targets.csv`: one cumulative factor per
  (band, year), 2018–2097 — observed residual counts of non-filing adults
  through 2023 (40.8M in 2020 against 48.5M in 2017; no demographic factor
  produces that), then a 10-year phase from the 2023 residual share to its
  2017–2019 norm on the CBO band population. `factor_phase5` is the 5-year
  variant, same file.

**Review questions:** the 10-vs-5-year phase choice; the stated assumption
that the claimed-dependent netting share within band is stable; the
weights@2023 / income-factors@2022 split (income factors are upstream in
Compiled-SOI-Tables and unchanged here).

**Flagged, not fixed — the dep_age_group join quirk.** The person-slot
demography pivots `dep_age_group1-3` (codes 1–4, not ages) into the `age`
column it joins population factors on, so a dependent matches the factor for
age 1–4. Pre-existing on `main`, affects every record with dependents, filers
included. Replicated here because fixing it changes filer results and the
right fix (exclude dep slots vs map codes to representative ages) is a repo
owner's call.

## Commit 3 — Read the constructed ASEC non-filer pool instead of DINA

- `impute_nonfilers.R` collapses to read + assertions + bind. The `runif()`
  age draws and hard-coded dependent probabilities go — ages, sex and
  dependents are observed columns. All seven assertions verified against the
  published file (`ASEC-Nonfilers` v1).
- `demographics.R`: non-filer sex is no longer blanked and redrawn (S14).
- `DINA` interface entry **stays**: `demographics.R` still uses its sex split
  for the `filer = 1` cells.

**Review question:** reproduce the `male1`/`male2` distributions against
`main` before merging — they route EITC and CDCTC.

## Mortality: no change needed, and why (write-up, not code)

Ages are never incremented; cells are rescaled to each year's population. A
record is an **age slice** (age-a people in year y), not an aging cohort.
`q_death = q_baseline(age, sex, year)` — frozen age, actual year — is the
*correct* lookup under that semantics, and the CBO cell populations already
carry deaths on the weight side (cohort ratios track CBO survival to within
0.5pp at ages 75/80/84, 2030 and 2050). q_death never scales weights in this
repo. **Do not "fix" frozen ages into cohort aging** — that would
double-count mortality. The ledger covers appended ids (no filer filter) and
now sees observed rather than redrawn non-filer sex.

## Before merging

Run the pipeline end-to-end on a sample and check, in order: filer totals vs
`main` (must be attributable to commits 1–2 exactly), `male1`/`male2`
distributions, then the non-filer aggregates against the acceptance battery
in the Tax-Simulator bundle.

## Deliberately left undone

- No filer dimension on `factor_ledger` income growth (S18(a)): non-filer
  income still grows at filer-derived rates. Future work: differential wage
  growth across the income distribution — blocked on a distributional wage
  series, not on a decision.
- The dep_age_group quirk above.
- Extending income factors past 2022 (upstream).
