---
title: "Per-year national filer refit (S40) — design and open forks"
role: notes
workstream: state_weights
status: open
updated: 2026-09-27
sot: research/state_weights/plan.md
supersedes: []
superseded_by: null
---

# Per-year national filer refit (S40) — design and open forks

Phase 3b step 3 (`research/state_weights/plan.md` §5a). The targets exist:
`resources/filer_refit_targets.csv` (3,453 targets over TY2017–2023, about
490 a year), built and gated by
`research/state_weights/filer_refit/01_refit_targets.R`. This note is the
design for fitting TY2018–2023 filer weights to them, with the forks that
need JI's call before code.

## What changes

`compute_weights_for_year()` (`src/project_puf.R`) gives each 2017 filer, for
2018–2023, its 2017 weight times one factor per filing status × age group
(Pub 1304 Table 1.6 counts relative to 2017). The refit keeps that as the
**prior** and adjusts each year's filer weights to hit, per year:

| Source | Targets / year | What it adds |
|---|---|---|
| `t16` | 423–443 | status × age × AGI class (combined cells as blocks) — the core |
| `t16_u18` | 12–18 | under-18 returns by AGI — the only age split the status blocks lack |
| `t14_wages` | 19 | returns with wages by AGI |
| `t17_all`, `t17_wages` | 12 + 12 | dependent returns, all and with wages, by AGI — where the TY2023 excess sits |

Non-filers are untouched (their weights are the pool's own, S22), so the S21
partition — filing adults + pool adults + netting = SSA-area adults — still
holds as long as the status × age totals are hit (they are targets).
Filers after 2023 grow from the 2023 weights, so the refit reaches every
projected year.

## The three things that make it harder than the 2017 LP

1. **No AGI on an aged record.** The 2017 LP classes records by the PUF's
   `E00100`. An aged record has only components. 2017 AGI is recoverable
   exactly — `process_puf.R` defines `other_inc` as AGI less its published
   components — but two components need tax calculation in later years:
   taxable Social Security (`E02500`; the file carries gross) and the
   capital-loss limit. Both live in Tax-Simulator.
2. **Incomes depend on weights.** From 2020 each variable's intensive factor
   divides by an extensive factor computed from the weight ledger
   (`build_factor_rows_2020plus()`), so reweighting changes incomes, which
   changes AGI classes.
3. **Scale.** 207,692 filer records × ~490 targets × 6 years. The 2017 LP
   (`lpSolve`, minimize Σ|δ| within tolerance bands) solves ~1 year once;
   run time at this size is unmeasured.

## Forks for JI

**F-A. Fit method.** (i) The 2017 LP (`reweight_lp()`): consistent with the
base year, but its solutions sit at corners — many records at their bounds.
(ii) Calibration: minimize the divergence from the prior (entropy/raking)
subject to the targets — smoother weights, closer to what Phase 1 chose for
the state split. *Recommendation:* build both and bake them off on the same
targets (the pattern of Phase 1 and Phase 3). Separating metrics: held-out
fit (Table 1.4 counts by AGI for items not targeted — interest, dividends,
pensions, Social Security, self-employment — and HT2's national sums), weight
quality (Kish effective sample size, max weight ratio to prior), and
year-over-year weight change of a persistent record.

**F-B. AGI on aged records.** (i) A proxy: the 2017 AGI identity with
taxable Social Security at the record's own 2017 taxable share of gross and
capital gains uncapped. (ii) Tax-Simulator's computed AGI on the aged file —
exact, but a cross-repo loop (Tax-Simulator runs on Tax-Data's output).
*Recommendation:* (i), validated two ways: in TY2017 against `E00100`
(class agreement of proxy vs true AGI, record by record), and in TY2023 by
comparing proxy-class counts against a Tax-Simulator AGI run on the current
vintage, once.

**F-C. The weight–income loop.** Fixed-point iteration: fit on the current
factors, rebuild factors from the new weights, reclassify, refit; stop when
class counts move < 0.1%. *Recommendation:* iterate, expecting 2–3 rounds
(the extensive factors move by count, not by amount).

**F-D. Tolerances.** The 2017 LP uses ±0.5% on AGI classes and ±1% on
status totals. Table 1.6 blocks in thin cells are small numbers.
*Recommendation:* ±0.5% for the status × age totals (so the S21 assertion
holds), a floor in returns for small cells, and report per-source misses.

## Order of work

1. AGI proxy + its TY2017 validation against `E00100` (needs the Phase 1
   `puf` object, so a Slurm job at the pipeline's memory figure).
2. The two fit arms on one year (TY2023), prior → targets, scored.
3. All six years, with the F-C loop, for the chosen arm(s).
4. Before/after diagnostics per `notes/low_income_filer_aging.md` — the
   TY2019 and TY2023 low-income wage filer tables, dependents, and the
   Affordability-Index tabulation — for JI's review; then the Phase 3 re-run.
