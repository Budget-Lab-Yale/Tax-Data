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

## Findings, 2026-09-27

### The AGI proxy works (F-B settled)

`src/agi_proxy.R`, scored by `filer_refit/02_agi_proxy_check.R` against
Tax-Simulator's own AGI on the same records (baseline run `phase0_s33`, on
Tax-Data vintage `2026092220_s33_scratch`; `E00100` is no benchmark, since
`create_2017_puf.R` rescales the components but never ages it):

| TY | same Table 1.6 AGI class | within $100 or 1% | same class, AGI < $25k |
|---|---|---|---|
| 2017 | 99.0% | 97.0% | 99.0% |
| 2019 | 99.0% | 97.0% | 98.9% |
| 2023 | 98.9% | 96.4% | 98.9% |

(weighted filers; the note's total-income proxy scores 80–81% on the same
class and 72% below $25k). Taxable Social Security matches on 99.0% of
records, net capital gain on 100%, above-the-line deductions on 98–99.5%.
The main residual is by design: Tax-Simulator keeps only a *net gain* of
partnership + S corporation income, the proxy the allowed net either way
(Schedule E $767B against $889B in TY2017).

### The 2017 base weights are bang-bang (changes F-A, and the scope)

Scored by that same AGI, the TY2017 file's returns by class miss Table 1.4
in an alternating pattern inside each targeted $10k band — $1–5k 13.5M
against 9.75M, $5–10k 7.2M against 10.8M, $10–15k 15.4M against 11.6M,
$15–20k 6.6M against 10.7M — while the coarse bands the 2017 LP targets
($1–10k, $10–20k, $20–30k, `config/target_info/baseline.csv`) come out
right (20.7M against 20.5M). The cause is the LP itself:
`resources/cache/lp/weight_deltas.rds` puts **every one of the 207,692
records at a bound — 62% at ×0.34, 38% at ×1.66, none at 1**, because the
objective `set.objfn(lprw, rep(1, nrow(lhs)))` (`src/reweight.R`) minimizes
the *sum* of the multipliers, not their distance from 1. Inside a
constraint cell the choice of which records get ×1.66 is arbitrary with
respect to anything untargeted, and neighbouring records' weights differ
~5×. Records per $1k of AGI are smooth; weight per record steps at the
fine class bounds.

Consequences:
- **F-A.** The LP arm as written is out: it would reproduce this pattern
  every year. An LP arm needs a real objective — minimize Σ|δ − 1| (split
  positive and negative deviations) — or the calibration arm alone.
- **Scope.** Every later year's prior inherits the 2017 weights, so a
  2018–2023 refit starts from them. The targets file already covers TY2017;
  refitting TY2017 by the same method (from the pre-LP weights) would fix
  the base, and change every year of the file. JI's call.

## Order of work

1. AGI proxy + its TY2017 validation against `E00100` (needs the Phase 1
   `puf` object, so a Slurm job at the pipeline's memory figure).
2. The two fit arms on one year (TY2023), prior → targets, scored.
3. All six years, with the F-C loop, for the chosen arm(s).
4. Before/after diagnostics per `notes/low_income_filer_aging.md` — the
   TY2019 and TY2023 low-income wage filer tables, dependents, and the
   Affordability-Index tabulation — for JI's review; then the Phase 3 re-run.
