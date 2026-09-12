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

## Closed after the 2026-09-11 review

- **S19 reached Tax-Data's output (finding 1).** The 2017 pool aged on the
  PEP-basis band series landed 2023 at 38.26M non-filer adults against the
  41.23M the handoff partition requires on CBO's Social Security area
  universe; the producer's per-band ssArea scale lived only in the 2023 pool
  file, which this pipeline never read. `project_puf.R` now pins the
  non-filer band level from 2023 on to `ssarea_alignment_2023.csv` (since superseded by S21's `ssarea_partition_2023.csv`, next item; shipped
  in the pinned vintage) and uses the S18(b) series for growth relative to
  its 2023 value; years through 2022 are unchanged. The partition is asserted
  (non-filer side exact) and printed (full identity, soft) after the weight
  ledger is built, and written to `ssarea_partition_2023.csv` in the output.
- **End-to-end run with the anchor: vintage 2026091113** (job 25945655,
  2026-09-11, 3h16m, 16 cpu / 384G with cached fits; peak memory 380 GB of
  384 -- request more next time). Against 2026083111: 2017, 2020 and 2022
  identical on every filer and non-filer aggregate; 2023 non-filer adults
  38.26M -> 41.23M, the S19 target exactly, with the partition printed and
  written (`ssarea_partition_2023.csv`: non-filer side to 1e-16 per band,
  full identity +0.70%). Filer weights and `factor_ledger` are identical in
  every year. Filer dollar aggregates move at the sixth significant figure
  from 2023 on (2023 wages -6e-7) through one channel: the Forbes billionaire
  splice recomputes its receiver weight adjustments on each year's
  materialized file, whose non-filer weights changed, so its receiver set
  differs slightly from 2023 (2022 identical). Not a defect of the anchor;
  worth knowing when reading any 2023+ filer diff against older vintages.
- **S21 built (2026-09-11):** the per-band S19 scale on the 2023 pool is
  retired; `15_ssarea_alignment.R` builds the partition on the Social Security
  area universe for every year 2017–2023 with a named not-in-universe block;
  vintage **2026091201** published (2017–2022 byte-identical to 2026090101,
  2023 unscaled at 37.45M adults) and pinned; `project_puf.R` pins the 2023
  non-filer level to the built 2023 pool. Non-filer adults now run 35.05M
  (2022 pool) → 37.45M (2023), the observed +6.8%, with no universe step.
  Caveat found while building: the block is Jan-1/Jul-1 timing + series
  vintage + geography, negative in some bands before 2022 (see the S21 entry).
- **HT2 reaches TY2023.** SOI published Historic Table 2 for TY2023 in
  August 2026; it is mirrored at `raw_data/IRS-Ind/state/HT2/ht2_2023.csv.gz`
  (2026-09-08). The state ceiling and the national ceiling now coincide at
  2023, so "2023 national, 2022 with state products" is no longer the
  constraint -- the anchor pair (2017/2022) has simply not been rebuilt yet
  (Tax-Simulator decisions log S18, premise note).

## Block E (branch `block-e`, 2026-09-11) — design A, the union base

- `impute_nonfilers.R` appends every pool year in the pinned vintage (from
  the manifest), each in its own id block (`offset_nonfiler_ids()`), tagged
  `base_year` / `weight_own`; `weight` is the 2017 weight (zero for later
  pools). D4 extended: one record set, unique ids, filer order untouched.
- `materialize()` applies `factor(y) / factor(base_year)` (tests 17–18).
- `project_puf.R`: the year-y pool carries its own weight in y and zero
  elsewhere; the 2023 pool carries 2023+ on the S18(b) series relative to
  2023; the S21 partition is asserted for every pool year (non-filer side
  equals the pool, hard; full identity printed) and written to
  `ssarea_partition_check.csv`. The morning's 2023 handoff anchor is gone.
- `main.R`: wealth imputed for records with positive weight in some year
  ≥ 2022 (filers + 2022 + 2023 pools); other pool records get 0, not NA.
- Known limitation: later pools' nominal dollars rank against 2017 cut points
  in Phase 1 imputations (deflate-at-append is the fix, blocked on having the
  income factors before the append).

**Verified end to end: vintage 2026091119** (job 25972897, 2h22m55s, peak
685 GB of 900 GB requested; 16 cpu, cached fits).

- **Every year reproduces its pool exactly.** Non-filer units, adults AND
  wages in `tax_units_{year}.csv` equal the published `nonfiler_pool_{year}`
  for all of 2017-2023 -- wages too, which is what proves the base-year
  factor division. 2025 and 2040 carry the 2023 pool's 168,785 records.
- **Composition now moves with the year** (the Block F test): mean age 51.9
  (2017) -> 54.1 (2019) -> 55.1 (2020) -> 52.0 (2022) -> 52.2 (2023); 65+
  share 0.305 -> 0.352 -> 0.348 -> 0.300 -> 0.313; share with dependents
  0.161 -> 0.145 -> 0.163; interest receipt 0.139 -> 0.162. Every one of
  these was frozen at its 2017 value before.
- **Filer PUF-NATIVE aggregates are bit-identical** at 2017 and 2020 to the
  S21 run (vintage 2026091118): wages, interest, dividends, gains, Social
  Security, sole proprietorship, and every filer weight. Those columns are
  read from the PUF and scaled by deterministic factors, so the union base
  cannot touch them.
- **Filer IMPUTED variables DO move**, and this is the cost the review brief
  predicted under "what design A does not fix". Phase 1's draws are
  positional, and tripling the base changes how many each module consumes,
  so every module downstream of the first sees a shifted stream. Measured at
  the record level by `05_preflight_vintage.R` (46 of 188 columns at 2017)
  and in aggregate against the S21 run:

  | filer aggregate | 2017 | 2023 |
  |---|---|---|
  | `wagebill_sole_prop` | −3.9% | −3.4% |
  | `care_exp` | +2.0% | +2.2% |
  | `auto_int_exp` | −1.3% | −1.2% |
  | `tips` | +0.7% | +1.3% |
  | `kg_lt_basis` | +0.6% | +1.1% |
  | consumption categories | ≤0.9% | ≤0.5% |
  | `wages1` / `wages2` | ≤0.03% | ≤0.04% |

  Monte Carlo noise from a reshuffled stream, not a bias -- but the QBI wage
  bill and childcare expenses route real tax, so the E2 tripwire will read
  this as filer-side movement under a non-filer-only change. **Keying the
  Phase 1 draws by record id is what closes it**, and it is the one
  follow-up that should land before merge. An earlier note in this file
  claimed the filer side was bit-identical; it is not, and this table
  replaces that claim.
- **Tax-Simulator's contract holds.** After `filter(id %in% sample_ids)`
  every year has exactly 1,399,234 rows in identical order to 2017, so
  `bind_cols(random_numbers)` pairs the same record with the same draw in
  every year -- the property design A exists to preserve.
- **Cost:** 930 MB per year against 320 MB, 78 GB total against 27 GB;
  about 1.02M zero-weight rows per file; wealth ran on 548,807 records of
  1,399,234. Runtime was *shorter* than the 374,630-record baseline
  (2h23m against 2h50m), on a different node.
- **Pre-existing defect surfaced, not caused, by the id checks:** the Forbes
  synthetic billionaire rows (749 from 2022, 935 from 2025) are absent from
  the 2017 file `sample_ids` is taken from, so Tax-Simulator drops all of
  them. True on `main` too. Recorded in `SERVER_TODO.md`.

## Deliberately left undone

- No filer dimension on `factor_ledger` income growth (S18(a)): non-filer
  income still grows at filer-derived rates. Future work: differential wage
  growth across the income distribution — blocked on a distributional wage
  series, not on a decision.
- The dep_age_group quirk above.
- Extending income factors past 2022 (upstream).
