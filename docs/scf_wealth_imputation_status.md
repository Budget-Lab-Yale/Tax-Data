# SCF Wealth Imputation — Status (2026-04-25)

Where the SCF → PUF wealth imputation stands after today's work. Summary of
architectural changes, diagnostic findings, and open questions. For full
design history see `../stage3_calibration_design.md` at repo root.

## TL;DR

Architecture flipped from a **single global forest** to **per-cell
(income-stratified) forests**. Stage 3 now covers 8 cells (added a split at
99.9). End-to-end pipeline produces aggregates that match SCF by construction
with rescale factors far tighter than before. The residual PUF over-
imputation (pre-calibration +44% = $62T) decomposes arithmetically into
three quantified sources that sum to the whole:

| source | $T | share of $62T gap |
|---|---|---|
| Pop-scale (188M PUF vs 170M SCF tax units) | 14.6 | 24% |
| Age × int_div marginal X-shift | 26.9 | 43% |
| Multivariate feature interactions | 20.4 | 33% |
| **Total** | **$62T** | 100% |

## The architecture

Naming convention used throughout this doc:
- **Global-forest scheme** (prior): one DRF trained uniformly on 29k SCF
  rows; `min.node.size=5, mtry=ncol(X)`; weight-proportional leaf draw at
  inference. In session shorthand this was "option b."
- **Per-cell scheme** (current): 8 DRFs trained on weighted bootstraps within
  income cells `[0, 20, 40, 60, 80, 90, 99, 99.9, 100]`. Per cell: 100k
  bootstrap rows (150k for the top 0.1% cell), `min.node.size=20, mtry=10,
  num.features=50`, uniform leaf draw at inference. In session shorthand
  this was "option a, per-cell."

Key code changes today:
- `src/imputations/stage3_target_qc.R`: `CALIB_INCOME_EDGES` extended to 99.9
  split; `assign_calibration_cells` now uses continuous rank (not quantized
  percentile) because 99.9 isn't an integer break.
- `src/imputations/wealth.R`: per-cell bootstrap loop + per-cell forest
  training + per-cell walk; uniform leaf draw at inference; combined
  `scf_boot` / `leaf_donors_list` preserve existing Stage 3 interface.
- Forests cached at `resources/cache/qrf/wealth_percell_*.rds` (~3.4GB, 8
  files). Training seed is `100 + cell_idx` where `cell_idx` ∈ 1..8 (i.e.
  seeds 101..108) — so any run on the same SCF input is bit-identical.

## Current results

### SCF self-consistency (`src/eda/wealth_percell_diagnostic.R`)

| metric | global-forest (old) | per-cell (new) | SCF truth |
|---|---|---|---|
| SCF aggregate self-test vs truth | +22.8% | **+0.4%** | 0 |
| top-1 share (SCF self) | 0.468 | **0.389** | 0.384 |

Per-cell forest is internally consistent on its own training data.

### PUF end-to-end (`src/eda/wealth_harness.R` under the new `wealth.R`)

| metric | PUF pre-tilt | PUF post-Stage-3 | SCF truth |
|---|---|---|---|
| Aggregate NW ($T) | 201.06 | **139.12** (exact) | 139.12 |
| Overshoot vs SCF | +44.5% | 0% (by construction) | — |
| top-1 share | 0.443 | **0.412** | 0.384 |
| top-0.1 share | 0.208 | **0.177** | 0.162 |
| top-10 share | 0.804 | 0.786 | 0.773 |
| Gini | 0.884 | 0.878 | 0.863 |

Post-Stage-3, top-1 share is +2.8pp over truth (vs +5.9pp pre-tilt); top-0.1
share is +1.5pp over truth (vs +4.6pp pre-tilt). The top-0.1 residual is
the more policy-relevant one because wealth-tax proposals typically bite
there.

### Rescale factor health

Old (global forest): **31 of 70 factors outside [0.5, 2.0]**; range 0.03 to
> 2. The rescale was papering over structural forest errors.

New (per-cell): **13 of 80 factors outside [0.5, 2.0]**; range 0.10 to
1.199; median 0.721, mean 0.700. No factor > 1.20 — the forest never
under-imputes, only over-imputes (by ~30–40% uniformly). Rescale is now
light calibration, not structural rescue.

## Decomposition of the +$62T PUF overshoot

Pre-tilt forest: $201T. SCF truth: $139T. Gap $62T. Arithmetic
decomposition (from `src/eda/wealth_reweight_scf.R`):

1. **Pop-scale: $14.6T** (24% of gap).
   SCF 170.4M tax units, PUF 188.3M. Gap = 17.9M tax units = 10.5%.
   $139.12T × (188.3/170.4 − 1) = $14.6T. Mostly one age bracket (see
   measurement-vs-structural section below).

2. **Age × int_div joint marginal shift: $26.9T** (43% of gap).
   Reweighting SCF to match PUF's (age × int_div) within-cell marginals
   raises aggregate from $153.75T (pop-scaled SCF) to $180.64T.

3. **Multivariate interactions on other features: $20.4T** (33% of gap).
   Gap between joint-marginal reweight ($180.64T) and full forest ($201.06T).
   Not attributable to any single feature marginal; represents interactions
   involving business, cap_gains, wages, ss_pens, family composition.

## Measurement vs structural — the most important distinction

Not all of the $62T overshoot represents real wealth. Some of it is a
definitional/measurement gap between SCF (survey) and PUF (tax forms). This
matters because measurement artifacts are candidates for **upstream
correction**, while structural differences are what Stage 3 rescale is
for.

### int_div: same dollars, different populations

- SCF total int_div: $630.3B; PUF total: $627.9B. **Aggregates identical.**
- SCF: 19.4% of tax units hold int_div > 0, mean $19,079 if positive.
- PUF: **29.8%** hold int_div > 0, mean $11,194 if positive.
- PUF has 53% more holders, each with 47% of SCF's average amount. Money is
  the same; holder count isn't.
- Almost certainly a **threshold/recall effect**: PUF's 1099-INT/DIV data
  catches every reportable dollar; SCF's survey captures "substantial"
  holdings. Small-dollar holders are in PUF but not SCF.
- Forest uses binary `has_int_div` — treats PUF's small-dollar holders the
  same as SCF's large holders, over-imputes wealth to them.
- **This is an artifact candidate for upstream fix.** Thresholding PUF's
  `has_int_div` at some dollar amount (say $500) would recalibrate PUF's
  share-positive toward SCF's 19%.

### Age: mostly a real frame difference

- PUF is +4.6yr older than SCF in top-0.1% cell; −3.7yr younger in pct00to20.
- Bimodal, not simply "PUF older." Reflects tax-unit vs household frame.
- Within-cell age-NW gradient is steep (up to 19× in top 0.1%), so age
  shifts translate to substantial wealth shifts.
- **This is mostly structural**, not measurement.

### Pop-scale: almost entirely one age bracket

- SCF 170.4M vs PUF 188.3M (gap 17.9M).
- 18-24 bracket: SCF 8.7M, PUF 21.5M (gap 12.9M = 72% of total pop gap).
- Other brackets all within ±13%.
- Census ACS 2022: ~30M persons aged 18-24. PUF 21.5M tax units is closer
  to Census; SCF 8.7M tax units is far below.
- Most likely explanation: DINA-nonfiler construction adds young adults as
  independent tax units, while SCF's household frame folds them into
  parents. **Not tested — see open questions.**

## Per-cell compositional decomposition

From `src/eda/wealth_compositional_decomp.R` — counterfactual swap of each
feature group to SCF's within-cell distribution. "Top-2 %" is the sum of
the two largest single-group gap contributions; omitted in cells where
single-group closures sum to > 100% from redundancy:

| cell | gap $T | #1 driver | #2 driver | top-2 % |
|---|---|---|---|---|
| pct00to20 | 1.76 | int_div 55% | age 39% | 95% |
| pct20to40 | 0.56 | int_div 75% | age 63% | — (overlap > 100%) |
| pct40to60 | 1.43 | int_div 143% | age 86% | — (overlap > 100%) |
| pct60to80 | 8.35 | int_div 60% | age 51% | 111% |
| pct80to90 | 4.70 | age 41% | cap_gains 19% | 60% |
| pct90to99 | 15.83 | int_div 37% | age 32% | 69% |
| pct99to99.9 | 14.78 | age 16% | ss_pens 12% | 27% (diffuse) |
| pct99.9to100 | 14.66 | age 37% | int_div 31% | 67% |

`pct99to99.9` is the anomaly — its gap lives in feature interactions, not
in any one marginal distribution. Full-X swap closes 83% there; individual
single-feature swaps sum to only 41%.

## Frame ambiguity

SCF $139T is **household-frame** aggregate (matches Fed Z.1 macro). PUF
tax-unit-frame aggregate under SCF's Y|X mapping is ~$180–200T depending
on how much X-shift you accept. These aren't the same concept — splitting
households into tax units redistributes wealth, doesn't multiply it.

Stage 3 rescale chooses the household-frame target ($139T) by construction.
This is the correct choice for tax-policy aggregates that need to match
macro anchors. But some of what Stage 3 "corrects" is real tax-unit frame
structure, not forest error. The stretching in Stage 3B is proportionally
mild (median factor 0.72, range [0.10, 1.20]) — acceptable for aggregate
use cases, lossy for joint cross-category individual-level analyses.

## What we settled today (2026-04-25)

### The pop-scale piece is filers, not nonfilers

Empirical decomposition of PUF's 21.5M tax units age 18-24 (`src/eda/nonfiler_intdiv_probe.R`):

- 19.34M filers + 2.17M nonfilers
- SCF total at 18-24: 8.65M
- Gap = 12.86M, of which **only 2.17M (17%) is the DINA-nonfiler imputation**

Nonfilers themselves skew elderly (12M of 28M total nonfilers are 65+). The 18-24 deficit in SCF is *filers* — young W-2 earners filing their own returns who SCF folds into parents' households. The DINA-nonfiler-as-culprit hypothesis is falsified.

**What this means for the $14.6T pop-scale**: largely a real tax-unit-vs-household-frame gap, not phantom population. Closing it requires changing SCF's tax-unit construction (Moore's SAS port), not the nonfiler imputation.

### int_div definitional gap and the $100 threshold (committed)

- SCF int_div: $630.3B aggregate, 19.4% share-positive, mean $19,079 if positive
- PUF int_div: $627.9B aggregate, 29.8% share-positive, mean $11,194 if positive

Same dollars, ~3× more "holders" in PUF — PUF's 1099-INT/DIV catches every reportable dollar; SCF's survey only captures meaningful holdings.

**Threshold sweep on the (age × has_int_div) joint reweight** (`src/eda/intdiv_threshold_reweight.R`):

| PUF threshold | share_pos | reweight $T | gap closed $T |
|---|---|---|---|
| 0 (current) | 29.8% | 180.64 | 0 |
| **100** | **20.4%** | **164.90** | **15.74** (59% of $27T age×int_div piece) |
| 200 | 17.7% | 159.42 | 21.22 (79%) |

**Committed**: PUF int_div is now thresholded to zero in the (0, $100] band before computing `has_int_div` and `pctile_int_div`. SCF unchanged. Closes the share-positive gap to within 1pp at 0.1% aggregate cost ($0.7B of $628B).

End-to-end harness with $100 threshold:
- Pre-tilt aggregate: **$187.23T** (was $201.06T, −$14T)
- Post-tilt: matches SCF $139T by construction; top-1 0.411, top-0.1 0.174

Stage 3 absorbs most of the pre-tilt distributional improvement (post-tilt top shares barely move), so the threshold is reducing how hard rescale has to work, not directly improving post-tilt distribution.

### Bootstrap is faithful

`src/eda/scf_bootstrap_drift.R` confirms the per-cell weighted bootstrap reproduces raw SCF cell aggregates to within ±0.86% per cell, ±0.01% in total. The per-cell architecture's tight weight dispersion within each cell eliminates the heavy-tail aggregate drift that the old single global forest had. **Bootstrap drift is not a contributor to the SCF-vs-PUF gap.**

## SCF-vs-PUF tax-unit-distribution diagnosis (2026-04-25, deeper round)

Three EDA scripts (`scf_puf_age_income_joint.R`, `wealth_age_income_reweight.R`,
`scf_taxunit_construction_diagnostic.R`) plus an external review of the
Sabelhaus/Moore reference Stata produced the following.

### Where the 17.9M tax-unit gap lives

The gap is concentrated in (18-24 × bottom income cells) and almost
nowhere else:

| (cell, age)            | SCF M | PUF M | gap M | filer-only gap |
|------------------------|-------|-------|-------|----------------|
| pct00to20 × 18-24      | 2.85  | 10.87 | +8.02 | **+6.82**      |
| pct20to40 × 18-24      | 3.09  | 5.85  | +2.76 | +1.82          |
| pct40to60 × 18-24      | 2.08  | 3.38  | +1.30 | +1.27          |

Marginal age-bin: 18-24 gap +12.86M (matches earlier finding); filers-only
gap there is +10.69M. SCF *over*-counts middle-aged (25-49) and elderly
(65+) low-income units relative to PUF filers, and under-counts 18-24
filers severely. SCF total minus PUF-filers-alone is −9.74M (SCF actually
exceeds PUF filers); PUF gets to 188.3M only after DINA's +27.7M nonfiler
addition.

### How much of the wealth gap does (age × cell) mismatch drive?

`wealth_age_income_reweight.R` reports:

| Reweight                                   | Aggregate $T | Closure of $61.9T gap |
|--------------------------------------------|--------------|------------------------|
| SCF truth                                  | 139.12       | 0%                     |
| Pop-only scaling                           | 153.75       | 24% ($14.6T)           |
| Cell-only                                  | 153.60       | 24%                    |
| Age-only                                   | 150.75       | 19%                    |
| **age × cell joint (PUF all)**             | **165.51**   | **43% ($26.4T)**       |
| age × cell joint (PUF filers only)         | 162.03       | 37% ($22.9T)           |
| (existing) age × int_div                   | 180.64       | 67%                    |
| Forest pre-tilt                            | 201.06       | 100%                   |

Tax-unit-count mismatch (age × cell) accounts for ~43% of the +$62T overshoot.
Almost the same fraction as (age × int_div), so they're capturing
overlapping signal. Per-cell: the bottom cells barely move; pct60to80
(+$3.4T), pct90to99 (+$7T), and the top 0.1% (+$7T) jump under reweight.

### The Sabelhaus methodology context (this is the key reframe)

Our R port (`stage1_scf_tax_units.R`) faithfully implements the NPEU
emission rule from Brookings/TPC's reference `scf_taxsim_main.do` (line
993): `npeu_filer_p=1 if age_p>17 & findep_p~=1`. The 18-24 deficit is
**a documented, intentional limitation of the published algorithm**, not
a port bug. Sabelhaus et al. 2022 (TPC, p.5) state explicitly:

> "We assume that children between ages 18 and 24 who are reported to be
> financially dependent on the R and SP are students, or otherwise
> eligible to be claimed as dependents on the main household tax return."

And p.3:

> "financially dependent filers such as the example 18-year-old child
> living at home will not be captured in the SCF tax unit disaggregation
> implemented here. We expect (and confirm) a shortfall in single-filer
> tax units when we compare the SCF tax unit counts to published SOI
> tallies, and that shortfall is consistent with the number of dependent
> tax filers reported by the SOI."

Sabelhaus's published counts:
- 2022 paper (filers only): 144.1M (SCF 2015), 147.6M (SCF 2018) ≈ 96% SOI
- 2025 Gale/Hall/Sabelhaus paper (filers + nonfilers): **163.9M** (2018) = 107% SOI 153.8M
- Our port for SCF 2022: 170.4M ≈ 104% of est. 2022 SOI 163M

So we're matching the 2025 nonfiler-inclusive methodology, not 2022.
**The 18-24 gap is a definitional disagreement between the SCF frame
(dependent students fold into parents) and the PUF frame (a dependent
who files their own 1040 for refund-of-withholding is their own tax
unit, marked via `parent_rank` / `dep_status`).**

### Codex code-review divergences from reference Stata

A separate code review against `scf_taxsim_main.do` flagged additional
divergences in our R port (real, but orthogonal to the 18-24 issue):

1. **Married-couple PEU collapse rule missing** (Stata 1235-1238): the
   reference collapses split married PEUs back to one unit when either
   spouse has zero tax income. We over-split, distorting `married` and
   `n_dep` for some cases.
2. **No filer/nonfiler distinction at output** — matches 2025
   methodology, departs from 2022. Our 170M is "potential tax-unit"
   count, not Sabelhaus 2022's narrower filer-only count.
3. **`n_dep` is a child-count shortcut**: PEU rows use `n_dep = KIDS_u`,
   NPEU is hard-coded `n_dep = 0`. Reference Stata (2220-2280) has
   substantial dependent-allocation logic across PEU/NPEU. **wealth.R
   uses `n_dep` as a training feature** — material model-input mismatch.
4. **NPEU forced single/unmarried**: `MARRIED=0, FILESTAT=1, age2=NA,
   n_dep=0` for all NPEU. Reference (1623-1627, 2271-2277, 2416-2420)
   upgrades NPEU to MFJ/MFS/HoH based on the NPEU member's actual
   marital state and dependents.
5. **Income allocation simplified**: most non-labor/non-business fields
   route through `share_earn`; reference uses item-specific rules
   (cap inc 50/50, SS person-assigned, etc.).
6. **Qualified widow status acknowledged but not implemented**.
7. **$1,000 NPEU income floor missing**: `scf_taxsim_main.do:1033`
   suppresses NPEU emissions whose `npeu_totinc < 1000`. We don't. So
   we emit some no-income NPEU rows the 2022 algorithm would drop —
   our 22.3M NPEU pop is an upper bound vs Sabelhaus's count.

Net: our port is a *reduced* Stage-1 splitter, with multiple departures
from Sabelhaus that all push (a) tax-unit count, (b) `n_dep` accuracy,
(c) NPEU filing-status fidelity in different directions. The 18-24
issue is a separately documented design choice of the *published*
algorithm; the rest are local divergences.

### The smoking gun in NPEU emission

`scf_taxunit_construction_diagnostic.R` shows: of 28.3M roster members
aged 18-30 in SCF 2022, **NPEU machinery emits zero rows in 18-24**
(first non-empty band is 25-29 with 10.5M). 13.73M roster members 18-30
have `findep == 1` ("part of PEU"); the remaining 14.5M with
`findep ∈ {2..5}` flow into NPEU.

Single biggest factor in the 12.86M missing 18-24 gap is the documented
`findep == 1` exclusion.

**Note on raw-SCF age coding**: roster ages in the public-use file are
disclosure-rounded (`scf_taxsim_main.do:727-738`). All actual ages
18-25 collapse to coded `age=25`; 26+ rounds to nearest 5. Reference
Stata applies a `publicuseagerecode` random-assignment function (in
`public_use_age_recode.do`) to back out specific ages within each
band; **our R port skips this**, so NPEU rows in `scf_tax_units.rds`
carry rounded ages (25, 30, 35, …, 95) rather than actual ages. This
is mostly harmless for cell-level analyses but means age = 18-24 is
literally absent from NPEU rows.

### Prototype: relaxing the `findep == 1` exclusion at 18-24 (option b)

`src/eda/scf_18_24_relax_prototype.R` adds NPEU rows for raw `age==25`
roster members with `fd=1, lw∈{1..4}` (= actual 18-27 dependents living
with the PEU), assigning them `age1 = 21`, `income = 0`, all wealth
fields = 0.

| Metric                                | Baseline  | After relaxation | PUF target |
|---------------------------------------|-----------|------------------|------------|
| Total SCF tax units                   | 170.4M    | **182.7M**       | 188.3M     |
| SCF 18-24 marginal                    | 8.65M     | **21.0M**        | 21.5M      |
| pct00to20 × 18-24                     | 2.85M     | 14.54M           | 10.87M     |
| pct20to40 × 18-24                     | 3.09M     | 3.47M            | 5.85M      |
| (age × cell) reweight aggregate       | $165.51T  | **$156.75T**     | —          |
| Forest pre-tilt (ref)                 | $201.06T  | —                | —          |
| SCF truth                             | $139.12T  | $139.12T         | —          |

Closes ~$8.8T (14%) of the +$62T forest overshoot, or 33% of the
(age × cell) reweight contribution. Marginal 18-24 count is brought to
within 0.5M of PUF.

**Caveats** (the prototype is illustrative, not a stage1 rewrite):

1. New rows get `income = 0`, so all land in pct00to20. SCF then *over*-
   counts (18-24, pct00to20) by 3.7M while still under-counting
   (18-24, pct20to40). Real fix: allocate part of the PEU's `WAGEINC`
   to the new NPEU member when the SCF roster reports work hours /
   wages for that person. Reference Stata does this for fd=5 NPEU
   members at `scf_taxsim_main.do:1003-1011`; needs porting.
2. Per-cell reweight delta isn't only at pct00to20 (pct40to60 −$1.70T,
   pct80to90 −$2.09T, pct90to99 −$2.02T) because adding 12M rows
   shifts SCF's rank-based cell boundaries. In a Stage-3-calibrated
   pipeline this re-flows through the per-cell calibration; magnitude
   of the side-effect needs re-checking after a real fix.
3. `publicuseagerecode` should be ported so new NPEU rows carry
   distributionally-correct ages within 18-25, not all 21.
4. The $1,000 NPEU income floor (Stata line 1033) is still missing from
   our port. Adding both the floor and the 18-24 relaxation simultane-
   ously is the cleaner experiment.

Not committed to production. The status doc records the option-(b)
result as a quantitative upper bound on what raw 18-24 relaxation
buys; a faithful refactor (rounded-age recode + per-roster-member
income allocation + $1,000 floor + Codex divergences #1, #3, #4) is
the path forward — see open question 1.

## Open questions

1. **SCF tax-unit construction (the big one).** The 18-24 finding suggests Moore's SAS port may be under-creating tax units relative to PUF/Census reality — folding young earners into parents' households when they file independently. Investigation:
   - Compute SCF's tax-units-per-household ratio; benchmark against CPS-ASEC's tax-unit construction or Census-based estimates.
   - Cross-tab where the under-creation concentrates (multi-generational households, adult children present).
   - If we correct stage1 to create more tax units, SCF's pop count rises toward PUF's 188M-frame, Stage 3 targets can be pop-scaled per cell, and the bottom-quintile mean-NW under-imputation should resolve.
   - Real refactor of `src/imputations/stage1_scf_tax_units.R`. Likely the highest-leverage move on post-Stage-3 distributional fidelity.

2. **`pct99to99.9` interaction structure.** Single-feature swaps close only 41% of this cell's gap; full swap closes 83%. What's distinctive about this window? Possibly exec-comp profiles or something specific to the 99-99.9 income range.

3. **Top-0.1 wealth concentration residual.** Post-Stage-3 (with $100 threshold): top-0.1 wealth share = 0.174 vs SCF truth 0.162 (+1.2pp). Most policy-relevant residual.

4. **Revisit int_div threshold value.** $100 closes 59% of the age×int_div gap; $200 closes 79%. Picked $100 because it matches SCF share-positive; $200 also defensible (matches SCF mean|pos better). Worth re-examining once the tax-unit-construction work above lands, since X-shift accounting will shift.

5. **Stage 3 cell-level over-correction at p0-20.** Mean NW post-tilt $51,603 vs SCF's $62,735. PUF's pct00to20 has 42.5M tax units vs SCF's 37.4M — same cell aggregate forced via rescale spreads thinner per unit. Diagnoses point to the tax-unit-frame issue (item 1).

## Next steps

### Operational (before ship)

1. Run `src/main.R` via `slurm_main.sh` end-to-end to generate 2022–2030+
   outputs with the $100 int_div threshold in place. Verify Tax-Simulator
   consumption isn't broken by the changes.
2. Re-verify Stage 3 extreme-factor cluster (12 factors outside [0.5, 2.0]
   under the threshold, mostly in pct00to20 thin categories). Accept or tune.

### Iterate after

3. **SCF tax-unit construction investigation** (open question 1) — the
   highest-leverage move on post-Stage-3 distributional fidelity. Likely
   3-5 day refactor of `stage1_scf_tax_units.R`.
4. **Threshold revisit** (open question 4) — re-examine $100 vs $200 once
   the tax-unit work above lands.
5. **`pct99to99.9` interaction structure** (open question 2) — would help
   close the post-tilt top-0.1 residual.

## Key files

- **Architecture**: `src/imputations/wealth.R`, `src/imputations/stage3_target_qc.R`
- **Pipeline entry**: `src/main.R` → `run_wealth_imputation()`
- **Harness**: `src/eda/wealth_harness.R` — fast iteration on full pipeline
- **SCF self-test**: `src/eda/wealth_percell_diagnostic.R`
- **PUF test**: `src/eda/wealth_percell_puf_diagnostic.R`
- **Decompositions**:
  - `src/eda/wealth_percell_decomp.R` — global vs per-cell counterfactual
  - `src/eda/wealth_compositional_decomp.R` — per-(cell × feature group) swap
  - `src/eda/wealth_reweight_scf.R` — reweight SCF to PUF marginals
  - `src/eda/scf_puf_census_intdiv.R` — age dist + int_div definitional gap
  - `src/eda/nonfiler_intdiv_probe.R` — filer/nonfiler decomposition + int_div threshold sweep
  - `src/eda/intdiv_threshold_reweight.R` — threshold sweep on (age × int_div) reweight
  - `src/eda/scf_intdiv_lowmass.R` — SCF small-dollar int_div mass
  - `src/eda/scf_bootstrap_drift.R` — verifies per-cell bootstrap is faithful
  - `src/eda/scf_puf_age_income_joint.R` — full (age × cell) joint count tables, filer/nonfiler split
  - `src/eda/wealth_age_income_reweight.R` — quantifies wealth-gap closure from (age × cell) reweight
  - `src/eda/scf_taxunit_construction_diagnostic.R` — anatomy of Moore's port output, NPEU emission breakdown, FS vs SOI
  - `src/eda/scf_18_24_relax_prototype.R` — prototype option (b): relax `fd==1` exclusion for 18-24 dependents
- **Cached forests**: `resources/cache/qrf/wealth_percell_*.rds` (8 files)
