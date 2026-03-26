# CEX Processing Pipeline: Changes Log

Changes to `src/cex.R` and `src/imputations/consumption.R` made in the `cex-resumption` branch.

---

## 1. ~~Eliminated CQ + PQ double-counting~~ — RETRACTED

**This entry was wrong.** CQ and PQ are NOT overlapping windows — they cover different portions of the 3-month reference period and ARE additive. BLS codebook: "TOTEXP (sum of TOTEXPPQ and TOTEXPCQ) describes total expenditures the family incurred for the three months prior to the interview." Empirically: `(CQ + PQ) * 4 = $73,481`, matching BLS published annual of $72,967. CQ alone gives only ~$24,700.

The original code that summed CQ + PQ was correct. The "fix" that dropped PQ removed ~2/3 of expenditure data.

**Current status:** CQ + PQ are now summed for each expenditure category in the FMLI read.

---

## 2. Added complete income reporter filter

**File:** `src/cex.R:125`, `src/cex.R:143`

BLS flags CUs where income data is unreliable via `COMP_INC`. The old code had no filter, so the training data included CUs whose income BLS itself considers untrustworthy. This distorts income percentile bins and the conditional spending distributions the QRF learns from.

**Fix:** Added `COMP_INC` to the FMLI variable read and filter to `COMP_INC == 1` after the join, before percentile construction.

---

## 3. Removed life insurance from services; dropped savings categories

**Files:** `src/cex.R:116-122`, `src/cex.R:170`, `src/cex.R:211`

Life insurance (`LIFINS_exp`) is a savings vehicle, not consumption, but it was included in the `services` aggregate. Cash contributions (`CASHCO`) and retirement/pension contributions (`RETPEN`) were read and processed but never entered the goods/services split — pure dead weight.

**Fix:** Removed `LIFINS_exp` from the services sum. Removed `CASHCOCQ`, `LIFINSCQ`, `RETPENCQ` (and their PQ counterparts, now moot) from the expenditure vectors entirely.

---

## 4. Fixed TU_DPNDT handling (3 upstream bugs + 1 reassignment bug)

**File:** `src/cex.R:44-66`

The old code constructed a "claiming TU" identifier from `TU_DPNDT` using `TU_CODE` (the person's *role* — 1=payer, 2=spouse, 3=dependent) instead of `TAX_UNIT` (the person's *tax unit number*). These are different variables that happen to share the same value space (1, 2, 3, ...).

```r
# Old (wrong): used TU_CODE (role) instead of TAX_UNIT (unit number)
TU_DPNDT = case_when(
  is.na(TU_DPNDT) ~ paste0(NEWID, "00", TU_CODE),   # e.g., "003" for a dependent — points to "TU #3", which may not exist
  TU_DPNDT == "B" ~ paste0(NEWID, "00", TU_CODE),
  ...
)
```

Additionally, the old code:
- Constructed spurious caretaker IDs for non-dependents (taxpayers and spouses whose `TU_DPNDT` is correctly blank)
- Only reassigned people with `TU_CODE == 3`, missing "dependent filers" — e.g., a teenager with a summer job who has their own `TAX_UNIT` (`TU_CODE == 1`) but is claimed on a parent's return via `TU_DPNDT`

**Fix:** Complete rewrite. `TU_ID` is now always constructed from `TAX_UNIT`. Dependency is determined by whether `TU_DPNDT` contains a valid value (not NA, not blank, not "B"). Anyone with a valid `TU_DPNDT` pointing to a different TU — whether `TU_CODE` is 1 or 3 — gets reassigned:

```r
has_valid_dpndt = !is.na(TU_DPNDT) & TU_DPNDT != "" & TU_DPNDT != "B",
claiming_TU_ID = if_else(has_valid_dpndt, as.numeric(paste0(NEWID, "00", TU_DPNDT)), NA_real_),
is_dep = as.numeric(TU_CODE == 3 | has_valid_dpndt),
TU_ID = if_else(!is.na(claiming_TU_ID) & claiming_TU_ID != TU_ID, claiming_TU_ID, TU_ID)
```

---

## 5. Fixed dependent numbering from CU-wide to TU-specific

**File:** `src/cex.R:69`

Dependents were numbered sequentially across the entire CU (`group_by(NEWID, is_dep)`). In a CU with two tax units each having 2 dependents, the dependents would be numbered 1-4 across the CU. After the pivot to tax-unit rows, dep3 and dep4 from TU #2 could land in TU #1's columns.

**Fix:** Changed grouping to `group_by(NEWID, TU_ID, is_dep)` so dependents are numbered within their own tax unit.

---

## 6. Expanded income definition (CEX and PUF sides)

**Files:** `src/cex.R:20-21`, `src/cex.R:41`, `src/imputations/consumption.R:80-83`

The old code used only 3 income sources from MEMI: wages (`SALARYXM`), self-employment (`SEMPFRMM`), and Social Security (`SOCRRXM`). This misses investment, pension, and dividend income — understating income for retirees, investors, and transfer recipients and distorting percentile assignment.

A comment in `consumption.R` claimed income was "restricted to what is available in MEMI," but MEMI contains additional income fields.

**Fix:** Added `INTEARNM` (interest), `PENSIONM` (pensions/annuities), and `DIVIDM` (dividends) to the MEMI read and income sum. On the PUF prediction side, added `txbl_int`, `div_ord`, `div_pref`, and `gross_pens_dist` to match. Rental income was excluded from both sides because MEMI lacks a clean rental income variable — keeping the definitions aligned is more important than including every source.

**Superseded by #18:** `INTEARNM`, `PENSIONM`, and `DIVIDM` do not exist in MEMI. See entry #18 for the corrected income definition.

---

## 7. Dropped filing_status; added n_dep as QRF feature

**Files:** `src/cex.R:103-113`, `src/imputations/consumption.R:33,42,51,60,102`

The old code computed `filing_status` (1=single, 2=married, 4=HoH) and assigned head-of-household to all single filers with dependents. Real HoH requires paying >50% of household costs, which MEMI cannot verify — so HoH was overstated and single filers understated.

**Fix:** Removed `filing_status` entirely. The QRF now conditions on `married` (0/1) and `n_dep` (count of dependents) directly, which are observable from the data. Added `n_dep` to all four QRF feature vectors and the prediction-side select in `consumption.R`.

---

## 8. Dropped self-reported income; unified to XM-based income

**Files:** `src/cex.R:20-22,26-28,41-43,110,113`

The old code maintained two parallel income variables: `inc` (self-reported: `SALARYX + SEMPFRMX + SOCRRX`) and `incm` (BLS multiply-imputed: `SALARYXM + SEMPFRMM + SOCRRXM`). Only `income2` (from `incm`) was used for percentiles and ratios, but `income` (from `inc`) was carried through the pipeline. Self-reported NAs were set to 0, conflating "not reported" with "truly zero."

**Fix:** Removed self-reported income variables entirely. A single `income` column is now constructed from XM variables only. The NA-to-0 treatment is retained for XM variables as a safety measure, though BLS XM variables are believed to always have values (this should be verified against BLS CEX documentation).

---

## 9. Removed unused MEMI variables

**File:** `src/cex.R:20`

`IN_COLL`, `IN_COLL_`, `INCNONWK`, and `EARNER` were selected from MEMI but never referenced downstream. Removed from the select.

---

## 10. Moved payer/spouse promotions inside grouped mutate

**File:** `src/cex.R:31-37`

The payer and spouse promotion logic (promoting `CU_CODE == 1` to `TU_CODE = 1` when no payer exists, etc.) previously ran in an ungrouped `mutate()` and relied on per-TU flags (`no_payer`, `no_spouse`) broadcast from an earlier `group_by(NEWID, TAX_UNIT)` block. While this happened to work because the flags were TU-specific, performing the promotions inside the grouped block makes the TU-scoping explicit and prevents any possibility of cross-TU promotion.

---

## 11. Fixed training object name mismatch in consumption.R

**File:** `src/imputations/consumption.R:23,33-34,51-52`

`build_cex_training()` was assigned to `cex_training`, but all QRF training calls referenced `training` — an undefined variable. Re-enabling the module would crash with `object 'training' not found`.

**Fix:** Replaced all `training` references with `cex_training`.

---

## 12. Fixed irregular-income blend logic (operator precedence bug)

**File:** `src/imputations/consumption.R:128-131`

The old blending expressions had an operator precedence bug. The intent was to give irregular-income units 100% weight on direct-dollar prediction and 0% on the ratio-to-income prediction:

```r
# Old (wrong): (1 + has_income) != 1, not 1 + (has_income != 1)
goods.c = ((.5 * goods * (1 + has_income != 1)) + (.5 * goods_per * income * (1 - has_income != 1))) * 4
```

In R, `!=` binds looser than `+`, so `1 + has_income != 1` parses as `(1 + has_income) != 1`. For zero-income units (`has_income = 0`): `(1+0) != 1` evaluates to FALSE, zeroing out *both* terms — imputing zero consumption. For negative-income units (`has_income = -1`): both terms evaluate to TRUE, applying the 50/50 blend with negative income in the ratio term.

**Fix:** Replaced with explicit weight variables:

```r
w_direct = if_else(has_income == 1, 0.5, 1.0),
w_ratio  = if_else(has_income == 1, 0.5, 0.0),
goods.c    = (w_direct * goods    + w_ratio * goods_per    * income) * 4,
services.c = (w_direct * services + w_ratio * services_per * income) * 4,
```

---

## 13. Fixed outlier cap: 400x to 4x

**File:** `src/imputations/consumption.R:23`

The ratio-model training filter was intended to exclude observations where consumption exceeds 400% of income. But `expenses_per` is defined as `expenses / income` (a ratio, not a percentage), so 400% = 4.0. The old filter `expenses_per < 400` allowed spending up to 40,000% of income.

**Fix:** Changed to `expenses_per < 4`.

---

## 14. Aligned has_income threshold between training and prediction

**Files:** `src/cex.R:150`, `src/imputations/consumption.R:86`

Training defined regular income as `income > 0`; prediction used `income > 1`. Tax units with small positive income in (0, 1] were treated as regular during training but irregular during prediction — the model was trained and applied under different feature definitions.

**Fix:** Both sides now use `income > 0`.

---

## 15. Added MO_SCOPE annual weighting

**File:** `src/cex.R:166-185`, `src/cex.R:290`, `src/cex.R:354`

The pipeline uses 5 quarters of Interview data (year t Q1–Q4 plus year t+1 Q1) to build calendar-year estimates. Each interview's CQ covers the 3 months before the interview, but not all 3 reference months necessarily fall in the target year. For example, a February 2023 interview references Nov 2022–Jan 2023 — only 1 of 3 months is in 2023. The old code treated all interviews equally via raw `FINLWT21`, systematically overweighting boundary-quarter interviews whose reference months partially fall outside the target year.

BLS prescribes `FINLWT21 / 4 * (MO_SCOPE / 3)` where `MO_SCOPE` counts how many of the 3 CQ reference months fall in the target calendar year (0, 1, 2, or 3). See https://www.bls.gov/cex/pumd-getting-started-guide.htm.

**Fix:** Added `MO_SCOPE` computation from `QINTRVMO`/`QINTRVYR` and derived `WT_ANNUAL = FINLWT21 / 4 * (MO_SCOPE / 3)` in the FMLI processing step. Replaced `FINLWT21` with `WT_ANNUAL` in two places:
1. Weighted percentile construction (`wtd.quantile` weights)
2. Bootstrap sampling (`slice_sample` weight_by)

Interviews with MO_SCOPE = 0 (e.g., January interview referencing only prior-year months) get zero weight and are effectively excluded. Both `FINLWT21` and `WT_ANNUAL` are carried in the output for diagnostics.

---

## 16. Added NTAXI validation function

**File:** `src/cex.R:359-478`

The pipeline constructs tax units from MEMI using BLS's `TAX_UNIT`, `TU_CODE`, and `TU_DPNDT` fields but had no way to verify the construction was correct. BLS publishes NTAXI files containing one record per tax unit per interview quarter with `DEPCNT` (dependent count), `FILESTAT` (filing status), and `TAX_UNIT` (unit number within CU).

**Fix:** Added `validate_tax_units_against_ntaxi()` — an optional diagnostic function (not called during normal pipeline execution) that reads NTAXI quarterly files, rebuilds tax units from MEMI using the same logic as `build_cex_training()`, and reports:
- Dependent count agreement (`our_n_dep` vs `NTAXI.DEPCNT`)
- Married flag agreement (`our_married` vs `NTAXI.FILESTAT == 2`)
- TU count per CU agreement
- Crosstabs of disagreements to identify systematic patterns

Returns a list with match rates and the full comparison table for further inspection. Expect >95% on dependent counts and >90% on married flag; lower rates warrant investigation.

---

## 17. Replaced FMLI sub-variables with UCC-level MTBI data

**Files:** `src/cex.R:115-199`, `resources/ucc_pce_bridge.csv`, `src/build_ucc_bridge.R`

The pipeline previously read 33 FMLI sub-variables (e.g., HEALTHCQ, VEHINSCQ, PETTOYCQ) and mapped them to 18 PCE categories via hardcoded R formulas. This was lossy: HEALTHCQ lumped drugs, equipment, insurance, and provider services into one `health_care` bucket; VEHINSCQ put all vehicle insurance into `financial_insurance`; PETTOYCQ was the sole source for `other_durables`, missing jewelry and therapeutic equipment.

**Fix:** Read MTBI (Monthly Tabulation — Interview) files at the UCC level (~594 active codes), join to a bridge CSV (`resources/ucc_pce_bridge.csv`) that maps each UCC to one of 20 PCE categories, and aggregate by NEWID + PCE category. The bridge was generated from BLS CE PUMD hierarchical groupings (`resources/cex_ucc/stubs/CE-HG-Integ-2023.txt`) via a one-time script (`src/build_ucc_bridge.R`).

Key classification fixes enabled by UCC-level data:
- Health insurance (580xxx) → `financial_insurance` (was `health_care`)
- Prescription drugs (540000) → `other_nondurables` (was `health_care`)
- Medical equipment (550xxx) → `other_durables` (was `health_care`)
- Jewelry/watches (430110/430120) → `other_durables` (was `clothing`)
- Internet service (690114) → `communication` (was `other_services`)
- Cable/satellite TV (270310/270311) → `communication` (was `rec_goods`)
- Non-consumption UCCs (contributions, pensions) → `exclude` (filtered out)

The bridge validates completeness at runtime: any MTBI UCC not in the bridge CSV triggers a hard stop. Non-consumption UCCs are marked `exclude` in the bridge and filtered during aggregation.

FMLI is still read for weights (FINLWT21, WT_ANNUAL), demographics (FAM_SIZE), and tech variables (COMP_INC, QINTRVMO, QINTRVYR). The 2×18-line duplicated formula blocks were replaced by a single `across(all_of(pce_cats), ~ .x * CU_pct)`, eliminating code duplication.

---

## 18. Corrected income definition: actual MEMI vars + CU-level capital income

**Files:** `src/cex.R`

Entry #6 added `INTEARNM`, `PENSIONM`, and `DIVIDM` to the MEMI income definition, but these columns do not exist in the MEMI files. The code would crash at the `select()` call. This was a latent bug — the expanded income definition was never successfully run.

The actual MEMI multiply-imputed income variables are:

| Variable | Content |
|---|---|
| `SALARYXM` | Wages/salary |
| `SEMPFRMM` | Self-employment |
| `SOCRRXM` | Social Security |
| `SSIXM` | SSI |
| `ANGOVRTM` | Government retirement/pension |
| `ANPRVPNM` | Private pension |

Interest, dividends, and rental income exist only at the CU level in FMLI (`INTRDVXM`, `NETRENTM`), not at the member level.

**Fix:** Member-level income uses the 6 MEMI variables above. CU-level capital income (`INTRDVXM + NETRENTM`) is read from FMLI, CPI-deflated, and allocated to TUs within each CU pro-rata by member-level income. When all TUs in a CU have zero member income, capital income is split equally across TUs.

```r
# Member-level (clean TU allocation)
inc = SALARYXM + SEMPFRMM + SOCRRXM + SSIXM + ANGOVRTM + ANPRVPNM

# CU-level capital income allocation (after FMLI join, within group_by(NEWID))
cu_member_inc = sum(pmax(income, 0)),
cap_share = if_else(cu_member_inc > 0, pmax(income, 0) / cu_member_inc, 1 / n_tu),
capital_income = (INTRDVXM + NETRENTM) * cap_share,
income = income + capital_income
```

This gives the broadest possible income definition while correctly handling the member-vs-CU distinction. The PUF prediction side (`consumption.R`) already includes `txbl_int`, `div_ord`, `div_pref`, and `gross_pens_dist`, so the two sides are now better aligned.

---

## 19. Fixed `read_cex` to match BLS on-disk layout

**File:** `src/cex.R:1-26`

Each BLS annual release contains Q2-Q4 of year N plus Q1 of year N+1, not Q1-Q4 of year N:
```
CEX/2022/: *222, *223, *224, *231
CEX/2023/: *232, *233, *234, *241
CEX/2024/: *241x, *242, *243, *244, *251
```

The old `read_cex` looked for all quarters in `CEX/{year}/` and the boundary Q1 in `CEX/{year+1}/`. This was incorrect: Q1 of any year lives in the *prior* release directory.

**Fix:** Rewritten to search two directories per calendar year:
- `CEX/{year-1}/` for `^{prefix}{yy}1\.csv$` (Q1 from prior release)
- `CEX/{year}/` for `^{prefix}{yy}[2-4]\.csv$` (Q2-Q4) and `^{prefix}{yy+1}1\.csv$` (boundary Q1 of next year)

For year=2022, the Q1 file would require `CEX/2021/` which doesn't exist — handled gracefully (`dir()` returns `character(0)`).

---

## 20. Multi-year pooling (2022-2024)

**File:** `src/cex.R`

The pipeline previously read only survey year 2023 (~6k CUs/quarter). This limits tail coverage for the QRF/ranger training data.

**Fix:** Set `years = c(2022, 2023, 2024)`. Each `map()` call adds a `SURVEY_YEAR` column. After binding, deduplication removes boundary-file overlap: `distinct(NEWID, MEMBNO)` for MEMI, `distinct(NEWID)` for FMLI, full-row `distinct()` for MTBI. The output now includes a `SURVEY_YEAR` column.

---

## 21. CPI deflation to 2017 dollars

**File:** `src/cex.R`

With multi-year pooling, monetary values from different survey years are in different nominal dollars. The PUF base year is 2017, so all CEX income and expenditure values must be deflated to 2017 dollars for consistent training.

**Fix:** Hardcoded CPI-U (IRS year-average) deflators from Macro-Projections `historical.csv`:
```r
cpi_deflator = c('2022' = 1.17443, '2023' = 1.23825, '2024' = 1.27760)
```

Applied to:
- MEMI income variables (6 vars) immediately after read, before tax unit construction
- FMLI capital income variables (`INTRDVXM`, `NETRENTM`) after read, before join
- MTBI `COST` after read, before aggregation

FMLI weights (`FINLWT21`) are not deflated — they are not monetary values.

---

## 22. Fixed MO_SCOPE to use SURVEY_YEAR; divided weights by pool size

**File:** `src/cex.R`

With multi-year pooling, the MO_SCOPE calculation used `years[1]` (always 2022) instead of each observation's actual survey year. This gave incorrect scope for 2023 and 2024 observations. Additionally, the WT_ANNUAL formula produced weights that represented the full US population for *each* year, triple-counting when pooled.

**Fix:**
- MO_SCOPE `case_when` now uses `SURVEY_YEAR` instead of `years[1]`
- `WT_ANNUAL = FINLWT21 / 4 * (MO_SCOPE / 3) / length(years)` — dividing by 3 (number of pooled years) so that `sum(WT_ANNUAL)` represents one year's worth of tax units (~130M), not three (~400M)

---

## 23. Removed bootstrap resampling

**File:** `src/cex.R`

The pipeline previously bootstrap-resampled 100k rows from the CEX data using `slice_sample(n = 100000, replace = TRUE, weight_by = WT_ANNUAL)`. This baked the survey weights into resampling rather than passing them as case weights to the model, discarding information about the joint distribution and capping training data at 100k regardless of pool size.

**Fix:** Removed `slice_sample` block. `build_cex_training()` now returns all observations (~30-40k rows with 3 years pooled) with `WT_ANNUAL` as a column. The ranger models in `consumption.R` use `case.weights` to handle the weights properly.

A comment notes that PCE benchmarking (scaling category totals to BEA aggregates) is deferred to `project_puf.R`, where it can be applied in projection-year dollars.

---

## 24. Switched consumption imputation from quantregForest to ranger

**Files:** `src/imputations/consumption.R`, `src/imputations/helpers.R`, `requirements.txt`, `src/impute_variables.R`

The old consumption imputation was disabled (commented out in `impute_variables.R`) and used `quantregForest` with separate goods/services models. quantregForest does not support case weights natively, which was the motivation for bootstrap resampling.

**Fix:** Complete rewrite of `consumption.R` using ranger with `quantreg = TRUE`:
- Two models: `consumption_rf` (total consumption, direct) and `consumption_per_rf` (consumption-to-income ratio)
- Both trained with `case.weights = WT_ANNUAL` — no bootstrap needed
- Prediction: 50% direct + 50% ratio×income for positive income; 100% direct for irregular income; annualized by ×4

Added to `helpers.R`:
- `train_or_load_ranger()`: formula-based interface matching `train_or_load_qrf()` pattern
- `predict_ranger_draw()`: stochastic quantile prediction — predicts 1%-99% quantile grid, draws one random quantile per observation

After predicting total consumption, distributes across 20 PCE categories using average expenditure shares by income percentile computed from CEX training data. Shares are normalized to sum to 1 within each percentile bin.

Added `ranger` to `requirements.txt`. Uncommented `source('src/imputations/consumption.R')` in `impute_variables.R`.
