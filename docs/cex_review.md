# CEX Pipeline Review: Branch Summary and Validation Guide

Branch: `cex-resumption` (10 commits ahead of `main`)

## What This Branch Does

This branch overhauls the CEX (Consumer Expenditure Survey) processing pipeline
in `src/cex.R`. The pipeline reads BLS CEX microdata, constructs tax units from
consumer units, computes consumption in 20 BEA PCE categories, and outputs a
100k-row bootstrap sample used to train QRF models that impute consumption onto
PUF tax returns.

## Summary of All Changes (Chronological)

### Phase 1: Bug Fixes (commits 69db382 through ee29eab)

17 bugs fixed in `src/cex.R` and `src/imputations/consumption.R`. The most
critical:

1. **Eliminated CQ+PQ double-counting** — was causing ~2x consumption
   overstatement. Each FMLI row reports CQ (current quarter) and PQ (prior
   quarter); old code summed both (~6 months) then multiplied by 4, producing
   ~24 months of annual consumption.

2. **Fixed TU_DPNDT dependent handling** — was using TU_CODE (role) instead of
   TAX_UNIT (unit number) in tax unit ID construction, causing cross-TU
   misassignment. Now correctly uses TAX_UNIT and handles dependent filers.

3. **Fixed dependent numbering** — changed from CU-wide to TU-specific grouping.

4. **Expanded income definition** — added interest, pensions, dividends (6
   sources instead of 3).

5. **Added COMP_INC filter** — training only on complete income reporters.

6. **Fixed irregular-income blend logic** — operator precedence bug zeroed out
   consumption for zero-income units.

7. **Fixed outlier cap** — 400x to 4x (consumption-to-income ratio threshold).

Full details: `docs/cex_updates.md` entries 1-14.

### Phase 2: PCE Category Disaggregation (commit 305a3a8)

Replaced the old goods/services two-bucket split with 20 BEA PCE major
categories. Added `src/pce_benchmark.R` for NIPA benchmarking and
`resources/pce_targets_2023.csv` for control totals. Initially used 33 FMLI
sub-variables mapped to 18 CEX-observed categories + 2 neutral-distribution
categories (NPISH, net foreign travel).

### Phase 3: MO_SCOPE Weighting (commit 21dfffb)

Added BLS-prescribed annual weighting: `WT_ANNUAL = FINLWT21 / 4 * (MO_SCOPE / 3)`.
MO_SCOPE counts how many of each interview's 3 CQ reference months fall in the
target calendar year. Replaced raw FINLWT21 in percentile construction and
bootstrap sampling. Interviews with MO_SCOPE = 0 get zero weight.

### Phase 4: NTAXI Validation (commit ed09aa6)

Added optional diagnostic function `validate_tax_units_against_ntaxi()` that
reads BLS NTAXI files and compares dependent counts, filing status, and TU
counts per CU against our MEMI-based construction. Not called during normal
pipeline execution.

### Phase 5: UCC-Level MTBI Integration (commits 2d5a411 through 1cf48a5)

Replaced FMLI sub-variable consumption with UCC-level MTBI data. This is the
largest change and the most recent work.

**New files:**
- `resources/cex_ucc/stubs/CE-HG-Integ-2023.txt` — BLS hierarchical groupings
- `resources/ucc_pce_bridge.csv` — 594 UCCs mapped to 20 PCE categories
- `src/build_ucc_bridge.R` — one-time script that parsed BLS stubs to generate
  the bridge CSV

**Key classification fixes:**
- Health insurance (580xxx) -> `financial_insurance` (was all in `health_care`)
- Prescription drugs (540000) -> `other_nondurables` (was `health_care`)
- Medical equipment (550xxx) -> `other_durables` (was `health_care`)
- Jewelry/watches (430110/430120) -> `other_durables` (was `clothing`)
- Internet service (690114) -> `communication` (was `other_services`)
- Cable/satellite TV (270310/270311) -> `communication` (was `rec_goods`)
- Non-consumption UCCs (contributions, pensions) -> `exclude` (filtered out)

**Code simplification:** The duplicated 2x18-line FMLI formula blocks (one for
irregular income, one for regular income) were replaced by a single
`across(all_of(pce_cats), ~ .x * CU_pct)`. Net change: -135/+70 lines.

**What changed in the pipeline flow:**
1. MEMI -> build tax units (UNCHANGED)
2. FMLI -> weights + demographics only (NO expenditure sub-variables)
3. MTBI -> join to bridge CSV -> aggregate by NEWID + PCE category (NEW)
4. Join TU + FMLI + MTBI -> allocate by CU_pct -> compute ratios (SIMPLIFIED)
5. Bootstrap sample 100k rows by WT_ANNUAL (UNCHANGED)

## Key Files

| File | Role |
|---|---|
| `src/cex.R` | Main pipeline: `build_cex_training()` + `validate_tax_units_against_ntaxi()` |
| `src/pce_benchmark.R` | `benchmark_to_pce()` and `compute_expenditure_shares()` |
| `src/build_ucc_bridge.R` | One-time script to generate bridge CSV from BLS stubs |
| `resources/ucc_pce_bridge.csv` | UCC -> pce_major mapping (594 rows) |
| `resources/pce_targets_2023.csv` | NIPA PCE control totals (20 categories) |
| `resources/cex_ucc/stubs/CE-HG-Integ-2023.txt` | BLS hierarchical groupings |
| `src/imputations/consumption.R` | QRF consumption imputation (currently DISABLED) |
| `docs/cex_updates.md` | Detailed changelog (17 entries) |
| `docs/cex_improvement_todo.md` | Improvement decisions and status |
| `docs/pce_benchmarking.md` | PCE benchmarking methodology |

## Data Paths (Cluster)

All CEX microdata is at: `/gpfs/gibbs/project/sarin/shared/raw_data/CEX/2023`

Files read by `build_cex_training()`:
- `memi231.csv` through `memi241.csv` (5 quarterly MEMI files)
- `fmli231.csv` through `fmli241.csv` (5 quarterly FMLI files)
- `mtbi231.csv` through `mtbi241.csv` (5 quarterly MTBI files)

---

## Validation Checklist

Run `build_cex_training()` on the cluster. The following checks should pass:

### 1. Pipeline runs without error

The bridge completeness check (line ~172) will `stop()` if any MTBI UCC is not
in `resources/ucc_pce_bridge.csv`. If this fires:

```
Error: Unmapped UCCs in MTBI data: 123456, 789012. Add these to resources/ucc_pce_bridge.csv
```

**Fix:** Add the listed UCCs to the bridge CSV with appropriate `pce_major` and
re-run. Use `src/build_ucc_bridge.R` and the stubs file as reference for where
each UCC belongs in the hierarchy.

### 2. Output shape and schema

The output should be a 100,000-row tibble with these columns:
- `NEWID`, `QINTRVYR`, `FINLWT21`, `WT_ANNUAL`
- `pctile_income` (1-100 for regular income, -1 for irregular)
- `married`, `age1`, `n_dep`, `n_dep_ctc`, `male1`
- `income`, `has_income`
- 20 PCE category columns (quarterly $): `clothing`, `motor_vehicles`,
  `other_durables`, `furnishings`, `rec_goods`, `other_nondurables`,
  `food_off_premises`, `communication`, `npish`, `other_services`,
  `transport_services`, `rec_services`, `net_foreign_travel`,
  `food_accommodations`, `health_care`, `utilities`, `gasoline`, `education`,
  `financial_insurance`, `housing`
- `total_consumption`
- 20 `_per` ratio columns + `total_consumption_per`

Verify:
```r
result = build_cex_training()
dim(result)          # should be 100000 x 53
sum(is.na(result))   # should be 0 (or very small, only in dep age cols)
```

### 3. No NAs in PCE category columns

```r
sapply(pce_cats, function(cat) sum(is.na(result[[cat]])))
# All should be 0
```

### 4. Non-negative consumption in all categories

```r
sapply(pce_cats, function(cat) sum(result[[cat]] < 0))
# Should be 0 or very small (refunds can cause small negatives)
```

### 5. NPISH and net_foreign_travel are zero

These have no CEX source and are allocated in `pce_benchmark.R`:
```r
sum(result$npish)              # should be 0
sum(result$net_foreign_travel) # should be 0
```

### 6. Aggregate weighted totals are in the right ballpark

Compute CEX-implied national totals and compare to NIPA:
```r
targets = fread('resources/pce_targets_2023.csv')
# Use the pre-bootstrap joined data (before slice_sample) for this check.
# Or approximate from the bootstrap sample:
cex_totals = sapply(pce_cats, function(cat) {
  sum(result[[cat]] * 4 * result$WT_ANNUAL, na.rm = TRUE) / 1e9
})
coverage = cex_totals / setNames(targets$pce_billions, targets$category)[pce_cats]
print(round(coverage * 100, 1))
```

Expected coverage (CEX as % of NIPA) based on TPC Table A1:
- `gasoline`: ~100-125% (CEX overshoots slightly)
- `food_off_premises`: ~80-110%
- `housing`: ~80-120%
- `health_care`: ~15-25% (OOP only; third-party payments excluded)
- `financial_insurance`: ~20-40% (FISIM not in CEX)
- `food_accommodations`: ~60-80%
- `clothing`: ~60-80%

If any category shows 0% or >500%, the bridge mapping for that category is
likely wrong.

### 7. Benchmark scaling factors are reasonable

```r
bench = benchmark_to_pce(result, weight_col = 'WT_ANNUAL')
print(bench$diagnostics)
```

- Scaling factors should be between 0.5 and 5.0 for most categories
- Factors outside 0.1-10.0 warrant investigation
- `npish` and `net_foreign_travel` will show `NA` pre-benchmark (neutral dist.)

### 8. Health care is now smaller (classification fix verification)

Under the old FMLI approach, `health_care` included insurance, drugs, and
equipment. Under MTBI, those are split out. Verify:

```r
# health_care should be noticeably smaller than old HEALTHCQ-based values
# financial_insurance should be larger (now includes health insurance premiums)
# other_nondurables should be larger (now includes prescription drugs)
# other_durables should be larger (now includes medical equipment, jewelry)
```

### 9. Consumption-to-income ratios are reasonable

```r
regular = result %>% filter(has_income == 1)
summary(regular$total_consumption_per)
# Median should be ~0.3-1.0 (quarterly consumption / annual income)
# Max should be < 4 (the outlier cap from pce_benchmark.R)
quantile(regular$total_consumption_per, c(0.05, 0.25, 0.5, 0.75, 0.95))
```

### 10. NTAXI validation (optional but recommended)

```r
ntaxi_check = validate_tax_units_against_ntaxi()
# Dependent count agreement: expect >95%
# Married flag agreement: expect >90%
# TU count per CU agreement: expect >95%
```
