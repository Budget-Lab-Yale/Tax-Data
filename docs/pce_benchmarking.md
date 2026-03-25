# PCE Category Alignment and Benchmarking

## Purpose

This document describes the methodology for aligning Consumer Expenditure Survey
(CEX) data to Bureau of Economic Analysis (BEA) Personal Consumption Expenditure
(PCE) categories, benchmarking imputed consumption to NIPA control totals, and
computing expenditure shares for excise tax incidence analysis.

The implementation follows the methodology described in Rosenberg (2015), "The
Distributional Burden of Federal Excise Taxes," Tax Policy Center.

### Files Modified or Created

| File | Role |
|------|------|
| `src/cex.R` | Modified: reads FMLI sub-variables, constructs 18 PCE categories |
| `src/pce_benchmark.R` | New: `benchmark_to_pce()` and `compute_expenditure_shares()` |
| `resources/pce_targets_2023.csv` | New: NIPA PCE control totals for 2023 |

---

## Stage 1: CEX-to-PCE Category Disaggregation

### Prior Approach

The original `cex.R` read 15 top-level FMLI aggregate variables (FOODCQ, HOUSCQ,
TRANSCQ, etc.) and collapsed them into two categories:

```
goods    = FOOD + ALCBEV + APPAR + TOBACC
services = HOUS + HEALTH + TRANS + ENTERT + PERSCA + READ + EDUCA
```

This prevented any distributional analysis that required finer category detail,
such as distinguishing gasoline (highway excise) from vehicle purchases, or
separating tenant rent from utilities.

### New Approach

We now read 33 FMLI sub-variables and map them to 18 of the 20 BEA PCE major
categories. Two PCE categories (NPISH and net foreign travel) have no CEX
analogue and are excluded.

### Complete Crosswalk

| PCE Category | R Variable | FMLI Sub-Variables | FMLI Parent |
|---|---|---|---|
| Clothing and footwear | `clothing` | APPARCQ | (top-level) |
| Motor vehicles and parts | `motor_vehicles` | CARTKNCQ + CARTKUCQ + OTHVEHCQ | TRANSCQ |
| Other durable goods | `other_durables` | PETTOYCQ | ENTERTCQ |
| Furnishings and durable household equipment | `furnishings` | HOUSEQCQ | HOUSCQ |
| Recreational goods and vehicles | `rec_goods` | TVRDIOCQ + OTHEQPCQ | ENTERTCQ |
| Other nondurable goods | `other_nondurables` | TOBACCCQ + PERSCACQ + READCQ | (top-level) |
| Food and bev for off-premises | `food_off_premises` | FDHOMECQ + ALCBEVCQ | FOODCQ + (top-level) |
| Communication | `communication` | TELEPHCQ | HOUSCQ > UTILCQ |
| Other services | `other_services` | HOUSOPCQ + MISCCQ + OTHENTCQ | HOUSCQ + (top-level) + ENTERTCQ |
| Transportation services | `transport_services` | MAINRPCQ + VRNTLOCQ + PUBTRACQ | TRANSCQ |
| Recreation services | `rec_services` | FEEADMCQ | ENTERTCQ |
| Food services and accommodations | `food_accommodations` | FDAWAYCQ + OTHLODCQ | FOODCQ + HOUSCQ > SHELTCQ |
| Health care | `health_care` | HEALTHCQ | (top-level) |
| Household utilities | `utilities` | NTLGASCQ + ELCTRCCQ + ALLFULCQ + WATRPSCQ | HOUSCQ > UTILCQ |
| Gasoline and other energy goods | `gasoline` | GASMOCQ | TRANSCQ |
| Education | `education` | EDUCACQ | (top-level) |
| Financial services and insurance | `financial_insurance` | LIFINSCQ + VEHINSCQ + VEHFINCQ | PERINSCQ + TRANSCQ |
| Housing | `housing` | OWNDWECQ + RENDWECQ | HOUSCQ > SHELTCQ |

### FMLI Variable Hierarchy and Double-Counting Prevention

The FMLI file contains both aggregate and sub-variables. We use exclusively
sub-variables to avoid double-counting. The decomposition is verified to be
exhaustive and mutually exclusive within each aggregate:

```
FOODCQ
├── FDHOMECQ  → food_off_premises
└── FDAWAYCQ  → food_accommodations

HOUSCQ
├── SHELTCQ
│   ├── OWNDWECQ  → housing
│   ├── RENDWECQ  → housing
│   └── OTHLODCQ  → food_accommodations
├── UTILCQ
│   ├── NTLGASCQ  → utilities
│   ├── ELCTRCCQ  → utilities
│   ├── ALLFULCQ  → utilities
│   ├── TELEPHCQ  → communication
│   └── WATRPSCQ  → utilities
├── HOUSOPCQ      → other_services
└── HOUSEQCQ      → furnishings

TRANSCQ
├── CARTKNCQ  → motor_vehicles
├── CARTKUCQ  → motor_vehicles
├── OTHVEHCQ  → motor_vehicles
├── GASMOCQ   → gasoline
├── VEHFINCQ  → financial_insurance
├── MAINRPCQ  → transport_services
├── VEHINSCQ  → financial_insurance
├── VRNTLOCQ  → transport_services
└── PUBTRACQ  → transport_services

ENTERTCQ
├── FEEADMCQ  → rec_services
├── TVRDIOCQ  → rec_goods
├── OTHEQPCQ  → rec_goods
├── PETTOYCQ  → other_durables
└── OTHENTCQ  → other_services

Top-level (no parent aggregate):
  ALCBEVCQ  → food_off_premises
  APPARCQ   → clothing
  HEALTHCQ  → health_care
  EDUCACQ   → education
  TOBACCCQ  → other_nondurables
  PERSCACQ  → other_nondurables
  READCQ    → other_nondurables
  MISCCQ    → other_services

PERINSCQ (in ETOTALC, outside TOTEXPCQ):
  LIFINSCQ  → financial_insurance
```

### Why Sub-Variables Instead of UCC-Level MTAB Data

The CEX publishes expenditure data at two levels of granularity:

1. **FMLI sub-variables** (used here): Pre-aggregated by BLS within the
   family-level interview files. Available as named columns (e.g., GASMOCQ).
2. **MTAB UCC codes**: Monthly expenditure detail at the Universal
   Classification Code level. Finer granularity but requires joining additional
   files, handling monthly-to-quarterly aggregation, and maintaining a
   UCC-to-PCE crosswalk that changes across survey vintages.

We use FMLI sub-variables because:
- They are directly available in the files we already read
- BLS has already performed the within-category aggregation
- The 33 sub-variables provide sufficient granularity for the 18 PCE categories
- No additional file joins or vintage-specific crosswalks are needed

The trade-off is that a few PCE categories have imprecise boundaries at the FMLI
level. Most notably, "Other durable goods" is mapped only to PETTOYCQ (pets,
toys, hobbies), missing items like luggage, jewelry, and therapeutic appliances
that would require UCC-level disaggregation. The PCE benchmarking step (Stage 2)
corrects levels but cannot fix compositional bias within categories.

### Excluded PCE Categories

**NPISH (Final consumption expenditures of nonprofit institutions serving
households):** This is a NIPA construct representing spending by religious
organizations, social welfare groups, and other nonprofits on behalf of
households. It has no CEX analogue. TPC (Rosenberg, 2015) also excludes it.

**Net foreign travel:** PCE defines this as spending by US residents abroad minus
spending by foreign visitors in the US. The CEX captures some travel spending
(via OTHLODCQ and PUBTRACQ) but has no concept of netting against foreign
visitor spending. TPC treats this as part of a residual category.

---

## Stage 2: PCE Benchmarking (NIPA Adjustment)

### Motivation

The CEX systematically undercounts consumption relative to NIPA PCE aggregates.
TPC (Rosenberg, 2015, Table A1) documents CEX coverage ranging from 22% (health
care) to 125% (gasoline). Without benchmarking, distributional shares would be
distorted by differential underreporting across categories.

### NIPA Control Totals

Target values are drawn from BEA NIPA Table 2.4.5U (Personal Consumption
Expenditures by Type of Product) for calendar year 2023. These are stored in
`resources/pce_targets_2023.csv`.

**Important:** The values in the CSV are approximate and should be verified
against the official BEA publication at
https://apps.bea.gov/iTable/?reqid=19&step=2. The `nipa_line` column
indicates the line number in Table 2.4.5U for cross-referencing.

### Scaling Algorithm

The benchmarking function `benchmark_to_pce()` in `src/pce_benchmark.R` operates
as follows:

**Step 1: Compute current weighted totals.**

For each PCE category k:

```
CEX_total_k = sum(value_k_i * weight_i * annualize) / 1e9
```

where `annualize = 4` converts quarterly CQ data to annual, and the division by
1e9 converts to billions to match NIPA units.

**Step 2: Compute scaling factors.**

```
sf_k = NIPA_target_k / CEX_total_k
```

A scaling factor of 2.0 means the CEX captures only 50% of the PCE for that
category. Factors less than 1.0 indicate CEX overshoot (common for gasoline
and food at home due to measurement differences).

**Step 3: Apply scaling.**

```
adjusted_k_i = value_k_i * sf_k
```

Each tax unit's category-level consumption is multiplied by the category's
scaling factor. This preserves relative variation across tax units within a
category while matching the aggregate to the NIPA target.

**Step 4: Cap consumption-to-income ratios at the 95th percentile.**

Following TPC, we prevent the NIPA scaling from inflating already-extreme
consumption-to-income ratios:

```
For tax units with positive income:
  total_C_i = sum(adjusted_k_i for all k)
  if total_C_i / income_i > p95_ratio:
    shrink_i = (p95_ratio * income_i) / total_C_i
    adjusted_k_i = adjusted_k_i * shrink_i for all k
```

The p95 threshold is computed from the post-scaling distribution of
consumption-to-income ratios among positive-income tax units.

After capping, the aggregate may fall slightly below the NIPA target. TPC
notes this is acceptable; the cap prevents a small number of extreme
observations from driving the distributional results.

### When to Apply Benchmarking

The benchmarking function is designed to operate on **weighted** data:

- **Validation (CEX):** Apply to pre-bootstrap CEX data using FINLWT21 weights.
  This lets you inspect the scaling factors and verify they are reasonable.
- **Production (PUF):** Apply to post-QRF-imputation PUF data using filing
  weights. This is the TPC approach—benchmark after imputation to the tax model,
  not before.

The benchmarking is **not** applied inside `cex.R` (the training data pipeline).
The QRF learns raw CEX-level consumption patterns. Benchmarking is a separate
post-processing step.

### Relationship to TPC Methodology

Our approach differs from TPC's in the imputation method (QRF vs. cell-based
donor matching) but follows TPC's benchmarking logic closely:

| Aspect | TPC (Rosenberg, 2015) | This Implementation |
|---|---|---|
| Number of categories | 16 | 18 (BEA PCE major) |
| Imputation method | Cell-based statistical match | Quantile Regression Forests |
| Benchmarking target | 2011 NIPA | 2023 NIPA |
| Cap threshold | 95th percentile | 95th percentile (configurable) |
| High-income treatment | Log-log regression for >$200K | QRF handles full income range |
| When benchmarking occurs | After match to tax model | After QRF prediction to PUF |
| Categories excluded | NPISH, financial services | NPISH, net foreign travel |

---

## Stage 3: Expenditure Shares

### Purpose

Expenditure shares are the key input to the **relative-price component** of
excise tax incidence. When an excise tax raises the price of gasoline relative
to other goods, the burden falls disproportionately on tax units that spend a
larger share of their consumption on gasoline.

### Computation

```
share_k_i = category_k_i / total_consumption_i
```

For tax units with zero total consumption, all shares are set to 0.

The function `compute_expenditure_shares()` in `src/pce_benchmark.R` adds
`_share` columns for each PCE category.

### Usage for Excise Tax Distribution

Following Rosenberg (2015), the excise tax burden has two components:

1. **Income-reduction component:** Excise taxes reduce real incomes. Assigned
   in proportion to each tax unit's share of burdened income sources (wages,
   above-normal capital income, wage-indexed transfers). This piece uses
   income data from the PUF, not consumption data.

2. **Relative-price component:** Excise taxes change the price of taxed goods
   relative to all other consumption. Assigned based on expenditure shares.
   TPC assumes the following retail-level pass-through rates:
   - 100% of alcohol and tobacco excises
   - 75% of gasoline excises
   - 50% of aviation and health excises
   - Remaining revenue treated as business cost, spread across all consumption

The expenditure shares computed here feed directly into the relative-price
component.

---

## Known Limitations

### CEX Coverage Gaps

Based on TPC Table A1 and general CEX literature, expected CEX-to-NIPA coverage
by category:

| Category | Expected Coverage | Notes |
|---|---|---|
| Gasoline | ~125% | CEX overshoots NIPA |
| Food at home | ~110% | Slight overshoot |
| Housing (rent/owned) | ~120% | Owned dwelling includes mortgage, not imputed rent |
| Health care | ~22% | Most health spending is third-party; CEX captures OOP only |
| Financial services | ~30% | Imputed financial intermediation (FISIM) not in CEX |
| Alcohol | ~29% | Severe underreporting |
| Other durables | ~24% | Many BEA categories not captured in PETTOYCQ |

The PCE benchmarking corrects levels via scaling factors, but cannot correct
compositional bias (e.g., if underreported health spending is disproportionately
from high-income units, the within-category distribution will be biased).

### Conceptual Differences Between CEX and NIPA

**Owner-occupied housing:** NIPA PCE uses imputed rental value for
owner-occupied housing (what the owner would pay in rent). CEX reports actual
costs (mortgage interest, property taxes, insurance, maintenance). These are
fundamentally different concepts. Our `housing` category uses CEX actual costs,
and the PCE scaling factor adjusts the aggregate to match NIPA's imputed-rent
total. This corrects levels but may distort the distribution if actual costs and
imputed rents are distributed differently across income groups.

**Health insurance:** NIPA PCE for health care includes the net cost of health
insurance (premiums minus benefits paid). CEX HEALTHCQ includes out-of-pocket
spending and some premium data. The scaling factor will be very large for this
category.

**Financial services:** NIPA includes Financial Intermediation Services
Indirectly Measured (FISIM), an imputed value for bank services provided without
explicit charges. CEX captures only explicit fees and insurance premiums.

### Quarterly vs. Annual Data

The CEX training data uses Current Quarter (CQ) variables, representing 3-month
expenditure totals. The `annualize` parameter in `benchmark_to_pce()` (default:
4) converts to annual for comparison with NIPA targets. The QRF training data
remains in quarterly units; annualization should occur after QRF prediction
(as currently done in `consumption.R` with the `* 4` multiplier).

### Excise-Relevant Sub-Categories

For excise tax analysis, some PCE categories bundle items with different excise
treatment:

- **Tobacco** is bundled into `other_nondurables` with personal care and reading.
  For tobacco excise distribution, split it out using the TOBACC_exp component.
- **Alcohol** is bundled into `food_off_premises` with food at home. For alcohol
  excise distribution, split using ALCBEV_exp.
- **Air travel** is bundled into `transport_services` with vehicle maintenance,
  rental, and public transit (PUBTRACQ includes airline fares). For aviation
  excise distribution, use TAIRFARC (a T-series table variable) if available, or
  estimate airfare share from PUBTRACQ.

The 18-category structure is designed for PCE benchmarking. For excise-specific
analysis, further disaggregation within categories may be needed using the
underlying _exp variables.

---

## Validation Checklist

Before using benchmarked data for distributional analysis:

1. **Sub-variable summation:** Verify that FMLI sub-variables sum to their
   parent aggregates (add the parent aggregates to the fread select list and
   compare). Discrepancies indicate missing sub-components or variable name
   changes in newer CEX vintages.

2. **Scaling factors:** Inspect `diagnostics$scale_factor` from
   `benchmark_to_pce()`. Factors between 0.5 and 5.0 are typical. Factors
   outside 0.1-10.0 suggest a mapping error or data issue.

3. **Post-benchmark totals:** The `diagnostics$post_benchmark` column shows
   actual post-scaling totals, which may fall below NIPA targets due to the
   p95 cap. Differences greater than 5% warrant investigation.

4. **Expenditure shares:** Verify shares sum to 1.0 for all tax units with
   positive total consumption.

5. **Distribution sanity checks:** Compare the income-quintile distribution
   of each category to TPC Table A4 (or equivalent updated estimates). Large
   deviations suggest mapping or imputation problems.

---

## References

- Rosenberg, Joseph. 2015. "The Distributional Burden of Federal Excise Taxes."
  Urban-Brookings Tax Policy Center.

- Toder, Eric, James Nunns, and Joseph Rosenberg. 2011. "Methodology for
  Distributing a VAT." Urban-Brookings Tax Policy Center.

- Bureau of Economic Analysis. NIPA Table 2.4.5U, Personal Consumption
  Expenditures by Type of Product.
  https://apps.bea.gov/iTable/?reqid=19&step=2

- Bureau of Labor Statistics. Consumer Expenditure Survey, Interview Survey
  Public-Use Microdata. https://www.bls.gov/cex/pumd.htm

- Bureau of Labor Statistics. CE PUMD Interview and Diary Dictionary.
  https://www.bls.gov/cex/pumd/ce-pumd-interview-diary-dictionary.xlsx

- Garner, Thesia I. et al. 2006. "The CE and the PCE: A Comparison." Monthly
  Labor Review, September 2006. Bureau of Labor Statistics.
