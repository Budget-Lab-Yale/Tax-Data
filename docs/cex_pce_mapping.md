# CEX → PCE Category Mapping

This doc describes the two-sided bridge that the consumption imputation rests on:
CEX FMLI CQ sub-variables → 8 collapsed `c_*` categories (built in `src/cex.R`),
and 8 `c_*` categories → BEA NIPA fine 20-category PCE targets (used by
`src/pce_benchmark.R` for benchmarking).

## The 8 collapsed categories

| `c_*` category | What it covers |
|---|---|
| `c_clothing` | Apparel and footwear |
| `c_motor_vehicles` | Vehicle purchases (cars, trucks, motorcycles, aircraft) |
| `c_durables` | Household equipment, rec goods, medical durables, pet/hobby supplies |
| `c_other_nondurables` | Tobacco, personal care products, reading, Rx drugs |
| `c_food_off_premises` | Groceries + alcohol bought for home |
| `c_gasoline` | Gasoline and motor oil |
| `c_housing_utilities` | Owned + rented dwellings + gas/electric/fuel/water |
| `c_other_services_health` | Everything else — health, insurance, education, recreation, travel, dining out, misc services |

## CEX side: FMLI CQ → `c_*`

CEX values are read from the FMLI summary file. Each CQ variable is a BLS-
published per-household quarterly sum of UCCs; values are CQ + PQ (full
3-month reference period), annualized × 4, and CPI-deflated to 2017 dollars
in `src/cex.R:176-180`. The per-tax-unit allocation then multiplies by
`CU_pct = tu_size / FAM_SIZE`.

| `c_*` category | FMLI CQ inputs |
|---|---|
| `c_clothing` | `APPARCQ` (apparel + services; includes jewelry — a minor leakage) |
| `c_motor_vehicles` | `CARTKNCQ` (new) + `CARTKUCQ` (used) + `OTHVEHCQ` (motorcycles/aircraft) |
| `c_durables` | `HOUSEQCQ` (furnishings/appliances) + `PETTOYCQ` (pets/toys/hobbies) + `TVRDIOCQ` (audio/video) + `MEDSUPCQ` (eyeglasses/hearing aids/medical equipment) |
| `c_other_nondurables` | `TOBACCCQ` + `PERSCACQ` (personal care) + `READCQ` + `PREDRGCQ` (drugs) |
| `c_food_off_premises` | `FDHOMECQ` (groceries) + `ALCBEVCQ` |
| `c_gasoline` | `GASMOCQ` |
| `c_housing_utilities` | `OWNDWECQ` + `RENDWECQ` + `NTLGASCQ` + `ELCTRCCQ` + `ALLFULCQ` (other fuels) + `WATRPSCQ` (water) |
| `c_other_services_health` | `TELEPHCQ` + `HOUSOPCQ` (domestic/repair services) + `MISCCQ` + `OTHENTCQ` + `MAINRPCQ` (vehicle maint) + `VRNTLOCQ` (vehicle rental/license) + `PUBTRACQ` + `FEEADMCQ` (entertainment admissions) + `FDAWAYCQ` (restaurants) + `OTHLODCQ` (hotels) + `HLTHINCQ` (health insurance) + `MEDSRVCQ` (medical services) + `EDUCACQ` + `LIFINSCQ` + `VEHINSCQ` + `VEHFINCQ` |

## NIPA side: `c_*` → fine 20-category PCE

Defined in `src/pce_benchmark.R::pce_collapse_map`. Each `c_*` category
aggregates one or more of BEA's 20 fine PCE lines. Benchmarking computes
the sum of NIPA targets for the fine categories and scales the `c_*`
column to hit that total.

| `c_*` category | NIPA fine categories | NIPA 2017$ target ($B) |
|---|---|---|
| `c_clothing` | `clothing` (Clothing and footwear) | 414.5 |
| `c_motor_vehicles` | `motor_vehicles` (Motor vehicles and parts) | 606.0 |
| `c_durables` | `other_durables` + `furnishings` + `rec_goods` | 1,123.1 |
| `c_other_nondurables` | `other_nondurables` (residual: drugs, household supplies, tobacco, personal care, reading, etc.) | 1,258.3 |
| `c_food_off_premises` | `food_off_premises` | 1,161.6 |
| `c_gasoline` | `gasoline` (Gasoline and other energy goods) | 375.4 |
| `c_housing_utilities` | `housing` + `utilities` | 2,726.0 |
| `c_other_services_health` | `communication` + `npish` + `other_services` + `transport_services` + `rec_services` + `net_foreign_travel` + `food_accommodations` + `health_care` + `education` + `financial_insurance` | 7,544.5 |

Total NIPA 2017$ = $15,209.5B. Nominal 2017 PCE is $13.3T; the $15.2T
figure reflects CPI-deflation of 2023-dollar targets in
`resources/pce_targets_2023.csv` using the CPI-U ratio 1.23825. See
*Known issues* below — the target-year-mismatch is a live bug.

## Known issues with the mapping

1. **HKPGSUPP (housekeeping supplies, ~$200B NIPA) has no FMLI CQ variable.**
   UCCs for detergents, paper towels, household cleaners, postage, and
   stationery roll up somewhere inside `HOUSEQCQ` or `MISCCQ` in BLS's
   published summaries but are not exposed as a named CQ sub-variable.
   Retrieving them requires MTBI-level UCC parsing, which we deliberately
   do not do. This leaves a permanent gap in `c_other_nondurables`.

2. **Drug expenditures are captured only as household out-of-pocket.**
   `PREDRGCQ` is small (~$100/CU/year) because most prescription drug
   spending flows through insurance reimbursement, which households
   report under `HLTHINCQ` as premiums (routed to services). The NIPA
   "other nondurables" line counts the full retail drug value — so
   `c_other_nondurables` will never close the gap to NIPA via FMLI CQ.
   Post-rework scale factor is ~8.65× (down from 11.42× pre-rework).

3. **`c_motor_vehicles` exceeds NIPA** (scale factor 0.89 in 2017$). This
   is a definitional gap: NIPA line 4 subtracts consumer-to-consumer used
   vehicle sales; CEX household reports do not capture the receipts side.
   113% coverage is canonical and not a classification bug. Benchmarking
   shrinks this category — which pulls per-household variation downward.

4. **Target-year mismatch.** `resources/pce_targets_2023.csv` holds
   2023-dollar NIPA values; `project_puf.R:25` applies them to 2017-dollar
   PUF. Scale factors are biased up by ~1.24×. Resolution: either generate
   `pce_targets_2017.csv` or move benchmarking into the aging layer so
   each category grows via `variable_guide$grow_with` (mirrors how
   mortgage/wages already work). The latter is the cleaner architectural
   choice.

5. **APPARCQ includes jewelry/watches** (NIPA `other_durables`), leaking
   ~5% of `c_clothing` into the wrong NIPA bucket. Minor.

6. **OTHENTCQ mixes goods and services** (boats/RVs vs arcades/admissions).
   Currently 100% routed to services. Minor aggregate impact.

## References

- `src/cex.R:152-162` — FMLI CQ variable list
- `src/cex.R:239-252` — CEX → `c_*` mapping
- `src/pce_benchmark.R:12-28` — `c_*` → fine NIPA map
- `resources/pce_targets_2023.csv` — NIPA fine targets (2023$, to be deflated)
- `resources/cex/stubs/CE-HG-Integ-2023.txt` — BLS stub file defining UCC hierarchy
- `src/build_ucc_bridge.R` + `resources/ucc_pce_bridge.csv` — UCC → fine NIPA crosswalk (reference only, not used at runtime)
