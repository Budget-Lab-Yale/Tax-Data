---
title: "Gross Social Security in the aged file: right in total, off in count and AGI profile; non-filers short"
role: notes
workstream: state_weights
status: open
updated: 2026-09-28
sot: research/state_weights/plan.md
supersedes: []
superseded_by: null
---

# Gross Social Security in the aged file

*Raised 2026-09-28 by the Affordability-Index income construction (JI), which now takes household
Social Security from the donor's `gross_ss` (its decision D77; audit in its
`docs/25_social_security_source.md`). Vintage read: `2026091119` (built 2026-09-11, before the
S40 filer refit and S24), `baseline/tax_units_2019.csv` and `tax_units_2023.csv`, weight > 0.
Donor AGI is the pinned Tax-Simulator (`main` 4818d7b) run on each record as filed. First thing
to check: how much of this S40 already changes.*

## 1. Filers against SOI Table 1.4

| | TY2019 file | SOI | gap | TY2023 file | SOI | gap |
|---|---|---|---|---|---|---|
| Returns with benefits | 31.05M | 30.02M | +3.4% | 34.10M | 32.10M | +6.2% |
| Gross benefits (line 6a) | $729.7B | $718.9B | +1.5% | $947.7B | $973.6B | −2.7% |
| Returns with taxable benefits | 22.10M | 22.42M | −1.4% | 25.78M | 25.72M | +0.2% |
| Taxable benefits (line 6b) | $360.2B | $360.0B | +0.0% | $507.5B | $527.1B | −3.7% |

Average benefit per return, TY2023: $27,790 in the file against $30,328 in SOI (−8%). The
aging after 2019 (`src/project_puf.R:505-610`: OASDI outlays over the growth in filer records
with benefits) carries too many recipients at too little each. Nothing calibrates `gross_ss`
after the 2017 rescale (`src/create_2017_puf.R:71-126`), and the count of returns with benefits
is never targeted.

**By AGI (returns with benefits, millions, file / SOI):**

| AGI | TY2019 | TY2023 |
|---|---|---|
| under $1 | 1.19 / 0.93 | 1.30 / 0.92 |
| $1–5k | 2.64 / 1.92 | 2.47 / 2.01 |
| $5–10k | 2.60 / 1.91 | 2.34 / 1.76 |
| $10–15k | 1.86 / 2.08 | 2.09 / 1.75 |
| $15–20k | 1.18 / 1.75 | 1.08 / 1.62 |
| $20–25k | 1.41 / 1.49 | 0.95 / 1.36 |
| $25–30k | 1.82 / 1.39 | 0.99 / 1.16 |
| $30–40k | 2.17 / 2.25 | 3.00 / 2.14 |
| $40–50k | 1.37 / 1.86 | 2.08 / 1.97 |
| $50–75k | 3.25 / 4.45 | 3.21 / 4.47 |
| $75–100k | 3.73 / 3.32 | 3.34 / 3.70 |
| $100–200k | 6.00 / 4.95 | 8.51 / 6.67 |
| $200–500k | 1.55 / 1.40 | 2.37 / 2.12 |

Dollars follow the same pattern (TY2023: $1–10k $108B / $87B; $15–25k $47B / $76B; $50–75k
$87B / $126B; $100–200k $282B / $246B; $200–500k $75B / $91B). The profile is off by 20–40% in
both directions in both years. Under $10k the excess is the same one the low-income filer note
(`low_income_filer_aging.md`) found for wage returns.

**Suggested:** calibrate returns with benefits and gross benefits to Table 1.4 by AGI class in
each year Table 1.4 exists (TY2023 is published), as the 2017 rescale does, rather than aging
from 2019; or add `count__gross_ss` / `amount__gross_ss` by AGI to the refit targets.
Compiled-SOI-Tables `historical/table_1_4.csv` stops at 2019; the TY2023 file is
`IRS-Ind/national/by_size/income_sources_2023.xls`, sheet TBL14, columns CH–CK.

## 2. Non-filers against SSA

SSA OASDI cash benefits paid (Annual Statistical Supplement Table 4.A4), less payments abroad
and to the outlying areas (Table 5.J1): $1,031.9B (2019), $1,359.1B (2023).

| $B | 2019 | 2023 |
|---|---|---|
| SSA, domestic | 1,031.9 | 1,359.1 |
| File, all records | 929.2 (90%) | 1,179.7 (87%) |
| SOI Table 1.4 (filers) | 718.9 | 973.6 |
| Children's benefits (Tables 4.A5/4.A6) | 35.4 | 42.0 |
| Residual: adults not on returns | ≈ 277.6 | ≈ 343.5 |
| File, non-filers | 199.5 (−28%) | 232.0 (−32%) |

Approximate (railroad tier-1 benefits on returns, group quarters and late filers are not
allocated), but the direction is clear: non-filer benefits are about 30% short. The pool's
`gross_ss` is uncorrected CPS ASEC `INCSS`; CPS-ASEC-Corrected measures CPS Social Security at
82–87% of SSA dollars, with 15% false-negative receipt among respondents and 48.5% for DI. Its
joint SSI/OASDI receipt correction is planned, not built. The pool also drops dependents' own
benefits (every record `dep_status = 0`): $42B of children's benefits in 2023 appear nowhere
in the file.

## 3. Medicare premiums: no change needed

Line 6a is Form SSA-1099 box 5, which includes the Medicare premiums deducted (IRS Pub 915:
don't reduce box 5 by them; SSA POMS GN 05002.010), so E02400 is gross of the premium. The CPS
ASEC records gross too. No add-back on either side.

## What would help downstream, in order

1. The TY2023 count and AGI profile of filers' benefits against Table 1.4 (after S40).
2. A target on non-filer benefits from the SSA-minus-SOI residual, or the corrected CPS values
   once CPS-ASEC-Corrected builds them.
3. Whether dependents' own benefits should be carried (not needed for tax; downstream fills
   them from the ACS in any case).
