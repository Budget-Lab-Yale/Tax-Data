---
title: "Low-income filer counts in the aged file miss the TY2019-to-TY2023 decline"
role: notes
workstream: state_weights
status: open
updated: 2026-09-26
sot: research/state_weights/plan.md
supersedes: []
superseded_by: null
---

# Low-income filer counts in the aged file miss the TY2019-to-TY2023 decline

*Raised 2026-09-26 by the Affordability-Index income construction (JI), a downstream
consumer that matches ACS tax units to Tax-Data v1 records and takes each unit's filing
status from its donor. Vintage read: `2026091119`, `baseline/tax_units_2019.csv` and
`tax_units_2023.csv`. This is a fix-in-Tax-Data problem: the consumer cannot supply filing
behaviour the donor file does not carry.*

## What the IRS shows

Returns with wages at AGI $1–10,000 fell by a third between TY2019 and TY2023, and those at
$10–25,000 by almost a quarter (HT2, national sum of states, `N00200` by `AGI_STUB`):

| Returns with wages | TY2019 | TY2023 | Change |
|---|---|---|---|
| AGI $1–10,000 | 14.24M | 9.33M | −34% |
| AGI $10–25,000 | 24.13M | 18.69M | −23% |

Where the drop sits (SOI Pub. 1304):
- **Age** (Table 1.6, all returns at AGI $1–10,000): ages 18–25 fell from 7.67M to 4.99M
  (−35%); every other age under 65 fell 17–25%; 65 and over rose 7%.
- **Filing status** (Table 1.6): single returns at $1–10,000 fell from 16.55M to 12.71M, almost
  the whole decline.
- **Dependents** (Table 1.7): dependent returns with wages at AGI $1–10,000 fell from 6.08M to
  3.75M, about half of the decline. Other low-wage returns still fell about 32%.

## What the donor file shows

Weighted records with wages (weight > 0), classed by the record's total income as an AGI proxy
(wages, self-employment, interest and dividends, rents, gross Social Security, pension and IRA
distributions, other income, capital gains):

| Records with wages, total income $1–10,000 | TY2019 | TY2023 | Change |
|---|---|---|---|
| Filers | 13.59M | 12.11M | −11% |
| Non-filers | 0.89M | 1.06M | |
| Share filing | 94% | 92% | |
| Filers aged 18–25 | 9.48M | 8.59M | −9% |
| Non-filers aged 18–25 | 0.12M | 0.09M | |

At total income $10–25,000: filers 21.62M to 18.56M (−14%) against the IRS's −23%.

So in TY2019 the file's low-income wage filers sit close to HT2 (13.6M against 14.2M); in TY2023
they are about 30% above it (12.1M against 9.3M), and the file has almost no low-income
non-filers to offer in their place, fewest of all among young adults.

## Why it matters downstream

Affordability-Index draws each ACS unit's candidates from the file and inherits the donor's
`filer` flag. Its prior object (the match before any fitting) has returns with wages at
$1–10,000 at +16.7% against HT2 in TY2023 and −8.4% in TY2019; the fit, which can only move
weight onto non-filer records that exist and is capacity-limited, brings TY2023 to +11.3%. The
ACS itself is not the cause: ACS units with wages at that gross income moved 13.76M to 13.27M,
and a larger reference-period adjustment at the bottom moves the class by under 3% (a 5% extra
scaling of ACS 2023 incomes moves it 2.3%).

## Where it comes from (first check done 2026-09-26)

**Neither C9 nor Phase 3 fixes it.**
- *C9.* `2026091119` does not predate the ASEC pool, as first written here: its
  `dependencies.csv` reads the same `ASEC-Nonfilers` vintage (`2026091201`) as the post-merge
  rebuild `2026092215_postfix_s33`, and the two agree to two decimals on every count in the
  tables above and below. A newer vintage will not move the Affordability-Index pin.
- *Phase 3.* The per-year filer fit (`scripts/phase3_fit.R`) splits each record's national
  weight across states; `save_fit()` asserts every record's shares sum to 1. It cannot change a
  national count by income class.

**The mechanism is within-cell income composition, not the filing decision.** For 2018–2023
`compute_weights_for_year()` (`src/project_puf.R`) scales every 2017 filer by one factor per
filing status × age group, the Table 1.6 return-count ratio to 2017
(`resources/return_counts_2023.csv`). The factor has no income dimension; weights are fit to
returns by AGI class only in the 2017 reweight (`config/target_info/baseline.csv`). Those cell
counts barely moved: single returns under 26 went 23.01M (TY2019) to 22.26M (TY2023), −3%. So
young single filers did not stop filing: most of the −34% at AGI $1–10,000 is returns moving
*up* out of the class, which the file cannot reproduce because a record's income grows only by
average per-variable factors and nothing re-targets the distribution within a cell. The filing
model and the non-filer pool are not the problem.

**Dependent filers are almost the whole gap.** Wage filers at total income $1–10,000 (the proxy
above, weight > 0, file `dep_status`), against the IRS figures quoted in the first section:

| Wage filers, total income $1–10,000 | TY2019 file | TY2019 IRS | TY2023 file | TY2023 IRS |
|---|---|---|---|---|
| Dependent filers | 7.11M | 6.08M | 6.61M | 3.75M |
| All other filers | 6.80M | 8.16M | 5.74M | 5.58M |

In TY2023 dependents carry about 2.9M of the roughly 3.0M excess; other filers sit within 3%.
Dependent filers were already 17% high in TY2019, because no reweight targets dependent status,
and the file then holds them nearly flat (−7%) while the IRS count fell 38%. (The proxy
differs slightly from the one behind the table above, hence 13.91M rather than 13.59M
TY2019 filers.) Caveats: records are classed by total income, since the file carries no AGI, and
the IRS figures were not re-pulled for this check.

**What a fix takes.** Refit filer weights for each observed year 2018–2023 to returns by AGI
class × filing status × age group, plus dependent returns by AGI class, with the 2017 machinery
(`reweight_lp()` in `src/reweight.R`). It changes filer results in every year from 2018 on,
Tax-Simulator's included. The pinned `Compiled-SOI-Tables` vintage (`2023100323`) cannot supply
the targets: Table 1.4 ends at TY2019 and Table 1.6 by AGI covers only 2015 and 2017.

**The targets exist on disk (checked 2026-09-26).** The IRS-Ind mirror (under
`raw_data_root()`, `IRS-Ind/national/by_size/`, with source URLs and md5s in its
`manifest.csv`) holds the published Pub 1304 tables for every year TY2018–TY2023. TY2023 is
stamped March 2026.

| Table | File | Crosses | Gives the refit |
|---|---|---|---|
| 1.6 | `returns_marital_age_{year}.xls` | age × filing status × 19 AGI classes | the three-way return counts the 2017 LP (linear-programming reweight) targets |
| 1.4 | `income_sources_{year}.xls` | same 19 AGI classes | returns with wages by AGI |
| 1.7 | `dependent_returns_{year}.xls` | AGI only, 12 classes that nest into 1.6's | dependent returns by AGI |

- *Table 1.6.* The layout is the same every year: an all-returns block with seven age bands
  (Under 18 split out), then MFJ+QSS (married filing jointly plus qualifying surviving
  spouse), MFS (married filing separately), HoH (head of household) and single blocks with six
  bands, Under 26 first. Its status × age totals are exactly `resources/return_counts_2023.csv`
  (single, under 26, TY2023: 22,258,754 in both). Disclosure-combined cells (`**`) read as 0
  through `readxl`, and the count moves into a neighbouring age row. The subagent that read the
  files reports 14–38 such cells a year, worst in TY2018. A three-way fit has to merge these
  cells, not target zeros.
- *Table 1.4.* TY2022–2023 relabel the wage-count column "Total wages". That concept is
  broader than the earlier "Salaries and wages"; the Form W-2 sub-column is the closer
  continuation.
- *Table 1.7.* Dependent returns number 9.46M, 9.55M, 6.86M, 7.23M, 7.54M and 7.53M for
  2018–2023. The 2020 drop of about 2.7M persists.
- *HT2.* It has no dependent-filer field, and it is a different estimand from Pub 1304
  (processed returns rather than a sample), so the refit should target Pub 1304 alone.

`src/nonfilers/sw_soi.R` already opens Tables 1.6 and 1.7 (`read_pub1304_t16()`,
`read_pub1304_t17_total()`), but it reads only the all-AGI total column. The AGI columns need
a new reader. The alternative is to extend `Compiled-SOI-Tables`, whose v3 vintage
(`20250319`) adds only TY2022 and has no builder code.

## Checks that would settle it

For each tax year the file covers, against SOI:
1. Returns with wages by AGI class, against HT2 `N00200` (national sum of states).
2. Returns by age x filing status x AGI class, against Table 1.6.
3. Dependent returns by AGI class, against Table 1.7.

A minimal tabulation, run from the Affordability-Index repo root (its `load_donor_pool()`
evaluates the bridge concepts; `total_income` is their sum):

```r
source("R/00_utils.R"); source("R/00_inputs.R"); source("R/02_income/05_candidates.R")
d <- load_donor_pool(file.path(resolve_input("tax_data")$path, "tax_units_2023.csv"))
d |> dplyr::filter(wages > 0, total_income >= 1, total_income < 1e4) |>
  dplyr::summarise(filers = sum(weight[filer == 1]), nonfilers = sum(weight[filer == 0]))
```

The fix belongs here: the file's low-income filer counts by year, age, filing status and
dependent status should follow HT2 and Tables 1.6–1.7. Until it lands, Affordability-Index will overstate
TY2023 low-income filing, and any Tax-Simulator run on the TY2023 file will carry too many
low-income returns.
