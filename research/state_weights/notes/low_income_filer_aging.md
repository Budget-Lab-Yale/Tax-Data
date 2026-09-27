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

## Where it probably comes from

The TY2023 file is the 2017 base year aged forward (`src/project_puf.R`), with non-filers from
the Mok (2017) below-threshold probits (estimated on 2007 linked data) and the Pub 5785
above-threshold hazard (`src/nonfilers/filing_model.R`). Neither step knows that low-income
filing fell between 2019 and 2023, mostly among young single filers and dependents who were not
required to file. Whether the Phase 3 per-year filer fit (2017–2023, branch `phase3-refit`)
already targets low-income filer counts by year is the first thing to check.

**Vintage timing.** The vintage read here, `2026091119`, predates C9 (the ASEC non-filer pool,
merged 2026-09-21 as PR #31; `research/state_weights/plan.md` §2). The first check is whether a
post-C9 vintage, and the Phase 3 re-fit, already move low-income filers and non-filers toward
HT2; if they do, the Affordability-Index pin should move and this note can close.

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

The fix belongs here: the file's low-income filer and non-filer counts by year, age and filing
status should follow HT2 and Tables 1.6–1.7. Until it lands, Affordability-Index will overstate
TY2023 low-income filing, and any Tax-Simulator run on the TY2023 file will carry too many
low-income returns.
