---
title: "Minimal example: SOI validation and target tables"
role: evidence
workstream: rebuild
status: current
updated: 2026-10-14
sot: research/rebuild/design/plan.md
supersedes: []
superseded_by: null
---

# SOI validation and target tables for the minimal example

These files hold the published IRS Statistics of Income (SOI) numbers that
the minimal experiment is checked against for tax year 2015 and tilted to for
tax year 2019. All of them come from SOI Publication 1304 (the "Complete
Report" basic tables). Nothing was downloaded for this. The script that
builds them is `research/rebuild/minimal/01_targets.py` (Python standard
library only, runs in a few seconds).

Files live in `/nfs/roberts/scratch/pi_nrs36/jar335/minimal/targets/`. This
README is also copied to `research/rebuild/minimal/01_targets_README.md`.

## The files

| File | What it holds |
|---|---|
| `soi_2015_validation.csv` | 2015: Table 1.4 (income items by AGI), Table 1.2 (returns and AGI by filing status and AGI), Table 2.3 (dependent exemptions by AGI). 893 rows. |
| `soi_2019_targets.csv` | 2019: Table 1.4 and Table 1.2, same layout as 2015. No dependents table exists for 2019 (see below). 640 rows. |
| `soi_totals_2015_2019.csv` | The all-returns total row of each item above, for both years, in one place. |
| `crosscheck_repo_copies.csv` | Every comparison of these numbers against Tax-Data's own hand-compiled copies of the same tables. |

All three data files share one long layout:

- `year`: tax year.
- `table`: the Pub 1304 table number (`1.4`, `1.2`, `2.3`).
- `filing_status`: `all`; or, from Table 1.2 only, `joint`, `separate`,
  `head_of_household`, `single`, and `nonjoint`. SOI publishes married filing
  jointly and surviving spouses as one group, so `joint` includes surviving
  spouses. `nonjoint` is not published: it is separate + head of household +
  single, added up here.
- `agi_bracket_lo`, `agi_bracket_hi`, `agi_bracket_label`: the AGI class (see
  below). The published total row has `agi_bracket_label = all` and bounds
  `-Inf` and `Inf`.
- `variable`, `statistic`: what the number is. `statistic` is `count` (number
  of returns, or number of exemptions for the Table 2.3 exemption items) or
  `amount` (dollars).
- `value`: the number.
- `flag`: SOI's disclosure mark on the cell, if any (`caution` for a small
  sample, `combined` for a cell merged with a neighbour). No cell used here
  carries one; the column is kept so that a future extension shows them.
- `source`: the SOI table, SOI's own file name for it, and the file it was
  read from. Rows computed here start with `DERIVED:` and say how.

## Where each row came from

Every published number was read from the SOI mirror in the shared raw-data
store, `/nfs/roberts/project/pi_nrs36/shared/raw_data/IRS-Ind/aligned/`. That
store holds every published cell of the Pub 1304 tables for 2011 to 2023 in
long form, with SOI's marks kept. It was built from SOI's own Excel files;
its manifest records the URLs. The files used:

| Table | SOI file | Mirror file | Years used |
|---|---|---|---|
| 1.4, "All returns: sources of income, adjustments, and tax items, by size of AGI" | `15in14ar.xls`, `19in14ar.xls` | `bysize_income_sources.csv` | 2015, 2019 |
| 1.2, "All returns: AGI, exemptions, deductions, and tax items, by size of AGI and by marital status" | `15in12ms.xls`, `19in12ms.xls` | `bysize_marital_status.csv` | 2015, 2019 |
| 2.3, "All returns: number of exemptions and AGI, by type of exemption and size of AGI" | `15in23ar.xls` | `bysize_exemptions.csv` | 2015 only |

Only the "All returns" panel of each table is used; the taxable and
nontaxable sub-panels are left out.

Why not Tax-Data's own copies? The repository's `Compiled-SOI-Tables`
interface (read by `src/process_targets.R`) was compiled by hand and covers
less: its pinned version (v2) has Table 1.4 for 2017–2019 only, Table 1.6
(returns by filing status and age) for 2015 and 2017, and Table 2.3 for 2017
only. It has no Table 1.2 at all. An older version (v1) does carry Table 1.4
for 2015. So the repository's copies were used as a check instead:

- **Table 1.4, 2015**, against v1 `SOI 1.4.csv`: returns, AGI, wages, both
  interest items, both dividend items, Schedule D gain and loss, count and
  amount, all 19 AGI classes. 304 comparisons, all exact.
- **Table 1.4, 2019**, against v2 `table_1_4.csv` (the version `main.R`
  reads): the same items except returns and AGI, which that file lacks. 266
  comparisons, all exact.
- **Returns by filing status, 2015**: Table 1.2 here against the repository's
  Table 1.6 summed over age. 76 comparisons; 45 exact, 31 differ by one or
  two returns, which is SOI rounding the age cells separately.

## Variables

From Table 1.4 (filing status `all`, 19 AGI classes plus the total):

| `variable` | SOI column | Notes |
|---|---|---|
| `returns` | Number of returns | |
| `agi` | Adjusted gross income less deficit | |
| `wages` | Salaries and wages | |
| `taxable_interest` | Taxable interest | |
| `tax_exempt_interest` | Tax-exempt interest | |
| `ordinary_dividends` | Ordinary dividends | Includes qualified dividends. |
| `qualified_dividends` | Qualified dividends | A part of ordinary dividends. |
| `capital_gain_distributions_no_sch_d` | Capital gain distributions reported on Form 1040 | Returns that reported distributions directly on the 1040 without Schedule D. |
| `sch_d_net_gain` | Sales of capital assets, Schedule D, taxable net gain | |
| `sch_d_net_loss` | Sales of capital assets, Schedule D, taxable net loss | The loss actually deducted (at most $3,000, or $1,500 married filing separately), published as a positive number. Not the full loss. |
| `interest_total` | DERIVED | Taxable plus tax-exempt interest. Amount only: a return can have both, so the two counts cannot be added. |
| `net_capital_gain` | DERIVED | Distributions without Schedule D + Schedule D net gain − Schedule D net loss. This is the Form 1040 capital gain line, which is the PUF's `E01000`. Amount only. |

From Table 1.2 (by filing status): `returns` and `agi`, 19 AGI classes plus
the total, for `all`, the four published groups and the derived `nonjoint`.

From Table 2.3 (2015, filing status `all`, its own AGI classes):
`returns` (all returns); `returns_with_dependent_exemptions` and
`dependent_exemptions` (returns claiming any dependent, and the number of
dependents they claim); and the same pair for each type of dependent:
children at home, children away from home, parents, other dependents.

## AGI classes

- Lower bound included, upper bound excluded: `[lo, hi)`, in dollars of AGI.
- Tables 1.4 and 1.2 use the same 19 classes: no AGI (`-Inf` to `1`, which is
  AGI of zero or less), $1–5,000, then 5,000 steps to $30,000, then $30–40k,
  $40–50k, $50–75k, $75–100k, $100–200k, $200–500k, $500k–1M, $1–1.5M,
  $1.5–2M, $2–5M, $5–10M, and $10M or more (`hi = Inf`).
- Table 2.3 uses different classes: under $5,000 (which includes zero and
  negative AGI), then $5,000 steps to $60,000, then $60–75k and the same
  classes as Table 1.4 from $75,000 up.
- The published total does not always equal the sum of its classes exactly,
  because SOI rounds each cell. The largest gap is 3 units (3 returns, or
  $3,000 of amount). Do not add the total row to the class rows.

## Units

SOI publishes money amounts in thousands of dollars. Every `amount` here has
been multiplied by 1,000 and is in dollars. Counts are as published. All
values are SOI sample estimates, not full counts.

## Totals, all returns

| | 2015 | 2019 |
|---|---|---|
| Returns | 150,493,263 | 157,796,807 |
| Joint returns (incl. surviving spouses) | 54,294,820 | 54,796,116 |
| Wages | $7,112.2 billion | $8,273.1 billion |
| Taxable interest | $95.9 billion | $153.3 billion |
| Tax-exempt interest | $61.9 billion | $61.6 billion |
| Interest, total | $157.8 billion | $214.8 billion |
| Ordinary dividends | $260.3 billion | $331.8 billion |
| Net capital gain (1040 line) | $706.5 billion | $864.7 billion |
| of which Schedule D net gain | $713.6 billion | $869.8 billion |
| of which distributions without Schedule D | $11.6 billion | $11.2 billion |
| of which deducted net loss | −$18.6 billion | −$16.3 billion |

## What is not here

- **Income items by filing status.** Wages, interest, dividends and capital
  gains split by joint and nonjoint are published in Pub 1304 Table 1.3. No
  copy exists in the repository or in the shared raw-data mirror, which lists
  Table 1.3 as not yet mirrored. Only returns and AGI by filing status are
  here (Table 1.2). Getting Table 1.3 would mean downloading it for 2015 and
  2019 from SOI's Pub 1304 page; that was not done.
- **Returns by number of dependents (0, 1, 2, 3 or more).** SOI does not
  publish this split for all returns in either year. For 2015, Table 2.3
  gives the number of returns claiming any dependent and the number of
  dependents claimed, by AGI. For 2019 there is nothing: Table 2.3 ended
  with tax year 2017, because from 2018 the Tax Cuts and Jobs Act set the
  personal exemption to zero. The nearest published 2019 items are Table 2.5 (earned
  income credit returns by number of qualifying children, credit claimants
  only) and the count of returns claiming the child tax credit or the credit
  for other dependents (39.7 million in Pub 4801 for 2019). Neither is
  extracted here.
- **Returns by marital status other than filing status.** Not needed; Table
  1.2's groups are filing statuses.

No published cell used here is blank or carries an SOI disclosure mark, so
nothing had to be left missing or filled.

## Note on the front matter

`role: report` is not in the closed list of roles in `research/CONVENTIONS.md`
(`plan`, `method`, `procedure`, `evidence`, `notes`, `review`, `status`,
`index`), so `research/tools/check_conventions.R` would flag this file.
`evidence` is the nearest allowed role.
