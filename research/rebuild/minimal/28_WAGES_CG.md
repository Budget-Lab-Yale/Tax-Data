---
title: "Step 28: wages and capital gains, the 2016 SCF wave against the 2015 PUF"
role: evidence
workstream: rebuild
status: current
updated: 2026-10-17
sot: research/rebuild/design/plan.md
supersedes: []
superseded_by: null
---

# Step 28: wages and capital gains, SCF against PUF

This note repeats step 25's comparison of interest and dividends
(`design/reporting_rules.md`, section 6) for wages and capital gains. It has
two purposes: to decide whether the SCF's wages can be treated as reported
correctly, and to show what reporting pattern SCF capital gains have before
they are mapped.

Script `28_wages_cg.py`, run by `28_run.sbatch`. Full tables are in scratch
`checks/step28/wages_cg.md`.

## What was compared

- **SCF:** the 2016 wave from table E4N. The wave asks about 2015 income,
  and the table builder records those rows as tax year 2015
  (`01_build_table.R`: year = wave − 1). Amounts are the raw interview items
  in 2015 dollars. All five implicates are used with their weights, giving
  139.6 million units. Split-off units are not in the table (see below).
- **PUF filers:** the 2015 PUF without its four aggregate records, 150.8
  million returns.
- **PUF plus crude non-filers:** the step 23 stopgap, which is the 2017 CPS
  non-filer pool deflated to 2015 dollars with CPI-U, 31.2 million units.
  These units have wages but no capital gains.
- **IRS:** SOI Publication 1304, table 1.4, 2015, all returns.
- **Missing values:** there are none in the weights, wages or capital gains
  of table E4N, or in the raw PUF columns read.

### How each side defines wages

- **PUF (E00200):** Form 1040 line 7, which is W-2 box 1. It excludes
  elective deferrals to 401(k)-type plans and pre-tax health premiums.
- **SCF (X5702):** "wages and salaries" for the primary economic unit, asked
  before deductions. It probably includes elective deferrals. The size of
  that difference has not been estimated.
- **Non-filers:** CPS wages, which are also before deductions.
- **Different units.** An SCF unit puts the wages of everyone in the
  primary economic unit together, including dependents. On the PUF side,
  dependents who file make their own returns (9.3 million). The SCF's
  financially independent adults (split-off units: 23.4 million units with
  $348 billion of wages) were dropped from the table on 14 October.

### How each side defines capital gains

- **SCF (X5712):** "net gains or losses from mutual funds, stocks, bonds,
  real estate". The 2016 codebook ties it to Form 1040 lines 13 and 14
  (`01_REPORT.md`, section 6). Lines 13 and 14 include capital gain
  distributions from mutual funds and exclude gains inside retirement
  accounts and unrealized gains. I did not read the exact question text, so
  whether respondents actually leave out retirement-account gains is not
  checked. Losses are asked as the respondent's loss with no $3,000 limit
  stated. In practice the reports are a mix:
  - 0.93 million of the 2.47 million SCF units with a loss report exactly
    −$3,000, which is the line-13 figure after the limit;
  - 0.70 million report a loss larger than $3,000.
- **PUF, the table's variable (E01000):** this is not all of line 13. It is
  the Schedule D result after the loss limit. **Surprise:** for all 4.37
  million returns that report capital gain distributions directly on line
  13 without a Schedule D (E01100, $11.8 billion), E01000 is zero. Those
  dollars sit in `other_agi`. The full line 13 is E01000 + E01100. With
  E01100 added, the PUF's count of returns with a gain is 16.1 million,
  against 16.0 million in the IRS table. Without it, the count is 11.7
  million. This affects every earlier step that used `capital_gains`.
- **PUF before the loss limit:** Schedule D lines 7 + 15 (P22250 + P23250)
  plus E01100. This sum includes losses carried over from earlier years.
  It is an approximation, because for 4.5 million weighted returns it
  conflicts with E01000 by more than rounding. Two examples: 1.66 million
  returns have a positive Schedule D sum but a line-13 loss, and 1.99
  million have no Schedule D amounts but a nonzero E01000. The cause has
  not been established; one possibility is the PUF's disclosure
  protection.
- **Line 14** (E01200, other gains from Form 4797) is small: $1.3 billion
  on 2.3 million returns.

## Wages

Totals (2015 dollars):

| | Units (millions) | Total ($ billion) | Units with wages (millions) | Share with wages |
|---|---:|---:|---:|---:|
| SCF 2016 wave | 139.6 | 7,667 | 97.1 | 69.6% |
| PUF filers | 150.8 | 7,145 | 125.0 | 82.9% |
| PUF + crude non-filers | 182.0 | 7,418 | 134.8 | 74.1% |
| IRS 2015 | 150.5 | 7,112 | 124.6 | 82.8% |
| SCF split-off units (not in the table) | 23.4 | 348 | | |

SCF wage holders as a share of the PUF side's, by amount:

| Wages | SCF units (millions) | PUF + non-filers (millions) | SCF as % of PUF filers | SCF as % of PUF + non-filers |
|---|---:|---:|---:|---:|
| Under $5,000 | 3.75 | 11.53 | 36% | 32% |
| $5,000 to $20,000 | 15.96 | 32.36 | 55% | 49% |
| $20,000 to $50,000 | 32.01 | 44.68 | 79% | 72% |
| $50,000 to $100,000 | 25.92 | 28.26 | 95% | 92% |
| $100,000 to $200,000 | 13.75 | 13.89 | 100% | 99% |
| $200,000 to $500,000 | 4.64 | 3.50 | 133% | 132% |
| $500,000 to $1 million | 0.62 | 0.45 | 138% | 138% |
| Over $1 million | 0.47 | 0.15 | 306% | 306% |

Percentiles among units with wages:

| | p10 | p25 | p50 | p75 | p90 | p99 | p99.9 |
|---|---:|---:|---:|---:|---:|---:|---:|
| SCF | $10,137 | $23,500 | $45,000 | $85,000 | $150,000 | $550,000 | $2,420,000 |
| PUF filers | $5,950 | $15,600 | $34,600 | $69,000 | $120,000 | $348,300 | $1,107,000 |
| PUF + non-filers | $5,830 | $15,100 | $33,200 | $66,100 | $116,200 | $334,300 | $1,064,000 |
| SCF / PUF filers | 1.70 | 1.51 | 1.30 | 1.23 | 1.25 | 1.58 | 2.19 |

## Capital gains

Totals. "Gains" and "losses" are the sums of the positive and of the
negative amounts.

| | Net ($ billion) | Gains ($ billion) | Losses ($ billion) | Positive | Zero | Negative |
|---|---:|---:|---:|---:|---:|---:|
| SCF 2016 wave | 678.3 | 700.1 | −21.8 | 6.6% | 91.6% | 1.8% |
| PUF filers, E01000 (the table) | 614.7 | 633.6 | −18.9 | 7.8% | 86.7% | 5.6% |
| PUF filers, full line 13 | 626.5 | 645.3 | −18.9 | 10.6% | 83.8% | 5.6% |
| PUF + non-filers, full line 13 | 626.5 | 645.3 | −18.9 | 8.8% | 86.6% | 4.6% |
| PUF filers, before the loss limit | 606.2 | 686.7 | −80.6 | 11.7% | 85.1% | 3.1% |
| IRS 2015, line 13 | 706.5 | 725.2 | −18.6 | 10.6% | 83.9% | 5.5% |

The PUF falls about $80 billion short of the IRS net total because the
four aggregate records, which hold the very top, are left out
(`01_REPORT.md`: with them, line 13 is $701 billion). This understates the
PUF's top holders in every table below.

SCF holders as a share of the PUF side's, by amount (PUF side = filers +
non-filers):

| Amount | Gains vs full line 13 | Gains vs E01000 | Losses vs line 13 | Losses vs before the limit |
|---|---:|---:|---:|---:|
| Under $100 | 10% | 19% | 17% | 18% |
| $100 to $500 | 34% | 67% | 31% | 32% |
| $500 to $2,000 | 46% | 76% | 30% | 36% |
| $2,000 to $10,000 | 56% | 72% | see below | see below |
| $10,000 to $50,000 | 79% | 85% | | 63% |
| Over $50,000 | 131% | 133% | | 14% |

For losses between $2,000 and $10,000, the PUF's line 13 holds 5.3
million returns at exactly −$3,000, against 0.93 million SCF units at
exactly −$3,000 and 0.32 million SCF units elsewhere in that range. SCF
units with losses number 2.5 million, against 8.4 million PUF returns.

Percentiles among holders of gains:

| | p10 | p25 | p50 | p75 | p90 | p99 | p99.9 |
|---|---:|---:|---:|---:|---:|---:|---:|
| SCF | $500 | $1,500 | $6,200 | $33,000 | $104,000 | $1,000,000 | $8,840,000 |
| PUF filers, full line 13 | $120 | $590 | $2,710 | $11,400 | $39,600 | $470,200 | $4,671,000 |
| PUF filers, E01000 | $220 | $1,090 | $4,730 | $17,100 | $56,100 | $661,200 | $6,029,000 |
| SCF / PUF full line 13 | 4.17 | 2.54 | 2.29 | 2.89 | 2.63 | 2.13 | 1.89 |
| SCF / PUF E01000 | 2.27 | 1.38 | 1.31 | 1.93 | 1.85 | 1.51 | 1.47 |

Percentiles of the size of losses: the SCF's median is $3,000, its 90th
percentile $20,000 and its 99th $85,000. The PUF before the limit has
$2,080, $23,670 and $223,980. On line 13 the PUF cannot go beyond $3,000.

## What the tables show

**Wages.** The SCF does not look like a correct report of the PUF's
concept. The differences run in both directions:

- **Total:** the SCF's total is $7,667 billion. That is 7% above PUF
  filers ($7,145 billion) and 3% above PUF filers plus non-filers ($7,418
  billion), even though $348 billion of split-off wages is left out.
- **Low wages:** the SCF has far fewer units with low wages. It finds 32%
  of the PUF side's units under $5,000 and 72% at $20,000 to $50,000. Its
  share of all units with any wages is 69.6%, against 74.1%. Much of this
  may be the unit definition and not misreporting: the SCF combines
  dependents' wages with the family's, and it drops 23.4 million split-off
  units. These tables cannot separate the two.
- **High wages:** the SCF has many more units with high wages, and the
  unit definition does not explain this. Above $200,000 it has 5.7 million
  units and $2.73 trillion of wages, against 4.1 million returns and $1.64
  trillion on the PUF. Above $1 million it has 3.1 times the PUF's count
  (0.47 against 0.15 million) and $1.02 trillion against $0.37 trillion.
  The PUF's missing aggregate records hold only about $11 billion of
  wages, so they do not explain the gap.
- **Percentiles:** the SCF's percentiles are 1.2 to 1.3 times the PUF's
  between the median and the 90th percentile. The ratio is 1.6 at the
  99th percentile and 2.2 at the 99.9th.
- **Not established:** the cause of the top gap. Candidates are the
  definition (deferrals, which are too small to explain a factor of three
  above $1 million), the SCF's wealthy oversample and its weights, or
  respondents reporting business income as wages.

**Capital gains.** The pattern is a mix of three things.

1. **Small holders reporting zero.** This matches interest and dividends.
   Against the full line 13, the SCF finds 10% of the PUF's holders of
   gains under $100, 34% at $100 to $500, 56% at $2,000 to $10,000 and 79%
   at $10,000 to $50,000. Above $50,000 it finds 131%. The share of units
   with a gain is 6.6%, against 10.6% for PUF filers and 8.8% with
   non-filers.
2. **More holders than the PUF at the top, not dollars scaled up.** Above
   $50,000 the SCF has 1.74 million holders, against 1.33 million on the
   PUF. Their average gain is lower than the PUF's: $363,000 against
   $415,000. So the larger SCF percentiles (2 to 3 times the PUF's at every
   point) come from which holders are present, not from each holder
   reporting more. The PUF's missing aggregate records account for part of
   the top gap. The SCF's net total ($678 billion) is 96% of the IRS's
   ($706 billion).
3. **Definition differences:**
   - **Losses:** the SCF reports losses on a mixed basis. About 40% of
     SCF loss reporters give exactly −$3,000, the limited line-13 figure,
     and about 30% give losses beyond the limit. Loss reporters are 29%
     of the PUF's count, and that share is low at every size.
   - **Distributions without a Schedule D:** the table's PUF variable
     leaves these out. Comparing the SCF with E01000 instead of the full
     line 13 makes the SCF look closer at small amounts (67% against 34%
     at $100 to $500) because the PUF side is missing 4.4 million small
     holders.
   - **Line 14** is too small to matter ($1.3 billion).

## Open points for John

- The table's `capital_gains` on the PUF side (E01000) omits $11.8 billion
  of capital gain distributions on 4.4 million returns. Whether to add
  E01100 to it, which would change `other_agi` and the AGI identity, is a
  definition change and John's decision. Nothing has been changed.
- The SCF's high-wage excess has no explanation yet.

## Revision history

- 2026-10-17: written.
