---
title: "Proposal: rebuild the top of the 2017 base from the PUF's aggregate records and Table 4.3"
role: notes
workstream: state_weights
status: open
updated: 2026-09-29
sot: research/state_weights/plan.md
supersedes: []
superseded_by: null
---

# Proposal: rebuild the top of the 2017 base from the PUF's aggregate records and Table 4.3

For John Ricco. This is dimension 1 ("base construction") of the revamp scope
in `research/state_weights/notes/top_tail_backcast.md` §3, written up as a
design with options and open questions. It proposes **synthetic extreme
returns**: records assembled from evidence, one per (band × income pattern),
that put back what the PUF's four aggregate records removed — under S41,
which keeps the aggregate records themselves out because an averaged
composite is not a tax return. Nothing here is built.

## 1. The problem, in the base year

The TY2015 PUF replaces about 1,100 extreme returns (1,233 weighted) with
four averaged records, one per AGI band; `process_puf.R` drops them. The 2017
rescaling in `create_2017_puf.R` puts the dollars back by scaling every
$10M+ record, so class totals match Table 1.4 while the shape inside the
class is lost. Measured against IRS Table 4.3 for **TY2017, the base year**
(`research/state_weights/filer_refit/08_table43.R`):

| Top 0.001% of returns excluding dependents, TY2017 | IRS | File | File ÷ IRS |
|---|---|---|---|
| Returns | 1,433 | 1,433 | 1.00 |
| AGI floor | $63.4M | $66.1M | 1.04 |
| AGI | $256B (2.34% of all AGI) | $147B (1.35%) | **0.57** |
| Net capital gain | $134B | $98B | 0.73 |
| Partnership + S-corp net income | **$59B** | **$15B** | **0.26** |
| Ordinary dividends | $21.0B | $9.8B | 0.47 |
| Taxable interest | $9.9B | $5.2B | 0.52 |
| Wages | $22.3B | $15.4B | 0.69 |

Our floor is above the IRS's while our total is 43% short: the top ~1,400
returns are too uniform, with too little at the extreme. The top 0.01% is at
0.90 of IRS AGI, the top 0.1% and 1% within 2–5%. The gap is a **support**
problem — the file has no return above ~$136M of AGI, while the dropped
$100M+ aggregate averages $292M over 465 returns — so no reweighting can
close it (weights move mass among records that exist), and no uniform
factor can either. It persists in every later year because the aging carries
2017's shape forward (`top_tail_backcast.md` §1.2: 0.57–0.76 of IRS AGI,
2017–2023).

What sits in the hole is not mainly billionaires' income. Partnership and
S-corporation net income is $44B of the $110B gap in 2017, and a third to
two-thirds of the gap in every year; the Forbes splice cannot fill it (BSYZ
give billionaires negative business income). The missing returns look like
business sales and large passthrough distributions.

## 2. What the evidence supports

Three sources describe the missing returns, none of them a return.

**(a) The four aggregate records themselves** (`RECID` 999996–999999), each
the per-return mean of every dollar field over its band:

| Band | Weighted returns | Mean AGI | Mean gains (E01000) | Mean dividends (E00600) | Mean partnership/S-corp (E26270) | Mean Schedule E (E02000) | Mean wages (E00200) |
|---|---|---|---|---|---|---|---|
| AGI $100M+ (999999) | 465 | $292M | $166M | $39M | $29M | $26M | $21M |
| AGI $10–100M (999998) | 394 | $44M | $15M | $4.3M | $7.5M | $12M | $2.9M |
| AGI $0–10M (999997) | 233 | $4.0M | $4.3M | $0.5M | −$3.1M | −$2.5M | $0.6M |
| AGI < 0 (999996) | 140 | −$96M | $18M | $3.7M | −$31M | −$32M | $0.7M |

**(b) The booklet's tabulation** ("Weighted Counts and Sum of the Amounts for
Returns Used to Populate Aggregate Records", 2015 PUF booklet pp. 31–32):
for every field, over all 1,232 aggregated returns, the weighted count and
sum **split into positive and negative parts**. E.g. E00900 (Schedule C):
444 returns, 237 positive summing to $1.88B, 207 negative summing to
−$0.95B; E02000 (Schedule E): 1,152 returns, 679 positive $30.4B, 473
negative −$18.2B; E01000: 1,179 returns, $86.7B. This is a marginal
distribution per field with its sign structure, not per band.

**(c) IRS Table 4.3, TY2017** (and every year 2014–2023): the top 0.001%,
0.01%, 0.1%, 1% with count, AGI floor, AGI, and wages, interest, dividends,
qualified dividends, net capital gain, Schedule C income and loss,
partnership/S-corp income and loss. Also Table 1.4's $10M+ class by line.

Together: (a) gives per-band means, (b) gives per-field sign splits and
counts, (c) gives the target the finished base must hit at the percentiles.
What none of them gives is the **joint** distribution across lines within a
return — which returns have gains *and* passthrough income, how the $100M+
band's $292M mean splits between a few $1B returns and many $150M ones.

## 3. The design

**Synthetic extreme returns**, built once at the 2015 base and carried through
the pipeline like any other record.

### 3.1 Records

For each of the four bands, N_b synthetic records whose weights sum to the
band's weighted count (465 / 394 / 233 / 140). N_b is a design knob; 40–120
per band (roughly weights of 2–10 each) gives shape without pretending to
more resolution than the evidence has. Ids in a reserved block, as the
Forbes splice does (S25: Forbes at 9e6+; a block just below it is free).

### 3.2 Income patterns from donors

Each synthetic record starts from a **donor**: a real PUF record from the top
of the file (say the top 2,000 by AGI, or by the band's dominant line), so
its joint structure — which lines are nonzero together, filing status, age,
dependents, itemization pattern — is a real return's. The donor's dollar
lines are then scaled to the band. Two ways to pick and scale donors:

- **D1, cluster-and-scale.** Cluster the top records by income *pattern*
  (shares of AGI in gains / passthrough / dividends+interest / wages /
  Schedule E / losses), pick donors across clusters in proportion to what
  (b)'s sign counts imply (e.g. 473 of 1,232 returns have negative Schedule
  E), and scale each donor's lines by one scalar so its AGI lands on a draw
  from a within-band AGI distribution (§3.3).
- **D2, per-line assembly** (the Forbes v2 "assemble, don't copy" pattern):
  set each line explicitly from (a) and (b), using the top records only for
  within-category shares. More control, less realism in the joint structure.

D1 is recommended: the joint structure is the thing we cannot observe and
donors are the only evidence of it.

### 3.3 The within-band AGI distribution

The one shape assumption. Fit a Pareto tail to the file's own $10M+ records
plus the band means from (a) — the $100M+ band's mean of $292M over 465
returns with a floor at $100M pins a Pareto index of about 1.5 — and draw
the N_b AGIs from it (deterministically: quantiles, not random draws, so
the record set is reproducible and id-keyed draws stay untouched). Table
4.3's TY2017 floor ($63M) and top-0.001% AGI ($256B) check the result. For
the negative-AGI band, the same on the loss side (mean −$96M, 140 returns).

### 3.4 Calibration to the evidence

Given records and weights, an income-neutral-by-construction fit, in this
order of priority:

1. **Band totals** — each band's AGI and each field's sum equal the
   aggregate record × its count (exact: this is (a), and it is what the
   rescaling currently smears). A per-(band, line) scalar closes this.
2. **Sign splits** — per field, the positive and negative counts and sums
   match (b), across bands. Achievable through donor selection (D1), checked
   not solved.
3. **Table 4.3 TY2017** — after the synthetic records enter and the 2017
   rescaling runs on the augmented file, the top 0.001% / 0.01% / 0.1% AGI
   and lines against IRS. This is the acceptance test, not a target: the
   records are 2015, the table is 2017, and the rescaling sits between.

### 3.5 Demographics and everything the imputations need

From the donor: filing status, ages, dependents, itemizer status. The
aggregate records carry none of this, which is part of why S41 rejected
using them directly. Every imputation then sees a plausible record. Wealth
(2022 base) imputes on income and demographics and will place them at the
top; the Forbes splice's source pool (top records by income or wealth) will
include them, so from 2022 they are among the records that give up weight to
the billionaires — correct, since some of them *are* billionaires' returns.

### 3.6 Where it enters the pipeline

In `process_puf.R`, replacing the `filter(RECID < 999996)`: read the four
aggregates, build the synthetic records, drop the aggregates, append. They
then pass through age imputation (their donors' ages), stage 0 weights,
S40 stage 1, and the rescaling — whose $10M+ factors should fall from about
1.5–1.75 to near 1, a built-in check. The extensive factors, S40's classes
and the Forbes splice all pick them up without code changes.

## 4. What it should deliver, and how we would know

| Test | Now (TY2017) | Target |
|---|---|---|
| Top 0.001% AGI ÷ IRS (Table 4.3) | 0.57 | ≥ 0.95 |
| Top 0.001% passthrough income ÷ IRS | 0.26 | ≥ 0.8 (the line least constrained by (a); see §6) |
| Top 0.001% AGI floor ÷ IRS | 1.04 | 0.95–1.05 |
| $10M+ rescaling factors, interest / gains | ~1.75 / ~1.5 | 0.95–1.05 |
| Table 1.4 $10M+ class, every line | matched by rescaling | matched with factors near 1 |
| Top 0.01%, 0.1%, 1% AGI ÷ IRS | 0.90 / 0.98 / 0.99 | unchanged or better |
| Share of $10M+ AGI above $100M (07_top_tail_2022.R, 2015 PUF with aggregates: 29%) | 6% at the 2015 base | ~29% |
| 2018–2023 Table 4.3, with dimension 2's bucket aging | 0.67–0.76 | ≥ 0.9 |

Regression: S40 class diagnostics, S21 partition, E2 tripwire (the synthetic
ids are new records; filer draws keyed by id must not move), Forbes splice
constraints still certified.

## 5. Relation to the other dimensions

- **Dimension 2, aging discipline.** Prototype B (percentile-specific value
  factors from Table 4.3, `09_bucket_aging_prototype.R`) shows how much of
  the 2018–2023 gap is shape drift the aging can fix and how much is the
  2017 base gap carried forward — the part this proposal addresses. §7 has
  the numbers. The two are complements: C sets the level in 2017, B keeps
  the shape right after.
- **Dimension 3, Forbes.** Unchanged. The billionaires' realized income is a
  different population from the extreme returns; both are needed. Watch the
  overlap from 2022: the splice's income-neutral calibration will take weight
  from the synthetic extremes as from any top record.
- **S41.** Consistent: the aggregate records are inputs to a construction,
  not records.

## 6. Open questions for John

1. **Is the joint structure worth the donor machinery (D1), or is per-line
   assembly (D2) enough** for what Tax-Simulator does with these records
   (rates on gains, NIIT, passthrough deduction, AMT)?
2. **The passthrough line.** Table 4.3 says the TY2017 top 0.001% reported
   $59B of partnership/S-corp net income; the aggregate records' E26270
   means imply only about $13B + $3B across the two top bands (with large
   negatives in the two lower bands). Either the 2015 extremes were
   gains-heavy and 2017's were passthrough-heavy, or Schedule E's E02000
   (which includes rents, royalties, estates as well as passthroughs) is
   where it sits. Should the acceptance test on the passthrough line be
   Table 4.3 2017 (level) or (a)'s 2015 means (composition), when they
   disagree?
3. **N_b and the Pareto index** — accept the recommendation (40–120 per
   band, index fit to the file's tail with the band mean as a constraint), or
   pin them from an outside source (e.g. the DINA top-tail files)?
4. **A later PUF base.** If a TY2017+ PUF is in reach, its aggregate records
   would be closer to the target years and the 2015→2017 rescaling would go.
   Does that change whether this is worth building on 2015?
5. **Sequencing.** Before or after the from-scratch rebuild that resolves the
   unweighted-cache and RNG items (dimension 5)? Building it before means one
   more confounded rebuild; after means the top tail waits.

## 7. Prototype B: how much of the later-year gap is aging, how much is base

`research/state_weights/filer_refit/09_bucket_aging_prototype.R` applies
percentile-specific value factors to the v2 files off-pipeline: four bands
(top 0.001%, 0.001–0.01%, 0.01–0.1%, 0.1–1% by AGI proxy), eight Table 4.3
lines, and for each (band, line) one scalar chosen so the band's total equals
**the file's own 2017 band total carried forward by the IRS band's growth**.
The factors therefore carry only the shape drift since 2017; what remains
against IRS levels is the 2017 base gap carried forward. Two membership
treatments — re-ranked each pass (Slurm 27756841; oscillates at the extreme
band, unsettled after 12 rounds) and frozen on the year's unadjusted AGI, the
`record_bucket.R` convention (Slurm 27757147; one pass) — agree within $5B.
Frozen-mode results, top 0.001% AGI, $B:

| | 2018 | 2019 | 2020 | 2021 | 2022 | 2023 |
|---|---|---|---|---|---|---|
| IRS | 242 | 214 | 298 | 443 | 348 | 317 |
| v2 file | 162 | 155 | 195 | 319 | 252 | 239 |
| v2 + bucket aging | 148 | 134 | 187 | 282 | 211 | 199 |
| Observed gap (IRS − v2) | 80 | 59 | 103 | 124 | 96 | 78 |
| **Base gap carried forward (IRS − aged)** | **94** | **80** | **111** | **161** | **137** | **118** |
| Share of the observed gap closed by B | −17% | −36% | −8% | −29% | −43% | −52% |

**Percentile-specific aging does not close the gap; it widens it.** The
file's top slice, aged by uniform national factors, has grown *faster* since
2017 than the IRS's actual top slice — so a faithful percentile-specific aging
lowers it, and the base gap carried forward is larger than the gap we see:
$80–161B a year, 33–41% of the IRS figure. The level problem at the top is
entirely a 2017 base problem, and it is somewhat masked in later years by the
uniform aging over-growing the top. Three details:

- **The shape drift B captures is real, just not a level fix.** Top-band
  growth-relative factors: wages 2.11 in 2021 and 0.80–0.86 in 2019/2022–23;
  dividends 1.47 in 2021, 0.56–0.59 in 2022–23; interest 0.56–0.78 in
  2019–2022; gains 0.79–0.94 every year. These are the timing and composition
  differences a per-percentile ledger would carry once the base is right.
- **Below the top band B is neutral to slightly negative**: the top 0.01%
  moves from 0.95–1.03 of IRS AGI to 0.92–0.96, the top 0.1% and 1% by
  under 3 points, because dollars taken off the top band re-rank records
  downward. With a correct base this reverses.
- **The level factors the top band would still need after B** size the base
  hole by line: partnership/S-corp income 2.6–6.7×, Schedule C income
  3.2–5.9×, dividends 1.6–2.1×, interest 1.4–2.1×, gains 1.3–1.5×, wages
  1.2–1.7×. Those are not factors any aging should apply; they are the
  returns this proposal adds.

Design lesson for dimension 2: freeze bucket membership at a reference year
(as wealth does), or damp the re-ranking; a per-band factor with live
re-ranking at the 0.001% margin does not converge.

Outputs: `research/state_weights/filer_refit/results/bucket_aging/{frozen,rerank}/`
— `decomposition.csv`, `comparison.csv` (every slice × line, before/after),
`factors.csv`, `level_factors.csv`, `bands.csv`.

## 8. Effort

Roughly: donor clustering and the Pareto fit, two days; the build in
`process_puf.R` with its gates, two days; a full rebuild and the test
battery, one day of wall-clock plus review. The tests exist
(`06`–`09` in `research/state_weights/filer_refit/`).

## Revision history

- **2026-09-29** — Written (JI's session), from the Table 4.3 seven-year
  comparison and the aging walkthrough.
