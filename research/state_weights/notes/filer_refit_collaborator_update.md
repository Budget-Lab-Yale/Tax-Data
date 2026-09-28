---
title: "Tax-Data: state weights, filer weights and IRS data — update, 24–27 September 2026"
role: notes
workstream: state_weights
status: frozen
updated: 2026-09-27
true_as_of: "Tax-Data phase3b-filer-refit at 699515e; IRS-Ind main at 76e5142"
sot: research/state_weights/plan.md
supersedes: []
superseded_by: null
---

# Tax-Data: state weights, filer weights and IRS data

*Update on the work of 24–27 September 2026, for collaborators. Detailed records:
`research/state_weights/plan.md` (§5a, Phases 3 and 3b); decisions S39–S40 in
`research/decisions_log.md`; `research/state_weights/notes/filer_refit_design.md`;
`research/state_weights/notes/low_income_filer_aging.md`.*

**In one paragraph.** We picked a tentative method for state weights across 2017–2023. A
downstream user then found the aged file keeps far too many low-income filers in 2023.
Tracing it exposed a deeper flaw: the 2017 base weights were all-or-nothing. We built a
checked reader for the IRS's national tables and rebuilt filer weights in every year from
2017 to 2023 by calibrating them to IRS return counts. The first rebuild matches the IRS by
income class and fixes the low-income gap; a second rebuild, which also keeps 2017 income
amounts on their IRS totals, is running now.

## 1. State weights, Phase 3: choosing how to fit each year

Tax-Data splits each tax return's national weight across states, so state-level results add
up to published IRS state totals (HT2, the IRS's state-by-income-class tables). Phase 3
extended this from one year to every year 2017–2023, and compared three ways of doing it:

- **A:** fit each year independently.
- **B:** start each year from the previous year's fit.
- **C:** fit all years jointly, with a penalty on a return's state split changing from year
  to year.

| Arm (2018–2023 averages) | Targets within 2% | Error on untargeted HT2 series | Year-over-year churn in state shares |
|---|---|---|---|
| A — independent | 95.6% | 19.02% | 5.6% |
| B — warm-started chain | 96.0% | 18.69% | 5.3% |
| C — joint, warm-started from B | 95.3% | **18.63%** | **2.2%** |

C warm-started from B is best on everything untargeted, and gives up 0.7 points on the
targeted fit. **It is the tentative choice (decision S39)**, to be re-run once the national
filer weights below are rebuilt, since the state split sits on top of them.

## 2. A problem found downstream: too many low-income filers

A downstream project (Affordability-Index) found that Tax-Data's aged file does not
reproduce the fall in low-income filing:

| Returns with wages, AGI $1–10k | TY2019 | TY2023 |
|---|---|---|
| IRS | 14.2M | 9.3M (−34%) |
| Tax-Data | 13.6M | 12.1M (−11%, about 30% high) |

We traced it to how the file ages. For 2018 onward, each 2017 record's weight is scaled by
one factor per filing status × age group. Those totals are right by construction, but the
income mix inside each group stays frozen at 2017's. Most of the TY2023 excess (about 2.9M of
3.0M) is dependent filers, such as teenagers with small jobs.

## 3. A problem in the 2017 base weights

Checking the fix turned up a pre-existing flaw. The linear program that built the 2017
weights minimized the *sum* of the weight multipliers instead of how far each moved from 1.
So **every one of the 207,692 records sat at a bound: 62% multiplied by 0.34, 38% by 1.66**,
and none left unchanged.

The totals it targeted were hit, in $10k AGI bands. Inside each band the weights alternated
sharply. For example, TY2017 returns at AGI $1–5k were 38% too high and at $5–10k 33% too
low, against IRS Table 1.4. Every later year inherited this.

## 4. IRS data: a clean, checked reader for the national tables

To fit to finer IRS detail, we built a reader in the IRS-Ind repo for the 15 national "by
size of AGI" tables of IRS Publication 1304, 2011–2023 (one table back to 1996). It
produces:

- **One long panel per table**, with every published value flagged. The IRS's disclosure
  marks ("use with caution", "combined to prevent disclosure") are hidden in Excel number
  formats, and standard readers turn a combined cell into a false zero.
- **Built-in checks:** size classes must add to their totals, and return counts must agree
  across tables. All 20,000+ comparisons pass or are explained.
- **A harmonized version** that keeps each series consistent across years when the IRS
  merges, splits or relabels a line.
- **A version of Table 1.6 (returns by age × filing status × AGI) ready for reweighting.**
  Cells the IRS combined for disclosure are grouped into blocks whose totals are known, and
  every row and column total balances.

## 5. The fix: rebuilding filer weights for every year, 2017–2023

Filer weights are now **calibrated each year to IRS targets** (decision S40):

- returns by filing status × age × AGI class (Table 1.6);
- returns with wages by AGI class (Table 1.4);
- dependent returns, all and with wages, by AGI class (Table 1.7).

How it works:

- **AGI:** records are classed with a reconstruction of AGI from each record's components.
  It matches Tax-Simulator's computed AGI for 99% of returns.
- **Method:** we compared the old linear program with its objective fixed against entropy
  calibration, which moves weights as little as possible from a starting point. Both hit the
  targets. **Calibration was clearly better on weight quality**: a higher effective sample
  size, a third of the linear program's lumpiness, and almost no records at a bound, against
  6–11% for the linear program. It was also slightly better on untargeted IRS series.
- **The 2017 base** is refit from its weights before the old linear program ran.
- **Later years:** each is fit starting from its prior (the refit 2017 weights × the status
  × age factors), iterating with the income factors that depend on the weights. Years after
  2023 grow from the refit 2023 weights.

Results in a full rebuild (vintage `2026092712_filer_refit`, compared with the previous
vintage):

| | Before | After |
|---|---|---|
| Returns by AGI class vs Table 1.4, average error | 12–13% | under 0.5% |
| Returns with wages by AGI class, average error | 15–17% | about 0 |
| TY2023 wage filers at AGI $1–10k (IRS 9.56M) | 13.29M | 9.56M |
| TY2023 dependent wage filers at AGI $1–10k (IRS 3.75M) | 6.69M | 3.75M |
| Total filers vs IRS | 0.5% short | exact |

A column-by-column check confirmed that only weight-dependent values changed.

## 6. In progress

That rebuild drifted the 2017 income amounts off their IRS totals: business income +23%,
interest +7%. The cause is that the step scaling 2017 amounts to IRS totals ran on the old
weights. The fix, now in the code, calibrates the 2017 weights *before* that step. A test
confirmed 2017 amounts then match IRS totals exactly. **A second rebuild with this fix is
running now**, followed by the same diagnostics.

**After that:**

1. Re-run the state-weight comparison (section 1) on the new national weights.
2. Run Tax-Simulator on the new vintage.
3. Pick up the smaller open items:
   - the file has no filer under 18, although the IRS counts about 2M such returns a year;
   - some research scripts still need to switch to the shared pipeline code.

**What collaborators should expect.** Once this ships, filer weights change in every year
from 2017 on, so Tax-Simulator results shift, most noticeably at low incomes and for
dependents.
