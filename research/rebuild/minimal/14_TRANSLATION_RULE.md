---
title: "The translation rule: how a survey's shared variables are put on the PUF's scale"
role: plan
workstream: rebuild
status: current
updated: 2026-10-15
sot: research/rebuild/design/plan.md
supersedes: []
superseded_by: null
---

# The translation rule

How a variable that both the PUF and a survey have is prepared before the
model sees it. Agreed in conversation with John on 15 October after the
experiments in `09_RESULTS.md`, `10_RESULTS_A.md`, `10_RESULTS_YEARS.md`
and `13_RESULTS_B.md` and the theory in `11_SHARED_X_THEORY.md` and
`12_CROSSWALK_THEORY.md`. Confirmed by John on 15 October and written
into `design/plan.md` as decision 15.

## The rule in one sentence

Get the survey's dollars onto the PUF's terms first, then place every
record on the PUF's dollar scale for that year.

## What "the PUF's dollar scale" means

The PUF's distribution of the variable, for the year, is cut into 84
slices at the PUF's percentiles: one slice for exact zero, ten for losses,
then slices that are coarse in the middle and fine at the top (45 of 2%,
9 of 1%, 9 of 0.1%, 10 of 0.01%). Each slice is a dollar range. A record
from any source goes into the slice whose dollar range holds its amount.
So slice 50 of interest means $540 to $640 whether the record is from the
PUF or the SCF.

The survey's dollars are not changed by this step. Only the slice
numbers are. Under the old rule each survey was cut at its own
percentiles, so the same slice number meant different dollars in
different files; that is what doubled wealth.

The model also sees which source a record came from. It uses that to
learn, for example, that the SCF's zero slice is bigger than the PUF's.

## What "onto the PUF's terms" means, case by case

Before placing a survey's variable on the PUF's scale, look at how the
survey measures it compared with the PUF. The detection table
(`01_detection_table.py`: share of units by dollar bracket in each file,
mean amount within each bracket, share above each bracket edge, and the
totals against the IRS) shows which case applies.

| Mismatch | How it shows in the table | What to do before mapping | Example |
|---|---|---|---|
| Survey leaves out small holders | Totals match; survey has fewer units at low amounts, as many or more at high amounts; amounts within a bracket match | Nothing. The model handles the survey's bigger zero slice through the source label. | SCF interest, dividends, capital gains |
| Survey rounds amounts | Same as above, with spikes at round numbers | Nothing. Rounding is omission below half the rounding unit. | SCF amounts in round thousands |
| Survey's dollars are systematically low or high | Survey is short (or long) of units at every amount, not only the bottom; totals off by about the same factor | Scale the dollars first. The factor comes from comparing the two files in the range where both report fully, not from the ratio of totals. | CPS dividends at 0.41 of the IRS total |
| Survey's variable means something different | Totals off; the by-bracket pattern is not a uniform shift | Fix the definition in the table builder if the components exist. If they do not, the variable is not shared: make it survey-owned, like net worth. | CPS interest includes IRA interest, 2.3 times the IRS |
| Survey top-codes | Pile-up at the cap, nothing above; totals short at the top | Everything above the cap is absent on the survey side (like net worth on the PUF). The PUF owns the top. | CPS wages, interest, dividends |
| Survey did not ask the question | Known from documentation | Absent, never zero. | CPS capital gains before 2018 |
| Survey includes people the PUF does not | Extra zeros on every variable at once | Not a variable problem. Apply a filing rule to the survey and drop or flag the units that would not file. No mapping rule touches this (`13_RESULTS_B.md`). | SCF 30% zero wages vs PUF 17% |
| Different year | Known | Use the PUF's scale for that year. With one PUF, grow the 2015 cut points by the IRS growth in the item (`10_RESULTS_YEARS.md`); IRS by-bracket tables would do it better. | SCF 2018 and 2021 waves |

Several can apply at once. Handle population first, then definition,
then dollars, then map.

**The population check** (John, 2026-10-16). After the survey's tax units
are built the IRS's way and nonfilers are handled, the survey's weighted
count of units should equal the IRS return count for the same year. A gap
that a difference in years does not explain most likely means the
tax-unit construction or the filing rule is not right yet. Now: 143
million SCF units against 158 million returns for 2019, most likely
dependents who file their own returns but sit inside their parents' unit
in the SCF (not checked).

## What the rule assumes, and what it cannot do

It assumes that a dollar the survey reports is the dollar the return
would show, once documented errors are undone, and that whether a
household reports an amount depends on the amount but not on its wealth
given the amount. It assumes nothing about who is in the survey or the
shape of its distribution. (`12_CROSSWALK_THEORY.md`, section 2.)

It cannot fix a survey zero that hides a real amount: the model can only
learn the average over such households. It cannot check the dollars of a
variable that has no PUF version and no outside total, such as wealth
borrowed from two surveys; there, ranking within each survey is the
fallback.

## Why not ranks

Ranking each survey on its own scale, including zeros, was tried
(`13_RESULTS_B.md`). It assumes the survey and the PUF are the same
population. They are not: the SCF has more units with no wages and more
large holders of investment income. The rank map sent PUF earners of $700
to $10,900 onto SCF units with no wages, and their wealth came out at
3.7 times the SCF's median for that group.

## Where it is applied

`01_variants.py` (table E4) is the current implementation for the SCF's
four shared variables; `10_years_tables.py` (table Y1) for the later
waves. The sheet of per-variable decisions, one line per survey wave and
variable, is to be kept in `01_REPORT.md` as surveys are added.

## Revision history

- 2026-10-15: written after the option B run; confirmed and added to the plan.
