---
title: "n_dep_eitc is zero for every married-filing-separately filer: the PUF EIC count is a claimed count, not an eligibility count"
role: notes
workstream: state_weights
status: open
updated: 2026-10-08
sot: research/state_weights/plan.md
supersedes: []
superseded_by: null
---

# Married-filing-separately filers have no EITC-qualifying children

*Raised 2026-10-08 by the Tax-Simulator state-tax cross-model work (JI). Vintage read:
`2026092720_filer_refit_v2`. Code references are to `phase3b-filer-refit` at this note's commit.*

## 1. What we see

`n_dep_eitc` ("number of EITC-qualifying children under 2015 rules",
`config/variable_guide/baseline.csv`) is taken from the PUF `EIC` field, which counts the
qualifying children on a return that **claimed** the credit. Married-filing-separately filers
could not claim the EITC before TY2021 (IRC 32(d)), so the field is zero on every one of them,
and the projection carries that zero into every year.

In the Tax-Simulator cross-model sample for TY2023 (filers only, `2026092720_filer_refit_v2`),
985 of 4,305 separate filers have dependents (`n_dep > 0`), and all 4,305 have `n_dep_eitc = 0`.

## 2. Why it matters now

- **Federal.** The American Rescue Plan Act added IRC 32(d)(2) for TY2021 onward: a separated
  spouse who files separately, has a qualifying child living with them for over half the year,
  and lived apart from the spouse for the last six months (or is legally separated) can claim
  the EITC. Tax-Simulator encodes this (`eitc.mfs_eligible` = 1 from 2021, commit `22c259749`),
  but the rule is inert: no separate filer has a qualifying child to satisfy it.
- **State.** Tax-Simulator's state calculators now carry the separate-filer rules of eleven
  states (California, Minnesota, New Jersey, Missouri, Washington, Maryland, Nebraska,
  Massachusetts, Montana, Virginia, Kentucky), most keyed on `n_dep_eitc`. Every one of them is
  also inert, and the cross-model comparison against TAXSIM and PolicyEngine cannot test them.
- **Direction.** The federal and state credits for separate filers are understated by the full
  credit for every such family. The affected population is small (separate filers with children
  who live apart from the spouse) but the per-unit amounts are the largest in the schedule.

## 3. What a fix would look like

`n_dep_eitc` for a separate filer should be derived the way it is for the constructed non-filers
(`research/state_weights/nonfiler_pool/05_emit_pool.R` line 169: dependents under the EITC age
limit, students under 24, capped at three), from the return's own dependent ages, instead of
copied from the claimed count. The living-apart condition is unobserved in the PUF and would be
assumed met, as Tax-Simulator already assumes for its state rules; a take-up or living-apart
share could be applied downstream if the result over-pays.

The claimed count is still the right source for joint, single and head-of-household returns,
where eligibility and claiming coincide apart from take-up.

## 4. Status

Open. Needs John Ricco's call on whether to derive the count in `create_2017_puf.R` /
`project_puf.R` or leave it for a Tax-Simulator-side imputation.
