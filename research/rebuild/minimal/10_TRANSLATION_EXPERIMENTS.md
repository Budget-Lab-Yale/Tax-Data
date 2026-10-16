---
title: "Minimal example: translation experiments (distorted third survey, and years)"
role: plan
workstream: rebuild
status: current
updated: 2026-10-15
sot: research/rebuild/design/plan.md
supersedes: []
superseded_by: null
---

# Translation experiments

Written 15 October at about 3 a.m., after the bridge grid (`09_RESULTS.md`)
showed that banding shared variables on the PUF's dollar cut points (E4)
fixes the 2x wealth error for the SCF's four shared variables. John asked,
before leaving for the night, for empirical evidence on how the banding
rules behave when a survey's inconsistency is of a different kind (scaled
dollars, a different definition, rounding, top-coding), and across years.
He has not chosen a method; these runs are evidence for that discussion.
Launched with his go ("try that while I'm out").

## Set A: a third survey with known distortions

There is no third survey prepared, so the SCF 2015 wave stands in for one.
Each distortion is applied to the SCF's shared variables (wages, interest,
dividends, capital gains) before banding; the truth is the undistorted SCF,
so the cost of each distortion under each banding rule is measured exactly.

Distortions (survey side only; the PUF is never changed):

- **D1 scale.** Every shared amount multiplied by 0.5. John's example.
- **D2 scale, uneven.** Wages x 0.9, interest x 0.5, dividends x 0.5,
  capital gains x 2 (the SCF's pass-through income is about 2x SOI).
- **D3 definition.** SCF wages replaced by wages plus business and farm
  income (SCF variable BUSSEFARMINC from the raw income file), the way a
  survey "earnings" concept bundles what the PUF keeps apart.
- **D4 rounding.** Amounts rounded to the nearest $1,000 (so anything under
  $500 becomes zero), as respondents do.
- **D5 top-coding.** Amounts capped at the PUF's 99th percentile, as the CPS
  does.

Banding rules, each applied to each distortion:

- **R0 within-source ranks** (the v2 rule).
- **R1 PUF cut points, no translation** (E4).
- **R2 translate, then PUF cut points.** The translation is chosen by a
  script from the detection table and the aggregate totals, by a stated
  rule (see `10_translate.py`), not by hand. If the rule says "scale by
  s", the survey's amounts are divided by s before banding. If it says
  "same dollars", nothing is done (R2 = R1). The rule cannot fix D3; that
  run shows what a definition problem costs when it is misread as scale.

Fifteen runs, tags A_D1_R0 ... A_D5_R2, all scored by `09_score.py` on the
09 scoreboard (wealth on PUF-style records against the real SCF, holder
shares, label swap, tilt move). References: E0 (R0, no distortion) and E4
(R1, no distortion) from `09_RESULTS.md`.

What we expect, written before running: D1 under R0 equals E0 (ranks are
unchanged by a uniform scaling); D1 under R1 is worse than E4; D1 under R2
returns to E4. D4 under R0 is a bit worse than E0 and under R1 about E4.
D3 is bad under every rule and the detection table flags it. D5 hurts the
top under R1 and R2, less under R0.

## Set B: across years

The three-wave table (`stacked_years.parquet`: PUF 2015, SCF waves 2016,
2019, 2022 = tax years 2015, 2018, 2021). Four ways to band the SCF's
later waves, since there is no PUF for 2018 or 2021:

- **Y0 within source-year ranks** (the v2 rule, the table as built).
- **Y1 PUF 2015 cuts grown.** Cut points for year t = PUF 2015 cut points
  times a growth factor per variable (SOI totals if the 2018 and 2021
  totals are at hand, otherwise the SCF's own wave-to-wave growth in the
  variable's total, stated in the results).
- **Y2 PUF 2015 cuts unchanged** for every year (no growth), to see what
  the growth adjustment is worth.
- **Y3 PUF 2015 cuts moved by the SCF's own percentile growth**: cut k in
  year t is the SCF wave t's quantile at the percentile where cut k sits in
  the SCF 2015 wave. Growth per percentile rather than one factor.

Scored per year: PUF-style records generated with the year token set to
2015, 2018, 2021, net worth against the actual SCF wave of that year (total,
mean, median, top 1% share), holder shares against that wave; the 2015 column
also against the PUF. Results in `10_RESULTS_YEARS.md`.

## Set C: scoping a real third survey

Read-only. What the production CPS ASEC tax-unit builder
(`src/nonfilers/asec_tax_units.R`) yields for 2015: which of our shared
variables exist (capital gains do not), what a CPS-owned variable would be
(age is fully present; the PUF's age is missing non-randomly), and what a
build would take. Written to `10_CPS_SCOPING.md`. Nothing built.

## Revision history

- 2026-10-15: written, before any run.
