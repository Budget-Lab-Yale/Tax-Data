---
title: "Which SCF waves the starting file for each year draws from"
role: design
workstream: rebuild
status: current
updated: 2026-10-18
sot: research/rebuild/design/plan.md
supersedes: []
superseded_by: null
---

# Which SCF waves the starting file for each year draws from

Decided by John on 18 October 2026. Post-training for a year is two lists:
the targets for that year, and the starting file ("prior") that is
reweighted to them. This note records the rule for the prior's mix of SCF
waves. The targets for each year are a separate list, not yet written
(for wealth, the Fed's Distributional Financial Accounts give totals and
shares every quarter, `resources/dfa/`).

## How a wave is dated

Each SCF wave asks about the previous calendar year's income and the
wealth held at the interview. A wave is labelled by its income year, and
its wealth is treated as wealth at the end of that year: the 2016 wave is
"2015", the 2019 wave "2018", the 2022 wave "2021", the 2025 wave "2024".
The few months between the end of the income year and the interview are
ignored.

## The rule

For target year T:

1. Use the next survey at or after T, if one exists, and the surveys
   before T. Never a survey beyond the next one.
2. Weight each by exp(−|T − t| / h), where t is the survey's income year
   and h = 2 years.
3. Drop any survey whose share is under 5%, and rescale the rest to 100%.

Past the last survey, the mix stays fixed.

## The weights it gives (%)

Surveys by income year (wave in brackets).

| Target year | Weights |
|---|---|
| 2012 | 2012 (2013 wave) 82, 2009 (2010) 18 |
| 2013 | 2015 (2016) 33, 2012 (2013) 55, 2009 (2010) 12 |
| 2014 | 2015 (2016) 57, 2012 (2013) 35, 2009 (2010) 8 |
| 2015 | 2015 (2016) 82, 2012 (2013) 18 |
| 2016 | 2018 (2019) 33, 2015 (2016) 55, 2012 (2013) 12 |
| 2017 | 2018 (2019) 57, 2015 (2016) 35, 2012 (2013) 8 |
| 2018 | 2018 (2019) 82, 2015 (2016) 18 |
| 2019 | 2021 (2022) 33, 2018 (2019) 55, 2015 (2016) 12 |
| 2020 | 2021 (2022) 57, 2018 (2019) 35, 2015 (2016) 8 |
| 2021 | 2021 (2022) 82, 2018 (2019) 18 |
| 2022 | 2024 (2025) 33, 2021 (2022) 55, 2018 (2019) 12 |
| 2023 | 2024 (2025) 57, 2021 (2022) 35, 2018 (2019) 8 |
| 2024 to 2060 | 2024 (2025) 82, 2021 (2022) 18 |

## What it needs

- The 2025 wave (income year 2024) is out and will be used; it is not yet
  in the training data.
- Years before 2013 need the 2010 wave and earlier in training.
- The 2019 file takes a third of its prior from the 2022 wave, the
  pandemic-era survey; an earlier test found that wave alone starts too
  rich (`minimal/21_PRIORS.md`). Noted, not a reason to change the rule.
- Each record's net worth position is turned into dollars with its own
  wave's values (plan decision 10), not grown from another wave's.
- The mixing code exists (`minimal/32_mix.py`, from the year-mix test of
  18 October, `minimal/32_RESULTS.md`).
