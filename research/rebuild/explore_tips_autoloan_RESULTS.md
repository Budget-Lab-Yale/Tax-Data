# Tips (SIPP) and auto loan interest (SCF): results, 27 October

Candidate run on branch `rebuild-tips-autoloan` (worktree `Tax-Data-tips`), outputs in
`/nfs/roberts/scratch/pi_nrs36/jar335/rebuild_tips`. Nothing here is installed as latest.

## 1. What was built

Two new variables, each learned in pretraining with the shared variables and drawn at generation:

- **Tips, from the SIPP**, a fifth source with its own label. Five reference years (2015, 2017, 2019, 2021, 2023;
  about 128,000 tax-unit rows) are split into tax units by the shared module (`tax_units.py`, CBO's rules) and go into
  the one training table. The SIPP shares with the PUF married, dependents, the age range, the exact ages of the
  primary filer and the spouse, and wages; it shares with the CPS the spouse's share of the couple's wages. Its own
  slots are each adult's tips as a share of that adult's wages, and whether that adult's tips are mostly from leisure
  and hospitality. At generation they are drawn last, under the SIPP label at 2019, after the CPS has drawn the split
  of the couple's wages. The file's tips1 is the primary filer's share times wages1, tips2 the spouse's share times
  wages2, and tips = tips1 + tips2 (largest difference on any record $0.00000000005). No record has tips above that
  person's wages, tips2 without a spouse, or a leisure flag without tips.
- **Auto loan interest, from the SCF** (`auto_int_exp`), a new SCF-only amount in every wave: the amount still owed on
  each of up to four vehicle loans times its current annual rate (Tedeschi's rule). It is drawn in the SCF phase from
  the record's own wave and grown per tax unit to the 2019 wave, like the balance sheet.
- The whole pipeline ran once (every stage, two pooled models trained 80 passes each, 400,000 filer records), and the
  tilt used the current 2019 targets and settings unchanged. Code: `roster_sipp.py`, `sipp_read.py`, `report_sipp.py`
  (new); `build_scf.R`, `build_table.py`, `translate.py`, `model.py`, `train.py`, `generate.py`, `dollars.py`,
  `tilt.py`, `tax_units.py`, `settings.json`, `run.sbatch` (changed). `FEATURES.md` has candidate paragraphs on both.

**Checks before the run.**
- The SIPP's tax units pass every integrity test of `check_units.py` in all five years (no one counted twice or in no
  unit, every unit one head; `rebuild_tips/data/checks/checks_sipp_<year>.md`).
- The SIPP's units against the CPS's units of the same year (millions, nominal dollars):

| Year | Units, SIPP | Units, CPS | People, SIPP | People, CPS | Joint, SIPP | Joint, CPS | Wages, SIPP ($bn) | Wages, CPS ($bn) | Units with wages, SIPP | Units with wages, CPS | Tips, SIPP ($bn) | People with tips, SIPP |
|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| 2015 | 162.5 | 163.1 | 314.5 | 318.6 | 59.8 | 60.6 | 6,822 | 7,576 | 115.7 | 118.6 | 26.5 | 3.20 |
| 2017 | 168.5 | 166.8 | 321.2 | 323.0 | 60.8 | 61.5 | 7,794 | 8,294 | 117.8 | 121.3 | 28.8 | 3.57 |
| 2019 | 170.3 | 168.6 | 323.9 | 327.4 | 59.6 | 62.2 | 8,410 | 9,348 | 118.6 | 124.4 | 33.6 | 3.77 |
| 2021 | 173.3 | 171.6 | 325.0 | 328.6 | 59.5 | 61.2 | 9,116 | 9,850 | 118.7 | 122.8 | 34.9 | 3.71 |
| 2023 | 178.7 | 175.6 | 330.1 | 332.1 | 59.8 | 62.1 | 10,864 | 11,112 | 124.4 | 127.1 | 34.2 | 3.63 |

  Wages are the primary filer's and the spouse's on jobs for an employer; the CPS's are its reported wages before
  translation. Separate returns are already split on both sides.
- The translation report (`rebuild_tips/data/translation_report.csv`; with the fill-in stage,
  `model/label_seed20261016/translation_report.md`) has a SIPP section for every year: married, dependents, the age
  range and the exact ages are close to the tax data and the CPS (2019: 35.0% married against 35.5%; 71.7% with no
  dependents against 67.3%, the CPS's gap too); wages are in decision 9; the spouse's share of wages is close to the
  CPS's (2019: recorded on 26.8% of units against 29.5%, median 0.38 against 0.39, 17.6% at 1 against 14.1%).

### Decisions, for approval or rejection

1. **Years.** Reference years 2015, 2017, 2019, 2021 and 2023, the CPS's years (settings `sipp.years`): 2015 is the
   2014 panel's wave 3, the others the 2018, 2020, 2022 and 2024 releases in `SIPP/v2/20260927` (the complete 2018
   file). The SIPP label draws at 2019, the target year, as the CEX and the CPS do.
2. **Households as of the interview**, the CPS's convention: the people present at the interview, income for the
   12 months of the reference year. People who left before the interview are not household members (2019: 1,202
   people, 7.7 million weighted). A sample unit that holds two or more householders at the interview (2019: 437 of
   21,982) is split into one household per householder, each person going with the householder of their December
   residence. Ages at the interview (TAGE), as the CPS. Weights: each person's December weight; units with weight 0
   (17-72 a year) are left out of the table.
3. **The SIPP's inputs to the tax-unit rules** (`tax_units.FALLBACKS['sipp']`): spouse and parent pointers as the SIPP
   records them; student = enrolled in December with no earnings that month (for the CPS's "in school and not in the
   labor force"); disability = any of the six core questions (RDIS, the CPS's DIFFANY); income test from wages,
   self-employment, property income, unemployment, Social Security and other income including pensions.
   Separate returns by the CEX's rule (keyed draw to SOI Table 1.6's count).
4. **What counts as tips: production's definition** (tips.R, annualize.R): tips on jobs for an employer (class of
   worker 1-6), job lines 1-4, summed over the 12 months; tips on self-employment are left out. The SIPP's job
   earnings include tips (in every release no job-month has tips above the job's earnings), so tips are part of
   wages, as on Form W-2.
5. **Leisure and hospitality: Census industry codes 8560-8690**, more than half of the person's tips from such jobs
   (production: 8561-8690). The 2014 panel uses the 2012 Census codes, where 8560 holds performing arts, spectator
   sports and independent artists (later split into 8561-8564); starting at 8560 keeps that group in 2015.
6. **Tips as each adult's share of that adult's wages** (zero without wages), not dollars. It keeps tips inside
   the person's wages and makes tips1 and tips2 follow wages1 and wages2 exactly. Its cost is decision 7.
7. **Known problem from decision 6: tips at high wages.** The file gives 0.013 million primary filers with their own
   wages above $200,000 (441 records) tips of $2.7 billion (8% of the file's tips); the SIPP has almost no tipped workers above
   $200,000 ($0.06 billion), and its wages are top-coded. The network gives a few top earners a tip share that the
   SIPP sees only at low wages, and the share multiplies large wages. Options, John's call: (a) draw tips in dollars
   instead, with tips at most the person's wages as a rule; (b) keep shares but hold tips to the SIPP's range of
   wages; (c) leave it, as untargeted noise. Nothing was changed.
8. **The spouse's share of the couple's wages is shared between the SIPP and the CPS**, a new kind of translation
   row (`shared_with: cps`): taken as reported, cut on the CPS's cut points of the same year. Without it the SIPP
   view could not tell which spouse earns, and tips2 would often fall on a spouse with no wages.
9. **SIPP wages by count from the top** (dollar line 0, as the CPS's wages), the one departure from the defaults: the
   SIPP has more units than the tax data above $100,000 in every year (2019: 25.0 against 22.0 million) and almost
   none above $1 million (0.01 against 0.19 million), the top 1% holding 10.9% of wages against 14.3%. Its share with
   no wages is 6-8 points above the tax data's in every year (2019: 30.4% against 23.1%); rank from the top leaves
   that as the SIPP's own pattern of zeros, seen through its label. Tip shares use the SIPP's own wages.
10. **Shared with the PUF: only married, dependents, the age range, the exact ages and wages.** The SIPP also has
    self-employment, Social Security and property income; they are not shared, since tips need none of them and each
    would need its own translation check.
11. **Dependent filers hold no tips** (every survey item is zero on them, the existing rule), and a SIPP dependent's
    own tips go nowhere (build_cps.R's rule). Production gives dependent filers $1.75 billion of tips on 0.96 million
    returns.
12. **No benchmarking to W-2 tips.** Production scales tips to 2017 IRS W-2 counts and averages; the rebuild adds
    no targets (TARGETS.md S10), so the file keeps the SIPP's level (decision for later: a W-2 target).
13. **Auto loans: the same items in every wave.** X2218/X2219, X2318/X2319, X2418/X2419, X7169/X7170 are "how much
    still owed" and "current APR" (hundredths of a percent; median about 4-5%) for vehicles 1-4 in the 2013, 2016,
    2019 and 2022 codebook labels and data. The rate's code -1 (about 2.2-2.9 million households with a balance)
    reads as 0, as in Tedeschi's code. The four balances are 7-10% below the summary file's VEH_INST, which also
    holds loans on other vehicles; those are left out, as in his code.
14. **Auto loans within the household: as installment debt.** A unit holding the whole PEU takes all of it; when the
    respondent and spouse or partner are separate units, each takes the share build_scf.R gives installment debt
    (their earnings shares); units outside the PEU take none (the group's vehicle loans are not asked; its debts
    are one item, X6439).
15. **Auto loan interest in 2019 dollars** by the balance sheet's rule: each wave grown per tax unit to the 2019
    wave's mean. It is a plain SCF amount, not tied to installment debt by a rule (0.04 million of the file's 51.05
    million units with interest have no installment debt), and not part of net worth.
16. **Cards.** One model trained on an H100, the other on an H200 (the latest run's two on L40S cards).

## 2. Tips and auto loan interest in the 2019 file

Nominal 2019 dollars, after the tilt. "Units with any" in millions; "mean" is among units with any. Neither variable
is targeted. Production is the file Tax-Simulator reads (`Tax-Data/v1/2026070814/baseline/tax_units_2019.csv`); its
AGI is approximate (computed here from its income columns, taxable Social Security by the worksheet, no adjustments).
Full tables: `rebuild_tips/explore/results_tables.md` and `rebuild_tips/tilt/seed20261016/sipp_report.md`.

### Tips

| | File | SIPP 2019 | Production |
|---|---:|---:|---:|
| total ($ billion) | 33.95 | 33.64 | 43.87 |
| units with any (millions) | 3.67 | 3.76 | 6.93 |
| people with any (millions) | 3.68 | 3.77 | 6.96 |
| mean per unit with any ($) | 9,249 | 8,956 | 6,328 |
| primary filers: people, tips ($bn) | 3.30, 30.36 | 3.25, 29.71 | 6.36, 39.10 |
| spouses: people, tips ($bn) | 0.38, 3.58 | 0.52, 3.93 | 0.60, 4.77 |
| tips mostly from leisure and hospitality ($bn; share of tips) | 22.85 (67%) | 24.51 (73%) | 26.82 (61%) |
| people whose tips are mostly from leisure and hospitality (millions) | 2.52 | 2.64 | 4.01 |
| dependent filers' tips ($bn) | 0 | (go nowhere) | 1.75 |

Production benchmarks tips to IRS W-2 data for 2017: 5.97 million tipped workers (1.16 million married, averaging
$7,072; 4.80 million unmarried, averaging $5,686), $35.5 billion in 2017 dollars (`src/imputations/tips.R`), then
raises the number of tipped workers by the SIPP's rise in tipping from 2017-19 to 2021-22. The SIPP finds about 60%
of the W-2 count of tipped workers at a higher average, so the file has about three quarters of production's total.

By the unit's wages (the SIPP's wages translated by count from the top):

| Wages | File: units with any | File: $bn | File: mean | SIPP: units with any | SIPP: $bn | SIPP: mean | Prod.: units with any | Prod.: $bn | Prod.: mean |
|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| $1-25k | 1.68 | 6.84 | 4,063 | 1.49 | 5.45 | 3,664 | 3.90 | 10.40 | 2,669 |
| $25k-50k | 1.17 | 10.47 | 8,977 | 1.31 | 12.71 | 9,731 | 1.63 | 12.47 | 7,643 |
| $50k-100k | 0.62 | 9.32 | 14,980 | 0.71 | 10.49 | 14,807 | 1.07 | 12.40 | 11,627 |
| $100k-200k | 0.16 | 3.41 | 21,863 | 0.20 | 4.14 | 20,582 | 0.28 | 6.64 | 24,066 |
| $200k+ | 0.04 | 3.90 | 93,600 | 0.05 | 0.86 | 15,889 | 0.06 | 1.97 | 31,385 |

By AGI (the SIPP has no AGI):

| AGI | File: units with any | File: $bn | File: mean | Prod.: units with any | Prod.: $bn | Prod.: mean |
|---|---:|---:|---:|---:|---:|---:|
| zero or a loss | 0.01 | 0.10 | 10,009 | 0.03 | 0.15 | 5,601 |
| $1-25k | 1.58 | 6.45 | 4,091 | 3.59 | 10.10 | 2,815 |
| $25k-50k | 1.16 | 10.07 | 8,716 | 1.66 | 12.18 | 7,349 |
| $50k-100k | 0.66 | 9.28 | 14,154 | 1.12 | 11.26 | 10,068 |
| $100k-200k | 0.21 | 3.68 | 17,921 | 0.41 | 7.35 | 17,862 |
| $200k-500k | 0.06 | 2.04 | 37,157 | 0.09 | 1.59 | 17,149 |
| $500k-1M | 0.01 | 0.67 | 90,029 | 0.02 | 0.26 | 11,841 |
| $1M+ | 0.01 | 1.66 | 306,185 | 0.02 | 0.98 | 62,524 |

The top rows are decision 7: below $200,000 of wages the file follows the SIPP closely; above it the file has $3.9
billion against the SIPP's $0.9 billion.

### Auto loan interest

| | File | SCF pooled (2019 54.65%, 2022 33.15%, 2016 12.20%) | Production |
|---|---:|---:|---:|
| units (millions) | 178.89 | 162.76 | 183.48 |
| total ($ billion) | 45.93 | 43.16 | 62.15 |
| units with any (millions) | 51.05 | 47.32 | 52.22 |
| mean per unit with any ($) | 900 | 912 | 1,190 |
| per unit, all units ($) | 257 | 265 | 339 |

The file has more units than the SCF (dependent filers and the CPS's non-filers), so its total is higher at about
the SCF's amount per unit. The SCF pooled is each wave grown per tax unit to the 2019 wave, weights scaled to the 2019
wave's units, as the SCF targets are (each wave's 2019-dollar total per unit is therefore the 2019 wave's).

By income (the seven items the SCF and the file share: wages, interest, dividends, capital gains, gross retirement
income, Schedules C and F, Schedule E; production by its AGI):

| Income | File: with any | File: $bn | File: mean | SCF: with any | SCF: $bn | SCF: mean | Prod.: with any | Prod.: $bn | Prod.: mean |
|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| zero or a loss | 0.40 | 0.39 | 983 | 0.58 | 0.62 | 1,068 | 2.05 | 2.46 | 1,197 |
| $1-25k | 7.65 | 6.56 | 858 | 5.60 | 4.08 | 728 | 9.84 | 12.93 | 1,314 |
| $25k-50k | 14.35 | 12.92 | 900 | 11.87 | 11.18 | 942 | 15.54 | 19.01 | 1,223 |
| $50k-100k | 16.11 | 14.84 | 921 | 15.55 | 15.02 | 966 | 14.35 | 15.75 | 1,097 |
| $100k-200k | 9.68 | 8.59 | 888 | 11.00 | 9.64 | 876 | 8.15 | 8.30 | 1,018 |
| $200k-500k | 2.56 | 2.28 | 889 | 2.41 | 2.18 | 904 | 2.04 | 2.93 | 1,436 |
| $500k-1M | 0.24 | 0.26 | 1,057 | 0.25 | 0.30 | 1,233 | 0.17 | 0.55 | 3,219 |
| $1M+ | 0.06 | 0.09 | 1,385 | 0.06 | 0.13 | 2,347 | 0.08 | 0.23 | 3,010 |

By AGI in the file: $0.86 billion below $1, $7.73 billion at $1-25k, $12.25 billion at $25k-50k, $14.01 billion at
$50k-100k, $8.61 billion at $100k-200k, $2.19 billion at $200k-500k, $0.22 billion at $500k-1M, $0.07 billion at
$1M+ (production: 2.46, 12.93, 19.01, 15.75, 8.30, 2.93, 0.55, 0.23).

By the unit's wages:

| Wages | File: with any | File: $bn | SCF: with any | SCF: $bn | Prod.: with any | Prod.: $bn |
|---|---:|---:|---:|---:|---:|---:|
| none | 6.41 | 6.09 | 6.89 | 6.70 | 6.04 | 6.64 |
| $1-25k | 8.01 | 6.77 | 5.70 | 4.40 | 9.29 | 12.57 |
| $25k-50k | 13.02 | 11.77 | 9.95 | 9.28 | 15.59 | 18.36 |
| $50k-100k | 13.77 | 12.53 | 13.48 | 12.70 | 13.45 | 15.39 |
| $100k-200k | 7.95 | 7.07 | 9.39 | 8.23 | 6.18 | 6.54 |
| $200k+ | 1.89 | 1.70 | 1.91 | 1.86 | 1.67 | 2.65 |

By the primary filer's age range (production: age1):

| Age | File: with any | File: $bn | File: mean | SCF: with any | SCF: $bn | SCF: mean | Prod.: with any | Prod.: $bn |
|---|---:|---:|---:|---:|---:|---:|---:|---:|
| under 26 | 2.99 | 2.20 | 735 | 2.54 | 1.82 | 716 | 6.49 | 7.51 |
| 26-34 | 10.48 | 8.77 | 837 | 9.97 | 8.62 | 865 | 11.42 | 15.45 |
| 35-44 | 11.22 | 11.10 | 989 | 9.92 | 9.74 | 982 | 10.15 | 11.72 |
| 45-54 | 10.63 | 10.62 | 999 | 9.84 | 9.72 | 988 | 10.67 | 12.28 |
| 55-64 | 9.06 | 7.79 | 860 | 8.00 | 7.00 | 876 | 8.37 | 10.60 |
| 65+ | 6.67 | 5.46 | 818 | 7.06 | 6.26 | 887 | 5.12 | 4.59 |

The file tracks the SCF by age and income; it has more holders with low wages and income than the SCF (8.0 against
5.7 million with wages of $1-25k). Production's total is 35% higher, mostly from a higher mean per holder ($1,190 against $900; its model is
fitted on the 2022 SCF alone).

## 3. What else moved, against the latest run

Latest: `/nfs/roberts/scratch/pi_nrs36/jar335/rebuild/tilt/seed20261016/`. Both runs: the same 496 targets and
settings, 454,379 records. Full tables: `rebuild_tips/explore/compare_latest.md`.

| | Latest | New |
|---|---:|---:|
| targets inside their ranges | 496 of 496 | 496 of 496 |
| effective sample kept | 98.0% | 97.5% |
| KL move (nats) | 0.0138 | 0.0149 |
| weight factor, 1st and 99th percentiles | 0.32, 1.99 | 0.37, 2.05 |
| largest weight factor in a targeted cell | 2,594 | 307 |
| records moved more than tenfold up, down | 85, 1,457 | 112, 630 |
| thin cells: stops, warnings | 33, 49 | 27, 53 |
| published SOI cells not targeted, median miss in standard errors: Tables 1.4, 1.6, 1.7 | 1.55, 2.02, 1.86 | 1.49, 1.83, 2.15 |
| filers with AGI $1M+, before the tilt (millions) | 0.596 | 0.606 |
| filers with AGI at or below 0, before the tilt (millions) | 3.02 | 3.16 |
| net worth before, after the tilt ($ trillion; pooled SCF 97.1) | 119.5, 97.4 | 103.0, 97.3 |
| median net worth after the tilt | $53,623 | $53,809 |
| top 1% share of net worth after the tilt | 40.7% | 41.0% |

Totals in the file after the tilt ($ billion), latest against new:

| Item | Latest | New | Change |
|---|---:|---:|---:|
| wages | 8,530 | 8,631 | +1.2% |
| interest | 153.9 | 154.5 | +0.3% |
| dividends | 352.4 | 351.5 | -0.2% |
| capital gains | 851.1 | 853.7 | +0.3% |
| business income | 1,279.6 | 1,256.8 | -1.8% |
| taxable pensions | 904.7 | 869.0 | -3.9% |
| gross Social Security plus pensions | 2,328.5 | 2,226.8 | -4.4% |
| gross Social Security | 888.5 | 885.8 | -0.3% |
| mortgage interest paid | 310.4 | 305.9 | -1.4% |
| total consumption | 6,904.9 | 6,884.7 | -0.3% |
| the spouse's wages (wages2) | 2,102.6 | 1,996.0 | -5.1% |
| childcare | 73.8 | 64.9 | -12.1% |

The PUF items moved little: the targeted ones by under 2%, the untargeted pensions by 4%. Two untargeted survey items
moved more. Childcare fell 12% (10.2 against 11.3 million units with any); the spouse's wages fell 5%, the CPS's split
of the couple's wages, which the SIPP now shares. Before the tilt the new models give $16 trillion less net worth
($103.0 trillion, closer to the SCF's $97.1 trillion); after the tilt the two agree. Of the published cells not
targeted, the largest moves are in thin cells: rental and royalty losses at AGI $500k-1M went from 3.4 standard
errors short to 22 over; estate and trust income at $5-10M from 69 over to 2.0. Some of these differences are what a
second pair of training seeds would give on its own; that has not been measured.

## 4. Training time and memory

| | Card | Minutes | Seconds a pass (median) | Peak GPU memory (GB) | Held-back loss, last pass |
|---|---|---:|---:|---:|---:|
| latest, seed 20261016 | L40S | 45.7 | 33.9 | 8.57 | 0.746 |
| latest, seed 20261017 | L40S | 45.5 | 33.8 | 8.57 | 0.747 |
| new, seed 20261016 | H100 | 67.1 | 50.1 | 8.78 | 0.756 |
| new, seed 20261017 | H200 | 35.1 | 25.8 | 8.79 | 0.759 |

The table grew by about 128,000 SIPP rows (about 14%), with few slots each; peak memory rose 2%. The latest models
were trained on L40S cards, so the times are not on the same card; the H200 took 35 minutes. The held-back loss per
source is unchanged for the PUF (0.825, 0.826), the SCF (1.299, 1.311), the CEX and the CPS; the SIPP's is 1.071.
Generation, the tilt and the reports took 16 minutes on an H100.

### Not done, or left open
- The tips problem at high wages (decision 7) is not fixed.
- No second pair of seeds, so how much of section 3 is seed noise is not measured.
- `FEATURES.md` describes both as candidates; `HISTORY.md` and the latest run are untouched.
