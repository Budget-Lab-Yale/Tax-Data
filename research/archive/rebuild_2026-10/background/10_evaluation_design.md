---
title: "How we will know the rebuilt file is better: an evaluation design"
role: evidence
workstream: rebuild
status: current
updated: 2026-10-07
sot: research/rebuild/BRIEF.md
supersedes: []
superseded_by: null
---

# How we will know the rebuilt file is better: an evaluation design

Background paper 10 for the Tax-Data rebuild. It proposes the tests and the
numbers John would use to judge a candidate file against the current one. It
draws on the other background papers in `research/rebuild/background/`
(01, 03, 04, 04b, 05, 06, 07, 08; 02 and 09 were unfinished when this was
written), on the Tax-Data code, and on read-only inspection of Tax-Simulator
and tax-simulator-gpu. Nothing was run.

"Verified" means I opened the file, page or code and saw the fact. Facts
taken from another background paper are cited to that paper, which states its
own verification. The web-search allowance was exhausted; the few pages I
fetched directly are listed in the sources section.

## Summary

1. **Today's validation is a set of one-off scripts, not a standing test.**
   `src/eda/` holds 152 scripts, almost all written to answer one question
   about one module (wealth, consumption, Forbes, non-filers). The pipeline
   itself writes one diagnostic file (the population partition check) and
   computes, but does not save, a comparison of the 2017 file with SOI. There
   are no held-out targets, no weight diagnostics, no backtest, and no fixed
   downstream comparison. Several pieces are reusable (section 1).
2. **The most useful single design is a backtest across known years.** Fit
   the method using data only through a cutoff year, project the following
   years, and score the projections against SOI Publication 1304 tables
   that were published later. Three cutoffs fit the data we have: 2017
   (scores the TCJA years 2018–2019), 2019 (scores the pandemic years
   2020–2021 and 2022–2023), and 2021 (scores 2022–2023). The benchmark
   to beat is the current method: the 2017 linear program plus one growth
   factor per variable. Realtime Inequality, HMRC and Treasury's 1989 paper
   all judge their aging methods this way (section 2).
3. **Run the backtest with realized aggregates first.** Feeding the method
   the actual later totals isolates what Tax-Data is responsible for, the
   distribution given the totals, from the macroeconomic forecast, which
   Macro-Projections owns. A second run with CBO's June 2017 projections
   (on disk) measures total error for the 2017 cutoff only.
4. **Some targets must stay out of every fit so they remain checks.**
   Income tax after credits by AGI (Table 3.3), credits by AGI, and SOI's
   top-percentile table (Table 4.3) are the natural permanent holdouts,
   because they are what Tax-Simulator computes. Other target groups are
   rotated out one at a time (section 3). PolicyEngine's Populace leaves
   projected incomes untargeted for the same reason (report 07).
5. **The new solver supplies most calibration diagnostics for free.** Each
   target's miss divided by its standard error, the mean square of those
   misses within each source group, the multiplier on each target, the
   effective sample size overall and by cell, the spread of weight ratios,
   and a per-output "data-determined share" can all be computed from
   quantities the solver already holds (section 4).
6. **Tax-Simulator is the test that matters, and the GPU model makes a
   large panel cheap.** One ten-scenario, thirty-year run took about 100
   to 220 seconds on one H200 (tax-simulator-gpu's own measurement, taken
   before September 2026). A candidate Tax-Data vintage needs one
   conversion job before the GPU model can read it. Calibrated values in
   both models are pinned to a Tax-Data vintage and stop a run when the
   vintage changes; the comparison has to say whether they were held fixed
   or re-fitted (section 6).
7. **Change and improvement are different claims.** Improvement can be
   shown only against something true: held-out tables, backtested years,
   and tax liability under actual law. For forward reform scores, which
   have no true value, the design records whether the score moved, why
   (which inputs moved it), and whether those inputs moved toward or away
   from held-out evidence. JCT, CBO, Treasury and TPC estimates serve as
   reference points, not as truth.
8. **A difference is meaningful only beyond a measured noise floor.**
   Shifting Tax-Data's random stream alone moved imputed aggregates by up
   to about 4 percent (measured on 2026-09-12, before draws were keyed to
   record id), and two rebuilds of one commit differed by 9.5 percent in
   aggregate tips before the S31 fix (CLAUDE.md). The design measures seed noise
   and sampling noise (by dropping groups of records and refitting) before
   any old-versus-new comparison is read.
9. **A simulation with a known answer tests the method apart from the
   data.** Split the PUF in two, distort one half into a known "true" later
   population, generate SOI-style tables from it with SOI's own sampling
   error and the itemization filter, start from the other half aged the
   wrong way, and check what the method recovers. This separates solver
   bugs, identification limits and data problems (section 7).
10. **A one-page scorecard, written by every run, puts the old and new
    numbers side by side** with a red-flag rule for each (section 8).
11. **Inside two weeks:** the scorecard with integrity and calibration
    diagnostics, the target table with standard errors and holdout flags,
    a solver test with a known answer, the downstream comparison harness
    on the GPU model, one backtest (cutoff 2017, realized aggregates), and
    a seed-noise measurement. **Later:** rolling backtests and real-time
    macro inputs, full rotation of held-out groups to tune the prior's
    strength, the simulation study at full scope, sampling-noise replicates
    for both old and new pipelines, and a library of digitized outside
    estimates (section 9).

## 1. What validation exists today

### In the pipeline

- **`src/summary.R`** defines `compare_puf_actual()`, which tabulates the
  file's count and amount by AGI class for every variable that has an SOI
  table and reports the percentage difference. `src/create_2017_puf.R`
  calls it once, on the 2017 file after the linear program, and uses the
  result only to rescale dollar amounts. The comparison is not written to
  disk. It counts a record as having an item when the item is nonzero and
  sums with `na.rm = T`.
- **`src/project_puf.R`** writes `ssarea_partition_check.csv`: filer adults
  plus non-filer adults plus netting against Social Security area adults,
  by year and age band, and stops if the non-filer side misses the pool by
  more than one part in a million.
- **`src/materialize.R`** carries built-in assertions on its own arithmetic
  (lines 373–519), and `write_outputs.R` writes an emit manifest.
- **`src/tests/`** has five tests: the ASEC tax-unit builder, the
  non-filer contract, reproducibility of the forked quantile-forest fits
  (S31), the state-weights data layer, and the linear program's
  tolerance search. None tests the weighting method's statistical
  behaviour.

### In `src/eda/`

152 files. By name and a sample of headers, they fall into these groups:

| Group | Examples | Reusable for |
|---|---|---|
| Vintage comparison | `compare_vintages.R` (schema, weighted aggregates per variable and year, split-sum identities, mortality checks), `validate_refactor_diff.R` (record-level differences within a tolerance) | The integrity layer of the scorecard |
| Linear-program checks | `lp_instrument.R` (recomputes every constraint on the solved weights), `lp_convergence.R`, `senior_cell_drift.R` | The rule that every target is recomputed on the written file, not taken from the solver |
| Aging checks | `sanity_check_aging.R` (year-over-year jumps, consumption against NIPA and GDP), `verify_aging_pce.R`, `verify_split_identity.R` | Integrity and bystander checks |
| Imputation validation | `conditional_diagnostic.R` (consumption conditional on income percentile and demographics, 81 cells), `validate_top_tail.R`, `verify_pce_bench.R`, `stage3_target_match_verify.R` (wealth tilt targets matched within 5, 10, 25 percent), `validate_salt_*.R`, `validate_mortality.R` | The standard imputation battery (section 5) |
| Module investigations | about 100 wealth, Forbes, SCF and age scripts | Mostly not reusable as standing tests |
| Reports | `deep_review_pass1–4.R`, `report_v3.R`, `joint_objective_html_report.R` | Formatting templates |

### In `research/state_weights/`

`research/state_weights/nonfiler_federal_validation.md` is the closest thing
to a standing procedure for judging a new vintage through Tax-Simulator. It
does four things the rebuild should copy:

- It writes down, before running, which Tax-Simulator outputs must move
  and which must not ("the predicted signature").
- It builds the change as a sequence of vintages, each adding one piece,
  so each difference can be attributed.
- It runs a cheap pre-flight comparison of the two vintages before any
  model run.
- It separates hard assertions from printed judgment calls.

Its first run (`research/state_weights/nonfiler_federal_validation_findings.md`)
found that 187 of 209 filer-gated columns moved when only non-filers
changed, because random draws in both repositories were tied to record
position. Both have since been keyed to record id (S23 in Tax-Data; in
Tax-Simulator, `config_parser.R` now joins draws by id, and the GPU model's
bridge draws are "a deterministic function of household id, purpose, and
seed"). That run also measured how far imputed aggregates move when only
the random stream changes, which section 6 uses as a first noise floor.

### What is missing

No fit report is saved; no target is withheld; there are no weight
diagnostics; nothing compares projections with years that were later
published; there is no fixed set of Tax-Simulator outputs compared between
vintages; and there is no test of the method on data with a known answer.

## 2. Backtests

### Design

A backtest pretends that the newest data do not exist. Fix a cutoff year
T. Build the file using only targets for years up to T, project it to
T+1, …, T+k, and score each projected year against the SOI tables
published later for that year. The horizon is k.

Because the 2015 PUF is the only microdata, every backtest starts from
the same 2015 records. What changes between backtests is the last year of
targets the method is allowed to see.

| Cutoff T | Targets allowed | Scored years | What it tests |
|---|---|---|---|
| 2015 | 2015 tables only | 2016–2019 | Pure aging from the PUF year, with no TCJA in the fitted years |
| 2017 | 2015–2017 | 2018–2019, then 2020–2023 | The TCJA break: whether the reporting simulation predicts post-2017 itemizer counts and amounts |
| 2019 | 2015–2019 | 2020–2021, 2022–2023 | The pandemic years, and a two-year TCJA history |
| 2021 | 2015–2021 | 2022–2023 | Recent years at short horizons |

Report 08 describes how Realtime Inequality does this for 1975–2023 at
one- and two-year horizons, comparing each method with "uniform growth"
and "rescale totals only" benchmarks by income group. HMRC publishes mean
absolute errors of its projections of the tax-record sample: about 2–4
percent one year ahead and 4–6 percent three years ahead for taxpayer
counts, total income and liabilities. The Federal Reserve's Distributional
Financial Accounts drop a whole SCF wave and predict it. Treasury's OTA
Paper 62 (1989, report 07) aged a 1981 file to 1983 with actual 1983
totals as targets and scored each item's distribution across AGI classes.
The design here combines these: rolling cutoffs, explicit naive
benchmarks, and scoring by item and AGI class.

### Scored outcomes for each year

All of these exist for every year 2016–2023 unless noted (report 01). Most
are already in the shared store `raw_data/IRS-Ind`, which Tax-Data does not
read yet.

| Table | Scored quantities | Notes |
|---|---|---|
| Pub 1304 Table 1.4 | Count and amount of every income item by 19 AGI classes | The core. Table 1.4CV gives a coefficient of variation for each cell, so sampling noise in the "truth" can be allowed for. 1.4CV is not yet in the store. |
| Table 1.4A | Capital gains detail by AGI | |
| Table 1.6 | Returns by AGI × filing status × age | Disclosure-merged cells handled by the store's target view |
| Table 1.5 | Income items by age of primary taxpayer | Not in the store |
| Table 1.2 / 1.3 | Items by filing status | Not in the store |
| Table 2.1 | Itemized deductions by AGI, itemizers only | Law-censored after 2017; SALT before the cap and medical before the floor are reported for itemizers |
| Table 2.5 | EITC by number of children × AGI | |
| Table 3.3 | Tax liability, each credit, payments by AGI | Tax-Simulator output |
| Tables 3.4 / 3.6 | Taxable income and tax by marginal rate × filing status | Tax-Simulator output; not in the store |
| Table 4.3 | Top 0.001% to 50% groups: AGI share, income by source, tax | Confirmed 2019–2023 by file probe; not in the store |
| Pub 4801 | National line-item totals | Parsed in the store, 2011–2023 |
| Filing season statistics | All returns processed, about 50 items by 17 AGI classes | Tax year 2024; a population count, not a sample |

Year-2024 filing season statistics allow one further out-of-sample year at
the totals-by-AGI level.

### What each year's score is

For each item (wages, dividends, and so on), each scored year, and each
method:

- **Level error.** The log of the file's total over the published total.
- **Shape error.** Theil's information gain across AGI classes:
  Σ_k s_k log(s_k / ŝ_k), where s_k is the published share of the item's
  amount in AGI class k and ŝ_k the file's share. It is zero when the
  shares match and grows as they diverge. This is OTA Paper 62's measure
  (report 07). A small floor on ŝ_k avoids division by zero.
- **Cell errors in standard-error units.** (file − published) divided by
  the published value times its coefficient of variation from Table
  1.4CV. Large values in cells SOI measures precisely matter more than
  large values in thin cells.
- **Counts and amounts separately.** A single growth factor per variable
  changes the amount per recipient, not the number of recipients (SPSD/M's
  point, report 08), so recipient counts are a distinct test.

Summaries: mean absolute log error and mean information gain across
items, by horizon; and the **relative skill**, the new method's error
divided by the benchmark's error, for each item and horizon. Below one
means the new method is better. Report how many items improve and how
many get worse, not only the average, and report items that drive tax
(wages, capital gains, pass-through income, Social Security) on their own
rows.

### Benchmarks

1. **The current method.** The 2017 linear program and per-bin rescaling,
   then demographic weight growth and one growth factor per variable from
   the same realized aggregates the new method sees. This is the bar the
   rebuild must clear.
2. **Uniform growth.** The 2015 PUF with weights scaled by the total
   return count and each amount scaled by its realized aggregate growth.
   This is Realtime Inequality's naive benchmark. If the current method
   does not beat it, that is worth knowing too.
3. **Persistence of shares** (for the 2017, 2019 and 2021 cutoffs). Each
   item's AGI-class shares in year T carried forward, with the realized
   total. This is the bar any distributional method must clear at short
   horizons.

### Realized versus forecast aggregates

Run every backtest twice where possible.

- **Realized aggregates.** The method receives the actual later totals of
  each income item (from Pub 4801 or Table 1.4 totals), and is scored on
  how it distributes them across AGI classes, filing statuses and ages.
  This measures what Tax-Data controls.
- **Real-time forecasts.** The method receives only the projections
  available at T. CBO's June 2017 economic and budget projections are on
  disk (`raw_data/CBO-Econ-Proj/v3/20170629`,
  `raw_data/CBO-Budget-Proj/v3/20170629`); the next stored vintage is
  May 2023, so a real-time run is possible for the 2017 cutoff only. This
  measures total error, including CBO's forecast error, and shows how much
  of the total is distributional.

### The TCJA break

Items fall into three kinds, reported separately:

- **Law-invariant items** (wages, interest, dividends, capital gains,
  pensions, Social Security, most business income): scored directly.
- **Law-dependent items** (itemized deductions, the number of itemizers,
  child and dependent credits, the 199A deduction, exemptions): scored
  through the reporting simulation. The file's underlying expenses are
  run through that year's law, and the reported amount for those who
  would itemize is compared with Table 2.1. The 2017 cutoff, which sees
  only pre-TCJA itemization, is the direct test of this machinery. The
  current method has no such step; its post-2017 itemized amounts come
  from growth factors applied to pre-TCJA reported amounts, so its error
  on these items is the reference.
- **Behaviour-sensitive items** (pass-through income after 199A, charitable
  giving around the 2017–2018 change, SALT around the cap): the method
  does not model behaviour, so misses here partly reflect responses, not
  data. Realtime Inequality's largest top-1% misses fell in the 1987–88
  reform years (report 08). Report these items but do not count them in
  the headline skill ratio.

### The pandemic years

- 2020 and 2021 carried an unemployment compensation exclusion, stimulus
  payments, an advance child tax credit, and a large rise in returns
  filed to claim payments. Score them, but report them in their own
  block.
- Do not use 2020 or 2021 as a cutoff year in the default rotation. HMRC
  skipped 2020-21 as a base year because COVID distorted it (report 08).
- In the realized-aggregate run, give the method the actual totals for
  unemployment compensation and wages. The question is whether it puts
  them in the right AGI classes. Realtime Inequality found that rescaling
  totals overstates growth for the bottom half in recessions, and that
  using wage-distribution data cut the bias for the bottom half from 3.6
  to 0.2 percentage points (report 08). The current method's single wage
  factor should fail in the same way, which makes 2020 the most
  informative year for the earnings part of a new aging step.

### Avoiding leakage

A backtest is invalid if later data enter through a side door. Known
routes in the current pipeline:

- Table 1.6 return counts through 2023 grow weights
  (`src/project_puf.R`). Truncate them at T.
- The non-filer pool is built from each year's ASEC through 2023. For a
  backtest, freeze it at T or accept that low-AGI filer counts after T
  are partly informed by later surveys, and say so.
- Imputations trained on later surveys (SCF 2022, SIPP through 2024, CEX
  through 2024). They do not feed the scored IRS items before 2025 law
  (tips and overtime deductions start in 2025), so they can stay, but
  the tax-liability score uses them slightly.
- Hand-chosen settings tuned while looking at later years.

### Other microdata routes

- **Older public use files aged to 2015.** Report 08 suggested aging an
  earlier PUF to 2015 and comparing it record-level with the 2015 PUF.
  The shared data store holds only the 2015 SOI PUF
  (`raw_data/IRS-PUF/v1/2023091310/historical/puf_2015.csv`). It also holds
  `raw_data/Saez-Zucman-PUF`, with files named `pufonline1962.dta`
  through `pufonline2016.dta` (about 5 MB each). I did not open them;
  their size suggests a reduced variable set. If they are the
  Piketty–Saez–Zucman tax-unit files, they would support a long series of
  one- and two-year aging backtests in Realtime Inequality's design, on a
  smaller set of income concepts. Someone should check their contents.
- **The 2016 synthetic PUF**, if SOI releases it (report 01: promised,
  not out as of May 2026), would be a second prior to compare against,
  not a truth (report 03).

## 3. Held-out sources

### Permanent holdouts

These stay out of every fit, in every year, so they remain independent
checks.

- **Income tax liability, credits and payments by AGI (Table 3.3), EITC
  by children and AGI (Table 2.5), and tax by marginal rate (Tables 3.4
  and 3.6).** These are what Tax-Simulator computes. If the file is
  forced to match them, the data absorb errors in the tax calculator and
  the check is lost. Populace's static-aging document gives the reason in
  these words: "a weight vector forced to match one can no longer be
  wrong about it, so it can no longer be checked" (report 07). The
  current 2017 target set includes one such item, the saver's credit
  (one row of `config/target_info/baseline.csv`); under this rule it
  would become a check.
- **Table 4.3's top-percentile income composition**, at least in the
  first version, because it is the only published joint view of income
  sources within the top 1 percent and is not needed to fit AGI-class
  totals. Whether to promote it to a target later is a decision for John;
  until then it is the main check on the top tail.
- **CBO's line-by-line 1040 build-up** (see section 6) for actual years.

### Rotating holdouts

The remaining targets are grouped, and each group is withheld in turn
while the rest are fitted. This is cross-validation over target groups.
Proposed groups:

| Group | Contents |
|---|---|
| Counts by status × age | Table 1.6 |
| Labour income by AGI | Table 1.4 wages, counts and amounts |
| Capital income by AGI | Interest, dividends, capital gains (1.4, 1.4A) |
| Business income by AGI | Schedule C, partnership and S corporation, farm |
| Retirement income by AGI | Pensions, IRA distributions, Social Security |
| Itemized deductions | Table 2.1, through the reporting simulation |
| Income by age | Table 1.5 |
| Items by filing status | Tables 1.2 and 1.3 |
| Non-IRS totals | BEA, Financial Accounts, CEX-based totals |
| Whole years | Fit alternate years in a joint multi-year fit (2017, 2019, 2021, 2023) and predict the others |

Two rules make the rotation honest.

- **A held-out group must not be implied by the targets that remain.**
  Total wages in Table 1.3 equals total wages in Table 1.4; holding out
  one while fitting the other tests nothing. Before using a group as a
  holdout, compute its data-determined share given the remaining targets
  (section 4). If it is close to one, the group is not independent.
- **The prior must not have been tuned on the holdout.** A setting chosen
  by looking at held-out misses has to be re-checked on a group not used
  for the choice.

### What the rotation gives

- For each group, the standardized misses of its targets when held out.
  If the stated standard errors and the prior are both about right, their
  mean square should be near one plus the prior's own spread for those
  targets. Very large values mean the method cannot predict that group
  from the others, and the group carries information that must be
  targeted.
- A way to choose the strength of the prior. Report 04 notes that how
  many records' worth of information the prior carries is a tuning number
  the data do not supply. Choosing it to minimize held-out error is the
  standard approach, and is the "direct test" report 04 proposes (hold
  out a random tenth of targets).

## 4. Calibration diagnostics

Notation, as in reports 04 and 06: prior weight d_i, fitted weight w_i,
target j with published value y_j and standard error σ_j, record i's
contribution a_ij, and the file's estimate ŷ_j = Σ_i w_i a_ij. With the
entropy objective, w_i = d_i exp(Σ_j λ_j a_ij), and λ_j is target j's
multiplier, the price the solution pays per unit of that target.

### Weight concentration

- **Kish effective sample size**, (Σ w_i)² / Σ w_i²: the number of equally
  weighted records with the same concentration (reports 05 and 04b).
  Report it for the whole file, for every AGI × filing-status cell, and
  for the top AGI classes, and report its ratio to the same quantity
  under the prior weights. Report it also at the level of original PUF
  records when records have been copied, since copies inflate the count
  (report 04b).
- For scale: if every record in a cell had the same prior weight, the
  current linear program's split of 62 percent of records at 0.34 and 38
  percent at 1.66 alone would cut the cell's effective sample size to
  about 63 percent of the prior's. This is arithmetic on the brief's
  figures, not a measurement.
- **Top-weight share**: the share of a cell's weight carried by its ten
  heaviest records.

### Weight ratios against the prior

- Quantiles (1st, 10th, 50th, 90th, 99th, maximum) of w_i / d_i, overall
  and by cell.
- The share of records with ratios outside [0.25, 4] and outside
  [0.1, 10]. OTA's panel model caps factors at 0.1 and 10, and Populace
  caps weights at 50 times their start (report 07).
- Under a bounded distance, the share of records at a bound. Under the
  current linear program this is all but 114 records.

### Misses in standard-error units

- z_j = (ŷ_j − y_j) / σ_j for every target, listed, with the largest in
  absolute value at the top.
- **Mean square of z_j within each source group** (IRS counts, IRS
  amounts, BEA totals, Financial Accounts, projections). Report 06
  takes this from weather forecasting.
- Three versions are needed, because a fitted target's miss is pulled
  toward zero by the fit:
  - **before fitting** (prior against target): its mean square should
    exceed one, by how much the prior is off;
  - **after fitting**: its mean square should be below one;
  - **the cross-product check of Desroziers and co-authors (2005)**
    (report 06): the average of (y_j − ŷ_j after) × (y_j − ŷ_j before) /
    σ_j². Under a linear, normal approximation it should be close to one
    in each group when the stated σ_j are right. Well above one means σ_j
    are too small, or the group conflicts with the prior or with other
    targets; well below one means σ_j are too large.
- For held-out groups, the mean square of z_j is the cleanest single
  check (section 3).

### Multipliers

- For a soft target with normal errors, the miss equals −σ_j² λ_j (report
  06), so λ_j σ_j = −z_j. For soft targets the multiplier carries the
  same information as the standardized miss, and need not be reported
  twice.
- Multipliers carry separate information for **hard constraints** (σ = 0)
  and for **tolerance-band targets**, where the miss is capped. For those,
  report λ_j directly.
- **Which targets move the weights.** The log weight ratio of each record
  is Σ_j λ_j a_ij. Its weighted variance can be split by target. The
  share attributable to each target shows which targets bend the prior
  most. The Tax-Calculator team used constraint prices this way to find
  and drop targets that cost six to eight orders of magnitude more than
  the rest (report 07).
- Two targets with large multipliers of opposite sign on overlapping
  records signal a definitional conflict (report 04).

### How much the targets determine each output

- **Data-determined share** (report 04): for an output F = Σ_i w_i f_i,
  regress f_i on the target contributions a_i1 … a_iK, weighted, with each
  target discounted by its noise. The R-squared approximates the share of
  the prior's uncertainty about F that the targets remove. Revenue from a
  rate change by AGI bracket will have a share near one if AGI-bin totals
  are targeted; revenue from a tax on top wealth will be near zero.
  Compute it for every output in the downstream panel (section 6), using
  each record's tax change from Tax-Simulator's detail file as f_i.
- **Range consistent with the targets** (report 04b): the minimum and
  maximum of an output over all weightings that fit the targets within
  their stated errors. On fixed records each end is a linear program.
  It is not a probability interval. It shows how far an output could move
  without worsening the fit. Wide ranges flag outputs that depend on the
  prior.
- **Swap the prior** (report 04): rerun with an alternative prior (for
  example, a different imputation draw, or uniform aging instead of the
  new aging layer) and report the change in each output. The first
  measure says how much the targets constrain an output; this one says
  how much the unconstrained part matters.

Two limits from report 04: these measures cover uncertainty about weights,
not bias in the prior's values, and they rely on a linear approximation
that holds only when the targets move the file modestly.

### Bystanders

Variables no target touches (student loan interest, the AMT base, and
others) can drift when weights move. TMD's area-weighting notes report
untargeted items drifting by about 10 percent (report 07). List each
untargeted variable's change against the prior, and against any outside
total.

### Recomputation

Every target is recomputed from the file as written, after any rounding,
splitting or sampling, not taken from the solver's internal state (report
04b). `src/eda/lp_instrument.R` does this for the current linear program;
the new pipeline should do it on every run.

## 5. Imputation diagnostics

A separate paper covers imputation methods. The standard battery each
imputed variable should report:

1. **Marginal distribution against the donor survey**: weighted quantiles,
   the share with zero, and the top tail.
2. **Conditional distributions** within cells of the conditioning
   variables, as `src/eda/conditional_diagnostic.R` does for consumption
   (81 cells): quantile gaps or the largest gap between the two cumulative
   distributions, by cell.
3. **Hold-out predictive check on the donor survey.** Train on part of the
   survey, predict the rest, and score with the continuous ranked
   probability score, which measures the distance between the predicted
   and the observed cumulative distribution and rewards getting the whole
   distribution right, not only the mean. Also report the share of held-out
   observations falling below each predicted quantile (a probability
   integral transform histogram); a flat histogram means the predicted
   spread is right.
4. **Joint relationships**: rank correlations between the imputed variable
   and the main PUF variables, in the recipient file and in the donor.
   These rest on the conditional independence assumption, which no test
   on the two files can confirm (report 05); report them, and vary the
   conditioning set to see how much they move.
5. **Aggregate against an outside total**, with the concept bridge and
   its uncertainty stated (report 08).
6. **Support**: the share of recipients whose predictors fall outside the
   donor's range, especially at the top.
7. **Donor reuse**: the number of distinct donors used and the largest
   weight any one donor carries. The wealth top-tail lumpiness came from
   this (`MEMORY.md`).
8. **Seed stability**: refit with a new seed and report the change in the
   aggregate. Before the S31 fix, two rebuilds of one commit differed by
   9.5 percent in aggregate tips (CLAUDE.md).

## 6. Downstream tests through Tax-Simulator

### How a Tax-Data vintage reaches Tax-Simulator

**R Tax-Simulator** (`/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Simulator`):

- A runscript is a CSV of scenarios with eight columns: `ID`, `tax_law`,
  `economy`, `behavior`, `years`, `dist_years`, `mtr_vars`, `mtr_types`.
  The Tax-Data vintage is no longer a runscript column. It is set in
  `config/scenarios/economy/default/interfaces.yaml`
  (`tax_data_vintage: '2026070814'` at the time of reading), and a
  scenario reads another vintage by naming an economy alternative whose
  `interfaces.yaml` re-declares it.
- `slurm_run.sh <runscript> <scenario_id> <local> <vintage> <pct_sample>
  <stacked> <baseline_vintage> <delete_detail> [chains|batch]
  [years_per_task]` submits a chain of Slurm array jobs (baseline by
  year, counterfactual passes, aggregation, post-processing).
- Outputs per scenario and rung: `totals/1040.csv`,
  `totals/1040_by_agi.csv`, `totals/payroll.csv`, `totals/receipts.csv`,
  and `static/supplemental/` (distribution, horizontal equity, revenue
  deltas).
- **CBO comparison.** Commit `4d446a23d` (29 May 2026) added
  `cbo_comparison.csv`, a line-by-line comparison of the baseline 1040
  build-up with CBO's February 2026 "Individual Income Tax Details"
  (wages through taxable income, tax by bracket, credits, number of
  returns and itemizers, and rows labelled top 1, 5, 10, 25 and 50
  percent, whose definition I did not check), for 2022–2036. It is on
  branches `worktree-cbo-comparison` and `state-tax`, not on `main`.
  A September 2026 output tree (`weights_2022_agi`) contains the file;
  for 2022 it shows wages 0.9 percent and capital gains 7.2 percent
  below CBO.
- **Sample universe.** `config_parser.R` now takes the union of record
  ids across every Tax-Data year the run reads and joins random numbers
  by id. A vintage with a different record set still draws a fresh
  random stream, because the draws are generated in sequence over the
  whole universe; the first run of the non-filer validation measured the
  effect (section 1).

**tax-simulator-gpu** (`/nfs/roberts/project/pi_nrs36/jar335/Repositories/tax-simulator-gpu`):

- A vintage must first be converted by `jobs/bridge_convert.sbatch`
  (about 18 GB of CSV parsed into one Parquet file per year, with the
  wage split and the random draws, which are a deterministic function of
  household id, purpose and seed). The GPU model reads only the
  converted form (`docs/TAX_DATA_BRIDGE.md`).
- Runs are submitted with `jobs/runscript_run.sbatch` or `taxmodel-run
  <runscript> --slurm`. Every scenario in one run must read the same
  population, so old and new vintages need two runs.
  `jobs/compare_run_outputs.py` compares every published table of two
  runs, scaling each difference by the largest value in its column.
- Measured before the September 2026 capital-gains rebuild: a
  ten-scenario, thirty-year run took 209–221 seconds from an empty
  compile cache and about 100 seconds with a warm one, on one H200
  (`docs/PERFORMANCE.md`, which says to re-measure before relying on it).
- The distribution table ranks the population into 10,000 equal-weight
  groups and reports quintiles, the top 10, 5, 1, 0.1 and 0.01 percent,
  and a negative-income group, on four rankings (pretax cash income,
  accrual income, AGI, net worth), by age and parent status, for each tax
  separately and together (`src/taxmodel/reporting/distribution.py`).

**Calibrations pinned to vintages.** In the R model, any `calibrated`
value whose `derived_under` data vintages differ from the run's stops the
run, unless re-derived, overridden, or given a dated waiver. The estate
valuation bridge (fitted to SOI estate filings) and the capital-gains
realization parameters are among these. The GPU model stops on a stale
capital-gains calibration unless run with `--accept-stale-calibration`.
For a data comparison this matters in two ways:

- **Calibrations held fixed** (waived): differences reflect the data
  alone. This is the primary comparison.
- **Calibrations re-fitted to the new vintage**: differences are what
  production would publish. Outputs that the calibration pins to an
  outside number (estate tax against SOI and JCT; capital-gains moments)
  stop being independent checks in this arm, for the same reason given
  in section 3.

### The fixed panel

Run on the GPU model, static rung (no behavioural response) as the
primary data test, conventional rung second. Years 2017–2035 where the
law files allow; the GPU law files carry values from 2017, but I did not
check that historical-year runs reproduce SOI.

**Baseline levels, by year**

- Individual income tax liability, payroll tax, estate tax.
- The 1040 build-up line by line (`totals/1040.csv`), against CBO's
  Individual Income Tax Details for 2022–2036 and against Pub 1304 Table
  3.3 and Pub 4801 for 2017–2023.
- Counts: returns, itemizers, AMT payers, returns claiming CTC and EITC.
- Effective tax rates by income percentile (the distribution table's level
  columns), and the share of income tax paid by the top 1, 5, 10 and 50
  percent.

**Reform scores, ten-year totals and by year.** Existing scenario folders
in tax-simulator-gpu cover each requested type:

| Reform | Scenario folder (under `config/scenarios/tax_law/alternatives/`) | Main data dependence |
|---|---|---|
| Top rate to 39.6 percent | `tests/top_rate_396` | Ordinary income above the top threshold |
| Clinton-era rates | `tests/clinton_rates` | Taxable income across brackets |
| SALT cap change | `public/obbba/retrospective_2026/stack_vs_2024/04-salt` (2025 law's cap change, stacked on the pre-2025 baseline) | Underlying SALT by income, itemization |
| CTC expansion | `public/ctc/wyden_smith` | Children, earnings at the bottom |
| Capital gains rate | `tests/kg_5pp_all`, `tests/kg_5pp_top` | Realized gains by income |
| Estate tax | `public/estate_5m_45`, `public/estate_2009` | Wealth, mortality, top tail |
| Mortgage interest repeal | `tests/mort_int_repeal` | Underlying mortgage interest |
| 2025 law, all parts stacked | `public/obbba/retrospective_2026/stack_vs_2024/01`–`08` (rates, AMT, deductions, SALT, family, QBI, new deductions for tips, overtime and others, estate) | Nearly every variable, including imputed tips, overtime and auto-loan interest |
| TCJA extension | `baseline_2024_tcja_ext` against `baseline_2024` | Broad |

For each score: the total, the distribution of the change (share of the
change by quintile and top groups, average change, share with a cut or
increase), and the data-determined share (section 4).

### Outside estimates

These serve as reference points. They are not truth: each scorer uses
different data, income concepts and units.

- **CBO's Individual Income Tax Details**, February 2026, as stored in
  Tax-Simulator's `resources/cbo/cbo_iit_detail_feb2026.csv` on the side
  branch (commit message: CBO publication 61882). Line items 2022–2036.
  2022 is an actual year in CBO's file, which makes it a check on levels.
- **JCT** estimates of the 2025 law and its parts, and of TCJA extension.
  Tax-simulator-gpu's top-tax methodology memo says its estate
  calibration used "the JCT estimate of the 2025 reconciliation act's
  estate provisions", so at least that number has been collected once.
  I could not open jct.gov (blocked) and did not verify any JCT document
  number.
- **Treasury OTA distribution tables.** The OTA page lists "Distribution
  of Tax Burden, Current Law" and "Distribution of Income by Source",
  with a 2025 file and, per the page, history back to 2015. I confirmed
  the links exist; I could not render the PDF and did not check the
  income concept or groups.
- **TPC baseline distribution tables and reform scores.** Report 07
  covers TPC's model; its site blocked me, so I did not verify specific
  table series.
- **CRS R49052** (30 July 2026) compares Tax-Simulator with Tax-Calculator
  and reports both about 9 percent below CBO on income tax (report 07,
  read through a summary). It is a published outside view of the current
  file.

Compare shapes (shares by group, ratios across reforms) before levels,
since income concepts differ.

### How large a difference is meaningful

Three noise sources, each measured, not assumed:

1. **Tax-Data seed noise.** Rebuild the same candidate with five
   different master seeds and run the panel on each. The standard
   deviation of each output across seeds is its seed noise. A first
   reading exists: in the 2026-09-12 run, before draws were keyed to
   record id, shifted draws moved filer aggregates of imputed variables
   by up to 3.9 percent (sole-proprietor wage bill), 2.2 percent
   (childcare), 1.3 percent (tips, auto-loan interest), and income tax
   revenue by 0.078 percent
   (`research/state_weights/nonfiler_federal_validation_findings.md`).
   The S23 and S31 fixes should shrink this, which is itself worth
   confirming.
2. **Sampling noise.** Split PUF records into, say, 20 random groups
   within weight classes. For each group, drop it, refit the calibration
   on the rest, and rerun the panel. The spread of the 20 results gives a
   jackknife standard error: √((G−1)/G × Σ_g (θ_g − θ̄)²) for G groups.
   Apply the same dropped group to old and new pipelines and take the
   jackknife of the difference: because both files share the PUF
   records, much sampling noise cancels in the difference, and the
   difference is what is being judged. On the GPU model, 20 replicates of
   the panel take hours. Re-fitting the old pipeline 20 times is heavier
   (about two hours and 141 GB each), so in the first round jackknife
   only the new method's weighting step.
3. **Tax-Simulator's own randomness** (take-up and similar draws). Hold
   the draws fixed by id, as both models now do, so this drops out of the
   old-versus-new difference.

Rule: a change in an output counts as real if it exceeds twice its noise
standard error **and** a materiality threshold. Suggested thresholds, for
John to set: 0.5 percent of a year's baseline income tax; 2 percent of a
ten-year reform score; 0.2 percentage points of an effective tax rate for
a group; 1 percentage point of a group's share of a tax change. These are
judgment defaults, not derived numbers.

### Separating improvement from change

1. **Predict before running.** For each staged change, write down which
   panel outputs should move, in which direction, and roughly by how
   much, as the non-filer procedure does. Movement outside the
   prediction is investigated before it is accepted.
2. **Stage the vintages.** For example: V0 the current file; V1 the new
   weighting on the 2017 targets only; V2 plus later-year targets; V3 plus
   the value-changing aging layer; V4 plus new imputations. Each step's
   difference is attributed to that step.
3. **Claim improvement only against something true.** The candidates are
   held-out SOI tables; backtested years; Tax-Simulator's tax liability by
   AGI under actual law for 2017–2023 against Table 3.3; and CBO's actual
   2022 line items. If old and new files are run through the same
   calculator, the calculator's own errors are common to both (apart from
   interactions), so the difference in fit is attributable to the data.
4. **For forward scores, trace the cause.** Decompose each reform score's
   old-to-new difference by AGI class and by the input variables that
   drive it (each record's tax change times its weight change, plus its
   value change). Then ask whether those inputs moved toward or away from
   the held-out evidence. If the SALT score moved because underlying SALT
   among high earners rose, and Table 2.1's pre-cap SALT for itemizers
   (held out) is now matched better, that is evidence of improvement. If
   the inputs that moved have a low data-determined share, the change
   comes from prior assumptions and should be labelled as such.
5. **Use outside estimates as a tiebreaker, not a target.** Moving toward
   JCT on a score whose inputs also moved toward SOI is supporting
   evidence. Moving toward JCT for no traceable reason is not.

## 7. Simulation tests with a known answer

### Solver tests

These test the computation alone.

- **Exact recovery.** Choose multipliers λ*, set w*_i = d_i exp(Σ_j λ*_j
  a_ij), compute y_j = Σ_i w*_i a_ij exactly, and give the solver y with
  tiny σ. It must return λ* and w* to numerical tolerance.
- **Limits.** With very large σ the solution must equal the prior; with
  σ → 0 every target must be hit; hard constraints must hold exactly in
  every case.
- **Small-case agreement.** On a problem with a few hundred records,
  compare the GPU solver with a general convex solver.
- **Censored targets.** Check the itemization-switch target against a
  direct record-by-record computation under the year's law.

### Method tests

These test identification: what the method can and cannot recover when
the data are like SOI's.

**Building the truth.** Split the 2015 PUF records into two halves at
random within weight classes. Half A becomes the "true" population; half B
supplies the prior. Disjoint halves matter: if the prior contained the
truth's own records, reweighting could recover the truth exactly and
overstate identification. Then transform half A into a known "2019"
population. Each transformation tests one thing:

| Scenario | Change applied to the truth | What the method should do |
|---|---|---|
| S1 composition | Weights shifted by age, filing status and AGI | Recover all margins; recover untargeted items to the extent they correlate with targeted ones |
| S2 shape | Wage growth rising with wage rank; capital gains up sharply at the top | Weights alone cannot fully recover the within-cell shape; a value-changing layer should |
| S3 censoring | Underlying deductible expenses grow at different known rates above and below the itemization threshold; only censored totals published under 2019 law | Recover above the threshold; below it, return the prior. The data-determined share for below-threshold deductions should be near zero, and the diagnostics must say so |
| S4 missing support | Add truth records of a type absent from half B (for example, young filers with very large gains) | Fail visibly: large multipliers, collapsing effective sample size in that cell |
| S5 conflicting target | Add a BEA-style total 20 percent off the truth's concept | The cross-product check and standardized misses should flag that group |
| S6 wrong precision | State σ ten times too small, then ten times too large | The cross-product check should move well away from one |

**Generating the targets.** From each truth, compute every table in the
target set under 2019 law, including itemizer-only Table 2.1 items. Add
normal noise with standard deviation equal to the published value times
SOI's coefficient of variation for that cell (Table 1.4CV). Suppress and
merge cells the way SOI does. Draw the noise 20 times.

**The wrong prior.** Half B aged by uniform growth: one factor per
variable and demographic weight growth, the current method's assumptions.

**Scoring.**

- Targeted quantities: fit, against the noise added.
- Untargeted quantities: the gap between recovered and true, as a
  fraction of the gap between prior and true. Zero means fully recovered;
  one means no better than the prior.
- Joint relationships: rank correlations (wages with capital gains,
  income with deductions) in truth, prior and result.
- Tax-Simulator panel on truth, prior and result: how much of the
  prior's error in each reform score the method removes.
- The diagnostics themselves: whether the data-determined share,
  multipliers and cross-product check flag the cases where the method
  fails (S3–S6). A diagnostic that stays quiet in S4 is not useful.
- An oracle run, half B calibrated to the truth's exact targets without
  noise, separates the effect of noisy targets from the method's limits.

Talts and co-authors' simulation-based calibration is the formal version
of this practice for Bayesian computation (sources). Full uncertainty
calibration is not needed here, because the decision is a single best
file.

## 8. The scorecard

Produced automatically by every run, as a CSV and a one-page rendered
table, with the current production vintage's value in a second column
wherever the metric applies to it. Thresholds are starting points for
John to revise.

| # | Number | What it means | Red flag |
|---|---|---|---|
| **Integrity** | | | |
| 1 | Identities: split sums (wages1 + wages2 = wages and analogues), population partition, nonnegativity | Exact constraints hold | Any failure (hard stop) |
| 2 | NA count by column and year, with cause | No silent missing values | Any unexpected NA |
| 3 | Same seed twice: byte-identical output | Reproducible | Any difference |
| 4 | Every target recomputed from the written file | Solver and file agree | Any target off by more than 0.1σ from the solver's own value |
| **Fit** | | | |
| 5 | Mean square of standardized misses, after fit, by source group | Fit relative to stated precision | Above 1 in any group |
| 6 | Cross-product check by source group | Stated σ are about right | Outside 0.5–2 |
| 7 | Five largest \|z\| with target names | Where the fit strains | Any \|z\| above 3 for an IRS target |
| 8 | Mean square of standardized misses, held-out groups | Out-of-sample fit | Above 4, or worse than the current file |
| 9 | Five targets with the largest share of log-weight movement; multipliers on hard constraints | Which targets bend the prior | One target above a third of all movement |
| **Weights** | | | |
| 10 | Kish effective sample size, whole file and smallest AGI × status cell, as a ratio to the prior | Information lost to reweighting | Below 0.5 overall, or below 0.2 in any cell |
| 11 | Weight-ratio quantiles (1st, 99th, maximum) and share outside [0.25, 4] | How far weights moved | Maximum above 10, or more than 5 percent outside |
| 12 | Top-ten records' share of weight in each top AGI class | Fragility at the top | Above 25 percent |
| **Backtest** (when run) | | | |
| 13 | Relative skill against the current method and uniform growth, by horizon, law-invariant items | Projection accuracy | Above 1 at any horizon |
| 14 | Mean information gain across AGI classes, by item and horizon | Shape accuracy | Worse than persistence of shares |
| **Downstream** | | | |
| 15 | Income tax liability by AGI against Table 3.3, 2017–2023 | Level check through the calculator | Worse than the current file in more AGI classes than better |
| 16 | 1040 line items against CBO 2022 actual and 2023–2035 | Level check against CBO | Any major line moving away from CBO beyond the noise band without explanation |
| 17 | Panel reform scores: old, new, difference, noise band, data-determined share, outside estimate | The comparison that matters | A difference beyond the noise band that was not predicted |
| 18 | Effective tax rate by income group, old and new | Distributional level | Change above 0.2 points in any group without explanation |
| **Bystanders** | | | |
| 19 | Change in every untargeted variable against the prior | Unintended drift | Above 5 percent with no target reason |

## 9. Build order

### Inside the two-week window

Ordered so that each step uses the previous one and the first numbers
arrive early.

1. **Days 1–2. Target table with precision and holdout columns.** Extend
   the target configuration with a standard error (or relative precision
   by source), a source-group label, and a holdout flag. Mark Table 3.3,
   2.5, 3.4, 3.6 and 4.3 as permanent holdouts.
2. **Days 1–3. Scorecard skeleton with integrity and calibration
   diagnostics.** Rows 1–12 and 19 come from the solver and the written
   file. Compute the same rows for the current production vintage, so
   every later number has a comparison. Reuse `compare_vintages.R` and
   `lp_instrument.R` logic.
3. **Day 3. Solver tests with a known answer** (section 7, solver part).
   Small, and catches bugs before any data result is read.
4. **Days 4–6. Downstream harness on the GPU model.** Bridge conversion
   of a candidate vintage, one panel runscript, waivers for pinned
   calibrations, `compare_run_outputs.py` plus a short script producing
   rows 15–18. Port the CBO comparison (side branch, R) or apply it to
   GPU outputs.
5. **Days 6–9. First backtest: cutoff 2017, realized aggregates**, scored
   on 2018–2019 against Tables 1.4, 1.6 and 2.1, with the current method
   and uniform growth as benchmarks. This tests the TCJA reporting
   simulation directly. Most inputs are in `raw_data/IRS-Ind`.
6. **Days 8–10. Seed noise.** Five seeds of the candidate through the
   panel. This sets the first noise bands for row 17.
7. **Days 10–12. Simulation study, reduced scope**: scenarios S1, S3 and
   S4, a few noise draws.

### Later

- Rolling backtests at cutoffs 2015, 2019 and 2021, with the pandemic
  block reported separately; the real-time variant with CBO's June 2017
  projections.
- Full rotation of held-out groups, used to choose the prior's strength.
- Jackknife sampling noise for old and new pipelines, and for the
  difference.
- Ranges consistent with the targets for the panel outputs.
- The full simulation study (S1–S6, 20 noise draws) and a check of the
  `Saez-Zucman-PUF` files for a long-history aging backtest.
- A digitized library of outside estimates (JCT, Treasury, TPC, CBO
  distributional tables) for the panel reforms, with concepts recorded.
- The imputation battery (section 5) applied uniformly to every module.
- The 2016 synthetic PUF as an alternative prior, if released.

## Sources

### Repository files read (verified)

- Tax-Data: `src/main.R`, `src/summary.R`, `src/create_2017_puf.R`
  (lines 55–80), `src/project_puf.R` (partition check), `src/materialize.R`
  (assertions), `src/tests/*.R` headers, `src/eda/` listing and headers of
  16 scripts, `config/target_info/baseline.csv`,
  `research/state_weights/nonfiler_federal_validation.md`,
  `research/state_weights/nonfiler_federal_validation_findings.md`,
  `research/CONVENTIONS.md`.
- Tax-Simulator: `slurm_run.sh`, `CLAUDE.md` (runscripts, calibration
  staleness, estate bridge, output files),
  `config/scenarios/economy/default/interfaces.yaml`,
  `config/interfaces/interface_versions.yaml`, `src/misc/config_parser.R`
  (sample universe and random numbers), commit `4d446a23d` and its CBO
  reference file, and the output tree
  `/nfs/roberts/project/pi_nrs36/shared/model_data/Tax-Simulator/v1/weights_2022_agi/`.
- tax-simulator-gpu: `README.md`, `ARCHITECTURE.md` (outputs, testing),
  `docs/PERFORMANCE.md`, `docs/TAX_DATA_BRIDGE.md`,
  `jobs/compare_run_outputs.py`, `jobs/bridge_convert.sbatch`,
  `src/taxmodel/reporting/distribution.py`,
  `src/taxmodel/calibration/staleness.py`, the scenario folders listed in
  section 6, `projects/top_tax/methodology_memo.md` (one passage).
- Shared data listings: `raw_data/IRS-PUF`, `raw_data/Saez-Zucman-PUF`
  (file names and sizes only), `raw_data/CBO-Econ-Proj/v3`,
  `raw_data/CBO-Budget-Proj/v3`.

### Background papers relied on

`research/rebuild/background/01_data_irs.md` (table inventory, release
dates, coefficients of variation), `03_data_microdata.md`,
`04_methods_first_principles.md` (data-determined share, failure modes),
`04b_methods_first_principles_codex.md` (effective sample size at donor
level, ranges consistent with targets, recomputation),
`05_lit_survey_statistics.md` (Kish effective sample size),
`06_lit_ml_bayes.md` (standardized misses, Desroziers et al. 2005,
multipliers), `07_practice_tax_models.md` (OTA Paper 62, Populace, TMD,
CRS R49052), `08_practice_other_fields.md` (Realtime Inequality backtest,
HMRC error table, DFA wave drop).

### Outside sources

- Talts, Betancourt, Simpson, Vehtari and Gelman, "Validating Bayesian
  Inference Algorithms with Simulation-Based Calibration", 2018,
  https://arxiv.org/abs/1804.06788 (verified: abstract page).
- U.S. Treasury Office of Tax Analysis page,
  https://home.treasury.gov/policy-issues/tax-policy/office-of-tax-analysis
  (verified that it links "Distribution of Tax Burden, Current Law" and
  "Distribution of Income by Source", 2025 files, history said to run
  back to 2015); the PDF
  https://home.treasury.gov/system/files/131/Distribution-of-Tax-Burden-Current-Law-2025.pdf
  downloaded but I could not render it, so its contents are not verified.
- OTA Paper 62, https://home.treasury.gov/system/files/131/WP-62.pdf
  (verified by report 07, not by me).
- Desroziers, Berre, Chapnik and Poli (2005),
  https://doi.org/10.1256/qj.05.108 (cited by report 06; not opened by me).
- Gneiting and Raftery (2007), "Strictly Proper Scoring Rules, Prediction,
  and Estimation", *Journal of the American Statistical Association*, the
  standard reference for the continuous ranked probability score. Not
  verified: the DOI I tried
  (https://doi.org/10.1198/016214506000000437) returned "not found".

### What I looked for and could not find or open

- JCT publications page (https://www.jct.gov/publications/) and TPC's
  model estimates page (https://taxpolicycenter.org/model-estimates):
  both returned "forbidden". I did not verify any JCT document number
  for the 2025 law, TCJA extension or the 2024 CTC bill, or any TPC
  table series.
- CBO pages: blocked for report 08 as well; the CBO income tax detail
  used here is the copy committed in Tax-Simulator.
- Whether the `Saez-Zucman-PUF` files are the Piketty–Saez–Zucman
  microfiles and which variables they hold.
- Whether Tax-Simulator's historical-year runs (2017–2020) reproduce
  SOI tax liability; no comparison was found in either simulator.
- Any older SOI PUF (2012–2014) on the cluster; only the 2015 file was
  found.
