# Reweighting targets

This file is the reference for the targets of the reweighting, the stage that sets each year's weights. It says what
a target is, where each target's value and standard error come from, how the weights are solved, how a year's targets
are written down, and what every run reports. The target families and the measurement rule of each are listed in
`FEATURES.md`, "Reweighting".

## The stage in outline

For each year t, `reweight/tilt.py`:

1. Builds the start: tax units, filers and non-filers, with starting weights and every dollar amount in nominal
   dollars of year t. Past years (2015-2023) start from a fresh draw from the trained networks; projection years
   (2024-2075) from an earlier year's finished file grown to year t (`scale/start.py`, `scale/projection_steps.py`).
2. Scales the starting weights to year t's population (the prescaling).
3. Adds the columns that need no new microdata (`scale/non_micro_imputations.py`) and computes every record's taxes
   under year t's law with tax-simulator-gpu (`reweight/taxes.py`).
4. Builds year t's targets, each a value with a standard error (`reweight/target_list.py`, `reweight/errors.py`,
   `reweight/scf_targets.py`), and checks them (`reweight/precheck.py`).
5. Moves the weights to minimize one cost: the misses on the targets, each measured in its own standard error, plus
   the size of the move away from the starting weights (`reweight/solver.py`).

Records keep their values; only the weights change. In past years the installed weights come from the integrated
solve of national and state weights (`reweight/state_joint.py`). That solve uses this stage's national targets and
the same cost, with the state targets added (`FEATURES.md`, "State weights").

## What a target is

A target is one statistic of the year-t population that the file should reproduce, together with a standard error
saying how well the statistic is known. It has five parts: the statistic; the population and the cell within it (an
AGI group, an age range, a filing status); the variable, either a column of the file or a column the tax calculator
computes from the record under year t's law; the value; and the standard error, in the units of the statistic.

The statistics:

- `units`: the weighted number of units in the cell (for filers, the number of returns).
- `count`: the weighted number of units in the cell with a nonzero value of the variable (a positive value for the
  Table 2.1 itemized deduction categories, which count returns claiming the item).
- `amount`: the weighted total of the variable in the cell, in dollars or, for a people variable, in people.
- `average`: the weighted mean of the variable over the cell's units (CEX consumption, CPS overtime, SCF auto loan
  interest). Its standard error is on the average itself.
- `share`: the share of the population's units that fall in the cell (SCF units above a net worth line).
- `workers`, `worker share`: W-2 statistics counted per worker rather than per return.

The populations, defined in `targets_config/catalog.yaml`, are `filers`, `non_filers`, `all_units`,
`dependent_filers`, and three more:

- `units_other_than_dependent_filers`: the surveys count a dependent filer inside the unit that claims him, so every
  survey target leaves dependent filers out;
- `independent_filers`: returns other than dependent filers' (Table 4.3's population, the W-2 targets);
- `scf_units_below_a_billion`: units other than dependent filers with net worth below $1 billion, in the SCF and in the
  file; the Forbes list supplies the units above.

A target that is exact by definition is an identity: its standard error is zero and it is met exactly. The identities
are the non-filer count in years with CPS data, the person and return counts of the projection years, and the joint
and non-joint return totals inside the prescaling.

### Rules a target follows

- The file must be able to compute the same statistic, for the same population, under the same definition as the
  source. A survey value is computed on the survey's own tax units, built by the shared tax-unit rules
  (`TAX_UNITS.md`), so the survey and the file count units the same way. Each survey row of the target list names the
  tax-unit rules its value was computed on.
- A small remaining gap between the source's definition and the file's is added to the standard error. A large or
  unknown gap means the number is reported as a comparison, not targeted.
- A target exists only where there is information about year t. Where nothing is known, the start's value is already
  the best guess.
- Outside information that comes only as a change is applied to the file's own level. The target is the start file's
  own total times the outside growth, never the outside level. Example: the 2026 AMT target is the 2024 file's number
  of returns paying AMT times CBO's growth in that number from 2024 to 2026. The gap between the outside concept and
  the file's is assumed to stay fixed.
- Dependent filers are real returns and are in every SOI count. They carry PUF items only. Where an SOI table splits
  by something they lack, they are placed by what the PUF has. In Table 1.6 the PUF gives only under 26 or 26 and over;
  those 26 and over are spread over the ranges 26-34 to 65+ in proportion to the entry's own published values, within
  the same filing status or AGI group (the entry's `dependent_filers` key). Table 1.6's under-26 range is not targeted.
- Each year has one file. Target values are not drawn from their errors to make several versions.

### Cells left out

When the list is expanded, a cell published as zero is left out, and so is a cell that no record with a positive
starting weight can reach. Both are written to `targets_excluded.csv`. Cells an entry lists under `must_reach` (Table
4.3's top 0.001%) must be reachable; the run stops if one is not.

## Where values come from

Each entry's `value` names one of four kinds of source, and the source must be defined in the catalog.

- `published`: SOI Tables 1.2, 1.3, 1.4, 1.6, 1.7, 2.1, 2.5, 3.3 and 4.3, and the W-2 statistics Tables 4.B
  (`soi w2`), 4.C (`soi w2 pctiles`) and 2.D (`soi w2 2.d`). The value is the published cell, or the sum of the
  published cells a target's group spans. `prep/soi.py` reads Tables 1.2, 1.4 and 3.3 into the run's
  `targets/soi.csv`; the others are read from the SOI mirror, and Table 2.1 from
  `research/rebuild/resources/table21_2015_2023.csv`.
- `survey`: `scf`, `cex`, `cps`, `cps units`. The same statistic on the survey's own tax units, over the year's mix of
  survey years.
- `pipeline`: `filing_stage`, `tax_data_nonfilers`. Computed by an earlier stage of the same run.
- `outside`: `projection persons`, `projection returns`, `projection items`, `projection filing season`,
  `cbo counts`, `z1 household net worth`. The start file's own level times an outside growth (projection years).

**Calculator targets.** Entries of kind `calculator` have published values (SOI Tables 1.4, 2.1, 2.5, 3.3), but the
file's side is a column the tax calculator computes (`calc_*`): itemizers and each itemized deduction, taxable income,
income tax before credits, the QBI deduction, the child tax credits, the education credits, the saver's credit, the
EITC, self-employment tax and the AMT. Reweighting does not change records, so the calculator runs once per year, on
the start after the prescaling, and the solve only adds up its outputs. An entry marked `needs_calculator` (the CBO
count of AMT returns) waits for the taxes in the same way.

**Non-filer values.** From the CPS filing stage:

- The non-filer count is the CPS's expected non-filers: the sum over the CPS units of weight times one minus the
  chance of filing. The generated non-filer batch is sized by the same count.
- The people in non-filer units are counted by band: children under 17, and heads and spouses aged 18-25, 26-34,
  35-44, 45-54, 55-64, 65-74 and 75 and over. Each band's value is the sum over CPS units of weight times one minus the
  chance of filing times the unit's people in the band, counted by the rule the file's records are counted by.
- Non-filers' dividends (amount and holders) come from the non-filers of the year's tax data: CPS units weighted by
  their chance of not filing, amounts in PUF concepts, before the training's fill-in.
- The CPS years are the odd years 2015-2023. An even year takes the geometric mean of the years either side. A year
  after 2023 takes 2023's value grown by the Social Security area population.

### The year's mix of survey years

A survey target is computed on the same survey years, with the same shares, that the file draws that survey's items
from. The shares are written by hand in the year's `prior.yaml` (entries `balance sheet` for the SCF, `consumption`
for the CEX, `cps items` for the CPS, `sipp tips` for the SIPP); the code reads them and does not compute them. They
follow two rules.

- **SCF.** Each wave is dated by its income year (the 2019 wave is income year 2018). For target year T, take the
  first wave with income year at or after T, if there is one, and every wave before T. Weight each by
  exp(-|T - t| / 2), with t the wave's income year, drop any wave under 5% and rescale the rest to sum to one. After
  the last wave the mix stays fixed. 2019: the 2019 wave 54.65%, the 2022 wave 33.15%, the 2016 wave 12.20%. 2024
  and later: the 2025 wave 81.76%, the 2022 wave 18.24%.
- **CEX, CPS and SIPP.** Their years are the odd tax years 2015-2023. For target year T, take the two years nearest T
  and weight each by exp(-|T - t| / 2), rescaled to sum to one. An odd year takes its own year at 73.11% and the
  neighbouring odd year at 26.89% (2019: 2019 and 2017; 2015: 2015 and 2017). An even year takes the years either side
  at 50% each. After 2023 the mix stays at 2023 (73.11%) and 2021 (26.89%).

### SCF targets

`reweight/scf_targets.py` computes every SCF target (kinds `wealth`, `wealth grid`, `auto loans`) wave by wave and
then pools the waves.

1. A wave's units are its rows of the training table, all five implicates.
2. Its balance sheet is grown per tax unit to the target year's wave, item by item, by the same factors the dollar
   maps use. The home mortgage moves with the home. Net worth is the grown assets less the grown debts. So the targets
   and the file share one dollar concept.
3. Its weights are scaled so that its units add up to the target year's wave's number of units. Dollars per unit
   carry over; the file's number of units is set by SOI's returns and the non-filer count.
4. Its lines are its own: the net worth lines at its own weighted percentiles, and the income rank groups at its own
   weighted percentiles of the seven income items.
5. Pooled value = the sum over waves of share times the wave's value. The waves are independent samples, so pooled
   variance = the sum over waves of share squared times the wave's variance.

The file is given one dollar line per net worth percentile, the share-weighted average of the waves' lines. The
file's income rank groups are set by the file's own ranks on its starting weights after the prescaling. In
projection years the SCF targets are grown on to the year by the factors the start's records get
(`scale/projection_steps.scf_factor`). `scf_targets_by_wave.csv` records each target's value, standard error, lines
and sample households in each wave.

### Table-specific measurement rules

- Table 4.3's groups are the returns with AGI at or above SOI's published cutoff for the group and year, which does
  not move in the solve. A year with no published cutoff for a targeted group stops the run. Net losses are read as
  sizes, since SOI prints them with either sign.
- Table 1.6 by AGI uses the 15 standard AGI groups. SOI's 2018 workbook prints the 26-34 row's $40,000-50,000 and
  $50,000-75,000 cells 2.21 million returns too high; in 2018 those two groups are one, $40,000-75,000, for every age,
  and the 26-34 cell is the row's total less its other columns. Any other row that does not add up stops the run.
- Table 1.3's capital loss is the loss allowed on the return (at most $3,000, $1,500 if married filing separately).
- Table 2.1's taxes line is state and local taxes as limited, not total taxes paid. Its under-$25,000 and
  $25,000-50,000 groups are one group, under $50,000.
- Cells with few returns are folded into coarser groups, defined as catalog groupings: the nonrefundable child credit
  under $20,000 of AGI; the additional child tax credit at $100,000 and over; the AMT under $500,000; the EITC at
  $40,000 and over; farm and farm rental income as one overall cell; married filing separately in Table 1.3 by returns
  and AGI only; $1-1.5 million and $1.5-2 million of AGI as one group, $1-2 million.

## Standard errors

Standard errors matter where targets conflict: they decide which target gives way. They need to be roughly right.
Every target has one of five error kinds, set by `reweight/errors.py` and, for the table-specific rules, by the value
readers in `reweight/target_list.py`. A definition gap, where one is set, is added to the sampling error as the square
root of the sum of squares.

### SOI tables with published coefficients of variation (`soi_cv`)

Tables 1.4, 2.1 and 3.3. SOI publishes their coefficients of variation (CV) in workbooks at
`https://www.irs.gov/pub/irs-soi/{yy}in14cv.xls`, `{yy}in21cv.xls` and `{yy}in33cv.xls`, which no SOI page links.
The stage `soi_cv` (`prep/soi_cv.py`) parses them into `<out>/targets/soi_cv/soi_cv.csv`, joining each CV to its cell
by tax year, panel, AGI class and column. A year with no workbook uses the latest earlier year's CVs, and the reason
says so.

- A target that sums several published cells gets the standard error of the sum, the cells taken as independent.
- A cell SOI blanked and folded into a neighbour for disclosure has no CV of its own; the receiving cell's CV is taken
  as the CV of the combined estimate.
- SOI samples every return at $10 million of AGI and over, and prints a CV of 0 (or 0.01 in 2022). A CV below 0.1% is
  treated as 0. A target resting only on such cells gets a judgment error of 1% of its value, standing for definition
  gaps.
- Table 2.1: SOI's grouped CVs, with the published groups that form one of ours added in quadrature. A missing CV is
  borrowed from the same category and AGI group in the nearest year. The residual "other" deductions have no CV and
  get 10% of the value.

### Other SOI tables from the same sample (`soi_borrowed`)

Tables 1.2, 1.3, 1.6, 1.7, 2.5, 4.3 and the W-2 tables publish no CVs. Each cell borrows a CV from the Table 1.4 cells
of the same year: the median CV of the five closest cells. A count borrows from Table 1.4's count cells closest in
number of returns. An amount borrows from the amount cells closest in amount and in number of returns (distance in
logs). Cells sampled in full and cells without a CV are not lent.

- Table 1.6 and Table 1.2 by AGI group borrow only from Table 1.4's count cells in the same AGI group, because SOI's
  sampling rate rises with income. A group SOI samples in full ($10 million and over) gets 1% of the value.
- Table 4.3: a group whose cutoff is at or above $10 million (in every year the top 0.01% and 0.001%) gets 1% of the
  value. Below that (the top 0.1% and 1%), a count borrows from Table 1.4's count cells in the AGI classes reaching
  above the cutoff; an amount borrows from the whole table.
- Table 1.7 amounts borrow by amount and by the cell's own count of returns with the item.
- W-2 counts and amounts borrow as above. Where the value was reduced for dependent filers, a share of the reduction
  (the catalog's `gap_share`) is added as a definition gap.
- W-2 worker shares above a tips percentile line: the borrowed CVs of the count above the line and of the number of
  workers, in quadrature, times the share; plus half the most the dependent filers could move the share, in
  quadrature.

### Survey targets (`survey_replicate`)

The survey's own replicate weights, carried to the tax units as the main weight is (a unit's replicate weight is its
household's or interview's).

- **SCF**, within each wave: replicate weights WT1B times MM (the Federal Reserve's method), on the first implicate.
  Sampling variance = the sum over the 999 replicates of (estimate minus the mean of the replicate estimates) squared,
  divided by 998. Imputation variance = the variance of the five implicates' estimates. Total = sampling + 1.2 times
  imputation. The percentile lines are held at their full-sample values. The waves pool as above.
- **CEX**: balanced repeated replication with the 44 replicate weights of the FMLI files. Variance = the sum over
  replicates of (replicate estimate minus full-sample estimate) squared, divided by 44.
- **CPS** (overtime, and the counts of units by the primary's age in kind `older ages`): successive difference
  replication with the ASEC's 160 person replicate weights, the unit head's. Variance = 4/160 times the sum of squared
  deviations. Years in the mix pool as the SCF's waves do.

The SCF replicate weights are downloaded once from the Federal Reserve (`scf<year>rw1s.zip`) into
`<out>/data/scf_rw/<wave>/`; every wave in a year's mix needs its folder. `reweight/errors.py` gives the download
commands and checksums.

### Judgment (`judgment`)

A number written in the year's `targets.yaml` as `error: {kind: judgment, relative: <share>, reason: <text>}`: a
relative standard error and a one-line reason. It covers outside forecasts, other models' outputs and definition gaps.

| Target | Standard error |
|---|---|
| People in non-filer units, by band | 0.5% of the band's CPS population (`NONFILER_PEOPLE_SE` in `reweight/errors.py`) |
| Non-filer count, even years | 2% |
| Non-filers' dividends: amount, holders | 50%, 25% |
| Cells SOI samples in full | 1% |
| Table 2.1 residual "other" deductions | 10% |
| Projection item totals | each item's error in `projection_items.csv` (1% today) |
| Projection filing-season targets | `filing_season_errors.csv`, at least 1% |
| AMT count, projection years | 5% |
| Household net worth (Financial Accounts), projection years | 2% |

### Identities (`identity`)

Standard error zero; met exactly.

## Importance

Each target's miss is multiplied by an importance in the cost. Importance is 1 for SOI's published figures, the W-2
statistics and the calculator's columns against SOI's, and 1/1.5 (two thirds) for every other kind (settings
`tilt.reweighting.importance_one_kinds` and `other_importance_divisor`). `importance.csv` lists each target's value.

- Importance 1: `IRS`, `business`, `soi 1.3`, `soi 4.3`, `age`, `age by AGI`, `age by status`, `status by AGI`,
  `dependent filers`, `tips`, `deferrals`, `calculator`.
- Importance 2/3: `wealth`, `wealth grid`, `consumption`, `auto loans`, `overtime`, `older ages`, `non-filers`,
  `non-filer people`, `non-filer items`, and the projection kinds `projection items`, `projection filing season`,
  `cbo counts` and `z1 wealth`.

Identities are exact whatever their importance.

## The solve

`reweight/solver.py`. The new weights w minimize

    sum over targets of 0.5 x importance x (miss / standard error)^2  +  tau x D(w)  +  soft cap terms,

where a miss is where the file lands less the target's value, and D(w) is the Kullback-Leibler divergence of the new
weights from the starting weights, in nats, on weights divided by the sum of the starting weights. Identities are met
exactly. No target has a range and none is dropped.

- **tau** is 1,000 (settings `tilt.reweighting.tau`): the weight of the move against the misses. A larger tau keeps
  the file closer to the start and leaves larger misses.
- **Soft cap.** Each record has a cap U equal to k times the mean starting weight of its group, with k = 10. The nine
  groups are filers by the calculator's AGI ($0 or less, $0-50,000, $50,000-100,000, $100,000-200,000,
  $200,000-500,000, $500,000-1 million, $1-10 million, $10 million and over) and non-filers; the mean is over records
  with a positive starting weight, records held fixed left out. A record's weight w adds 0.5 x beta x ((w - U) / U)^2
  to the cost above its cap and nothing below it, with beta = 1 (settings `tilt.soft_cap`). With beta = x^2, a record
  at twice its cap costs as much as an x-standard-error miss on a target of importance 1. `soft_cap_groups.csv`
  lists each group's cap.
- **Averages and shares.** "The group's average of v is m" is written as the sum over the group of w (v - m) = 0. Its
  miss is the group's weight times the miss on the average, so the standard error used is the average's standard
  error times the group's weight after the reweighting. That weight is not known in advance: the solve is taken at the
  start's group weights and, if any group's weight moved by more than 0.1% (`average_weight_change`), once more at
  the first answer's.
- **Records held fixed.** The Forbes list's records keep their weights. Each target's part from them is a constant,
  and `top_feasibility.csv` says, before the solve, whether the other records can reach what is left.
- **Method.** Every target is a linear condition on the weights. The minimizing weights have the form
  w = w0 x exp(F lambda), one multiplier per target, and the multipliers minimize a smooth convex function found by
  Newton's method. The budget is 150 Newton steps in all (`newton_budget`), with a stopping size of 1e-8 for the
  gradient, measured as a miss over the target's size (`gradient_tolerance`).

The demographic prescaling uses the same cost on its own cells.

### Check before the solve

`reweight/precheck.py` flags targets and changes nothing (settings `tilt.precheck`; `precheck_flags.csv`,
`precheck.md` and the summary). A target is flagged when:

1. For a cell with a count and an amount target, the average per return they require is outside the range of the
   amounts per return of the records in the cell by more than one standard error (the two relative standard errors in
   quadrature). Both targets are flagged.
2. Its relative standard error is below 0.1%, which no solve within the step budget can reach.
3. The start is more than 300 standard errors from it.

### Thin cells

After the solve, each target gets two measures (in `fit.csv`):

- the effective number of records behind it, (sum of w |f|)^2 / sum of (w f)^2 over the records that contribute,
  where f is a record's contribution to the target (for an average, its value of v within the group);
- the largest weight factor, final over starting weight, among the records holding at least 1% of the cell's weighted
  total after the solve.

| Flag | Effective records | Largest weight factor |
|---|---:|---:|
| Warning | fewer than 30 | above 10 |
| Stop | fewer than 10 | above 100 |

These thresholds are placeholders. A stop does not stop the run: the summary says STOP at the top, and the target
table suggests a neighbouring cell to merge with.

## The start and the prescaling

### Past years

1. The start is the generated file (`generate/generate.py`, drawn in equal shares from each trained model), its
   dollars turned into year t's dollars by year t's dollar maps (`FEATURES.md`).
2. The top of the file is prepared (`generate/top_records.py`): rebuilt hidden returns, generated balance sheets of
   $1 billion or more drawn again below that line, and the year's Forbes list added with weight 1, held fixed.
3. Filers are scaled to SOI's number of returns and non-filers to the CPS's expected non-filers. Filers are then
   scaled within joint and non-joint returns to Table 1.2's counts.
4. The demographic prescaling (`start_scaling: {weights: demographic}`, always used for a start drawn from the networks):
   filers are moved to Table 1.6's returns by the primary filer's age range within joint and non-joint returns,
   keeping the two totals exact; non-filers are moved to the non-filer people targets, keeping their total. Each batch
   takes the smallest move under the same cost. These cells stay targets in the main solve.

### Projection years

Each projection year's `targets.yaml` says `start: {from_year: 2023, run: y2023}` and
`start_scaling: {weights: persons, dollars: item_factor}` (`scale/projection_steps.py`).

- **The start file.** 2024 starts from the finished 2023 file. Every later year starts from the finished 2024 file,
  the jump-off year: the last year with IRS data, SOI's November filing-season table included (settings
  `tilt.projection.filing_season`; `target_list.start_spec` turns `from_year: 2023` into 2024). With that setting off,
  every year starts from 2023. No projection year depends on any other except through its start file.
- **Step 1, people.** The start's weights take the smallest move under which the people in the file match exact counts
  by marital status and single year of age (0 to 79, then 80 and over). Each count is the start file's own count
  times the growth of the same people in the Social Security area population (Macro-Projections, settings
  `tilt.projection.population_dir`). Every member of a unit carries the unit's weight; a dependent filer is counted on
  the return that claims him; a married person under 18 is counted unmarried. The dependent filers' six cells (age
  range by married) are exact too, each grown with its population.
- **Step 2, dollars.** Each dollar item is multiplied by one factor so that its total grows exactly at its series, or
  per record for a per-unit series. Gains and losses get separate factors. The series, kind, whether the item is
  targeted and its error are in `targets_config/projection_items.csv`; extra growth in percent a year, by item and
  range of years, is in `projection_add_factors.csv`, and every non-zero row carries a note saying who set it and why.
  After 2035 (settings `tilt.projection.cbo_last_year`) every series continues at the growth of its long-run
  Macro-Projections aggregate. From the jump-off year on, taxable interest grows by nominal GDP times CBO's 3-month
  Treasury bill rate (settings `tilt.projection.interest_growth`).
- **Step 3** is the main solve on the year's targets.

The Forbes list's records are left out of steps 1 and 2. Each target is the start file's level without them, grown,
plus the year's list's own fixed contribution.

## Targets of past and projection years

Both kinds of year run the same stage: build the start, build the targets, solve. They differ in the start and in
which sources exist.

### Past years (2015-2023)

Every past year targets SOI's published tables, the surveys and the non-filer counts. About 1,300 targets after
expansion (the installed files: 1,272 in 2015, 1,325 in 2019, 1,343 in 2023). In 2019:

| Kind | What | Targets |
|---|---|---:|
| `IRS` | Table 1.4 returns, income items and their holders by AGI group; Table 1.2 joint returns | 222 |
| `business` | business income and losses by AGI group, farm income overall | 128 |
| `soi 1.3` | income items by filing status | 86 |
| `soi 4.3` | the top 0.001%, 0.01%, 0.1% and 1% of returns | 72 |
| `age`, `age by status`, `age by AGI`, `status by AGI` | Table 1.6 returns by age; Table 1.2 by AGI | 142 |
| `dependent filers` | Table 1.7 | 23 |
| `tips`, `deferrals` | W-2 statistics | 35 |
| `calculator` | the calculator's columns against Tables 1.4, 2.1, 2.5 and 3.3 | 296 |
| `wealth`, `wealth grid`, `auto loans` | SCF | 192 |
| `consumption` | CEX consumption by age and married | 104 |
| `overtime`, `older ages` | CPS | 14 |
| `non-filers`, `non-filer people`, `non-filer items` | the CPS filing stage and the tax data | 11 |

Entries present only in some years, because the source or the law differs:

| Entry | Years |
|---|---|
| W-2 tips and elective deferrals | 2019-2023 |
| QBI deduction; itemized "other" deductions | 2018-2023 |
| EITC by number of children (Table 2.5) | 2015-2020 |
| Child tax credits, nonrefundable and refundable | every year but 2021 |
| State income tax refunds | every year but 2018 |

### Projection years (2024-2075)

The entries of every projection year's `targets.yaml`:

- `persons_by_age`: step 1's people by marital status and single year of age. Identity.
- `returns_indexed`: in each group of the primary filer's age band (under 20, five-year bands, 80 and over;
  dependent filers by age range) and marital status, the start file's share of the group's units that file times the
  group's units after step 1. Identity.
- `item_totals`: each targeted item's total after step 2, gains and losses separately, for items with at least 500
  records and $500 million in the start file. The item's error in `projection_items.csv` (1% today).
- `filing_season_groups`, 2024 only: SOI's table of returns processed through November gives each item's growth from
  2023 by AGI group. The target is the 2023 file's total in the group times that growth, blended with CBO's national
  growth by the item's trust: the weight on the November growth, 1, 2/3, 1/3 or 0 (`filing_season_trust.csv`). Items
  of full trust also target the returns with the item, and the number of returns is targeted as shares across the
  groups. These replace the items' national totals. The error is the root mean square miss of the November growth
  against final SOI in past years (`filing_season_errors.csv`), at least 1%.
- `amt_cbo`: the start file's number of AMT returns times CBO's growth in that number; none after 2035. 5%.
- `net_worth_z1`: the start file's household net worth times the growth of the Financial Accounts' household net worth
  (after 2025 at a fixed ratio to GDP). 2%.
- `wealth_categories`: SCF balance-sheet categories, holders and dollars, below and above the SCF's 90th percentile of
  net worth. Replicate weights.

The installed files have 427 targets in 2024 and about 305 in each later year. The number of itemizers is not
targeted; it follows from the calculator and is reported against CBO's count.

## How a year's targets are written down

Each year has a folder, `pipeline/targets_config/<year>/`, written out in full; nothing is inherited from another
year. A new year starts as a copy of the nearest year's folder and is then edited.

| File | Holds |
|---|---|
| `<year>/targets.yaml` | the year's target entries, its start and start scaling, and what the run reports |
| `<year>/prior.yaml` | the shares of the survey years the file is drawn from (SCF waves, CEX, CPS and SIPP years) |
| `<year>/growth.yaml` | one line for every dollar item the pipeline grows |
| `catalog.yaml` | shared by every year: sources, populations, statistics, error kinds, variables, groupings |
| `projection_items.csv` | the projection years' series for each grown column: series, kind, whether targeted, error |
| `projection_add_factors.csv` | extra growth by item and range of years, with a note for each non-zero row |
| `filing_season_trust.csv` | each item's trust in the November filing-season tables (full, mostly, mostly not, none) |
| `filing_season_errors.csv` | the standard error of each filing-season target, from the backtest against final SOI |

The catalog says what the words in the year files mean: each source, each population's rule, each variable and its
line in each published table, the groupings of units into cells, and the tax-unit rules each survey's units were
built with.

### targets.yaml

Top-level keys: `year`, `kind` (`historical` or `projection`), `start` (`generate`, the default, or
`{from_year: <year>, run: <run name>}`), `start_scaling` (`weights`: `demographic` or `persons`; `dollars`: `none`
or `item_factor`), `report` and `targets`. `kind` and `report` are recorded in the resolved configuration; no code
reads them.

Each entry under `targets` is a block of targets: every statistic times every variable times every cell. Example:

    - id: irs_wages
      kind: IRS
      statistic: [count, amount]
      population: filers
      variable: income_items_standard
      cells: {standard_agi_groups: all}
      value: {published: soi 1.4}
      error: {kind: soi_cv}
      key: {group: '{cell}', target: '{variable} {statistic}'}

| Key | Meaning |
|---|---|
| `id` | unique within the year |
| `kind` | the kind of target; sets the importance and the error rule |
| `statistic` | one statistic or a list |
| `population` | a catalog population |
| `variable` | a catalog variable or variable list; absent for a count of units |
| `cells` | `all`, or catalog groupings with the labels to use (`all` for every label) |
| `value` | `{<kind>: <source>}`, optionally with `year` when the value's year is not the target year |
| `error` | `{kind: <error kind>}`; a judgment error also needs `relative` and `reason` |
| `key` | templates for the target's group and target names, as in `fit.csv` (below) |
| `outside_line` | the line of an outside series |
| `dependent_filers` | how dependent filers are placed in a table that splits by something they lack |
| `percentiles` | the percentile lines of a worker-share entry |
| `must_reach` | cells that must be reachable |
| `needs_calculator` | the entry waits for the taxes |
| `reason`, `note` | why the entry is as it is in this year |

In a key, `{cell}` is the cell's label, `{<grouping>}` the label of that grouping's part of the cell, `{variable}` the
variable's short name, `{label}` its label, `{statistic}` the statistic and `{year}` the year of the value.

### growth.yaml

One line per dollar item: `source` (the series for positive values, or `none: <reason>`), `negative` (`same`,
another series, or `none: <reason>`), `measure` (what the series is per: per return with the item, per holder, per
household, a price) and `reason`. `shared/growth.py` reads it. The series are used for every year the run grows
across: the PUF year to the target year (the dollar maps), the PUF year to each survey year, and an earlier year's
file to the target year.

Every dollar item needs a source in every year, and the source must be exogenous to tax policy; SOI's itemized
deduction tables are therefore not used, since the 2017 law's fall in itemizers moves their totals and means. The
series of each item, with its factor from 2015 to 2019, are in `FEATURES.md`, section 4, "Dollars in the target
year"; the data are in `research/rebuild/resources/growth/`, with `sources.csv` saying where each came from. The
capital loss allowed on a return ($3,000) is a statutory nominal amount and is not grown.

### Writing rules and checks

`reweight/target_list.py` checks every file when it reads it:

- no YAML anchors, aliases or tags;
- numbers written out (`1000000`, `0.02`), never `1e6`, which YAML reads as text;
- text containing a comma quoted inside `[ ]` or `{ }`; digits joined by an underscore quoted (`'18_25'`);
- an unknown key is an error.

Before any stage reads the configuration (`target_list.resolve`), the pipeline checks that every entry has its parts,
a known population, statistic and error kind, a judgment error's number and reason, and a value source the catalog
defines; that entry ids are unique; and that every dollar item has a growth line with a positive factor to the year
(`growth.check`). Otherwise it stops and lists what is missing.

### Numbers from other models

Numbers produced inside the pipeline (the non-filer counts) are computed fresh in each run and versioned by the code
commit. Outside series are files: CBO's February 2026 individual income tax projections and counts of returns, the
Financial Accounts' net worth and the growth series are in `research/rebuild/resources/` (`growth/`,
`cbo_counts.csv`); the Social Security area population is read from the Macro-Projections vintage named in settings
`tilt.projection.population_dir`. Every row of the target list names its source.

## What every run reports

In the year's tilt folder:

| File | Contents |
|---|---|
| `file_<year>.parquet` | every record, every dollar column in nominal dollars of the year, `weight_start` and `weight` |
| `fit.csv` | every target before and after the solve, its miss in standard errors and the thin-cell measures |
| `targets.csv`, `resolved_<year>.yaml` | the expanded target list and the configuration as resolved |
| `targets_excluded.csv` | the cells left out, with the reason |
| `importance.csv` | each target's importance |
| `soft_cap_groups.csv`, `weight_factors.csv` | each group's cap; weight factors by AGI group |
| `precheck_flags.csv`, `precheck.md` | the check before the solve |
| `scf_targets_by_wave.csv` | each SCF target in each wave |
| `top_feasibility.csv` | each target's remainder after the fixed records |
| `taxes/taxes_fit.csv` | the calculator targets before and after |
| `summary.md` | the solve, its misses and thin cells, the move of the weights, the start and the prescaling |

The effective sample of a set of weights is (sum of w)^2 / sum of w^2; the share kept compares it after and before
the solve.

`output/report_tilt.py` writes `report.md`, one page per year, with a table behind each section:

1. every target: value, error, where the file lands and the miss in standard errors, the largest misses first
   (`report_targets.csv`);
2. thin cells (`report_thin_cells.csv`);
3. published numbers not targeted: SOI's cells of Tables 1.4, 1.6 by AGI and 1.7, with the file's miss
   (`report_published.csv`);
4. untargeted statistics before and after the solve, the largest moves first; a large move no target asked for points
   to a missing target or a poor start (`report_moves.csv`);
5. each trained model's value on the untargeted statistics; a large gap marks a statistic the start is unsure of
   (`report_models.csv`);
6. overall: the effective sample kept, the Kullback-Leibler move, the settings, the code commit and the vintages read
   (`report_overall.csv`), with the non-microdata imputations' checks.

## Known limitations

- The definition gap between a source and the file is usually the larger part of a target's error and is usually
  unknown. Most standard errors are sampling errors only, so the solve treats many targets as better known than they
  are.
- The thin-cell thresholds are placeholders, and many cells flag: the installed 2019 file has 162 warnings and 102
  stops.
- The CPS non-filer people are counted by the file's rule (heads and spouses by their own age, at most three
  dependents per unit), while the filing stage calibrates to the CPS head's band and every child; in 2019 the two
  differ by up to 0.09 million people in a band and by 0.20 million children.
