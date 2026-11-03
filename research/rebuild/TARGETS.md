# Targets: principles, rederived

Started 24 October 2026 with John. A ground-up redesign of post-training targets. The principles are worked out
one question at a time, and each one is recorded here once agreed. The earlier statement (22 October, in
`explore/2026-10-22/targets/DESIGN_CONTEXT.md`) and its four overnight reports are background. This document does
not inherit from them.

## The process on one page (as decided 25 October)

The sections below are the record of how each decision was reached. This page says what the process is. Where a
later decision replaced an earlier one, this page follows the later one.

**What the file is for.** Tax-Simulator's input for each year: one weighted file of tax units, filers and
non-filers, whose revenue and distribution results are right. Top incomes and top wealth come first (S1).

**1. Pretraining (once, not per year).** One network learns the joint distribution of tax units from the PUF
(2015), the SCF, the CEX, the CPS and the SIPP. Each training record carries its source and its year. Amounts are learned as
ranks, and rank-to-dollar maps turn them into each year's dollars. Each record counts by its survey weight times q,
so the few records at the top of the PUF and the SCF count as much as the many in the middle; PUF records also carry
their AGI group as a label, always shown (S8). Two copies are trained from different random starts, and every file
pools them half and half (section 8).

**2. Each year t: the start.** The year's configuration file says which of two starts (S7):
- *generate*: draw a fresh sample from the two models, with year t's mix of survey years (section 7; built for the
  SCF's waves). The AGI group is drawn first: a set number of records in each of 12 AGI groups, more at the top,
  each weighted by its group's returns over the records drawn in it, which undoes q exactly (S8). Its dollars come
  from year t's rank-to-dollar maps (each PUF amount grown to year t by its line in the year's growth.yaml: per
  return with the item where SOI Table 1.4 has it, else a declared series exogenous to tax policy, section 12; the
  balance sheet grown per tax unit from its wave to year t's wave), and its weights are scaled to year t's
  demographic cells (section 4).
  Historical years use this.
- *from_year s*: take year s's finished file, scale its weights to year t's demographic cells and grow each
  record's amounts to year t. Future years use this, from the last historical year, so they carry its
  distribution of income.

**3. Each year t: the targets.** Facts about year t, each with a value and a standard error (sections 2, 3, 9):
- hand-written in the configuration: SOI detail in historical years (AGI brackets with the five top ones separate,
  income items and their composition at the top, income items by filing status (Table 1.3) and in the top 0.01%,
  0.1% and 1% (Table 4.3), business income, ages, dependent filers, W-2 tips), SCF wealth at the top and by asset
  type, an SCF income x net worth grid (26 October), SCF auto loan interest, CEX consumption, the non-filer count
  and non-filer people by age; in future years, macro
  totals and population by age (projected totals may be exact, to match the baseline);
- optionally built automatically from the start file (*hold_distribution*): each item's share in AGI rank groups,
  so the distribution does not drift.
A family of targets is kept only if it bears on an output, its source matches the concept, and the file would
miss it without it (S5). The SCF targets are computed on the same mix of SCF waves the start draws its balance
sheet from (S9).

**4. Each year t: the tilt.** One cost (S11, since 1 November): the weights minimize the sum over targets of
0.5 x importance x (miss ÷ standard error)² plus tau x the Kullback-Leibler move from the start. Identities are exact; no target has a range
and none is dropped; tau is 1,000. Cells resting on too few records are flagged, with a stop for the worst (section 5).

**5. Each year t: the report** (section 14): every target and where it landed, the thin cells, published numbers
not targeted, what the tilt moved, and how far the two models disagree.

**Built, as of 27 October:** steps 1-5 for 2019 (753 targets), with grouped weighting and the AGI group drawn first (S8), the
SCF's waves mixed by section 7's rule in the start and in the SCF targets (S9), every dollar amount in target-year
dollars before the tilt, ranges by kind of target and the report; the from_year start and hold_distribution are being tested on 2022. **Not built:** the borrowing rule for
the CEX, the CPS and the SIPP (drawn at fixed years), calculator targets, the Forbes 400 and Distributional Financial
Accounts. **Known weakness:** the network's drawn parts often add up to another AGI group than the one it was given
(about 14% of filer records); the file still has about 1.4 times the PUF's returns at $10 million and over before
the tilt, and 1.5 times its returns with AGI at or below zero (the latest run, `FEATURES.md`).

## 1. What post-training is for (agreed 24 October)

The year-t file is our best estimate of the population of tax units in year t. We have two kinds of information
about it:

1. **Microdata** (the PUF, SCF, CEX and CPS, from various years). They describe the joint distribution: who has
   what along with what. The trained model turns them into a prior for year t.
2. **Facts about year t itself:** published totals, counts and averages for that year, or projections of them.
   Example: SOI counts 27.60 million 2019 returns with a primary filer aged 65 or over.

Post-training combines the two. The prior supplies the joint structure, and the facts move it to where year t
actually is. Two consequences follow:
- A target is a measurement of one statistic of the year-t population, and every measurement has some error.
  Whether to hit it exactly depends on how good the measurement is relative to the prior.
- The prior is information too. Moving away from it costs something, in proportion to how much we trust it.

## 2. What makes a fact usable as a target (agreed 24 October)

The file must be able to compute the same statistic, for the same population, under the same definitions. This is
the translation rule (every source put into PUF concepts before comparing) applied to targets. A target has five
parts:

1. **Statistic:** a count, a dollar total, a share or an average.
2. **Population:** filers, non-filers or all tax units, and the cell (an AGI bracket, an age range).
3. **Variable:** in the file's definition. It is either:
   - a column of the file; or
   - **a function of the record and year t's tax law, computed by the tax calculator** (tax-simulator-gpu,
     called through an interface). Example: the number of itemizers. A record contributes 1 if the calculator says
     it itemizes under that year's law, else 0. Reweighting does not change records, so the calculator runs once
     per year and the reweighting only adds up its outputs.
4. **Value.**
5. **Error.** It has two parts:
   - **sampling error** in the source (SOI publishes it for some tables);
   - **the definition gap** between the source's concept and the file's. This is usually the larger part, and
     usually unknown.

The rule:
- A fact enters as a target only once its concept matches the file's.
- A small remaining gap goes into the error.
- A large or unknown gap means the fact is reported as a comparison, not targeted.

A calculator target is subject to the same rule. The calculator can only compute what the file gives it. The
2019 itemizer count (SOI: 17.3 million) becomes usable only once the file carries the Schedule A inputs (state and
local tax, charity, medical) in a concept the calculator can read. With those inputs missing, the 22 October test
computed 5.3 million (`explore/2026-10-22/targets/calculator_api/REPORT.md`).

## 3. Where a target's value comes from when nothing is published for year t (agreed 24 October)

**A target exists only where we have information about year t.** Where nothing is known, the prior's answer is
already the best guess and a target adds nothing. The exception is a **guard**: a statistic we do not want the
other targets to move by accident. A guard is held near the prior's value with a wide error.

**Outside information that comes only as a change is applied to our own level.** If an outside forecaster says
wages grow 10% from 2023 to 2026, the 2026 target is 1.10 times our file's 2023 wages, not the forecaster's 2026
dollars. The forecaster's wage concept differs from ours. The working assumption is that this definition gap stays
fixed, so only the growth carries over.

A value therefore comes from one of three places:

| Where the value comes from | Its error |
|---|---|
| Published for year t (SOI's 2019 tables) | The source's sampling error, plus any definition gap |
| Our own latest level times an outside change | Our level's error combined with the forecaster's error on that change, judged from its past forecasts |
| A guard at the prior's value | Wide, set by us |

Historical years and projection years are one case. They differ only in which of the three is available for each
statistic.

**CBO's counts of returns follow the same rule (1 November 2026).** In the projection years 2024-2035 the number of
all returns, of itemizers and of returns affected by the AMT are each the previous year's file count times CBO's growth
in that count from the previous year to this one, with the error of 1% as a judgment. Until 31 October they were CBO's
published levels, which count a return differently from the file (CBO's 2024 count of all returns, 167.1 million, was 2.4%
above the file's). `target_list.cbo_count_rows` reads the previous year's file. The population cells (previous count
times CBO's population growth) and the dollar totals (previous total times CBO's growth) were already of this form.

## 4. What the prior for year t contains before any target (agreed 24 October)

The prior is our best guess for year t, with every change we can explain from outside facts already applied. The
cost of moving away from the prior should be paid only for change we cannot explain.

Example, with made-up numbers. The prior comes from 2019 records and the target year is 2030. Projections say the
population aged 65 and over grows 20% more than everyone else. If the prior keeps 2019's age mix, the reweighting
has to pull that whole shift out of it and pays for it as a large departure. If the prior already has 2030's age
mix, aging costs nothing. Cost arises only where 2030's facts disagree with an aged 2019.

Two things are applied to the prior before targeting:
1. **Population weights** scaled to year-t counts in demographic cells (age, marital status, number of children):
   SOI and Census counts for historical years, demographic projections for future years.
2. **Dollars.** The model draws each amount as a rank. A year-t map from rank to dollars turns it into dollars,
   built from the latest observed map times growth per holder (the rule of section 3).

Each is also stated as a target, so that other targets cannot drag the age mix or the dollar levels away from what
was put in.

Left for a later question: which source-years the prior draws on (for example, the 2019 SCF alone, or the 2019 SCF
with some of 2022).

Considered and set aside (24 October): drawing the demographic mix directly in generation instead of reweighting
it. It gives the same distribution as reweighting, and keeps more of the effective sample only when the shifts are
large. It would need repeatable draws per demographic cell so that year-to-year changes are not sampling noise.
tax-simulator-gpu accepts a record set that changes by year, but its memory grows with the number of distinct ids
across all years. John judged it a solution in search of a problem.

## 5. How the targets and the prior are weighed (agreed 24 October)

Targets are what we trust. The prior is not a competing estimate; it is a crude first approximation whose job is
to supply realistic records (who has what along with what).

- **Since 1 November (S11) every target is weighed against the move of the weights, in one cost.** A miss costs
  0.5 x importance x (miss ÷ standard error)², so noisy targets give way and precise ones hold. Example, with made-up
  numbers: one table puts a group at 27.60 million (standard error 0.28 million), another implies 28.00 million
  (standard error 0.07 million). The file lands near the precise one, 27.98 million. The move of the weights costs
  tau times its Kullback-Leibler divergence, so the prior is weighed against the targets, with the weight tau says.
- Exact, with no error, only for identities.

(What this section said on 24 October, that every target is hit exactly when all can be, and that standard errors
decide only when they cannot, was replaced by ranges on 25 October (S6) and by the one cost on 1 November (S11).)

**A target that can be hit only by stretching a few records far is flagged, not silently hit.** The metric is the
effective number of records behind each target cell after the reweighting: how many records the cell's total
really rests on. A cell of 120 records where 5 carry most of the weight rests on about 5. A backup metric is the
largest weight factor, a record's final weight over its starting weight. Overall effective sample kept is too
coarse: a file can keep 96% while one top cell rests on a handful of records. Placeholder thresholds, to be set
from experience:

| | Effective records behind a target cell | Largest weight factor |
|---|---:|---:|
| Warning | fewer than about 30 | above 10 |
| Stop | fewer than about 10 | above 100 |

The fix is manual: the user respecifies the target, for example by merging a thin cell with its neighbour. Two
things keep this manageable across many years:
1. Defaults that rarely trip the warning, such as merging thin top brackets until each target cell has enough
   records (as today's IRS brackets are merged).
2. A report that, when a warning fires, names the target, the records carrying it and a merge that would fix it.

## 6. Which targets, and how many (agreed 24 October)

1. **A floor.** Every variable in the file gets at least a count of holders and a dollar total, wherever a source
   with a matching concept exists (section 2). No item drifts merely because nothing watches it. The open worry:
   for items Tax-Simulator barely uses, the floor adds targets that can only cause trouble.
2. **More detail where it moves tax.** Items that drive liability get detail, such as by AGI bracket and filing
   status. Calculator outputs (tax before credits, itemizers, EITC claims) join once the file carries their inputs.
3. **Detail is added by a test.** Hold out a finer published table:
   - if the file already lands within its errors, the coarser target is enough;
   - if it misses, add the finer target.

   Two results from the 22 October overnight test (`explore/2026-10-22/targets/many_targets/REPORT.md`):
   - Business income targeted in 6 AGI groups brought the median miss on Schedule C across SOI's 15 brackets from
     16.7% to 3.6%.
   - Dependent filers (Table 1.7) left untargeted got worse as other targets pushed on their brackets, from 3.2 to
     about 6 standard errors off. They need their own target.

Everything published but not targeted goes into each run's report as a comparison.

## The method stays reweighting (decided 24 October)

Before question 7, John asked whether reweighting was path dependence: whether the pretrained model allows some
new way to squeeze its distribution onto the targets. A memo surveyed recent AI research on post-training and
controlled generation (`explore/2026-10-24/posttrain_ideas/REPORT.md`). Its conclusion, accepted:

- Every post-training method that starts from a pretrained model aims at the same distribution as the tilt. This
  includes reward fine-tuning with a KL penalty (RLHF), DPO, distribution matching, controlled decoding and guidance
  in diffusion models. They differ only in where the change is stored: in the network, in the sampler, or as
  weights on draws.
- Language models store it in the network because every user prompt needs fresh, unweighted samples. The rebuild
  delivers one weighted file, so that reason does not apply, and weights on draws are the exact route.
- Under the conditional limit theorem (Csiszár 1984), the tilt is what you get by drawing populations from the
  prior and keeping only those whose totals hit the targets. Any method that hits the same targets from the same
  prior without changing anything else ends at the tilt.
- Reweighting is the only form whose assumption about untargeted relationships can be stated and printed. It also
  needs the tax calculator only once per year, where every method that changes the network would need gradients
  through it.

Also considered and set aside on 24 October:
- tilting the band probabilities while sampling;
- editing records by repeated redraws, because it bends untargeted relationships invisibly and loses the audit
  trail;
- fitting the year input to hit targets;
- every form of fine-tuning;
- keeping the luckiest of several draws, because that fits noise.

Two things to do, from the memo:
1. **Oversample by rough AGI before the tilt.** Draw the income items for many records and compute each record's
   partial AGI. Keep every record above a line (say $1 million) and a fraction of the rest, then finish the kept
   records. Each record's weight undoes the thinning exactly. This generalizes the boost of the time, which
   oversampled the top 1% of one variable at a time. Example: the file would go from about 120 records above $10
   million of AGI to about 1,200, and the tilt does not change. **Built 24 October, then replaced on 26 October**
   by the AGI group drawn first (S8), which sets the number of records in each AGI group directly.
2. **Test whether the year input carries survey change into the prior.** The surveys cover many years and the PUF
   only 2015. Generate the PUF filer batch with the year input at 2019 instead of 2015 and compare both, before any
   tilt, with SOI's 2019 targets and with untargeted statistics. If the 2019 setting moves holdings and the joint
   distribution toward 2019 without distorting PUF-only items, it becomes part of question 4's prior for survey
   years. **Tested 24 October and closed** (`explore/2026-10-24/year_token/REPORT.md`). With the year input at
   2019, the two trained models moved the batch in different, often opposite directions. The median miss on the
   136 IRS rows got worse under both (8.0% to 8.8%, and 8.9% to 9.3%), and the tilt had to move about 35% further.
   Some items moved in ways no 2019 fact supports: Schedule C holders fell from 16.1% to 12.0% under one model
   (SOI 2019: 17.3%), and returns with no AGI rose 40% under the other. The reason is in the data: the network has
   seen the PUF only at 2015, and for 27 PUF items the CPS records zeros because it does not ask them, so at 2019
   the year input has nothing about tax items to carry. The year input stays a source-year label.

## 7. Which survey years each year's prior draws on, and how years relate (agreed 24 October)

**Each survey has its own rule for borrowing across its years.** The SCF's is the rule John set on 18 October
(`research/archive/rebuild_2026-10/design/priors_by_year.md`). Each wave is dated by its income year (the 2019 wave
is 2018). For target year T:
1. use the next survey at or after T, if one exists, and the surveys before T;
2. weight each by exp(−|T − t| ÷ 2 years);
3. drop any under 5% and rescale the rest.

Past the last wave the mix stays fixed. The rule gives the 2019 file 55% of the 2019 wave, 33% of the 2022 wave and
12% of the 2016 wave.

**The CPS, the CEX and the SIPP (John, 27 October): the two nearest of their years, weighted by distance.** Their
years are the odd tax years 2015-2023. For target year T, use the two years nearest T and weight each by
exp(−|T − t| ÷ 2 years), the SCF's formula, rescaled to sum to one. An odd T takes its own year and the one two
years before: 73% and 27% (2019: 73% 2019, 27% 2017). An even T takes the years either side, 50% each (2020: 2019
and 2021). Past the last year the mix stays fixed at 2023 and 2021, 73% and 27%. Not built yet: it goes into
generation in the next big run (NEXT_STEPS.md), with or without overtime.
**Built for the SCF on 26 October:** generation reads the year file's prior ("balance sheet": the 2019 wave
54.65%, the 2022 wave 33.15%, the 2016 wave 12.20% for 2019), gives each record one wave at random with those
shares, draws its balance sheet at that wave's year and turns it into dollars with that wave's own records, grown
per tax unit to the target year's wave item by item (`FEATURES.md`). The SCF targets are computed on the same
waves with the same shares (S9). The shares are written in the year file by
hand from the rule; the code does not compute them. The CEX, CPS and SIPP are still drawn at one fixed year (2019)
until the rule above is built. This is ROADMAP item 3.

**No chaining for historical years.** Each historical year has its own prior, so last year's file is not this
year's starting point.

**Continuity in tax items can come from the draw instead.** The PUF is only ever 2015. If every year's draw uses
the same random seed and draws the tax items first, each record's tax items are the same in every year. Only the
survey-donated items (wealth, consumption, the CPS's items) change with the year's mix. Year-to-year changes in tax
items then come only from the targets. This is a design option, not yet checked against the code. Non-filers
differ by year in any case, because their number does.

**Future years share one prior.** Their records are the same, and each year is tilted fresh from that prior rather
than from the previous year's weights, so drift does not build up.

## 8. One trained model or several (agreed 24 October)

**The prior pools several trained models.** Training the same model with different random starting points gives
different files. Before the tilt, one model's 2019 file has $96 trillion of net worth and the other's $107
trillion. The tilt brings both to every target, so the difference survives only in statistics we do not target,
such as how wealth splits between stocks and housing within an income group.

The year's file draws the same total number of records, in equal shares from each model, and the combined file is
tilted once. The file is the same size as from one model, so Tax-Simulator's memory use does not change. Start with
two models.

**Every run reports how far apart the models are** on statistics we do not target. A large gap marks a statistic
the prior is unsure of, and is the evidence for adding a third model or a target.

## 9. Where each target's error comes from (agreed 24 October)

Errors only matter when targets conflict (section 5): they decide which target gives way. So they need to be
roughly right, and only the first three kinds are computed.

1. **SOI tables that publish errors** (Tables 1.4, 2.1 and 3.3): the published coefficients of variation.
2. **Other SOI tables** (1.6, 1.7, the business tables): the same SOI sample, so each cell borrows the coefficient
   of variation of the Table 1.4 cell closest to it in number of returns and amount.
3. **Survey targets** (SCF, CEX): sampling error from the survey's replicate weights.

Everything else gets a judgment number, written in the year's configuration with a one-line reason. That covers
outside forecasts, other models' outputs (such as the non-filer calibration) and definition gaps. A definition gap
adds to the sampling error as the square root of the sum of squares. Identities have no error and are hit exactly.

## 10. Dependent filers in SOI tables (agreed 24 October)

Dependent filers are real returns, so every SOI count includes them, and the file carries them as records with PUF
items only (they take nothing from the surveys). When an SOI table splits by a variable these records lack, they are
placed by whatever coarse information the PUF has, and the rest is spread in proportion to that table's own counts.
The tilt treats that placement as fixed.

Today the one case is age in Table 1.6. The PUF puts dependent filers under 26 or 26 and over. Those under 26 go in
the under-26 range. Those 26 and over are spread across the five older ranges in proportion to Table 1.6. This is
what the code already does.

## 11. One file per year (agreed 24 October)

Each year gets one file. We do not build several versions per year with target values drawn from their errors to
show a range from data uncertainty, and the code does not need to support it.

## 12. How a year's targets are written down (agreed 24 October; one folder per year since 29 October)

**Since 29 October (John)** each year has a folder of its own, `pipeline/targets_config/<year>/`, with three files
written out in full; nothing is inherited at run time:

| File | Holds |
|---|---|
| `<year>/targets.yaml` | every target entry of the year (statistic, population, variable, cells, value source, error, key), the year's start settings and what the run reports |
| `<year>/prior.yaml` | the year's prior: the shares of the SCF waves and of the CEX, CPS and SIPP years (section 7) |
| `<year>/growth.yaml` | one line for every dollar item the pipeline grows: its source series, the measure (per return with the item, per holder, per household, a price), how negative values are grown, and a one-line reason |
| `catalog.yaml` (shared) | the dictionary: sources, populations, variables and their lines in each published table, groupings, kinds of error. It is not a layer of defaults |
| `OVERVIEW.md`, `OVERVIEW.csv` (generated) | one table: every setting (each target with its value source and definition, each growth line, each prior share) by year, a cell that differs from the year before marked. Regenerate with `python research/rebuild/pipeline/config_overview.py` |

A new year starts as a copy of the nearest year's folder, then is edited. Before any run (`target_list.resolve`,
which every stage that reads the configuration calls) the pipeline checks that every entry has a value source the
catalog defines and that every dollar item has a growth line with a positive factor to the year (`growth.check`);
otherwise it stops and lists what is missing. 2019 is the only year with a folder; its files were written from the
layers below and expand to the same targets with the same values (proof: scratch
`explore/2026-10-29/yearcfg/RESULTS.md`).

**Growth sources (John, 29 October).** Every dollar item needs a growth source in every year. The source must be
exogenous to tax policy, so SOI's itemized deduction tables (Table 2.1) are ruled out: the 2017 law's collapse in
itemizers moves their totals and means. A series on the underlying spending or tax is used, per holder where a
clean one exists, else per household when the share of holders is steady, else AGI per return as a rough prior.
SOI Table 1.4's lines (dollars per return with the item) stay as they were for the items Table 1.4 publishes. For
the 26 items with no Table 1.4 line, which until 29 October were not grown at all: mortgage interest by BEA's
mortgage interest of owner-occupants per ACS home with a mortgage; real estate tax by the ACS's real estate taxes
per owner-occupied home paying them; state and local income or sales tax by Census state and local income tax
revenue per return; medical expenses by MEPS out-of-pocket spending per person with any; tax preparation fees by
the CPI's price of tax preparation; the two education credits' expenses by the College Board's published tuition
and fees; everything else by AGI per return (SOI Table 1.1), extended past SOI's last year by BEA personal income
per capita and CBO's projection of it. The capital loss allowed on a return ($3,000) is a statutory nominal amount
and is not grown. The series and where each came from are in `research/rebuild/resources/growth/` (`sources.csv`).

**Until 29 October** the layout was the overnight prototype's (`explore/2026-10-22/targets/config_format/`), which
reproduced the 317 targets of 22 October exactly:

The layout is the overnight prototype's (`explore/2026-10-22/targets/config_format/`), which reproduced the 317
targets of 22 October exactly:

| File | Holds | Edited |
|---|---|---|
| catalog | Definitions: sources, variables and their line in each published table, groupings (such as the SOI AGI brackets), kinds of error | Rarely |
| defaults | The targets every year gets (the floor of section 6) and what every run reports | Rarely |
| historical, projection | What every year of each kind gets | Sometimes |
| one file per year | Which prior the year uses, and what it adds, drops or overrides; every change carries a written reason | Per year |

Each target entry states the parts of section 2 in full words: statistic, population, variable, cells, where the
value comes from, and its error. Two additions to the prototype:
1. A survey target names the tax-unit rules its value was computed on. Moving the SCF to the shared rules changed
   its median net worth from $91,050 to $63,570.
2. Each year's expanded list of targets, one row per target with value and error, is saved with the run.

The prototype has to be brought up to the 383 targets of 24 October before anything is built on it.

The defaults, historical and projection layers and the year files under `years/` were removed on 29 October, when
the one folder per year replaced them (git history keeps them).

## 13. Values that come from other models (agreed 24 October)

1. **Produced inside the pipeline**, such as the filing stage's non-filer counts (a pipeline stage since commit
   212dfc0): computed fresh in each run and versioned by the code commit.
2. **From outside models** (CBO and Macro-Projections forecasts, SSA population, any series a level is grown by):
   a version pointer in `config/interfaces/` names the vintage, and the series is one CSV per vintage. The year's
   configuration names the series, and the run records which vintage it read. A new vintage gets a new pointer;
   an old one is never edited.

Small published numbers that do not change, such as Census children under 17 (`resources/pep_children.csv`), stay
in `resources/`.

## 14. What every run reports (agreed 24 October)

One short summary page per year, with one table behind each item:
1. **Every target:** value, error, where the file lands, and the miss in errors; targets more than one standard error from their value first.
2. **Thin cells:** effective records behind each target and the largest weight factor, with section 5's warnings
   and stops.
3. **Published numbers not targeted:** the file's value and the miss in errors (Table 1.7 today).
4. **What the tilt moved:** untargeted statistics before and after the tilt, largest moves first. A big move that
   no target asked for points to a missing target or a bad prior.
5. **Model disagreement** (section 8): each model's value on untargeted statistics.
6. **Overall:** share of the effective sample kept, the year's configuration and expanded target list (section
   12), the code commit and the vintages read (section 13).

A formula that predicts each untargeted move before the tilt (from the 24 October ideas memo) is not used; item 4
shows the actual moves.

## Target selection, restarted (25 October)

The first build (night of 24 October, `explore/2026-10-24/build/RESULTS.md`) reproduced the old 2019 targets as a
benchmark. A third of them were CEX consumption cells, which no principle chose. Target selection starts again from
what the file is for.

### S1. What the file must get right first (agreed 25 October)

**Top incomes and top wealth.** The tax system is progressive, so revenue and its change under most policies rest
on the people at the top, and much of the Budget Lab's modelling work is about getting them right. Targets, the
sample design and the checks are chosen with this first; everything else is secondary.

### S2. What "right at the top" means for this build (agreed 25 October)

Targets, all from SOI Table 1.4 and the SCF:
1. **Finer top AGI brackets:** $1M-1.5M, $1.5M-2M, $2M-5M, $5M-10M and $10M and over, in place of one "$1M or
   more" cell.
2. **Composition in each top bracket:** the number with and the amount of wages, capital gains, dividends,
   interest, partnership and S corporation income, and Schedule C.
3. **SCF top wealth in more detail:** shares of units and of net worth above its top percentile lines, by asset type
   (stocks, business equity, real estate, retirement accounts).

The Forbes 400, the Distributional Financial Accounts and the income-wealth link at the top come in a later build.

### S3. The sample at generation (agreed 25 October)

**Generate a large synthetic population, then sample it the way SOI samples returns.**
1. Draw a pool of complete records (balance sheet and consumption included), about 5 million, and compute each
   record's actual AGI and net worth.
2. Stratify on actual AGI (SOI's brackets, the five top ones separately), net worth (the SCF's top lines) and
   business income, with a sampling rate per stratum up to 100% at the very top; each top AGI bracket and top net
   worth line gets a floor of records (placeholder: 2,000), the rest of the budget (about 450,000 records) spread
   over everyone else.
3. Weight each record by one over its chance of being drawn, so the weights stay exact.

This replaces the four "top 1% of one item" parts and the single $1 million rough-AGI line. Drawing with a "top X%"
label was considered and set aside: the strata are on totals computed from the record, not on any one item the
network draws.

### S4. Strata as labels the network draws (agreed 25 October)

**Replaced on 26 October by S8** (grouped weighting and the AGI group drawn first). The strata labels, the net
worth class and its boost, and the design "strata" are gone from the pipeline (in git history). Kept here as the
record of how it was reached.

This replaces the pool-then-sample design of S3: instead of drawing a large pool and sampling it, the network is
taught a coarse stratum label and each record's stratum is fixed before the rest of it is drawn.

**Labels, defined up front by rank, on the training records:**
- **AGI class (PUF rows),** by weighted rank of AGI among 2015 PUF returns: AGI zero or negative; below the 90th
  percentile; 90th-99th; 99th-99.9th; 99.9th-99.99th; 99.99th and over.
- **Business flag (PUF rows):** nonzero Schedule C, partnership or S corporation, or farm income. With the AGI class,
  12 strata.
- **Net worth class (SCF rows),** by weighted rank among SCF tax units: below the 90th percentile; 90th-99th;
  99th-99.9th; 99.9th and over.

Survey rows leave the PUF labels blank, and PUF rows the net worth label, as for any item a source lacks.

**Generation.** A table in the settings gives the records drawn in each AGI x business stratum; each record starts
with its stratum fixed and the network draws the rest. The net worth class is drawn with its high classes boosted
when the SCF items are drawn. Weights are exact: a stratum's weight is the model's own chance of the stratum over its
share of the sample; a boosted draw is corrected by its chance under the model over its boosted chance. AGI and
net worth are still computed from their parts, and the labels are checked against them. **Revised 25 October:** a
record whose computed class differs from its label is kept, and the share is only reported. Dropping such records
biased the file (returns at p99.9-99.99 38% low against an unstratified draw, because the network often draws
amounts that contradict its label at the top); keeping them leaves the weights exact for the model's own
distribution, at some cost in records at the very top.
Sampling rates rise steadily across the classes. A continuous rule (a sampling rate smooth in AGI rank) was
considered and set aside: it needs a fine label that is often contradicted, and strata lined up with the target
brackets put records where the targets need them.

This replaces the four "top 1% of one item" parts, the pilot and the AGI thinning.

### S5. The test each target family must pass (agreed 25 October)

A family is kept only if:
1. **it bears on an output** Tax-Simulator produces: revenue, distribution tables, credits and transfers, or a tax
   on another base (the most important condition);
2. **its source matches the concept**, with a known error (sections 2 and 9);
3. **it is needed:** without it the file would miss the published number by more than its error. This is a
   judgment, informed by the run report's comparisons of numbers not targeted, not a formal test.

The 104 CEX consumption cells (13 cells x 8 categories) are kept for now (25 October): with ranges (S6, since replaced by S11) they no
longer conflict with the non-filer people targets.

### S6. Targets are ranges of one standard error (agreed 25 October; replaces section 5's "hit exactly"; replaced by S11, 1 November)

*Superseded: the range solver is removed; this section is the record of the rule that ran from 25 October to 1 November.*

Each target is met anywhere within its value ± 1 standard error, at no cost inside the range; the prior decides
where inside it (the smallest move). Identities stay exact. If the ranges cannot all be met, a miss beyond a range
costs (miss beyond the range ÷ standard error)², as before. A range on an average is measured on the average
itself, with the cell's weight after the tilt. So noisy targets barely bind (capital gains at AGI $1-5k: $137
million, standard error 82%, range $25-249 million) and precise ones stay nearly exact (returns at AGI $50-75k:
22.24 million ± 113,000). The width multiplier is a setting.

**Half a standard error, tested 26 October** (`HISTORY.md`, 25–26 October). At 0.5 for every target,
every target was met, but the effective sample kept fell from 98% to 11%, through one generated non-filer couple
whose spouse was coded aged 16 (counted as a child in the non-filer targets; its weight rose 2,416 times). A spouse,
and a non-filer unit's head, must now be 18 or over at generation (26 October). With that fixed
(run P1), 0.5 for every target still left 23 targets unmet (non-filer
people bands against CEX food and health averages) and kept 31% of the effective sample. Since 26 October the
range is set by kind of target: 0.5 for the SCF targets, 1 for the rest (S9).

### S7. One algorithm for every year, with the start as a parameter (agreed 25 October)

Every year t runs the same three steps: build year t's start, build year t's targets, tilt. The year's
configuration file says how each is built.

1. **The start**, one of:
   - `generate`: a fresh draw from the trained models with year t's survey mix (section 7), weights prescaled to
     year t's demographic cells (section 4);
   - `from_year: s`: year s's tilted file (s earlier than t, already built), its weights rescaled to year t's
     demographic cells and each record's amounts grown item by item to year t (per-return growth).
   Historical years normally use `generate`; future years `from_year` the last historical year. The start from a
   tilted file carries the latest distribution of income, which cannot be written down cleanly as aggregate
   targets; with demographics and nominal levels rescaled, the tilt has little left to do.
2. **The targets:** the hand-written entries (since 29 October the year's own `targets.yaml`, with the catalog):
   SOI detail in historical years, macro totals and population by age in future ones. Plus, optionally,
   `hold_distribution`: targets built automatically from the start file, each item's share of its total in AGI
   rank groups (e.g. p0-50, p50-90, p90-99, p99-99.9, p99.9+), with a judgment error. Stated by rank, so nominal
   growth does not disturb them. They make "the distribution does not drift from what we agreed" a target that is
   checked and reported.
3. **The tilt**, one cost (S11).

Projected totals in future years may be identities, so the file matches the baseline it is scored against.

### S8. Grouped weighting and the AGI group drawn first (agreed 26 October; replaces S4)

Tested 25–26 October; the record is in `HISTORY.md` (the scratch folders were deleted on 26 October).

**Training counts the top as much as the middle.** Each training record counts by its survey weight times q, with
q = records ÷ survey weight within its group: 12 groups by AGI on the PUF, 5 income classes per wave on the SCF, q = 1
on the CEX and CPS. The groups are computed from the record's bands (each band at a fixed representative amount),
not its exact dollars, because the network sees only bands: records it cannot tell apart always get the same q.

**The AGI group is a label, always shown.** The PUF record's group is a category the network learns, never hidden in
training, because generation always sets it. With the label hidden half the time like any other slot, the file had
3.3 times the PUF's $10 million-and-over returns; always shown, 1.2 times.

**Generation draws the group first.** 400,000 filer records are split across the 12 groups in proportion to the
PUF's records per group; each starts with its group set and everything else is drawn given it. A record's weight is
its group's 2015 returns over the records drawn in it, which is 1 ÷ q and undoes the training's up-counting exactly.
Every drawn record is kept: about 13% have parts that add up to another group, and that share is only reported.
Redrawing them until they agree was tried and not kept: it fixed the top counts but thinned business income in
the middle.

**Training settings with it:** batches of 512 records, 80 passes, bfloat16, a network of width 128 and 4 layers.

### S9. SCF targets from the year's mix of waves, held to half a standard error (agreed 26 October; the half-standard-error ranges ended 1 November, S11)

Tested 26 October (run P3, approved by John; `HISTORY.md`); in the pipeline since 26 October
(`pipeline/scf_targets.py`, commit 5441ad6).

**The SCF targets come from the same waves as the file's balance sheet.** Every SCF target (the 22 wealth targets
and the 22 grid targets) is computed on the year's mix of SCF waves, the year file's prior "balance sheet" (section
7; 2019: the 2019 wave 54.65%, the 2022 wave 33.15%, the 2016 wave 12.20%). Before, they came from the target
year's wave alone, while the file drew its balance sheet from all three.
- **Dollars.** Each wave's balance sheet is grown per tax unit to the target year's wave, item by item, by the
  factors the dollar maps use, so the targets and the file share one dollar concept. Each wave's weights are
  scaled to the target year's wave's number of units (162.76 million in 2019): dollars per unit carry over, and
  the file's number of units is set by SOI and the non-filer count.
- **Lines.** Each wave keeps its own: its income groups by its own percentiles of the seven income items (each in
  its own income year's dollars), its net worth lines at its own percentiles. A cell's value is computed within
  each wave. The file gets one dollar line per net worth percentile, the share-weighted average of the waves'
  lines.
- **Value and error.** Value = sum over waves of share x the wave's value. The waves are independent samples, so
  variance = sum over waves of share² x the wave's variance, each from its own 999 replicate weights and five
  implicates. Standard errors fall to about 0.65 of one wave's.
- **The values move little.** 36 of the 44 targets moved by less than half of the old standard error. The largest
  move: net worth with both income and net worth below p90, $16.61 to $17.32 trillion (+2.2 old standard errors),
  from the 2022 wave.

**Ranges by kind of target.** The SCF targets are met within ± 0.5 standard errors, every other kind within ± 1
(settings `tilt.range_in_se_by_kind`). Half a standard error for every target leaves 23 non-SCF targets unmet and
31% of the effective sample (S6); for the SCF targets alone, every target is met and 97.75% of the effective sample
is kept (run P3), against 97.9% with every range at 1. The narrower SCF ranges keep more net worth at the top
of income: $9.91 trillion with units at p99.9+ of income (pooled SCF: $10.38 trillion), against $9.35 trillion
with every range at 1. The pipeline's own test (the latest run, other trained models) gives the same picture: every
target met, 98.0% kept, $10.15 trillion at p99.9+ of income, 3.5% of the top 0.1% by wealth with income below p90
(pooled SCF: 2.8%).

**Settings** (as of 26 October; none of the three is left: the ranges went with the one-cost solve of 1 November,
and `tilt.scf_target_waves` on 3 November, leaving the year's mix as the only rule): `tilt.scf_target_waves` "prior" (the year's mix; "target year" gives the single-wave targets of
before, for checks), `tilt.range_in_se_by_kind` {wealth: 0.5, wealth grid: 0.5}, `tilt.range_in_se` 1. The
replicate weights of every wave in the mix sit in `<out>/data/scf_rw/<wave>/` (errors.py says how to fetch them).

### S10. Target changes wait for the core of the file (agreed 26 October; four families added 27 October)

**No other targets are added or changed until three things are built:** every PUF variable in pretraining (every
item production carries; done 27 October), the imputed variables that `src/main.R` adds today (ROADMAP item 4), and
the tax calculator in post-training (ROADMAP item 9). Until then the cost of a target cannot be judged: it depends on what the finished
file holds and on taxes computed from it.

**The exception (John, 27 October).** Four families were added with the tips, auto loan interest and the other PUF
items: Table 1.3 by filing status (184), Table 4.3 for the top 0.01%, 0.1% and 1% (54; item 6 below), W-2 box 7
tips (4, and 7 more on the spread of tips per worker later that day) and SCF auto loan interest (8). With them the list has 753 targets;
the latest run meets every one with 87.2% of the effective sample kept, against 98.0% with the 496 before (`FEATURES.md`; `HISTORY.md`, 26–27 October).
- **Deductions need the calculator.** Who itemizes depends on each year's law; the 2018 law change (TCJA) cut the
  number of itemizers sharply, so SOI Table 2.1's itemized deductions cannot be targeted by reweighting the records
  without computing each record's choice under that year's law. Table 1.3's state income tax refund targets already
  move itemizers: few 2019 returns have a taxable refund, and the refunds sit almost only on itemizers, so the tilt
  cuts itemizers by 13%. That waits for the calculator, when itemized deductions are revisited (John, 27 October).
- **Targets that move a lot from year to year are untested.** Example: unemployment compensation in 2020. Whether
  reweighting alone can reach it, or generation must draw the year differently, is not known.

**Ideas set aside for when the core is built:**
1. **Automatic measures at three stages:** after pretraining, for the generated file after the demographic
   prescaling (against the target year), and for the reweighted file, the last one most of all. Three kinds:
   - pass/fail rules that stop a run (split sums add up exactly, no spouse under 18, positive weights, the
     record's parts against its AGI group);
   - held-out targets: reweight to part of the list and measure the misses on the rest, in standard errors;
   - outside benchmarks, for computed tax items only: liability, child tax credit, the number of itemizers and
     similar, against SOI for historical years and CBO's projected Form 1040 lines for future years; JCT and
     Treasury tax expenditure estimates for some items. Some become hard targets (itemizers).
2. **What makes one target list better than another.** Not the number of published cells hit. What matters is
   that the joint distributions relevant to tax and distribution are right; a small above-the-line item moving
   does not matter, the number of mortgage interest deductions does. Most relationships within demographic
   groups move slowly (wages per working-age adult, the chance of children by age) and the prescaling with a small
   tilt handles them. The hard ones change in kind between 2015 and the target year. Example: interest. With
   higher rates after 2022 more units hold interest and hold more, and the new holders are likely younger and
   lower-income than 2015's. A target on holders alone scales up 2015-style holders (older, richer); only a
   joint target (holders by income, by age) picks the new ones. The prior for what a record looks like is the
   source data, so untargeted joint distributions should move as little as possible.
3. **A backtest on a year when the world moved.** Build 2022 from the 2015 prior and score it on a short list,
   ranked by importance to tax: mortgage interest (returns and dollars by AGI), itemizers by AGI, interest and
   dividends by AGI, the SCF 2022's interest holders by age. A list is better if it gets these right without
   being told them, or with one targeted fix. With the calculator: misses measured in tax dollars.
4. **Fewer CEX cells by age x married,** traded for cells by income.
5. **A catalog of SOI's tables:** every table in the shared mirror, what it publishes and for which years.
6. **Tables 1.3 and 4.3, tested 26 October, added 27 October** (`Tax-Data-targets`, branch rebuild-targets-13-43;
   its page is in `/nfs/roberts/scratch/pi_nrs36/jar335/rebuild/explore/2026-10-27/`). Table 1.3's income items by
   filing status: all met, effective sample kept 98.0% to 88.0%, side effects small (John: acceptable). Table 4.3:
   the top 0.001% (1,482 returns, AGI above $60.7 million) cannot be met; the PUF's aggregate records are not
   unpacked yet, so the file likely lacks those records. Decided (John, 26 October): Table 4.3's top group is the
   top 0.01% as a whole, with the 0.1% and 1% below it, until the aggregate records are in. Table 1.3's capital loss
   is measured as the loss allowed on the return (at most $3,000), as SOI publishes it; generated records that carry
   a net loss past the cap are left as drawn (John, 27 October: the item is definitional).


### S11. The reweighting is one cost and no other rule (John, 1 November 2026; decisions 57, 62-65 and 68)

The range solver (S6), the smoothing sequence, the finish step, the penalty sequence, the rule that dropped the costliest
block of calculator targets in historical years, the limit on each weight's factor and the hard cap on weights are removed;
none is kept as an option. The weights minimize

    sum over targets of 0.5 x importance x (miss ÷ standard error)²  +  tau x (Kullback-Leibler move of the weights),

with identities exact. Importance is 1 for the published figures of the IRS (kinds IRS, business, soi 1.3, soi 4.3, age,
dependent filers, tips, deferrals) and for the calculator's columns against them, and 1/1.5 for the survey and pipeline kinds
(wealth, wealth grid, consumption, auto loans, overtime, non-filers). tau is 1,000 (settings `tilt.reweighting`). Each
record's weight also pays a soft cap: 0.5 x beta x ((w - U) / U)² above U = 10 x the mean starting weight of its AGI group (decision 73; before it was the median)
(settings `tilt.soft_cap`; beta 1 as the mild strength). The demographic prescaling uses the same cost. At most 100 Newton
steps in all, never raised. The SCF targets are no longer held to half a standard error: their standard errors enter the cost
like the others' (S9's value and error are unchanged; its half-standard-error ranges are not).

### S12. Target consolidation (John, 1 November 2026; decisions 71 and 72)

Counts under 250,000 returns (scratch `explore/2026-11-01/dryrun/out/tiny_count_cells.csv`) are folded into their
neighbours, every historical year 2015-2023 (the projection years do not carry these targets):

1. Means-tested credit cells where few qualify are folded, each inside its own SOI line. Nonrefundable child credit
   (child and other dependent credit): no AGI, $1-10k and $10-20k are one group, under $20k. Additional child tax credit:
   $100k and over is one group. AMT: under $200k and $200k-500k are one group, under $500k. EITC: $40k-50k, $50k-75k and
   above are one group, $40k and over. (Groupings `nonref_credit_agi_groups`, `ref_credit_agi_groups`,
   `amt_agi_groups`, `eitc_agi_groups` in the catalog.)
2. Farm and farm rental: one overall target each for net income and net loss, count and amount (entry `business_farm`, no AGI groups).
3. Table 1.3, married filing separately: returns and AGI only (the income items are targeted for single, joint and head of household).
4. AGI groups $1m-1.5m and $1.5m-2m are one group, $1m-2m, in Table 1.4's grid and the business grid and in the calculator targets.
5. The calculator's income tax after credits targets are dropped.
A merged group sums the published cells and combines their errors as before. Table 4.3's top groups, dependent filers
and the deferrals are unchanged.

### S13. Projection years start from the 2023 file, not from each other (John, 1 November 2026; decision 74)

Every year 2024-2035 starts from the same base, the 2023 reweighted file (`start: {from_year: 2023}`), grown directly to
the year: weights times the population growth from 2023 to the year (age x joint cells for filers, age bands for
non-filers), dollars grown per return from 2023 to the year by CBO's and the macro series compounded over the years
(`projection_growth.plan_factors` with `from_year`), and targets the 2023 file's level times the cumulative growth from
2023 to the year (population cells exact; CBO totals and the counts of returns, itemizers and AMT returns with the error of
1% as before). The prior in the cost is the 2023 file prescaled to the year. The years are independent and run in parallel;
no year inherits another's misses or moves.
