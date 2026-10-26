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
(2015), the SCF, the CEX and the CPS. Each training record carries its source and its year. Amounts are learned as
ranks, and rank-to-dollar maps turn them into each year's dollars. Each record counts by its survey weight times q,
so the few records at the top of the PUF and the SCF count as much as the many in the middle; PUF records also carry
their AGI group as a label, always shown (S8). Two copies are trained from different random starts, and every file
pools them half and half (section 8).

**2. Each year t: the start.** The year's configuration file says which of two starts (S7):
- *generate*: draw a fresh sample from the two models, with year t's mix of survey years (section 7; built for the
  SCF's waves). The AGI group is drawn first: a set number of records in each of 12 AGI groups, more at the top,
  each weighted by its group's returns over the records drawn in it, which undoes q exactly (S8). Its dollars come
  from year t's rank-to-dollar maps (the PUF's amounts grown to year t per return, the balance sheet grown per tax
  unit from its wave to year t's wave), and its weights are scaled to year t's demographic cells (section 4).
  Historical years use this.
- *from_year s*: take year s's finished file, scale its weights to year t's demographic cells and grow each
  record's amounts to year t. Future years use this, from the last historical year, so they carry its
  distribution of income.

**3. Each year t: the targets.** Facts about year t, each with a value and a standard error (sections 2, 3, 9):
- hand-written in the configuration: SOI detail in historical years (AGI brackets with the five top ones separate,
  income items and their composition at the top, business income, ages, dependent filers), SCF wealth at the top
  and by asset type, an SCF income x net worth grid (tentative, 26 October), CEX consumption, the non-filer count
  and non-filer people by age; in future years, macro
  totals and population by age (projected totals may be exact, to match the baseline);
- optionally built automatically from the start file (*hold_distribution*): each item's share in AGI rank groups,
  so the distribution does not drift.
A family of targets is kept only if it bears on an output, its source matches the concept, and the file would
miss it without it (S5).

**4. Each year t: the tilt.** The weights move as little as possible (smallest Kullback-Leibler move) so that every
target lands within its value ± k standard errors (k = 1 for now, a setting; S6; 0.5 was tested on 26 October and
waits, see S6). Identities are exact. If the
ranges cannot all be met, misses beyond them are weighed by their standard errors. Cells resting on too few
records are flagged, with a stop for the worst (section 5).

**5. Each year t: the report** (section 14): every target and where it landed, the thin cells, published numbers
not targeted, what the tilt moved, and how far the two models disagree.

**Built, as of 26 October:** steps 1-5 for 2019, with grouped weighting and the AGI group drawn first (S8), the
SCF's waves mixed by section 7's rule, every dollar amount in target-year dollars before the tilt, ranges and the
report; the from_year start and hold_distribution are being tested on 2022. **Not built:** the borrowing rule for
the CEX and the CPS (drawn at fixed years), calculator targets, the Forbes 400 and Distributional Financial
Accounts. **Known weakness:** the network's drawn parts often add up to another AGI group than the one it was given
(about 13% of filer records); the file still has about 1.3 times the PUF's returns at $10 million and over before
the tilt, and 1.5 times its returns with AGI at or below zero.

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

- **When every target can be hit, every target is hit exactly.** Among the reweightings that do so, the one that
  moves the prior least is chosen. The prior only breaks ties; it is never weighed against a target.
- **When the targets cannot all be hit, each target's standard error decides who gives way.** A miss costs
  (miss ÷ standard error)², so noisy targets give way and precise ones hold. Example, with made-up numbers: one
  table puts a group at 27.60 million (standard error 0.28 million), another implies 28.00 million (standard error
  0.07 million). The file lands at 27.98 million, near the precise one.
- Exact, with no error, only for identities.

This is what the current solver does with its default setting. The 22 October plan's extra weight on the prior,
which would let standard errors loosen targets that can be hit, is dropped.

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
   records. Each record's weight undoes the thinning exactly. This generalizes today's boost, which oversamples the
   top 1% of one variable at a time. Example: the file would go from about 120 records above $10 million of AGI to
   about 1,200, and the tilt does not change.
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
12% of the 2016 wave. The CEX's and the CPS's rules are still to be written; they field more often than the SCF.
**Built for the SCF on 26 October:** generation reads the year file's prior ("balance sheet": the 2019 wave
54.65%, the 2022 wave 33.15%, the 2016 wave 12.20% for 2019), gives each record one wave at random with those
shares, draws its balance sheet at that wave's year and turns it into dollars with that wave's own records, grown
per tax unit to the target year's wave item by item (`FEATURES.md`). The shares are written in the year file by
hand from the rule; the code does not compute them. The CEX and CPS are still drawn at one fixed year (2019). This
is ROADMAP item 3.

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

## 12. How a year's targets are written down (agreed 24 October)

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
1. **Every target:** value, error, where the file lands, and the miss in errors; targets that gave way first.
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

The 104 CEX consumption cells (13 cells x 8 categories) are kept for now (25 October): with ranges (S6) they no
longer conflict with the non-filer people targets.

### S6. Targets are ranges of one standard error (agreed 25 October; replaces section 5's "hit exactly")

Each target is met anywhere within its value ± 1 standard error, at no cost inside the range; the prior decides
where inside it (the smallest move). Identities stay exact. If the ranges cannot all be met, a miss beyond a range
costs (miss beyond the range ÷ standard error)², as before. A range on an average is measured on the average
itself, with the cell's weight after the tilt. So noisy targets barely bind (capital gains at AGI $1-5k: $137
million, standard error 82%, range $25-249 million) and precise ones stay nearly exact (returns at AGI $50-75k:
22.24 million ± 113,000). The width multiplier is a setting.

**Half a standard error, tested 26 October (tentative, not on).** `explore/2026-10-26/scf/07_grid_growth`: at 0.5
every target was met, but the effective sample kept fell from 98% to 11%, through one generated non-filer couple
whose spouse was coded aged 16 (counted as a child in the non-filer targets; its weight rose 2,416 times). A spouse
must now be 18 or over at generation (26 October). The setting stays at 1 until John decides.

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
2. **The targets:** the hand-written entries (catalog, defaults, historical or projection, the year's own file):
   SOI detail in historical years, macro totals and population by age in future ones. Plus, optionally,
   `hold_distribution`: targets built automatically from the start file, each item's share of its total in AGI
   rank groups (e.g. p0-50, p50-90, p90-99, p99-99.9, p99.9+), with a judgment error. Stated by rank, so nominal
   growth does not disturb them. They make "the distribution does not drift from what we agreed" a target that is
   checked and reported.
3. **The tilt**, with ranges (S6).

Projected totals in future years may be identities, so the file matches the baseline it is scored against.

### S8. Grouped weighting and the AGI group drawn first (agreed 26 October; replaces S4)

Tested in `explore/2026-10-25/q_build` (q_band, q_label) and `explore/2026-10-26/overnight/experiments` (21, 23).

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
