# How we got here

The full record, with every script, report and decision, is in
`research/archive/rebuild_2026-10/`, also committed as `b3b3a2f`. Start
there at `README.md` and `design/plan.md`. This is the short version.

## The path

**6–7 October: background.** The problem was stated, and twelve
background reports were written on statistical matching, generative
models, imputation methods, and survey and tax data.

**8–14 October: first experiments.** A transformer that fills in hidden
variables of a record from the rest, tested on the PUF alone, then on the
PUF plus the SCF, then with reweighting to targets. The lessons were
mostly about data, not the model:
- compute totals from their parts, never predict them;
- train on the sample as drawn, so the top is not starved;
- translate surveys into tax concepts before training;
- score on held-out records.

**14–16 October: the minimal example.** Seven variables, the 2015 PUF
and the 2016 SCF, a small transformer, a rank-to-dollar map, and the
reweighting ("tilt") to 136 IRS targets for 2019 plus SCF wealth targets.
- **Income worked:** the tilt moved the model about as far as it moves
  the real PUF.
- **Wealth came out wrong by a factor of two.** The SCF and the PUF
  measure shared variables differently: the SCF misses small holders of
  interest, dividends and gains. That became the central problem.
- **The fix that worked:** put the SCF's dollars on the PUF's dollar
  scale, and keep a source label so the model learns the SCF's extra
  zeros. That brought wealth within a few percent.
- **Also settled:**
  - draw a record's own source's variables first and borrowed ones last;
  - draw net worth as an SCF household would be asked;
  - state the targets in dollars, not ranks.

**16–18 October: translation, round two.** A "fake survey" (PUF records
made to report the way SCF households do) was built to test translation
rules against a known truth. That led to the **cell method**:
- In cells of wage group by marital status, the PUF sets how many SCF
  zero-reporters of interest, dividends and capital gains are hidden
  holders, and at what sizes.
- The model decides which households they are, during training.
- On the fake survey it removed 80–90% of the error of taking zeros at
  face value.
- Crude non-filers were added to the PUF side, capital gains were split
  into four pieces, and three more SCF waves were added.

**18–19 October: more PUF items, and why rare items came out short.**
- More AGI items were added, and business income was split into its 18
  PUF parts, with the total computed from them.
- The generated files had 10–20% too few holders of rare parts. The main
  cause was dropout, the random switching-off of parts of the network
  during training: rare items were right on average with it on and too
  low with it off, as in generation. Training without it closed half the
  gap.
- The rest comes from the model's answers not fitting one joint
  distribution exactly.
- IRS business targets were added to the tilt, and they absorb what is
  left cheaply.
- Wealth count targets moved to the file's own units.
- The real 2015 PUF turned out to cost no more to reweight to 2019 than
  the synthetic file.

**20 October: this fresh start.** The old folder was archived and the
pipeline rewritten to reproduce the latest run (`FEATURES.md`). Checks:
- the new table builder gave the old training table exactly: every row,
  band, dollar value, band edge and dollar map;
- the new generation and tilt, run on the old no-dropout model, gave the
  latest run's results within noise: effective sample 91.0% against 90.9%,
  median net worth $90,852 against $90,847, every target met. The records
  themselves differ because a different GPU rounds differently;
- the training script ran a 4-pass test; it was not rerun to 80 passes.

**20–21 October: the balance sheet.** Overnight explorations (reports in
`/nfs/roberts/scratch/pi_nrs36/jar335/rebuild/explore/2026-10-20/` and
`2026-10-21/`), then John's decisions of 21 October (`2026-10-21/DECISIONS.md`):
- **Real PUF records with model-drawn net worth** cost the same to tilt as
  the synthetic file and keep the business patterns no target covers; net
  worth comes out the same. Not adopted yet; no decision taken.
- **The 20-item balance sheet in place of net worth alone:** the large items
  came out within about 10% of the SCF with no item targets. Adopted, with
  net worth computed from the items.
- **Mortgages above the home's value** were 6.2% against the SCF's 1.5%,
  4.15 points of it from the model's choice of bands. Adopted: the mortgage
  as a share of the home's value.
- **The SCF's business items:** X5704 is Schedules C and F, X5714 is
  Schedule E. Business income with no business equity in the SCF is
  explained by its answers (mostly self-employed people with nothing to
  sell). Adopted: the two subtotals shared, a shared partnership/S
  corporation/LLC flag, an SCF "owns a business" item.
- **Consistency rules at generation** for rent, partnership and S
  corporation items, mortgage interest and real estate tax. Not for
  dividends (John's call).
- **Learning rate:** peaks of 0.001 to 0.01, two or three seeds each, on the
  balance-sheet table. Higher rates raised the top income group's net worth
  before the tilt toward the SCF's and lowered the bottom group's, at a
  slightly worse held-back fit; 0.003 adopted. A network six times larger,
  50 passes and a fixed drawing order were within seed noise.

**21 October: two slots replace the cell method.** Worked out with John
(`explore/2026-10-21/reporting_formula/`, `two_slot/`, `fake/`; status in
`explore/2026-10-21/STATUS_reporting_formula.md`):
- **The reporting-rate formula.** A zero-reporter's chance of a hidden
  holding = its chance of reporting × (PUF share ÷ SCF share − 1) for its
  type ÷ its chance of reporting zero. Averaging the household answers to
  get the type's SCF share (not asking the model separately) made two
  seeds agree within 0.7 points instead of 3.9.
- **Two slots** (reported and PUF concept) let it run in one pretraining
  without the model learning from its own fill-ins.
- **Real SCF, three seeds each:** the 2019 file after the tilt matched the
  cell method (median, net worth by wage group, tilt move 0.05–0.06,
  about 90% of the sample kept); training took half as long (17 against
  36 seconds a pass). Net worth before the tilt swings by about $10–14T
  across seeds under both methods.
- **Fake survey** (half the PUF filers; taxable pensions in place of net
  worth): the fill-ins recover the number of hidden holders almost exactly
  (interest 70.7–73.0% with none against 71.6% true). Where reporting
  also depends on the survey-only variable (the formula's assumption
  broken), it overcorrected by about 8% against the cell method's 0%.
  The test was weak: taking zeros at face value was off by 3% at most.
- **Careful and careless respondents** (three items jointly) fitted the
  SCF's combinations better than independent coin flips, but was fragile
  household by household and slow; not adopted.

**21 October (evening): the primary filer's age range.** The first step of
`AGES.md`, with John:
- **Step 0** for the PUF records with no age: a logit from the return,
  calibrated to SOI Table 1.6 less the known ages, only in cells where at
  least 10% of the weight is missing. Backing out each cell's residual
  alone fixes the totals but not which records; the logit sorts them
  (Social Security recipients 82% 65+ against 86% true, on a hold-out).
- **The primary filer in survey couples:** the man, except the woman with
  a chance by the man's age that reproduces the PUF's share by range.
- Sex, exact ages and dependents' ages left for later.
- **Result:** no change in the tilt's cost or the medians. Net worth rises
  with age as in the SCF for most of the file, but high earners over 45
  hold too little, most of all at 65+. The model run on the SCF's own
  units gets 75–85% of the SCF's net worth there; the rest is who is in
  the cell: the SCF has 5.7 million units with $200k+ of wages against
  the PUF's 4.1 million (2015), with more capital income at every age
  (`explore/2026-10-21/ages_v1/`, `wages/`).
- **What others do with SCF household members outside the main unit**
  (`explore/2026-10-21/npeu_research.md`): the SCF asks pooled questions
  about their assets and debts; most work drops them or gives them no
  wealth; about 24 million such units in 2019.

**21 October (night): SCF top wages rescaled.** The SCF has 2 to 4 times
the tax data's units with $1M+ of wages in every wave (110–140 households
behind each count); the literature finds the same by AGI. Pre-tax
deductions (the SCF asks wages before any deductions) explain much of the
gap at $200,000 and none above $500,000. A single factor cannot fix it
(the factor needed falls from 0.97 at $100,000 to 0.50 at $1M), so SCF
wages above $100,000 are placed on the PUF's distribution by count from
the top. A modest gain for $200k+ earners at 35–64; none at 65+, where the
SCF's households hold far more capital income than the PUF's at the same
wages and age (`explore/2026-10-21/wages/RESULTS.md`, `wage_scale/`).
Considered and not done: down-weighting the SCF's high earners (shrinks
the SCF wealth totals the tilt targets), wealth targets by age and wage
band (built on the SCF's own wages), wages as a two-slot item.

**21–22 October: the CEX, and making room for more surveys.** Tips were the
first idea and were set aside: they are earned by a person, and the
pipeline has no wages per person yet. Consumption from the CEX was added
instead (`explore/2026-10-21/cex/`, `explore/2026-10-22/`):
- **Tax units from BLS's codes,** as production builds them. The first
  night used 2022–2024 and only households that are one tax unit; then
  all six years John chose (2015 to 2023 every other year, and 2024, as
  2025 is not out), every tax unit, a shared household's spending divided
  by head count.
- **Pensions out of the shared slot.** With Social Security plus pensions
  shared, the file's 65+ spent 17% too much: PUF total pensions include
  rollovers. Gross Social Security alone is now shared; 65+ spending went
  from $44.6k to $40.3k, against $38.0k for the CEX at the file's mix of
  units.
- **Dependent filers** hold no wealth or consumption of their own (John):
  zero on the file, folded into the claiming unit in the CEX.
- **Consumption targets as averages per unit, not totals** (John): with
  totals, the file's extra units (187 million against 153 million) forced
  per-unit spending down and the tilt kept 72% of the effective sample;
  with averages, 89.8%, as without consumption targets.
- **Memos:** splitting a shared household's spending
  (`explore/2026-10-21/cex/splitting/SPLITTING.md`, by Codex); the shared
  variables' definitions (`explore/2026-10-21/cex/translation/TRANSLATION.md`).
- **Speed and memory** (`explore/2026-10-22/perf/`, `PERFORMANCE.md`):
  packing, batched output layers and a faster generator adopted;
  bfloat16 tested and left as a setting.
- **A bigger network** (4.9 million parameters) was no closer to the CEX
  and 3 times slower a pass.
- **Bug fixed:** SOI renamed its wage line in tax year 2022, and the
  table builder read growth factors of zero for 2022 on; it now stops if
  any growth factor is missing.

**22 October (evening): a review with John.** Decisions:
- **Dependent filers are outside the normal process.** They keep their PUF
  items, get no survey item (zero for now), and are left out of every
  comparison of tax units with a survey. Consistency rules no longer apply
  to them, and the fill-in formula's PUF side leaves them out.
- **The universe is always tax units, never filers.** Every comparison of
  a survey with the PUF uses "the tax data" for the survey's year (ROADMAP
  item 2): the PUF scaled to SOI's counts, grown per return, with that
  year's non-filers. Translation becomes two standard functions and one
  table of settings, replacing the per-variable rules (not built yet).
- Wealth counts targeted as shares; estates and trusts out of the business
  targets until the targeting overhaul; the held-back slice no longer
  scores unknown fill-in amounts; the two generators merged into one.

**23 October: the code cleaned up** (a review by a subagent, then John's go):
- one settings file for every stage; the PUF year and the target year set
  once ("years") instead of typed in about 20 places;
- `targets.py` holds every SOI lookup and the one table of business
  families; `fillin.py` holds the fill-in with its quantities named;
  `model.py` every call of the network; training runs packed batches only;
- the generator draws in three named steps; the balance sheet's dollars
  are carried to the target year in the dollar maps, so the file has one
  currency;
- bugs fixed: settings overrides read after the seed and passes; the
  last-resort fallback loosening consumption averages to about 12%; the
  balance sheet's 2016 dollars compared with wealth targets in 2015 dollars
  (1.3%; `FEATURES.md`, post-training).
Checked against the run before it (`explore/2026-10-22/review_fix/STATUS.md`):
the survey units, table, maps, SOI targets and a 4-pass training identical
bit for bit; generation the same draws with weights equal to machine
precision; IRS, business and consumption fits the same to rounding. Wealth
moves only by the 1.3% correction: effective sample kept 90.2% against
90.2%, median net worth $70,408 against $70,376 (seed 20261016).
(`explore/2026-10-22/review_fix/`).

### 23 October: the standardized translation

ROADMAP item 2 built (`FEATURES.md`, translation): the tax data for each
survey year, one table of translation rows, function 1 with a dollar line,
function 2 driven by the table, and a report on every row. Today's choices
carried over: SCF wages with a line at $100,000, the business subtotals at a
line of zero, everything else in dollars. Changes against the run before:
growth per return in place of growth in totals; the tax data reweighted by
Table 1.6 and with non-filers in every comparison; the business subtotals
by count from the top in place of by share of holders; the top-wage map
without its continuity constant; the training non-filers scaled to 2015 by
SOI returns and growth per return in place of the CPI. Found while building:
SOI's net other gains are negative in 2012; partnership and S corporation
income is split in two from 2021; pensions are combined with IRAs in 2018,
so the SCF 2018 growth for retirement income had been Social Security's
alone. Results, two seeds against the previous latest's: every target met;
effective sample kept 89.1% and 89.5% (90.2%, 89.6%); mean net worth at
$200k+ of wages $4.26M and $4.07M ($4.09M, $3.96M; the SCF's $4.36M);
consumption at $200k+ $137.9k and $136.2k ($130.5k, $129.8k; the CEX's
$113.3k). SCF interest, 2018: 87% report none, 72% after the fill-in, 73%
in the tax data. Promoted to latest (`explore/2026-10-23/translation/`).

### 22–23 October: the CPS, and exact ages

**The CPS ASEC as a fourth source** (overnight 22–23 October; `FEATURES.md`,
"The CPS"). Tax years 2015, 2017, 2019, 2021 and 2023, every tax unit, built
with production's ASEC builder, the filing flag from the non-filer pool's
model and calibration. Each shared variable was decided from the
translation report against the tax data: wages and dividends by count from
the top, gross Social Security in dollars; interest, Schedules C and F,
unemployment, pensions and Schedule E not shared (the CPS's interest
holders are twice the returns'; its self-employment holders under half).
CPS-only: exact ages, sex, each spouse's share of wages and self-employment
income, weeks, hours, child care. The rest of the model was no worse (held-
back fit by source unchanged; the tilt about 7% larger). The couples' wage
split came out within 2 points of SOI's W-2 table 1.D by the woman's share
(production's split puts 97% of couples at exactly 0, 50% or 100%).

**Exact ages shared, then latest** (23 October). The primary filer's and
spouse's exact ages (to 79, then 80 and over: the finest top code all three
surveys allow, John's call) became shared by the SCF, CEX and CPS and are
drawn right after the PUF items, so wealth and spending see them. IRAs and
DC accounts now fall from 65–69 to 80+ about as in the SCF; DB balances at
80+ fell further below it. The 2015 CPS filing flag was then calibrated
(84.2% file, against 78.0%), after the latest runs were trained.

**What the comparisons found** (`rebuild_cps/explore/comparisons/`,
`explore/2026-10-23/dep_ages/`, `explore/2026-10-23/tax_units_review/`,
`explore/2026-10-23/cbo_units/`):
- The file has about 86 million children against Census's 73 million. In
  2015, on the real PUF returns, the model makes dependents too young
  (79.5% under 17 against the IRS's 76.2%) and matches the IRS's range for
  the youngest dependent 47% of the time: the PUF's own dependent age
  ranges (AGEDP1–3), its earnings split (EARNSPLIT) and its primary's sex
  (GENDER) were not used. In 2019 the file also has more dependents than
  the tax data, from generation and the tilt. Plan: `AGES.md`, section 0
  and Step 3.
- The CPS tax units come from production's non-filer builder, made to find
  people who do not file. Against the 2015 PUF it finds 18 million fewer
  dependents and half the heads of household. CBO's public algorithm (run
  here on the March 2024 CPS, reproducing CBO's published tables exactly)
  gets head of household close to SOI (20.1 million against 21.6) and finds
  more adult dependents; neither finds the children claimed from outside the
  household. Plan: `TAX_UNITS.md` (one Python tax unit module from CBO's
  algorithm, the filing step kept separate, standing tests for every
  survey).

### 23–24 October: the CPS as its own source, one tax-unit module, latest

**Every source is its own label** (review of 23 October;
`explore/2026-10-23/buildout/PLAN_v2.md`).
- The PUF rows are filers only; the CPS rows are every CPS tax unit, each
  with a filing chance. Training redraws the filing flag from that chance in
  every pass.
- Generation makes two batches: filers from the PUF label, non-filers from
  the CPS label for the target year.
- The fill-in mixes the two labels' answers by the counts of filers and
  non-filers.

**The filing rule** (step 1, `explore/2026-10-23/step1_units/`).
- The CPS units come from CBO's rules: reassignment on, some couples split
  into separate returns.
- Mok's probits and the Pub 5785 hazard were dropped. In their place is a
  rule whose settings are solved each year to fixed counts: non-filing
  adults by age band (CPS adults less SOI adults on returns less dependents),
  children, and Pub 5785's required non-filers.
- **v2** had a flat non-filing rate among units required to file. It gave
  non-filers $823bn of wages.
- **v3** made the rate fall with income, matched to Pub 5785's income share.
- **v4** added business income, matched to Pub 5785's share with business
  income. Required non-filers in 2015, against Pub 5785:
  - wages: 7.4m units and $250bn, against 6.9m and $232bn;
  - business: 5.0m units and $109bn, against 5.0m and $100bn.
- Moved from explore/ into the pipeline on 24 October (stage filing,
  `pipeline/filing.py`), with the same results.

**Reweighting targets by age** (`explore/2026-10-23/age_tilt/`,
`nonfiler_tilt/`, `candidate/`).
- Added: filer returns by the primary's age (SOI Table 1.6, joint and
  non-joint) and people in non-filer units by age (step 1's numbers).
- Both fought the CEX spending targets by age range. With everything on,
  the tilt kept 73% of the effective sample.

**Why: the surveys' tax units** (`explore/2026-10-24/spending_age/`,
`scf_units/`).
- The SCF had 143m units, because the builder dropped 23.6m financially
  independent adults outside the PEU. It also had dependent faults.
- The CEX had 152m units, because BLS makes 4.7m earning adults
  dependents, and it has more married couples.
- One module of CBO's rules now builds the units of all three surveys
  (`TAX_UNITS.md`; `explore/2026-10-24/unified_units/`). The CPS's units
  are unchanged, unit for unit.
- The SCF and CEX each have about 163m units, against about 168m in the tax
  data.
- The consumption targets became averages by age range × married. John's
  rule: no reweighting of a survey's mix; the model learns conditional
  relationships, and weights come only at the end.

**Tolerance on spending** (`explore/2026-10-24/cex_fidelity/`,
`unified_run/`).
- On the CEX's own units the model reproduces spending within the CEX's
  sampling error, about 2–2.6% per cell. Exact cell targets were chasing
  that noise: the tilt kept 42% of the effective sample.
- A 5% band (free inside, a hard limit outside) keeps 96% with every other
  target exact. That run became latest on 24 October.

**24–25 October: targets rederived** (`TARGETS.md`; builds in
`explore/2026-10-24/build/`; the 25 October build's folder was deleted on 26 October).
- The principles were worked out with John one question at a time and
  written in `TARGETS.md`, which opens with the process on one page.
- A configuration of targets (`targets_config/`, `target_list.py`) with a
  standard error on every target (`errors.py`) reproduced the 383 targets
  exactly, then grew to 474: the five top AGI brackets separately, their
  composition, dependent filers (Table 1.7) and SCF top wealth by asset type.
- Hitting every target exactly collapsed the tilt (0.5–2.8% of the effective
  sample kept): the CEX's age × married cells and the non-filer people
  targets could be met together only by piling weight on a few odd records.
  Targets became ranges of one standard error (`solver.py`), and the same
  targets then kept 97.6%.
- Generation became stratified by labels the network learns (AGI class ×
  business flag, net worth class). Dropping records whose drawn amounts
  contradict their label biased the file, so they are kept. The network is
  weak at the top: about 35% short of the PUF's returns at p99.9–99.99.
- One algorithm for every year: the start is a fresh draw or an earlier
  year's file carried forward, with optional targets that hold the
  distribution. 2022 carried from 2019 matched SOI's untargeted bracket
  detail within 3% for returns and wages, not for gains or interest.
- The combined 2019 run (`previous_2026-10-25_strata/tilt/strata_full2019`): all 474 targets within
  range, 99.9% of the effective sample kept, 33 thin-cell stops at the top.

**25 October: cleanup.** The code, settings and documents were brought in line
with the system above, with no change in what it does (the 2019 tilt
reproduces `strata_full2019`). Removed: tilt.py's old target builders
and solver (replaced by `targets_config/`, `target_list.py` and
`solver.py`), the settings only they read (the wealth and fallback
tolerances, the consumption band, the age level, the business AGI groups,
the bracket rule) and the unused CPS separate-returns switch. FEATURES.md's
figures for the runs of 20–24 October are in its git history (commit
bc72cc7).

### 25–26 October: the top of the network, and SCF wealth on the wrong incomes

The scratch record of these two days was deleted on 26 October; what is worth
keeping is here. The code is in commits 157b9e9 (grouped weighting, group-first
generation, the label always shown, batch 512, the wave mix, growth per tax
unit, the income × wealth grid, two age rules) and 5441ad6 (pooled SCF targets,
ranges by kind of target); the decisions are `TARGETS.md` S8 and S9. Two pages
keep the tables: the overnight review
(https://claude.ai/artifact/DRp9vSS2BCYBis7Ejz1XMx) and the 2019 comparison
page, which shows run P3 below (https://claude.ai/artifact/3m7FrTpMnvY8zWegcaFf9C).
Unless said, PUF comparisons are 2015 filers in 2015 dollars, two trained seeds
shown as "a / b".

**The problem: the network underfit the top, and the tilt hid it.** The strata
run of 25 October met all 474 targets, but underneath:
- 200,000 filers drawn with nothing preset held 22 records with AGI of $10M+
  (the PUF: 9,739 records). A record drawn with the label "top 0.01%" had parts
  adding up to a lower AGI 73–80% of the time (the PUF's own records, through
  the same band-to-dollar step: 16%). Partnership and S corporation income at
  $10M+: $22bn against the PUF's $80bn.
- In the 2019 file the top 0.1% by income held $39.5M of net worth on average
  (SCF $65.6M), and 21% of the wealthiest 0.1% had income below p90 (SCF 4%).

**Two causes.**
- **Weights that differ 10–100 times between groups.** Training counted each
  record by its survey weight. A PUF record stands for about 1,400 returns
  below p50, 22 at p99.5–99.9 and 1.6 at p99.99+, so records at p99 and above
  were 29% of the records and about 1% of what the network was fitted to. The
  SCF is the same: its wealthiest 0.1% are 5% of rows and 0.1% of weight.
- **The training loss barely charges small chances.** A 0.1% chance that a
  below-p50 record has a $10M item costs almost nothing in the loss. At
  generation that record carries 1,400 returns, so the small chance becomes
  many top returns. Cutting every band with a chance under 0.1% at generation
  removed only 0.3–1.0% of each draw's chance, yet halved the $10M+ returns
  (52.6k / 66.0k to 23.4k / 28.6k; PUF 18.0k). Only 20% of the returns landing
  at p99.99+ had been drawn for that group.

**Not the fix: size, precision, drawing order.** Twenty models (width × layers
from 128 × 4 to 512 × 8, 32-bit and bfloat16, two seeds):
- bfloat16 matched 32-bit within seed noise and was a third faster (adopted);
- a wider or deeper network lowered the PUF's held-back error at p99.99+ only
  from 2.12 to 2.10, and began to memorise the SCF (held-back SCF error 1.12
  rising to 1.17–1.20); 512 × 8 was worse everywhere;
- fixed drawing orders (largest amounts first, smallest first, one fixed
  shuffle) gave 17–32 records at $10M+ per 200,000, against 22 in random order.

**Grouped weighting (q).** Each training record counts by its survey weight
times q, q = records ÷ weight within its group (12 AGI groups on the PUF, 5
income classes per wave on the SCF, 1 on the CEX and CPS), so each group counts
by its number of records; generation weights each record by 1 ÷ q. The first
version: held-back loss at p99.99+ fell 19%, $10M+ returns came to 0.99 of the
PUF (old weighting 0.71) on 646 effective records (34), and the tilt met every
target keeping 96.8% of the effective sample (old weighting, unstratified: 100
targets unmet, 73.9%). But held-back loss below p50 rose 6%, and SCF net worth
before the tilt was $139.6T against $96.6T.

**Groups from bands, not dollars.** The network sees only bands. With groups
set from exact dollars:
- two SCF households with the same bands but on opposite sides of a class line
  got different q, and the network blended them toward the up-counted one;
- a PUF record's generation weight came from one random dollar pick within its
  bands, and later steps picked again, so the weight could belong to another
  group than the record's final AGI.

Groups are now computed from each band's fixed representative amount (the
weighted mean of the real 2015 PUF amounts in it); 93.5% of PUF records keep
their group. Training and generation use the same function of bands, so 1 ÷ q
undoes the up-counting exactly. Each record's dollar picks are drawn once and
stored, so it has one set of dollars in every step. Result: $10M+ returns 0.96
of the PUF on 6,783 effective records; tilt 97.3% kept. The middle still
slipped (held-back loss below p50 +6.5%).

**Group-first generation and the hidden label: the biggest gain.** The PUF
record's group became a label the network learns. Generation sets it first,
400,000 filers split by the PUF's records per group, each weighted by its
group's returns over the records drawn in it. The first version had 3.3 times
the PUF's $10M+ returns. The cause, found overnight: training hid the label
like any other item, on 51% of PUF rows, while generation always sets it, so
half of training asked the network to predict the group, a total computed from
the parts. Always showing it (one line of code):
- $10M+ returns 52.6k / 66.0k to 23.3k / 21.4k (PUF 18.0k; 3.3 to 1.2 times),
  their AGI $1,128bn / $1,752bn to $514bn / $493bn (PUF $408bn);
- $1M+ returns 693k / 838k to 485k / 510k (PUF 446k);
- partnership non-passive income at p99+ $289bn / $445bn to $228bn / $227bn
  (PUF $200bn); the two seeds now agree;
- the 2019 tilt kept 98.1% (96.9%).

The rule: always show in training any label that generation always presets.

**Drift, and why every drawn record is kept.** Drift is the share of drawn
records whose parts add up to AGI in another group than the one they were
drawn for: 19.3% with the label hidden, 15.4% shown, 13–14% with batch 512.
Between p50 and p99.5 a typical miss is about 21% of the group's AGI line.
Redrawing a drifting record until it lands in its group (up to 20 tries)
fixed the top counts (label hidden: $10M+ returns 1.01 of the PUF, AGI ≤ 0 returns 2.08m
against 2.04m) but kept only records the network draws consistently, which
drops holders of rare and offsetting items: partnership holders below p90
were 1.88m / 1.85m in the redrawn file with the label shown, against the PUF's 2.79m, and 2.87m /
2.83m in the file with every record kept (label shown, batch 512). Kept: every
drawn record, weighted by the group it was drawn for, which keeps the weights
exact for the model's own distribution.

**Training length and batch** (label hidden unless said):
- 320 passes instead of 80: drift 19.3% to 14.9%, $10M+ returns 34.0k / 40.4k
  (2.1 times the PUF); less than showing the label;
- 640 passes: nothing more ($10M+ 39.9k / 46.8k), and held-back SCF loss kept
  rising (1.153, 1.188, 1.214 at 80, 320, 640);
- the end of the learning-rate schedule (a final rate of 0.5% of the peak, or
  80 more passes at the final rate): nothing, or slightly worse (AGI ≤ 0
  returns 4.0m);
- with the label shown, batch 512 instead of 4,096: drift 15.4% to 13.9 /
  12.7%; long-term gains at p50–90 $50.4bn / $47.4bn (PUF $52.1bn); about
  twice the training time. Batch 256 and 128 put more returns at the top
  ($10M+ 1.6 and 2.2 times the PUF, against 1.3) with no lower drift. 512 was
  adopted.

**Closed negatives** (two seeds each unless said, judged on the drawn file):
- *A loss term rewarding parts that add up to the group:* drift 19% to 5%, but
  $1M+ returns 951k / 876k (PUF 446k), S corporation passive holders below
  p90 921k / 1,229k (PUF 300k), tilt 82% kept; three times stronger, $10M+
  returns 13 / 5.8 times the PUF.
- *The ranked probability score in the loss* (charges chance by how many bands
  from the truth): the wealthiest 0.1% with income below p90 rose from 21.9% to
  36.7% (one seed).
- *AGI fixed from a real PUF record of the group, other income the remainder:*
  counts right (AGI ≤ 0 2.02m / 1.99m, $10M+ 16.6k / 17.1k), but other income
  became -38% / -21% of AGI below p50 (PUF -0.7%) and long-term gains at p50–90
  $297bn / $339bn (PUF $52bn). When a total is fixed, the error moves into the
  part left free.
- *No q on PUF rows, once the group is drawn first:* drift at p99.99+ 72–75%
  (24–26% with q).
- *Training on PUF records below p90 only:* middle records still drew $1M+
  items, now with offsetting losses (0.33 / 0.49% of p50–90 returns, against
  0.18 / 0.24%; PUF 0.00%).
- *Chain weighting* (each scored item weighted so training counts every step
  of a draw equally): $10M+ returns 81.5k / 107.3k.
- *Fixed drawing orders:* wages last was modestly better than random order
  ($1M+ returns 608k / 809k against 693k / 838k; wages first 842k / 947k),
  still 1.5 times the PUF.
- *Cutting small chances at generation:* the top came right, but partnership
  non-passive holders below p90 fell from 763k / 797k to 146k / 188k (PUF
  1,104k): an item held by 1–2% of a group spreads its chance over many small
  bands.
- *The running total of AGI, and each shown band's dollars, as inputs:* the
  network read them (a wrong total misled it), but with wages hidden it kept
  the record in its group only 0.3–2.8 points more often (p99+: 82% to 84%);
  drift 18.5% and 19.2% against 19.3%. "It cannot add because it never sees
  dollars" was not supported.

**Held-back loss barely moves; judge by the file.** In every overnight
experiment held-back loss moved under 0.5% while the file's top moved 2–3
times (label shown: PUF 0.4974 to 0.4964, with $10M+ returns more than
halved). On a synthetic table with a known truth, drift fell from 4.0% to 1.1%
while held-back loss stayed at 2.26–2.29.

**SCF top wealth on the wrong incomes.** In the SCF own-households test (real
2019-wave households keep their incomes, the network draws their wealth),
20–22% of the wealthiest 0.1% had income below p90, against 3.9% recorded.
- *Right in sample households, wrong in weights.* In every wave the network
  draws 10–15 such sample households, against 8–15 recorded; but the drawn
  ones are households that stand for 1,700–2,600 units each, the recorded ones
  for 200–700 (the SCF oversamples the rich with small weights). The network
  does not see weights, so it cannot tell an oversampled rich household from
  an ordinary one with similar items. Batch 512 moved the share to 12–14%; no
  training setting fixed it.
- *The wave mix does not fix it.* Drawing the balance sheet from three waves
  (TARGETS.md section 7) left 16.6% before the tilt, against 16.3% from the
  2016 wave alone.
- *Growth per tax unit.* The waves were first carried to 2019-wave dollars by
  one asset-price factor (0.8627 for the 2022 wave). That carries prices only:
  the 2022 wave came to $120.7T, 25% above the 2019 wave's $96.6T, and the
  file's net worth before the tilt rose from $107.5T to $119.9T. Now each item
  is grown by the 2019 wave's mean per unit over the drawing wave's (stocks
  0.64 for the 2022 wave; the mortgage with the home); the 2022 wave comes to
  $98.5T, and the file's start to $112.5T. Every dollar amount is now in
  target-year dollars, grown per unit, before the tilt.
- *The income × wealth grid fixes it in the reweighting.* 22 SCF targets: net
  worth classes at the SCF's lines within four income groups (below p90,
  p90–99, p99–99.9, p99.9+, each side at its own percentiles), shares and
  dollars, cells under 10 SCF households merged. After the tilt the share of
  the wealthiest 0.1% below p90 fell from 12.2% to 6.3%, and net worth at
  p99.9+ of income rose from $6.7T to $9.0T (SCF $10.6T), for 0.6 points of
  effective sample.
- *Top-income wealth sat at the bottom of wide ranges.* The two p99.9+ income
  cells ended at the lower edge of their ranges in every run (above the p99.9
  line: $7.05T against $8.39T ± $1.35T).
- *Pooled SCF targets narrow them.* Computed on the same wave mix as the file,
  their standard errors fell to about 0.65 of one wave's, and the thinnest
  cells rest on 50 households instead of 15.
- *Half a standard error everywhere is infeasible.* 23 non-SCF targets gave
  way (six non-filer people bands, 17 CEX food-at-home and health averages,
  which pull against each other), 31% of the effective sample kept, weight
  piled on non-filer couples with a head near 50 and a spouse aged 19–23. For
  the SCF targets alone, with 1 for the rest (run P3), every one of 496 targets
  was met with 97.75% kept; net worth at p99.9+ of income $9.91T (pooled SCF
  $10.38T; $9.35T with every range at 1), and 3.5% of the wealthiest 0.1% below
  p90 (pooled SCF 2.8%). Adopted (5441ad6).
- *Two age rules.* At half a standard error one generated non-filer couple
  whose spouse was coded aged 16 (counted as a child in the non-filer targets)
  went from 381 to 921,000 units and took the effective sample to 11%. With a
  spouse 18 or over on every record, it was 43%: next, 14 non-filers living
  alone with a head aged 17, who fall in no people band, carried 1.08m units.
  Rules: a spouse is 18 or over; a non-filer unit's head is 18 or over. The tilt
  now stops if a non-filer head or spouse falls in no people band, or in two.

**Latest since 26 October** (`FEATURES.md`, "How the latest run comes out"): the
pipeline of 157b9e9 and 5441ad6, retrained and redrawn (all 496 targets met,
98.0% of the effective sample kept). The strata run of 25 October moved to
`previous_2026-10-25_strata/`.

**Open items.**
- *Too many returns with AGI ≤ 0:* about 3.1m in the overnight runs (2.85m in
  the run now latest) against the PUF's 2.04m. With the label shown, the excess
  is mostly small records drawn for below p50: 43% with AGI between -$1k and $0,
  about 465k with every item zero. John chose not to pursue a rule for
  all-zero records.
- *One record in the latest stack moves about 2,600-fold:* labelled in the top
  AGI group, its parts give AGI of -$0.83M and net worth of -$30M (weight 0.81
  to 2,091). Its share of the squared weights is negligible.
- *Generation reproduces only on the same GPU type:* the same seeds on an L40S
  and an H200 differed on 446,575 of 454,379 records. Pin the partition when a
  draw must be repeated.
- *Too little wealth for top earners below the top wealth line:* units at p99.9+
  of income with net worth at or below the p99.9 line start at $1.58T against
  a target of $2.07T and end at the range's lower edge ($1.89T).

### 26–27 October: tips, auto loan interest, more targets, the other PUF items

Built overnight in three stages, each checked before the next began; merged on
27 October (5d2a637) and installed as latest (the run before is in
`previous_2026-10-26/`). The results pages, the Table 1.3 and 4.3 experiment's
page and the overnight log are in
`/nfs/roberts/scratch/pi_nrs36/jar335/rebuild/explore/2026-10-27/`. Nominal 2019
dollars unless said.

**Stage 1: tips from the SIPP, auto loan interest from the SCF.**
- The SIPP became a fifth source with its own label: reference years 2015,
  2017, 2019, 2021 and 2023, about 128,000 tax-unit rows built by the shared
  rules. It shares married, dependents, the age range, the exact ages and wages
  with the PUF (wages by count from the top), and the spouse's share of the
  couple's wages with the CPS. Its own slots: each adult's tips as a share of
  that adult's wages, and whether those tips are mostly from leisure and
  hospitality.
- Auto loan interest from the SCF, every wave: the balance owed on each of up
  to four vehicle loans times its rate (Tedeschi's rule).
- With the 496 targets of the run before (neither item targeted): tips $34.0
  billion on 3.67 million units (SIPP 2019: $33.6 billion on 3.76 million); auto
  loan interest $45.9 billion (SCF pooled: $43.2 billion, about $900 per holder
  in both). Targeted PUF items moved under 2%; 97.5% of the effective sample
  kept (98.0% before).
- Found: the SIPP has about 60% of the W-2's tipped workers (3.77 million
  people against 6.59 million) at a higher average. Primary filers with their
  own wages over $200,000 got $2.7 billion of tips (SIPP: $0.06 billion): a tip
  share drawn for a few top earners multiplies large wages.

**Stage 2: 250 targets added, 746 in all** (the 496 unchanged):
- SOI Table 1.3 (184): returns, AGI and 22 income items in each of the four
  filing statuses;
- SOI Table 4.3 (54): returns, AGI and 8 income items in the top 0.01%, 0.1%
  and 1% of AGI;
- Form W-2 box 7 tips for 2019 (4): returns with tips and the amount, joint and
  non-joint, the file's tips held to the Social Security wage base as box 7
  holds them;
- SCF auto loan interest (8): the share with any and dollars per unit in the
  four income groups of the wealth grid.

Reweighting stage 1's records to all 746 (run S2) met every target with 87.7%
of the effective sample kept. Tables 1.3 and 4.3 cost about nine points, tips
about one. To reach the W-2's 6.5 million tipped returns the tilt raised the
weight of tipped records 1.66 times on average; tips rose to $47.6 billion (W-2:
$42.4 billion, filers only). Table 1.3's refund targets cut state income tax
refunds from $27.4 billion to $5.7 billion (SOI: $5.6 billion), and mortgage
interest paid fell 13% with them.

Later on 27 October, seven more tips targets (753 in all) went into the latest run: tipped workers on joint returns (Table 4.B)
and the share of tipped workers above six of Table 4.C's percentile lines. The tilt alone was rerun on the same records; all 753
are met, the effective sample kept went from 87.3% to 87.2%, and the file's tips per worker moved toward the W-2's (median
$3,531 to $2,789, against $2,444).

Tables 1.3 and 4.3 were first tested on the run before's records
(`Tax-Data-targets`, branch `rebuild-targets-13-43`; TARGETS.md S10):
- Table 1.3 alone: every target met, 88.0% kept.
- Table 4.3 with its top 0.001% (1,482 returns): 15 targets could not be met.
  John: the top 0.01% as one group until the PUF's aggregate records are
  unpacked.
- Table 1.3's capital loss is the loss allowed on the return (at most $3,000).
  Measured on the file without that cap, the file's losses were $43.8 billion
  against SOI's $16.3 billion; with the cap, $14.8 billion. The target now
  measures the allowed loss. Generated records still carry 2.6 million net
  losses past the cap; John: the item is definitional, no generation fix.

**Stage 3: the PUF items the model did not draw.** 40 PUF-only slots under
production's names: qualified and other dividends, the eleven statutory
adjustments, two capital gain items, partnership income subject to
self-employment tax, nine itemized deduction items, two AMT items, eleven
credits and two other taxes. Ordinary dividends and the adjustments are now
computed from these parts. Both models retrained (73 minutes each on an H100, a
pass 9% longer), a new stack drawn and reweighted to the 746:
- every target met, 87.3% of the effective sample kept;
- before the tilt the top slightly worse: returns with AGI of $10 million and
  over 1.44 times the PUF's, against 1.33 with stage 1's models, about what a
  new pair of seeds gives;
- after the tilt 31 of the 40 new items' totals are within 15% of the 2015 PUF
  grown to 2019. The largest departures: the foreign tax credit 65% high
  ($36.0 billion against $21.8 billion, both in 2015 dollars, since the item has
  no SOI line to grow it), with $4.8 billion at AGI under $50,000 against $0.1
  billion; the AMT net operating loss adjustment spread up the income scale
  ($15.1 billion at AGI of $1 million and over against $0.6 billion; on the PUF
  almost all of it is at AGI below $1);
- itemizers fell 13% in the tilt (46.3 million returns with an itemized item
  before, 40.4 million after; the PUF grown, 46.6 million): Table 1.3's refund
  targets cut the weight of itemizers with a state refund to about a fifth.

**Decided (John, 27 October).**
- Stage 3 is kept. The foreign tax credit and AMT problems wait for the tax
  calculator.
- The itemizer drop waits for the calculator; itemized deductions are revisited
  then.
- Deductions the law removed are zero in the years after repeal: domestic
  production activities from 2020, tuition and fees from 2021. Not built yet;
  until it is, a target year from 2020 on stops at the dollar maps, which find no
  SOI line to grow them by.
- other_item_exp stays as the PUF has it, including its negatives, which come
  from the PUF rounding each item separately.
- Tips on earners over $200,000, and whether to draw more tipped workers instead
  of raising their weights: the W-2's spread of tips per worker (Table 4.C) was
  tested as reweighting targets and adopted on 27 October (see above); the two
  questions stay open (`ROADMAP.md`).

### 27–28 October: wages and deferrals, the survey mappings, the year mix, overtime, three seeds

Planned on 27 October (late) as four pieces, each built on its own branch and checked at the table stage (no
training), then one big run. Overtime joined on 28 October. Installed as latest on 28 October (merge d9bd762 of
`rebuild-ot3`, then d56c75d; the run before is in `previous_2026-10-28_before_bigrun/`). The write-ups are in
`/nfs/roberts/scratch/pi_nrs36/jar335/rebuild/explore/2026-10-28/`, named below. Nominal dollars of the income year
for the surveys, nominal 2019 dollars for the file.

**Tips in dollars** (branch `rebuild-tips-dollars`, decided 27 October; `explore/2026-10-27/tips_dollars/`). Each
adult's tips are drawn in dollars rather than as a share of wages, then held to that person's wages. Tested on the
models of the day: tips on units with $200,000 or more of wages $1.2 billion against $5.9 billion with shares (SIPP
$0.9 billion); before reweighting $30.3 billion against $53.7 billion (SIPP $33.6 billion); all 753 targets met,
87.1% of the effective sample kept against 87.2%. The cap takes off about 10% of drawn tip dollars. Folded into the
big run rather than installed alone.

**Wages and deferrals** (`rebuild-wages-deferrals`, 515ca78 and 9694c14; `wages_deferrals/RESULTS.md`).
- Each survey's link wage is its gross wages less the traditional elective deferrals it reports (the SCF; the SIPP
  in 2021 and 2023), mapped onto the PUF's box 1 wages.
- The first mapping, each survey earner given the tax data's wage at the same percentile among earners, moved wages
  by 9-19% rather than the 3-8% expected (SCF 2018: -11%), because every survey has 1.5-8 points fewer units with
  wages than the tax data. Worked example, SCF 2018: its median earner's link wage of $42,610 became the tax data's
  median, $38,992. John decided: fill in the surveys' missing small earners first (function 2 on wages), then map by
  percentile. Built as the match `percentile_from_top`: each earner keeps its share of all units counted from the
  top, and the fill-in supplies the bottom.
- SCF deferrals, each person's held to the income year's legal limit and to their own wages: $330.5 billion in the
  2019 wave against the W-2's $332.5 billion for 2018. Before the limit the SCF's mean per contributor was 2-2.4
  times the W-2's from $500,000 of wages up; after it, within about 3%.
- John's seven deferral decisions of 27 October (the income year's limit; the SIPP also loses the Roth share and is
  held to the limit; each person's deferrals at most their own wages; a second slot for the spouse's share; none
  without wages at generation; each person held to the target year's limit; W-2 Table 2.D targets with a
  dependent-filer allowance) were built on 28 October: 24 targets in eight AGI groups, kept by John.

**The survey mappings** (`rebuild-mappings`, 1281d02 and 69dde7a; `mappings/RESULTS.md`, `scf_social_security/`,
`scf_schedule_e/`). The audit of 27 October, approved survey by survey:
- function 2 (the fill-in) runs on every survey, not only the SCF: wages everywhere; SCF Schedules C and F,
  tax-exempt interest and both Schedule E parts; CEX self-employment; CPS dividends, long-term gains, pensions,
  unemployment and Schedule C; SIPP dividends and pensions;
- a dollar line above zero maps the amounts above it by percentile among the units above it (count from the top had
  broken the order of units just above the line);
- SCF Social Security alone, from the benefits section (current benefit, SSI left out), in place of Social Security
  plus pensions: 25.8% of units with any against the tax data's 22.9%, $890 billion against $858 billion (2019
  wave against 2018);
- SCF Schedule E split into rental income (the property section's net income plus rent from part of the home or
  farm) against the PUF's rents and royalties, and the rest against the PUF's other Schedule E parts. The business
  section's profit share was not used: it is book profit, $1,845 billion against the tax data's $806 billion of
  partnership and S corporation income;
- CPS survivor and disability pensions paid on Form 1099-R moved into pensions ($50.5 billion on 3.39 million units
  in 2019); SIPP dividends, gross Social Security and pensions shared for the first time.

A 5-pass training test found that the fill-in barely moved the CPS, because a total built from the reported part
(Schedules C and F from the reported Schedule C) gave the answer away; hiding such totals took CPS Schedule C holders
from 7.9% to 16.3% (tax data 19.9%). After 80 passes in the installed run (model of seed 20261017, share of units
with any, main year):

| Survey and item | Reported | After the fill-in | Tax data |
|---|---:|---:|---:|
| SCF wages | 72.3% | 77.6% | 77.0% |
| SCF interest | 11.6% | 24.9% | 26.0% |
| SCF Schedules C and F | 10.7% | 17.7% | 20.7% |
| SCF rental income | 6.4% | 9.7% | 7.6% |
| CEX self-employment | 11.4% | 22.6% | 22.0% |
| CPS wages | 73.8% | 74.1% | 77.3% |
| CPS Schedule C | 7.1% | 15.6% | 20.7% |
| CPS pensions | 15.8% | 16.7% | 24.4% |
| SIPP wages | 69.5% | 78.3% | 77.1% |
| SIPP dividends | 14.0% | 20.5% | 19.5% |

CPS wages are not filled in: 88.6% of its units without wages say nobody worked any week (left as is, John).

**The survey year mix** (`rebuild-year-mix`, 31bf8a9 and c70481c; `year_mix/RESULTS.md`). Each record draws its CEX,
CPS and SIPP year from the two years nearest the target year, weighted by distance (2019: 73.11% 2019, 26.89%
2017), as its SCF wave already was; each year has its own dollar map, and the CEX consumption targets average the
two years. Tested on the tips-in-dollars models with no training: all 753 targets met, 86.1% kept against 87.1%;
the consumption targets' standard errors fall 22% at the median, and targets held at an edge rose from 502 to 525.

**Overtime** (`rebuild-overtime`, 5375112 and 24252df; `overtime/RESULTS.md`). Each CPS worker's overtime pay by
Ernie Tedeschi's rules (coverage, exempt occupations and industries, the duties test above the salary threshold,
time and a half for hours over 40 last week, times weeks worked), only for the workers asked the earnings
questions. The weekly pay the March 2024 ASEC lost for its month-4 group is recovered from the March basic file for
99.8% of workers. 2019: 13.3 million workers, $255 billion; 2023: 12.0 million and $244 billion against Tedeschi's
12.6 million and $272 billion, the gap above $200,000 of pay. Decided by John on 27 October: the whole overtime pay
under Tax-Simulator's names (`ot1`, `ot2`, `ot`), Tedeschi's rule on varying hours, hours at the main job, the tax
year's threshold, 12 CPS targets (share with any and dollars per unit, all units and five wage groups), each
person's overtime held to their wages less their tips.

**The big run** (`explore/2026-10-28/bigrun_full/`, branch `rebuild-bigrun-1028`; `bigrun_full/COMPARISON.md`). The
first four pieces, seeds 20261016 and 20261017. All 777 targets met (753 plus the 24 deferral targets); effective
sample kept 79.7% against the latest's 87.2%; KL move 0.139 against 0.098.
- **The seed check.** Each model drawn and reweighted alone kept 77.0% (seed 20261016) and 80.9% (seed 20261017).
  Before reweighting, the two models' target values differed by a median 15%; the big run and the latest by 6.5%, so
  most of the change was within training chance. But seed 20261016 was the worse model on every kind of target
  (median miss before reweighting, Table 1.3: 28.0% against 11.0%; SCF wealth 31.7% against 8.1%), with almost the
  same held-back loss (0.7063 against 0.7049).
- **One record of other debt.** A PUF-labelled single filer with $52,600 of wages and $34.6 million of other debt
  had its weight raised 38.6 times and carried $0.92 trillion of other debt: the cheapest way for the tilt to take
  $1.7 trillion off the wealth grid cell "net worth below p90, income below p90". The file's other debt was $1.10
  trillion against $0.30 trillion in the latest.

**The overtime run** (`bigrun_ot/`, the same plus overtime, both seeds retrained). All 789 targets met; 79.2% kept;
overtime after reweighting $241 billion, 7.8% of units with any (CPS year mix: $247 billion, 7.6%). The cap took
$14.1 billion of $256 billion drawn through wages and $0.7 billion more through tips. The other-debt record
repeated (another record, $7.4 million, weight raised from 686 to 30,549, $227 billion), and seed 20261016 was again
the worse model (76.7% kept alone against 80.4%).

**Why seed 20261016 was worse** (`scf_bug/training/RESULTS.md`, `scf_bug/generation/RESULTS.md`). No bug:
- both trainings use the same records and held-back slice; only the random draws differ;
- swapping generation seeds moves net worth before reweighting by about $3 trillion, swapping networks by about $34
  trillion, so the cause is the trained network, not generation;
- network 16 draws too many filers with capital and business income for a given AGI group (dividends 21.6% against
  the PUF's 18.4% and network 17's 18.7%; Schedule C 17.6% against 16.4% and 14.6%), and the balance sheet follows
  those items; on the same records it also draws about 6% more wealth than network 17.

**How much networks vary** (`model_variability/RESULTS.md`; 14.2 GPU-hours):
- of four seeds, three are close (net worth before reweighting $105.1, $105.6 and $111.0 trillion against the SCF's
  $97.1 trillion) and seed 20261016 is the outlier ($139.7 trillion);
- the held-back loss does not pick out the bad network (all differ in the third or fourth decimal place). A check of
  the share of filers with seven income items in each Table 1.4 AGI class against the PUF does: median miss 0.56 to
  0.99 points for the good seeds, 1.69 for seed 20261016; its rank correlation with net worth is 0.77;
- the networks saved at passes 20, 40 and 60 of the 80-pass run are worse on every check than at pass 80;
- 8 layers in place of 4 moved both seeds toward the SCF but did not fix the bad seed, at 45% more time a pass;
- the same seed, data, code and GPU kind reproduce a network exactly, weight for weight.

**The three-seed run** (`bigrun_ot3/`, run `seeds17_18_19`; John, 28 October). Seed 20261016 dropped; seeds
20261017, 20261018 and 20261019 pooled, a third of the records each. All 789 targets met; 81.2% kept; KL move 0.131;
net worth $105.6 trillion before reweighting, $97.2 trillion after. Median miss before reweighting against the
latest: IRS 7.2% (9.0%), Table 1.3 12.8% (13.8%), business 16.2% (20.7%), SCF wealth 9.0% (14.3%), tips 35.1%
(23.3%, tips now in dollars). The other-debt record appeared again: a CPS-labelled record aged 71 with $53,000 of
wages and $15.4 million of other debt, weight raised from 373 to 17,545; other debt $472 billion after reweighting
against $189 billion before.

**The other-debt cap** (54c4f27; John, 28 October). The SCF's largest other-debt amounts ($15-69 million) reach
ordinary records through the dollar maps. Each wave's map now holds other debt to the 99.9th percentile among the
wave's holders: $560,000 (2013 wave), $900,000 (2016), $500,000 (2019), $2.0 million (2022). The training table and
its bands are unchanged, so the three models were kept; the stack was redrawn and reweighted
(`explore/2026-10-28/bigrun_ot3cap/`). Other debt is $140 billion after reweighting against $135 billion before.
All 789 targets met; 81.3% kept; KL move 0.131. This run was installed as latest.

**The calculator experiment** (branch `rebuild-calculator`, not installed; `calculator/RESULTS.md`). Every record
of the big run's and the overtime run's files through the GPU tax calculator under 2019 law: taxable income 3.2%
above SOI and income tax before credits 5.3-5.5% above, the excess from AGI $500,000 up; 36 of 165 inputs still 0.
Its open questions are in `NEXT_STEPS.md`.

**Decided (John, 27–28 October).**
- Tips in dollars; the wage mapping (fill-in, then percentile); the seven deferral decisions; the survey mappings;
  the year mix; overtime with its names, cap and targets; all in the big run.
- The CPS wages fill-in and the young tax units left as is.
- Seed 20261016 dropped; three seeds pooled (20261017-19); the default training seed is 20261017, so the default run
  name is the installed latest.
- SCF other debt held to the 99.9th percentile among holders in each wave's dollar map.
- Tips on units with $200,000 or more of wages, an open question since 27 October, came down with tips in dollars:
  $1.0 billion in the installed file against the SIPP year mix's $0.8 billion.
- To do: a fill-in on the SIPP's tips, a joint-return deferral count target, screening each seed alone before
  pooling, and the calculator's open questions (`NEXT_STEPS.md`).

### 28 October: the non-microdata imputations

**Built on branch `rebuild-nonmicro`** (not installed; scratch `explore/2026-10-28/nonmicro/RESULTS.md`). John decided
the design question by question (`NEXT_STEPS.md` item 1, Q1-Q9). One module, `non_micro_imputations.py`, runs inside
the tilt stage on the start: dependent filers' exact age and sex from the CPS's earning dependents, child credit
eligibility, death probabilities, yearly unrealized gains at each year's Z.1 revaluation rates, cost basis, the
holding period and basis of long-term gains, parents' rank; placeholders for dependents' Social Security numbers,
blindness and the QBI items. On the latest run's stack the tilt gives the installed weights exactly and meets all 789
targets; the checks are reported in `report.md` section 6.

### 29–30 October: targets for every year (overnight, piece 4)

Year folders 2015-2035 built and run on the installed networks (branch `rebuild-targets-all-years`; scratch
`explore/2026-10-30/targets/RESULTS.md`). 2017-2023 met every target, with 95-48% of the effective sample kept
(lowest in 2020 and 2023, where Table 1.3's taxable state refunds start 9-12 times SOI's after the 2017 law); 2015
and 2016 met all but 27 targets, a conflict between Table 4.3's top 1% count and Table 1.4's top brackets. 2024-2035
chained from 2023 by CBO's growth, every target met. Found on the way: Table 4.3 prints some net losses as negative
numbers (a negative target made the 2020 tilt diverge); the March 2016 ASEC has negative replicate weights.

### 30 October (overnight): taxes computed inside the model

**Built on branch `rebuild-taxes-in-model`** (not installed; scratch `explore/2026-10-30/taxes/RESULTS.md`). Piece 3 of
the overnight plan John approved on 29 October. Every record's taxes from tax-simulator-gpu's new batch command (its
local branch `taxdata-batch`) inside the tilt, before the reweighting; the QBI dials solved first (a = 3.00, b = 3.66:
implausible wage bills, flagged); 176 calculator targets from SOI Tables 1.4 and 3.3. With all of them the 2019
targets could not all be met (58.4% kept); leaving out the QBI deduction by AGI, the costliest block, every target is
met at 78.0% (80.0% without calculator targets). Decisions 20-25 of `explore/2026-10-30/DECISIONS_FOR_JOHN.md`.

### 29-30 October: Tax-Simulator's names everywhere

John (29 October): "there is no reason for the names to differ. at all." The rebuild's variables took the input names of
tax-simulator-gpu (50 renamed, for example `mars` to `filing_status`, `interest` to `txbl_int`, `w_cash` to
`value.cash`), by a script from one table (`pipeline/tools/rename_map.csv`, `apply_rename.py`), on branch
`rebuild-names-gains`. This file keeps the old names in the entries above. The training table and a 2019 draw and tilt
from the installed networks were checked equal before and after, apart from the names (`FEATURES.md`, "The names").

### 30 October - 1 November: every year from one branch, and one cost for the reweighting

Decisions 40-76 of scratch `explore/2026-10-30/DECISIONS_FOR_JOHN.md`; the running log is `explore/2026-10-30/STATUS.md`.
- 30 October: SCF targets for nine balance sheet categories and two net worth classes, with each category grown by its
  Financial Accounts series after the last wave (`explore/2026-10-30/wealthcat/`); calculator and Table 2.1 targets
  in every past year; projection years wired to population cells and CBO growth per return; the refund rule, the
  merged groups under $50,000 and the greedy Schedule A step (decisions 47-49).
- 1 November, the reweighting redesigned (John; decisions 57, 62, 68, 73): one cost minimized in one Newton solve,
  no ranges and no target dropped, tau 1,000, importance 1 for IRS figures and 2/3 for the rest, a soft cap on each
  weight at 10 times the mean starting weight of its AGI group, at most 100 Newton steps (never raised; a slow solve
  means a wrong target). The range solver, the hard caps and the factor limits were removed
  (`explore/2026-11-01/single_cost/`, `only_solver/`).
- 1 November: the dry run of all 21 years (`explore/2026-11-01/dryrun/RESULTS.md`), then the fixes it called for
  (`fixes/RESULTS.md`): the refund rule keeps totals equal to their parts, CBO's counts anchored to the file, target
  consolidation (852-919 targets a past year down to 737-804), and every projection year started from the 2023 file
  rather than chained (decision 74). Then the projection years' SCF targets grown from 2023 like the start (decision
  76; `networth/RESULTS.md`): net worth misses fell from up to 15 standard errors to 0.5-1.0.
- Night of 1-2 November: tax expenditures 2025-2029 on the rebuild's and production's files against JCT and OTA
  (`explore/2026-11-01/tax_expenditures/report/REPORT.md`); the real-PUF start (`puf_start/`, branch
  `rebuild-puf-start`, not merged); behaviour-identical simplifications (`simplify/`).

### 2 November: findings, fixes and one integration branch

Each folder is under scratch `explore/2026-11-02/` and has a RESULTS.md.
- `social_security/`: after 2023 gross Social Security had grown with CBO's taxable benefits. Fixed: SSA's benefits
  paid for 2024 and 2025, CBO's Social Security outlays from 2026; gross benefits targeted in SOI years. The 2025 file's
  gross benefits fell 14%.
- `dependent_filers/`, `dependent_filers_fix/`: dependent filers' ages came from the CPS dependents earning above the
  filing threshold, 64-83% aged 18. Now drawn from the CPS dependents who file their own return (18 is 37-46% of the
  18-25 range); five more Table 1.7 cells targeted (dependent filers' net capital gain, returns and gains at AGI
  $50k-200k and $200k and over), which removed most of the dependent filers' gains above $10 million of AGI.
- `dep_std_deduction/`: the dependent standard deduction in tax-simulator-gpu is right; its personal exemption is given
  to dependent filers, against the law. Fix approved for simulator master.
- `stepup/`: step-up at death on the rebuild, once dependent filers were left out of the gains model's age cells
  (simulator branch `te-stepup`): fiscal 2026 $26.0 billion, production $33.0 billion, OTA $40.3 billion, JCT $72.5
  billion.
- `kg_scf_2022/`, `kg_old/`, `kg_split/`, `ages_cps_scf/`, `young_split/`: the rebuild's units 80 and over held $3.2
  trillion of unrealized gains in 2022 against the SCF's $5.5 trillion, and units under 55 about $10 trillion more net
  worth than the SCF. About 60% of the 80+ shortfall came before the reweighting (the network's gain share is too flat
  by age; exact ages drawn under the CPS label) and about 40% in it, mostly business wealth on low-income records,
  which the wealth grid removes. The excess under 55 is all before the reweighting. No wealth target had an age.
- `age_targets/`: SCF categories and unrealized gains by the primary's age, and the CPS's units by age within 65 and
  over, as targets in every past year (John approved counts over shares). 2022: net worth under 55 $40.2 trillion
  (SCF $39.8), 80 and over $11.7 trillion (SCF $11.7); gains at 80+ $4.47 trillion (SCF $4.53); 3-7 points of
  effective sample.
- `billionaires/`: Forbes's lists for every year 2015-2026, the 2026 list grown after 2026. Left free after 2026, the
  solver cut the ten richest to weights near zero; held at their starting weight since, as a placeholder.
- `estate_heirs/`: Tax-Simulator's heir data is joined by production's record ids, so on the rebuild's files it belongs
  to unrelated households; tax expenditures run without it. Inheritances are to be drawn by the network later.
- `projection_design/`: John called the projection-year targeting a disaster; the first pieces of a design without a
  solver are agreed (`NEXT_STEPS.md`, item 1).
- `integration/`: the day's branches merged as `rebuild-integration-1102` (rebuild-ss-fix, rebuild-dep-filers,
  rebuild-age-targets, rebuild-billionaires-all-years with rebuild-te-files, rebuild-simplify), the unused setting
  `tilt.fixed_columns` removed, the post-2026 billionaires held, and all 21 years rerun on the existing draws.
  `rebuild-puf-start` left out (John has not decided). Past years reproduce `rebuild-age-targets` apart from the
  lists (2019, same list, identical to the last digit); every year converged within 100 steps, but 2023 took 99 and
  ran out of steps on a second node, so the merge waits for John (`NEXT_STEPS.md`).

### 3 November: the step budget raised to 150

- The night of 2-3 November (scratch `explore/2026-11-03/`, `MORNING.md` there): `rebuild-integration-1102` merged into
  `rebuild-joint-model` with the 2023 Forbes list off (c526b34; 2023 identical to the 2 November control, 2024-2035 in
  11-14 steps). The slow 2023 solve with the list traced to the soft cap meeting Table 4.3's tight top 0.001% cells,
  which the list fills a quarter of; no target is wrong, and the step count varied from 72 to 100 with the processor
  count alone (`forbes2023/RESULTS.md`).
- John raised the Newton budget from 100 to 150 and switched the 2023 list back on (branch `rebuild-budget150`): 2023
  converged in 92, 99 and 103 steps on 1, 4 and 16 processors, 2024-2035 in 12-14 (`budget150/`). No further raise
  without John.
- tax-simulator-gpu master pushed with the AMT senior deduction fix, car loan interest below the line and the QBI
  income limit net of the Schedule 1-A deductions; the R Tax-Simulator's fixes on its branch
  `fix/senior-deduction-amt-itemizers` (`simfixes/`, `simfixes2/`).
- The projection-year redesign begun with John (`explore/2026-11-02/projection_design/DESIGN.md`; `NEXT_STEPS.md`,
  step 4).
- Social Security and pensions (branch `rebuild-pensions`, for the next retrain; `explore/2026-11-03/pensions/`). The
  2023 file had gross pensions below zero on 14.8 million units and taxable pensions above gross pensions on 30.4
  million, because `retirement_gross` and `gross_ss` were drawn and turned into dollars apart (`pairs_C/RESULTS.md`).
  John chose to change the slots rather than patch the draw: the network draws gross Social Security, taxable pensions
  and the non-taxable rest of pensions; gross pensions and taxable Social Security (the section 86 worksheet) are
  computed, and taxable Social Security is no longer targeted (FEATURES.md).

### 3 November (overnight): the 2025 SCF wave and inheritances received this year

Scratch `explore/2026-11-03/scf2025/` (RESULTS.md, DECISIONS.md), branch `rebuild-scf2025`. The 2025 SCF (released
October 2026) is added as a fifth wave; inheritances received this year become an SCF-only slot, each reported
inheritance counted with the chance its rounded year code carries (Gale and Sabelhaus's rule; 0.5 for the 2025
wave's code 2025). The three networks were retrained on the new table and 2019, 2022 and 2023 drawn and reweighted.
The 2025 wave shows twice the inheritances of the earlier waves ($1.09 trillion in 2024 against $0.55-0.59 trillion
in 2018 and 2021), mostly from about a dozen households reporting $1-6 million.

### 3 November (evening): the day's branches merged into `rebuild-joint-model`

John approved each merge (scratch `explore/2026-11-03/`, one RESULTS.md per piece):
- **The projection years' design** (`rebuild-addfactors`, with `rebuild-proj-a`, `-proj-b`, `-itemcbo-base` and
  `-proj-noitem` under it; `proj_A/`, `proj_B/`, `noitem/`, `addfactors/`). Each year 2024-2035 starts from the 2023
  file: weights calibrated to people by age and marital status, one dollar factor per item from John's series, then
  the main reweighting. No itemizer count target; the AMT payer count is kept. Medical expenses grow with AGI per
  return times CBO's PCE medical price over its PCE price. Add factors on the Schedule A items: +2% a year in
  2024-2028, +1% in 2029-2033, none after.
- **Social Security and pensions** (`rebuild-pensions`, `pensions/`): the network draws gross Social Security,
  taxable pensions and the non-taxable rest; gross pensions are the two parts added, and taxable Social Security is
  computed by the section 86 worksheet. Broad income no longer counts Social Security twice; the Forbes records' IRA
  distributions are no longer in their pensions. The projection items table follows: no row for taxable Social
  Security, and a row for the rest on CBO's pensions line in place of `retirement_gross`'s.
- **Two small fixes** (`rebuild-small-fixes`): other itemized deductions held at zero from below in the table. Its
  broad income fix was covered by the pensions branch, whose version was kept.
- **The 2025 SCF and inheritances** (`rebuild-scf2025`, `scf2025/`), as built overnight; John decided the 2025 wave
  goes in as it is, with no adjustment to its inheritances, and the top left as the SCF has it. The conflicts with the
  later work were resolved in its favour; the projection items table got a row for the inheritance item (grown by its
  growth.yaml rule, not targeted).
- **The RA's check tool** (`rebuild-tabtool`, `check_tool/`), outside the pipeline.
- **The full run on the merged branch** (3-4 November, `bigrun/`): three networks retrained on the new table, every
  year drawn again with a fresh top cache, all 21 years reweighted. Every year converged (2015-2023 in 36-89 Newton
  steps, 2024-2035 in 5-13); no break in the pension and Social Security pairs; tax after credits 0.981-0.989 of CBO
  in 2026-2035. One fix on the way: the top step's copy of the other itemized deductions rule lacked the small fix's
  floor at zero, and its check stopped every year's top build (2a42643).

### 3-4 November (overnight): a search over the pretraining settings

Scratch `explore/2026-11-04/netsearch/` (RESULTS.md, SCORE.md, DECISIONS.md, results.csv), branch `rebuild-netsearch`
(18f1363, not merged; every idea is a setting whose default is the installed behaviour, and with the settings off the
code reproduces the big run's networks weight for weight). Six ideas, each trained on two seeds (20261017, 20261018)
and scored by one number fixed before any network was scored: the mean absolute log miss of the file before
reweighting against the truth, each source weighted equally (PUF 2015 shares, dollars and top counts by AGI class and
how often a record's parts land in its labelled AGI group; SCF 2019 net worth by income group and the top 0.1%'s
income mix; CEX consumption means; CPS family and work items). The installed settings on five seeds scored 0.108,
0.119, 0.121, 0.125 and 0.130 (mean 0.1205, standard deviation 0.008); a win needed a two-seed mean below 0.1124.
No idea won; John closed the search on 4 November.

| Idea | Two-seed mean |
|---|---:|
| Average of the weights over the last 10 passes | 0.1129 |
| Moving average of the weights from pass 50 | 0.1134 |
| One output layer per source (in place of one shared across sources) | 0.1139 |
| Width 256 (50% more time a pass; best PUF part, worse SCF part) | 0.1156 |
| Peak learning rate 0.0015 / 10-pass warm-up / peak 0.006 | 0.1167 / 0.1181 / 0.1188 |
| Installed settings (five seeds) | 0.1205 |
| Peak held, then straight-line decay | 0.1214 |
| Finer bands at the top (105 dollar bands) | 0.1275 |
| Muon optimizer at its usual settings | 0.1388 |
| Weight decay 0.05 / 0.1 (also with width 256: 0.157 / 0.198) | 0.158 / 0.218 |

The seed moved the score more than any setting. Not tried, if the search is reopened: weight decay below the
installed 0.01 (0.001 and 0), since raising it was the largest effect seen; more seeds for the three best ideas; and
training five or six seeds and keeping the best three by this score.

### 3-4 November (night): the projection years to 2075 (branch `rebuild-longrun`, merged 4 November)

John: "lets just run to 2075"; the same three steps, only longer series (scratch `explore/2026-11-04/longrun/`,
RESULTS.md, DECISIONS.md, MAPPING.md). Every series of the items table continues past 2035 at one Macro-Projections
aggregate (production's long-run mapping); per-return series are divided by adults with a married couple counted
once. The 27 items on today's code rule became series files with the same growth to 2035, so the start no longer
reads CBO's planner. Configuration folders 2036-2075, 401(k) limits, the Z.1 and population files, the growth.yaml
rules and the Forbes list's growth extended; the calculator's law compiles to 2075. Cheap checks only (2024-2035
reproduced to 7 in 10^16; every factor finite and positive; every year's configuration loads); no year was run.
Open then: CBO has no married 16-year-olds in 2070-2072, so step 1 could not run those three years; settled when
married people under 18 were counted as unmarried (ede6f65).

### 4 November: the install, Table 4.3's count errors, tax expenditures, the first years past 2035

- **The full run of 3-4 November installed** as the latest (John). The planned second run was not made: the run's
  reweighting already ran on the code with the age-by-AGI targets, Table 4.3 at SOI's cutoffs and the under-18 rule,
  and nothing that touches training had changed. Its settings file predated the age-by-AGI merge, so those targets
  had importance 1/1.5.
- **Table 4.3's counts in the top 0.1% and 1%** borrow their errors only from Table 1.4 count cells in the AGI classes
  reaching above the group's cutoff (John; `TARGETS.md` S15). Borrowing by count alone gave the top 0.1% count 8% and
  some item counts up to 26%. Side check on the big run's 2015, 2019 and 2023 draws (`explore/2026-11-04/t43se/`):
  every count still met, nothing else moved by more than 0.06 standard errors.
- **Tax expenditures** on the 3 November calculator, production and the big run's 2024-2029, against JCT and OTA
  (`explore/2026-11-03/tax_expenditures_1103/`; simulator branch `te-1103`, not pushed). The rebuild's absolute gaps
  to JCT over 19 provisions fell from $105 billion to $79 billion in fiscal 2026, mostly from the Social Security
  exclusion (+35% to +4%) and charity (-16% to -4%).
- **Years past 2035:** the 2075 code (`rebuild-longrun`) merged with the day's branch and tested on 2036, 2045, 2055,
  2065 and 2075, then run on the other 35 years (`explore/2026-11-04/longrun/test/RESULTS.md`): every year 2036-2075
  converges in 5-13 Newton steps, the largest miss rises smoothly from 1.23 to 2.03 standard errors, AGI holds at
  57-58% of nominal GDP. The branch merged into `rebuild-joint-model` and 2036-2075 installed beside the big run's
  2015-2035 (John).
- **The check tool's folder** was unreadable for the RA: two of its files were in the owner's own group. The tool now
  gives every file the folder's group.
- **Run3, installed** (John): the big run's networks and draws reweighted again on the code with the Table 4.3 count
  errors and the age-by-AGI targets at importance 1, all 61 years 2015-2075 (`explore/2026-11-04/run3/`). Every year
  converges; the age-by-AGI misses shrink (mean 0.11 to 0.09 standard errors). 2024-2026 from the ceiling fix below.
- **The number of returns** after 2023 kept as built (John): SOI's 2023 count, grown with the population at 2023 filing
  shares. CBO's 2023 count is a projection 4.4 million above SOI's; in 2022, both actual, they agree.
- **The billionaires.** After the last list they stay held at their starting weight, now as the rule (John: records like
  any other, aged naively, not moved by the solver). In a list year no other record may be at or above $1 billion
  (`top_records.hold_below_ceiling`): 82-292 grown 2023 records in 2024-2026 held at $999 million. After the last list,
  records grown past $1 billion stay as the new billionaires; wealth above $1 billion in 2026 dollars goes from 4.7% of
  net worth in 2026 to 6.9% in 2075 (`explore/2026-11-04/billionaires_talk/`).
- **The November filing-season tables** (branch `rebuild-nowcast`, not merged; scratch `explore/2026-11-04/nowcast/`:
  PLAN.md, BACKTEST.md, RESULTS.md). A backtest of the November tables' growth against final SOI (2016-2023, the
  years both exist) led John to four trust levels and a blend with CBO's national growth for 2024, with targets by
  AGI group in 2024 only (FEATURES.md). With the blend, 2024 meets its 149 group targets within half a standard error
  in 16 Newton steps; taxable interest is 21% above run3 in every year run, IRA distributions about 10%, ordinary dividends
  about 9%, pensions 4.5-5.3% below; income tax after credits is 0.4% below run3 in 2024 and 1.2-1.7% above it in 2025-2075.
  Without it 2024 and 2030 are run3's files exactly. Schedule C's 16.5% fall in 2024 is mostly larger losses on returns with no
  AGI; it stays on its series.
- **The jump-off year** (John, 4 November evening): 2024, the last year with IRS data, is the base of 2025-2075 when the
  November blend is on. The step in income tax between 2024 and 2025 shrinks from 2.05 to 0.36 points (against run3:
  -0.39% in 2024, -0.03% in 2025, at most +0.11% after). A first run from the 2024 file missed net worth by up to 3.2
  standard errors, because the SCF targets lacked 2023's growth to 2024; fixed by taking the 2024 run's own factors.
- **The real-PUF test for CRS** (branch `rebuild-realpuf`, scratch `explore/2026-11-04/realpuf/`). The 2019 file made
  on the real 2015 PUF records: each record keeps its PUF items, each network draws the rest given it (three copies
  at a third of the weight), the aggregate records kept as records, Part A off. 87 Newton steps; the new misses are in
  Table 4.3's top 0.001% (Schedule C or F profit count 272 against 243), because the aggregate records' copies cannot
  be split; held-out score 6.86 against run3's 8.14. A package of factors per PUF item, imputed values and the extra
  records, with a merge script, reproduces the file to the last binary digit on 232 of 244 columns and to rounding on
  the rest.

## Paths tried and not taken

| What | Why not |
|---|---|
| Sequential trees (as in the R package synthpop) in place of the transformer | Tied or beat it on PUF-only generation and were about even on wealth; the transformer was kept for the minimal example, and the comparison was left open in the old plan |
| Ranking SCF values over the whole distribution, zeros included | Assumes the SCF covers the same population as the filers; it does not |
| SCF zeros treated as "zero or small" | Lost the SCF's exact zeros; replaced by the cell method |
| A fitted reporting curve (chance of reporting by size) | Worse than the cell method on the fake survey |
| S-curves inside the fill-in | Removed in the cell method; no gain |
| A translation layer learned inside the network | Direction agreed, not built; superseded for now by the cell method |
| Separate models per source, plus reweighting | The same for the PUF variables, but net worth before the tilt was 25–37% too high in total and too flat across wage groups; one model trained on both sources did better |
| Wealth targets stated by rank, solved by iteration | Never settled; replaced by fixed dollar lines |
| Wealth counts on the SCF's own 143 million units | Pushed the file's 44 million extra units below every line; the median fell to $39,000 against the SCF's $91,000 |
| Business income drawn as one item | Kept the total right but lost the parts, which tax law needs |
| A bigger network (4 times the parameters) | Within seed noise of the base network |
| Training longer, weighting the loss by how much of a record is hidden, oversampling rare holders in training | No measurable gain on rare items |
| Dropout left on in generation | Works about as well as training without dropout; dropped for simplicity |
| Resampling each variable given the rest after generation (Gibbs) | The file drifted with every pass instead of settling |
| Tips first (22 October) | Earned by a person; the pipeline has only the unit's wages |
| CEX training on one-tax-unit households only | They spend more per unit than the average tax unit (under 26: $30k against $24k); the file drew too much |
| Consumption targets as totals | The two sources count different numbers of units; the tilt kept 72% of the effective sample |
| Consumption targets by age range alone, with the age targets (23 October) | The CEX had more married couples and fewer, larger units than the tax data; the tilt cut 11,000 records to meet them (73% kept) |
| A Gaussian tolerance of 5% on the spending cells (24 October) | Its penalty outweighed the tilt's own cost, so every cell still ended at 1.000 (71% kept); replaced by a hard 5% band |
| Mok's filing probits and the Pub 5785 hazard for the CPS's non-filers | An outside model fitted to old data; replaced by a rule calibrated to counts each year (v2, then v3 and v4) |
| Social Security plus pensions shared with the CEX | PUF total pensions include rollovers; 65+ spent 17% too much |
| Post-training by changing the network instead of reweighting (22 October): fine-tuning to the targets by reward, by distribution matching (Khalifa and coauthors 2021), and distilling the tilted stack | Not viable or practical (John). For 2019 none met the targets without a final tilt, each moved as far from the pretrained model as the tilt or further, at 5–10 times the GPU time; untargeted tables the same as the tilt's. Distilling loses accuracy by itself (a copy of the untilted stack misses it by 5–7%, thin at the top of net worth), so it compounds over chained years. Post-training is reweighting. The distilled network is kept only as a query tool. Reports: `explore/2026-10-22/finetune_direct/`, `explore/2026-10-22/distill_machine/` |
| Strata labels: AGI class × business flag, and a net worth class boosted at generation (25 October) | Records drawn for the top class added up to a lower class 73–80% of the time; replaced by grouped weighting and the AGI group drawn first (TARGETS.md S8) |
| A bigger network for the top (25 October; up to 512 × 8) | PUF held-back error at p99.99+ 2.12 to 2.10; began to memorise the SCF; 512 × 8 worse everywhere |
| Grouped weighting with groups set from exact dollars | The network sees bands: households with the same bands got different weights and were blended; weights followed one random dollar pick. Groups now come from bands |
| Redrawing records whose parts land in another group (the redraw file) | Top counts right, but drops holders of rare and offsetting items: partnership holders below p90 1.88m against the PUF's 2.79m |
| Weighting each record by the group it lands in rather than the one it was drawn for | $10M+ returns 1.23 times the PUF with the label hidden; not pursued once the label was shown |
| The AGI group label hidden in training like any other item | Generation always sets it; $10M+ returns 3.3 times the PUF against 1.2 with it always shown |
| No grouped weighting on PUF rows once the group is drawn first | Drift at p99.99+ 72–75% against 24–26% |
| A loss term rewarding parts that add up to the group | Drift 19% to 5%, but $1M+ returns about twice the PUF's and the tilt kept 82% |
| The ranked probability score in the loss | Wealthiest 0.1% with income below p90 21.9% to 36.7%; drift up on a synthetic table |
| AGI fixed from a real PUF record, one part computed as the remainder | Counts right, the error moved into the remainder (long-term gains at p50–90 six times the PUF's) |
| Training on PUF records below p90 only | Middle records still drew $1M+ items, with offsetting losses |
| Chain weighting (each step of a draw counted equally in training) | $10M+ returns 81.5k / 107.3k against 52.6k / 66.0k |
| Fixed drawing orders at generation (25 and 26 October) | No more top records than random order; wages last modestly better, still 1.5 times the PUF at $1M+ |
| Cutting chances under 0.1% at generation | Top right, but removes real rare holders (partnership holders below p90 to a seventh of the PUF's) |
| The running total of AGI, or each shown band's dollars, as network inputs | Group kept 0.3–2.8 points more often; drift and the file unchanged |
| 640 passes, or a lower end to the learning-rate schedule | No gain over 320 passes, and the SCF held-back fit worse; the schedule's end did nothing |
| Batches of 256 or 128 | More returns at the top (1.6 and 2.2 times the PUF at $10M+) and no lower drift than 512 |
| SCF waves carried to the target year's wave by one asset-price factor | Carries prices only; the 2022 wave came to 25% above the 2019 wave. Replaced by growth per tax unit, item by item |
| SCF targets from the target year's wave alone | The file draws its balance sheet from three waves; replaced by targets on the same mix, with narrower errors |
| Ranges of half a standard error for every target | 23 non-SCF targets unmet and 31% of the effective sample kept; half a standard error is kept for the SCF targets only |
| Table 4.3's top 0.001% as its own group (26 October) | 15 targets could not be met; the file probably lacks those records until the PUF's aggregate records are unpacked. The top 0.01% is one group |
| Table 1.3's capital loss measured on the file without the $3,000 cap (26 October) | SOI publishes the allowed loss; uncapped, the file's losses were 2.7 times SOI's and the tilt pulled weight off records with large losses |
| Tips as a share of each adult's wages (26–27 October) | A share drawn for a few top earners multiplied large wages: $5.9 billion of tips on units with $200,000 or more of wages against the SIPP's $0.9 billion. Tips are drawn in dollars |
| Survey wages mapped by percentile among earners alone (27–28 October) | The surveys have 1.5–8 points fewer earners than the tax data, so every earner moved down a few percentiles and wages fell 9–19%. The missing small earners are filled in first |
| Count from the top above a dollar line above zero (the SCF's wages to 28 October) | Breaks the order of units just above the line. Amounts above a line are now mapped by percentile among the units above it |
| SCF Social Security plus pensions as one shared item | The SCF's pensions leave out rollovers, which the PUF's include. Replaced by gross Social Security alone |
| The SCF's business-section profit share as partnership and S corporation income | Book profit: $1,845 billion against the tax data's $806 billion (2018) |
| Seed 20261016 in the pool (28 October) | Drew too many filers with capital and business income for their AGI group, and the balance sheet followed; net worth before reweighting $139.7 trillion against $105-111 trillion for three other seeds |
| Choosing networks by their held-back loss | Differences in the third or fourth decimal place, not in the order of the networks' wealth fit. The income-item check against the PUF does pick out the bad network |
| A network saved at pass 20, 40 or 60 of the 80-pass run | Worse on every check than at pass 80 (seed 20261017's net worth before reweighting $226, $141, $126 and $105 trillion) |
| 8 layers in place of 4 (28 October) | Moved both seeds toward the SCF a little but did not fix the bad seed; 45% more time a pass. Not adopted on two seeds |
| Weight decay of 0.05 or 0.1 in place of 0.01 (4 November) | Worse on every source; at 0.1 about 51,000 returns with AGI of $10 million or more against the PUF's 18,000 |
| The Muon optimizer (4 November) | Worse on average and less stable: one seed drew too many top returns and too much wealth |
| 105 dollar bands with finer bands at the top (4 November) | Worse on both seeds (0.1275 against 0.1205) |
