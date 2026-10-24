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
