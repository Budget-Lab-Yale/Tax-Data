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
| Social Security plus pensions shared with the CEX | PUF total pensions include rollovers; 65+ spent 17% too much |
