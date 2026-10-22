# Roadmap: from the current pipeline to the end goal

What the pipeline does now is in `FEATURES.md`; the end state is in
`GOAL.md`. Nothing on this list is started. The order is rough. Paths
starting `archive/` are under `research/archive/rebuild_2026-10/`.

1. **Target construction, made formal.**
   - Which published tables to use, and at what detail.
   - Exact targets or a tolerance; tolerances from SOI's own sampling
     errors.
   - Targets where nothing is published: projection years, splits SOI
     does not show. Build them from macro and demographic projections.
   - Choose the level of detail with held-out targets.
   - Starting points: `archive/design/tilt_next_steps.md` and
     `archive/minimal/36_RESULTS.md`.
2. **Translation, finalized.** An algorithm for lining up the variables
   the sources share, by population, definition and measurement, driven by
   data and theory. The two-slot fill-in (21 October) replaced the cell method, which needed
   hand-set assumptions for each variable
   (`archive/background/13_translation_as_a_known_problem.md`,
   `archive/design/cell_method.md`).
3. **Year priors.** A rule for which source-years a target year draws on,
   for every year through 2060. The year token is in the model; the rule
   is not.
4. **The other surveys and imputed variables.** Everything `src/main.R`
   imputes today: tips, overtime, consumption, state and local taxes, auto
   loan interest, child care and the rest. Add the CPS and CEX to
   pretraining.
5. **Half-measured PUF variables.** Age, sex and similar items that the
   PUF has only coarsely or for some records. These are filled in, not
   added as new columns. Ages: plan in `AGES.md`; the primary filer's
   range is built (21 October), exact ages, sex and dependents are not.
6. **Pretraining settings.** How the network and training choices affect
   the file.
   - Known so far: dropout biases rare items down, so it is off.
   - The model's answers do not fit one joint distribution exactly, which
     costs a few percent of rare holders.
   - Training speed, before the table grows with more surveys:
     `PERFORMANCE.md` (to do; first step a benchmark).
7. **Real PUF records or synthetic ones** for the PUF's own variables.
   - On 19 October the real 2015 PUF cost no more to reweight to 2019
     than a synthetic file, and it kept the business-income patterns no
     target covers.
   - The test: the real PUF with model-drawn net worth, against the
     current file.
   - Decide early: it changes what pretraining must get right.
8. **Real non-filers.** The CPS-based non-filer pool in this repository,
   in place of the crude stopgap.
9. **A tax calculator in post-training.** Targets on computed tax values:
   itemizers after TCJA, taxable income, liability by AGI. Possibly
   calibration to revenue estimates. Each record's contribution is
   computed once by the calculator, then the tilt runs as now.
10. **Pre-scaling by age as well as filing status,** once records carry
    age. It matters more for distant target years.
11. **Validation.** A standing set of benchmarks and checks of what no
    target covers, with a sense of good enough for each variable.
12. **Output to Tax-Simulator.** The variable contract, record IDs and
    seeds, run time, and replacing `src/main.R`.

**Known gaps in the current file,** for whichever items above address
them:
- The two-slot fill-in leaves interest about 1.7 points short of the PUF
  (share with none 71.5% against 69.8%): about 4% of zero-reporters come
  out above 100% and are capped. Most report the other items, so reporting
  is linked across items; careful-and-careless respondents is the
  candidate fix.
- If hidden holders differ from reporters in ways the record does not
  show, the fill-in overcorrects (about 8% on the fake survey). A stronger
  fake-survey test, with a survey-only variable closely tied to holding
  interest, is still to do.
- Earners over $200,000 hold too little net worth: $3.5 million against
  the SCF's $4.49 million (on the current definitions; earlier notes said
  $4.4 million). The same with real PUF records in place of synthetic ones
  (`explore/2026-10-20/real_puf/`), so it comes from the net-worth draw.
  Units with $1 million or more of income: the tilt lowers their mean net
  worth to about $17 million against the SCF's $26 million, even when it
  starts near the SCF's.
- Units with no interest, dividends or capital gains hold about half the
  SCF's net worth.
- Too many low-income units hold business equity (about 10% under $25,000
  against 3% in the SCF). The PUF has about 3.6 times as many low-income
  Schedule C and F filers as the SCF has reporters, and each gets the
  SCF reporters' chance of owning a business. A translation question:
  fill in hidden small Schedule C holders and losses on the SCF side
  (`explore/2026-10-21/business/`).
- Home mortgage dollars are $11.1–11.6 trillion against the SCF's $9.8
  trillion. The offset (`FEATURES.md`) brought mortgage holders back to
  the model's count but does not change sizes.
- High earners over 45 hold too little net worth, most of all at 65+
  with $200k+ of wages: median $2.8M against the SCF's $6.4M (about $12
  trillion of the SCF's wealth held by $200k+ earners sits with other
  units in the file). The SCF's top wages are now rescaled to the PUF's
  counts (`FEATURES.md`), which moved 65+ only from $2.7M to $2.9M. What
  is left is who is in the cell: at the same age and wages the SCF's
  households hold far more capital income (65+, $200k+ of wages, under
  $400k of income: dividends over $10k on 37% against the PUF's 14%).
  Either the SCF's older high earners are a richer group or its top
  weights carry too many; no data here tells which. The model itself, on
  the SCF's own units, gets 75–85% of the SCF's net worth at 65+. If the
  estate tax needs it: wealth targets for older households in the tilt.
- SCF household members outside the main unit (adult children,
  roommates) are dropped by `build_scf.R`, so the SCF has about 9 million
  fewer units under 26 than the PUF side. The SCF asks pooled questions
  about their assets (`explore/2026-10-21/npeu_research.md`).
- Too many units hold both partnership and S corporation income.
- Too few nonpassive losses.

Paths starting `explore/` are under
`/nfs/roberts/scratch/pi_nrs36/jar335/rebuild/explore/`.
