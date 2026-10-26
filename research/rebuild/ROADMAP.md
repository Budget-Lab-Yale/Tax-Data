# Roadmap: from the current pipeline to the end goal

What the pipeline does now is in `FEATURES.md`; the end state is in
`GOAL.md`. Items 1, 4, 5, 8 and 10 are done or mostly done; items 2, 3 and 6 are partly done; the rest are not
started. The order is rough. Paths starting `archive/` are under `research/archive/rebuild_2026-10/`.

1. **Target construction, made formal.** Built 25 October (`TARGETS.md`; `FEATURES.md`, post-training): the
   year's targets from a configuration (`pipeline/targets_config/`), each with a standard error (SOI's published
   coefficients of variation, borrowed ones, survey replicate weights, or a judgment with a reason), met within a
   range of one standard error; one algorithm every year, from a generated start or from an earlier year's file.
   Left:
   - Targets where nothing is published: projection years (macro totals, population by age), splits SOI does
     not show. The projection configuration exists; its targets are not built.
   - The from_year start and hold_distribution are being tested on 2022.
   - Estates and trusts are still left out of the business targets.
   - Choose the level of detail with held-out targets.
   - Calculator targets (item 9), the Forbes 400 and the Distributional Financial Accounts.
2. **Translation, finalized.** An algorithm for lining up the variables
   the sources share, by population, definition and measurement, driven by
   data and theory. The two-slot fill-in (21 October) replaced the cell method, which needed
   hand-set assumptions for each variable
   (`archive/background/13_translation_as_a_known_problem.md`,
   `archive/design/cell_method.md`).
   - Rule (John, 22 October; non-negotiable minimum): every comparison
     between a survey in year y and the PUF uses the tax data as of year
     y: PUF filers with weights scaled to SOI's year-y return counts in as
     much detail as SOI publishes (filing status x age, Table 1.6), every
     dollar item grown by SOI's growth per return with the item, the
     year's CPS non-filer units appended (item 8), dependent filers left
     out. Every shared variable, the assumed-correct ones included, gets a
     standing distribution comparison against it, and every translation
     rule (cut points, ranks, the top-wage swap, the fill-in formula's PUF
     side) uses it. Past SOI's last year: the last year's growth carried
     forward, a placeholder. Still to do: the rule for which survey years
     a target year borrows from (item 3; `TARGETS.md`, section 7).
   - Design (John, 22 October): translation is two standard functions and
     one configuration table, with no bespoke code paths per variable or
     survey.
     - **Function 1, amounts.** Puts any shared amount from any survey on
       the PUF's bands against the tax data as of the survey's year. One
       setting, a dollar line: dollars matched below it, rank (count from
       the top) above it. No line is pure dollars; a line at zero is pure
       rank. The SCF top-wage swap becomes function 1 with a line at
       $100,000. It replaces the dollar rule, the rank rule and the swap.
     - **Function 2, holding.** The two-slot fill-in, for any survey and
       item whose zeros hide holders. Always runs after function 1, on its
       bands.
     - **The table** (`settings.json`): one row per survey and shared
       variable, giving the survey items, definition fixes (such as the
       $3,000 capital loss cap), the dollar line, whether function 2
       applies, whether the variable is a type that function 2 conditions
       on (those cannot have function 2 themselves), and a one-line reason.
       Defaults: dollars, no fill-in; any departure needs its reason,
       backed by the standing comparison. A variable not in the table is
       not shared.
     - **The report** each run writes, one section per row, against the
       tax data as of the survey's year: the survey as reported, after
       function 1 (table stage) and after function 2 (training); in each,
       the share with none, counts by size range, counts above fixed lines,
       the mean and top shares.
   - Built 23 October (`FEATURES.md`, translation). Left:
     - function 2's PUF side still reads the model's 2015 PUF, not the
       tax data for each survey year;
     - function 2 runs on the SCF only;
     - the after-function-2 report gives only the share with none and the
       shares by size range.
3. **Year priors: the survey-year borrowing rule.** A rule for which
   source years a target year draws on, for every year through 2060. The
   year token is in the model. Done 26 October for the SCF: the balance
   sheet is drawn from a mix of waves read from the year file (TARGETS.md
   section 7), each wave's dollars grown per tax unit to the target year.
   Left: the shares are written by hand in the year file, not computed from
   the rule; the CEX's and the CPS's rules (they are still drawn at fixed
   years, settings cex and cps, generation_year); the growth carries only
   each wave's mean per unit, not the shape of its distribution.
4. **The other surveys and imputed variables.** Everything `src/main.R`
   imputes today: tips, overtime, consumption, state and local taxes, auto
   loan interest, child care and the rest.
   - Done: consumption in 8 categories from the CEX (22 October); the CPS
     in pretraining, with each person's items, the split of a couple's
     wages and self-employment income, and childcare (22-24 October;
     `FEATURES.md`).
   - Tips and overtime are earned by a person; the split of a couple's
     wages is now drawn, so they can be built on it.
   - CEX tax units: divide a shared household's spending by a model, not
     head count (`explore/2026-10-21/cex/splitting/SPLITTING.md`: learn
     from one-unit households what a unit spends, divide in proportion,
     keep the household total; test by combining real one-unit households
     into fake shared ones).
5. **Half-measured PUF variables.** Age, sex and similar items that the
   PUF has only coarsely or for some records. These are filled in, not
   added as new columns. Ages: plan in `AGES.md`; built: the primary
   filer's range (21 October), and from the CPS the exact ages, sex and
   dependents' ages (23 October). Known problem: dependents' ages come out
   too young (`FEATURES.md`).
6. **Pretraining settings.** How the network and training choices affect
   the file.
   - Known so far: dropout biases rare items down, so it is off.
   - Adopted 26 October (TARGETS.md S8): grouped weighting, the AGI group
     label always shown, batches of 512, bfloat16. Left: about 13% of filer
     records still have parts adding up to another AGI group than their
     label; AGI at or below zero comes out 1.4 times the PUF's, and the
     network draws too little wealth for top earners below the top wealth
     line (`HISTORY.md`, 25–26 October, open items).
   - The model's answers do not fit one joint distribution exactly, which
     costs a few percent of rare holders.
   - Training speed, before the table grows with more surveys:
     `PERFORMANCE.md`. Measured and partly done 22 October (packing, batched
     output layers, faster generation; bfloat16 switched on 26 October);
     left: the B200 question, fewer passes over a larger table. Batches of
     512 make training about twice as slow as 4,096 on the same card (an
     hour per model on an H100).
7. **Real PUF records or synthetic ones** for the PUF's own variables.
   - On 19 October the real 2015 PUF cost no more to reweight to 2019
     than a synthetic file, and it kept the business-income patterns no
     target covers.
   - The test: the real PUF with model-drawn net worth, against the
     current file.
   - Decide early: it changes what pretraining must get right.
8. **Real non-filers.** Done 23-24 October: non-filers are drawn under the
   CPS label, the CPS's units built by CBO's rules with their filing
   chances from the calibrated filing rule (v4), in CPS years 2015, 2017,
   2019, 2021 and 2023; the tax data for a year appends that year's CPS
   non-filers. Left: years between and beyond the CPS years take the
   nearest CPS year (the tilt's non-filer count, the geometric mean of the
   two around it), a placeholder.
9. **A tax calculator in post-training.** Targets on computed tax values:
   itemizers after TCJA, taxable income, liability by AGI. Possibly
   calibration to revenue estimates. Each record's contribution is
   computed once by the calculator, then the tilt runs as now.
10. **Pre-scaling by age as well as filing status.** Done 25 October: the
    start is scaled to Table 1.6's returns by age in joint and non-joint
    returns, and non-filers to the non-filer people by age, before the tilt
    (`tilt.prescale` "demographic").
11. **Validation.** A standing set of benchmarks and checks of what no
    target covers, with a sense of good enough for each variable.
12. **Output to Tax-Simulator.** The variable contract, record IDs and
    seeds, run time, and replacing `src/main.R`.

**Known gaps in the current file,** for whichever items above address
them. Figures are from runs before 25 October unless a date says otherwise:
- The two-slot fill-in leaves interest about 1.7 points short of the PUF
  (share with none 71.5% against 69.8%): about 4% of zero-reporters come
  out above 100% and are capped. Most report the other items, so reporting
  is linked across items; careful-and-careless respondents is the
  candidate fix.
- If hidden holders differ from reporters in ways the record does not
  show, the fill-in overcorrects (about 8% on the fake survey). A stronger
  fake-survey test, with a survey-only variable closely tied to holding
  interest, is still to do.
- Earners over $200,000 held too little net worth before the wealth
  targets of 25 October: $3.5 million against the SCF's $4.49 million. With
  them (the 25 October run) the mean is $4.28 million against the SCF's
  $4.38 million. Units with $1 million or more of income: the tilt lowers their mean net
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
- **Consumption, not targeted** (22 October, nominal 2019 dollars, file
  against CEX 2019): wages $200k+ $130k against $113k; married with
  dependents $75k against $67k; the bottom income decile $29k against $23k
  and the top $92k against $99k (each source's own income, AGI against the
  CEX's, so partly a difference in what income means). The CEX replaces
  wages above about $210,000 a person with one shared value, so its top is
  compressed.
- **CEX translation left to do** (`explore/2026-10-21/cex/translation/`):
  wages before deductions and top-coded; many small self-employment
  holders and most losses missing; interest and dividends one top-coded
  item, unused.
- **CEX data:** BLS stopped marking dependent filers in 2024 (1.6 million
  against 8–9 million), so 2024's teenage workers count as their own
  units; education spending is nearly empty in 2021 and 2022; food at home
  jumps 29% in 2024 under a new item name. In every year about 15% of
  married units are elderly couples that BLS codes as dependents of their
  own unit and never as joint filers in its tax file; we treat them as
  couples, as production does. They may be BLS's marker for couples who
  do not file.
- **CEX income folded with dependent filers:** their own wages ($101bn in
  2019) leave the CEX table; the household's interest, rent and pensions
  are divided among the remaining units.
- **CEX against NIPA:** CEX spending is about 46% of NIPA PCE (2019:
  $6.7 trillion against $14.4 trillion). Production scales each category
  up to PCE afterwards (capped at 2 times); the rebuild does not.
- **Survey cut points are grown by SOI's growth in each item's total**, not
  per return with the item. Where holders grow, survey amounts land in
  lower PUF bands (Social Security, 2015–2022: 1.44 against 1.27). Applies
  to the SCF too; intended or not is undecided.

Paths starting `explore/` are under
`/nfs/roberts/scratch/pi_nrs36/jar335/rebuild/explore/`.
