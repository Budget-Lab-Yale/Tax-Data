# Next steps (as of 27 October 2026)

The latest run has five sources (the PUF, SCF, CEX, CPS and SIPP), tips, auto loan interest, about 40 more PUF
items and 753 targets (`FEATURES.md`). This list is the order of work agreed with John on 27 October. Each step is
one change on a branch, a full run, a comparison with the latest run, John's approval, then the new latest.

## Before the tax calculator

1. **Rules applied after drawing.** No new data. Child credit eligibility for each dependent, dependents' Social
   Security numbers, blindness, the QBI wage and specified-service flags for each business, parents' income rank,
   death probabilities, yearly unrealized gains on each asset, and the basis and holding period of capital gains
   from SOI's Sales of Capital Assets tables (published tables, so a rule, not a source). Production's versions are
   in `src/imputations/` and `src/mortality_ledger.R`.
   **Next, from 28 October (John).** The proposal is in scratch `explore/2026-10-28/rules_framework/PROPOSAL.md`:
   one module, `rules.py`, one function per item, each year's own published table, draws keyed by record id, a
   published check per rule. Nine questions for John, taken one at a time; decisions recorded here as they are made.
   These items are also most of the 39 inputs the calculator experiment sets to 0 (branch `rebuild-calculator`,
   `CALCULATOR_CONTRACT.md`).
   - Q1, where the rules run (John, 28 October): inside the tilt stage, on the start, before the targets are built,
     so the calculator in post-training can use the columns; the two ranking rules (death probabilities, parents'
     rank) rank on the start's weights and hold the ranks fixed.
   - Q2, dependent filers (John, 28 October): each gets an exact age and a sex drawn from the CPS's dependents who
     would file (earnings above the filing threshold), within the record's PUF dependent-filer age range, the draw
     keyed by record id; then the normal death-probability and blindness rules apply.
   - Q3, blindness (John, 28 October): read from the PUF, learned in pretraining, not a flat rate. On unmarried
     standard-deduction returns the deduction above the basic amount, in units of the additional amount ($1,550 in
     2015), gives the extra boxes (whole numbers for 97% of them); blind = boxes less one at 65 and over. Joint
     returns show only 0, 2 or 4 boxes in the 2015 PUF (the spouses evened out), so blindness is unknown there, as on
     itemizers, the surveys and the 3% that are not whole numbers. The model learns it by age and draws it for the
     primary filer and the spouse; checked against Pub 4801's blind boxes (scratch
     `explore/2026-10-28/rules_framework/blind_puf.py`).
   - Q4, dependents' Social Security numbers (John, 28 October): learned from the CPS in pretraining (citizenship,
     birthplace and the parents' items turned into "probably has no number" by a rule), per child, not production's
     flat 1.5%. The method is being researched (scratch `explore/2026-10-28/rules_framework/ssn/`, including whether
     any of David Splinter's papers gives a basis).
   - Q5, QBI service status and wage bill (John, 28 October): something more involved than production's rule;
     being researched (SCF business items, SOI pass-through tables by industry, the 199A literature; scratch
     `explore/2026-10-28/rules_framework/qbi/`).
2. **The file Tax-Simulator reads.** Production's column names, one CSV per year, record IDs and seeds; every
   column Tax-Simulator reads checked against what the file has. Deductions the law removed (domestic production
   activities from 2020, tuition and fees from 2021) set to zero after repeal.
3. **Tax-Simulator on the rebuild's 2019 file.** Revenue, the distribution tables and the main credits against
   production's file and against SOI. The first test of the whole chain.
4. **The calculator in post-training.** Each record's taxes computed once, then targets on computed values:
   itemizers, taxable income, the child tax credit, liability by AGI. Itemized deductions, the foreign tax credit,
   the AMT items and the 13% fall in itemizers after the tilt are revisited here.

## After the calculator

5. **Other years.** 2015 to 2023 from the same method, then the projection years.
6. **The aggregate records and the Forbes 400,** so the very top (Table 4.3's top 0.001%) can be targeted.

## In progress

**The big run: done, installed as latest on 28 October** (merge d9bd762 of `rebuild-ot3`, and d56c75d; `FEATURES.md`
describes the result, `HISTORY.md` "27–28 October" the steps and numbers). Each piece was built on its own branch and
checked at the table stage before the run, as planned on 27 October:
1. tips drawn in dollars, each person's tips at most their wages (`rebuild-tips-dollars`);
2. wages net of elective deferrals: each survey's link wage, the fill-in on wages, then the percentile mapping; the
   SCF's elective deferrals and the spouse's share of them as SCF slots; 24 W-2 Table 2.D deferral targets
   (`rebuild-wages-deferrals`);
3. the shared-variable mappings approved on 27 October, with the fill-in on every survey (`rebuild-mappings`);
4. the surveys' year mix in generation (`rebuild-year-mix`);
5. overtime pay from the CPS, with 12 CPS overtime targets (`rebuild-overtime`; added to the run on 28 October);
6. three seeds, 20261017-19, in place of 20261016 and 20261017, and the SCF's other debt held to the 99.9th
   percentile among holders in the dollar maps (`rebuild-ot3`, 54c4f27).

789 targets, all met; effective sample kept 81.3% (87.2% in the run before, with 753 targets). The run before is in
`previous_2026-10-28_before_bigrun/` of the output folder.

Left as is (John, 27 October):
- **Young tax units:** the CPS has 20.0 million units headed by someone under 26 against the tax data's 17.6
  million, about 1.7 million of the excess with no wages; likely young adults the CPS rules make their own unit whom
  real filers claim as dependents ("it is what it is").
- **CPS wages not filled in:** the CPS has 74.3% of units with wages against the tax data's 77.2%, but 88.6% of its
  zero-wage units say nobody worked any week, so the fill-in finds almost no hidden earners there (scratch
  `explore/2026-10-28/mappings/RESULTS.md`, open question 2). The other option was to hide weeks worked when the
  fill-in reads the chance of reporting wages.

## To do (John, 28 October)

- **A fill-in on the SIPP's tips.** The SIPP finds about half the W-2's tipped workers: 3.7 million tipped people in
  the 2019 file's SIPP year mix against 6.05 million tipped workers on the W-2 (filers other than dependent filers,
  as targeted). Today the tilt reaches the W-2 count by raising the weight of tipped records. Function 2 on the
  SIPP's tips would add the missing tipped workers in training instead.
- **A deferral count target on joint returns.** W-2 Table 2.D counts people, up to two per joint return, so today
  only the dollars are targeted on joint returns. The file now carries each person's deferrals (`elective1`,
  `elective2`), so the target can count the contributing people on joint returns in the file.
- **Screen each trained seed alone before pooling, whenever models are retrained.** Draw each network alone and run
  the income-item check against the PUF (share of filers with each of seven items in each Table 1.4 AGI class; scratch
  `explore/2026-10-28/model_variability/RESULTS.md`); drop a network that misses by much more than the others and
  train a replacement seed. On 28 October seed 20261016 missed by a median 1.69 points against 0.56-0.99 for seeds
  20261017-19. The threshold is not set.
- **The calculator experiment's open questions** (branch `rebuild-calculator`, not installed; scratch
  `explore/2026-10-28/calculator/RESULTS.md` and `OPEN_QUESTIONS.md`, 20 questions). The main ones:
  - inputs at 0 until the rules of step 1 exist: dependents' Social Security numbers and child credit eligibility
    (the child credit comes to $32.6 billion against SOI's $83.1 billion, no refundable part), the QBI wage bills and
    specified-service flags ($83.6 billion of QBI deduction against $155.2 billion), blindness;
  - definitions where the calculator and the file differ: the calculator leaves partnership and S corporation net
    losses out of AGI ($177 billion on 2.7 million returns); it computes taxable Social Security, the $3,000
    capital loss limit and half of self-employment tax itself, where the file draws them; capital gain
    distributions, farm rental and the rest of Schedule E; the calculator's 2019 law (its pass-through state tax
    workaround in every year, two indexed values a little off);
  - the file's own consistency: AMT at five times SOI's, from AMT adjustments drawn without the matching net
    operating losses; gross pensions below taxable pensions on 27.3 million units; the PUF items still in 2015
    dollars; ages for dependent filers (settled by the rules framework, Q2); $70 billion of income tax on non-filers;
  - how to compare: which AGI sorts records into SOI's groups, and whether to target the calculator's taxable
    income and tax before credits now (the side test met all 74 such targets, partly through the gaps above).

## Waiting on something

- **State and local taxes:** not now (John, 27 October).

## Open decisions

- Tips: the size of the dependent-filer allowance in the W-2 spread targets' errors. Until the SIPP tips fill-in
  (to do, above) the W-2 count of tipped workers is reached by reweighting.
- Production's double count of the above-the-line adjustments (about $145 billion in 2019): left as is (John, 27
  October). The check is in scratch `rebuild/explore/2026-10-27/other_inc_check.md`.
- Left open by the pieces of the big run (not yet put to John):
  - Mappings (`explore/2026-10-28/mappings/RESULTS.md`, section 6): the fill-in can add holders but not remove them,
    so SCF rental income and the rest of Schedule E end above the tax data's share with any (9.7% against 7.6%, and
    8.4% against 7.3%, after 80 passes of the seed-20261017 model); CPS capital gains before tax year 2018 stay zero (not asked); the fill-in now creates losses on
    the CPS (capital gains and Schedule C); filled-in CPS and SIPP couples have no recorded spouse's share of wages;
    the CPS's "mixed" survivor and disability classes are judgment calls; the SCF's 2013 wave's Social Security
    includes SSI (its public file has no SSI code).
  - Year mix (`explore/2026-10-28/year_mix/RESULTS.md`): the non-filer count and the non-filer people targets are
    still 2019's alone; 2017's childcare, tips and overtime are grown to 2019 by SOI's wages per return (a
    placeholder); the CEX report's income groups mix 2017 and 2019 dollars; the start from an earlier year's file
    (`tilt.grow_dollars`) grows each column by one factor, not by each record's own survey year.
  - Overtime (`explore/2026-10-28/overtime/RESULTS.md`, Part 3): overtime is drawn before tips, so the model does not
    see a person's tips when it draws their overtime (the cap applies tips first); the top wage group of the overtime
    targets ($200,000 and over) rests on about 430 asked CPS units a year and could be merged with $100,000-200,000;
    the March basic extract sits in the repository's local `inputs/CPS-March-Basic/` (gitignored), to move to
    `raw_data`.
  - Training (`explore/2026-10-28/model_variability/RESULTS.md`): a training schedule shortened properly (40 passes,
    the learning rate brought down by pass 40) was not tested; 8 layers in place of 4 were not adopted (two seeds
    cannot separate the change from seed-to-seed variation, at 45% more time a pass).
