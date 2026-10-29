# Next steps (as of 28 October 2026)

The latest run (installed 28 October) has five sources (the PUF, SCF, CEX, CPS and SIPP), three pooled seeds,
tips in dollars, elective deferrals, overtime, the survey year mix, the fill-in on every survey and 789 targets
(`FEATURES.md`). This list is the order of work agreed with John on 27 October. Each step is
one change on a branch, a full run, a comparison with the latest run, John's approval, then the new latest.

## Before the tax calculator

1. **Non-microdata imputations** (named by John, 28 October; earlier "rules applied after drawing"). No new data. Child credit eligibility for each dependent, dependents' Social
   Security numbers, blindness, the QBI wage and specified-service flags for each business, parents' income rank,
   death probabilities, yearly unrealized gains on each asset, and the basis and holding period of capital gains
   from SOI's Sales of Capital Assets tables (published tables, so a rule, not a source). Production's versions are
   in `src/imputations/` and `src/mortality_ledger.R`.
   **Next, from 28 October (John).** The proposal is in scratch `explore/2026-10-28/rules_framework/PROPOSAL.md`:
   one module, `non_micro_imputations.py` (named by John, 28 October), one function per item, each year's own published table, draws keyed by record id, a
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
     `explore/2026-10-28/rules_framework/blind_puf.py`). Changed 29 October (John), after the learned slot drew ten
     times Pub 4801's count: a rule, the PUF's rate by age range times one factor per person type that makes the
     filers' count equal Pub 4801's boxes (`FEATURES.md`, "The second retrain of 29 October").
   - Q4, dependents' Social Security numbers (John, 28 October): learned from the CPS in pretraining (citizenship,
     birthplace and the parents' items turned into "probably has no number" by a rule), per child, not production's
     flat 1.5%. Research in scratch `explore/2026-10-28/rules_framework/ssn/RESULTS.md` (no Splinter paper estimates
     it; two adjust for ITIN dependents). Decided (John, 28 October): option 2. The CPS builder gives each person a
     status (citizen; noncitizen with a sign of legal status; noncitizen without one), entered in pretraining under
     the CPS label. A noncitizen without a sign lacks a number; the signs are rule B2 as coded in the scratch
     `size_check.py`: for a child, its own Social Security, SSI, Medicare or military cover, a citizen parent in the
     unit, or a noncitizen parent with a sign of their own (Social Security, SSI, Medicare, Medicaid, military cover,
     unemployment insurance, public housing, a government job, veteran status, or marriage to a citizen); the filer
     and spouse use that adult list. A child's own Medicaid is not a sign. The rule is fixed (1.03 million dependents under 17
     in 2019 against DHS's 1.01 million unauthorized children under 18); the DHS comparison is reported only. Per
     child, and for the filer and spouse too (`ssn1`, `ssn2`; Tax-Simulator needs a column for them, step 2).
     Children of temporary visa holders fall in the third group. Possible check: SOI's count of ITIN dependents,
     which David Splinter might be asked for. Built through the training table on branch `rebuild-ssn` (8a4aa2c, 28 October; scratch
     `explore/2026-10-28/ssn_build/RESULTS.md`): counts equal the size check, the rest of the table unchanged; enters
     the file at the next retrain.
   - Q5, QBI service status and wage bill (John, 28 October): the simple plan in scratch
     `explore/2026-10-28/qbi_plan/SIMPLE_PLAN.md` (research in `PLAN.md` there and `rules_framework/qbi/RESULTS.md`).
     The SCF is the prior: for each of Schedule C, partnership and S corporation, the industry group of the largest
     business and the owner's share of outside workers, learned in pretraining. The rule: specified service drawn with
     chance b x a fixed chance per SCF industry group (Treasury's Form 8995-A shares by detailed industry, averaged
     within the group); wage bill = a x outside workers x average pay in the group, plus an S corporation owner's own
     wages. Two dials solved before the reweighting: a so that non-specified owners above the phase-in keep
     Treasury's measured share of the full 20% (Goodman, Lim, Sacerdote and Whitten, Table A1, column 1: 0.59 to
     0.84 by industry, actual 2018-2019 returns); then b so that the deduction from AGI $500,000 up equals SOI's.
     Needs each record's taxable income before the deduction from the calculator once (open: bring the calculator
     in early, or run with a = b = 1 until step 4). Goes in with the next retrain.
   - Q6, yearly unrealized gains (John, 28 October): one column. Historical years use that year's actual Federal
     Reserve Financial Accounts (Z.1) revaluation rate by asset, so each record's gain lines up with its year; years
     without full data (projections) use the long-run averages production uses now. The retirement accounts' stock
     share is fitted as a single SCF variable in pretraining, not production's age rule. Bonds in retirement accounts and trusts also
     gain at the year's actual Z.1 debt-securities rate in historical years (John, 28 October; production uses 0).
   - Q7, holding period and basis of long-term gains (John, 28 October): production's method for now (holding
     group drawn from SOCA's gain shares, unconditional; basis from the SOCA regression), evaluated at the target
     year's trailing S&P returns.
   - Q8, parents' income rank (John, 28 October): production's method as is (each unit's broad income ranked among
     all units, parent rank drawn from Chetty et al.'s matrix). The rebuild follows production's stated intent where its code
     puts the bottom 9% at rank 100; production itself is not our concern (John, 28 October).
   - Q9, checks (John, 28 October): each imputation's published check is reported only, not a stop, until normal
     misses are known; then limits may be set from them.
   - **Built 28 October on branch `rebuild-nonmicro`, merged into the latest the same day** (`FEATURES.md`, "The non-microdata
     imputations"; scratch `explore/2026-10-28/nonmicro/RESULTS.md`): every item but three, which are placeholders
     until their own work is done: dependents' Social Security numbers (Q4), blindness (Q3, a retrain) and the QBI
     items (Q5); and the retirement accounts' stock share uses production's age rule until the SCF item is trained
     (Q6).
2. **The file Tax-Simulator reads.** Production's column names, one CSV per year, record IDs and seeds; every
   column Tax-Simulator reads checked against what the file has. Deductions the law removed (domestic production
   activities from 2020, tuition and fees from 2021) set to zero after repeal.
3. **Tax-Simulator on the rebuild's 2019 file.** Revenue, the distribution tables and the main credits against
   production's file and against SOI. The first test of the whole chain.
   **Items 2 and 3 built 29 October on branch `rebuild-taxsim-file`, not merged** (John, "all the way through"):
   `pipeline/taxsim_file.py` (stage `taxsim_file`) writes production's 188 columns with its conventions for missing
   values (`TAXSIM_FILE_COLUMNS.csv`); Tax-Simulator (R, main 145a80eb1) ran 2019 on it and on production's file.
   Results and open points: scratch `explore/2026-10-29/taxsim_file/RESULTS.md` (income tax before credits within
   0.1% of SOI; itemizers 13.9 million against SOI's 17.4 million). The repeal dates for the file: the domestic
   production activities deduction from 2018 (the statute), tuition and fees from 2021.
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

- **The next retrain, one bundle** (John, 28 October). Everything below needs the network retrained; it goes in
  together, then the usual full run, comparison and approval:
  - blindness learned from the PUF (Q3);
  - the retirement accounts' stock share as an SCF item (Q6);
  - Social Security number status from the CPS (Q4; built on branch `rebuild-ssn`);
  - the CPS's capital gains shown as not asked (not as zero) in tax years before 2018, so CPS records from those
    years no longer teach the model that CPS units have no gains;
  - the primary filer's sex never drawn as none (generation rule; two records in the latest);
  - the overtime targets' top wage group ($200,000 and over) merged with $100,000-200,000 (targets only);
  - the QBI items: SCF industry group and outside workers by kind of business, the rule and the two dials (Q5;
    scratch `explore/2026-10-28/qbi_plan/SIMPLE_PLAN.md`);
  - the SCF overfitting result, if it calls for a change (scratch `explore/2026-10-28/overfit/`).
  Trained 29 October (scratch `explore/2026-10-29/COMPARISON.md`). Two problems found (scratch
  `explore/2026-10-29/diagnose/RESULTS.md`) led to a second retrain the same day (John, 29 October), on the same
  branch, in two versions, with all SCF implicates each pass and with one (`training.scf_one_implicate_per_pass`):
  - blindness became a rule in the non-microdata step, the PUF's blind rate by age times one factor per person type
    so the filers' count meets Publication 4801's boxes (the learned slot drew ten times too many);
  - the CPS no longer carries capital gains in training in any year;
  - the fill-in's recomputed totals leave a total missing when a part is missing (a band of -1 was read as the top
    band, writing a large capital gain onto every 2015 and 2017 CPS training record).
  Results: scratch `explore/2026-10-29/retrain2/RESULTS.md`. **Installed 29 October** (John): the version with all
  five SCF implicates each pass (the one-implicate version cut the SCF overfitting to a third but drew net worth of
  $125 trillion before reweighting against the SCF's $97 trillion). The old run is in scratch
  `previous_2026-10-29_before_retrain2/`. Left open: the new networks give too many filers dividends, interest and
  capital gains before reweighting (capital gains 15.3% against the PUF's 13.3%), not yet traced; and non-filers no
  longer have capital gains ($65 billion on 1.7 million before), which John accepted.
- **Every dollar item grown, from a declared source, and the targets configuration as one folder per year**
  (John, 29 October). Today `build_table.growth()` grows each PUF item by SOI Table 1.4's dollars per return with the
  item and silently leaves 26 items with no Table 1.4 line in 2015 dollars (mortgage interest, real estate tax and
  the other itemized deductions, the AMT items, the credits and two other taxes; `NO_GROWTH`). Tax-Simulator on the
  2019 file found itemized deductions 25-35% below production's from this. Rules:
  - every dollar item needs a growth source in every year; the pipeline stops if one is missing;
  - the source must be exogenous to tax policy: SOI's itemized deduction tables (Table 2.1) are ruled out, since
    the 2017 law's collapse in itemizers moves their totals and means; a series on the underlying spending or tax,
    per holder where a clean one exists (for example the ACS's real estate taxes paid by owners), else per household
    when the share of holders is steady, else AGI per return as a rough prior (Codex research: scratch
    `explore/2026-10-29/growth_sources/REPORT.md`);
  - the targets configuration becomes one folder per year (`targets_config/<year>/targets.yaml`, `growth.yaml`,
    `prior.yaml`), fully written out, no inheritance; a new year starts as a copy of the nearest year's folder; a
    check before every run (a growth line for every dollar item, a value source for every target); and one overview
    table, settings by years, marking what differs from the neighbouring year, in place of the defaults layers.
- **For the next retrain: capital gains drawn before the $3,000 loss limit** (John, 29 October). The limit is
  computed at tax time; it is not a feature of the data. Today the rebuild draws a third gains piece, `kg_other`, the
  PUF's Form 1040 line 13 (E01000) less short- and long-term gains, and on returns with net losses that piece is the
  limit itself, drawn as if it were an item. The calculator applies the limit on its own and ignores `kg_other`, which
  is why the two disagree on 11.0 million returns (calculator open question 7), and why growing a record to a later
  year can turn a return held at the limit into one with a gain (found building the PUF's hidden top returns). Draw
  the full short- and long-term gains and losses from Schedule D, and the loss carryovers if the PUF has them (check
  which fields it carries); keep only what is left of line 13 that is not the limit as an item, or nothing; let the
  calculator apply the limit. Needs a retrain.
  **Built 29-30 October on branch `rebuild-names-gains`** (`FEATURES.md`, "Capital gains before the loss limit"):
  `kg_other` dropped, the limit applied where AGI is made. Loss carryovers: P22250 and P23250 are Schedule D's net
  lines, which take the carryovers in already; the PUF's other Schedule D fields (E22320, E22370) were not examined and
  are not drawn. Retrained overnight (scratch
  `explore/2026-10-30/train/RESULTS.md`).
- **Later: a removal rule beside the fill-in** (John, 28 October). The fill-in can add holders a survey misses but
  not remove holders a survey has too many of (SCF rental income 9.7% against the tax data's 7.6%); the tilt closes
  the gap for now.

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
    dollars; ages for dependent filers (settled by the non-microdata imputations, Q2); $70 billion of income tax on non-filers;
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
    8.4% against 7.3%, after 80 passes of the seed-20261017 model); the fill-in now creates losses on
    the CPS (Schedule C; capital gains until the CPS's gains left training, 29 October); filled-in CPS and SIPP couples have no recorded spouse's share of wages;
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
