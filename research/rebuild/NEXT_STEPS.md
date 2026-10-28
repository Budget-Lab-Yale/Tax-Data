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

Plan agreed 27 October (late). Each piece is built on one branch and checked at the table stage (no training)
before the next; then one big run:
1. Commit the day's documents; branch from `rebuild-tips-dollars` (tips in dollars).
2. Wages and deferrals (below): link wage, percentile mapping, SCF deferral slot. Check: the translation report.
   **Wage mapping decided (John, 27 October):** fill in the surveys' missing small earners first (function 2 on
   wages, so each survey's share with wages matches the tax data's), then map by percentile among earners. The
   percentile rule alone moved wages 9-19% (SCF 2018 -11%), because the surveys have 1.5-8 points fewer earners
   (built on `rebuild-wages-deferrals`, 515ca78; scratch `explore/2026-10-28/wages_deferrals/RESULTS.md`, with four
   smaller open questions on deferrals). Built with the fill-in extension in piece 3.
   **Deferral questions decided (John, 27 October):** (1) the SCF uses the income year's legal limit; (2) the
   SIPP's deferrals also lose the Roth share and are held to the limit; (3) each person's deferrals at most their
   own wages, in the surveys; (4) a second slot for the spouse's share of the unit's deferrals, as for wages, so
   Tax-Simulator gets each spouse's traditional deferrals; (5) at generation, deferrals at most each person's
   wages (none without wages), as for tips; (6) after a wave is grown to the target year, each person's deferrals
   held to the target year's limit for their age; (7) W-2 targets from Table 2.D (contributions by AGI, joint and
   non-joint returns; dollars only on joint returns), dependent filers allowed for in the targets' errors as for
   tips (needs a new reader). Built 28 October (9694c14 on `rebuild-wages-deferrals`): 24 targets in eight AGI
   groups (under $25k, $25-50k, $50-75k, $75-100k, $100-200k, $200-500k, $500k-1m, $1m+), kept by John.
3. The non-wage mappings, after John goes through the audit (scratch `explore/2026-10-27/mapping/AUDIT.md`). Check:
   the same report. **Approved 27 October (late), survey by survey:** "fill-in" is function 2; "percentile" is
   the survey's p-th percentile holder getting the tax data's p-th percentile holder's amount.
   - Function 2 is extended to the CEX and the CPS (it runs only on the SCF today).
   - Any rule with a dollar line maps above the line by percentile among the units above it, in each data set
     (the audit's flaw in function 1, which today breaks the order of units just above the line).
   - SCF: interest, dividends, the partnership flag as today; capital gains fill-in and loss cap, above $50,000
     percentile; Schedules C and F fill-in plus count from the top; tax-exempt interest fill-in. Social Security
     plus pensions is replaced by gross Social Security alone, matched to the return's gross Social Security
     (John, 27 October): the SCF's benefits section (X5301-X5312, respondent and spouse, current benefit at
     interview), SSI left out (scratch `explore/2026-10-28/scf_social_security/`). **Schedule E decided (John, 27 October):** the SCF's X5714 is split in
     two: rental income (the property section's net income per property, X1730, X1830, X2010, X2020, plus rent
     from part of the home or farm) against the PUF's rents and royalties, and the rest (X5714 less rental,
     floored at zero) against the PUF's other Schedule E parts. In each, function 2 fills in missing holders,
     losses included (zeros only), steered to owners of property or a pass-through, then amounts by percentile,
     income and losses separately. No depreciation estimate. The SCF's business-section profit share is not used
     (book profit, $1,845 billion against $806 billion). Scratch `explore/2026-10-28/scf_schedule_e/`.
   - CEX: Social Security as today; self-employment fill-in plus count from the top; interest and dividends,
     pensions and net rent not shared.
   - CPS: Social Security and the filing flag as today; dividends fill-in plus percentile; pensions fill-in
     (compared with taxable pensions plus taxable IRA distributions); capital gains fill-in plus percentile;
     unemployment fill-in; Schedule C fill-in plus count from the top; rents income as today; survivor and
     disability pensions moved into pensions (check which disability sources are taxable); interest not shared.
   - SIPP, new shared items: dividends (fill-in, dollars below $50,000, percentile above), gross Social Security
     as reported, and pensions (fill-in plus percentile, against taxable pensions). Interest, self-employment,
     unemployment, rent and alimony not shared.
4. The surveys' year mix in generation (TARGETS.md section 7).
   **Built and approved (John, 27 October):** branch `rebuild-year-mix` (31bf8a9, c70481c), tested on the
   tips-in-dollars models with no training: 753 of 753 targets met, effective sample kept 86.1% against 87.1%;
   CEX consumption targets pooled over the two years. Left open: non-filer counts still 2019 only; 2017 childcare
   and tips grown by wages per return; the CEX report's income groups mix dollar years (scratch
   `explore/2026-10-28/year_mix/RESULTS.md`).
5. The big run: train, draw, reweight with the W-2 deferral targets added; compare with the latest; John approves.
Left out for now (John, 27 October): overtime (data pulled, below) and the young tax units (below; "it is what it is").

- **Overtime (parked).** The CPS ASEC with the earnings-question items (weekly and hourly pay, paid by the hour, hours last
  week, the earnings weight), occupation, industry and metro size was pulled on 27 October for all twelve March
  samples, under John's IPUMS key, into `/nfs/roberts/project/pi_nrs36/jar335/raw_data/CPS-ASEC/cps_asec_common_v2`
  (tool: `Repositories/common_ipums_download`, copied from ji252's). The shared `cps_asec_common` is untouched until
  the two are compared. Next: overtime per person in `build_cps.R` by Ernie Tedeschi's rules
  (`Reports/2024/Overtime`, with `eap_codes.dta`), with each year's salary threshold and occupation codes; it
  enters pretraining under the CPS label. Dollars or share of wages follows the tips test.
  - **Problem, left open (John, 27 October):** weekly pay and hourly wage are zero in the ASEC files for the
    earnings-question group in its 4th month in sample in March 2024 (tax year 2023), and for nearly everyone in
    March 2025 and 2026. Hours, paid-by-the-hour, occupation and industry are intact. Not the Census privacy change
    (that only rounds pay and moves its top code), so probably a processing gap at Census or IPUMS; the March basic
    files may hold the same people's pay (link `MARBASECIDP`), untested. Scratch
    `explore/2026-10-27/cps_overtime/WEEKPAY_GAP.md`. For now overtime is missing (not zero) on the records without
    pay; those records are used for everything else, in pretraining and in generation's year mix.
  - **Built 28 October** on branch `rebuild-overtime` (worktree `Tax-Data-overtime`; scratch
    `explore/2026-10-28/overtime/RESULTS.md`): Tedeschi's rules per CPS worker, overtime for hours over 40 last week
    times weeks worked, held to wages. 2023: 12.0 million workers, $244 billion (Tedeschi's file 12.6 million, $272
    billion; the gap is above $200,000 of pay). The lost 2024-2026 weekly pay is recovered from the March basic
    monthly file for 99.8% of workers (link `MARBASECIDP`). Joins the next big run, not the 28 October one.
  - **Decided (John, 27 October):**
    1. The whole overtime pay is stored, not the premium, under Tax-Simulator's existing names: `ot1` (primary
       filer), `ot2` (spouse), `ot` (the unit). The branch's `overtime1`/`overtime2`/`overtime` are renamed.
    2. Tedeschi's rule kept: a covered worker whose usual hours vary has no overtime.
    3. Hours at the main job (from the linked March basic file), not at all jobs.
    4. The tax year's salary threshold, not the one at the interview.
    5. A target, so overtime does not drift: from the CPS's own units, as the CEX consumption targets are; the share
       of units with any overtime and overtime dollars per unit, over all units and in 5 groups of the unit's wages
       (under $25,000, $25,000-50,000, $50,000-100,000, $100,000-200,000, $200,000 and over), 12 targets with the
       CPS's own errors; years without their own CPS year take the pretraining year mix, grown with wages per return.
    6. Each person's overtime is held to their wages less their tips (tips drawn first); the overtime check reports
       how many people and dollars the cap takes.
    7. The March basic extract sits in the repo's local `inputs/CPS-March-Basic/` (gitignored) for now, to move to
       `raw_data` later; settings point there (`ccc78b4`).
- **The surveys' year mix** (TARGETS.md section 7, decided 27 October): the CPS, CEX and SIPP each draw on their two
  years nearest the target year, weighted by distance (2019: 73% 2019, 27% 2017). Built in generation as the
  SCF's waves are, in the big run (step 4 above).
- **Tips in dollars (decided 27 October):** each adult's tips are drawn in dollars, not as a share of wages, with the
  rule tips at most the person's wages. Test run on branch `rebuild-tips-dollars` (scratch
  `explore/2026-10-27/tips_dollars/`): tips on units with $200,000+ of wages $1.2 billion against the share run's
  $5.9 billion (SIPP $0.9 billion); before reweighting $30.3 billion against $53.7 billion (SIPP $33.6 billion); all
  753 targets met, effective sample kept 87.1% against 87.2%. The cap takes off about 10% of drawn tip dollars ($3.5
  billion of $33.8 billion before reweighting), more than half of it from people with $30,000 or more of wages. The
  dollars version has fewer tipped units with wages under $25,000 than the SIPP (4.3% against 5.0%). Folded into the
  one big run, not installed alone. Some of the largest draws repeat the SIPP's top-coded $104,160. Overtime
  is drawn in dollars too, with the same cap.

- **Wages and pre-tax deductions (decided 27 October):** the file carries net wages (W-2 box 1, the PUF's), elective
  deferrals, and Medicare wages = net + traditional deferrals (payroll tax). Health premiums and other cafeteria-plan
  items are left out for now (John, 27 October: not needed for the tax calculation); they stay inside each survey's
  link wage for the mapping to absorb. Plan:
  1. Each survey's link wage is its gross wages less the traditional deferrals it reports: SCF (capped at the legal
     limit, Roth split off as below); SIPP where it has them (2021 and 2023); CPS if the RETCB_VAL pull is made
     (from tax year 2018); otherwise, and for the CEX, as reported.
  2. Translation maps each link wage onto the PUF's net wages: the survey's share with wages kept, holders matched
     by percentile (the survey's p-th percentile earner gets the tax data's p-th percentile among earners). The
     assumption: the deductions left in (premiums everywhere; deferrals too where a survey has none) do not reorder earners much.
  3. The report shows raw, link, mapped and tax data; the mapping's move should be about the size of what is left
     in (about 3% where deferrals come off, about 8% where they do not).
  4. Deferrals are a survey slot (empty on PUF rows), drawn at generation. Deferrals come from the SCF
     (John, 27 October: consistent with its retirement assets). SCF preprocessing, definitional only: each person's
     elective contributions capped at the legal limit ($18,500 in 2018, $24,500 from age 50); missing, not zero, for
     earners outside the primary economic unit, who are not asked. The slot holds elective contributions,
     traditional plus Roth, as the SCF reports them and as the W-2 Tables 2.x count them (the targets). Traditional
     = elective x (1 - Roth share), the share from W-2 Table 4.B by joint and non-joint returns (about 9% in 2019);
     Roth stays inside box 1, so Medicare wages = net + traditional, and the SCF's link wage is gross less
     traditional. Levels and who contributes (the SCF has too few contributors from $100,000 up, SCF_VS_W2.md) are
     left to the W-2 targets.
  No preprocessing of the PUF. Today's count-from-the-top wage mapping is replaced. Options
  and checks in scratch `explore/2026-10-27/pretax/` (OPTIONS.md; CHECK.md and OTHER_MODELS.md under way). The
  mapping of every other shared variable is to be revisited the same way (definitions side by side, raw against
  the tax data, the mildest fix).
- **Young tax units:** the CPS has 20.0 million units headed by someone under 26 against the tax data's 17.6
  million, about 1.7 million of the excess with no wages; likely young adults the CPS rules make their own unit whom
  real filers claim as dependents. Left as is (John, 27 October).
- **CPS wages not filled in:** the CPS has 74.3% of units with wages against the tax data's 77.2%, but 88.6% of its
  zero-wage units say nobody worked any week, so the fill-in finds almost no hidden earners there (scratch
  `explore/2026-10-28/mappings/RESULTS.md`, open question 2). Left as is for now (John, 27 October); the other
  option was to hide weeks worked when the fill-in reads the chance of reporting wages.

## Waiting on something

- **State and local taxes:** not now (John, 27 October).

## Open decisions

- Tips: the size of the dependent-filer allowance in the W-2 spread targets' errors. The W-2 count of tipped workers
  stays reached by reweighting: raising the weight of tipped records and drawing more of them are the same thing
  statistically (John, 27 October).
- Production's double count of the above-the-line adjustments (about $145 billion in 2019): left as is (John, 27
  October). The check is in scratch `rebuild/explore/2026-10-27/other_inc_check.md`.
- Tips on units with $200,000 or more of wages: $5.9 billion in the latest run (the W-2 spread targets raised it
  from $4.9 billion), against the SIPP's $0.9 billion.
