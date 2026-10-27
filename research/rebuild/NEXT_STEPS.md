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
     interview), SSI left out (scratch `explore/2026-10-28/scf_social_security/`). **Open:** Schedule E, including whether function 2
     can turn zeros or positives into losses (scratch `explore/2026-10-28/scf_schedule_e/`).
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
