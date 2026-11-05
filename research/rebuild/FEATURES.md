# What the pipeline does now

As of 2 November 2026 (branch `rebuild-integration-1102`). The pipeline makes a file of tax units for each year
2015-2035 in two stages: networks trained on the PUF and four surveys draw records, then a reweighting moves the
records' weights so the file meets the year's published totals. The code is in `pipeline/`; every setting is in
`pipeline/settings.json`; each year's targets, growth rules and survey mix are in `pipeline/targets_config/<year>/`.
How the latest runs came out is at the end of this file.

- **What a record carries.** Every PUF item production carries; from the SCF the household balance sheet (20 items,
  net worth computed from them), elective deferrals, auto loan interest and (since 3 November) inheritances received
  this year; spending in 8 categories from the CEX; from
  the CPS each person's age and sex, dependents' ages, the split of a couple's wages and self-employment income,
  weeks and hours worked, childcare and overtime pay; each adult's tips from the SIPP; and the non-microdata
  imputations (below): dependent filers' ages, child credit eligibility, blindness, death probabilities, unrealized
  gains and basis, parents' income rank, the AMT loss rule. Names are Tax-Simulator's.
- **Filers and non-filers.** Filers are drawn under the PUF label, non-filers under the CPS label; the CPS's filing
  chances come from a calibrated rule (v4). The CPS, SCF, CEX and SIPP build tax units with one module of CBO's rules
  (`TAX_UNITS.md`).
- **Translation.** Each survey's shared items are put on the PUF's terms by two standard functions and one table: a
  dollar mapping, and a fill-in during training of the holders a survey misses (every survey since 28 October).
- **Training and generation.** Each record counts by its survey weight times q (grouped weighting), and the PUF
  record's AGI group is a label the network always sees. Filers are drawn AGI group by AGI group from three trained
  networks (seeds 20261017-19, trained 29-30 October), a third of the records each. The balance sheet is drawn from
  the year's mix of SCF waves, the CEX, CPS and SIPP items from the year's mix of survey years.
- **The top of the file.** The PUF's four aggregate records are rebuilt from generated records; any generated balance
  sheet at $1 billion or more is drawn again; Forbes's list for the year (2015-2026) is added at weight 1, held fixed;
  after 2026 the 2026 list is grown and held at its starting weight. In a list year no other record may be at or above
  $1 billion (held just below it). Below $1 billion the SCF targets apply.
- **Taxes.** Every record's taxes come from tax-simulator-gpu inside the reweighting stage, so calculator columns
  (itemizers, taxable income, credits, AMT) can be targets.
- **The reweighting.** One cost minimized in one Newton solve: the squared misses in standard errors, weighted by
  importance (1 for IRS figures, 2/3 for the rest), plus 1,000 times how far the weights move (Kullback-Leibler),
  plus a soft cap on each weight at 10 times the mean starting weight of its AGI group; at most 150 Newton steps.
  Past years have 862-929 targets: SOI's tables, the calculator against SOI, SCF wealth (also by the primary's
  age), CEX consumption, CPS overtime and older ages, W-2 tips and deferrals, dependent filers (Table 1.7), non-filers.
- **Projection years (2024-2035)** start from the 2023 file grown to the year, with 108 targets anchored to the
  2023 file times CBO's and the macro series' growth. This part is to be redesigned (`NEXT_STEPS.md`, item 1).
- **Output.** `taxsim_file.py` writes each year as Tax-Simulator's `tax_units_<year>.csv` (production's 188
  columns).

The sections below follow the pipeline's order (data, translation, the model, generation, post-training), then the
pieces added since 28 October, each under the date and branch it was built on.

## Data

**Sources:**

| Source | Rows | What it covers |
|---|---:|---|
| 2015 PUF | 207,692 | filers |
| SCF waves 2013, 2016, 2019, 2022, 2025 | 36,478 / 38,698 / 35,515 / 28,211 / 27,216 | tax years 2012–2024, five implicates each; tax units by the shared rules (below); the 2025 wave since 3 November (branch `rebuild-scf2025`) |
| CEX interview survey 2015, 2017, 2019, 2021, 2023, 2024 | 36,335 / 34,281 / 30,206 / 29,133 / 26,893 / 26,411 | every tax unit, one row per quarterly interview (`build_cex.R`, below) |
| CPS ASEC, tax years 2015, 2017, 2019, 2021, 2023 | 88,231 / 86,913 / 77,282 / 75,672 / 72,952 | every tax unit, filers and non-filers (`build_cps.R`, below) |
| SIPP, reference years 2015, 2017, 2019, 2021, 2023 | 23,699 / 33,536 / 27,907 / 22,140 / 20,317 | every tax unit (`roster_sipp.py`, below) |

Rows are the latest run's training table (`model/<run>/train_meta.json`),
held-back rows included.

**Variables:**
- marital status (joint or not), dependents (0–3), and the filing flag;
- the primary filer's age range (below);
- wages, taxable interest, ordinary dividends (computed: qualified plus
  other), and capital gains in three pieces (short-term and long-term gains
  and losses from Schedule D before the $3,000 loss limit, and capital gain
  distributions; since 29-30 October, below);
- more AGI items: other gains, taxable IRA distributions, taxable
  pensions, unemployment, alimony, state refunds,
  the statutory adjustments (computed from their eleven parts), and the rest
  of AGI ("other income");
- three items outside AGI: the non-taxable rest of pensions (since 3 November),
  tax-exempt interest, and gross Social Security (shared with every
  survey);
- two items computed, never drawn (since 3 November, below, "Social Security and
  pensions"): gross pensions, taxable pensions plus the rest; and taxable Social
  Security, by the section 86 worksheet on the record's own items, part of the
  file's AGI;
- the other PUF items production carries (since 27 October), 40 PUF-only
  slots under production's names: qualified and other dividends, the eleven
  adjustments, unrecaptured section 1250 gain, 28% collectibles gain,
  partnership income subject to self-employment tax (part_se), nine itemized
  deduction items (medical, state and local income or sales tax, cash and
  noncash charity, casualty, job expenses, tax preparation, other
  miscellaneous, other itemized), two AMT items, eleven credits and two other
  taxes (below, "The other PUF items");
- two itemized deductions (PUF only, itemizers only): mortgage interest
  paid and real estate tax. SOI Table 1.4 has no row to grow them by; since
  29 October they are grown by BEA mortgage interest per mortgaged home and
  ACS real estate taxes per home paying them (`targets_config/2019/growth.yaml`;
  until then they stayed in 2015 dollars);
- business income in its 18 PUF parts: Schedule C; partnership and
  S corporation income, losses and section 179, passive and nonpassive;
  rents, royalties, estates and trusts; farm rental; farm. Business income
  is computed from the parts, and so are four subtotals of it: Schedules C
  and F; Schedule E; and Schedule E in two parts, rents and royalties
  (`sche_rent`) and the rest (`sche_rest`: partnerships, S corporations,
  estates and trusts, farm rental), which the SCF shares (since 28 October);
- whether the unit holds a partnership, S corporation or LLC interest
  (on the PUF: any partnership or S corporation part nonzero);
- the balance sheet (SCF only), the 20 items of production's wealth
  schema: 14 assets (deposits; stocks and stock funds; bonds and bond
  funds; IRAs and DC accounts; DB account balances; cash-value life
  insurance; annuities; trusts; other financial; business equity; the
  home; other residential real estate; nonresidential real estate;
  vehicles and other nonfinancial) and 6 debts (home mortgage; debt on
  other residential property; other lines of credit; credit cards;
  installment loans; other debt). Net worth is computed from them. The
  home mortgage is carried as its share of the home's value;
- the 4 unrealized gains (home, other real estate, business, stocks and
  funds) and whether the unit owns any private business (SCF only);
- auto loan interest (SCF only, since 27 October; `auto_int_exp`): a
  year's interest on the household's vehicle loans at the interview's
  balance and rate (below, "Auto loan interest");
- elective deferrals (SCF only, since 28 October): the unit's elective
  contributions to 401(k)-type plans, traditional plus Roth, and the spouse's
  share of them (below, "Elective deferrals");
- tips (SIPP only, since 27 October; in dollars since 28 October): each
  adult's tips in dollars, and whether that adult's tips are mostly from
  leisure and hospitality (below, "The SIPP");
- spending in production's 8 categories (CEX only): clothing; motor
  vehicles; other durables; other nondurables; food at home (with
  alcohol); gasoline; housing and utilities; other services and health.
  Total consumption is computed from them.
- the exact age of the primary filer and of the spouse (SCF, CEX, CPS and
  SIPP; not on the PUF, which has only the age range): below, "Exact ages";
- person items (CPS only): the exact ages of up to three dependents
  (youngest first); the sex of the primary
  filer and the spouse; the spouse's share of the couple's wages and of its
  Schedules C and F income; each adult's weeks worked and usual weekly
  hours; childcare expenses; each adult's overtime pay (since 28 October;
  below, "Overtime"). The file's wages1 and wages2 (and se1, se2) are the
  total split by the drawn share, so they add up to the total.
- the AGI group label (PUF only; `qgroups.py`, `build_table.py`
  agi_group_label): the record's band-AGI group, one of 12 (below). It is
  computed from the record's bands, not its exact dollars.

All five sources share married, dependents, the primary filer's age range,
wages and gross Social Security. The SCF, the CEX, the CPS and the SIPP share
the exact ages of the primary filer and the spouse; the CPS and the SIPP share
the spouse's share of the couple's wages. The PUF also shares:
- with the SCF, the CEX and the CPS, Schedules C and F income (the CPS's as its
  Schedule C);
- with the SCF, the CPS and the SIPP, ordinary dividends (the CPS's go to
  qualified dividends);
- with the SCF, capital gains (on the branch `rebuild-retrain-1029` since the second retrain of 29 October; before,
  also the CPS's as long-term gains from tax year 2018), with the SCF and the CPS the two parts of Schedule E, tax-exempt interest and the
  partnership/S corporation/LLC flag (on the CPS, tax-exempt interest and the
  flag are zero: it does not ask them);
- with the SCF alone, taxable interest;
- with the CPS and the SIPP, taxable pensions;
- with the CPS alone, every other PUF tax item, because its rows are the
  non-filers: the filing flag, filing status, child credit and EITC children,
  unemployment, rents and the rest of AGI from its answers, and zero for the
  items it does not ask (short-term gains, the partnership and S corporation
  parts, the non-taxable rest of pensions and the like).

The rest belong to one source. A slot a source lacks is absent on its records
(`model.py`, "present_on").

**The CPS** (`pipeline/build_cps.R`, settings "cps"). The ASEC asking
about tax years 2015, 2017, 2019, 2021 and 2023, the household population
split into tax units by CBO's rules (`pipeline/tax_units.py`, the shared
module the SCF and CEX also use), every unit, filers and non-filers, as
the filing stage built them (below). Dependent filers are folded into
the unit that claims them. Separate returns, each return's filing status,
its statutory filing requirement and its filing chance come from the
filing stage's returns (settings cps.step1_dir and cps.step1_out, inside
the run's data folder). This is the only path: until 24 October a setting
(cps.tax_units) could switch back to production's builder
(`src/nonfilers/asec_tax_units.R`) and the non-filer pool's filing model
and calibration; that path is gone (it is in git history).
Childcare is the SPM unit's child care expenses, given to the
units that claim its children. Survivor and disability pensions paid on Form
1099-R (company or union, government, military and railroad pensions,
annuities) count as pensions since 28 October; the other survivor and
disability payments (workers' compensation, estates and trusts, accident
insurance and the like) stay in the rest of AGI (`data/cps_survivor_disability_sources.csv`).

**The CPS's filing chances: the filing rule v4** (stage filing:
`pipeline/filing.py` and `pipeline/filing_requirement.R`, settings
"filing"; moved from `explore/2026-10-23/step1_units/` on 24 October, with
the same results). Three steps, every CPS year:
1. The units, by the shared rules, people in group quarters left out
   (group quarters are left out by John's decision).
2. Each unit's statutory filing requirement (Form 1040, Charts A to C;
   production's `add_filing_requirement`).
3. The rule: log odds of filing = a (age band of the head) + k × (a child
   under 17) + required × (r + s × log gross income, centred on the year's
   median among required units + b × (any business or farm income)). The
   11 numbers are solved each year so that 11 targets hold exactly:
   - non-filing heads and spouses in 7 age bands = CPS adults less adults
     on returns (SOI Table 1.6) less dependents aged 15 and over not filing
     a return of their own;
   - non-filing children under 17: 3.08% of CPS children under 17 (the 2015
     share: Census population less the PUF's child credit children);
   - non-filing units among those required to file: 9.44% (Pub 5785's 11.19
     million, a 2014–16 average, over the 2015 CPS's required units);
   - their gross income: 4.555% of all required units' (Pub 5785's $409.4
     billion over the 2015 CPS's);
   - those with business or farm income: 44.6% of required non-filers
     (Pub 5785: 4.99 of 11.19 million).

   A dependent aged 15 or over files a return of its own when required to
   or with any wages. Separate returns: joint couples taken by a draw keyed
   by unit id until their expected returns reach SOI Table 1.6's count.
   No outside filing model (Mok's probits and the Pub 5785 hazard are gone).

Outputs in `data/filing/`: the units (`units/`), the requirement
(`requirement/`), the returns build_cps.R reads (`returns/`), and checks
per year (`checks/`), among them `<year>_nonfilers_vs_targets.csv`, whose
targets the tilt reads for non-filers by age.
- **Primary filer first.** Person items are the primary filer's and the
  spouse's, the primary chosen by the rule the age range uses (ages.py,
  keyed by household); the builder's person 1 is the man, so where the
  rule picks the woman every pair of person columns is swapped
  (`build_table.py`, cps_person_slots).
- **Encodings** (`build_table.py`, CPS_AMOUNTS and CPS_CATEGORIES):
  - exact ages are categories: 0 for none (no spouse, no k-th dependent),
    else the age plus 1, so an infant (age 0) is not "none". Dependents'
    ages: 87 values (the CPS's top code is 85). The primary filer's and the
    spouse's: 82 values, 80 and over as 80 (below, "Exact ages"). Sex: 0
    none, 1 a man, 2 a woman;
  - the spouse's share of the couple's wages, and of its Schedules C and F
    income, is an amount from 0 to 1, cut into bands within each year like
    the CEX's categories. It is recorded only on married couples with some
    of the item (absent elsewhere, so the model is never asked it there).
    The file's spouse amount is the share times the record's own total, and
    the primary filer's is the total less the spouse's: the parts add up to
    the PUF's total exactly, whatever the total's translation. Couples whose
    two self-employment incomes have opposite signs (0.4%) have their share
    cut to 0-1;
  - weeks worked, usual weekly hours, childcare and each adult's overtime
    pay are amounts, cut within each year; childcare's and overtime's
    dollars are grown from the survey year to the target year by SOI's wages
    per return (a placeholder).
- **Rules at generation** (`generate.py`, cps_structure): the spouse's
  items are none or zero unless married, and a married spouse has an age
  of 18 or over (since 26 October) and a sex; the primary filer's sex is a man or a woman, never none (since 29 October;
  two records of the 20261019 model had an age and no sex; takes effect at the next generation); the k-th dependent has an age exactly when there are at least
  k dependents; a non-filer unit's head is 18 or over (since 26 October); a
  spouse's share is zero when the couple has none of the item; overtime is
  drawn after the other CPS items, the primary filer's zero without wages and
  the spouse's zero without a share of wages. A dependent
  filer holds no CPS item (ages none, shares zero, so its wages are all its
  own). Where a share is absent on the training rows (no couple, or none of
  the item, or a dependent filer), generation leaves it absent too until
  the CPS phase is over, then sets it to zero: the network reads a present
  slot, even a hidden one, as "a couple with some of this". (The first
  candidate run made it present: unmarried primary filers came out 91% men
  against the CPS's 53%.)
- **Checks** on the file: wages1 + wages2 equals wages, and se1 +
  se2 equals Schedules C and F income, to within $0.000000001 on every
  record (floating point); every rule above holds on every record.

**Overtime** (since 28 October; `pipeline/overtime_cps.R`, called by `build_cps.R`; settings cps.overtime). Each CPS
worker's overtime pay by Ernie Tedeschi's rules (`Reports/2024/Overtime`): who the Fair Labor Standards Act covers
(private, state and local government employees, not clergy; the postal service, TVA and Library of Congress put
back); the named exempt occupations and industries; for salaried workers at or above the tax year's salary threshold
($455 a week through 2019, $684 from 2020), the white-collar duties test as a chance from the Labor Department's
occupation class and the person's pay (one seeded draw per person); the highly compensated line ($100,000 through
2019, $107,432 from 2020); then time and a half for hours over 40 last week at the main job, times weeks worked in
the tax year, at most the person's wages. A covered worker whose usual hours vary has none, as in Tedeschi's code.
Occupations are read on IPUMS's harmonized 2010 codes in every year.
- **Who is asked.** Only the CPS's earnings-question group (months 4 and 8 in sample) is asked weekly pay and hours
  at the main job; on other units overtime is missing, not zero, and absent in training, and those units are used for
  everything else. Hours at the main job, and the weekly pay the March 2024 ASEC lost for its month-4 group, come from
  the March basic monthly file, linked person by person (IPUMS's `MARBASECIDP`; pay recovered for 99.8% of workers;
  settings `paths.raw.cps_march_basic`, today the repository's local `inputs/CPS-March-Basic/`, gitignored).
- **Levels.** Workers with overtime pay and the dollars (nominal dollars of the tax year): 2015 12.3 million and $204
  billion; 2019 13.3 million and $255 billion; 2023 12.0 million and $244 billion, against Tedeschi's 12.6 million
  and $272 billion for 2023 (his file; the gap is above $200,000 of pay, where the CPS's weekly pay is top-coded and
  each person's overtime is held to their wages). The whole overtime pay is stored, not the premium alone (one third
  of it), which the 2025 law deducts.
- **In the file:** Tax-Simulator's names, `ot1` (the primary filer), `ot2` (the spouse) and `ot = ot1 + ot2`
  exactly; the drawn amounts before the cap are `ot_primary` and `ot_spouse`. Each person's overtime is at most their
  wages less their tips (tips are made first, then overtime gives way): `ot1 = min(ot_primary, max(wages1 - tips1,
  0))`, the same for the spouse. In the model's drawing order overtime comes before tips, so the model does not see a
  person's tips when it draws their overtime; the cap applies where bands become dollars. The CPS report
  (`tilt/<run>/cps_report.md`) shows what the wages and the tips each take off.
- **Targets:** 12, from the CPS's own units (post-training, below).

**The SIPP** (since 27 October; `pipeline/roster_sipp.py`, `pipeline/sipp_read.py`, settings "sipp"). The
Census Bureau's Survey of Income and Program Participation, one public-use release per reference year: 2015 (the 2014
panel, wave 3), 2017, 2019, 2021 and 2023 (the releases of 2018, 2020, 2022 and 2024), the CPS's years. The household
is the sample unit as interviewed (the people present at the interview, the CPS's convention), split into tax units
by the shared rules; income is the reference year's 12 months. Dependent filers are folded into the unit that claims
them, separate returns by the CEX's rule.
- **Shared with the PUF:** married, dependents, the age range, the exact ages of the primary filer and the spouse,
  wages (jobs for an employer, less the traditional part of each adult's elective deferrals in 2021 and 2023, the
  years the SIPP asks them: below, "Wages and elective deferrals"), and since 28 October dividends (income from stocks
  and mutual funds, TINC_STMF, against ordinary dividends), gross Social Security (TSSSAMT, before Medicare premiums)
  and pensions (retirement income items 1-8, TRET1AMT-TRET8AMT, against taxable pensions), each the primary filer's
  plus the spouse's. Shared with the CPS: the spouse's share of the couple's wages (a translation-table row with
  `shared_with`, cut on the CPS's cut points of the same year).
- **SIPP only:** each adult's tips in dollars (since 28 October; before, a share of that adult's wages, which put
  $5.9 billion of tips on units with $200,000 or more of wages), and the leisure and hospitality flags. Tips are
  production's definition (`src/imputations/tips.R`): tips on jobs for an employer (TJBn_TXAMT, job lines 1-4), not
  on self-employment; a person's tips are from leisure and hospitality when more than half are from jobs with Census
  industry codes 8560-8690. The SIPP's job earnings include the job's tips (no job-month has tips above its
  earnings), so tips are part of wages, as on Form W-2.
- **In the file:** tips1 and tips2 are each person's drawn tips (`tips_primary`, `tips_spouse`) held to at most that
  person's wages (wages1, wages2), and tips = tips1 + tips2 exactly; tips_lh1 and tips_lh2 are 0 without tips.
  Dependent filers hold none. The SIPP finds about half the W-2's tipped workers (3.7 million tipped people in the
  2019 file's year mix against the W-2's 6.05 million); the W-2 box 7 targets (post-training, below) set the number of
  tipped returns and workers and the amount.

**Auto loan interest** (since 27 October; `build_scf.R`). The SCF's own vehicles 1-4 (every wave the model uses:
2013, 2016, 2019, 2022 and, since 3 November, 2025, with the same items and codes): the amount still owed (X2218, X2318, X2418, X7169) times the
current annual rate in hundredths of a percent (X2219, X2319, X2419, X7170) over 10,000, Tedeschi's car interest
rule; a rate coded -1 (no interest) reads as 0. A household debt, divided among the PEU's units as installment debt
is, none to units outside the PEU (the group's vehicle loans are not asked). An SCF-only amount, drawn in the SCF
phase from the record's wave and grown per tax unit to the target year's wave like the balance sheet; not part of
net worth. Targeted by income group (post-training, below).

**The other PUF items** (since 27 October; `build_table.py`, STAGE3). Every PUF item production's variable guide
takes from the PUF that the model did not draw before, with production's names and definitions
(`config/variable_guide/baseline.csv`, `src/process_puf.R`), plus two adjustments the file's total already held and
production does not carry (se_tax_ded, E03260, and hsa_contr, E03290). All are PUF-only: absent on the SCF, the CEX
and the SIPP, not measured by the CPS, so a non-filer has none.
- Ordinary dividends = div_pref (E00650) + div_ord (E00600 - E00650), and the adjustments = their eleven parts, are
  computed from the parts at generation and in the dollars, so they add up exactly. The CPS's one dividend amount goes
  to div_pref, ranked against the tax data's ordinary dividends (the translation table's `rank_against`), so its
  translated dollars stay as before; its adjustments, which it never asked, are absent.
- part_se: on a return with any partnership income or loss, (E30400 + E30500) / 0.9235 less Schedules C and F.
  other_item_exp: itemized deductions before the overall limitation (P04470 + E21040) less deductible medical
  expenses (floor 7.5% of AGI at 65+, else 10%), the two taxes, interest, charity, casualty and net limited
  miscellaneous deductions, on itemizers. The PUF rounds each item on its own, so it is negative on 15 million
  returns, mostly by under $500; held at zero from below since 3 November 2026 (John; kept as the PUF had it from 27 October).
- Growth to the target year: per return with the item by its own SOI Table 1.4 line (qualified dividends; ordinary
  dividends for div_ord; each adjustment; Schedule D gains for the two gains; partnership and S corporation income
  for part_se). The itemized deduction items, AMT items, credits and other taxes have no Table 1.4 line; until 29
  October they stayed in PUF-year dollars. Since then each is grown by the series its line in
  `targets_config/<year>/growth.yaml` names (below, "Growth of every dollar item").
- No itemizer flag is drawn (Tax-Simulator chooses between the standard and itemized deductions), and no
  consistency rules among the new items. The AGI group label is unchanged.

**The CEX** (`pipeline/build_cex.R`, settings "cex"). BLS's interview
survey, five quarterly files per survey year (the year's first quarter
from the previous release from 2020 on), weights summing to the year's
consumer units (they match BLS's published counts). Households are split
into tax units by the shared rules (below); the latest run used BLS's own
codes in the member file, as production's `src/cex.R` does, with its repairs. Spending is each interview's three
months times 4. A household's spending is divided among its tax units by
head count (production's rule). In the latest run a dependent filer was folded into the unit that claims
it (its own income going nowhere) and the units were checked against BLS's
tax-unit file (NTAXI); under the shared rules a dependent with income of
their own simply stays a person of the claiming unit.

**Tax units: one set of rules for every survey (24 October).** `tax_units.py`
holds CBO's rules, run on a member list each survey's reader builds
(`roster_cps.py`, `roster_scf.py`, `roster_cex.py`; TAX_UNITS.md, section 4);
the scf and cex stages run the reader before the R script. Where a survey
lacks an input the rule uses a named fallback (`tax_units.FALLBACKS`), and
every unit carries flags for the inputs that were assumed. Settings
`tax_units.reassign` (CBO's reassignment pass, on) and `tax_units.npeu_split`
(below). `check_units.py` runs the standing tests on every survey and year.
The old builders (production's SCF builder, BLS's CEX codes) are no longer
used; they are in git history. The latest run was trained on the old units.
- The CPS: the filing stage's units (identical, unit for unit, to step 1's in explore/, 2015-2023).
- The SCF (`build_scf.R`): the financially independent adults outside the
  PEU are units of their own, with their share of the group's income and
  balance sheet (`npeu_split`: equal, or Moore's rule for wages and Social
  Security), flagged `group_level`; married couples among them file jointly;
  the SCF's support answer stands in for the student and income tests;
  children living elsewhere count only where the family paid alimony or child
  support; separate returns from the couple's own answer. The PEU's income
  and balance sheet go to the respondent's unit, or are divided between the
  respondent's and the spouse's or partner's units by Moore's shares (ported
  from production's builder). 2019 wave: 162.8 million units (143.2 under the
  old builder); median net worth $63,570 ($91,050).
- The CEX (`build_cex.R`): CBO's tests on the member file instead of BLS's
  codes (BLS's units only a comparison column in the checks); spending
  divided by head count, dependents' own returns staying people of the
  claiming unit; separate returns by the CPS's draw to SOI's count. 2019:
  162.8 million units (152.5 with BLS's codes).

**Gross Social Security on the SCF** (since 28 October; `build_scf.R`; scratch `explore/2026-10-28/scf_social_security/`).
The SCF's benefits section, asked in every wave before the income section: the respondent's and the spouse's or
partner's current benefit (X5301-X5312), before Medicare premiums, made yearly from its period; SSI (kind code 7)
left out, since it is not Social Security for tax purposes (the 2013 wave's public file has no SSI code, so its
amounts include SSI). Adults outside the PEU get their share of the group's Social Security item (X6406). It replaces
the SCF's combined Social Security and pensions (X5722), whose pensions leave out the rollovers the PUF's include.
2019 wave: 25.8% of units with any against the tax data's 22.9% (2018), $890 billion against $858 billion.

**Schedule E on the SCF in two parts** (since 28 October; `build_scf.R`; scratch `explore/2026-10-28/scf_schedule_e/`).
The SCF's Schedule E question (X5714) is split: rental income, the property section's net income per property
(X1730, X1830, X2010, X2020) plus gross rent from part of the home or farm (X1224, X505), against the PUF's rents and
royalties (`sche_rent`); and the rest, X5714 less rental income, floored at zero, against the PUF's other Schedule E
parts (`sche_rest`). No depreciation estimate is made. The SCF's business-section share of the business's net income
is not used: it is book profit ($1,845 billion against the tax data's $806 billion of partnership and S corporation
income in 2018).

**Elective deferrals on the SCF** (since 28 October; `build_scf.R`, settings "deferrals"). Each person's elective
contributions to up to two plans on the current main job, the respondent's and the spouse's or partner's, traditional
plus Roth (the SCF does not separate them), as the W-2 counts them:
1. each person's held to the income year's legal limit for their age at the interview ($18,500, or $24,500 from age
   50, in 2018);
2. then to that person's own wages (the PEU's wages split between the two by their pay on the main job);
3. missing, not zero, on units outside the PEU, who are not asked.

2019 wave: $330.5 billion on 43.1 million units, against the W-2's $332.5 billion for 2018. Two SCF slots: the
unit's elective deferrals (`elective_deferrals`), and the spouse's share of them (`spouse_deferral_share`: 0 unless
married with deferrals, missing where not asked; not grown between waves). The traditional part, elective x (1 - the
Roth share), comes off the SCF's wages to make its link wage (below, "Wages and elective deferrals"). The Roth share is
SOI's (W-2 Table 5.A for 2012-2018, Table 4.B for 2019-2020, joint and non-joint returns; 2019: 8.0% joint, 9.9%
non-joint). The SIPP's deferrals (2021 and 2023) are held to the same limits and to the person's wages, and only make
its link wage; the CEX and the CPS report none.

**Business items on the SCF.** The SCF's two business income questions
follow the tax return: X5704 is Schedules C and F, X5714 is Schedule E
(where partnership and S corporation income goes). The ownership items come
from the SCF's business section: owns any private business (X3103 or
X3401); a partnership, S corporation or LLC interest (the legal form of
actively managed businesses 1 and 2, X3119 and X3219, or a non-actively
managed one, X3407, X3411, X3451, X3415). Both are household answers; each
goes to the unit that holds the household's business equity, or to the
respondent's unit when that equity is zero.

**Amounts as bands.** Each amount is coded as one of 84 bands:
- band 0 is exactly zero;
- 10 bands of losses;
- 73 bands of positives, finer at the top. The top bands each hold 0.01%
  of positive weight.

A band becomes dollars again by drawing a real record's value from that
band.

**Exact ages** (since 23 October; `build_table.py`, EXACT_AGES). The
primary filer's and the spouse's exact ages are on the SCF, CEX, CPS and SIPP
records, absent on the PUF's (filers and non-filers alike), so the balance
sheet, consumption and tips are learned given exact age.
- The primary filer is chosen by the rule the age range uses (ages.py), on
  every source, so the exact age lies inside the age range.
- One coding on every source: exact ages to 79, then 80 for 80 and over.
  It is the finest all three allow: the CPS codes 80-84 as 80 and 85 and
  over as 85; the CEX codes everyone from 83 (84 in 2019 and 2024) as one
  value, 87 or 88; the SCF reports ages to 95.
- Translation table: taken as reported, with that coding. The tax data has
  no exact ages, so the report compares each SCF and CEX year with the CPS
  of the nearest year, by age group.
- Generation draws them right after the PUF's own items, before the SCF,
  CEX, CPS and SIPP phases, under the CPS label (below, Generation).

**The primary filer's age range** (`pipeline/ages.py`, settings "ages"),
shared by every source. Nine categories: the PUF demographic file's six
ranges (under 26, 26–34, 35–44, 45–54, 55–64, 65+) and its three for
dependent filers (under 18, 18–25, 26+), kept apart.
- **PUF:** the demographic file's range. The 88,269 records with none
  (3.68 million returns, mostly AGI above $400,000) carry probabilities
  instead ("step 0"), and training draws a range from them at the start
  of every pass:
  1. a logit of the range on what the return shows about age (filing
     status, AGI, wages, Social Security, pensions, IRA distributions, the
     early-withdrawal penalty, the standard deduction's 65+/blind
     additions, children at home, other dependents; no capital or business
     income), fitted on the known ranges;
  2. in each SOI Table 1.6 cell (filing status by AGI bracket) where at
     least 10% of the weight has no range, one factor per range scales
     every missing record's probabilities until their weight in each range
     is the table's count less the known-range weight (where the table
     merges under 26 into 26–34, the pair has one target). Below that
     share the residual is mostly sampling noise, and the logit stands.
     30 cells are calibrated, holding 2.68 million of the 3.68 million.
  Tested by hiding the known ranges at AGI $250,000–400,000: among Social
  Security recipients 82% come out 65+ against 86% true (cell shares
  alone: 18%); with long-term gains over $50,000, 46% against 48%
  (`explore/2026-10-21/step0/`).
- **SCF, CEX and non-filers:** the range of the primary filer's age. In a
  mixed-sex couple the man is the primary, except the woman with a chance
  that depends on the man's range (22% under 26 down to 4% at 65+), set so
  the SCF's female primaries by range equal the PUF's joint returns
  (27.5% under 26 down to 3.4% at 65+); keyed by household. Same-sex
  couples: the older partner. Ages as the surveys report them, with no
  adjustment for the interview date.

## Translation: putting the surveys on the PUF's terms

Two standard functions and one table (`settings.json`, "translation";
`translate.py`). No variable or survey has its own code path.

- **The tax data for a year** (`build_table.py`, `tax_data`), the base of
  every comparison and translation:
  - the PUF's filers, with dependent filers left out;
  - weights scaled in each filing status and primary filer's age range by
    SOI Table 1.6's returns in that year over 2015's. A record with no age
    range is averaged over step 0's probabilities;
  - every dollar item grown by SOI's growth in dollars per return with the
    item. Totals such as capital gains and the business subtotals are
    computed from their grown parts, as in the file;
  - the non-filers: every CPS unit of that year, weighted by its chance of
    not filing (weight × (1 − filer_p)), its amounts in PUF terms from the
    translation table.
  - Placeholders. The CPS covers 2015, 2017, 2019, 2021 and 2023: any other
    year takes the nearest CPS year, its weights scaled by SOI's total
    returns and its dollars grown per return. Past 2023, the last SOI year,
    the last year's growth is carried forward.
  - The PUF side of the training table is the PUF's filers, dependent filers
    included; the non-filers are on the CPS rows.
- **The table.** One row per survey and shared variable: the survey's
  items (summed), definition fixes, the dollar line and how amounts beyond it
  are matched, whether function 2 applies (and its size ranges), whether the
  variable is part of the type function 2 conditions on, and the reason. A
  variable not in the table is not shared. Defaults: dollars, no fill-in.
- **The rules by survey** (since 28 October; John's decisions of 27 October,
  scratch `explore/2026-10-27/mapping/AUDIT.md`). "Fill-in" is function 2;
  "percentile" means the survey's p-th percentile holder gets the tax data's
  p-th percentile holder's amount; "count from the top" means the survey unit
  with N units above it gets the tax data's amount with N units above it.

  | Survey | Item | Rule |
  |---|---|---|
  | every survey | wages | the link wage (below), fill-in, then percentile counted over all units from the top |
  | SCF | taxable interest, dividends | dollars, fill-in |
  | SCF | capital gains | losses read as no more than $3,000; fill-in; dollars to $50,000, percentile above |
  | SCF | tax-exempt interest | dollars, fill-in |
  | SCF | Schedules C and F | fill-in, count from the top |
  | SCF | Schedule E: rental income; the rest | each fill-in (losses included, zeros only), then percentile, income and losses separately; the rest's fill-in steered by the partnership flag |
  | SCF | gross Social Security | dollars (the benefits section, above) |
  | CEX | self-employment, against Schedules C and F | fill-in, count from the top |
  | CEX | gross Social Security | dollars |
  | CPS | dividends, into qualified dividends | ranked against the tax data's ordinary dividends; fill-in, percentile |
  | CPS | capital gains, into long-term gains | kept out of training in every year since the second retrain of 29 October (branch `rebuild-retrain-1029`); the row still gives the tax data's non-filers their gains from tax year 2018 (zero before); before, shown from 2018 with the fill-in, percentile |
  | CPS | pensions, with the 1099-R survivor and disability pensions | all taken as taxable pensions; compared with taxable pensions plus taxable IRA distributions; fill-in, dollars. The non-taxable rest of pensions is zero (3 November) |
  | CPS | unemployment | fill-in, dollars |
  | CPS | Schedule C (nonfarm and farm self-employment) | fill-in, count from the top |
  | CPS | gross Social Security, rents, the filing flag, the rest | dollars, as before |
  | SIPP | dividends, against ordinary dividends | fill-in; dollars to $50,000, percentile above |
  | SIPP | gross Social Security | dollars |
  | SIPP | pensions, against taxable pensions | fill-in, percentile |
  | SIPP | the spouse's share of the couple's wages | taken as reported, cut on the CPS's cut points of the same year (`shared_with`) |

  Not shared: CEX interest and dividends (one item, top-coded), pensions and
  net rent; CPS interest; SIPP interest, self-employment, unemployment, rent and
  alimony.
- **Wages and elective deferrals** (since 28 October; settings "deferrals";
  scratch `explore/2026-10-28/wages_deferrals/RESULTS.md`). The file carries
  net wages (Form W-2 box 1, the PUF's), so each survey's wages are first made
  a link wage: gross wages less the traditional part of the elective deferrals
  the survey reports (the SCF; the SIPP in 2021 and 2023), and gross wages as
  reported where it reports none (the CEX, the CPS, the SIPP to 2019). Health
  premiums and other cafeteria-plan items stay inside the link wage for the
  mapping to absorb. Every survey has fewer units with wages than the tax data
  (by 1.5 to 8 points), so the missing small earners are filled in first:
  function 1 gives each earner the tax data's wage at the same share of all
  units counted from the top (`match` "percentile_from_top"), which leaves the
  tax data's smallest wages to function 2. Mapped by percentile among earners
  alone, wages fell 9-19% (SCF 2018: its median earner's $42,610 became the tax
  data's median, $38,992). Worked example of the rule used: in the SCF 2018,
  units with $100,000 or more of wages are 12.5% of units after function 1, as
  in the tax data.
- **Function 1, amounts.** An amount within the dollar line (2015 dollars,
  grown per return to the survey's year) keeps its dollars. Beyond the line,
  by the row's match: count from the top (the default, with a line of zero
  only; losses counted from the bottom); percentile among the units beyond the
  line in each data set (required with a line above zero, so the order of units
  is kept and the map is continuous at the line; since 28 October); or, for
  wages, percentile counted over all units from the top. No line means
  dollars throughout; a line of zero means the match throughout. The result is
  cut on the PUF's cut points grown to that year.
- **The report** (`data/translation_report.csv`, and the table stage's log)
  covers each row and survey year: the survey as reported, after function
  1, and the tax data. Each column gives units, the share with none, the
  share with a loss, units by size range, units above $1k, $10k, $100k and
  $1M, the mean per unit, and the top 1% and 10% of units' shares of
  positive dollars. Categories: the share in each value. The report stage
  (`report_translation.py`) adds the stage after function 2 from the trained
  model, with filled-in amounts at their band's median, and writes all four
  stages side by side: `model/<run>/translation_report.md`.
- **The CEX, known gaps** (`explore/2026-10-21/cex/translation/TRANSLATION.md`):
  - Pensions are not shared. The PUF's total pensions (line 5a) include
    rollovers and lump sums, the CEX's do not ($0.3–0.4 trillion against
    $1.5 trillion on returns). With a combined Social Security and
    pensions slot, the file's 65+ spent 17% too much
    (`explore/2026-10-21/cex/age_check/`).
  - The CEX asks wages before deductions and replaces wages above about
    $210,000 a person with one shared value.
  - It misses many small self-employment holders and most losses.
  - Interest and dividends are one item, top-coded, and not used.
- **The CPS, why the rules** (`rebuild_cps/explore/cps_translation/TRANSLATION_CPS.md` in scratch, and the
  27 October audit):
  - wages: the CPS has more units than the tax data above every line from
    $25,000 (2019: 29.0 against 21.9 million above $100,000) and fewer below
    $10,000; its wages are before pre-tax deductions. The fill-in barely moves
    it (74.1% with wages after the fill-in against the tax data's 77.3%):
    88.6% of its units without wages say nobody worked any week (left as is,
    John, 27 October);
  - dividends, capital gains, pensions, unemployment, Schedule C: the CPS's
    zeros hide holders (2019: 7.1% of units with self-employment income against
    the tax data's 17.9%, almost no losses; half the unemployment recipients;
    15.8% with pensions against 23.9%), hence the fill-ins;
  - not shared: interest (the CPS has twice the tax data's holders and 2-6
    times its dollars, more after its 2019 redesign; the fill-in cannot remove
    holders).
- **The SCF, why the departures.** The SCF reports more high earners than
  tax data at every line (2015: 0.47 million units with $1M+ of wages
  against the PUF's 0.15 million), as the literature also finds (Gale,
  Joshi, Pulliam and Sabelhaus 2022: 2.2 times as many units with $1M+ of
  AGI). The SCF's own pattern of zeros stays visible, through a source
  label the model always sees. The balance sheet is not shared: it is
  banded within each wave.
- **Function 2: two slots, filled during training** (`fillin.py`; on the SCF
  since 21 October, on every survey since 28 October). Surveys miss holders:
  many units that report zero wages, interest, dividends, capital gains,
  pensions or business income hold some. Each item with the fill-in has two
  slots on that survey's rows: what the unit reported (never changed) and the
  PUF-concept amount (reporters copy their answer; zero-reporters are filled
  in). The PUF has only the PUF-concept slot.
  - From pass 3, at the start of each pass, each zero-reporter gets its
    chance of a hidden holding in each size range from the reporting-rate
    formula: its own chance of reporting that range (from the rest of its
    record, every filled-in slot hidden), times (tax data share ÷ survey share
    − 1) for its type, divided by its chance of reporting zero; capped at 100%.
    Each batch redraws the fill-ins from those chances.
  - The type is the survey year plus married, dependents and the wage band,
    less the item itself (wages are not in their own type); the SCF's rest of
    Schedule E adds the partnership flag.
  - The tax data's share for a type mixes filers (the PUF's own training rows,
    dependent filers left out) and non-filers (the CPS label, filing flag 0) by
    their counts in the tax data.
  - The formula reads only the reported slots and the tax data, never a
    fill-in. Totals built from a filled-in part (on the CPS: ordinary dividends,
    capital gains, Schedules C and F, business income) and the spouse's share of
    wages are hidden when the formula reads a unit's chance of reporting, and
    recomputed from the filled-in parts after each draw (since 28 October; before
    the fix the CPS's Schedule C total gave its reported answer away). A total with
    a missing part stays missing (since the second retrain of 29 October: before,
    the recomputation read a missing part as its top band, which put a capital gain
    of about $45 million on every 2015 and 2017 CPS training record).
  - A range where the survey has as many holders as the tax data, or more, gets
    none: the fill-in adds holders and never removes them. So a survey with too
    many holders in some ranges ends above the tax data overall (SCF rental
    income, below).
  - It assumes a holder's chance of reporting depends only on its type and
    true amount, not on net worth or anything else on the record.
  - A row fills in only in the years its survey asks the item, and only if it is
    shown in training (the CPS's capital gains are not, since the second retrain of
    29 October).
  - In the 2% held-back slice no fill-in is drawn, so there a zero-reporter's
    PUF-concept amount is unknown: not shown and not scored.
  - Net worth is drawn given the PUF-concept amounts; the reported slots
    stay hidden at generation.
  - Share of units with any, main year, after 80 passes of the installed
    seed-20261017 model (`model/<run>/fill_log.csv`):

    | Survey and item | Reported | After the fill-in | Tax data at the survey's types |
    |---|---:|---:|---:|
    | SCF 2018 wages | 72.3% | 77.6% | 77.0% |
    | SCF 2018 taxable interest | 11.6% | 24.9% | 26.0% |
    | SCF 2018 dividends | 10.4% | 17.0% | 18.8% |
    | SCF 2018 capital gains | 7.4% | 14.2% | 14.7% |
    | SCF 2018 Schedules C and F | 10.7% | 17.7% | 20.7% |
    | SCF 2018 rental income | 6.4% | 9.7% | 7.6% |
    | SCF 2018 rest of Schedule E | 5.4% | 8.4% | 7.3% |
    | CEX 2019 wages | 75.4% | 79.5% | 77.6% |
    | CEX 2019 self-employment | 11.4% | 22.6% | 22.0% |
    | CPS 2019 wages | 73.8% | 74.1% | 77.3% |
    | CPS 2019 dividends | 15.6% | 18.8% | 19.0% |
    | CPS 2019 capital gains | 6.1% | 10.2% | 11.3% |
    | CPS 2019 pensions | 15.8% | 16.7% | 24.4% |
    | CPS 2019 unemployment | 2.0% | 4.1% | 4.1% |
    | CPS 2019 Schedule C | 7.1% | 15.6% | 20.7% |
    | SIPP 2019 wages | 69.5% | 78.3% | 77.1% |
    | SIPP 2019 dividends | 14.0% | 20.5% | 19.5% |
    | SIPP 2019 pensions | 13.6% | 22.4% | 21.7% |

## The model

- **Network.** A transformer: 4 layers, width 128, about 1.4 million
  parameters (0.85 million of them in the output layers, one per
  variable), no dropout. A network with 4.9 million (6 layers, width 256)
  was about 3 times slower a pass and no closer to the CEX (22 October).
- **Packing.** Each batch holds one
  source's records and carries only that source's slots (the PUF has the
  most, since 27 October about 90). The answers
  are the same to rounding; a pass takes half the time and GPU memory.
  Each batch's error is scaled so every record counts as much as in a
  mixed batch (`explore/2026-10-22/perf/packing/PACKING.md`).
- **Output layers** are computed in one batched multiplication, only the
  batch's own source's (the same answers, 10% faster a pass).
- **Precision**: bfloat16 for the
  training steps since 26 October (37% faster a pass than standard 32-bit,
  with results within the difference between two seeds;
  `explore/2026-10-22/perf/precision/PRECISION.md`). The 32-bit options
  were a setting until 3 November. The fill-in, the held-back loss and the
  logs run in 32-bit.
- **What it learns.** It is trained to fill in any hidden subset of a
  record's variables from the rest. So one network answers every question
  of the form "this variable, given those".
- **Weights: grouped weighting** (`qgroups.py`; settings "training",
  q_groups; since 26 October, TARGETS.md S8). Each record
  counts by its survey weight times q, where q = records ÷ survey weight
  within the record's group. Within a group records count by weight as
  before; across groups they count by number. Each source's share of
  training is unchanged (the weights are rescaled within each source).
  - The PUF: 12 groups by band-AGI, AGI computed from a fixed amount for
    each band (the weighted mean of the real 2015 PUF amounts in it), so a
    record's group depends only on what the network sees. AGI zero or
    negative in three groups (below -$1 million, -$1 million to -$50,000,
    -$50,000 to 0), positive AGI at the weighted rank lines p50, p75, p90,
    p95, p99, p99.5, p99.9 and p99.99 of band-AGI among 2015 returns
    ($35,813 to $11.3 million). A record at p99.99 and over counts about 1.6
    returns' worth of weight, one below p50 about 1,390: the top group,
    0.02% of returns, is 4.6% of the PUF's training weight.
  - The SCF: 5 classes per wave at the weighted p50, p90, p99 and p99.9 of
    band-income (wages, interest, dividends, capital gains, gross Social
    Security, Schedules C and F, and Schedule E's two parts; since 28 October
    gross Social Security and the two parts replace gross retirement income and
    Schedule E as one item). For every item with the fill-in, wages included,
    the band is the household's reported one, not the filled-in slot.
  - The CEX, the CPS and the SIPP: q = 1.
  - Averages meant to describe the population use the survey weights
    without q: the fill-in's averages over a type and the per-variable log.
    The fill-in's PUF side (the filers' share by type) is read from the PUF's
    own training rows (since 3 November the only way; it was a setting), because the
    network's answer given the type alone leans toward the top AGI groups
    under q; for types with fewer than 30 PUF records (0.7% of SCF weight)
    the network's answer is kept.
- **The AGI group label** (always shown since 3 November; it was a setting). Each PUF
  record carries its band-AGI group as a category. In training the label is
  never hidden on PUF rows and never scored, because generation always sets
  it (TARGETS.md S8: with the
  label hidden half the time, as any other slot, the file had 3.3 times the
  PUF's $10 million-and-over returns; always shown, 1.2 times). The other
  slots are hidden exactly as before. One consequence: the network is never
  asked about a PUF record with its label hidden, so no step may ask it that
  (generation always presets it; the fill-in's rare-type fallback does ask,
  for 0.7% of SCF weight).
- **Training.** 80 passes in batches of 512 records (4,096 until 26 October:
  a batch one eighth as large lowered the share of records whose drawn parts
  add up to another AGI group from 15% to 13% and brought the middle of the
  income distribution closer, at about twice the training time on the same
  card; HISTORY.md, 25–26 October), all
  records but a 2% held-back slice. The installed models took 94 to 103
  minutes each on an H100 (about 75 before 28 October; the fill-in's refit on
  every survey takes about 18 seconds a pass against 8 on the SCF alone). Peak
  learning rate 0.003: in a sweep of 0.001 to 0.01 (two or three seeds
  each, 21 October), it brought the top income group's net worth before
  the tilt close to the SCF's, at a slightly worse fit on the average
  held-back record.
- **Why no dropout.** Dropout made rare items come out 10–20% too low
  (`HISTORY.md`).
- **Seeds.** The same seed, data, code and kind of GPU (H100) give the same
  network, weight for weight. Networks of different seeds vary more than their
  held-back loss shows: of four trained on 28 October, three drew net worth
  before reweighting of $105-111 trillion and one (seed 20261016) $140 trillion,
  against the SCF's $97.1 trillion, with held-back losses equal to the third
  decimal place. The bad network drew too many filers with capital and business
  income for their AGI group. A check of the share of filers with seven income
  items in each Table 1.4 AGI class against the PUF picks it out (median miss
  1.69 points against 0.56-0.99) and needs no SCF data
  (`explore/2026-10-28/model_variability/RESULTS.md`). Seed 20261016 was
  dropped; the file pools seeds 20261017-19 (below).

## Generation

**One record is drawn slot by slot:**
1. the PUF variables, in a random order;
2. line-13 capital gains, business income and its two subtotals, ordinary
   dividends and the adjustments, computed from their parts; then the
   partnership/S corporation/LLC flag;
3. the exact ages of the primary filer and the spouse, in a random order,
   as the CPS would see the unit (CPS label, at the record's CPS year): the
   CPS covers every tax unit, filers and non-filers, in every year, and its
   view holds the filing flag. The primary filer's age lies inside its age
   range; the spouse's exists exactly when married and is 18 or over (since
   26 October, John: a spouse is an adult; ages under 18 get no chance and
   the other ages are rescaled, on every record, filers and non-filers. Before
   it, a generated non-filer couple with a spouse of 16 counted as a child in
   the non-filer targets and took most of the weight at ranges of half a
   standard error). A non-filer unit's head (the CPS-label batch) is also 18
   or over, by the same rule (26 October, John): a 17-year-old head is counted
   in none of the non-filer people bands (adults from 18, children under 17),
   so the tilt could move the non-filer count through such units, about 200
   times their weight at tight ranges. Filers keep their PUF age ranges, since
   dependent filers can be teenagers. The tilt checks that every non-filer head
   and spouse falls in exactly one counted band and stops if not
   (`target_list.check_nonfiler_bands`);
4. the SCF-only items, in a random order (the mortgage share after the
   home; the spouse's share of deferrals after the deferrals), as the SCF
   would see the household (SCF label, PUF-only variables left out, the exact
   ages seen), at the income year of the record's own SCF wave (below). Net
   worth is computed from the balance sheet in dollars. Deferrals are zero
   without wages, and the spouse's share is zero unless married;
5. the 8 consumption categories, in a random order, as the CEX would
   see the unit (CEX label, at the record's CEX year, everything the CEX
   lacks left out, the exact ages seen);
6. the CPS's own person items and childcare, in a random order, then the two
   adults' overtime, as the CPS would see the unit (CPS label, at the record's
   CPS year), under the rules above (The CPS);
7. the SIPP's items last: the two adults' tips in dollars in a random order,
   then the two leisure and hospitality flags, as the SIPP would see the
   unit (SIPP label, at the record's SIPP year), given the spouse's share of
   wages drawn in step 6. The spouse's items are zero unless married; tips
   are zero without that adult's wages; a flag is zero without tips.

**Dependent filers** (the PUF's three dependent-filer age ranges) hold no
SCF, CEX, CPS or SIPP item: every balance-sheet item, unrealized gain, business
ownership, auto loan interest, elective deferral, consumption category, exact
age, CPS person item, overtime and tip is zero or none on their records. The surveys
count their wealth and spending in the household that claims them. Their
tax items stay as drawn.

**Speed.** Each step runs the network only on the slots present in its
phase and computes only the output layers it needs: the same draws, half the
time and memory (`explore/2026-10-22/perf/generation/GENERATION.md`). Since
22 October this is the only drawing loop in `generate.py`.

**Consistency rules** (`settings.json`, "consistency"; each rule is a list
of items that trigger it and a list of holdings of which at least one must
be nonzero, so a new rule is a settings change). When a PUF item implies a
holding, that holding cannot be drawn as zero:

| On the return | Must hold at least one of |
|---|---|
| rental income or loss | other residential real estate, nonresidential real estate |
| any partnership or S corporation item | a private business (it may be valued at zero), other residential or nonresidential real estate |
| mortgage interest paid | a home, and a mortgage on it |
| real estate tax paid | a home, other residential real estate |

Where several holdings would do, the one that must be nonzero is picked
when the first of them comes up, in proportion to the model's own chance
that each is nonzero. Ruling out zero sets the model's chance of zero to
nothing and rescales the rest; the model still decides the size. With no
home, the mortgage share is zero. No rule applies to a dependent filer: it
keeps its PUF items as drawn and holds no survey item at all (22 October;
before, about 0.1 million dependent returns had a partnership or rental
item and, by the zero-wealth rule, no matching holding).

**The offset.** The rules add holders and take none away, so without more
each ruled holding ends with more holders than the model alone would draw.
Generation therefore runs three times (`offset_generations`). After each
run but the last, for each ruled holding, one factor on every record's odds
of holding it is found so that forced holders plus the expected holders
among the rest equal the model's own expected holders (weighted). On a
forced record the rule overrides the factor, so in effect only the records
no rule touches are scaled down. The last run is kept; `gen/<run>/
consistency.csv` reports, per run and holding, the model's own expected
holders, the holders with the rules, and the factors.

**The filer batch: the AGI group first** (since 26 October, TARGETS.md
S8). 400,000 filer records (`label_records`) are split across the 12
band-AGI groups in proportion to the PUF's records per group: about 18,400 at p99.99 and over, 30,500 at
p99.9-99.99, 102,000 below p50. Each record starts with its group set; everything else is drawn given it. Its weight
is the group's 2015 returns over the records drawn in it, 1 ÷ q of the group, which undoes training's grouped
weighting exactly. Every drawn record is kept.
- **The label is checked, not trusted.** Each record's band-AGI group is computed from its drawn bands and compared
  with its label (`gen/<run>/label_check.csv`, and the stack's columns q_group and label_ok); the weight follows
  the label. In the latest run 14.1% of filer records had parts adding to another group.
  Redrawing such records until they agree was tried and not kept: it thins business income in the middle
  (`HISTORY.md`, 25–26 October).

**The SCF waves** (TARGETS.md section 7; since 26 October). Each record,
filers and non-filers alike, is given one SCF wave at random with the shares in the year's configuration
(`targets_config/2019/prior.yaml`, "balance sheet": the 2019 wave 54.65%, the 2022 wave 33.15%, the 2016
wave 12.20%, by the rule of 24 October), by a random generator of its own (`generation.scf_wave_seed`), so no other
draw moves. Its SCF phase runs at that wave's income year, and its balance sheet becomes dollars through that wave's
own map (below). The stack carries scf_wave and scf_year.

**The survey year mix** (TARGETS.md section 7; since 28 October; `survey_mix.py`). In the same way each record draws
its CEX year, its CPS year and its SIPP year, each by a random generator of its own (`generation.survey_year_seeds`),
with the shares in the year's configuration (prior, "consumption", "cps items", "sipp tips"): the survey's two years
nearest the target year, weighted by distance, for 2019 the 2019 survey 73.11% and the 2017 survey 26.89%. The exact
ages and the CPS phase run at the record's CPS year, the CEX phase at its CEX year, the SIPP phase at its SIPP year,
and each year's bands become dollars through that year's own map. The stack carries cex_year, cps_year and sipp_year.
Generation refuses a year the training table lacks for a survey. The non-filer count and the non-filer people
targets are still the 2019 filing stage's.

**Stored picks** (`dollars.py`). For each record and each amount the stack stores one uniform number
(u_<variable>) that picks the record's real dollar amount within its band, so every step that turns bands into
dollars gives a record the same amounts, and its 2019 dollars are its 2015 dollars grown.

**Three models pooled** (settings `generation.models`; TARGETS.md section 8; three since 28 October). Every batch
is drawn in equal thirds from the three trained models (`label_seed20261017`, `label_seed20261018`,
`label_seed20261019`), each with its own generation seeds (its `seed_shift`: 0, 1000, 2000), each record saying which
drew it (column `model`), ids unique across models. The consistency offset is found per model. Pooling averages the
models: a pooled file's net worth before reweighting is about the average of its models drawn alone, so it dilutes a
bad network but does not remove it (above, "Seeds").

## Post-training: the 2019 file

**Dollars.** Every dollar amount is put in 2019 dollars before the tilt, item
by item, by its line in `targets_config/2019/growth.yaml` (below, "Growth of
every dollar item"). The PUF's: each band's real 2015 values, grown to 2019 by
SOI's growth in dollars per return holding each item where Table 1.4 has the
item, else by the item's declared series. The balance sheet (since 26 October):
each band's real values from the
record's own SCF wave, grown per tax unit to the 2019 wave item by item: the
2019 wave's weighted mean of the item per unit over the drawing wave's, each in
its own dollars (`build_table.scf_growth`; `data/maps/scf_growth_factors.csv`).
For the 2022 wave stocks grow by 0.641, homes by 0.704, cash by 0.709; for the
2016 wave by 1.058, 1.166 and 1.110. The mortgage is drawn as a share of the
home's value, so it moves with the home. The growth carries only each wave's
mean: the shape of a wave's distribution is not changed. The
start from an earlier year's file (`from_year`) grows the balance sheet by the
same rule. Consumption: each
band's real CEX values of the record's CEX year (2017's grown to 2019 by NIPA
consumption per return, category by category). The CPS's items: each band's
real CPS values of the record's CPS year (childcare and overtime grown by SOI's
wages per return; 2017 to 2019, 1.062). Tips: the SIPP's values of the record's
SIPP year, grown the same way. Auto loan interest and elective deferrals grow per
tax unit with the SCF wave, like the balance sheet (deferrals by the mean per unit
over the units asked); then each person's deferrals are held to the target year's
legal limit for their age ($19,000, or $25,000 from 50, in 2019) and to their own
wages. Every dollar column of the file is in nominal
2019 dollars (until 29 October the PUF items SOI Table 1.4 has no line for
stayed in 2015 dollars). `tilt/<run>/cps_report.md`
compares the file with the CPS year mix (only overtime is targeted from the CPS);
`sipp_report.md` the file's tips with the SIPP year mix.

**SCF other debt held to the 99.9th percentile** (since 28 October; settings `dollars.scf_caps`;
`build_table.cap_scf_map`). In each SCF wave's dollar map, before it is grown to the target year, other debt is held
to the 99.9th percentile among the wave's holders (weighted): $560,000 in the 2013 wave, $900,000 in 2016, $500,000
in 2019, $2.0 million in 2022. Without it the SCF's largest amounts ($15-69 million) reached ordinary records through
the maps, and in each of three runs the tilt used one such record (other debt $7-35 million, wages $33,000-53,000) to
lower net worth in a wealth grid cell, raising its weight 39 to 47 fold until it carried $0.23-0.92 trillion of other
debt. Only the dollars
a drawn band turns into change; the training table, its bands and the models do not. In the latest run other debt is
$140 billion after the tilt against $135 billion before.

**In the file, per person** (`dollars.py`): wages1 and wages2; tips1, tips2 and tips; ot1, ot2 and ot; elective1 and
elective2 (each person's elective deferrals after the holds), Tax-Simulator's trad_contr_er1 and trad_contr_er2 (each
person's traditional part, elective x (1 - the W-2 Roth share for joint or non-joint returns)), trad_deferrals (their
sum) and medicare_wages (wages plus both); elective_deferrals keeps the drawn total. Each sum adds up to its parts
exactly.

**Growth of every dollar item** (since 29 October; John; TARGETS.md section 12; `growth.py`,
`targets_config/<year>/growth.yaml`, the series in `research/rebuild/resources/growth/` with `sources.csv`). Every
dollar item the pipeline grows between years has one line: its source series, the measure, how negative values are
grown, and a reason; the pipeline stops if an item has none. The source must be exogenous to tax policy (SOI's
itemized deduction tables are ruled out). The Table 1.4 lines are as before. The 26 PUF items with no Table 1.4 line,
which until then were not grown, and their factors from 2015 to 2019:

| Item | Series | Measure | 2015 to 2019 |
|---|---|---|---:|
| mortgage interest | BEA NIPA Table 7.11 line 16 over ACS homes with a mortgage (B25081; 2020 interpolated) | per mortgaged owner-occupied home | 1.089 |
| real estate tax | ACS aggregate real estate taxes (B25090) over owner homes paying any (B25102; 2020 interpolated) | per owner home paying the tax | 1.138 |
| state and local income or sales tax | Census state and local individual income tax revenue (QTAX), calendar years, over SOI's returns | per return | 1.148 |
| medical expenses | MEPS out-of-pocket spending (TOTSLF) | per person with any | 1.356 |
| tax preparation fees | BLS CPI-U, tax return preparation and other accounting fees | a price | 1.168 |
| education credit expenses (American opportunity, lifetime learning) | College Board published tuition and fees, fixed sector weights, tax year = average of two academic years | per full-time undergraduate | 1.126 |
| the other 19 (charity cash and noncash, casualty, job expenses, other miscellaneous, other itemized, two AMT items, the foreign tax credit, eight other credits, two other taxes) | SOI AGI over all returns (Table 1.1), past SOI's last year BEA personal income per capita, past BEA's last year CBO's | per return | 1.118 |

Capital losses are not grown (the $3,000 limit is a statutory nominal amount). The CEX categories (NIPA PCE per
return), the CPS's childcare and overtime and the SIPP's tips (SOI wages per return with wages) and the SCF balance
sheet (its own mean per tax unit between waves) have lines too, with the series they had. Known weak points: MEPS's
redesigned interview from 2018 may break its series (the value per person rises 20% from 2017 to 2018, against 4-5%
in other years); the College Board weights are fall 2019 enrollment (no 2015 split is published in the workbook); the
CPI for tax preparation is the mean of the months BLS published (all twelve in 2015 and 2019). The CPS's mortgage
interest and real estate tax are translated against the tax data with these factors, so their cut points move at the
next table build and retrain; until then only the dollar maps changed.

**The year's configuration** (`pipeline/targets_config/`; TARGETS.md section 12 and S7; one folder per year since 29
October): the catalog of definitions (shared) and, for each year, `<year>/targets.yaml`, `prior.yaml` and
`growth.yaml`, written out in full with no inheritance; `config_overview.py` writes `OVERVIEW.md`, every setting by
year. `target_list.py` checks the folder before every run and expands it into
one row per target (`tilt/<run>/targets.csv`), with each target's standard error from `errors.py` (SOI's published
coefficients of variation for Tables 1.4, 2.1 and 3.3; borrowed ones for Tables 1.2, 1.6 and 1.7; CEX and SCF
replicate weights; a judgment number with a reason otherwise; TARGETS.md section 9).

**Start** (the year file's `start`): `generate` (the generated stack) or `from_year: s` (year s's tilted file, its
dollars grown item by item to year t). Weights are scaled to SOI's return count and the non-filer count, to joint
and non-joint returns, then (`tilt.prescale` "demographic") to Table 1.6's returns by age x joint and to the
non-filer people by age (section 4).

**The tilt** (`solver.py`; TARGETS.md S11): the one-cost reweighting, the only rule. The weights minimize the sum over
targets of 0.5 x importance x (miss ÷ standard error)² plus tau x the Kullback-Leibler move from the start; identities
exact; no target has a range and none is dropped. tau is 1,000 and the budget 150 Newton steps in all (100 before 3 November) (settings
`tilt.reweighting`); importance is 1 for the IRS's published figures and the calculator's columns, 1/1.5 for the survey and
pipeline kinds. A soft cap on each weight (`tilt.soft_cap`: 0.5 x beta x ((w - U) ÷ U)² above U = 10 x the mean starting
weight of the record's AGI group (decision 73; before it was the median), beta 1) replaces the earlier hard caps and factor limits. An average's standard error is on the average itself, with the cell's weight after
the reweighting. The demographic prescaling uses the same cost. Cells resting on too few effective records, or on a
record moved too far, are flagged as warnings or stops (section 5). Every run's summary carries the check before the
reweighting and the table of weight factors (final over starting weight) by AGI group. Schedule A's growth adjustment
(decision 49) is limited to ±25% of the declared factor.
- **History: why half a standard error only for the SCF (the range rule of 26 October, ended 1 November).** At 0.5 for every target (run P1, `HISTORY.md` 25–26 October)
  23 targets could not be met, the non-filer people bands against the CEX food and health averages, and the
  effective sample kept fell to 31%. At 0.5 for the SCF targets alone (run P3, approved by John) every target is met
  and 97.75% is kept; the file keeps more net worth at the top of income than with every range at 1 ($9.91
  against $9.35 trillion with units at p99.9+ of income; pooled SCF $10.38 trillion).

**The SCF targets** (`scf_targets.py`; TARGETS.md S9; since 26 October) are computed on the same SCF waves, with
the same shares, as the file's balance sheet: the year file's prior "balance sheet" (2019: the 2019 wave 54.65%,
2022 33.15%, 2016 12.20%). Each wave's balance sheet is grown per tax unit
to the target year's wave by the maps' factors, its weights scaled to the target year's wave's 162.76 million units;
each wave keeps its own income ranks and net worth lines, and its value and variance in each cell come from its own
999 replicate weights and five implicates. Value = Σ share × the wave's value; variance = Σ share² × the wave's
variance; the file's net worth lines are the share-weighted average of the waves' lines. Standard errors are about
0.65 of one wave's. The replicate weights of each wave sit in `<out>/data/scf_rw/<wave>/` (errors.py says how to
fetch them). `tilt/<run>/scf_targets_by_wave.csv` has each target's value, error, lines and SCF households by wave.

The 2019 targets (789 on 28 October; 787 from the next run, with the overtime groups merged):

| Targets | Number |
|---|---:|
| IRS (SOI Table 1.4): returns and the number with and amount of wages, interest, dividends and capital gains, in AGI brackets with the five top ones ($1M-1.5M, $1.5M-2M, $2M-5M, $5M-10M, $10M+) separate; joint returns (Table 1.2) | 172 |
| SOI Table 1.3: returns, AGI, and the number with and amount of 22 income items, in each of the four filing statuses | 184 |
| SOI Table 4.3: returns, AGI and 8 income items in the top 0.01% (one group), 0.1% and 1% of AGI, returns other than dependent filers' | 54 |
| Age (Table 1.6): returns by primary age 26-34 to 65+, joint and non-joint | 10 |
| Age by AGI (Table 1.6, All returns panel; since 3 November 2026, branch rebuild-age-by-agi): returns by primary age 26-34 to 65+ in each of the 15 standard AGI groups (kind `age by AGI`; summed over AGI they equal the row above's joint plus non-joint totals, so both are kept; the prescaling uses only the row above) | 75 |
| Dependent filers (Table 1.7): returns by AGI | 9 |
| Business: net income and net loss, returns and dollars; Schedule C and partnership/S corp in 10 AGI groups (the five top ones separate), rent and royalty, farm and farm rental in 6 (cells SOI combined are merged) | 148 |
| Non-filer count (identity) and non-filer people by own age band and children under 17 | 9 |
| Tips: SOI's 2019 Form W-2 statistics (Tables 4.B and 4.C, box 7): returns with tips and the amount, joint and non-joint (4); tipped workers on joint returns (1); the share of tipped workers above each of six percentile lines of tips per worker (6) | 11 |
| Elective deferrals: SOI's 2019 Form W-2 Table 2.D, in eight AGI groups: contributions on joint returns (8); returns with any and contributions on non-joint returns (16) | 24 |
| SCF wealth, the 2016, 2019 and 2022 waves pooled: shares of units and net worth above the 99.9th, 99th, 90th and 50th percentile lines and $1 million; the four asset types (stocks, business equity, real estate, retirement accounts) above the 99.9th, 99th and 90th lines | 22 |
| SCF income x net worth grid, the same waves pooled (below): in 4 income groups, the share of the group's units in each net worth class and each class's net worth | 22 |
| SCF auto loan interest, the same waves pooled: the share of units with any and dollars per unit in the grid's 4 income groups | 8 |
| CEX consumption, the 2019 and 2017 surveys pooled (73.11% and 26.89%): 8 categories' averages per unit, over all units and in 12 age x married cells | 104 |
| CPS overtime, the 2019 and 2017 surveys pooled: the share of units with any and overtime per unit, over all units and in 4 groups of the unit's wages (since 29 October; 5 groups and 12 targets before) | 10 |

**Tables 1.3 and 4.3** (since 27 October; `targets_config/2019/targets.yaml`, read from the local IRS-Ind store's tidy files, settings
`paths.raw.soi_tables`). Errors are borrowed from the nearest Table 1.4 cells. Table 1.3's capital loss is measured
on the file as the loss allowed on the return (at most $3,000, $1,500 if married filing separately; catalog
transform `loss_allowed`), which is what SOI publishes. Table 4.3's groups (since 3 November, John; `TARGETS.md`
S15) are the returns other than dependent filers' with AGI at or above SOI's published cutoff for each group and
year (the item "adjusted gross income floor on percentiles"; catalog grouping `agi_cumulative`, `cutoffs:
published`), fixed through the tilt; a missing cutoff stops the run, and every year 2015-2023 has them. The count
of returns above each cutoff is a target like the others (its error borrowed from Table 1.4's count cells in the AGI classes reaching above
the cutoff since 4 November, or 1% where the cutoff is $10 million or more). Before, the groups were set at the starting file's own AGI percentiles,
which sat above SOI's because the draw is too heavy at the top (2015: $504,632 for the top 1% against SOI's
$480,930), so SOI's totals were imposed on too few returns. On the 1 November draws (scratch
`explore/2026-11-03/t43_lines/RESULTS.md`), the change met the 2015 top 1% (returns and wages had been 2.3 and 2.6
standard errors short), left 2019 and 2023 almost unchanged, cut the Newton steps from 78, 73 and 99 to 41, 50 and 62
in 2015, 2019 and 2023, and kept the same effective sample. The top 0.001% was folded into the top 0.01% from 26
October until the PUF's aggregate records were rebuilt (29 October, `top_hidden.py`); it is a group of its own again.

**Tips** (since 27 October; `targets_config/2019/targets.yaml`, settings `paths.raw.soi_w2`; the W-2 reader handles the 2019 and 2020
layout only). The file's tips are measured as box 7 holds them: each person's tips at most the Social Security wage
base ($132,900 in 2019) less that person's other wages (catalog `ss_tips`). The W-2 counts include dependent filers,
who hold no tips in the file, so the non-joint values are reduced by the SIPP's share of dependents among non-joint
tipped people (10.1% of people, 2.9% of tips), half of that reduction added to the error. Non-filers' tips are not
targeted (the W-2 statistics cover filed returns only).

Tips per worker (27 October, after the first tips targets). Seven more targets give the spread of tips among tipped people: the
number of tipped workers on joint returns (Table 4.B, 1,237,794, error 2.7%) and the share of tipped workers whose box 7 tips are above
$121, $537, $2,444, $8,409, $18,281 and $44,054 (Table 4.C's 10th, 25th, 50th, 75th, 90th and 99th percentiles, so the targets are
90%, 75%, 50%, 25%, 10% and 1%). A worker is a person, the primary filer or the spouse, with box 7 tips above zero, over filers other
than dependent filers. A non-joint return has one worker, so the non-joint worker count is the count of non-joint returns with tips,
already a target. Table 4.C includes dependent filers, who hold no tips in the file, so each share's error carries half the most that
removing them could move it (4.2 points at the $121 line, 0.13 at the $44,054 line). Code: `person_values`, the statistics `workers` and
`worker share` and `published_w2_shares` in `pipeline/target_list.py`; configuration in `catalog.yaml` (source `soi w2 pctiles`) and
`targets_config/2019/targets.yaml` (`tips_w2_workers`, `tips_w2_spread`).

**Elective deferrals** (since 28 October; `targets_config/2019/targets.yaml`, reader `target_list._w2_by_agi`, catalog source `soi w2
2.d`). SOI's W-2 Table 2.D, its 19 AGI rows summed into eight groups (under $25,000, $25,000-50,000,
$50,000-75,000, $75,000-100,000, $100,000-200,000, $200,000-500,000, $500,000-1 million, $1 million and over). The
file's measure is elective1 + elective2 over filers other than dependent filers. Table 2.D counts people, up to two
on a joint return, so on joint returns only the dollars are targeted. As for tips, the non-joint values are reduced
by the SIPP's share of dependents among non-joint people with deferrals (SIPP 2021, the nearest year that asks: 0.5%
of people, 0.1% of dollars), half of that reduction added to the error; errors are borrowed from the nearest Table
1.4 cells. 2019 targets: $243.6 billion on joint returns; 27.7 million non-joint returns with $109.1 billion.

**Overtime** (since 28 October; `targets_config/2019/targets.yaml` entries `overtime_all` and `overtime_by_wages`; catalog source
`cps`; `target_list.cps_mix_rows`, `errors.overtime_errors`). Built as the CEX consumption targets are: from the CPS's
own units (the shared tax-unit rules), units other than dependent filers, filers and non-filers. Only units asked the
earnings questions have overtime; each target is an average over the asked units in the cell with their own weights.
The unit's wages are the CPS's after function 1, so a wage group holds the same ranks on both sides. The 2019 value
is 73.11% of the 2019 CPS's plus 26.89% of the 2017 CPS's (grown by SOI's wages per return). Errors from the ASEC's
160 person replicate weights (successive difference replication: variance = 4/160 times the sum of squared
differences), pooled over the two years as for the CEX.

| Unit's wages | Share with any overtime | Overtime per unit |
|---|---:|---:|
| all units | 7.57% (standard error 0.22 points) | $1,469 ($55) |
| under $25,000 | 2.01% (0.17) | $180 ($21) |
| $25,000-50,000 | 13.22% (0.60) | $1,755 ($95) |
| $50,000-100,000 | 13.94% (0.65) | $3,065 ($187) |
| $100,000-200,000 | 12.76% (0.80) | $4,256 ($355) |
| $200,000 and over | 7.45% (1.19) | $2,808 ($537) |

Since 29 October (John, 28 October; branch `rebuild-retrain-1029`) the top two groups are one, $100,000 and over
(catalog `wage_groups_4`): the $200,000-and-over group rested on about 430 asked CPS units a year. The table above
is the five-group version of 28 October. The merged group: $100,000 and over, 11.51% with any overtime (standard error
0.65 points), $3,918 per unit ($298); the other groups are unchanged.

**Consumption over the year mix** (since 28 October). Each CEX year's spending is put in 2019 dollars by NIPA
consumption per return, category by category; the target is 73.11% of the 2019 average plus 26.89% of the grown 2017
average, and its standard error combines each year's own (from its 44 replicate weights) as independent samples.
Pooling lowered the standard errors by 22% at the median. The target labels still say "CEX 2019".

**Auto loan interest** (since 27 October; `targets_config/2019/targets.yaml`). Per unit rather than totals, because the SCF's units
(162.8 million) are fewer than the file's units other than dependent filers (about 169.5 million).

**The income x net worth grid** (since 26 October, approved with run P3; targets_config/2019/targets.yaml, wealth_grid_shares and wealth_grid_dollars; scf_targets.py). Four income groups by the seven income items the SCF and the file share (wages, interest,
dividends, capital gains, gross Social Security, Schedules C and F, Schedule E; on the SCF Schedule E is its two parts;
since 28 October gross Social Security replaces gross retirement income on both sides, which moves some units between
income groups): below p90, p90-99, p99-99.9 and
p99.9 and over, each side by its own weighted percentiles (the file's lines set on its starting weights, then held
fixed in dollars through the tilt). Four net worth classes between the SCF's dollar lines at p90, p99 and p99.9
(pooled over the waves: $986,300, $8.95 million, $39.50 million; the 2019 wave alone $957,700, $9.21 million,
$38.66 million). In each cell the share of the income group's units in the class (the lowest class left out: one
minus the others) and the class's net worth. A cell resting on fewer than 10 households of the 2019 wave is merged
into the next richer class of its income group (three cells; kept after pooling, which roughly triples each cell's
households). Without the grid the tilt left $6.7 trillion with units at p99.9+ of income against the SCF's $10.6
trillion, and 12% of the top 0.1% by wealth had income below p90 (SCF: 3.9%); with it, $9.0 trillion and 6.3% (26 October,
ranges of one standard error, targets from the 2019 wave alone; `HISTORY.md`).

Counts by kind as in `tilt/<run>/targets.csv`.
`hold_distribution` (optional, S7) adds each item's share in AGI rank groups of the start file as targets.
`report_tilt.py` writes the run report (section 14).

## The non-microdata imputations

Built 28 October on branch `rebuild-nonmicro` (John's decisions Q1-Q9 of 28 October; scratch
`explore/2026-10-28/rules_framework/PROPOSAL.md`). These are items set from what the model has already drawn plus a
published table, with no new microdata. Code: `pipeline/non_micro_imputations.py`, one function per item; settings
`non_micro_imputations`, which registers each item: the columns it reads, the published tables it reads, the columns it
writes, its random stream and the reason for each choice. The tables are CSVs in `research/rebuild/resources/non_micro/`
(a year column where the numbers change by year), built from their sources by `pipeline/build_non_micro_tables.py`
(stage `non_micro_tables`, run only when a source changes); `sources.csv` there gives each table's source.

**Where they run** (Q1). Inside the tilt stage, on the start: after every dollar amount is in target-year dollars and
the weights are prescaled, before the solve. They add new columns only and never change a drawn item or a weight, with one exception: a primary filer with an age and no sex (two records in the 20261019 model's generation, ids 20019566 and 20035145, both unmarried filers aged 39 and 38 with sex 0) is given a sex, man or woman with even chances, by the death item's keyed draw, written back to `sex_primary` before any item runs (`fill_missing_sex`). A second exception since 29 October (branch `rebuild-amt-rule`): an item may change a drawn column it lists in its settings `writes_back`, keeping the drawn value in a new column; only the AMT rule does (`amt_nols`, its drawn value kept as `amt_nols_drawn`). The
two items that rank records (death probabilities, parents' rank) rank on the prescaled start's weights, and the ranks
are held fixed through the tilt. No target reads the new columns yet, so the weights do not depend on them: on the
latest run's stack the tilt gives the installed weights exactly, to the last digit. They can also run on a finished
file, for checks (`python non_micro_imputations.py --run <tilt folder> --out <folder>`); imputing again on a finished
file gives its columns exactly.

**Random numbers.** Each is a fixed function of the record id, the item's stream number and the person
(`ages.uniform_by_id`, salt = `salt_base` + 8 × stream + person; person 0 the unit, 1-2 the adults, 3-5 the
dependents), so a draw never moves when other code or other records change. Streams are numbered once in the settings
and never renumbered.

**Years.** Each item reads its table's row for the target year; with no such row, the last year before it. The run's
`summary.md` says which row each item used.

| Item | Columns | Method | Published table |
|---|---|---|---|
| Dependent filers' exact age and sex (Q2) | `dep_filer_age`, `dep_filer_sex` | each dependent filer gets an age and a sex drawn from the CPS's dependents whom the filing stage counts as filing their own return, in the record's CPS year, within its PUF dependent-filer age range (since 2 November 2026; before, the dependents with earnings above the dependent filing threshold, which put 66-79% of the 18-25 range at 18); 80 means 80 and over | Form 1040 Chart B thresholds; the filing stage's persons (`data/filing/returns/persons_<year>.parquet`) |
| Child tax credit eligibility | `dep_ctc1-3` | production's rule (`src/imputations/ages.R`): dependent k (youngest first) passes every test but age and Social Security number when under 17 and `n_dep_ctc` is at least k; 17-year-olds pass with production's chance; dependents from 17 to the year's law age less one by chance | the credit's age limit by year (IRC 24(c); under 18 in 2021) |
| Dependents' Social Security numbers | `dep_ssn1-3` | since 29 October (branch `rebuild-retrain-1029`) not a non-microdata item: `dollars.py` makes `ssn1`, `ssn2` and `dep_ssn1-3` from the model's drawn status (below, "The retrain of 29 October"); before, a placeholder (every dependent has a number) | |
| Death probabilities | `q_death1`, `q_death2` | production's three layers (`src/mortality_ledger.R`), no draw: SSA life table × Chetty income ratio (percentile within sex and single age, start weights) × Johnson marital factor for men; dependent filers use their drawn age and sex | SSA period life tables (2024 Trustees Report), Chetty et al. (2016) Table 15, Johnson et al. (2000) |
| Yearly unrealized gains (Q6) | `accruals.equities`, `.pass_throughs`, `.primary_home`, `.other_home`, `.re_fund`, `.dc`, `.trusts` | value × the year's Z.1 revaluation rate (households and nonprofits): stocks at corporate equities', businesses at noncorporate equity's, real estate at real estate's, trusts at 60% stocks and 40% bonds; retirement accounts at a stock share (since 29 October the SCF's, `w_dc_stock_share`, learned in pretraining and drawn with the balance sheet; before, and for a file from an earlier model, production's age rule, 1.10 − age/100 held to 0.20-0.95, which the code dropped on 3 November: a file without the drawn share now stops the stage); years without a full year of Z.1 data take production's long-run rates | Z.1 table S1M.i.a, release of 10 September 2026, 1990-2025 |
| Cost basis of each asset | `basis.equities`, `.pass_throughs`, `.primary_home`, `.other_home`, `.re_fund` | production's rule: value − the SCF's unrealized gain (other real estate's gain split by value), the gain first held to no more than the value so a basis is never below 0 (29 October; the gain and the value are drawn separately) | |
| Holding period and basis of long-term gains (Q7) | `kg_lt_years_held`, `kg_lt_basis` | production's method (`src/imputations/capital_gains.R`): holding group drawn with SOCA's gain (or loss) dollar shares, period within the group from a Weibull shape (exponential above 20 years), then held to at most the older adult's age less 18 and at least 1 year (29 October); basis from production's SOCA regression evaluated at the target year's trailing S&P 500 returns at the held period | SOCA panel and shares, S&P 500 total return index |
| Parents' income rank (Q8) | `parent_rank` | production's method (`src/imputations/mobility.R`): income rank among all units (start weights), bottom 10 merged into rank 10, parent rank drawn from Chetty et al.'s matrix | Chetty, Hendren, Kline and Saez (2014) |
| Blindness | `blind1`, `blind2` | since the second retrain of 29 October (branch `rebuild-retrain-1029`) a rule: each primary filer, and each spouse on a married-joint return, blind with chance = the 2015 PUF's blind rate for the person's age range × one factor per person type, the factor set so blind filers at the start's weights equal Publication 4801's boxes for the target year (below, "The second retrain of 29 October"); before, a PUF slot learned in pretraining | the PUF's blind rates by age range (`puf_blind_rates.csv`); SOI Publication 4801's blind boxes (`pub4801_blind_boxes.csv`) |
| AMT net operating loss adjustment held to the loss | `amt_nols` (changed), `amt_nols_drawn` (the drawn value) | since 29 October (branch `rebuild-amt-rule`; John): `amt_nols` (the PUF's P60100) is held to at most the record's net operating loss, the negative part of `other_agi_remainder` (the PUF has no field of its own for the loss; a net operating loss deduction is a negative amount in other income, Form 1040 line 21, which sits in the AGI remainder), so it is 0 on a record with no loss; a negative `amt_nols` is left as drawn. `amt_nols` is grown from 2015 by AGI per return since 29 October (before, it stayed in 2015 dollars), the loss is in target-year dollars. No target reads `amt_nols`, so the weights do not move. Below, "The AMT rule of 29 October" | |
| QBI service flags and wage bills | `sstb_*`, `wagebill_*` (sole proprietorship, partnership, S corporation, farm) | since 29 October (branch `rebuild-retrain-1029`) the simple plan's rule on the SCF items the model draws (below, "The retrain of 29 October"); 0 for a file from an earlier model | the CPS's average pay of private employees by SCF industry group (`cps_private_pay_by_scf_group.csv`) |

Conventions as in the rest of the file: −1 for an age or a person that does not exist, 0 for no sex; the flags are 1
or 0, −1 where the dependent does not exist; `q_death2` is −1 without a spouse; `kg_lt_years_held` is −1 and
`kg_lt_basis` 0 where there is no long-term gain or loss.

Two departures from production, both stated in the settings: production's code sends the bottom 9% of incomes to
parents' rank 100 (R's `cut()` gives them no group and `replace_na(100)` fills it), against its own comment; the rebuild
uses the stated intent (rank 10). And in historical years the bond part of retirement accounts and trusts takes the
year's actual Z.1 debt-securities revaluation (5.4% in 2019), where production's long-run rate is 0.

**Checks** (Q9: reported only, never a stop). On the weights after the tilt, in `tilt/<run>/non_micro_checks.md` (and
`non_micro_check_*.csv`), section 6 of `report.md`. On the latest run's stack, 2019, counts in millions, nominal 2019
dollars:

| Item | File | Published |
|---|---|---|
| dependent filers | 9.81, 52% men; ages 15-17: 1.97, 18-25: 7.75, 26 and over: 0.09 | SOI Table 1.7: 9.55 (no ages published); the CPS pools' age shares within each range by construction |
| children under 17 flagged for the credit | 68.91 (66.63 on filers' returns); units with one: 41.6 | Census population under 17: 69.00; children on returns claiming the additional child tax credit (Schedule 8812 line 4 ÷ $1,400): 35.99, a lower bound |
| expected deaths, heads and spouses | 2.566 million among 241.0 million people; deaths per 1,000 within 6% of SSA's in every age band and sex (within 2.5% from 35 to 79) | SSA life table × Census population 18 and over: 2.847 million among 256.9 million |
| yearly unrealized gains | stocks $3,144 billion, businesses $1,246 billion, real estate $1,056 billion, retirement accounts $2,203 billion, trusts $436 billion | Z.1 2019 holding gains: corporate equities $3,615 billion, noncorporate business $654 billion, real estate $832 billion |
| long-term gains by holding group | each group's share of gain dollars within 2.4 points of SOCA's; basis $1,727 billion on $947 billion of gains | SOCA's gain shares; the regression's basis-to-sales ratio by group (0.78 under 18 months to 0.31 at 20 years and over) |
| cost basis, after holding each gain to its value (29 October) | records changed, weighted units, and basis added: stocks 12,200 (1.64 million, $775 billion); businesses 15,874 (1.87 million, $846 billion); home 3,338 (0.80 million, $145 billion); other homes 8,012 (1.15 million, $449 billion) and other real estate 7,376 (1.35 million, $264 billion), one gain on both, 11,230 records (1.81 million units); no basis below 0 except 474 records (0.04 million units) where the drawn value of other real estate is itself negative | the SCF's gain and value are drawn separately |
| holding periods, held to the older adult's age less 18 (at least 1 year; 29 October) | 10,150 of 151,981 records with a long-term gain or loss changed (1.57 of 19.31 million units); longest period 182 years before, 62 after; mean for gains 11.5 to 10.2 years; basis of long-term gains $1,959 billion to $1,980 billion | |
| parents' rank | mean parents' rank by own-rank decile within 0.4 of the matrix's | Chetty et al.'s matrix (a check of the draw) |

The business line differs from Z.1 mostly in what is held: the SCF's businesses ($21.6 trillion in the file) include
S and C corporations the household runs, Z.1's noncorporate equity ($11.4 trillion at the end of 2018) does not.

## The retrain of 29 October (branch `rebuild-retrain-1029`)

The bundle John set on 28 October (`NEXT_STEPS.md`, "The next retrain, one bundle"), one commit (or a commit and its
follow-up) per piece, so any piece can be dropped. Everything here changes the training table or generation, so it
takes effect only with newly trained networks. The table-stage checks are in scratch
`explore/2026-10-29/retrain/RESULTS.md`.

**Social Security number status** (Q4; branch `rebuild-ssn` merged, 8a4aa2c; scratch
`explore/2026-10-28/ssn_build/RESULTS.md`). `build_cps.R` gives every person in a CPS tax unit a status: 1 a citizen,
2 a noncitizen with a sign of lawful status, 3 a noncitizen without one, taken to have no Social Security number (rule
B2: for a child its own Social Security, SSI, Medicare or military cover, a citizen parent, or a noncitizen parent with
a sign; for the filer and spouse their own Social Security, SSI, Medicare, Medicaid, military cover, unemployment
insurance, public housing, a government job, veteran status, or marriage to a citizen). The table has five CPS-only
slots, `ssn_status_primary`, `ssn_status_spouse`, `ssn_status_dep1-3` (0 none, then the status), in primary-filer
order; generation draws them with the other CPS items (the k-th dependent's exactly when there are k dependents, the
primary filer's never none, as the primary filer's sex is never none). `dollars.py` turns them into `ssn1`, `ssn2` and
`dep_ssn1-3` (1 has a number, 0 none, missing where there is no such person; `ssn1` missing on dependent filers, whose
survey items are all none). The non-microdata placeholder for dependents' numbers is removed (the two would write the
same columns). In 2019, 1.03 million dependents under 17 lack a number (DHS: 1.01 million unauthorized children under
18 in 2020, reported only).

**Blindness learned from the PUF** (Q3; replaced by a rule in the second retrain of 29 October, below; this paragraph
describes the first retrain). A PUF slot `blind` with three values: 0 not known, 1 not blind, 2 blind. It is known only
on returns that are single or head of household, take the standard deduction, are not dependent filers and have an
age range: there the deduction above the basic amount ($6,300 single, $9,250 head of household in 2015), divided by
the additional amount ($1,550), is the number of extra boxes (a whole number on 97% of these returns), and blind =
the boxes less one at 65 and over. Joint returns (whose boxes are evened out between the spouses on the 2015 PUF),
separate returns, itemizers, dependent filers, records with no age range and boxes that are not a whole number or do
not fit the age are "not known". "Not known" is a value rather than a slot left out, so that the slot's presence never
tells the network anything about the return (a slot present only on single standard-deduction returns would tell every
other draw that the return is single and does not itemize). The surveys do not have the slot. Generation draws
blindness last, for the primary filer and the spouse alike: the network under the PUF label, the slot hidden, the
person's own age range shown (the spouse's from the drawn exact age; a dependent filer's under-18 and 18-25 ranges as
under 26, its 26-and-over range hidden), married and full filing status hidden on joint and separate returns (the
network never saw blindness beside them), and only "not blind" and "blind" allowed (the chance of "not known" is set
aside). Non-filers are seen with the filing flag hidden and their band-AGI group as the AGI group label. The draws have
a random generator of their own, so no other draw moves. The stack carries `blind_drawn1`, `blind_drawn2` and the
chances `blind_chance1`, `blind_chance2`; `non_micro_imputations.blindness` writes the file's `blind1`, `blind2`, and
its check compares blind people by age with Publication 4801's blind boxes (typed in from the publication,
`resources/non_micro/pub4801_blind_boxes.csv`: 2019, 260,291 primary and 80,564 spouse boxes on all returns).

**The retirement accounts' stock share as an SCF item** (Q6; `build_scf.R`, `build_table.py`). The SCF summary file's
equity in quasi-liquid retirement assets over those assets, RETEQ / RETQLIQ (RETQLIQ = IRAKH + THRIFT + FUTPEN +
CURRPEN, the rebuild's `value.dc` plus `value.db`; RETEQ never exceeds it, but it exceeds IRAKH + THRIFT alone in about 1,300
rows a wave, so the share is taken over all four and applied to `value.dc`). RETEQ counts each account as all, half or
none in stocks by the respondent's answer. A household ratio, the same on each of the household's PEU units; 0 where
the unit has no `value.dc`; missing (not shown in training) on units outside the PEU and other units, as their elective
deferrals. In the table it is the SCF slot `w_dc_stock_share`, a share of `value.dc` (meta `ratios`: generation draws it
after `value.dc` and sets it to 0 without `value.dc`; it is not grown between waves; `dollars.py` also makes `w_dc_stocks` =
share × `value.dc`). The weighted share is 0.53-0.55 in every wave. `non_micro_imputations` uses the drawn share for
`accruals.dc` in place of production's age rule.

**CPS capital gains not shown before tax year 2018** (`build_cps.R`, `translate.not_asked`, `build_table.py`; since the
second retrain of 29 October not shown in any year, below). The
CPS asks capital gains from tax year 2018 only. Until 28 October the 2015 and 2017 CPS rows carried them as zero, which
taught the model that CPS units have no gains. Now `build_cps.R` writes them as missing in those years and the
translation table's row (first_year 2018) leaves `kg_lt`, and `capital_gains` computed from it, missing on those rows:
absent, not shown in training, as any item a survey does not ask. Nothing else in the table moves: the tax data's
non-filers of 2015 and 2017 (the CPS of those years) still read the item as zero, as before, and the fill-in runs only
from 2018, as before (`fillin.py` now allows the item to be absent in the years it does not fill in).

**The overtime targets' wage groups** (`targets_config`; above, "Overtime" under the targets). The top group, $200,000
and over, is merged into $100,000 and over (catalog `wage_groups_4`): 10 overtime targets instead of 12, 787 targets in
all. Targets only; no retrain needed for this piece.

**The QBI items** (Q5; the simple plan, scratch `explore/2026-10-28/qbi_plan/SIMPLE_PLAN.md`, research in `PLAN.md`
there). Two parts.
- *SCF items learned in pretraining* (`build_scf.R`, `build_table.py`). From the household's actively managed
  businesses 1 and 2, for each of three kinds by legal form: Schedule C (sole proprietorship, or "not a formal
  business"), partnership (partnership, or limited partnership, LLP or LLC) and S corporation; other corporations get no
  item. `biz_industry_<kind>` (a category): the industry group of the household's largest business of the kind, largest
  by the value of the household's share, the SCF public file's 7 groups, 8 for "no industry code" (the businesses worth
  $100 million or more), 0 with no such business. `biz_workers_<kind>` (an amount, a count not grown between waves): the
  owner's share of outside workers, the business's workers less the household members working in it, times the
  household's ownership share, summed over the kind's businesses. Given, as `owns_business`, to the units holding the
  household's business equity, the workers in proportion to each unit's equity; 0 on units outside the PEU. Generation
  draws each kind's workers after its industry group (zero with none) and never draws "none" for the industry group of
  a record with nonzero income of that kind (its PUF parts: Schedule C; the partnership parts; the S corporation parts).
- *The rule* (`non_micro_imputations.qbi`, settings `non_micro_imputations.items.qbi`), for each record and kind with
  positive income of that kind: specified service (`sstb_<kind>`) when the record's keyed draw is below b × c(group),
  at most 1, with c(group) the plan's fixed chance per SCF group (6: 0.16, 3: 0.09, 5: 0.05, 1: 0.02, the others 0.01;
  for "no industry code" the average of the seven weighted by the kind's positive income on the file); wage bill
  (`wagebill_<kind>`) = a × the drawn owner's share of outside workers × the average pay of private employees in the
  group in the target year (from the CPS ASEC, `build_non_micro_tables.cps_private_pay`, by the SCF codebook's collapse
  of Census industry codes; "no industry code" takes all private employees), plus, for an S corporation, all the
  record's wages when it has S corporation income and no Schedule C or partnership income (first version). Farms:
  never specified service; wage bill = positive farm income. The dials a and b are 1 for now (solved later against
  Treasury's and SOI's numbers, with the calculator); a, b and c are settings, so changing them needs only a rerun of
  the tilt stage. No QBI targets.

## The second retrain of 29 October (branch `rebuild-retrain-1029`)

Four changes to the first retrain, set by John on 29 October after the diagnosis in scratch
`explore/2026-10-29/diagnose/RESULTS.md`. Results in scratch `explore/2026-10-29/retrain2/RESULTS.md`.

**Blindness is a rule, not learned** (`non_micro_imputations.blindness`; `build_non_micro_tables.puf_blind_rates`;
settings `blindness` and `non_micro_imputations.items.blindness`). The PUF slot learned in pretraining drew 2.95
million blind primary filers in the 2019 file against Publication 4801's 260 thousand boxes: generation asked the
network about joint returns and itemizers, where training only ever showed "not known". The slot is gone from the
training table and the draw from generation. In its place, in the non-microdata step (after generation, before
reweighting):
1. The PUF's blind rate by the primary filer's age range, on the 62.5 million 2015 returns where the return shows it
   (single or head of household, standard deduction, not a dependent filer, an age range; read as before), is a small
   table, `resources/non_micro/puf_blind_rates.csv`: about 0.10% under 26 up to 0.86% at 65 and over.
2. Each primary filer, and each spouse on a married-joint return (filing status 2), is blind when a draw keyed by
   record id and person (stream 6; no other draw moves) falls below the rate for that person's age range times a
   factor, one for primary filers and one for spouses.
3. Each factor is set so the file's weighted count of blind filers, at the start's weights (before reweighting),
   equals Publication 4801's boxes for the target year (2019: 260,291 "you are blind" and 80,564 "spouse is blind").
   Non-filers are drawn by the same rule and not counted.
The check (`non_micro_checks.md`) gives the counts before and after reweighting against the boxes.

**The CPS carries no capital gains in training** (settings `translation.table`, `in_training` false on the CPS's four
capital gains rows; `translate.in_training`). In every CPS year the four pieces and the total are missing on the
training rows (not shown), and there is no fill-in for them. The rows still give the tax data's non-filers their gains
(from tax year 2018), so the tax data, and every other survey's translation against it, do not move. The non-filer
batch, drawn under the CPS label, now gets zero capital gains, as it does for any PUF item the CPS does not measure.

**A total with a missing part stays missing** (`fillin.FillIn.recompute_totals`). The fill-in recomputes the CPS's
totals from their parts after each draw. A missing part (band -1) was read as the top band of its dollar table (Python
reads index -1 as the last), which wrote a capital gain at the top band onto every 2015 and 2017 CPS training record
in the first retrain and raised the held-back CPS loss from 0.37 to 0.45-0.50. Now the total stays missing, no band
table is read at -1 (also in the fill-in's log), and an assert stops on a negative band where every part is present.

**Two training options, both off:**
- `scf_one_implicate_per_pass` (from branch `rebuild-implicate1`, b12c683): each pass trains on one of the five
  implicates of each SCF training household, chosen at random by the seed and the pass, its rows counting 5 times
  their weight; the learning-rate schedule counts the steps taken. Not adopted; the setting and its code were taken
  out on 3 November.
- `overfit_log` (settings `training`; from branch `rebuild-overfit-diag`, 448d148): the loss per slot on the held-back records and on a
  fixed sample of training records each pass (`slot_log.csv`), and the network every 10 passes. Logging only; training
  is the same either way.

## The AMT rule of 29 October (branch `rebuild-amt-rule`)

A non-microdata item, `amt_nol_cap` (John, 29 October): the AMT net operating loss adjustment (`amt_nols`, the PUF's
P60100) is held to at most the record's net operating loss, and is 0 on a record with no loss. The PUF has no field of
its own for the loss; a net operating loss deduction is reported as a negative amount in other income (Form 1040 line
21), which is in the AGI remainder, so the loss is the negative part of `other_agi_remainder`. A negative `amt_nols`
is left as drawn. The drawn value is kept as `amt_nols_drawn`. The model draws `amt_nols` and the remainder
separately; on the 28 October calculator run AMT came to five times SOI's, 88% of it on records whose AMT adjustment
exceeded their whole AGI.

On the 2015 PUF itself (its own weights) the rule would change little: of $66.3 billion of positive `amt_nols`, $1.3
billion is above the record's negative remainder ($0.55 billion of it on records with no negative remainder), and the
median ratio of the adjustment to the loss is 0.94. On the second retrain's pooled file (version A, `five`, 2019): 5,096
of the 7,957 records with a positive `amt_nols` are changed, 2,683 of them because they have no loss; 0.21 million
units at the start's weights and $46.0 billion of $74.8 billion removed; after the tilt, 0.18 million units and $34.3
billion of $58.0 billion removed (filers; non-filers carry no `amt_nols`). Imputing again on that finished file
reproduces every other column exactly. No target reads `amt_nols`, so the tilt's weights do not change.

## The billionaires and one reweighting with them (branch `rebuild-top-billionaires`, 29 October)

Parts B and C of the plan John approved on 29 October (scratch `explore/2026-10-29/top_research/PLAN.md`, steps 10-21),
with Part A merged in from `rebuild-top-hidden` (the PUF's four aggregate records rebuilt from generated records,
`pipeline/top_hidden.py`, and Table 4.3's top 0.001% restored as a target; its own record is scratch
`explore/2026-10-29/top_build/hidden/RESULTS.md`). Results in scratch
`explore/2026-10-29/top_build/billionaires/RESULTS.md`.

**Where it runs.** `tilt.main()` runs the steps of `top_records.py` on the start, in target-year dollars, before it
saves the population masks, builds the target conditions or does the demographic prescaling (settings `tilt.top`;
`enabled` true). In order: an earlier year's billionaires are removed when the start is an earlier year's file; Part
A's rebuilt returns are added (`tilt.top.hidden_returns`, generated starts only; in the generated records' weight units,
so the start's scaling grows them with the count of returns; free in the tilt; `synthetic_top` and `hidden_group` mark
them; the build is cached in `<gen>/hidden`); generated balance
sheets at $1 billion or more are drawn again; the year's Forbes list is added at weight 1. The ordinary non-microdata
step then runs on the whole file. Table 4.3's top 0.001% (returns, AGI and the eight items) is a target again while
`tilt.top.enabled` is true (its entries' `must_reach` cells, which `target_list.expand` must keep); with it false they
are left out and the pipeline runs as before 29 October.

**Above $1 billion the file is the Forbes list alone; below it, the SCF** (John's choice, a labelled assumption).
1. The SCF targets are on the SCF's units below $1 billion of net worth (catalog population
   `scf_units_below_a_billion`, net worth in nominal target-year dollars after each wave is grown): every amount, share,
   grid cell, percentile line and standard error is computed on that domain of each wave (`scf_targets.wave_cell`,
   inside the full sample, so the replicate weights and implicates are used as they are; the restricted SCF is not
   scaled back up). The file's side uses the same ceiling (`target_list.population`).
2. Any generated record whose net worth comes out at $1 billion or more has its whole SCF block drawn again
   (`top_billionaires.redraw_balance_sheets`): every SCF-only slot, under the SCF label given everything else on the
   record, as generation drew it (its model, its SCF wave, the consistency rules with that model's offset factors).
   20 tries per record (`redraw_tries`), drawn as one batch from generators seeded by the record id alone, each try's
   dollar picks keyed by the record id; the first try below $1 billion is kept, else the lowest, flagged
   `wealth_redraw_fallback`. The file carries `wealth_redrawn`. One function, `top_billionaires.redraw_record`, does
   this for the generated records and for Part A's rebuilt returns (its step 7, with settings `top_hidden.wealth_*`).

**The lists** (`resources/top/forbes_billionaires_<year>.csv`, 2015-2026, built by `build_forbes.py` from Forbes's own
data for each year's World's Billionaires list; 2 November 2026, before then 2019 only): tax year t takes the March or
April list of year t. US citizens with $1 billion or more: 535 (2015) to 991 (2026), within two of Forbes's published
counts (2019: 607, $3,111.0 billion, as before). Marital status, children and public holdings are not in the lists'
data; they come from production's later profiles (Forbes's 2025 data), as the public holdings' share of the person's
2025 net worth times the list's net worth. Spouses listed separately become one joint return (the search redone for
every year: same-surname groups, biographies naming a US billionaire spouse, and entries sharing one fortune; from 2023
Forbes lists a couple's shared fortune as two entries of half each): 1 to 9 couples a year, `SPOUSE_PAIRS` in the
builder; ex-spouses and couples whose divorce was filed before the list are separate. A family fortune, or a couple or
siblings named in one entry, stays one record. In a list year the records have weight 1 and are held fixed.

**After the last list (2027-2035; John, 2 November 2026)**: the 2026 list grown to the year (settings
`tilt.top.after_last_list.grow`; `top_billionaires.grow_list`): every fortune grown from 2026 by one factor, chosen
so that the list's net worth on its starting weights grows as Z.1 household net worth, the series of the file's own
net worth target (John, 4 November 2026: "I want them to grow at the same rate"; settings
`tilt.top.after_last_list.wealth_growth` 'net worth', `top_records.net_worth_growth`; 2027: x 1.030, Z.1's 1.043 over
the list's weight growth). The list then keeps its 2026 share of the file's net worth (4.5% in 2026, 4.3% in 2075;
scratch `explore/2026-11-04/listgrowth/shares.csv`). Until then ('parts') each part grew by the rule of its balance
sheet item (public stock and private business by the Financial Accounts, then GDP, per return; real estate by the
SCF rule), and the list's share rose to 5.8% by 2075. Ages raised; income then computed from the IRS study's ratios as in a list year. **Their weights grow like any other
person's**: each starts at 1 times its demographic cell's population growth from 2026 to the year (the start scaling
every projection-year record gets from 2023); no count target for them. **They are held fixed at that starting weight
in the solver** (2 November 2026; kept as the rule by John on 4 November: after the last list they are records like any
other, aged naively, and not moved by the solver), as the list years hold theirs at 1 (settings
`tilt.top.after_last_list.fixed` true; `top_records.add_list`); their dollars count toward every target, so the other
records make room for them. Left free (`fixed` false), the solver took the ten richest to weights near zero in every
year 2027-2035 and the list's net worth fell from $8.4 trillion (2026) to $3.9 trillion (2027) (scratch
`explore/2026-11-02/billionaires/RESULTS.md`): a record of weight 1 holding billions of dividends and gains is the
cheapest place to cut, because the cost of moving a weight is relative to its size.

**No other record at or above $1 billion in a list year (John, 4 November 2026; `top_records.hold_below_ceiling`).** The
list is every US billionaire, so in a year with a Forbes list a record not on it at or above the ceiling has its whole
balance sheet (net worth's assets and debts, the unrealized gains `w_kg_*`, the cost bases `basis.*`, `w_dc_stocks`)
multiplied by one factor that puts its net worth at 0.999 times the ceiling; its income, weight and every other column
are unchanged, and `held_below_ceiling.csv` lists them. In 2024-2026 these are 2023 records that growth carried past $1
billion (82, 209 and 292 records; $0.09, $0.21 and $0.42 trillion before, factors 0.72-1.00); in a generated year they
would be redraw fallbacks (none since 2 November). After the last list such records stay: they stand for the people who
become billionaires after it (scratch `explore/2026-11-04/ceiling/`). The
network part of the top step (Part A, the redraw) is cached (`REBUILD_TOP_CACHE`) and the list is built on every run
(`top_records.network_part`, `add_list`).

**Each billionaire record** (`top_billionaires.build_billionaires`; flags `billionaire`, `billionaire_rank`,
`forbes_uri`; ids from 95,000,000, after Part A's block at 90,000,000):
- Rank groups: the top 92 units, the next 276 (BSYZ's "top 100" and "next 300", as tax units), and the rest, which take
  the next group's figures (an extrapolation).
- Wealth: Forbes's net worth; public holdings as above, else the rank group's wealth-weighted average share; the rest
  private: real estate (`value.re_fund`) when Forbes's industry is real estate, otherwise `value.pass_throughs`. No home, no
  debt. Basis zero for self-made fortunes (Forbes's 2019 flag), equal to value for inherited ones, written as
  accumulated gains so the non-microdata step's `cost_basis` gives it.
- Income: each group's dollars of each kind = its Forbes wealth times the IRS study's amount per dollar of wealth
  (Balkir, Saez, Yagan and Zucman, IRS study Table 1, `resources/top/irs_wealth_groups_2018_2020.csv`: the same as
  wealth x BSYZ's fiscal-income ratio x BSYZ's shares, on the underlying amounts). Kinds: capital gains and dividends,
  shared among the group's units by public stock; business profits, and business losses separately, by private
  holdings; taxable and tax-exempt interest, rental and royalty profits, rental losses, estate income, other income
  and wages and pensions by total wealth. Net operating losses are left out.
- Tax items: each kind split as in the item mix group (Part A's rebuilt returns of the over-$100-million aggregate
  record, `hidden_group` 3, in target-year dollars; without Part A a stand-in, the generated filers with AGI of $100
  million or more), except partnership against S corporation
  profits and losses, from the IRS study. Schedule C and farm profits go only to the mix group's share of units
  holding them (losses likewise), chosen by a draw keyed by record id and shared among those by private holdings:
  spread over every unit, the list alone exceeded Table 4.3's count of top-0.001% returns with a Schedule C or F
  profit (255 against 243), and the solve could not settle. Items beyond the kinds (state income tax, foreign tax credits, business
  credits, self-employment items, other itemized deductions) at their ratio to AGI in the mix; consumption at its
  average; Social Security, credits that phase out, and the rest listed in `ZERO_ITEMS` zero. Pensions are taxable
  pensions alone (the non-taxable rest zero); taxable IRA distributions stay their own item and, since 3 November, are
  no longer counted in the units' gross retirement income.
- Charity: gifts of 0.7% of wealth in the top group and 0.6% in the rest (BSYZ Table 2), cash and other as in the mix,
  entered as gifts; the calculator computes the deduction.
- People: Forbes's 2019 age (held to 80), sex; married from the later profiles (missing: married, production's rule),
  a spouse of the same age and the other sex (a separately listed spouse keeps their own); dependents by production's
  rule; all earnings to the primary filer.
- The non-microdata step places billionaire people at income percentile 100 for the death probabilities.

**One reweighting.** In a list year `solver.solve(..., fixed=)` holds the billionaires at weight 1: their contribution is taken off
each total, an average's fixed members enter as a constant and in the group weight its range is set on, and only the
other weights are solved for (also in the demographic prescaling; the start's scaling to SOI's returns counts them).
Before the solve, `solver.fixed_feasibility` checks each target: what the billionaires contribute, what is left, and
whether the others can reach it (`top_feasibility.csv`). SOI's income targets apply to everyone, the SCF's to the units
below $1 billion. The tilt folder also gets `billionaire_units.csv`, `billionaire_groups.csv`, `redraw.csv` and
`top_notes.md` (the item mix used).

**Growth and the allowed capital loss** (`tilt.grow_dollars`, found by Part A): a return held at the allowed loss
($3,000; $1,500 married filing separately) stays at it after growth. The rule grows the positive "other" gain piece but
not losses, so such a return used to turn into a gain.

**With the merges of 29 October.** The AMT rule (`amt_nol_cap`) runs on every record in the non-microdata step, Part A's rebuilt
returns included: their `amt_nols` was scaled to the aggregate records' dollars, and the rule then holds it to each
record's loss.

## Every year, 2015-2035 (branch `rebuild-targets-all-years`, 30 October)

**Historical years 2015-2023**, each with its own folder in `targets_config/`, a copy of 2019's edited for the
year (what differs is written at the top of each `targets.yaml`): a fresh draw with the year's survey mix (section
7's rule computed for the year in `prior.yaml`), the year's own SOI, SCF, CEX, CPS and W-2 numbers, then the tilt.
What the code does for a year other than 2019:
- the target year's dollar maps from the table already built (`build_table.py maps`, stage `maps`), so the installed
  networks stay valid;
- the SCF at an income year with no wave: each item's mean per tax unit interpolated geometrically between the waves
  either side; after the last wave grown with household net worth per person, Financial Accounts S1M.b over the
  Social Security area population, at a fixed ratio to GDP after 2025 (`build_table.scf_mean`, `scf_units_at`);
- even years (no CPS year): the non-filer count and people the geometric mean of the CPS years either side, the batch
  drawn half at each (`target_list.nonfiler_count`, `generate.nonfiler_years`);
- Table 1.4's lines under their later names (wages from 2022; partnership and S corporation separately from 2021,
  families `partnership` and `scorp`); Table 4.3's items under a year's own name (catalog `item_aliases`); 2018's
  Table 1.3 with IRA distributions and pensions as one line; published net loss amounts read as sizes (Table 4.3
  prints some as negative numbers); W-2 Table 2.D's AGI groups moved to its finer rows from 2021;
- growth: repealed deductions not grown past their last published year; direct series past their end by personal
  income per person; NIPA PCE by the CEX's categories from 2010 (`resources/growth/nipa_pce_categories.csv`).
No W-2 targets before 2019 (older workbook layout). Forbes lists for every year 2015-2026 since 2 November (above).

**Years after the data, 2024-2035** (`kind: projection`; to be redesigned, `NEXT_STEPS.md` item 1). Since 1 November
(decision 74) every projection year starts from the 2023 file (`from_year: 2023`), not from the year before, so the
years can run in parallel. The start: each record's weight grown by its demographic cell's population growth from 2023
to the year (`projection_demographics.population_cells`), each record's dollars grown by its item's source compounded
from 2023 to the year and divided by CBO's growth in returns (`projection_growth.plan_factors`; Table 1.4 items by CBO's
February 2026 growth per return of the closest line; gross Social Security, since 2 November, by SSA's benefits paid in
2024 and 2025 and CBO's Social Security outlays from 2026, not CBO's taxable benefits). The targets, each the 2023
file's own level times the cumulative growth to the year: the non-filer count; eight national totals (nine until 3
November, when taxable Social Security stopped being targeted; value kind
`outside`, `target_list.outside_rows`); the counts of all returns, itemizers and AMT returns (CBO's growth in the count,
`target_list.cbo_count_rows`); household net worth by the Financial Accounts' growth; the SCF categories, grown from 2023
by the factors the start's records get, on the SCF's units grown with the file's units (decision 76); and
`hold_distribution` over five AGI rank groups. A start from an earlier year's file drops and remakes the
non-microdata imputations and does not redraw balance sheets at the ceiling.

**Target consolidation (1 November, decisions 71 and 72).** Small count cells are folded into neighbours in every
historical year (credits within each SOI line, farm overall, Table 1.3 separate returns and AGI only, $1m-2m, no
income tax after credits targets; TARGETS.md S12). Targets per past year fell from 852-919 to 737-804; the 2 November
additions (Table 1.7 and the age entries) bring them to 862-929.

**The refund rule recomputes the totals built from the refund (1 November, decision 75).** `taxes.apply_refund_rule`
sets taxable state refunds to zero on records that do not itemize and lowers `other_agi`, which holds the refund, by
the same dollars, so every total equals the sum of its parts on every record (before this, 38,777 records and $15.9
billion in 2023 did not). The chained start's check is strict again (the dry run's workaround, commit ffcee11, is removed).

Results: scratch `explore/2026-10-30/targets/RESULTS.md`.


## Taxes computed inside the model (branch `rebuild-taxes-in-model`, 30 October)

Piece 3 of the overnight plan of 29-30 October (scratch `explore/2026-10-30/taxes/RESULTS.md`; design of 22 October,
`explore/2026-10-22/targets/calculator_api/REPORT.md`). Inside the tilt stage, after the top of the file and the
non-microdata imputations and before the reweighting, every record's federal income and payroll taxes under the
target year's law are computed by tax-simulator-gpu (`pipeline/taxes.py`, settings `taxes`):

1. The file becomes the calculator's input frame (`calculator.convert`, the contract of `CALCULATOR_CONTRACT.md`),
   written as a parquet file keyed by record id.
2. tax-simulator-gpu's batch command, `python -m taxmodel.run.batch` (a module on its local branch
   `taxdata-batch-1103`, not pushed: master 080b8dbb of 3 November with the batch command and the itemized deduction
   categories added; pinned at c89e272 since 3 November, 0bc55a1 before), runs it on the CPU in the calculator's own Python environment, from a copy of that
   repository at the pinned commit; it writes one row per record with every computed column, and a manifest. About 20
   seconds for 457,000 records. It compiles the law of 2014 to 2040.
3. The QBI dials of the non-microdata imputations (`a` for the wage bill, `b` for specified services; scratch
   `explore/2026-10-28/qbi_plan/SIMPLE_PLAN.md`, step 4) are solved on the start's weights from the first run's
   taxable income before the deduction, with the calculator's own QBI formula (checked against the calculator on every
   record: equal to the cent): `a` so that owners of one non-specified business above the phase-in keep Treasury's
   share of 20% of their income, then `b` so that the deduction from AGI $500,000 up equals SOI's; in turn until they
   settle. The QBI columns are imputed again with them and the calculator runs a second time. Since 30 October `a` is
   held at 1 (settings `taxes.qbi_dials.a_range`) and only `b` is solved: the solve of `a` gave 3.00 on the installed
   networks and ran to its limit, 20, on the new ones, with wage bills many times the economy's private wages, because
   most high-income owners of one business have no outside workers drawn. On the new networks b = 0.33.
4. The second run's columns join the file by record id, each the calculator's name with `calc_` in front
   (`taxes.OUTPUTS`): AGI, itemizing and itemized deductions claimed, the standard deduction, the QBI deduction,
   taxable income, income tax before credits, the AMT, the child credit's two parts, the EITC, nonrefundable and
   refundable credits, income tax after credits, the net investment income tax, total income tax, income tax net of
   refundable credits, self-employment and payroll tax, and the marginal rates of income tax and payroll tax on the
   primary earner's wages ($100 more).

The tilt moves weights only, so the computed taxes stay right after it. The calculator targets (kind `calculator`,
`targets_config/2019/targets.yaml`, entries `calc_*`) are SOI Tables 1.4 and 3.3 by the file's AGI, with SOI's CVs
(`targets.T14`, `targets.T33`; `errors.soi_errors`): itemizers and itemized deductions claimed, taxable income, income tax
before and after credits, the child credit's two parts, the EITC and the AMT, 154 targets in 2019. The QBI deduction
by AGI (22 more) is left out of 2019: with it the targets could not all be met (the overnight rule; RESULTS.md). On the
new networks (pieces 1-2) the 2019 file meets all 959 targets and keeps 79.1% of the effective sample (80.7% with the
805 targets before the calculator targets).
`target_list.expand` builds them after the taxes (`which`); with settings `taxes.target` false they are only reported.
Each run writes `tilt/<run>/taxes/` (the calculator's inputs and outputs, `taxes_fit.csv`, `qbi_dials.csv`,
`taxes_summary.md`). `python taxes.py report <file> <folder>` computes the taxes on a finished file and evaluates the
year's calculator targets on it.
## The names: Tax-Simulator's, everywhere (29-30 October)
<!-- old names -->

John, 29 October: "there is no reason for the names to differ. at all." Every variable of the rebuild that is the same
item as an input column of tax-simulator-gpu (master 9424483, 156 columns read) carries the calculator's name, in the
training table, the network's slots, the generated file, the targets configuration, the settings and the reports.
Where the calculator does not read an item but production Tax-Data's file has a name for it, that name is used
(`n_dep_ctc`, the consumption categories).

- **The list.** `pipeline/tools/rename_map.csv`, one row per name: `renamed` (50: the same item under the rebuild's
  old name, for example `mars` to `filing_status`, `interest` to `txbl_int`, `sch_c` to `sole_prop`, the partnership
  and S corporation `pass`/`nonpass` items to `passive`/`active`, `ss_gross` to `gross_ss`, `childcare` to
  `care_exp`, the 20 balance-sheet items `w_<item>` to `value.<item>`, `age_primary` to `age1` and `age_dep1` to
  `dep_age1`), `shared` (the name was already the calculator's or production's), `computed` (a calculator column
  made from rebuild items: each spouse's Schedule C, farm and partnership self-employment income, `dep_status`,
  `male1`, gross pensions, other income, `kg_lt` with distributions, the cost bases, the zeros) and `rebuild-only`
  (no calculator or production name: totals such as `dividends` and `capital_gains`, the survey slots, the shares).
- **Kept on purpose** (`DECISIONS_FOR_JOHN.md` of 30 October): `record_id` (the bare `id` means other things in the
  rebuild; the calculator and Tax-Simulator files write it as `id`); `sex_primary`/`sex_spouse` (coded 1 man, 2
  woman; `male1`/`male2` are computed); the surveys' person slots `tips_primary`, `ot_primary` and the like, because
  the file has `tips1`, `ot1` made from them. The interest paid deduction (PUF E19200) is `first_mort_int`, the
  calculator's column, though it also holds investment interest (production's own name for the whole item is
  `int_exp`).
- **How it was applied.** `apply_rename.py` (in `pipeline/tools/` until 3 November, now in
  `research/archive/rebuild_2026-10/tools/`) applies the `renamed` rows to `research/rebuild` (not
  `HISTORY.md`), and can be run on another branch made before the rename. Names with an underscore are replaced
  wherever they stand as a whole name, with their band, reported, drawn and `u_` forms; one-word names (`interest`,
  `unemployment`, `childcare`, `mars`) only where they are plainly names, not in prose. Names built in code from pieces
  were fixed by hand: the balance sheet's prefix (`value.` for the 20 items in `build_table.py` and `build_scf.R`,
  `w_kg_` kept for the four unrealized gains), and the surveys' own person-1 and person-2 ages, which inside
  `build_table.py` are `svy_age1`, `svy_age2`, `svy_dep_age1`-`3` until the primary-filer rule has run (the R survey
  builders keep `age1`, `age2` for the survey's person 1 and 2).
- **Networks trained before the rename.** Their slot positions are unchanged; `model.load_model` reads their band
  layout's names through the map (`model.renamed`), and checks names and order against today's. Networks saved since
  carry their slot list.
- **Checked** (scratch `explore/2026-10-30/train/RESULTS.md`): the training table built after the rename equals the one
  built before, column for column, apart from the names; a 2019 draw and tilt from the installed networks on the
  renamed code equal those on the code before.

<!-- /old names -->

## Capital gains before the loss limit (29-30 October)

John, 29 October: the $3,000 limit on a net capital loss is computed at tax time; it is not a feature of the data.
Until then the rebuild drew a third gains piece, `kg_other`, Form 1040 line 13 (PUF E01000) less short- and long-term
gains, which on a return with a net loss is the part of the loss the limit holds back, drawn as if it were an item.

- **The pieces.** The training table carries `kg_st` (PUF P22250) and `kg_lt` (P23250), the full Schedule D gains and
  losses, and `kg_dist` (E01100, distributions reported on Form 1040). `kg_other` is gone. The table's total
  `capital_gains` is the sum of the three, before the limit.
- **The limit, where AGI is made.** `common.allowed_capital_gain`: the Schedule D net (short plus long term) with a net
  loss held to $3,000 ($1,500 on a separate return), nominal and not indexed. The file's `capital_gains` (Schedule D
  net in AGI, SOI Table 1.4's line) and `capital_gains_line13` (with distributions) apply it in `dollars.py`, in the
  tilt's recombination of a grown file, in the top step's rebuilt hidden returns and in the band AGI that sets the AGI
  group label. The calculator applies the same limit itself to `kg_st` and `kg_lt`; its `kg_lt` includes `kg_dist`
  (taxsim_file and calculator.convert add them), so the limit falls on short term plus long term plus distributions
  there, and on short plus long term alone in the file. The two differ only on a record with distributions and a net
  loss past the limit: 1 record of the 2019 pooled file of 30 October, $71 (the PUF has no such return).
- **Line 13 on the PUF.** On 6.2 million 2015 returns ($52 billion) line 13 (E01000) is below the allowed Schedule D
  net of the two pieces. By what the pieces show (review of 30 October): on 3.63 million the pieces net a gain (on 1.98
  million line 13 is a smaller gain, on 1.66 million a loss, 1.43 million of them the full allowed loss); on 2.02
  million the pieces are zero and line 13 is a loss (1.63 million the full allowed loss); on 0.55 million the pieces
  net a loss smaller than line 13's. The booklet calls P22250 and P23250 gains less losses net of carryover, so a
  carryover does not explain it. The gaps are not small: half are $3,000 or more, a tenth $100,000 or more. That
  shortfall is put on the short-term piece, so the return's allowed net is its line 13 again and it keeps its loss
  (SOI counts it among the returns with a net loss); about 3.7 million returns whose pieces show no net loss become
  loss returns, and short-term gains on returns that keep a gain are cut. What is left the other way ($0.1 billion on 1.7 million
  returns) stays in other AGI, so each PUF record's AGI is still exactly the PUF's.
- **Growth.** The pieces' losses grow with the gains (Schedule D net gain per return with one, `growth.yaml`); until
  now losses were not grown because they sat at the nominal limit.
- **The SCF.** It asks the amount on the return, where a loss stops at $3,000. Its losses are read as at most $3,000,
  and those held at the limit are put on the tax data's losses past $3,000 by the row's percentile match (the new
  `loss_line` setting, $2,999); smaller losses and gains as before. The CPS carries no capital gains in training.
- **Checked** (scratch `explore/2026-10-30/train/RESULTS.md`): the table before and after, gains by source.

## Wealth and gains by age, and units by age within 65 and over (branch `rebuild-age-targets`, 2 November)

Before this branch no wealth target had an age in it, and the reweighting took net worth from units aged 65-79 rather
than from the young (2022: under 55 $48.8 trillion against the SCF tax units' $38.9 trillion; 80 and over $9.3
trillion against $13.3 trillion). Three entries, in every historical year 2015-2023, each on its year's SCF wave mix and CPS year mix (John, 2
November; projection years untouched):

- `wealth_categories_by_age`: the holders and dollars of the nine SCF balance sheet categories in four groups of the
  primary filer's age (under 55, 55-64, 65-79, 80 and over; catalog grouping `scf_age_groups`), the same form,
  population, wave mix and errors as `wealth_categories` (72 targets).
- `unrealized_gains_by_age`: the holders and dollars of the unrealized gain on stocks and funds, businesses, the primary
  home and other real estate in the same groups, each gain held to its asset's value and never below zero, as
  tax-simulator-gpu has it (catalog variables `kg_*`, definition `gain_held_to_value`; 32 targets).
- `units_by_older_age`: the number of units, filers and non-filers, whose primary is 65-69, 70-74, 75-79 and 80 and over
  (grouping `older_age_groups`), from the CPS's tax units in the training table over the year's CPS mix, standard errors
  from the ASEC's 160 replicate weights (source `cps units`, `target_list.cps_age_rows`, `errors.cps_age_errors`; new
  kind `older ages`, importance 2/3). SOI's Table 1.6 stops at 65 and over.

The primary's age is `age1` on the file (years, 80 and over held at 80) and the training table's `age1` less 1 on the
SCF's and CPS's units (`scf_targets.frame`, catalog variable `primary_age`). An age group is a domain of the SCF's full
sample, so each wave's replicate weights and implicates are used as they are (`scf_targets.wave_cell`). 108 targets a year in
all. Every existing target keeps its value and standard error. The runs of every historical year, on the draw of the
dependent filers fix, are in scratch `explore/2026-11-02/age_targets/RESULTS.md` (2022: net worth under 55 $40.2
trillion against the SCF's $39.8 trillion, $48.9 trillion without these targets; 80 and over $11.7 trillion against
$11.7 trillion, $9.0 trillion without; 51 Newton steps, 59.1% of the effective sample kept against 65.8% without).

## The projection years in three steps (John's design, 3 November; merged from `rebuild-addfactors`)

John's design of 3 November (scratch `explore/2026-11-02/projection_design/DESIGN.md`, "Agreed on 3 November";
`pipeline/projection_steps.py`; results in scratch `explore/2026-11-03/proj_A/RESULTS.md`). Every projection year
2024-2035 starts from the 2023 file and does not depend on any other projection year. A year's targets.yaml switches it
on with `start_scaling: {weights: persons, dollars: item_factor}`.

1. **Step 1, people.** The 2023 weights are calibrated (the smallest Kullback-Leibler move, every count exact) to
   people by marital status and single year of age, 0 to 79 and 80 and over: the primary, a joint return's spouse
   and every dependent with an age, each carrying the unit's weight; a dependent filer's own record counts only its
   dependents; a married person under 18 counts as unmarried, in the file and in CBO's population (John, 4 November 2026; CBO's projections have no married 16-year-olds in 2070-2072). Each count is the 2023 file's count times CBO's growth
   of the same people (Macro-Projections population, settings `tilt.projection.population_dir`). The dependent
   filers' six cells (age range by marital status) are exact too, at their population growth as before.
2. **Step 2, one factor per dollar item.** Each grown column has a row in `targets_config/projection_items.csv`
   (settings `tilt.projection.items_table`): its series (`current`: today's rule; `file:<csv>:<column>`;
   `same_as:<column>`; `none`), kind (`total`: the series is the growth of the item's total, and the factor makes the
   start's total equal the 2023 total times it on the step 1 weights, gains and losses each on their own; `per_unit`:
   the series multiplies each record), whether its total is targeted, and its error.
3. **Step 3, the reweighting.** Targets: the step 1 people (exact; kind `projection persons`); returns in each group
   of the primary's age band (settings `tilt.projection.return_age_edges`) and marital status, and of the dependent
   filers' age ranges, at the 2023 file's filing share times the group's units after step 1 (exact; `projection
   returns`; CBO's count of returns is not used); every targeted item's total on the start, gains and losses
   separately where each has at least 500 records and $0.5 billion in 2023 (judgment, the table's error, 1% for now;
   `projection items`); itemizers and AMT payers at the 2023 file's count times CBO's growth in the count, 5%; household
   net worth and the SCF categories as before, the SCF targets grown by step 2's factors (`scf_targets.
   projection_extension`). No `hold_distribution`. The 2023 Forbes list's records are outside steps 1 and 2; the
   year's list adds its own fixed contribution to every target.

**Add factors** (John, 3 November; branch `rebuild-addfactors`; `projection_steps.add_factor`). A table of extra
growth on top of an item's series, set by hand: `targets_config/projection_add_factors.csv` (settings
`tilt.projection.add_factors_table`), one row per item and range of years, with columns `column`, `first_year`,
`last_year`, `percent` (extra growth in percent a year) and `note`. The item's growth from 2023 to year t is its
series' growth times the product of (1 + percent / 100) over the years 2024 to t that its rows cover; for example +2%
a year from 2024 makes the 2030 growth the series' growth times 1.02^7 = 1.149. It enters step 2 as part of the
series' growth, so the step 2 factor, the item's total target in step 3 and, for a balance sheet item, the SCF targets'
growth all include it. It works for any item in the items table; a `same_as` item takes the other column's factors,
add factors included, times its own. Rules, checked when the table is read: every column must be in the items table;
years are whole numbers after 2023; no item has a year in two rows; and every row with a non-zero percent needs a note
saying who set it, when and why. A run stops if an add factor in force names a column the start did not grow. Each
year's `summary.md` lists the add factors in force (also `projection_add_factors.csv` in the year's tilt folder), and
`projection_step2_items.csv` has each item's `add factor`. The table is meant to be revisited at each CBO update. An
empty table (a header only) gives every item an add factor of exactly 1, and the year is what it was without the
feature (checked on 2026: scratch `explore/2026-11-03/addfactors/RESULTS.md`). The Forbes list's records are outside
step 2, so add factors do not touch them.

## The projection years to 2075 (4 November; merged from `rebuild-longrun`, every year run)

Scratch `explore/2026-11-04/longrun/` (RESULTS.md, DECISIONS.md, MAPPING.md). Years 2036-2075 run the same three
steps as 2024-2035 (`projection_steps.py` unchanged); only the series are longer.

- **Configuration.** `targets_config/2036` to `2075`: copies of 2035 (the same targets; the survey mixes fixed after
  the last wave). Settings `tilt.projection.cbo_last_year` (2035): the last year a CBO projection is used.
- **Series.** Each items-table series continues from its 2035 value at the growth of one Macro-Projections aggregate,
  one row per series column in `resources/projection_long_run_mapping.csv` (production's mapping, `src/project_puf.R`
  income_far, where it has the item: wages with wages, interest with personal interest income, dividends and gains
  with corporate profits, pass-through income with proprietors' income plus rents plus corporate profits, rents with
  rental income, pensions and Social Security with Social Security outlays, consumption and consumer debt with
  consumption; nominal GDP where there is no natural aggregate, and for the balance sheet). Kind `total`: the 2035
  level times the aggregate's growth. Kind `per_unit`: per return over the growth of adults with a married couple
  counted once (unmarried people 18 and over plus half of all married people), per person over the population, a
  price with CPI-U; medical expenses keep their 2035 price relative to all consumer prices. Alimony keeps its yearly
  decline. `resources/growth/build_long_run.py` writes `projection_series_b.csv` and `projection_series_med.csv` (rows
  2036-2075 added), `projection_series_current.csv` and `growth/macro_long_run.csv` (the aggregates and the two
  counts, 2026-2075), and extends the Z.1 and population files of `resources/growth/` to 2075 by their own rules.
- **The 27 items that used today's code rule** (`current`: wages, elective deferrals, overtime, tips, the Financial
  Accounts balance-sheet items, the consumption categories) read series files instead, with the same growth through
  2035 (to 7 in 10^16), so `tilt.grow_dollars` in item mode no longer calls `projection_growth.plan_factors` or
  CBO's count of returns. The 15 `current` rows left are per-unit rules (kind `per_unit`); a `current` row of kind
  `total` stops the run.
- **Past CBO's last year** the growth.yaml rules continue too (`targets.cbo_growth`, `growth.income_per_person_growth`,
  `projection_growth.gross_ss_between`, `projection_growth.long_run_growth`), so every year's configuration check
  passes; the Forbes list after 2026 grows per return by the items table's series over the growth of returns
  (`projection_growth.returns_growth`; `top_billionaires.growth_factors`); the AMT count target has no row after 2035;
  401(k) limits to 2075 by their rule (2.4% a year, rounded down to $500).
- **Open.** CBO's population has no married 16-year-olds in 2070-2072, so step 1 cannot meet that cell in those years
  (DECISIONS.md, item 1).

## Social Security and pensions: parts drawn, totals computed (branch `rebuild-pensions`, merged 3 November)

John's decision of 3 November; it took effect with the retrain of 3-4 November (scratch `explore/2026-11-03/bigrun/`). Results: scratch `explore/2026-11-03/pensions/RESULTS.md`. It replaces the post-draw patch of
`explore/2026-11-03/pairs_C/`, which John did not take.

- **Slots.** The network draws gross Social Security (`gross_ss`, as before), taxable pensions (`txbl_pens_dist`, as
  before) and a new slot, the non-taxable rest of pensions (`nontxbl_pens_dist`): on the PUF total pensions less
  taxable pensions, E01500 - E01700, never below zero. The slots `retirement_gross` (gross Social Security plus total
  pensions, E02400 + E01500, a leftover from when the SCF and CEX shared it) and `ss_taxable` (E02500) are gone.
- **Computed, never drawn** (`dollars.retirement_items`, called by `draw_dollars`, `tilt.recombine` after any growth,
  and again after the refund rule, which lowers AGI):
  - gross pensions `gross_pens_dist` = taxable pensions + the rest, exactly; the Tax-Simulator file's
    `gross_pens_dist` is this column;
  - taxable Social Security `ss_taxable`, on every record, by the section 86 worksheet as tax-simulator-gpu computes it
    (`common.taxable_social_security`): provisional income is half the benefits plus modified AGI (AGI without
    taxable benefits, plus tax-exempt interest, plus the student loan interest deduction); 50% above $25,000 ($32,000
    joint), 85% above $34,000 ($44,000 joint), at most 85% of the benefits; separate returns on the single amounts.
    It is added into `other_agi`, so the file's AGI holds it. Never targeted.
- **The PUF's rows.** `other_agi` (the remainder) is AGI less every item and less the PUF's own taxable Social
  Security, so a PUF record's AGI in the file is E00100 with E02500 replaced by the worksheet's amount (the table
  stage writes `data/puf_ss_check.csv`). The AGI group label's band-AGI uses the worksheet too (`qgroups.band_agi`).
- **The surveys.** The SCF, CEX and SIPP share gross Social Security only; the SIPP's pensions stay against taxable
  pensions, so the rest is absent on the SIPP, the SCF and the CEX. The CPS's pensions all go to taxable pensions
  (agreed 23 October), so the rest is zero on the CPS and its gross pensions equal its taxable pensions, as before.
  The SCF's own Social Security plus pensions (X5722), used only for the units' income test, is renamed
  `ss_pensions_scf` in `roster_scf.py` and `build_scf.R`.
- **Growth.** The rest grows by SOI Table 1.4's total less taxable pensions, per return with total pensions
  (`targets.SOI_ITEMS`, a line whose returns are not added); past SOI's last year by CBO's taxable pensions line
  (`projection_growth_mapping.csv`). Taxable Social Security has no growth line: the worksheet is applied to the grown
  items, with the unindexed thresholds.
- **Targets.** Taxable Social Security is no longer targeted: out of Table 1.3's items in every past year (catalog
  `soi_1_3_items`, `soi_1_3_items_2018`), and in the projection years it has no row in the items table (`projection_items.csv`), so it
  is neither grown nor targeted there; the rest of pensions has a row of its own, on CBO's pensions line like taxable
  pensions, in place of `retirement_gross`'s. The top
  step's rebuilt aggregate records no longer match the booklet's count of returns with taxable benefits (E02500 and
  E01500 are checks only).
- **The top of the file.** `top_hidden.py`: the two pension parts are scaled like any part (the bounds on taxable
  Social Security and taxable pensions are gone); each rebuilt record's taxable Social Security is the worksheet's, and
  the gap to the raw record's E02500 moves into the residual, so each group's AGI is still the raw record's.
  `top_billionaires.py`: IRA distributions are no longer put in the Forbes units' retirement income.
- **Elsewhere.** The non-microdata imputations' broad income adds gross pensions, not `retirement_gross` (which
  counted gross Social Security twice); `calculator.convert` takes `gross_pens_dist` from the two parts.

## The 2025 SCF wave and inheritances received this year (branch `rebuild-scf2025`, merged 3 November)

**The 2025 wave.** The Federal Reserve released the 2025 SCF in October 2026. Its files (the full public data set, the
summary extract, the replicate weights, the codebook) were downloaded to scratch `explore/2026-11-03/scf2025/raw/`
and are read from there (settings `paths.raw.scf_wave_dirs`) until they are copied to the shared raw-data folder.
Every raw item the pipeline reads has the same number and question as in 2022 (one label change: the group outside the
PEU's public assistance now names SSI). The wave is income year 2024 in the table, after the other waves (ids offset
by 9e8). Section 7's rule now gives 2022 the 2022 wave 54.65%, the 2025 wave 33.15% and the 2019 wave 12.20%; 2023 the
2025 wave 57.41%, the 2022 wave 34.82%, the 2019 wave 7.77%; 2024 on the 2025 wave 81.76% and the 2022 wave 18.24%;
2015-2021 are unchanged. After the last wave the SCF's items grow by Z.1 from 2025 rather than 2022.

**Inheritances received this year** (`inheritance`; John, 2 November). An SCF-only slot like the balance sheet: no
tax-data counterpart, no translation row and no fill-in; zeros and amounts are one distribution, as every slot. In
preprocessing (`roster_scf.inheritances`): the SCF asks the household about up to three inheritances, trusts or gifts
received, with the value and the year; gifts are left out. The public file rounds the year to the nearest 5 (years
that would round past the interview are set to the survey year), so each reported inheritance counts as received in the
wave's income year with the chance its year code carries (settings `inheritance.year_weights`): for 2013-2022 the
Estate-Tax-Distribution model's weights, calibrated to Gale and Sabelhaus (2024) (2022 wave: code 2022 with chance 1,
code 2020 with 0.28); for the 2025 wave Gale and Sabelhaus's own rule, 1.25 over the years the code covers (code 2025
covers 2023 to mid-2025, so 0.5). The chance is realised by a draw keyed by the household, implicate and the
inheritance's position, so the expected count and dollars are right and the five implicates draw independently. The
value when received is taken as income-year dollars. In `build_scf.R` a unit holding the whole PEU takes all of it, a
split couple half each; units outside the PEU and other units are not asked, so the item is missing there (band -1,
as the elective deferrals). Between waves it is grown by the SCF's net worth per tax unit, after the last wave by Z.1
household net worth per person (growth.yaml series `scf net worth per unit between waves`). Dependent filers get none
(generation's rule for every survey slot). Its random stream is the last in `dollars.ORDER`. The top is not adjusted:
the SCF has a handful of households a wave with more than $5 million. Weighted, the waves give 2.3, 2.2, 2.9, 3.0 and
3.8 million inheritances and $367, 372, 554, 595 and 1,088 billion (income years 2012, 2015, 2018, 2021, 2024).
Results of the retrain: scratch `explore/2026-11-03/scf2025/RESULTS.md`. In the projection years the item has a row in the
items table (`projection_items.csv`: series `current`, so its growth.yaml rule; not targeted). The networks of the
full run on the merged code (scratch `explore/2026-11-03/bigrun/`) are the first trained with this item and the
pensions slots together.

## How the latest runs come out (3-4 November 2026)

Every year on `rebuild-joint-model` after the evening merges of 3 November (2a42643): three networks retrained on the
merged table, every year 2015-2023 drawn again with a fresh top cache, 2024-2035 from the new 2023 (scratch
`explore/2026-11-03/bigrun/RESULTS.md`; run folders `run/<year>/`). The run also includes the Table 1.6 age-by-AGI
targets and Table 4.3's published cutoffs, merged onto the branch while it ran. Newton steps out of the budget of 150,
share of the effective sample kept, largest miss in standard errors, and the calculator's income tax after credits on
filers ($ billion, nominal dollars of the year).

| Year | Steps | Effective sample kept | Largest miss (se) | Tax after credits ($ billion) |
|---|---:|---:|---:|---:|
| 2015 | 36 | 74.6% | 2.79 | 1,443 |
| 2016 | 48 | 75.6% | 2.82 | 1,433 |
| 2017 | 46 | 72.5% | 2.81 | 1,588 |
| 2018 | 59 | 74.5% | 3.10 | 1,519 |
| 2019 | 65 | 65.9% | 3.78 | 1,561 |
| 2020 | 53 | 41.1% | 5.59 | 1,492 |
| 2021 | 89 | 41.3% | 6.01 | 1,975 |
| 2022 | 72 | 54.8% | 4.89 | 2,112 |
| 2023 | 61 | 54.2% | 4.96 | 2,122 |
| 2024 | 8 | 96.4% | 0.83 | 2,337 |
| 2025 | 12 | 96.5% | 0.86 | 2,421 |
| 2026 | 11 | 96.6% | 0.89 | 2,581 |
| 2027 | 8 | 96.6% | 0.93 | 2,688 |
| 2028 | 13 | 96.8% | 0.97 | 2,769 |
| 2029 | 11 | 96.7% | 1.01 | 2,931 |
| 2030 | 8 | 96.7% | 1.04 | 3,090 |
| 2031 | 7 | 96.7% | 1.07 | 3,220 |
| 2032 | 6 | 96.8% | 1.11 | 3,356 |
| 2033 | 5 | 96.7% | 1.14 | 3,495 |
| 2034 | 5 | 96.8% | 1.17 | 3,649 |
| 2035 | 5 | 96.9% | 1.20 | 3,808 |

Every year converged. The pension and Social Security pairs have no breaks in any year, and taxable Social Security,
now computed by the worksheet and untargeted, lands on SOI (2023: $528 billion against $527 billion). Against CBO,
income tax after credits is 0.981-0.989 of CBO's in 2026-2035.

Earlier runs' figures are in `HISTORY.md` and in this file's git history (the 2019 run of 28 October, up to commit
4c2c518). Known gaps are listed in `ROADMAP.md` and `NEXT_STEPS.md`.

## Autoresearch version A (4-5 November 2026; branch autoresearch-llm)

Each round compared run3's file with published SOI lines that were not targets, picked the ones that were far off,
and added a target or a rule for them. Dollars are nominal dollars of the year. "Standard errors" are the target
errors used by the reweighting. The log of every round is `LOG.md` in the scratch folder
`explore/2026-11-04/autoresearch/llm/`. Only what was kept is listed here.

**Round 1: estates and trusts, Form 4797, alimony, and the tax benefit rule on state refunds.**
- The file against SOI Table 1.4, 2019: estate and trust net income $51.9 billion against $36.2 billion; Form 4797 net
  gain $56.3 against $37.9 billion and net loss $52.3 against $18.5 billion; alimony received $20.5 billion against
  $10.8 billion (2023: $27.0 against $6.7 billion); taxable state refunds $14.1 billion on 6.59 million returns
  against $5.6 billion on 4.31 million.
- Fix: estate and trust income and loss and Form 4797 gain and loss are targets in the business block's six AGI
  groups; alimony received and paid are targets for all AGI. In `taxes.py`, an itemizer's taxable refund is held to
  the tax benefit of the deduction it repays (IRS Publication 525); the setting is `taxes.refund_tax_benefit`.
- Result: every new target met within 0.12 standard errors, at a cost of 0.2 points of effective sample kept. The
  refund rule alone left 2019 refunds at $2.3 billion, too low, which round 2 corrects.

**Round 2: state refunds, heads of household and separate filers by age, education credits.**
- Heads of household under 26 in 2019: 4.06 million in the file against 3.46 million in SOI's published cells; only
  joint against not joint had been targeted by age. Refundable American opportunity credit $7.8 billion on 9.18
  million returns against $6.1 billion on 6.94 million.
- Fix: state refunds as Table 1.4 targets in the six business AGI groups; a new target kind, "age by status", for
  Table 1.6 heads of household and married filing separately by the six age ranges (importance 1); Table 3.3
  nonrefundable education credits and the refundable American opportunity credit by AGI (calculator columns
  `calc_ed_nonref`, `calc_ed_ref`).
- Result: all met within 0.3 standard errors except refunds at AGI $1 million and over (round 3). 2019 refunds
  $5.16 billion on 4.31 million returns. Effective sample kept 65.7% to 64.7% (2019) and 53.9% to 53.0% (2023).

**Round 3: self-employment tax, dependent filers' investment income, itemizers' gifts.**
- Self-employment tax, 2019: $59.8 billion against SOI's $67.3 billion (2023: $72.2 against $81.1 billion). The
  calculator's split of a couple's self-employment income between spouses puts it on the higher-wage spouse in 41% of
  couples, against 21% in the CPS, so more of it lies above the Social Security wage base. Dependent filers' taxable
  interest $2.1 billion against $0.38 billion. Itemizers' cash gifts $156 billion against $130 billion, and gifts
  other than cash $98 billion against $75 billion (Table 2.1).
- Fix: targets for Table 3.3 self-employment tax in the business AGI groups (`calc_liab_seca`); Table 1.7 dependent
  filers' taxable interest and ordinary dividends, all AGI; Table 2.1 cash and other gifts on itemizing returns in the
  itemized AGI groups (`calc_char_cash_itemizers`, `calc_char_noncash_itemizers`; rows added to
  `table21_2015_2023.csv` by `resources/build_table21_charity.py`). A refund the tax benefit rule would remove is
  kept with probability 0.15, drawn from the record id (`taxes.refund_reported_anyway`), because SOI shows refunds on
  about 5% of returns at AGI $2-5 million.
- Result: every new target met within 0.5 standard errors. Refunds at $1 million and over are now met (2019 $0.66
  billion; the cell was 20.8 standard errors short). Largest miss 3.80 (2019) and 4.99 (2023) standard errors.

**Round 4: the QBI deduction's dollars by AGI, 2018-2023.**
- Run3's 2019 file had $9.5 billion at AGI $10 million and over against SOI's $15.3 billion, and $5.7 against $8.5
  billion at $5-10 million. The counts are not targeted because SOI counts returns whose deduction comes from REIT
  dividends and publicly traded partnerships, which the file lacks.
- Fix: the deduction's dollars, without counts, are targets in the 11 calculator AGI groups.
- Result: every cell met within 0.12 standard errors (2019 at $10 million and over: $11.6 to $15.3 billion; 2023:
  $20.7 to $32.1 billion). Effective sample unchanged (64.7%, 52.9%). Newton steps rose to 84 (2019) and 119 (2023)
  against the budget of 150.

**Tried and dropped.** Stating the SCF holder counts as shares of each cell's units: the file already holds the items
at higher rates than the SCF, and the targets missed by up to 82 standard errors. Switching off the calculator's
pass-through entity tax election before 2024: dropped on John's instruction, because the loop changes data, not tax
law. The file's AGI stays above the calculator's by the election's amount ($27.1 billion in 2019), a known difference.

**Rounds 5-6: more SOI lines, and two rules that make the file's adjustments the calculator's.**
- Targets: qualified dividends (Table 1.4 broad groups; Table 4.3 top 0.1% and 1%; before, 1.5-2 times SOI's at the
  top); Table 4.3's charitable, interest and QBI deductions in the top 0.1% and 1%; the EITC by qualifying children
  (Table 2.5, source `soi 2.5`, 2015-2020; the credit without children had been 0.68 of SOI's); Table 3.3's child care
  and saver's credits; capital gain distributions on Form 1040; more of Table 1.7 (dependent filers' tax-exempt
  interest, distributions, capital losses, Schedule C).
- Rules in `taxes.py`: `taxes.repealed_adjustments` zeroes the production activities deduction from 2018 and the
  tuition and fees deduction from 2021; `taxes.se_tax_ded_from_calculator` takes the deductible half of
  self-employment tax from the calculator. Together they make the file's adjustments equal the calculator's on every
  record. The top 0.001% and 0.01% cells of Table 4.3 for the new items and its taxes paid line were left out (the
  solve did not converge with them).

**Rounds 7-8: Table 1.4's other lines and Table 1.2 by AGI.** Statutory adjustments, tax-exempt interest,
unemployment compensation, taxable IRA distributions and taxable pensions in the business AGI groups; Table 1.2's
single, separate and head of household returns in each standard AGI group (kind `status by AGI`,
`target_list.published_12_agi`). Single returns at $1-10 million had been 0.63-0.70 of SOI's.

Round 12 added gross pensions and annuities (Table 1.4 total, taxable plus the non-taxable rest) to the same entry.

**Round 9: non-filers' dividends** against the non-filers of the pipeline's tax data (value source
`tax_data_nonfilers`, kind `non-filer items`, errors half the value and a quarter of the holders). The 2019 file had
$30.0 billion against $7.1 billion; after, $9.3 billion.

**Round 10: the Schedule D net capital gain on the standard AGI grid** (it had been on the broad grid). Also
`taxes.refund_tax_benefit_from` (2019) and a fix in `targets.published_cells` for a blank cell held in the next
combined cell (Table 3.3, 2015). **Round 11:** no state refund targets in 2018 (its refunds repay 2017 taxes).

**Round 12: gross pensions and annuities** as a target in the business AGI groups.

**Round 13: the QBI items drawn after every other SCF item** (settings `generation.qbi_items_last`,
`generate.scf_order`). The QBI rule gives a Schedule C, partnership or S corporation industry group to every record
with that kind of income, while in the SCF only business owners have one; drawn first, the industry group made the
record an owner. Drawn last, owns_business and the balance sheet follow the record's income as the SCF taught: 2019
ownership 16.8% to 11.0% (SCF 11.6%), bottom income quarter 13.6% to 6.4% (SCF 3.4%); 2023 18.0% to 11.6% (SCF
12.6%). A new draw of 2019 and 2023 (GPU), no retrain.

Also tried and dropped: an SCF target on the share of units owning a private business (2019 effective sample kept
61.8% to 58.6% for an item no tax calculation reads); round 13 fixed the cause in the draw instead.

The log of every round, with the numbers before and after, is `LOG.md`.
