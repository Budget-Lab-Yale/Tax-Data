# What the pipeline does now

The current run ("latest") produces a 2019 file of tax units: every PUF
variable, plus the household balance sheet from the SCF (20 items, with net
worth computed from them), spending in 8 categories from the CEX, and
from the CPS ASEC each person's age and sex, dependents' ages, the split of
a couple's wages and self-employment income between the spouses, weeks and
hours worked, and childcare, weighted to 2019 targets.
- **Filers and non-filers.** Filers come from the PUF label and non-filers
  from the CPS label. The CPS's filing chances come from a calibrated rule
  (v4) with no outside model in it.
- **Tax units.** The CPS, SCF and CEX all build their tax units with one
  module of CBO's rules (`TAX_UNITS.md`).
- **Generation and targets.** Filers are drawn stratum by stratum (AGI class
  × business), pooled from two trained models; the 2019 targets come from
  `pipeline/targets_config/`, each with a standard error, and are met within
  ranges (`TARGETS.md`).
- **Since when.** This is latest since 25 October. The code is in
  `pipeline/`; every setting is in `pipeline/settings.json`.

## Data

**Sources:**

| Source | Rows | What it covers |
|---|---:|---|
| 2015 PUF | 207,692 | filers |
| SCF waves 2013, 2016, 2019, 2022 | 33,058 / 34,377 / 31,830 / 25,600 | tax years 2012–2021, five implicates each; tax units by the shared rules (below) |
| CEX interview survey 2015, 2017, 2019, 2021, 2023, 2024 | 34,901 / 32,924 / 28,431 / 27,550 / 25,470 / 26,630 | every tax unit, one row per quarterly interview (`build_cex.R`, below) |
| CPS ASEC, tax years 2015, 2017, 2019, 2021, 2023 | 90,351 / 88,954 / 79,054 / 77,618 / 74,502 | every tax unit, filers and non-filers (`build_cps.R`, below) |

**Variables:**
- marital status (joint or not), dependents (0–3), and the filing flag;
- the primary filer's age range (below);
- wages, taxable interest, ordinary dividends, and capital gains in four
  pieces (short term, long term, distributions, other);
- more AGI items: other gains, taxable IRA distributions, taxable
  pensions, taxable Social Security, unemployment, alimony, state refunds,
  adjustments, and the rest of AGI ("other income");
- three items outside AGI: gross Social Security plus pensions,
  tax-exempt interest, and gross Social Security alone (shared with the
  CEX);
- two itemized deductions (PUF only, itemizers only): mortgage interest
  paid and real estate tax. They are kept in 2015 dollars in the 2019 file
  (SOI Table 1.4 has no row to grow them by);
- business income in its 18 PUF parts: Schedule C; partnership and
  S corporation income, losses and section 179, passive and nonpassive;
  rents, royalties, estates and trusts; farm rental; farm. Business income
  is computed from the parts, and so are two subtotals of it: Schedules C
  and F, and Schedule E;
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
- spending in production's 8 categories (CEX only): clothing; motor
  vehicles; other durables; other nondurables; food at home (with
  alcohol); gasoline; housing and utilities; other services and health.
  Total consumption is computed from them.
- the exact age of the primary filer and of the spouse (SCF, CEX and CPS;
  not on the PUF, which has only the age range): below, "Exact ages";
- person items (CPS only): the exact ages of up to three dependents
  (youngest first); the sex of the primary
  filer and the spouse; the spouse's share of the couple's wages and of its
  Schedules C and F income; each adult's weeks worked and usual weekly
  hours; childcare expenses. The file's wages1 and wages2 (and se1, se2)
  are the total split by the drawn share, so they add up to the total.

All four sources share married, dependents, the primary filer's age range
and wages. The SCF, the CEX and the CPS share the exact ages of the
primary filer and the spouse. The PUF, the SCF and the CEX also share Schedules C and F
income. The PUF and the SCF also share interest, dividends, capital gains,
Schedule E income, the partnership/S corporation/LLC flag, gross Social
Security plus pensions and tax-exempt interest; the PUF and the CEX also
share gross Social Security; the PUF and the CPS also share the filing flag,
dividends and gross Social Security. The rest belong to one source. A slot a source lacks is absent on its records
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
units that claim its children.

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
targets the tilt reads for non-filers by age (settings
tilt.nonfiler_targets_dir).
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
  - weeks worked, usual weekly hours and childcare are amounts, cut within
    each year; childcare's dollars are grown from the generation year to
    the target year by SOI's wages per return (a placeholder).
- **Rules at generation** (`generate.py`, cps_structure): the spouse's
  items are none or zero unless married, and a married spouse has an age
  and a sex; the k-th dependent has an age exactly when there are at least
  k dependents; a
  spouse's share is zero when the couple has none of the item. A dependent
  filer holds no CPS item (ages none, shares zero, so its wages are all its
  own). Where a share is absent on the training rows (no couple, or none of
  the item, or a dependent filer), generation leaves it absent too until
  the CPS phase is over, then sets it to zero: the network reads a present
  slot, even a hidden one, as "a couple with some of this". (The first
  candidate run made it present: unmarried primary filers came out 91% men
  against the CPS's 53%.)
- **Checks** on the candidate file: wages1 + wages2 equals wages, and se1 +
  se2 equals Schedules C and F income, to within $0.000000001 on every
  record (floating point); every rule above holds on every record.

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

**Exact ages** (candidate, 23 October; `build_table.py`, EXACT_AGES). The
primary filer's and the spouse's exact ages are on the SCF, CEX and CPS
records, absent on the PUF's (filers and non-filers alike), so the balance
sheet and consumption are learned given exact age.
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
  CEX and CPS phases, under the CPS label (below, Generation).

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
  items (summed), definition fixes, the dollar line, whether function 2
  applies, whether the variable is part of the type function 2 conditions
  on, and the reason. A variable not in the table is not shared.
  - Defaults: dollars, no fill-in.
  - Departures today:
    - SCF wages: a dollar line at $100,000;
    - the SCF's two business subtotals and the CEX's self-employment
      income: a line of zero;
    - CPS wages and dividends: a line of zero;
    - SCF capital losses read as no more than $3,000;
    - function 2 on SCF interest, dividends and capital gains.
- **Function 1, amounts.** An amount within the dollar line (2015 dollars,
  grown per return to the survey's year) keeps its dollars. Above the line,
  a unit gets the tax data's amount with the same number of units above it.
  Below minus the line, it gets the tax data's loss with the same number
  below it. No line means dollars throughout; a line of zero means rank
  throughout. The result is cut on the PUF's cut points grown to that year.
  The old wage swap also multiplied by a constant that kept the map
  continuous at $100,000; function 1 has no such constant.
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
- **The CPS, why the departures and exclusions**
  (`rebuild_cps/explore/cps_translation/TRANSLATION_CPS.md` in scratch):
  - wages: the CPS has more units than the tax data above every line from
    $25,000 (2019: 29.0 against 21.9 million above $100,000; 0.30 against
    0.19 million above $1 million) and fewer below $10,000; the share with
    none is within 3 points. Its wages are before pre-tax deductions, and
    its top-coding does not leave it short at the top;
  - dividends: the share with none is within 3 points, but fewer units
    above every line (2019: 0.27 against 0.42 million above $100,000);
  - not shared: interest (the CPS has twice the tax data's holders and 2-6
    times its dollars, more after its 2019 redesign; no setting removes
    holders); Schedules C and F, unemployment and taxable pensions (the
    CPS's zeros hide holders: 7% of units with self-employment income
    against 18%, almost no losses; half the unemployment recipients outside
    2021; 30-40% fewer pension holders), which wait for function 2 on the
    CPS; Schedule E (it includes partnership and S corporation income, the
    CPS's rent item does not).
- **The SCF, why the departures.** The SCF reports more high earners than
  tax data at every line (2015: 0.47 million units with $1M+ of wages
  against the PUF's 0.15 million), as the literature also finds (Gale,
  Joshi, Pulliam and Sabelhaus 2022: 2.2 times as many units with $1M+ of
  AGI). The SCF's own pattern of zeros stays visible, through a source
  label the model always sees. The balance sheet is not shared: it is
  banded within each wave.
- **Function 2: two slots, filled during training** (`fillin.py`).
 Many SCF households that report
  zero interest, dividends or capital gains do hold some. Each of these
  items has two slots on SCF rows: what the household reported (never
  changed) and the PUF-concept amount (reporters copy their answer;
  zero-reporters are filled in). The PUF has only the PUF-concept slot.
  - From pass 3, at the start of each pass, each zero-reporter gets its
    chance of a hidden holding in each size range from the reporting-rate
    formula: its own chance of reporting that range (from the rest of its
    record, every filled-in slot hidden), times (PUF share ÷ SCF share − 1)
    for its type (married, dependents, wage band), divided by its chance
    of reporting zero; capped at 100%. Each batch redraws the fill-ins from
    those chances.
  - The formula reads only the reported slots and the PUF, never a fill-in.
  - No cells, top lines or loss floors: a range where the SCF has as many
    holders as the PUF, or more, gets none.
  - It assumes a holder's chance of reporting depends only on its type and
    true amount, not on net worth or anything else on the record.
  - The PUF side of the formula (the PUF share for each type) leaves out
    dependent filers, which are not tax units in any comparison with a
    survey: it mixes the model's answers over the six adult age ranges, by
    the model's chance of each range given the type.
  - In the 2% held-back slice no fill-in is drawn, so there a zero-reporter's
    PUF-concept amount is unknown: not shown and not scored.
  - Net worth is drawn given the PUF-concept amounts; the reported slots
    stay hidden at generation.

## The model

- **Network.** A transformer: 4 layers, width 128, about 1.4 million
  parameters (0.85 million of them in the output layers, one per
  variable), no dropout. A network with 4.9 million (6 layers, width 256)
  was about 3 times slower a pass and no closer to the CEX (22 October).
- **Packing.** Each batch holds one
  source's records and carries only that source's slots (a PUF record
  uses 50 of the 86, an SCF record 42, a CEX record 16, counting the source and year). The answers
  are the same to rounding; a pass takes half the time and GPU memory.
  Each batch's error is scaled so every record counts as much as in a
  mixed batch (`explore/2026-10-22/perf/packing/PACKING.md`).
- **Output layers** are computed in one batched multiplication, only the
  batch's own source's (the same answers, 10% faster a pass).
- **Precision** (settings "training", "precision"): standard 32-bit.
  bfloat16 for the training steps was 37% faster a pass with results
  within the difference between two seeds; not switched on
  (`explore/2026-10-22/perf/precision/PRECISION.md`).
- **What it learns.** It is trained to fill in any hidden subset of a
  record's variables from the rest. So one network answers every question
  of the form "this variable, given those".
- **Weights.** Survey weights are used as they are, rescaled within each
  source.
- **Training.** 80 passes, all records but a 2% held-back slice. Peak
  learning rate 0.003: in a sweep of 0.001 to 0.01 (two or three seeds
  each, 21 October), it brought the top income group's net worth before
  the tilt close to the SCF's, at a slightly worse fit on the average
  held-back record.
- **Why no dropout.** Dropout made rare items come out 10–20% too low
  (`HISTORY.md`).

## Generation

**One record is drawn slot by slot:**
1. the PUF variables, in a random order;
2. line-13 capital gains, business income and its two subtotals, computed
   from their parts; then the partnership/S corporation/LLC flag;
3. the exact ages of the primary filer and the spouse, in a random order,
   as the CPS would see the unit (CPS label and generation year 2019): the
   CPS covers every tax unit, filers and non-filers, in every year, and its
   view holds the filing flag. The primary filer's age lies inside its age
   range; the spouse's exists exactly when married;
4. the SCF-only items, in a random order (the mortgage share after the
   home), as the SCF would see the household (SCF label, PUF-only variables
   left out, the exact ages seen). Net worth is computed from the balance
   sheet in dollars;
5. the 8 consumption categories, in a random order, as the CEX would
   see the unit (CEX label and generation year 2019, everything the CEX
   lacks left out, the exact ages seen);
6. the CPS's own person items and childcare last, in a random order, as
   the CPS would see the unit (CPS label and generation year 2019), under
   the rules above (The CPS).

**Dependent filers** (the PUF's three dependent-filer age ranges) hold no
SCF, CEX or CPS item: every balance-sheet item, unrealized gain, business
ownership, consumption category, exact age and CPS person item is zero or none on
their records. The surveys
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

**The stack: strata** (settings `generation.design` "strata"; TARGETS.md S4; since 25 October). The
training table carries three labels computed from each record's own dollars (`build_table.py labels`, written to
`data/strata/`): on PUF records an AGI class by weighted rank among 2015 returns (zero or negative; below p90,
$133,700; p90-99, to $464,200; p99-99.9, to $2,127,000; p99.9-99.99, to $11,050,000; p99.99 and over) and a business
flag (Schedule C, partnership or S corporation, or farm income nonzero); on SCF records a net worth class (below
p90, p90-99, p99-99.9, p99.9 and over). The network learns them like any item (`model/strata_seed*`).
- **Filers:** a fixed number of records in each of the 12 AGI class x business strata (`generation.strata.records`,
  400,000 in all, from about 0.15% of returns below p90 to 30-60% at p99.99 and over). Each record starts with its
  stratum preset; its weight is the model's own chance of the stratum over the stratum's share of the sample.
- **Net worth:** when the SCF items are drawn, the net worth class comes first, its chances boosted 1, 2, 5 and 10
  times by class (`nw_boost`), corrected by p / q.
- **Labels are checked, not trusted.** AGI and net worth are computed from the drawn parts; a record whose
  computed class differs from its label is kept and counted (`strata_check.csv`; `drop_mismatches` false). The
  network often contradicts its own label at the top (80% of p99.99-and-over records compute lower), and dropping
  them biased the file (returns at p99.9-99.99 38% low against an unstratified draw).
- `design` "top_parts" restores the earlier stack (four parts oversampling the top 1% of wages, long-term gains,
  S corporation nonpassive income and business equity, with oversampling by rough AGI); "unstratified" draws with
  nothing preset.

**Two models pooled** (settings `generation.models`; TARGETS.md section 8). Every batch is drawn half from each
trained model (now `strata_seed20261016`, `strata_seed20261017`), each with its own generation seeds (its
`seed_shift`), each record saying which drew it (column `model`), ids unique across models. The consistency offset
is found per model.

## Post-training: the 2019 file

**Dollars.** Each band's real 2015 values, grown to 2019 by SOI's growth
in dollars per return holding each item. The balance sheet: each band's real
values from the 2016 SCF wave, carried to the 2019 wave's dollars by the two
waves' asset price factors, 1.2334 and 1.1592 (2022 dollars per wave
dollar). Consumption: each
band's real CEX 2019 values, with no growth. The CPS's items: each band's
real CPS 2019 values (childcare grown by SOI's wages per return, which is
no growth when the generation year is the target year). Every dollar column
of the file is in nominal 2019 dollars. Nothing from the CPS is targeted
(`tilt/<run>/cps_report.md` compares the file with the CPS 2019).

**The year's configuration** (`pipeline/targets_config/`; TARGETS.md section 12 and S7): a catalog of
definitions, defaults, historical and projection files, and one file per year. `target_list.py` expands it into
one row per target (`tilt/<run>/targets.csv`), with each target's standard error from `errors.py` (SOI's published
coefficients of variation for Tables 1.4, 2.1 and 3.3; borrowed ones for Tables 1.2, 1.6 and 1.7; CEX and SCF
replicate weights; a judgment number with a reason otherwise; TARGETS.md section 9).

**Start** (the year file's `start`): `generate` (the generated stack) or `from_year: s` (year s's tilted file, its
dollars grown item by item to year t). Weights are scaled to SOI's return count and the non-filer count, to joint
and non-joint returns, then (`tilt.prescale` "demographic") to Table 1.6's returns by age x joint and to the
non-filer people by age (section 4).

**The tilt** (`solver.py`; TARGETS.md S6): the smallest reweighting, by Kullback-Leibler divergence, that puts every
target within its value ± `tilt.range_in_se` (1) standard errors; identities exact. If the ranges cannot all be
met, a miss beyond a range costs (miss ÷ standard error)². A range on an average is measured on the average itself,
with the cell's weight after the tilt. Cells resting on too few effective records, or on a record moved too far,
are flagged as warnings or stops (section 5). The 2019 targets (474):

| Targets | Number |
|---|---:|
| IRS (SOI Table 1.4): returns and the number with and amount of wages, interest, dividends and capital gains, in AGI brackets with the five top ones ($1M-1.5M, $1.5M-2M, $2M-5M, $5M-10M, $10M+) separate; joint returns (Table 1.2) | 172 |
| Age (Table 1.6): returns by primary age 26-34 to 65+, joint and non-joint | 10 |
| Dependent filers (Table 1.7): returns by AGI | 9 |
| Business: net income and net loss, returns and dollars; Schedule C and partnership/S corp in 10 AGI groups (the five top ones separate), rent and royalty, farm and farm rental in 6 (cells SOI combined are merged) | 148 |
| Non-filer count (identity) and non-filer people by own age band and children under 17 | 9 |
| SCF 2019 wealth: shares of units and net worth above the 99.9th, 99th, 90th and 50th percentile lines and $1 million; the four asset types (stocks, business equity, real estate, retirement accounts) above the 99.9th, 99th and 90th lines | 22 |
| CEX 2019 consumption: 8 categories' averages per unit, over all units and in 12 age x married cells | 104 |

Counts by kind as in `tilt/strata_full2019/targets.csv`.
`hold_distribution` (optional, S7) adds each item's share in AGI rank groups of the start file as targets.
`report_tilt.py` writes the run report (section 14).

The combined 2019 run of 25 October (`tilt/strata_full2019`, strata models, mismatched labels kept): all 474
targets inside their ranges; effective sample kept 99.9% from the prescaled start; 61 warnings and 33 stops, most
at net-loss and interest amounts at the top and at no AGI, where one record holds most of a cell's dollars.

## How the latest run comes out

The combined 2019 run of 25 October (`tilt/strata_full2019`; the stack is `gen/strata_pooled_nodrop`: the two
strata models, 400,000 filer records and 54,379 non-filer records, mismatched labels kept). Its `summary.md` and
`report.md` have the full tables. Counts in millions, nominal 2019 dollars.

| | File | Reference |
|---|---:|---:|
| targets inside their ranges | 474 of 474 (323 at an edge) | |
| effective sample kept, from the prescaled start | 99.9% | |
| effective sample kept, from the start before the prescaling | 99.2% | |
| weight factor, 1st and 99th percentiles | 0.47, 1.59 | |
| thin cells: warnings, stops | 61, 33 | |
| non-filer units | 21.45 | CPS (filing rule v4): 21.45 |
| net worth, total | $98.3 trillion | SCF 2019: $96.6 trillion |
| median net worth, all 179.0 million units | $47,900 | SCF 2019, 162.8 million units: $63,570 |
| top 1% share of net worth | 41.6% | SCF 2019: 40.2% |
| mean net worth, wages $200k+ | $4.28 million | SCF 2019: $4.38 million |

- **The prescaling.** Before it, filer returns with a primary filer 65 and over were 16.8% (joint) and 10.5%
  (non-joint) below SOI Table 1.6, and non-filer heads and spouses aged 18-25 39.6% above the CPS; it moved the
  start by 0.002 nats in each batch.
- **Thin cells.** Most stops are dollar amounts at the top (net losses of partnerships, S corporations and
  Schedule C at $5 million and over, taxable interest at $10 million and over) and at no AGI, where a few records
  hold most of a cell's dollars.
- **Not targeted.** Of 1,381 published SOI cells the file is not targeted on, the median miss is 3.4 standard
  errors in Table 1.4, 2.4 in Table 1.6 and 2.2 in Table 1.7. The largest are Schedule D net losses above
  $1.5 million of AGI (the file has several times SOI's) and taxable Social Security at no AGI.
- **Net worth.** The file's units include its dependent filers, who hold no wealth; the SCF's units do not, so
  the two medians are not on the same base.

Earlier runs' figures are in `HISTORY.md` and in this file's git history (up to commit bc72cc7).

Known gaps are listed at the end of `ROADMAP.md`.
