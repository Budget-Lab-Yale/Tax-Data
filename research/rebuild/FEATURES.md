# What the pipeline does now

The current run ("latest") produces a 2019 file of tax units: every PUF
variable, plus the household balance sheet from the SCF (20 items, with net
worth computed from them), spending in 8 categories from the CEX, and
from the CPS ASEC each person's age and sex, dependents' ages, the split of
a couple's wages and self-employment income between the spouses, weeks and
hours worked, and childcare, weighted to 2019 targets. (The CPS was added on
the night of 22 October and is in latest since 23 October, with exact ages
shared by the SCF, CEX and CPS. Its tax units and the items that depend on
them, dependents' ages above all, are provisional: `TAX_UNITS.md`,
`AGES.md` section 0.) The code is in `pipeline/`; every setting is in
`pipeline/settings.json`.

## Data

**Sources:**

| Source | Rows | What it covers |
|---|---:|---|
| 2015 PUF | 207,692 | filers |
| 2017 CPS non-filer pool | 166,938 | non-filers on the PUF side, scaled to 2015 as the tax data is |
| SCF waves 2013, 2016, 2019, 2022 | 33,058 / 34,377 / 31,830 / 25,600 | tax years 2012–2021, split into tax units by the production builder, five implicates each |
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
split into tax units by production's builder (`src/nonfilers/
asec_tax_units.R`, the non-filer pool's rules), every unit, filers and
non-filers. Dependent filers are folded into the unit that claims them. The
filing flag is drawn from the pool's filing model (Mok's probits below the
filing threshold, the Pub 5785 hazard above it, the pool's calibration).
The calibration is part of the CPS step: for a year without the pool's
calibration file, build_cps.R runs the pool's own steps 01-04 for that year
and stops, naming the missing inputs, when it cannot. 2015 was calibrated
this way on 23 October (filers 84.2% of units, against 78.0% before; 82.2%
in the tax data). For it, the pool's 04_calibrate.R now treats an anchor
file whose dorm column is empty in every state (2014-2016) as no file and
skips that informational check. The latest run was trained before this, on
the uncalibrated 2015 CPS.
Childcare is the SPM unit's child care expenses, given to the
units that claim its children.
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
into tax units by BLS's own codes in the member file, as production's
`src/cex.R` does, with its repairs. Spending is each interview's three
months times 4. A household's spending is divided among its tax units by
head count (production's rule). A dependent filer is folded into the unit
that claims it: its head count goes to that unit's share and its own row
is dropped; its own income goes nowhere (it belongs on its own return).
Checked against BLS's tax-unit file (NTAXI): the same units in every year.

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
  - that year's non-filer tax units from the CPS pool appended.
  - Placeholders. The pool covers 2017–2023: any other year takes the
    nearest pool year, its weights scaled by SOI's total returns and its
    dollars grown per return. Past 2023, the last SOI year, the last year's
    growth is carried forward.
  - The PUF side of the training table is the tax data for 2015 plus the
    dependent filers. Its non-filers are the 2017 pool, scaled this way.
    Until 23 October they were put in 2015 dollars with the CPI.
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

**The stack.** 400,000 records in four parts. Each part oversamples the
top 1% of one variable (wages, long-term gains, S corporation nonpassive
income, business equity). Weights undo the oversampling exactly.

## Post-training: the 2019 file

**Dollars.** Each band's real 2015 values, grown to 2019 by SOI's growth
in dollars per return holding each item. The balance sheet: each band's real
values from the 2016 SCF wave, carried to the 2019 wave's dollars by the two
waves' asset price factors, 1.2334 and 1.1592 (2022 dollars per wave
dollar). Before 23 October the tilt compared these 2016-dollar balance
sheets with targets put in 2015 dollars (factor 1.2493), so net worth before
the tilt was overstated by 1.3% and the tilt pushed it down that much more;
the 2019-dollar output was converted back with the same factor and lined up
with the SCF. Consumption: each
band's real CEX 2019 values, with no growth. The CPS's items: each band's
real CPS 2019 values (childcare grown by SOI's wages per return, which is
no growth when the generation year is the target year). Every dollar column
of the file is in nominal 2019 dollars. Nothing from the CPS is targeted
(`tilt/<run>/cps_report.md` compares the file with the CPS 2019).

**Start.**
- Weights are scaled to SOI's 2019 return count and the non-filer count.
- Filers are then scaled within joint and non-joint returns to SOI's 2019
  counts.

**The tilt.** The smallest reweighting, by Kullback–Leibler divergence,
that meets:

| Targets | Number | How they are met |
|---|---:|---|
| IRS (SOI 2019 Table 1.4): returns, and the number with and amount of wages, interest, dividends and capital gains, in 15 AGI brackets; plus joint returns | 136 | exactly |
| Non-filers | 1 | exactly |
| IRS business: five families (estates and trusts left out until the targeting overhaul, 22 October), net income and net loss, returns and dollars, in 6 AGI groups | 116 | exactly, or within 2% if that fails |
| SCF 2019 wealth: the share of units above the 99th, 90th and 50th percentiles and above $1 million, among the file's units other than dependent filers; net worth above each percentile and in total | 8 | within 2% |
| CEX 2019 consumption: each category's average per tax unit, over all units and in each of the 6 adult age ranges of the primary filer (dependent filers left out on both sides) | 56 | within 2% of the average |

Consumption targets are averages, not totals, so they do not depend on how
many units each source counts (the file 177 million units other than
dependent filers, the CEX 153 million). The wealth count targets are written
the same way, as shares, since 22 October; `fit.csv` reports each share and
average itself.
An average is a linear condition on the weights; it is written so the
tilt's scaling by target size works (`tilt.py`, consumption_targets).
A first run targeted totals: the file then had 17% more units over which
to spread the same dollars, and the tilt kept only 72% of the effective
sample.

## How the latest run comes out

**Since 23 October (afternoon), "latest" has the CPS and shared exact ages**
(`HISTORY.md`, 22–23 October). Two seeds, made in
`/nfs/roberts/scratch/pi_nrs36/jar335/rebuild_cps_ages/` (runs ages_s1,
ages_s2) and copied into the main output folders; the previous latest is in
`previous_2026-10-23_translation/`. Every target is met (317; the 116
business targets exactly). Nominal 2019 dollars; the two seeds, then the
previous latest's two:

| | Latest | Previous latest | SCF or CEX 2019 |
|---|---:|---:|---:|
| effective sample kept | 89.0%, 88.6% | 89.1%, 89.5% | |
| net worth before the tilt | $100.9T, $104.5T | $96.3T, $107.6T | $96.1T |
| median net worth, all units | $70,200, $71,600 | $70,100, $70,500 | $91,050 |
| mean net worth, wages $200k+ | $3.90M, $4.08M | $4.26M, $4.07M | $4.36M |
| consumption, wages $1-25k | $23.3k, $23.5k | $22.7k, $23.3k | $27.6k |
| consumption, wages $100-200k | $76.7k, $76.4k | $76.6k, $76.0k | $70.0k |
| consumption, wages $200k+ | $130.9k, $135.6k | $137.9k, $136.2k | $113.3k |

What the exact ages changed, 65 and over (mean IRAs and DC accounts): $151k
and $154k at 65–69, $82k and $98k at 80 and over, against the SCF's $171k
and $100k; without exact ages the fall from 65–69 to 80+ was about half the
SCF's. DB balances at 80+ got worse ($8k and $10k against the SCF's $24k;
$17k and $18k before). Units aged 80+ are too many (9.7 million against the
SCF's 6.7 million), as before. Known problems in the CPS-only items: the
dependents' ages are too young (in 2015, on the real PUF returns, 79.5%
under 17 against the IRS's 76.2%, and the IRS's range picked 47% of the
time for the youngest dependent; `AGES.md`, Step 3), and the file has about
86 million children under 18 against Census's 73 million. Comparisons with
production and outside tables:
`/nfs/roberts/scratch/pi_nrs36/jar335/rebuild_cps/explore/comparisons/COMPARISONS.md`.

**Before that, from 23 October (morning), "latest" used the standardized translation**
(translation section above; `HISTORY.md`, 23 October). Two seeds, made in
`explore/2026-10-23/translation/` and copied into the main output folders.
The previous latest is in `previous_2026-10-22_cleanup/`. Every target is
met (317; the 116 business targets exactly). Nominal 2019 dollars; the two
seeds, then the previous latest's two:

| | Latest | Previous latest | SCF or CEX 2019 |
|---|---:|---:|---:|
| effective sample kept | 89.1%, 89.5% | 90.2%, 89.6% | |
| net worth before the tilt | $96.3T, $107.6T | $104.9T, $102.3T | $96.1T |
| median net worth, all units | $70,100, $70,500 | $70,400, $71,100 | $91,050 |
| mean net worth, wages $200k+ | $4.26M, $4.07M | $4.09M, $3.96M | $4.36M |
| consumption, wages $1-25k | $22.7k, $23.3k | $23.0k, $23.5k | $27.6k |
| consumption, wages $100-200k | $76.6k, $76.0k | $77.5k, $76.4k | $70.0k |
| consumption, wages $200k+ | $137.9k, $136.2k | $130.5k, $129.8k | $113.3k |

The median is below the SCF's because the file has 187 million units
against the SCF's 143 million. The extra units (young single people, adult
children, non-filers) hold little wealth. The wealth targets set totals and
the share of units above each line, not the median. Translation report:
`model/<run>/translation_report.md`.


**Before that, from 22 October (late), "latest" was the reviewed and cleaned-up code**
(`HISTORY.md`, 22 October evening and 23 October): consistency rules not
applied to dependent filers, the fill-in's PUF side without them, wealth
counts as shares, estates and trusts out of the business targets, the
held-back slice not scoring unknown amounts, and one currency (the 1.3%
correction). Two seeds: models from `explore/2026-10-22/review_fix/out2/`
(trained by the commit before the cleanup, which trains identically),
generation and tilt from `out3/` with the cleaned-up code, copied into the
main output folders. The previous latest is in `previous_2026-10-22_cex/`.
Every target met (317; the 116 business targets exactly); nominal 2019
dollars; the two seeds, then the previous latest's two:

| | Latest | Previous latest | SCF or CEX 2019 |
|---|---:|---:|---:|
| effective sample kept | 90.2%, 89.6% | 89.8%, 89.3% | |
| net worth before the tilt | $104.9T, $102.3T | $103.8T, $113.7T | $96.1T |
| median net worth, all units | $70,400, $71,100 | $74,500, $73,900 | $91,050 |
| mean net worth, wages $200k+ | $4.09M, $3.96M | $4.10M, $4.01M | $4.36M |
| consumption, wages $1-25k | $23.0k, $23.5k | $24.4k, $24.3k | $27.6k |
| consumption, wages $100-200k | $77.5k, $76.4k | $76.2k, $77.0k | $70.0k |
| consumption, wages $200k+ | $130.5k, $129.8k | $129.9k, $131.9k | $113.3k |

The SCF's mean at $200k+ of wages reads $4.36M here and $4.49M before:
$200,000 of wages now falls in the $200k+ group in every report, as in
the consumption report.

**Before that, from 22 October, "latest" added consumption from the CEX** (with gross
Social Security shared, dependent filers at zero wealth and consumption,
the consumption targets, packing and the faster generator). Two seeds
(20261016–17), made in `explore/2026-10-22/cex6/` (runs `dep0_seed<n>`)
and copied into the main output folders (`data/`, `targets/`,
`model/`, `gen/`, `tilt/seed<n>/`) without rerunning; the code that made
them differs from the committed code only in computing the output layers
one by one (the same answers to rounding) and in where paths are set. The
previous latest is in `previous_2026-10-21_wage_scale/`. After the tilt: every IRS, business, wealth and
consumption target met; effective sample kept 89.3–89.8% (the previous
latest 88.5–89.0%). Median net worth over all units $74k, because the
9.9 million dependent returns now hold none; the SCF's $91k counts them in
their parents' households, and the wealth count targets now leave them out.

Consumption, not targeted, nominal 2019 dollars ($ thousands per unit;
the two seeds, then the CEX 2019 over all tax units):

| Group | File | CEX 2019 |
|---|---:|---:|
| no wages | 36.5–36.8 | 34.1 |
| wages $1–25k | 24.3–24.4 | 27.6 |
| wages $25–50k | 34.4–34.6 | 33.3 |
| wages $50–100k | 50.1–50.3 | 47.8 |
| wages $100–200k | 76.2–77.0 | 70.0 |
| wages $200k+ | 129.9–131.9 | 113.3 |
| bottom income decile (each source's own income) | 28.2–28.6 | 22.9 |
| 5th decile | 34.4–34.6 | 35.2 |
| top decile | 91.9–93.2 | 99.1 |
| married, no dependents | 59.4–60.1 | 56.0 |
| married, with dependents | 74.7–74.9 | 67.3 |
| not married, no dependents | 26.5–26.7 | 27.5 |
| not married, with dependents | 39.6–40.2 | 38.5 |
| 65+, gross Social Security $15–30k | 37.3–38.2 | 38.3 |
| 65+, gross Social Security $30–50k | 58.0–59.4 | 52.0 |

The two seeds agree within about $1–2k, so these gaps are not chance.
Category shares within each wage band are within about 1 point of the
CEX's. The income deciles compare AGI with the CEX's own income measure,
which runs well below AGI at the same rank; and the CEX compresses its top
wages. Report: `tilt/<run>/cex_report.md`.

A check run of this committed code from scratch (seed 20261016, H100,
`explore/2026-10-22/pipeline_candidate/`) rebuilt the CEX and SCF units
byte for byte and gave the same consumption by wages within about $1–3k.
Its exact business tilt failed on one tiny cell (estate and trust net
losses at AGI $50–100k: 6,232 returns and $107 million targeted; the stack
drew 12,698 returns and $40 million), so the business targets fell back
to 2%, and that cell still missed by 8–9%; everything else was met, with
90.0% of the effective sample kept.

**Before that, from 21 October (night), "latest" rescaled the SCF's top wages**
(above); three seeds; the age-range runs moved to
`previous_2026-10-21_ages/`. Against them, after the tilt: mean net worth
at wages $200k+ $3.67–3.73M ($3.40–3.56M), median $1.53–1.56M
($1.37–1.44M); effective sample kept 88.5–89.0% (88.4–89.6%). Median net
worth at 65+ with $200k+ of wages barely moves: $2.9M against the SCF's
$6.5M (`ROADMAP.md`, known gaps). Full tables:
`explore/2026-10-21/wage_scale/compare.md`.

**Before that, "latest" added the primary filer's age range** to the
two-slot run, three seeds (20261016–18); the two-slot runs
moved to `previous_2026-10-21_two_slot/`. Against them, after the tilt:
median $90.9k for both; tilt move 0.05–0.06 for both, effective sample
kept 88.4–89.6% (89.1–89.8%); mean net worth at wages $200k+
$3.40–3.56M ($3.47–3.57M). Generated filers' ranges match the PUF's
(before the tilt, 11.2% under 26 and 16.4% 65+, against 11.1% and 16.2%).
Full tables: `explore/2026-10-21/ages_v1/compare.md`. Net worth by age
within wage band against the SCF is in the known gaps (`ROADMAP.md`).

**Until then, "latest" was the two-slot run**, three
seeds (20261016–18), in `model/`, `gen/` and `tilt/`; the cell-method runs
described below moved to `previous_2026-10-21_cell/`. Two-slot against cell
method, after the tilt, all three seeds: median $90.9k for both; mean net
worth at wages $200k+ $3.47–3.57M (cell $3.38–3.68M); wages $1–25k
$183–185k (cell $180–192k); tilt move 0.05–0.06 and about 90% of the
effective sample kept for both. Full tables:
`explore/2026-10-21/two_slot/compare.md`. The text below describes the
cell-method runs.

The balance-sheet run of 21 October, three seeds (20261016–18), outputs
under `/nfs/roberts/scratch/pi_nrs36/jar335/rebuild/`; full tables in
`explore/2026-10-21/compare_latest.md`. The previous latest (one net worth
item) is kept in `previous_2026-10-20/`. Nominal 2019 dollars; ranges are
over the three seeds.

**With the offset** (the current latest; the run without it is kept as
`gen/` and `tilt/seed<n>_no_offset/`). After the last of the three
generations every ruled holding is within 0.02% of the model's own expected
holders. After the tilt:

| | With offset | Without | SCF 2019 |
|---|---:|---:|---:|
| home mortgage holders | 38.6–40.1% | 45.1–46.8% | 41.6% |
| homeowners with a mortgage | 65.1–67.0% | 72.1–74.2% | 65.6% |
| home mortgage dollars | $11.1–11.6 trillion | $12.3–12.8 trillion | $9.8 trillion |
| other residential real estate holders | 11.4–12.9% | 15.7–17.7% | 12.7% |
| nonresidential real estate holders | 5.4–5.7% | 6.7–7.1% | 6.5% |
| homeowners | 59.4–60.0% | 62.5–63.1% | 63.4% |

Shares of units are not fully comparable: the file has 187 million units
(returns plus non-filers), the SCF 143 million, and the extra units are
mostly dependent filers and non-filers who rarely hold these assets. The
offset fixes counts, not sizes: mortgage dollars stay above the SCF's.
Every IRS and business target is met exactly; 89.5–90.1% of the effective
sample kept.

**Without the offset** (the figures below):

**The tilt:** every IRS target and all 140 business targets met exactly in
every seed; 89.8–90.8% of the effective sample kept.

**Net worth before the tilt:** $96.5–107.5 trillion (SCF $96.1 trillion).

**After the tilt:**

| | File | SCF 2019 |
|---|---:|---:|
| median net worth | $90,900–91,000 | $91,050 |
| mean net worth, income under $25k | $125,000–131,000 | $78,900 |
| mean net worth, income $1 million and over | $16.0–18.3 million | $26.2 million |
| homeowners | 62.5–63.1% | 63.4% |
| home mortgage holders | 45.1–46.8% | 41.6% |
| home mortgage dollars | $12.3–12.8 trillion | $9.8 trillion |
| mortgage above the home's value (% of mortgagors) | 2.9–3.3% | 1.5% |
| other residential real estate holders | 15.7–17.7% | 12.7% |
| business equity holders | 12.1–12.6% | 11.4% |
| stocks outside retirement, holders | 16.2–17.3% | 19.2% |
| dividends over $1,000 with no stocks (no rule) | 18.8–21.0% | 18.7% |

Every consistency rule holds on every record (0% breaks).

**What the rules add.** Of the draws each rule forced, the share the model
alone would have drawn as zero, as a share of all holders in the stack
(unweighted draws): home about 6%; home mortgage about 10%; other
residential real estate about 25%; nonresidential real estate about 24%;
owning a business about 0.1%. Nothing takes these holders back elsewhere, so
mortgages and other real estate come out above the SCF.

The previous latest (one net worth item, 20 October), after the tilt:
median $90,850, income $1 million and over $17.2 million, income under $25k
$121,400; 90% of the effective sample kept.

Known gaps are listed at the end of `ROADMAP.md`.
