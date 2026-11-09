# The rebuild pipeline: method of record

The pipeline makes a file of tax units, filers and non-filers, for every year 2015-2075. Networks trained on the 2015
PUF and four household surveys draw the records; a reweighting then moves the records' weights so the file meets the
year's published totals. Every dollar in a year's file is in nominal dollars of that year. Variable names are
Tax-Simulator's wherever Tax-Simulator has the item.

The code is in `pipeline/`, one folder per stage. Every setting is in `pipeline/settings.json` (or the copy named by
`REBUILD_SETTINGS`); each year's targets, growth rules and survey mix are in `pipeline/targets_config/<year>/`.
The sections below follow the stages in the order the runner runs them.

| Stage | Folder | What it does |
|---|---|---|
| Data preparation | `prep/` | survey tax units, the CPS filing stage, SOI tables and their errors, the training table |
| Pretraining | `pretrain/` | the network and its training |
| Generation | `generate/` | drawing records; the top of the file (aggregate records rebuilt, billionaires) |
| Scaling to the year | `scale/` | the year's dollar maps, growth to the year, projection steps, non-microdata imputations |
| Reweighting | `reweight/` | taxes from the calculator, targets and their errors, the solve, state weights |
| Output | `output/` | the Tax-Simulator file, the package for PUF holders, reports |
| Resource builders | `resources_build/` | scripts that rebuild the committed tables in `research/rebuild/resources/` (run by hand) |

Code shared by three or more stages is in `shared/`. Known limitations are listed at the end.

---

## 1. Data preparation

### Sources

| Source | Years in the training table | Rows | Covers |
|---|---|---:|---|
| 2015 PUF | 2015 | 207,692 | filers |
| SCF | waves 2013, 2016, 2019, 2022, 2025 (income years 2012, 2015, 2018, 2021, 2024) | 36,478 / 38,698 / 35,515 / 28,211 / 27,216 | every tax unit; five implicates each |
| CEX interview survey | 2015, 2017, 2019, 2021, 2023, 2024 | 36,335 / 34,281 / 30,206 / 29,133 / 26,893 / 26,400 | every tax unit, one row per quarterly interview |
| CPS ASEC | tax years 2015, 2017, 2019, 2021, 2023 | 88,231 / 86,913 / 77,282 / 75,672 / 72,952 | every tax unit, filers and non-filers |
| SIPP | reference years 2015, 2017, 2019, 2021, 2023 | 23,699 / 33,536 / 27,907 / 22,140 / 20,317 | every tax unit |

Rows include the 2% held back from training. The PUF's four aggregate records are not in the table; they are rebuilt
after generation (section 3, "The top of the file").

### Tax units in the surveys

Every survey is split into tax units by one module of CBO's rules (`prep/tax_units.py`), run on a member list that
each survey's reader builds (`prep/roster_cps.py`, `roster_scf.py`, `roster_cex.py`, `roster_sipp.py`). Where a
survey lacks an input the rule uses a named fallback (`tax_units.FALLBACKS`), and every unit carries flags for the
inputs that were assumed. CBO's reassignment pass is on (settings `tax_units.reassign`). Dependents with income of
their own stay people of the claiming unit; dependent filers are not separate survey units. Group quarters are left
out. `TAX_UNITS.md` has the rules and each survey's treatment.

### The PUF

The PUF rows carry every PUF item production Tax-Data carries, under production's names, plus two adjustments
production does not carry (`se_tax_ded`, E03260; `hsa_contr`, E03290):

- marital status (joint or not), dependents (0-3), full filing status, child credit children and EITC children;
- the primary filer's age range (below, "Ages");
- wages; taxable interest; qualified dividends (`div_pref`, E00650) and other dividends (`div_ord`, E00600 less
  E00650), with ordinary dividends computed from them;
- capital gains in three pieces: short-term (`kg_st`, P22250) and long-term (`kg_lt`, P23250) gains and losses from
  Schedule D before the $3,000 loss limit, and capital gain distributions (`kg_dist`, E01100). The total
  `capital_gains` is computed from them (below);
- other gains, taxable IRA distributions, taxable pensions (`txbl_pens_dist`, E01700), the non-taxable rest of
  pensions (`nontxbl_pens_dist`, E01500 less E01700, never below zero), unemployment, alimony, state refunds,
  tax-exempt interest, gross Social Security (`gross_ss`), and the eleven statutory adjustments (their total computed);
- the rest of AGI (`other_agi_remainder`): AGI less every identified item. A net operating loss deduction is a negative
  amount here;
- business income in its 18 PUF parts: Schedule C; partnership and S corporation income, losses and section 179,
  passive and nonpassive; rents, royalties, estates and trusts; farm rental; farm. Business income and four subtotals
  are computed from the parts: Schedules C and F (`sch_cf_income`); Schedule E; rents and royalties (`sche_rent`);
  and the rest of Schedule E (`sche_rest`: partnerships, S corporations, estates and trusts, farm rental);
- `k1_owner`, whether the unit holds a partnership or S corporation interest (any partnership or S corporation part
  nonzero);
- 40 further PUF-only items: unrecaptured section 1250 gain, 28% collectibles gain, `part_se` (below), nine itemized
  deduction items (medical, state and local income or sales tax, cash and noncash charity, casualty, job expenses,
  tax preparation, other miscellaneous, other itemized), mortgage interest and real estate tax, two AMT items, eleven
  credits and two other taxes;
- the spouse's share of the couple's self-employment income and the earnings split code (below).

**Computed items, never drawn.** Ordinary dividends, the adjustments, capital gains, business income and its
subtotals, and `k1_owner` are computed from their parts wherever records are made or grown, so every total equals the
sum of its parts. Two more:

- gross pensions `gross_pens_dist` = taxable pensions + the non-taxable rest;
- taxable Social Security `ss_taxable`, on every record, by the section 86 worksheet as tax-simulator-gpu computes it
  (`shared/common.taxable_social_security`): provisional income is half the benefits plus modified AGI (AGI without
  taxable benefits, plus tax-exempt interest, plus the student loan interest deduction); 50% is taxable above $25,000
  ($32,000 joint), 85% above $34,000 ($44,000 joint), at most 85% of benefits; separate returns use the single
  amounts. It is part of `other_agi`, so the file's AGI holds it. It has no growth line: the worksheet is applied to
  grown items with its unindexed thresholds. On a PUF row, AGI is the PUF's E00100 with E02500 replaced by the
  worksheet's amount.

**The capital loss limit.** It is applied where AGI is made, not in the data: `shared/common.allowed_capital_gain`
holds a Schedule D net loss (short plus long term) to $3,000 ($1,500 on a separate return), nominal and not indexed.
The file's `capital_gains` (Schedule D net in AGI, SOI Table 1.4's line) and `capital_gains_line13` (with
distributions) apply it. The calculator applies the limit itself to `kg_st` and `kg_lt`, and its `kg_lt` includes
distributions, so the two can differ only on a record with distributions and a net loss past the limit.

On 6.2 million 2015 returns ($52 billion) Form 1040 line 13 (E01000) is below the allowed Schedule D net of the two
pieces. The shortfall is put on the short-term piece, so each such return's allowed net equals its line 13 and it
keeps its loss as SOI counts it. What is left the other way ($0.1 billion on 1.7 million returns) stays in other
AGI, so each PUF record's AGI is still the PUF's.

**Self-employment earnings other than Schedules C and F (`part_se`)** (`prep/earn_split.part_se_from_schedule_se`;
the same rule in `generate/top_hidden.py` and in both merge scripts). E30400 is the lesser of a person's Schedule SE
net earnings and the room under the Social Security wage base, so it cannot be read directly. The rule:

1. Self-employment tax is 12.4% of E30400 plus 2.9% of net earnings, so net earnings = (E09400 - 0.124 x E30400) /
   0.029. Where E09400 is within the PUF's rounding of 15.3% of E30400, net earnings are E30400.
2. On a return with partnership income, `part_se` = net earnings / 0.9235 less Schedules C and F.
3. On any other return, the same where net earnings differ from 92.35% of Schedules C and F by more than the larger
   of 1% and $50, and not where net earnings are zero with Schedules C and F at most $433. Otherwise zero.

`part_se` can be negative (net earnings below 92.35% of Schedules C and F): 0.94 million 2015 returns, $9.2 billion.
Example: a partner with no wages reports tax of $43,694 and E30400 of $118,500; net earnings are (43,694 - 14,694) /
0.029 = $1,000,000, and `part_se` is $1,082,837.

**The split of a couple's earnings** (`prep/earn_split.py`). The spouse's share of the couple's self-employment income
is E30500 / E30400 on a joint return (E30400 is the couple's Schedule SE earnings, E30500 the spouse's part). The
earnings split code is the PUF's EARNSPLIT (1-3); where a joint return has no demographics it is filled by a gradient
boosted classifier fitted on the known codes (settings `earn_split.model`), whose probabilities are carried as
`es_p*`. Both are shared with the CPS (the CPS couple's own code and share). The spouse's share of wages is drawn
inside the range the code allows (section 3). `AGES.md` has the reading of the PUF fields.

`other_item_exp` (other itemized deductions: P04470 + E21040 less the identified itemized items and the medical floor)
is negative on 15 million returns because the PUF rounds each item on its own; it is held at zero from below.

### Ages

The primary filer's age range is shared by every source: the PUF demographic file's six ranges (under 26, 26-34,
35-44, 45-54, 55-64, 65+) and its three for dependent filers (under 18, 18-25, 26+). The 88,269 PUF records with no
range (3.68 million returns, mostly AGI above $400,000) carry probabilities from a logit calibrated to SOI Table 1.6,
and training draws a range from them each pass. In a survey couple the primary filer is chosen by a rule keyed by
household that reproduces the PUF's share of female primaries by range. Exact ages of the primary filer and spouse are
on the SCF, CEX, CPS and SIPP (to 79, then 80 for 80 and over); dependents' exact ages and both sexes are CPS-only.
`AGES.md` has the method.

### The CPS

The ASEC asking about tax years 2015, 2017, 2019, 2021 and 2023 (`prep/build_cps.R`, settings `cps`). Every unit,
filers and non-filers, as the filing stage builds them. Separate returns, each return's filing status, its statutory
filing requirement and its filing chance come from the filing stage's returns (`data/filing/returns/`).

**The filing stage** (`prep/filing.py`, `prep/filing_requirement.R`, settings `filing`). Every CPS year:

1. The units, by the shared rules, people in group quarters left out.
2. Each unit's statutory filing requirement (Form 1040, Charts A to C; production's `add_filing_requirement`).
3. The filing rule: log odds of filing = a (age band of the head) + k x (a child under 17) + required x (r + s x log
   gross income centred on the year's median among required units + b x any business or farm income). The 11 numbers
   are solved each year so that 11 targets hold exactly:
   - non-filing heads and spouses in 7 age bands = CPS adults less adults on returns (SOI Table 1.6) less dependents
     aged 15 and over not filing a return of their own;
   - non-filing children under 17: 3.08% of CPS children under 17 (the 2015 share: Census population less the PUF's
     child credit children);
   - non-filing units among those required to file: 9.44% (Pub 5785's 11.19 million, a 2014-16 average, over the 2015
     CPS's required units);
   - their gross income: 4.555% of all required units' (Pub 5785's $409.4 billion over the 2015 CPS's);
   - those with business or farm income: 44.6% of required non-filers (Pub 5785: 4.99 of 11.19 million).

A dependent aged 15 or over files a return of its own when required to or with any wages. Separate returns: joint
couples are taken by a draw keyed by unit id until their expected returns reach SOI Table 1.6's count. Outputs in
`data/filing/`: `units/`, `requirement/`, `returns/` and per-year checks (`checks/`), among them
`<year>_nonfilers_vs_targets.csv`, which the reweighting reads for non-filers by age.

**Items.** Each person's items in primary-filer order (the builder's person 1 is the man; where the primary-filer rule
picks the woman every pair of person columns is swapped, `prep/build_table.cps_person_slots`):

- the exact ages of up to three dependents (youngest first) and the sex of the primary filer and spouse;
- the spouse's share of the couple's wages; each adult's weeks worked and usual weekly hours;
- childcare: the SPM unit's child care expenses, given to the units that claim its children;
- each adult's overtime pay (below);
- each person's Social Security number status (below);
- the household's state (below);
- tax items: wages, dividends (as qualified dividends), pensions, unemployment, Schedule C (nonfarm and farm
  self-employment), gross Social Security, rents, the rest of AGI, the filing flag, filing status, child credit and
  EITC children. Survivor and disability pensions paid on Form 1099-R (company or union, government, military and
  railroad pensions, annuities) count as pensions; other survivor and disability payments stay in the rest of AGI
  (`data/cps_survivor_disability_sources.csv`). Items the CPS does not ask (interest is not shared; short-term gains,
  partnership and S corporation parts and the like) are zero on CPS rows. Capital gains are not shown in training.

**Overtime** (`prep/overtime_cps.R`, settings `cps.overtime`), by Ernie Tedeschi's rules (`Reports/2024/Overtime`):
who the Fair Labor Standards Act covers (private, state and local government employees, not clergy; the postal
service, TVA and Library of Congress put back); the named exempt occupations and industries; for salaried workers at or
above the year's salary threshold ($455 a week through 2019, $684 from 2020), the white-collar duties test as a chance
from the Labor Department's occupation class and the person's pay (one seeded draw per person); the highly compensated
line ($100,000 through 2019, $107,432 from 2020); then time and a half for hours over 40 last week at the main job,
times weeks worked, at most the person's wages. A covered worker whose usual hours vary has none. Occupations are
IPUMS's harmonized 2010 codes in every year.

- Only the earnings-question group (months 4 and 8 in sample) is asked weekly pay and hours; on other units overtime
  is missing, not zero, and absent in training. Hours at the main job, and the weekly pay the March 2024 ASEC lost for
  its month-4 group, come from the March basic monthly file linked person by person (IPUMS's `MARBASECIDP`; pay
  recovered for 99.8% of workers; settings `paths.raw.cps_march_basic`).
- Levels: 2015 12.3 million workers and $204 billion; 2019 13.3 million and $255 billion; 2023 12.0 million and
  $244 billion (Tedeschi's 2023 file: 12.6 million and $272 billion; the gap is above $200,000 of pay, where weekly
  pay is top-coded). The whole overtime pay is stored, not the premium alone (one third of it), which the 2025 law
  deducts.

**Social Security number status** (`prep/build_cps.R`). Every person: 1 a citizen; 2 a noncitizen with a sign of
lawful status; 3 a noncitizen without one, taken to have no number. For a child the signs are its own Social
Security, SSI, Medicare or military cover, a citizen parent, or a noncitizen parent with a sign; for the filer and
spouse their own Social Security, SSI, Medicare, Medicaid, military cover, unemployment insurance, public housing, a
government job, veteran status, or marriage to a citizen. Five CPS slots (`ssn_status_primary`, `_spouse`,
`_dep1-3`; 0 none). In 2019, 1.03 million dependents under 17 lack a number (DHS: 1.01 million unauthorized children
under 18 in 2020).

**State.** ASEC's STATEFIP as the CPS-only category `state` (0 none, else 1 + the position among the 50 states and DC
by FIPS code); a unit's state is its household's. The networks read it only for state weights (section 5).

**Encodings** (`prep/build_table.py`, `CPS_AMOUNTS` and `CPS_CATEGORIES`): exact ages are categories, 0 for none,
else the age plus 1 (so an infant is not "none"): 87 values for dependents (the CPS top-codes at 85), 82 for the
primary filer and spouse (80 and over as 80). Sex: 0 none, 1 a man, 2 a woman. The spouse's share of wages is an
amount from 0 to 1 cut into bands within each year, recorded only on married couples with some wages (absent
elsewhere). Weeks, hours, childcare and overtime are amounts cut within each year.

### The SCF

Waves 2013-2025 (`prep/roster_scf.py`, then `prep/build_scf.R`, which sources production's
`src/imputations/wealth_schema.R`). The financially independent adults outside the primary economic unit (PEU) are
units of their own, with their share of the group's income and balance sheet (settings `tax_units.npeu_split`:
equal, or Moore's rule for wages and Social Security), flagged `group_level`; married couples among them file
jointly; the SCF's support answer stands in for the student and income tests; children living elsewhere count only
where the family paid alimony or child support; separate returns from the couple's own answer. The PEU's income and
balance sheet go to the respondent's unit, or are divided between the respondent's and the spouse's or partner's
units by Moore's shares. 2019 wave: 162.8 million units.

**Income items.**

- Gross Social Security: the benefits section, the respondent's and the spouse's or partner's current benefit
  (X5301-X5312), before Medicare premiums, made yearly from its period; SSI (kind code 7) left out (the 2013 public
  file has no SSI code, so its amounts include SSI). Adults outside the PEU get their share of X6406.
- Schedule E in two parts: rental income, the property section's net income per property (X1730, X1830, X2010,
  X2020) plus gross rent from part of the home or farm (X1224, X505), against the PUF's `sche_rent`; and the rest,
  X5714 less rental income, floored at zero, against `sche_rest`. No depreciation estimate is made.
- Schedules C and F (X5704); interest, dividends, capital gains, tax-exempt interest, wages.
- Ownership items, household answers given to the unit holding the household's business equity (or the respondent's
  unit when that equity is zero): owns any private business (X3103 or X3401); a partnership, S corporation or LLC
  interest (the legal form of actively managed businesses 1 and 2, X3119 and X3219, or a non-actively managed one,
  X3407, X3411, X3451, X3415).

**The balance sheet**, production's 20-item wealth schema: 14 assets (deposits; stocks and stock funds; bonds and bond
funds; IRAs and DC accounts; DB account balances; cash-value life insurance; annuities; trusts; other financial;
business equity; the home; other residential real estate; nonresidential real estate; vehicles and other
nonfinancial) and 6 debts (home mortgage; other residential debt; other lines of credit; credit cards; installment
loans; other debt), as `value.<item>`. Net worth is computed from them. The home mortgage is carried as a share of the
home's value (`w_primary_ltv`). Also the four unrealized gains (home, other real estate, business, stocks and funds;
`w_kg_*`).

**The retirement accounts' stock share** (`w_dc_stock_share`): the summary file's RETEQ / RETQLIQ (RETQLIQ = IRAKH +
THRIFT + FUTPEN + CURRPEN, the rebuild's `value.dc` plus `value.db`), applied to `value.dc`. A household ratio, the
same on each PEU unit; 0 without `value.dc`; missing on units outside the PEU. Weighted share 0.53-0.55 in every wave.
`w_dc_stocks` = share x `value.dc`.

**Elective deferrals** (settings `deferrals`). Each person's elective contributions to up to two plans on the current
main job, traditional plus Roth (the SCF does not separate them): held to the income year's legal limit for their age
($18,500, or $24,500 from age 50, in 2018), then to that person's own wages (the PEU's wages split by pay on the main
job); missing on units outside the PEU. Two slots: `elective_deferrals` and `spouse_deferral_share` (0 unless married
with deferrals). 2019 wave: $330.5 billion on 43.1 million units (W-2: $332.5 billion for 2018). The traditional part,
elective x (1 - the Roth share), comes off wages to make the SCF's link wage (below, "Translation"). The Roth share
is SOI's (W-2 Table 5.A for 2012-2018, Table 4.B for 2019-2020, joint and non-joint; 2019: 8.0% joint, 9.9%
non-joint).

**Auto loan interest** (`auto_int_exp`): for vehicles 1-4, the amount owed (X2218, X2318, X2418, X7169) times the
annual rate in hundredths of a percent (X2219, X2319, X2419, X7170) over 10,000; a rate coded -1 reads as 0. Divided
among the PEU's units as installment debt is; none to units outside the PEU. Not part of net worth.

**Inheritances received this year** (`inheritance`; `roster_scf.inheritances`, settings `inheritance`). Up to three
inheritances or trusts received (gifts left out), with the value and the year. The public file rounds the year to
the nearest 5, so each counts as received in the wave's income year with the chance its year code carries
(`inheritance.year_weights`: for 2013-2022 the Estate-Tax-Distribution model's weights calibrated to Gale and
Sabelhaus (2024); for 2025, 1.25 over the years the code covers, so 0.5), realised by a draw keyed by household,
implicate and position. The value is taken as income-year dollars. A unit holding the whole PEU takes all of it, a
split couple half each; missing on units outside the PEU. No translation row and no fill-in. Weighted totals by wave
(income years 2012-2024): 2.3, 2.2, 2.9, 3.0 and 3.8 million inheritances; $367, 372, 554, 595 and 1,088 billion.

**Business workers and industry, for the QBI items.** Two groups by the legal form of the household's actively
managed businesses 1 and 2: `cf` (sole proprietorship, or "not a formal business") and `ps` (partnership, limited
partnership, LLP, LLC, S corporation); other corporations get no item.

- `biz_workers_<group>` (a count, not grown between waves): the owner's share of the workers, summed over the
  group's businesses. In an S corporation the count is all its workers times the ownership share, household members
  included, because an S corporation pays a working owner W-2 wages. Every other form counts outside workers only.
- `biz_industry_<group>`: the SCF industry group of the group's largest business by the household's share of value;
  8 for "no industry code", 0 with none.

The 2025 wave's raw files are read from `paths.raw.scf_wave_dirs`. Its record ids are offset by 9e8.

### The CEX

BLS's interview survey (`prep/roster_cex.py`, then `prep/build_cex.R`, settings `cex`), five quarterly files per
survey year (the first quarter from the previous release from 2020 on), weights summing to the year's consumer units.
Households are split into tax units by the shared rules; BLS's own tax-unit codes are only a comparison column in the
checks. Spending is each interview's three months times 4, in production's 8 categories (clothing; motor vehicles;
other durables; other nondurables; food at home with alcohol; gasoline; housing and utilities; other services and
health), divided among a household's units by head count. Total consumption is computed from the categories.
Separate returns by the CPS's draw to SOI's count. 2019: 162.8 million units. The dollar map uses NIPA PCE by category
(`resources/growth/nipa_pce_categories.csv`).

### The SIPP

One public-use release per reference year: 2015 (the 2014 panel, wave 3), 2017, 2019, 2021 and 2023 (the releases of
2018, 2020, 2022 and 2024; `prep/roster_sipp.py`, settings `sipp`). The household as interviewed, split into tax units
by the shared rules; income is the reference year's 12 months; separate returns by the CEX's rule.

- Shared with the PUF: married, dependents, the age range, exact ages, wages (jobs for an employer, less the
  traditional part of each adult's elective deferrals in 2021 and 2023), dividends (TINC_STMF), gross Social Security
  (TSSSAMT, before Medicare premiums) and pensions (TRET1AMT-TRET8AMT, against taxable pensions), each the primary
  filer's plus the spouse's. Shared with the CPS: the spouse's share of the couple's wages.
- SIPP only: each adult's tips in dollars and whether that adult's tips are mostly from leisure and hospitality. Tips
  are production's definition (`src/imputations/tips.R`): tips on jobs for an employer (TJBn_TXAMT, job lines 1-4),
  not on self-employment; mostly leisure and hospitality when more than half are from jobs with Census industry codes
  8560-8690. Job earnings include the job's tips, so tips are part of wages, as on Form W-2. The SIPP finds about half
  the W-2's tipped workers (3.7 million against 6.05 million in 2019); the W-2 targets set the level.
- Deferrals (2021 and 2023) are held to the same limits as the SCF's and only make the link wage.

### SOI tables and their errors

`prep/soi.py` reads every SOI number the pipeline uses from the local mirror (`paths.raw.soi_mirror`) into
`targets/soi.csv`: the targets, and the totals that grow dollars and set cut points. Past SOI's last year (2023) SOI
items grow by CBO's lines (`resources/growth/cbo_individual_income_tax.csv`). `prep/soi_cv.py` parses SOI's published
coefficients of variation from the downloaded workbooks (`targets/soi_cv/raw/`) into `targets/soi_cv/soi_cv.csv`.

### The training table

`prep/build_table.py` stacks the five sources into `data/table.parquet`, with the band edges (`band_edges.json`),
the dollar maps, each year's tax data, the translation report and per-survey-year side files.

**Shared variables.** All five sources share married, dependents, the primary filer's age range, wages and gross
Social Security. The SCF, CEX, CPS and SIPP share the exact ages of the primary filer and spouse; the CPS and SIPP
share the spouse's share of wages. The PUF also shares:

- with the CPS, the spouse's share of self-employment income and the earnings split code;
- with the SCF, CEX and CPS, Schedules C and F (the CPS's as Schedule C);
- with the SCF, CPS and SIPP, ordinary dividends (the CPS's as qualified dividends);
- with the SCF, capital gains; with the SCF and CPS, the two parts of Schedule E, tax-exempt interest and the
  partnership flag (zero on the CPS, which does not ask them);
- with the SCF alone, taxable interest;
- with the CPS and SIPP, taxable pensions;
- with the CPS alone, every other PUF tax item, because its rows are the non-filers.

Every other variable belongs to one source. A slot a source lacks is absent on its records (`pretrain/model.py`,
`present_on`).

**Amounts as bands.** Each amount is one of 84 bands: band 0 is exactly zero, 10 bands of losses, 73 bands of
positives, finer at the top (the top bands each hold 0.01% of positive weight). A band becomes dollars by drawing a
real record's value from that band.

**The AGI group label** (PUF only; `pretrain/qgroups.py`). The record's band-AGI group, one of 12, computed from its
bands: AGI computed from a fixed amount for each band (the weighted mean of the real 2015 PUF amounts in it), with
taxable Social Security by the worksheet. Three groups at or below zero (below -$1 million, -$1 million to -$50,000,
-$50,000 to 0) and nine above, split at the weighted p50, p75, p90, p95, p99, p99.5, p99.9 and p99.99 of band-AGI
among 2015 returns ($35,813 to $11.3 million).

**Side files.** For each target year's mix of survey years the table stage writes `cex_/cps_/sipp_units_<y>_all.parquet`
(the survey's units in tax-data terms); the targets read them. The side-files step writes them for every odd year
2015-2023 so any target year can use them.

### Translation: putting the surveys on the PUF's terms

Two standard functions and one table (settings `translation`; `shared/translate.py`). No variable or survey has its
own code path. A variable not in the table is not shared.

**The tax data for a year** (`prep/build_table.tax_data`), the base of every comparison:

- the PUF's filers, dependent filers left out, weights scaled in each filing status and age range by SOI Table 1.6's
  returns in that year over 2015's (a record with no range averaged over its range probabilities);
- every dollar item grown by SOI's growth in dollars per return with the item, totals recomputed from grown parts;
- the non-filers: every CPS unit of that year weighted by its chance of not filing (weight x (1 - `filer_p`)), its
  amounts in PUF terms by the translation table;
- a year with no CPS takes the nearest CPS year, weights scaled by SOI's returns and dollars grown per return. Past
  2023 the last year's growth is carried forward.

**The table.** One row per survey and shared variable: the survey's items (summed), definition fixes, the dollar line
and how amounts beyond it are matched, whether function 2 applies (and its size ranges), whether the variable is part
of the type function 2 conditions on, whether it is shown in training, the first year the survey asks it, and the
reason. Defaults: dollars, no fill-in.

**Function 1, amounts.** An amount within the dollar line (2015 dollars grown per return to the survey's year) keeps
its dollars. Beyond the line, by the row's match:

- count from the top (the default; with a line of zero only): the survey unit with N units above it gets the tax
  data's amount with N units above it; losses counted from the bottom;
- percentile among the units beyond the line in each data set (required with a line above zero, so the map is
  continuous at the line);
- for wages, percentile counted over all units from the top (`percentile_from_top`).

No line means dollars throughout; a line of zero means the match throughout. The result is cut on the PUF's cut
points grown to that year.

**Wages and elective deferrals.** The file carries W-2 box 1 wages, so each survey's wages are first made a link wage:
gross wages less the traditional part of the elective deferrals the survey reports (the SCF; the SIPP in 2021 and
2023), and gross wages as reported elsewhere. Health premiums and other cafeteria-plan items stay inside the link wage
for the mapping to absorb. Every survey has fewer units with wages than the tax data (by 1.5 to 8 points); function 1
gives each earner the tax data's wage at the same share of all units counted from the top, which leaves the smallest
wages to function 2. Example: in the SCF 2018, units with $100,000 or more of wages are 12.5% of units after function
1, as in the tax data.

**The rules by survey.** "Fill-in" is function 2; "percentile" means the survey's p-th percentile holder gets the tax
data's p-th percentile holder's amount.

| Survey | Item | Rule |
|---|---|---|
| every survey | wages | the link wage, fill-in, then percentile counted over all units from the top |
| SCF | taxable interest, dividends, tax-exempt interest | dollars, fill-in |
| SCF | capital gains | losses read as at most $3,000 and those at the limit matched to the tax data's losses past $3,000 (`loss_line` $2,999); fill-in; dollars to $50,000, percentile above |
| SCF | Schedules C and F | fill-in, count from the top |
| SCF | rental income; the rest of Schedule E | each fill-in, then percentile, income and losses separately; the rest's fill-in also conditions on the partnership flag |
| SCF | gross Social Security | dollars |
| CEX | self-employment, against Schedules C and F | fill-in, count from the top |
| CEX | gross Social Security | dollars |
| CPS | dividends, into qualified dividends | ranked against the tax data's ordinary dividends; fill-in, percentile |
| CPS | capital gains, into long-term gains | not shown in training; gives the tax data's non-filers their gains from tax year 2018 (zero before) |
| CPS | pensions, with the 1099-R survivor and disability pensions | all taken as taxable pensions, compared with taxable pensions plus taxable IRA distributions; fill-in, dollars; the non-taxable rest is zero |
| CPS | unemployment | fill-in, dollars |
| CPS | Schedule C | fill-in, count from the top |
| CPS | gross Social Security, rents, the filing flag, the rest | dollars |
| SIPP | dividends, against ordinary dividends | fill-in; dollars to $50,000, percentile above |
| SIPP | gross Social Security | dollars |
| SIPP | pensions, against taxable pensions | fill-in, percentile |
| SIPP | the spouse's share of wages | as reported, cut on the CPS's cut points of the same year (`shared_with`) |

Not shared: CEX interest and dividends (one item, top-coded), pensions and net rent; CPS interest (the CPS has twice
the tax data's holders and 2-6 times its dollars); SIPP interest, self-employment, unemployment, rent and alimony. The
SCF's balance sheet is not shared: it is banded within each wave. The SCF reports more high earners than tax data at
every line (2015: 0.47 million units with $1 million or more of wages against the PUF's 0.15 million); its pattern
stays visible through the source label the network always sees.

**Function 2: the fill-in, during training** (`pretrain/fillin.py`). Surveys miss holders. Each item with the fill-in
has two slots on that survey's rows: what the unit reported (never changed) and the PUF-concept amount (reporters copy
their answer; zero-reporters are filled in). The PUF has only the PUF-concept slot.

- From pass 3, at the start of each pass, each zero-reporter gets its chance of a hidden holding in each size range:
  its own chance of reporting that range (from the rest of its record, every filled-in slot hidden), times (tax data
  share / survey share - 1) for its type, divided by its chance of reporting zero, capped at 100%. Each batch redraws
  the fill-ins from those chances.
- The type is the survey year plus married, dependents and the wage band, less the item itself; the SCF's rest of
  Schedule E adds the partnership flag.
- The tax data's share for a type mixes filers (the PUF's own training rows, dependent filers left out) and non-filers
  (CPS label, filing flag 0) by their counts in the tax data. For types with fewer than 30 PUF records (0.7% of SCF
  weight) the network's answer given the type is used instead. Averages over a type use survey weights without q.
- Totals built from a filled-in part (on the CPS: ordinary dividends, capital gains, Schedules C and F, business
  income) and the spouse's share of wages are hidden when the formula reads a unit's chance of reporting, and
  recomputed from the filled-in parts after each draw. A total with a missing part stays missing.
- A range where the survey has as many holders as the tax data, or more, gets none: the fill-in adds holders and never
  removes them.
- It assumes a holder's chance of reporting depends only on its type and true amount.
- A row fills in only in the years its survey asks the item, and only if it is shown in training.
- In the held-back slice no fill-in is drawn, so a zero-reporter's PUF-concept amount there is not scored.
- At generation the reported slots stay hidden; net worth is drawn given the PUF-concept amounts.

Share of units with any, after the 80th pass of the installed seed-20261017 network (`model/<run>/fill_log.csv`):

| Survey and item | Reported | After the fill-in | Tax data at the survey's types |
|---|---:|---:|---:|
| SCF 2018 wages | 72.3% | 77.2% | 76.3% |
| SCF 2018 taxable interest | 11.6% | 24.8% | 25.9% |
| SCF 2018 dividends | 10.4% | 17.2% | 19.4% |
| SCF 2018 capital gains | 7.4% | 14.0% | 14.7% |
| SCF 2018 tax-exempt interest | 2.1% | 4.1% | 3.7% |
| SCF 2018 Schedules C and F | 10.7% | 18.8% | 22.0% |
| SCF 2018 rental income | 6.4% | 9.6% | 7.6% |
| SCF 2018 rest of Schedule E | 5.4% | 8.0% | 7.3% |
| CEX 2019 wages | 75.4% | 79.3% | 76.8% |
| CEX 2019 self-employment | 11.4% | 23.8% | 23.2% |
| CPS 2019 wages | 73.8% | 74.0% | 76.5% |
| CPS 2019 dividends | 15.6% | 21.5% | 19.5% |
| CPS 2019 pensions | 15.8% | 24.9% | 25.5% |
| CPS 2019 unemployment | 2.0% | 4.3% | 4.1% |
| CPS 2019 Schedule C | 7.1% | 14.3% | 22.0% |
| SIPP 2019 wages | 69.5% | 78.0% | 76.3% |
| SIPP 2019 dividends | 14.0% | 21.3% | 20.2% |
| SIPP 2019 pensions | 13.6% | 23.4% | 22.9% |

88.6% of CPS units without wages say nobody worked any week, so the CPS's wage fill-in barely moves.

**The translation report** (`data/translation_report.csv`) covers each row and survey year: the survey as reported,
after function 1, and the tax data, each as units, the share with none and with a loss, units by size range and above
$1k, $10k, $100k and $1M, the mean, and the top 1% and 10% shares. The report stage (`output/report_translation.py`)
adds the stage after function 2 from the trained network: `model/<run>/translation_report.md`.

---

## 2. Pretraining

`pretrain/train.py` with `pretrain/model.py`, `fillin.py` and `qgroups.py`; settings `network` and `training`.

- **The network.** A transformer over slots, one slot per variable: 4 layers, width 128, 4 attention heads, an inner
  layer of 256, no dropout; about 1.4 million parameters, 0.85 million of them in the output layers (one per
  variable, up to 84 choices each). It is trained to fill in any hidden subset of a record's variables from the rest,
  so one network answers every question of the form "this variable, given those". Every record carries its source
  label and survey year.
- **Packing.** Each batch holds one source's records and only that source's slots. Each batch's loss is scaled so every
  record counts as much as in a mixed batch. Output layers are computed in one batched multiplication, only for the
  batch's own source.
- **Precision.** Training steps run in bfloat16; the fill-in, the held-back loss and the logs run in 32-bit.
- **Grouped weighting** (settings `training.q_groups`). Each record counts by its survey weight times q, where q =
  records / survey weight within the record's group. Within a group records count by weight; across groups they count
  by number. Each source's share of training is unchanged. Groups: on the PUF the 12 AGI groups (a record at p99.99
  and over counts about 1.6 returns' worth of weight, one below p50 about 1,390; the top group, 0.02% of returns, is
  4.6% of the PUF's training weight); on the SCF 5 classes per wave at the weighted p50, p90, p99 and p99.9 of
  band-income (wages, interest, dividends, capital gains, gross Social Security, Schedules C and F, and Schedule E's
  two parts, each as reported); on the CEX, CPS and SIPP q = 1. Generation undoes q exactly (section 3).
- **The AGI group label** is never hidden on PUF rows and never scored, because generation always sets it. The network
  is therefore never asked about a PUF record with its label hidden, except by the fill-in's fallback for rare types.
- **Schedule.** 80 passes in batches of 512 records; all records but a 2% held-back slice (seed 20261015). Peak
  learning rate 0.003, warm-up over 3 passes, decay to 5% of the peak; weight decay 0.01.
- **Seeds and pooling.** Three networks are trained, `label_seed20261017`, `label_seed20261018` and
  `label_seed20261019` (`REBUILD_RUN` set to the name, `REBUILD_SEED` to the seed). The same seed, data, code and kind
  of GPU give the same network, weight for weight. Networks of different seeds can differ more than their held-back
  loss shows; a check of the share of filers with each of seven income items in each Table 1.4 AGI class against the
  PUF picks out a bad network without SCF data.
- **Outputs** in `model/<run>/`: `model.pt` (with its slot list), `train_log.csv`, `train_meta.json`, `fill_*.csv`,
  `var_log.csv`.
- **Time.** About 62 minutes a network on an H200 (80 passes).

---

## 3. Generation

`generate/generate.py`; settings `generation` and `consistency`. Two batches, each drawn in equal thirds from the
three networks (each with its own generation seeds, `seed_shift` 0, 1000, 2000; column `model` says which; ids unique
across networks):

- **Filers**, under the PUF label at 2015. 400,000 records (`label_records`), split across the 12 AGI groups in
  proportion to the PUF's records per group (about 18,400 at p99.99 and over, 30,500 at p99.9-99.99, 102,000 below
  p50). Each record starts with its group label set; everything else is drawn given it. Its weight is the group's
  2015 returns over the records drawn in it (1/q of the group), which undoes training's grouped weighting. Every drawn
  record is kept. Each record's band-AGI group is computed from its drawn bands and compared with its label
  (`gen/<run>/label_check.csv`, stack columns `q_group` and `label_ok`); the weight follows the label. About 15% of
  filer records in 2019 have parts adding to another group.
- **Non-filers**, under the CPS label at the target year (filing flag 0). Records = filer records x the CPS's expected
  non-filers (sum over its units of weight x (1 - `filer_p`)) / SOI returns. In a year with no CPS (even years) the
  count is the geometric mean of the CPS years either side and the batch is drawn half at each. A PUF amount the CPS
  does not measure is zero on every non-filer.

**Order of the draw.** One record is drawn slot by slot:

1. the batch's own items: the type (married, dependents, the age range) in random order, then full filing status,
   child credit children and EITC children, then every amount in random order. Full filing status is joint exactly
   when married. Under the CPS label the age range is one of the six adult ranges;
2. the computed totals (capital gains, business income and its subtotals, ordinary dividends, the adjustments) from
   their parts, then `k1_owner`; then, under the batch's label, the spouse's share of the couple's self-employment
   income (only for a married couple with Schedules C or F or `part_se`) and the earnings split code (0 unless
   married; 1-3 with wages);
3. the exact ages of the primary filer and spouse, in random order, under the CPS label at the record's CPS year. The
   primary filer's age lies inside its age range; the spouse's exists exactly when married and is 18 or over; a
   non-filer head is 18 or over;
4. the SCF-only items in random order (the mortgage share after the home; the spouse's deferral share after the
   deferrals; with `generation.qbi_items_last` the QBI workers and industry after every other SCF item), under the SCF
   label at the income year of the record's SCF wave, PUF-only variables left out;
5. the 8 consumption categories in random order, under the CEX label at the record's CEX year;
6. the CPS person items, childcare, Social Security number status and state in random order, then the two adults'
   overtime, under the CPS label at the record's CPS year;
7. the SIPP items: the two adults' tips in random order, then the two leisure and hospitality flags, under the SIPP
   label at the record's SIPP year.

Each step runs the network only on the slots present in its phase and computes only the output layers it needs.

**Rules at generation.**

- The spouse's items are none or zero unless married; a married spouse has an age and a sex; the primary filer's sex
  and Social Security status are never none; the k-th dependent has an age and a status exactly when there are at
  least k dependents.
- The spouse's share of wages is drawn only in bands that reach the range the earnings split code allows; the exact
  share is then held inside the range when bands become dollars.
- A share is zero when the couple has none of the item. Where a share was absent on the training rows, generation
  leaves it absent until its phase is over, then sets it to zero.
- Deferrals are zero without wages; overtime and tips are zero for a person without wages (the spouse without a share
  of wages); a leisure and hospitality flag is zero without tips.
- **Dependent filers** (the PUF's three dependent-filer age ranges) hold no SCF, CEX, CPS or SIPP item: every
  balance-sheet item, gain, ownership flag, auto loan interest, deferral, consumption category, exact age, person
  item, overtime and tip is zero or none. The surveys count their wealth and spending in the claiming household.

**Consistency rules** (settings `consistency`). When a PUF item implies a holding, the holding cannot be drawn as zero:

| On the return | Must hold at least one of |
|---|---|
| rental income or loss | other residential real estate, nonresidential real estate |
| any partnership or S corporation item | a private business (it may be valued at zero), other residential or nonresidential real estate |
| mortgage interest paid | a home, and a mortgage on it |
| real estate tax paid | a home, other residential real estate |

Where several holdings would do, the one that must be nonzero is picked when the first of them comes up, in proportion
to the network's own chance that each is nonzero. Ruling out zero removes the chance of zero and rescales the rest;
the network still decides the size. With no home, the mortgage share is zero. No rule applies to a dependent filer.

The rules add holders and remove none, so generation runs three times (`offset_generations`). After each run but the
last, for each ruled holding and network, one factor on every record's odds of holding it is found so that forced
holders plus expected holders among the rest equal the network's own expected holders. The last run is kept;
`gen/<run>/consistency.csv` reports the counts and factors.

**Survey waves and years.** Each record, filers and non-filers, is given one SCF wave with the shares in the year's
`targets_config/<year>/prior.yaml` ("balance sheet"), and one CEX, CPS and SIPP year with the shares there
("consumption", "cps items", "sipp tips"), each by a random generator of its own (`generation.scf_wave_seed`,
`survey_year_seeds`), so no other draw moves. Shares by year: `TARGETS.md`. For 2019: SCF 2019 wave 54.65%, 2022
33.15%, 2016 12.20%; each other survey's 2019 year 73.11% and 2017 26.89%. The stack carries `scf_wave`, `scf_year`,
`cex_year`, `cps_year`, `sipp_year`. Generation refuses a year the training table lacks.

**Stored picks.** For each record and amount the stack stores one uniform number (`u_<variable>`) that picks the real
dollar amount within its band, so every step that turns bands into dollars gives a record the same amounts.

**Outputs** in `gen/y<year>/`: `stack.parquet`, `label_check.csv`, `consistency.csv`.

### The real PUF as the filers

Settings `generation.filers`: `synthetic` (the default) or `real_puf`. With `real_puf` the filer batch is every 2015
PUF record, the four aggregate records included: all its PUF slots and computed totals preset, each network drawing
everything else given the record, so each record appears once per network ("copy" 1-3) at its PUF weight over the
number of networks. Where the PUF has no age range or earnings split code, each copy draws one from the record's
probabilities (salt `generation.real_puf_earn_split_salt` for the code). The aggregate records' filing status,
dependents, age range and earnings split are drawn (adult ranges only); their blank alimony fields read as zero. A
real record's own 2015 amounts are carried as `own_<item>` and grown exactly as the dollar map is grown. Record ids are
50,000,000 + (copy - 1) x 1,000,000 + RECID. The non-filer batch and the Forbes list are as in a synthetic run.

Choosing `real_puf` also: turns off the rebuild of the aggregate records (they are in the file as real records); runs
the top step's redraw on the same GPU after the draw in a past year, into the top cache; adds the package stage after
the Tax-Simulator file; and requires a projection year's start file to be of the same kind (the reweighting stops
otherwise). A projection year writes its growth factors in full precision (`start_growth.csv`) for the package.

### The top of the file

`generate/top_hidden.py`, `top_billionaires.py`, `top_records.py`; settings `tilt.top` and `top_hidden`. The steps run
on the start in target-year dollars, before the prescaling and the targets. The part that needs the networks (the
rebuilt aggregate records and every redraw) runs on a GPU into the top cache (`REBUILD_TOP_CACHE`); the Forbes list is
added on every run.

**The PUF's four aggregate records, rebuilt.** The 2015 PUF holds 1,232.78 returns in four records of group means:
negative AGI (RECID 999996, 140.31 returns), $0 to $10 million (999997, 233.47), over $10 million to $100 million
(999998, 394) and over $100 million (999999, 465). They are rebuilt from generated records in 2015 dollars, then
grown to the year (`top_hidden.rebuild_hidden_returns`):

1. Candidates: an extra filer batch from each network, weighted to the 2015 PUF.
2. Selection, as SOI selected the aggregated returns: the top 400 by AGI, the 30 highest of each income item, the 30
   lowest of each loss, the 10 highest (and lowest, for two-signed fields) of most other amount fields, ranks in
   population weight. Capped, income-limited and derived fields are left out of the rule.
3. Groups by AGI: first by rank in the shares of the three positive groups' counts; after the dollars are scaled,
   each record must lie in its group by its final AGI (records moved or held inside through `other_agi_remainder`;
   a record still outside is dropped). Up to 750 records a group.
4. Starting weights: each group's population count from the raw record, divided equally.
5. Counts: SOI's booklet table for the aggregate records (2015 PUF booklet, pp. 32-36,
   `resources/top_hidden/soi_2015_aggregate_booklet.csv`) gives the returns with a positive and with a negative amount
   of each field; the weights are reweighted to it (standard error the larger of 2 returns and 2%), each group's count
   exact.
6. Dollars: each group's net amount of each part is the raw record's mean times its weight; positive and negative
   amounts are scaled by one factor each per group, chosen closest to one common factor, so every group's net and
   (where the booklet splits a field) positive total match. Totals are computed from the parts; taxable Social Security
   is the worksheet's, the gap to the raw E02500 moved into the residual, so each group's AGI equals the raw record's.
7. Wealth: every rebuilt return's whole SCF block is drawn again given its final income (20 tries; the first below
   $1 billion is kept, else the lowest, flagged `hidden_wealth_fallback`).
8. Into the file: PUF amounts grown by the year's growth lines, totals recomputed, other survey items the donor's own,
   flagged `synthetic_top` and `hidden_group`, ids from 90,000,000; weight in the generated file's units, free in the
   reweighting.

**Balance sheets at $1 billion or more are drawn again.** Above $1 billion the file is the Forbes list alone; below it
the SCF. Any generated record with net worth at or above $1 billion has its whole SCF block drawn again under the SCF
label given everything else on the record, as generation drew it (its network, wave and consistency factors): 20
tries per record from generators seeded by the record id, the first try below $1 billion kept, else the lowest,
flagged `wealth_redraw_fallback`; the file carries `wealth_redrawn`. The SCF targets are on the SCF's units below $1
billion (catalog population `scf_units_below_a_billion`), and the file's side uses the same ceiling.

**The Forbes lists** (`resources/top/forbes_billionaires_<year>.csv`, 2015-2026, built by
`resources_build/build_forbes.py` from Forbes's World's Billionaires data). Tax year t takes the March or April list of
year t: US citizens with $1 billion or more, 535 (2015) to 991 (2026), within two of Forbes's published counts.
Marital status, children and public holdings come from production's later profiles (Forbes's 2025 data), public
holdings as their share of 2025 net worth times the list's net worth. Spouses listed separately become one joint
return (1 to 9 couples a year, `SPOUSE_PAIRS` in the builder); ex-spouses and couples whose divorce was filed before the
list stay separate. A family fortune named in one entry stays one record. In a list year the records have weight 1,
held fixed in the solve.

**Each billionaire record** (`top_billionaires.build_billionaires`; flags `billionaire`, `billionaire_rank`,
`forbes_uri`; ids from 95,000,000):

- Rank groups: the top 92 units, the next 276 (the "top 100" and "next 300" of Balkir, Saez, Yagan and Zucman, as tax
  units), and the rest, which take the next group's figures.
- Wealth: Forbes's net worth; public holdings as above, else the rank group's wealth-weighted share; the rest private,
  as `value.re_fund` when the industry is real estate, otherwise `value.pass_throughs`. No home, no debt. Basis zero
  for self-made fortunes, equal to value for inherited ones.
- Income: each group's dollars of each kind = its wealth times the IRS study's amount per dollar of wealth (Balkir,
  Saez, Yagan and Zucman, Table 1; `resources/top/irs_wealth_groups_2018_2020.csv`). Capital gains and dividends are
  shared among the group's units by public stock; business profits and losses by private holdings; interest, rents,
  estate income, other income, wages and pensions by total wealth. Net operating losses are left out.
- Tax items: each kind split as in the rebuilt over-$100-million aggregate record (`hidden_group` 3), except
  partnership against S corporation income, from the IRS study. Schedule C and farm profits go only to that group's
  share of units holding them, by a draw keyed by record id. Items beyond the kinds at their ratio to AGI there;
  consumption at its average; Social Security, phased-out credits and the rest in `ZERO_ITEMS` zero.
- Charity: gifts of 0.7% of wealth in the top group and 0.6% in the rest (BSYZ Table 2).
- People: Forbes's age (held to 80) and sex; married from the profiles (missing: married); a spouse of the same age
  and the other sex unless listed; dependents by production's rule; all earnings to the primary filer.

**No other record at or above $1 billion in a list year** (`top_records.hold_below_ceiling`). A record not on the list
at or above the ceiling has its whole balance sheet (assets, debts, unrealized gains, cost bases, `w_dc_stocks`)
multiplied by one factor that puts its net worth at 0.999 times the ceiling; its income, weight and other columns are
unchanged (`held_below_ceiling.csv`).

**After the last list (2027 on).** The 2026 list is grown: every fortune by one factor, chosen so that the list's net
worth on its starting weights grows as Z.1 household net worth (settings `tilt.top.after_last_list`), so the list keeps
its 2026 share of the file's net worth (4.5% in 2026, 4.3% in 2075). Ages are raised; income is computed from the IRS
study's ratios as in a list year. Each record's weight starts at its demographic cell's population growth from 2026 and
is held fixed in the solve. Records grown past $1 billion stay, as the people who become billionaires after the list.

**Outputs** in the year's tilt folder: `billionaire_units.csv`, `billionaire_groups.csv`, `redraw.csv`,
`top_feasibility.csv`, `top_notes.md`; the rebuilt records in `gen/<run>/hidden/`.

---

## 4. Scaling to the target year

### Dollars in the target year

Every dollar amount is put in target-year dollars item by item before the reweighting, by its line in
`targets_config/<year>/growth.yaml` (`shared/growth.py`; series in `research/rebuild/resources/growth/` with
`sources.csv`). Each line gives the source series, the measure, how negative values are grown, and a reason; the
pipeline stops if an item has none. Sources must be exogenous to tax policy.

- **PUF items**: each band's real 2015 values, grown by SOI's growth in dollars per return holding the item where Table
  1.4 has it (qualified dividends; ordinary dividends for `div_ord`; each adjustment; Schedule D net gain for the gain
  pieces, losses included; partnership and S corporation income for `part_se`; total less taxable pensions for the
  non-taxable rest). Capital losses held at the allowed loss stay there after growth. Items with no Table 1.4 line:

| Item | Series | Measure | 2015 to 2019 |
|---|---|---|---:|
| mortgage interest | BEA NIPA Table 7.11 line 16 over ACS homes with a mortgage (B25081) | per mortgaged owner-occupied home | 1.089 |
| real estate tax | ACS aggregate real estate taxes (B25090) over owner homes paying any (B25102) | per owner home paying | 1.138 |
| state and local income or sales tax | Census state and local individual income tax revenue (QTAX), over SOI's returns | per return | 1.148 |
| medical expenses | MEPS out-of-pocket spending (TOTSLF) | per person with any | 1.356 |
| tax preparation fees | BLS CPI-U, tax return preparation and other accounting fees | a price | 1.168 |
| education credit expenses | College Board published tuition and fees, fixed sector weights, tax year = mean of two academic years | per full-time undergraduate | 1.126 |
| the other 19 (charity, casualty, job expenses, other miscellaneous, other itemized, two AMT items, the foreign tax credit, eight other credits, two other taxes) | SOI AGI over all returns (Table 1.1); past SOI's last year BEA personal income per capita; past BEA's last year CBO's | per return | 1.118 |

  ACS 2020 values are interpolated. Repealed deductions are not grown past their last published year; direct series
  past their end grow by personal income per person.
- **The balance sheet, auto loan interest, deferrals and inheritances**: each band's real values from the record's
  own SCF wave, grown per tax unit to the target year's wave item by item (the target wave's weighted mean per unit
  over the drawing wave's; `scale/maps.scf_growth`, `data/maps/scf_growth_factors.csv`; deferrals by the mean over the
  units asked). For 2019, the 2022 wave's stocks grow by 0.641, homes by 0.704, cash by 0.709; the 2016 wave's by
  1.058, 1.166 and 1.110. Only each wave's mean moves, not the shape of its distribution. At an income year with no
  wave, each item's mean per unit is interpolated geometrically between the waves either side; after the last wave it
  grows with household net worth per person (Z.1 S1M.b over the Social Security area population), at a fixed ratio to
  GDP after 2025. The mortgage moves with the home. Each person's deferrals are then held to the target year's legal
  limit for their age ($19,000, or $25,000 from 50, in 2019; limits to 2075 grow 2.4% a year, rounded down to $500)
  and to their own wages.
- **SCF other debt** is held to the 99.9th percentile among each wave's holders in its dollar map before growth
  (settings `dollars.scf_caps`): $560,000 in the 2013 wave, $900,000 in 2016, $500,000 in 2019, $2.0 million in 2022.
- **Consumption**: each band's real CEX values of the record's CEX year, grown by NIPA PCE per return, category by
  category.
- **CPS childcare and overtime, SIPP tips**: the survey year's values grown by SOI's wages per return with wages.

The dollar maps for a year are built from the training table without retraining (`scale/maps.py`, stage `maps`), so
the installed networks serve every year.

**Per-person columns** (`shared/dollars.py`). The file's `wages1` and `wages2` (and `se1`, `se2`) are the total split by
the drawn share; the primary filer's is the total less the spouse's, so they add up exactly (couples whose two
self-employment incomes have opposite signs have their share cut to 0-1). The wage share's range counts each person's
earnings only up to the room under the Social Security wage base (2015's $118,500 grown by the wage factor).
`tips1`, `tips2` are each person's drawn tips held to that person's wages; `ot1 = min(ot_primary, max(wages1 - tips1,
0))`, the same for the spouse; `elective1`, `elective2` after the holds; `trad_contr_er1`, `trad_contr_er2` each
person's traditional part (elective x (1 - the W-2 Roth share)); `trad_deferrals` their sum; `medicare_wages` wages
plus both. `tips`, `ot` and the other sums equal their parts exactly; `elective_deferrals` keeps the drawn total. `ssn1`,
`ssn2`, `dep_ssn1-3` are made from the drawn status (1 has a number, 0 none, missing where there is no such person).

### The start and its prescaling

The year's `targets.yaml` names the start (`scale/start.py`, called by the reweighting and the top step):

- `generate`: the year's generated stack, dollars from the year's maps, each record at its generation weight;
- `from_year: s`: year s's reweighted file, its weights in place of the generation weights, every dollar column grown
  item by item from s to the target year (projection years, below).

Weights are then scaled: filers to SOI's returns and non-filers to the CPS's expected non-filers; filers within joint
and non-joint returns to Table 1.2; then (settings `tilt.prescale` `demographic`) filers to SOI Table 1.6's returns by
the primary's age range in joint and non-joint returns, and non-filers to the non-filer people targets, each keeping
its total, by the same solver as the reweighting. These cells stay targets.

### Projection years

The last year of final SOI tables (2023) is a past year. 2024 starts from the 2023 file; every year 2025-2075 starts
from the finished 2024 file (the jump-off year, `scale/projection_steps.jump_off`), so the later years run in parallel.
A year's `targets.yaml` sets `start_scaling: {weights: persons, dollars: item_factor}`. Three steps
(`scale/projection_steps.py`):

1. **People.** The start's weights are calibrated (the smallest Kullback-Leibler move, every count exact) to people by
   marital status and single year of age, 0 to 79 and 80 and over: the primary, a joint return's spouse and every
   dependent, each carrying the unit's weight; a dependent filer's own record counts only its dependents; a married
   person under 18 counts as unmarried. Each count is the start file's count times the growth of the same people in
   Macro-Projections' population (settings `tilt.projection.population_dir`). The dependent filers' six cells (age
   range by marital status) are exact too.
2. **One factor per dollar item.** Each grown column has a row in `targets_config/projection_items.csv`: its series
   (`current`, a per-unit rule; `file:<csv>:<column>`; `same_as:<column>`; `none`), its kind (`total`: the factor makes
   the column's total on the step 1 weights equal the start's total times the series' growth, gains and losses each on
   their own; `per_unit`: the series multiplies each record), whether its total is targeted, and its error. Series are
   CBO's February 2026 projections and Macro-Projections; gross Social Security follows SSA's benefits paid for 2024-2025
   and CBO's Social Security outlays after. Taxable Social Security is not grown (the worksheet recomputes it).
3. **The reweighting.** Targets: the step 1 people (exact); returns in each group of the primary's age band (settings
   `tilt.projection.return_age_edges`) and marital status, and of the dependent filers' ranges, at the start file's
   filing share times the group's units after step 1 (exact); every targeted item's total, gains and losses
   separately where each has at least 500 records and $0.5 billion (error 1%); AMT payers at the start's count times
   CBO's growth in the count (5%; no row after 2035); household net worth by Z.1's growth; the SCF categories grown by
   step 2's factors (`reweight/scf_targets.projection_extension`, on the SCF's units grown with the file's units). No
   itemizer target. The Forbes records are outside steps 1 and 2; the year's list adds its own fixed contribution to
   every target. The number of returns is SOI's 2023 count grown with the population at each age and marital group's
   filing share.

**Add factors** (`targets_config/projection_add_factors.csv`, settings `tilt.projection.add_factors_table`). Extra
growth on top of an item's series, set by hand, one row per item and range of years (`column`, `first_year`,
`last_year`, `percent` a year, `note`). The item's growth to year t is its series' growth times the product of (1 +
percent/100) over the covered years after the start; +2% a year from 2024 makes the 2030 growth the series' times
1.02^7 = 1.149. It enters step 2, the step 3 total target and, for a balance sheet item, the SCF targets. Rules,
checked on reading: every column is in the items table; years are after 2023; no item has a year in two rows; every
non-zero row has a note saying who set it, when and why. An empty table gives every item a factor of exactly 1. In
force: Schedule A +2% a year 2024-2028 and +1% 2029-2033, a step toward CBO's itemized deductions, to be revisited at
each CBO update. Each year's `summary.md` lists the add factors in force.

**Past 2035** (settings `tilt.projection.cbo_last_year`). Each series continues from its 2035 value at the growth of one
Macro-Projections aggregate (`resources/projection_long_run_mapping.csv`, production's long-run mapping): wages with
wages, interest with personal interest income, dividends and gains with corporate profits, pass-through income with
proprietors' income plus rents plus corporate profits, rents with rental income, pensions and Social Security with
Social Security outlays, consumption and consumer debt with consumption, nominal GDP otherwise and for the balance
sheet. Per-unit series: per return over the growth of adults with a married couple counted once, per person over
population, a price with CPI-U (medical expenses keep their 2035 price relative to all prices). Alimony keeps its
yearly decline. `resources_build/build_long_run.py` writes the extended series. `targets_config/2036` to `2075` repeat
2035's targets; the survey mixes stay fixed after the last wave.

**The first projection year from SOI's November filing-season tables** (settings `tilt.projection.filing_season`).
2024 takes part of its growth from SOI's table of returns processed through November (tax year 2024 against 2023):

- `resources_build/build_filing_season.py` (stage `filing_season`, run when SOI posts a table) reads every November
  table (tax years 2015-2018 and 2021-2024) into `resources/filing_season/november_tables.csv`, on the standard AGI grid
  with $1 million and over at the top; `line_mapping.csv` says which line is which item.
- Trust levels (`targets_config/filing_season_trust.csv`): full (1: taxable interest, ordinary and qualified dividends,
  taxable IRA distributions, taxable pensions, the number of returns), mostly (2/3: Schedule E), little (1/3: wages,
  net capital gain or loss), none (0: Schedule C, Schedule F, unemployment).
- An item's growth in AGI group g is w x (the November tables' growth in g) + (1 - w) x (its national series' growth).
- Step 2 gives each column one more national factor so its total grows at the blended national rate; the factor stays
  in every later year. Non-qualified dividends get the factor that makes ordinary dividends grow at their blend; the
  gain pieces by the ratio of the blended Form 1040 net gain or loss to CBO's capital gains line; Schedule E's parts by
  one common factor.
- Step 3 in 2024 replaces each trusted item's national total with its blended total in each 2024 AGI group on filers
  (for full-trust items also the number of returns with the item; the number of returns as shares across groups). Each
  value is the 2023 file's own group total (the Forbes list left out) times the blended growth, plus the year's list.
  Groups: the standard grid with no AGI and $1-10,000 merged and $1 million and over as the top; for Schedule E every
  group below $75,000 is one. Errors: the backtest's root mean square miss for the item and group
  (`targets_config/filing_season_errors.csv`), at least 1%. 149 group targets in 2024.
- The blend is applied only to the year after the last final SOI year. When SOI publishes final tables for 2024, 2024
  becomes a past year, `base_year` and `year` move to 2024 and 2025, and 2025 becomes the jump-off year.

**Taxable interest after the jump-off year** (settings `tilt.projection.interest_growth`). From 2025, taxable interest
grows from the start year by nominal GDP times the 3-month Treasury bill rate (series `int_gdp_bill` in
`projection_series_b.csv`: CBO's February 2026 economic projections to 2035, then Macro-Projections). Non-qualified
dividends are taken as two parts of the 2024 file: a stock-like part (SOI's 2022 non-qualified dividends grown by
qualified dividends' growth to the 2024 file's filers), which follows CBO's qualified dividend line, and a rate-like
part (the rest), which grows as taxable interest; the column grows by the share-weighted blend (stock-like share 0.489
on the installed 2024 file), computed each run and written to `summary.md` and `projection_interest.csv`. Tax-exempt
interest keeps the `int_remainder` series (CBO's interest and non-qualified dividend line less non-qualified
dividends).

**Other projection rules.** A start from an earlier year's file drops and remakes the non-microdata imputations, and
the earlier year's Forbes records are removed and the year's list added. The SCF targets of 2025 on take the 2024
run's growth from 2023 (`scf_targets.jump_off_chain`).

### The non-microdata imputations

Items set from what the networks drew plus a published table, with no new microdata (`scale/non_micro_imputations.py`,
one function per item; settings `non_micro_imputations` registers each item's inputs, tables, outputs, random stream
and reasons). Tables are in `research/rebuild/resources/non_micro/` (with `sources.csv`), built by
`resources_build/build_non_micro_tables.py` (stage `non_micro_tables`, run when a source changes).

They run in the reweighting stage on the start, after the dollars are in target-year dollars and the weights are
prescaled, before the taxes and the solve. They add columns only, with two exceptions: a primary filer with an age and
no sex is given one (even chances, keyed draw); and the AMT rule changes `amt_nols`, keeping the drawn value as
`amt_nols_drawn`. The two ranking items rank on the prescaled start's weights. No target reads these columns except
through the calculator. They can be run on a finished file for checks (`python -m scale.non_micro_imputations --run
<tilt folder> --out <folder>`), which reproduces its columns exactly.

**Random numbers.** Each is a fixed function of the record id, the item's stream and the person
(`shared/ages.uniform_by_id`, salt = `salt_base` + 8 x stream + person; person 0 the unit, 1-2 the adults, 3-5 the
dependents). Streams are never renumbered. **Years.** Each item reads its table's row for the target year, or the last year before it; `summary.md`
says which.

| Item | Columns | Method | Published source |
|---|---|---|---|
| Dependent filers' age and sex | `dep_filer_age`, `dep_filer_sex` | drawn from the CPS dependents the filing stage counts as filing their own return, in the record's CPS year, within its PUF dependent-filer range; 80 means 80 and over | the filing stage's persons (`data/filing/returns/persons_<year>.parquet`) |
| Child tax credit eligibility | `dep_ctc1-3` | production's rule (`src/imputations/ages.R`): dependent k passes every test but age and SSN when under 17 and `n_dep_ctc` is at least k; 17-year-olds pass with production's chance; dependents from 17 to the year's age limit less one by chance | the credit's age limit by year (IRC 24(c); under 18 in 2021) |
| Death probabilities | `q_death1`, `q_death2` | production's three layers (`src/mortality_ledger.R`), no draw: SSA life table x Chetty income ratio (percentile within sex and age, start weights) x Johnson marital factor for men; billionaires at percentile 100 | SSA period life tables (2024 Trustees Report); Chetty et al. (2016) Table 15; Johnson et al. (2000) |
| Yearly unrealized gains | `accruals.equities`, `.pass_throughs`, `.primary_home`, `.other_home`, `.re_fund`, `.dc`, `.trusts` | value x the year's Z.1 revaluation rate (households and nonprofits): stocks at corporate equities', businesses at noncorporate equity's, real estate at real estate's, trusts at 60% stocks and 40% bonds, retirement accounts at the drawn stock share; years without a full year of Z.1 take production's long-run rates | Z.1 table S1M.i.a, 1990-2025 |
| Cost basis | `basis.equities`, `.pass_throughs`, `.primary_home`, `.other_home`, `.re_fund` | value less the SCF's unrealized gain (other real estate's gain split by value), the gain first held to the value | |
| Holding period and basis of long-term gains | `kg_lt_years_held`, `kg_lt_basis` | production's method (`src/imputations/capital_gains.R`): holding group drawn with SOCA's gain (or loss) shares, period from a Weibull shape (exponential above 20 years), held to at most the older adult's age less 18 and at least 1 year; basis from production's SOCA regression at the target year's trailing S&P 500 returns | SOCA panel and shares; S&P 500 total return index |
| Parents' income rank | `parent_rank` | production's method (`src/imputations/mobility.R`): income rank among all units (start weights), bottom 10 merged into rank 10, parent rank drawn from Chetty et al.'s matrix | Chetty, Hendren, Kline and Saez (2014) |
| Blindness | `blind1`, `blind2` | each primary filer, and each spouse on a joint return, blind with chance = the 2015 PUF's blind rate for the person's age range x one factor per person type, set so blind filers at the start's weights equal Publication 4801's boxes for the year (stream 6) | `puf_blind_rates.csv` (about 0.10% under 26 to 0.86% at 65+, from the 62.5 million returns where the return shows it); `pub4801_blind_boxes.csv` (2019: 260,291 primary, 80,564 spouse) |
| AMT net operating loss | `amt_nols` (changed), `amt_nols_drawn` | `amt_nols` (P60100) held to at most the record's net operating loss, the negative part of `other_agi_remainder`, so 0 with no loss; a negative value left as drawn; grown by AGI per return | |
| QBI service flags and wage bills | `sstb_*`, `wagebill_*` (sole proprietorship, partnership, S corporation, farm) | below | SOI wages by type of business (`irs_wages_by_business_type.csv`); the law's service list by SCF industry group (settings) |

**QBI wages.** A group's wages on a filing record = its drawn worker count x the group's scalar for the year; non-filers
have none. The scalar is set once a year on the start's prescaled weights: the IRS total over the sum, over filers with
positive income of the type, of weight x count. `cf` wages all go to `wagebill_sole_prop` (on positive Schedule C
income), against SOI's Schedule C payroll; `ps` wages are split by each type's positive income into `wagebill_part`
and `wagebill_scorp`. `wagebill_farm` = positive farm income. IRS totals for 2019: Schedule C payroll $144.0 billion;
partnership salaries and wages $712.1 billion (without guaranteed payments); S corporation salaries and wages $971.9
billion plus officers' compensation $293.7 billion. Past 2023 the scalar is the 2023 scalar times SOI's wages per return
growth (extended by CBO and Macro-Projections); the 2023 scalars travel in `qbi_scalars.csv` beside each year's file and
the run stops if they are missing. No target holds the total through the reweighting. **Service flag**: one keyed
draw per group against the share of self-employed people in the SCF industry group whose detailed industry is on the
law's list (settings `c_by_scf_group`: group 1 0.019, 3 0.414, 5 0.102, 6 0.291, others 0; "no industry code" the
income-weighted average); one flag for a record's partnership and S corporation together; farms never.

**Conventions.** -1 for an age or a person that does not exist, 0 for no sex; flags 1 or 0, -1 where the dependent does
not exist; `q_death2` is -1 without a spouse; `kg_lt_years_held` -1 and `kg_lt_basis` 0 with no long-term gain or loss.
Two departures from production: parents' rank for the bottom 9% of incomes uses production's stated intent (rank 10)
where its code gives 100; in historical years the bond part of retirement accounts and trusts takes the year's actual
Z.1 debt-securities revaluation, where production's long-run rate is 0.

**Checks** (reported, never a stop), on the final weights: `non_micro_checks.md` and `non_micro_check_*.csv` in the
tilt folder, section 6 of `report.md`. They compare dependent filers with SOI Table 1.7, credit-eligible children with
Census population under 17, expected deaths with SSA's life table times Census population, yearly gains with Z.1's
holding gains, holding-period shares with SOCA, blind filers with Publication 4801, and parents' rank with the matrix.
The SCF's businesses include S and C corporations the household runs, which Z.1's noncorporate equity does not.

---

## 5. Reweighting

`reweight/tilt.py`, with `target_list.py`, `scf_targets.py`, `errors.py`, `taxes.py`, `precheck.py` and `solver.py`;
settings `tilt`, `taxes`, `errors`. In order: the start and the top of the file (sections 3-4), the non-microdata
imputations, the taxes, the targets, the solve, the file. `output/report_tilt.py` writes the run report.

### Taxes from the calculator

Every record's federal income and payroll taxes under the target year's law come from tax-simulator-gpu
(`reweight/taxes.py`, settings `taxes`):

1. The file becomes the calculator's input frame (`shared/calculator_input.convert`, `CALCULATOR_CONTRACT.md`).
2. tax-simulator-gpu's batch command (`python -m taxmodel.run.batch`) runs it on the CPU in the calculator's own Python
   environment (`taxes.python`), from a copy of that repository at the pinned commit (`taxes.commit`, written once under
   `<out>/lib/`). About 20 seconds for 457,000 records.
3. The output columns join the file by record id, each the calculator's name with `calc_` in front: AGI, itemizing and
   itemized deductions claimed, the standard deduction, the QBI deduction, taxable income, income tax before credits,
   the AMT, the child credit's two parts, the EITC, nonrefundable and refundable credits, income tax after credits, the
   net investment income tax, total income tax, income tax net of refundable credits, self-employment and payroll tax,
   and the marginal rates of income and payroll tax on the primary earner's wages ($100 more, `wage_increment`).

The reweighting moves weights only, so the taxes stay right after it. With `taxes.target` false the calculator targets
are only reported. Each run writes `tilt/<run>/taxes/` (inputs, outputs, `taxes_fit.csv`, `taxes_summary.md`).

Rules applied with the calculator's help before the targets, each recomputing every total built from the item:

- **Repealed adjustments** (`taxes.repealed_adjustments`): the domestic production activities deduction from 2018 and
  the tuition and fees deduction from 2021 are set to zero.
- **The deductible half of self-employment tax** is taken from the calculator (`taxes.se_tax_ded_from_calculator`), so
  the file's adjustments equal the calculator's.
- **The refund rule** (`taxes.refund_rule`): taxable state refunds are zero on records the calculator says do not
  itemize. From 2019 (`refund_tax_benefit_from`; a 2018 refund repays uncapped 2017 taxes) the tax benefit rule
  (Publication 525) also holds an itemizer's refund to the benefit the deduction gave: the taxes deduction claimed less
  the one it would have claimed had it paid the refund less tax, and at most the itemized deductions less the standard
  deduction. A record keeps its whole refund with chance 0.15 (`refund_reported_anyway`, keyed by record id), because
  SOI shows refunds on about 5% of returns at AGI $2-5 million.
- **The Schedule A step** (`taxes.greedy_schedule_a`): four Table 2.1 categories (medical; taxes, from salt income or
  sales and property; interest; charity, cash and noncash) are each multiplied by one factor so that the start's
  dollars per claiming return match Table 2.1's, in 3 passes through the calculator, each factor within 0.75-1.25 of the
  declared growth. A year with no Table 2.1 keeps the declared growth. The six multipliers are reported in
  `summary.md`.

### Targets

Each year's targets are written out in full in `targets_config/<year>/targets.yaml`, with definitions in the shared
`targets_config/catalog.yaml`; `reweight/target_list.py` checks the folder before every run and expands it into one row
per target (`tilt/<run>/targets.csv`). Each target has a standard error (`reweight/errors.py`): SOI's published
coefficients of variation for Tables 1.4, 2.1 and 3.3; errors borrowed from neighbouring cells for Tables 1.2, 1.3,
1.6, 1.7 and 4.3; replicate weights for the SCF (999, with five implicates), CEX (44) and CPS (160, successive
difference replication); a judgment number with a reason otherwise. `TARGETS.md` describes the configuration, the
errors and the principles.

A past year has 1,231-1,343 targets (2019: 1,325); a projection year about 305 (2024: 427, with the filing-season
groups). The families in a past year, with 2019's counts:

| Kind | Targets (2019) | What |
|---|---:|---|
| IRS | 222 | SOI Table 1.4 by AGI bracket (the five top brackets, $1M-1.5M to $10M+, separate): returns and the count and amount of wages, interest, dividends and capital gains; the other income items on a broad grid; Table 1.2 joint returns; state refunds, other Table 1.4 lines, alimony |
| business | 128 | net income and loss, returns and dollars: Schedule C and partnership and S corporation in 10 AGI groups, rent and royalty, farm and farm rental, estates and trusts, Form 4797, in 6; cells SOI combined are merged; 2021 on, partnerships and S corporations separately |
| soi 1.3 | 86 | Table 1.3: returns and AGI in each filing status, income items for single, joint and head of household returns; capital loss measured as the loss allowed on the return |
| soi 4.3 | 72 | Table 4.3: returns, AGI and 8 income items in the top 0.001%, 0.01%, 0.1% and 1% of AGI, returns other than dependent filers' |
| age, age by AGI, age by status, status by AGI | 10, 75, 12, 45 | Table 1.6 returns by the primary's age range, joint and non-joint; by age in each of the 15 standard AGI groups; heads of household and separate filers by age; Table 1.2 single, separate and head of household returns by AGI |
| dependent filers | 23 | Table 1.7: returns by AGI, and their gains, interest, dividends and other items |
| non-filers, non-filer people, non-filer items | 1, 8, 2 | the non-filer count (identity); non-filer people by own age band and children under 17; non-filers' dividends against the tax data's non-filers |
| tips | 11 | W-2 box 7 (Tables 4.B and 4.C): returns with tips and the amount, joint and non-joint; tipped workers on joint returns; the share of tipped workers above each of six percentile lines |
| deferrals | 24 | W-2 Table 2.D in eight AGI groups: contributions on joint returns; returns and contributions on non-joint returns |
| calculator | 296 | the calculator's columns against Tables 1.4, 2.1 and 3.3 by AGI: itemizers and itemized deductions by category, taxable income, tax before and after credits, the child credit's two parts, the EITC (by children 2015-2020), the AMT, the QBI deduction's dollars, education credits, self-employment tax, the saver's credit |
| wealth, wealth grid | 162, 22 | the SCF over the year's wave mix (below $1 billion): shares of units and net worth above the p99.9, p99, p90 and p50 lines and $1 million; four asset types above the top lines; holders and dollars of the nine balance sheet categories by net worth class and in four age groups of the primary (under 55, 55-64, 65-79, 80+); unrealized gains on four assets by age; the income by net worth grid |
| auto loans | 8 | SCF: share of units with any and dollars per unit in 4 income groups |
| consumption | 104 | CEX over the year's survey-year mix: 8 categories' averages per unit, all units and 12 age by married cells |
| overtime | 10 | CPS over the year's mix: share of units with any and overtime per unit, all units and 4 groups of the unit's wages |
| older ages | 4 | CPS units whose primary is 65-69, 70-74, 75-79 and 80+ |

**Measurement rules for particular families.**

- *Table 4.3* groups are the returns other than dependent filers' with AGI at or above SOI's published cutoff for each
  group and year ("adjusted gross income floor on percentiles"), fixed through the solve. Their counts borrow errors
  from Table 1.4's count cells at or above the cutoff, or 1% where the cutoff is $10 million or more.
- *Tips* are measured as box 7 holds them: each person's tips at most the Social Security wage base less that person's
  other wages. The W-2 counts include dependent filers, who hold no tips in the file, so non-joint values are reduced by
  the SIPP's share of dependents among non-joint tipped people (10.1% of people, 2.9% of tips), half the reduction
  added to the error. A worker is a primary filer or spouse with box 7 tips above zero. The share targets carry half the
  most removing dependent filers could move them. Non-filers' tips are not targeted. No W-2 targets before 2019.
- *Deferrals* are `elective1 + elective2` over filers other than dependent filers; on joint returns only dollars
  (Table 2.D counts people). Non-joint values are reduced as for tips (SIPP 2021: 0.5% of people, 0.1% of dollars).
- *Overtime and consumption* are averages over the survey's own units (filers and non-filers, dependent filers left
  out; for overtime only units asked the earnings questions), each year's values grown to the target year and pooled
  by the year's shares; errors combine each year's as independent samples. The unit's wages are the CPS's after
  function 1.
- *SCF targets* (`reweight/scf_targets.py`) use the same waves and shares as the file's balance sheet. Each wave is grown
  per tax unit to the target year's wave, its weights scaled to the target wave's units; each keeps its own income ranks
  and net worth lines, and its value and variance in each cell come from its own replicate weights and implicates.
  Value = sum of share x the wave's value; variance = sum of share^2 x the wave's variance. An age group or the
  below-$1-billion domain is a domain of each wave's full sample. `scf_targets_by_wave.csv` has every target by wave.
  The SCF replicate weights sit in `<out>/data/scf_rw/<wave>/`.
- *The income by net worth grid*: four income groups by the seven income items the SCF and the file share (below p90,
  p90-99, p99-99.9, p99.9+, each side by its own percentiles, the file's lines set on its starting weights and then
  fixed), four net worth classes between the SCF's lines at p90, p99 and p99.9; in each cell the share of the group's
  units in the class and the class's net worth. A cell resting on fewer than 10 households of the main wave is merged
  into the next richer class.
- *Auto loan interest* is per unit, because the SCF's units are fewer than the file's.
- *Older ages* come from the CPS's tax units over the year's CPS mix (160 replicate weights); the primary's age is
  `age1` on the file (80 and over held at 80).
- *Unrealized gains by age* hold each gain to its asset's value and at zero from below, as tax-simulator-gpu does.

### The solve

`reweight/solver.py`. The weights minimize one cost:

    sum over targets of 0.5 x importance x (miss / standard error)^2  +  tau x KL(w, w0)
      +  sum over records of 0.5 x beta x ((w - U) / U)^2 above U

- KL is the Kullback-Leibler move of the weights from the start, in nats, on weights divided by the sum of the
  starting weights. tau is 1,000 (settings `tilt.reweighting.tau`).
- Importance is 1 for the kinds in `importance_one_kinds` (IRS, business, Tables 1.3 and 4.3, the age and status
  kinds, dependent filers, tips, deferrals, calculator) and 1/1.5 for every other kind.
- The soft cap U is 10 times the mean starting weight of the record's AGI group (calculator AGI, `soft_cap.k`), beta 1.
- Identities (such as the non-filer count) are met exactly. No target has a range and none is dropped.
- The standard error of an average is on the average itself, times the group's weight after the reweighting; the solve
  is repeated once at the first answer's group weights if any moved more than 0.1%.
- Newton's method on the multipliers, at most 150 steps in all (`newton_budget`), stopping when the largest gradient is
  below 1e-8. A slow solve is a reason to check the targets, not to raise the budget.
- Records held fixed (the Forbes list) enter as fixed amounts; `solver.fixed_feasibility` checks before the solve what
  the others must reach (`top_feasibility.csv`).

**Precheck** (`reweight/precheck.py`, settings `tilt.precheck`) flags targets the records cannot reach before the
solve; it changes nothing. **Thin cells**: for each target, after the solve, the effective number of records behind
it and the largest weight factor among records holding at least 1% of the cell; a warning below 30 records or above a
factor of 10, STOP in the summary below 10 or above 100 (the run continues).

**Outputs** in `tilt/y<year>/`: `file_<year>.parquet` (every record, every dollar column in nominal target-year
dollars, `weight_start` and `weight`), `fit.csv` (every target before and after, miss in standard errors, thin-cell
measures), `targets.csv`, `resolved_<year>.yaml`, `summary.md` (convergence, Kullback-Leibler move, effective sample
kept, weight factors by AGI group, notes), `report.md` and `report_*.csv`, the non-microdata checks, the taxes folder,
and the top step's files. With `REBUILD_SAVE_SOLVE` set, the solve's inputs are saved for the state solve.

### State weights

A file for each year 2015-2075 splitting every record's national weight across the 50 states, DC, the IRS's Other
Areas, and Puerto Rico from 2018: `files/state_weights_<year>.parquet` (id, state, weight; Zstandard; about 190 MB and
24 million rows a year). A record's national weight is the sum of its weights across areas. Settings `state_weights`.

**Past years 2015-2023:** a national-only solve of the year (whose amounts set the state targets), then the
integrated solve of national and state weights together. **Future years 2024-2075:** the national solve alone, each
record keeping its 2023 split.

- **The start** (`reweight/state_start.py`, GPU, about a minute a year). Each record is read under the CPS label with
  the target year's CPS mix, averaged over the three networks: shown are what the record has that the CPS asks, its
  PUF-only items and its AGI group label; hidden are state, the reported slots, every slot that is zero on all CPS
  rows, and on filers other income and rental loss (other income held by 58.7% of filers but 2.7% of CPS units, 44% in
  Alaska). A dependent filer's age range and person items are hidden. Other Areas and Puerto Rico start at the IRS's
  share of returns in the record's AGI group, the 51 scaled to the rest. Non-filers have only the 51.
- **Forbes records** are pinned to `residence_state` by `forbes_uri` (the nearest list that gives one; a residence
  abroad goes to Other Areas; otherwise as any record in a past year, and the year's pinned records' distribution in a
  projection year). Pinned records enter the solve as fixed amounts.
- **Filer targets** (`reweight/state_targets.py`). For area s, AGI group g (Historic Table 2's ten, on `calc_agi`) and
  item v: the file's national total of v in g times the IRS's share of s in g and v. Schedule A items count on records
  the calculator itemizes. A cell has no target where the IRS merged it (zero in a group whose national total is not),
  where the areas' values cancel or the file and the IRS have opposite signs, where the list records hold 90% or more
  of it, or where fewer than 250 effective records stand behind the item in the group (about 5 for each of the 53
  areas). The list is `FINAL_ITEMS` (49-52 items a year, counts and amounts): returns, single, joint and head of
  household returns, people, AGI, wages, interest, dividends, returns with capital gains, state and local taxes, real
  estate taxes, mortgage interest, the EITC; business income, pensions, Social Security, federal income tax; IRA
  distributions, pensions and annuities, unemployment, qualified dividends, partnership and S corporation returns,
  taxable income, itemizers, charity, self-employment tax, state refunds, the QBI deduction, the net investment income
  tax, the refundable child credit and tax-exempt interest, each where the year's table has the line. Three dollar
  amounts are not state targets (`NOT_STATE_TARGETS`): capital gains, partnership and S corporation income, and
  itemized deductions; their counts are.
- **Non-filer targets** (`state_targets.nonfiler_state_table`). A state's share of non-filer adults is its share of:
  CPS adults in tax units (heads and spouses of the training table's CPS rows, pooled over the five CPS years with
  equal weights, times the year's CPS total), less IRS adults on returns (N1 + MARS2), plus dependent returns (SOI Table
  1.7's national count, `resources/soi_table_1_7_dependent_returns.csv`, spread by CPS people aged 15-24). A residual at
  or below zero stops the run. Example: 2019, Colorado: 1.793% x 230.81 million = 4,138,588 CPS adults, less 3,910,950,
  plus 169,230 = 396,868, 1.364% of the national 29.09 million. A state by age band table, raked to these residuals and
  to the file's non-filer adults by band, gives each target.
- **Standard errors.** Historic Table 2 has no sampling error, so each state target's error is `tolerance` (2%) times
  the larger of the target and 0.1% of the item's national total in the group, plus the rounding of the IRS's counts
  to the nearest 10 for a count. Each is divided by the square root of the AGI group's share of the weight.
- **The integrated solve** (`reweight/state_joint.py`, 16 CPUs, about 10 seconds a Newton step). Record-by-area
  weights w_is = w0_i x p0_is x exp(x_i' lambda_national + x_i' lambda_s), with the solver's cost, tau and soft cap,
  the year's national targets and the state targets. Two state multipliers interact only within one area and AGI
  group, so the Newton system reduces to one dense matrix the size of the national targets (about 1,300 rows), about 580
  small blocks, and a term for records above their cap. Budget 150 Newton steps.
- **Future years** (`reweight/state_weights.py --carry`): a record's state weight is the year's national weight times
  its 2023 share, by record id (`shares/shares_2023.npz`). Any record other than a Forbes record that lacks a 2023 share
  stops the stage.
- **A year that stops short**: one that uses all 150 steps with its largest gradient within 10 times the stopping size
  is used and named in the log and the check; beyond that its files are written, its job fails and the check stops.
- **Check** (`state_weights.py --check`): every year's file and state weights against each other, into
  `checks/by_year.csv`.

---

## 6. Output

**The year file** (`tilt/y<year>/file_<year>.parquet`): every record with its weight, every drawn and computed
column, the non-microdata columns and the `calc_*` columns; for past years after the state solve, the file in
`final/<year>/tilt/y<year>/`.

**The Tax-Simulator file** (`output/taxsim_file.py`, stage `taxsim_file`): the year file as Tax-Simulator's
`tax_units_<year>.csv` (and `.parquet`), production's 188 columns in its order with its conventions for missing values,
in `taxsim/y<year>/`. Where each column comes from is `TAXSIM_FILE_COLUMNS.csv`, written beside the output and copied to
`research/rebuild/`. The record id is written as `id`; Tax-Simulator's `kg_lt` includes distributions.

**The package for a PUF holder** (`output/package.py`, stage `package`, only with `generation.filers` `real_puf`):
per year, in `<out>/package/<run>/` (permissions 700), the files that turn a holder's own 2015 PUF into the year's
Tax-Simulator file, and the round trip that checks both merge scripts reproduce our file. `PACKAGE.md` describes it.

**Reports.** `output/report_tilt.py` writes each year's `report.md` and `report_*.csv` (every target, thin cells,
published numbers not targeted, untargeted statistics before and after, each network's values, the commit and inputs).
The report stage (`output/report_cex.py`, `report_cps.py`, `report_sipp.py`, `report_translation.py`) compares the file's
consumption, CPS person items and overtime, and tips with the target year's survey mix, and writes the translation
report.

---

## 7. Time and memory

| Step | Hardware | Time |
|---|---|---|
| survey preparation (scf, cex, filing, cps, sipp) | CPU | 1-4 minutes each |
| SOI tables, errors | CPU | under a minute |
| training table, side files | CPU | about 5 minutes each |
| training, per network | GPU (H200) | about 62 minutes |
| a past year's folder, targets and dollar maps | CPU | about 3 minutes |
| draw and top step | GPU, 120 GB | 45-50 minutes (draw 17, top 31) |
| national solve, past year | 4 CPUs, 72 GB | 11-20 minutes |
| state chances | GPU | about a minute |
| integrated state solve | 16 CPUs, 96 GB | 15-42 minutes |
| projection year (solve and carried split) | 4 CPUs | 3-4 minutes |
| Tax-Simulator file | 2 CPUs | about a minute |
| package and round trip | CPU | about 3 minutes |

The GPU is used only to train, draw, run the top step and read state chances. Training and generation run on H100 or
H200 GPUs; a stack drawn on a B200 once had about 11% fewer rare business holders, untested since. Every year
2015-2075 takes about 3 GPU hours of training plus about 4 hours of wall time with Slurm's parallelism; one step
after another on one machine with one GPU it would be roughly 20 hours.

- **Training.** Packing (each batch one source's slots) halves the time and memory of a pass; bfloat16 makes a pass
  about 37% faster than 32-bit. Empty slots cost more than in proportion, and attention cost grows with the square of
  the number of slots, so packing matters more as surveys are added.
- **Generation** runs the network only on each phase's slots and only the needed output layers: about 9 seconds and
  11 GB per 50,000 records in 32-bit.
- **A memory limit.** PyTorch's masked attention fails ("illegal memory access") once records x heads x slots x slots
  passes about 2.1 billion in one step, so every forward pass without gradients is chunked to stay under 1 billion
  (`pretrain/model.safe_chunk`).

---

## 8. Known limitations

**Data and translation.**
- The fill-in adds holders a survey misses but cannot remove holders it has too many of (SCF rental income ends above
  the tax data); its PUF side reads the 2015 PUF, not the tax data of each survey year.
- The CEX: BLS stopped marking dependent filers in 2024; education spending is nearly empty in 2021 and 2022; food at
  home jumps 29% in 2024 under a new item name. It asks wages before deductions and replaces wages above about
  $210,000 a person with one value; it misses many small self-employment holders and most losses.
- The CPS has more units than the tax data above every wage line from $25,000 (2019: 29.0 against 21.9 million above
  $100,000) and fewer below $10,000; its zeros hide holders of dividends, pensions, unemployment and Schedule C.
- Consumption is not scaled to NIPA PCE (CEX spending is about 46% of PCE in 2019).
- Growth series: MEPS's redesign from 2018 may break its series; the College Board weights are fall 2019 enrollment;
  the CPI for tax preparation is the mean of the months BLS published.
- Inheritances: the SCF has almost no heirs above $18 million (estate data imply about $90-100 billion more a year);
  the Tax-Simulator file does not yet carry the item.

**The draw.**
- The gain as a share of an asset's value is too flat by age (on the SCF's own units, a share of 0.31 at 80 and over
  against the SCF's 0.43); the age targets make the reweighting meet the SCF's gains by age at a cost in effective
  sample.
- The network puts business wealth on records the SCF hardly has (net worth above the SCF's p99 with income below its
  p90).
- Too few units aged 80 and over; the CPS's 65+ count conflicts with SOI's Table 1.6 (Table 1.6's 65+ returns end 1.1-2.1
  standard errors above SOI).
- Too many units hold both partnership and S corporation income.
- `part_se` grows by the partnership and S corporation lines on every return; on joint returns with tax but E30400 of
  zero the spouse's share is drawn, which overcharges about $1 billion in 2019.

**The reweighting and projection.**
- The self-employment tax target is met by moving weight to records with large partnership income subject to
  self-employment tax.
- The 2020-2021 files hold about 1,500 records above weight 5,000; 2020 and 2021 keep the least effective sample.
- The child credit moves in steps of up to 2.5% a year past 2035.
- CBO's population has no married 16-year-olds in 2070-2072, so step 1 cannot meet that cell in those years.
- Records grown past $1 billion after the last Forbes list are under no net worth target (Z.1's covers units below $1
  billion), so their wealth grows with their items alone.
- Interest: the file matches SOI's 2023 taxable interest ($314 billion) while CBO's 2023 line is $287 billion.
- The file's AGI stays above the calculator's by the calculator's pass-through entity tax election ($27.1 billion in
  2019); whether that election subtracts taxes SOI's Schedule E is already net of is unchecked.
- Schedule C has no trust in the filing-season blend until its 2024 fall is understood.

**The real-PUF files.** In every real-PUF year the solve sets at least 9 of the 12 copies of the four aggregate records
to a weight near zero, and the top 0.001%'s AGI misses by up to 18 standard errors.
