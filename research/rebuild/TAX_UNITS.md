# Tax units in the surveys: plan

Written 23 October 2026. It covers how every survey (the CPS, SCF and CEX,
and later others) is turned into tax units before training, and how we test
that it was done well. The shared module of section 4 was built on 24 October
(section 4, "What was built") and is now the only way the pipeline builds
units; the latest run was trained before it, on the old units. Checking the
training table and the tilt on the new units is the next step, after John has
seen the tests.

## 1. Why it matters

The model learns how survey items relate to tax items through what the two
sides share: marital status and filing status, the number and ages of
dependents, the primary filer's age range, wages. A survey record only means
the same thing as a return if its tax unit is built the way returns are. If
the survey's units differ (a parent claimed in one source and a separate
unit in the other, a head of household counted as single), every
relationship learned through those shared items is off, and nothing
downstream can tell.

There are two separate steps, and they should stay separate:

1. **Building the units.** Who is in which unit, who is the primary filer and
   the spouse, who is claimed as a dependent and by whom, the filing status,
   which dependents have returns of their own.
2. **The filing decision.** Which units file. Today: Mok's filing probits
   below the filing threshold, the Pub 5785 hazard above it, then the
   non-filer pool's calibration so non-filing adults by age band equal
   population less adults on filed returns
   (`research/state_weights/nonfiler_pool/04_calibrate.R`). Only the CPS
   needs this step for training; it is also what makes the non-filers.

CBO's public algorithm (below) does step 1 only. It treats every unit as a
potential return, and adds a separate dependent return for every dependent
with any income. CBO decides filing elsewhere: it matches the units to the
IRS sample, and units that match a return are filers. We have no IRS file
to match to, so our filing step stays.

The two steps still interact. Rules that decide who claims a child (CBO's
reassignment pass, Iselin's strategic claiming; section 3) assume people
claim the way filers would, to get the most credits; and the filing model
uses the unit's children and income, so moving a child between units
changes both units' chances of filing.

## 2. What we do today, and what is wrong

Three surveys, three different methods:

| Survey | Method | Origin |
|---|---|---|
| CPS | production's non-filer builder, `src/nonfilers/asec_tax_units.R`, called by `pipeline/build_cps.R` | written in 2026 for the non-filer pool; follows Mok's construction on purpose, so her filing coefficients keep their meaning |
| SCF | production's `src/imputations/stage1_scf_tax_units.R`, called by `pipeline/build_scf.R` | a port of part of Kevin Moore's (Federal Reserve) SCF-to-TAXSIM SAS code |
| CEX | BLS's own tax unit codes (TAX_UNIT, TU_CODE, TU_DPNDT) with repairs, `pipeline/build_cex.R` | BLS; taken as given, not tested against returns |

**The CPS builder against the PUF, tax year 2015** (Codex review,
`/nfs/roberts/scratch/pi_nrs36/jar335/rebuild/explore/2026-10-23/tax_units_review/REPORT.md`):

| Millions | CPS filers | CPS, all units | PUF |
|---|---:|---:|---:|
| Dependents (capped as the PUF caps them) | 73.4 | 81.8 | 91.7 |
| Head of household | 11.8 | 14.5 | 22.3 |
| Married filing separately | 0 | 0 | 2.9 |
| Joint | 58.5 | 62.6 | 54.4 |

Of the 18.3 million missing dependents, 8.4 million sit on CPS units the
filing model calls non-filers; 9.9 million are missing even if every unit
filed. Specific faults found in the code:
- The support test is not the TRIM3 test it cites: it compares a person's
  income with half of household income per person, and rejects 2.8 million
  otherwise eligible people, 1.5 million of them students aged 19–23.
- A child attaches only through a parent in the household, so grandchildren
  with no parent present (1.2 million), siblings and foster children are
  missed as qualifying children.
- Head of household is given only to units with a qualifying child.
- No married filing separately.

**The CBO algorithm against ours, tax year 2023** (March 2024 CPS; our run of
CBO's code reproduces CBO's published tabulation exactly;
`/nfs/roberts/scratch/pi_nrs36/jar335/rebuild/explore/2026-10-23/cbo_units/`):

| Millions, all units | CBO | Ours | SOI 2023, filed returns |
|---|---:|---:|---:|
| Single | 84.4 | 101.2 | 80.3 |
| Joint | 64.3 | 64.8 | 54.6 |
| Head of household | 20.1 | 13.1 | 21.6 |
| Dependents, all | 89.0 | 78.4 | |
| Dependents, first three | 84.5 | 78.4 | |
| of which under 17 | 62.1 | 61.0 | |
| of which 17–23 | 13.0 | 11.8 | |
| of which 24 and over | 9.4 | 5.5 | |
| Dependents' own returns (with income) | 11.0 | dropped | |

CBO gets head of household right and finds more adult dependents (parents,
other relatives). Neither finds the children that returns claim from
outside the household: on 2015 returns, 67.4 million children qualified
for the child tax credit (N24), against about 61–62 million under 17 in
either CPS method. CBO's linked study (Mok) found about 11 million more
dependents on returns than in CPS units in 2006, from claims across
households and changes between December and March.

**The SCF and CEX, tested on 24 October** (explore/2026-10-24/scf_units and
spending_age). The SCF as built has 143.2 million units in 2019 against 168
million in the tax data: `build_scf.R` drops the 23.6 million financially
independent adults outside the primary economic unit (PEU), and the builder
counts a child up to three times on a split couple, gives the one child of a
split couple to both units, reads bracketed ages as exact, and adds a "child
living elsewhere" to 10.7 million households. The CEX as built has 152.5
million units: BLS's codes make 4.7 million earning adults dependents, and
dependent filers are folded into the claiming unit. Both are rebuilt by the
shared module (section 4).

Earlier notes: the SCF
builder counts children under 19 plus one possible child elsewhere, and on a
household split into two units can count a child twice; only the primary
economic unit's finances are measured. The CEX uses BLS's units, which
code many elderly couples as dependents of their own unit (repaired) and,
from 2024, roommates and partners the same way.

## 3. John Iselin's questions, per survey

From a conversation on 23 October. Iselin (Budget Lab) builds CPS tax units
with his own method, loosely based on the Tax Policy Center's, in three
variants: rules applied strictly; income-aware choice of which parent claims
a child within the household; and strategic EITC claiming allowed. His view
is that the third is closest to what returns show, which is what merging
needs.

CBO's optional reassignment pass is the same idea (`docs/algorithm.md`,
"Reassignment Routines"). After the first pass, within a household, it:
1. folds a related subfamily into the main unit when the subfamily's head
   qualifies as a dependent (a young mother with no income living with her
   parents becomes their dependent, with her child);
2. moves up to two of the youngest qualifying children between the main
   unit and a related subfamily, to spread children more evenly and lower
   the household's tax (the grandmother claims one grandchild, the mother
   the other);
3. does the same between unmarried partners.
It decides who claims whom, never whether anyone files. The 2023
comparison in section 2 ran with it on (CBO's default).

| Question | CPS ASEC | SCF | CEX interview |
|---|---|---|---|
| What is a household? | everyone living at the address in March; group quarters excluded from the ASEC (the pool adds them from the ACS) | the primary economic unit (PEU): the economically dominant person or couple and everyone financially dependent on them; others in the home are the non-PEU, asked far less | the consumer unit: people sharing major expenses; one address can hold several |
| Which relationships are stored? | each person's relationship to the householder, and pointers to spouse, mother and father (second parents from 2019), cohabiting partner, family and subfamily | each roster member's relationship to the respondent, age and sex; whether dependents are claimed; little on the non-PEU's links | each member's relationship to the reference person, age, sex; BLS's own tax unit, role and claimant |
| What period? | income for the previous calendar year; people and ages as of the March interview | income for the previous calendar year; people and wealth at the interview (spread over the year) | income for the last 12 months; spending for the last 3 months; interviews every quarter |
| Whose income is measured? | every person 15 and over, by source | the PEU's in detail, by person for head and spouse; the non-PEU's in a few summary items | each member's earnings and some transfers; other income for the consumer unit as a whole |
| How far from the law? | to decide (section 4) | to decide | BLS's rules, unknown in detail |
| Dependents' own returns? | built and scored, then dropped in the rebuild today | not built | BLS marks them; dropped today |

Two consequences. The SCF and CEX cannot support the CPS's detail (who
claims whom, students, support), so a single algorithm cannot be applied
identically to all three; what can be identical is the definitions and the
output. And the SCF's and CEX's own units are measured only for the
economic unit they survey, so their tests must use matching benchmark
subsets.

## 4. The proposal

1. **One Python module of tax unit rules, run on a standard person roster.**
   Each survey has its own reader that produces the roster: person, household,
   age, sex, relationship to the household's reference person, pointers to
   spouse and parents where the survey has them, student and disability where
   it has them, income items, and any claim the survey itself records (the
   CEX's TU_DPNDT, the SCF's dependents). The rules run on the roster; where a
   survey lacks an input, the rule falls back in a documented way, and the
   output says which fields were observed and which assumed.
2. **The rules start from CBO's algorithm,** translated into Python. Check:
   the Python version reproduces CBO's published tabulations
   (`outputs/Kinships_in_Tax_Units_CPS_CY_20YY.csv`) and our Stata run of
   their code exactly, for 2022–2024.
3. **Variants as settings, chosen by the tests after the filing step.**
   Strict legal rules; income-aware claiming within the household; strategic
   claiming (Iselin's three, and CBO's reassignment pass). Strategic claiming
   is a guess about how filers behave, so each variant is judged on its
   filers, after the filing step: filers by filing status, number of
   children and income against SOI, above all head of household and EITC
   returns by number of children (section 5). The units before the filing
   step are tested too, against population counts, but those tests cannot
   tell the variants apart on claiming. The choice is John's.
4. **Dependents' own returns kept,** as separate records linked to the unit
   that claims them, counted once as people. The PUF has dependent filers;
   the surveys should too.
5. **Output in the PUF's terms:** full filing status (including head of
   household, and married filing separately where it can be assigned),
   dependents by type (child at home, child away, other, parent) under the
   PUF's caps and type order, child credit and EITC child counts, the
   primary filer by the rebuild's rule (`AGES.md`, step 2), the earnings split
   on the PUF's definition.
6. **The filing step stays separate** and runs after construction, on the
   new CPS units: Mok's probits and the pool's calibration, reused as they
   are, then rechecked, since Mok's coefficients were estimated on her own
   construction. The model's inputs include the unit's children and income,
   so the construction variant feeds into it directly; changing the units
   changes which units it picks as non-filers even when the calibration
   holds the counts by age.

### What was built (24 October 2026; uncommitted, branch rebuild-step2)

John's decisions of 24 October: one shared module with CBO's rules for all
three surveys; the SCF's financially independent adults outside the PEU
become units and training rows; Gale, Joshi, Pulliam and Sabelhaus (2022) where
the SCF lacks inputs; no reweighting of any survey to fix its mix; separate
returns by step 1's draw in the CPS and CEX and by the couple's own answer in
the SCF.

| File | What it does |
|---|---|
| `pipeline/tax_units.py` | the rules only: `flag_dependents`, `assign_units`, `claim_within_group` (new), `reassign`, `collapse_units`, `mfs_split`, and `build_units(member list)`; the fallback table `FALLBACKS` and the assumed-input flags `ASSUMED_FLAGS`. Run as a script it still builds the CPS. |
| `pipeline/roster_cps.py` | the CPS reader, moved out of `tax_units.py` unchanged. Step 1's units for 2015-2023 come out identical, unit for unit. |
| `pipeline/roster_scf.py` | the SCF reader and the SCF's units, with separate returns from the couple's own answer |
| `pipeline/roster_cex.py` | the CEX reader and the CEX's units, with separate returns by step 1's draw to SOI's count |
| `pipeline/build_scf.R` | units from the module; the PEU's income and balance sheet divided between the respondent's and the spouse's or partner's units by Moore's shares (ported from production's builder, which is no longer called); units outside the PEU take their share of the group's answers |
| `pipeline/build_cex.R` | units from the module; spending by head count. BLS's own unit codes are only a comparison column in the checks (`roster_cex.py`). |
| `pipeline/run.sbatch` | the scf and cex stages run the reader (Python) before the R script |

There is no setting to go back to the old builders (John, 24 October); they
are in git history. Settings that are choices inside the rules are in
`settings.json` under `tax_units`: `reassign` (CBO's reassignment pass) and
`npeu_split` (how the SCF's outside-PEU group's income is divided).
| `pipeline/check_units.py` | the standing tests of section 5, every survey and year |

**The member list.** One row per person, with CBO's Census codes (PERRP,
HHDFMX and the family pointers) as the shared code list; each reader
translates its own relationship codes into them. Added to CBO's inputs: the
survey's own support answer (the SCF's financial dependence), whether the
person's own income is measured, and three flags for assumed inputs (an age
drawn within a bracket, a child or couple placed without a pointer, income not
measured). Every unit carries `any_<flag>` columns.

**Fallbacks, by survey** (`tax_units.FALLBACKS`):

| Input | CPS | SCF | CEX |
|---|---|---|---|
| Relationship | observed | observed, merged codes | observed |
| Parent links | observed | a child goes to the reference person's unit | the same |
| Couples beyond the reference person's | observed | the only two married adults outside the PEU are paired, filing jointly | a married child with a married in-law; else the only two other married adults |
| Exact age | observed | drawn within the bracket for members other than the couple | observed |
| Student test | observed | the support answer, ages 18-24 | college enrolment |
| Income test | observed | the support answer where own income is not measured; a non-relative adult is never claimed | member income only |
| Group claim | none | the highest-income unit outside the PEU claims the group's relatives who pass the income test | none |
| Children living elsewhere | none | dependents of the reference person's unit if the family paid alimony or child support | none |
| Separate returns | step 1's draw to SOI (after filing) | the couple's own answer, with Moore's prior-marriage condition | step 1's draw to SOI |

**Results, 2019** (millions of units; primary filer = the unit's head; the
SCF is its 2019 wave, tax year 2018; details in
`explore/2026-10-24/unified_units/checks/`):

| | SCF as built | SCF, shared rules | CEX, BLS units | CEX, shared rules | CPS, shared rules | Tax data |
|---|---:|---:|---:|---:|---:|---:|
| All units | 143.2 | 162.8 | 152.5 | 162.8 | 166.5 | 168.5 |
| Primary under 26 | 10.2 | 17.1 | 13.6 | 17.1 | 20.1 | 16.4 |
| Primary 26-34 | 23.6 | 28.3 | 26.2 | 29.1 | 29.3 | 31.7 |
| Primary 35-44 | 24.2 | 26.3 | 25.1 | 26.8 | 27.4 | 29.8 |
| Primary 45-54 | 24.1 | 26.1 | 25.3 | 26.1 | 26.3 | 27.9 |
| Primary 55-64 | 26.0 | 27.4 | 26.9 | 27.8 | 27.6 | 28.2 |
| Primary 65+ | 35.2 | 37.5 | 35.3 | 36.0 | 35.9 | 34.8 |
| Joint | 56.7 | 58.5 | 66.8 | 64.6 | 64.3 | 60.3 |
| Separate returns | (2.0 couples split) | 2.9 (SOI, 2018: 3.3) | 0 | 3.7 | after filing | 3.7 |

The SCF's median net worth per unit falls from $91,050 to $63,570 (2019
dollars); total net worth is unchanged apart from the $0.5 trillion of the
group outside the PEU. The CEX's spending per unit falls by about 5% in
every age range; total spending is unchanged.

## 5. Standing tests, every survey and year, before training

From the Codex review, section 4. A failed test keeps the affected variable
from being shared with the PUF.

| Test | Benchmark |
|---|---|
| Every person counted once; every spouse and claimant link valid; no one claimed twice | exact |
| Units by full filing status, primary age range and income | SOI Table 1.6; the PUF (2015) |
| Dependents by age range and type, under the PUF's caps | the PUF (2015: XOCAH, XOCAWH, XOODEP, XOPAR, AGEDP1–3) |
| Child credit and EITC children | the PUF's N24 and EIC; SOI's credit tables |
| After the filing step (CPS): filers by filing status, number of children and income; head of household returns; EITC returns by number of children | SOI; the PUF (2015). This is the test that tells the claiming variants apart |
| Dependents' own returns | SOI and the PUF's dependent filers |
| People by age and sex | Census population estimates |
| All units, filers and non-filers | returns plus the non-filer pool's counts |

Sampling error and documented differences set the tolerances. Targets used
to calibrate cannot also serve as tests.

## 6. Years

- **Building units.** CBO's code reads the Census Bureau's CSV files, which
  start with the 2019 survey. A Python version reading IPUMS works for every
  year IPUMS covers, since IPUMS keeps the same layout; the relationship
  detail is cruder further back (cohabiting partners from the mid-1990s,
  second parents only from 2019). The CPS ASEC files on disk cover survey
  years 2015–2026; John will pull earlier ones.
- **The filing step.** It needs, per year: population by age (Census),
  filers by age (SOI, back to 2000), and a Pub 5785 non-filer count
  (published for 2014–2016 only; other years use the pool's projection).
  2000 on is workable; earlier is thin.
- **Testing.** Only 2015 has a PUF; other years are tested against SOI's
  published tables.

## 7. Open decisions

1. How far from the law: strict rules, income-aware claiming, or strategic
   claiming (section 4, item 3). The shared module runs CBO's reassignment
   pass on every survey by default.
2. ~~Whether the SCF and CEX keep their own constructions~~ Decided 24
   October: rebuilt from their member lists by the shared module.
3. Whether the filing model moves to Python with the units, or stays in R.
4. ~~Married filing separately~~ Decided 24 October: the CPS and CEX by step
   1's draw to SOI's count, the SCF by the couple's own answer. The SCF's
   count against SOI Table 1.6, millions of separate returns: 3.1 against 2.7
   (2012), 3.9 against 3.0 (2015), 2.9 against 3.3 (2018), 3.3 against 3.9
   (2021). Within a million either way, with no steady direction.
5. Iselin's code: get it and run it through the same comparison as CBO's.
6. Wiring the module's SCF and CEX units into the training table
   (`build_table.py`: the SCF's units outside the PEU enter as SCF rows, their
   balance sheet flagged as group-level) and the tilt (the SCF's wealth
   targets recomputed on the new units). Not done; after John sees the tests.
7. The CEX's extra married couples (about 4 million more than the tax data in
   2019, in the data, not in BLS's coding) are not fixed by reweighting
   (John, 24 October). The consumption targets will move from averages by age
   range to averages by age range and married or not; that needs the CEX's
   units by age range and married (built here) and the file's married flag on
   the same definition (joint returns plus non-filing married couples, with
   separate returns counted as not married or as married, to decide).
8. Children of unmarried partners in the SCF and CEX: no parent pointer, so
   all go to the reference person's unit (flagged). CBO's reassignment pass
   moves children between partners only where the pointers say whose they are.

## 8. Sources

- CBO, CPS tax filing units: <https://github.com/US-CBO/CPS-tax-filing-units>
  (`docs/algorithm.md`, `docs/data.md`).
- Codex review of the CPS builder and other methods (NBER/Feenberg, IPUMS and
  Census, PSL taxdata, PolicyEngine, TRIM3, TPC, Piketty–Saez–Zucman), with
  links: `/nfs/roberts/scratch/pi_nrs36/jar335/rebuild/explore/2026-10-23/tax_units_review/REPORT.md`.
- Mok, CBO working paper 2017-06 (filing model and linked study):
  `research/state_weights/nonfiler_residual/resources/mok2017_cbo_wp2017-06.pdf`.
- The current CPS builder's design: `research/state_weights/nonfiler_residual/10_asec_tax_unit_design.md`.
