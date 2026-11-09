# Tax units in the surveys

Every survey in training (the CPS ASEC, the SCF, the CEX interview survey and the SIPP) is split into tax units by
one set of rules before it enters the training table. The rules are CBO's public algorithm for CPS tax filing units
(US-CBO/CPS-tax-filing-units), translated rule by rule into Python. Each survey has a reader that turns its person
records into a standard member list; the rules run on that list; where a survey lacks an input, a named fallback
stands in and the person and unit are flagged. The model learns how survey items relate to tax items through what
the two share (marital and filing status, dependents, the primary filer's age range, wages), so the survey's units
must be built the way returns are.

The rules build units only. Only the CPS needs a filing decision: the filing stage (`prep/filing.py`, described in
`FEATURES.md`) gives each CPS unit a filing chance and makes the non-filers.

## Files and stages

| Survey | Years | Reader | Units built by | File the table stage reads |
|---|---|---|---|---|
| CPS ASEC | tax years 2015, 2017, 2019, 2021, 2023 | `prep/roster_cps.py` | `prep/filing.py units`; separate returns in `filing.py calibrate` | `cps_units_<year>.csv.gz` (`prep/build_cps.R`) |
| SCF | waves 2013, 2016, 2019, 2022, 2025 | `prep/roster_scf.py` | `roster_scf.py` | `scf_units_<wave>.csv.gz` (`prep/build_scf.R`) |
| CEX | survey years 2015, 2017, 2019, 2021, 2023, 2024 | `prep/roster_cex.py` | `roster_cex.py` | `cex_units_<year>.csv.gz` (`prep/build_cex.R`) |
| SIPP | reference years 2015, 2017, 2019, 2021, 2023 | `prep/roster_sipp.py` | `roster_sipp.py` | `sipp_units_<year>.csv.gz` |

The rules are in `prep/tax_units.py`. Settings: `tax_units` (`reassign`, `npeu_split`), `cps` (`years`,
`cbo_variant`), `cex.years`, `sipp`, `paths.raw.scf_waves`.

## The member list

One row per person, in CBO's Census codes, into which each reader translates its own: household, person number,
weight, age, sex; `perrp` (the detailed relationship to the household's reference person) and `hhdfmx` (the detailed
family position) with the family pointers; spouse and parent pointers and marital status; student and disability
inputs; income items and gross income by CBO's definition (wages, self-employment, interest, dividends, rents, estates
and trusts, pensions, unemployment, other income, and the taxable part of Social Security by the Form 1040
worksheet); the survey's own answer on support where it has one (the SCF's financial dependence); and three flags for
assumed inputs: an exact age drawn within a reported bracket (`age_assumed`), a child placed or a couple paired with
no pointer to say so (`link_assumed`), own income not measured or measured only for a group (`income_assumed`).
Every unit carries `any_<flag>` columns.

## The rules

In order (`tax_units.build_units`):

1. **Dependent tests** (`flag_dependents`). A qualifying child is under 19, or under 24 and a student, or disabled. A
   qualifying relative has gross income below the personal exemption amount. A married person whose own or spouse's
   gross income is above the standard deduction for married filing separately cannot be either. The two amounts by
   tax year are in `tax_units.PARAMS` (2012-2025).
2. **First assignment** (`assign_units`). Every person goes to one unit as head, spouse, qualifying child or
   qualifying relative, by family position: the primary family, other relatives, unrelated subfamilies, group
   quarters, people living with no relatives, subfamily heads and spouses, children in subfamilies. An unmarried
   partner who passes the qualifying-relative test is the householder's dependent when the householder's gross
   income is more than twice the partner's.
3. **Group claim** (SCF only, `claim_within_group`). Within the SCF's members outside the primary economic unit, the
   unit with the highest income claims the group's other relatives of the respondent who are on their own, pass the
   income test and are not married (Gale, Joshi, Pulliam and Sabelhaus 2022).
4. **Reassignment** (`reassign`, always applied, as in CBO's code). Within a household: a related subfamily whose head qualifies as a dependent joins the householder's unit;
   up to two of the youngest qualifying children move between the householder's unit and a related subfamily, and
   between the householder and an unmarried partner, to spread the children more evenly. It decides who claims whom,
   never whether anyone files.
5. A person CBO's code would leave as a second head in the householder's unit heads a unit of their own.
6. **One row per unit** (`collapse_units`). Head and spouse incomes are summed. Filing status is joint with a spouse,
   head of household with any dependent, single otherwise. For each unit: the number of dependents (uncapped), the
   number by type (child at home, child away (SCF only), other dependent, parent), child credit children (qualifying
   children under 17), EITC children (qualifying children), and the dependents the PUF would show (its caps by filing
   status and its type order, then youngest first). Every dependent with any gross income, wages or self-employment income also gets a
   return of their own, linked to the claiming unit.

## Fallbacks by survey

| Input | CPS | SCF | CEX | SIPP |
|---|---|---|---|---|
| Relationship | observed | observed; the public file merges some codes | observed | observed |
| Parent links | observed | none: a child goes to the respondent's unit | none: the same | observed |
| Spouse links | observed | outside the PEU, the only two married adults are paired | a married child with a married in-law of the other sex; else the only two other married adults of the other sex | observed |
| Exact age | observed | drawn within the bracket for members other than the couple | observed (seven 2015 children with none are given 8) | observed |
| Student test | observed | the support answer, ages 18-24 | enrolled in college | enrolled in December with no earnings that month |
| Disability | observed | none | none | any of six core disability items |
| Income test | observed | the support answer where own income is not measured; a non-relative adult is never claimed | member income only: wages, self-employment, Social Security, SSI | observed |
| Group claim | none | the highest-income unit outside the PEU | none | none |
| Children living elsewhere | none | dependents of the respondent's unit if the family paid alimony or child support | none | none |

## Each survey

**CPS ASEC** (`roster_cps.py`). The ASEC of March of year Y+1 asks about tax year Y. The reader uses the IPUMS
extract and rebuilds the Census codes CBO's code reads (`hhdfmx`, `perrp`, the family pointers) from IPUMS's
relationship, family and pointer variables; the few inputs IPUMS lacks and their stand-ins are noted in
`prep/roster_cps.py`, beside the code that uses them. People in group quarters are left out. Ages are as the ASEC reports them, at the
March interview. The filing stage builds the units, gives each a filing chance, and splits joint couples into separate
returns: couples are taken in the order of a uniform keyed by unit id (settings `filing.mfs_salt`) until their weight
times filing chance reaches half of SOI Table 1.6's separate returns, and each couple becomes a return for person 1,
with all the dependents, and one for person 2. `build_cps.R` then writes one row per unit:
- dependents' own returns are dropped: the dependent is counted once, in the claiming unit's `n_dep`, and their income
  goes nowhere;
- `n_dep` is the number of dependents, capped at 3; `dep_age1-3` are the three youngest dependents' ages; `n_dep_ctc`
  counts qualifying children under 17 and `n_dep_eitc` qualifying children on units eligible for the EITC, each
  capped at 3; `mars` is the filing status (1 single, 2 joint, 3 separate, 4 head of household);
- person 1 is the man in a mixed-sex couple and the older partner in a same-sex couple (`AGES.md`);
- `record_id` is the IPUMS household serial times 100 plus the head's person number; `household_id` is the serial.

**SCF** (`roster_scf.py`, `build_scf.R`). The SCF lists everyone in the household but measures in detail only the
primary economic unit (PEU): the respondent, a spouse or partner who shares finances, and everyone financially
dependent on them. Other adults who usually live there and are financially independent are asked about as a group:
their total wages, other income by kind, vehicles, accounts, other assets and debts. Members who neither usually live
there nor depend on the PEU are left out, as in the SCF's own household count. Each implicate is a household.
- Dependents' own returns are not written as units.
- Members other than the respondent and the spouse or partner are reported in age brackets (0-3, 4-6, 7-12, 13-17,
  18-27, then five-year groups, 93 and over); an exact age is drawn uniformly within the bracket, keyed by household
  and position.
- A married couple living together files separately when it says it did and either spouse was married before.
- The PEU's income and balance sheet: a unit holding the whole PEU takes all of it. When the respondent and the spouse
  or partner are separate units, each takes a share: wages by the two people's labor income, Schedules C and F by
  their business income from the main job, the home, other real estate, their mortgages and their unrealized gains
  half each, everything else by their labor, business, pension and account-withdrawal income (half each when both
  are zero).
- Units outside the PEU take their members' shares of the group's answers: the group's income divided equally among
  its adults, and the balance sheet equally too (`roster_scf.npeu_shares`). Capital gains and tax-exempt
  interest are not asked of the group. Items the SCF asks only of the PEU (elective deferrals, the retirement
  accounts' stock share, inheritances) are missing on these units, not zero.
- Other units (a non-relative in the PEU, a partner outside it) and dependents hold no PEU dollars.
- Column `taxunit_code`: 0 the whole PEU, 1 the respondent's part, 2 the spouse's or partner's part, 8 other, 9
  outside the PEU.

**CEX** (`roster_cex.py`, `build_cex.R`). A household is one interview, the consumer unit: the people sharing major
expenses. Other consumer units at the same address are other interviews. Relationship to the reference person is
observed; there are no pointers, so children and grandchildren go to the reference person's unit. Joint couples are
split into separate returns by the same keyed draw as the CPS, with no filing chance, until their weight reaches half
of SOI Table 1.6's separate returns; on those rows married is 0. Dependents who would file their own return stay
people of the claiming unit, with their income. The consumer unit's interest, dividends, rents, pensions and other
income, asked of the unit as a whole, are divided among its tax units in proportion to each unit's positive member
income (equally when none has any); spending is divided by the number of people on each return.

**SIPP** (`roster_sipp.py`). The household is everyone present at the interview, with their relationship to the
householder then; people who left before the interview are not members. A sample unit holding more than one
householder at the interview is divided into one household per householder, each person going with their December
residence's householder. Census's family codes are rebuilt from the spouse and parent pointers. Income is summed over
the reference year's twelve months. Separate returns are drawn as for the CEX (settings `sipp.mfs_salt`). Dependent
filers are folded into the claiming unit, their own income going nowhere, as in the CPS.

## Separate returns

| Survey | How a couple files separately | Count |
|---|---|---|
| CPS | keyed draw among joint couples, by weight times filing chance | half of SOI Table 1.6's separate returns, in couples |
| SCF | the couple says it files separately and either spouse was married before | as the survey answers |
| CEX | keyed draw among joint couples, by weight | half of SOI Table 1.6's separate returns, in couples |
| SIPP | as the CEX | as the CEX |

The SCF's count against SOI Table 1.6, in millions of separate returns: 3.1 against 2.7 (2012), 3.9 against 3.0
(2015), 2.9 against 3.3 (2018), 3.3 against 3.9 (2021).

## Dependent filers

No survey contributes dependent-filer rows to training: the CPS and the SIPP drop dependents' own returns (their
income goes nowhere), the CEX keeps the dependents and their income in the claiming unit, and the SCF writes only the
main units. The dependent filers in the year file come from the PUF. The CPS's dependents' own returns are used twice:
the filing stage decides which of them file (a dependent aged 15 or over files when required to or with any wages),
and the dependent filers' exact ages and sex are drawn from those who file (`AGES.md`).

## Group quarters

No survey's units include people in group quarters. The CPS units leave them out (the ASEC covers only non-institutional
group quarters); the SCF, CEX and SIPP sample households only. The filing stage's calibration counts them
as zero.

## Standing checks

- Built into the rules on every run: every person in exactly one unit; one head and at most one spouse per unit;
  every spouse and dependent pointing at a unit with a head; every dependent's own return pointing at a claiming unit.
- `build_cps.R` stops unless the filing stage's returns and its units match one for one, wages and self-employment
  income split exactly between the two people, and the dependents' ages agree with `n_dep`.
- The filing stage writes, per year, its fit to each target and checks against SOI and, for 2015, the PUF (filers and
  non-filers by filing status, people by age, persons counted once, required filers by income), in `filing/checks/`.

## Units by source, 2019

Millions of units under the shared rules. The primary filer here is the unit's head. The SCF is its 2019 wave (tax
year 2018). The tax data are SOI Table 1.6 by the primary filer's age, less dependent filers, plus the CPS's
expected non-filers.

| | SCF | CEX | CPS | Tax data |
|---|---:|---:|---:|---:|
| All units | 162.8 | 162.8 | 166.5 | 168.5 |
| Primary under 26 | 17.1 | 17.1 | 20.1 | 16.4 |
| Primary 26-34 | 28.3 | 29.1 | 29.3 | 31.7 |
| Primary 35-44 | 26.3 | 26.8 | 27.4 | 29.8 |
| Primary 45-54 | 26.1 | 26.1 | 26.3 | 27.9 |
| Primary 55-64 | 27.4 | 27.8 | 27.6 | 28.2 |
| Primary 65 and over | 37.5 | 36.0 | 35.9 | 34.8 |
| Joint | 58.5 | 64.6 | 64.3 | 60.3 |
| Separate returns | 2.9 | 3.7 | after filing | 3.7 |

Under these units the SCF's median net worth per unit in the 2019 wave is $63,570 (2019 dollars).

## Limitations

- **Children claimed from outside the household are missed.** The rules attach dependents only among the people of a
  household. On 2015 returns 67.4 million children qualified for the child tax credit (PUF N24); CBO's algorithm,
  run on the CPS, finds about 62 million dependents under 17. CBO's linked study (Mok 2017) found about 11 million more
  dependents on returns than in CPS units in 2006, from claims across households and changes between December and
  March.
- **No child living away.** The CPS counts college students at their parents' home, so the type "child away" is never
  assigned there; only the SCF adds children living elsewhere, and only where the family paid alimony or child support.
- **More married and fewer dependents than returns.** In the CPS 36-38% of units are married against 32-34% of the tax
  data's units, and 72-75% claim no dependent against 66-69%. The CEX has about 4 million more married couples than the
  tax data in 2019; no survey is reweighted to correct its mix.
- **SCF and CEX links are assumed.** With no parent pointers, children of an unmarried partner go to the reference
  person's unit, and couples outside the reference person's are paired by rule. These units are flagged.
- **SCF members outside the PEU** have their income measured only as a group and divided by rule, and their ages
  drawn within brackets.
- **Separate returns** in the CPS, CEX and SIPP are a random set of couples sized to SOI's count, not couples that
  look like those who file separately.
- **Group quarters** are not in any survey's units.
