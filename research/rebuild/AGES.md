# Ages and sex of the people in a tax unit

Every record of the year file carries the primary filer's age range, the exact ages of the primary filer, the spouse
and up to three dependents, and the sex of the primary filer and the spouse. This file says how each is set, on the
PUF, on the survey rows of the training table, and in the generated file. It also covers the split of a married
couple's earnings, which depends on which spouse is the primary filer.

Code: `shared/ages.py` (the age range, step 0, the primary-filer rule), `prep/build_table.py` (the survey rows),
`prep/earn_split.py`, `generate/generate.py` (`cps_structure`), `scale/non_micro_imputations.py` (dependent filers),
`output/taxsim_file.py` and `shared/calculator_input.py` (the output forms). Settings: `ages`, `earn_split`,
`non_micro_imputations.items.dependent_filer_age_sex`.

## Summary

| Item | PUF records | Survey rows in training | Generated file |
|---|---|---|---|
| Primary filer's age range | the demographic file's range, or step 0's probabilities | the range of the primary filer's exact age | drawn under the PUF label (filers) or the CPS label (non-filers) |
| Exact age, primary filer and spouse | absent | SCF, CEX, CPS, SIPP | drawn under the CPS label, inside the range |
| Dependents' exact ages | absent (AGEDP1-3 not used) | CPS only | drawn under the CPS label |
| Sex, primary filer and spouse | absent (GENDER sets the primary-filer rule only) | CPS only | drawn under the CPS label |
| Dependent filers' age and sex | one of three dependent-filer ranges | none | drawn from CPS dependents who file, inside the range |

## Age ranges

The slot `age_range` has nine categories: the six ranges of the 2015 PUF demographic file for filers who are not
someone's dependent, and its three ranges for dependent filers, kept apart.

| Value | Range | Value | Range |
|---:|---|---:|---|
| 0 | under 26 | 5 | 65 and over |
| 1 | 26-34 | 6 | dependent filer under 18 |
| 2 | 35-44 | 7 | dependent filer 18-25 |
| 3 | 45-54 | 8 | dependent filer 26 and over |
| 4 | 55-64 | | |

The PUF's primary filer is the taxpayer listed first on the return. SOI Table 1.6 measures age the same way ("Age for
joint returns was based on the primary taxpayer's age"), and where the PUF's ranges cover an AGI bracket fully they
reproduce the table.

## The primary filer's age range on the PUF

A PUF record keeps the demographic file's range where it has one. Records with no range get a probability for each
range instead ("step 0"), and training draws a range from them at the start of every pass.

Which records have no range:
- 88,017 records (2.2% of returns) have no demographics at all. SOI removes them by sampling stratum, so the cut is
  on income: above about $400,000 of AGI no record has an age. These records hold 23.7% of AGI and 77% of long-term
  gains.
- 252 records are coded "not present".
- Dependent filers with no dependent-filer range.

Step 0, run once in the table stage (`ages.puf_age_ranges`):
1. A prior: a multinomial logit of the six adult ranges on what the return shows about age (filing status, AGI,
   wages, Social Security, pensions, IRA distributions, the early-withdrawal penalty, the standard deduction's
   additions for age 65 or blindness, children at home, other dependents). No capital or business income enters.
   It is fitted, weighted, on the records with a known range, once for joint returns and once for the rest.
2. Calibration to SOI Table 1.6 (2015), cell by cell (filing status by 19 AGI brackets). In each cell where at least
   10% of the weight has no range (settings `ages.min_missing_share`), one factor per range multiplies every
   missing record's prior, and each record's probabilities are rescaled to sum to one, until the missing records'
   weight in each range equals the table's count less the known-range weight. A negative residual is set to zero and
   the rest rescaled to the missing weight. Where the table merges under 26 into 26-34, the pair has one target.
   Known dependent filers under 26 count in the under-26 row. In cells below the threshold the prior stands.
3. "Not present" records lie outside Table 1.6 and keep the prior. Dependent filers with no range get the known
   dependent filers' weighted shares over the three dependent-filer ranges.

The cell log is `data/age_step0_cells.csv`. In the installed build 3.33 million returns in Table 1.6's cells have no
range; 30 cells are calibrated and hold 2.68 million of them. From $500,000 of AGI up, every return in a cell lacks
a range; at $200,000-500,000 between 20% and 29% do; among joint returns with AGI below $1, 19% of the weight does.

Where the range is used after step 0:
- Training draws one range per record and pass from the probabilities.
- Generation of synthetic filers draws the range under the PUF label, with married and the number of dependents, in
  a random order per record. The rebuilt aggregate records at the top draw only the six adult ranges.
- With settings `generation.filers` set to `real_puf`, a real record with no range draws one per record and network
  copy from step 0's probabilities (a draw keyed by record id and copy), and keeps the observed range otherwise.

## The primary filer in a survey couple

Single, head-of-household and separate returns have one filer. For a couple filing jointly, every survey (SCF, CEX,
CPS, SIPP) uses the same rule:
1. In a mixed-sex couple the man is the primary filer, except that the woman is, with a chance that depends on the
   man's age range.
2. In a same-sex couple the older partner is the primary filer.
3. In a couple whose sex is not reported (only the SCF's units outside the primary economic unit), person 1, the
   unit's head, is the primary filer.

The chances in rule 1 are set so that the share of female primaries by the primary's range on the SCF's mixed-sex
couples (all waves, weighted) equals the PUF's share on joint returns with demographics (not dependent filers). They
are found by a fixed-point iteration (`ages.woman_first_chance`) and printed in the table stage's log. The draw is a
uniform keyed by the household id (settings `ages.primary_rule_salt`), so it does not depend on other records.
Everything about the person follows the choice together: the age range, the exact ages, the sex, and every person
item (wages, self-employment income, weeks, hours, overtime, tips, elective deferrals, Social Security number
status). The survey builders write the couple with the man (or the older partner) as person 1; the table stage swaps
the two people's columns where the woman goes first.

What the 2015 PUF shows on joint returns with demographics (51.6 million returns):
- The primary filer is the man on 90.0% of returns.
- Female primaries are 27.5% of returns with a primary under 26, 18.9% at 26-34, 12.8% at 35-44, 9.4% at 45-54,
  7.8% at 55-64 and 3.4% at 65 and over.
- The order does not follow earnings: the share of male primaries is 91.0% where the primary earns 75% or more of the
  couple's earnings, 87.2% at 25-75% and 88.5% under 25%.
- The order does not follow age: on standard-deduction returns with exactly one addition for age or blindness, the
  male primary is the one aged 65 or over 79.2% of the time; in CPS couples with one partner 65 or over, the man is
  that partner 79.6% of the time.

## Exact ages of the primary filer and the spouse

On the training rows, `age1` (the primary filer) and `age2` (the spouse) are categories: 0 none, otherwise the age
plus 1, with every age of 80 and over coded as 80. That top code is the finest all the surveys allow: the CPS codes
80-84 as 80 and 85 and over as 85; the CEX codes everyone from 83 (84 in 2019 and 2024) as one value; the SCF reports
ages to 95.

| Source | Primary filer | Spouse | Age measured |
|---|---|---|---|
| PUF | absent | absent | |
| SCF | the respondent or the spouse or partner, by the rule above | the other, couples only | at the interview, in the year after the income year |
| CEX | exact | couples only | at the interview |
| CPS | exact | couples only | at the March interview after the tax year |
| SIPP | exact | couples only | at the interview after the reference year |

Ages are taken as the surveys report them; no adjustment is made for the interview date. The table stage checks that
every survey row's exact age lies in its age range and that a spouse's age is present exactly on couples.

At generation the two ages are drawn right after the PUF items, in a random order per record, under the CPS label and
the record's own CPS year, with every slot the CPS lacks left out. The draw is held to these rules
(`generate.cps_structure`):
- the primary filer's age lies inside the record's age range;
- a non-filer unit's head is 18 or over;
- the spouse has an age exactly when the unit is married, and it is 18 or over.

The SCF, CEX, CPS and SIPP phases that follow all see the drawn ages.

## Dependents' ages

Only the CPS rows carry dependents' ages (`dep_age1` to `dep_age3`). They are the ages of the unit's three youngest
dependents, youngest first, taken from all its dependents (`prep/build_cps.R`). They are not the PUF's selection: the
PUF shows at most three dependents on joint and head-of-household returns, two on single returns and one on separate
returns, chosen by type (children at home, children away, other dependents, parents) before age. The CPS ages are
coded 0 for none, otherwise the age plus 1, ages 0 to 85 (the ASEC's top codes). The SCF, CEX and SIPP carry only the
number of dependents.

The PUF's own dependent age ranges (AGEDP1-AGEDP3) are not used anywhere in training or generation.

At generation each dependent's age is drawn under the CPS label, after the PUF items and the exact adult ages, among
the other CPS-only slots in a random order per record. The k-th dependent has an age exactly when the record has k or
more dependents (`n_dep`, the PUF's count of dependents, capped at 3). Generation does not force the three ages into
youngest-first order.

## Sex

Only the CPS rows carry sex (`sex_primary`, `sex_spouse`: 0 none, 1 man, 2 woman). At generation both are drawn under
the CPS label: the primary filer's is always a man or a woman, and the spouse's is present exactly when the unit is
married. The PUF's own sex of the primary filer (GENDER) is used only to set the chances of the primary-filer rule
above; it is not a slot, so the primary filer's sex on a filer record comes from the CPS given the record.

## Dependent filers

A PUF record filed by someone claimed as a dependent (DSI = 1) carries one of the three dependent-filer ranges. The
surveys have no dependent-filer rows: each survey folds a dependent's own return into the unit that claims them
(`TAX_UNITS.md`). At generation a dependent filer holds no survey item: its exact ages and sex are none (`age1` is -1).

The non-microdata imputations then give each dependent filer an exact age and a sex (`dep_filer_age`,
`dep_filer_sex`). They are drawn from the CPS dependents that the filing stage counts as filing a return of their own
(required to file, or with any wages), in the record's own CPS year and inside the record's range (under 18, 18-25,
26 and over), with the CPS weights; ages above 80 are set to 80. The draw is keyed by the record id. Elsewhere
`dep_filer_age` is -1 and `dep_filer_sex` 0. The calculator input and the Tax-Simulator file use these as the
dependent filer's `age1` and `male1`.

## Records at the top of the file

- The rebuilt aggregate records (`synthetic_top`) draw their age range (adult ranges only), exact ages and sex from
  the networks like any filer.
- The Forbes list's records take the age on the list (held to 80), raised by the years since the list when a list is
  carried to a later year, and the sex on the list. A married billionaire's spouse is the same age and the other sex
  unless the list names the spouse separately. Billionaires under 45 have up to three dependents, aged 12, 9 and 6.

## Encodings

| Where | Ages | Sex |
|---|---|---|
| Training table | `age1`, `age2`: 0 none, else age + 1, 80 and over as 80; `dep_age1-3`: 0 none, else age + 1, to 85 | 0 none, 1 man, 2 woman |
| Year file | `age1`, `age2`, `dep_age1-3` in years, -1 none (dependent filers: `age1` -1, see `dep_filer_age`) | `sex_primary`, `sex_spouse`: 0 none, 1 man, 2 woman |
| Calculator input | `age1` always set (dependent filers from `dep_filer_age`); `age2`, `dep_age1-3` missing where none | `male1` 1 or 0 |
| Tax-Simulator file | as the calculator input | `male1` 1 or 0; `male2` missing without a spouse |

## The split of a couple's earnings

Two PUF items describe how a joint return's earnings divide between the spouses. Both are shared with the CPS, and
both depend on which spouse is the primary filer.

- **The spouse's share of the couple's self-employment income** (`spouse_se_share`). On the PUF it is E30500 / E30400:
  E30400 is the couple's Schedule SE net earnings (92.35% of self-employment income, none below $400, up to the room
  under the Social Security wage base) and E30500 the spouse's part. The data confirm this reading: E30400 is never
  below E30500, and on joint returns with no wages it reproduces the demographic file's earnings split code on 99.9%
  of returns. The share is asked only of a joint return with Schedule C, Schedule F or other Schedule SE earnings
  (`part_se`). It is missing on 5.06 million such returns that have no Schedule SE earnings up to the wage base
  (losses, amounts under $400, or wages at the cap). On the CPS it is the same share computed from each person's
  self-employment income; where the couple has self-employment income but no Schedule SE earnings, the share of the
  income itself, held to 0-1. One share multiplies Schedule C, Schedule F and `part_se` alike (`sole_prop2`, `farm2`,
  `part_se2`).
- **The earnings split code** (`earn_split`, the PUF's EARNSPLIT): the primary filer's share of the couple's wages
  plus Schedule SE earnings, 1 for 75% or more, 2 for 25-75%, 3 for under 25%, 0 where the return is not joint or has
  no earnings. On the CPS it is the code of the couple's own earnings on the same definition. Joint PUF returns with no
  demographics and some earnings (2.51 million returns) get probabilities over codes 1-3 from a gradient boosted
  classifier fitted on the joint returns with demographics and earnings, cut to the codes the return's wages and
  Schedule SE split allow, and training draws a code each pass.

The classifier's check is `data/earn_split_step0_check.csv`. Fitted below $250,000 of AGI, it predicts the known codes
at $250,000-400,000. On the records with no demographics it gives 1.45, 0.73 and 0.33 million returns in codes 1-3;
the demographic booklet's full sample less the public file has 1.36, 0.63 and 0.39 million (a check, not a target).

At generation the self-employment share and then the code are drawn under the batch's label after the PUF items.
Under the CPS label the spouse's share of wages is drawn only in the bands that can reach the code's range given the
couple's wages and self-employment split, and the dollar step holds the exact share inside that range.

## Checks

- The table stage asserts on every survey row: the primary filer's exact age lies in the age range; a spouse's age and
  sex are present exactly on couples; the k-th dependent's age is present exactly when there are k or more
  dependents; the primary filer's sex is never none (CPS).
- The table stage logs the woman-first chances, the PUF's and the SCF's female-primary shares by range, and the share
  of couples with the woman first, same-sex, and of unknown sex in each survey; and a table of age-range shares by
  source and year (PUF filers with step 0's probabilities, every SCF wave, CEX, CPS and SIPP year).
- The translation report compares each survey's exact ages with the CPS of the nearest year.
- The calculator input's conversion notes count records with no primary age, joint returns with no spouse age, spouse
  ages on returns that are not joint, and records where dependents' ages and `n_dep` disagree.
- The non-microdata imputations' checks report the dependent filers' ages and sex by range and CPS year.

## Limitations

- **Exact age at the top is assumed, not measured.** Above $400,000 of AGI the PUF has no ranges, step 0 fixes how
  many returns are in each range in each AGI bracket but not which ones, and the CPS, under whose label exact ages are
  drawn, is thin there and top-codes wages. Age matters for wealth at the top: among SCF households with $200,000 or
  more of wages, median net worth is $2.4 million at 55-64 and $15.9 million at 75 and over.
- **The CPS ties age to capital income less closely than the SCF among the old.** Among units with a primary filer 65
  or over and $10,000-100,000 of dividends plus interest, the share aged 80 and over is 33%, 22% and 29% in the SCF's
  2016, 2019 and 2022 waves and 22%, 21% and 21% in the matching CPS years. Pooled over the waves the gap is beyond
  sampling error. Exact ages are drawn under the CPS label, so the file follows the CPS here.
- **Dependents' ages ignore the PUF's own ranges**, and they come from CPS units whose dependents are found only among
  the people of the household (`TAX_UNITS.md`). Nothing forces the drawn ages to agree with the child credit count
  (`n_dep_ctc`) or the EITC count.
- **The CPS's dependents are not selected as the PUF's are.** The CPS rows keep the three youngest of all dependents
  and cap the count at three for every filing status; the PUF caps by filing status and selects by type first. A CPS
  separate return carries all of the couple's dependents (up to three), where the PUF shows at most one.
- **No more than three dependents.** `n_dep` is capped at 3, as the PUF shows it; about 4.8 million dependents are
  missing against SOI's count of exemptions.
- **The primary filer's sex on filer records is the CPS's, not the PUF's.**
- **No adjustment for the interview date.** CPS ages are measured in March after the tax year, so about one person in
  five is a year older than at the end of the tax year; the SCF's interviews spread over the year after the income
  year. This matters at range edges, most of all at 64 and 65.
