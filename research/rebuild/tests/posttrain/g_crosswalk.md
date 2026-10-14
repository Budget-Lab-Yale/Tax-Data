---
title: "Test 3, piece G — which of the 39 PUF variables the SCF measures (crosswalk proposal)"
role: method
workstream: rebuild
status: current
updated: 2026-10-09
sot: research/rebuild/tests/posttrain/SPEC.md
supersedes: []
superseded_by: null
---

# Which PUF variables the SCF measures

This note proposes, for each of test 1's 39 PUF variables (weight excluded),
whether SCF filer tax units observe it and how it is built from SCF items.
A variable the SCF does not measure is **unobserved** for every SCF record
(token -1), never zero: the model fills it from what it learned on the PUF.
The dataset `full` (`$T2/data/full/`) is built with the first version below;
`full_v2` (`$T2/data/full_v2/`) adds the second-version items. Weighted totals and
nonzero rates, PUF against SCF, are in `G_DATA_REPORT.md`.

The test applied: the SCF must ask about the same economic item for the same
tax unit, so that "has a nonzero amount" and "ranks high among holders" mean
roughly the same thing in both sources. Amounts are rank tokens within
source and year, so differences in level (the SCF's wages run 12 percent
above the PUF's) are absorbed; differences in who has a nonzero amount are
not. Where the SCF lumps several PUF lines into one question, the lump is
assigned to the PUF line that carries most of it only when the others are
small; otherwise the PUF lines stay unobserved.

SCF incomes are the raw interview items for the tax year (2012 for the 2013
wave, 2015 for the 2016 wave), split between the units of a household by
test 2's builder (wages by own wages, business by own business income, the
rest by earnings share), plus the separately reported income of split-off
adults. Details of the items are in `../scf_joint/a_crosswalk.md`.

## First version (dataset `full`, 9 October)

| PUF variable | SCF status | SCF definition |
|---|---|---|
| `MARS` filing status | observed | Builder filing status: 1 single, 2 joint (a married couple kept as one unit, including couples where only one spouse filed), 3 separate (a married couple split into two units), 4 head of household (single with children). |
| `DSI` dependent filer | observed | 0 for every unit. The builder never makes a financially dependent person into a tax unit, so the SCF filer population has no dependent filers (the PUF has 9.3 million). |
| `XTOT` exemptions | observed | 1, plus 1 for a joint unit, plus the builder's dependents (children under 19 in the household, plus one if the household supports a child living elsewhere), top-coded at 5 as on the PUF. The PUF also counts older students and other relatives. |
| `N24` children for the child credit | unobserved (v1); observed (v2) | v2: children under 17 in the unit (builder's KIDSU17), top-coded at 3. |
| `EIC` children for the EITC | unobserved | Depends on claiming the credit, which depends on income rules the SCF does not record. |
| `FDED` itemized or standard | unobserved | The SCF does not ask. |
| `AGERANGE` age range | observed | Age of the unit's head at the interview, in the PUF's six ranges. PUF records outside the demographic file (and the 252 coded 0) are unobserved on the PUF side. |
| `GENDER` | unobserved (v1); observed (v2) | v2: sex of the unit's head (X8021 for the respondent's unit, X103 for the spouse's separate unit). The SCF's respondent in a mixed-sex couple is usually the man, as is the PUF's primary filer on most joint returns. Split-off units: unobserved (the builder does not carry their sex). |
| `EARNSPLIT` | unobserved | The SCF has each spouse's wages from the main job, but the PUF's band boundaries need the demographic booklet to confirm; left for John (see open questions). |
| `E00100` AGI | unobserved | The SCF asks total income (X5729), which is not AGI (it includes nontaxable Social Security and transfers and excludes adjustments). |
| `E00200` wages | observed | X5702. |
| `E00900` Schedule C | observed | X5704, which is Schedule C **and** F combined (the SCF asks one question for both). Farm is small (1.2 percent of PUF returns nonzero, a net loss of $15 billion). v2: 0 for households that filed Schedule F but not C (their amount goes to `E02100`). |
| `E26270` partnership and S corporation | unobserved | The SCF's X5714 lumps rent, royalties, partnerships, S corporations, estates and trusts into one amount (Schedule E line 17). No credible split. |
| `P23250` long-term gains | observed | X5712, net capital gain or loss of all holding periods including fund distributions. Long-term gains carry most of the dollars (PUF: $637 billion long-term against a net short-term loss), so the lump is assigned here. |
| `P22250` short-term gains | unobserved | Inside X5712. |
| `E00600` ordinary dividends | observed | X5710 dividends. |
| `E00650` qualified dividends | unobserved | The SCF does not separate qualified dividends. |
| `E00300` taxable interest | observed | X5708 (interest other than tax-exempt). Split-off units: v1 unobserved where they report any interest or dividends (36 rows), v2 their interest. |
| `E01500` total pensions | unobserved | Includes rollovers between retirement accounts, which the SCF does not count as income. |
| `E01700` taxable pensions | unobserved (v1); observed (v2) | v2: X5722 (Social Security, pensions and annuities) less the household's current Social Security benefit annualized, floored at zero, split by earnings share; split-off units: their pension income. The taxable share is unknown, so this is gross pension and annuity income placed against the PUF's taxable amount, the closer of the two PUF lines because it excludes rollovers. |
| `E01400` taxable IRA distributions | unobserved | The SCF asks IRA withdrawals, but the builder's withdrawal amount mixes IRA, 401(k) and pension-account withdrawals; left out, as in test 2. |
| `E02400` Social Security | unobserved (v1); observed (v2) | v2: current monthly (or other period) Social Security benefit of the respondent and spouse, annualized (X5306/X5307, X5311/X5312), assigned to the person's own unit when a couple is split; split-off units: their Social Security. A current benefit at the interview stands for the prior year's benefit. |
| `E25850`, `E25860` rent and royalty income, loss | unobserved | Inside X5714. |
| `E02100` farm | unobserved (v1); observed (v2) | v2: X5704 for households that filed Schedule F but not Schedule C (X5749, X5747), else 0; unobserved for households filing both and for split-off units with business income. |
| `E00400` tax-exempt interest | observed | X5706. Split-off units: 0 (v2; the SCF's short section has no tax-exempt item). |
| `E26390`, `E26400` estate and trust income, loss | unobserved | Inside X5714. |
| `E01100` capital gain distributions | unobserved | Inside X5712. |
| `E02300` unemployment compensation | observed | X5716, unemployment **and workers' compensation** (one question); split-off units: their unemployment. Alimony and child support (X5718) are left out. |
| `E03150` IRA contributions | unobserved | Not asked as a deduction. |
| `E03210` student loan interest | unobserved | The SCF has balances and payments, not deductible interest. |
| `E03270` self-employed health insurance | unobserved | Not asked. |
| `E17500` medical | unobserved | Itemizers only on the PUF; not asked. |
| `E18400` state and local income or sales tax | unobserved | Not asked. |
| `E18500` real estate tax | unobserved | The SCF asks property tax on the home (X721), but the PUF amount exists only for itemizers, and itemizing is not known in the SCF. |
| `E19200` interest paid | unobserved | Same reason: the SCF has mortgage payments for everyone, the PUF has deductible interest for itemizers only. |
| `E19800`, `E20100` cash and non-cash charity | unobserved | Not asked in 2013 or 2016. |

So the SCF observes 11 of the 39 PUF variables in the first version
(`MARS`, `DSI`, `XTOT`, `AGERANGE`, `E00200`, `E00900`, `P23250`, `E00600`,
`E00300`, `E00400`, `E02300`) and 16 in the second (adding `N24`, `GENDER`,
`E02400`, `E01700`, `E02100`). The second version also has a 40th PUF-side
variable, total Schedule E, observed in both sources (next section).

## Decision: total Schedule E as a 40th variable (John, 9 October)

John chose option (b) below. Version 2 (`full_v2`) adds **`E02000`**,
Schedule E net income or loss (1040 line 17: rent, royalties, partnerships,
S corporations, estates and trusts), observed in both sources:

- PUF: `E02000` from the raw 2015 PUF (not one of test 1's 40 variables;
  the same column as test 2's `rent`).
- SCF: X5714, split between a household's units by earnings share, with the
  public file's masked negatives (-9) kept as small losses, plus split-off
  units' rent income; this is test 2's `rent` exactly.

It sits in the generation order right after `E00900` (Schedule C) and before
the five separate Schedule E pieces (`E26270`, `E25850`, `E25860`, `E26390`,
`E26400`), which stay PUF-only. So the model sees the SCF's Schedule E
amount through the one PUF variable that measures the same thing, and fills
the pieces for SCF records from how they relate to the total on the PUF.
Version 1 (`full`) does not have this variable.

## Population choices

- **PUF records.** All of test 1's 207,692 records and test 1's split,
  including the 6,526 dependent filers (DSI = 1). Test 2 removed them because
  the SCF has none; here `DSI` is a variable, so the model learns that SCF
  units are never dependent filers. When PUF records are completed with the
  SCF source tag, dependent filers are records the model never saw with that
  tag; pieces completing PUF wealth may want to exclude them or report them
  separately.
- **PUF demographic codes.** AGERANGE -1 (not in the demographic file) and
  0 ("not present"), GENDER -1 and EARNSPLIT -1 are unobserved, not
  categories, as in test 2. In test 1 they were categories meaning "large
  income"; here the model has to fill in an age for high-income records,
  which the SCF (where everyone has an age) can teach it. EARNSPLIT 0 ("not
  present", non-joint returns) stays a category.
- **SCF units.** Test 2's filer rule and test 2's household split, reused
  by household id (70/10/20 per wave). Split-off units keep their rows with
  wealth unobserved.

## Open questions for John

1. **Schedule E** (decided 9 October: option b, above). The options were:
   (a) leave the five PUF Schedule E pieces unobserved in the SCF (version
   1); (b) add test 2's `rent` (E02000 on the PUF) as a 40th variable
   observed in both (version 2); (c) split X5714 using the SCF's business
   and rental property sections.
2. **Capital gains and interest/dividend nonzero rates.** As in test 2, the
   PUF has many more small holders (see the table in `G_DATA_REPORT.md`).
3. **EARNSPLIT.** Could be built from the spouses' wages if the PUF booklet's
   band definitions are confirmed (production code reads codes 1, 2, 3 as the
   primary earning all, half, none).
4. **Dependent filers** kept on the PUF side (above).
