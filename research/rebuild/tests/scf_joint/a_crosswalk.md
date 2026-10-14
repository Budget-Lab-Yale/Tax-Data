---
title: "Test 2 — PUF-to-SCF crosswalk for the shared variables (proposal for review)"
role: method
workstream: rebuild
status: current
updated: 2026-10-09
sot: research/rebuild/tests/scf_joint/SPEC.md
supersedes: []
superseded_by: null
---

# PUF-to-SCF crosswalk for the shared variables

This note proposes how each shared variable is defined in the 2015 PUF and
in the SCF tax units, for John's review. The datasets in
`$S/data/` (`expB`, and the SCF side of every experiment A dataset) are built
with the definitions marked **used**. Numbers come from
`a_prepare.py crosswalk` (table: `$S/data/crosswalk_table.csv` and
`crosswalk_demographics.csv`); all are weighted aggregates.

## How the SCF side is built

The SCF asks each household for its income in the previous calendar year by
source, and each question names the Form 1040 lines it corresponds to (SCF
2016 codebook, items X5701 to X5725). The production tax-unit builder takes
its incomes from the SCF summary file (SCFP), which combines some of these
items; here they are taken from the raw interview items instead, for three
reasons found while checking the waves:

1. The SCFP files on the cluster are in **2022 dollars** for every wave. SCFP
   wages are exactly 1.2493 times the raw 2015 interview amount in the 2016
   wave, 1.2921 times the 2012 amount in the 2013 wave, and 1.1802 times in
   2019 (same ratio for every record). The raw items are in the tax year's own
   dollars, which is what the PUF comparison needs.
2. SCFP's business income (`BUSSEFARMINC`) is Schedule C and F income **plus**
   the Schedule E item (rent, royalties, partnerships, S corporations, trusts):
   its ratio to raw X5704 + X5714 is exactly the dollar factor above. So
   production's `business_scf` includes Schedule E income.
3. SCFP's `RENT` is **rent paid** by renters (it sits among the spending
   variables in the file), not rental income. Production's `rent_scf` column
   (`stage1_scf_tax_units.R`, line 692) is therefore rent paid. The column is
   used only by an ablation diagnostic, not by `wealth.R`, but the name is
   wrong.

Each SCF item is a household (primary economic unit) amount; the builder
splits it between the two units of a couple filing separately (wages by each
spouse's own wages, business by own business income, the rest by the
spouses' shares of earned income) and adds the separately reported income of
other adults in the household who form their own tax units ("split-off"
units). The split-off units' income comes from a short section of the
interview with fewer categories.

Two public-file codes need care. A business (X5704) or Schedule E (X5714)
amount of -9 means "negative, amount withheld, and total income negative";
-1 means "nothing" and is set to zero. In the 2016 filer records there are 135
records (tax unit by implicate) with business = -9 and 144 with Schedule E =
-9, kept as small negative values, so
they land in the negative tokens at the bottom of the negative range; 2013
has 80 and 89 such records.

## The seven income categories

Totals in billions of 2015 dollars (SCF 2013 values, which are tax-year 2012
amounts, scaled by CPI-U 237.017 / 229.594 for this table only). PUF: the
2015 PUF without its four aggregate records and without dependent filers
(141.6 million returns). SCF: filer tax units (140.2 million in 2016, 132.3
million in 2013).

| Category | PUF lines (used) | SCF items (used) | PUF total | SCF 2016 total | SCF 2013 total | PUF share nonzero | SCF 2016 share nonzero |
|---|---|---|---|---|---|---|---|
| `wages` | E00200 wages (1040 line 7) | X5702 wages (line 7) | 7,086 | 7,941 | 6,948 | 82.2% | 79.9% |
| `business` | E00900 Schedule C + E02100 Schedule F (lines 12, 18) | X5704 sole proprietorship or farm (lines 12, 18) | 316 | 576 | 410 | 18.1% | 11.6% |
| `int_div` | E00300 taxable interest + E00400 tax-exempt interest + E00600 ordinary dividends (lines 8a, 8b, 9a) | X5706 tax-exempt + X5708 other interest + X5710 dividends (lines 8a, 8b, 9a/b) | 387 | 422 | 368 | 34.8% | 18.0% |
| `capital_gains` | P22250 + P23250 (Schedule D net short- and long-term gain or loss, before the $3,000 loss limit) + E01100 (capital gain distributions reported directly on the 1040) + E01200 (other gains, Form 4797; line 14) | X5712 net gains or losses from mutual funds, stocks, bonds, real estate (lines 13, 14; Schedule D line 16 if a loss) | 604 | 678 | 494 | 15.8% | 8.3% |
| `rent` | E02000 Schedule E net income or loss (line 17: rent, royalties, partnerships, S corporations, estates, trusts) | X5714 other businesses or investments, net rent, trusts, royalties (line 17) | 704 | 1,142 | 980 | 12.4% | 10.2% |
| `ss_pens` | E01700 taxable pensions and annuities + E02400 gross Social Security | X5722 Social Security, pensions, annuities, other disability or retirement programs (lines 16a, 20a; excludes IRA and 401(k) withdrawals) | 1,295 | 1,275 | 1,063 | 26.9% | 26.8% |
| `ui_other` | E02300 unemployment compensation + E00800 alimony received (lines 19, 11) | X5716 unemployment and workers' compensation + X5718 child support and alimony (lines 19, 11) | 35 | 57 | 102 | 4.6% | 7.2% |

The PUF check that E02000 is the sum of its parts: E25850 - E25860 + E26270 +
E26390 - E26400 totals 700 billion against 704 for E02000 (the remainder is
other Schedule E items such as REMIC income).

Alternatives computed (PUF, excluding dependent filers):

| Alternative | PUF total | Share nonzero | Why not used |
|---|---|---|---|
| `ss_pens` with E01500 total pensions instead of taxable E01700 | 1,781 | 28.4% | Line 16a includes direct rollovers between retirement accounts, which are not income in the SCF's sense; it puts the PUF 40% above the SCF. |
| `ss_pens` as E01500 total pensions + E02400 + E01400 taxable IRA distributions | 2,032 | 30.2% | The SCF question excludes IRA and 401(k) withdrawals (asked elsewhere). See "Open questions". |
| `capital_gains` as 1040 line 13 (E01000) + line 14 | 613 | 14.3% | Line 13 caps losses at $3,000; the SCF asks for the net gain or loss and points to Schedule D line 16 for losses. |
| `ui_other` plus E00700 taxable state refunds (the SCF lists line 10 under "other income") | 66 | 18.3% | Refund recipients are mostly itemizers with middle and high incomes; on the SCF side the matching "other income" item is mixed with welfare and other transfers. |
| SCF `ui_other` including welfare (X5720) and other income (X5724), the SCFP definition | SCF 2016: 424 | 18.8% | Most of it (TANF, SNAP, SSI, other) has no tax-return counterpart; with ranks, the same token would mean "receives transfers" in the SCF and "has unemployment or alimony" in the PUF. |

IRA distributions (E01400, 251 billion, 10.0% of returns) are in none of
the seven categories on the PUF side, matching the SCF definitions.

## Where the two sources differ a lot

Ranks absorb differences in level (scaling, under- or over-reporting that
keeps the order of records), but not differences in **who has a nonzero
amount**, since exact zero is its own token. The share-nonzero gaps are the
ones that matter for the model:

1. **Interest and dividends: 34.8% of PUF returns, 18.0% of SCF filer units.**
   The PUF records every 1099-INT and 1099-DIV, including very small amounts;
   SCF respondents leave out small interest. With ranks, the PUF records with
   tiny amounts are placed in the low-rank bins among holders, and SCF
   holders are on average further up the distribution, so "lowest 10% of
   holders" means a much smaller amount in the PUF than in the SCF.
2. **Capital gains: 15.8% against 8.3%.** The PUF includes mutual-fund
   capital gain distributions, which many small fund holders receive and
   may not think of as gains, and many small losses.
3. **Business: 18.1% against 11.6%, and 4.9% of PUF returns report a loss
   against 0.9% of SCF units.** The PUF has many small Schedule C businesses
   and losses; the SCF total is nearly twice the PUF total. Part of the gap may
   be S-corporation or partnership income that respondents report as
   "business" (the SCF business section covers actively managed businesses of
   all legal forms); that would also explain why the SCF Schedule E total is
   62% above the PUF's.
4. **Wages** are 12% higher in the SCF 2016 than on the PUF, with similar
   shares nonzero; a level difference the ranks absorb.
5. **ui_other** is small in both and means slightly different things
   (workers' compensation and child support appear only in the SCF).
6. **Business and Schedule E losses** in the SCF public file are partly masked
   (the -9 code above), so the SCF's negative-value tokens for these two are
   less informative than the PUF's.

Experiment A's variants test three of these kinds of difference directly
(noise, income moved between categories, capital income not reported).

## Demographic variables

| | PUF 2015 (no dependent filers) | SCF 2013 filers | SCF 2016 filers |
|---|---|---|---|
| Returns or units, millions | 141.6 | 132.3 | 140.2 |
| Married filing jointly | 38.4% | 40.8% | 39.1% |
| Dependents 0 / 1 / 2 / 3+ | 62.5 / 17.3 / 13.1 / 7.0% | 65.1 / 15.0 / 9.6 / 10.3% | 69.2 / 12.8 / 8.8 / 9.3% |
| Age range under 26 / 26-34 / 35-44 / 45-54 / 55-64 / 65+ (PUF: share of records with an age range) | 12.0 / 19.3 / 17.9 / 17.5 / 16.0 / 17.1% | 11.7 / 16.8 / 18.3 / 18.9 / 16.8 / 17.6% | 13.9 / 15.9 / 17.1 / 17.7 / 16.5 / 19.0% |

- `married`: PUF MARS = 2; SCF builder filing status 2 (a married couple kept
  as one unit). SCF couples where only one spouse filed (X5746 = 3 or 4) stay
  one unit and count as married filing jointly here; a married couple filing
  separately (X5746 = 2) becomes two unmarried-looking units.
- `n_dep`: PUF total exemptions (XTOT) minus 1, minus 1 more for joint returns,
  top-coded at 3 (11 records below zero set to 0). SCF: the builder's count of
  the household's children under 19, plus one if the household supports a
  child living elsewhere, divided between the two units of a split couple.
  The definitions differ: the PUF counts college students up to 23 and other
  relatives; the SCF counts only children under 19. The SCF has fewer units
  with one or two dependents and more with three or more.
- `age_range`: PUF AGERANGE (codes 1-6 as in `src/imputations/ages.R`); records
  absent from the demographic file (87,950, all with weight under 200: the
  high-income strata) and the 224 coded 0 ("not present") are unobserved (-1).
  SCF: age of the tax unit's head (the respondent, the spouse for the spouse's
  separate unit, the member's own age for a split-off unit), measured at the
  interview (2013 or 2016), about one year after the tax year, so SCF ages run
  slightly older than the PUF's. The PLAN mentioned the older spouse's age;
  SPEC says the head's; the head's is used.

## Dependent filers on the PUF

PUF returns with DSI = 1 (a dependent of another taxpayer: 6,526 records, 9.3
million weighted) are removed. The SCF has no counterpart: a household
member who is financially dependent on the household is never made into a
separate tax unit by the builder (only financially independent members are),
so the SCF filer population has no dependent filers. Removing them gives 141.6
million PUF returns against 140.2 million SCF 2016 filer units.

## Open questions for John

0. **Split-off adults** (decided 9 October: kept, wealth unobserved; financially independent adults living in an SCF
   household, 13% of 2016 filer units) have zero wealth by construction and
   no filing answer; see `DATA_REPORT.md`, section 2.

1. **IRA and 401(k) withdrawals** are left out of both sides. They are
   observed in both (PUF E01400, SCF pension-account withdrawal items) and are
   informative about retirement-account wealth; adding them as an eighth
   shared variable, or into `ss_pens` on both sides, is a small change.
2. **Schedule E and business** could be pooled into one shared variable if the
   SCF's reporting of S-corporation and partnership income as "business" is a
   concern; the pooled totals are 1,020 (PUF) against 1,718 (SCF 2016).
3. **Interest and dividends, capital gains**: the share-nonzero gaps could be
   reduced by treating PUF amounts below a small floor (for example $100) as
   zero. Not done, since it changes the PUF side by a rule we would have to
   defend; flagged for decision.
