---
title: "Step 35: business income in the 2015 PUF and the 2016 SCF"
role: evidence
workstream: rebuild
status: current
updated: 2026-10-18
sot: research/rebuild/design/plan.md
supersedes: []
superseded_by: null
---

# Step 35: business income in the 2015 PUF and the 2016 SCF

The question (John, 18 October): before choosing how SCF business income is
put on the PUF's terms, what do the two files actually show, what does the
SCF actually ask, and what choices are there? This note describes; it
changes no setting and no model, and it does not choose. Full tables:
`$R/step35/tables.md` and `$R/step35/extra.md` (`$R` =
`/nfs/roberts/scratch/pi_nrs36/jar335/minimal`). All figures are weighted,
in dollars of tax year 2015.

## Summary

- **The SCF's two questions follow the 1040's lines.** In the 2016
  questionnaire, X5704 asks for "net annual income from a sole
  proprietorship or a farm" and cites Form 1040 lines 12 and 18 (Schedules
  C and F). X5714 asks for "income from other businesses or investments,
  net rent, trusts, or royalties" and cites line 17, which is the whole of
  Schedule E, partnerships and S corporations included. The 2013, 2019 and
  2022 questionnaires use the same wording. Step 33 described X5704 as
  including "professional practice, business, limited partnership"; that
  is not the 2016 wording.
- **Compared on matching schedules, the profits are close in count.** The
  share of units with a profit is 11.3% on X5704 against 12.7% for
  Schedules C + F, and 9.3% on X5714 against 7.6% for Schedule E. The SCF's
  profits are larger in total: $595 billion against $401 billion on C + F,
  and $1,171 billion against $854 billion on Schedule E. Among units with a
  profit, X5714 and Schedule E have almost the same percentiles from the
  50th to the 90th (within about 11%); below the median the SCF's are
  higher. X5704 is about twice Schedules C + F at the 25th to 90th
  percentiles.
- **Losses are the difference.** 0.9% of SCF tax units report a loss on
  X5704, against 4.6% of returns on C + F. On X5714 the figures are 1.2%
  against 4.1% on Schedule E. On the sum the figures are 1.7% against 7.4%
  of filers, or 6.3% of the PUF side with the crude non-filers. The sizes
  of the losses that are reported are similar in the two files: the median
  loss on the sum is $4,000 in the SCF against $5,380 on the PUF.
- **Where PUF losses come from.** There are 11.1 million returns with a
  net loss on C + E + F. For 45% of them the largest loss is on Schedule C.
  For 29% it is rents and royalties, for 9% partnerships, for 8% Schedule
  F and for 8% S corporations. Half of these returns lose less than $5,000
  in total. The 2% that lose $100,000 or more carry about half of the
  dollars of net loss ($103 billion of $209 billion).
- **When respondents looked at their tax returns, they reported losses
  about as often as the PUF shows.** For 20.1 million households the
  interviewer recorded that the respondent referred to income tax returns
  (X7451). Among those of them with any business amount, 22.6% report a
  loss. Among the other households the figure is 5.9%. On the PUF, 29% of
  filers with any C + E + F amount have a net loss. Households whose return
  had not yet been filed at the interview report a loss at 13.4%.
- **The SCF also under-reports having the schedules at all.** 12.0
  million SCF households say a return of theirs had a Schedule C, against
  24.7 million PUF returns with a nonzero Schedule C. For Schedule E the
  figures are 8.2 million against 17.7 million. Among households who say
  they filed Schedule E, 15.6% report a loss on X5714. On the PUF, 35% of
  returns with a nonzero Schedule E have a loss.
- **The codes.** In the codebook, −1 means "Nothing", a reported zero, so
  step 33's reading of −1 as zero is right. Two other codes are losses of
  unknown size. −2 means "Negative value inferred from data on business".
  −9 marks a negative value that the public file hides when total income
  (X5729) is negative. Step 33 kept −9 as a $9 loss. These are 100
  household rows on X5704 and 114 on X5714; the households are rich (mean
  net worth $3.7 million and $10.1 million in 2022 dollars).
- **Only one PUF is in the data paths, the 2015 PUF** (see section 1).

## 1. What was compared

**PUF.** The 2015 IRS PUF:
`/nfs/roberts/project/pi_nrs36/shared/raw_data/IRS-PUF/v1/2023091310/historical/puf_2015.csv`.
It has 207,692 records after the four aggregate records are dropped, with
150.8 million returns weighted. **No 2016 PUF exists in the repository's
data paths.** I searched the shared `raw_data` and `model_data` trees and
the scratch folder. `raw_data/IRS-PUF` holds only `puf_2015.csv`. Two
things carry "puf" in their names but are not a 2016 IRS PUF:

- `raw_data/Saez-Zucman-PUF/.../pufonline2016.dta` is Piketty, Saez and
  Zucman's public synthetic file;
- `model_data/Tax-Data/.../puf_2022_snapshot.rds` are this pipeline's own
  projected files.

The PUF side of step 33's table E33 adds the crude CPS non-filers: 31.2
million units, whose business income is the CPS sole proprietorship, rent
and rent-loss amounts.

**PUF components.** These come from the fields named in
`config/variable_guide/baseline.csv`:

- Schedule C: E00900.
- Schedule E total: E02000.
- Partnerships: passive and nonpassive income minus passive and nonpassive
  losses minus the section 179 deduction (E25940 + E25980 − E25920 −
  E25960 − E26110).
- S corporations: the same construction (E26170 + E26190 − E26160 −
  E26180 − E26100).
- Rents and royalties: E25850 − E25860.
- Estates and trusts: E26390 − E26400.
- Farm rental (Form 4835): E27200.
- Schedule F: E02100.

The partnership and S corporation pieces add to the PUF's own combined
field E26270 except for $0.1 billion of absolute differences. The parts of
Schedule E add to E02000 except for a remainder of $0.2 billion in absolute
value; it is nonzero on 1.7% of returns and is mostly amounts under $100,
probably rounding.

**SCF.** The 2016 wave asks about 2015 income. The amounts come from the
raw interview file `p16i6.dta`, all five implicates, with weight X42001 / 5.
There are two levels:

- **Households.** The SCF's "family", the primary economic unit (the
  economically dominant person or couple and those financially dependent
  on them): 126.0 million.
- **Tax units.** The units into which the production builder splits each
  household, as used in table E33, with split-off units dropped: 139.6
  million. X5704 is split by each spouse's business income from the labor
  section. X5714 is split by the share of earned income.

**Codes.** −1 is read as zero. −2 and −9 are counted as losses of unknown
size: they count in the share with a loss but are left out of loss
percentiles and dollar totals.

**Groups.** Wages are PUF E00200 and SCF X5702 (for households) or the unit
wages from table E33. Married means MARS = 2 on the PUF and a joint-filing
couple for SCF tax units. For SCF households it is the SCF's MARRIED = 1,
which includes couples living together.

## 2. What the SCF asks

All quotations are from the 2016 codebook
(`$R/step35/codebook/codebk2016.txt`, downloaded from
federalreserve.gov). The codebooks for the 2013, 2019 and 2022 waves,
downloaded beside it, use the same wording for X5704 and X5714.

**General rules.**

- Every amount refers to the "family" (primary economic unit), not to the
  whole household. Income of other household members is asked separately
  and removed.
- In the codebook's words: "a value of zero has only one meaning: that the
  item in question is inapplicable … Whenever zero is a legitimate
  response to a question, a value of −1 is used to signify that value."
- The income section opens with: "we would like to make sure we have the
  best overview of the sources of income you (and your family living
  here) received in 2015." It contains no instruction to consult the tax
  return.
- Each amount in the income section is printed with an "IRS FORM 1040 LINE
  NUMBER" for the interviewer.
- Each amount question ends "before deductions for taxes and anything
  else".

### Income section (tax year 2015, family)

| Item | Wording (exact) | Tax return or income received | Net, and losses | Codes |
|---|---|---|---|---|
| X5703 | "(Other than wages or salaries, did/Did) you (or anyone else) have income or losses from a sole proprietorship or a farm in 2015, before deductions for taxes and anything else?" Line 12, 18. "PROBE IF NECESSARY: DID YOU FILE EITHER A SCHEDULE C OR F?" | screener; mentions losses | | 1 yes, 5 no |
| X5704 | "In total, what was your (family's) net annual income from a sole proprietorship or a farm in 2015, before deductions for taxes and anything else?" Line 12, 18 | income, with the tax line given to the interviewer | "net"; "TO ENTER A LOSS, ENTER THE MINUS SIGN" | −1 "Nothing"; −2 "Negative value inferred from data on business"; −9 "Other negative value and x5729 negative (public data set only)"; 0 Inap. Public file: "NEGATIVE VALUES INCLUDING −2 ARE SET TO −9 WHEN X5729 IS NEGATIVE" |
| X5713 | "Did you (or anyone else) have income from other businesses or investments, net rent, trusts, or royalties?" Line 17. "PROBE IF NECESSARY: DID YOU FILE A SCHEDULE E?" | screener; says "income" only, not "or losses" | | 1 yes, 5 no |
| X5714 | "In total, what was your (family's) annual income from other businesses or investments, net rent, trusts, or royalties in 2015, before deductions for taxes and anything else?" Line 17 | income, with the tax line given to the interviewer | "net" attaches to rent; loss entry allowed ("TO ENTER A LOSS, ENTER THE MINUS SIGN") | −1 "Nothing"; −9 "Negative and X5729 negative (public data set only)"; 0 Inap. |
| X5712 (for comparison) | net gains or losses from mutual funds, stocks, bonds, real estate; lines 13, 14 | | losses allowed | −1 "Nothing"; 0 Inap. |
| X5724 | other income (lines 10, 21); X5725 gives the type, including "36. Net operating loss carryforward" and "23. Trustee fee; executor fee" | | losses allowed | −9 as above |
| X5729 | total income ("What would be the correct total?"); "IF R SAYS TOTAL IS ZERO, ASK WHETHER THERE WAS A LOSS OR IF THE AMOUNT IS ACTUALLY ZERO" | | | −1, −9 |
| X7650, X6766 | income unusually high or low; reasons for low include "1. Low business/self-employment income", "10. Depreciation", "13. Took less money out of business" | | | |

### Tax filing questions (household)

| Item | Wording | Notes |
|---|---|---|
| X5744 | "Did you (or your {husband/wife/partner/spouse} file, or do you expect to file, a Federal Income tax return for 2015?" | 1 filed, 5 no, 6 "YES, NOT *YET* FILED" |
| X5746 | joint, separate, or only one spouse filed | |
| X5747–X5750 (joint return), X7758–X7761 (head's separate), X7762–X7765 (spouse's separate) | "(Did/Will) you … file a schedule C, E, or F with your return? IF YES: Which ones?" The interviewer's definitions: "SCHEDULE C IS FOR SELF EMPLOYMENT INCOME. SCHEDULE E IS FOR REAL ESTATE, PARTNERSHIP, S CORP., ROYALTY, AND TRUST INCOME. SCHEDULE F IS FOR FARM INCOME." | 1 checked, 5 not checked |
| X7451 | interviewer: the respondent referred to "INCOME TAX RETURNS" | 1 yes; 5 referred to other documents; 0 no documents |

No question asks for adjusted gross income, Schedule D, or K-1 amounts.

### Actively managed businesses (up to two in detail; family's ownership)

| Item | Wording | Notes |
|---|---|---|
| X3103 | "Do you (and your family living here) own or share ownership in any privately-held businesses, including farms, professional practices, limited partnerships, private equity, or any other business investments that are not publicly traded?" | |
| X3119, X3219 | "Is it a partnership, a sole-proprietorship, an LLC, a subchapter S corporation, another type of corporation, or something else?" | 1 partnership, 2 sole proprietorship, 3 S corporation, 4 other corporation, 11 limited partnership / LLP (public file merges LLC into 11) |
| X3128, X3172 | percent of the business the family owns; fraction the respondent personally owns | |
| X3131 | gross sales of the business as a whole | |
| X3132, X3232 | "What was the business's total pre-tax net income in 2015?" | **The whole business, not the family's share.** Losses allowed. Interviewer notes map it to tax lines: "PARTNERSHIP: ORDINARY INCOME/LOSS: IRS FORM 1065, LINE 22 / SOLE-PROPRIETORSHIP: NET PROFIT/LOSS: … SCH. C, LINE 31 / S-CORPORATION: NET INCOME: IRS FORM 1120S, LINE 21 / OTHER CORPORATION: TAXABLE INCOME BEFORE NET OPERATING LOSS DEDUCTION: IRS FORM 1120 LINE 30". −1 "Nothing" |
| X3337 | remaining businesses: "what was the total net income you (and your family living here) received from these businesses in 2015?" | the family's receipts |

There is no question on how much the family took out of businesses #1 and
#2, and none on whether that income was already counted in X5704 or X5714.

### Businesses not actively managed

X3410, X3414, X3454, X3418, X3422 and X3430 ask: "What was the total net
income you (and your family living here) received from all such [limited
partnerships / other partnerships / LLCs / subchapter S corporations /
other corporations / other] in 2015?" The amount is the family's received
income, by legal form. Negative values are allowed by the range, but no
instruction mentions losses; only 37 rows are negative.

### Real estate

- X1729/X1730 and X1829/X1830 (properties #1 and #2): "Did you (and your
  family living here) receive any income from this property in 2015?" then
  "How much net income did (you/your family) receive?"
  - The codebook contradicts itself: allowed values run from −999,999,999,
    but it also says "IF < 0, OUT OF RANGE".
  - 130 rows are negative anyway.
- X2009/X2010 and X2019/X2020 (the remaining properties): net income from
  renting them out, zero or positive only.
- No question asks about depreciation.

### Labor section (respondent and spouse, current job)

- **X4106, X4706.** Works for someone else or is self-employed.
- **X4131, X4731.** "(In addition to regular salary, how/How) much (do
  you/does he…) personally receive from the business before taxes? PROBE:
  What did (you/he/she) get in 2015? … In total, how much did you earn in
  2015?"
  - Losses are allowed ("TO ENTER A LOSS…").
  - The amount may be per period (frequency in X4132, X4732).
  - This is the item the builder uses to split X5704 between spouses.

### Trusts and royalties

Trust and managed-account income (X6588, X6590: "How much income did you
… receive in 2015 from these accounts … Please include any earnings that
were reinvested") is asked in the asset section; only zero or positive
amounts are allowed. Royalties have no amount of their own outside X5714.

### How respondents with losses would most likely answer

These follow from the wording. The codebook does not document how
respondents actually behave.

- **A sole proprietorship with a loss.** The wording is clearest here.
  X5703 asks about "income or losses" and probes for Schedule C or F. X5704
  asks for "net" income and gives the loss instruction. A respondent who
  knows the Schedule C result would enter it as a negative number.
- **A partnership or S corporation loss.** By the line references and the
  definition of Schedule E, this belongs in X5714. But the X5713 screener
  asks only about "income from other businesses". A family whose only
  Schedule E result is a loss can answer no, unless the interviewer uses
  the Schedule E probe. Section 4 shows that S corporation owners do in
  fact report mostly on X5714.
- **A rental property that shows a loss after depreciation.**
  - X5714 asks for "net rent" and cites line 17, so a respondent reading
    from the return would enter the loss.
  - A respondent answering from memory is likely to think of rent received
    less cash expenses, which is usually positive or zero, because
    depreciation is not a cash outlay.
  - The real estate section cannot hold the tax-return loss at all.
- **All three.** The phrase "before deductions for taxes and anything else"
  can pull a respondent toward gross receipts, or toward cash drawn from
  the business, rather than tax-return net income.

## 3. The distributions

### 3.1 Matched schedules, all units

The PUF columns are filers only, except the row marked "with non-filers".
"Loss size" is the size of the loss as a positive number, among units with
a loss.

| | Units (m) | Profit % | Loss % | None % | Profits $bn | Losses $bn | Net $bn |
|---|---:|---:|---:|---:|---:|---:|---:|
| PUF filers: Schedules C + F | 150.8 | 12.7 | 4.6 | 82.7 | 400.7 | −84.6 | 316.1 |
| SCF households: X5704 | 126.0 | 11.7 | 1.0 | 87.4 | 595.1 | −12.2 | 583.0 |
| SCF tax units: X5704 split | 139.6 | 11.3 | 0.9 | 87.7 | 595.1 | −12.2 | 583.0 |
| PUF filers: Schedule E | 150.8 | 7.6 | 4.1 | 88.3 | 853.9 | −148.4 | 705.5 |
| SCF households: X5714 | 126.0 | 9.9 | 1.3 | 88.8 | 1,170.9 | −21.8 | 1,149.1 |
| SCF tax units: X5714 split | 139.6 | 9.3 | 1.2 | 89.4 | 1,170.9 | −21.8 | 1,149.1 |
| PUF filers: C + E + F | 150.8 | 18.3 | 7.4 | 74.3 | 1,230.1 | −208.5 | 1,021.6 |
| PUF side with non-filers: C + E + F | 182.0 | 18.1 | 6.3 | 75.7 | 1,362.1 | −209.9 | 1,152.3 |
| SCF households: X5704 + X5714 | 126.0 | 19.4 | 1.8 | 78.8 | 1,758.5 | −26.5 | 1,732.1 |
| SCF tax units: X5704 + X5714 | 139.6 | 18.7 | 1.7 | 79.5 | 1,758.6 | −26.6 | 1,732.1 |

In the SCF rows, the loss share includes about 0.1 point of losses of
unknown size (codes −2 and −9).

Percentiles among units with a profit, and of the size of losses among
units with a loss:

| | Profit P10 | P25 | P50 | P75 | P90 | P99 | Loss P10 | P25 | P50 | P75 | P90 | P99 |
|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| PUF filers: C + F | 630 | 2,250 | 8,060 | 18,100 | 42,700 | 227,100 | 490 | 1,635 | 4,690 | 11,200 | 23,172 | 107,697 |
| SCF units: X5704 | 1,600 | 5,000 | 15,000 | 39,000 | 80,000 | 350,000 | 480 | 850 | 2,700 | 10,000 | 26,000 | 128,000 |
| PUF filers: Schedule E | 350 | 2,030 | 9,670 | 39,200 | 128,800 | 1,034,946 | 480 | 1,890 | 5,410 | 13,200 | 28,400 | 277,376 |
| SCF units: X5714 | 1,000 | 3,800 | 10,000 | 35,000 | 136,530 | 1,600,000 | 660 | 2,300 | 4,200 | 12,000 | 29,700 | 107,348 |
| PUF filers: C + E + F | 610 | 2,430 | 9,440 | 25,300 | 73,600 | 575,471 | 550 | 1,910 | 5,380 | 12,600 | 25,600 | 176,400 |
| PUF side with non-filers | 600 | 2,740 | 10,400 | 27,110 | 69,600 | 504,800 | 520 | 1,880 | 5,320 | 12,300 | 25,100 | 171,800 |
| SCF units: X5704 + X5714 | 1,500 | 4,800 | 14,000 | 41,000 | 117,000 | 1,020,000 | 530 | 1,600 | 4,000 | 11,060 | 26,651 | 62,529 |

Charts: `$R/step35/shares_profit_loss.png` shows the shares with a profit
and a loss. In it, the first two panels use PUF filers and the third uses
the PUF side with non-filers. `$R/step35/percentiles.png` shows the
percentiles on a log scale.

What the two tables show:

- **Schedule E and X5714 match closely among profits.** Their medians are
  $9,670 and $10,000, and their 90th percentiles $128,800 and $136,530.
  X5714 is higher at the 99th percentile ($1.6 million against $1.0
  million). The SCF has 9.3% of units with a profit against 7.6% of
  returns, and $317 billion more in profits.
- **X5704 is about twice Schedules C + F** from the 25th to the 90th
  percentile, with a median of $15,000 against $8,060. The share with a
  profit is a little lower in the SCF (11.3% against 12.7%). The PUF has
  many small Schedule C profits: its 10th percentile is $630.
- **Losses differ in count, not much in size.** Among those who report a
  loss, the median loss is $2,700 against $4,690 on C + F, and $4,200
  against $5,410 on Schedule E. The PUF's largest Schedule E losses are
  larger: the 99th percentile is $277,000 against $107,000.
- **Households and tax units give the same shares to within a point.** The
  split mainly divides cohabiting couples into two units.

### 3.2 PUF components (filers)

| Component | Profit % | Loss % | Profits $bn | Losses $bn | Net $bn | Profit P50 | Profit P90 | Loss P50 | Loss P90 |
|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| Schedule C | 12.4 | 3.9 | 389.4 | −57.9 | 331.5 | 7,990 | 42,200 | 4,050 | 19,393 |
| Schedule E total | 7.6 | 4.1 | 853.9 | −148.4 | 705.5 | 9,670 | 128,800 | 5,410 | 28,400 |
| E: partnerships | 2.2 | 1.3 | 316.8 | −78.5 | 238.3 | 11,500 | 196,100 | 3,170 | 48,708 |
| E: S corporations | 2.3 | 0.8 | 434.1 | −50.5 | 383.6 | 30,600 | 216,592 | 7,355 | 62,533 |
| E: rents and royalties | 4.4 | 3.0 | 97.1 | −46.2 | 51.0 | 3,400 | 29,000 | 5,110 | 21,440 |
| E: estates and trusts | 0.4 | 0.0 | 30.9 | −2.5 | 28.4 | 8,100 | 80,286 | 1,995 | 35,351 |
| E: farm rental (Form 4835) | 0.3 | 0.1 | 5.1 | −0.7 | 4.3 | 4,241 | 29,585 | 2,871 | 13,634 |
| Schedule F | 0.4 | 0.8 | 14.1 | −29.5 | −15.4 | 7,050 | 63,400 | 9,310 | 44,400 |

Schedule F is a net loss in total. Rents and royalties net only $51
billion, because 3.0% of returns have a rental loss against 4.4% with a
profit.

### 3.3 By wage group and marital status

C + E + F on the PUF side (filers and crude non-filers), against X5704 +
X5714 for SCF tax units:

| Group | Units (m), PUF | SCF | Profit %, PUF | SCF | Loss %, PUF | SCF | Net $bn, PUF | SCF | Profit P50, PUF | SCF | Loss P50, PUF | SCF |
|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| No wages | 47.2 | 42.5 | 28.4 | 24.9 | 4.8 | 1.3 | 443.9 | 719.6 | 14,500 | 17,000 | 4,690 | 1,800 |
| Under $25k | 53.8 | 27.2 | 12.4 | 11.8 | 3.0 | 1.0 | 138.7 | 102.4 | 7,074 | 8,097 | 3,710 | 1,600 |
| $25k–75k | 52.6 | 41.1 | 12.8 | 13.5 | 7.2 | 1.2 | 145.3 | 185.6 | 6,769 | 12,000 | 5,802 | 6,200 |
| $75k–200k | 24.3 | 23.6 | 19.7 | 21.1 | 13.0 | 3.0 | 186.0 | 290.8 | 6,788 | 11,104 | 5,740 | 6,000 |
| Over $200k | 4.1 | 5.3 | 30.7 | 34.7 | 13.5 | 7.3 | 238.4 | 433.6 | 20,600 | 35,000 | 9,071 | 5,200 |
| Married | 58.8 | 57.2 | 29.0 | 25.2 | 11.0 | 2.6 | 850.6 | 1,331.6 | 13,200 | 20,000 | 5,397 | 5,000 |
| Not married | 123.2 | 82.5 | 12.8 | 14.3 | 4.0 | 1.1 | 301.6 | 400.5 | 8,650 | 10,000 | 5,195 | 2,500 |

Loss shares on the matched schedules, PUF filers against SCF tax units:

| Group | C + F, PUF | X5704, SCF | Schedule E, PUF | X5714, SCF |
|---|---:|---:|---:|---:|
| No wages | 4.7 | 0.7 | 6.1 | 0.9 |
| Under $25k | 2.3 | 0.7 | 1.6 | 0.6 |
| $25k–75k | 5.2 | 0.8 | 3.3 | 0.7 |
| $75k–200k | 7.4 | 1.3 | 7.7 | 2.3 |
| Over $200k | 7.3 | 3.1 | 9.2 | 6.6 |
| Married | 7.2 | 1.5 | 7.2 | 2.1 |
| Not married | 3.1 | 0.6 | 2.3 | 0.7 |

- The share with a profit is close in every group, within 4 points.
- The loss gap is largest in the middle of the wage distribution: 7% to
  13% of PUF units have a loss with wages of $25,000 to $200,000, against
  1% to 3% in the SCF.
- With wages over $200,000 the SCF captures about half the PUF's loss
  share; on Schedule E it captures 6.6% against 9.2%.
- The by-group tables for every PUF component and for SCF households,
  with all percentiles, are in `tables.md`, sections 1, 3, 4 and 7.

### 3.4 With and without the crude non-filers

The crude non-filers add 31.2 million units:

- 17.0% of them have a profit, almost all CPS sole proprietorship income
  (median $16,438), and 0.9% a loss.
- Together they add $131 billion of net business income.
- They lower the PUF side's loss share from 7.4% to 6.3% and leave the
  percentiles of profits and losses almost unchanged.
- In the SCF, 15.7 million households said they did not file and do not
  expect to file. 7.6% of them have a business amount, and none of those
  has a loss.

## 4. Losses

### 4.1 SCF households that report a loss

| Item | Households (m) | Rows | Loss P10 | P25 | P50 | P75 | P90 | P99 | Total $bn | Mean net worth ($m, 2022 dollars) |
|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| X5704, dollar loss | 1.07 | 525 | 500 | 1,200 | 3,500 | 10,000 | 26,000 | 128,148 | −12.2 | 3.20 |
| X5704, code −9 | 0.13 | 100 | | | | | | | unknown | 3.70 |
| X5714, dollar loss | 1.58 | 681 | 640 | 2,000 | 4,000 | 12,000 | 36,000 | 107,761 | −21.8 | 3.65 |
| X5714, code −9 | 0.08 | 114 | | | | | | | unknown | 10.07 |

- **The codes.** −1 appears on 131 rows of X5704 and 230 rows of X5714
  (0.32 and 0.39 million households). −2 does not appear in the public
  file (no rows).
- **The households behind −9.** 264 rows (0.27 million households) have
  negative total income (X5729 = −9). Their business losses are hidden
  whatever their size.
- **Wealth.** The SCF's loss reporters are rich: $3.2 to $3.7 million of
  mean net worth, against a population mean of under $1 million.

### 4.2 Where PUF losses come from (filers)

11.1 million returns (7.4%) have a net loss on C + E + F, $208.5 billion
in all.

| Component | Returns with a loss in it (m) | Its losses, all returns $bn | Net-loss returns with a loss in it % | Net-loss returns for which it is the largest loss % | Its losses on net-loss returns $bn |
|---|---:|---:|---:|---:|---:|
| Schedule C | 5.92 | −57.9 | 48.7 | 45.5 | −52.3 |
| E: rents and royalties | 4.54 | −46.2 | 32.3 | 29.1 | −36.9 |
| E: partnerships | 1.91 | −78.5 | 11.5 | 9.4 | −66.9 |
| E: S corporations | 1.27 | −50.5 | 8.7 | 7.5 | −45.0 |
| Schedule F | 1.25 | −29.5 | 8.3 | 7.8 | −22.7 |
| E: estates and trusts | 0.06 | −2.5 | 0.3 | 0.2 | −2.0 |
| E: farm rental | 0.11 | −0.7 | 0.5 | 0.5 | −0.6 |

By size of the net loss:

| Net loss | Returns (m) | Share of net-loss returns % | Net loss $bn |
|---|---:|---:|---:|
| up to $1,000 | 1.77 | 15.9 | −0.8 |
| $1,000 to $5,000 | 3.53 | 31.8 | −9.9 |
| $5,000 to $10,000 | 2.32 | 20.9 | −16.7 |
| $10,000 to $25,000 | 2.34 | 21.1 | −37.6 |
| $25,000 to $100,000 | 0.92 | 8.3 | −41.0 |
| $100,000 or more | 0.22 | 2.0 | −102.6 |

- **In count, the losses are mostly small Schedule C and rental losses.**
  Of the returns with a Schedule C loss, 56% lose under $5,000; of the
  returns with a rental loss, 49% do.
- **In dollars, partnerships and S corporations matter most.** They carry
  $112 billion of the $209 billion on net-loss returns, concentrated in
  the few large losses.
- **Netting is small.** 9.1% of returns have a loss in at least one
  component, against 7.4% with a net loss; on Schedule E alone the figures
  are 4.9% against 4.1%. So most loss returns are not a loss in one
  activity offset by a profit in another.
- **Form 8582.** The PUF's F8582 field is set on 2.0 million of the 4.5
  million returns with a rental loss. Its meaning is presumably that Form
  8582 (passive activity loss limitations) was filed, but this is not
  documented in the repository.

### 4.3 What the SCF's other answers say about the missing losses

**Having the schedule at all.** SCF households' own report of the
schedules on their return, against PUF returns with a nonzero amount:

| Schedule | SCF households (m) | % of households | PUF returns (m) | % of returns |
|---|---:|---:|---:|---:|
| C | 12.0 | 9.5 | 24.7 | 16.4 |
| E | 8.2 | 6.5 | 17.7 | 11.7 |
| F | 1.4 | 1.1 | 1.8 | 1.2 |
| any of C, E, F | 19.2 | 15.3 | 38.7 | 25.7 |

Among households who say they had Schedule C or F:

- 87.7% give a profit on X5704 and 8.3% a loss;
- the PUF's ratio among returns with a nonzero C + F is 4.6 / 17.3, that
  is 27% with a loss.

Among households who say they had Schedule E:

- 79.6% give a profit on X5714 and 15.6% a loss;
- on the PUF, 35% of returns with a nonzero Schedule E have a loss.

So even among self-declared filers of the schedule, the SCF has a half to
a third of the PUF's loss rate. In addition, about half of the PUF's
schedule filers do not show up as schedule filers in the SCF at all.

**Respondents who looked at their tax return.** X7451 is the
interviewer's record that the respondent referred to income tax returns.

| X7451 | Households (m) | Some business amount, no loss % | A loss % | Loss among those with any amount % |
|---|---:|---:|---:|---:|
| referred to tax returns | 20.1 | 27.3 | 8.0 | 22.6 |
| referred to other documents only | 29.9 | | | |
| referred to no documents | 72.3 | | | |
| other documents or none, together | 102.2 | 17.3 | 1.1 | 5.9 |
| missing in the file (1,045 rows, kept) | 3.7 | 24.1 | 1.3 | 5.1 |

The loss rate among reporters who used their return (22.6%) is near the
PUF's 29%. This fits losses being missed in recall. It may also partly
reflect selection: households with complicated returns, which more often
have losses, may be more likely to fetch them.

**Timing of filing.** By X5744:

| X5744 | Households (m) | With some business amount % | Loss among those % |
|---|---:|---:|---:|
| filed | 104.8 | 22.3 | 10.5 |
| not yet filed at the interview | 5.5 | 40.9 | 13.4 |
| will not file | 15.7 | 7.6 | 0.0 |

Households that had not yet filed are disproportionately business owners,
probably on extension.

**Legal form of the business against where the income was reported.** For
actively managed businesses #1 and #2 (weighted businesses):

| Legal form | Businesses (m) | Business's own net income a loss (X3132) % | X5704 positive % | X5714 positive % |
|---|---:|---:|---:|---:|
| sole proprietorship | 7.59 | 4.5 | 79.8 | 17.1 |
| S corporation | 1.96 | 6.5 | 31.4 | 70.8 |
| partnership | 1.17 | 5.4 | 46.2 | 53.3 |
| limited partnership, LLP, LLC | 5.61 | 7.3 | 41.7 | 56.9 |
| other corporation | 1.09 | 6.0 | 34.2 | 54.0 |

- **Where each form's income is reported.** Owners of sole proprietorships
  report mainly on X5704, and owners of S corporations mainly on X5714, as
  the line references direct. Partnership and LLC owners split between
  the two.
- **Business-level losses.** The business's own pre-tax net income is a
  loss for only 4.5% to 7.3% of businesses, and is "Nothing" (−1) for 13%
  to 18%.
- **How owners with a business loss answered the income section.** 0.96
  million households have business #1 or #2 with a loss:
  - 56.5% of them report a loss on X5704 or X5714;
  - 26.0% report a positive amount and no loss;
  - 17.5% report neither.

**Real estate.** Of the 17.4 million households with other residential
real estate, 5.9% report a loss on X5714 and 60.3% report nothing there.
The real estate section's net rental income (X1730, X1830) is positive for
6.6 million households and negative for 0.35 million. On the PUF, 6.6
million returns have a rental and royalty profit and 4.5 million a loss.

**Depreciation as a reason for low income.** X6766 code 10 ("Depreciation")
is given by essentially no household (0.00%); code 1 ("Low
business/self-employment income") by 6.5% of loss reporters.

## 5. Differences between the two files

**Concept.**

- **The return's figure.**
  - Schedules C and F are net profit after every deductible expense,
    including depreciation. In 2015 that included 50% bonus depreciation
    and section 179 expensing; the PUF records the section 179 deduction
    that flows through partnerships and S corporations separately (E26100,
    E26110).
  - Schedule E rental and pass-through results are after the limits on
    losses: basis, at-risk, and the passive activity rules of Form 8582.
    Rental losses are allowed up to $25,000 against other income for
    active participants, phased out between $100,000 and $150,000 of
    modified AGI. Losses disallowed in a year are carried forward and
    released later.
  - Net operating loss carryforwards are not in C, E or F (they sit on
    line 21, in the PUF's "other income").
- **The SCF's figure.**
  - The SCF asks for net income "before deductions for taxes and anything
    else". It points the interviewer to the 1040 line but does not ask the
    respondent to use the return. A respondent answering from memory is
    likely to give cash income: profit before depreciation, money drawn,
    rent received less cash costs.
  - The loss evidence in section 4.3 fits that: respondents who used their
    return report losses at near the PUF's rate.
  - The SCF also captures income not reported on returns. IRS tax gap
    studies estimate that about half of sole proprietor income is missing
    from returns (from memory, not checked here). That is one reason X5704
    can sit above Schedules C + F in total ($595 billion against $401
    billion) while having fewer small profits.

**Timing.** Both files measure tax year 2015. The SCF interviews in 2016,
and 5.5 million households, many of them business owners, had not yet
filed. Their figure is an estimate made before the return was done.

**Unit.**

- The PUF unit is a return; the SCF unit is a family. The builder splits
  the family into tax units, giving 139.6 million units against 150.8
  million returns (182.0 million with the crude non-filers).
- The split matters little for shares: households and units differ by
  under a point.
- It does matter for who holds the amount. X5704 is split by each spouse's
  business earnings from the labor section, and X5714 by the share of
  earned income, not by ownership.

**Who is in each file.**

- **The PUF.** It has filers only, including dependent filers. Table E33
  adds 31.2 million crude CPS non-filers, 17% of whom have business
  profits.
- **The SCF.** It covers all families, filers or not:
  - 15.7 million households say they do not file;
  - split-off units are dropped in table E33;
  - the Forbes 400 are excluded by design;
  - wealthy families are oversampled through the list sample, which is
    one reason X5714's 99th percentile is above Schedule E's.

**Public-file editing.** On the public SCF file, business losses are hidden
as −9 when total income is negative (0.27 million households). These are
likely to be the largest losses relative to income.

## 6. Settings John could choose from (described, not chosen)

**A. Which items are shared.**

1. **One item: C + E + F against X5704 + X5714 (step 33).**
   - Assumes only that the total is comparable.
   - Step 33's reason for one item was that X5714 is far above Schedule E
     parts I and III. That reason does not hold once partnerships and S
     corporations are counted in Schedule E, as the question's line 17
     reference says they are.
2. **Two items: C + F against X5704, and Schedule E against X5714.**
   - Assumes respondents follow the line definitions. The legal-form table
     supports this for sole proprietorships (80% on X5704) and S
     corporations (71% on X5714), less so for partnerships and LLCs
     (split).
   - The two items behave differently in dollars, which bears on setting
     C: X5714 matches Schedule E among profits, while X5704 is about twice
     C + F.
3. **Finer PUF pieces** (partnerships, S corporations and rents as
   PUF-owned parts under a shared total). This is the capital gains
   pattern, which step 34 is building.

**B. How the gap in losses is read.**

1. **Taken as reported (step 33).**
   - Assumes the SCF's 1.7% of loss units stand for the PUF's 6.3%, so
     the model learns everything about loss units from the SCF's few, rich
     reporters.
   - Section 4.3 says the SCF misses most losses, so this assumption fails
     on the evidence.
2. **Hidden loss reporters, filled in by the cell method.**
   - SCF units reporting nothing would be read as possibly hiding a loss.
     The PUF sets how many hidden losses there are in each cell; the
     model picks which units.
   - Assumes the hidden losses sit among the zero reporters and resemble
     the units that do report losses.
   - Two findings bear on this. The tax-return users' loss rate (22.6%)
     suggests most losses are recall gaps. But 26% of owners whose
     business had a loss reported a positive amount on X5704 or X5714. So
     some hidden losses sit among positive reporters, which the cell
     method as built (zeros only) would not reach.
   - It would also need losses as a range in the method, as capital gains
     has.
3. **Loss and nothing merged on the SCF side.**
   - Treat SCF losses and zeros as one group ("no positive business
     income"), and take only positives as reported. What a loss implies
     for net worth is then learned from no one: the model's loss units
     would get the net worth of the merged group.
   - Assumes the SCF carries no usable information about losses.
4. **A concept conversion from other SCF questions.**
   - For actively managed businesses #1 and #2, the business's own
     pre-tax net income (X3132, mapped by the interviewer notes to Form
     1065 line 22, Schedule C line 31 and Form 1120S line 21, all after
     depreciation) times the family's ownership share (X3128) gives a
     figure close to the tax-return result for pass-through businesses.
     It could replace or check X5704 and X5714 for those owners.
   - Assumes X3132 is answered on the tax concept and that C corporations
     (form 4) are excluded, since their income does not flow through.
   - It covers only the first two businesses. The other business and real
     estate questions ask for income received, in practice zero or
     positive, and ask nothing about depreciation. So rental losses cannot
     be rebuilt this way.
   - Step 33's net worth problem came mainly from loss units, which this
     route reaches only for active owners.
5. **Leave it to the tilt.**
   - Assumes the reweighting can correct net worth among loss units.
   - The tilt can only reweight records; it cannot change the net worth
     drawn for a loss unit. Step 33 found the tilt had to move the file
     18% more because of this.
6. **The −9 code.** Choices for the 0.2 million households whose loss the
   public file hides:
   - a loss of unknown size, with the size drawn from the PUF's
     distribution in the cell;
   - missing;
   - zero;
   - −$9 (step 33, unintended).
   −1 as zero is right by the codebook.

**C. The dollar rule** (the setting in `33_settings_scf.json`).

- **Rank (step 33).** Keeps the SCF's shares and gives SCF holders the
  PUF's dollar shape. It says nothing about whether the SCF's higher
  amounts are real.
- **Unchanged.**
  - It fits X5714 against Schedule E among profits from the median up:
    the percentiles agree within about 11% from the 50th to the 90th. Below the
    median the SCF's profits are higher ($3,800 against $2,030 at the
    25th percentile).
  - It does not fit X5704 against C + F, which differ by about a factor
    of two. A factor (the third choice) of about 0.5 on X5704 would line
    up its 25th to 90th percentiles.
  - Loss sizes are close in both pairs, so "unchanged" for losses and
    "rank" for losses give similar answers.
- **Two items.** If business income becomes two items (A2), each can have
  its own rule.

**D. The split of X5714 within households.** X5714 is split by the share
of earned income. A split by personal ownership (X3172) or equally
between spouses are the other choices. This matters only for couples
living together who are not married, which is where the split creates two
units.

## 7. Files

**Code.** All in `research/rebuild/minimal/`; all computation ran through
sbatch.

- `35_scf_raw.R`: extracts the raw 2016 SCF items and labels.
- `35_tables.py`: the tables and charts.
- `35_extra.py`: the supplementary counts.
- `35_cpu.sbatch`: the job script.

**Scratch outputs.** All in `$R/step35/`.

- `tables.md`: every table, including all by-group tables.
- `extra.md`.
- `puf_filers.csv`, `puf_side.csv`, `scf_households.csv`,
  `scf_units.csv`, `side_by_side.csv`.
- `shares_profit_loss.png`, `percentiles.png`.
- `scf2016_labels.tsv`: Stata labels of every SCF variable.
- `scf2016_hh_items.csv.gz`.
- `codebook/codebk{2013,2016,2019,2022}.txt`.
- `x5744.py`: the filing-timing counts. Its output is in
  `logs/step35/x574435_30006861.out`.

**Missing values.**

- None in the PUF fields used or in the non-filer fields used.
- In the SCF extract, four columns have missing values (X8490, X6532,
  X7451, X7454; 1,045 rows each for X7451). Only X7451 is used, and its
  missing rows are shown as their own row.

## John's choices so far (18 October 2026, evening)

- Two shared items, as the question wording gives: Schedules C + F
  against X5704; Schedule E against X5714.
- The code −9 ("a loss of unknown size") becomes a loss of typical size:
  the median reported SCF loss on that item.
- Still open: how to treat the loss gap (as reported; fill in hidden
  losses with the cell method; merge loss and nothing; rebuild from the
  business section) and the dollar rule for each item.
