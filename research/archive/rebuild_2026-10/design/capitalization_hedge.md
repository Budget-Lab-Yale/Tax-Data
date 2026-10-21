---
title: "Capitalized wealth beside the SCF-trained net worth: what it is, how it could enter the rebuild, a first experiment"
role: design
workstream: rebuild
status: draft
updated: 2026-10-18
sot: research/rebuild/design/plan.md
supersedes: []
superseded_by: null
---

# Capitalized wealth beside the SCF-trained net worth

Written 18 October 2026 for John's question: what would it look like if
some of the file's wealth came from the capitalization method of Saez and
Zucman (2016) and Smith, Zidar and Zwick (2023) instead of the model
trained on the SCF? This note expands the "Capitalized wealth as a second
version" item in `plan.md`, "Future refinements". Nothing here is decided.
Two small read-only checks were run for it (section 4); their scripts and
output are in scratch `capcheck/`.

## Summary

1. **The method.** Divide each capital income item on a return by a rate
   of return for its asset class: interest by a fixed-income yield,
   dividends by an equity yield, business profits by a business yield, and
   so on. Saez and Zucman use one rate per class, chosen so that the class
   adds up to the Federal Reserve's household balance sheet. Smith, Zidar
   and Zwick let the rate rise with wealth for interest, and value private
   businesses from firm data. At the top the two disagree by a lot: in
   2016 the top 0.1% hold 20.4% of wealth with one rate per class and
   15.7% with varying rates.
2. **What the rebuild's file supports.** Capitalization needs interest,
   dividends and capital gains, which the file has. It also needs
   tax-exempt interest, which step 33 is adding, and business,
   partnership, rental and trust income as separate items, each split
   into profit and loss. Step 33 adds these only as one combined item,
   which is too coarse to capitalize. Owner-occupied housing, pensions
   and most debt produce no taxable income, so they cannot be
   capitalized from this file. Capitalization therefore gives
   about half of net worth (the income-producing part) and needs the
   SCF-trained model, or the SCF's own patterns, for the other half.
3. **A first look shows a gap at the top.** On the step 31 file, among
   records whose capitalized interest and dividends alone come to $10
   million or more, the model's net worth is below even a cautious
   capitalized value (high yields, so low wealth) for about 30% of units
   (24% to 33% across seeds). In the SCF itself the same test gives
   about 15%. So the SCF-trained draw holds less wealth for large
   capital incomes than the SCF does. That part of the gap points to the
   model, not to a disagreement between the SCF and capitalization.
4. **How to use it.** A 50/50 mixture of records gives, for totals and
   counts, exactly the average of the two methods. For top shares and
   thresholds it gives a number that belongs to neither method. Carrying
   both as separate columns gives the same averages for totals and
   counts, and also shows the range. Averaging the two values on each record understates
   any tax with a high exemption. The most useful places for
   capitalization are as a check on the top, as a source of tilt targets
   above the SCF's 99th percentile, and as a second wealth column for
   Tax-Simulator.
5. **Proposed first experiment** (section 5): compute capitalized wealth
   by asset class on the step 33 file and on the SCF waves, with both
   kinds of rates. Compare the three (SCF, model, capitalization) by
   capitalized-wealth group. Then score a stylized wealth tax five ways.

## 1. What the capitalization method does

A **capitalization factor** is the number of dollars of wealth per dollar
of income from an asset: one over its rate of return. A bond paying 4% has
a factor of 25.

### Saez and Zucman (2016)

Their steps, for each year:

1. Map each capital income line on the 1040 to an asset class on the
   household balance sheet of the Financial Accounts of the United States.
   They use eight classes from tax data: corporate equities, taxable fixed
   income, tax-exempt bonds, tenant-occupied housing, mortgages, sole
   proprietorships, partnerships, S corporation equity. Estate and trust
   income is a ninth flow.
2. For each class, the factor is the class total in the Financial
   Accounts divided by the total of the matching income on all returns.
   In 2000 they find about $5 trillion of interest-bearing assets
   against $200 billion of taxable interest, a factor of 25 (a 4% return).
   Rents had a factor of 37 that year and partnership profits 7. Only
   positive business profits are capitalized.
3. Every return gets the same factor within a class (the "equal returns"
   assumption). Wealth in a class is then shared out across returns in
   proportion to that class's income. The method does not need the true
   rate of return. It needs the income to be distributed like the wealth.
4. For equities, dividends rank households and dividends plus realized
   gains set the group totals (their "mixed method"). The top 0.1%
   share in 2012 is 21.6% using dividends only, 23.6% using dividends
   and gains, and 22.1% using the mixed method.
5. Assets with no taxable income are allocated by other rules. Owner-
   occupied housing comes from property taxes paid by itemizers (one
   effective tax rate for everyone; itemizers assumed to own 75% of
   homes). Mortgages come from mortgage interest (itemizers hold 80%).
   Pensions are allocated 60% by pension income and 40% by wages above
   the median. Life insurance is distributed like pensions. Currency,
   non-interest deposits and non-mortgage debt follow the SCF. Trust
   wealth follows Schedule E trust income.
6. Checks: foundations, which report both income and wealth to the IRS.
   Capitalizing their income gives the top 1% of foundations 62.2% of
   foundation wealth, against a true 62.8%. Matched estate and income tax
   returns and the SCF were also used as checks.

Their argument for equal returns is that return differences matter little
when wealth differences are much larger. They show that a moderate spread
of returns that is unrelated to wealth overstates top shares by under
about 9%.

### Smith, Zidar and Zwick (2023)

They keep the frame and change the factors where returns differ with
wealth:

- **Interest.** They linked every 1099-INT and K-1 interest payment to its
  owner (2001–2016) and split taxable interest by source. The rich earn
  much more of their interest from high-yield sources, such as
  partnerships holding distressed or mezzanine debt. In 2016 the yield is
  about 1% for the bottom 99.9%, 1.6% for P99.9–99.99, and 3.8% for the
  top 0.01%, which is 3.5 times the average. The equal-returns factor for
  interest was 126 in 2016. The factors at Moody's Aaa and 10-year
  Treasury rates would be 27 and 54. Fixed-income wealth of the top 0.1%
  falls from 6.4% of all household wealth under equal returns to 2.2%.
- **Pass-through businesses.** They value each firm from its sales,
  assets and a profit measure, using four-digit industry valuation
  multiples from public companies, minus 10% for illiquidity. For firms
  with profits under $50 million, only 25% of profits counts as a return
  to capital; the rest is treated as the owner's labor. Owners get a
  share of firm value by their ownership. Sole proprietorships keep simple
  capitalization because there is no industry data for them.
- **C corporation equity.** They capitalize 0.9 × dividends + 0.1 ×
  realized gains, with the weights fit to SCF top equity shares. Letting
  returns differ by wealth matters little for equities.
- **Housing.** Each state gets its own effective property tax rate. A
  dollar of property tax in California means about four times as much
  house as a dollar in Illinois.
- **Pensions.** Wages and pension income are capitalized separately by
  age group. Unfunded defined-benefit plans and Social Security are
  supplementary series only.

Their 2016 results, individuals (couples split equally), all three
series on the same Financial Accounts totals:

| Group | Smith, Zidar, Zwick | Equal returns | Saez–Zucman revised (2020) |
|---|---:|---:|---:|
| Top 1% share | 33.7% | 38.9% | 36.3% |
| Top 0.1% share | 15.7% | 20.4% | 18.4% |
| Top 0.01% share | 7.1% | 10.5% | 9.3% |
| Average wealth, top 0.1% | $50.3M | $65.1M | $58.9M |
| Average wealth, top 0.01% | $228M | $337M | $297M |
| Bottom 90% share | 31.4% | 26.6% | 26.6% |

Most of the gap between their series and equal returns comes from
interest. For the rebuild, the useful point is that equal returns and
varying returns differ by about 30% in the average wealth of the top
0.1% and about 50% at the top 0.01%. A wealth tax depends mostly on those
groups.

For comparison, the Distributional Financial Accounts (households, built
from the SCF scaled to the Financial Accounts, with the Forbes 400 added)
give the top 1% 31.4% and the top 0.1% 12.5% in 2019Q4 (local copy,
`resources/dfa/`). The rebuild's file after the tilt gives the top 1% of
tax units 42.7%, against 38.4% in the SCF's own tax units. Units and
concepts differ across all of these, so the levels are not directly
comparable.

### Where the methods are weak

- **Rates of return are assumed, not observed.** With interest rates
  near zero, small errors in the rate become large errors in wealth.
  Kopczuk (2015) made this point about the rise in top shares after
  2000. Bricker, Henriques, Krimmel and Sabelhaus (2016) found lower top
  shares from the SCF. Fagereng, Guiso, Malacrino and Pistaferri (2020)
  show, with Norwegian registry data, that returns rise with wealth and
  persist.
- **Realized gains are lumpy.** A one-time sale makes a household look
  rich in that year. Unrealized gains, which dominate the very top, do
  not appear at all.
- **Business income is not business value.** Schedule C and K-1 income
  mixes labor and capital. Smith, Zidar and Zwick find the SCF shows far
  more private business wealth for the top 1% than the Financial
  Accounts, mostly in P99–99.9. Scaling private business to the
  Financial Accounts totals removes their whole gap with the SCF at the
  top 1%.
- **Assets with no taxable income are allocated by rules.** These are
  owner-occupied housing, mortgages, pensions (401(k) balances are not
  reported to the IRS), life insurance, currency and consumer debt.
  Together they are most of the wealth of the bottom 90%: pensions alone
  are about two thirds of it in Smith, Zidar and Zwick. Since 2018 the housing rule is
  much weaker: itemizers fell from 44.6 million returns in 2015 to 17.3
  million in 2019 (SOI Table 1.4), so property tax and mortgage interest
  are seen for about 11% of returns.
- **Income not reported to the IRS.** Offshore wealth is missing; Saez
  and Zucman add it as a supplementary series.

## 2. What the rebuild's file can support

### Income items needed

| Item | Asset class | In the file now | Step 33 |
|---|---|---|---|
| Taxable interest | Taxable fixed income | yes | — |
| Ordinary dividends | Equities (and bond and money market funds, through nonqualified dividends) | yes | — |
| Qualified dividends, separately | Separates stock dividends from fund dividends (Smith, Zidar and Zwick capitalize bond funds through nonqualified dividends) | no | not listed |
| Capital gains (four pieces) | Equities (weight 0.1, or for ranking only) | yes | — |
| Tax-exempt interest | Municipal bonds | no | yes (PUF and SCF) |
| Schedule C profit | Sole proprietorships | no | inside one "business income" item |
| Partnership and S corporation income | Partnerships, S corporations | no | inside "business income" (Schedule E total) |
| Rents and royalties | Rental real estate | no | inside "business income" (Schedule E total) |
| Estate and trust income | Trusts | no | inside "business income" (Schedule E total) |
| Taxable pensions, IRA distributions, taxable Social Security; gross pensions plus Social Security | Pensions (allocation, with wages) | no | yes; the SCF side has the gross item only |
| Age | Pension allocation by age | no | no |
| Property taxes, mortgage interest | Housing, mortgages | no (deferred to the tax-law step) | no |

One detail matters. Step 33 (`minimal/33_tables.py`) makes one item,
business income, out of Schedule C, the whole of Schedule E (rents,
royalties, partnerships, S corporations, estates and trusts) and Schedule
F, net of losses. The SCF side gets the matching sum of two SCF items.
That is fine for training. For capitalization it is too coarse. Saez and
Zucman's factors differ a lot by class (rents 37 and partnership profits
7 in 2000), and only profits are capitalized, not losses. Capitalizing
business properly needs at least Schedule C, partnership and S
corporation income, and rents and royalties as separate items, each
split into profit and loss. The PUF has all of these (for example E00900,
E26270, and the rent and royalty fields).

### Factors for 2019

The equal-returns factors come from dividing the class totals in the
Federal Reserve's balance sheet for 2019Q4 by the SOI totals for tax
year 2019. A rough version from the local Distributional Financial
Accounts copy and SOI Table 1.4:

| Class | Balance sheet, 2019Q4 | Income, TY2019 | Factor | Implied return |
|---|---:|---:|---:|---:|
| Taxable fixed income (deposits, taxable bonds, loans; munis taken out at about $1.8T) | about $13.7T | interest $153.3B | about 89 | 1.1% |
| Equities and mutual funds (0.9 × dividends + 0.1 × gains) | $29.3T | $386.7B | about 76 | 1.3% |
| The same, dividends only | $29.3T | $331.8B | about 88 | 1.1% |
| Private noncorporate business, including household rental property | $12.1T | positive Schedule C, partnership and S corporation, and rental income $1,412B | about 8.5 | crude: S corporation equity sits in equities in this balance sheet |

These are approximate. In particular, the municipal bond holding is a
round figure that still needs to be checked. The experiment should take
the factors class by class from the Piketty–Saez–Zucman files, which
run to 2019, and use the table above only as a check.

One property follows from the tilt. The tilt makes the file's filers hit
the IRS totals of interest, dividends and gains by AGI bracket exactly.
With factors defined as balance-sheet total over IRS total, the
capitalized class totals on the file then match the balance sheet
automatically, apart from non-filers' income. The step 31 file gives
$14.3 trillion of fixed income and $29.9 trillion of equities against
$13.7 trillion and $29.3 trillion. So capitalization on
this file says nothing new about totals. What it adds is who holds the
wealth.

### Concepts have to match first

The rebuild's net worth is the SCF's. The SCF leaves out
defined-benefit pension wealth and most consumer durables. The
Financial Accounts include defined-benefit entitlements ($17.2 trillion
in 2019Q4 in the distributional accounts) and durables ($5.7 trillion).
Their private business value is far below the SCF's. Mixing or averaging
the two methods is meaningful only after both are put on one concept,
class by class. The natural choice is the SCF's concept, since the
targets are in it. That means capitalizing only the classes the SCF also
counts, and not using the balance-sheet total for net worth as a whole.

## 3. Ways to use it

Notation: for a record with income items x, the SCF-trained model gives a
random draw W_S from its distribution of net worth given x; capitalization
gives a fixed value for the income-producing classes. W_C is a full net
worth built from it, by adding the other classes as in (e1) below;
income-producing assets alone are a different concept and cannot be
mixed with W_S. In every option below the record weights come from the
tilt.

### (a) Half the records capitalized, half model-drawn

Each record gets W_S or W_C with probability one half. The file is then a
50/50 mixture of the two methods' files with the same incomes.

- **Totals, means by group, counts above a line, and the revenue of any
  tax computed record by record** are sums over records. Each comes out
  as the average of the two methods' values, apart from sampling noise.
  Example: if the model file puts $2.0 trillion above $50 million and
  the capitalized file $3.0 trillion, the mixture has about $2.5
  trillion.
- **Top shares, thresholds such as the 99.9th percentile, and the Gini**
  are not sums over records. The mixture's top 0.1% is drawn mostly from
  whichever method puts more units at the top, so its top share is not
  the average of the two. It is the top share of a distribution neither
  method describes.
- **Joint relationships.** Two records with identical income get very
  different wealth depending on the coin flip. Any analysis of wealth
  given income on the mixed file sees an artificial spread.
- **Sampling.** Each method has half the records, so standard errors at
  the top are about 1.4 times larger for each.
- **The tilt.** If the mixed file is tilted to the SCF wealth targets,
  the tilt moves weight between records to make the mixture hit them. The
  capitalized half is pulled toward the SCF on every targeted line, which
  removes most of the difference the mixture was meant to carry. The
  mixture keeps its meaning only if the wealth targets are dropped or
  applied to the SCF half alone.

It solves one problem: a downstream user who can take only one wealth
column gets one file whose totals are the average of the two methods.
Everything else it gives can be had more clearly from (d).

### (b) A weighted average on each record

W = 0.5 W_S + 0.5 W_C on every record.

- Totals and means are again the average of the two methods.
- For records with the same incomes, the spread of wealth is cut.
  Averaging halves each part's spread; for the capitalized part, which
  is fixed given x, the remaining spread comes only from the model. If
  W_C had no spread of its own, the variance given x would be a quarter
  of the model's.
- Any tax with a high exemption or rising rates gives less revenue on the
  average than the average of the two revenues. Take a 2% tax above $50
  million and a record with W_S = $30 million and W_C = $90 million. The
  two methods give $0 and $800,000, averaging $400,000. The averaged
  wealth of $60 million gives $200,000.

This is the one option that is biased in a known direction for the
main downstream use. It is not recommended.

### (c) A check, or extra tilt targets

**As a check after the fact.** On the finished file, compare the model's
net worth with capitalized wealth by capitalized-wealth group, as in
section 4. Do the same on the SCF itself. The SCF comparison separates
two questions: whether the model reproduces the SCF, and whether the SCF
agrees with capitalization. A floor check can be added: with generous
yields (interest at the Baa corporate rate, dividends at 3%), capitalized
income-producing assets give a low value for gross assets. At the top,
debts are small, so net worth well below that value is suspect. Lower
down, debts can be large and the floor does not apply.

**As targets.** The SCF targets stop at its 99th percentile, because the
SCF has a few dozen tax units above the 99.9th and leaves out the Forbes
400 (`minimal/17_DISCUSSION_2026-10-15.md`, section 6). Capitalized
series, or the Distributional Financial Accounts with Forbes added, can
give targets above that. One example is net worth held by units above a
dollar line at the 99.9th percentile. Each target needs a precision that
reflects how much the methods disagree. As the table in section 1 shows,
that disagreement is large at the top. The tilt would meet such a
target by moving weight among top records toward those whose SCF-model
draws are higher. It does not change any record's wealth. A second kind of
target is wealth among the units ranked highest by capitalized wealth.
It ties the model's draw to the return income without replacing it.

Costs: the published series are in individuals (couples split equally) or
households, not tax units, and Smith, Zidar and Zwick stop at 2016. Each
target needs a conversion stated on the target sheet, as decision 17
requires for shares.

### (d) Two wealth columns

Keep the model draw as net worth and add a second net worth built from
capitalization. Both sit on the same records with the same weights. A
wealth tax is scored on each and the range is reported. The second
column should be built as in (e1) below, so that both columns have the
same concept. It can come in two versions, equal returns and returns
that rise with wealth. That gives three numbers per estimate, and the
spread between the two capitalized versions is itself informative.

- Solves: shows how much a result depends on the SCF, at no cost to the
  main column; the main use (wealth tax scoring) is where the methods
  differ most.
- Costs: a second set of columns in the Tax-Simulator interface. The
  production output has 14 asset values, basis and accruals per record;
  the capitalized column would have values for the income-producing
  classes only. Only the main column is tilted to SCF targets, so the
  two columns answer different questions on the same weights. That should
  be stated wherever the range is reported.

### (e) Other options

**(e1) Combine by asset class.** Saez and Zucman already do this:
capitalize the classes that produce taxable income and take the rest
(home equity, pensions, vehicles, consumer debt) from survey-based
rules. In the rebuild the rest would come from the SCF-trained model.
That needs the model to draw net worth in two parts under the SCF label:
income-producing financial and business assets, and everything else. A
total is never drawn beside its parts (decision 12), so the two parts
would replace the one net worth variable. This is the cleanest way to
put capitalization into the file. It is a change to a model variable,
and John's decision.

**(e2) Learn the ratio from the SCF.** In the SCF, compute each
household's capitalized wealth from its own reported interest and
dividends, using the same factors. The model then learns the ratio of
net worth to that value, given the record. On a PUF record, net worth is
the drawn ratio times the record's capitalized value. The SCF still
supplies the spread, housing and pensions. The scale at the top comes
from the PUF's income, through a ratio rather than through a range of
income the SCF has few households in. This touches pretraining and the
borrowed-variable draw (decision 16). It is listed as a direction, not
proposed for now.

**(e3) Replace the top only.** Above a line in capitalized wealth (for
example the top 0.1%), use capitalized income-producing wealth plus a
model draw of the rest. Below the line, use the model. This is close to
what production does with the Forbes splice. It moves the top without
touching the middle, but it puts a seam in the distribution at the line.

### Summary of options

| Option | What it gives | Main cost | Extra assumption | With the tilt to SCF targets |
|---|---|---|---|---|
| (a) half the records each way | one file, totals averaged | top shares belong to neither method; noisier top | none beyond the factors | pulls the capitalized half to the SCF unless targets are dropped |
| (b) average on each record | one file, totals averaged | understates any tax with an exemption | none beyond the factors | as (a) |
| (c) check | a measure of where the model departs from the SCF and from capitalization | none to the file | the floor yields | none |
| (c) targets above P99 | a top that is not limited by the SCF's thin top | conversions of unit and year; wide precisions | the published series | adds to it |
| (d) two columns | a range for every estimate | interface work | none beyond the factors | main column only |
| (e1) by asset class | a single file that uses each source where it is strongest | a new variable in the model | split of net worth into two parts in the SCF | unchanged |
| (e2) ratio from the SCF | top scale from the PUF, spread from the SCF | pretraining change | the ratio carries over from the SCF to the PUF | unchanged |
| (e3) top replaced | a top from tax data | a seam at the line | where the line is | the tilt still sees the top |

## 4. A worked example and a first look at the step 31 file

Two small read-only jobs on 18 October. Scripts and output:
`capcheck/cap_check.py`, `capcheck/scf_check.py` and their `.out` files in
scratch. Capitalized value = 89.4 × interest + 75.7 × (0.9 × dividends +
0.1 × positive gains), the equal-returns factors above. The floor uses
interest ÷ 4.4% (about Moody's Baa in 2019) plus dividends ÷ 3%, gains
left out. Model records are from the step 31 files, seeds 1 to 3, in 2019
dollars. Weights are after the 136 IRS targets only, so that no wealth
target shapes the comparison; with the wealth targets the numbers barely
change. SCF rows are the 2019 wave's tax units, with 2018 income and
2019 wealth.

### Six records

Five are real generated records from seed 1; the sixth (F) is made up to
show business income. "Model" is that record's SCF-trained draw.

| Record | Wages | Interest | Dividends | Gains | Capitalized: fixed income | Capitalized: equity | Capitalized total | Floor | Model net worth |
|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| A: earner, small interest | $58,900 | $518 | 0 | 0 | $46,000 | 0 | $46,000 | $12,000 | $214,000 |
| B: no wages, interest and dividends | 0 | $5,300 | $5,500 | 0 | $473,000 | $375,000 | $848,000 | $304,000 | $3.33M |
| C: high earner | $464,800 | $9,900 | $30,800 | −$1,700 | $887,000 | $2.10M | $2.99M | $1.25M | $1.05M |
| D: large dividends | 0 | $56 | $1.27M | −$95,500 | $5,000 | $86.6M | $86.6M | $42.4M | $1.26M |
| E: very large interest | $6.95M | $3.25M | $470,000 | $1.01M | $290.6M | $39.6M | $330.2M | $89.5M | $14.7M |
| F (made up): business owner | 0 | $20,000 | $10,000 | 0 | $1.79M | $681,000 | $2.47M, plus business | | |

How to read them:

- **A and B.** The capitalized value covers only deposits, bonds and
  stocks. The rest of the model's figure is plausibly a home and a
  retirement account, which capitalization cannot see from this file.
  For records like these the two methods do not conflict.
- **C.** The model's $1.05 million is below the floor of $1.25 million,
  which assumes a dividend yield of 3%. A household with $30,800 of
  dividends at the 2019 market yield of about 2% holds about $1.5
  million of stock alone.
- **D.** $1.27 million of dividends and a model net worth of $1.26
  million cannot both be right unless the dividend yield is about 100%.
  This is the case capitalization catches.
- **E.** Here equal returns and rising returns disagree most. Equal
  returns turn $3.25 million of interest into $291 million. At Smith,
  Zidar and Zwick's top-0.01% yield of 3.8% it is $86 million, which
  puts the record at about $125 million. The model's $14.7 million is
  below both.
- **F.** Suppose $400,000 of Schedule C or partnership profit. At the
  crude factor of 8.5 that is $3.4 million of business value. With Smith,
  Zidar and Zwick's split, only $100,000 (25%) is a return to capital.
  Valued at a profit multiple of about 10, that gives about $1 million.
  Business is where the two capitalization methods differ most per
  record. It is also where the SCF reports more wealth than either.

### By capitalized-wealth group: the model against the SCF itself

Share of units whose net worth is below the floor, and the median of net
worth ÷ capitalized value. Model: mean of three seeds (range).

| Capitalized interest and dividends | SCF 2019: % below floor | Model: % below floor | SCF: median net worth ÷ capitalized | Model: median net worth ÷ capitalized |
|---|---:|---:|---:|---:|
| $100k–$1M | 1.3% | 6.0% (5.9–6.3) | | |
| $1M–$10M | 5.0% | 15.9% (14.3–17.1) | 1.63 | 1.19 (1.11–1.28) |
| $10M–$100M | 14.0% | 29.4% (24.4–32.9) | 0.99 (with $100M+) | 0.58 (0.54–0.65) (with $100M+) |
| $100M and over | 24.2% | 60.9% (57.8–65.4) | | |

Mean net worth in the same groups:

| Capitalized interest and dividends | SCF: mean net worth | SCF: mean capitalized | Model: mean net worth | Model: mean capitalized |
|---|---:|---:|---:|---:|
| $1M–$10M | $6.4M | $3.0M | $5.2M | $2.7M |
| $10M–$100M | $26.9M | $22.6M | $29.2M | $24.4M |
| $100M and over | $189M | $230M | $72M | $262M |

What this shows, with its limits:

- **The SCF itself does not follow equal returns at the top.** In the
  SCF's $100 million group, mean net worth is below capitalized interest
  and dividends alone. A quarter of those households are below even the
  floor. This matches Smith, Zidar and Zwick: the rich earn higher yields
  than average, so equal returns overstate their wealth. The floor is a
  screen, not a hard bound.
- **The model departs from the SCF more than the SCF departs from
  capitalization.** For every group above $100,000, the model has two to
  five times as many units below the floor as the SCF. Its median ratio
  is lower by a quarter to two fifths. In the top group its mean is less
  than half the SCF's. A likely cause, not checked: the SCF has few
  households with capital income this large (about 460 in the two top
  groups), and the model's draw for such records falls back toward lower
  wealth. This fits two known gaps: the top wage group is short by
  $700,000, and the top 0.1% is thin.
- **Limits.** The SCF's top is thin: the $100 million group is 536 rows,
  about 107 households across five implicates, and the $10–100 million
  group about 350. SCF income is 2018 dollars and the model's is 2019. The
  floor yields are a judgement. The model's mean in the $10–100 million
  group is close to the SCF's; the difference there is in the spread
  (more records very low and very high), not the level. This is one
  look at one file, not a validated result.

## 5. A first experiment

**Question.** Does capitalization point to a model problem at the top, a
disagreement between the SCF and tax data, or both? And how far apart are
the methods on what a wealth tax would collect?

**Inputs.**

- The step 33 file when it exists (it adds tax-exempt interest; its
  combined business item can be capitalized only crudely), else step 31.
  Three seeds. Weights after the 136 IRS targets only, and after IRS plus
  SCF wealth targets.
- The SCF waves 2016, 2019 and 2022 as tax units, with the step 33 items.
- Factors by class for 2019, both equal returns and Smith, Zidar and
  Zwick's yields by wealth group for interest. The equal-returns
  factors come from the Piketty–Saez–Zucman files, checked against the
  balance sheet ÷ SOI table in section 2. The varying yields need
  iteration, because the wealth group depends on the yield: rank by
  wealth outside interest, assign the yield, recompute.
- The floor yields (Baa for interest, 3% for dividends; business left out).

**Steps.**

1. Capitalize each income-producing class on every model record and every
   SCF tax unit, with each set of factors.
2. Tabulate, by capitalized-wealth group and by AGI group: mean and median
   net worth, the ratio quantiles, and the share below the floor. Do this
   three ways (SCF, model on PUF records, capitalization). Section 4's
   tables are this step for interest and dividends only.
3. Top shares of net worth among tax units: model, SCF, and capitalized
   income-producing wealth plus the model's draw of the rest. The last is
   an approximate (e1). Until the model draws net worth in two parts, the
   rest is taken from SCF households of similar capitalized wealth by
   their median ratio of other wealth to income-producing wealth.
4. A stylized wealth tax (2% above $50 million, 3% above $1 billion) on
   five versions: model; capitalized at equal returns; capitalized at
   varying returns; (a) the 50/50 record mixture; (b) the per-record
   average. This shows the differences in section 3 with numbers.
5. Repeat step 4 with a tilt that adds one target above the 99.9th
   percentile, from the Distributional Financial Accounts with Forbes
   (option c). Report how far the tilt moves and what happens to the
   untargeted tables.

**What to compare.** The model against the SCF at the top, as section 4
does. Wealth tax revenue across the five versions. How far the top target
moves the tilt.

**What would make it worth keeping.**

- If the model stays well below the SCF's own relationship at the top
  (for example, more than 1.5 times the SCF's share below the floor
  above $10 million in all seeds), keep capitalization as a standing
  check after every run. The top then needs work in the model, a target,
  or the splice of (e3).
- If the two capitalized versions bracket the model's wealth tax revenue
  and differ from it by more than sampling noise across seeds, carry the
  second column of (d) for Tax-Simulator.
- If adding the top target moves the tilt by little and leaves the
  untargeted tables in place, adopt it.

## 6. Proposals and questions for John

1. **Which uses to pursue.** Proposed: the check of (c) after every run,
   a second wealth column (d) for Tax-Simulator, and a target above the
   99th percentile tried once. Not proposed: (b). (a) only if a
   downstream user must have one wealth column.
2. **Net worth in two parts under the SCF label** (e1): income-producing
   financial and business assets, and everything else. This changes a
   model variable. Your call.
3. **Which concept for the second column**: the SCF's (proposed, so it
   compares with the targets), or the Financial Accounts', which is what
   the published capitalized series use.
4. **Which rates**: equal returns, Smith, Zidar and Zwick's rising
   interest yields, or both as a range (proposed: both).
5. **Whether section 4's finding goes on the model's list of known gaps**
   next to the top wage group and the thin top 0.1%. It says the
   SCF-trained draw is too low for records with very large capital income,
   compared with the SCF itself.
6. **Items for later**: step 33's business income split into Schedule C,
   partnership and S corporation, and rents and royalties, each split
   into profit and loss; qualified dividends separately; age (for
   pensions); property tax and mortgage interest when the tax-law step
   comes.

## Sources

Read for this note (method sections):

- Saez, E. and G. Zucman (2016). "Wealth Inequality in the United States
  since 1913: Evidence from Capitalized Income Tax Data." *Quarterly
  Journal of Economics* 131(2), 519–578.
  https://eml.berkeley.edu/~saez/SaezZucman2016QJE.pdf (sections III and
  IV read).
- Smith, M., O. Zidar and E. Zwick (2023). "Top Wealth in America: New
  Estimates under Heterogeneous Returns." *Quarterly Journal of Economics*
  138(1), 515–573. https://doi.org/10.1093/qje/qjac033 ; author copy
  https://zidar.princeton.edu/sites/g/files/toruqf3371/files/documents/wealth2023.pdf
  (sections I–III, V, VI.A–B, VII.B and Tables I–II read).

Cited from those papers or from earlier background reports, not re-read:

- Saez, E. and G. Zucman (2020). "The Rise of Income and Wealth
  Inequality in America: Evidence from Distributional Macroeconomic
  Accounts." *Journal of Economic Perspectives* 34(4). (The "revised"
  series, SZ20.)
- Piketty, T., E. Saez and G. Zucman (2018). "Distributional National
  Accounts: Methods and Estimates for the United States." *QJE* 133(2).
  Data: https://gabriel-zucman.eu/usdina/
- Kopczuk, W. (2015). "What Do We Know about the Evolution of Top Wealth
  Shares in the United States?" *Journal of Economic Perspectives* 29(1).
- Bricker, J., A. Henriques, J. Krimmel and J. Sabelhaus (2016).
  "Measuring Income and Wealth at the Top Using Administrative and Survey
  Data." *Brookings Papers on Economic Activity*, Spring.
- Fagereng, A., L. Guiso, D. Malacrino and L. Pistaferri (2020).
  "Heterogeneity and Persistence in Returns to Wealth." *Econometrica*
  88(1).
- Batty, M. et al. (2019). "Introducing the Distributional Financial
  Accounts of the United States." Federal Reserve FEDS 2019-017. Local
  copy: `resources/dfa/`.

Data used: SOI Table 1.4, TY2015 and TY2019
(`/nfs/roberts/project/pi_nrs36/shared/raw_data/IRS-Ind/aligned/bysize_income_sources.csv`);
the Distributional Financial Accounts, 2019Q4 (`resources/dfa/raw/`); the
step 31 files and SCF 2019 tax units in scratch.

In this repository: the production SCF-to-PUF wealth imputation,
`src/imputations/wealth.R`, depends on the SCF in the same way. The
same check would apply to it.
