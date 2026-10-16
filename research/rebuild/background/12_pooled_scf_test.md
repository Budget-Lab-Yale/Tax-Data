---
title: "Pooled SCF donors with several draws per record: a test on the real SCF"
role: evidence
workstream: rebuild
status: current
updated: 2026-10-07
sot: research/rebuild/BRIEF.md
supersedes: []
superseded_by: null
---

# Pooled SCF donors with several draws per record: a test on the real SCF

Background paper 12 for the Tax-Data rebuild (see `research/rebuild/BRIEF.md`).
Design decision D21 (`research/archive/design_decisions_2026-10-14_superseded.md`) says that wealth
and other imputed variables are drawn from a donor pool that combines every
survey wave, with several copies of each record, and that each year's
calibration then moves weight among the copies to hit that year's targets. D21's
own test used simulated data. This paper repeats the test on the real Survey of
Consumer Finances (SCF), waves 2013, 2016, 2019 and 2022.

The question: if a file is calibrated to one wave's published-style tables, does
it recover that wave's joint distribution of balance sheets when the
imputation draws from (a) a single other wave, (b) a pool of other waves that
excludes the target wave, (c) a pool that includes the target wave, compared
with (d) the target wave alone? How do the number of copies, the way waves are
put into common dollars, and the richness and precision of the targets change
the answer?

The unit throughout is the SCF household. **Tax units were not used.**

## Summary

1. **Data.** The SCF summary extracts are already in 2022 dollars, adjusted by a
   price index. The weight `WGT` is the raw weight divided by 5, so summed over
   the five implicates it gives the household count (131.3 million in 2022).
   There are no missing values in any variable used.
2. **With near-exact targets, every donor set hits all 84 targets.** The median
   miss after calibration is 0.5–0.7% everywhere. The donor sets differ only on
   things the targets do not mention.
3. **Calibration does not fix joint patterns the targets do not describe.** In
   2022, 80.4% of stockholders owned a home, fewer than in earlier waves. After
   calibration, the file built from 2016 donors put it 7.5 points too high, and
   the file built from 2019 donors 3.7 points too high. The benchmark built from
   the 2022 wave itself was 1.2 points too high.
4. **Whether pooling helps depends on where the target year sits.** For 2019,
   which lies between other waves, the pool of other waves beat the nearest
   single wave. For 2016 it was about the same. For 2022 (looking forward) and
   2013 (looking back), the equal-weight pool did clearly worse than the nearest
   single wave. Every projection year is at the edge.
5. **Calibration does not find the right wave inside a pool.** When the target
   wave's other half was in an equal-weight pool, its donors held 25% of the
   starting weight and 25–26% after calibration, in every year. Starting with
   60% of the weight on the target wave closed most of the gap to the benchmark
   in energy distance in 2022 and 2013, and about half the gap in homeownership
   among stockholders in 2022.
6. **Leaning 60% toward the nearest wave helped the pool, but did not beat the
   nearest wave used alone** in 2022 or 2013.
7. **Copies change accuracy little but control the weights.** With near-exact
   targets, records whose weight moved outside 0.5–2× fell from 2–7% with one
   copy to 0.2–3% with five and 0–2% with twenty. With twenty copies the largest
   single-record change was still 3–14× (median over splits) with price-adjusted
   levels, and up to 62× with ratios to income, in the benchmark as well. Those
   extremes come from thin table cells, not from differences between waves.
8. **Common dollars.** Using no price adjustment is clearly worse. Imputing
   amounts as ratios to income gives a better starting point at the edge years
   than price-adjusted levels, and about the same calibrated result in
   between. Neither method reproduces the rise in wealth relative to income
   across waves.
9. **Holder counts are the targets that matter.** Without the number holding
   each asset by income group, holding rates by decile are 45–95% further
   off. Net worth totals alone do little. No table by income group alone
   fixes cross-asset patterns such as homeownership among stockholders.
10. **With honest precisions, calibration moves little.** Half-wave SCF tables
    have standard errors of 6–46% of the cell value. With precisions set to
    those, calibration roughly halved the median target miss but left the
    untargeted patterns near where the imputation put them. The choice of
    donors and starting weights then decides the answer.

## Data and checks

Source: `/nfs/roberts/project/pi_nrs36/shared/raw_data/SCF/v1/{2013,2016,2019,2022}/historical/`,
summary extracts `SCFP<year>.csv`, with the raw files `p<yy>i6.dta` used only
for the checks below. 2009 (panel only) was skipped. The checks are in
`00_explore.R` and `00b_dollar_basis.R`.

**Implicates.** Every household appears five times, once per implicate,
numbered 1–5. Households: 6,015 (2013), 6,248 (2016), 5,777 (2019), 4,595
(2022). The Fed's 2022 page lists 4,595 families in the public data (verified,
https://www.federalreserve.gov/econres/scfindex.htm). A household's weight
differs slightly across its five implicates, for every household.

**Weights.** In all four waves, `WGT` equals the raw weight X42001 divided by
5, for every row. Each implicate's weights sum to one fifth of the total, so
summing over all rows gives: 122.5 million (2013), 126.0 million (2016), 128.6
million (2019) and 131.3 million (2022). I believe these are the Fed's
published family counts, but I could not verify that. The bulletin PDF
(https://www.federalreserve.gov/publications/files/scf23.pdf) downloaded, but no
tool on this machine could extract its text. The 2022 extract gives a weighted
mean net worth of $1,059,470 and a median of $192,700. I believe the Fed
reports $1,063,700 and $192,900; this is also unverified.

**Dollars.** The Fed's page says the extract files have "all dollar variables
inflation-adjusted to 2022 dollars" (verified, same URL). The data confirm it.
Directly held stocks in the extract equal the raw nominal value (X3915) times
a constant in each wave: 1.2728 (2013), 1.2334 (2016), 1.1592 (2019), 1.0000
(2022). Income equals raw total income (X5729, which refers to the prior
calendar year) times 1.2921, 1.2493, 1.1802 and 1.0809, for at least the middle
98% of rows. About 1% of rows have a different ratio, up to 33; I did not trace
why. These factors are close to, but not exactly, ratios of the CPI-U. I
believe the Fed uses the CPI-U-RS, but did not verify which index it uses.

**Missing and odd values.** No variable used has a missing value in any wave.
A few rows carry −1: home value (10 rows in 2013, 5 in 2016), retirement
accounts (5 rows in 2022), wage income (20 rows in 2022). They were kept as
they are. Total income is never negative; 190–257 rows per wave report zero.
About 500 rows per wave are flagged as owning a business worth zero. Holding is
therefore taken from the Fed's own flags (`HHOUSES`, `HSTOCKS` or `HNMMF`,
`HRETQLIQ`, `HBUS`, `HDEBT`). For every other component these flags equal
"amount above zero".

**Wealth relative to prices.** Weighted mean net worth in 2022 dollars was
$673k (2013), $850k (2016), $866k (2019) and $1,059k (2022). Wealth grew much
faster than prices, which matters for the common-dollar question.

## Design as run

**Recipients and truth.** In each target wave, a random half of households
(split by household, with all five implicates of a household kept together)
became the recipients. They keep total income, the head's age band (the Fed's
six bands), married or not, and number of children. Children were carried but
not used in matching. The rest of their balance sheet is held out as the truth.
Each implicate row is one record carrying the household's weight. Recipient
records: 11,485 (2022), 14,440 (2019), 15,620 (2016), 15,035 (2013).

**Components.** Primary residence (`HOUSES`); stocks and funds (directly held
stocks `STOCKS` plus pooled investment funds other than money market funds
`NMMF`); retirement accounts (`RETQLIQ`: IRAs, Keoghs and account-type
pensions); business equity (`BUS`); total debt (`DEBT`); and net worth
(`NETWORTH`).

**Donor sets.** Waves are those available: 2013, 2016, 2019, 2022.

| Target | (a) single wave | (b) pooled, excluding target | (c) pooled, including target | (d) benchmark |
|---|---|---|---|---|
| 2022 | 2019; 2016 | 2013, 2016, 2019 | (b) + other half of 2022 | other half of 2022 |
| 2019 | 2016; 2013 | 2013, 2016, 2022 | (b) + other half of 2019 | other half of 2019 |
| 2016 | 2013 | 2013, 2019, 2022 | (b) + other half of 2016 | other half of 2016 |
| 2013 (backcast) | 2016 (no older wave) | 2016, 2019, 2022 | (b) + other half of 2013 | other half of 2013 |

In a pool, each wave's weights are scaled to the same total.

**Imputation.** A weighted hot deck: a method that fills each recipient's
missing values with the values of a similar donor. Each recipient's
neighbourhood is the donors nearest in income rank within its cell of age band
× married (12 cells). The neighbourhood is 3% of the cell's donor rows, at
least 30 rows (about six households), so it covers a similar span of ranks
whatever the pool size. One donor per copy is drawn with probability
proportional to its weight. Each copy takes the donor's whole vector of
components, so a copy's balance sheet is internally consistent. Income ranks
are weighted percentiles computed within each wave, or within each half for the
target wave.

**Common dollars.** Three versions:

- *Nominal*: the Fed's price adjustment undone. This means no common dollars.
- *Price-adjusted levels*: the extracts as published, in 2022 dollars. This is
  the CPI-deflation option.
- *Ratio to income*: each component divided by the donor's income, then
  multiplied by the recipient's income. Both incomes are floored at the 5th
  percentile of income in their own wave, so that near-zero incomes do not
  produce huge ratios.

The main comparisons use the last two. Nominal was run for the single nearest
wave and the pool of other waves, at five copies.

**Copies.** 1, 5 and 20 draws per recipient, with the record's weight split
evenly among them. In the per-wave variant, each record gets five copies from
each wave, and the starting weight puts 60% on the nearest wave and splits the
rest evenly. For (b) the nearest wave is the closest other wave, with ties going
to the older wave; for (c) it is the target wave's other half.

**Targets.** These are built from the recipients' true values and weights,
grouped by income: the bottom four quintiles, the 80th–90th, 90th–99th and top
1% of recipients' income. The top 1% held 2,506 rows in the full 2022 wave
(about 500 households), so about 250 households per split, which is thin but
usable. In each group there are 12 targets: the number of households; for each
of five components, the number holding it and the total amount; and total net
worth. That makes 84 targets ("rich" targets). Two leaner sets drop the holder
counts ("totals") or keep only household counts and net worth ("net worth
only").

**Calibration.** Soft entropy calibration, as in D3 and the D21 test. The new
weights stay as close as possible to the starting weights, measured by relative
entropy, while each target may miss by an amount set by its precision. It was
solved by Newton's method on the dual problem; every run converged. Weights
were scaled to average one per record. This sets how strongly the starting file
resists change: it counts as many observations as it has records. Household
counts always have a precision of 0.2%. All other targets use one of two
settings:

- *1% (near exact).* In this test the targets are the recipients' own true
  values, so they are exact. This shows the most the tables can do.
- *Standard error.* Each target is trusted to its own sampling standard error,
  as a published table would be. The standard error was estimated by a
  bootstrap: the recipient half was redrawn 200 times by household, with
  replacement. The floor was 1%. As a share of the cell value (the coefficient
  of variation), the 2022 standard errors were 6–14% for household counts,
  6–26% for holder counts, 7–18% for home and debt totals, and 10–46% for
  stock, retirement and business totals. A table built from the full wave
  would have standard errors about 30% smaller.

**Evaluation** is on things the targets do not mention, compared with the
recipients' true values:

- *Net worth by decile*: for income deciles 1–8, the mean absolute percentage
  error of total net worth. Deciles 9 and 10 coincide with target groups and
  are left out. The same is done for the five components, and for holding rates
  (in percentage points).
- *By age band*: the same, by the six age bands.
- *Top shares*: the top 1% and top 10% shares of net worth.
- *Joint patterns*: the share of stockholders who own a home, and the share of
  business owners who hold stocks or funds.
- *Wealth relative to income*: the median and 90th percentile of net worth to
  income in each target income group, among households with income above zero
  (99.1–99.4% of rows).
- *Energy distance* between the file's and the truth's joint distribution of
  income, the five components and net worth. Each amount is transformed by
  asinh(x / $10,000), which behaves like a logarithm for large amounts and is
  defined at zero and for negative values. Energy distance is twice the average
  distance between a draw from one distribution and a draw from the other,
  minus the average distance within each. It is zero only when the two are the
  same. It was computed on 6,000 weighted draws from each side. The noise floor
  (the truth against an independent redraw of itself) was 1.5–2.1 (all energy
  figures are × 1,000).
- *Weights*, at the record level (summing over a record's copies): target
  misses; the largest ratio of a record's new weight to its starting weight;
  the share of records whose ratio falls outside 0.5–2× (D14's per-year cap,
  reported here, not imposed); and the effective sample size. That is the Kish
  measure (sum of weights)² / (sum of squared weights), shown as a share of its
  starting value, so 100% means calibration did not concentrate weight.

**Repetitions.** Ten random splits per target wave. The tables show means over
splits. The split-to-split standard deviations were about 3–5 points for the
decile net worth error, 1.4–3 points for homeownership among stockholders, and
1–4 for the energy distance. The standard error of a ten-split mean is these
divided by about 3.2. Methods were run on the same splits, so differences
between them are measured more precisely than that. The paired standard errors
quoted below are from those differences.

Each job took about 6 minutes on two cores. The 40 jobs ran as one Slurm array.

## Results

### 2022, the main case

Five copies, rich targets at 1% precision. The second column is before
calibration; the other columns are after.

**Price-adjusted levels**

| Donor set | Median target miss before | Net worth by decile, error | Holding rates by decile, error (points) | Homeowners among stockholders, error (points) | Stockholders among business owners, error (points) | Median net worth/income by group, error | Energy distance |
|---|---|---|---|---|---|---|---|
| (a) single wave 2016 | 17.4% | 13% | 2.4 | +7.5 | −5.6 | 15% | 14.3 |
| (a) single wave 2019 | 15.8% | 10% | 2.4 | +3.7 | −1.8 | 15% | 12.3 |
| (b) pooled, excluding 2022 | 19.8% | 12% | 2.4 | +6.5 | −2.2 | 16% | 16.7 |
| (b) per wave, 60% on 2019 | 17.4% | 10% | 2.3 | +5.3 | −2.0 | 15% | 13.9 |
| (c) pooled, including 2022 half | 15.6% | 10% | 2.4 | +5.2 | −2.1 | 11% | 12.1 |
| (c) per wave, 60% on 2022 half | 12.5% | 9% | 2.4 | +3.3 | −0.9 | 11% | 6.7 |
| (d) 2022 half only | 11.1% | 10% | 2.7 | +1.2 | −0.3 | 15% | 5.6 |

**Ratio to income**

| Donor set | Median target miss before | Net worth by decile, error | Holding rates by decile, error (points) | Homeowners among stockholders, error (points) | Stockholders among business owners, error (points) | Median net worth/income by group, error | Energy distance |
|---|---|---|---|---|---|---|---|
| (a) single wave 2016 | 14.5% | 13% | 2.3 | +6.8 | −5.9 | 14% | 10.7 |
| (a) single wave 2019 | 14.5% | 11% | 2.6 | +3.4 | −2.2 | 16% | 10.1 |
| (b) pooled, excluding 2022 | 15.9% | 13% | 2.3 | +5.8 | −2.9 | 13% | 12.6 |
| (b) per wave, 60% on 2019 | 14.8% | 12% | 2.3 | +4.8 | −2.8 | 15% | 11.8 |
| (c) pooled, including 2022 half | 12.8% | 11% | 2.3 | +4.6 | −2.2 | 11% | 8.7 |
| (c) per wave, 60% on 2022 half | 10.7% | 10% | 2.3 | +3.0 | −0.9 | 12% | 6.4 |
| (d) 2022 half only | 10.3% | 10% | 2.7 | +1.5 | −0.9 | 15% | 5.6 |

The truth in 2022: 80.4% of stockholders own a home, and 44.9% of business
owners hold stocks or funds. Before calibration, every donor set built from
older waves only put the first 5.5–8.2 points too high and the second
10.6–12.5 points too low. Those
are the 2022 changes: more stockholders who rent, and more business owners who
hold stocks. Calibration on holder counts and totals by income group closes
much of the business-owner gap, but less of the homeownership gap.

The paired differences against the single 2019 wave (levels) are as follows.
The pool excluding 2022 was worse on the energy distance by 4.4 ± 1.3 and on
homeownership among stockholders by 2.8 ± 0.2 points. The per-wave lean on 2019
was worse by 1.6 ± 1.4 and 1.6 ± 0.2. The 2022-half benchmark was better by
6.7 ± 1.1 and 1.5 ± 0.5.

The net worth by decile and holding rates by decile do not separate the donor
sets. Every set, the benchmark included, lands at about 9–13% and 2.3–2.7
points; that is the noise floor of this sample size. The benchmark's holding
rates are slightly worse than the pools', because it draws from only half a
wave.

Top 1% and top 10% net worth shares (truth 34.3% and 73.0%) came within 3.2
points of the truth in every file calibrated at 1% precision, in every year.
With standard-error precisions the largest miss was 5.3 points.

### 2022 with standard-error precisions

Five copies, rich targets, each trusted to its bootstrap standard error.

| Donor set | Footing | Median target miss after | Mean miss in standard errors | Net worth by decile, error | Homeowners among stockholders, error | Stockholders among business owners, error | Energy distance after (before) |
|---|---|---|---|---|---|---|---|
| (a) single wave 2019 | levels | 8.3% | 0.89 | 18% | +5.3 | −10.6 | 19.3 (15.5) |
| (b) pooled, excluding 2022 | levels | 9.5% | 0.98 | 21% | +7.9 | −10.2 | 32.4 (25.1) |
| (c) pooled, including 2022 half | levels | 8.3% | 0.82 | 16% | +6.5 | −8.2 | 19.6 (13.7) |
| (c) per wave, 60% on 2022 half | levels | 6.7% | 0.71 | 13% | +4.9 | −4.4 | 8.6 (9.0) |
| (d) 2022 half only | levels | 6.4% | 0.75 | 14% | +3.1 | −0.5 | 8.7 (19.3) |
| (b) pooled, excluding 2022 | ratio | 8.3% | 0.87 | 18% | +7.5 | −10.9 | 21.2 (15.0) |

With honest precisions, the joint patterns stay close to where the imputation
put them. Calibration improved the energy distance only when the donors came
mostly from the target wave. For donors from other waves it made the distance
worse, even as the targeted tables improved. I did not establish why.

### Other target years

Five copies, rich targets at 1% precision, price-adjusted levels. The ratio
footing gives the same ranking, except where noted in the section on common
dollars.

| Target | Donor set | Median target miss before | Energy distance after | Homeowners among stockholders, error | Stockholders among business owners, error | Median net worth/income, error |
|---|---|---|---|---|---|---|
| 2019 | (a) single wave 2016 | 9.5% | 4.5 | +7.1 | −4.1 | 7% |
| 2019 | (a) single wave 2013 | 14.8% | 7.1 | +7.3 | +1.2 | 15% |
| 2019 | (b) pooled, excluding 2019 | 7.7% | 3.5 | +4.7 | −0.5 | 7% |
| 2019 | (b) per wave, 60% on 2016 | 8.0% | 4.0 | +5.5 | −1.9 | 7% |
| 2019 | (c) pooled, including 2019 half | 7.9% | 4.0 | +4.4 | −0.9 | 7% |
| 2019 | (c) per wave, 60% on 2019 half | 8.7% | 4.1 | +3.4 | −1.8 | 8% |
| 2019 | (d) 2019 half only | 10.3% | 5.1 | +2.5 | −2.7 | 11% |
| 2016 | (a) single wave 2013 | 11.4% | 4.9 | +2.9 | +3.6 | 13% |
| 2016 | (b) pooled, excluding 2016 | 8.7% | 5.4 | −1.5 | +3.0 | 15% |
| 2016 | (b) per wave, 60% on 2013 | 7.3% | 4.4 | +0.1 | +3.1 | 11% |
| 2016 | (c) pooled, including 2016 half | 8.2% | 5.4 | −0.6 | +2.4 | 13% |
| 2016 | (c) per wave, 60% on 2016 half | 8.1% | 5.4 | +0.6 | +0.8 | 11% |
| 2016 | (d) 2016 half only | 9.0% | 5.4 | +2.3 | −0.9 | 10% |
| 2013 | (a) single wave 2016 | 9.9% | 6.3 | +0.3 | −4.9 | 20% |
| 2013 | (b) pooled, excluding 2013 | 13.9% | 10.3 | −2.8 | −2.9 | 30% |
| 2013 | (b) per wave, 60% on 2016 | 11.9% | 7.8 | −2.1 | −3.6 | 26% |
| 2013 | (c) pooled, including 2013 half | 11.6% | 8.3 | −1.8 | −2.2 | 23% |
| 2013 | (c) per wave, 60% on 2013 half | 9.4% | 5.5 | −0.5 | −1.5 | 16% |
| 2013 | (d) 2013 half only | 10.2% | 4.6 | +1.7 | −0.5 | 12% |

**2019** sits between waves. The pool excluding 2019 beat the nearest single
wave, 2016: energy distance lower by 1.0 ± 0.4, and homeownership among
stockholders 2.4 ± 0.2 points closer. It also beat the 2019-half benchmark.
With a quarter of a wave, the benchmark's own sampling noise outweighs the
differences between waves.

**2016**: the pool and the single 2013 wave were about equal on energy
distance (difference 0.5 ± 1.0 with levels; 2.1 ± 0.7 against the pool with
ratios). The two joint patterns are within noise of the benchmark. In 2016 and
2013, the benchmark's own errors on them are as large as the others'.

**2013** is a backcast from later waves. The pool was clearly worse than the
single 2016 wave: energy distance higher by 4.0 ± 0.8. Its median net worth to
income was 30% off, against 20% for 2016 alone and 12% for the benchmark.
Later waves hold much more wealth per dollar of income, and calibration to
income-group totals does not bring the medians down.

**Target wave in the pool.** Calibration did not shift weight toward the target
wave's donors. Their share was 25% before calibration and 24.6–26.1% after, in
every year, at both precisions and every number of copies.

### Copies and weights

Price-adjusted levels, rich targets. Each cell shows four numbers: the largest
record-level weight change (median over splits) / share of records outside
0.5–2× / effective sample size / energy distance.

| Target | Donor set | Precision | 1 copy | 5 copies | 20 copies |
|---|---|---|---|---|---|
| 2022 | (b) pooled, excluding target | 1% | 116× / 4.9% / 84% / 15.8 | 26× / 0.9% / 96% / 16.7 | 11× / 0.5% / 98% / 18.4 |
| 2022 | (b) pooled, excluding target | s.e. | 14× / 0.4% / 99% / 30.3 | 6× / 0.1% / 99% / 32.4 | 5× / 0.1% / 99% / 31.5 |
| 2022 | (d) target half only | 1% | 94× / 6.6% / 87% / 7.0 | 11× / 2.2% / 96% / 5.6 | 6× / 1.5% / 97% / 5.1 |
| 2019 | (b) pooled, excluding target | 1% | 47× / 1.8% / 95% / 4.2 | 5× / 0.2% / 98% / 3.5 | 3× / 0.0% / 99% / 3.7 |
| 2019 | (d) target half only | 1% | 62× / 4.2% / 93% / 5.4 | 8× / 1.2% / 97% / 5.1 | 5× / 0.7% / 98% / 5.2 |
| 2016 | (b) pooled, excluding target | 1% | 40× / 1.9% / 95% / 6.2 | 13× / 0.2% / 98% / 5.4 | 7× / 0.1% / 99% / 6.0 |
| 2016 | (d) target half only | 1% | 85× / 3.3% / 94% / 5.4 | 7× / 0.5% / 98% / 5.4 | 10× / 0.3% / 99% / 5.0 |
| 2013 | (b) pooled, excluding target | 1% | 32× / 5.8% / 93% / 9.5 | 10× / 1.7% / 97% / 10.3 | 14× / 1.1% / 98% / 11.2 |
| 2013 | (d) target half only | 1% | 26× / 3.8% / 92% / 5.0 | 8× / 1.0% / 98% / 4.6 | 4× / 0.6% / 99% / 5.2 |

Across all single-wave, pooled and benchmark sets, the median (over splits) of
the largest change ranged over 12–116× with one copy, 3–57× with five and
3–14× with twenty under price-adjusted levels. Under ratios to income it ranged
over 15–263×, 5–64× and 6–62×. The worst single split was much larger: 4,453×
with one copy, 730× with five and 319× with twenty. Records outside 0.5–2×
were 1.7–7.1%, 0.2–2.6% and 0–1.9%, and the effective sample size 82–96%,
95–99% and 97–99%. The benchmark needs extreme weights just as
the other donor sets do. The cause is that totals of stocks and business equity
in some income groups rest on a few households, so hitting them within 1% can
require a large weight on one draw.

On accuracy, twenty copies against one cut the 2022 net-worth-by-decile error
by 2–7 points (paired standard errors 1–2). Elsewhere the difference was within
noise. The energy distance did not improve. For the 2022 pool excluding the
target it rose by 2.5 ± 0.8 with ratios and 2.6 ± 1.4 with levels.

### Common dollars

2022, five copies:

| Donor set | Footing | Median target miss before | Net worth by decile, error before | Top 1% share, error before (points) | Energy distance before | Net worth by decile, error after | Median net worth/income, error after | Energy distance after | Largest weight change after |
|---|---|---|---|---|---|---|---|---|---|
| (a) single wave 2019 | nominal | 22.2% | 36% | +1.7 | 29.8 | 12% | 17% | 19.5 | 84× |
| (a) single wave 2019 | price-adjusted | 15.8% | 27% | +2.2 | 15.5 | 10% | 15% | 12.3 | 16× |
| (a) single wave 2019 | ratio to income | 14.5% | 24% | +5.8 | 14.1 | 11% | 16% | 10.1 | 14× |
| (b) pooled, excluding 2022 | nominal | 29.2% | 45% | +1.7 | 62.5 | 17% | 23% | 30.4 | 69× |
| (b) pooled, excluding 2022 | price-adjusted | 19.8% | 32% | +1.7 | 25.1 | 12% | 16% | 16.7 | 26× |
| (b) pooled, excluding 2022 | ratio to income | 15.9% | 27% | +4.3 | 15.0 | 13% | 13% | 12.6 | 20× |

Across years, at five copies, rich targets, 1% precision: the first pair of
columns is the energy distance with price-adjusted levels, the second with
ratios. In each pair the number before the arrow is the starting file and the
number after is the calibrated file.

| Target | Donor set | Energy, levels | Energy, ratio | Median net worth/income error after, levels / ratio |
|---|---|---|---|---|
| 2022 | (a) single wave 2019 | 15.5 → 12.3 | 14.1 → 10.1 | 15% / 16% |
| 2022 | (b) pooled, excluding target | 25.1 → 16.7 | 15.0 → 12.6 | 16% / 13% |
| 2019 | (a) single wave 2016 | 15.4 → 4.5 | 19.0 → 4.4 | 7% / 7% |
| 2019 | (b) pooled, excluding target | 20.6 → 3.5 | 26.1 → 3.7 | 7% / 8% |
| 2016 | (a) single wave 2013 | 17.4 → 4.9 | 25.3 → 4.5 | 13% / 10% |
| 2016 | (b) pooled, excluding target | 47.2 → 5.4 | 47.2 → 6.6 | 15% / 12% |
| 2013 | (a) single wave 2016 | 37.7 → 6.3 | 22.2 → 5.5 | 20% / 14% |
| 2013 | (b) pooled, excluding target | 68.0 → 10.3 | 42.8 → 9.0 | 30% / 22% |

Leaving waves in nominal dollars roughly doubles the starting distance from the
truth and leaves the calibrated file worse, with larger weight changes. At the
edge years (2022, 2013), ratios to income gave a better starting point and a
better calibrated file than price-adjusted levels. In 2016 and 2019, ratios
started further from the truth but ended about the same after calibration,
except the 2016 pool (6.6 against 5.4). Before calibration, ratios
overstate the top 1% share of net worth by 4–6 points, because a high-income
recipient multiplies a donor's ratio by a very large income. Calibration
removes that.

### How rich the targets must be

The pool excluding the target, five copies, price-adjusted levels. "Holder
count miss" and "total miss" are the largest misses on the rich targets, which
the leaner sets do not all include.

| Target | Targets | Precision | Holder count miss (largest) | Total miss (largest) | Holding rates by decile, error (points) | Homeowners among stockholders | Stockholders among business owners | Median net worth/income, error | Energy distance |
|---|---|---|---|---|---|---|---|---|---|
| 2022 | rich | 1% | 6% | 5% | 2.4 | +6.5 | −2.2 | 16% | 16.7 |
| 2022 | totals only | 1% | 46% | 5% | 4.7 | +9.3 | −9.6 | 18% | 18.3 |
| 2022 | net worth only | 1% | 43% | 190% | 4.4 | +7.8 | −9.9 | 21% | 25.2 |
| 2022 | rich | s.e. | 41% | 75% | 4.4 | +7.9 | −10.2 | 23% | 32.4 |
| 2019 | rich | 1% | 4% | 2% | 1.6 | +4.7 | −0.5 | 7% | 3.5 |
| 2019 | totals only | 1% | 36% | 3% | 2.4 | +6.0 | +1.6 | 13% | 7.9 |
| 2019 | net worth only | 1% | 32% | 128% | 2.5 | +6.0 | +1.8 | 14% | 7.8 |
| 2016 | rich | 1% | 3% | 4% | 1.5 | −1.5 | +3.0 | 15% | 5.4 |
| 2016 | totals only | 1% | 33% | 4% | 2.2 | −1.7 | +3.5 | 17% | 8.4 |
| 2016 | net worth only | 1% | 36% | 70% | 2.8 | +0.2 | +3.5 | 28% | 17.9 |
| 2013 | rich | 1% | 4% | 5% | 1.8 | −2.8 | −2.9 | 30% | 10.3 |
| 2013 | totals only | 1% | 43% | 5% | 2.6 | −4.3 | −2.0 | 31% | 11.5 |
| 2013 | net worth only | 1% | 44% | 88% | 2.8 | −1.8 | −1.5 | 44% | 22.3 |

Holder counts carry most of the information. Without them, holding rates by
decile are 45–95% further off, and the 2022 gap in stockholding among business
owners stays at about 10 points. Net worth totals alone leave component totals
off by 70–190% in the worst cell. Even the rich set does not fix homeownership
among stockholders in 2022: no target describes the two assets jointly.

## What the results say about D21's open details

**Pooling and leaning toward the nearest wave.** On the real SCF, pooling with
equal wave weights helps when the year being built lies inside the span of the
pooled waves, and hurts when it lies at the edge. The pool's starting point is
then a mix of mostly older (or, for a backcast, newer) waves, and calibration to
income-group tables does not pull it back. The simulated test showed the
opposite, because there the pool contained the target wave's kind of household
and the targets measured exactly the dimension that changed. With real waves,
the changes that matter here are joint patterns: stockholders who rent, and
business owners who hold stocks. Holder counts by income group describe them
only in part.

Two things follow for D21. First, the starting weights across waves matter
more than the pooling itself. The per-wave variant leaning 60% toward the
target wave came close to the benchmark in 2022 and 2013. Leaning 60% toward
the nearest *other* wave was not enough to beat that wave used alone; a
stronger lean, or a lean that fades with distance in years, is worth testing.
Second, projection years always sit at the edge. For them the newest wave
should carry most of the starting weight.

**Calibration does not choose among waves.** With the target wave inside an
equal-weight pool, calibration left its donors' share at about 25%. The tables
used here cannot tell one wave's balance sheets from another's beyond the
dimensions they describe. Whatever weight the starting file puts on each wave
largely remains.

**Number of copies.** Copies are a weight-control device, not an accuracy
device. Five copies removed most of the records that move outside 0.5–2× and
raised the effective sample size to 95–99%. Twenty copies went somewhat
further. Neither number of copies kept every record inside D14's cap when
targets were near exact; the extremes come from thin cells. Ratios to income
produced larger extremes than price-adjusted levels. Splitting is worth
keeping, but the cap question (open question 1) has to be settled together
with the precisions on thin cells.

**Common dollars.** Some adjustment is required; nominal pooling is clearly
worse. Ratios to income are the better default at the edges, and about the
same in between once calibrated. Neither a price index nor ratios to income captures the rise in
wealth relative to income from 2013 to 2022. Asset-price growth (from the
Financial Accounts, by component) is the natural third option. It was not
tested here, because the test has no source for it that is independent of the
target wave.

**Richness of targets.** The number holding each component by income group is
essential; totals and net worth alone leave participation wrong. Patterns
across components (owners who also hold stocks) need cross-tabulated targets
if they matter downstream, such as home owners by stockholding, or business
owners by stockholding. I did not check which such cross-tabulations the Fed
publishes.

**Precision.** With precisions set to the tables' sampling error, calibration
does little to untargeted patterns. Under D21 the imputation then decides most
of the joint distribution. The donor waves and their starting weights are
therefore the main lever, not a detail.

## Limits of the test

- **Households, not tax units, and SCF recipients, not PUF records.** The
  recipients come from the target wave itself, so they share its income
  concept, demographics and income distribution exactly. In production the
  recipients are PUF tax units, whose income is defined differently and whose
  distribution differs from the SCF's. That adds error that this test does not
  measure.
- **Targets come from the same half they are scored against.** At 1%
  precision they are exact for the truth, which is more than a real table
  offers. The standard-error setting is the realistic one, and it used
  half-wave standard errors, larger than the full-wave ones.
- **Each half is small.** A half of the 2022 wave is about 2,300 households.
  Differences between waves are of the same order as one half-wave's sampling
  noise. That is why the benchmark is beaten in 2019 and ties in 2016. The real
  wave-year case would use the whole wave, with half the noise.
- **The hot deck matches on income rank, age band and marriage only.** Even
  the same-wave benchmark overstates homeownership among stockholders by 1–4
  points before calibration in every year; I did not trace why. A richer
  imputation model (D21's production setting) may behave differently.
- **Four waves, 2013–2022.** The 2025 wave is not available and the 2009 panel
  was skipped. With 2025 added, 2022 would sit inside the pool's span.
- **Two joint patterns stand in for "new kinds of household".** Other changes
  between waves are measured only through the energy distance.
- **The per-wave variant used five copies per wave** (15 or 20 in total), so it
  has more copies than the five-copy pools it is compared with.
- **D14's caps were reported, not imposed.**

## Files

Scripts and small outputs are in
`research/rebuild/tests/pooled_scf/`:

- `00_explore.R`, `00b_dollar_basis.R`: implicate, weight, dollar and
  missing-value checks.
- `01_pooled_scf_test.R`: the test for one target wave and split (arguments:
  target year, split number).
- `02_summarise.R`: means and standard deviations over splits.
- `run_*.sh`: Slurm job scripts. The test is an array of 40 jobs (4 targets × 10
  splits).
- `results_mean.csv`, `results_sd.csv`: every configuration, before and after
  calibration. `truth_mean.csv`: truth values by target year.

Per-split results (`res_<target>_<split>.rds`), the combined by-split table
(`results_by_split.csv`) and logs are in
`/nfs/roberts/scratch/pi_nrs36/jar335/pooled_scf/`. That is scratch space and
may be purged.
