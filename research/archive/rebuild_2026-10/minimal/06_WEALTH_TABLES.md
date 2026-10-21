---
title: "Minimal example: wealth, actual SCF against generated, before and after the tilt"
role: evidence
workstream: rebuild
status: current
updated: 2026-10-14
sot: research/rebuild/design/plan.md
supersedes: []
superseded_by: null
---

# Minimal example: wealth, actual SCF against generated, before and after the tilt

"Actual SCF" is every 2016-wave SCF unit in the step 1 table, with survey
weights. "Pretrain SCF" is the 40,000 generated SCF-style records, and
"pretrain PUF-style" the 200,000 generated PUF-style records that also carry
net worth. Both get dollars drawn from the step 4 maps and equal weights:
the SCF-style file sums to the actual SCF's 140 million units, and the
PUF-style file to SOI's 158 million returns for 2019, the starting weights of
run B1. "Posttrain PUF-style" is that file reweighted to run B1's 136
AGI-bracket targets, which do not include net worth. "Posttrain, wealth
targeted" adds 2019 SCF wealth targets to those 136 (section at the end).
All net worth is in 2015 dollars. Groups use the drawn 2015-dollar incomes.

Script: `06_wealth_tables.py`, run by `06_run.sbatch` (Slurm job 29595379).
Outputs: `/nfs/roberts/scratch/pi_nrs36/jar335/minimal/wealth/`.

## Table 1. Net worth, all units

|  | actual SCF | pretrain SCF | pretrain PUF-style | posttrain PUF-style | posttrain, wealth targeted |
|---|---:|---:|---:|---:|---:|
| units (millions) | 140 | 140 | 158 | 158 | 158 |
| total net worth ($ trillion) | 85.8 | 78.9 | 165 | 176 | 89.1 |
| mean ($) | 614,000 | 565,000 | 1,040,000 | 1,120,000 | 565,000 |
| median ($) | 74,700 | 61,700 | 104,000 | 111,000 | 66,700 |
| share negative | 11.0% | 12.0% | 11.7% | 11.5% | 9.79% |
| top 10% share | 78.3% | 79.6% | 80.4% | 80.7% | 77.7% |
| top 1% share | 39.7% | 43.2% | 44.3% | 43.5% | 38.4% |
| top 0.1% share | 15.3% | 16.4% | 17.3% | 16.8% | 9.27% |
| 90th percentile ($) | 1,040,000 | 914,000 | 1,430,000 | 1,540,000 | 907,000 |
| 99th percentile ($) | 9,660,000 | 8,920,000 | 18,300,000 | 19,100,000 | 10,800,000 |
| 99.9th percentile ($) | 41,500,000 | 42,900,000 | 72,700,000 | 80,100,000 | 38,900,000 |

## Table 2a. Mean net worth by group ($)

| wages | actual SCF | pretrain SCF | pretrain PUF-style | posttrain PUF-style | posttrain, wealth targeted |
|---|---:|---:|---:|---:|---:|
| 0 | 663,000 | 679,000 | 2,380,000 | 2,430,000 | 1,180,000 |
| 1-25k | 173,000 | 136,000 | 411,000 | 427,000 | 211,000 |
| 25-50k | 181,000 | 153,000 | 443,000 | 444,000 | 233,000 |
| 50-100k | 353,000 | 341,000 | 788,000 | 826,000 | 434,000 |
| 100-200k | 934,000 | 819,000 | 1,550,000 | 1,660,000 | 859,000 |
| 200k+ | 5,120,000 | 4,640,000 | 5,560,000 | 5,920,000 | 3,180,000 |

| filing status | actual SCF | pretrain SCF | pretrain PUF-style | posttrain PUF-style | posttrain, wealth targeted |
|---|---:|---:|---:|---:|---:|
| joint | 1,120,000 | 1,040,000 | 2,040,000 | 2,210,000 | 1,120,000 |
| not joint | 262,000 | 243,000 | 483,000 | 534,000 | 270,000 |

| dividends | actual SCF | pretrain SCF | pretrain PUF-style | posttrain PUF-style | posttrain, wealth targeted |
|---|---:|---:|---:|---:|---:|
| 0 | 250,000 | 265,000 | 464,000 | 481,000 | 233,000 |
| 1-1k | 829,000 | 863,000 | 1,580,000 | 1,670,000 | 924,000 |
| 1k-10k | 2,340,000 | 2,070,000 | 4,380,000 | 4,530,000 | 2,350,000 |
| 10k+ | 9,290,000 | 8,890,000 | 10,800,000 | 11,100,000 | 6,000,000 |

## Table 2b. Median net worth by group ($)

| wages | actual SCF | pretrain SCF | pretrain PUF-style | posttrain PUF-style | posttrain, wealth targeted |
|---|---:|---:|---:|---:|---:|
| 0 | 96,800 | 72,100 | 418,000 | 437,000 | 223,000 |
| 1-25k | 8,490 | 6,020 | 9,870 | 9,870 | 10,600 |
| 25-50k | 30,100 | 28,400 | 44,400 | 43,300 | 35,900 |
| 50-100k | 109,000 | 110,000 | 169,000 | 171,000 | 110,000 |
| 100-200k | 334,000 | 350,000 | 465,000 | 493,000 | 294,000 |
| 200k+ | 1,790,000 | 1,380,000 | 1,320,000 | 1,400,000 | 797,000 |

| filing status | actual SCF | pretrain SCF | pretrain PUF-style | posttrain PUF-style | posttrain, wealth targeted |
|---|---:|---:|---:|---:|---:|
| joint | 221,000 | 205,000 | 403,000 | 436,000 | 251,000 |
| not joint | 25,400 | 16,400 | 28,700 | 31,200 | 29,000 |

| dividends | actual SCF | pretrain SCF | pretrain PUF-style | posttrain PUF-style | posttrain, wealth targeted |
|---|---:|---:|---:|---:|---:|
| 0 | 49,400 | 40,700 | 57,300 | 59,700 | 42,600 |
| 1-1k | 428,000 | 401,000 | 582,000 | 593,000 | 373,000 |
| 1k-10k | 1,320,000 | 1,080,000 | 1,370,000 | 1,390,000 | 768,000 |
| 10k+ | 4,370,000 | 3,310,000 | 3,680,000 | 4,020,000 | 2,100,000 |

## Table 2c. Share of units in each group

| wages | actual SCF | pretrain SCF | pretrain PUF-style | posttrain PUF-style | posttrain, wealth targeted |
|---|---:|---:|---:|---:|---:|
| 0 | 30.4% | 30.0% | 16.8% | 17.8% | 17.8% |
| 1-25k | 19.5% | 20.4% | 32.4% | 30.8% | 30.7% |
| 25-50k | 19.1% | 19.5% | 21.4% | 22.1% | 22.1% |
| 50-100k | 17.9% | 17.3% | 18.2% | 18.0% | 18.1% |
| 100-200k | 9.32% | 9.09% | 8.73% | 8.60% | 8.61% |
| 200k+ | 3.79% | 3.68% | 2.46% | 2.75% | 2.73% |

| filing status | actual SCF | pretrain SCF | pretrain PUF-style | posttrain PUF-style | posttrain, wealth targeted |
|---|---:|---:|---:|---:|---:|
| joint | 40.9% | 40.7% | 36.0% | 34.7% | 34.7% |
| not joint | 59.1% | 59.3% | 64.0% | 65.3% | 65.3% |

| dividends | actual SCF | pretrain SCF | pretrain PUF-style | posttrain PUF-style | posttrain, wealth targeted |
|---|---:|---:|---:|---:|---:|
| 0 | 88.0% | 89.4% | 82.3% | 81.6% | 81.6% |
| 1-1k | 5.07% | 4.29% | 9.47% | 9.56% | 10.1% |
| 1k-10k | 4.24% | 3.92% | 5.93% | 6.28% | 5.89% |
| 10k+ | 2.73% | 2.37% | 2.34% | 2.52% | 2.39% |

## Table 3. Share of units with net worth above $1 million

| wages | actual SCF | pretrain SCF | pretrain PUF-style | posttrain PUF-style | posttrain, wealth targeted |
|---|---:|---:|---:|---:|---:|
| 0 | 11.9% | 10.6% | 31.5% | 32.3% | 19.6% |
| 1-25k | 3.13% | 2.10% | 5.96% | 6.06% | 3.12% |
| 25-50k | 2.08% | 2.01% | 7.25% | 7.15% | 3.54% |
| 50-100k | 6.06% | 6.07% | 13.8% | 14.2% | 7.31% |
| 100-200k | 21.2% | 22.1% | 27.8% | 29.4% | 16.9% |
| 200k+ | 72.6% | 63.8% | 60.2% | 62.7% | 44.6% |

## Records in each group (unweighted)

| wages | actual SCF | pretrain SCF | pretrain PUF-style | posttrain PUF-style | posttrain, wealth targeted |
|---|---:|---:|---:|---:|---:|
| 0 | 10,399 | 12,006 | 33,683 | 33,683 | 33,683 |
| 1-25k | 5,835 | 8,149 | 64,828 | 64,828 | 64,828 |
| 25-50k | 5,738 | 7,817 | 42,724 | 42,724 | 42,724 |
| 50-100k | 5,644 | 6,921 | 36,384 | 36,384 | 36,384 |
| 100-200k | 3,618 | 3,636 | 17,454 | 17,454 | 17,454 |
| 200k+ | 3,143 | 1,471 | 4,927 | 4,927 | 4,927 |

| filing status | actual SCF | pretrain SCF | pretrain PUF-style | posttrain PUF-style | posttrain, wealth targeted |
|---|---:|---:|---:|---:|---:|
| joint | 16,233 | 16,290 | 72,008 | 72,008 | 72,008 |
| not joint | 18,144 | 23,710 | 127,992 | 127,992 | 127,992 |

| dividends | actual SCF | pretrain SCF | pretrain PUF-style | posttrain PUF-style | posttrain, wealth targeted |
|---|---:|---:|---:|---:|---:|
| 0 | 26,427 | 35,772 | 164,521 | 164,521 | 164,521 |
| 1-1k | 1,937 | 1,715 | 18,934 | 18,934 | 18,934 |
| 1k-10k | 2,067 | 1,567 | 11,865 | 11,865 | 11,865 |
| 10k+ | 3,946 | 946 | 4,680 | 4,680 | 4,680 |
## What stands out

- The SCF-style generated records are close to the actual SCF: $78.9
  trillion against $85.8 trillion, with a top 1% share of 43.2% against
  39.7%.
- The PUF-style records hold almost twice the SCF's wealth: $165 trillion,
  with a mean of $1.04 million against $614,000. Most of the excess is in
  units with no wages: their mean is $2.38 million against $663,000, and
  31.5% of them are above $1 million against 11.9%. The tilt to the 136
  income targets raises the total further, to $176 trillion.
- With the wealth targets added, the total falls to $89.1 trillion. Means
  by filing status and dividend group come close to the actual SCF. The very
  top thins out, though. The top 0.1% share is 9.27% against 15.3%, and
  among wage earners above $200,000, 44.6% are above $1 million against
  72.6%.

## 2026-10-14 (later): wealth as a target

At John's request, the tilt was rerun with wealth targets added. This is the
fifth column above.

**Targets.** They come from the 2019 SCF wave, built into tax units the way
step 1 built the 2016 wave: the production builder, with split-off units
dropped (`06_build_scf2019.R`, run by `06_run_scf2019.sbatch`, Slurm job
29594080). That gives 31,830 unit rows: 5,777 households × 5 implicates,
143.2 million weighted units. The units are saved as training data for the
next round at
`/nfs/roberts/scratch/pi_nrs36/jar335/minimal/data/scf2019_units.parquet`.

- **Years.** Wealth in the 2019 wave is measured in 2019, which matches a
  tax-year-2019 file. The incomes in that wave are for 2018. The wealth
  targets do not use incomes.
- **Dollars.** The SCFP file is in 2022 dollars. For this wave, the summary
  file's asset items divided by the raw items give 1.1592 on every record
  (savings bonds, SAVBND / X3902, and cash value of life insurance,
  CASHLI / X4006). Wages give 1.1802, matching the crosswalk note; they
  differ because incomes are a year earlier. Net worth in 2019 dollars is
  the SCFP value divided by 1.1592: $96.1 trillion. For the tilt, net worth
  is put in 2015 dollars, the basis of the generated records, by dividing
  the SCFP value by 1.2493, the factor step 1 used for the 2016 wave. That
  gives $89.1 trillion. By the same ratio, $1 million in 2019 dollars is
  $927,884 in 2015 dollars.
- **The six targets**, weighted over the 2019 units: total net worth; net
  worth held by the top 1%, next 9%, next 40% and bottom 50% of units
  ranked by net worth; and the number of units above $1 million (2019
  dollars). The 136 income targets stay exact.

**How the tilt treats them.** They are soft targets, each with a standard
deviation of 2% of the target. This uses the per-target tolerance already in
`05_tilt.py`'s solver. It minimizes the move plus the sum, over the soft
targets, of (miss / target)² / (2 × 0.02²). The four group targets depend on
each record's rank under the new weights, so the solve is repeated:

1. Fix the ranks.
2. Solve the tilt from the starting weights.
3. Recompute the ranks.
4. Repeat until every wealth statistic changes by less than one part in
   10,000.

That took **16 passes**. In the last two passes no record changed group.

**The move.** The move is measured from the equal starting weights.

| | Move (nats) | Effective sample size ratio | Smallest and largest weight factor |
|---|---:|---:|---:|
| 136 income targets | 0.00903 | 0.980 | 0.116 to 11.8 |
| 136 income targets + 6 wealth targets | 0.0749 | 0.859 | 0.00000000002 to 15.4 |

With the wealth targets, 241 records fall below a tenth of their starting
weight, and 5 rise above ten times.

**Wealth targets after the tilt** (trillions of 2015 dollars; units in
millions):

| Target | 2019 SCF | Pretrain PUF-style | Posttrain, income only | Posttrain, wealth targeted | Miss |
|---|---:|---:|---:|---:|---:|
| Total net worth | 89.1 | 165 | 176 | 89.1 | +0.0008% |
| Top 1% | 34.2 | 73.0 | 76.6 | 34.2 | −0.0001% |
| Next 9% | 35.1 | 59.5 | 65.5 | 35.1 | +0.0002% |
| Next 40% | 18.9 | 31.1 | 32.9 | 18.9 | +0.004% |
| Bottom 50% | 0.956 | 1.07 | 1.18 | 0.956 | −0.002% |
| Units above $1 million | 15.3 | 25.3 | 26.6 | 15.3 | +0.004% |

The soft targets are hit almost exactly. A miss of one standard deviation
(2%) adds 0.5 to the quantity minimized. That is far more than the whole
move of 0.075 nats, so the solver gives up almost nothing on these targets.
A 2% standard deviation is not soft at this scale.

The groups are ranked within each file. The 2019 SCF has 143 million units
and the PUF-style file has 158 million returns, so "top 1%" covers a
different number of units on the two sides.

Files in `/nfs/roberts/scratch/pi_nrs36/jar335/minimal/wealth/`:
`moves.csv`, `wealth_targets.csv`, `wealth_tilt_passes.csv`,
`weights_wealth.parquet`. Checks on the 2019 units are in
`/nfs/roberts/scratch/pi_nrs36/jar335/minimal/data/checks_scf2019.csv`. The
units add back to each household's raw income items and SCFP net worth.
