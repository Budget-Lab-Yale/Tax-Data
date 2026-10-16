---
title: "Minimal example: baseline v3, oversampled stack tilted to 2019 IRS and SCF targets"
role: evidence
workstream: rebuild
status: superseded
updated: 2026-10-14
sot: research/rebuild/design/plan.md
supersedes: []
superseded_by: research/rebuild/minimal/15_BASELINE_V4.md
---

# Minimal example: baseline v3, oversampled stack tilted to 2019 IRS and SCF targets

> **Note, 15 October 2026.** This is the v3 baseline, built on the old model (before the wealth fix).
> `15_BASELINE_V4.md` replaces it.

This is the baseline. Later changes are compared against it.

Script: `07_baseline.py`, run by `07_run.sbatch` (Slurm job 29596322).
Outputs: `/nfs/roberts/scratch/pi_nrs36/jar335/minimal/tilt/v3/`.

## The file

`gen/gen_puf_stack_v3.parquet` holds 400,000 generated PUF-style records,
each with net worth, from the step 2 model. It stacks four parts of 100,000
records. Each part oversamples the top of one amount: wages, capital gains,
other_agi or net worth. Within a part, the bands that cover the top 1% are
drawn 10 times as often as the model would draw them. The `weight` column
holds importance weights: the model's probability of the record's band over
the probability it was drawn with. They sum to 100,000 within each part.
Because of these weights, the 400,000 records are worth 378,933 equally
weighted ones.

- **Dollars.** Incomes are drawn from the step 4 2019 PUF map, as in run B1.
  Net worth is drawn from the 2015 SCF map and stays in 2015 dollars.
- **Starting weights.** The importance weights, scaled to SOI's 157.8
  million returns for 2019.

## Targets

- **The 136 targets of run B1, exact.** For each AGI bracket: the number of
  returns, and for wages, taxable interest, dividends and capital gains the
  number of returns with the item and its amount. Plus joint returns. All
  from SOI for 2019.
- **Six wealth targets, soft.** They come from the 2019 SCF wave's tax units,
  built in step 6 (`06_WEALTH_TABLES.md`, last section). In 2015 dollars,
  the targets are:
  - total net worth;
  - the net worth held by the top 1%, the next 9%, the next 40% and the
    bottom 50% of units, ranked by net worth;
  - the number of units above $1 million in 2019 dollars ($927,884 in 2015
    dollars).

  Each wealth target has a standard deviation of 2% of its value. The ranks
  are recomputed after each solve until they settle, as in step 6. That took
  **14 passes**.

## Move and weights

The move is measured from the starting importance weights.

| | Move (nats) | Effective sample size, before → after | Ratio | Smallest weight factor | Largest weight factor | Records below 0.1× | Records above 10× |
|---|---:|---:|---:|---:|---:|---:|---:|
| 136 targets only | 0.00931 | 378,933 → 369,459 | 0.975 | 0.00000051 | 55.9 | 6 | 6 |
| **136 + 6 wealth targets (baseline)** | **0.0772** | 378,933 → 319,227 | **0.842** | 0.00000000001 | 64.5 | 1,010 | 8 |

Every one of the 136 income targets is met exactly.

## Wealth targets

Values are in trillions of 2015 dollars. The last row is in millions of
units.

| Target | 2019 SCF | Pretrain | Posttrain, 136 only | Posttrain (baseline) | Miss | Redraw | Redraw miss |
|---|---:|---:|---:|---:|---:|---:|---:|
| Total net worth | 89.1 | 171 | 184 | 89.1 | +0.0009% | 91.8 | +2.95% |
| Top 1% | 34.2 | 78.8 | 83.5 | 34.2 | −0.003% | 36.0 | +5.15% |
| Next 9% | 35.1 | 59.7 | 66.5 | 35.1 | +0.003% | 35.9 | +2.27% |
| Next 40% | 18.9 | 31.4 | 33.2 | 18.9 | +0.005% | 19.0 | +0.42% |
| Bottom 50% | 0.956 | 1.10 | 1.21 | 0.955 | −0.016% | 0.955 | −0.04% |
| Units above $1 million | 15.3 | 25.5 | 26.8 | 15.3 | +0.004% | 15.3 | +0.31% |

As in step 6, the soft targets are met almost exactly. A miss of one
standard deviation would add 0.5 to the quantity minimized, and the whole
move is 0.077 nats.

## Redraw

The redraw takes 200,000 records, with replacement, with probability
proportional to the tilted weight. Each gets an equal weight. It picked
150,780 distinct records.

- **The 136 income targets.** 134 are within 2 standard errors. The two
  that are not:
  - returns with no AGI: −4.1%, 2.2 standard errors;
  - returns with taxable interest at $10,000–15,000 of AGI: −6.9%, 3.1
    standard errors.
- **The largest misses in percent** are in amounts that few records carry:
  - capital gains with no AGI: −76%, 1.3 standard errors;
  - dividends with no AGI: −29%;
  - dividends at $1 million or more: +26%.
- **Wealth targets on the redraw** (ranks recomputed on the redrawn file) are
  in the table above. The top 1% is 5.1% high, and total net worth 3.0% high.

## Untargeted relationships

"Actual 2015 PUF" is the real PUF with its survey weights and its dollars
grown to 2019, the "Real" column of step 5. "Pretrain" uses the starting
weights and "posttrain" the baseline weights. The groups use 2019 dollars.

**Any dividends, by wage group and filing status** (% of returns)

| wages | filing status | actual 2015 PUF | pretrain | posttrain |
|---|---|---:|---:|---:|
| 0 | joint | 43.4 | 45.4 | 46.4 |
| 0 | not joint | 31.1 | 27.1 | 28.4 |
| 1-25k | joint | 22.9 | 23.8 | 23.5 |
| 1-25k | not joint | 5.20 | 4.70 | 4.77 |
| 25-50k | joint | 15.3 | 17.2 | 16.5 |
| 25-50k | not joint | 5.91 | 5.24 | 5.28 |
| 50-100k | joint | 18.9 | 20.7 | 20.7 |
| 50-100k | not joint | 13.2 | 11.7 | 12.0 |
| 100-200k | joint | 30.3 | 30.5 | 32.6 |
| 100-200k | not joint | 28.8 | 24.9 | 26.4 |
| 200k+ | joint | 61.1 | 57.2 | 62.8 |
| 200k+ | not joint | 57.7 | 56.3 | 62.7 |

**Any capital gains, by dividend group** (% of returns)

| dividends | actual 2015 PUF | pretrain | posttrain |
|---|---:|---:|---:|
| 0 | 3.66 | 3.73 | 3.90 |
| 1-1k | 38.7 | 38.1 | 40.8 |
| 1k-10k | 67.5 | 67.5 | 70.7 |
| 10k-100k | 87.0 | 84.0 | 86.2 |
| 100k+ | 93.5 | 84.0 | 86.0 |

**Number of dependents, by filing status** (% of returns)

| filing status | dependents | actual 2015 PUF | pretrain | posttrain |
|---|---|---:|---:|---:|
| joint | 0 | 49.7 | 53.6 | 53.4 |
| joint | 1 | 18.1 | 18.0 | 18.0 |
| joint | 2 | 19.8 | 18.4 | 18.6 |
| joint | 3+ | 12.3 | 9.99 | 10.0 |
| not joint | 0 | 73.3 | 78.5 | 78.2 |
| not joint | 1 | 15.2 | 12.9 | 12.9 |
| not joint | 2 | 8.16 | 6.22 | 6.36 |
| not joint | 3+ | 3.39 | 2.46 | 2.52 |

## Wealth tables

All net worth is in 2015 dollars.

- **The two actual SCF columns.** The 2016 wave is the step 1 table. The
  2019 wave is the step 6 units. Both have survey weights.
- **The 2019 wave's incomes are for 2018**, in 2018 dollars. Its wage and
  dividend groups are therefore a year and a price level off from the other
  columns. The by-wage and by-dividend tables are affected; the others are
  not.
- **The generated columns** group records by their 2015-dollar draws.

### Table 1. Net worth, all units

|  | actual SCF (2016 wave) | actual SCF (2019 wave) | pretrain | posttrain |
|---|---:|---:|---:|---:|
| units (millions) | 140 | 143 | 158 | 158 |
| total net worth ($ trillion) | 85.8 | 89.1 | 171 | 89.1 |
| mean ($) | 614,000 | 623,000 | 1,080,000 | 565,000 |
| median ($) | 74,700 | 84,500 | 106,000 | 66,600 |
| share negative | 11.0% | 10.7% | 11.6% | 9.80% |
| top 10% share | 78.3% | 77.7% | 81.0% | 77.7% |
| top 1% share | 39.7% | 38.4% | 46.1% | 38.4% |
| top 0.1% share | 15.3% | 14.7% | 18.5% | 9.43% |
| 90th percentile ($) | 1,040,000 | 992,000 | 1,420,000 | 909,000 |
| 99th percentile ($) | 9,660,000 | 9,400,000 | 18,300,000 | 10,700,000 |
| 99.9th percentile ($) | 41,500,000 | 39,100,000 | 92,600,000 | 38,900,000 |

### Table 2a. Mean net worth by group ($)

| wages | actual SCF (2016 wave) | actual SCF (2019 wave) | pretrain | posttrain |
|---|---:|---:|---:|---:|
| 0 | 663,000 | 706,000 | 2,430,000 | 1,170,000 |
| 1-25k | 173,000 | 204,000 | 461,000 | 216,000 |
| 25-50k | 181,000 | 148,000 | 430,000 | 228,000 |
| 50-100k | 353,000 | 324,000 | 817,000 | 428,000 |
| 100-200k | 934,000 | 856,000 | 1,490,000 | 846,000 |
| 200k+ | 5,120,000 | 4,160,000 | 6,230,000 | 3,270,000 |

| filing status | actual SCF (2016 wave) | actual SCF (2019 wave) | pretrain | posttrain |
|---|---:|---:|---:|---:|
| joint | 1,120,000 | 1,200,000 | 2,110,000 | 1,120,000 |
| not joint | 262,000 | 244,000 | 501,000 | 268,000 |

| dividends | actual SCF (2016 wave) | actual SCF (2019 wave) | pretrain | posttrain |
|---|---:|---:|---:|---:|
| 0 | 250,000 | 276,000 | 467,000 | 231,000 |
| 1-1k | 829,000 | 820,000 | 1,720,000 | 929,000 |
| 1k-10k | 2,340,000 | 1,960,000 | 4,510,000 | 2,380,000 |
| 10k+ | 9,290,000 | 10,600,000 | 11,700,000 | 5,940,000 |

### Table 2b. Median net worth by group ($)

| wages | actual SCF (2016 wave) | actual SCF (2019 wave) | pretrain | posttrain |
|---|---:|---:|---:|---:|
| 0 | 96,800 | 109,000 | 421,000 | 225,000 |
| 1-25k | 8,490 | 11,900 | 9,970 | 10,500 |
| 25-50k | 30,100 | 23,900 | 45,300 | 35,600 |
| 50-100k | 109,000 | 101,000 | 169,000 | 111,000 |
| 100-200k | 334,000 | 332,000 | 463,000 | 293,000 |
| 200k+ | 1,790,000 | 1,510,000 | 1,300,000 | 797,000 |

| filing status | actual SCF (2016 wave) | actual SCF (2019 wave) | pretrain | posttrain |
|---|---:|---:|---:|---:|
| joint | 221,000 | 251,000 | 407,000 | 253,000 |
| not joint | 25,400 | 32,000 | 28,800 | 29,000 |

| dividends | actual SCF (2016 wave) | actual SCF (2019 wave) | pretrain | posttrain |
|---|---:|---:|---:|---:|
| 0 | 49,400 | 62,400 | 59,200 | 42,900 |
| 1-1k | 428,000 | 445,000 | 587,000 | 377,000 |
| 1k-10k | 1,320,000 | 1,000,000 | 1,360,000 | 750,000 |
| 10k+ | 4,370,000 | 5,370,000 | 3,610,000 | 2,110,000 |

### Table 2c. Share of units in each group

| wages | actual SCF (2016 wave) | actual SCF (2019 wave) | pretrain | posttrain |
|---|---:|---:|---:|---:|
| 0 | 30.4% | 29.8% | 16.8% | 17.8% |
| 1-25k | 19.5% | 16.7% | 32.4% | 30.7% |
| 25-50k | 19.1% | 18.5% | 21.3% | 22.1% |
| 50-100k | 17.9% | 19.5% | 18.1% | 18.1% |
| 100-200k | 9.32% | 10.9% | 8.88% | 8.61% |
| 200k+ | 3.79% | 4.68% | 2.49% | 2.74% |

| filing status | actual SCF (2016 wave) | actual SCF (2019 wave) | pretrain | posttrain |
|---|---:|---:|---:|---:|
| joint | 40.9% | 39.6% | 36.2% | 34.7% |
| not joint | 59.1% | 60.4% | 63.8% | 65.3% |

| dividends | actual SCF (2016 wave) | actual SCF (2019 wave) | pretrain | posttrain |
|---|---:|---:|---:|---:|
| 0 | 88.0% | 88.3% | 82.4% | 81.6% |
| 1-1k | 5.07% | 4.78% | 9.43% | 10.0% |
| 1k-10k | 4.24% | 4.57% | 5.90% | 5.94% |
| 10k+ | 2.73% | 2.36% | 2.31% | 2.39% |

### Table 3. Share of units with net worth above $1 million

| wages | actual SCF (2016 wave) | actual SCF (2019 wave) | pretrain | posttrain |
|---|---:|---:|---:|---:|
| 0 | 11.9% | 10.9% | 31.6% | 19.7% |
| 1-25k | 3.13% | 3.69% | 6.08% | 3.21% |
| 25-50k | 2.08% | 2.19% | 7.23% | 3.49% |
| 50-100k | 6.06% | 4.22% | 13.8% | 7.22% |
| 100-200k | 21.2% | 17.5% | 27.7% | 16.6% |
| 200k+ | 72.6% | 62.5% | 60.1% | 44.1% |

### Records per group

| wages | actual SCF (2016 wave) | actual SCF (2019 wave) | pretrain | posttrain |
|---|---:|---:|---:|---:|
| 0 | 10,399 | 9,399 | 67,085 | 67,085 |
| 1-25k | 5,835 | 4,586 | 126,724 | 126,724 |
| 25-50k | 5,738 | 5,167 | 83,170 | 83,170 |
| 50-100k | 5,644 | 5,336 | 71,058 | 71,058 |
| 100-200k | 3,618 | 3,875 | 34,981 | 34,981 |
| 200k+ | 3,143 | 3,467 | 16,982 | 16,982 |

| filing status | actual SCF (2016 wave) | actual SCF (2019 wave) | pretrain | posttrain |
|---|---:|---:|---:|---:|
| joint | 16,233 | 14,920 | 150,000 | 150,000 |
| not joint | 18,144 | 16,910 | 250,000 | 250,000 |

| dividends | actual SCF (2016 wave) | actual SCF (2019 wave) | pretrain | posttrain |
|---|---:|---:|---:|---:|
| 0 | 26,427 | 24,125 | 323,375 | 323,375 |
| 1-1k | 1,937 | 1,681 | 38,656 | 38,656 |
| 1k-10k | 2,067 | 2,016 | 25,848 | 25,848 |
| 10k+ | 3,946 | 4,008 | 12,121 | 12,121 |

## What stands out

- **The wealth targets dominate the move.** Adding them raises the move
  from 0.009 to 0.077 nats. The effective sample size ratio falls from 0.975
  to 0.842. 1,010 records end below a tenth of their starting weight.
- **Before the tilt, the stack holds almost twice the 2019 SCF's net
  worth** ($171 trillion against $89.1 trillion). The top 1% holds 46.1%
  against 38.4%.
- **After the tilt, all six wealth targets are met**, but the very top is
  thin. The top 0.1% holds 9.43% against 14.7% in the 2019 wave. Among wage
  earners above $200,000, 44.1% are above $1 million, against 62.5%.
- **The untargeted relationships stay within 7.5 points of the real
  PUF.** The tilt moves none of them by more than 6.4 points. The largest
  gaps after the tilt are:
  - capital gains among those with dividends over $100,000: 86.0% against
    93.5%;
  - dividends among non-joint wage earners above $200,000: 62.7% against
    57.7%;
  - non-joint returns with no dependents: 78.2% against 73.3%.
