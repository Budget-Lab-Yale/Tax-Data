---
title: "Minimal example, step 1: the stacked table"
role: evidence
workstream: rebuild
status: current
updated: 2026-10-14
sot: research/rebuild/design/plan.md
supersedes: []
superseded_by: null
---

# Minimal example, step 1: the stacked table

**Coarsened zeros: dollar lines, added 2026-10-14 (late).**

`01_coarsen.py` applies the rule in
`research/rebuild/minimal/08_STATE_2026-10-14.md` ("Rule for the dollar
line"). The rule rests on one assumption: when fewer SCF units than PUF
returns hold an item, the SCF is missing small amounts. The line f is set
so that the PUF's share above f equals the SCF's holder share.

- **Comparison year.** The PUF 2015 share is compared with every SCF year,
  since there is no later PUF.
- **Capital gains** use the size of the gain or loss, so losses count as
  holders. The SCF asks for the loss before the $3,000 limit and the PUF
  reports it after.

Shares of units holding the item (weighted), and the line:

| | PUF 2015 | SCF 2015 | f | SCF 2018 | f | SCF 2021 | f |
|---|---|---|---|---|---|---|---|
| Interest | 28.3% | 12.6% | $110 | 13.2% | $100 | 15.3% | $70 |
| Dividends | 18.4% | 12.0% | $290 | 11.7% | $320 | 15.5% | $60 |
| Capital gains | 13.3% | 8.4% | $2,650 | 8.5% | $2,450 | 12.4% | no line (within 1 point) |

Every variable got a line except 2021 capital gains. In no case was the
SCF share higher than the PUF's.

**What the new columns mean.** Both parquet files gained
`interest_coarse_hi`, `dividends_coarse_hi`, `capital_gains_coarse_hi` and
`capital_gains_coarse_neg`.

- For SCF rows at exactly zero, `_coarse_hi` is the highest positive band,
  on that SCF year's own bands, whose top edge is at or below f. The row's
  zero then means "zero, or any band up to this one".
- `capital_gains_coarse_neg` is the most negative loss band that lies wholly
  between −f and 0. Here it is always band 10, the smallest losses.
- Every other row has −1. The band columns and `band_edges*.json` are
  unchanged.
- The lines and shares are in `coarsen_lines.json`; the check table is in
  `coarsen_check.csv`.

**The check does not support the assumption.** If small amounts were all
that is missing, the SCF and PUF holders above f should look alike. They do
not:

| SCF 2015 (PUF 2015 in brackets), holders above f | 10th percentile | 50th | 90th |
|---|---|---|---|
| Interest | $200 ($150) | $1,000 ($520) | $14,000 ($6,460) |
| Dividends | $500 ($470) | $3,600 ($2,500) | $40,000 ($22,300) |
| Capital gains (size) | $3,000 ($3,000) | $13,000 ($3,870) | $128,000 ($52,100) |

- Above f, the SCF's amounts run 1.5 to 3 times the PUF's at the median and
  the 90th percentile. 2018 and 2021 look the same.
- So the SCF is short on small holders, and its holders also report larger
  amounts.
- The bands at or below f still hold many SCF holders: 36% of the SCF's
  interest holder weight, 22% for dividends and 25% for capital gains in
  2015. The SCF is not empty below f.
- The 2021 dividend line ($60) is low because the 2021 SCF share (15.5%) is
  close to the PUF 2015 share.

**Several-years table, added 2026-10-14 (late).**

There is now a second table with the PUF and three SCF waves. It is written
to new files, and the single-year files above are unchanged:
`stacked_years.parquet`, `band_edges_years.json`, `band_report_years.csv` and
`summary_years.csv`, in the same data folder.

- **How the waves were built.** 2016 and 2022 came from `01_build_table.R`
  run by wave (`--scf-wave`). 2019 came from `06_build_scf2019.R`, written by
  another agent. All three follow the same rules: taxable interest only,
  split-off units dropped, and incomes from the interview items in the tax
  year's dollars.
- **Years and bands.** `year` is the tax year: 2015, 2018 or 2021. Bands are
  computed within source and tax year, with the same 84 bands as above.
- **Split.** The PUF rows and the 2015 SCF rows keep their labels from
  `stacked.parquet`. The new waves are split 80/10/10 by household.
- **New IDs.** SCF record and household ids for 2018 and 2021 have
  100 million × (year − 2015) added, so no id repeats across waves.

SCF waves (weighted; totals in billions of dollars):

| | 2016 wave (tax year 2015) | 2019 wave (2018) | 2022 wave (2021) |
|---|---|---|---|
| Rows (households × 5 implicates) | 34,377 (6,248) | 31,830 (5,777) | 25,600 (4,595) |
| Weighted units (millions) | 139.6 | 143.2 | 148.1 |
| Wages: total, share with any | 7,667, 70% | 8,206, 70% | 10,124, 69% |
| Taxable interest | 90, 12.6% | 117, 13.2% | 134, 15.3% |
| Dividends | 250, 12.0% | 251, 11.7% | 365, 15.5% |
| Capital gains (gain or loss) | 678, 8.4% | 724, 8.5% | 1,762, 12.4% |
| Net worth: total, share not zero | 86,865, 98% | 96,071, 98% | 139,115, 98% |
| Income factor (SCFP over raw wages) | 1.2493 | 1.1802 | 1.0809 |
| Asset factor (used for net worth) | 1.2334 | 1.1592 | 1.0000 |

- **Dollars.** Incomes are in the tax year's own dollars. Net worth is in the
  interview year's dollars: the SCF's 2022-dollar figure divided by the
  wave's asset factor. Each factor was checked: it is the same for every
  record, and the two asset items checked agree on it.
- **2016 net worth differs between the two tables.** In this table it is
  1.3% higher than in `stacked.parquet`, which used the income factor (2015
  prices). Its bands are the same.

Odd things:

- **Wages coded −1 in 2022.** In the 2022 wave, 4 households (20 rows) report
  wages of −1 after saying they had wages. The 2022 codebook lists no −1 code
  for wages; −1 means "nothing" on the items where it is listed. I set these
  to zero. The SCF's own summary file keeps them as −$1.
- **Capital gains in 2021** are 2.4 times their 2018 level, and 12.4% of
  units have some. I take this as the 2021 market, but I did not check it
  against SOI.
- **No SCF totals were checked against SOI** for 2018 or 2021.
- **The 2019 wave** was checked only through its own checks file: units add
  up to the household items, and the factors are constant.

**Changes on 2026-10-14, decided by John, after the first build:**

1. The SCF split-off units were dropped. These are financially independent
   adults living in an SCF household. The SCF does not ask about their wealth,
   so their net worth was missing.
2. Interest is now taxable interest only, on both sides.
3. A sixth amount was added, `other_agi`. It is the rest of AGI, and it
   exists for PUF records only.
4. Losses now get 10 bands instead of 4, finer at the extreme (section 4).
   Before this change, the most negative band of other_agi ran from about
   −$5,300 down to −$137 million. A dollar drawn within that band could give
   an ordinary return a huge loss. The table itself did not change; only the
   bands were redone (Slurm job 29589960).

The table's numbers are from the rebuild (Slurm job 29586258).

## 1. What ran and what it wrote

`research/rebuild/minimal/01_run.sbatch` runs two scripts.

- `01_build_table.R` builds the table. It reads the 2015 PUF the way
  `src/process_puf.R` does, and builds the 2016 SCF tax units with the
  production builder.
- `01_band.py` adds the bands and the train / validation / test split.

Both sources are tax year 2015, in 2015 dollars. No filing rule was applied.
The only records dropped are the PUF's four aggregate records and the SCF
split-off units.

The outputs are in `/nfs/roberts/scratch/pi_nrs36/jar335/minimal/data/`:

- `stacked.parquet`: the table.
- `band_edges.json`: the dollar cut points for each band.
- `summary.csv` and `band_report.csv`: the numbers quoted below.
- `checks_r.csv`: the build checks.

The log is `/nfs/roberts/scratch/pi_nrs36/jar335/minimal/logs/01_run_29586258.out`.

**Rows:** 242,069 in total.

- PUF: 207,692 records, 150.8 million weighted returns.
- SCF: 34,377 rows (6,248 households × 5 implicates), 139.6 million weighted
  units.

**Columns.**

| Variable | PUF | SCF |
|---|---|---|
| married | MARS = 2 | the builder's couples kept as one unit |
| n_dep | XOCAH + XOCAWH + XOPAR + XOODEP, capped at 3 | the builder's n_dep, capped at 3 |
| wages | E00200 | X5702 |
| interest (taxable) | E00300 | X5708 |
| dividends | E00600 | X5710 |
| capital_gains | E01000 | X5712 |
| other_agi | E00100 − E00200 − E00300 − E00600 − E01000 | missing |
| net_worth | missing | SCFP net worth |

- The PUF weight is S006 / 100. The SCF weight is X42001 / 5.
- other_agi is missing for every SCF row, and net_worth for every PUF row.
  These are the only missing values. Their band columns hold −1.
- For PUF records, wages + interest + dividends + capital_gains + other_agi
  equals AGI (E00100) exactly. The build checks this on every record.

## 2. The two sources

Totals are in billions of 2015 dollars. Shares are weighted. Percentiles are
taken over all units, including zeros.

**PUF**

| | wages | interest | dividends | capital gains | other AGI |
|---|---|---|---|---|---|
| Total ($bn) | 7,145 | 89 | 241 | 615 | 2,021 |
| Share at zero | 17% | 72% | 82% | 87% | 34% |
| Share negative | 0 | 0 | 0 | 5.6% | 18% |
| 50th percentile | 26,000 | 0 | 0 | 0 | 0 |
| 90th | 109,300 | 200 | 620 | 0 | 40,850 |
| 99th | 315,000 | 8,410 | 26,300 | 41,700 | 178,800 |
| 99.9th | 1,004,000 | 70,900 | 168,600 | 501,800 | 907,900 |

**SCF**

| | wages | interest | dividends | capital gains | net worth |
|---|---|---|---|---|---|
| Total ($bn) | 7,667 | 90 | 250 | 678 | 85,757 |
| Share at zero | 30% | 87% | 88% | 92% | 2.4% |
| Share negative | 0 | 0 | 0 | 1.8% | 11% |
| 50th percentile | 25,789 | 0 | 0 | 0 | 74,705 |
| 90th | 120,000 | 40 | 140 | 0 | 1,039,223 |
| 99th | 425,000 | 11,000 | 36,000 | 60,000 | 9,662,363 |
| 99.9th | 2,200,000 | 100,000 | 207,000 | 660,000 | 41,506,639 |

**Married and dependents.** 36% of PUF units are married filing jointly,
against 41% of SCF units. 65% of PUF units have no dependents, against 66% of
SCF units.

**PUF against SOI's published 2015 totals** (Pub 1304 Table 1.4, from the
shared store's parse at
`/nfs/roberts/project/pi_nrs36/shared/raw_data/IRS-Ind/aligned/`). The repo's
own SOI tables do not cover 2015.

| | SOI | PUF | PUF plus the 4 aggregate records |
|---|---|---|---|
| Wages | 7,112 | 7,145 | 7,156 |
| Taxable interest | 96 | 89 | 98 |
| Dividends | 260 | 241 | 261 |
| Capital gains (line 13) | 706 | 615 | 701 |
| AGI | 10,210 | 10,111 | 10,251 |

The PUF falls short on interest, dividends and capital gains because the four
aggregate records hold the very top. With them added back, every total is
within 2% of SOI.

## 3. Who has any interest, dividends or capital gains

| | PUF | PUF without dependent filers | SCF |
|---|---|---|---|
| Interest | 28.3% | 29.6% | 12.6% |
| Dividends | 18.4% | 18.9% | 12.0% |
| Capital gains | 13.3% | 13.8% | 8.4% |

- The earlier test's 19% (PUF) against 12% (SCF) for dividends reproduces.
- The PUF's 28.3% with taxable interest matches SOI, whose returns with
  taxable interest are 28.3% of all returns.
- The gap between the sources is largest for interest.

## 4. Bands

Every variable uses the same 84 bands:

- band 0 is exactly zero;
- bands 1–10 are losses, with band 1 the most negative;
- bands 11–83 are positives, finer at the top (2% steps to the 90th
  percentile, then 1%, 0.1% and 0.01%).

The loss bands mirror the positive top. Measured by weight among the
negative values, starting from the smallest losses:

- four bands of 22.5% cover the smallest 90% of losses;
- three bands of 3% run to 99%;
- three bands of 0.5%, 0.4% and 0.1% cover the largest 1%.

Bands are cut by weighted quantile within each source. A value that sits
exactly on a cut goes to the lower-dollar band. When one dollar value holds
more weight than a band's width, the bands it swallows are empty.

Bands that are not empty, out of 84:

| | PUF | SCF |
|---|---|---|
| wages | 74 | 73 |
| interest | 58 | 64 |
| dividends | 72 | 72 |
| capital gains | 77 | 75 |
| other AGI | 84 | — |
| net worth | — | 84 |

- Wages, interest and dividends have no losses, so bands 1–10 are always
  empty for them.
- The other empty bands come from round amounts: $10 or $20 of interest, and
  round answers in the SCF.

Loss band edges (dollars; each band runs from its lower edge up to the next
band's):

| Band | PUF other AGI | PUF capital gains | SCF capital gains |
|---|---|---|---|
| 1 (most negative) | −136.7 million to −1,686,400 | exactly −3,000 | −1,000,000 |
| 2 | to −326,720 | empty | to −146,667 |
| 3 | to −168,900 | empty | to −85,000 |
| 4 | to −45,200 | empty | to −35,000 |
| 5 | to −24,300 | empty | to −24,000 |
| 6 | to −16,570 | empty | to −20,000 |
| 7 | to −3,600 | empty | to −3,000 |
| 8 | to −1,400 | empty | empty |
| 9 | to −330 | −2,990 to −1,020 | to −980 |
| 10 | to −2 | to −2 | to −0.22 |

- **PUF capital gains.** 63% of losses sit exactly at the −$3,000 limit, so
  band 1 holds only that value and bands 2–8 are empty.
- **PUF other AGI.** Band 1 now holds the most negative 0.1% of losses:
  3,413 records with 27,695 weighted returns and a median of −$3.0 million.
  It still spans a wide range, from −$1.7 million down to −$137 million.
  Most of these are high-income records.
- **SCF net worth.** The loss bands run from −$3,524 to −$342,367, and band
  1 goes down to −$2.1 million. Band 1 holds 8 rows.
- **Thin SCF bands.** The SCF's extreme bands hold few rows. One
  positive capital-gains band holds a single row, and loss band 1 holds 10
  rows, all at −$1 million.

**Split.** 80 / 10 / 10 with fixed seeds. PUF records are split one by one;
SCF households are kept whole. Dropping the split-off units removed no
household, so the remaining SCF rows kept their split.

## 5. SCF items and the dollar factor

- The SCF items come from the 2016 codebook, and each names its Form 1040
  line.
  - X5708, "other interest", is line 8a, taxable interest.
  - X5706 is line 8b, tax-exempt interest. It is no longer used.
  - Wages are X5702, dividends X5710, and capital gains X5712. For X5712,
    the code "−1, nothing" was set to zero.
- The production builder splits these items between the units of a
  household. The units add back to the household's items exactly.
- **Dollar factor.** The SCF summary file is in 2022 dollars. Its wages are
  exactly 1.2493 times the raw 2015 wages for every record, as the crosswalk
  note said. Net worth was divided by 1.2493, which puts it in 2015 prices.
- **A different factor for assets.** The summary file's asset items use
  1.2334, because assets were measured at the 2016 interview. In 2016
  dollars, net worth would be 1.3% higher. This changes the dollar edges, not
  the bands.

## 6. Decisions and things not checked

- `src/configure.R` was not run, because it writes into the shared
  production folder. The script builds the same input paths from the same
  settings files.
- Only the builder's function was loaded from its file. The rest of the file
  would overwrite the production SCF cache.
- PUF dependent filers are kept: 6,526 records, 9.3 million weighted.
- **SCF married couples who file separately.** The builder splits them into
  two units, and each is coded unmarried, as the PUF codes married filing
  separately.
- **Capital gains are not defined quite the same way.** The PUF item is
  line 13, after the $3,000 loss limit. The SCF question covers lines 13 and
  14, and asks for losses before the limit.
- **Not checked:**
  - SCF totals against outside sources (SCF wages are 7% above the PUF's).
  - Whether these definition differences explain the gaps in section 3.
