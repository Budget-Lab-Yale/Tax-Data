# What the pipeline does now

The current run ("latest") produces a 2019 file of tax units: every PUF
variable, plus net worth from the SCF, weighted to 2019 targets. The code is
in `pipeline/`; every setting is in `pipeline/settings.json`.

## Data

**Sources:**

| Source | Rows | What it covers |
|---|---:|---|
| 2015 PUF | 207,692 | filers |
| 2017 CPS non-filer pool | 166,938 | crude non-filers on the PUF side, dollars put in 2015 terms |
| SCF waves 2013, 2016, 2019, 2022 | 33,058 / 34,377 / 31,830 / 25,600 | tax years 2012–2021, split into tax units by the production builder, five implicates each |

**Variables:**
- marital status (joint or not), dependents (0–3), and the filing flag;
- wages, taxable interest, ordinary dividends, and capital gains in four
  pieces (short term, long term, distributions, other);
- more AGI items: other gains, taxable IRA distributions, taxable
  pensions, taxable Social Security, unemployment, alimony, state refunds,
  adjustments, and the rest of AGI ("other income");
- two items outside AGI: gross Social Security plus pensions, and
  tax-exempt interest;
- business income in its 18 PUF parts: Schedule C; partnership and
  S corporation income, losses and section 179, passive and nonpassive;
  rents, royalties, estates and trusts; farm rental; farm;
- net worth (SCF only).

The sources share married, dependents, wages, interest, dividends, capital
gains, business income, gross retirement income and tax-exempt interest.
The rest belong to one source; on the other source the slot is absent.

**Amounts as bands.** Each amount is coded as one of 84 bands:
- band 0 is exactly zero;
- 10 bands of losses;
- 73 bands of positives, finer at the top. The top bands each hold 0.01%
  of positive weight.

A band becomes dollars again by drawing a real record's value from that
band.

## Translation: putting the SCF on the PUF's terms

- **SCF dollars on the PUF's dollar scale.** Wages, interest, dividends,
  capital gains, retirement income and tax-exempt interest are banded on
  the PUF's cut points, grown by SOI to each wave's year. The SCF's own
  pattern of zeros stays visible, through a source label the model always
  sees.
- **Business income** is placed by its rank within each SCF wave.
- **Capital losses** below −$3,000 in the SCF are read as −$3,000, the
  deductible limit on the return.
- **The cell method, during training.** Many SCF households that report
  zero interest, dividends or capital gains do hold some.
  - In cells of wage group by marital status, the PUF's shares by size set
    how many hidden holders to fill in, and at what sizes. Only below a top
    line: $2,000 for interest and dividends, $10,000 for gains.
  - The model decides which households, redrawing them as it trains.

## The model

- **Network.** A transformer: 4 layers, width 128, about 1 million
  parameters, no dropout.
- **What it learns.** It is trained to fill in any hidden subset of a
  record's variables from the rest. So one network answers every question
  of the form "this variable, given those".
- **Weights.** Survey weights are used as they are, rescaled within each
  source.
- **Training.** 80 passes, all records but a 2% held-back slice.
- **Why no dropout.** Dropout made rare items come out 10–20% too low
  (`HISTORY.md`).

## Generation

**One record is drawn slot by slot:**
1. the PUF variables, in a random order;
2. line-13 capital gains and business income, computed from their parts;
3. net worth last, as the SCF would see the household (SCF label,
   PUF-only variables left out).

**The stack.** 400,000 records in four parts. Each part oversamples the
top 1% of one variable (wages, long-term gains, S corporation nonpassive
income, net worth). Weights undo the oversampling exactly.

## Post-training: the 2019 file

**Dollars.** Each band's real 2015 values, grown to 2019 by SOI's growth
in dollars per return holding each item.

**Start.**
- Weights are scaled to SOI's 2019 return count and the non-filer count.
- Filers are then scaled within joint and non-joint returns to SOI's 2019
  counts.

**The tilt.** The smallest reweighting, by Kullback–Leibler divergence,
that meets:

| Targets | Number | How they are met |
|---|---:|---|
| IRS (SOI 2019 Table 1.4): returns, and the number with and amount of wages, interest, dividends and capital gains, in 15 AGI brackets; plus joint returns | 136 | exactly |
| Non-filers | 1 | exactly |
| IRS business: six families, net income and net loss, returns and dollars, in 6 AGI groups | about 140 | exactly, or within 2% if that fails |
| SCF 2019 wealth: units and net worth above the 99th, 90th and 50th percentiles, total net worth, units above $1 million; counts on the file's own units | 8 | within 2% |

## How the latest run comes out

Before the tilt:
- partnership nonpassive holders 1.24% against the PUF's 1.33%;
- business income 90% of the PUF's;
- net worth $99 trillion against the SCF's $96 trillion.

After the tilt:
- every target met;
- 90% of the effective sample kept;
- median net worth $90,800 (SCF $91,100);
- top 1% share 38.4% (SCF 38.4%).

Known gaps are listed at the end of `ROADMAP.md`.
