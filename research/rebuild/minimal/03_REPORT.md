---
title: "Minimal example, step 3: generation in block order"
role: evidence
workstream: rebuild
status: current
updated: 2026-10-14
sot: research/rebuild/design/plan.md
supersedes: []
superseded_by: null
---

# Minimal example, step 3: generation in block order

## Update, 14 October 2026, near midnight: generation from model v3 (coarsened zeros)

**Bottom line.**

- **The PUF-style files are fine.** Their holder shares still match the PUF.
- **The SCF-style file is broken.** Almost every generated SCF record has
  interest, dividends and capital gains.
- **Wealth for PUF-style records did not improve:** $139 trillion, against
  $131 trillion from v2 and $86 trillion in the SCF.
- **So I have not written `gen/GEN_DONE_v3fix`.** That marker would start
  another agent's v4 stack and tilt from these files.
- **Where the files are.** `gen/gen_scf.parquet`, `gen_puf.parquet`,
  `gen_puf_with_net_worth.parquet`, `gen_puf_fixed_order.parquet` and
  `gen_puf_second_seed.parquet` are now from the v3 model. The v2 versions
  are in `gen/v2/`. Why the model does this is in `02_REPORT.md`.

**What ran.**

- `03_generate.sbatch` (job 29600320) used the same sizes and seeds as
  before. In `gen_puf_with_net_worth.parquet`, net worth is now drawn under
  the SCF label (owner label).
- A CPU job (29600321) then ran `03_wealth_check.py` for the wealth tables
  and the holder shares. Its output is in `wealth/v3fix/tables.md`; the
  step 3 comparison is in `gen/03_tables.md`.

**Holder shares** (weighted share with a nonzero amount, band space):

| file | interest | dividends | capital gains |
|---|---:|---:|---:|
| actual PUF | 28.3% | 18.4% | 13.3% |
| actual SCF, as reported | 12.6% | 12.0% | 8.4% |
| generated SCF-style, v3 | 97.6% | 99.3% | 98.2% |
| generated SCF-style, v2 | 11.1% | 10.6% | 7.4% |
| generated PUF-style, v3 (`gen_puf`) | 28.0% | 18.0% | 13.0% |
| generated PUF-style, v2 (`gen_puf`) | 28.1% | 18.1% | 13.1% |
| generated PUF-style, v3, with net worth | 27.4% | 17.8% | 12.9% |

The generated SCF-style capital gains also hold 15% small losses, against 2%
in the data.

**Table 1. Net worth, all units.**

| | actual SCF | SCF-style v3 | PUF-style v3 (owner label) | PUF-style v2 (owner label) |
|---|---:|---:|---:|---:|
| units (millions) | 140 | 140 | 158 | 158 |
| total ($ trillion) | 85.8 | 161 | 139 | 131 |
| mean ($) | 614,000 | 1,150,000 | 879,000 | 830,000 |
| median ($) | 74,700 | 478,000 | 119,000 | 131,000 |
| share negative | 11.0% | 2.80% | 11.5% | 10.0% |
| top 10% share | 78.3% | 58.0% | 75.5% | 74.7% |
| top 1% share | 39.7% | 27.2% | 35.9% | 36.3% |
| top 0.1% share | 15.3% | 10.4% | 13.8% | 14.1% |
| 90th percentile ($) | 1,040,000 | 2,040,000 | 1,500,000 | 1,390,000 |
| 99th percentile ($) | 9,660,000 | 10,600,000 | 11,600,000 | 11,500,000 |
| 99.9th percentile ($) | 41,500,000 | 52,900,000 | 47,400,000 | 45,900,000 |

**Mean net worth by wage group ($).**

| wages | actual SCF | SCF-style v3 | PUF-style v3 | PUF-style v2 |
|---|---:|---:|---:|---:|
| 0 | 663,000 | 1,390,000 | 1,990,000 | 1,760,000 |
| 1-25k | 173,000 | 860,000 | 337,000 | 304,000 |
| 25-50k | 181,000 | 507,000 | 322,000 | 311,000 |
| 50-100k | 353,000 | 699,000 | 652,000 | 647,000 |
| 100-200k | 934,000 | 1,120,000 | 1,310,000 | 1,350,000 |
| 200k+ | 5,120,000 | 3,590,000 | 5,300,000 | 5,450,000 |

**Share of units with net worth above $1 million.**

| wages | actual SCF | SCF-style v3 | PUF-style v3 | PUF-style v2 |
|---|---:|---:|---:|---:|
| 0 | 11.9% | 33.0% | 34.6% | 31.0% |
| 1-25k | 3.13% | 18.3% | 5.63% | 5.17% |
| 25-50k | 2.08% | 11.4% | 5.94% | 5.58% |
| 50-100k | 6.06% | 17.5% | 11.9% | 11.9% |
| 100-200k | 21.2% | 33.6% | 30.1% | 30.3% |
| 200k+ | 72.6% | 62.0% | 69.4% | 70.9% |

**Reading it.**

- **SCF-style records are now far too wealthy below the top.** The model
  gives nearly all of them investment income, and net worth follows.
- **PUF-style records' wealth is essentially unchanged from v2.** Their
  incomes are drawn under the PUF label and still match the PUF. Their net
  worth, drawn under the SCF label, is as high as before.

## Update, 14 October 2026, late night (2): which part of the net worth draw goes wrong

**Bottom line.**

- **The label is what matters, not the other_agi slot.** Drawing net worth
  under the SCF label lowers total wealth from $165 trillion to about $130
  trillion, whether or not other_agi is visible. Keeping the PUF label and
  only hiding other_agi changes nothing.
- **The problem is already in the model's band draws, before any dollars.**
  PUF-style records land in the top net worth bands about twice as often as
  SCF records.
- **Likely cause:** income bands mean different dollars in the two sources.
  Under the SCF label, the model reads a PUF filer's small interest or
  dividends as an SCF investor's. The coordinator has since confirmed this
  from `band_edges.json`: at the 80th percentile of holders, the same
  interest band is $360 in the PUF and $1,500 in the SCF.

**What ran.** `03_owner_label.py`, Slurm job 29597097. The incomes, married
and n_dep of PUF-style records are drawn as before, under the PUF label. Only
the model's view of the record for the net worth draw changes:

| variant | label for the net worth draw | other_agi in that draw |
|---|---|---|
| A | SCF | visible |
| B | SCF | left out, so the record has exactly an SCF record's slots |
| C (diagnostic) | PUF | hidden behind the mask token |

- Each variant has 200,000 records, seed 3. All three ran in one job, so
  their incomes are identical record for record.
- **Files:**
  - Variant A: `gen/gen_puf_with_net_worth_ownerlabel.parquet`
  - Variants B and C: `wealth/owner_label/`, with the full tables in
    `tables.md`.
- This run replaces the earlier owner-label numbers above. Those came from a
  different draw.

**Table 1. Net worth, all units.**

| | actual SCF | SCF-style | current PUF-style | A | B | C |
|---|---:|---:|---:|---:|---:|---:|
| total ($ trillion) | 85.8 | 78.9 | 165 | 131 | 129 | 172 |
| mean ($) | 614,000 | 565,000 | 1,040,000 | 830,000 | 817,000 | 1,090,000 |
| median ($) | 74,700 | 61,700 | 104,000 | 131,000 | 96,800 | 127,000 |
| share negative | 11.0% | 12.0% | 11.7% | 10.0% | 11.1% | 12.1% |
| top 10% share | 78.3% | 79.6% | 80.4% | 74.7% | 76.9% | 79.8% |
| top 1% share | 39.7% | 43.2% | 44.3% | 36.3% | 36.0% | 42.8% |
| top 0.1% share | 15.3% | 16.4% | 17.3% | 14.1% | 13.3% | 16.5% |
| 90th percentile ($) | 1,040,000 | 914,000 | 1,430,000 | 1,390,000 | 1,400,000 | 1,530,000 |
| 99th percentile ($) | 9,660,000 | 8,920,000 | 18,300,000 | 11,500,000 | 11,500,000 | 18,600,000 |
| 99.9th percentile ($) | 41,500,000 | 42,900,000 | 72,700,000 | 45,900,000 | 45,900,000 | 73,300,000 |

The SCF files have 140 million units; the PUF-style files have 158 million.

**Mean net worth by wage group ($).**

| wages | actual SCF | SCF-style | current PUF-style | A | B | C |
|---|---:|---:|---:|---:|---:|---:|
| 0 | 663,000 | 679,000 | 2,380,000 | 1,760,000 | 1,720,000 | 2,540,000 |
| 1-25k | 173,000 | 136,000 | 411,000 | 304,000 | 266,000 | 419,000 |
| 25-50k | 181,000 | 153,000 | 443,000 | 311,000 | 305,000 | 486,000 |
| 50-100k | 353,000 | 341,000 | 788,000 | 647,000 | 626,000 | 791,000 |
| 100-200k | 934,000 | 819,000 | 1,550,000 | 1,350,000 | 1,350,000 | 1,580,000 |
| 200k+ | 5,120,000 | 4,640,000 | 5,560,000 | 5,450,000 | 5,870,000 | 5,700,000 |

**Check 1: the excess is already there before any dollars are drawn.** The
table shows the share of records in each net worth band group (the SCF's
positive ranks).

| net worth group | actual SCF | SCF-style | current PUF-style | A | B | C |
|---|---:|---:|---:|---:|---:|---:|
| zero or negative | 13.4% | 14.8% | 12.4% | 10.5% | 12.3% | 12.8% |
| P0–20 | 17.3% | 19.3% | 18.8% | 13.9% | 15.5% | 15.1% |
| P20–40 | 17.3% | 16.6% | 12.4% | 15.3% | 16.0% | 11.9% |
| P40–60 | 17.3% | 16.7% | 15.3% | 16.1% | 15.9% | 17.3% |
| P60–80 | 17.3% | 16.7% | 17.1% | 20.0% | 17.7% | 18.1% |
| P80–90 | 8.7% | 8.4% | 11.2% | 12.4% | 10.9% | 11.2% |
| P90–99 | 7.8% | 6.6% | 11.0% | 10.5% | 10.4% | 11.6% |
| P99–100 | 0.9% | 0.8% | 1.9% | 1.2% | 1.2% | 2.0% |

- **The current PUF-style file is too rich already in band space.** It has
  twice the SCF's share in the top 1% group, and 11% against 8% in the
  P90–99 group. So the dollar maps are not the cause; the model's band draws
  are.
- **The SCF label (A, B) brings the top 1% group down to 1.2%.** The P80–99
  groups stay well above the SCF's.

**Check 2: zero-wage PUF-style records, mean net worth by other_agi.**

| file | other_agi zero: share | mean | other_agi nonzero: share | mean |
|---|---:|---:|---:|---:|
| current PUF-style | 4.0% | 612,000 | 96.0% | 2,450,000 |
| A | 4.0% | 647,000 | 96.0% | 1,800,000 |
| B | 4.0% | 992,000 | 96.0% | 1,750,000 |
| C | 4.0% | 1,410,000 | 96.0% | 2,590,000 |

- 96% of zero-wage PUF-style records have some other_agi.
- Their mean net worth is 2.6 to 3.7 times the SCF's zero-wage mean in every
  variant, with or without other_agi in view.
- So the other_agi slot is not what drives the excess.

**Did the incomes move?** No. Every income share is identical across A, B
and C, and equal to the current file's: any interest 27.4%, any dividends
17.7%, any other_agi 65.6%. Married moves a little, because it is drawn
after net worth: 36.0% current, 36.4% A, 36.2% B, 37.2% C.

**Status.** A rebanding was proposed so that shared variables use the PUF's
dollar cut points in both sources, followed by a retrain and the v4 stack.
That is on hold; John has not approved it. Nothing has been retrained.
`model/` is still the v2 model.

## Update, 14 October 2026, late night: net worth drawn under the SCF label

**Bottom line.**

- **Drawing net worth under the SCF label helps, but by about a third, not
  enough.** Total net worth on the PUF-style file falls from $165 trillion to
  $138 trillion. The SCF has $86 trillion.
- **The top of the distribution is now close to the SCF's.** The 99th
  percentile falls from $18.3 million to $11.5 million, against the SCF's
  $9.7 million. The top 1% now holds 39% of net worth, as in the SCF.
- **The middle stays too high.** Mean net worth by wage, filing and
  dividend group is still 1.1 to 2.8 times the SCF's. The gap is close to
  nothing for earners above $200k and largest for zero-wage records.
- **The incomes did not move.**
- I did not rebuild the stack (v4), because the fix does not work well
  enough to be the baseline.

**What ran.**

- **Code.** `03_generate.py` has a new option, `owner_label`. A borrowed
  variable is drawn under the label of the survey that owns it.
  - For a PUF-style record, the incomes, married and n_dep are drawn under
    the PUF token as before.
  - Only the net worth draw sets the source token to SCF. The rest of the
    record is unchanged.
- **Job.** `03_owner_label.py`, run by `03_owner_label.sbatch` (Slurm job
  29596608).
  - It wrote `gen/gen_puf_with_net_worth_ownerlabel.parquet`: 200,000
    records, seed 3, described in `gen_meta_ownerlabel.json`.
  - It then rebuilt Tables 1 and 2a of `06_WEALTH_TABLES.md` with the
    functions of `06_wealth_tables.py`. Dollars come from the step 4 maps,
    and PUF-style files get equal weights summing to 158 million.
- **An extra check I added.** The owner-label record still has an other_agi
  slot, and no SCF record ever had one. So I also drew net worth with
  other_agi left out, giving a record of exactly the SCF's shape. That is the
  last column below. It barely differs, so the leftover other_agi slot is
  not the problem.
- **Files.** The full tables are in
  `/nfs/roberts/scratch/pi_nrs36/jar335/minimal/wealth/owner_label/tables.md`.

**Table 1. Net worth, all units.**

| | actual SCF | pretrain SCF-style | pretrain PUF-style (current) | PUF-style, owner label | owner label, no other_agi |
|---|---:|---:|---:|---:|---:|
| units (millions) | 140 | 140 | 158 | 158 | 158 |
| total ($ trillion) | 85.8 | 78.9 | 165 | 138 | 136 |
| mean ($) | 614,000 | 565,000 | 1,040,000 | 873,000 | 860,000 |
| median ($) | 74,700 | 61,700 | 104,000 | 131,000 | 97,500 |
| share negative | 11.0% | 12.0% | 11.7% | 9.95% | 11.1% |
| top 10% share | 78.3% | 79.6% | 80.4% | 75.9% | 77.9% |
| top 1% share | 39.7% | 43.2% | 44.3% | 39.1% | 39.1% |
| top 0.1% share | 15.3% | 16.4% | 17.3% | 16.5% | 16.5% |
| 90th percentile ($) | 1,040,000 | 914,000 | 1,430,000 | 1,390,000 | 1,400,000 |
| 99th percentile ($) | 9,660,000 | 8,920,000 | 18,300,000 | 11,500,000 | 11,500,000 |
| 99.9th percentile ($) | 41,500,000 | 42,900,000 | 72,700,000 | 52,900,000 | 49,200,000 |

**Table 2a. Mean net worth by group ($).**

| wages | actual SCF | pretrain SCF-style | PUF-style (current) | PUF-style, owner label | owner label, no other_agi |
|---|---:|---:|---:|---:|---:|
| 0 | 663,000 | 679,000 | 2,380,000 | 1,860,000 | 1,820,000 |
| 1-25k | 173,000 | 136,000 | 411,000 | 329,000 | 285,000 |
| 25-50k | 181,000 | 153,000 | 443,000 | 349,000 | 332,000 |
| 50-100k | 353,000 | 341,000 | 788,000 | 647,000 | 635,000 |
| 100-200k | 934,000 | 819,000 | 1,550,000 | 1,410,000 | 1,450,000 |
| 200k+ | 5,120,000 | 4,640,000 | 5,560,000 | 5,440,000 | 5,840,000 |

| filing status | actual SCF | pretrain SCF-style | PUF-style (current) | PUF-style, owner label | owner label, no other_agi |
|---|---:|---:|---:|---:|---:|
| joint | 1,120,000 | 1,040,000 | 2,040,000 | 1,670,000 | 1,670,000 |
| not joint | 262,000 | 243,000 | 483,000 | 411,000 | 398,000 |

| dividends | actual SCF | pretrain SCF-style | PUF-style (current) | PUF-style, owner label | owner label, no other_agi |
|---|---:|---:|---:|---:|---:|
| 0 | 250,000 | 265,000 | 464,000 | 396,000 | 355,000 |
| 1-1k | 829,000 | 863,000 | 1,580,000 | 1,200,000 | 1,220,000 |
| 1k-10k | 2,340,000 | 2,070,000 | 4,380,000 | 3,170,000 | 3,290,000 |
| 10k+ | 9,290,000 | 8,890,000 | 10,800,000 | 10,500,000 | 11,000,000 |

**The incomes did not move.** Weighted shares of records:

| | plain | owner label |
|---|---:|---:|
| married | 36.0% | 36.6% |
| any interest | 27.4% | 27.4% |
| any dividends | 17.7% | 17.8% |
| any other_agi | 65.6% | 65.5% |
| any capital gain or loss | 12.9% | 12.8% |

- **Married moves by 0.6 points.** It is drawn after net worth, so a
  different net worth can change it.
- **The two files are not record-for-record pairs.** I used the plain file's
  seed hoping the incomes would come out identical record for record. They
  did not: only 0.3% of records match. That is probably because the job ran
  on a different GPU model, which can change the random draws. So the
  comparison above is between two independent samples of 200,000 records.

**Why the fix only goes a third of the way: my best guess, not tested.**

- The income slots are bands, and a band is a rank within its own source.
- On the PUF, 27% of records have interest; in the SCF, 12% of units do.
  "Has interest" therefore means something different in each source.
  - On a PUF record it is common.
  - In the SCF it marks a wealthier household.
- Under the SCF label, the model reads a PUF record's income bands as SCF
  bands. So it takes a PUF filer with a little interest or dividends for one
  of the SCF's investment-income households, and gives them that household's
  wealth.
- **The table fits this.** The gap is largest where the two sources differ
  most on who has investment income: zero wages, and low dividends. It is
  smallest for the highest earners.
- **If this is right,** the label alone cannot fix it. The incomes would
  have to mean the same thing in both sources first: the "translate before
  training" step of the plan (D23). For example, both sources could use
  the same dollar cut points for interest and dividends.

**Not done.** I did not build the v4 stack and did not write `GEN_DONE_v4`,
because the result is still well off the SCF. The owner-label option is in
`03_generate.py` if John wants the v4 stack built anyway.

## Update, 14 October 2026, night: the oversampled stack (v3)

**Bottom line.**

- **`gen/gen_puf_stack_v3.parquet` is built and passes the check.** It is
  the baseline file for the later steps. Its weighted distribution of every
  variable agrees with the plain file within noise. It has six to eight times
  the plain file's records in the top bands.
- **The first version failed.** It drew net worth first in the net worth
  file, and that pulled the whole record toward the SCF.
- **What that shows.** Drawing a variable that only the other survey has,
  first, pulls the whole record toward that survey. That is why block order
  puts a record's own survey's variables first.

**What the file is.** Four files of 100,000 PUF-labeled records each, stacked.
Every record has the six PUF variables plus net worth. Each file oversamples
the top of one variable: in the 19 bands covering the top 1% of positive
weight, records are drawn ten times as often, and each record's weight
p(band)/q(band) undoes this exactly.

- **Wages, capital gains, other_agi files.** These use the method of the
  next section: the variable's band is drawn first. Then come the other
  incomes in random order, then net worth, married and n_dep.
- **Net worth file.** It keeps the normal block order: incomes first, in
  random order. Net worth is then drawn from a boosted version of the model's
  conditional distribution for that record, given the incomes just drawn.
  The weight uses that same conditional probability, so it is still exact.
  Then married and n_dep. Its weights range from 0.10 to 8.2.
- **Weights.** Each file's weights are scaled to sum to 100,000, so the
  400,000 records sum to 400,000.
- **Columns.** As in `gen_puf_with_net_worth.parquet`, plus `p_band`,
  `q_band` (for the net worth file, the record's conditional values) and
  `stratifier`. `gen_meta_v3.json` describes the file. The code is
  `03_stack.py` and `03_stack.sbatch`.
- **Jobs.** The first version was Slurm job 29594592. The net worth file was
  then rebuilt alone in job 29595186. The other three files were kept as
  they were. The first version is in `gen/v3_first_try/`.

**Weighted shares by file.** "Plain" is `gen_puf_with_net_worth.parquet`:
200,000 records, in normal order, with no oversampling.

| | plain | wages file | capital gains file | other_agi file | net worth file, now | net worth file, first version |
|---|---|---|---|---|---|---|
| married | 36.0% | 36.5% | 36.0% | 36.1% | 36.1% | 41.8% |
| any interest | 27.4% | 27.5% | 27.2% | 27.1% | 27.4% | 35.0% |
| any dividends | 17.7% | 17.8% | 17.6% | 17.6% | 17.5% | 24.6% |
| any other_agi | 65.6% | 65.7% | 65.6% | 65.6% | 65.7% | 75.0% |
| any capital gain or loss | 12.9% | 12.8% | 13.0% | 12.7% | 12.7% | 17.4% |

**The whole stack against plain.** Total variation distance between the two
band distributions (0 = the same), with the distance between two plain draws
with different seeds as the yardstick for chance:

| variable | stack vs plain | plain vs plain, new seed |
|---|---|---|
| married | 0.002 | 0.001 |
| n_dep | 0.001 | 0.002 |
| wages | 0.009 | 0.007 |
| interest | 0.005 | 0.004 |
| dividends | 0.004 | 0.004 |
| capital gains | 0.003 | 0.004 |
| other_agi | 0.008 | 0.008 |
| net worth | 0.011 | 0.011 |

**Records in the top bands, plain (200,000 records) against the stack
(400,000).** The weighted share is the share of all records.

| variable | bands | plain records | stack records | of which from its own file | plain weighted share | stack weighted share |
|---|---|---|---|---|---|---|
| wages | top 1% | 1,492 | 9,858 | 7,110 | 0.746% | 0.737% |
| wages | top 0.1% | 170 | 953 | 576 | 0.085% | 0.078% |
| capital gains | top 1% | 151 | 943 | 501 | 0.076% | 0.075% |
| capital gains | top 0.1% | 18 | 79 | 35 | 0.009% | 0.006% |
| other_agi | top 1% | 856 | 5,910 | 4,000 | 0.428% | 0.445% |
| other_agi | top 0.1% | 87 | 529 | 292 | 0.044% | 0.044% |
| net worth | top 1% | 3,786 | 17,646 | 9,052 | 1.89% | 1.90% |
| net worth | top 0.1% | 516 | 2,702 | 1,344 | 0.26% | 0.29% |

- **The weighted top shares now match the plain file.** The plain file's top
  0.1% counts are 18 to 516 records, so the differences there (up to a
  third, for capital gains, where plain has 18 records) are within noise.
- **The capital gains file is still thin at its own top.** It contributes
  501 top-1% capital gains records, not the roughly 1,500 a tenfold boost
  would give. Its weighted share of the capital gains top 1% is 0.050%,
  against 0.07% to 0.10% in the other three files. As the next section
  found, the model's capital gains distribution with nothing else known is
  short at the very top.
- **Where the shortfall goes.** Drawing capital gains first leans on that
  nothing-known distribution. Drawing it after the other incomes does not.
  In the stack the other three files make up for it, so the stack's overall
  share matches plain.

**Why the first version failed.**

- In the first version's net worth file, net worth was drawn first, from the
  model's distribution for a PUF record with nothing else known. The incomes
  were then drawn given that net worth.
- Net worth is an SCF variable. The model never saw it on a PUF record.
- Drawing it first pulled the whole record toward the SCF's pattern:
  - 42% married against 36%;
  - 35% with interest against 27%;
  - 75% with other_agi against 66%.
- The stack's top shares were inflated with it. The wage top 1% was 0.90%
  against 0.75%.
- Drawing net worth after the incomes, from its conditional distribution,
  removes the problem entirely: the net worth file now matches plain within
  noise.
- **Lesson.** Block order puts a record's own survey's variables first.
  Drawing an other-survey variable first pulls the record toward that
  survey.

## Update, 14 October 2026, late evening: oversampling the top, with exact weights

**What ran.** `03_oversample.py`, through `03_oversample.sbatch` (Slurm job
29590921), using the second-run model. It produces PUF-labeled files in which
the top of one amount is drawn ten times as often as usual. Each record
carries the exact weight that undoes this.

How a record is drawn, taking capital gains as the stratifying variable:

1. Take p(band): the model's distribution of capital gains for a PUF record
   with nothing else known.
2. Build q(band) from p. The bands covering the top 1% of positive capital
   gains (the 0.1% bands and the 0.01% bands, 19 in all) get ten times
   their probability. Then q is rescaled to sum to one.
3. Draw the capital gains band from q.
4. Draw everything else given that band in the usual order: the other
   incomes in random order, then married, then n_dep.
5. The record's weight is p(band) / q(band). Both probabilities are stored
   in the file (columns `p_band`, `q_band`) so the weight can be checked.

The second file does the same with wages as the stratifying variable. Each
file has 200,000 records, and there is a second seed of each. The files are
in `gen/`:

- `gen_puf_oversampled_kg.parquet` and `gen_puf_oversampled_kg_seed2.parquet`
- `gen_puf_oversampled_wages.parquet` and
  `gen_puf_oversampled_wages_seed2.parquet`

The full tables are in `gen/03_oversample_tables.md`.

**Bottom line.**

- **The oversampling works as designed.** There are about seven to ten times
  as many records in the top bands. The top-1% shares vary three to four
  times less from seed to seed.
- **The weights sum to 200,000 within about 0.02%,** and the weighted
  distributions agree with the plain file. The one difference is in the
  stratifying variable's own top.
- **That difference shows the model's nothing-known distribution is too thin
  at the very top of capital gains.** It puts 0.055% of PUF records in the
  top 1% of positive gains; the PUF itself has 0.077%. The plain file, where
  capital gains is usually drawn after other incomes, gets 0.074%.

**Records in the top bands**, plain file against stratified file (seed 1 /
seed 2):

| | plain | stratified |
|---|---|---|
| capital gains, top 1% of positives (19 bands) | 133 / 162 | 1,042 / 1,099 |
| capital gains, top 0.1% (10 bands) | 9 / 13 | 81 / 63 |
| wages, top 1% of positives | 1,548 / 1,506 | 14,749 / 14,727 |
| wages, top 0.1% | 157 / 141 | 1,135 / 1,161 |

- The capital gains gain is closer to eight times than ten. That is because
  ten times p's top mass is less than ten times what the plain file draws
  (see the bottom line).
- In the plain file, several of the capital gains 0.01% bands have zero or
  one record. In the stratified files each has 2 to 13.

**Seed-to-seed variation of top shares** (weighted share of all records, in
units of 0.01%):

| measure | file | seed 1 | seed 2 | difference | standard error, one file |
|---|---|---|---|---|---|
| capital gains, top 1% | plain | 6.65 | 8.10 | 1.45 | 0.58 |
| capital gains, top 1% | capital-gains stratified | 5.23 | 5.52 | 0.29 | 0.16 |
| capital gains, top 0.1% | plain | 0.45 | 0.65 | 0.20 | 0.15 |
| capital gains, top 0.1% | capital-gains stratified | 0.41 | 0.32 | −0.09 | 0.05 |
| wages, top 1% | plain | 77.4 | 75.3 | −2.1 | 2.0 |
| wages, top 1% | wages stratified | 79.0 | 78.9 | −0.1 | 0.7 |
| wages, top 0.1% | plain | 7.85 | 7.05 | −0.80 | 0.63 |
| wages, top 0.1% | wages stratified | 6.08 | 6.22 | 0.14 | 0.18 |

The standard error is computed from one file with the usual formula for a
weighted share.

- **Stratifying cuts the standard error of the stratified variable's top
  shares by a factor of three to four.** For the same precision, a plain
  file would need roughly ten times as many records.
- **Stratifying on one variable does not help the other.** The
  capital-gains-stratified file's wage shares are as noisy as the plain
  file's.

**Where plain and stratified disagree, and why.**

- **The weighted band distributions match the plain file closely.**
  - Total variation distance: 0.001 to 0.010 for the non-stratifying
    variables, against 0.000 to 0.009 between two plain seeds.
  - For the stratifying variable: 0.005 for capital gains and 0.015 for
    wages.
- **Weighted, the stratified file estimates the model's nothing-known
  distribution of the stratifying variable,** because that variable is
  always drawn first. The plain file draws it in a random position, mostly
  after other incomes. Where the two differ, the model's answers given
  different known slots do not quite agree. The same disagreement drove the
  order effects in the first run.
- **The top of capital gains shows the clearest disagreement:**

  | | top 1% of positive capital gains | top 0.1% |
  |---|---|---|
  | PUF, all records | 0.077% | 0.0077% |
  | plain file, average of two seeds | 0.074% | 0.0055% |
  | model with nothing known (exact, from p) | 0.055% | — |
  | stratified file, average of two seeds | 0.054% | 0.0037% |

  So the nothing-known distribution is about 30% short at the top of gains.
- **Wages show a milder version.** The nothing-known distribution has 0.79%
  in the top 1%, against 0.83% in the PUF. The top 0.1% is 0.061% against
  0.083%.
- **What this means for the design.** If generation starts by drawing a
  stratifying variable from the model's nothing-known distribution, that
  distribution's own top needs checking first. The plan's income-group
  token, drawn first from IRS counts, would sidestep this.

## Update, 14 October 2026, evening: second run (v2)

All files in `gen/` were regenerated from the retrained model. They have the
same names, schema and seeds. The model changes are described at the top of
`02_REPORT.md`: ten negative bands instead of four, and a learning rate that
now decays fully. The first run's files are kept in `gen/v1/`. Everything in
this report below this section describes the first run. The new tables are
in `gen/03_tables.md`. The marker `gen/GEN_DONE_v2` is written.

**The interest shortfall is gone.** Share of PUF records with any amount:

| | training | held-out | first run | second run |
|---|---|---|---|---|
| wages | 82.9% | 82.3% | 82.2% | 82.8% |
| interest | 28.4% | 28.2% | 26.7% | 28.1% |
| dividends | 18.3% | 19.1% | 18.3% | 18.1% |
| capital_gains | 13.3% | 13.4% | 13.6% | 13.1% |
| other_agi | 66.4% | 67.3% | 65.7% | 66.1% |

**The capital-gains miss is smaller.** Share of generated PUF records with
any capital gain or loss, by dividend group:

| dividend group | held-out | first run | second run |
|---|---|---|---|
| P40–60 | 0.53 | 0.57 | 0.55 |
| P60–80 | 0.64 | 0.73 | 0.70 |
| P80–90 | 0.79 | 0.84 | 0.82 |

**Draw order now matters about as much as chance.** Second run, PUF file (i):

| variable | measure | random | random, new seed | fixed |
|---|---|---|---|---|
| wages | any amount | 0.8283 | 0.8273 | 0.8266 |
| wages | top 1% | 0.0077 | 0.0075 | 0.0079 |
| interest | any amount | 0.2811 | 0.2809 | 0.2822 |
| dividends | any amount | 0.1812 | 0.1826 | 0.1802 |
| capital_gains | any amount | 0.1311 | 0.1312 | 0.1311 |
| other_agi | any amount | 0.6607 | 0.6610 | 0.6563 |
| other_agi | top 1% | 0.0044 | 0.0047 | 0.0048 |

- **The fixed order now moves the any-amount shares by at most 0.4 points**
  (other_agi). In the first run it moved them by up to 1.8 points. The two
  random-order runs differ by up to 0.14 points.
- **The wage top 1% now differs by about as much between the two seeds as
  between the two orders.** In the first run, the fixed order shrank it by a
  seventh.

**What got worse.**

- **The wage top 1% is too small:** 0.77% of generated records, against
  0.87% held out. The first run gave 0.86%.
- **The SCF's net worth top is thinner:**
  - 6.6% of generated SCF records are in the P90–99 net worth group, against
    8.6% held out (first run: 8.2%).
  - Among the top 10% of earners, 34% are in that group, against 52% in the
    data. The first run gave 40%. That 52% rests on 72 households.

**Net worth on PUF-labeled records (iii)** looks much as before. 1.9% are in
the top 1% of positive net worth, against 0.9% in the SCF. 28% of zero-wage
records are in the top net worth groups.

## The first run (v1)

## Bottom line

- **PUF records look right one variable at a time.** For each variable, the
  generated records differ from the held-out records by about as much as the
  real training and test splits differ from each other.
- **Two misses.**
  - Too few records with interest: 26.7% have any, against 28.4% in training
    and 28.2% held out. other_agi is low too, by about a point. This one is
    new in generation.
  - Too many capital gains among records with mid-sized dividends. This one
    carries over from step 2.
- **The SCF records have the right broad shape.** The model again flattens
  the top of wealth for the highest earners. The SCF test split is small
  (621 households), so most SCF differences cannot be told apart from noise.
- **The draw order matters a little, and more than chance.** Drawing
  incomes in a fixed order instead of a random one moves the share with any
  other_agi down by 1.8 points and the wage top 1% down by about a seventh.
  A second random-order run with a new seed moves these by 0.3 points or
  less.
- **PUF-labeled records with net worth, which the model never trained on,
  come out plausible but richer than the SCF.** Twice the SCF's share is in
  the top 1% of positive net worth.

## What ran

Slurm job 29587350 ran `03_generate.py` and `03_compare.py` through
`03_generate.sbatch`, using the step 2 model. Generation took under a minute
on one GPU. The comparison tables were then rerun on a CPU node (job
29588082) to print four decimals. The generated files were not touched.

**How a record is generated.**

1. Start with source and year set and every predicted slot masked.
2. Fill one slot at a time. Draw it from the model's probabilities given
   everything filled so far, at temperature 1.
3. The order follows the plan's blocks:
   - Block 1: the incomes, in a random order per record. PUF records have
     five: wages, interest, dividends, capital_gains and other_agi. SCF
     records have the first four.
   - Block 2: net worth, when the record is to have it.
   - Block 3: married, then n_dep.

**Files.** In `/nfs/roberts/scratch/pi_nrs36/jar335/minimal/gen/`. Each record
has weight 1. The columns are those of the stacked table without dollars.
Band = −1 means the slot is absent. `gen_meta.json` describes each file.

| file | records | what |
|---|---|---|
| `gen_puf.parquet` | 200,000 | (i) PUF-labeled: the six PUF variables, other_agi included |
| `gen_scf.parquet` | 40,000 | (ii) SCF-labeled: the six SCF variables, net worth included |
| `gen_puf_with_net_worth.parquet` | 200,000 | (iii) PUF-labeled, with net worth drawn in block 2 |
| `gen_puf_fixed_order.parquet` | 200,000 | (i) again, block 1 in the fixed order wages, interest, dividends, capital_gains, other_agi |
| `gen_puf_second_seed.parquet` | 200,000 | (i) again with a new seed, as a measure of sampling noise |

The full tables are in `03_tables.md` and `03_compare.json` in the same
folder.

## 1. One variable at a time

The shares below are shares of all records:

- "zero" and "negative" count records with no amount or a negative amount.
- The last three columns count records in the 90–99th, 99–99.9th and top
  0.1% bands of the positives.

"Distance" is the total variation distance between the two 78-band
distributions: half the sum of the absolute differences in band shares. It is
0 when the distributions are the same and 1 when they share nothing. The last
column gives the same distance between the real training and test splits, as
a yardstick for chance.

**PUF: held-out against generated (i)**

| variable | zero | negative | P90–99 | P99–99.9 | top 0.1% | distance | train vs test |
|---|---|---|---|---|---|---|---|
| wages | 0.177 / 0.178 | 0 / 0 | 0.074 / 0.069 | 0.0078 / 0.0076 | 0.0008 / 0.0010 | 0.036 | 0.030 |
| interest | 0.718 / 0.733 | 0 / 0 | 0.027 / 0.025 | 0.0025 / 0.0027 | 0.0003 / 0.0003 | 0.022 | 0.013 |
| dividends | 0.809 / 0.818 | 0 / 0 | 0.018 / 0.018 | 0.0019 / 0.0019 | 0.0002 / 0.0002 | 0.018 | 0.016 |
| capital_gains | 0.866 / 0.864 | 0.057 / 0.057 | 0.007 / 0.007 | 0.0007 / 0.0008 | 0.0001 / 0.0001 | 0.009 | 0.009 |
| other_agi | 0.327 / 0.343 | 0.182 / 0.172 | 0.043 / 0.045 | 0.0042 / 0.0044 | 0.0005 / 0.0004 | 0.035 | 0.026 |

Married: 35.9% held out, 35.5% generated. Dependents 0 / 1 / 2 / 3+: 65.4 /
16.0 / 12.3 / 6.2% held out, 65.1 / 16.0 / 12.1 / 6.8% generated.

**Who has any amount.** The held-out split is itself off from the training
split by up to 0.8 points, so the training share is shown too.

| | training | held-out | generated (i) |
|---|---|---|---|
| wages | 82.9% | 82.3% | 82.2% |
| interest | 28.4% | 28.2% | 26.7% |
| dividends | 18.3% | 19.1% | 18.3% |
| capital_gains | 13.3% | 13.4% | 13.6% |
| other_agi | 66.4% | 67.3% | 65.7% |

Interest is short by about 1.6 points. other_agi and wages are short by
about 0.7 points against training. In step 2, the model with every slot
hidden scored interest the same as the training distribution (1.567 nats).
So the shortfall most likely comes in when interest is drawn after other
incomes. The model's predictions given
different subsets of slots do not fully agree with one another, and the
order of drawing exposes that.

**SCF: held-out against generated (ii)**

| variable | zero | negative | P90–99 | P99–100 | distance | train vs test |
|---|---|---|---|---|---|---|
| wages | 0.314 / 0.293 | 0 / 0 | 0.065 / 0.061 | 0.0075 / 0.0081 | 0.133 | 0.125 |
| interest | 0.884 / 0.885 | 0 / 0 | 0.003 / 0.012 | 0.0004 / 0.0017 | 0.036 | 0.037 |
| dividends | 0.876 / 0.887 | 0 / 0 | 0.009 / 0.011 | 0.0007 / 0.0013 | 0.046 | 0.043 |
| capital_gains | 0.914 / 0.919 | 0.018 / 0.016 | 0.006 / 0.005 | 0.0013 / 0.0009 | 0.038 | 0.037 |
| net_worth | 0.018 / 0.023 | 0.103 / 0.123 | 0.086 / 0.082 | 0.0090 / 0.0094 | 0.149 | 0.131 |

For every SCF variable, the distance from generated to held-out is about the
same as the distance from training to held-out. The SCF test sample cannot
tell the model apart from the training data.

Married: 42.5% held out, 41.3% generated. One dependent: 13.0% held out,
15.2% generated.

## 2. Joint tables, held-out against generated

**PUF: share with any dividends, by wage group and married** (selected rows;
all rows in `03_tables.md`)

| wage group | married | held-out | generated |
|---|---|---|---|
| zero | no | 0.33 | 0.30 |
| zero | yes | 0.44 | 0.43 |
| P20–40 | yes | 0.16 | 0.17 |
| P60–80 | yes | 0.22 | 0.18 |
| P90–99 | yes | 0.42 | 0.45 |
| P99–100 | no | 0.68 | 0.77 |
| P99–100 | yes | 0.78 | 0.80 |

Every other row is within 2.5 points.

**PUF: share with any capital gain or loss, by dividend group**

| dividend group | held-out | generated |
|---|---|---|
| zero | 0.035 | 0.036 |
| P0–20 | 0.32 | 0.32 |
| P20–40 | 0.44 | 0.42 |
| P40–60 | 0.53 | 0.57 |
| P60–80 | 0.64 | 0.73 |
| P80–90 | 0.79 | 0.84 |
| P90–99 | 0.87 | 0.87 |
| P99–100 | 0.89 | 0.89 |

This is the same miss as in step 2: too many gains in the P40–90 dividend
groups.

**Dependents by married.**

- PUF: within 2 points in every cell.
- SCF: married couples get 2 dependents 14.5% of the time, against 19.5% in
  the data, and 1 dependent 15.0% against 11.9%.

**SCF: net worth by wage group.** The model gets the pattern but flattens the
top. Of the highest earners (P99–100 of wages), 47% of generated records are
in the P90–99 net worth group and 36% in the top 1%. In the data the figures
are 68% and 32%, but they rest on 31 households. Wage group P90–99 shows the
same flattening: 40% against 52%, from 72 households.

## 3. Draw order

Three runs of (i) side by side:

- random order inside block 1 (the main file);
- random order with a different seed, to show sampling noise;
- the fixed order wages, interest, dividends, capital_gains, other_agi.

| variable | measure | random | random, new seed | fixed | held-out |
|---|---|---|---|---|---|
| wages | any amount | 0.8221 | 0.8214 | 0.8285 | 0.8227 |
| wages | top 1% | 0.0086 | 0.0088 | 0.0074 | 0.0087 |
| interest | any amount | 0.2672 | 0.2678 | 0.2610 | 0.2816 |
| dividends | any amount | 0.1825 | 0.1822 | 0.1772 | 0.1906 |
| dividends | top 1% | 0.0021 | 0.0019 | 0.0017 | 0.0021 |
| capital_gains | any amount | 0.1357 | 0.1351 | 0.1326 | 0.1343 |
| other_agi | any amount | 0.6566 | 0.6541 | 0.6386 | 0.6728 |
| other_agi | top 1% | 0.0049 | 0.0047 | 0.0045 | 0.0046 |

"Top 1%" is the share of all records in the top 1% bands of the positives.

- **The order matters a little, and beyond chance.** The two random-order
  runs agree to within 0.3 points on every any-amount share. The fixed order
  differs from them by 0.3 to 1.8 points.
- **Under the fixed order, wages (drawn first) come out slightly higher and
  every other income slightly lower.** other_agi, drawn last, drops most.
- **The wage top 1% is 14% smaller under the fixed order** (0.0074 against
  0.0086).
- **Neither order is closer to the data on every measure.**

The top 0.1% shares are too small to compare at 200,000 records: 20 to 200
records each. They are in `03_tables.md`.

## 4. Net worth for PUF-labeled records

The model never saw a PUF record with net worth. File (iii) asks it for one
anyway. The table shows net worth groups among the positives, with negatives
counted together with zero.

| net worth group | SCF held-out | SCF generated (ii) | PUF-labeled generated (iii) |
|---|---|---|---|
| zero or negative | 0.121 | 0.146 | 0.121 |
| P0–20 | 0.170 | 0.195 | 0.181 |
| P20–40 | 0.186 | 0.171 | 0.162 |
| P40–60 | 0.175 | 0.156 | 0.149 |
| P60–80 | 0.172 | 0.152 | 0.164 |
| P80–90 | 0.081 | 0.088 | 0.097 |
| P90–99 | 0.086 | 0.082 | 0.107 |
| P99–100 | 0.009 | 0.009 | 0.019 |

Share in the top net worth groups (P90 of positives and above), by wage
group:

| wage group | SCF held-out | SCF generated | PUF-labeled generated |
|---|---|---|---|
| zero | 0.08 | 0.11 | 0.27 |
| P20–40 | 0.01 | 0.01 | 0.04 |
| P60–80 | 0.05 | 0.06 | 0.09 |
| P90–99 | 0.55 | 0.43 | 0.32 |
| P99–100 | 1.00 | 0.83 | 0.73 |

What this shows:

- **The PUF-labeled records are richer than the SCF's** in the top groups,
  and twice as likely to be in the top 1%.
- **Most of the difference is among records with no wages.** 27% of
  PUF-labeled zero-wage records are in the top net worth groups, against
  about 10% in the SCF. That direction is plausible:
  - Zero-wage PUF filers are often retirees with investment income. 30 to
    44% of them have dividends.
  - The SCF's zero-wage units include non-filers, of whom 6 to 30% have
    dividends.
  - I have not checked this against any outside source.
- **Among the highest earners, PUF-labeled records are less concentrated at
  the top than the SCF's.**

So the model does give an answer it was never trained on, and the answer
moves with the record's incomes. Whether it is the right answer is the
label-indifference question for later steps.

## What I am not sure of

- **Why interest is short.** I have not traced the 1.6-point shortfall to a
  specific conditional.
- **The capital-gains miss.** It has the same unknown cause as in step 2. The
  step 2 learning rate had decayed only to about two-thirds of its peak when
  training stopped. A retrain with a full decay is the first thing I would
  try.
- **Thin SCF cells.** Most SCF comparisons rest on a few hundred households.
- **Equal weights.** Generated records count equally. Held-out records count
  by survey weight. Shares are comparable. But the generated PUF file has
  far fewer records at the top than the real PUF, because the real PUF
  oversamples the top. There are 1,505 married top-1% earners per 200,000
  generated records, against 2,840 in the 20,758 held-out records. That is
  expected; the plan gives this job to the tilt in a later step.
