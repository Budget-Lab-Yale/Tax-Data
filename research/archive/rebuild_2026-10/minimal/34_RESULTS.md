---
title: "Step 34: business income in its parts on the PUF side — results"
role: evidence
workstream: rebuild
status: current
updated: 2026-10-19
sot: research/rebuild/design/plan.md
supersedes: []
superseded_by: null
---

# Step 34: business income in its parts on the PUF side

Status, job IDs and how to resume: `34_STATUS.md`. Report tables written by
the chain: `/nfs/roberts/scratch/pi_nrs36/jar335/minimal/step34/report.md`.
Table checks: `/nfs/roberts/scratch/pi_nrs36/jar335/minimal/step34/tables_checks.md`.

## 1. What changed from step 33

Step 33 had one PUF item, business income = E00900 + E02000 + E02100
(Schedule C, all of Schedule E, Schedule F). Step 34 replaces it on the PUF
side by eighteen PUF-only model variables. The SCF side is unchanged: the
same SCF item (X5704 + X5714), the same split to tax units, the same "rank"
dollar rule, taken as reported; the settings sheet `34_settings_scf.json` is
step 33's with one added block that only records the parts (the table job
checks that every SCF setting equals step 33's).

On a generated PUF record, business income is no longer drawn. It is
computed from the drawn parts, the same way the line-13 capital gains total
is computed from its four pieces: while the PUF variables are drawn, the
business income slot is present but hidden; once every part is drawn, each
part gets a dollar value within its band, the signed sum is placed in a
business income band on the PUF's 2015 cut points, and only then is net
worth drawn under the SCF label, seeing that computed total (and not the
parts, which the SCF never has). In the dollar step, business income is
the exact signed sum of the parts' dollars.

### The parts and how the PUF stores them

Ten of the fields are stored on the PUF as positive numbers even though they
are losses or deductions. They stay positive numbers in the model and enter
the sum with a minus sign. The check on the data: none of the ten has a value
below zero on any record. Four are stored with their sign.

| Model variable | PUF field | What | Stored as | Sign in the sum | Weighted share with it (%) | 2015 total ($ billion, as stored) |
|---|---|---|---|---|---|---|
| sch_c | E00900 | Schedule C net profit or loss | signed | + | 16.4 (3.9 below zero) | 331.5 |
| part_pass_inc | E25940 | partnership passive income | positive | + | 1.3 | 71.0 |
| part_nonpass_inc | E25980 | partnership nonpassive income | positive | + | 1.6 | 293.2 |
| part_pass_loss | E25920 | partnership passive loss | positive | − | 0.9 | 27.5 |
| part_nonpass_loss | E25960 | partnership nonpassive loss | positive | − | 1.1 | 90.0 |
| part_179 | E26110 | partnership section 179 deduction | positive | − | 0.3 | 8.5 |
| scorp_pass_inc | E26170 | S corporation passive income | positive | + | 0.4 | 37.2 |
| scorp_nonpass_inc | E26190 | S corporation nonpassive income | positive | + | 2.1 | 447.0 |
| scorp_pass_loss | E26160 | S corporation passive loss | positive | − | 0.1 | 4.7 |
| scorp_nonpass_loss | E26180 | S corporation nonpassive loss | positive | − | 0.9 | 58.8 |
| scorp_179 | E26100 | S corporation section 179 deduction | positive | − | 0.8 | 37.2 |
| rent_inc | E25850 | rent and royalty net income | positive | + | 5.0 | 111.0 |
| rent_loss | E25860 | rent and royalty net loss | positive | − | 3.8 | 60.0 |
| estate_inc | E26390 | estate and trust net income | positive | + | 0.4 | 32.1 |
| estate_loss | E26400 | estate and trust net loss | positive | − | 0.1 | 3.7 |
| farm_rent | E27200 | farm rental net income or loss (Form 4835) | signed | + | 0.3 (0.1 below zero) | 4.3 |
| sche_other | remainder | E02000 minus the fifteen Schedule E fields above | signed | + | 2.7 (1.4 below zero) | −0.1 |
| farm | E02100 | Schedule F net profit or loss | signed | + | 1.2 (0.8 below zero) | −15.4 |

Shares and totals are for PUF filers, weighted, 2015 dollars. The variable
guide (`config/variable_guide/baseline.csv`) gives the same fields for the
partnership, S corporation, rent and estate items, and the production
pipeline subtracts the same ten fields.

The Schedule E remainder is rounding. It is nonzero on 2.7% of filers, but
its amounts are tiny (median about $30 either way, 99th percentile of the
positive values $460), and the sum of its absolute values over all filers is
$0.31 billion against $705 billion of Schedule E. About half of it is the
difference between the ten partnership and S corporation fields and their
combined net on the PUF (E26270), and the rest is the difference between
E02000 and E26270 plus the rent, estate and farm rental fields; both look
like each field being rounded on its own for disclosure. It is kept as a
model variable so that the parts add exactly. The PUF carries no separate
field for REMIC income; if any is on the file it sits in this remainder.

### Checks (every PUF record, whole dollars)

- The eighteen parts with their signs add to E00900 + E02000 + E02100 on all
  207,692 PUF records: 0 records off.
- They equal table E33's business income on all 207,692 filer records: 0 off.
- AGI (E00100) = wages + interest + dividends + the four capital gains
  pieces + the other eight step-33 AGI items with their signs + the eighteen
  parts with their signs + the remainder (other_agi, unchanged from E33): 0
  records off.
- No missing value in any raw field used, and none in any part on the PUF
  side (filers and non-filers). SCF rows have the parts absent by design.

### The crude non-filers

They have two CPS business fields. Schedule C gets the CPS sole
proprietorship income (`sole_prop`, signed: 357 of the pool's records are
below zero). rent_inc gets the CPS rent (`rent`, stored at zero or above in
the pool) and rent_loss gets minus the CPS rent loss (`rent_loss`, stored at
zero or below in the pool, so it becomes a positive number as on the PUF).
Every other part is zero. These add to step 33's non-filer business income
exactly (largest gap $0.000000). Weighted: 5.36 million non-filer units with
Schedule C income ($125.5 billion), 0.61 million with rent income ($5.4
billion), 0.05 million with a rent loss ($0.2 billion), 2015 dollars.

### Dollars in 2019

Each part's dollars grow from 2015 to 2019 by the IRS (SOI Table 1.4) growth
in the amount per return with the item. Where the IRS row exists, positive
and negative values grow separately:

| Part | IRS row used | Factor, positive values | Factor, negative values |
|---|---|---|---|
| sch_c | business or profession net income / net loss | 1.058 | 1.210 |
| farm | farm net income / net loss | 0.940 | 1.256 |
| farm_rent | farm rental net income / net loss | 1.075 | 0.996 |
| rent_inc, rent_loss | total rental and royalty net income; net loss | 1.158 (income), 1.261 (loss) | |
| estate_inc, estate_loss | estate and trust net income; net loss | 1.117 (income), 1.358 (loss) | |
| the four partnership and four S corporation income fields | partnership and S corporation net income (the IRS publishes the two together and does not split passive from nonpassive) | 1.112 | |
| the four partnership and four S corporation loss fields | partnership and S corporation net loss | 1.383 | |
| part_179, scorp_179 | no IRS row: partnership and S corporation net income | 1.112 | |
| sche_other | no IRS row: step 33's business income factor | 1.033 | |

Full table: `/nfs/roberts/scratch/pi_nrs36/jar335/minimal/map/V34/growth_factors_34.csv`.

### Targets

The tilt uses step 33's 136 IRS targets and eight SCF wealth targets, on both
wealth count bases. No targets were added for the parts: the target files
built by `01_targets.py` hold wages, interest, dividends, capital gains,
capital gain distributions, tax-exempt interest and AGI, and nothing on
business income. (The SOI mirror does have Table 1.4 business rows by AGI
bracket, so such targets could be built; that is a choice for John.)

## 2. Where the PUF's business losses come from (real PUF side, 2015)

This table uses only the real data (table E34's PUF side: 2015 filers plus
the crude non-filers, with their weights), so it does not depend on any
model run. A unit has a net business loss when Schedules C, E and F together
are below zero: 6.27% of PUF-side units, with net losses of $209.9 billion.
On those units the parts that pull the total down add to $252.2 billion
("gross losses"); positive parts on the same units offset $42 billion of it.

| Part | Units with a net business loss on which this part is a loss (%) | Share of the gross loss dollars (%) | Positive amounts of this part on the same units, as % of gross losses |
|---|---|---|---|
| Schedule C (E00900 below zero) | 49.9 | 21.3 | 1.5 |
| partnership nonpassive loss (E25960) | 8.3 | 25.6 | |
| partnership passive loss (E25920) | 4.9 | 6.3 | |
| partnership section 179 (E26110) | 0.3 | 0.2 | |
| partnership income (E25940, E25980) | | | 6.9 |
| S corporation nonpassive loss (E26180) | 8.0 | 18.0 | |
| S corporation passive loss (E26160) | 0.6 | 1.1 | |
| S corporation section 179 (E26100) | 0.7 | 1.1 | |
| S corporation income (E26170, E26190) | | | 4.1 |
| rent and royalty net loss (E25860) | 32.7 | 16.3 | |
| rent and royalty net income (E25850) | | | 3.6 |
| estate and trust net loss (E26400) | 0.3 | 0.9 | 0.4 (income) |
| farm rental (E27200 below zero) | 0.5 | 0.2 | 0.1 |
| Schedule E remainder (rounding) | 4.5 | 0.0 | 0.0 |
| Schedule F (E02100 below zero) | 8.1 | 9.0 | 0.2 |

In words: half of the units with a net business loss have a Schedule C
loss and a third have a rental loss, but in dollars the partnership and S
corporation losses dominate: 52% of the gross loss dollars come from the
partnership and S corporation fields (mostly nonpassive), 21% from Schedule
C, 16% from rent and royalty losses, and 9% from farms.

## 3. Model results (18 October)

Base network: Ns1–Ns3, seeds 20261016–18. Bigger network: Bs1–Bs2, with
6 layers, width 256, 8 heads and feed-forward 512 (4.04 million parameters
against 0.97 million), seeds 20261016–17. Tables:
`$R/step34/report.md` (base, copy at `report_base_runs.md`) and
`$R/step34/bigger/report.md`.

**Who holds each part.**
- Within about 10% of the PUF.
- Parts combine on one record about right: 1.6% of units hold three or
  more kinds, generated and PUF alike.

**Dollars run low.**
- The business income total computed from the parts averages 89% of the
  PUF's on the base network and 92% on the bigger one. Drawn as one item in
  step 33, it was 98%.
- Bigger network by run: Bs1 87%, Bs2 98%. Same seed against the base
  network: +3 to 4 points, within noise.

**Section 179.** It appears too often on units with a net business loss:
S corporations 3.2% against 0.7%; partnerships 1.4% against 0.3%.

**Reweighting cost.** About the same as step 33: effective records 94%
against 95%. Net worth before reweighting is $108.6 trillion against the
SCF's $96.1 trillion; this problem is unchanged, since the SCF side did not
change.

## 4. Why rare holders come out short (19 October)

Every number below is before reweighting. The PUF figures are the PUF side:
filers and crude non-filers, PUF weights.

### 4.1 Where the shortfall sits (`34_shortfall.py`, `$R/step34/shortfall.md`)

**The mean of one generated stack** has a standard error of about ±4%, so
the seed-to-seed spread is partly sampling noise. The gap between the parts
version and the one-item version is about 3 standard errors.

**Where the money goes.**
- About $460 per unit of the gap comes from too few units with business
  income of $50k–1m (about 12% too few).
- About $150 per unit comes from too many losses over $100k.
- The $5m-and-over group is right on average.

**Holders.** Mostly too few holders of partnership nonpassive income
(1.18% against 1.33%) and S corporation nonpassive income (1.57% against
1.77%). In every run these shares are 5–15 standard errors below the PUF.

### 4.2 The trained model itself is biased, not only noisy (`34_holdcheck.py`)

**The test.** Each trained model is shown the real PUF records under the
PUF label. For each record it gives a chance that the record holds a part.
Those chances are averaged with the PUF weights. For a correct model this
average must equal the PUF's share, whatever else is shown.

**Mean of the five runs, ratio to the PUF:**

| | Only this part hidden | Every slot hidden | Generated stack |
|---|---:|---:|---:|
| Partnership nonpassive | 0.88 | 0.94 | 0.89 |
| S corp nonpassive | 0.89 | 0.98 | 0.88 |
| Farm | 0.81 | 0.95 | 0.90 |
| Interest | 0.99 | 0.98 | 0.97 |

The models are low when they see the most of the record, in every run.
`34_hidecount.py` hid the part plus k − 1 other slots at random. For the
rare parts the ratio rises from about 0.85 at k = 1 to about 0.98 at
k = 16. That holds on held-back records too, so it is not memorization.

### 4.3 Dropout is the main cause (`34_dropcheck.py`)

**How it works.**
- In training, the network zeroes 10% of its internal numbers at random
  (dropout 0.1). Each band's output intercept settles where the average
  chance, with that noise, matches the data.
- For a rare outcome the chance is close to exp(logit). Noise in the logit
  raises the average of that, so the intercept settles lower.
- With dropout off, as in generation, the chance comes out too low. In
  John's terms: training matches the mean, generation uses the median.
- Common outcomes are not affected. It is the retransformation problem of
  a log model (Duan's smearing).

**The test.** The same models, the same questions, dropout off and on.
Ratios to the PUF:

| | Only this hidden: off / on | Every slot hidden: off / on |
|---|---:|---:|
| Partnership nonpassive | 0.88 / 0.94 | 0.94 / 0.99 |
| S corp nonpassive | 0.89 / 0.94 | 0.97 / 1.02 |
| Farm | 0.81 / 0.89 | 0.96 / 1.00 |
| Estate income | 0.83 / 0.99 | 0.97 / 1.02 |
| Wages | 0.99 / 1.00 | 0.99 / 1.00 |

### 4.4 The training and generation changes tested

**Runs.**
- All new training runs use Ns1's seed (20261016), except D0s2 and D0s3,
  which use Ns2's and Ns3's.
- "do" is the same trained model with dropout left on in generation
  (`MINIMAL_GEN_DROPOUT=1`).
- Tables: `$R/step34/fixes.md` and `fixes_d0s3.md`.
- A stack's mean has a standard error of about ±0.04.

**Generated stacks, mean over runs where several:**

| Files | Partnership nonpassive (PUF 1.33%) | S corp nonpassive (PUF 1.77%) | Mean business income, ratio to PUF |
|---|---:|---:|---:|
| Ns1–3, Bs1–2 as trained | 1.18% | 1.57% | 0.90 |
| the same five, dropout on in generation | 1.27% | 1.70% | 0.94 |
| no dropout: D0, D0s2 | 1.22% | 1.67% | 0.92 |
| no dropout: D0s3 | 1.26% | 1.65% | 0.85 |
| no dropout, the three seeds together | 1.24% | 1.66% | 0.90 |
| dropout off for the last 10 of 80 passes: Doff | 1.28% | 1.61% | 0.89 |
| no dropout, loss weighted by slots hidden: D0w | 1.36% | 1.70% | 0.91 |
| no dropout, rare business holders ×4 at ¼ weight: D0o | 1.27% | 1.60% | 0.87 |
| loss weighted by slots hidden: Hw1 | 1.19% | 1.54% | 0.77 |
| 160 passes: Lg1 | 1.14% | 1.53% | 0.85 |
| loss weighted by slots hidden, 160 passes: HL1 | 1.14% | 1.52% | 0.78 |

**What the table shows.**
- Either dropout fix closes about half of the holder gap.
- Generating with dropout on does as well as training without it, and
  needs no retraining.
- Three changes did nothing measurable: longer training, the loss weighted
  by slots hidden, and oversampling rare holders in training.
- The loss weighting was meant to even out how often training asks each
  question. It did not change the "only this hidden" shortfall either:
  HL1 0.86 against Ns1 0.86.
- Mean business income stays low in every version: 6–15% short (no
  dropout: 0.93, 0.92, 0.85 for the three seeds).

### 4.5 Where generation loses the rest (`34_gengap.py`, `$R/step34/gengap.md`)

**The steps, for each model:**
1. Real PUF records, asked generation's own questions: a random draw order;
   the earlier variables visible with their real values; the rest and the
   computed totals hidden.
2. The same questions on the model's plain generated file.
3. The plain file's actual share.
4. The stack's share.

**Partnership nonpassive, ratio to the PUF (steps 1 / 2 / 3 / 4):**

| Run | Steps 1 / 2 / 3 / 4 |
|---|---|
| D0 | 0.96 / 0.96 / 0.93 / 0.93 |
| D0s2 | 0.99 / 0.92 / 0.91 / 0.91 |
| D0w | 1.02 / 1.06 / 1.02 / 1.03 |
| Ns1 (dropout) | 0.91 / 0.87 / 0.86 / 0.87 |

**What the steps show.**
- **Step 1:** without dropout, the model is close to right on generation's
  questions.
- **Steps 1 to 2:** carrying generated values forward matters for some
  seeds (D0s2: −7%), not on average.
- **Steps 2 to 3:** a consistent 3–5% on every model. Records drawn in one
  order hold fewer holders than the same model, reading them in another
  order, expects.
  - The model's answers to different questions, such as a part given some
    variables and given others, are each learned separately.
  - They do not fit one joint distribution exactly, so the result depends
    on the order of the draws.
- **Steps 3 to 4:** the stack's oversampling and weights add nothing to
  the gap.

### 4.6 Resampling after generation does not work (`34_resample.py`, `$R/step34/resample.md`)

**The idea.** Redraw each PUF variable given every other variable on the
record, with the computed totals hidden, the variables in a random order,
for 1–3 sweeps over the whole record. Then recompute the totals. This is
Gibbs sampling, with the network's answers as the conditionals.

**It failed, for two reasons.**

1. **The question it relies on is not reliable.** On real records,
   "variable given all but the totals" gives, per model:
   - partnership 0.89–1.04;
   - S corp 0.86–1.00;
   - farm 0.81–1.12.

   Showing the totals, as `34_hidecount.py` did, makes the question nearly
   determined; without them, the calibration varies with the seed.

2. **The file never settles.** It drifts with every sweep. On D0, after
   3 sweeps against 0:
   - Schedule C 1.10 against 1.00;
   - dividends 1.10 against 1.01;
   - married 1.03 against 1.00;
   - mean business income 1.17 against 0.88.

   D0s2 drifts further. When the conditionals do not fit one joint
   distribution, repeated resampling wanders.

### 4.7 What follows

**Settled by these tests:**
- Dropout should be off in training, or on in generation. Which of the two
  is John's call; they test about the same.
- The other training changes can be dropped: longer training, the loss
  weighted by slots hidden, oversampling, dropout off only at the end.
- The remaining shortfall in rare holders (about 5–8%) and in mean
  business income (about 10%) is absorbed by the 2019 tilt with SOI business
  targets. At 6 AGI groups that costs 1–2 points of effective sample
  (`36_RESULTS.md`).

**Not reached by the tilt, so left to the model:**
- Too many units hold both partnership and S corporation items: 1.14–1.33
  of the PUF's share in every model, with or without dropout.
- Nonpassive losses are low.
- The passive share of partnership and S corp income is somewhat high.

## 5. Code switches added on 19 October

Each is off unless set, so every earlier run is unchanged.

- **`02_model.py`**
  - `MINIMAL_DROPOUT`: the dropout share for new training (default 0.1).
  - `MINIMAL_GEN_DROPOUT=1`: `load_model` returns the model with dropout
    on.
- **`31_train.py`**
  - `MINIMAL_HIDE_WEIGHT=1`: the loss weighted by the number of slots over
    the number hidden.
  - `MINIMAL_DROPOUT_OFF_LAST=N`: dropout off for the last N passes.
  - `MINIMAL_OVERSAMPLE=N`: PUF training records holding a partnership,
    S corp, estate or farm item appear N times, at 1/N weight.
  - `MINIMAL_VAR_LOG_EVERY` (default 5): `var_log.csv` with, per variable,
    on held-back and on 50,000 training PUF records:
    - the actual share of holders;
    - the model's share with only it hidden and with all hidden;
    - the loss on holders and on non-holders.

    This is on by default for new runs and changes no draw.
- **`34_run.sbatch`**
  - `STACK_SUFFIX`: a second stack from an existing model.
  - A run of 80 or more passes goes on to the stack.
