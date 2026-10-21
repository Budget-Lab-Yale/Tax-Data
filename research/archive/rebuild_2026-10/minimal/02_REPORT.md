---
title: "Minimal example, step 2: the model"
role: evidence
workstream: rebuild
status: current
updated: 2026-10-14
sot: research/rebuild/design/plan.md
supersedes: []
superseded_by: null
---

# Minimal example, step 2: the model

## Update, 14 October 2026, near midnight: model v3, coarsened SCF zeros

**Bottom line.** The model trained without trouble, but the coarsening did
not do what it was meant to. Under the SCF label, the model now puts almost
no probability on an exact zero for interest, dividends or capital gains. It
should have learned the split between zero and small from the PUF. Instead,
nothing in the SCF rows pins the split down, and the model went to one
extreme. Section "What went wrong" below.

**What changed.**

- **The loss.** In `02_model.py`, an SCF row whose reported interest,
  dividends or capital gains is zero now has a set as its target, not a
  single band. The set is {exact zero} plus the positive bands up to the
  dollar line chosen in step 1 ($110 of interest, $290 of dividends, $2,650
  of capital gains). For capital gains it also includes the smallest-loss
  negative band, which step 1 records in a column, `capital_gains_coarse_neg`.
  The loss is minus the log of the model's total probability over the set.
- **Inputs.** A coarsened zero that is visible as input still enters as band
  0, as reported.
- **Unchanged:** hiding, weights, network size, seed and the 80-epoch
  schedule.
- **Run.** Slurm job 29600319. Validation loss was 1.628. The v2 model is
  kept in `model/v2/`.

**Held-out cross-entropy, one variable hidden (nats), v2 → v3.**

| variable | PUF | SCF |
|---|---|---|
| married | 0.441 → 0.442 | 0.523 → 0.527 |
| n_dep | 0.894 → 0.894 | 0.902 → 0.903 |
| wages | 3.331 → 3.331 | 3.058 → 3.056 |
| interest | 1.307 → 1.307 | 0.658 → 0.513* |
| dividends | 0.976 → 0.976 | 0.668 → 0.555* |
| capital_gains | 0.651 → 0.650 | 0.528 → 0.431* |
| other_agi | 2.842 → 2.840 | — |
| net_worth | — | 3.774 → 3.784 |

\* The SCF test rows' zeros are now scored on the set, as in training. A set
is easier to hit than one band, so these three numbers are not comparable
with v2. Every other number is scored the same way in both runs, and none
moved by more than 0.005 nats.

**What went wrong.**

- **The model gives SCF records almost no exact zeros.** Given everything
  else in an SCF test record, it puts 98% to 99.5% probability on having
  dividends, in every wage group. Below the top 10% of earners, the data
  show 0% to 30%.
- **Why.** Every SCF zero is coarsened, so no SCF row ever tells the model
  how to split zero from small. The hope was that the split would carry over
  from the PUF. But the source label lets the model treat the two surveys
  separately, and it did.
  - The PUF side is untouched: its cross-entropies and its tables match v2.
  - Under the SCF label, the model now gives PUF records 3.7 to 4.6 nats
    worse scores for interest, dividends and capital gains. It no longer
    expects zeros under that label.
- **What this means for the fix.** The set-based loss needs something that
  ties the zero-versus-small split across the two surveys. Two
  possibilities:
  - share that part of the model across labels;
  - add a guard target on the SCF's holder shares when generating.

  Otherwise the split is free under the SCF label. This is John's call; I
  have not tried either.

## Update, 14 October 2026, evening: second run (v2)

**What changed.**

- **Finer negative bands.** Step 1 rebanded the table to 84 bands per
  amount:
  - band 0 is exactly zero;
  - bands 1–10 are negatives (there were 4);
  - bands 11–83 are the same 73 positive bands as before.

  The code now reads the layout from `band_edges.json` instead of assuming
  78 bands.
- **The learning-rate decay now finishes.** The cosine decay runs over 80
  epochs instead of 200, so the rate is down to 5% of its peak at the end.
  - The first run had stopped at epoch 89, with the rate still at two-thirds
    of its peak.
  - This time the run used all 80 epochs. The validation loss was still
    falling slightly at the end (1.6371 at epoch 70, 1.6367 at epoch 80), so
    early stopping never fired.
- **Unchanged:** the seed and the network. The parameter count is 609,278,
  up 5,412 for the six extra band tokens and outputs.
- **Files.** The first run's files are kept in `model/v1/`. Everything else
  in this report below this section describes the first run.

**Held-out cross-entropy, one variable hidden (nats), first run → second
run.**

| variable | PUF | SCF |
|---|---|---|
| married | 0.440 → 0.441 | 0.525 → 0.523 |
| n_dep | 0.895 → 0.894 | 0.903 → 0.902 |
| wages | 3.340 → 3.331 | 3.056 → 3.058 |
| interest | 1.308 → 1.307 | 0.670 → 0.658 |
| dividends | 0.977 → 0.976 | 0.685 → 0.668 |
| capital_gains* | 0.650 → 0.651 | 0.535 → 0.528 |
| other_agi* | 2.788 → 2.842 | — |
| net_worth* | — | 3.798 → 3.774 |

\* These variables now have ten negative bands instead of four. Finer bands
make the cross-entropy rise without any change in fit, so their two numbers
are not directly comparable. For the other five variables the bands are
unchanged, and the two runs are within 0.02 nats. As before, the all-hidden
cross-entropy equals the baseline for every variable; the full tables are in
`model/02_tables.md`.

The validation loss went from 1.630 to 1.637 nats per hidden slot. The rise
is the finer bands, not a worse fit.

**The capital-gains miss is smaller, but still there.** Share of PUF test
records with any capital gain or loss, by dividend group:

| dividend group | data | first run | second run |
|---|---|---|---|
| P40–60 | 0.53 | 0.58 | 0.56 |
| P60–80 | 0.64 | 0.76 | 0.71 |
| P80–90 | 0.79 | 0.85 | 0.82 |
| P99–100 | 0.89 | 0.94 | 0.95 |

**Label swap.**

- Interest still responds most to the label: 0.30 nats for PUF records under
  the SCF label, down from 0.38.
- Capital gains now respond more: 0.13 nats for PUF records, up from 0.08.
  Probably the ten negative bands make the difference between the sources'
  losses visible: the PUF caps losses at $3,000 and the SCF does not. I have
  not checked this.

Generation from the second run is in `03_REPORT.md`.

## The first run (v1)

## Bottom line

- The model learns real relationships. Knowing the rest of a record lowers
  the cross-entropy of every variable below its plain distribution: by 0.1
  to 0.45 nats for the amounts, most for wages, other_agi and net worth.
- On the PUF the conditional tables match the held-out data closely. The one
  clear miss: the model gives too many capital gains to records with mid-sized
  dividends, by up to 11 points.
- On the SCF the model is right in broad shape. It is weaker at the top of
  wealth: it puts too little of the highest earners in the P90–99 net worth
  group.
- The source label matters most for interest and wages, and little for
  married, dependents and dividends.

## What ran

Slurm job 29587349 ran `02_model.py` (training) and `02_evaluate.py`
(scoring) through `02_train.sbatch`, on one GPU. Training took 3 minutes.

The data is the rebuilt stacked table: 242,069 rows (207,692 PUF, 34,377
SCF). It has six amounts. `other_agi` (AGI minus the four named incomes) is
on PUF rows only. `net_worth` is on SCF rows only. Interest is taxable
interest only. Every SCF row now has net worth. Test split: 20,758 PUF rows
and 3,400 SCF rows.

**The model.**

- A record is a list of slots. Two are context, always given and never
  predicted: source and year. Eight are predicted: married, n_dep and the
  six amount bands.
- A slot that a source does not have is left out of that record. The network
  does not see it and is never scored on it. This applies to net worth on the
  PUF and to other_agi on the SCF.
- One token table serves all slots. The six amounts share one set of 78 band
  tokens: band 40 of wages and band 40 of dividends start from the same
  vector. A slot embedding tells the network which variable a slot holds.
  Each predicted variable has its own output layer.
- Four transformer layers, width 128, four attention heads. 603,866
  parameters.

**Training.**

- Masked any-subset. For each record, draw a share u uniformly between 0 and
  1. Hide ceil(u × n) of its n predicted slots at random, so at least one
  slot is always hidden. Score the hidden slots only.
- Weights follow the plain rule: survey weights rescaled within each source
  so that they sum to that source's record count.
- AdamW with a peak learning rate of 0.001, batches of 4,096, three epochs of
  warm-up, then cosine decay. Early stopping on the validation loss, with a
  fixed seed.
- The run stopped at epoch 89. The best was epoch 77, with a validation loss
  of 1.630 nats per hidden slot. Training loss at that point was 1.620. So
  there is no sign of overfitting.

**Files.** In `/nfs/roberts/scratch/pi_nrs36/jar335/minimal/model/`:

- `model.pt`: the trained weights.
- `train_log.csv`: loss by epoch.
- `train_meta.json`: the run settings.
- `02_eval.json`: every number in this report.
- `02_tables.md`: the full tables.

## 1. Held-out cross-entropy

Cross-entropy is the average of −log(probability the model gave the true
value). It is measured in nats, and lower is better. A drop of 0.1 nats means
the true value got about 10% more probability on average.

Three cases:

- **One hidden:** the variable is predicted from everything else in the
  record.
- **Half hidden:** the variable plus random others are hidden, half the
  record in all. This is averaged over three draws.
- **All hidden:** the model sees only source and year.

The baseline is the training data's weighted distribution of that variable
for that source.

PUF test records:

| variable | one hidden | half hidden | all hidden | baseline |
|---|---|---|---|---|
| married | 0.440 | 0.525 | 0.653 | 0.653 |
| n_dep | 0.895 | 0.947 | 1.003 | 1.002 |
| wages | 3.340 | 3.518 | 3.767 | 3.765 |
| interest | 1.308 | 1.383 | 1.567 | 1.567 |
| dividends | 0.977 | 1.053 | 1.240 | 1.239 |
| capital_gains | 0.650 | 0.707 | 0.847 | 0.847 |
| other_agi | 2.788 | 2.973 | 3.241 | 3.241 |

SCF test records:

| variable | one hidden | half hidden | all hidden | baseline |
|---|---|---|---|---|
| married | 0.525 | 0.580 | 0.682 | 0.682 |
| n_dep | 0.903 | 0.950 | 1.000 | 1.000 |
| wages | 3.056 | 3.156 | 3.335 | 3.332 |
| interest | 0.670 | 0.691 | 0.784 | 0.782 |
| dividends | 0.685 | 0.728 | 0.867 | 0.866 |
| capital_gains | 0.535 | 0.557 | 0.635 | 0.634 |
| net_worth | 3.798 | 3.903 | 4.113 | 4.103 |

How to read these:

- **All hidden equals the baseline.** With nothing to condition on, the model
  gives back the plain distribution for each source. This is the starting
  point for generation, so it needs to be right, and it is.
- **The gain from conditioning is the baseline minus the one-hidden column.**
  - Largest for wages, other_agi and net worth: 0.3 to 0.45 nats. These have
    many bands and depend strongly on the rest of the record.
  - Smallest for n_dep: about 0.1 nats. The incomes say little about the
    number of children.
- **Half hidden falls between the other two**, as it should.

## 2. Conditional tables, model against held-out data

The model column is the model's probability for the variable given all other
slots of each test record, averaged with the survey weights. The data column
is the weighted share in the same records. "records" is the number of test
records in the cell.

Wage and dividend groups are ranks within the positives: P0–20 is the lowest
fifth of those with any wages, and so on. "zero" is no wages.

**(a) PUF: share with any dividends, by wage group and married.** The last
column puts the same records under the SCF label (see section 3).

| wage group | married | records | data | model | model, SCF label |
|---|---|---|---|---|---|
| zero | no | 1,968 | 0.33 | 0.31 | 0.25 |
| zero | yes | 2,287 | 0.44 | 0.43 | 0.37 |
| P0–20 | no | 1,613 | 0.06 | 0.06 | 0.04 |
| P0–20 | yes | 611 | 0.29 | 0.33 | 0.27 |
| P20–40 | no | 1,501 | 0.04 | 0.04 | 0.03 |
| P20–40 | yes | 524 | 0.16 | 0.18 | 0.11 |
| P40–60 | no | 1,326 | 0.06 | 0.07 | 0.05 |
| P40–60 | yes | 728 | 0.15 | 0.15 | 0.09 |
| P60–80 | no | 1,031 | 0.14 | 0.13 | 0.11 |
| P60–80 | yes | 1,221 | 0.22 | 0.20 | 0.15 |
| P80–90 | no | 358 | 0.26 | 0.23 | 0.22 |
| P80–90 | yes | 1,063 | 0.25 | 0.25 | 0.23 |
| P90–99 | no | 451 | 0.42 | 0.41 | 0.41 |
| P90–99 | yes | 2,812 | 0.42 | 0.43 | 0.41 |
| P99–100 | no | 424 | 0.68 | 0.72 | 0.76 |
| P99–100 | yes | 2,840 | 0.78 | 0.78 | 0.79 |

The model is within 5 points of the data in every cell. The same table for
the SCF is in `02_tables.md`. Its cells hold 12 to 685 records, so most of
its gaps are within sampling noise.

**(b) PUF: share with any capital gain or loss, by dividend group.**

| dividend group | records | data | model |
|---|---|---|---|
| zero | 11,000 | 0.035 | 0.035 |
| P0–20 | 868 | 0.32 | 0.29 |
| P20–40 | 915 | 0.44 | 0.40 |
| P40–60 | 1,158 | 0.53 | 0.58 |
| P60–80 | 1,459 | 0.64 | 0.76 |
| P80–90 | 1,140 | 0.79 | 0.85 |
| P90–99 | 2,317 | 0.87 | 0.88 |
| P99–100 | 1,901 | 0.89 | 0.94 |

This is the clearest miss. For records with dividends in the P60–80 group,
the model says 76% have a gain or loss and the data say 64%. With 1,459
records in the cell, a gap that large is not sampling noise. I do not know
the cause yet.

**(c) Dependents by married (data / model).**

| source | married | 0 | 1 | 2 | 3+ |
|---|---|---|---|---|---|
| PUF | no | 0.74 / 0.73 | 0.15 / 0.15 | 0.08 / 0.08 | 0.03 / 0.04 |
| PUF | yes | 0.51 / 0.51 | 0.18 / 0.18 | 0.20 / 0.19 | 0.12 / 0.12 |
| SCF | no | 0.72 / 0.72 | 0.14 / 0.15 | 0.03 / 0.05 | 0.11 / 0.09 |
| SCF | yes | 0.59 / 0.61 | 0.12 / 0.15 | 0.20 / 0.15 | 0.10 / 0.10 |

PUF: close. SCF: off by up to 5 points for married couples with one or two
dependents. The SCF has 1,625 married test rows, but they come from 325
households, so these numbers are noisier than the row count suggests.

**(d) SCF: net worth group by wage group (data / model), for selected wage
groups.**

| wage group | records | zero or negative | P40–60 | P60–80 | P80–90 | P90–99 | P99–100 |
|---|---|---|---|---|---|---|---|
| zero | 1,060 | 0.12 / 0.13 | 0.14 / 0.17 | 0.19 / 0.19 | 0.09 / 0.09 | 0.07 / 0.09 | 0.01 / 0.01 |
| P40–60 | 462 | 0.13 / 0.16 | 0.27 / 0.20 | 0.10 / 0.13 | 0.03 / 0.05 | 0.01 / 0.02 | 0.00 / 0.00 |
| P90–99 | 338 | 0.02 / 0.02 | 0.10 / 0.09 | 0.13 / 0.18 | 0.17 / 0.27 | 0.52 / 0.38 | 0.02 / 0.03 |
| P99–100 | 143 | 0.00 / 0.00 | 0.00 / 0.01 | 0.00 / 0.02 | 0.00 / 0.12 | 0.68 / 0.58 | 0.32 / 0.27 |

The full table, with all eight wage groups and all eight net worth groups, is
in `02_tables.md`.

The pattern is right: higher wages go with higher net worth. But the model is
too smooth at the top. Among the top 10% of earners it moves 10 to 14 points
of weight from the P90–99 net worth group down to lower groups, mostly P80–90. The top two wage
groups hold 143 and 338 test rows, from 31 and 72 households.

## 3. Label swap

Each held-out record was scored again under the other source's label, with
the same hidden slots. Slots that the other source never has were removed
from both scorings: net worth from SCF records and other_agi from PUF
records. So the only change is the label.

The table shows the increase in cross-entropy in nats. Positive means the
other label fits worse.

| variable | PUF records under SCF label: one hidden | all hidden | SCF records under PUF label: one hidden | all hidden |
|---|---|---|---|---|
| married | 0.04 | 0.01 | 0.06 | 0.01 |
| n_dep | 0.05 | 0.02 | 0.06 | 0.01 |
| wages | 0.19 | 0.09 | 0.21 | 0.11 |
| interest | 0.38 | 0.27 | 0.29 | 0.27 |
| dividends | 0.08 | 0.06 | 0.02 | 0.02 |
| capital_gains | 0.08 | 0.08 | 0.07 | 0.09 |

The half-hidden numbers are in `02_tables.md` and fall between these two.

- **Interest responds most to the label.** That fits step 1: the sources
  differ most on who has any interest.
- **Wages come next.** The SCF covers more people with no wages and has a
  different top.
- **Married, dependents and dividends respond little.**

Under the SCF label, the share of PUF records with dividends falls by 3 to 6
points in the lower and middle wage groups. It barely moves at the top. That
is the last column of table (a).

## What I am not sure of

- **The learning-rate schedule.** The cosine decay was set for 200 epochs,
  but early stopping ended the run at epoch 89. At that point the learning
  rate was still about two-thirds of its peak. A schedule that decays fully
  before stopping might improve the fit a little, and might narrow the
  capital-gains miss in (b). I did not retrain, because the step 3 files were
  already being used.
- **Thin SCF test cells.** The SCF test split has 3,400 rows from 621
  households. Each household appears five times, once per implicate. Most of
  the SCF table gaps cannot be told apart from sampling noise.
- **The baseline's smoothing.** Each band gets an extra half record's weight,
  so that a band with no training records does not score as infinite. This
  changes the baseline by less than 0.01 nats.
