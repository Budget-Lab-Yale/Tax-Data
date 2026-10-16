---
title: "Minimal example: network size sweep at seven variables"
role: evidence
workstream: rebuild
status: current
updated: 2026-10-14
sot: research/rebuild/design/plan.md
supersedes: []
superseded_by: null
---

# Network size sweep at seven variables

## Bottom line

- A larger network does not help at the current seven variables. Held-out
  loss flattens at about 0.3 to 0.6 million parameters, which is where the
  current network (0.61M) sits.
- A larger network fits the training records better but not the held-out
  ones. So it is learning those particular records, not the relationships.

## What ran

- I trained the step 2 model five times, at five sizes. Everything except
  width and depth was the same as the v2 run:
  - the same data, split and seed;
  - the same training recipe: cosine decay over 80 epochs, then early
    stopping on validation loss.
- Heads keep 32 numbers each, and the feed-forward layer is twice the
  width, as in v2.
- The 4-layer, width-128 run reproduces v2 exactly (validation loss
  1.6367 at epoch 80).
- Slurm array job 29595489 did the training and test scoring. Job
  29595974 rescored the gap (see below).
- Files are in `/nfs/roberts/scratch/pi_nrs36/jar335/minimal/model/sweep/`:
  one folder per size, plus `summary.md`, which also has the half-hidden
  tables.

**Measuring the gap.** The gap is the training loss minus the validation
loss. My first measurement was too noisy to use. The same validation
records, scored twice with different random choices of which slots to
hide, gave losses 0.010 nats apart, which is as large as the gap itself.
The cause is the uneven survey weights: a few heavy records decide much of
the loss.

So each saved network was scored again on both splits, two ways:

- **32 hiding patterns per record**, as in training. The leftover noise
  from the random hiding is about 0.001.
- **One slot hidden at a time**, for every slot. This involves no random
  hiding at all.

A negative gap means the training records fit better. Some of the gap at
the smallest size comes from the two splits holding different records, not
from overfitting. The signal is how the gap changes with size.

## Size against loss (nats per hidden slot)

| layers | width | parameters | minutes | best epoch / epochs run | validation loss during training | training loss, rescored | validation loss, rescored | gap | gap, one slot hidden |
|---|---|---|---|---|---|---|---|---|---|
| 3 | 64 | 140,350 | 1.0 | 80 / 80 | 1.6388 | 1.6339 | 1.6451 | −0.011 | −0.016 |
| 3 | 96 | 283,998 | 2.5 | 79 / 80 | 1.6372 | 1.6310 | 1.6433 | −0.012 | −0.019 |
| 4 | 128 | 609,278 (v2) | 3.9 | 80 / 80 | 1.6367 | 1.6283 | 1.6426 | −0.014 | −0.025 |
| 5 | 160 | 1,131,870 | 6.9 | 79 / 80 | 1.6371 | 1.6261 | 1.6432 | −0.017 | −0.035 |
| 6 | 224 | 2,561,726 | 7.3 | 38 / 50 | 1.6387 | 1.6310 | 1.6447 | −0.014 | −0.027 |

The validation loss during training is the number early stopping used, and
the one 02_REPORT.md quotes. The rescored validation loss is higher at
every size. It uses different random choices of which slots to hide, and
those happen to be less favourable. The same choices are used for every
size, so comparisons down a column are fair.

## Test loss by variable, PUF (nats)

One slot hidden, by network size; then all hidden, which is the same at
every size.

| variable | 0.14M | 0.28M | 0.61M | 1.13M | 2.56M | all hidden, any size | baseline |
|---|---|---|---|---|---|---|---|
| married | 0.450 | 0.443 | 0.441 | 0.441 | 0.442 | 0.653–0.654 | 0.653 |
| n_dep | 0.903 | 0.898 | 0.894 | 0.893 | 0.898 | 1.002–1.003 | 1.002 |
| wages | 3.345 | 3.337 | 3.331 | 3.331 | 3.341 | 3.766–3.767 | 3.765 |
| interest | 1.315 | 1.310 | 1.307 | 1.306 | 1.310 | 1.567–1.569 | 1.567 |
| dividends | 0.986 | 0.980 | 0.976 | 0.974 | 0.979 | 1.239–1.240 | 1.239 |
| capital_gains | 0.659 | 0.653 | 0.651 | 0.649 | 0.654 | 0.848–0.849 | 0.848 |
| other_agi | 2.862 | 2.846 | 2.842 | 2.840 | 2.850 | 3.301–3.304 | 3.300 |

## Test loss by variable, SCF (nats)

| variable | 0.14M | 0.28M | 0.61M | 1.13M | 2.56M | all hidden, any size | baseline |
|---|---|---|---|---|---|---|---|
| married | 0.534 | 0.525 | 0.523 | 0.527 | 0.517 | 0.683–0.685 | 0.682 |
| n_dep | 0.904 | 0.901 | 0.902 | 0.904 | 0.909 | 1.000–1.001 | 1.000 |
| wages | 3.063 | 3.044 | 3.058 | 3.074 | 3.060 | 3.328–3.339 | 3.332 |
| interest | 0.668 | 0.650 | 0.658 | 0.666 | 0.678 | 0.781–0.785 | 0.782 |
| dividends | 0.681 | 0.680 | 0.668 | 0.681 | 0.679 | 0.866–0.868 | 0.866 |
| capital_gains | 0.532 | 0.533 | 0.528 | 0.541 | 0.541 | 0.633–0.636 | 0.632 |
| net_worth | 3.756 | 3.768 | 3.774 | 3.824 | 3.804 | 4.136–4.153 | 4.134 |

## What it shows

- **The curve flattens at the current size.**
  - Validation loss falls by 0.002 nats from the smallest network to the
    current one.
  - Doubling the current size changes nothing.
  - The largest network is worse.
  - On the PUF, every variable gains 0.008 to 0.020 nats from the smallest
    network to the current one, and 0.002 or less from the next size up.
    The largest network is worse than the current one on every variable.
- **The gap opens as the network grows.**
  - From the smallest network to the 1.13M one, the one-slot-hidden gap
    roughly doubles, from 0.016 to 0.035 nats. Training records fit 0.028
    nats better, but validation records only 0.009 nats better.
  - The largest network stopped early, at epoch 50, because its validation
    loss had stopped improving. Its learning rate was still at 60% of its
    peak. So its row partly reflects the training recipe as well as size.
- **The SCF is too thin to show a trend.**
  - Its numbers move by 0.01 to 0.05 nats between sizes with no order
    (621 test households).
  - Net worth is the exception: it gets worse above the current size, from
    3.774 to 3.824. This is consistent with the larger networks starting
    to memorize the SCF's few thousand households first.

**Not checked.** Whether a lower learning rate would help the largest
network. With seven variables, no size test is needed until variables are
added.

## Files

- `research/rebuild/minimal/02_size_sweep.py`: training, rescoring and
  summary. It reuses 02_model.py and 02_evaluate.py unchanged.
- `02_size_sweep.sbatch`: the five-size array.
- `02_size_sweep_rescore.sbatch`: the rescoring and the summary.

## Revision history

- 2026-10-14: written.
