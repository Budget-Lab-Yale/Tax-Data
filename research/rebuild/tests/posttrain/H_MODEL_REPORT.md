---
title: "Test 3, piece H — pretraining the joint PUF and SCF model"
role: report
workstream: rebuild
status: current
updated: 2026-10-09
sot: research/rebuild/tests/posttrain/SPEC.md
supersedes: []
superseded_by: null
---

# Pretraining the joint PUF and SCF model

Piece H trained the "pretrained" model of John's plan: one transformer over
every PUF variable of test 1 and the 24 SCF wealth variables, fitted to the
2015 PUF and the 2013 and 2016 SCF together. It then wrote the generated
and completed records that the pretraining evaluation (piece E) and the
post-training methods (A, B, C, D, F) read. File names and columns are in
SPEC.md, contract entries "H, 9 October 20:46" and "H, 21:25".

Summary:

- Six models: two versions of the data, three seeds each. Each took 8 to
  18 minutes to train on one GPU and stopped after 29 to 31 epochs, keeping
  epoch 14 to 16. Each has about 6.9 million parameters.
- On wealth completion for SCF records, version 2 (the main model) scores
  the same as test 2's model; version 1 scores slightly worse.
- In every run the SCF part of the validation loss is lowest around epoch
  12 and rises after it, while the PUF part is still falling when training
  stops.
- All completions and generations were written for every model:
  `$T2/model/full/` and `$T2/model/full_v2/`, about 750 MB each.

## What was trained

Two versions of piece G's data, three training runs ("seeds") each. A seed
here is the starting point of the random numbers used in training (initial
network weights, the order of records, which values are hidden); two seeds
differ only by chance, so their spread shows how much of a result is noise.

| | Version 1 (`full`) | Version 2 (`full_v2`, the main model) |
|---|---|---|
| Model variables | 63: test 1's 39 PUF variables, 24 wealth | 64: the same plus total Schedule E (`E02000`) |
| PUF variables the SCF also measures | 11 | 17 (adds dependents under 17, gender, Social Security, taxable pensions, farm, total Schedule E) |
| Training records | 213,600 (PUF 166,156; SCF 47,444 = 5 implicates of each tax unit) | same records |
| Validation records | 27,477 (PUF 20,768; SCF 6,709) | same |
| Parameters | 6.87 million | 6.92 million |
| Checkpoint | `$T2/model/full/checkpoint_full_w1_seedNN.pt`, 28 MB | `$T2/model/full_v2/checkpoint_full_v2_w1_seedNN.pt`, 28 MB |

Settings are test 2's `joint_w1` runs, unchanged (`h_train.py` is a copy of
test 2's `b_train.py`; the network code is test 2's `b_model.py`):

- **Network.** Base size: 6 layers, width 256, 8 attention heads. Each
  record is a set of variable slots; the network sees the record's source
  (PUF or SCF) and its year as a number, so 2015 lies between the SCF waves.
- **What it learns.** At every step, for each record, a share r drawn
  uniformly between 0 and 1 of its *observed* values is hidden, and the
  network is scored on predicting the hidden values from the rest
  ("any-order" training). Values a source does not measure (wealth on the
  PUF, most PUF items on the SCF, age on PUF records outside the demographic
  file) are always hidden and never scored. So one network learns every
  conditional distribution the generation and completion steps need.
- **Weights.** Each record's contribution to the loss is multiplied by its
  population weight (PUF: S006/100; SCF: the implicate weight), scaled to
  average 1. The weight is not a model variable. Weighting leaves the
  equivalent of about 84,000 equally weighted records out of 213,600,
  because PUF weights range from about 1 to over 1,000.
- **Stopping.** After each pass over the training records ("epoch") the
  loss is measured on the validation records, with hiding patterns fixed
  once; training stops after 15 epochs without improvement and keeps the
  best epoch. Optimizer AdamW, learning rate 5e-4 with warm-up and cosine
  decay, batch 512 records, bf16 arithmetic.

## Training results

Loss is the cross-entropy of hidden values: minus the log of the
probability the model gave the true value, averaged over hidden values, in
nats (lower is better). Losses of the two versions are not comparable with
each other: version 2 scores more variables on SCF records.

| Version | Seed | Best epoch (of) | Validation loss: all | PUF records | SCF records | Minutes | GPU |
|---|---|---|---|---|---|---|---|
| 1 | 1 | 14 (29) | 1.0449 | 0.7364 | 1.2579 | 17.6 | RTX A5000 |
| 1 | 2 | 14 (29) | 1.0451 | 0.7364 | 1.2582 | 17.6 | RTX A5000 |
| 1 | 3 | 14 (29) | 1.0442 | 0.7404 | 1.2534 | 8.4 | H100 |
| 2 | 1 | 16 (31) | 0.9962 | 0.7266 | 1.1578 | 15.8 | RTX 5000 Ada |
| 2 | 2 | 15 (30) | 0.9988 | 0.7278 | 1.1612 | 11.7 | L40S |
| 2 | 3 | 15 (30) | 0.9975 | 0.7263 | 1.1601 | 18.4 | RTX A5000 |

Training curves: `h_training_curves_full.png` and
`h_training_curves_full_v2.png` in this folder (left: training and
validation loss over all records; middle and right: validation loss on PUF
and on SCF records).

**The two sources want different stopping points.** In every run the SCF
records' validation loss reaches its minimum around epoch 11 to 13 and then
rises, while the PUF records' keeps falling until training stops (version
1, seed 1: PUF 0.736 at the kept epoch 14 and 0.731 at epoch 29; SCF 1.2573
at epoch 12, 1.2725 at epoch 29). The SCF has about 9,500 tax units (each
in five implicates) against 166,000 PUF records, so the network starts to
memorize SCF units first. The stopping rule uses the weighted sum of
the two, so the kept model is slightly short of the best for the PUF and
slightly past the best for the SCF. The differences are small (under 1
percent of the loss each way), but they are the same in every run.

## Wealth completion score

The one score comparable across versions and with test 2: for SCF records
with wealth observed, hide all 24 wealth values and score the true values in the
fixed completion order, each given the record's observed variables and the
true values of the wealth variables before it (nats per record, lower is
better).

| Model and records | Seed 1 | Seed 2 | Seed 3 |
|---|---|---|---|
| Version 1, SCF validation records with wealth observed (6,112) | 35.66 | 35.68 | 35.68 |
| Version 2, same | 35.50 | 35.52 | 35.56 |
| Test 2 `joint_w1` (10 PUF-side variables), same records | 35.51 | 35.50 | 35.50 |
| Version 1, SCF training records (42,057) | 33.36 | 33.37 | 33.38 |
| Version 1, SCF test records (12,021) | 34.84 | 34.83 | 34.85 |
| Version 2, SCF training records | 32.87 | 32.97 | 32.97 |
| Version 2, SCF test records | 34.69 | 34.69 | 34.71 |
| Test 2 `joint_w1`, SCF test records | 34.71 | 34.74 | 34.69 |

Version 1 gives SCF records fewer income items than test 2 did (no rent,
pensions or Social Security among its 11 shared variables). It completes
wealth worse than test 2's model by about 0.15 nats per record, against a
spread across seeds of at most 0.06. Version 2, whose 17 shared variables
include those items, scores the same as test 2's model, which had no
PUF-only variables at all. The training records score 1.5 (version 1) to
1.8 (version 2) nats per record better than the test records: that is how
much closer the kept model is to the records it was trained on than to new
records from the same survey.

## Completions written

For each version and seed, after training (`h_complete.py`; all draws at
temperature 1 in a fixed order, each restricted to codes the decoder can
turn into dollars, as in test 2; tokens decoded to dollars with piece G's
`g_tokenizer.py`):

| File | What | Records × draws | Minutes (version 1, seed 1, L40S GPU) |
|---|---|---|---|
| `h_gen_puf` | (a) whole PUF records from nothing: tag PUF, year 2015, the PUF variables in the data's generation order (test 1's), equal weights summing to the PUF training total (120.7 million returns) | 166,156 × 1 | 2.4 |
| `h_puf_wealth` | (b) PUF training records with their 24 wealth values drawn, tag SCF, year 2015, decoded at 2015 between the SCF waves | 166,156 × 5 | 7.3 |
| `h_scf_train` | (c) SCF training records' wealth redrawn from their observed variables | 47,444 × 5 | 2.1 |
| `h_scf_test` | (c) the same for SCF test records | 13,415 × 5 | 0.6 |

Two choices to note. In (b), PUF records without a demographic-file age
keep it unknown: age is not drawn for them, and their wealth is drawn given
what is known. In (a), age, gender and the earnings split are drawn for
every generated record, while the real PUF has them only for records in the
demographic file; comparisons of those three variables should use the
records where they are known. Test 1's "PUF records completed from partial
views" were not written.

## Code and checks

`h_train.py` differs from test 2's `b_train.py` in three ways: it logs the
validation loss separately for each source; it drops any weight-kind
variable automatically (`full` has none); and it identifies the wealth
variables from the data (observed on SCF records, never on PUF records).
`h_complete.py` differs from test 2's `b_complete.py` in filling only the
requested variables (test 2's filled every unobserved slot, which on the
`full` data would have drawn the 28 PUF-only variables for SCF records),
and in generating records from nothing. Before the real runs, both were run
on a fabricated 63-variable dataset with the `full` layout
(`h_make_fake.py`), and `h_complete.py` was run with test 2's checkpoint on
test 2's data, where it reproduced test 2's completion score (34.6 nats per
record on 3,000 SCF test records).

The dataset each model was trained on is copied next to its checkpoints
(`$T2/model/full/data/`, `$T2/model/full_v2/data/`), and the completions
read that copy.
