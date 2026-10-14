---
title: "Test 1 — part B: the model"
role: results
workstream: rebuild
status: draft
updated: 2026-10-08
sot: research/rebuild/tests/puf_generator/SPEC.md
supersedes: []
superseded_by: null
---

# Part B: the model

This report describes the model trained for Test 1 (SPEC.md) and how it was
trained. It reports losses only; whether the generated records are close
to the PUF is part C's job (`EVAL_REPORT.md`).

**Status: draft written while training was still running.** Sections
marked "pending" are filled from `$S/model/run_*.json`,
`$S/model/train_log_*.csv` and `$S/model/synthetic_info.json` once the
runs finish ($S is `/nfs/roberts/scratch/pi_nrs36/jar335/puf_generator`).

## Terms

- **Token.** Part A turns each value into an integer code: a category, an
  amount bin, an exact point value (such as zero), or a weight value. The
  model works on these codes. Part A's `tokenizer.decode` turns codes back
  into dollar values.
- **Embedding.** A learned vector of numbers (here 128, 256 or 512 of them)
  that stands for a code or a variable inside the model.
- **Transformer encoder.** A stack of layers in which every variable's
  vector is updated using a weighted combination of every other variable's
  vector ("attention"); the weights depend on the content, so the model can
  learn which variables inform which. "Width" is the vector length;
  "heads" is the number of separate attention weightings per layer.
- **Cross-entropy (loss).** The average of minus the log of the
  probability the model gave to the true code, in nats (natural-log units).
  Lower is better; 0 would mean certainty about every hidden value.
- **Epoch.** One pass over all 166,156 training records.
- **Overfitting.** The model fitting quirks of the training records that do
  not hold in other records; it shows as validation loss rising while
  training loss keeps falling.
- **Early stopping.** Keeping the model from the epoch with the lowest
  validation loss and stopping once it has not improved for 15 epochs.

## Architecture

- One input slot per variable (40 slots: the weight, 9 categoricals, 30
  amounts). There is no position encoding: slots are an unordered set, told
  apart only by the variable-identity embedding.
- A slot's input is the sum of (1) an embedding for the variable's
  identity, (2) an embedding for its code, or a single learned "hidden"
  embedding if the value is hidden, and (3) for amounts and the weight, a
  learned function of the code's position in the variable's value order
  (0 for the lowest bin, 1 for the highest; codes are ranked by their `lo`
  and `hi` bounds in `meta.json`). The function is a small two-layer
  network applied to the position and to sines and cosines of it at 16
  frequencies. This lets the model treat neighbouring bins as similar from
  the start.
- A transformer encoder (pre-normalisation layers, feed-forward width four
  times the model width, dropout 0.1), then a final normalisation and a
  linear layer.
- Output: for each slot, a score for each of that variable's own codes,
  computed as the dot product of the slot's output vector with a learned
  output embedding per code (which also gets the same kind of
  position-in-order term for amounts and the weight) plus a per-code
  constant. Scores are turned into probabilities over that variable's codes
  only. The 40 variables have 7,153 codes in all; the largest variable has
  318.
- Code: `model.py` (model, masking, generation), `train.py`, `generate.py`,
  Slurm scripts `train.sbatch`, `generate.sbatch`, `finish.sbatch`.

## Training

- **Any-order masking**, as in SPEC.md: for every record at every step, a
  hiding rate r is drawn uniformly from (0, 1), each variable is hidden
  with probability r, and at least one is always hidden. The loss is the
  cross-entropy of the hidden variables given the visible ones, averaged
  over all hidden slots in the batch. Records are unweighted; the weight
  is one of the variables.
- **Validation loss** is computed on the 20,768 validation records with a
  fixed set of masks (four per record, drawn once with seed 777), so it is
  comparable across epochs and across model sizes.
- **Second validation measure: sequential log-likelihood.** For the best
  checkpoint of each run, the exact log-likelihood of each validation
  record under the generation procedure: the sum over the 40 variables, in
  generation order, of minus the log probability of the true code given
  the variables before it. Reported per record (nats). This measures the
  model as it is used to generate, which the masked loss does not do
  exactly.
- Settings: batch 512 records (325 steps per epoch); AdamW optimiser
  (a standard variant of gradient descent), learning rate 5e-4 with a
  1,000-step warm-up and cosine decay to one tenth over at most 300
  epochs; weight decay 0.01; gradient norm clipped at 1; bf16 mixed
  precision (16-bit arithmetic for most of the computation). Seed
  20261008 for initialisation, shuffling and masks.
- **Moving average of weights.** Validation, checkpoints and generation use
  an exponential moving average of the model's parameters over training
  steps (decay 0.999, so roughly the last 1,000 steps). On the fabricated
  test data below this reduced the gap between generated and true
  distributions by about a third for the small model and by more for a
  larger one that otherwise overfitted quickly. It makes validation loss
  in the first epochs higher than training loss, because the average
  still contains the random starting values.

## Check on fabricated data

Before part A's files existed, the code was tested on fabricated records
with a known joint distribution (`toy_check.py`, outputs in `$S/dev/`): a
weight with six values, a categorical A, a categorical B that depends on A,
an amount C whose zero share and level depend on B, an amount D that
follows C, and five independent categoricals. With 100,000 training
records and a small model (3 layers, width 128), 100,000 generated records
reproduced P(B | A) to within about 0.02 in every cell. For two- and
three-variable tables the total variation distance (half the sum of
absolute differences in cell shares) between generated and true records
was 1.5 to 3 times the distance between two independent true samples of
the same size, for example 0.012 against 0.005 for the A by B table, with
the moving average on. Without it the ratios were 2 to 5. Larger models
overfitted the fabricated data within 40 to 70 epochs and were worse.

An experimental option, `--order-frac`, makes a share of training masks
follow the generation order. It is off in all runs reported here, since
the spec asks for any-order training (needed for imputation under D22).

## Model sizes and losses — pending

| Run | Layers | Width | Heads | Parameters | Best epoch | Validation loss | Sequential log-likelihood per record | Training time | GPU |
|---|---|---|---|---|---|---|---|---|---|
| small | 4 | 128 | 4 | 2,695,153 | 125 (stopped at 140) | 1.3653 | 50.74 | 12.0 min | RTX 5000 Ada |
| base | 6 | 256 | 8 | 8,559,601 | pending | | | | RTX 5000 Ada |
| large | 8 | 512 | 8 | 32,976,369 | pending | | | | RTX 5000 Ada |

Training curves: `$S/model/training_curves.png`; per-epoch values in
`$S/model/train_log_<run>.csv`; the chosen run's log is copied to
`$S/model/train_log.csv`.

## Generation — pending

All 40 variables start hidden and are filled one at a time in
`meta.json`'s order, each drawn at temperature 1 from the model's
distribution given what has been filled; batches of 8,192 records on one
GPU; decoded with part A's `tokenizer.decode` (random generator seeded with
the same seed). Output `$S/model/synthetic.parquet`, 166,156 records (as
many as `values_train.parquet`), seed 20261009; the settings are in
`$S/model/synthetic_info.json` and in the parquet file's metadata, and the
generated codes in `$S/model/synthetic_tokens.npy`.

## Problems and notes

- Slurm allows one `gpu_devel` job at a time per user; extra short jobs
  went to `gpu`.
- No records or values were dropped by part B.
