---
title: "Test 1 — can a transformer learn the 2015 PUF? Shared specification"
role: method
workstream: rebuild
status: current
updated: 2026-10-08
sot: research/rebuild/design_decisions.md
supersedes: []
superseded_by: null
---

# Test 1: can a transformer learn the 2015 PUF?

Proof of concept for D22 in `research/rebuild/design_decisions.md`. Read
D22 and `research/rebuild/rlhf_analogy.md` for context. This file is the
contract between three pieces of work done in parallel: data preparation
(A), the model (B) and the evaluation (C). Each piece reads and writes only
the files named here.

## The question

Can one masked transformer model, trained on about 166,000 PUF records,
generate records whose joint distribution is close to the PUF's own? "Close"
is judged against a floor: the gap between two sets of real PUF records
(training versus test), which is sampling noise only. The model is not
expected to beat the floor. The test measures how far above it the model
sits, and where.

## Rules for everyone

- **Compute.** Every Python run goes through Slurm (`sbatch` or `srun`),
  never the login node, including quick checks. CPU work: partition `day`
  (or `devel` for a few minutes). GPU work: partition `gpu` with
  `--gres=gpu:1` (L40S, A40 or A5000), or `gpu_devel` for short debugging.
- **Environment.** PyTorch 2.9.1, scikit-learn 1.6.1, pandas, pyarrow,
  matplotlib, all from modules:
  ```
  module load PyTorch/2.9.1-foss-2024a-CUDA-12.8.0 scikit-learn/1.6.1-gfbf-2024a \
              Arrow/17.0.0-gfbf-2024a matplotlib/3.9.2-gfbf-2024a
  ```
  Do not pip-install anything.
- **The PUF is licensed data.** It and anything derived from it, including
  generated records, stay on this cluster, under the scratch directory
  below. Never write PUF values into the git repository, never print more
  than a handful of individual records in logs, and never send any of it to
  an outside service.
- **Paths.**
  - Code: `research/rebuild/tests/puf_generator/` in the Tax-Data repo
    (`/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data`).
  - Data and outputs: `/nfs/roberts/scratch/pi_nrs36/jar335/puf_generator/`
    (called `$S` below). Subfolders: `data/`, `model/`, `eval/`, `logs/`.
  - Slurm logs go to `$S/logs/`, not the repo root.
- **Raw inputs** (read only):
  `/nfs/roberts/project/pi_nrs36/shared/raw_data/IRS-PUF/v1/2023091310/historical/`
  `puf_2015.csv` (207,696 records, 222 columns) and `demographics_2015.csv`
  (119,675 records; joins on `RECID`). Booklets (PDF) are in the same folder.
- **Seeds.** Every random step takes an explicit seed, recorded in outputs.
- **Writing.** Any document or report is for a public finance economist who
  knows taxes well but is not a machine-learning specialist. Plain English,
  full sentences, define any machine-learning term in one sentence when first
  used, no invented shorthand, no dramatic phrasing. Describe; do not sell.
- **No silent dropping.** Do not drop records or values without stating the
  count and reason in your output.

## Variables (about 40)

Proof of concept: the important variables only.

| Kind | PUF columns |
|---|---|
| Weight | `S006` (weight × 100; the record weight is `S006 / 100`) |
| Categorical | `MARS`, `DSI`, `XTOT`, `EIC`, `N24`, `FDED`; from the demographic file `AGERANGE`, `GENDER`, `EARNSPLIT` (records absent from the demographic file get the category `missing`) |
| Amounts | `E00100` (AGI), `E00200`, `E00300`, `E00400`, `E00600`, `E00650`, `E00900`, `E01100`, `P22250`, `P23250`, `E01400`, `E01500`, `E01700`, `E02100`, `E02300`, `E02400`, `E25850`, `E25860`, `E26270`, `E26390`, `E26400`, `E03150`, `E03210`, `E03270`, `E17500`, `E18400`, `E18500`, `E19200`, `E19800`, `E20100` |

Meanings are in the PUF booklet and in `config/variable_guide/baseline.csv`.
AGI (`E00100`) is generated like any other variable in this test; the
evaluation checks its consistency with the components.

Records with `RECID >= 999996` are the PUF's aggregate top records; set them
aside (count them in the data report).

## A. Data preparation — outputs in `$S/data/`

1. `meta.json`: the variable list, in a fixed default order, each with
   - `name`, `kind` (`weight`, `categorical`, `amount`);
   - `vocab_size` (number of tokens for this variable; tokens are integers
     `0 .. vocab_size-1`, local to the variable);
   - for categoricals: the value of each token;
   - for amounts and the weight: each token's `lo`, `hi`, and whether it is
     an exact single value (`point: true`) — see binning below;
   - `order`: a sensible generation order (weight, filing status and other
     categoricals, AGI, large incomes, small incomes, adjustments,
     deductions).
   Also the seeds, the split sizes and the counts set aside.
2. `tokens_train.npy`, `tokens_val.npy`, `tokens_test.npy`: integer arrays
   of shape `[records, variables]`, columns in `meta.json` order.
3. `values_train.parquet`, `values_val.parquet`, `values_test.parquet`:
   the original values, columns `RECID`, `weight` (= `S006/100`), then every
   variable by its PUF name (categoricals as their original codes, with
   `missing` coded as -1). Same row order as the token arrays.
4. `tokenizer.py` (in the code folder, importable): `encode(df, meta)` and
   `decode(tokens, meta, rng)` → DataFrame in the `values_*.parquet` format
   (without `RECID`; `weight` decoded from the weight token).
5. `DATA_REPORT.md` (in the code folder): counts, set-asides, bins per
   variable, and the round-trip check below.

**Binning.**
- Each amount variable gets a token for exact zero; bins for negative values
  if any occur; bins for positive values.
- Bins are set at quantiles of the training records' nonzero values,
  unweighted: up to 256 positive and up to 64 negative bins, with at least
  about 20 training records per bin (rare variables get fewer bins).
- Any single value that holds a large share of a variable's records (more
  than one bin's worth, for example a statutory cap) gets its own point
  token, decoded exactly.
- Decoding draws uniformly within a bin on a log scale for amounts
  (uniform on the original scale where a bin spans zero or is very narrow).
- The weight: if `S006` has few distinct values (256 or fewer), each value
  is a point token; otherwise bin log weight into up to 128 quantile bins.
- **Round-trip check:** `decode(encode(train))` versus train — weighted
  quantiles (10th to 99.9th), shares at zero and weighted totals for every
  variable. This is the error from binning alone and must be small.

**Split.** 80% train, 10% validation, 10% test, stratified on weight band ×
AGI band so the top is represented in each part (state the strata). Seed 20261008.

## B. The model — outputs in `$S/model/`

- **Architecture.** A transformer encoder with no position encoding. Each
  variable is one input slot; its input is the sum of a learned embedding
  for the variable's identity and a learned embedding for its token (or a
  learned `MASK` embedding when hidden). For amount and weight tokens, also
  add a learned function of the bin's position in the variable's order, so
  the model knows neighbouring bins are close. One output head per variable,
  giving probabilities over that variable's tokens. Start at about 6 layers,
  width 256, 8 attention heads; try two or three sizes if time allows.
- **Training.** Each record, each step: draw a hiding rate r uniformly from
  (0, 1), hide each variable with probability r (at least one hidden), and
  minimise the cross-entropy of the hidden variables given the visible ones.
  Records are unweighted (the weight is itself a variable). Stop on
  validation loss; record the training curve.
- **Generation.** Start with all variables hidden; fill them one at a time
  in `meta.json`'s `order`, each drawn from the model's distribution given
  everything filled so far, at temperature 1. Decode with `tokenizer.py`.
- **Outputs.**
  - `$S/model/checkpoint_*.pt`, `$S/model/train_log.csv`;
  - `$S/model/synthetic.parquet`: exactly as many generated records as
    `values_train.parquet`, in the same format (without `RECID`), seed
    recorded;
  - `MODEL_REPORT.md` in the code folder: architecture, settings, training
    curve, run times, GPU used.
- Code: `model.py`, `train.py`, `generate.py`, Slurm scripts `train.sbatch`,
  `generate.sbatch`.

## C. The evaluation — outputs in `$S/eval/`

`evaluate.py --synthetic <file> [--label <name>]` compares a candidate file
with `values_test.parquet`. Every measure is computed for three candidates:

1. **Floor:** `values_train.parquet` (real records; sampling noise only).
2. **Model:** the generated file.
3. **Independent:** each variable drawn independently from its own weighted
   distribution in train (all relationships between variables lost).

Each file's weights are normalised to sum to one for distribution
comparisons; also report each file's total weight against train's.

Measures:

1. **Single variables.** Share at zero, share negative, weighted quantiles
   (10, 25, 50, 75, 90, 99, 99.9), weighted mean.
2. **Two- and three-variable tables.** Each amount variable put in bins
   (negative, zero, deciles of positive values among test records);
   categoricals as they are. For every pair of variables, the total
   variation distance between the weighted two-way tables (total variation
   distance: half the sum of absolute differences in cell shares; 0 means
   identical, 1 means no overlap). Report the distribution across pairs and
   the worst pairs. Three-way tables for a short list of triples chosen to
   matter for tax (AGI band × filing status × has capital gains; AGI band ×
   itemizes (`FDED`) × age range; wage decile × dividend decile × filing
   status; and a few more).
3. **Which incomes appear together.** Frequency of each pattern of nonzero
   values across the 15 most common income variables; total variation
   distance.
4. **Conditional profiles.** Share nonzero and weighted mean of each amount
   by AGI percentile group (deciles, plus 90–99, 99–99.9, top 0.1%). Plots.
5. **The top.** Shares of AGI and of each income type held above the 99th
   and 99.9th AGI percentiles.
6. **Classifier test.** A gradient-boosted tree classifier (scikit-learn
   `HistGradientBoostingClassifier`) trained to tell candidate records from
   test records, on balanced subsamples, with all variables including the
   weight as features; held-out accuracy and AUC (AUC: the chance the
   classifier ranks a random candidate record above a random test record as
   "candidate"; 0.5 means indistinguishable). Report which variables the
   classifier relies on (permutation importance).
7. **AGI consistency.** An approximate AGI from components (state the
   formula) versus `E00100`, for real and generated records: the
   distribution of the gap should look alike.
8. **Copying.** On rank-transformed variables, each candidate record's
   distance to its nearest training record, compared with each test
   record's distance to its nearest training record; also the share of
   candidate records identical to a training record. Floor: test records.

Output `$S/eval/<label>/` with `summary.json`, tables (CSV) and plots, plus
`EVAL_REPORT.md` in the code folder written for the economist reader: a
one-screen summary, then the detail.

C can develop before A and B finish by making its own temporary split from
the raw files; it must run on A's files in the end.
