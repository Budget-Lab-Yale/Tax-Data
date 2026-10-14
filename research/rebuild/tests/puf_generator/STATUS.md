---
title: "Test 1 — status for resuming"
role: status
workstream: rebuild
status: current
updated: 2026-10-08
sot: research/rebuild/tests/puf_generator/SPEC.md
supersedes: []
superseded_by: null
---

# Test 1 status (2026-10-08, about 20:57)

Written because the Claude session driving this test runs inside an
interactive Slurm job (28777639) that hits its 4-hour limit at about
21:23. The subagents for parts B and C die with it. Resume from the files.

Scratch folder: `/nfs/roberts/scratch/pi_nrs36/jar335/puf_generator/` (`$S`).

## Part A — data preparation: finished

Outputs final in `$S/data/` (marker `DONE`). Report: `DATA_REPORT.md`.

## Part B — model: training running

- Code done: `model.py`, `train.py`, `generate.py`, `train.sbatch`,
  `generate.sbatch`. Toy checks and a 4-epoch smoke run on real data passed
  (8.6M parameters, about 7 s per epoch, validation loss 2.78 → 1.54;
  generating 20,000 records took 6 s). Smoke output: `$S/dev/smoke/`.
- Three training jobs started 20:52 on partition `gpu`: 28795742,
  28795743, 28795744 (presumably three model sizes; check the `args` in each
  log). Logs: `$S/logs/train_<jobid>.out`.
- Still to do: pick the best run by validation loss, run `generate.sbatch`
  to write `$S/model/synthetic.parquet` (as many records as train), create
  `$S/model/DONE`, write `MODEL_REPORT.md`.

## Part C — evaluation: running when this was written

- Code: `evaluate.py`, `evaluate.sbatch`. Dev runs on its own split in
  `$S/eval/dev/`. A run (job 28795422, partition `day`) was in progress,
  probably the `baseline` run (floor and independent) on part A's files.
- Still to do: confirm the baseline run finished; run with
  `--synthetic $S/model/synthetic.parquet --label model`; write
  `EVAL_REPORT.md`.

## After that

An independent review of the results, then a summary for John. The spec is
`SPEC.md`; the decision context is D22 in
`research/rebuild/design_decisions.md`.

## Part C — evaluation: status written by part C at 21:01

Finished:
- Dev runs on C's own temporary split (`$S/eval/dev/`): jobs 28795421
  (label `dev_val`), 28795868 (`dev_copy`, a sample of training records, to
  check the copying measures flag copies; they do). Job 28795422 was an
  earlier `dev_copy` run, superseded.
- Preview on part A's final files: job 28795883, label `baseline_preview`,
  output `$S/eval/baseline_preview/` (floor and independent; about 2 minutes).

Finished at 21:03 (was running at 21:01):
- Job 28796536, label `baseline`: floor, independent and the new
  `floor_binned` reference (training records passed through the tokenizer)
  on part A's final files. Output `$S/eval/baseline/`; it also fills the
  generated tables in `EVAL_REPORT.md` (`--update-report`). Log:
  `$S/logs/eval_28796536.out` / `.err`.

Remaining:
- (Done: 28796536 finished in 155 s; EVAL_REPORT.md tables and summary
  prose filled for the references.)
- When `$S/model/DONE` exists, evaluate the model (references come from the
  cache, a few minutes):
  ```
  CODE=/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data/research/rebuild/tests/puf_generator
  S=/nfs/roberts/scratch/pi_nrs36/jar335/puf_generator
  cd $S && sbatch $CODE/evaluate.sbatch --synthetic $S/model/synthetic.parquet \
      --label model --update-report $CODE/EVAL_REPORT.md
  ```
  Outputs in `$S/eval/model/`. Then revise the Summary prose in
  `EVAL_REPORT.md` to describe the model's column (the tables update
  themselves).

## Part B (model) — status at 21:10, 8 October

Code is finished and was checked on fabricated data (`toy_check.py`) and in
a short trial on part A's final files (job 28795427). Three training runs
are in progress on the `gpu` partition (RTX 5000 Ada). All use the same
settings apart from size: any-order masking as in SPEC.md, batch 512,
AdamW learning rate 5e-4 with 1,000 warm-up steps and cosine decay over at
most 300 epochs, dropout 0.1, weight decay 0.01, a moving average of the
weights (decay 0.999) used for validation and saved, stopping after 15
epochs without a lower validation loss, seed 20261008.

| Job | Tag | Size | Parameters | Time per epoch |
|---|---|---|---|---|
| 28795742 | `small` | 4 layers, width 128, 4 heads | 2.7 million | about 5 s |
| 28795743 | `base` | 6 layers, width 256, 8 heads | 8.6 million | about 13 s |
| 28795744 | `large` | 8 layers, width 512, 8 heads | 33.0 million | about 37 s (up to about 3 hours) |

Each run writes to `$S/model/`: `checkpoint_<tag>.pt` (lowest validation
loss), `train_log_<tag>.csv` (loss per epoch), and `run_<tag>.json`
(settings, timings, final losses) when it ends. Slurm logs:
`$S/logs/train_<job>.out` / `.err`.

**Remaining steps are queued.** Job 28797194 (`finish.sbatch`) starts when
all three training jobs have ended. It picks the run with the lowest
validation loss from the `run_*.json` files, copies its log to
`$S/model/train_log.csv`, writes `$S/model/best_run.txt`, plots
`$S/model/training_curves.png`, generates `$S/model/synthetic.parquet`
(166,156 records, seed 20261009) and creates `$S/model/DONE`. Log:
`$S/logs/finish_28797194.out` / `.err`. If it fails, the manual equivalent
is:

```
CODE=/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data/research/rebuild/tests/puf_generator
S=/nfs/roberts/scratch/pi_nrs36/jar335/puf_generator
grep best_val_loss $S/model/run_*.json          # lowest value wins; call its tag BEST
sbatch $CODE/generate.sbatch --checkpoint $S/model/checkpoint_BEST.pt --seed 20261009
# after checking $S/logs/generate_<job>.out for errors:
touch $S/model/DONE
```

Still to write: `MODEL_REPORT.md` (architecture, curves, sizes and losses,
run times, generation time). Not yet written if this session ended first.

Part B update 21:09: `small` finished (job 28795742): best validation loss
1.3653 at epoch 125, stopped at 140, 12 minutes. `base` was already at
1.3624 by epoch 53, so `base` or `large` will be chosen. `MODEL_REPORT.md`
is drafted; its "pending" rows need filling from `$S/model/run_*.json` and
`$S/model/synthetic_info.json` once job 28797194 has finished.

## Chained jobs (added by the main session at 21:12)

The whole remaining pipeline is queued as Slurm jobs and needs no session:

- Training: 28795742 (small, finished: best validation loss 1.3653 at
  epoch 125), 28795743 (base), 28795744 (large), running.
- 28797194 `finish.sbatch` (part B's finishing step; runs after both
  training jobs end): picks the run with the lowest validation loss, plots
  the curves, writes `$S/model/synthetic.parquet` and `$S/model/DONE`.
- 28797508 `evaluate.sbatch --synthetic $S/model/synthetic.parquet --label
  model --update-report EVAL_REPORT.md` (runs only if 28797194 succeeds).
  Outputs `$S/eval/model/`; fills the tables in `EVAL_REPORT.md`.

Left for the next session: check each log (`$S/logs/finish_28797194.*`,
`$S/logs/eval_28797508.*`), write `MODEL_REPORT.md` if part B did not,
revise the summary prose in `EVAL_REPORT.md`, run an independent review,
and summarise for John.

## Noise, order and training-set checks (queued 2026-10-08, about 22:05)

- `large` finished: best validation loss 1.36119 (base 1.36188, small
  1.36526). `finish.sbatch` chose it; `$S/model/synthetic.parquet` is the
  large model, seed 20261009. Its evaluation is job 28797508 (label `model`).
- Seed 20261009 files for small and base: `synthetic_small.parquet`,
  `synthetic_base.parquet` (evaluated as `model_small`, `model_base`).
- Order variants of base (seed 20261009): `synthetic_base_demoglast.parquet`
  (demographics generated last), `synthetic_base_gibbs2.parquet` (two passes
  redrawing each variable given all others). `generate.py` gained `--order`
  and `--gibbs-sweeps`; `model.py` gained `gibbs_refine`.
- `evaluate.py` gained `--reference <file>` (compare against a file other
  than the test records). New scripts: `gen_seeds.sbatch`, `eval_list.sbatch`.
- Jobs: generation with seeds 20261010–12, 28800494 (small), 28800495
  (base), 28800496 (large); then evaluation of all 14 files against the test
  records (28800502, labels `<model>_seedNN`, `base_demoglast`,
  `base_gibbs2`) and against the training records (28800503, same labels +
  `_vstrain`); seed-to-seed noise: seed 10 against seed 09 for each model
  (28800499–501, labels `<model>_s0_vs_s1`).

## Overnight diagnostics (8 October, 22:20–23:45) — finished

All jobs finished; no agents or jobs running. Results and a summary at the
top of `EXPERIMENTS.md`. New options: `train.py` `--train-order`,
`--drop-token`, `--loss-weight-power`, `--val-mask-mode` (defaults keep the
old behaviour); scripts `diag2_*`, `exp_*`. Outputs under `$S/diag2/` and
`$S/exp/`. Next decision for John: whether to move to test 2 (joint
imputation, PUF plus SCF), given that the model's high-information
predictions are near calibrated and the problems are in generating whole
records from nothing.
