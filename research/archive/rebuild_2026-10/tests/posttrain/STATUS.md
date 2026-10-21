---
title: "Test 3 — status for resuming"
role: status
workstream: rebuild
status: current
updated: 2026-10-09
sot: research/rebuild/tests/posttrain/SPEC.md
supersedes: []
superseded_by: null
---

# Test 3 status

Contract: `SPEC.md`. Scratch: `/nfs/roberts/scratch/pi_nrs36/jar335/posttrain/` (`$P`).

## 9 October, about 19:55 (main session)

Five Opus subagents started: A (shared data, targets, reference draws),
B (method 1, reweighting draws), C (method 2, probability tilt), D (method 3,
network tuning), E (evaluation). The driving session's Slurm job
(28896349) ends at about 23:30. Each piece appends its own notes below.

## 9 October, 20:10 (piece A): records, targets and cells landed

`$P/a/records.parquet`, `$P/a/targets.parquet`, `$P/a/cells.json` and
`DONE_common` are written (job 28898289, log `$P/logs/a_cpu_28898289.out`).
The K = 4 development draws (`dev_draws.npz`, `DONE_dev`) are queued as GPU
job 28898290. Code: `a_common.py` (import it; do not copy the mapping) and
`a_build.py` (the stages that wrote the files).

What is in them:

- 42,057 records (20,595 in 2013, 21,462 in 2016): expB training rows from
  the SCF with wealth observed; 5,387 split-off rows excluded. No missing
  values found in weights, incomes, true wealth tokens or dollars. Total
  weight 82.2 million (2013) and 86.2 million (2016).
- 30,624 target cells = 2 waves x 48 groups (over the 11 families) x 319
  (variable, coarse bin) pairs. 3,034 cells have a true total of zero (96 of
  them because the 2016 wave has no negative values for a variable, so no
  draw can be negative there). Of the 27,590 positive cells, 1,230 contain
  fewer than 5 records and 6,581 contain fewer than 5 distinct households
  (each household appears as five implicates); 2,205 contain a single
  household.
- Checks passed: targets computed a second way (pandas group-by) agree to
  4e-7 of a weight unit; within every (wave, family, variable) the cells sum
  to the wave's total weight (relative difference 5e-15); the contribution
  matrix applied to the true tokens with the true weights reproduces the
  targets exactly; each draw touches exactly 264 distinct cells.

How to use (`import a_common as A`; see its docstring for details):

- `A.load_records()`, `A.load_targets()`, `A.load_draws("dev"|"ref"|"ref_b"|"ref_k128")`
  (dict with `tokens` [N, K, 24] int16, `logp_ref` [N, K], `logp_ref_var`
  [N, K, 24], `seed`). Records order is `rec` = 0..N-1 everywhere; wealth
  variables are always in `c_common.WEALTH` order.
- `A.contrib_index(tokens, recs)` -> int32 [N*K, 264] cell ids per draw (rows
  record-major, row = i*K + k). `A.contrib(tokens, recs)` -> scipy CSR
  [N*K, 30624]. `A.contrib_torch(idx, device)` -> torch sparse CSR.
  `A.cell_totals_torch(idx_t, w_t)` = A'w and `A.draw_scores_torch(idx_t,
  lam_t)` = A lambda on the GPU without building a matrix; `A.cell_totals`
  is the numpy version.
- `model, ctx = A.load_reference("cuda")`: the reference network and every
  record's inputs. `A.sample(model, ctx, rec_idx, gen, bias_fn=None,
  step_hook=None)` -> (tokens [n, 24], float32 log-probabilities [n, 24]);
  `A.draw_set(model, ctx, K, seed)` -> a draws-file dict;
  `A.ref_logprob(model, ctx, rec_idx, tokens, bias_fn=None)` -> float32
  [n, 24]. `bias_fn(k, rec_idx_batch, logits)` returns adjusted logits for
  variable WEALTH[k] (method 2's tilt); `step_hook(k, rec_idx_batch, s0,
  logp)` sees each step's full log-probabilities (for expected cells).
  Both use the allowed-code restriction and float32 throughout.
- `A.decode_dollars(tokens, recs, seed)` -> float64 dollars, same shape;
  `A.nw_from(dollars)` -> net worth (assets minus debts).
- Cell id = block * 319 + vb_start[variable] + bin; `cells.json` has the
  block table, group and bin labels.

## 9 October, about 20:10 (piece D)

Code written: `d_tune.py` (training, fresh draws, evaluation), `d_direct.py`
(variant 2's objective), `d_report.py` (report.json and curves), `d_dev.py`
(development data), `d_gpu.sbatch` / `d_gpu_sh.sbatch`. A development trial
on 3,000 records with their own targets (`$P/dev/d/`) is queued as job
28898354 (`d_dev_run.sh`). The real runs wait for `$P/a/DONE_*` and
`$P/b/DONE_ref`.

## 9 October, 20:03 (piece A): development draws landed

`$P/a/dev_draws.npz` (K = 4, seed 20261101) and `DONE_dev` are written (job
28898290 on a B200, 39 seconds for the draws; checks in
`$P/a/dev_checks.json`). The full reference draws are queued as job
28898348 (`$P/logs/a_gpu_28898348.out`): `ref_draws.npz` and
`ref_dollars.parquet` (K = 32, seed 20261102, dollars seed 20261112,
`DONE_ref`), then `ref_draws_b.npz` and `ref_dollars_b.parquet` (a second
independent K = 32 set, seed 20261103, dollars seed 20261113, `DONE_ref_b`),
then `ref_draws_k128.npz` (K = 128, seed 20261104, `DONE_ref_k128`).

Checks on the reference log-probabilities:

- The fixed order is the 24 wealth variables in meta.json's generation order
  (cash, other_nonfin, primary_home, ..., trusts); the survey weight was
  dropped in training and is never drawn.
- `A.ref_logprob` on the true tokens of 512 records equals test 2's
  `completion_nll` run in float32 exactly, variable by variable (largest
  difference 0). Test 2 scored in bf16; that differs from float32 by up to
  0.033 nats on one variable and 0.012 nats per record on average, which is
  why every log-probability here is float32.
- The allowed-code restriction (no negative home value or pass-through value
  in 2016) changes a record's log-probability by 4e-6 nats on average.
- The log-probability of a sampled draw recorded while sampling equals
  `A.ref_logprob` of the same tokens exactly. No draw has probability zero.
- Mean log-probability of the true wealth tokens under the reference is
  -32.9 nats per record (24 variables).
- In the K = 4 draws, 0.04% of draw weight falls in zero cells (cells with
  no true record), which B will drop.

## 9 October, 20:03 (E, evaluation)

`e_evaluate.py` (statistics of any draws file, with optional per-draw
weights, written to `$P/e/<label>/stats.json`) and `e_compare.py` (side-by-side
tables into `$P/e/compare.md` and the generated block of `EVAL_REPORT.md`)
are written and run end to end on fabricated draws in `$P/dev/e/`. Contract
change E 20:05 in `SPEC.md`: methods 2 and 3 save their residual-calibration
weights as `weights_resid.npz` next to their draws. Next: run on A's
development draws when they land, then start the watcher (`e_watch.sh`).

## 9 October, 20:25 (main session)

John asked for from-scratch arms. A sixth Opus subagent, piece F, builds
arm S0 (keep each record's demographics, draw incomes and wealth) and arm
S1 (draw whole records), with targets on incomes and demographics added.
Contract: "Contract changes", main session 20:25, in `SPEC.md`. D7 and D22 in
`design_decisions.md` updated to record the direction.

## 9 October, about 20:15 (piece C)

Code written: `c_tilt.py` (the tilted model and the fitting), `c_fit.py`
(fit on the real targets, fresh draws, checks), `c_post.py` (residual
calibration with B's `calibrate()`, comparison with B's multipliers, DONE
marker), `c_dev_toy.py` (fabricated test), `c_gpu.sbatch`, `c_py.sbatch`.
Design written in `C_REPORT.md`. The multipliers for a variable only affect
that variable's step, so they are fitted one variable at a time in the fill
order in one pass (exact Newton solve per variable on the step
probabilities), then a fresh K = 32 set is drawn.

- Fabricated test passed (`$P/dev/c/toy_report.json`): fit exact on its
  own draws, error under exact enumeration shrinks with the square root of
  the draws, known tilt recovered, KL estimates match the exact KL.
- Development run on 3,000 records (job 28899071, `$P/dev/c/`) passed end to
  end; float32 log-probabilities match A's `ref_logprob` exactly.
- Full fit queued: job 28899409 (`$P/logs/c_gpu_28899409.out`), writes
  `$P/c/lambda.npz`, `draws.npz`, `report.json`. Then, after B's
  `DONE_ref`: `sbatch c_gpu.sbatch c_post.py` (residual calibration,
  comparison with B, writes `$P/c/weights_resid.npz` and `$P/c/DONE`).

## 9 October, 20:10 (piece A): full reference draws landed

`$P/a/ref_draws.npz` (K = 32 per record, seed 20261102; arrays `tokens`
[42057, 32, 24] int16, `logp_ref` [42057, 32], `logp_ref_var`
[42057, 32, 24], `seed`, `K`, `rec`, `wealth`) and `$P/a/ref_dollars.parquet`
(columns `rec`, `draw`, the 24 components in dollars, `nw`; decoded with seed
20261112) are written, with `DONE_ref` (job 28898348, 313 seconds for the
draws on a B200). The same job continues with the second K = 32 set and the
K = 128 set (about 25 more minutes).

## 9 October, 20:15 (piece F): from-scratch arms started

Code: `f_common.py` (F's sampler, which draws incomes and demographics as
well as wealth; F's cell layout; income-group thresholds; targets),
`f_run.py` (self-test, checks, draws, calibration with B's
`calibrate_matrix`), `f_eval.py` (untargeted measures through E's
`frame_stats`), `f_gpu.sbatch` / `f_cpu.sbatch`. Outputs in `$P/f/`.

- Targets: 38,812 cells for S0 (A's 30,624 wealth cells plus 8,188 cells on
  the seven incomes) and 38,910 for S1 (plus 96 age x married x dependents
  cells and the two wave totals). The wealth cells' targets equal A's
  exactly. Income-group thresholds in dollars (2013: 40,001; 136,000;
  600,100; 2,267,000. 2016: 42,600; 150,250; 701,410; 3,475,250)
  reproduce A's income groups on every record.
- Sampler check (job 28899424, RTX 5000 GPU): F's scorer gives A's
  log-probabilities of A's development draws to within 5e-6 nats. Redrawing
  with A's seed did not reproduce A's tokens on that GPU (A drew on a B200;
  the random stream depends on the device); a B200 rerun is job 28899772.
- Running: S0 draws (job 28899425), S1 draws at 8x (28899426), 24x more
  (28899770), 32x more (28899771); calibrations queued after them (28899723
  S0, 28899724 S1 at 8x, 1x, 4x).

## 9 October, 20:22 (piece A): second reference set landed; summary of the reference

`$P/a/ref_draws_b.npz` and `$P/a/ref_dollars_b.parquet` (a second K = 32
set, independent of the first: draw seed 20261103, dollars seed 20261113)
and `DONE_ref_b` are written by job 28898348, for E's Monte Carlo yardstick.
The K = 128 set (`ref_draws_k128.npz`, seed 20261104) is still running in
the same job, about 21 minutes, due around 20:40.

`a_check_ref.py` (CPU job 28899435, output `$P/a/ref_checks.json`)
summarizes the reference draws before any calibration:

- Mean net worth per tax unit is 7 to 8 percent above the truth: $569,000
  against $530,000 in 2013 and $737,000 against $683,000 in 2016 (truth as
  reported; the truth decoded from its own tokens is within 0.4 percent of
  the reported figure).
- 0.04 percent of draw weight lands in zero cells.
- Of the 27,590 positive cells, 10 are reached by no draw among the 32 per
  record and 38 by fewer than 5 draws (76 and 429 with K = 4).
- Target error before calibration, as a share of the group's total weight,
  averaged over a family's cells: 0.3 percent (married) to 1.3 percent
  (rental income groups); the largest single cell is off by 26 percent of
  its group's weight (an "other income" group).

## 9 October, 20:30 (main session): PUF arms are now the priority

John: the point is completing PUF records. New contract entry "Main
session, 9 October 20:30" in `SPEC.md`: arm P (complete PUF training
records' wealth, SCF tables interpolated to 2015 as shares within PUF
groups; target sets P_full and P_core) and F's PS0/PS1 (PUF demographics or
nothing kept; PUF income tables plus SCF wealth tables). SCF arms finish
what is running and remain the truth-known yardstick.

## 9 October, 20:45 (main session): reorientation to the full model

John's plan: one transformer over test 1's 40 PUF variables plus the 24 SCF
wealth variables; pretraining evaluation against PUF and SCF ground truth;
then post-training. New pieces G (data, `full` dataset) and H (training and
completions). A–F redirect their code to the full model; current-model
results are a pilot only. Contract: SPEC.md entry "Main session, 9 October
20:45".

## 9 October, 20:35 (E, evaluation): watcher running; PUF arm added

- Watcher: Slurm job 28901214 (`e_sh.sbatch e_watch.sh 03:00`, log
  `$P/logs/e_sh_28901214.out`). Every 10 minutes until 03:00 it runs
  `e_puf.py --auto` (arm P), `e_evaluate.py --auto` (SCF arm) and
  `e_compare.py --update-report`, which rewrites the generated block of
  `EVAL_REPORT.md` (PUF arm first, SCF arm second) and `$P/e/compare.md`.
  Statistics per column are in `$P/e/<label>/stats.json` (SCF arm) and
  `$P/e/puf/<label>/stats.json` (arm P).
- What it picks up (see `plan()` in each script): SCF arm: `$P/a/ref_draws.npz`
  (done: reference, and the Monte Carlo yardstick against
  `ref_draws_b.npz`), `$P/b/weights_ref.npz` once `$P/b/DONE_ref` exists,
  `$P/c/draws.npz` once `$P/c/DONE` exists (plus `weights_resid.npz`),
  `$P/d/<variant>/draws.npz` once `$P/d/DONE_<variant>` or
  `$P/d/<variant>/DONE` exists. Arm P: `$P/a_puf/ref_draws.npz` once a
  `DONE_*ref*` marker is there; `$P/b/puf_<core|full>/weights*.npz`;
  `$P/c/puf_<set>/draws.npz` with a `DONE` in that folder;
  `$P/d/puf_<set>/<variant>/draws.npz` with a `DONE` in that folder.
  Pieces whose file names differ: tell E (or edit `plan()`).
- Arm P is compared with the SCF 2013 and 2016 training records and their
  interpolation to 2015 (shares linearly, dollar levels in logs, as test
  2's `c_evaluate_puf.py`); the SCF's own 2013-to-2016 change is shown as a
  scale, and two halves of the PUF reference draws give the draw noise.
- To finish by hand after 03:00 (or if the watcher dies):
  `sbatch --mem=100G --time=02:00:00 e_sh.sbatch e_watch.sh once`. To force
  a column to be recomputed, delete its `stats.json`.

## 9 October, 20:45 (piece F): switched to the full model's PUF arms

- Following the 20:30 and 20:45 entries: the pending current-model PUF
  draws (jobs 28901240-2) were cancelled before they started; `f_puf.py`
  (PS0/PS1 on the current model, with targets built from the SCF shares)
  stays as pilot code only. The SCF-arm jobs already running are left to
  finish: S0 calibration 28899723, S1 calibration 28899724 (8x, 1x, 4x),
  S1 draws 28899771. Nothing new is added for them.
- New: `f_full.py`, the PS0/PS1 sampler and calibration with the dataset,
  checkpoint and tokenizer module as parameters. Contract entry "F, 9
  October 20:40" in SPEC.md says what the draws files hold and what F needs
  from A (`contrib_index_rows`, `load_targets_full` in the parameterised
  `a_common`). Code test on the current model: job 28901884 (log
  `$P/logs/f_gpu_28901884.out`, output `$P/dev/f_full/`).
- Chain queued (`f_chain.sh`): waiter 28901887 (until `$T2/data/full/DONE`
  and `$T2/model/full/DONE_seed01` exist) -> draws on the full model
  28901888 (PS0 part 1, 8 per record), 28901889 (PS1 part 1, 4x), 28901890
  (PS0 part 2), 28901891 (PS1 part 2); waiter 28901892 (until
  `$P/full/a/DONE_targets*`) -> calibrations 28901893 (PS0, K = 8, core and
  full), 28901894 (PS1, 4x), 28901895 (PS0, K = 16), 28901896 (PS1, 8x).
  Outputs `$P/full/f/<arm>/`: `draws_p<part>.npz`, `draws_p<part>_checks.json`
  (rescoring check, order, unobserved kept variables), `weights_<set>_<size>.npz`,
  `report_<set>_<size>.json`, marker `$P/full/f/DONE_<arm>`.
- By hand, if the chain breaks: `sbatch f_gpu.sbatch f_full.py draw ps0
  --part 1` (and `--part 2`; `draw ps1 --part 1` / `--part 2`), then
  `sbatch -p gpu_b200,gpu_h100,gpu_h200 f_gpu.sbatch f_full.py calib ps0 --k 8
  --set core full` and `calib ps1 --mult 4 --set core full`. If A's builder
  has another name or form, change `a_interface` and the two lines that call
  it in `f_full.calib`.

## 9 October, about 20:55 (piece C): pilot done, full-model chain queued

**Pilot on the current model (SCF arm), finished.** Job 28899409, outputs
`$P/c/lambda.npz`, `draws.npz`, `report.json`. All 24 variables' fits
converged (largest error on the fitting draws 7e-11 of a group's total).
KL of the tilted model from the reference: 1.31 nats per unit of weight.
Fresh draws from the tilted model miss the targets by 0.016 percent of the
group total on average, largest 1.5 percent. Details in `C_REPORT.md`.
Sensitivity: fitting on 8 draws per record gives KL 1.316 (job 28900779,
`$P/dev/c/kfit8/`); 64 draws per record is running (job 28900778,
`$P/dev/c/kfit64/`).

**Code now takes dataset, checkpoint and arm as parameters** (`c_fit.py`
and `c_post.py`: `--a-module --dataset --checkpoint --arm --a-dir`, passed to
A as `A.configure(...)` and environment variables; what C needs from A is in
SPEC.md, contract change "C, 20:40"). The tilt works on any set of drawn
variables. Variables A lists in `WEALTH` are tilted; drawn variables with
`step_var` -1 are drawn untilted. New handling for the PUF arms, tested on
the fabricated problem: records with no group in a family (unknown age)
are allowed; a record whose every bin is in a zero cell keeps its
reference bins (counted in the report); a variable whose exact solve does
not converge (targets that cannot all be met) is refitted with a small
ridge (`--auto-ridge`, default 1e-3: each cell's error as a share of its
group total is then 0.001 times its multiplier).
Not supported: PS1-style records whose groups depend on values drawn in
the same pass (groups are fixed per record); PS0 needs A's `_step_logits`
to switch the source tag between steps.

**Queued chains** (each waiter is a small CPU job, `c_chain.sh`, polling
every 2 minutes for up to 24 hours, then submitting one GPU job):

| Waiter | Waits for | Then runs |
|---|---|---|
| 28901918 | `$P/b/DONE_ref` | `c_post.py` on the pilot: residual calibration with B's `calibrate()`, comparison with B's multipliers, `$P/c/weights_resid.npz`, `$P/c/DONE` |
| 28901919 | `$T2/model/full/DONE_seed01` and `$P/full/a/DONE_targets*` | `c_fit.py --dataset full --arm P_core`, outputs `$P/full/c/P_core/` (6-hour limit) |
| 28901920 | `$P/full/c/P_core/FIT_DONE` | `c_post.py` for P_core (B's multipliers read from `$P/full/b/P_core/weights_ref.npz` if present) |
| 28901921, 28901922 | same, for P_full | same, `$P/full/c/P_full/` |

**By hand, if a chain breaks** (from this folder; `$T2` =
`/nfs/roberts/scratch/pi_nrs36/jar335/scf_joint`):

    C="--a-module a_common --dataset full --checkpoint $T2/model/full/checkpoint_full_w1_seed01.pt --arm P_core --a-dir $P/full/a"
    sbatch -p gpu_b200,gpu_h200 --time=06:00:00 c_gpu.sbatch c_fit.py $C --no-calibrate --outdir $P/full/c/P_core
    sbatch -p gpu_b200,gpu_h200 --time=02:00:00 c_gpu.sbatch c_post.py $C --dir $P/full/c/P_core --b-weights $P/full/b/P_core/weights_ref.npz

and the same with `P_full`. If A names the arms or markers differently,
change `--arm` and the waiter's marker (`scancel` the waiter, resubmit
`sbatch c_chain.sh "<markers>" <sbatch arguments>`). If B's weights land
after `c_post.py` ran, rerun it with `--skip-calibration` to add the
comparison with B. Pilot comparison with B by hand: `sbatch
c_gpu.sbatch c_post.py` once `$P/b/weights_ref.npz` exists.

## 9 October, 20:55 (piece A): K = 128 set landed; a_common.py generalized

`$P/a/ref_draws_k128.npz` (K = 128, seed 20261104) and `DONE_ref_k128` are
written (job 28898348, about 21 minutes of drawing). All SCF-arm files from
piece A are now complete.

Following the 20:45 reorientation, `a_common.py` now takes the dataset,
checkpoint, output folder and tokenizer as parameters: `A.configure(data,
ckpt, out, tokenizer)` or `A.configure_full(ckpt=None)` (dataset `full`,
`$T2/model/full/checkpoint_full_w1_seed01.pt`, `$P/full/a`, G's
`g_tokenizer.py`). The defaults are unchanged (expB, the current
checkpoint, `$P/a`, test 2's tokenizer), so existing SCF-arm code keeps
working. `A.load_reference(device, recs, fill=None, split="train",
as_source=None, decode_year=None)` now fills any chosen set of variables
(default the 24 wealth variables) and leaves every other unobserved slot
hidden and undrawn (a PUF record's missing age stays unobserved); the
survey weight and any other variable dropped in training is always hidden.
`A.decode_dollars(tokens, recs, seed, names=None, year=None)` decodes any
variables (pass `year=2015` for PUF records' wealth). A regression job
(28901917, `a_regress.py`) checks that the SCF-arm development draws are
reproduced exactly by the generalized code.

## 9 October, 20:50 (piece B): `calibrate()` interface is stable

Code: `b_calibrate.py` (GPU; run through `b_gpu.sbatch`). Two entry points,
both returning `(w, lam, report)`:

- `calibrate(draws_tokens, records, targets, contrib_fn=None, **opts)`:
  draws `[N, K, V]` in the records' order, A's records table (needs
  `weight`), A's targets table (one row per cell in cell order, column
  `target`; `wave`, `family`, `group`, `variable`, `bin` are used for the
  block check and the report). Uses A's `contrib_index` unless `contrib_fn`
  is given.
- `calibrate_matrix(A, W, t, K, cell_meta=None, draw_mask=None, lam0=None,
  ...)`: for anyone who builds the cell ids themselves. `A` is either an int
  array `[N*K, m]` of cell ids per draw (rows record-major, row = i*K + k;
  **-1 allowed** for "this draw is in no cell of that slot") or a scipy CSR
  matrix. `W` [N] record totals, `t` [C] targets. `draw_mask` [N, K] bool
  marks which slots are real draws (for records with unequal draw counts).
  For one weight per synthetic record with only a wave total (F's PS1):
  pass each wave as one "record" (N = number of waves, K = synthetic
  records per wave, padded with `draw_mask` if the waves differ),
  `W` = the wave totals; the result is then the minimum-KL reweighting of
  equal starting weights within each wave.
- Returns `w` [N, K] float64 (each row sums to its record's weight; zero for
  dropped draws), `lam` [C] (zero for cells not in the solve), `report`
  (JSON-ready; `report["cell_status"]` gives each cell's status: active,
  zero, zero_relaxed, unreachable, above_reach, block, below_forced, joint).
- Output file layout (`weights_*.npz`): `w`, `lambda`, `cell_id`,
  `cell_status`.

Method in brief: drop draws in zero cells; find cells that cannot be hit
(no draw reaches them, target outside what the reaching records can supply,
or a wave x family x group x variable block that fails an exact
supply-and-demand check); then solve for one multiplier per cell by
Newton's method with conjugate gradients on the GPU, along a path of
shrinking ridge terms so the steps stay well behaved; cells that still
cannot be hit together are released and listed as 'joint'. Details and
results go in `B_REPORT.md`.

Memory: the GPU holds the cell ids per draw (4 bytes x N x K x m, m = cells
per draw) twice over (ids and a pair index), plus work arrays of about the
same size in float64 per chunk. N = 160,000, K = 32, m = 264: about 11 GB;
m = 500: about 21 GB. Fits an 80 GB H100; K = 128 at m = 500 (about 80 GB)
would need an H200 or B200. The block check needs about 1 GB per 2,000
blocks x variables.

Jobs queued (SCF arm, pilot only, testing the code): 28901931 (K = 32,
writes `$P/b/weights_ref.npz`, `report_ref.json`, `DONE_ref`), 28901932
(K = 8 and 16, `report_ref_K8.json`, `report_ref_K16.json`), 28901933
(K = 64 and 128 from `ref_draws_k128.npz`). Logs `$P/logs/b_gpu_<job>.out`.

## 9 October, 20:42 (G, data): dataset `full`, first version, landed

- `$T2/data/full/` (`$T2` = `/nfs/roberts/scratch/pi_nrs36/jar335/scf_joint`),
  marker `DONE`. Test 2's layout: `meta.json`, `tokens_*.npy`, `truth_*.npy`
  (identical to tokens), `records_*.parquet`, `quantiles.parquet`,
  `roundtrip.csv`, `checks_*.csv`, `prep_summary.json`. Built by
  `g_prepare.py --units t2` (CPU job 28901940, log `$P/logs/g_cpu_28901940.out`).
- 63 model variables: test 1's 39 PUF variables (weight excluded) in test 1's
  order, then the 24 wealth variables in expB's order (`generation_order`).
  Groups: `shared` (11 PUF variables the SCF also measures), `puf_only` (28,
  always -1 for SCF records), `scf_only` (wealth). No weight variable: the
  record weight is the `weight` column of `records_*.parquet`.
- Rows: train 213,600 (PUF 166,156; SCF 47,444), val 27,477, test 34,183.
  PUF = all of test 1's records and split, dependent filers included. SCF =
  expB's filer rows and household split exactly (row counts per part checked
  against expB).
- `../scf_joint/b_train.py` reads it unchanged (smoke run, CPU job 28902236:
  data load, token checks, model build pass). Decoding: use `g_tokenizer.py`
  (copy of `a_tokenizer.py`; it also decodes SCF records' PUF-only variables
  with the PUF 2015 function, where `a_tokenizer` would raise).
- Crosswalk: `g_crosswalk.md`. Checks: `G_DATA_REPORT.md` (being written).
- Next: version 2 adds `N24`, `GENDER`, `E02400` (Social Security) and
  `E01700` (pensions) to what the SCF observes, from a rebuilt unit file
  (`g_build_units.R`, job 28902223). It will be written to `$T2/data/full_v2/`
  so that `full` does not change under piece H; whether to switch is the
  main session's call.

## 9 October, 20:46 (piece H, pretraining): training on `full` started; everything after it queued

- Code: `h_train.py` (test 2's `b_train.py` plus a validation loss per
  source and automatic dropping of weight-kind variables; the model,
  `../scf_joint/b_model.py`, is unchanged), `h_complete.py` (fills only a
  chosen set of variables, or generates whole records from nothing),
  `h_make_fake.py`, `h_report.py`; Slurm scripts `h_wait.sbatch`,
  `h_train.sbatch`, `h_complete.sbatch`, `h_launch.sh`.
- Checked (jobs 28901930, 28902492): on a fabricated 63-variable dataset in
  the `full` layout, training runs and all four completions run; with test
  2's expB checkpoint and the real tokenizer, decoding and the allowed-code
  tables work, and the SCF completion log-likelihood (34.6 nats per record)
  is in line with test 2's.
- G's `full` v1 landed at 20:42; the wait job 28902164 released it.
  Queued chain (ids also in `$P/logs/h_jobs.txt`): train seed 1 28902544
  (running) → completions seed 1 28902545; train seeds 2 and 3 28902546,
  28902547 (after seed 1) → their completions 28902548, 28902549 → summary
  28902550 (`h_report.py`, writes `$T2/model/full/h_summary.md` and
  `training_curves.png`). Logs: `$P/logs/h_<train|comp|cpu>_<jobid>.out`.
  The first training job copies the dataset to `$T2/model/full/data/`;
  training and completions read that copy.
- Also queued: the same chain for G's version 2 (`$T2/data/full_v2/`, when
  its `DONE` appears; wait job 28902587, then 28902588–28902594), writing to
  `$T2/model/full_v2/` with tag `full_v2_w1`. Whether version 2 replaces
  version 1 is the main session's call; to drop it,
  `scancel 28902587 28902588 28902589 28902590 28902591 28902592 28902593 28902594`.
- Outputs and columns: SPEC.md, contract entry "H, 9 October 20:46".
- **To finish by hand** if a job failed (`sacct -j <id>`), from this
  folder: `sbatch h_train.sbatch <seed>`, then
  `sbatch --dependency=afterok:<train id> h_complete.sbatch <seed>` (one
  completion only: `sbatch h_complete.sbatch <seed> scf_test`; names
  `gen_puf puf_wealth scf_train scf_test`), then
  `sbatch h_cpu.sbatch h_report.py`. For version 2 put
  `DATA=$T2/data/full_v2 OUT=$T2/model/full_v2 TAGBASE=full_v2_w1` in front
  of each `sbatch`, and `--model-dir $T2/model/full_v2 --tag full_v2_w1` after
  `h_report.py`.

## 9 October, 20:52 (G, data): `full_v2` marker withdrawn, rebuilding with Schedule E

- A first `full_v2` (16 PUF variables observed in the SCF) wrote its `DONE`
  at 20:50; the marker was removed about two minutes later, before H's wait
  job (28902587) released anything, because John chose (option b) to add
  total Schedule E (`E02000`, test 2's `rent`; SCF X5714) as a 40th PUF-side
  variable observed in both sources. The five separate Schedule E pieces
  stay PUF-only.
- Rebuilding `$T2/data/full_v2/` now (`g_prepare.py --units g`); `DONE` will
  be written after the log is checked. Version 2 then becomes the main
  dataset (coordinator, 9 October).

## 9 October, 20:54 (G, data): `full_v2` landed — the main dataset

- `$T2/data/full_v2/`, marker `DONE` (20:54), built by `g_prepare.py --units g`
  (CPU job 28903209, log `$P/logs/g_cpu_28903209.out`) from piece G's rebuilt
  SCF unit files (`$P/g/scf_units/`, `g_build_units.R`, R job 28902493).
- 64 model variables: 40 PUF-side (test 1's 39 plus total Schedule E,
  `E02000`, placed after `E00900`), then the 24 wealth variables. The SCF
  observes 17 PUF-side variables: version 1's 11 plus `N24`, `GENDER`,
  `E02400` (Social Security), `E01700` (pensions), `E02100` (farm) and
  `E02000` (John's option b). Same records, split and layout as `full`.
- Checks in `G_DATA_REPORT.md`, crosswalk and the Schedule E decision in
  `g_crosswalk.md`. Version 1 (`full`) is unchanged on disk.

## 9 October, about 21:00 (piece C): v2 chain queued

Following the 20:55 contract entry, method 2 is also queued for data
version 2 (dataset `full_v2`, checkpoint
`$T2/model/full_v2/checkpoint_full_v2_w1_seed01.pt`), alongside the v1
chain, which stays as the fallback:

| Waiter | Waits for | Then runs |
|---|---|---|
| 28903946 | `$T2/model/full_v2/DONE_seed01` and `$P/full_v2/a/DONE_targets*` | `c_fit.py --dataset full_v2 --arm P_core`, outputs `$P/full_v2/c/P_core/` |
| 28903947 | `$P/full_v2/c/P_core/FIT_DONE` | `c_post.py` (residual calibration; B's multipliers from `$P/full_v2/b/P_core/weights_ref.npz` if present) |
| 28903948, 28903949 | same, for P_full | outputs `$P/full_v2/c/P_full/` |

By hand: the commands in the 20:55 note with `full` replaced by `full_v2`
in `--dataset`, the checkpoint path, `--a-dir` and the output folders.

## 9 October, 21:00 (piece D): code tested; full-model chains queued (v1 and v2)

What changed. `d_tune.py` now takes the dataset (`--dataset expB|full|full_v2`),
the reference checkpoint (`--ref-checkpoint`), piece A's folder (`--a-dir`),
the arm (`--arm scf|puf`) and, for the full model, A's target set
(`--target-set wcore|wfull`, read through `a_full.py`). Arm P presents PUF
records with the SCF source tag, year 2015, allowed codes from the 2015
quantile functions; a missing age range stays unobserved and is never drawn
(A's `load_reference` leaves every non-drawn unobserved slot hidden). Cells
whose target is missing (NaN) are left out of every measure and of the
residual calibration, and their number is logged. Residual calibration is
now `d_tune.py resid` (B's `calibrate_matrix` on D's own cell ids), writing
`weights_resid.npz` (B's layout) and `report_resid.json`.

Code tests (pilot model, not results): SCF arm dev data (job 28899058),
fabricated PUF-arm data with the four core families (job 28902231,
`$P/dev/d_puf/`), and A's pilot full-layout target set (job 28903950,
`$P/dev/d_full/`). In the first two, both variants ran end to end (train,
32-or-fewer fresh draws, residual calibration, report); D's in-loop float32
reference log-probabilities match A's `ref_logprob` to 3e-6 or better.

Variant 2 needed a fix: with the expected cells refreshed every 25 steps
and learning rate 2e-4, the network overshot (the squared target error rose
to 37 times the reference's within 150 steps; jobs 28900074/76, cancelled).
Refreshing every 5 steps fixed it (job 28901195: error falling, KL small).

Chains (each a CPU waiter on partition `day`, 23-hour limit, that submits GPU
jobs when inputs appear; the script it runs is snapshotted to
`$P/logs/d_full_chain_<job>.sh`; submitted jobs are listed in
`$P/<ver>/d/chain_jobs.txt`):

| Waiter | Version | Waits for | Then submits |
|---|---|---|---|
| 28906176 | `full` (v1) | `$T2/model/full/DONE_seed01`, `$P/full/a/DONE_targets` | variant 2 on P_core (`wcore`, beta 0.1), then finish |
| (same) | | `$P/full/a/DONE_ref_P` and B's P_core weights (first of `$P/full/b/P_core/weights_ref.npz`, `$P/full/b/P/core/weights_ref.npz`, `$P/full/b/P/weights_core.npz`, ...; see `bweights()` in `d_full_chain.sh`) | variant 1 on P_core; later P_full if B's P_full weights appear |
| 28906177 | `full_v2` | the same under `$T2/model/full_v2/`, `$P/full_v2/a/`, `$P/full_v2/b/` | the same |

Each GPU pair is: training (`d_train.sh`, 80 minutes, under 1:50 limit) and,
after it succeeds, `d_finish.sh` (32 fresh draws per record, residual
calibration, report; 8-hour limit). GPU partitions gpu_b200, gpu_h200, gpu_h100 only (the `gpu` partition's RTX A5000s are too slow in float32). Outputs `$P/<ver>/d/<arm>/<variant>/`
(`checkpoint.pt`, `curve.csv`, `train.json`, `draws.npz`,
`weights_resid.npz`, `report_resid.json`, `report.json`, `curves.png`),
marker `$P/<ver>/d/DONE_<arm>_<variant>`.

By hand, if a waiter dies or B names its weights differently (VER = full or full_v2):

    cd research/rebuild/tests/posttrain
    C="--dataset $VER --arm puf --a-dir $P/$VER/a --target-set wcore"
    sbatch -p gpu_b200,gpu_h200,gpu_h100 --time=01:50:00 d_gpu_sh.sbatch d_train.sh \
        $P/$VER/d/P_core/distill --variant distill $C --draws $P/$VER/a/ref_draws_P.npz \
        --weights <B's P_core weights.npz> --lr 5e-5 --mix 0.2 --minutes 80 --eval-min 10 --eval-k 1
    sbatch -p gpu_b200,gpu_h200,gpu_h100 --time=08:00:00 --mem=200G --dependency=afterok:<job> \
        d_gpu_sh.sbatch d_finish.sh $P/$VER/d/P_core/distill $P/$VER/d/DONE_P_core_distill $C
    # variant 2: same with --variant direct --beta 0.1 --lr 1e-4 --rl-batch 1024 --mix 0 (no draws or weights)

## 9 October, 20:58 (piece F): oversampled PS1 added; v2 chain queued

- **Block-check problem** (SPEC.md, F 20:45): B's block check is wrong for
  draws whose groups come from drawn values. The S0 calibration that used
  it was cancelled and rerun without it (job 28902234). F's calls skip it.
- **New arm PS1s, from scratch with the top oversampled**
  (`f_full.draw_strat`, `calib_strata`). Reason: with equal starting
  weights, a from-scratch sample has very few records in the top income
  groups (S1 at 8x the real count: 53 synthetic records in the 2013 top
  0.1% group against 298 positive target cells for it; the real SCF has
  1,035 records there), so the top cells cannot be hit together. PS1s draws
  a pool of 16x the PUF count only as far as the income measure (E00100 in
  the full model), keeps every unit in the top three income groups and a
  4x-sized random subset of the bottom two, then draws the rest of each
  kept record. Method 1 then treats each income group as a "record" whose
  total is its targeted weight; KL from the model is reported split into
  the move between income groups and the move within them. Code test on
  the pilot model: job 28902931 (`f_pilot_strat.sh`, output
  `$P/dev/f_full/ps1s/`).
- **Chains on the full model.** v1 (`full`): as listed at 20:45, plus
  28903086 (PS1s draws) and 28903087 (PS1s calibration); evaluation
  28902520 now also waits on 28903087. v2 (`full_v2`, the main model from
  the 20:55 entry), queued with `./f_chain_version.sh full_v2`: waiter
  28904023 (`$T2/data/full_v2/DONE`, `$T2/model/full_v2/DONE_seed01`),
  draws 28904024 / 28904025 / 28904027 / 28904028 (PS0 p1, PS1 p1, PS0 p2,
  PS1 p2), PS1s draws 28904030, waiter 28904031 (`$P/full_v2/a/DONE_targets*`),
  calibrations 28904032 (PS0 K=8), 28904034 (PS1 4x), 28904035 (PS1s),
  28904037 (PS0 K=16), 28904038 (PS1 8x), evaluation 28904039. Outputs
  `$P/full_v2/f/`. Calibrations need A's `contrib_index_rows` and
  `load_targets_full` (SPEC.md, F 20:40) after `A.configure_full(version)`;
  if A's names differ, edit `a_interface` and its two call sites in
  `f_full.py` and resubmit the calibration lines of `f_chain_version.sh`.

## 9 October, 21:00 (piece A): full-model targets built; chains queued

Version 1 (`full`) target sets are written to `$P/full/a/` with
`DONE_targets` (job 28903219; code `a_full.py`, choices in SPEC.md
"Contract changes", entry A 21:00):

- 166,156 PUF training records (dependent filers included), weight 120.7
  million; 110.4 million of it has a known, non-dependent age range.
- `puf` (PUF ground truth: 39 variables by AGI group, filing status, age,
  dependents, plus 10 x 10 crosses of main income items): 14,085 cells,
  1,027 zero, 367 positive cells with fewer than 5 records. The
  contribution matrix on the PUF's true values reproduces the targets
  exactly; within every family the cells add to the PUF weight (relative
  difference 3e-13).
- `wcore` (24 wealth variables by income group, age, married, dependents):
  5,423 cells, 328 zero. `wfull` (adds the bands of 7 income items both
  sources measure): 14,993 cells, 1,030 zero. Every group has SCF records
  in both waves (no cell lacks an SCF counterpart); the 2015 shares are
  exactly s_2013/3 + 2 s_2016/3 and sum to one within each group; the
  targets add to each PUF group's weight (relative difference 3e-16). 112
  (`wcore`) and 484 (`wfull`) positive cells rest on fewer than 5 SCF
  records over both waves.

Pilot check of the code on the current model (expB, `$P/full/a/pilot_expB/`,
K = 2 only, job 28903234): draws and float32 log-probabilities agree
(sampling against teacher-forced, difference 0); 70,536 PUF records with
age missing kept it hidden and undrawn; 11 positive `wcore` cells reached by
no draw. These pilot files test the code; they are not results.

Queued (run after this session):

- Job 28904192 (`a_chain.sbatch full`, log `$P/logs/a_chain_28904192.out`):
  waits for `$T2/model/full/DONE_seed01`, then submits the GPU job
  `a_full.py draws --dataset full --k 32` (B200/H100/H200, 8-hour limit),
  which writes `$P/full/a/ref_draws_P.npz` (`tokens` [166156, 32, 24],
  `logp_ref`, `logp_ref_var`, seed 20261121), `ref_dollars_P.parquet`
  (wealth at 2015, decode seed 20261122), `ref_P_checks.json` (cells
  reached by no draw, target error before calibration) and `DONE_ref_P`.
  Records are presented as SCF, year 2015.
- Job 28904193 (`a_chain.sbatch full_v2`): the same for version 2; it first
  waits for `$T2/data/full_v2/DONE` and builds the targets into
  `$P/full_v2/a/` (marker `DONE_targets`, including E02000), then waits for
  `$T2/model/full_v2/DONE_seed01`.
- By hand, if a chain dies: `sbatch a_cpu.sbatch a_full.py build --dataset
  <full|full_v2>` (if `DONE_targets` is missing), then `sbatch --time=08:00:00
  -p gpu_b200,gpu_h100,gpu_h200 a_gpu.sbatch a_full.py draws --dataset
  <full|full_v2> --k 32`. Expect about 40 minutes of drawing on a B200.

## 9 October, 21:03 (piece A): version 2 targets built

G's `full_v2` was already final (`DONE` at 20:54), so chain job 28904193
built the version 2 target sets at once: `$P/full_v2/a/` with
`DONE_targets`. It now waits for `$T2/model/full_v2/DONE_seed01` and then
submits arm P's reference draws, as for version 1.

- Same 166,156 PUF records. The SCF measures 11 of the PUF amounts here
  (wages, Schedule C, long-term gains, dividends, taxable interest, taxable
  pensions, Social Security, farm, tax-exempt interest, unemployment
  compensation, total Schedule E E02000). The wealth sets' income group
  ranks the sum of these without tax-exempt interest.
- `puf`: 15,707 cells (40 variables, E02000 included, and 11 x 11 income
  crosses), 1,106 zero, 378 positive cells with fewer than 5 records.
- `wcore`: 5,423 cells, 325 zero, 121 positive cells on fewer than 5 SCF
  records. `wfull`: 20,735 cells, 1,907 zero (724 of them in the farm-income
  family, since few SCF units farm), 682 positive cells on fewer than 5 SCF
  records.
- The same checks pass as for version 1 (exact reproduction of the PUF set
  by the PUF records; cells add to group weights; interpolation exact).

Still pending from piece A: job 28903090 (`a_regress.py`, on a B200), a
check that the generalized `a_common.py` reproduces the SCF arm's
development draws exactly, and compares the teacher-forced
log-probabilities of the stored draws (output `$P/logs/a_gpu_28903090.out`).
A first run on an RTX 5000 (job 28901917) did not reproduce the same tokens,
most likely because sampling differs between GPU types; the decoded dollars
of the true tokens were identical. Until the B200 run confirms it, treat the
generalized sampler as unverified against the original draws.

21:02 update: the B200 run (job 28903090) reproduced the development draws
token for token, with identical log-probabilities, identical teacher-forced
log-probabilities and identical decoded dollars. The generalized
`a_common.py` gives the same results as the original on the SCF arm.

## 9 October, 21:05 (piece H): seed 1 of the `full` model is trained

- `$T2/model/full/checkpoint_full_w1_seed01.pt` and marker `DONE_seed01`
  (job 28902544, 18 minutes on an RTX A5000). Best epoch 14 of 29;
  validation loss 1.045 nats per hidden value (PUF records 0.736, SCF
  records 1.258). 6.87 million parameters. Wealth completion
  log-likelihood on the SCF validation households: 35.66 nats per record
  (test 2's comparable `joint_w1` seed 1: 35.51).
- After epoch 11 the SCF records' validation loss rises while the PUF
  records' keeps falling; the stopping rule uses their weighted sum.
  Details will go in `H_MODEL_REPORT.md`.
- Running: completions for seed 1 (28902545), training seeds 2 and 3
  (28902546, 28902547). Version 2 of the data has landed and its seed 1 is
  training (28902588, `$T2/model/full_v2/`).

## 9 October, about 21:25 (piece C): note for B — `calibrate()` on C's draws

Integration test of `c_post.py` (job 28901936, log
`$P/logs/c_gpu_28901936.out`, copy of C's SCF-arm draws in
`$P/dev/c/post_test/`): `b_calibrate.calibrate()` (code as of about 20:50)
ran 35 minutes and ended with `converged=False`; statuses active 469,
joint 26,979, zero 3,034, block 68, unreachable 62, above_reach 12. So it
dropped almost every positive cell, and its KL (0.0002) and effective draws
(32.0) do not describe a calibration to the targets. C's draws already
miss the targets by 0.016 percent of the group total on average (largest
1.5 percent), so this should be an easy problem; B's own K = 8 reference
report shows the same pattern (active 5, joint 25,692). Probably a solver
issue rather than infeasibility. C's chain re-runs `c_post.py` automatically
when `$P/b/DONE_ref` appears. Meanwhile C adds a labelled fallback
(`c_resid.py`, L-BFGS on the same dual) so method 2 has residual-calibration
numbers.

## 9 October, 21:17 (piece F): full-model targets wired to A's `a_full.py`

- `f_full.a_targets` now builds F's target sets straight from A's
  `a_full.py` (core = A's `puf` + `wcore`; full = `puf` + `wfull`), grouping
  each draw by its own values. Check (`f_check_targets.py`, CPU): the PUF
  training records pushed through this path reproduce A's `puf` targets
  exactly. One bug in `a_full.rebuild_set` is worked around in F's code
  (SPEC.md, F 21:15).
- B's solver now rakes by default and needs every cell in a single column
  slot; F's unassigned entries now point at one no-target cell per slot,
  and padding in the oversampled PS1 uses B's `draw_mask`.
- v1 model and v1 targets are both done, so the v1 draws are running
  (28901888, 28901889, 28901890, 28901891); v1 calibrations follow
  automatically. v2's model and targets are also done; v2 draws are queued
  behind the GPU queue.

## 9 October, about 21:40 (piece C): current chain (supersedes C's 20:55 and 21:00 tables)

Code now reads arm P of the full model through A's `a_full.py` target sets
(`c_arm.py`: `--arm P_core` uses `wcore`, `--arm P_full` uses `wfull`;
`--dataset full | full_v2 | pilot_expB`). Tested end to end on A's pilot of
arm P on the current model (`$P/dev/c/pilotP_core/`, K = 8): 160,943 PUF
records, 70,536 with unknown age (left unobserved and in no age group).
KL 0.447 nats per unit of weight; mean target error 0.09 percent of the
group total, largest 2.4 percent. All 24 variables needed the ridge,
because the SCF shares carried to PUF groups do not agree across
families.

Waiters (CPU jobs on `day`, `c_chain.sh`, poll every 2 minutes, up to 24
hours), with `ds` = `full` and `full_v2`:

| Waiter | Waits for | Then submits (GPU: b200, h200 or h100) |
|---|---|---|
| 28907303 / 28907305 | `$T2/model/full/DONE_seed01`, `$P/full/a/DONE_targets*` | `c_fit.py --arm P_core` / `P_full --dataset full` -> `$P/full/c/<arm>/` (writes `FIT_DONE`) |
| 28907307 / 28907309 | same for `full_v2` | `$P/full_v2/c/<arm>/` |
X -> `weights_resid_fallback.npz`, `report_resid_fallback.json` |
| 28901920, 28901922, 28903947, 28903949 | `$P/<ds>/c/<arm>/FIT_DONE` | `c_post.py` (B's `calibrate()`, comparison with B, KL by income group, `DONE`) |
| 28901918 | `$P/b/DONE_ref` | `c_post.py` on the SCF pilot |

(28901919, 28901921, 28903946, 28903948 were cancelled and replaced by the
first two rows, to allow H100s.)

By hand, for one arm (from this folder):

    T2=/nfs/roberts/scratch/pi_nrs36/jar335/scf_joint; P=/nfs/roberts/scratch/pi_nrs36/jar335/posttrain
    ds=full_v2; arm=P_core; CK=$T2/model/$ds/checkpoint_${ds}_w1_seed01.pt
    sbatch -p gpu_b200,gpu_h200,gpu_h100 --time=06:00:00 c_gpu.sbatch c_fit.py --arm $arm --dataset $ds --checkpoint $CK --no-calibrate --outdir $P/$ds/c/$arm
    sbatch -p gpu_b200,gpu_h200,gpu_h100 --time=02:00:00 c_gpu.sbatch c_resid.py --arm $arm --dataset $ds --checkpoint $CK --dir $P/$ds/c/$arm
    sbatch -p gpu_b200,gpu_h200,gpu_h100 --time=02:00:00 c_gpu.sbatch c_post.py --arm $arm --dataset $ds --checkpoint $CK --dir $P/$ds/c/$arm --b-weights $P/$ds/b/$arm/weights_ref.npz

For E: until B's `calibrate()` converges, C's residual-calibration weights
are in `weights_resid_fallback.npz` (same layout as `weights_resid.npz`).

## 9 October, 21:25 (E, evaluation): full model added; watcher replaced

Following the 20:45 and 20:55 entries, E now evaluates the full model
(versions 1 and 2) first and keeps the current-model work as a pilot
appendix in `EVAL_REPORT.md`.

- Code: `e_full.py` (pretraining evaluation of piece H's files, and arm-P
  post-training files under `$P/<version>/`), plus the earlier
  `e_evaluate.py` / `e_puf.py` (pilot) and `e_compare.py` (report). Version
  is chosen by `E_FULL_VERSION=full|full_v2`.
- What the pretraining evaluation reads from `$T2/model/<version>/`:
  `h_gen_puf_seedNN.parquet` (test 1's measures against PUF training and
  held-out records, each run as its own Slurm job `e_t1eval`, outputs in
  `$P/e/puf_eval/<version>_gen_seedNN[_vstrain]/`, plus levels by AGI
  group); `h_scf_train_seedNN` / `h_scf_test_seedNN` (wealth against the
  same records' truth); `h_puf_wealth_seedNN` (against SCF 2013/2016
  interpolated to 2015). Income groups for wealth: test 2's seven-income
  total (`t2_*` columns), so the tables line up with the pilot.
- Post-training (arm P): `$P/<version>/a/ref_draws_P.npz` once `DONE_ref_P`
  exists, every `weights*.npz` under `$P/<version>/b/`, every `draws.npz`
  under `$P/<version>/c/` and `/d/` (with `weights_resid.npz` beside it if
  present), each once quiet for 5 minutes. PS0/PS1 (piece F) are not
  evaluated by E; F reports them.
- Watcher: Slurm job 28906346 (`e_sh.sbatch e_watch.sh 06:00`, log
  `$P/logs/e_sh_28906346.out`), every 10 minutes until 06:00. The earlier
  watchers (28898651, 28901214) were cancelled.
- First full-model results (version 1, seed 1) are in the report: whole PUF
  records generated from nothing, SCF training and test records' wealth,
  PUF records' wealth against the interpolated SCF.
- To finish by hand: `sbatch --mem=100G --time=03:00:00 e_sh.sbatch
  e_watch.sh once` (one round), then read `EVAL_REPORT.md`. To force one
  column to be recomputed, delete its `stats.json` and its line in the
  `.attempted` file of that folder.

## 9 October, 21:40 (piece B): full-model chains queued; solver changed

- Chains for arm P on the full model, one per data version: wait jobs
  28904202 (`full`) and 28904203 (`full_v2`), `b_wait.sbatch` ->
  `b_chain.sh <version>`, logs `$P/logs/b_wait_<job>.out`. Each waits for
  `$P/<version>/a/DONE_targets` and `$P/<version>/a/DONE_ref_P` (piece A's
  reference draws for arm P), then submits four GPU jobs in sequence:
  `b_full.py --version <v> --set wcore` (P_core), `--set wfull` (P_full), then
  the K = 8 and 16 subsets of each. Outputs: `$P/<version>/b/P_core/` and
  `P_full/` (`weights_P.npz`, `report_P.json`, `report_P_K8.json`,
  `report_P_K16.json`, markers `DONE_P`, `DONE_P_ksens`); submitted job ids
  are appended to `$P/<version>/b/chain_jobs.txt`.
- To run by hand: `sbatch b_gpu.sbatch b_full.py --version full_v2 --set wcore`
  (and `--set wfull`; add `--k 8 16 --marker DONE_P_ksens` for the
  sensitivity). Code test without draws: `--fake-draws 32 --outdir <dir>`
  (random tokens; checked at N = 166,156, K = 32, 264 cells per draw on an
  H100: runs, memory fits).
- PS0 / PS1 (piece F): call `b_calibrate.calibrate_matrix(idx, W, t, K,
  cell_meta=targets, draw_mask=...)` with F's own index (cell ids per draw;
  -1 or C = no cell). NaN targets are allowed and mean "no target" (status
  `no_target`, never treated as zero).
- Solver: the plain Newton method was unreliable on the real targets (many
  cells cannot be hit together, and the multipliers run away). The default
  is now raking (iterative proportional fitting, one family x variable at a
  time) followed by Newton from where raking stops, with cells whose
  multiplier passes 30 in absolute value released as 'joint'. A cell still
  off target by more than 1e-6 of itself when the time budget (45 minutes
  of solving) runs out is listed as 'missed'. Reports give the share of
  cells and of target weight within 1e-9, 1e-6, 1e-3, 1e-2 and 0.1.
- SCF arm (pilot) jobs resubmitted with this solver: 28910157 (K = 32,
  `DONE_ref`), 28910158 (K = 8, 16), 28910159 (K = 64, 128).

## 9 October, 21:40 (E): first finding on the full model (version 1), for H and the main session

Whole PUF records generated from nothing by the version-1 full model (seeds
1 and 3) are too thin at the top, and the shortfall is in the model's
tokens, not in decoding (`e_diag_gen_top.py`, aggregates only). Weighted
share of generated records whose AGI token lies in the top 1% rank band of
positive AGI: 0.32% (seed 1) and 0.23% (seed 3), against 0.99% in the PUF
training records; top 0.1% band: 0.012% and 0.007% against 0.099%. Wages,
partnership income and long-term gains show the same pattern. As a
result the generated files' top 1% share of AGI is 10.2% and 8.8% against
19.5%, total AGI is $6.7 and $6.5 trillion against $8.1 trillion, and mean
AGI in the top 0.1% AGI group is $2.0m and $1.4m against $6.1m. The
middle of the distribution matches closely. Test 1's classifier reports
AUC 1.0 only because generated records all carry one weight and that
classifier uses the weight; E adds a classifier without the weight.

## 9 October, 22:00 (piece D): variant 2 running on the full model; report drafted

- `D_REPORT.md` written (design, failure and fix of variant 2, code tests,
  outputs, how to read the results). Its results table holds only the
  first evaluations; final numbers will be in
  `$P/<ver>/d/P_core/<variant>/report.json`.
- Running: variant 2, P_core, beta 0.1: 28906187 (`full_v2`) and 28906192
  (`full`), each followed by its finish job (28906189, 28906194). First
  evaluations: misplaced share 4.10% -> 3.78% (v2) and 4.16% -> 3.79% (v1)
  after about 80 steps, KL 0.002-0.003 nats per unit weight.
- Waiting: variant 1 on P_core (and P_full) for each version, submitted by
  waiters 28906176 (`full`) and 28906177 (`full_v2`) when B's arm-P
  weights and `$P/<ver>/a/DONE_ref_P` exist. If B writes its weights under
  a name not in `bweights()` of `d_full_chain.sh`, submit by hand with the
  commands in the 21:00 entry.
- The earlier duplicate submissions (28904877, failed on a changed
  `configure_full` signature, now fixed; 28905653/28905654, cancelled as
  duplicates) can be ignored.

## 9 October, 22:00 (piece F): pilot results in F_REPORT.md; full-model runs

- `F_REPORT.md` written (pilot results for S0 / S1, first full-model draws).
  Pilot S1 calibration (job 28899724) ended without exact solutions at 1x
  and 8x and was cancelled before 4x to free job slots; its weights and
  reports are `$P/f/s1/weights_m{1,8}.npz`, `report_m{1,8}.json`, and the
  tables `$P/f/eval/tables.md`. The pilot PS1s calibration (28905285) was
  cancelled for the same reason.
- Full model v1: draws done for PS0 part 2, PS1 part 1 and PS1s; PS0 part 1
  and PS1 part 2 still drawing (28901888, 28901891). Unweighted
  evaluation of PS1 and PS1s: `$P/full/f/eval/tables.md`.
- v1 PS1s calibration resubmitted as 28914037 after a fix (padding rows now
  fall in zero-target cells: B's `draw_mask` path fails when a group has no
  draw clear of zero cells). The v1 evaluation 28902520 now waits on it.
  The v2 PS1s calibration (28904035) picks up the fix when it starts.
- Other F calibrations wait on the account's limit on running jobs.
- If a calibration fails: `sbatch --time=10:00:00 -p gpu_b200,gpu_h100,gpu_h200
  f_gpu.sbatch f_full.py calib <ps0|ps1|ps1s> [--k 8 | --mult 4] --set core full
  [--data $T2/data/full_v2 --ckpt $T2/model/full_v2/checkpoint_full_v2_w1_seed01.pt
  --out $P/full_v2/f --adir $P/full_v2/a]`, then `sbatch f_cpu.sbatch f_full.py
  eval --k 16 --mult 8 [same options]`.

## 9 October, 22:05 (piece D): waiters resubmitted for B's file names

B writes arm-P weights as `$P/<ver>/b/P_<set>/weights_P.npz` with marker
`DONE_P` (B's 21:40 entry), a name D's waiters did not look for. The
waiters were replaced: **28914943** (`full`) and **28914944** (`full_v2`),
started with `SKIP_DIRECT=1` so they do not resubmit variant 2 (already
running). They now submit variant 1 on P_core when `$P/<ver>/b/P_core/DONE_P`
and `$P/<ver>/a/DONE_ref_P` exist, and on P_full when `P_full/DONE_P`
exists. Waiters 28906176 and 28906177 were cancelled.

## 9 October, 22:10 (piece B): runs in flight, and how to finish by hand

Solver change (21:50): raking runs until the largest relative miss is below
1e-3 before Newton is tried (Newton from further away made the multipliers
run off); rounds of 400 raking sweeps repeat until the cells in the solve
are within 1e-9 or 45 minutes of solving have passed.

Running or queued (logs `$P/logs/b_gpu_<job>.out`, `b_cpu_<job>.out`):

- SCF arm (pilot): 28913699 (K = 32 -> `$P/b/weights_ref.npz`,
  `report_ref.json`, `DONE_ref`), 28913701 (K = 8, CPU), 28913700 (K = 16),
  28910159 (K = 64 and 128 from `ref_draws_k128.npz`).
- Arm P, `full` (v1): 28913728 (P_core), then 28913729 (P_full), then
  28913730 / 28913731 (K = 8, 16). Outputs `$P/full/b/P_core/`, `P_full/`.
- Arm P, `full_v2`: wait job 28913986 (replaces 28904203), same chain once
  `$P/full_v2/a/DONE_ref_P` exists; job ids go to
  `$P/full_v2/b/chain_jobs.txt`.

To finish by hand: when the jobs are done, `sbatch b_finish.sbatch` writes
Markdown tables (aggregates only) of every report that exists:
`$P/b/summary_scf.md`, `$P/<version>/b/P_<core|full>/summary.md`. Copy
the tables into `B_REPORT.md`. To rerun one piece:
`sbatch b_gpu.sbatch b_full.py --version full_v2 --set wcore` or
`sbatch b_gpu.sbatch b_calibrate.py --draws $P/a/ref_draws.npz --tag ref --marker DONE_ref`.

Correction (21:55, piece C): the fallback-calibration waiters 28907304,
28907306, 28907308, 28907310 were cancelled and replaced by 28916057
(full, P_core), 28916058 (full, P_full), 28916059 (full_v2, P_core),
28916060 (full_v2, P_full), which pass `--ridge 1e-3`. The PUF targets are
SCF shares carried to PUF groups and are not mutually consistent across
families, so an exact reweighting does not exist; with a weak ridge the
solver diverges (seen on the arm P pilot, job 28914500). By hand: add
`--ridge 1e-3` to the `c_resid.py` command above.

## 9 October, 22:15 (piece H): all pretraining work finished

- Both data versions, seeds 1 to 3: trained, and all four completion files
  written, decoded and checked (markers `DONE_seedNN` and `DONE_comp_seedNN`
  in `$T2/model/full/` and `$T2/model/full_v2/`; summaries `h_summary.md`
  there). Nothing of piece H's is queued or running.
- The version 2 seed 1 completions were rerun (job 28906268, replacing
  28902589) after a fix: whole PUF records are now generated in the data's
  own `generation_order`, which in version 2 puts total Schedule E after
  Schedule C; the first attempt drew it last.
- Version 2 wealth completion score on the SCF validation records: 35.50 to
  35.56 nats per record across seeds, the same as test 2's model (35.50 to
  35.51); version 1: 35.66 to 35.68.
- Report: `H_MODEL_REPORT.md`; training curves `h_training_curves_full.png`,
  `h_training_curves_full_v2.png`.

## 9 October, 22:45 (E, evaluation): state at hand-off

- Watcher **28906346** (`e_sh.sbatch e_watch.sh 06:00`, log
  `$P/logs/e_sh_28906346.out`) runs every 10 minutes until 06:00 on
  10 October: full model version 2, then version 1 (`e_full.py --auto`),
  then the pilot (`e_puf.py`, `e_evaluate.py`), then `e_compare.py
  --update-report`, which rewrites the generated block of `EVAL_REPORT.md`
  (version 2 first, version 1 second, pilot in the appendix) and
  `$P/e/compare.md`. Test 1's evaluations of generated PUF files are sent
  to their own Slurm jobs (`e_t1eval`).
- Evaluated so far: pretraining for version 1 (seeds 1 and 3) and version
  2 (seeds 1 to 3, all four kinds of file); arm P reference draws for both
  versions; method 2 (P_core, before residual calibration) on version 1.
  Waiting for: B's `$P/<v>/b/P_<set>/weights_P.npz`, C's remaining
  `draws.npz` and `weights_resid[_fallback].npz`, D's
  `$P/<v>/d/P_<set>/<variant>/draws.npz`. Each is evaluated once it has
  been unchanged for 5 minutes.
- Correction to E's 20:05 contract note: the watcher does not compute
  residual-calibration weights itself (too slow on a CPU); it evaluates
  `weights_resid.npz` or `weights_resid_fallback.npz` when the method
  writes one.
- `EVAL_REPORT.md`'s hand-written summary was written at 22:15-22:40 and
  covers pretraining and the first post-training result only. Whoever
  picks this up: rewrite the "Post-training" paragraph from the generated
  sections Q1 to Q3 once B, C and D's files are in (KL and effective draws
  first, then target fit, then the net-worth levels by income group).
- By hand, if the watcher has stopped: `sbatch --mem=120G --time=03:00:00
  e_sh.sbatch e_watch.sh once`; one version's post-training only:
  `sbatch --mem=120G --time=01:00:00 e_sh.sbatch e_post.sh full_v2`. To
  force a column to be recomputed, delete its `stats.json` and its line in
  that folder's `.attempted`.
- Not done by E: PS0 and PS1 (piece F reports them); the levels tables by
  the AGI-type income measure that defines the target groups (the tables
  use test 2's seven-income total, so they line up with the pilot and with
  test 2).

## 9 October, about 22:40 (piece C): closing note for the session

Done (pilot, current model; details and tables in `C_REPORT.md`):

- SCF arm: tilt fitted exactly on its own draws; fresh draws within 0.016
  percent of the group total on average (expected), largest 1.5 percent; KL
  from the reference 1.313 nats per unit of weight, the same when fitting on
  8, 32 or 64 draws per record. KL by income group rises from about 0.8
  (bottom half) to 8 to 9 nats (top 0.1 percent).
- Residual calibration of the tilted draws with C's fallback solver
  (`$P/c/weights_resid_fallback.npz`): KL 0.009, 31.45 effective draws of
  32, active cells within 0.017 percent; 74 cells set aside as unreachable
  with 32 draws.
- Method 1 by the same fallback solver on A's reference draws
  (`$P/dev/c/ref_fallback/`, not converged): KL 2.62, 2.85 effective draws,
  largest error 8.8 percent. B's converged result will replace this.
- Arm P pilot (`$P/dev/c/pilotP_core/`): KL 0.447; the SCF-share targets are
  inconsistent across families, so every variable needed the ridge; mean
  error 0.09 percent, largest 2.4 percent. Fallback residual calibration
  with ridge 1e-3 running as job 28917242 (writes
  `$P/dev/c/pilotP_core/report_resid_fallback.json`, about 23:25).

Still waiting (all automatic; table in the 21:40 note, with the 21:55
correction): fits for `full` and `full_v2`, arms P_core and P_full, on
`$T2/model/<ds>/DONE_seed01` and `$P/<ds>/a/DONE_targets*`; then C's
fallback calibration (ridge 1e-3) and `c_post.py` (B's `calibrate()`); and
`c_post.py` on the SCF pilot when `$P/b/DONE_ref` appears. After the runs
land, the numbers for `C_REPORT.md` are in `$P/<ds>/c/<arm>/report.json`
(fit, KL, KL by income group after `c_post.py`, `target_consistency`) and
`report_resid_fallback.json`.

Not done: method 2 on PS0 / PS1 (needs per-draw groups from values drawn
in the same pass, and source-tag switching inside A's step function); the
comparison of multipliers with B's converged solution (automatic once B's
weights exist).

Note for E (22:40, piece C): `$P/c/DONE` is written. `$P/c/weights_resid.npz`
holds C's fallback residual-calibration weights (same layout as B's), since
B's `calibrate()` did not converge on these draws. From now on `c_resid.py`
fills `weights_resid.npz` when it is absent, and `c_post.py` overwrites it
with B's weights only if B's solver reports `converged`; otherwise B's
weights go to `weights_resid_B_unconverged.npz`. The same rule applies in
`$P/<ds>/c/<arm>/`.

## 9 October, 22:50 (piece B): first results; what is still running

Results so far are in `B_REPORT.md`. In short: no run reached exact
calibration within its 45-minute solving budget. SCF arm at K = 32: KL 1.93
nats per unit of weight, 5.2 effective draws of 32, 562 cells impossible
before the solve and 5,997 released during it, 72% of positive cells within
1% of target, total miss 0.27% of target weight. Arm P (v1, P_core): KL
1.21, 9.6 effective draws, 70% of cells within 1%, total miss 1.6%.

Written: `$P/b/weights_ref.npz`, `report_ref.json`, `DONE_ref` (K = 32),
`report_ref_K8.json`, `report_ref_K16.json`, `report_ref_K64.json`;
`$P/full/b/P_core/weights_P.npz`, `report_P.json`, `DONE_P`.
Note for D and E: these weights are not exact; each report lists the cells
off target (statuses 'joint' and 'missed').

Still queued or running at 22:50:

- 28910159: SCF K = 128 (`report_ref_K128.json`).
- v1 arm P: 28913729 (P_full), 28913730 / 28913731 (K = 8, 16).
- v2 arm P: 28918412 (P_core) and 28918413 (P_full), resubmitted after the
  first attempts (28916145, 28916146) died on an uncorrectable GPU memory
  (ECC) error on node a1112u01n01; 28916147 / 28916148 (K = 8, 16).
  Pending jobs exclude that node.
- 28918495 (`b_finish.sbatch`, runs after all of the above): writes
  `$P/b/summary_scf.md` and `$P/<version>/b/P_<core|full>/summary.md`.
- 28918496: self-test of the current solver version on fabricated data
  (`$P/dev/b/selftest.json`).

By-family fit tables: `sbatch -p devel --mem=48G b_cpu.sbatch
b_fit_by_family.py P $P/<version>/b/P_core/weights_P.npz --version
<version> --set wcore` (or `scf $P/b/weights_ref.npz`).

## 9 October, 22:40 (main session): why generated PUF records are thin at the top

Every PUF training return in the top 1% of AGI lacks an age (weighted share
with age: 99.3% below the median, 90.8% at the 90th–99th percentile, 0.0% in
the top 1%). Generation draws AGERANGE (7th) before E00100 (10th), so every
generated record has a visible age, and the model learned from the PUF that
a visible age means not a top return. Check queued: job 28917534
(`$P/dev/main/demoglast.sbatch`) regenerates whole PUF records from the
version 2 seed 1 model with AGERANGE, GENDER and EARNSPLIT drawn after all
PUF amounts, then `top_compare.py` prints total AGI and top shares for the
PUF training records, the current generation and the age-last generation
(log `$P/logs/main_demoglast_28917534.out`).

## 9 October, 23:05 (piece F): hand-off

- `F_REPORT.md` is current as of 23:00 (pilot S0 / S1, full-model draws,
  first full-model calibrations). Headline: no from-scratch arm hits every
  target exactly. On the full model (core targets) the best compromise
  misses a cell by about 2% of an average cell, moves the weights about 1.1
  nats from the model (1.4 to 1.9 with the top oversampled) and leaves an
  effective sample of 4% to 6% of the draws. Both full models draw too few
  high-income records from scratch (top 0.1% AGI group: about a ninth to a
  tenth of its share).
- All F draws are done (v1 and v2: PS0 parts 1 and 2, PS1 parts 1 and 2,
  PS1s). Still running at 23:05, each writing `report_<set>_<size>.json`
  and `weights_<set>_<size>.npz` under `$P/<version>/f/<arm>/`:
  v1 28901893 (PS0 K=8), 28901895 (PS0 K=16), 28901894 (PS1 4x, full set
  next), 28901896 (PS1 8x), 28914037 (PS1s); v2 28904032 (PS0 K=8),
  28904037 (PS0 K=16), 28904034 (PS1 4x), 28904038 (PS1 8x), 28904035
  (PS1s). Then evaluation: v1 28902520, v2 28904039 (`$P/<version>/f/eval/tables.md`).
- To refresh the summary table: `sbatch f_cpu.sbatch f_summary.py`, then
  paste its table into section 5 of `F_REPORT.md`.

## 9 October, 23:05 (main session): age-last check, result

Job 28917534 finished. Whole PUF records generated from the version 2 seed 1
model (166,156 records, equal weights), compared with the PUF training
records on the training records' own thresholds:

| | Total AGI | Weight above true 99th pct | Above true 99.9th | AGI share above 99th | Above 99.9th |
|---|---|---|---|---|---|
| PUF training records | $8.09tn | 1.00% | 0.100% | 19.4% | 8.8% |
| Generated, age drawn first (as run) | $6.62tn | 0.30% | 0.010% | 4.8% | 1.3% |
| Generated, demographics drawn last | $7.37tn | 0.65% | 0.028% | 8.8% | 1.6% |

Drawing demographics after incomes roughly doubles the top (and closes half
the AGI shortfall), so the age order is a large part of the cause but not
all of it. Remaining candidates: early stopping driven by the SCF part of
the validation loss (the PUF part was still improving); population-weighted
loss giving the top 1% only 1% of the training signal; and errors
compounding when generating from nothing, which piece F also sees as drawn
incomes too low overall (from-scratch SCF draws put 75% of weight below the
true median income, against 44%).

## 9 October, 23:10 (piece D): end of session; what is queued

Results so far (full model, arm P, P_core; misplaced share = share of PUF
weight in the wrong coarse wealth bin, from the network's own draws):

- Variant 2 (beta 0.1, 80 minutes): `full_v2` 4.10% -> 2.25% at KL 0.167
  nats per unit weight; `full` 4.16% -> 2.21% at KL 0.208. Still improving
  when time ran out. Finish jobs (32 fresh draws, residual calibration,
  report) queued: 28906189 (`full_v2`), 28906194 (`full`).
- Variant 1 on `full`: training job 28918406 (B's P_core weights, 9.6
  effective draws of 32, method-1 KL 1.21), finish 28918407.
- Variant 1 on `full_v2` and on P_full: submitted by waiters 28914943
  (`full`) and 28914944 (`full_v2`) when B's `P_<set>/DONE_P` and A's
  `DONE_ref_P` exist.
- Pilot check of variant 1 on B's SCF-arm weights (job 28918265, 30
  minutes, `$P/d/scf_distill_test/`): misplaced share 2.25% -> 1.49% after 6
  minutes at KL 0.24 (method 1 on the same draws: KL 1.93).

When the finish jobs end, each `$P/<ver>/d/P_core/<variant>/report.json`
has the numbers `D_REPORT.md`'s table needs; markers
`$P/<ver>/d/DONE_P_core_<variant>`. To fill the table by hand, read
`fresh_draws` (fit before calibration, KL) and `residual_calibration` (KL,
effective draws, cell statuses). If a finish job fails, rerun
`d_finish.sh` with the commands in the 21:00 entry.

## 9 October, 23:10 (piece B): end of session

- Solver final version (23:00): after each round of 400 raking sweeps,
  Newton is tried and kept only if it lowers both the total and the largest
  miss. Self-test on fabricated data passes for the feasible and infeasible
  cases (exact to about 1e-12 on every cell kept); fails, as documented,
  when the only exact answer puts all weight on one draw of two.
  The SCF-arm results and v1 P_core used an intermediate version (raking
  only, in effect); jobs started after 22:55 use the final one.
- v1 P_full (28913729) ran out of memory on a 32 GB GPU; resubmitted as
  28919769 on 80 GB+ GPUs only. v2 P_full and the wfull K-sensitivity jobs
  were moved to 80 GB+ partitions too. `b_finish.sbatch` (28918495) now
  waits on 28910159, 28913730, 28913731, 28918412, 28918413, 28916147,
  28916148 and 28919769, then writes the summary tables.
- To finish by hand: once those jobs end, copy the tables from
  `$P/b/summary_scf.md` (adds K = 128) and
  `$P/<version>/b/P_<core|full>/summary.md` into `B_REPORT.md`, and run
  `b_fit_by_family.py` for by-family fit tables.
- Correction to the 20:50 memory estimate (it was too low): measured peak
  GPU memory was 24 GB for arm P with 96 cells per draw (N = 166,156,
  K = 32), and the 264-cells-per-draw set did not fit in 32 GB. Use GPUs
  with 80 GB or more for arm P; the SCF arm at K = 64 ran on an 80 GB H100
  (K = 128 was still running there at 23:05).

9 October, 23:12 (piece C): the arm P pilot's fallback residual
calibration finished (job 28918617): KL 0.014, 7.85 effective draws of 8,
errors 0.084 percent mean and 2.0 percent largest (inconsistent targets;
see `C_REPORT.md`). Piece C's interactive work ends here; everything else
is in the waiters listed at 21:40 and 21:55.

## 9 October, 23:15 (piece C): full-model fits are done

The waiters fired at 21:22 (checkpoints and A's targets already existed).
Fits finished for `full` and `full_v2`, arms P_core and P_full:
`$P/<ds>/c/<arm>/{lambda.npz, draws.npz, report.json, FIT_DONE}`. Results
table in `C_REPORT.md` ("Full model: results"): KL 0.67 / 0.69 (P_core,
v1 / v2) and 1.56 / 2.03 (P_full); families' targets disagree by up to 3.5
percent of all weight (P_core) and 6.5 to 6.9 percent (P_full).

Because the first waiters fired before they were cancelled at 21:3x, some
fits ran twice with the same seeds (identical outputs). The last duplicate
(28907548, `full` P_full) was cancelled at 23:13; the outputs in
`$P/full/c/P_full/` are from the earlier, complete run.

Still running when the session ends: fallback residual calibrations
(`c_resid.py`, e.g. 28917225 for full P_core, 28918634 for full_v2 P_core;
the others start as their waiters release them) and `c_post.py` with B's
`calibrate()` (28917031, 28917525, 28918655, 28918657 for the four full
runs; 28918021 for the SCF pilot). To finish the write-up by hand: read
`report_resid_fallback.json` (KL, effective draws, errors) and the
`residual_calibration`, `lambda_vs_B` and `kl_by_wave_income_group` entries
of `report.json` in each `$P/<ds>/c/<arm>/`, and add them to the
"Full model: results" table in `C_REPORT.md`. If a fallback calibration was
not started (waiter cancelled), run:

    sbatch -p gpu_b200,gpu_h200,gpu_h100 --time=02:00:00 c_gpu.sbatch c_resid.py --arm <arm> --dataset <ds> --dir $P/<ds>/c/<arm> --ridge 1e-3

23:16 (piece C): the P_full fallback calibrations (28917536 for `full`,
28918635 for `full_v2`) were killed for lack of host memory (the reach
screens in `c_resid.py` build a table of every record–cell pair: about 1.9
billion entries for P_full). Resubmitted with 400 GB: 28920522 (`full`)
and 28920523 (`full_v2`), outputs in `$P/<ds>/c/P_full/`. If they fail
again, the screen in `c_resid.py` (the `pairs` frame) needs to be done in
chunks of records.

## 9 October, 23:20 (main session): where things stand at the end of the session

All eight agents have reported; remaining work runs as queued Slurm jobs
(each piece's last entry above lists them). Results so far, in brief:

- **Pretraining.** Whole PUF records generated from nothing are thin at the
  top (total AGI $6.2–6.6tn against $8.1tn). About half of that is the
  generation order: every top-1% PUF return lacks an age, and age was drawn
  before income (23:05 entry). Completed PUF wealth has a median of $55–57k
  against the SCF's $91k at 2015 (E).
- **Post-training, arm P, version 1, core targets** (move from the model in
  nats per unit of weight; mean / worst target error as share of a group):
  tilt (C) 0.67, 0.08% / 2.0%; reweighting (B, not converged) 1.21, 0.23% /
  38%, 9.6 effective draws of 32; network tuning variant 2 (D, unfinished)
  0.17, misplaced share 4.1% → 2.25%. The tilt's move rises with income
  (pilot: about 0.8 nats in the bottom half, 8–9 in the top 0.1%).
- **The targets contradict each other.** SCF shares carried onto PUF groups
  imply different totals for the same wealth bin across families (largest
  disagreement 3.5% of weight on core targets, 6.9% on full; interest income
  worst). No method can do better; D24's translation is the intended fix.
- **From scratch (F):** no exact fit; best compromise misses about 2% of an
  average cell, moves 1.1–1.9 nats, keeps 4–6% effective sample; over 1,000
  cells per run have no synthetic record (thin top).

Morning tasks: rewrite the summary at the top of `EVAL_REPORT.md` from the
regenerated tables; fill D's and B's report tables from their `report.json`
files; read version 2's results.
