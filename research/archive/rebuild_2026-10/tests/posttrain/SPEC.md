---
title: "Test 3 — post-training the joint model to hit the data exactly: shared specification"
role: method
workstream: rebuild
status: current
updated: 2026-10-09
sot: research/rebuild/design/plan.md
supersedes: []
superseded_by: null
---

# Test 3: post-training

This file is the contract between five pieces of work done in parallel by
separate agents: shared data and targets (A), method 1 reweighting (B),
method 2 probability tilt (C), method 3 network tuning (D), and evaluation
(E). Each piece reads and writes only the files named here. If the contract
must change, write the change into "Contract changes" at the end, with your
letter and the time, before relying on it.

Read first: `research/archive/rlhf_analogy_2026-10-14_superseded.md` (the closed form),
D22 in `research/archive/design_decisions_2026-10-14_superseded.md`, and test 2 in
`research/rebuild/tests/scf_joint/` (`PLAN.md`, `SPEC.md`, the top of
`STATUS.md` and of `EVAL_REPORT.md`). Test 2's code is the base:
`b_model.py` (model, `complete_batch`, `completion_nll`, `fill_order`),
`b_complete.py` (allowed-code tables, decoding to dollars), `a_tokenizer.py`
(`decode`), `c_common.py` (`income_groups`, `token_info`, `wrank`, measures).
Import from `../scf_joint/`; do not edit files there.

## The question (John, 9 October)

John has little intuition for how good the first-stage ("pretrained") model's
fit needs to be. The final file will be calibrated so that it reproduces the
data exactly; the question is how far each post-training method has to move
the model to get there, and what the move does to relationships that were
not targeted. Held-out records are **not** part of this test. We complete the
SCF *training* records and target those same records' own tables. Their true
wealth is known; the point is to measure the distance between the learned
model and its own training data, and how each method closes it.

## Decisions (John, 9 October)

| Question | Decision |
|---|---|
| Held-out records | Not used. Targets come from the training records themselves. |
| Exactness | The end product hits every feasible target exactly. Infeasible cells are listed, not hidden. |
| Record weights | Each record keeps its own population weight. Only wealth changes, so every table of the observed variables (incomes, demographics) is reproduced exactly by construction. |
| Target detail | Very detailed: every wealth component's distribution crossed with every observed variable (below). |
| Methods | (1) reweight draws; (2) tilt the model's probabilities with one added number per target cell; (3) tune the network's weights. |
| Who | Opus subagents (Claude), not Codex. |

## Fixed setup

- **Model (the reference).** `$T2/model/expB/checkpoint_joint_w1_seed01.pt`:
  test 2's baseline joint PUF+SCF transformer, base size, full population
  loss weights, survey weight not a model variable. `$T2` is
  `/nfs/roberts/scratch/pi_nrs36/jar335/scf_joint`. Data: `$T2/data/expB/`.
- **Records.** expB *training* split, rows with source SCF and
  `wealth_observed` true (split-off units, whose wealth was never asked,
  are excluded from everything). Waves 2013 and 2016. Each row is one
  implicate of a tax unit at its population weight (`weight` column); a row
  is a "record" throughout. The records are always presented with source
  SCF and their own year.
- **Generation order.** One fixed order for everything: `fill_order(meta)`
  from `b_model.py`, the order test 2's completions used. The model's
  distribution of a record's 24 wealth variables is *defined* as the product
  of its conditional probabilities in this order. (Test 2 found other orders
  give different answers; we sidestep that by never using another.)
- **Allowed codes.** Use the same allowed-code restriction as
  `b_complete.py` (codes the decoder can turn into dollars for that wave).
  It is part of the reference distribution.
- **Log-probabilities in float32.** Sampling may use bf16 autocast, but every
  log-probability used for a KL or a likelihood is recomputed in float32.
- **Dollars.** Decoded with `a_tokenizer.decode` against the wave's own
  quantiles, position within a bin drawn with a fixed seed, as in test 2.

## Targets

Each wealth component v (24, `WEALTH` in `c_common.py`) has its tokens
mapped to 13 **coarse bins**: exact zero; any negative value (one bin); and
positive rank bands [0,.1), [.1,.2), …, [.8,.9), [.9,.99), [.99,.999),
[.999,1] (rank among the wave's positive holders, from the token's band;
`token_info` gives the band). Variables without negative tokens have 12.

A **target cell** is (wave, family, group, v, coarse bin). Its value is the
total population weight of records in that wave and group whose true token
for v falls in that coarse bin. Families and their groups:

| Family | Groups |
|---|---|
| `inc` | income group: weighted rank of the seven-income total within wave (as `income_groups` in `c_common.py`, ranked over all SCF training units of the wave, split-offs included), edges 0.5, 0.9, 0.99, 0.999 → 5 groups |
| `age` | `age_range` token (6) |
| `mar` | `married` (2) |
| `dep` | `n_dep` (4) |
| `x_<income>` | for each of the seven incomes: its own coarse bin on the record: zero, negative (if any), positive [0,.5), [.5,.9), [.9,1] of rank → 4–5 groups, 7 families |

That is 11 families. Cells with a true total of zero are **zero cells**: every
draw falling in one gets weight zero (methods 2–3: probability pushed to
zero). Cells are collinear (each family's cells sum to the same totals), so
solvers must tolerate redundant constraints.

**Contribution matrix.** For any set of completed draws, the sparse matrix
A with one row per (record, draw) and one column per target cell; entry 1
if the draw falls in that cell. A draw of a record touches exactly
11 × 24 cells. Every piece uses A's builder from `a_common.py`; nobody
re-implements the mapping.

## Pieces and files

Code and reports in this folder, `research/rebuild/tests/posttrain/`,
prefixed by piece letter. Data and outputs in
`/nfs/roberts/scratch/pi_nrs36/jar335/posttrain/` (`$P`), subfolders by piece
(`$P/a/`, `$P/b/`, …), Slurm logs in `$P/logs/`, temporary files in `$P/dev/`.

### A. Shared data, targets, reference draws

- `a_common.py`: load the records and the reference model; income and other
  groups; coarse-bin mapping; `targets()` → table of cells; `contrib(tokens,
  records)` → sparse A (scipy CSR, and a torch helper for the GPU);
  `ref_logprob(model, tokens, ...)` → per-draw float32 log-probability of
  the 24 wealth tokens under the reference in the fixed order (per variable
  and summed); `decode_dollars(...)`.
- `$P/a/records.parquet` (row id into expB train, source, year, weight,
  groups for every family, true wealth tokens, true decoded dollars),
  `$P/a/targets.parquet` (cell id, wave, family, group, variable, coarse
  bin, target weight, number of records in it), `$P/a/cells.json`.
- **Reference draws**, K = 32 per record: `$P/a/ref_draws.npz` with
  `tokens` [N, K, 24] (wealth tokens in `WEALTH` order), `logp_ref`
  [N, K] (float32 sum over the 24), `logp_ref_var` [N, K, 24]; plus
  `$P/a/ref_dollars.parquet` (record, draw, 24 components, net worth).
  Seed fixed and recorded.
- **Deliver the interface early.** Write `a_common.py` with the target
  definitions and contribution builder first, and a small file
  `$P/a/dev_draws.npz` (K = 4, same layout) within about 30 minutes, then
  the full draws. Append to `STATUS.md` when each lands, with a `DONE_<name>`
  marker in `$P/a/`.

### B. Method 1 — reweight draws (model unchanged)

Minimum-KL weights on the reference draws subject to (i) each record's
draws summing to its weight and (ii) every feasible target cell hit
exactly. With the record constraints eliminated, draw j of record i gets
`w_ij = W_i · exp(λ·a_ij) / Σ_j' exp(λ·a_ij')` and λ solves a smooth convex
problem in the cell dimension only (a few tens of thousands). Solve on GPU
(Newton with a small ridge or L-BFGS; your choice; justify).

- Before solving: drop draws in zero cells; find **infeasible cells**
  (positive target, no remaining draw can reach it, or record constraints
  make it unreachable) and list them; solve with those removed.
- `b_calibrate.py`: a reusable function and CLI, `calibrate(draws_tokens,
  records, targets) -> weights, lambda, report`, used again by C, D and E
  for the **residual calibration** of their own draws.
- Outputs `$P/b/weights_ref.npz` (`w` [N, K], `lambda`, cell ids),
  `$P/b/report_ref.json`: max and mean absolute target error (should be at
  machine precision for feasible cells), infeasible cells by family,
  KL = Σ_i Σ_j w_ij log(K w_ij / W_i) / Σ_i W_i (nats per unit of weight),
  effective draws per record (weighted mean of 1/Σ_j q_ij², q = w/W),
  share of records with fewer than 4 effective draws, iterations, time.
- Sensitivity: K = 8, 16, 32 (subsets of the draws), and, if A provides
  them, K = 64 or 128. How KL and infeasibility change with K is part of
  the answer.

### C. Method 2 — tilt the probabilities (network fixed)

Add a number to the reference model's logits at the generation step of
variable v, for every token in the coarse bin b, for records in group g of
family f and wave t: logit += Σ_f λ_c with c = (t, f, g_f(record), v, b).
Same parameter layout as B's λ. Draws from the tilted model are unweighted.
Fit λ so that the expected targets under the tilted model hit the targets.

- The expectation of a cell for variable v can be computed from the step's
  probabilities rather than the sampled token (less noise); upstream
  variables are sampled. Fit iteratively with fixed random numbers across
  iterations; B's λ is a natural starting point. Your design; write it down.
- Note in the write-up: the tilt is normalized at each step, so it is not
  the same distribution as B's (normalized over the whole record) even at
  the same λ; report how different the fitted λ are from B's.
- Outputs `$P/c/lambda.npz`, `$P/c/draws.npz` (K = 32, same layout as A's,
  plus `logp_new` and `logp_ref` [N, K]), `$P/c/report.json`: expected and
  realized target error, KL(tilted ‖ reference) per record estimated as the
  mean over draws of logp_new − logp_ref (weighted per unit of weight), and
  the **residual calibration** (B's function on these draws): its KL,
  effective draws and infeasible cells.

### D. Method 3 — tune the network

Start from the reference network and change its weights so that its own
draws hit the targets, staying close to the reference. Two variants, the
first required:

1. **Weighted distillation.** Fine-tune on B's calibrated draws: each draw is
   a training example of the fixed-order completion of the 24 wealth tokens
   given the record's observed variables, weighted by B's `w`. Stop on
   target fit of fresh draws (not on training loss). Optionally mix in the
   original training objective to keep the rest of the model intact.
2. **Direct target loss with a KL penalty** (if time allows):
   loss = squared target error of expected cells (from step probabilities,
   as in C) + β · KL(new ‖ reference) estimated on draws. A few values of β.

- Outputs per variant `$P/d/<variant>/checkpoint.pt`, `draws.npz` (K = 32,
  fresh draws from the tuned network, same layout as C's), `report.json`
  (as C's: target error before calibration, KL from the reference, residual
  calibration KL and effective draws), training curves.
- GPU partitions as in test 2. Keep each training run under about 90
  minutes.

### E. Evaluation

`e_evaluate.py` reads any draws file (+ optional weights) and writes one
comparison, `EVAL_REPORT.md` in this folder and `$P/e/`. Columns: truth
(training records), reference unweighted, method 1, method 2 before and after
residual calibration, method 3 variants before and after.

1. **Size of the move** (the headline): KL from the reference and effective
   draws, for each method, before and after residual calibration.
2. **Target fit**: by family, mean and max absolute error in cell weight as a
   share of the group total; infeasible cells.
3. **Untargeted relationships**, all on the same records:
   - net worth in dollars: mean, median and share of all net worth by
     income group (bottom 50, 50–90, 90–99, 99–99.9, top 0.1), top 1% and
     0.1% shares of net worth, the levels table John asked for in test 2
     (truth vs method, by income group);
   - joint holding between components (all pairs; summarize, and list
     mortgage among homeowners, stocks among business owners, unrealized
     gain present among holders of the asset);
   - two observed variables at once: holding and amount ranks by income
     group × age;
   - rank correlations between components and with total income;
   - the classifier test from test 2 (completed versus true records).
4. **Monte Carlo yardstick**: two independent reference draws against each
   other, so differences smaller than draw noise are not over-read.

Develop on `$P/a/dev_draws.npz` and B's first weights; rerun when final
files land (watch for `DONE_*` markers).

## Rules for everyone

- **Compute.** Every Python run goes through `sbatch`; never the login node,
  never `srun` from the session. CPU: partition `day` (or `devel` for a few
  minutes). GPU: `--gres=gpu:1` with `-p gpu_b200,gpu,gpu_h100,gpu_h200`.
  Slurm logs in `$P/logs/`.
- **Environment.** `module load PyTorch/2.9.1-foss-2024a-CUDA-12.8.0
  scikit-learn/1.6.1-gfbf-2024a Arrow/17.0.0-gfbf-2024a
  matplotlib/3.9.2-gfbf-2024a`. No pip installs. Copy the pattern of
  `../scf_joint/b_gpu.sbatch` and `c_py.sbatch`.
- **Licensed data.** SCF and PUF data and anything derived at record level
  stay under `$P` / `$T2`. The repository gets code, reports and aggregate
  tables only. Print no more than a handful of individual records.
- **Never drop NAs silently**; surface unexpected counts.
- **Writing.** Reports and status notes are for John, a public finance
  economist: plain, direct English, full sentences, no invented shorthand,
  define any term that is not standard in public finance. State what ran,
  then the numbers. Describe, do not argue for.
- **Status.** Append dated notes to `STATUS.md` (this folder): what is done,
  what is running (job ids, logs), how to finish by hand. The driving
  session's Slurm job ends at about 23:30 on 9 October; anything still
  needed after that must be queued as Slurm jobs and written down in
  `STATUS.md`.

## Contract changes

- **E, 9 October 20:05.** Residual-calibration weights for methods 2 and 3
  are saved next to the draws they calibrate, so that E evaluates exactly
  the weights each piece reports: `$P/c/weights_resid.npz` and
  `$P/d/<variant>/weights_resid.npz`, same layout as B's `weights_ref.npz`
  (`w` [N, K] in the record order of `$P/a/records.parquet`, `lambda`, cell
  ids). If the file is absent, E computes the weights itself with
  `b_calibrate.calibrate` and says so in the report. E also reads B's
  sensitivity reports if they are named `$P/b/report_ref_K<k>.json`.

- **A, 9 October 20:10.** (1) Coarse bins: the list in "Targets" has 14
  bins for a variable with negative values (negative, zero, nine tenths up
  to 0.9, then [.9,.99), [.99,.999), [.999,1]) and 13 without; the "13 and
  12" in the text is a miscount. A implements the list as written: 14 and 13
  bins, so 319 cells per (wave, family, group) block and 30,624 cells in
  all. (2) The reference checkpoint was trained with the survey weight
  dropped (`--drop-vars scf_weight`), so the weight is never drawn or used as
  an input; the fixed order is the 24 wealth variables in meta.json's
  generation order (`ctx.order_names` in `a_common.py`), and the model's
  distribution of the 24 is exactly the product of their 24 conditionals.
  (3) Extra files: `ref_draws_b.npz` and `ref_dollars_b.parquet` (a second,
  independent K = 32 set for E's Monte Carlo yardstick) and
  `ref_draws_k128.npz` (K = 128 for B's sensitivity, no dollars), each with
  its own `DONE_` marker. (4) `records.parquet` carries both `d_<v>` (the
  records' reported dollars) and `dec_<v>` (the true tokens decoded to
  dollars the same way draws are), with `nw` and `nw_dec`.

- **Main session, 9 October 20:25. New piece F: from-scratch arms.** John
  (9 October): the expected end state is whole records drawn from the model
  from scratch or close to it, calibrated on whole-record weights. Piece F
  adds two arms beside the completion arm above. Nothing above changes for
  A–E.

  *Arm S0, close to scratch.* Keep each real record's year, `age_range`,
  `married` and `n_dep`; draw the seven incomes and the 24 wealth variables
  (K = 32 per record, fixed order: the shared incomes in meta.json's
  generation order, then the 24 wealth variables as in the completion arm).
  Each record keeps its weight across its draws, as in the completion arm,
  so demographic tables hold by construction; incomes are now targets.

  *Arm S1, from scratch.* Draw whole SCF records for each wave: demographics,
  incomes and wealth, nothing observed except source SCF and the year.
  N synthetic records per wave (start at 8 × the wave's real record count;
  sensitivity 1×, 4×, 8×, and larger if cheap), equal starting weights summing
  to the wave's total weight. One weight per synthetic record; the only
  "record total" constraint is the wave total (B's `calibrate` with the
  wave as one record holding N draws, or F's own variant if B's layout
  cannot hold it).

  *Targets for S0 and S1.* All of A's wealth families, with each draw's
  groups computed from its own drawn values (income group: the drawn
  incomes' total in dollars against the real wave's total-income thresholds
  at 0.5, 0.9, 0.99, 0.999, from A's records), plus new families on the
  observed variables, each computed from the real records:
  - each income's coarse bins (same bin rule as wealth) × income group, age,
    married, dependents, and each other income's 4–5 band grouping;
  - for S1 also: age × married × dependents (one joint table), and the
    wave's total weight.
  Split-off units stay out of every target, as in A. The generator, asked
  for SCF records, also produces split-off-like units; the calibration must
  absorb that, and F reports how much weight it moves for it.

  *Methods.* Method 1 for both arms is required. Method 2 (A's `bias_fn`
  tilt, now on income variables too) and method 3 for the from-scratch arms
  are F's second priority, reusing C's and D's code once theirs is stable;
  coordinate through this section, not by editing their files.

  *Outputs.* `$P/f/<arm>/draws.npz` (S0: [N, K, 31] tokens; S1: [N_syn, 34]
  incl. demographics), groups per draw, targets file for the new families,
  weights, `report.json` (KL from the starting weights per unit of weight,
  effective sample size, infeasible cells by family, time), marker
  `$P/f/DONE_<arm>`, and `F_REPORT.md` in this folder. E adds the F arms to
  `EVAL_REPORT.md` if time allows; otherwise F writes the levels-by-income
  table and the untargeted measures for its arms itself using E's code.

- **Main session, 9 October 20:30. PUF arms — now the priority for every
  piece.** John: the point of this work is completing PUF records. The SCF
  arms above stay (finish what is running; they are the only place with
  known true wealth), but each piece's next work goes to the PUF arms below.

  *Arm P, completing PUF records.* Records: expB *training* rows with source
  PUF (2015), each at its own PUF weight, fixed across its draws. Presented
  to the model with the SCF source tag and year 2015 (test 2's better
  choice, `joint_w1_puf_asSCF`). Draw the 24 wealth variables, fixed order,
  K = 32 (K = 16 if memory forces it; say so). Records whose `age_range` is
  missing (about 42%) keep it unobserved: do not let the sampler fill it
  (pass it as unobserved, not hidden-to-fill), and say how that is done.

  *Targets for arm P.* SCF wealth tables carried onto the PUF population as
  shares. For a cell (family, group, v, coarse bin): s_2013 and s_2016 =
  the SCF training records' (wealth observed) weight share of that bin among
  the group's units in each wave; s_2015 = s_2013/3 + 2·s_2016/3 (as
  test 2's `c_evaluate_puf.py`); target = s_2015 × total PUF weight of the
  group. Groups on the PUF side use the PUF's own values: income group by
  weighted rank of the PUF seven-income total among PUF training records
  (same edges); married, dependents; age family only over PUF records with a
  known age (target scaled to that subset's group weight); `x_<income>`
  families from the PUF's own zero / negative / rank bands. Zero in both
  waves → zero cell.
  Two target sets, both run: **P_full** (all 11 families) and **P_core**
  (income group, age, married, dependents only). Reason: the income
  components are measured differently in the two sources (PUF 34.8% vs SCF
  18.0% nonzero interest and dividends; 15.8% vs 8.3% capital gains), so the
  `x_` families impose SCF wealth-by-income-item patterns on differently
  composed PUF groups. Report both; John decides.

  *Arms PS0 / PS1, PUF from (close to) scratch* — piece F. PS0: keep each
  PUF record's demographics (age where known, married, dependents), draw the
  seven incomes with the PUF source tag and then the 24 wealth variables
  with the SCF tag (source tag switched between steps). PS1: draw
  demographics and incomes as PUF, then wealth as SCF, from nothing but year
  2015. Targets: the PUF training records' own tables for demographics and
  incomes (F's income families computed on PUF truth), plus arm P's SCF
  wealth targets with groups from each draw's own values (income group
  against the PUF's total-income thresholds). PS0 keeps record weights
  fixed; PS1 has one weight per synthetic record.

  *Files.* A: `$P/a_puf/` with the same layout and functions as `$P/a/`
  (records, targets_full, targets_core, cells, ref_draws, dollars at 2015
  as in test 2), markers `DONE_*`. B, C, D: outputs under `$P/<piece>/puf_full/`
  and `puf_core/`. F: `$P/f/ps0/`, `$P/f/ps1/`. E: arm P has no true wealth,
  so the untargeted comparison is against the SCF 2013/2016 interpolated to
  2015 (reuse `../scf_joint/c_evaluate_puf.py`'s measures), net worth levels
  by income group first.

  *Order of work.* A builds `a_puf` now. B runs method 1 on P_core and
  P_full as soon as A lands. C fits method 2 on P (P_core first). D runs
  variant 1 on B's P_core weights first, then variant 2 on P_core; the SCF-
  arm runs only if time remains. F switches to PS0 then PS1, after finishing
  only what is already queued for S0/S1.

- **Main session, 9 October 20:45. REORIENTATION — supersedes the 20:30
  entry's arms and every earlier arm as the main line of work.** John's
  plan, in his words: train a transformer that completes records with all
  the PUF variables and the SCF wealth variables; look at how close those
  records get to ground-truth moments and joint distributions (this is
  pretraining); then post-train with several calibration methods to hit
  those moments and joint distributions. "All the PUF variables" = test 1's
  40 (`../puf_generator/DATA_REPORT.md`, "Variables and generation order").
  The model used so far knows only 10 PUF-side variables (7 summed incomes,
  age, married, dependents), so it is replaced.

  **1. Data — piece G (new).** Dataset `full` under `$T2/data/full/`, test 2's
  layout (`meta.json`, `tokens_*.npy`, `truth_*.npy`, `records_*.parquet`,
  `quantiles.parquet`, `DONE`), so `../scf_joint/b_model.py` and `b_train.py`
  work on it unchanged or with small, documented options.
  - PUF 2015: test 1's 39 non-weight variables, test 1's train / val / test
    split. The weight `S006` is not a model variable (it is the record
    weight; training weights the loss by it, as test 2's `w1` runs).
  - SCF 2013 and 2016 filer tax units (test 2's builder), all five
    implicates at one-fifth weight; split-off units keep their rows with
    wealth unobserved (as test 2). Each SCF unit observes the 24 wealth
    variables plus every one of the 39 PUF variables that the SCF measures
    credibly (G proposes the crosswalk in `g_crosswalk.md`: e.g. wages,
    taxable and tax-exempt interest, dividends, short and long gains,
    Schedule C, farm, rent, pensions, Social Security, unemployment, MARS,
    XTOT / N24, age range, mortgage and other interest paid, real estate
    tax, charity, if the SCF items support them). A PUF variable the SCF
    does not measure is **unobserved** for SCF records, never zero.
  - Amounts as ranks within source and year with zero and negative tokens,
    as test 2 (`../scf_joint/a_tokenizer.py`), so the two sources line up.
    Source tag and numeric year as in test 2.
  - Deliver fast: a first version within about an hour, marker `DONE`; a
    data report `G_DATA_REPORT.md` with the crosswalk and checks (weighted
    totals of each crosswalked item, PUF against SCF filers, nonzero rates).

  **2. Pretraining — piece H (new).** Train the joint model on `full`: base
  size, any-order masking, source tag and year, loss weighted by population
  weight, weight not a variable; early stopping as test 2. One seed first,
  two more queued after. Checkpoint `$T2/model/full/checkpoint_full_w1_seed01.pt`.
  Then completions and generations for the pretraining check:
  - PUF test 1 style: whole PUF records generated from nothing (year 2015,
    PUF tag), and PUF records completed from partial views;
  - PUF training records completed with wealth (SCF tag for the wealth
    steps, as test 2's better choice), year 2015;
  - SCF records' wealth completed from their observed variables.
  H writes `H_MODEL_REPORT.md` (training, sizes, time).

  **3. Pretraining evaluation — piece E.** Against ground truth: PUF
  variables' one-way and multi-way tables, zero patterns and top (reuse
  `../puf_generator/evaluate.py`'s measures) against the PUF training
  records; wealth against SCF truth on SCF records; PUF completed wealth
  against SCF interpolated to 2015 (`../scf_joint/c_evaluate_puf.py`).
  Levels tables by income group first.

  **4. Post-training — pieces A, B, C, D, F.** The same three methods,
  on the full model, once its checkpoint exists:
  - A rebuilds `a_common.py` so dataset and checkpoint are parameters (the
    current model stays usable as a pilot), and builds targets for the full
    model: (i) PUF ground truth — each of the 39 PUF variables' coarse-bin
    distribution crossed with AGI group (E00100 rank: 0.5, 0.9, 0.99,
    0.999), MARS, age range, dependents, and two-way crosses among the main
    income items; (ii) SCF wealth tables carried to the PUF as shares within
    groups, interpolated to 2015, as in the 20:30 entry (core and full sets).
    Outputs under `$P/full/a/`.
  - Arms on the full model: **P** (complete PUF records' wealth; PUF values
    and weights fixed, so only wealth targets bind), **PS0** (keep PUF
    demographics; draw the PUF amounts and wealth; PUF and wealth targets
    bind), **PS1** (whole records from nothing; one weight per record).
    F owns PS0/PS1 sampling; B, C, D run their methods on P first, then on
    PS0/PS1 draws.
  - Outputs under `$P/full/<piece>/<arm>/`.

  **What happens to the current-model work.** Finish only jobs already
  running. Results on the current model are a pilot that checks the code;
  do not start new current-model runs except to test code. Every piece's
  code must take the dataset and checkpoint as parameters.

  **Time.** The driving session ends about 23:30. Everything after must be
  chained Slurm jobs (dependencies on G's `DONE` and H's checkpoint marker
  `$T2/model/full/DONE_seed01`), written down in STATUS.md with how to finish
  by hand.

- **C, 9 October 20:40. What method 2 needs from A's module for the full
  model (please keep these names; tell C in STATUS.md if they change).**
  `c_fit.py` takes `--a-module` (default `a_common`) and passes
  `--dataset`, `--checkpoint`, `--arm`, `--a-dir` to `A.configure(...)` if
  A provides it, and also sets environment variables `POSTTRAIN_DATASET`,
  `POSTTRAIN_CHECKPOINT`, `POSTTRAIN_ARM`, `POSTTRAIN_ADIR` before
  importing. After that it uses only the names in today's `a_common.py`:
  `load_records()` (with `weight`, `wave_i` and the `BLOCK_COLS` block-id
  columns; a block id of -1 = the record is in no group of that family, e.g.
  unknown age), `load_targets()` (one row per cell in cell order, column
  `target`, `family`), `layout()` (`VB`, `vb_start`, `nbins`, `n_cells`,
  and `wbins[v] = (labels, tok2bin)` for every targeted variable),
  `WEALTH` (the targeted variables, in the order of the draws' last
  axis — for PS0/PS1 this list would include the PUF variables), `NV`,
  `true_tokens(recs)` (only for the SCF arm), `contrib_index`,
  `cell_totals`, `load_reference(device, recs=...)` returning `ctx` with
  `order`, `step_var`, `tok_in`, `allowed` [waves, NV, max_v], `wave_i`,
  `device`, `N`, and `_step_logits(model, ctx, cur, hid, ri, j, k, allowed,
  bias_fn)`. Source-tag switching between steps (PS0) belongs inside
  `_step_logits` / `ctx`. Method 2 can tilt any set of drawn variables;
  drawn variables that are not in `WEALTH` are drawn from the reference
  untilted.

- **F, 9 October 20:40. PS0 / PS1 draws on the full model, and what F needs
  from A.** F's sampler is `f_full.py` (dataset directory, checkpoint and
  tokenizer module are parameters). Draws files, `$P/full/f/<arm>/draws.npz`:
  - `tokens`: every slot variable of the full model, in meta.json's slot
    order (`b_model.slot_variables`), int16, -1 where a variable stays
    unobserved. PS0: [N, K, V] for N = the PUF training records in expB-
    style row order (`row` = row of `records_train.parquet`), K = 16; the
    kept demographics (the categoricals, see below) are the record's own,
    everything else is drawn. PS1: [N_syn, V].
  - `logp_var` (float32, the drawn variables' log-probabilities, 0 for kept
    ones), `logp` (float32 sum), `dollars` (float64, PUF variables decoded
    as PUF 2015, wealth as SCF at 2015), `slot_names`, `drawn` (bool [V]),
    `row` (PS0), `seed`.
  - Kept in PS0: every categorical PUF variable (MARS, DSI, XTOT, N24, EIC,
    FDED, AGERANGE, GENDER, EARNSPLIT in test 1's list, whichever `full`
    carries). A kept variable that is unobserved on a record (-1) stays
    unobserved and is never drawn. Drawn: every PUF amount (E00100
    included) with the PUF source tag, then the 24 wealth variables with
    the SCF tag, year 2015, fill order = `fill_order(meta)` with the PUF
    variables first and wealth after.
  - **Request to A:** in the parameterised `a_common`, a builder that takes
    rows of full slot tokens (and their decoded dollars, if A's groups use
    dollars) and returns the cell ids of A's full-model PUF target set,
    computing *every* group from the row's own values:
    `contrib_index_rows(tokens [R, V], dollars [R, V] or None, target_set)
    -> int32 [R, m]`, plus `load_targets_full(target_set)` with a `target`
    column in the same cell order. Cells a row cannot be assigned to (for
    example an age family when its AGERANGE is unobserved) may be returned
    as -1; F points them at one column kept out of the solve. If A's
    builder takes another form, say so here and F adapts `f_full.py`.
  - F's calibration jobs wait on `$T2/model/full/DONE_seed01`, A's
    `$P/full/a/DONE_targets` (F assumes this marker name; A, please confirm
    or correct here) and F's draws.

- **F, 9 October 20:45. B's block check assumes each record sits in one
  group per family.** `b_calibrate.block_check` counts a record's whole
  weight as forced into a block's bins when the record's draws *in that
  block* all fall in them. That holds for completion arms (a record's groups
  are fixed), but not when groups come from drawn values (S0, S1, PS0, PS1,
  and any arm where incomes or demographics are drawn): a record's draws
  spread over several groups of one family, and the check then releases
  feasible cells. On the S0 draws it released 15,539 cells as "block" (job
  28899723, cancelled). F now calls `calibrate_matrix` with a cell table
  without the `bin` column, which skips the check. B: please make the check
  conditional on fixed groups, or document that callers with drawn groups
  must skip it (C and D on PS0/PS1 draws included).

- **H, 9 October 20:46. Pretraining files: checkpoints and completions of
  the `full` model.** All under `$T2/model/full/` (`$T2` =
  `/nfs/roberts/scratch/pi_nrs36/jar335/scf_joint`). Code: `h_train.py`
  (copy of `b_train.py`; the model is `../scf_joint/b_model.py` unchanged,
  so `b_model.load_checkpoint` loads these checkpoints), `h_complete.py`,
  Slurm scripts `h_train.sbatch`, `h_complete.sbatch`, `h_launch.sh`.
  - **Checkpoints.** `checkpoint_full_w1_seedNN.pt` (NN = 01, 02, 03),
    `train_log_full_w1_seedNN.csv` (per epoch; validation loss overall and
    per source), `run_full_w1_seedNN.json`. Marker `DONE_seedNN` once a
    seed's training has finished. Settings: test 2's `joint_w1` (base size,
    any-order hiding of observed variables, source tag and numeric year,
    loss weighted by the records' `weight` column, no weight variable,
    stopping after 15 epochs without improvement; the kept model is the
    best epoch).
  - **Completions**, one set per seed, marker `DONE_comp_seedNN` when all
    four exist. Each is `h_<name>_seedNN.parquet` plus
    `h_<name>_seedNN_tokens.npz` (`tokens` [draws, N, 63] int16 over the 63
    model variables in meta.json order without `source`, -1 = hidden;
    `names`; `rows`; `fill` = filled variables in fill order) and
    `h_<name>_seedNN_info.json` (settings, order, timings, scores).
    1. `h_gen_puf_seedNN` — **(a) whole PUF records from nothing.** As many
       records as PUF training records, tag PUF, year 2015, one draw. The 39
       PUF variables drawn in meta.json's `generation_order` (test 1's
       order); wealth not drawn. Columns: `gen_id`, `source`, `year`,
       `weight` (every record the same: total PUF training weight / N, so the
       file has the training population total), `draw` (0), and for each of
       the 39 variables its value (dollars, or the category value) and
       `tok_<name>`. Note: AGERANGE, GENDER and EARNSPLIT are drawn for every
       generated record, while the PUF truth has them only for records in the
       demographic file; compare on the subset with them known.
    2. `h_puf_wealth_seedNN` — **(b) PUF training records completed with
       wealth.** Every PUF row of `records_train.parquet`; the 24 wealth
       variables drawn in the wealth fill order with the SCF tag at year 2015
       (test 2's `puf_asSCF`), decoded at 2015 (SCF 2013 and 2016 quantile
       functions interpolated, as test 2); five draws. PUF variables keep
       their true values; AGERANGE, GENDER, EARNSPLIT stay unobserved (NaN)
       where the PUF lacks them, and are not filled.
    3. `h_scf_train_seedNN`, `h_scf_test_seedNN` — **(c) SCF records'
       wealth from their observed variables.** Every SCF row of the train
       (test) split; each record's 24 wealth values set aside and redrawn,
       own source tag and year, five draws. PUF variables the SCF does not
       measure stay hidden (NaN), not filled. Split-off units (no true
       wealth) are completed too: filter on `wealth_observed`. The info file
       gives the completion log-likelihood of the true wealth (nats per
       record, plain and population-weighted).
    Columns of 2 and 3: `row` (0-based row in `records_<split>.parquet`),
    the record columns of that file (`source`, `year`, `hh_id`,
    `implicate`, `unit`, `weight`, `half`, `age`, `wealth_observed`), `draw`
    (0–4), and for each of the 63 variables its value (true where observed,
    decoded draw where filled, NaN where neither), `tok_<name>`,
    `obs_<name>` (True = the record's own value). Draws are restricted to
    codes the decoder can turn into dollars, as in test 2. Decoding uses
    `g_tokenizer.py`.
  - Not done by H: test 1's "PUF records completed from partial views"
    (item 2 of the 20:45 entry), and any source-tag switching between steps
    (PS0/PS1, piece F). `h_complete.py --source-by-step group:SOURCE` exists
    for that but is untested.

- **Main session, 9 October 20:55. Version 2 of the data is the main
  model.** John chose option (b) for Schedule E: total Schedule E income
  (PUF E02000; SCF X5714) is added as a 40th PUF-side variable observed in
  both sources, in `$T2/data/full_v2/`, with N24, GENDER, E02400 and E01700
  now SCF-observed too (15 + 1 shared variables). H's chain trains it into
  `$T2/model/full_v2/` (tag `full_v2_w1`). The `full` (v1) chains already
  queued run as they are and are the fallback. **Every downstream piece
  (A, B, C, D, E, F) also queues its chain for v2**: dataset `full_v2`,
  checkpoint `$T2/model/full_v2/checkpoint_full_v2_w1_seed01.pt`, marker
  `$T2/model/full_v2/DONE_seed01`, outputs under `$P/full_v2/<piece>/`.
  A's PUF targets for v2 include the new variable.

- **A, 9 October 21:00. Target sets for the full model (`a_full.py`).**
  How item 4 of the 20:45 entry is implemented; B, C, D, F read these.
  Files in `$P/<version>/a/` (`full`, `full_v2`): `records_puf.parquet`
  (PUF training records, 2015, own weight, in the order every draws file
  uses; group columns `g_<set>_<family>`, -1 = no group; `tok_<var>` and
  `d_<var>` for the PUF variables), `targets_puf.parquet`,
  `targets_wcore.parquet`, `targets_wfull.parquet` (one row per cell:
  `cell`, `table`, `family`, `group`, `variable`, `bin`, labels, `target`;
  the wealth sets also `s_2013`, `s_2016`, `s_2015`, SCF record counts and
  group weights, `puf_group_weight`), `cells_full.json` (layout and fitted
  income thresholds), marker `DONE_targets`.
  Choices that the 20:45 entry left open:
  (1) All PUF training records are included, dependent filers too (DSI = 1);
  the SCF has none, so in the wealth sets they take the shares of SCF units
  in their income, marital and dependents groups. Their age codes mean other
  ranges, so they are in no age group.
  (2) The SCF does not measure AGI, so the wealth sets' income group ranks
  the sum of the AGI-type items both sources measure (version 1: wages,
  Schedule C, long-term gains, dividends, taxable interest, unemployment
  compensation; version 2 adds whatever more both measure), each source
  ranked within itself (SCF within wave). The PUF ground-truth set's AGI
  group is E00100 among PUF records.
  (3) Number of dependents = XTOT minus the filer and, for joint returns,
  the spouse, capped at 3 (both sources).
  (4) Age family: AGERANGE codes 1 to 6 of non-dependent filers; records
  with age missing are in no age group, and its targets are scaled to the
  known-age records' weight. In the reference draws, missing age stays a
  hidden input and is never drawn.
  (5) `wfull`'s `x_` families: every amount both sources measure (version
  1: E00200, E00900, P23250, E00600, E00300, E00400, E02300). The PUF set's
  two-way crosses: each of the main income items (E00200, E00900, E26270,
  P23250, P22250, E00600, E00300, E01500, E02400, E25850, and E02000 in
  version 2) by each one's band.
  (6) Contribution: `ts = a_full.rebuild_set(name)`;
  `ts.contrib_index(G, view, tokens, names)` returns cell ids per draw with
  `ts.n_cells` meaning "no cell" (drop it, or pad lambda with a zero);
  `a_full.contrib_csr(idx, ts.n_cells)` gives scipy CSR. For arm P, `G` is
  the records' group columns for the set, repeated over draws.

- **F, 9 October 21:15. F uses `a_full.py` directly; one bug worked around.**
  F's request of 20:40 is withdrawn: `f_full.a_targets` builds a view of
  each draw with `a_full.view_from_tokens`, rebuilds A's target sets with
  `rebuild_set` and takes cell ids from `TargetSet.contrib_index`. F's
  target sets: `core` = A's `puf` + `wcore`, `full` = `puf` + `wfull`.
  Checked: the PUF training records pushed through this path reproduce A's
  `puf` targets exactly (largest difference 0). **For A:**
  `a_full.rebuild_set` sets a wealth set's rank-family measure to the text
  of `notes["income_group_measure_wealth_tables"]` ("sum of [...] (SCF
  lacks E00100)") instead of the list of items, so grouping draws with it
  fails with a KeyError; F rebuilds the list as `build` does
  (`common_income_items` that are in `MEASURE_ITEMS`). Anyone grouping draws
  with `rebuild_set` on a wealth set needs the same fix.

- **H, 9 October 21:25. Pretraining files for data version 2 (the main
  model).** Same files, columns and markers as the "H, 20:46" entry, under
  `$T2/model/full_v2/` with tag `full_v2_w1`:
  `checkpoint_full_v2_w1_seedNN.pt`, `DONE_seedNN`,
  `h_<gen_puf|puf_wealth|scf_train|scf_test>_seedNN.*`, `DONE_comp_seedNN`.
  Differences that follow from the data: 64 model variables (tokens arrays
  are [draws, N, 64]); `h_gen_puf` draws the 40 PUF-side variables in
  `full_v2`'s `generation_order` (test 1's order with `E02000` after
  `E00900`); in `h_scf_*` the PUF-side variables the SCF does not measure
  (23 of 40) stay hidden. The model reads `$T2/model/full_v2/data/`, a copy
  of `$T2/data/full_v2/` taken when training started.

- **B, 9 October 22:50. Method 1 outputs and what "exact" means in them.**
  (1) Arm P on the full model: `$P/<version>/b/P_core/` (target set
  `wcore`) and `$P/<version>/b/P_full/` (`wfull`), each with
  `weights_P.npz` (`w` [N, K] in the row order of piece A's
  `records_puf.parquet`, `lambda`, `cell_id`, `cell_status`),
  `report_P.json`, `report_P_K8.json`, `report_P_K16.json`, markers
  `DONE_P` and `DONE_P_ksens`. (2) The weights are not exact: within the
  45-minute solving budget no run hit every feasible cell to 1e-6. Every
  cell carries a status: `active` (hit to 1e-6), `zero`, `zero_relaxed`,
  `unreachable`, `above_reach`, `below_forced`, `block` (shown impossible
  before the solve), `joint` (released during the solve), `missed` (still
  in the solve but off target when time ran out), `no_target` (NaN target).
  Residual calibration by C, D and F through `calibrate`/`calibrate_matrix`
  returns the same statuses. See `B_REPORT.md`.
