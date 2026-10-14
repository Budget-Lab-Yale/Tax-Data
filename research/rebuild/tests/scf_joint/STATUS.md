---
title: "Test 2 — status for resuming"
role: status
workstream: rebuild
status: current
updated: 2026-10-09
sot: research/rebuild/tests/scf_joint/SPEC.md
supersedes: []
superseded_by: null
---

# Test 2 status

## 9 October: focused setup audit

- Job `28891998` overfit 64 complete, distinct-context SCF training rows
  with all 24 wealth fields hidden. The existing base transformer reached
  100% token and complete-record accuracy after 250 steps; result in
  `logs/b_overfit_result.json`. This establishes in-sample memorization
  capacity for tokenized records, not held-out accuracy.
- Earlier completion-task-only training overfit rapidly: seed 1's training
  loss decreased to 0.68 by epoch 20 while validation loss rose to 2.07;
  best checkpoint was epoch 5. Held-out wealth completion NLL was 38.75
  versus 35.51 for random-mask baseline.
- The PUF–SCF shared income measurement mismatch is substantial, especially
  exact zeros: interest/dividend nonzero shares 34.8% versus 18.0% and
  capital-gain nonzero shares 15.8% versus 8.3%. Source-year rank bins do
  not correct these prevalence differences. `a_crosswalk.md` contains the
  full weighted comparison. This cannot explain the transformer's weaker
  held-out SCF performance against DRF on the same shared input categories.
  Raw production Stage 2 DRF before calibration has 16.3% mean-wealth
  error by income group and 1.90-point holding error; the 7.2% and
  1.84-point figures are for an experimental survey-weighted DRF variant.
  Interpretation in `EVAL_REPORT.md`.

## 9 October: autonomous follow-up authorized while user offline

- User supplied a second opinion about autoregressive joint models, then
  authorized trying alternatives while away. No production defaults changed.
- Array `28887140` (15 tasks, at most four concurrently), `b_followup.sbatch`:
  - 0–2: expB, no source label, existing random-mask objective;
  - 3–5 and 6–8: expA_clean and expB, no source label, corrected orderless objective;
  - 9–11 and 12–14: expA_clean and expB, no source label, fixed-order objective.
  All base size, full population weights, three seeds, five completions.
- `--mask-style orderless`: uniform number of hidden observed fields from
  1 through D, uniform subset of that size, loss multiplied by D/hidden.
  This estimates the average full sequence NLL over random orders on each
  observed subset. Existing masking upweights tasks with more hidden fields.
- `--mask-style ordered`: uniform next observed variable in the fixed
  completion order; only earlier observed variables visible, only the next
  variable scored, loss multiplied by D. Both corrections retain a fixed
  weighted-observed-slot denominator. Tests in `b_mask_check.py` passed.
  With missing fields in the middle these remain observed-subsequence
  objectives, not exact marginal likelihoods of one fixed-order full joint.
- CPU oracle jobs `28887120` (expA_clean), `28887121` (expB): decode the true
  held-out wealth tokens with no model prediction, to isolate wealth
  representation loss. Income information loss is not measured by this oracle.
- Outputs isolated under `$S/followup_20261009/{model,eval}`. Every GPU task
  trains, completes and evaluates its SCF records; expB also completes PUF
  records and runs the existing PUF-vs-SCF aggregate comparison (not truth).
  Logs `$S/logs/b_followup_28887140_<task>.{out,err}` and
  `$S/logs/c_py_2888712{0,1}.{out,err}`. Oracle jobs completed: wealth
  decoding alone gives 0.36% / 0.72% mean-net-worth error by income group
  (clean / real data), far below model errors; does not test income inputs.
- After array completion: CPU calibration job `28887348` runs
  `c_followup_tilt.sh`: calibrates the five draws per record to four true
  income-group wealth means, preserving each record's original population
  mass. Explicit oracle-target diagnostic, not out-of-sample target prediction.
  Compares untargeted relationships and reports feasible bounds, KL and
  effective draws. Outputs `tilt_*/summary.json` in the follow-up eval tree.
- Sampling-order probe array `28887376` (six tasks, two at a time), also
  dependent on the training array: random and corrected-orderless expB
  checkpoints, three seeds each, reverse only the wealth generation order.
  `b_complete.py --reverse-wealth-order` is an opt-in diagnostic; default
  order unchanged. Tests whether arbitrary-order queries are close to
  agreeing. No fixed-order model is sampled in an untrained reverse order.
- All 15 training/evaluation tasks and both dependent diagnostics completed
  with exit code zero. Full results and limitations are in `EVAL_REPORT.md`
  under "9 October: follow-up training and calibration checks". The
  source-labeled transformer has 20.8% held-out SCF mean-wealth error by
  income group; no-source random 21.8%; corrected orderless 24.1%; fixed
  order 16.4% with worse amount ranks and correlation; survey-weighted
  forest 7.2%. Calibration to four *supplied* true SCF means was feasible
  in all cases with little loss of effective draws. Reversing wealth
  generation order changed untargeted outcomes, including mean-wealth
  error from 21.8% to 24.9% for no-source random masks.

## 9 October, 17:04: source-label ablation (requested in conceptual discussion)

- User requested the clean split-SCF experiment without source labels.
- Added `b_train.py --no-source`, saved as `use_source=False` in the model
  configuration. Source metadata remains available to select and evaluate
  records, but the source embedding does not enter the network. It stays
  allocated to preserve all other seeded initialization draws. Old
  checkpoints default to source embeddings enabled.
- Matched to `expA_clean/joint_w1`, seeds 1–3: same full population weights,
  dropped `scf_weight` field, base model, masking, early stopping, and five
  completion draws. Completion-task fraction remains zero.
- Submitted array `28886439` with `b_no_source.sbatch`. Output is isolated
  under `$S/no_source/{model,eval}/expA_clean` to avoid existing watchers.
  Logs: `$S/logs/b_no_source_28886439_<seed>.{out,err}`. Each task checks
  source invariance and checkpoint compatibility, trains, completes, then
  evaluates the plain completion using the existing C evaluator.
- This tests removal of the explicit source embedding, not an
  observed-data-likelihood model; patterns of missing variables can still
  reveal source. All three runs finished and were evaluated. Results in
  `EVAL_REPORT.md`, section "9 October: removing the explicit source label".
  Relative to original source labels: holding gap 5.06 → 3.96 pp; income–wealth
  rank-correlation gap 0.092 → 0.053; top 1% wealth share 34.3% → 39.1%
  (truth 39.2%). Roughly matches the existing as-SCF workaround. Mean net
  worth by income group remains inaccurate (44.3% average error).

Contract: `SPEC.md`. Plan and John's decisions: `PLAN.md`. Scratch:
`/nfs/roberts/scratch/pi_nrs36/jar335/scf_joint/` (`$S`).

## 9 October, 12:45 (main session)

Four Opus subagents started: A (data), B (model), C (evaluation), D
(comparison methods). The driving session's Slurm job ends at about 16:15;
each piece appends its own status below, hourly.

## 9 October, 13:00 (piece A, data)

- **`expA_clean` is ready, provisional**, in `$S/data/expA_clean/` (meta.json,
  tokens/truth/records for train, val, test, quantiles.parquet, roundtrip.csv).
  Tokens will not change in the final version. The wealth dollar columns in
  `records_*.parquet` and `quantiles.parquet` will be rescaled (see below), so
  anything in dollars should be re-read when the final version lands.
- Tokenizer: `research/rebuild/tests/scf_joint/a_tokenizer.py`
  (`load_meta`, `load_quantiles`, `encode`, `decode`).
- Found: the SCFP summary files on the cluster are all in 2022 dollars (SCFP
  income is 1.249 times the raw 2015 interview amounts for the 2016 wave, 1.292
  for 2013). Incomes in these datasets come from the raw interview items (tax-year
  dollars); wealth comes from SCFP and will be converted back to each survey
  year's own dollars. Job 28867904 measures the factors.
- SCF tax units for 2013, 2016, 2019 built: `$S/data/scf_units/` (checks in
  `checks_<wave>.csv`). Code: `a_scf_tax_units.R` (copy of the builder),
  `a_build_units.R`, `a_build_units.sbatch`; datasets: `a_prepare.py`,
  `a_prepare.sbatch`.

## 9 October, 13:00 addendum (piece A)

`expA_clean` will be rewritten once more at about 13:10 (job 28868089, after
the tax-unit rebuild 28868086/87): the `ui_other` income is narrowed to the
parts that have a tax-return counterpart (unemployment, workers'
compensation, child support and alimony; welfare and "other" income are
dropped from it), so the `ui_other` tokens change; wealth dollars are now in
2016 dollars. Everything else stays as it is. Pieces reading `expA_clean`
before then can develop on the current files, but should re-read them after
13:10. A line saying "expA_clean final" will follow, with a `DONE` marker.

## 9 October, 13:05 (piece C, evaluation)

- **Built:** `c_common.py` (shared measures), `c_evaluate.py` (completed vs
  true, plus the noise yardsticks), `c_evaluate_puf.py` (experiment B, PUF
  completed at 2015 against SCF 2013, 2016 and their interpolation),
  `c_compare.py` (side-by-side tables per dataset, written into
  `EVAL_REPORT.md`), `c_watch.sh` (evaluates whatever is ready), sbatch
  wrappers `c_py.sbatch` and `c_shell.sbatch`, and `c_make_dev.py`
  (fabricated data). Contract note added to `SPEC.md` (completed files carry
  `row` and `tok_<variable>`; B's and D's code already does this).
- **Checked on fabricated data** (`$S/dev/c/`): a perfect copy of the truth
  gives zero gaps and classifier AUC 0.500; shuffled wealth gives large
  gaps; a completion from the true model gives gaps about half the
  sampling-noise yardstick (written up in `EVAL_REPORT.md`).
- **Done on real data:** noise yardsticks for the provisional `expA_clean`
  (`$S/eval/expA_clean/ref_sampling`, `ref_shuffled`); tables in
  `EVAL_REPORT.md`.
- **Running:** job 28868114, `c_watch.sh 70`
  (log `$S/logs/c_sh_28868114.out`): every 10 minutes for about 12 hours
  it runs the yardsticks for any new or changed dataset in `$S/data/`,
  evaluates every new or changed `completed_*.parquet` in `$S/model/<ds>/`
  and `$S/comp/<ds>/` (label `model_<stem>` or `comp_<stem>`; for `expB`
  also the PUF rows), and refreshes the tables in `EVAL_REPORT.md`. When A
  replaces the provisional data, everything is re-run automatically.
- **To finish by hand** (if the watcher has stopped), from this folder:
  `sbatch c_shell.sbatch c_watch.sh 1` (one pass over everything ready).

## 9 October, 13:07 (piece A, data)

**expA_clean final.** All five datasets are built and have `DONE` markers:
`$S/data/{expA_clean,expA_noise,expA_recat,expA_unrep,expB}/` (build job
28868562, checks job 28868563, logs `$S/logs/a_prepare_28868562.out`,
`a_verify_28868563.out`). Since the 13:00 provisional version: `ui_other`
narrowed, wealth in each wave's own dollars, one more positive rank bin at the
top (0.99999) and finer negative bins at the large-loss end (vocabulary sizes
changed: read them from `meta.json`). `expB` uses the crosswalk proposed in
`a_crosswalk.md`; if John changes it, `expB` is rebuilt with the same layout.

Sizes: experiment A, train 31,299 (half 1: 15,653), val 3,515, test 17,443 (all
of half 1); `expB`, train 208,387 (PUF 160,943, SCF 47,444), val 26,802, test
33,545 (PUF 20,130, SCF 13,415).

Two things for B, C and D: (1) about 42% of PUF records have no `age_range`
(not in the demographic file; token -1), as in test 1. (2) In experiment A,
half 1's wealth decoded to dollars uses half 2's quantile functions, so dollar
totals of even the true half-1 tokens differ from half 1's true dollars by the
half-to-half sampling difference (credit lines -41%, defined-benefit pensions
+12%, other financial assets -8%; see `roundtrip.csv`, rows with source
`SCF_H1`). Compare in rank tokens, or against the decoded truth, where this
matters.

To rebuild everything (about 5 minutes):
```
cd /nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data/research/rebuild/tests/scf_joint
for w in 2016 2013 2019; do sbatch a_build_units.sbatch $w; done        # tax units (2-3 min each)
sbatch a_prepare.sbatch crosswalk expA_clean expB expA_noise expA_recat expA_unrep
sbatch a_verify.sbatch expA_clean expB expA_noise expA_recat expA_unrep
```
(`a_prepare.py` removes `DONE`; recreate by hand after checking.)
Still to do by A: `DATA_REPORT.md`.

## 9 October, 13:10 (piece A)

Correction to the line above: the five datasets are being rewritten once more
(job 28868774, about 3 minutes; `DONE` markers removed until it finishes and
is checked). The only change: the random tie-breaking in ranking now uses one
random stream per variable, so in the experiment A variants only the altered
incomes differ from `expA_clean` (before, unaltered incomes also differed by
tie-breaking noise in 2 to 6% of records). Layout and vocabularies are
unchanged. `DONE` markers will be restored after the checks.

## 9 October, 13:15 (piece B, model)

- **Built:** `b_model.py` (test 1's model plus a source embedding, a learned
  function of the year, and unobserved variables always hidden and never
  scored), `b_train.py`, `b_complete.py` (plain and weight-corrected
  completion, completion log-likelihood of the true values),
  `b_make_dev.py` and `b_check_dev.py` (fabricated-data check),
  `b_summarize.py` (tables and training curves), and Slurm scripts
  `b_gpu.sbatch`, `b_cpu.sbatch`, `b_pipeline.sbatch` (train one model,
  then write its completions), `b_launch.sbatch` (waits for a dataset's
  `DONE` marker, then submits the pipelines for three training seeds).
  File names and columns: "Contract changes", B 13:05, in `SPEC.md`.
- **Checked on fabricated data** (`$S/dev/b_expB*`): the planted link
  between interest and dividends and equity holding is recovered in the
  completed PUF records, and the weight correction moves the equity
  holding share from 0.54 (plain) to 0.45 against a true 0.44.
- **First real runs were cancelled:** A rewrote all five datasets at 13:10
  while they were starting. Their outputs are set aside in
  `$S/dev/b_stale_1310/` and `$S/dev/b_provisional_expA_clean/` (do not use).
- **Queued:** launcher jobs 28868956 (expA_clean: joint, half2,
  joint_small, half2_small), 28868957 (expB: joint, scfonly,
  scfonly_small), 28868958/9/60 (expA_noise, expA_recat, expA_unrep:
  joint). Each waits for `DONE` with no newer data file, then submits
  `b_pipeline.sbatch` for seeds 1–3; the job ids go to `$S/logs/b_jobs.txt`,
  logs to `$S/logs/b_pipe_<job>.out`. Outputs in `$S/model/<dataset>/`.
- **To finish by hand** if a launcher has died, from this folder:
  `sbatch b_pipeline.sbatch <dataset> <config> <seed>` for each missing
  run, then `sbatch b_cpu.sbatch b_summarize.py` for the tables.

## 9 October, 13:12 (piece A)

**All five datasets final, `DONE` markers restored**: `$S/data/expA_clean`,
`expA_noise`, `expA_recat`, `expA_unrep`, `expB` (build job 28868774, checks
28868775, summary 28868776). In the variants, only the altered incomes differ
from `expA_clean` now. Writing `DATA_REPORT.md` next.

## 9 October, 13:16 (piece A, data)

**Finished.** Everything in SPEC.md section A: SCF tax units 2013, 2016, 2019
with checks; the five datasets with `DONE` markers (13:12); `a_tokenizer.py`;
`a_crosswalk.md` (proposal for John's review); `DATA_REPORT.md` (checks,
round trip, and the decisions for John). Nothing of piece A is running.

Decisions waiting for John (DATA_REPORT.md, section 6): keep, drop, or hide
the wealth of split-off adults (zero wealth by construction, 13% of SCF
filer units); the filing rule for them; the crosswalk. If any changes, rerun
`a_build_units.sbatch` (only for the filing rule) and
`a_prepare.sbatch crosswalk expA_clean expB expA_noise expA_recat expA_unrep`,
then `a_verify.sbatch` and restore `DONE` by hand. That rewrites the data
under B's runs, so stop or restart them first.

## 9 October, 13:19 (piece A, data)

**Data being rebuilt; `DONE` markers removed from all five datasets.** John
decided that split-off SCF units (zero wealth by construction) keep their rows
but have all 24 wealth variables unobserved (see SPEC.md, Contract changes,
A 13:19). `expA_clean` first, then the other four; a line follows when each is
back.

## 9 October, 13:25 (piece B, model)

- All runs started at 13:14 were cancelled: A is rebuilding the datasets
  (split-off SCF units' wealth unobserved). Their outputs are set aside in
  `$S/dev/b_stale_1320/` (do not use).
- Added `b_complete.py --complete-as-source <name>`; completions with tag
  suffix `_asSCF` (experiment A `joint_asSCF`, experiment B
  `joint_puf_asSCF`) are written by `b_pipeline.sbatch` alongside the
  own-source ones. Contract change B 13:25 in `SPEC.md`.
- **Queued:** launchers 28869280 (expA_clean: joint, half2, joint_small,
  half2_small), 28869281 (expB: joint, scfonly, scfonly_small), 28869282,
  28869283, 28869284 (expA_noise, expA_recat, expA_unrep: joint). Each
  waits for a `DONE` marker written after 13:20 with no newer data file,
  then submits `b_pipeline.sbatch` for seeds 1–3 (ids in
  `$S/logs/b_jobs.txt`).

## 9 October, 13:21 (piece A)

**`expA_clean` final again, `DONE` restored** (job 28869261, checks
28869262), with split-off units' wealth unobserved and a `wealth_observed`
column in the records files. Half 1 (test): 17,443 rows, of which 15,380 have
true wealth and are graded and 2,063 are split-off units (completed, not
graded). Train 31,299 rows (27,583 with wealth observed), val 3,515 (3,108).
The other four datasets are being rebuilt (job 28869263, checks 28869264).

## 9 October, 13:23 (piece A)

**All five datasets final again, `DONE` restored** (expA_clean: job
28869261; the other four: 28869263, checks 28869264, summary 28869265), with
split-off units' wealth unobserved. Graded rows (wealth observed): experiment A
test 15,380 of 17,443 half-1 rows (2,063 split-off rows completed, not
graded); `expB` SCF test 12,021 of 13,415 (1,394 split-off). `DATA_REPORT.md`
updated. Nothing of piece A is running.

## 9 October, 13:28 (piece D, comparison methods)

- **Built:** `d_drf.R` (production method refit, Stage 2 of `wealth.R`
  without the tilt) with `d_drf_prep.py` (inputs and a check that income
  dollars match the income tokens) and `d_drf_finish.py` (encodes tokens,
  writes the completed file), run by `d_drf.sbatch`; `d_seqtrees.py`
  (sequential gradient-boosted tree classifiers in rank tokens);
  `d_common.py`; `d_check.py` (quick holding-rate check against truth);
  `d_submit.sh` (queues one dataset); `d_py.sbatch`; `d_devdata.py`
  (fabricated development data). Draft `COMPARATORS_REPORT.md`.
- Tested end to end on fabricated data and on the provisional `expA_clean`.
  Everything fit before A's 13:19 split-off change was deleted; both
  methods now train only on units with wealth observed.
- **Queued on the final data (13:27)**, outputs in `$S/comp/<dataset>/`,
  logs `$S/logs/d_py_<job>.out` and `$S/logs/d_drf_<job>.out`; job ids in
  `$S/logs/d_submit_*.txt`:
  - `expA_clean`: prep 28869508; production method seeds 1-3: 28869509,
    28869511, 28869513; sequential trees 28869510, 28869512, 28869514.
  - `expB` (SCF test units and PUF test records): prep 28869515;
    production 28869516, 28869518, 28869520; trees 28869517, 28869519,
    28869521.
  - variants `expA_noise`, `expA_recat`, `expA_unrep`: see
    `$S/logs/d_submit_variants.txt` (production runs wait for the matching
    `expA_clean` seed and reuse its forests).
- **To finish by hand** if anything failed, from this folder:
  `bash d_submit.sh <dataset> [--include-puf]` (expB needs `--include-puf`);
  for a variant reusing forests,
  `DRF_CACHE_FROM=expA_clean sbatch d_drf.sbatch <dataset> <seed> 5` after
  `sbatch d_py.sbatch d_drf_prep.py --dataset <dataset>`.

## 9 October, 13:45 (piece C, evaluation)

- **Split-off units (John's decision, A 13:19):** grading now uses only
  records with `wealth_observed`; the count left out is logged and stored
  in each `summary.json` (`n_excluded_wealth_unobserved`; in `expA_clean`
  2,063 of 17,443 half-1 records, 13.6% of the weight; in `expB` test 1,394
  of 13,415 SCF records). Completed rows for those units are set aside.
  For the PUF comparison, SCF income groups are ranked over all SCF filer
  units (split-offs included, as on the PUF side) and the wealth statistics
  then use units with wealth observed; the report states this mismatch.
- **Added:** a "top rank band" measure (share of each component's holders
  in its top 1% and 0.1% rank bands, against the truth), because the first
  results showed net worth at the top far too high for the model in dollars.
  If a completed file has no dollars (B's decode not yet run), C decodes
  the tokens itself with `a_tokenizer.decode`.
- **Running:** one watcher per dataset, `c_watch.sh 70 <dataset>`, jobs
  28870688 (expA_clean), 28870689 (expA_noise), 28870690 (expA_recat),
  28870691 (expA_unrep), 28870692 (expB); logs `$S/logs/c_sh_<job>.out`.
  They wait for A's `DONE` marker, redo the yardsticks when the data change,
  evaluate every new or changed completed file, and refresh `EVAL_REPORT.md`
  (locked so they do not overwrite each other). They were restarted at 13:45
  to re-evaluate everything with the new measure.
- **To finish by hand** if the watchers are gone:
  `sbatch c_shell.sbatch c_watch.sh 1` (one pass over every dataset).

## 9 October, 13:55 (piece B, model)

- **Finished on the final data (A's rebuild of 13:21–13:24):** all training
  runs, three seeds each: expA_clean (joint, half2, joint_small,
  half2_small), expA_noise / expA_recat / expA_unrep (joint), expB (joint,
  scfonly, scfonly_small); and the experiment A completions, own source and
  `_asSCF`. Outputs in `$S/model/<dataset>/`; tables in
  `$S/model/b_summary_runs.csv` and `b_summary_completions.csv`; my quick
  check of experiment A in `$S/model/expA_clean/b_quick_check.csv`.
- **Rewriting the experiment B completions** (decoder failures, fixed by
  drawing only decodable codes; SPEC.md contract change B 13:45): jobs
  28870679–28870687, `SKIP_TRAIN=1 b_pipeline.sbatch expB <config> <seed>`.
  Two experiment A pipelines still finishing: 28869485 (expA_recat joint
  seed 2), 28869476 (expA_unrep joint seed 1, requeued after pre-emption).
- **First reading (details in MODEL_REPORT.md once written):** on the true
  half-1 wealth, the completion log-likelihood is best for the joint model
  with half 1 presented as SCF (34.77 nats per record, mean of 3 seeds),
  then joint as its own source (35.15), the half-2 benchmark (35.40; small
  35.27). In experiment B the joint and SCF-only models score the same on
  the SCF test units (34.32). Completed net worth is far too high at the
  top in every configuration: 2–3.5% of weight above the true 99th
  percentile and 0.3–1.3% above the true 99.9th; the weight correction
  reduces this only a little.
- **To finish by hand** if jobs died: `sbatch --export=ALL,SKIP_TRAIN=1
  b_pipeline.sbatch expB <joint|scfonly|scfonly_small> <1|2|3>`; then
  `sbatch b_cpu.sbatch b_summarize.py` and `sbatch --mem=48G b_cpu.sbatch
  b_quick_check.py --dataset <dataset>`.

## 9 October, 14:18 (piece D)

- **All comparison runs are finished** on the final data: both methods,
  seeds 1-3, five draws per record, for `expA_clean`, `expA_noise`,
  `expA_recat`, `expA_unrep` and `expB` (SCF test units, and PUF test
  records in `completed_<method>-puf_seed<NN>.parquet`). Files in
  `$S/comp/<dataset>/`; C's watcher picks them up. Naming recorded in
  `SPEC.md` "Contract changes" (D, 14:10).
- First look (`d_check.py`, weighted mean net worth of graded half-1
  units, truth $725k): the production method without its calibration
  step gives $1.01-1.06M (about +40%); the sequential trees give
  $0.53-0.54M (about -26%). Experiment B SCF test units (truth $661k):
  $0.78-0.82M and $0.50-0.51M. Diagnosing the production method's
  overstatement now (dev runs under `$S/dev/d/`, not in `$S/comp/`).
- Nothing of D is queued that the evaluation depends on.

## 9 October, 14:15 (piece B): why the completions are too wealthy at the top

Experiment A, `expA_clean`, half 1 records with true wealth. Files:
`$S/model/expA_clean/b_quick_check.csv`, `b_diag_weight.csv`,
`calib_top_checkpoint_*_asSCF.csv`; scripts `b_quick_check.py`,
`b_diag_weight.py`, `b_calib_top.py`.

1. **The drawn weight, given incomes, matches the sample.** In the plain
   completions, 14.5–14.9% of draws get a weight code in the lowest eighth
   of SCF weights (mostly the over-sampled wealthy), against 13.2% of half
   1's records truly.
2. **Low drawn weight goes with high wealth, as it should; high drawn
   weight does not exclude it enough.** Share of records above the true
   99th percentile of net worth ($10.7 million), by weight code:
   lowest eighth 0.68 completed against 0.71 true; middle 0.07 against
   0.04; highest half 0.016–0.033 against 0.0025. The highest-weight half
   of records (ordinary households, most of the population weight) gets 6
   to 13 times too many top-wealth draws. Asked directly for the
   probability of a top-1% code given the incomes *and the true weight*,
   the model gives 0.0042 on average over the 24 components (half-2
   benchmark 0.0034) against a true weighted share of 0.0028. So the
   excess is in what the model learned for high-weight records, not only in
   the correction.
3. **The weight correction cannot fix this, and partly makes it worse.**
   Its selection uses about 16 of the 20 draws on average, so it has room
   to act, and it does favour high-weight draws (lowest-eighth share falls
   from 15% to 4.5%). But for a record whose incomes look wealthy, the
   high-weight draws it prefers are also wealthy (the leak in point 2), so
   among high-weight draws the share above the 99th percentile rises from
   1.6% to 5.1%. The top moves from one weight group to another instead of
   leaving.
4. **The plain completion reproduces the sample, slightly overshooting.**
   Unweighted share of records above the true 99th percentile: 11.2% in the
   truth (the over-sample), 13–14% in plain completions; the top-1%-band
   probability without the weight averages 0.0293 (joint) and 0.0253
   (half-2 benchmark) against 0.0253 in the sample.

Reading: the model learns the SCF sample well enough on average, but in
the rare top bands it gives ordinary (high-weight) households a small
probability of top wealth, borrowed from the many over-sampled wealthy
records; because those households carry nearly all the population weight,
the small leak becomes a large excess at the top. Weighted training
(John's direction) addresses exactly this.

**Running (one seed each, `expA_clean`, weight not a model variable, plain
completion only):** loss weighted by the population weight (`_w1`), by
its square root (`_wsqrt`), and unweighted with the weight dropped
(`_nowt`, control), each for the joint model and the half-2 benchmark:
jobs 28872342–28872347, logs `$S/logs/b_pipe_<job>.out`. I weight the loss
(each record's loss times its weight, normalised to mean 1), not the
batch sampling, and the validation loss used for stopping is weighted the
same way.

## 9 October, 14:14 (piece D)

- **Production-method files withdrawn and being rerun.** The first run
  ranked donors among wealth-observed SCF units only but targets among all
  units (split-off units included), which matched targets to richer donors
  (half-1 mean net worth about $1.05M against a true $725k). `d_drf.R` and
  `d_drf_prep.py` now rank the donors among all SCF units of the training
  split. All `completed_drf*` files were deleted; C should discard any
  `comp_drf_*` evaluations made before about 14:30 (the watcher re-runs on
  the new files). Sequential-tree files are unaffected and final.
- **Running**, job ids in `$S/logs/d_submit_drf_rerun.txt`: `expA_clean`
  seeds 1-3 (28871774-28871776), `expB` (28871778-28871780), then the three
  variants (28871782-28871792, which reuse `expA_clean`'s forests). Logs
  `$S/logs/d_drf_<job>.out`. Expected done by about 15:15.
- **To finish by hand** if a job failed: from this folder,
  `sbatch d_py.sbatch d_drf_prep.py --dataset <ds> [--include-puf]`, then
  `sbatch d_drf.sbatch <ds> <seed> 5` (variants: prefix
  `DRF_CACHE_FROM=expA_clean`).

## 9 October, 14:20 (piece C, evaluation)

- **Finished:** yardsticks for all five rebuilt datasets; evaluations of
  every completed file B and D had written by about 14:00 (experiment A:
  joint, joint presented as SCF, half-2-only, small variants, sequential
  trees, production forest seed 2; experiment B: joint and SCF-only on SCF
  test units, sequential trees). Summary table and findings written at the
  top of `EVAL_REPORT.md`; full side-by-side tables below it.
- **Headline:** holding and the middle of the amounts are near sampling
  noise for the models and the forest; the models put about three times
  too many holders in each component's top 1% rank band (the SCF sample's
  oversampling of the wealthy, only slightly reduced by the weight
  correction), so the top 1% share of net worth comes out 60–73% against
  39% in experiment A and 52% against 37% in experiment B.
- **For B:** several expB completions have no dollars because a drawn
  negative token for `primary_home` or `pass_throughs` in 2016 has no
  negative quantile function in that wave (`a_tokenizer.decode` raises
  KeyError). C now decodes such files itself, sending those few draws to
  the nearest wave that has negatives, and logs the counts.
- **Running:** watchers 28870688–28870692 (one per dataset), as at 13:45.
  The PUF evaluations (`model_joint_puf_*`, `comp_seqtrees-puf_*`) are
  re-queued and run in the expB watcher's next round.
- **To finish by hand:** `sbatch c_shell.sbatch c_watch.sh 1`.

## 9 October, 14:20 (piece B): weighted training, first seed

`expA_clean`, half 1 records with true wealth, population-weighted, mean of
5 draws, seed 1; plain completion; weight not a model variable. Truth:
mean net worth $725k, median $93k, 90th percentile $1.19m; 1.0% of weight
above the true 99th percentile, 0.10% above the 99.9th; records above the
99.9th hold 15% of total net worth.

| Model | Mean | Median | 90th pct | Above true 99th | Above true 99.9th | Share of net worth above true 99.9th |
|---|---|---|---|---|---|---|
| joint, as SCF, unweighted, weight-corrected (before) | $1.63m | $134k | $1.54m | 2.2% | 0.5% | 48% |
| joint, as SCF, weight dropped, unweighted (control) | $2.04m | $120k | $1.86m | 2.5% | 0.7% | 55% |
| joint, as SCF, square-root weights | $1.38m | $124k | $1.55m | 1.9% | 0.5% | 43% |
| joint, as SCF, full weights | $0.97m | $123k | $1.36m | 1.5% | 0.3% | 25% |
| half 2 only, unweighted, weight-corrected (before) | $1.55m | $142k | $1.85m | 2.3% | 0.3% | 40% |
| half 2 only, square-root weights | $1.30m | $141k | $1.62m | 1.8% | 0.3% | 36% |
| half 2 only, full weights | $0.99m | $135k | $1.45m | 1.5% | 0.2% | 22% |

- **Full weighting is the clear improvement**: the top is about half as
  over-filled as before, though still 1.5 times the truth above the 99th
  percentile and 2–3 times above the 99.9th, and the median is still
  30–45% high. Square-root weights sit in between. Dropping the weight
  without weighting (control) is no better than before, so the gain comes
  from the weighting.
- Weighting is in the loss (each record's loss times its population
  weight, normalised to mean 1; effective number of training records
  24,190 of 31,299 for the joint model, 12,055 of 15,646 for half 2), and
  the validation loss used for stopping is weighted the same way.
- The joint model completing half 1 *as its own source* stays worse on
  holding rates (mean gap 0.039 against 0.019 as SCF), so presenting
  records as SCF remains the better choice.
- Likelihood of the true wealth on half 2's validation households,
  population-weighted: joint 33.6–33.7 nats per record against half 2
  alone 34.2, whatever the weighting, so adding half 1 helps by about the
  same amount in every variant; the likelihood barely registers the top
  tail, where the differences above are.
- **Queued:** seeds 2 and 3 of joint and half-2 with full and square-root
  weights (jobs 28872968–28872975); experiment B with full weights, joint
  and SCF-only, seed 1 (28872976, 28872977). Table above from
  `$S/model/expA_clean/b_quick_check.csv`
  (`sbatch --mem=64G b_cpu.sbatch b_quick_check.py --dataset expA_clean`).

## 9 October, 14:30 (piece B): weighted training, three seeds

`expA_clean`, half 1 with true wealth, population-weighted, plain
completion, ranges over training seeds 1–3 (truth: mean $725k, median
$93k, 1.0% above the true 99th percentile, 0.10% above the 99.9th, 15% of
net worth above the 99.9th):

| Model | Mean | Median | Above 99th | Above 99.9th | Share above 99.9th | Holding-rate gap |
|---|---|---|---|---|---|---|
| joint, as SCF, full weights | $0.90–1.08m | $118–125k | 1.4–1.9% | 0.2–0.3% | 16–25% | 0.019–0.022 |
| joint, own source, full weights | $0.76–0.99m | $155–167k | 1.0–1.5% | 0.1–0.2% | 9–16% | 0.033–0.039 |
| half 2 only, full weights | $0.88–0.99m | $119–135k | 1.4–1.6% | 0.2% | 16–22% | 0.019–0.033 |
| joint, as SCF, square-root weights | $1.28–1.68m | $124–141k | 1.7–2.5% | 0.4–0.5% | 37–43% | 0.019–0.025 |
| half 2 only, square-root weights | $1.13–1.47m | $112–141k | 1.7–2.2% | 0.3–0.4% | 31–41% | 0.023–0.033 |

("Holding-rate gap": mean absolute difference between completed and true
shares holding each of the 24 components.) Full weights hold up across
seeds and beat square-root weights on every top measure. With full
weights the joint model (as SCF) and the half-2 benchmark are about
equal; seed-to-seed variation is as large as the difference between them.
Completing half 1 under its own tag gives a top closer to the truth but a
median and holding rates further off. Experiment B with full weights is
running (28872976 joint, 28872977 SCF only).

## 9 October, 14:35 (piece B): experiment B, SCF test units

13,415 SCF test units (2013 and 2016 together, each in its own year's
dollars), population-weighted; truth: mean net worth $661k, median $85k,
13% of net worth above the true 99.9th percentile.

| Model | Mean | Median | Above 99th | Above 99.9th | Share above 99.9th |
|---|---|---|---|---|---|
| joint, unweighted, plain (3 seeds) | $1.42–1.64m | $97–103k | 2.2–2.4% | 0.6–0.7% | 46–50% |
| joint, unweighted, weight-corrected (3 seeds) | $0.98–1.04m | $92–98k | 1.4–1.5% | 0.3–0.4% | 36–41% |
| SCF only, unweighted, weight-corrected (3 seeds) | $1.03–1.27m | $96–101k | 1.5–1.8% | 0.4% | 40–44% |
| **joint, full weights (seed 1)** | $0.69m | $94k | 1.1% | 0.1% | 15% |
| SCF only, full weights (seed 1) | $0.87m | $96k | 1.3% | 0.3% | 28% |

The weight correction does more here than in experiment A, and full
weighting does more still. The joint model with full weights (seed 1) is
close to the truth on every top measure; whether that holds, and whether
the gap to the SCF-only model is real rather than seed noise, waits for
seeds 2 and 3 (jobs 28873562–28873565). With full weights the joint
training set counts as 80,710 effective records of 208,387 (the PUF
over-samples high incomes too; SCF only: 36,911 of 47,444).
Also queued: the three altered-income variants with full weights, seeds
1–3 (28873553–28873561). File: `$S/model/expB/b_quick_check.csv`.

## 9 October, 14:40 (piece D, final for this session)

- **Finished.** Both comparison methods, seeds 1-3, five draws per record,
  on all five datasets, on the final data, in `$S/comp/<dataset>/`
  (`completed_drf_seed<NN>`, `completed_seqtrees_seed<NN>`, and for `expB`
  the `-puf` files). The production-method files were rerun after the
  ranking fix (14:14 entry) and finished at about 14:33; nothing of D is
  running or queued.
- C's per-dataset watchers (`c_w_*`) should re-evaluate the new
  `completed_drf*` files; `$S/eval/expA_clean/comp_drf_seed02` dated 13:54
  is from the withdrawn first run and is superseded when re-evaluated.
- Write-up: `COMPARATORS_REPORT.md` (methods, every deviation from
  production, run times, a first look at net worth, three decisions for
  John).
- To redo any piece: `bash d_submit.sh <dataset> [--include-puf]` (both
  methods, seeds 1-3); for the variants' production runs reuse
  `expA_clean`'s forests with `DRF_CACHE_FROM=expA_clean` (see 14:14 entry).

## 9 October, 14:40 (piece B): experiment B with full weights, three seeds

SCF test units, as in the 14:35 table (truth: mean $661k, median $85k,
1.0% above the 99th percentile, 0.1% above the 99.9th, 13% of net worth
above the 99.9th):

| Model, full weights | Mean | Median | Above 99th | Above 99.9th | Share above 99.9th | Holding-rate gap |
|---|---|---|---|---|---|---|
| joint (PUF + SCF), seeds 1–3 | $0.69–0.80m | $89–100k | 1.1–1.3% | 0.1–0.2% | 15–18% | 0.008–0.012 |
| SCF only, seeds 1–3 | $0.82–0.87m | $95–97k | 1.3% | 0.2–0.3% | 23–28% | 0.013–0.016 |

The joint model is closer to the truth than the SCF-only model on the top
share and on holding rates in all three seeds; the seed ranges do not
overlap. This is the first result where adding the PUF to training
clearly helps completion of SCF records. Completions of the PUF records
themselves (`completed_joint_w1_puf_*`, own tag and `_asSCF`) are written
for piece C's 2015 comparison.

## 9 October, 14:40 (piece B): end of piece B's work

- **Finished, all on A's final data, three training seeds each:**
  unweighted runs (expA_clean joint / half2 / small sizes; the three
  variants, joint; expB joint / SCF-only / small) with plain and
  weight-corrected completions; full-weight runs (`_w1`) for every
  dataset (expA_clean joint and half 2, the three variants joint, expB
  joint and SCF-only); square-root-weight runs (`_wsqrt`) on expA_clean;
  the unweighted weight-dropped control (`_nowt`, one seed). Nothing of
  piece B is queued or running.
- **Write-up:** `MODEL_REPORT.md` (method, fabricated-data check, results,
  the top-tail diagnosis, the weighting results, questions for John).
  Tables: `$S/model/b_summary_runs.csv`, `b_summary_completions.csv`,
  `$S/model/<dataset>/b_quick_check.csv`, `b_diag_weight.csv`,
  `calib_top_*.csv`; curves `$S/model/<dataset>/training_curves.png`.
- **To rerun anything:** `sbatch b_pipeline.sbatch <dataset> <config>
  <seed>` (configs `joint`, `half2`, `scfonly`, with optional `_small`,
  `_w1`, `_wsqrt`, `_nowt`; `--export=ALL,SKIP_TRAIN=1` to redo only the
  completions); then `sbatch b_cpu.sbatch b_summarize.py` and
  `sbatch --mem=64G b_cpu.sbatch b_quick_check.py --dataset <dataset>`.

## 9 October, 14:45 (piece D)

- Third comparator done, as the main session asked: the survey-weighted
  forest, `completed_drfsw_seed<NN>` (and `-puf` in `expB`), seeds 1-3, on
  all five datasets, in `$S/comp/<dataset>/` (jobs 28874070-28874084,
  `$S/logs/d_submit_drfsw.txt`; all exited 0). Name recorded in SPEC
  "Contract changes" (D, 14:40); described in `COMPARATORS_REPORT.md`.
  Rerun one: `D_MODE=sw sbatch d_drf.sbatch <dataset> <seed> 5`. Nothing of
  D is running.

## 9 October, 14:50 (piece C, evaluation) — final for this session

- **Finished:** every completed file in `$S/model/` and `$S/comp/` written by
  14:45 is evaluated against the current data (checked file by file:
  each `summary.json` is newer than its completed file), including D's
  rerun `drf`, `drfsw`, and B's `w1`, `wsqrt`, `nowt` runs; `compare.md`
  regenerated for all five datasets at 14:47 and copied into the generated
  blocks of `EVAL_REPORT.md`. Summary at the top of `EVAL_REPORT.md`
  rewritten for the weighted runs. Headline columns follow John's choice
  (model with full loss weights, plain completion, own source and as SCF;
  one-source model with full weights; `drf`; `drfsw`; sequential trees);
  everything else is in each dataset's "Other variants" table.
- **Headline:** with population-weighted training the model's top 1% share
  of net worth is 37.5% against 37.3% on held-out SCF units (experiment B)
  and 34–40% against 39% in experiment A; holding and amounts are near
  sampling noise for models and forests; the forests repeat donor balance
  sheets for 56–80% of their top 1%; the joint model beats the SCF-only
  model slightly in experiment B, not in experiment A.
- **Still running:** watchers 28870688–28870692 (one per dataset, until
  about 02:00); any later completed file is evaluated within about 10
  minutes and the tables refreshed. Not re-summarized by hand: the three
  income-measurement variants for the weighted runs.
- **To finish by hand:** `sbatch c_shell.sbatch c_watch.sh 1` (one pass),
  or `sbatch c_shell.sbatch c_report.sh` (tables only).

## 9 October, 14:55 (main session): all pieces finished

A, B, C and D are finished; only C's five watcher jobs (28870688–92) run, until
about 02:00, re-evaluating any new completed file. Decisions taken today
(John): split-off SCF units keep their rows with wealth unobserved; the
survey weight is not a model variable, and training weights each record's
loss by its population weight (`b_train.py --drop-vars scf_weight
--loss-weight survey`, the `w1` runs). D added a third comparator, `drfsw`
(the production forest fit with survey weights instead of a weighted
bootstrap). Headline tables: `EVAL_REPORT.md` and `$S/eval/<dataset>/compare.md`.
Open for John: source tag for completing PUF records (SCF tag looks better);
whether to chase the remaining excess at the top in experiment A; the
crosswalk and the filing rule for split-off adults; PUF records without an
age range (44%); a larger model with full weights (not run).

## 9 October, 14:55 (piece B): larger model with full weights (queued)

John's request: test 1's "large" size (8 layers, width 512, 8 heads, about
33 million parameters with test 1's vocabularies) with full loss weights,
weight out of the model, plain completion. New config suffix `_large` in
`b_pipeline.sbatch`. Three seeds each, jobs (logs `$S/logs/b_pipe_<job>.out`):
- expB `joint_w1_large` 28874999, 28875001, 28875002 (completes SCF test
  units, PUF test records as PUF and as SCF);
- expB `scfonly_w1_large` 28875006, 28875007, 28875008;
- expA_clean `joint_w1_large` 28875009, 28875016, 28875018 (half 1 as
  its own source and as SCF);
- expA_clean `half2_w1_large` 28875026, 28875027, 28875028.
Completed files: `completed_<config>_<tag>_plain_seed0N.parquet` in
`$S/model/<dataset>/`; C's watchers pick them up as new runs. If a job
died: `sbatch b_pipeline.sbatch <dataset> <config> <seed>`.
- Note for C: `c_compare.py`'s `order_key` puts only names ending in
  `joint_w1_plain`, `joint_w1_asSCF_plain`, `joint_w1_scf_plain`,
  `joint_w1_puf_plain`, `joint_w1_puf_asSCF_plain`, `half2_w1_plain`,
  `scfonly_w1_scf_plain` (and the comparators) in the headline table, so
  the `_w1_large` runs will appear in the "other variants" table only.
  Adding the same endings with `w1_large` would bring them into the
  headline table; I have not edited C's code.

## 9 October, 15:02 (piece A)

Building `expB_multi` (expB plus SCF 2019 and 2022; see SPEC.md, A 15:02):
2022 tax units job 28875292, dataset job 28875293. A line follows when its
`DONE` marker is written. The other five datasets are untouched.

## 9 October, 15:05 (piece B): expB_multi queued; large model on experiment A

- **expB_multi (A is building it; expB plus SCF 2019 and 2022):** launcher
  job 28875350 waits for `$S/data/expB_multi/DONE` (written after 15:02,
  no newer data file), then submits `b_pipeline.sbatch expB_multi
  joint_w1_large|scfonly_w1_large 1|2|3` (job ids appended to
  `$S/logs/b_jobs.txt`). Completions: `<config>_scf` = SCF test units of
  2013 and 2016 only (the same units as expB, so grades are comparable),
  `<config>_scf1922` = the 2019 and 2022 test units, and for the joint
  model `<config>_puf_asSCF` = PUF test records presented as SCF, decoded
  at 2015. New option `b_complete.py --target-years`.
- **Evaluation of expB_multi:** job 28875351 runs `b_eval_multi.sh`
  (inside C's `c_shell.sbatch`) every 10 minutes for 11 hours: C's
  `c_watch.sh 1 expB_multi` (yardsticks, every completed file, and
  `$S/eval/expB_multi/compare.md`), plus C's `c_evaluate_puf.py` on the
  PUF files. By hand: `sbatch c_shell.sbatch b_eval_multi.sh 1`.
- **Large model (8 layers, width 512, 28 million parameters), experiment A,
  full weights:** it does not overfit faster than the base size. Best
  epochs 37–42 for the joint model and 60–65 for half 2 alone, as at base
  size; population-weighted likelihood of half 2's validation wealth
  33.41–33.57 nats per record (base 33.64), half 2 alone 34.03–34.30 (base
  34.21). Completions are being written; experiment B large runs are
  training (about 50 seconds per epoch for the joint model).

## 9 October, 15:10 (piece A)

**`expB_multi` final, `DONE` written** (`$S/data/expB_multi/`; 2022 units job
28875292, build 28875616, checks 28875617, log `$S/logs/a_check_multi_28875617.out`).
`expB`'s rows come first in every part, in `expB`'s order, and their tokens and
truth are identical to `expB`'s (checked). Rows: train 248,671, val 32,662,
test 45,134; SCF test rows with wealth observed (graded): 22,335 (2013 5,924;
2016 6,097; 2019 5,731; 2022 4,583). Nothing of piece A is running.

## 9 October, 15:12 (piece B): large model, experiment A, graded by C

From `$S/eval/expA_clean/compare.md` (15:10); mean over three seeds
[lowest–highest seed]. The large runs appear in the "Other variants"
table, not the headline table (see the note at 14:55).

| Measure | Truth / sampling noise | joint as SCF, base | joint as SCF, large | half 2 only, base | half 2 only, large |
|---|---|---|---|---|---|
| Top 1% share of net worth (%) | 39.2 / ±2.6 | 39.7 [37.0–44.5] | 38.8 [33.8–43.1] | 40.3 [37.9–41.8] | 39.1 [28.5–47.6] |
| Holding by income group, average gap (points) | noise 3.52 | 3.80 [3.45–4.28] | 4.16 [3.94–4.41] | 4.35 [3.84–5.06] | 5.28 [4.77–5.57] |
| Amounts among holders by income group, gap (percentile ranks) | noise 6.63 | 5.81 [5.38–6.34] | 5.50 [5.18–5.91] | 5.67 [5.34–6.22] | 5.97 [5.62–6.16] |
| Classifier AUC (0.5 = cannot tell) | noise 0.530 | 0.709 [0.700–0.721] | 0.698 [0.693–0.704] | 0.760 [0.754–0.766] | 0.762 [0.755–0.768] |

The large joint model is about the same as the base one: a little better
on amounts and the classifier, a little worse on holding rates, all
within the seed ranges. The large half-2-only model is worse on holding
rates and much less stable at the top (top 1% share 28.5–47.6% across
seeds), which is what overfitting on a small training set would look
like. Experiment B large runs are still training.

## 9 October, 15:22 (piece B): expB_multi running

A marked expB_multi final; the launcher submitted at 15:12: joint
`joint_w1_large` jobs 28875972, 28875973, 28875974 and SCF-only
`scfonly_w1_large` 28875975, 28875976, 28875977 (logs
`$S/logs/b_pipe_<job>.out`). Training records: PUF 160,943 and SCF 87,728
(four waves); with full weights the joint set counts as 91,829 effective
records, the SCF-only set as 58,699. The joint model takes about 47
seconds per epoch, so its completions should land around 15:50. The
evaluation watcher 28875351 has written the yardsticks and a first
`$S/eval/expB_multi/compare.md`, and re-evaluates every 10 minutes
until about 02:00.

## 9 October, 15:32 (piece B): large model, experiment B, graded by C

SCF test units, from `$S/eval/expB/compare.md`; mean over three seeds
[lowest–highest seed]. Large runs are in the "Other variants" table.

| Measure | Truth / sampling noise | joint, base | joint, large | SCF only, base | SCF only, large |
|---|---|---|---|---|---|
| Top 1% share of net worth (%) | 37.3 / ±1.8 | 37.5 [36.8–38.0] | 39.1 [38.0–40.6] | 43.0 [41.7–45.2] | 37.0 [35.6–39.8] |
| Top 0.1% share of net worth (%) | 13.3 / ±1.2 | 12.9 [12.1–13.9] | 15.4 [14.2–17.0] | 16.3 [14.6–18.5] | 13.2 [11.5–15.6] |
| Holding by income group, average gap (points) | noise 3.45 | 2.30 [2.28–2.32] | 2.29 [2.24–2.35] | 2.62 [2.58–2.65] | 3.69 [3.65–3.73] |
| Amounts among holders by income group, gap (percentile ranks) | noise 6.66 | 5.00 [4.83–5.09] | 5.21 [5.19–5.22] | 5.10 [4.89–5.47] | 5.15 [5.00–5.28] |
| Median net worth, completed / true | noise ±0.120 | 1.11 [1.05–1.18] | 1.13 [1.10–1.14] | 1.13 [1.12–1.14] | 1.24 [1.22–1.28] |
| Classifier AUC | noise 0.506 | 0.604 [0.596–0.617] | 0.617 [0.614–0.621] | 0.610 [0.604–0.613] | 0.596 [0.587–0.602] |

The larger size does not help the joint model: it is the same on holding
rates and slightly worse on the top shares, amounts and the classifier.
For the SCF-only model it moves the top shares onto the truth but makes
holding rates by income group and the median clearly worse. So the base
size stays the better choice for the joint model; with the large size the
joint model's advantage over the SCF-only one is no longer at the top,
only in holding rates by income group (2.29 against 3.69).

## 9 October, 15:50 (main session)

- Large model (8 layers, width 512) with full loss weights on expB: no better
  than base size (holding gap by income group 2.29 against 2.30; top 1% share
  39.1% against 37.5%, truth 37.3%). Same as test 1: size is not the limit.
- Levels by income group (expB SCF test units; script
  `$S/dev/main/levels.py`, output `$S/logs/levels_28877312.out`): the
  transformer gives too much net worth to the bottom 50% and 50–90% income
  groups (mean $181k and $489k against true $132k and $407k) and too little
  to the top 1% by income (25% of net worth against 31%); medians by income
  group are right. The survey-weighted forest (`drfsw`) is closest on every
  row; the production forest gives the top 1% by income 38%. Read: the
  transformer's link between income and wealth is too weak at the top.
- Running: expB_multi (PUF + SCF 2013/2016/2019/2022), large full-weight
  model, joint and SCF-only, three seeds; launcher and evaluation watcher
  by piece B (job ids in this file above and `$S/logs/b_jobs.txt`). Tables
  will be in `$S/eval/expB_multi/compare.md`. To get the levels view, rerun
  `levels.py` with the dataset changed to expB_multi and the model paths
  changed to the `w1_large` files there.

## 9 October, 15:52 (piece B): expB_multi graded; end of session

All expB_multi runs have finished, and C's measures for them are in
`$S/eval/expB_multi/compare.md`. Large model, full weights, mean over
three seeds [range]. The 2013/2016 rows are graded on the same SCF test
units as expB, so they compare directly with the 15:32 table (truth: top 1%
share 37.3%).

| SCF test units 2013 and 2016 | joint, expB large | joint, expB_multi large | SCF only, expB large | SCF only, expB_multi large |
|---|---|---|---|---|
| Top 1% share of net worth (%) | 39.1 [38.0–40.6] | 39.1 [37.9–40.1] | 37.0 [35.6–39.8] | 38.1 [37.3–39.4] |
| Top 0.1% share (%) (truth 13.3) | 15.4 [14.2–17.0] | 14.8 [13.3–15.7] | 13.2 [11.5–15.6] | 13.8 [13.0–15.0] |
| Holding by income group, gap (points) | 2.29 [2.24–2.35] | 2.22 [2.19–2.26] | 3.69 [3.65–3.73] | 2.55 [2.35–2.91] |
| Amounts by income group, gap (percentile ranks) | 5.21 [5.19–5.22] | 4.84 [4.77–4.94] | 5.15 [5.00–5.28] | 4.79 [4.71–4.84] |
| Median net worth, completed / true | 1.13 [1.10–1.14] | 1.17 [1.09–1.29] | 1.24 [1.22–1.28] | 1.23 [1.21–1.27] |
| Classifier AUC | 0.617 [0.614–0.621] | 0.591 [0.589–0.593] | 0.596 [0.587–0.602] | 0.578 [0.570–0.588] |

Adding the 2019 and 2022 waves improves the amounts and the classifier
score for both models. It also improves holding rates by income group,
greatly for the SCF-only model. The joint model still has the better
holding rates by income group, and the seed ranges do not overlap (2.22
against 2.55). Top shares stay near the truth for both models.

The 2019/2022 test units (`_scf1922`, 10,314 units with wealth) are graded
too: the joint model has a top 1% share of 39.7 [37.6–42.6], a holding gap
of 2.34 and AUC 0.573; the SCF-only model has 36.9 [36.4–37.3], 2.66 and
0.567. The truth for these units is about 37–38%; read it from their
`summary.json`, because compare.md shows one truth column for all files.
The PUF completions at 2015 (`joint_w1_large_puf_asSCF`) are graded by
`c_evaluate_puf.py` into `$S/eval/expB_multi/model_*_puf/`.

Two caveats about `compare.md` for expB_multi. Its sampling-noise
yardstick is computed over all four waves' test units, not the subset
each file covers. Its headline table has no method columns, because C's
`order_key` does not match `w1_large` names, so every run is under "Other
variants".

Still running: evaluation watcher 28875351 (until about 02:00). Nothing
else of piece B is queued.

## 9 October, 15:55 (main session): four waves

expB_multi (large, full weights) graded by B; levels by income group on the
same 2013/2016 test units in `$S/logs/levels_multi_*.out` (script
`$S/dev/main/levels_multi.py`). Four waves barely move the weak income–wealth
link: share of net worth held by the top 1% by income 26.1% (two waves,
large: 23.5%; truth 30.6%; weighted forest 32.9%); bottom 50% still 10.6%
against 8.9%. More data of this size is not the fix. Next candidates: train
only on the completion task (all wealth hidden, given shared variables);
make the top count in training; average several trained models; income as a
plain number input.

## 9 October, 16:05 (main session): completion-task training queued

- New option `b_train.py --task-frac f` (main session; backup of the previous
  file in `$S/dev/main/b_train.py.bak`): a share f of records that observe
  SCF-only variables get the completion task each step (all observed wealth
  hidden and scored, everything else visible); the rest any-order hiding;
  validation masks drawn the same way. Untested before submission.
- Queued on expB, full weights, base size, seeds 1–3 (`b_jobs.txt`):
  `joint_w1_task` (f = 1) 28878957/60/63; `joint_w1_task50` (f = 0.5)
  28878958/61/64; `scfonly_w1_task` (f = 1) 28878959/62/65. Logs
  `$S/logs/b_pipe_<job>.out`. Check first that the log prints "completion task
  for a share ..." and training epochs, not a traceback.
- C's watcher `c_w_expB` grades the completed files (they land under "Other
  variants" in `$S/eval/expB/compare.md`).
- Levels by income group run automatically after all nine end (job 28879222,
  `$S/dev/main/levels_task.py`) → `$S/logs/levels_task_28879222.out`. If some
  runs failed it will fail too; fix and rerun `sbatch $S/dev/main/levels_task.sbatch`.
