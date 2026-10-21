---
title: "Test 2 — one model across the PUF and the SCF: shared specification"
role: method
workstream: rebuild
status: current
updated: 2026-10-09
sot: research/rebuild/tests/scf_joint/PLAN.md
supersedes: []
superseded_by: null
---

# Test 2: shared specification

This file is the contract between four pieces of work done in parallel:
data preparation (A), the model (B), the evaluation (C) and the comparison
methods (D). Read `PLAN.md` (this folder) first; it has the question and
John's decisions. Read test 1 for context and reusable code:
`research/rebuild/tests/puf_generator/` (`SPEC.md`, `EXPERIMENTS.md`
summary at the top, `model.py`, `train.py`, `tokenizer.py`, `evaluate.py`).
Each piece reads and writes only the files named here. If the contract
needs to change, write the change into the "Contract changes" section at the
end of this file, with your piece's letter and the time, before relying on
it.

## Rules for everyone

- **Compute.** Every Python and R run goes through `sbatch`, never the login
  node and never `srun` from this session (it hangs), including quick checks.
  CPU work: partition `day` (or `devel` for a few minutes). GPU work:
  `--gres=gpu:1` on `gpu_b200` (started immediately last night), `gpu`,
  `gpu_h100` or `gpu_h200`; a script may list several, comma-separated.
  Slurm logs go to `$S/logs/`, never the repo root.
- **Environment.**
  - Python: `module load PyTorch/2.9.1-foss-2024a-CUDA-12.8.0
    scikit-learn/1.6.1-gfbf-2024a Arrow/17.0.0-gfbf-2024a
    matplotlib/3.9.2-gfbf-2024a`. Do not pip-install anything.
  - R: `module load R/4.4.1-foss-2022b` (as in the repo's own scripts).
- **Licensed data.** The PUF and the SCF raw files, and anything derived
  from the PUF (including completed or generated records), stay on this
  cluster under `$S`. Never write PUF values into the git repository, never
  print more than a handful of individual records, never send any of it to
  an outside service. Code, reports and aggregate tables go in the repo.
- **Paths.**
  - Code and reports: `research/rebuild/tests/scf_joint/` in the Tax-Data
    repo (`/nfs/roberts/project/pi_nrs36/jar335/Repositories/Tax-Data`).
    Prefix your files by piece where names could clash (`a_`, `b_`, `c_`,
    `d_`), except the shared ones named below.
  - Data and outputs: `/nfs/roberts/scratch/pi_nrs36/jar335/scf_joint/`
    (`$S`). Subfolders: `data/`, `model/`, `eval/`, `comp/`, `logs/`,
    `dev/` (anything temporary).
- **Raw inputs** (read only):
  - PUF: `/nfs/roberts/project/pi_nrs36/shared/raw_data/IRS-PUF/v1/2023091310/historical/`
    `puf_2015.csv` and `demographics_2015.csv`. Test 1's prepared split is
    in `/nfs/roberts/scratch/pi_nrs36/jar335/puf_generator/data/`
    (`values_*.parquet` carry `RECID`).
  - SCF: `/nfs/roberts/project/pi_nrs36/shared/raw_data/SCF/v1/{2013,2016}/historical/`
    (`p13i6.dta`, `SCFP2013.csv`; `p16i6.dta`, `SCFP2016.csv`).
  - Tax-unit builder: `src/imputations/stage1_scf_tax_units.R`
    (`build_scf_tax_units(config, raw_path, scfp_path, syear)`), so far run
    only for 2019 and 2022. Production wealth imputation:
    `src/imputations/wealth.R`, `wealth_schema.R`, `pool_scf_donors.R`.
- **Seeds.** Every random step takes an explicit seed, recorded in outputs.
- **Writing.** Every document is for a public finance economist who knows
  taxes well and is not a machine-learning specialist. Plain English, full
  sentences, define any machine-learning term in one sentence when first
  used, no invented shorthand, no dramatic phrasing. Describe; do not sell.
- **No silent dropping.** Never drop records or values without stating the
  count and the reason. No `na.rm = TRUE` without a comment saying why.
- **Status.** The Claude session driving this work runs inside a Slurm job
  that ends at about 16:15 today (9 October), and your agent ends with it.
  Every hour, and before you stop, append a short dated section to
  `STATUS.md` in this folder: what is finished, what is running (job ids,
  log paths), and the exact commands to finish. Queue long work as Slurm
  jobs (chain with `--dependency=afterok:<id>`) so it completes without you.

## Variables

Three groups. Every variable is a token variable as in test 1 (integer
tokens `0 .. vocab_size-1`, local to the variable).

1. **Shared** (observed in both sources where present):
   - `age_range`: the primary filer's (SCF: the unit head's) age range in
     the PUF demographic file's `AGERANGE` categories. PUF records absent
     from the demographic file have it unobserved (not a `missing` token).
   - `married`: married filing jointly or not.
   - `n_dep`: number of dependents, top-coded at 3.
   - Seven income amounts, named as in the SCF builder: `wages`,
     `business` (business and farm), `int_div` (interest and dividends),
     `capital_gains`, `rent`, `ss_pens` (Social Security, pensions,
     annuities), `ui_other` (unemployment and other). A crosswalk maps PUF
     lines onto these (part A proposes it in `a_crosswalk.md`, for John's
     review; checked against totals in each source).
2. **SCF only:** the 24 wealth amounts of `wealth_schema.R` (14 assets,
   6 debts, 4 unrealized gains), and `scf_weight` (the survey weight; see
   "The weight" below).
3. **Context, always observed, not scored:** `source` (categorical) and
   `year` (a number, not a token; stored in the record file).

PUF-only variables are left out on purpose: the model cannot learn how they
relate to wealth, because no source observes both.

## Units: ranks

Each amount becomes a token from its **weighted rank within its own source
and year** (filers only on both sides, using the survey or PUF weight, with
each SCF implicate at one fifth of its weight):

- a token for exact zero;
- for positive values, bins of the weighted rank among positive values,
  on one grid shared by all sources and years, finer at the top (suggested:
  steps of 0.02 up to 0.90, 0.01 to 0.99, 0.001 to 0.999, then 0.9999 and
  1). Rare variables may use a coarser grid nested in the shared one; state
  which;
- for negative values, a few bins of the weighted rank among negative
  values (for example deciles), where negatives occur.

The same token therefore means the same rank band in every source. Dollars
come back through each source-year's own weighted quantile function: draw a
rank uniformly inside the bin and read off that source-year's quantile
(interpolated). For PUF records completed at year 2015, wealth dollars come
from interpolating the 2013 and 2016 SCF quantile functions in log dollars
(weight 1/3 on 2013 and 2/3 on 2016), and for each variable separately for
positives and negatives.

## The weight

The SCF oversamples wealthy households (its list sample is drawn from tax
records using a wealth index), so among SCF records with the same incomes,
the unweighted sample holds more wealth than the population. A model
trained on unweighted records learns the sample's relationship, not the
population's. Handling, as the default:

- `scf_weight` is a variable: bins of log weight within the SCF source and
  year (up to 64 quantile bins). Observed for SCF records, unobserved for
  PUF records and for the pretend-PUF half in experiment A.
- Completion draws `scf_weight` along with the wealth variables. The
  **weight-corrected completion** draws K completions per record (K = 20 by
  default) and keeps one with probability proportional to its drawn weight
  (decoded to the weight's value). This turns the sample relationship into
  the population relationship, if the model has learned the sample well.
- Part B also reports the plain completion (no correction) so the size of
  the correction is visible.

## A. Data preparation — outputs in `$S/data/`

1. **SCF tax units** for 2013 and 2016 with `build_scf_tax_units()`. Check
   each wave and write the checks into `DATA_REPORT.md`: unit counts,
   weighted totals of each income component and of net worth against the
   SCFP summary file, the share of households split into more than one unit,
   and the same for 2019 for comparison if cheap. Fix wave-specific
   variable differences if the builder fails on older waves, without
   changing its behaviour for 2019 and 2022 (copy it into this folder as
   `a_scf_tax_units.R` if it needs edits; do not edit `src/`).
2. **Filers only.** Keep units that filed (X5744 codes 1 and 6). Find out
   whether units split off a household carry their own filing answer; if
   not, state the rule used and the count affected.
3. **PUF side.** 2015 PUF records, the four aggregate records set aside;
   use test 1's split (join on `RECID`). Decide, with reasons, how to treat
   dependent filers (PUF `DSI = 1`) so the two universes match.
4. **Crosswalk** `a_crosswalk.md`: PUF lines for each of the seven SCF
   income categories, reasoned from the PUF booklet and the SCF codebook
   definitions, with weighted totals and shares nonzero in each source for
   each category (in 2015 dollars, SCF scaled by CPI only for the
   comparison table). Flag categories where the two differ a lot.
5. **Datasets.** One folder per dataset under `$S/data/`, each with the same
   layout:
   - `expA_clean`, `expA_noise`, `expA_recat`, `expA_unrep`: SCF 2016
     filers split by household into half 1 (`source = SCF_H1`, the pretend
     PUF: wealth and `scf_weight` unobserved) and half 2 (`source = SCF`).
     Seed 20261009. In the three variants, half 1's incomes (in dollars,
     before ranking) are altered: `noise` multiplies each nonzero amount by
     exp(N(0, 0.3²)); `recat` moves 30% of business income to wages for
     units with both or with business only; `unrep` sets interest and
     dividends to zero for a random 25% of units holding them and capital
     gains to zero for another random 25%. Seed 20261010. State all of this
     in the report.
   - `expB`: PUF 2015 (`source = PUF`, year 2015) plus SCF 2013 and 2016
     filers (`source = SCF`, years 2013 and 2016).
   Layout of each folder:
   - `meta.json`: variables in a fixed order with `name`, `group`
     (`shared`, `scf_only`, `context`), `kind` (`categorical`, `amount`,
     `weight`), `vocab_size`, and for each token its meaning (category
     value, or rank band `lo`/`hi`, and zero/negative flags); the sources
     and years present; seeds; counts; the default generation order for
     completion (suggested: `scf_weight`, then the wealth variables from
     most to least common holding).
   - `tokens_{train,val,test}.npy`: int16 or int32 arrays `[records,
     variables]` in `meta.json` order, with **-1 wherever the variable is
     unobserved for that record**. The model must never see a value that is
     unobserved, so true values of hidden variables are not in these files.
   - `truth_{train,val,test}.npy`: the same arrays with the true tokens
     filled in where they exist (half 1's wealth in experiment A; -1 for
     PUF wealth). For evaluation only.
   - `records_{train,val,test}.parquet`, same row order: `source`, `year`,
     `hh_id` (SCF household; PUF `RECID`), `implicate` (1–5; 0 for PUF),
     `unit` (tax unit within household), `weight` (the population weight:
     PUF `S006/100`; SCF weight / 5), `half` (1 or 2 in experiment A, else
     0), and the true dollar value of every amount (wealth dollars for half
     1 included, for evaluation only).
   - `quantiles.parquet`: each source-year-variable's weighted quantile
     function for positive and negative values (enough points for
     interpolation; all points at the top), used for decoding.
   - Splits. Experiment A: within each half, 10% of households to `val`,
     the rest to `train`; `test` = half 1's units (a copy of half 1's
     train and val rows, so evaluation reads one file). Experiment B: SCF
     households 70% train, 10% val, 20% test per wave, all implicates and
     units of a household together; PUF as in test 1. Seed 20261009.
   - `DONE` marker when the folder is final.
6. `a_tokenizer.py` (importable): `encode` and `decode(tokens, meta,
   records, quantiles, rng, year=None)`, returning dollars; `year` lets B
   decode PUF wealth at 2015. A round-trip check (dollars → tokens →
   dollars) for every variable in every source-year: weighted quantiles,
   shares at zero, totals, in `DATA_REPORT.md`.

**Order of work.** `expA_clean` first (it unblocks B, C and D), then `expB`,
then the three variants. Write a provisional `expA_clean` as soon as the
2016 tax units exist, even before the checks are finished, marked
provisional in `STATUS.md`.

## B. The model — outputs in `$S/model/<dataset>/`

Start from test 1's `model.py` and `train.py` (copy into this folder as
`b_model.py`, `b_train.py`, and so on).

- **Inputs.** As in test 1, plus: a learned embedding for `source`, added
  to every variable slot; the year through a small learned function of
  (year − 2015), also added to every slot. Unobserved variables (-1) are
  always hidden and never scored.
- **Training.** Any-order hiding as in test 1, applied to observed
  variables only; at least one observed, non-context variable hidden per
  record per step. Records unweighted (the weight is a variable). Base size
  (6 layers, width 256, 8 heads); try smaller if the SCF-only datasets
  overfit (they have about 40,000 records). Stop on validation loss of
  random hiding. Three training seeds per configuration.
- **Completion** (`b_complete.py`): for each record, keep every observed
  value; fill the unobserved variables one at a time in `meta.json`'s
  order, each drawn given everything observed and drawn so far, at
  temperature 1. Plain completion and weight-corrected completion (see "The
  weight"). Output `$S/model/<dataset>/completed_<tag>_seed<NN>.parquet`:
  the target records in the `records_*.parquet` layout plus the completed
  dollar values (decoded with `a_tokenizer.py`) and the completed tokens,
  with a column `draw` (several completions per record: 5 by default).
- **Runs.**
  - Experiment A, each of the four datasets: the joint model (half 1 and
    half 2) and, on `expA_clean` only, the benchmark trained on half 2
    alone (`--sources SCF`). Complete half 1.
  - Experiment B: the joint model (PUF + SCF) and the SCF-only model.
    Complete the SCF test units with both; complete the PUF test records
    with the joint model at year 2015.
- `MODEL_REPORT.md`: architecture changes from test 1, settings, curves,
  run times, GPUs.

## C. The evaluation — outputs in `$S/eval/<dataset>/<label>/`

`c_evaluate.py --dataset <name> --completed <file> --label <name>`
compares completed records with the truth for the same records
(experiment A: half 1; experiment B: SCF test units). All weighted by the
records' true population weights (`weight` in the record file), each draw
weighted 1/number of draws. Measures, each in rank tokens and in dollars:

1. Each wealth component: share holding, and weighted quantiles of the
   amount, by income rank group (total of the seven incomes, in groups
   0–50, 50–90, 90–99, top 1%) and by age range.
2. Joint patterns: the frequency of each pattern of holding across the 24
   components (total variation distance: half the sum of absolute
   differences in shares), pairwise holding rates for selected pairs
   (homeownership among stockholders, mortgage among homeowners, and so
   on), and rank correlations of amounts among holders.
3. Net worth (assets minus debts, in dollars): distribution, shares held by
   the top 10%, 1%, 0.1%; by income group.
4. Cloning at the top: among the top 1% by completed net worth, how often
   the same combination of wealth tokens repeats, against the truth.
5. Classifier test: a gradient-boosted classifier telling completed records
   from true records using shared and wealth variables; AUC and the
   variables it relies on.
6. Noise yardsticks: the same measures for (a) one random half of the true
   records' households against the other half (sampling noise), (b) one
   completion draw against another (completion noise), (c) runs with
   different training seeds (training noise).
7. Experiment B only, PUF records completed at 2015
   (`c_evaluate_puf.py`): holders and amounts of each component by income
   rank group and age range, and net worth shares at the top, against SCF
   2013, SCF 2016 and their interpolation.

`EVAL_REPORT.md`: a one-screen summary for the economist reader, then
detail, built from `$S/eval/*/*/summary.json`. C develops on a provisional
dataset or a fabricated one until A's files exist.

## D. The comparison methods — outputs in `$S/comp/<dataset>/`

Two methods, trained on the same records the model sees with wealth
observed (experiment A: half 2's train rows; experiment B: SCF train rows),
completing the same target records, written in B's completed-file layout
(`completed_<method>_seed<NN>.parquet`, 5 draws per record):

1. **Production method.** The per-income-cell distributional random forest
   of `src/imputations/wealth.R` (its fit and donor draw, without the
   Stage 3 tilt to aggregates), refit on these records with the features
   it uses, mapped from the shared variables. Dollars in, dollars out; the
   tokens are then encoded with `a_tokenizer.py` so C can compare in ranks
   too. Follow how `wealth.R` uses the survey weight. Copy what you need
   into this folder (`d_drf.R`); do not edit `src/`.
2. **Sequential trees.** In rank tokens: for each wealth variable in the
   completion order, a gradient-boosted tree classifier
   (`HistGradientBoostingClassifier`) predicting its token from the shared
   variables and the wealth variables already filled, trained weighted by
   the survey weight; draw from its predicted probabilities.

Run on `expA_clean` first (with seeds 1–3), then `expB`, then the variants.
`COMPARATORS_REPORT.md`: what was done, deviations from production, run
times.

## Contract changes

(Append here: piece, time, change.)

- **C, 12:55.** Completed files (B's and D's) carry, besides the record
  layout and `draw`: a column `row`, the 0-based row index of the record in
  the dataset's `records_<split>.parquet` that was completed (so C joins to
  `truth_<split>.npy` without guessing; if `row` is absent C falls back to
  joining on `source`, `year`, `hh_id`, `implicate`, `unit`); completed
  dollar values in columns named exactly as the variable (`cash`,
  `equities`, ...); completed tokens in columns `tok_<variable>`. If a file
  has no token columns, C encodes the dollars with `a_tokenizer.encode`.
  `draw` runs 0 .. K-1.

- **A, 13:00.** (1) In every dataset, column 0 of the token arrays is
  `source` (group `context`, categorical, always observed; tokens listed in
  `meta.json`). `year` is not a token; it is in the records file. (2)
  `records_*.parquet` also carries `age` (the SCF unit head's age in years;
  NaN for the PUF), and `orig_<income>` columns: the income before the
  experiment A alteration (equal to the income column in `expA_clean` and
  `expB`). The income columns themselves are what the model sees. (3) `meta.json`
  has `variable_names` (token column order), `generation_order` (`scf_weight`
  then wealth, most to least commonly held), and per variable `observed_in`.
  (4) `scf_weight` is the population weight (household weight / 5); its tokens
  are 64 bins of its unweighted rank within the SCF source-year;
  `a_tokenizer.decode` returns it in weight units. (5) Records unobserved for a
  variable have NaN in the records file and -1 in both token arrays (PUF
  wealth; PUF `age_range` outside the demographic file).
- **B, 13:05.** Completed files from the model, in `$S/model/<dataset>/`:
  `completed_<tag>_<plain|wc>_seed<NN>.parquet`, where `plain` is the
  completion as drawn and `wc` the weight-corrected one (K = 20 per draw,
  one kept with probability proportional to its drawn weight; the plain
  draw is the first of the same 20). Tags: experiment A, joint model
  `joint` (half 1 completed as its own source, `SCF_H1`) and `joint_asSCF`
  (the same model, half 1 presented as source `SCF`); benchmark trained on
  half 2 alone `half2` (presented as `SCF`, the only source it knows).
  Experiment B: `joint_scf` and `scfonly_scf` (SCF test units), `joint_puf`
  (PUF test records, decoded at 2015). Suffix `_small` for the smaller
  model if run. Columns, as C asked: `row`, every non-variable column of
  `records_<split>.parquet` (including `source`, `year`, `weight`, `age`,
  `orig_*`), one dollar column per non-context variable (true dollars where
  observed, decoded dollars where completed), `tok_<variable>` for every
  variable including `source`, `obs_<variable>` (True where kept, False
  where completed), and `draw` 0..4. For SCF test units in experiment B,
  all `scf_only` variables, including `scf_weight`, are hidden before
  completion, so they are completed as a PUF record would be. A
  `completed_<tag>_seed<NN>_info.json` beside each pair has settings and
  the completion log-likelihood of the true values.
- **A, 13:08.** Rank grid as built: positive edges as suggested plus one more
  at 0.99999 (66 positive bins); negative bins have edges 0, 0.001, 0.01,
  0.05, 0.1, 0.2, ..., 0.9, 1 among negatives, most negative first (13 bins),
  because plain deciles left the largest losses in one wide bin and made
  decoded totals noisy (PUF Schedule E losses off by 31%). Read vocabularies and
  bands from `meta.json`, not from this text. Amount variables with no negative
  values anywhere in the dataset have no negative tokens.
- **A, 13:19.** John's decision (9 October): SCF split-off units (unit code 3
  and above: financially independent adults the builder makes into their own
  tax units, whose wealth is zero by construction) keep their rows, incomes,
  demographics and `scf_weight`, but all 24 wealth variables are
  **unobserved** for them: -1 in both `tokens_*.npy` and `truth_*.npy`, NaN in
  the wealth columns of `records_*.parquet`, which gains a boolean column
  `wealth_observed` (False for these units and for every PUF record; True for
  other SCF units, including half 1 in experiment A, whose wealth is hidden
  from the model but known for grading). Wealth ranks and the wealth rows of
  `quantiles.parquet` use units with wealth observed only. In experiment A,
  half 1's split-off units are still completed but cannot be graded (no
  truth); C should grade only rows with `wealth_observed`. Seeds and splits
  unchanged. The datasets are rebuilt, `expA_clean` first.
- **B, 13:25.** `b_complete.py --complete-as-source <name>` (alias
  `--as-source`) overrides the source of the target records at completion
  time. Completed files with tag suffix `_asSCF` use it with `SCF`:
  experiment A `joint_asSCF` (half 1 presented as SCF; `joint` keeps its own
  tag `SCF_H1`), experiment B `joint_puf_asSCF` (PUF records presented as
  SCF; `joint_puf` keeps `PUF`). The half-2 benchmark and the SCF-only model
  know only `SCF`, so their files are always completed as `SCF`. SCF records
  whose wealth is unobserved (split-off units, per A's rebuild) are trained
  on for their incomes and weight only; in experiment B they are completed
  like the other SCF test units but have no true wealth to score.
- **B, 13:45.** Completion draws only codes that `a_tokenizer.decode` can
  turn into dollars for the record: an amount's negative codes are not drawn
  where the SCF year(s) used to decode it have no negative values (for
  example a negative home value for 2016, or for the PUF at 2015, which
  decodes through 2013 and 2016). Before this, a few such draws made the
  decoder fail (KeyError) and left the experiment B files without dollars.
  The restricted codes are listed in each info file (`codes_not_drawn`).
  The experiment B completions written before 13:45 are being rewritten by
  jobs 28870679–28870687 (same checkpoints, same seeds); experiment A was
  not affected (one year, decoded through its own 2016 functions).
- **D, 14:10.** D's completed files, in `$S/comp/<dataset>/`:
  `completed_drf_seed<NN>.parquet` (production method refit) and
  `completed_seqtrees_seed<NN>.parquet` (sequential trees), seeds 01-03, for
  half 1 (experiment A) or the SCF test units (experiment B, `source` SCF
  only). The PUF test records of experiment B are in separate files,
  `completed_drf-puf_seed<NN>.parquet` and
  `completed_seqtrees-puf_seed<NN>.parquet`. Columns as agreed (`row`,
  record columns, `draw` 0-4, wealth dollars under the variable names,
  `tok_<variable>` for the 24 wealth variables), plus `method` and `seed`;
  the production-method files also carry `donor_row`, `donor_hh`,
  `donor_year`, `leaf_size` and `cell` (the donor and income group). The
  `drf-puf` files have no token columns (no single SCF rank scale at 2015);
  read their dollars. Split-off units among the targets are completed but
  have no truth.
- **B, 14:30.** Weighted training (John, 9 October: the survey weight is
  not a model variable). New configurations, tag suffixes on the config
  name: `_w1` (loss weighted by the population weight), `_wsqrt` (by its
  square root), `_nowt` (unweighted; control). In all three `scf_weight` is
  always hidden and never scored (`b_train.py --drop-vars scf_weight
  --loss-weight survey|sqrt`), and completion is plain with K = 1: the
  `wc` file is identical to the `plain` file, and `tok_scf_weight` is -1
  and `scf_weight` NaN in the completed files. Files, e.g.
  `completed_joint_w1_asSCF_plain_seed01.parquet`,
  `completed_half2_w1_plain_seed01.parquet`, and in experiment B
  `completed_joint_w1_scf_*`, `completed_joint_w1_puf_*`,
  `completed_joint_w1_puf_asSCF_*`, `completed_scfonly_w1_scf_*`. In the
  joint model every record's loss is weighted by its own population weight
  (PUF `S006/100`, SCF weight / 5).
- **D, 14:40.** Third comparator, by the main session's request:
  `completed_drfsw_seed<NN>.parquet` (and `completed_drfsw-puf_seed<NN>` in
  `expB`), seeds 01-03, in `$S/comp/<dataset>/`, same layout as the `drf`
  files. It is the production refit with one change: no weighted bootstrap;
  each income group's forest is fit to the original records with the
  survey weight passed to the forest, and donors are drawn within the leaf
  in proportion to the survey weight. Not production; a diagnostic
  comparator.
- **A, 15:02 (done 15:10; 2022 "nothing" codes of -1 in Social Security and pensions and in defined-benefit value, 10 values, set to 0).** New dataset `expB_multi` (John, 9 October): `expB` plus SCF
  2019 and 2022 filers, same builder and rules (split-off units' wealth
  unobserved, five implicates at 1/5 weight, ranks within source and year on
  the same grid, `scf_weight` kept). Same layout and `DONE` marker. `expB`'s PUF
  rows and its 2013 and 2016 SCF households are in exactly the same parts;
  2019 and 2022 households are split 70/10/20 with seed 20261009 (the same
  generator, continued). Within each part the rows are ordered PUF, SCF 2013,
  2016, 2019, 2022, so `expB`'s rows come first, in `expB`'s order. Token
  columns are in `expB`'s order. SCF 2019 incomes are tax-year 2018 dollars
  and wealth 2019 dollars; 2022 incomes 2021 dollars, wealth 2022 dollars.
  PUF wealth at 2015 is still decoded from 2013 and 2016 (the waves either
  side of 2015).
