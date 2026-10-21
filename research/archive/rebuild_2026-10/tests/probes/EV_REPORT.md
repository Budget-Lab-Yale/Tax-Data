---
title: "Test 4, probe EV — one scorer for the transformer and sequential trees"
role: report
workstream: rebuild
status: current
updated: 2026-10-14
sot: research/rebuild/tests/probes/SPEC.md
supersedes: []
superseded_by: null
---

# One scorer for the transformer and sequential trees

## Summary

- **The question.** Is the transformer better than sequential trees on the
  multi-survey job? Both methods were run on the same data (probe C's
  `full_v3cz`) and scored by one script that only looks at draws, so it
  treats them the same way. Transformer: probe C's zero-coded model, three
  training seeds, files already on disk. Trees: probe TR's files, all of
  which were ready at 10:31 and are scored here.
- **Whole PUF records generated from scratch: the trees are clearly
  better.** On probe V's scorecard the trees beat the transformer on every
  distributional check; the transformer is closer only on total tax, where
  two of its errors offset. The largest gap in average tax rate by AGI group
  is 0.19 points for the trees and 2.69 for the transformer; the real
  validation records miss by 0.15. A classifier tells the trees' records
  from real ones with area under the curve 0.56, against 0.81 for the
  transformer (0.50 means it cannot tell them apart). The trees overshoot
  the top a little: total AGI is $8.18tn against $8.09tn, and tax in the
  top 0.1% group is $214bn against $204bn.
- **Wealth completed on held-out SCF records (true wealth known): the two
  are close.** On most of the 16 checks the difference is within two
  sampling errors. The transformer is better on the score for each
  household (0.542 against 0.550, with a sampling error of the difference
  of 0.0014), on which items are held together within income groups, and
  in the top 0.1% of income: mean net worth is $32m for the transformer
  and $20m for the trees, against a true $45m (sampling error $9m). The
  trees are better on mean net worth by filing status and by income × age,
  where the transformer overstates the middle of the distribution.
- **Wealth completed on PUF records, compared with the SCF interpolated to
  2015: the trees are worse at the top.** The SCF's top 0.1% by income
  has mean net worth $54.7m. The transformer gives the top 0.1% of PUF
  returns by AGI $38.6m and the trees $25.5m. Probe TR's variant that
  trains the SCF side unweighted, with the weight as a variable, reaches
  $52.2m in the top 0.1% but overstates the middle ($503k in the 50th–90th
  percentile, against $370k).
- **Both methods inherit the data's itemizer problem in the same way.**
  When non-itemizers' zero mortgage interest is read as "none paid", 28%
  of PUF returns are given a home (SCF 67%) by both methods. Handled
  differently (the transformer with deductions unknown for non-itemizers,
  the trees with separate trees for non-itemizers), home ownership is 57%
  for the transformer and 61% for the trees.
- **For the choice of model.** The trees should generate the tax side.
  The transformer completes wealth somewhat better, mainly at the top, but
  neither gets the top 0.1% right on SCF records or on PUF records, and
  the trees' gap at the top looks like a tuning problem (leaf size and
  weighting) more than a limit of the method. On this evidence the
  transformer's extra machinery buys a modest gain in wealth completion
  and costs a large loss in generation.

## What was run

All jobs ran in Slurm on 14 October between 10:19 and 10:40. Outputs are
in `$Q = /nfs/roberts/scratch/pi_nrs36/jar335/probes/ev/`, logs in
`../logs/ev_*`. Every table below is from `EV_TABLES.md` in this folder
(a copy is in `$Q`), written by `ev_collect.py`.

| Step | Script | Jobs |
|---|---|---|
| Check of probe C's file formats and row indices | `ev_inspect.py` | 29534326 |
| The scorer for completed wealth, SCF test records and PUF records | `ev_score.py` | 29535105–08 (first runs) |
| Transformer: probe C's `model_cz` (zero coding), seeds 1–3, SCF test records with the bridge variables observed and hidden, and PUF training records; probe C's unknown-coded model (`model`) as a second transformer column | `ev_transformer.sh`, `ev_transformer2.sh` | 29535507, 29535746 |
| Probe V's scorecard: the transformer's existing scorecards (probe C ran `v_score.py` unchanged on sampling seed 1 of each model seed), the PUF validation records and probe BL's minimum-leaf-20 trees (probe V's files), relabelled | `ev_prep_v.py` | in 29535507 |
| Waiting job: scores probe TR's files as they appear (`ev_score.py`; `v_score.py`, unchanged, for generated files) and rewrites the tables | `ev_tr_wait.sh` | 29535749 (running) |
| Tables | `ev_collect.py` (uses `v_collect.py` unchanged) | 29536224 and later |

**The scorer.** It reads any file in probe C's completion format (one row
per record and draw: the record's row in `full_v3cz`, the draw number, the
24 wealth variables in dollars). Nothing in it depends on how the draws
were made.

- *SCF test records.* The 12,021 SCF test records with wealth observed
  (split-off units have no wealth and are left out; 2,207 households, both
  waves pooled with their own weights). Groups: income (test 2's
  seven-income total, thresholds at the 50th, 90th, 99th and 99.9th
  weighted percentiles of each wave's SCF training records, as probes Q and
  X did), age of the head (six ranges) and filing status. Each draw counts
  with its record's weight divided by the number of draws. The untargeted
  relationships follow probe Q's `q_eval.py`: holding rates and mean net
  worth by income × age, joint holding of the 190 pairs of the 20 asset and
  debt items (overall and within income groups), rank correlation of net
  worth with eleven incomes, the top 1% share of net worth within each
  income group, and mortgage holding among homeowners. The per-record
  score is the energy score: for each household, the average distance
  between its true wealth vector and each of its five drawn vectors, minus
  half the average distance between pairs of drawn vectors, after each of
  the 20 asset and debt amounts is replaced by its weighted rank (0 to 1)
  among the true values; it is lowest when the draws are centred on the
  household's truth and spread no more and no less than the uncertainty
  warrants. A second version uses net worth alone. As a yardstick, draws of
  other households' whole wealth at random score 0.734 (20 items) and
  0.201 (net worth).
- *Sampling error.* 100 bootstrap resamples of SCF households (all
  implicates of a household together), the same resamples for every file.
  For each resample the truth and every method's statistics are recomputed
  on the resampled records, so the sampling error of the difference
  between two methods is the spread of that difference over resamples.
  "Better" in the tables means a difference of more than two sampling
  errors.
- *PUF records.* Probe X's and C's comparison (`c3_wealth.py`), rewritten
  to take any file: PUF training records, five draws each, grouped by true
  AGI, against the SCF 2013 and 2016 training records interpolated to 2015
  (one third 2013, two thirds 2016; dollar levels in logs). The SCF
  benchmark covers units with wealth observed; the all-returns mean and
  median with split-off units counted at zero are shown too, as X did. The
  SCF's sampling error is again 100 household bootstraps. This is a
  benchmark, not the truth for the PUF records: the PUF's top 0.1% by AGI
  is not the same people as the SCF's top 0.1% by its income measure.
- *Generated records.* Probe V's `v_score.py`, unchanged, on each file.
  "Sampling error" for this part is what the 20,768 real PUF validation
  records score when treated as a generated file.

**What was compared.** Transformer = probe C's model trained on
`full_v3cz` (zero coding), three training seeds; probe C's model with
deductions unknown for non-itemizers (`full_v3c`) is shown as a second
column where it matters. Trees = probe TR's `tr_synth.py`: tax variables
by probe BL's method (unweighted, weight drawn first, minimum leaf 20);
wealth by one tree per wealth variable fitted on SCF training records,
survey-weighted, minimum leaf 50, predictors the shared variables' rank
tokens plus earlier wealth, decoded to dollars as the transformer's draws
are. TR's arms: `zero` (every PUF record uses trees that see mortgage
interest and real estate tax, zeros read as zeros: the like-for-like
comparison with `model_cz`), `split` (non-itemizers use trees without
those two variables), `split_wv` (the SCF side trained unweighted with the
weight as a variable). One tree fit per arm (five draws), against three
transformer training seeds.

## Summary table

Transformer = `model_cz`, mean over three seeds (range in brackets). Trees
= `split_leaf50` for SCF test records, `zero_leaf50` for PUF records,
`joint_leaf50` (two files) for generated records. The full table, with
every row, is at the top of `EV_TABLES.md`.

| Check | Truth | Transformer | Trees | Sampling error | Better |
|---|---|---|---|---|---|
| **SCF test records** | | | | | |
| Score per household, 20 assets and debts (lower is better; random other household 0.734) | — | 0.5420 [0.5415–0.5426] | 0.5498 | 0.0014 (difference) | transformer |
| Score per household, net worth alone (random 0.201) | — | 0.092 | 0.093 | 0.0009 (difference) | neither |
| Mean net worth by income group, average miss (%) | 0 | 20.7 [17.9–22.6] | 21.0 | 2.7 | neither |
| Median net worth by income group, average miss (%) | 0 | 13.9 | 18.6 | 5.3 | neither |
| Holding rates of 9 items by income group, average gap (points) | 0 | 2.57 | 3.06 | 0.26 | neither |
| Mean net worth by age group, average miss (%) | 0 | 27.0 | 20.4 | 4.0 | neither |
| Mean net worth by filing status, average miss (%) | 0 | 16.0 | 7.6 | 3.9 | trees |
| Holding rates of 20 items by income × age, average gap (points) | 0 | 2.39 | 2.36 | 0.07 | neither |
| Mean net worth by income × age, average miss (% of group mean) | 0 | 35.7 | 22.5 | 4.0 | trees |
| Joint holding of 190 pairs of items within income groups, average gap (points) | 0 | 2.37 | 3.11 | 0.19 | transformer |
| Rank correlation of net worth with 11 incomes, average gap | 0 | 0.022 | 0.022 | 0.003 | neither |
| Top 1% share of net worth within income groups, average gap (points) | 0 | 5.6 | 3.4 | 1.3 | neither |
| Mortgage among homeowners within income groups, average gap (points) | 0 | 0.8 | 1.6 | 0.6 | neither |
| Mean net worth, 99th–99.9th percentile of income | $12.8m | $11.4m | $11.7m | $1.3m (truth) | neither |
| Mean net worth, top 0.1% of income | $44.9m | $32.1m [$31.0–34.3m] | $19.5m | $9.1m (truth) | transformer |
| Score per household, mortgage interest and real estate tax hidden | — | 0.596 | 0.602 | 0.0017 (difference) | transformer |
| Same, net worth alone | — | 0.111 | 0.108 | 0.0010 (difference) | trees |
| **PUF records against the SCF in 2015** | | | | | |
| Mean net worth, top 1% of AGI | $16.6m | $13.3m | $10.2m | $0.7m (SCF) | transformer |
| Mean net worth, top 0.1% of AGI | $54.7m | $38.6m [$37.9–39.7m] | $25.5m | $3.6m (SCF) | transformer |
| Median net worth, top 0.1% of AGI | $32.7m | $21.2m | $13.0m | $3.5m (SCF) | transformer |
| Own a home, all returns (%) | 66.5 | 28.0 | 28.3 | 0.7 (SCF) | equal (both wrong) |
| Mean net worth by AGI group, average miss from the SCF (%) | 0 | 12.3 | 28.6 | — | transformer |
| Same with non-itemizers handled (transformer deductions unknown; trees `split`): own a home (%) | 66.5 | 56.7 | 60.7 | 0.7 (SCF) | trees |
| Same: mean net worth, top 0.1% of AGI | $54.7m | $37.8m | $26.2m | $3.6m (SCF) | transformer |
| **Generated whole PUF records** | | | | | |
| Total AGI ($bn) | 8,094 | 7,819 | 8,184 | 65 (validation miss) | trees |
| Share of AGI above the real 99th percentile (%) | 19.8 | 19.4 [18.5–20.1] | 20.8 | 0.0 (validation miss) | about equal |
| Net income tax, all returns ($bn; probe V's simplified 2015 tax) | 1,095 | 1,093 [1,077–1,109] | 1,132 | 11 (validation miss) | transformer |
| Net income tax, top 0.1% group ($bn) | 204 | 204 [189–216] | 214 | 5 (validation miss) | neither (within seed spread) |
| Largest gap in average tax rate by AGI group (points) | 0 | 2.69 | 0.19 | 0.15 | trees |
| Dollar miss summed over 30 amounts × 5 AGI groups ($bn) | 0 | 877 | 368 | 492 | trees |
| Share with a nonzero amount, average gap (points) | 0 | 2.31 | 0.79 | 0.56 | trees |
| Co-holding of 28 pairs of items, average gap (points) | 0 | 2.49 | 0.90 | 0.56 | trees |
| Returns breaking a rule real records never break (%) | 0 | 1.00 | 0.30 | 0.02 | trees |
| Classifier telling generated from real (0.5 = cannot tell) | 0.50 | 0.814 | 0.562 | 0.501 | trees |
| Same, top 0.1% group | 0.50 | 0.802 | 0.648 | 0.507 | trees |
| Identical to a training record on all 37 variables (%) | — | 25.7 | 28.5 | 26.7 (unseen real records) | neither |

## Detailed tables

### SCF test records by income group

Truth is the records' own wealth; the sampling error is the bootstrap
standard deviation of the truth. Mean over the transformer's three seeds.
Tables by age group and filing status, and every method's column, are in
`EV_TABLES.md`.

| Income group | Statistic | Truth | Sampling error | Transformer | Trees | Trees, SCF weight as a variable |
|---|---|---|---|---|---|---|
| Bottom 50% | mean net worth | $127k | $14k | $158k | $142k | $182k |
| | median net worth | $18k | $2k | $17k | $18k | $19k |
| | own a home (%) | 47.6 | 1.8 | 47.2 | 48.1 | 48.0 |
| | mean stocks | $6k | $2k | $17k | $14k | $16k |
| 50th–90th | mean net worth | $378k | $36k | $514k | $469k | $586k |
| | median net worth | $145k | $10k | $146k | $160k | $154k |
| | mean business | $35k | $6k | $93k | $53k | $84k |
| 90th–99th | mean net worth | $1.9m | $143k | $2.0m | $2.0m | $2.8m |
| | median net worth | $732k | $96k | $799k | $808k | $909k |
| | hold a business (%) | 28.5 | 2.7 | 25.1 | 25.5 | 27.7 |
| 99th–99.9th | mean net worth | $12.8m | $1.3m | $11.4m | $11.7m | $15.8m |
| | median net worth | $8.0m | $1.2m | $6.1m | $6.3m | $6.6m |
| | hold a business (%) | 63.2 | 7.1 | 54.6 | 65.3 | 53.4 |
| Top 0.1% | mean net worth | $44.9m | $9.1m | $32.1m | $19.5m | $48.1m |
| | median net worth | $25.6m | $8.1m | $17.8m | $12.8m | $23.1m |
| | hold stocks (%) | 96.4 | 1.7 | 94.4 | 82.1 | 91.3 |
| | hold a business (%) | 84.0 | 7.6 | 67.1 | 63.6 | 68.1 |
| | mean stocks | $8.6m | $2.0m | $6.7m | $4.6m | $10.6m |
| | mean business | $17.6m | $4.9m | $12.9m | $5.9m | $18.3m |

Every method overstates mean stocks in the bottom half and the 50th–90th
percentile (truth $6k and $31k; transformer $17k and $62k; trees $14k and
$51k). Both methods decode a drawn rank band to dollars at a random point
within the band, and the truth here is in actual dollars, so part of this
is the decoding, shared by both. The transformer also overstates mean
business wealth in the 50th–90th percentile ($93k against $35k; trees
$53k), which is most of its overstatement of mean net worth there.

Score per household by income group (lower is better):

| Income group | Transformer | Trees | Trees, SCF weight as a variable |
|---|---|---|---|
| Bottom 50% | 0.456 | 0.458 | 0.459 |
| 50th–90th | 0.589 | 0.600 | 0.597 |
| 90th–99th | 0.660 | 0.676 | 0.677 |
| 99th–99.9th | 0.739 | 0.774 | 0.741 |
| Top 0.1% | 0.681 | 0.739 | 0.711 |

The transformer's advantage on the per-household score grows with income
and is largest at the top.

### Relationships no target covers, SCF test records

| | Truth | Sampling error | Transformer | Trees |
|---|---|---|---|---|
| Rank correlation of net worth with wages | 0.236 | 0.024 | 0.220 | 0.232 |
| with taxable interest | 0.582 | 0.016 | 0.562 | 0.559 |
| with dividends | 0.442 | 0.021 | 0.437 | 0.426 |
| with Social Security | 0.235 | 0.022 | 0.210 | 0.208 |
| Top 1% share of net worth within the bottom half (%) | 31.3 | 5.0 | 37.1 | 30.9 |
| within the 50th–90th percentile (%) | 20.0 | 4.4 | 28.4 | 21.7 |
| within the 90th–99th (%) | 11.8 | 1.1 | 18.5 | 19.1 |
| within the 99th–99.9th (%) | 10.5 | 1.7 | 14.4 | 15.9 |
| within the top 0.1% (%) | 13.5 | 2.1 | 10.3 | 11.2 |
| Mortgage among homeowners (%) | 70.9 | 1.6 | 71.3 | 69.5 |
| Stocks among business owners (%) | 36.9 | 3.0 | 40.0 | 38.8 |
| Mortgage among owners of other homes (%) | 41.8 | 3.1 | 38.9 | 38.7 |

Both methods weaken every correlation of net worth with income slightly,
by about the same amount.

### PUF records against the SCF interpolated to 2015

Groups by true AGI for the PUF and by the seven-income total for the SCF.
"Zero" = mortgage interest and real estate tax of non-itemizers read as
zero paid (transformer `model_cz`, trees `zero`); "handled" = transformer
with deductions unknown for non-itemizers (probe C's `model`, three seeds),
trees `split`.

| | SCF 2015 | Sampling error | Transformer, zero | Trees, zero | Transformer, handled | Trees, handled | Trees, SCF weight as a variable |
|---|---|---|---|---|---|---|---|
| All returns, mean | $628k | $17k | $508k | $439k | $559k | $498k | $676k |
| All returns, median | $91k | $4k | $24k | $22k | $56k | $62k | $64k |
| Mean, bottom half | $113k | $5k | $105k | $74k | $153k | $122k | $150k |
| Mean, 50th–90th | $370k | $13k | $362k | $328k | $423k | $408k | $503k |
| Mean, 90th–99th | $2.1m | $0.1m | $2.0m | $1.9m | $2.0m | $1.9m | $2.5m |
| Mean, 99th–99.9th | $12.3m | $0.7m | $10.4m | $8.5m | $10.2m | $8.6m | $13.3m |
| Mean, top 0.1% | $54.7m | $3.6m | $38.6m | $25.5m | $37.8m | $26.2m | $52.2m |
| Median, top 0.1% | $32.7m | $3.5m | $21.2m | $13.0m | $20.7m | $13.3m | $21.4m |
| Own a home, all (%) | 66.5 | 0.7 | 28.0 | 28.3 | 56.7 | 60.7 | 60.3 |
| Own a home, bottom half (%) | 48.8 | 1.1 | 7.4 | 8.8 | 36.5 | 42.6 | 42.4 |
| Have a mortgage, all (%) | 45.6 | 0.7 | 21.6 | 21.6 | 42.3 | 45.1 | 44.7 |
| Hold stocks, top 0.1% (%) | 93.5 | 2.7 | 89.6 | 83.6 | 88.9 | 83.8 | 84.0 |
| Hold a business, top 0.1% (%) | 85.2 | 6.1 | 73.3 | 68.5 | 70.8 | 69.2 | 65.0 |
| Top 1% share of net worth (%) | 37.8 | 0.8 | 41.5 | 40.0 | 38.5 | 35.7 | 46.0 |

With split-off units counted at zero the SCF's all-returns median is $57k
and mean $545k. For PUF returns that are not dependent filers the median is
$68k (transformer, handled) and $74k (trees, handled).

Probe TR's trees take a few amounts from one SCF household: 0.6% of
completed PUF records carry three or more nonzero wealth amounts from the
same SCF household, 0.003% five or more (TR's `_info.json` files). The
transformer has no equivalent count.

### Generated records: tax by AGI group

Net income tax after credits under probe V's simplified 2015 calculator
($bn) and average tax rate (%). Transformer: three model seeds; trees: two
files.

| AGI group | Real | PUF validation | Transformer | Trees | Real rate | Transformer rate | Trees rate |
|---|---|---|---|---|---|---|---|
| Bottom half | −37 | −39 | −30 | −35 | −4.48 | −3.93 | −4.31 |
| 50th–90th | 325 | 324 | 321 | 321 | 9.52 | 9.56 | 9.50 |
| 90th–99th | 373 | 367 | 368 | 384 | 16.60 | 16.90 | 16.77 |
| 99th–99.9th | 230 | 232 | 231 | 248 | 26.35 | 27.20 | 26.45 |
| Top 0.1% | 204 | 200 | 204 | 214 | 27.99 | 30.68 | 28.15 |
| All | 1,095 | 1,084 | 1,093 | 1,132 | 13.53 | 13.98 | 13.83 |

The transformer's total tax is right because two errors offset: too little
AGI in the top 0.1% ($665bn against $731bn) taxed at too high an average
rate (30.7% against 28.0%). The trees put slightly too much AGI above the
99th percentile ($761bn in the top 0.1%) and tax it at the right rates.
The full scorecard, with every variable by AGI group, is in
`EV_TABLES.md`.

## What it means for the choice of model

- **Generation of tax records.** The trees are better by a wide margin on
  every distributional check, and their tax by AGI group is right to
  within 0.2 points of the average rate. This repeats probe BL's result on
  the multi-survey dataset. The trees' small overshoot above the 99th
  percentile is the one thing to fix.
- **Wealth completion.** The transformer is a little better overall (a
  1.4% lower score per household, more than five sampling errors of the
  difference), and the gain is in
  the upper income groups. On most tables the two are within sampling
  error of each other. The transformer overstates mean wealth in the
  middle of the income distribution, which the trees do not. Neither
  method gets the top 0.1% right on held-out SCF records ($32m and $20m
  against $45m), so the low top seen on PUF records by probe C is mostly a
  problem of completing wealth at the top, not of the PUF looking
  different from the SCF.
- **The trees' gap at the top looks fixable.** Trained with the SCF weight
  as a variable, the trees reach $48m in the SCF's top 0.1% and $52m for
  PUF returns, but overstate the middle; some setting between the two (a
  smaller leaf at the top, or weighting between survey weights and none)
  has not been tried.
- **Overall.** The transformer's case on this evidence is a modest gain in
  wealth completion. Against that, its generated tax records are clearly
  worse. A combination is possible: trees for the tax side, and for
  wealth whichever method wins once the trees' top is tuned. Not tested
  here: stretching to another year (probe ST's test) for the trees, which
  is the other place the plan expected the transformer to earn its cost.

## To finish

Every result in this report is from files that existed at 10:40; nothing
in the tables above is pending. The scorer keeps running so that any later
probe TR file is scored the same way.

| Job | State at writing | What it does | Writes |
|---|---|---|---|
| 29535749 (`ev_tr_wait.sh`, CPU) | Running | Every 5 minutes, scores any new or rewritten file in `probes/tr/comp/` (`tr_scf_test_*`, `tr_scf_test_nobr_*`, `tr_puf_wealth_*`) and `probes/tr/gen/` (`tr_*_s<seed>.parquet`), each only once its `_info.json` exists; then rewrites the tables. Ends when probe TR has no job in the queue and nothing new has appeared (not before 13:00 on 14 October), or at 08:00 on 15 October. | `$Q/scf/*.json`, `$Q/puf/*.json`, `$Q/v/*.json`; tables in `EV_TABLES.md` (this folder) and `$Q/EV_TABLES.md`; log `$Q/wait.log` |

To finish: if `$Q/wait.log` shows new files scored after 10:40, read the
summary at the top of `EV_TABLES.md`. Its trees columns are fixed to TR's
minimum-leaf-50 files (`split_leaf50`, `zero_leaf50`, `joint_leaf50`), so
the summary table above does not change; any new TR variant appears as an
extra column in the detailed tables of `EV_TABLES.md`, and is worth adding
to this report only if it changes the top 0.1% rows. A failed score is
logged in `$Q/wait.log` as "FAILED" with its log file.

Not done: wealth on the trees' generated joint records (the transformer's
generated files carry no wealth, so there is nothing to compare against);
the stretching test for the trees; a Tax-Calculator run on the trees'
generated files (probe V's simplified calculator only).
