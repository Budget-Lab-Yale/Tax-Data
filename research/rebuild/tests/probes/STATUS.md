# Test 4 probes: status log

Append-only. Contract: `SPEC.md`.

9 October, 23:45 (main session): six probes started as subagents (T, Y, X,
Q, BR, AU). Session ends about 03:30 on 10 October; reports due 02:50.

9 October, 23:45 (probe BR): started. Reading the plan and
test 3's data and model reports; first job lists the SCF 2013 and 2016 items
on mortgages, property tax and charitable giving.

9 October, 23:37 (probe Q): started. Plan: retrain the version 2 model
with checkpoints saved at epochs 1, 2, 4, 8, 12, 20, 30 and the best epoch
(job 28922153, `q_train.sbatch`, output `$Q/model/`); tilt each checkpoint
to the SCF training records' own wealth tables by income group, age,
filing status and children (copy of piece C's method), and measure
untargeted wealth statistics before and after the tilt, on the training
and test records.

9 October, 23:50 (probe Y): started. Plan: write `y_translate.py` (a
general function translating a survey's shared variables into a reference
file's concepts, per D24), apply it to half 2 of each of test 2's
experiment A datasets with half 1 as the reference, and compare wealth by
income group against half 1's true wealth, alongside no adjustment and
reweighting. A fifth benchmark, half 2 with the same distortion applied
directly, shows what a perfect translation would give apart from sampling
differences between the halves. Also two added variants in which
non-reporting depends on wealth.

10 October, 00:00 (probe T): started. Reading done. Plan: (1) a diagnostic
job on the version 2 seed 1 model: the model's predicted distribution of the
AGI token given only the first six generated variables, against the weighted
truth; components drawn given the true AGI; tokenizer decoding by AGI group.
(2) Retrains on `full_v2` with a copy of `h_train.py` (`t_train.py`):
stopping on the PUF part of the validation loss; a learning-rate schedule
that actually decays; extra training weight on top-AGI PUF records with the
generated records weighted back. Outputs under
`/nfs/roberts/scratch/pi_nrs36/jar335/probes/t/`.

9 October, 23:50 (probe X): started. Writing my own translation script,
`x_translate.py` (no `y_translate.py` interface posted yet). Plan: translate
the SCF 2013 and 2016 rows of `full_v2` onto the PUF 2015 distribution of
each shared variable, build `full_v3x` in `/nfs/roberts/scratch/pi_nrs36/jar335/probes/x/`,
rebuild the arm P targets and their disagreement, retrain (seeds 1 and 2),
complete PUF wealth, compare with the untranslated model and the SCF.

9 October, 23:40 (probe AU): started. Reading the plan, D20, D23, D24, the
test 3 data report and crosswalks, the test 2 crosswalk, the current
pipeline's demographic code (`process_puf.R`, `ages.R`, `demographics.R`)
and the ASEC unit builder. Writing `AU_AUTHORITY_TABLE.md` from files only;
no Slurm jobs planned.

9 October, 23:50 (probe Y), for probe X: the translation function is in
`y_translate.py` (runs end to end on test 2's data; results to follow).
Interface: `from y_translate import translate, TranslateSpec`;
`out, diag = translate(survey_df, reference_df, TranslateSpec(amounts=[...
chain order...], cell_vars=[categorical cell columns, coarsest first],
categorical_crosswalks={col: fn(survey_df, rng) -> reference-concept
values}, flip_features=[survey-only columns such as wealth, used to choose
who is switched between zero and nonzero], weight="weight",
ref_weight="weight", household="hh_id", n_draws=5,
flip_rule="proportional"|"rank"|"random", total_groups=3,
min_cell_households=40, min_cell_ref=40, seed=1))`. Both tables need the
amount and cell columns with no missing values (the flip features may have
missing values). `out` has n_draws copies of each survey row (column
`draw`, weight divided by n_draws), translated values in the amount
columns, the survey's own values in `src_<amount>`, and `flip_<amount>`
(+1 switched on, -1 switched off). `diag` gives shares switched and rank
moves per amount. Amounts are translated by weighted rank among nonzero
values (signed) within cells of married-type categoricals and a group of
the running total of amounts already translated. Also `rake(...)` for the
reweighting comparison. Needs scikit-learn (module line as in
`y_py.sbatch`).

10 October, 00:05 (probe T): first result, step 1 (job 28922334,
`t_diag.py`, test 3 version 2 seeds 1-3, PUF training records). The model's
predicted AGI distribution, averaged over the records with population
weights, against the truth (weight at or above the 99th percentile band of
positive AGI: truth 0.99%):
- nothing visible: 0.97% / 0.95% (seeds 1, 2); right.
- every other PUF variable visible: 1.01% / 0.97%; right.
- only the first six generated variables visible (MARS, DSI, XTOT, N24,
  EIC, FDED): 0.63% / 0.63%; this is the step where the top is lost.
- the same six plus age, gender, earnings split: 0.32% (the old order).
The generated six variables themselves match the truth (itemizers 31.0%
against 29.8%). The shortfall sits in itemizers: joint itemizers 2.9%
predicted against 4.7% true; single itemizers 0.4% against 1.0%. So the
error is in what the model learned for this low-information question, not
compounding of later draws. Retrains running: 28922237 (stop on PUF loss),
28922238 (decaying learning rate), 28922239 (extra weight on top records,
weighted back), 28922416 (half the records trained on the exact
generation question).

9 October, 23:46 (probe AU): finished. `AU_AUTHORITY_TABLE.md` written:
one row per variable (the 40 PUF-side variables, the 24 wealth variables,
and the demographics the plan wants from Census), a table of what the PUF
lacks with where the model learns it and its post-training targets, a
proposed order for the rest of D24's chain, and 13 open questions. Built
from files only; no jobs run. Two things to verify against the SCF
documentation (not on disk): whether the `db` wealth variable (`FUTPEN` +
`CURRPEN`) is account-type balances rather than defined-benefit value, and
whether `re_fund` (`NNRESRE`) is net of debt.

9 October, 23:50 (probe BR): feasibility answered: yes for mortgage interest
and real estate tax, partly for charity. Both SCF waves (raw files
`p13i6.dta`, `p16i6.dta` in `shared/raw_data/SCF/v1/<wave>/historical/`)
have, for every owner: real estate taxes on the home (X721 amount, X722
frequency; 96 percent report annually); balance and annual interest rate of
each loan on the home (X805/X816, X905/X916, X1005/X1016, other purchase
loans X1044/X1045), of home-equity lines (X1108/X1111 etc., secured if
X1103 = 1), and of loans on other real estate (X1715/X1726, X1815/X1826,
with type X1703 and whether it yields income X1729). Charitable giving is
asked (X5822, X5823) but only gifts of $500 or more: the smallest positive
amount is $500 in both waves. On the PUF side, `full_v2` codes all six
itemized deductions as 0 (not unknown) for the 70 percent of returns that
do not itemize (FDED 2 or 3). Building `full_v3br`: mortgage interest
(balance times rate for the home, home-equity lines and personal-use second
homes, limited to interest on $1 million of purchase debt and $100,000 of
home-equity debt) and real estate tax observed on SCF units; on the PUF
both made unknown for non-itemizers; SCF amounts ranked on the PUF's dollar
scale. Charity not built (the $500 floor makes an SCF zero mean "under
$500"). Details in BR_REPORT.md when written.

9 October, 23:50 (probe X): translation done (`x_translate.py`, CPU job
28922552; dataset `full_v3x` in `/nfs/roberts/scratch/pi_nrs36/jar335/probes/x/data/`).
Arm P targets rebuilt from it (`x_targets.py`). C's disagreement measure,
largest spread as a share of all PUF weight: core set 3.5% before, 3.5%
after; full set 6.9% before, 3.7% after (the income-item families no longer
add anything; what remains is the same in both sets and comes from the
married and dependents families, which differ in population, not meaning:
split-off units have no wealth and the PUF includes dependent filers).
Training seeds 1 and 2 running (28922598, 28922599); completions
(28922880/1) and the tilt on the core set (28922882) chained after them.

9 October, 23:56 (probe Q): retraining done (job 28922153; best epoch 15,
stopped at 30; checkpoints at epochs 1, 2, 4, 8, 12, 20, 30 in `$Q/model/`).
Tilt runs (`q_fit.py`, one GPU job each) running for every checkpoint plus
two deliberately weak versions of the best model (incomes hidden; all
inputs hidden): jobs 28922633–28922637, 28922883–28922885, 28923169–28923170.
Outputs `$Q/runs/<name>/`. Then `q_eval.py` and `q_table.py` (CPU).

10 October, 00:05 (probe Y): first results. All variants ran (tables in
`/nfs/roberts/scratch/pi_nrs36/jar335/probes/y/y_summary.md`). Two designs:
test 2's halves (reference = half 1, a different sample) and "same
households" (reference = half 2 with the distortion applied). In the halves
design the sampling difference between halves (mean net worth by income
group off by about 13% even for a perfect translation) swamps every method
effect; the same-households design is the informative one. There: (1) with
no difference in meaning, translation is the identity (errors under 0.5%);
(2) where the survey under-reports an income (the SCF's direction against
the PUF), switching zeros on by likelihood fixes most of the error in
wealth by reporting status (6.3% to 3.8%, or 2.6% taking the most likely
first; random switching 8.0%); (3) where the file under-reports, removing
the "least likely" holders is wrong even when non-reporting is unrelated to
wealth (4.4% untranslated, 4.7% translated, 0.8% random); (4) moving
business income into wages is made worse by translation (it should be a
definitional rule); (5) noise is not undone; (6) reweighting is worst on
nearly every measure. Retraining test 2's model on translated `unrep`
data done (3 seeds); a second retrain with random switching queued.

10 October, 00:20 (probe T): second result. Drawing AGI first (before
filing status and the other categorical variables; demographics still
last), with the unchanged test 3 version 2 models, no retraining (jobs
28922607-09, levels by job 28923131): total AGI $8.05tn / $7.89tn / $7.60tn
(seeds 1-3) against $8.09tn; weight above the true 99th percentile 0.99% /
0.96% / 0.88% against 1.00%; top 1% share of AGI 19.2% / 17.4% / 16.1%
against 19.4%. The model's AGI distribution with nothing visible is right,
so asking it first recovers the AGI distribution. The mix of income at the
top is still wrong even given the true AGI (job 28922334): in the top 0.1%,
capital gains $200bn against $245bn, partnership and S corporation income
$105bn against $175bn; in the bottom half, capital gains $122bn against
$13bn. Next: the same checks on the retrained variants.

10 October, 00:20 (probe Y): finished; report `Y_REPORT.md`. Main points:
translation is the identity when nothing differs; where the survey misses
amounts the file records (the SCF's direction), switching zeros on by
likelihood given wealth removes most of the error in wealth by reporting
status (6.3% to 2.2–3.8%; random switching 8.0%). D23's rule for the other
direction ("remove the least likely holders") is wrong: removals should be
random within cells, which follows from D24's own assumption; a rule doing
"likely on, random off" (`flip_rule="likely_on_random_off"` in
`y_translate.py`, recommended to probe X) was best or within a point of
best in all six non-reporting variants. Business income reported as wages
is made worse by translation (needs a definitional rule); noise is not
undone; reweighting is worst. Retraining test 2's model on translated data
showed no measurable gain: the model's own error and seed spread are far
larger. No jobs left running.

10 October, 00:20 (probe X): following probe Y and the coordinator, the
translation rule now switches holders off at random within the cell
(switching on stays most likely first). New dataset `full_v3x2`; the first
version (`full_v3x`, least likely switched off) is kept as a sensitivity
check. Retraining seeds 1 and 2 on `full_v3x2` (28924153/4), completions,
targets and tilt chained. First result on `full_v3x` (seed 2): completed
PUF wealth moved away from the SCF, not toward it (median $26,000 against
$55,000-75,000 untranslated and the SCF's $91,000; home ownership 40%
against 52-56% and the SCF's 67%). A model-free check (SCF reweighted so
its shared variables look like the PUF's) says translation should lower the
median only to about $64,000; testing whether the PUF-only variables
visible on PUF records drive the rest (completions with them hidden).

10 October, 00:12 (probe Q): first results (epochs 2, 4, 8, 12, 15 = best,
30): on the training records, once tilted, every model from epoch 4 on gets
the untargeted wealth statistics about as right as the best one, near the
truth's own sampling error; the epoch 2 model (tilt 4.6 nats) does not, and
its tilt makes some untargeted relationships worse (mortgage among
homeowners). Still running: epochs 1, 3, 5, 6, 20 and the two weak versions
of the best model; evaluations chained after each (`$Q/eval/`).

10 October, 00:27 (probe BR): `full_v3br` (mortgage interest and real
estate tax as bridge variables) and `full_v3brc` (plus cash charity) built
and trained, two seeds each (jobs 28922897-8, 28923139-40). First result,
SCF test records, wealth completion score with the bridge variables
observed: 33.52 and 33.45 nats per record against version 2's 34.69 (both
seeds); with charity too 33.39 and 33.46. Tables comparing completed PUF
wealth are running (job 28924851).

10 October, 00:55 (probe BR): done; report `BR_REPORT.md`. With mortgage
interest and real estate tax shared, the wealth completion score on held-out
SCF records improves by 1.2 nats per record (33.45 and 33.52 against
34.69-34.71), almost all in home value and mortgage debt, and is unchanged
when they are hidden. PUF itemizers' completed housing wealth now follows
the deductions as in the SCF; overall net worth moves little. Charity as a
third bridge adds nothing measurable. No jobs left running.

10 October, 00:55 (probe T): interim results on retrains (seed 1 each;
levels job 28925594; 664,624 generated records each where marked 4x).
- Longer training with a decaying learning rate (`sched`): the PUF-best
  checkpoint (epoch 60) does not fix the first-six-variables question
  (0.65% at the top band against 0.99%) and its SCF wealth completion
  score worsens from 35.48 to 38.68. Early stopping is not the cause.
- Extra weight on top-AGI records, weighted back (`tilt`): AGI right when
  AGI is drawn first ($8.11tn, top 1% share 19.4%), but the model's
  incomes given a low AGI go wrong (bottom half capital gains $659bn
  against $13bn). Rejected.
- Half the records trained on the exact generation question (`mixord`,
  still running): the first-six question improves to 0.91%; given the true
  AGI, spurious bottom-half capital gains and partnership income mostly go
  ($21bn against $13bn; -$7bn against -$31bn). Costs any-order PUF
  validation loss (0.742 against 0.727) and over-predicts the top when
  every other variable is visible (1.23% against 1.00%).
Queued: 60-epoch versions of `mixord` (two seeds) and of the same with AGI
first in the trained order (two seeds); larger model; stop-on-PUF run.

10 October, 00:55 (probe X): results on `full_v3x2` (switch on by
likelihood, off at random). Target disagreement: core 3.52% before and
after; full set 6.85% -> 3.72%; the rest is coverage (SCF split-off units
have no wealth, PUF has dependent filers), not meaning. Completed PUF
wealth (seeds 1, 2): mean by income group now within ~10% of the SCF in
every group (untranslated: 35-100% too high below the 90th percentile);
overall mean $569k/$561k (SCF $628k; untranslated $713k-$904k); median
$50k/$32k (untranslated $55k-$75k; SCF $91k, or $57k counting split-off
units at zero); home ownership in the bottom half still 22-34% against
49%. Tilt on translated core targets (first version, seed 1): KL 0.554
against C's 0.689. Tilt on `full_v3x2` (28924158) running; report
`X_REPORT.md` drafted.

10 October, 00:54 (probe Q): done; report `Q_REPORT.md`, full tables
`q_tables_full.md`. Nothing of probe Q is running. Main result: after the
tilt, statistics built from the targeted groupings come out right for every
model with a tilt up to about 1.3 nats per record; relationships no target
covers (between wealth items; wealth against particular incomes) keep the
pretrained model's errors, and tilts of 1.8 nats or more make some worse.
The tilt size tracks this along the training ladder (fine at 0.6 nats or
less) but cannot see an error along a direction the targets do not measure.

10 October, 01:12 (probe X): finished. `X_REPORT.md` written. Tilt on the
translated core targets (`full_v3x2`, seed 1, 16 draws): KL 0.565 nats per
unit of weight against C's 0.689; first version 0.554. No jobs left running.

10 October, 01:30 (probe T): report written, `T_REPORT.md`. In brief:
the model knows the PUF's top (AGI with nothing visible: 0.90-0.97% of
weight in the top 1% band, truth 0.99%); the top is lost when AGI is drawn
given only the six categorical variables that precede it (0.60-0.63%).
Drawing AGI first, no retraining: total AGI $7.74-7.98tn against $8.09tn,
top 1% share 17.3-18.3% against 19.4% (three seeds). Stopping on the PUF
loss, a decaying rate, a larger model and extra weight on top records do
not do better and the first three harm SCF wealth completion (38.7-46.2
against 35.5). Training half the records on the generation questions in
the AGI-first order (two seeds) holds AGI and cleans up bottom-half
incomes. Still wrong in every variant: the mix of income in the top 0.1%
(components sum to $532bn against $704bn given the true AGI). Still
running: job 28923343 (larger model; read its run file when it ends).

10 October, 02:05 (probe T): third seed of the AGI-first ordered-task
variant (jobs 28927465-66, levels 28929416) overshoots the top (top 1% share
22.3%, top 0.1% 11.2% against 19.4% and 8.8%); across three seeds 18.9% to
22.3%, against 17.3% to 18.3% for the unchanged model with AGI drawn first.
`T_REPORT.md` updated. Recommendation unchanged: draw AGI first; the ordered
task is promising for the bottom of the income mix but not settled. All
probe T jobs have ended. Done.

## 10 October, 02:05 (main session): all six probes have reported

- **T (top of the PUF).** The model has learned the top; it is lost at one
  step, drawing AGI given only the six categoricals drawn before it (0.60–0.63%
  of weight above the true 99th percentile against 0.99%). Drawing AGI first,
  no retraining: total AGI $7.74–7.98tn against $8.09tn, top 1% share
  17.3–18.3% against 19.4%. Training changes did not beat this. Unsolved: the
  income components of the top 0.1% sum short ($532bn against $704bn).
- **Y (translation where truth is known).** D24 works when its assumptions
  hold; reweighting is worst throughout. Change to the flip rule: switch on by
  likelihood, switch off at random within cells.
- **X (real SCF translated).** Largest target disagreement on the full set
  6.85% → 3.72%; core unchanged at 3.52% (a coverage gap: split-off units,
  dependent filers). Completed PUF mean net worth within about 10% of the SCF
  in every income group; tilt KL 0.689 → 0.565.
- **Q (how good must pretraining be).** The tilt fixes anything built from the
  targeted groupings for nearly any model with a move under about 1.3 nats;
  relationships no target covers keep the model's errors; the move size is
  only a partial warning.
- **BR (bridge variables).** Mortgage interest and real estate tax from the
  SCF improve held-out wealth completion by about 1.2 nats per record. Found
  that `full_v2` codes non-itemizers' itemized deductions as 0, not unknown.
- **AU.** Draft authority table, `AU_AUTHORITY_TABLE.md`, with six open
  questions for John.

10 October, 07:55 (main session): probes O (generation orders, model
unchanged) and S (training on generation questions) started; see SPEC.md,
contract change 07:55. Reports due 10:45.

10 October, 08:00 (probe O): started. Plan: generate whole PUF records
from test 3's unchanged model (seeds 1 to 3) under eight generation orders
(T's two, income components first, AGI then components by size, test 3's
order reversed, a fresh random order per record, and two orders chosen
record by record from the model's own confidence), 664,624 records each, with
a repeat under a second sampling seed to measure sampling noise; and score
held-out PUF validation records by the chain rule in each order.

10 October, 07:50 (probe S): started. Plan: copy `t_train.py` to
`s_train.py`, add training on generation questions in a random order per
record, and train (60 epochs, decaying learning rate) with AGI-first ordered
questions on 25%, 50% and 100% of records and random-order questions on 50%
and 100%, several seeds each; every checkpoint evaluated with T's tables in
two generation orders (AGI first; demographics last). Scratch:
`/nfs/roberts/scratch/pi_nrs36/jar335/probes/s/`.

10 October, 08:00 (probe S): 16 training runs submitted (jobs 28961245–
28961308, each with an evaluation and two generations queued behind it;
list in `/nfs/roberts/scratch/pi_nrs36/jar335/probes/s/jobs.txt`): AGI-first
ordered questions on 25% (3 seeds), 50% (2 more seeds beside T's 3) and
100% (3 seeds) of records; random-order questions on 50% and 100% (3 seeds
each); and both orders mixed on 50% (2 seeds). The GPU partitions had about
2,700 jobs waiting, so the jobs also use the preemptible partition. Also
queued: repeat generations from T's checkpoints to separate sampling noise
from training-seed differences, and a score of real PUF records under
generation in four orders for T's checkpoints (`s_gennll.py`).

10 October, 08:25 (probe S): first results. The share of ordered AGI-first
questions moves the top up as T found, but much of the spread T saw across
seeds is sampling noise: four draws of 664,624 records from T's seed-1
model give a top 1% AGI share of 17.5% to 19.1%. Random-order training
behaves like the unchanged model (it poses the same questions; only the
weighting differs). A first run with every record on ordered questions
(100%) stopped at epoch 6 because early stopping watches the any-order loss
it no longer trains on; resubmitted to run 60 epochs and keep the last
(jobs 28962040–51). Added: 25% share seeds 4–5 and a 10% share (3 seeds),
jobs 28964777–96, and three more sampling draws of every checkpoint.

10 October, 23:05 (probe O): done; `O_REPORT.md` written (the earlier
session ended before the report). With the model unchanged, the order of
drawing moves the generated top 1% share of AGI from 4.7% to 24.5% (PUF
19.4%). AGI drawn before the income components gets the top roughly right
but leaves false capital and business income in the bottom half. Components
drawn before AGI clean up the bottom but overshoot the top, with AGI far
above its components in the top 0.1%. A fresh random order for each record
is the best compromise run ($7.99tn, 17.8%, bottom-half capital gains $32bn
against $13bn). Held-out log-probability differs by 0.44 nats per record
across orders, which measures the model's inconsistency; it flags the bad
orders but does not by itself pick the best generation order. No order
fixes the mix of income in the top 0.1%. No probe O jobs are running.

10 October, 23:10 (probe AS): started (John's question: make AGI the sum of
its components). Built dataset `full_v2s` in
`/nfs/roberts/scratch/pi_nrs36/jar335/probes/as/data/full_v2s/`: test 3's
version 2 data with AGI (E00100) replaced, in the same column, by a
remainder variable: AGI less the income items in the model (wages, taxable
interest, ordinary dividends, Schedule C, capital gain or loss on line 13
with the $3,000 loss limit, taxable IRA distributions, taxable pensions,
total Schedule E, farm, unemployment) plus the adjustments in the model
(self-employed health insurance, IRA deduction, student loan interest);
definitions in `as_agi.py`. Training with the version 2 recipe submitted,
three seeds (jobs 29087760–62); generation (a fresh random order per record,
and a fixed order with amounts largest first) to follow.

10 October, 23:15 (probe S): done; `S_REPORT.md` written. All 140 jobs
have ended (twelve evaluations that ran out of GPU memory on one bad node
were rerun, jobs 29084013–24). Findings: draw AGI first and train 10–25%
of each batch on the questions generation asks in that order. With 25%
(five seeds, four draws each) total AGI is $7.97tn, top 1% share 19.6% and
top 0.1% share 8.9% (PUF $8.09tn, 19.4%, 8.8%; test 3's model with AGI
first $7.88tn, 17.7%, 7.3%), bottom-half false capital gains $70bn against
$104bn (PUF $13bn), SCF wealth completion unchanged. 50% and 100%
overshoot the top; 100% breaks every other order. Random-order training is
the existing training reweighted and behaves like test 3's model; no
recipe removes the need to choose an order. With ordered questions, most of
the spread in top shares is between training seeds (1.3 points of top 1%
share at 25%), not sampling (0.4 points). The top 0.1% income mix is still
short in every recipe. Nothing left running.

10 October, 23:20 (probe AS): three seeds trained on `full_v2s` (jobs
29087760–62; a fourth, 29087849, as insurance against preemption). Wealth
completion on SCF validation records is unchanged: 35.52, 35.52, 35.50 nats
per record against test 3's 35.50, 35.52, 35.56. Check of the identity on
the PUF training records: AGI equals the income items less adjustments plus
the remainder for every record; decoding the tokens back to dollars puts
24% of records within 1% of their true AGI, the same as decoding AGI
directly in test 3's data (24%). Generation queued (jobs 29087860–70). At the
main session's request, also queued two seeds with probe S's recipe (25% of
each batch on ordered questions, in the size-first order; jobs 29089331,
29089333) and their generation.

10 October, 23:40 (main session): two more probes started at John's request.
AS (prefix `as_`): AGI replaced by a remainder variable, so generated AGI is
the sum of its components plus the remainder. UW (prefix `uw_`): PUF rows
trained on the sample as drawn (no population weights) with the PUF weight
as a model variable, so generated records carry a weight; SCF rows weighted
as in version 2. Reports due about 01:50 (AS) and 02:05 (UW); session ends
about 02:40 on 11 October.

10 October, 23:50 (probe UW): started. Built `full_v2w` in
`/nfs/roberts/scratch/pi_nrs36/jar335/probes/uw/full_v2w` (job 29094078):
test 3's version 2 data plus the PUF sampling weight (S006/100) as a model
variable on PUF rows. The 166,156 PUF training records carry only 73
distinct weights, so each distinct weight is its own token and decoding is
exact (total AGI, top shares and every component by AGI group unchanged).
The top 0.1% of AGI is 20,339 PUF training records, 12.2% of the sample.
In version 2 the SCF rows carried 61.6% of the total record weight in the
loss; that share is kept. Training queued: arm A (test 3's recipe, PUF
unweighted) seeds 1–2, jobs 29094106, 29094108; arm B (the same plus 25%
of each batch on ordered questions with the weight first, then AGI), seeds
1–2, jobs 29094107, 29094109.

11 October, 00:00 (probe AS): done; `AS_REPORT.md` written. Replacing AGI
by a remainder, so that generated AGI is items less adjustments plus the
remainder, fixes the bottom and mostly fixes the top. With a fresh random
order per record (four model seeds), total AGI is $7.99tn, the top 1% share
19.4% and the top 0.1% share 8.6% (PUF $8.09tn, 19.4%, 8.8%). Bottom-half
capital gains are about $0 (PUF $13bn; test 3's model $32bn in the same
order, $124bn with AGI first). In the top 0.1% the six main components sum
to $642bn (PUF $702bn; test 3 $341bn), and AGI exceeds them by $61bn (PUF
$28bn; test 3 $313bn). Still short in the top 0.1%: wages ($180bn against
$211bn) and partnership income ($142bn against $175bn). A fixed order with
the largest amounts first overshoots just below the top. Wealth completion
is unchanged (35.50–35.52). Probe S's 25% ordered-question recipe, tried
on two seeds in the size-first order, overshoots the top 0.1% (share 9.9%).
No probe AS jobs are running.

11 October, 00:10 (probe UW): arms A and B trained (seeds 1–3; SCF wealth
completion 35.49–35.58 nats per record, test 3 35.50–35.56). First
results (arm A seed 2, arm B seed 1): given each record's true AGI and
weight, the six components in the top 0.1% now add to $770bn against
$702bn (before: $400–537bn). Whole-record generation, however, overshoots
the top in arm A (top 1% share 25%) and draws too much negative income
at the bottom in both arms (bottom-half AGI $143bn and -$349bn against
$830bn): errors on records drawn with large weights are multiplied by
those weights. Added arms C and D (the same as A and B but PUF loss
weighted by the square root of the sampling weight; generated records
weighted by the square root of their drawn weight), two seeds each, jobs
29097237–52.

11 October, 00:20 (main session): four overnight probes started (V, C, ST,
BL; SPEC.md contract change 00:20). The session ends about 02:40; probes
submit self-finishing Slurm chains and write reports by 02:20 with a
"To finish" section. Probe UW is still running (report due 02:05).

11 October, 00:25 (probe ST): started. Question: does post-training stretch
the model to a year it was not trained on? Test with the SCF, where 2016's
truth is known. Built `data_no2016` in
`/nfs/roberts/scratch/pi_nrs36/jar335/probes/st/` (version 2 data with the
SCF 2016 rows removed from the training and validation splits; job
29097829) and queued training on it with test 3's recipe, seeds 1–3 (jobs
29097830–32).

11 October, 00:16 (probe BL): started. A standard baseline for whole-record
PUF generation: a sequential tree synthesizer (the method of the R package
synthpop) on the 40 PUF variables of probe AS's dataset, AGI computed as
items less adjustments plus the remainder. Weighted first draw, trees fitted
with population weights, real values drawn from each leaf in proportion to
weight, minimum leaf 5 records. Three arms, two sampling seeds each, 664,624
records per file: AS's size-first order (job 29097869), categoricals first
(29097870), and the PUF unweighted with the weight drawn first (29097871).
Files will be `/nfs/roberts/scratch/pi_nrs36/jar335/probes/bl/gen/bl_<arm>_s<seed>.parquet`.

11 October, 00:30 (probe C): started; dataset built and training running.
`full_v3c` (`/nfs/roberts/scratch/pi_nrs36/jar335/probes/c3/data/full_v3c`,
`c3_prepare.py`, job 29098235) is version 2 with all of test 4's data fixes:
probe BR's bridge variables (mortgage interest and real estate tax observed
on SCF rows), all six itemized deductions unknown rather than zero for PUF
non-itemizers, probe X's translated SCF shared variables, and D25 applied to
every total: AGI, total Schedule E, ordinary dividends and total pensions
are replaced in their columns by a remainder, a Schedule E remainder,
non-qualified dividends and non-taxable pensions; each total is computed
from its parts after generation. The identities reproduce every real total
on every PUF record. On SCF rows, X's translated total dividends and total
Schedule E are split into the PUF's parts using a PUF donor record with the
same rank band of the total. Training: three seeds with the version 2
recipe (jobs 29098346–48) and two seeds with probe UW's weighting (PUF
sample as drawn, weight a model variable; data `full_v3cw`, jobs
29098721–22). Chains of generation, wealth completion, probe V's scorecard
and a collector (job 29098802, writes `$Q/C_TABLES.md`) are queued;
job list in `$Q/jobs.txt`.

11 October, 00:25 (probe BL): generated files ready for probe V's scorecard,
664,624 records each, in probe AS's format (gen_id, source, year, weight,
draw, the 40 variables in dollars, tok_<name>), under
`/nfs/roberts/scratch/pi_nrs36/jar335/probes/bl/gen/`:
`bl_sizefirst_s{1,2}.parquet` (main arm: weighted, AS's size-first order,
minimum leaf 5; job 29098157), `bl_catfirst_s{1,2}` (categoricals first;
29098158), `bl_unweighted_s{1,2}` (PUF unweighted, weight drawn first and
carried by each record; 29098159), `bl_sizefirst_leaf20_s{1,2}` (minimum
leaf 20; 29098182). AGI is not a column: compute it as items less
adjustments plus AGI_REMAINDER (`as_agi.agi_from_parts`). The three
demographics from the PUF demographic file are missing (NaN, token -1) on
about 42% of records, as in the PUF training records. First tables (job
29098177, `bl/bl_tables.md`): the weighted synthesizer overshoots the top
(top 1% share 22.2%, PUF 19.4%); the unweighted one is close (20.1%; top
0.1% 9.1% against 8.8%; bottom-half capital gains $13bn, PUF $13bn).

11 October, 00:25 (probe V): started at 00:13. Scorecard script
`v_score.py` (any generated file; six sections: every variable, joint
relationships, rules real records never break, simplified 2015 income tax,
a classifier telling generated from real, copying), collector
`v_collect.py`, Tax-Calculator 6.8.4 installed in
`/nfs/roberts/scratch/pi_nrs36/jar335/probes/v/tc/venv` and run by
`v_taxcalc.py`. Scored: probe AS random order (model seeds 1–4), test 3's
model from probe O (AGI first and random, model seeds 1–3), the PUF
validation records as a reference, and the training records decoded from
their own tokens (what the tokenizer alone loses). First results (jobs
29098264–366): the classifier's area under the ROC curve is 0.81 for AS
against 0.97–0.98 for test 3 (0.50 for real held-out records); the
simplified tax gives $1,126bn of net income tax for AS against $1,095bn on
the real records.

11 October, 00:25 (probe ST): self-finishing chain submitted (`st_submit.sh`;
job list in `/nfs/roberts/scratch/pi_nrs36/jar335/probes/st/chain_jobs.txt`):
tilt fits to the 2016 tables for the model trained without 2016 (3 seeds)
and test 3's version 2 model (3 seeds), harder-stretch fits, entropy
calibration of the 2013 records to the 2016 tables, three evaluation jobs
and a collector (job 29098330, writes `st/ST_TABLES.md`). Seed 1 trained
(stopped at epoch 29, kept epoch 14). First number: reweighting the 2013
records to hit the 2016 tables takes 1.3 nats per record and leaves an
effective sample of 12% of the 2013 records' own (ridge 10⁻⁴).

11 October, 00:40 (probe C): a problem with "unknown" itemized deductions
for whole-record generation, and a second arm. A test generation from an
early checkpoint of the combined model (30,000 records) gave 88% itemizers
and $12.5tn of AGI when the deductions could be drawn before the form of
deduction (they are learned from itemizers only, so a drawn deduction is
always positive and pulls the record toward itemizing and higher income),
and 5.5% itemizers (PUF 30%) when they were drawn after it (with all six
deductions hidden, the model reads the hiding itself as a sign of a
non-itemizer, since non-itemizers always have them hidden in training).
So I added `full_v3cz`, identical except that the six deductions are zero
for PUF non-itemizers as in version 2 (BR's bridge values stay on SCF rows),
trained three seeds (jobs 29099935–37), and moved the weight-as-variable arm
onto it (`full_v3czw`, jobs 29099938–39). The unknown-coded model is still
generated and scored for comparison. Its SCF wealth completion score on
validation records is 34.13–34.17 nats per record (version 2 35.50–35.56,
BR 34.21–34.31, X 35.27–35.32). Job chains and collector (29099977) are
listed in `/nfs/roberts/scratch/pi_nrs36/jar335/probes/c3/jobs.txt`.

11 October, 00:45 (probe V): done; `V_REPORT.md` written. The scorecard
was run on AS's random order (4 model seeds; all 30 AS files in
`V_TABLES_AS_ALL_DRAWS.md`), test 3's model (AGI first and random), all
four probe BL arms, the PUF validation records and the training records
decoded from their own tokens. Tax liability: AS's records owe $1,126bn of
net income tax under a simplified 2015 calculator against $1,095bn on the
real records ($1,190bn against $1,158bn in Tax-Calculator 6.8.4). The
largest gap in any group's average rate is 2.2–2.4 points, in the top 0.1%.
Test 3's top 0.1% pays $122bn against $217bn in Tax-Calculator. Probe BL's
unweighted tree synthesizer is closer on almost every check (average rates
within 0.1 points; classifier area under the curve 0.53 against AS's 0.81
and test 3's 0.98). Decoding the real records' own tokens already gives
0.72 and breaks a real-record rule on 10.7% of returns. Largest problems
for AS, by tax effect: seed-to-seed spread in top 0.1% tax ($181–236bn over
12 draws, real $204bn); itemized deductions in the top 0.1% at $42bn against
$94bn (about $19bn of tax overall); bottom-half EITC $46.7bn against
$53.4bn. No copying. Nothing left running; tables in
`/nfs/roberts/scratch/pi_nrs36/jar335/probes/v/V_TABLES.md`.

11 October, 00:50 (probe BL): done; `BL_REPORT.md` written; no jobs
running. A sequential tree synthesizer (synthpop's method) on the same 40
PUF variables, scored with AS's tables. Fitted with population weights it
overshoots the top (top 1% share 22.2%, top 0.1% 10.5%; PUF 19.4%, 8.8%),
worse than AS's transformer. Fitted unweighted with the weight drawn first
(UW's idea), it matches or beats AS's best transformer arm on most rows:
total AGI $8.16tn, top shares 20.1% and 9.1%, top 0.1% wages $212bn
(PUF $211bn), partnership $174bn (PUF $175bn), bottom-half capital gains
$13bn (PUF $13bn); it overshoots the 99th–99.9th percentile ($918bn
against $873bn). A plain copy of the training records scores best of all,
so these tables cannot tell learning from copying. No tree record copies a
top 0.1% training record whole; with minimum leaf 5, 0.5–3% of top 1%
tree records carry five or more of one real taxpayer's exact amounts
(real unseen records: 0%); minimum leaf 20 removes that. Same conclusions
against the PUF validation records. Files for probe V:
`/nfs/roberts/scratch/pi_nrs36/jar335/probes/bl/gen/bl_*_s{1,2}.parquet`
(tables in `bl/bl_tables.md`, `bl/bl_tables_val.md`).

11 October, 00:50 (probe ST): first results (harder stretch, 2016 records
with the top 1% of net worth in each income group weighted ×2; training
records). The model trained without 2016, tilted to those tables (0.76
nats per record), misses the untargeted statistics about as much as test
3's model that saw 2016, tilted the same way (0.49 nats): holding rates by
income × age 1.3 against 1.2 points, mean net worth by income × age 8%
against 6%, top 1% share within income groups 1.6 against 1.6 points
(the truth's own sampling error 1.7, 9%, 1.5). The 2013 records reweighted
to the same tables need 2.0 nats, keep an effective 5% of the records, and
break the top (top 1% share within the 99–99.9 group 2% against 13.5%).
Main comparison (plain 2013→2016) still running; collector job 29098330.

11 October, 01:10 (probe C): first results. Generation from the model with
deductions unknown for non-itemizers fails as the 00:40 test showed (three
model seeds: plain random order $12.6–13.1tn of AGI and 88% itemizers;
deductions after the form of deduction $7.7–8.0tn and 6% itemizers; PUF
$8.09tn, 30%). One file from the zero-coded combined model (seed 3): $7.80tn,
29% itemizers, top 1% share 18.9%, top 0.1% share 8.4%, bottom-half capital
gains $1bn (PUF $13bn), but bottom-half partnership income −$78bn (PUF −$31bn).
SCF wealth completion on validation records: zero-coded 34.11–34.20,
unknown-coded 34.13–34.17 (version 2 35.50–35.56). Completed PUF wealth from
the unknown-coded model is too low at the top (top 0.1% mean net worth $38m
against the SCF's $55m and X's translated model's $53m). Added a diagnostic
arm, `full_v3czs`, with the SCF's Schedule E left unobserved instead of split
by a PUF donor (jobs 29103634–39), to test whether the split causes it. The
final collector pass is job 29103417.

11 October, 01:08 (probe UW): finished; report `UW_REPORT.md`, tables in
`/nfs/roberts/scratch/pi_nrs36/jar335/probes/uw/tables_final.md` (job
29102652). Training on the PUF sample as drawn, with the weight as a model
variable, fixes the top 0.1% income mix given AGI: the six components
there add to $769–776bn against $702bn (earlier recipes: $400–537bn).
Whole-record generation needs one more piece. Once the weight is drawn,
each later draw must be restricted to values the PUF sample shows for
records of similar weight; without that, rare tail errors on records with
weights near 1,400 dominate the totals (bottom-half AGI -$281bn in arm
B). Arm B (25% ordered questions, weight first) with the restriction, three
seeds: total AGI $8.02tn, top 1% share 19.3%, top 0.1% share 8.8%, six
components in the top 0.1% $679bn, bottom-half capital gains $16bn,
partnership -$28bn, dividends $14bn (PUF $8.09tn, 19.4%, 8.8%, $702bn,
$13bn, -$31bn, $14bn). Square-root weighting (arms C, D) is no better.
SCF wealth completion is unchanged (35.46–35.58). Nothing left running.

11 October, 01:30 (probe ST): done; `ST_REPORT.md` written, nothing
pending. The model trained without SCF 2016, tilted to 2016's tables (0.76
nats per record), matches test 3's model that saw 2016, tilted the same
way (0.49 nats), on every untargeted statistic to within about the truth's
sampling error, on training and unseen 2016 households; e.g. mean net
worth by income × age off 8% against 6% (sampling error 7%), top 1% share
within income groups 1.4 against 1.2 points (1.3). Reweighting the 2013
records to the same tables needs 1.3 nats, keeps 12% effective sample and
breaks the top (top 0.1% mean net worth doubled). Caveats: 2013→2016 is a
small change in rank terms, and the 2016 quantile functions were taken as
known. A harder stretch (top 1% of each income group weighted ×4) breaks
the top 1% share in both models alike (3.1–3.3 points against 1.6),
because the change is inside the top rank band the targets do not
resolve.

11 October, 01:55 (probe C): done; `C_REPORT.md` written, every job
finished, no probe C job running. Itemized deductions: "unknown" for
non-itemizers breaks whole-record generation (88% or 6% itemizers,
depending on order) and "zero" breaks completed housing wealth (28% of PUF
returns own a home, SCF 67%); the report suggests separate claimed and
paid variables. With zero coding the combined model generates about as
well as AS's (total AGI $7.85tn, top 1% share 19.0%, top 0.1% 8.5%; six
files) and, on probe V's scorecard, cuts impossible records from 12.9%
to 1.0%; bottom-half partnership losses are too deep (−$53bn, PUF −$31bn).
SCF wealth completion is the best so far (34.11–34.20 on validation
records; version 2 35.50–35.56), mostly from the bridge variables.
Completed PUF wealth (unknown coding) is too low above the 99th
percentile (top 0.1% mean $38m, SCF $55m, X $53m); hiding the SCF's
Schedule E split does not raise it (two seeds), cause not found. The
weight as a model variable overshoots, as in probe UW.

## 11 October, 01:55 (main session): overnight probes all reported

- **UW.** Training PUF records unweighted with the weight as a variable
  fixes the top 0.1% mix given true AGI ($769–776bn of the six main items
  against $702bn), but whole-record generation breaks (high-weight records'
  rare errors scaled up) unless later draws are limited to values seen at
  similar weights; with that limit: total AGI $8.02tn, top 1% 19.3%, top
  0.1% items $679bn (one draw per model; on AGI-as-a-variable data).
- **V.** AS's generated records pass the tax test (Tax-Calculator: $1,190bn
  against $1,158bn; group average rates within about 2.3 points); classifier
  0.81 (0.72 from rank-band decoding alone); no copying.
- **BL.** Sequential trees trained unweighted with the weight drawn first
  match or beat the transformer on the PUF alone (group tax rates within 0.1
  points, classifier 0.53), but carry real dollar amounts (fixed with a
  minimum leaf of 20).
- **ST.** A model trained without SCF 2016, tilted to 2016 tables, matches
  the model that saw 2016 on untargeted statistics within sampling error;
  reweighting 2013 records is worse. Changes inside the top band, which no
  target sees, are missed.
- **C.** All fixes combined: SCF wealth completion best yet (34.11–34.20);
  coding non-itemizers' deductions as unknown breaks generation, coding
  them as zero breaks completed wealth — separate variables for deduction
  claimed and amount paid (as D20 already proposed for imputation). UW's
  weighting did not combine (top 1% 33.9%, without UW's generation limit).
  Completed PUF wealth in the top 0.1% too low ($38m against $55m), cause
  unknown.
