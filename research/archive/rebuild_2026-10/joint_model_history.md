---
title: "Tax-Data rebuild — history of the joint model experiments, 8–14 October 2026"
role: notes
workstream: rebuild
status: current
updated: 2026-10-14
sot: research/rebuild/design/plan.md
supersedes: []
superseded_by: null
---

# History of the joint model experiments, 8–14 October 2026

**Superseded as a guide on 14 October 2026.** The design was gone through
from the top that day and rewritten as `research/rebuild/design/plan.md` and
`research/rebuild/design/idea_space.md`; read those first. This file stays as
the record of what each experiment ran and found. The plan and decisions it
cites now live in `research/archive/` (see `research/rebuild/design/README.md`).

**To pick the work back up:** read "The goal", then "What we learned",
"Open issues" and "Where I think the work should resume" at the end; the
timeline in between is the detail. The newest proposal is D26 (how survey
weights enter training) in `design_decisions.md`. The plan's own "Next
steps" list dates from 9 October; where they differ, this file is newer.

What we tried, what we found, what changed as a result, and where I (Claude,
the main session) think the work should resume. The plan itself is
`joint_model_plan.md`; the decisions are in `design_decisions.md` (D22–D25).
Every number here comes from a report listed in the last section; where I
give an opinion rather than a result, I say so.

## The goal

John wants one probability model of the population: a model trained on the
PUF and other surveys stacked together that can generate whole tax-unit
records from scratch, each carrying every PUF tax variable plus wealth from
the SCF (later consumption and other survey variables). For any year,
including future years, the model is then post-trained, meaning shifted by
the smallest amount that makes its records hit that year's targets. How far
it has to move is the measure of whether the pretrained model was good
enough.

The model is a transformer that learns to fill in any hidden variables of a
record from the rest. Dollar amounts are stored as rank bands within each
source and year (66 bands for positive amounts, finer at the top, plus
bands for negatives and a token for exact zero).

## Timeline

### 8 October — test 1: the PUF alone

A transformer trained on 39 PUF variables. Its predictions were close to
right when it could see most of a record, and weakest when generating whole
records from nothing. A larger model did not help.

### 9 October — test 2: PUF plus SCF

The model trained on the PUF and SCF together, with ten shared variables
and a label saying which source each row came from. On held-out SCF records
it matched the top 1 percent's share of wealth (37.5 against 37.3 percent)
and never copied a household. But wealth rose too little with income (the
top 1 percent by income held 25 percent of net worth against 31 percent),
and a survey-weighted version of the production forest did better on
wealth by income group.

### 9–10 October — test 3: post-training

John's three steps, stated after I had twice drifted from them: train a
model that completes records with all the PUF variables (40, with total
Schedule E added) and the SCF's 24 wealth variables; measure how close its
records come to the truth; then post-train it to hit the targets exactly.
Targets were the training data's own tables. Three post-training methods:

| Method | What it does | Move from the model (nats per unit of weight) | Fit |
|---|---|---|---|
| Tilt the probabilities | adds one number per target cell to the model's log-probabilities; network unchanged | 0.67 | mean error 0.08%, worst 2.0% |
| Reweight draws | 32 completions per record, reweighted to hit targets | 1.21 | worst error 38%; 9.6 of 32 draws carried real weight; not converged |
| Fine-tune the network | retrain toward the targets with a penalty for moving | 0.17 | stopped part-way; did not reach the targets |

(Completing wealth for real PUF records, version 1 model, core targets.)
A nat is the unit of the entropy distance between the model and its
post-trained version; smaller means less change.

Findings that shaped what came next:

- **The targets contradicted each other.** SCF wealth shares carried onto
  PUF groups implied different totals for the same wealth bin depending on
  the grouping: up to 3.5 percent of all weight on the core targets and 6.9
  percent on the full set. No method can fit contradictory targets. This
  is what led to translation (below).
- **Generating whole records from scratch was weak.** No method hit the
  targets exactly; over 1,000 target cells per run had no generated record
  at all, mostly at the top.
- **Generated PUF records were far too thin at the top.** The top 1
  percent's share of AGI came out at 4.8 percent against 19.4. One cause
  found that night: the PUF withholds age for every return in the top 1
  percent of AGI, and generation drew age before AGI, so the model had
  learned that a return with an age is never a top return. Drawing
  demographics last raised the share to 8.8 percent.

### 9 October (discussion) — translation, D23 and D24

Out of test 3's contradictions came the plan's central idea. A survey's
"dividends" or "marital status" does not mean what the PUF's does; SCF
dividends miss small holders a tax return records (19 percent of PUF
returns report dividends against 12 percent of SCF units). Rather than
reweighting the survey (which would say the survey has the wrong number of
each kind of household, and would change its overall wealth distribution),
each survey household's shared variables are moved to their tax-return
equivalents, one at a time, by rank within cells, keeping its weight and
its wealth. Each variable has one authority: the IRS for tax items, the
Census for demographics, the SCF for wealth. Written up as D23, D24 and
`joint_model_plan.md`.

### Night of 9–10 October — six probes

| Probe | Question | Result |
|---|---|---|
| Y | Does translation recover the truth where it is known (test 2's SCF split in halves, one half's incomes deliberately distorted)? | Yes, and reweighting was worst in every case. One rule changed: where the survey has too few holders of an income, switch on the most likely households (error 6.3% → 2.2–3.8%; random choice 8.0%); where it has too many, switch off at random within cells (0.8%), not the least likely ones (4.7%, worse than no adjustment at 4.4%). Business income reported as wages needs a definitional rule; random reporting noise cannot be undone. |
| X | Translate the real SCF: do the contradictions shrink, and does completed PUF wealth move toward the SCF? | Largest contradiction on the full set 6.85% → 3.72%; core unchanged at 3.52% (a coverage difference: the SCF leaves out adults split off into their own tax units, the PUF includes dependent filers). Completed PUF mean net worth within about 10% of the SCF in every income group (it had been 35–100% too high in the bottom 90%). Tilt 0.69 → 0.57 nats. |
| BR | Can the SCF supply mortgage interest and real estate tax, and do they help? | Yes. Held-out SCF wealth completion improved by about 1.2 nats per record. Found a bug: all six itemized deductions were coded zero for the 70% of returns that do not itemize, instead of unknown. |
| Q | How good must pretraining be? | The tilt fixes any statistic built from the targeted groupings for nearly any model, as long as the move is under about 1.3 nats. Relationships no target covers keep the model's errors (e.g. correlation of net worth with dividends 0.27 against 0.44 with incomes hidden). Moves above 1.8 nats make some things worse. The size of the move is only a partial warning. |
| T | Why is the top thin in generated records? | The model knows the top (asked for AGI with nothing else known, it puts 0.90–0.97% of weight in the top 1% against 0.99%) but loses it when asked for AGI given only six categorical variables (0.60–0.63%). Drawing AGI first, with no retraining: total AGI $7.74–7.98tn against $8.09tn, top 1% share 17.3–18.3%. Longer training and a bigger model did not help and hurt the SCF side. Unsolved: given each record's true AGI, the income items of the top 0.1% summed to $532bn against $704bn. |
| AU | The authority table (plan step 0) | Drafted, with six open questions for John (below). |

A correction from probe X: the SCF median net worth of $91k I had been
comparing against leaves out the split-off units; counting them at zero it
is $57k, so completed PUF wealth was less wrong than I had said.

### 10 October — the top of the distribution

John's reaction to the thin top was that a model that cannot learn the
PUF's top "seems fatal". He asked why the draw order should matter at all,
since the joint distribution is the same in any order. The answer: it is
the same for the true probabilities, but the model's estimates of the
different conditional distributions are not consistent with each other,
so the order decides which estimates you rely on.

| Probe | Question | Result |
|---|---|---|
| O | How much does the draw order matter, model unchanged? | A great deal: across ten orders the top 1% share ranged from 4.7% to 24.5%, total AGI from $6.36tn to $9.91tn. AGI first: top right, false capital gains in the bottom half ($77–124bn against $13bn). Income items first, AGI after: bottom clean, top overshoots, and AGI in the top 0.1% exceeded the sum of its items by $430–590bn (PUF: $28bn). A fresh random order per record was the best compromise. The model's inconsistency across orders measured 0.44 nats per record. No order fixed the top 0.1% mix. |
| S | Does training on the exact questions generation asks fix it? | Partly. A quarter of each training batch on those questions, AGI first: top 1% share 19.6%, top 0.1% 8.9%, bottom-half capital gains $70bn. More than a quarter overshoots; random-order training did nothing. Much of the run-to-run variation is chance: four draws from one model gave a top 1% share of 17.5–19.1%. |
| AS | Make AGI the sum of its parts (John's request) | The breakthrough. AGI replaced by a remainder variable (AGI less 10 income items plus 3 adjustments), and computed as the sum after generation. Same model, data and training: top 0.1% income items $642bn against $702bn (was $341bn); top 1% share 19.4% against 19.4%; bottom-half capital gains about $0 against $13bn; SCF side unchanged. |
| UW | Train on the PUF sample as drawn, with the weight as a model variable | Fixed the top 0.1% income mix given true AGI ($769–776bn against $702bn; every earlier recipe $400–537bn). But whole-record generation broke, because small errors on ordinary records are multiplied by weights of 1,000 or more. Fixed by a rule at generation: once the weight is drawn, later draws stay within values the PUF shows at similar weights. Then: total AGI $8.02tn, top 1% 19.3%, top 0.1% 8.8%, top 0.1% items $679bn (one draw per model; run on data with AGI still a variable). |

John made AS's result a requirement: **derived variables are never
predicted** (D25). Totals are computed from their parts; where the parts do
not cover the total, the model gets one remainder variable.

**Why population weighting starved the top.** The IRS samples high-income
returns almost with certainty (weights near 1) and ordinary returns thinly
(weights of 1,000 or more). About 20,000 of the 166,156 PUF sample records
are in the top 0.1% of AGI. Training counted each record in proportion to
its population weight, so those 20,000 records together counted for 0.1%
of training: the model learned the income mix of the 99.9% (mostly wages)
and barely learned the top's (partnership income, capital gains). Training
on the sample as drawn lets them count fully; the weight, learned as a
variable, restores the population.

### Night of 10–11 October — is the approach viable?

| Probe | Question | Result |
|---|---|---|
| V | A broad scorecard of AS's generated records against the real PUF | **The tax test passes.** Tax-Calculator 6.8.4: $1,190bn of income tax against $1,158bn; every AGI group's average tax rate within about 2.3 points; under the simplified calculator, top 0.1% tax $203bn on average across 12 draws (range $181–236bn) against $204bn. A classifier separates generated from real records with a score of 0.81, where 0.5 means it cannot tell; storing amounts as rank bands alone accounts for 0.72. No copying of real records. Largest remaining misses: top 0.1% itemized deductions $42bn against $94bn; bottom-half EITC $46.7bn against $53.4bn; top returns too rarely joint filers (74% against 83%). |
| BL | A standard baseline: sequential decision trees (as in the R package synthpop) on the same variables | **The baseline ties or beats the transformer on the PUF alone**, when trained unweighted with the weight drawn first: every group's average tax rate within 0.1 points, classifier 0.53, top 0.1% wages $212bn against $211bn. Trained with population weights, the trees also missed the top. The trees copy real dollar amounts from real records (0.5–3% of top 1% records carry five or more of one real taxpayer's amounts; a minimum leaf of 20 removes this). |
| ST | Can the model be stretched to a year it has not seen, by targets? | Yes, for a mild change. Trained without SCF 2016 and tilted to 2016 tables, it matched the model that saw 2016 on untargeted statistics within sampling error (e.g. mean net worth by income × age off 8% against 6%; sampling error 7%). It needed a larger tilt (0.76 against 0.49 nats). Reweighting the 2013 records instead needed 1.3 nats, kept 12% of their effective sample, and doubled top 0.1% wealth. A change inside the top band, which no target sees, was missed by both models. |
| C | All fixes in one dataset: D25 everywhere, translation, bridge variables, non-itemizers' deductions unknown | SCF wealth completion the best of any model (34.11–34.20 against 35.50–35.56 for version 2). But the itemized deductions cannot be coded one way for both jobs: unknown breaks generation (88% or 6% itemizers against 30%), zero breaks completed wealth (28% of returns with a home against 67%). Completed PUF wealth in the top 0.1% is too low ($38m against $55m), cause unknown. The weight as a variable, without UW's generation rule, overshot (top 1% share 33.9%). |

### 14 October — round 4: trees against the transformer, deductions, top wealth

| Probe | Question | Result |
|---|---|---|
| TR | Sequential trees on the multi-survey job, on probe C's combined data (zero-coded deductions) | Tax side as in BL: total AGI $8.19tn against $8.09tn, top 1% share 20.3% against 19.4%. Wealth drawn from trees fitted on SCF rows. PUF top 0.1% mean net worth $27m with SCF-weighted trees, $37m fitted on the SCF sample as drawn, $40m with the weight added as a predictor (SCF $55m). Zero-coded mortgage interest and real estate tax break home ownership for the trees too (28% against 67%). Copying negligible. |
| EV | One scorer for both methods, from their draws alone | **Generating tax records: trees clearly better** (largest gap in any AGI group's average tax rate 0.19 points against 2.69; held-out real records 0.15; classifier 0.56 against 0.81). **Wealth on held-out SCF records: close** (12 of 16 checks within two sampling errors; the transformer better per household and in the top 0.1% of income, $32m against $20m, truth $45m ± $9m; the trees better on some group tables). PUF top 0.1% wealth too low for both ($38.6m transformer, $25.5m trees, SCF $54.7m). Caveat: the transformer here was trained with population weights, without probe UW's change; the trees were trained on the sample as drawn. |
| DD | Each itemized deduction as two variables, the deduction claimed and the amount paid | One model now does both jobs, in the version where the amount paid is known only on SCF rows: generated returns itemize 29.4% (PUF 29.8%), total AGI $8.02tn; completed home ownership 55% (SCF 67%; zero coding gave 28%). Completion must hide the form of deduction and the claimed amounts (showing them: 39–41%). The version with the amount paid also known for PUF itemizers fails at generation (22.6% itemize). Not done: drawing the amount paid for non-itemizers (D20). |
| WT | Why completed top wealth is too low | The same cause as the PUF top: the SCF oversamples the wealthy (its top 0.1% by income is 4% of its records and 0.1% of its weight), so population-weighted training barely counts them; even on held-out SCF records with incomes known, the model gives $31m against $48m true. Training the SCF rows on the sample as drawn with the SCF weight as a variable, draws resampled by the drawn weight: PUF top 0.1% median $32m (SCF $33m), but bottom-half mean $253k against $113k. Limiting draws to values seen at similar weights brings every group's mean close (top 0.1% $54m, bottom half $129k) but the top median falls to $22m. |

**Thinking about weights (discussion with John, 14 October).** John
pointed out that a weight's size means different things in different
surveys, because it reflects each survey's design, so using the weight as
a model variable (probes UW, WT) cannot be the general answer. The
principled view, standard for regressions (Solon, Haider and Wooldridge,
"What Are We Weighting For?", 2015): if a survey chooses whom to interview
by X, then among people with the same X its sample is fair, so "Y given X"
can be learned without weights; weights are needed only for how many people
are in each X group. Our case differs in three ways: we want whole records,
so the group sizes are needed too (from the authority, e.g. IRS counts by
income class); the SCF chooses its wealthy households with a wealth
estimate not in our data, so within an income group it still over-picks
the wealthy and needs its weights there; and the transformer learns every
direction, so generation must start from the groups. The resulting rule,
the same for every survey:

1. Within each survey and income group, each row counts in proportion to
   its survey weight, rescaled so the group's rows add up to the number of
   rows in the group.
2. Across groups, attention follows the number of interviews, so the
   oversampled top is learned.
3. The survey weight itself is never a model variable.
4. Generation first sets how many records fall in each income group from
   the authority's counts, then draws each record's tax variables and then
   its wealth.

Groups are defined by variables that mean the same thing in every survey
after translation, such as income class. Not yet tested; written up as D26
(proposed) in `design_decisions.md`.

**The source label (discussion with John, 14 October).** Every training
row carries a label saying which survey it came from. When making a
record, the label is set, not predicted: tax variables are drawn with the
label PUF, so they follow the PUF's relationships; wealth is drawn with the
label SCF, because the model has only ever seen wealth on SCF rows, and
asking for wealth on a PUF-labelled record is a question it was never
trained on. Translation is what makes this fair: an SCF household's
incomes mean the same as a PUF record's. How much the label still changes
the answer after translation measures how good the translation is (not
yet measured). A check of every script on 14 October found the runs
already followed this rule: every completion of PUF wealth presented the
record as SCF (`--as-source SCF`), test 3's whole-record generation drew
tax variables as PUF and wealth as SCF, and the later from-scratch
experiments drew tax variables only.

## What we learned

Most of what we learned is about the data and what the model is trained to
do, not about the transformer:

| Lesson | Specific to the transformer? |
|---|---|
| Translate each survey's shared variables into tax concepts before training; reweight only for coverage | No |
| Compute totals from their parts; never predict them (D25) | No |
| Population-weighted training starves the oversampled top, on the PUF and the SCF alike; let the top count by its number of records. The weight-as-variable version is survey-specific; the general rule is weights within income groups, group sizes from the authority (untested) | No: it fixed the tree baseline too |
| Bridge variables (mortgage interest, real estate tax) add links between tax records and wealth | No |
| A variable that is zero for some filers because of a filing choice needs two variables: the amount claimed and the amount paid | No |
| Post-train by shifting the model's probabilities, not by reweighting draws; reweighting cannot reach records the model never draws | No: anything that gives probabilities can be tilted |
| Targets must be detailed where the change is, especially at the top; post-training cannot fix what no target sees | No |
| Score on held-out records: tables computed on the training records reward copying (a plain copy of the training records scores best) | No |
| Draw order, share of training on generation questions, rank bands | Yes |

The last row was mostly chasing symptoms before the structural fixes
(computing AGI; training on the sample) were found.

## Mistakes and corrections along the way

- I first built test 3 around SCF records; John's point was to complete PUF
  records. Reoriented, and recorded as a standing rule.
- I proposed reweighting surveys to the PUF's population; John objected
  that the SCF's weights should be trusted, which led to D24.
- I compared completed PUF wealth against an SCF median ($91k) that left
  out split-off units ($57k with them counted at zero).
- Probe T read variation between training runs as instability; probe S
  showed part of it was chance in drawing about 660 top records.
- I summarized test 3 as "tilting or fine-tuning beat reweighting"; only
  tilting did, since fine-tuning never finished.
- Twice the four-hour session ended with subagents still working; their
  jobs finished but reports had to be written in the next session. Later
  probes submitted self-finishing job chains.

## Open issues

1. Itemized deductions: the claimed/paid split works when the amount paid
   is known only on SCF rows; drawing the amount paid for non-itemizers
   (D20) is not done.
2. Test the general weighting rule (weights within income groups, group
   sizes from IRS counts) on both the PUF and the SCF side, in place of the
   weight as a model variable.
3. Completed PUF wealth in the top 0.1%: the cause is found (the starved
   SCF top); the fixes so far get either the top or the lower groups
   right, not both.
4. The six open questions from the authority table
   (`tests/probes/AU_AUTHORITY_TABLE.md`): dependents (tax-return
   dependents against household children); cohabiting couples; whose age
   and when; dependent filers, which the SCF lacks; two wealth variables
   that may be mislabelled (`db`, `re_fund`); and which pieces to translate
   together (Schedule C, farm, Schedule E).
5. Confirm the plan's "effect on earlier decisions" (replacing the aging
   loop D2, D4, D6, D16).

## Where I think the work should resume

My view, not a result.

The one large fork still open is the choice of model: the transformer or
sequential trees. On the PUF alone the trees tie or win. Everything else we
learned carries over to either, so this fork is where the search should
resume, and it should be settled before more tuning of either.

A fair test on the actual job:

- the same data for both: the combined dataset with translation, bridge
  variables, computed totals, deductions split into claimed and paid, and
  training on the sample with the weight as a variable;
- scored on what the trees have not yet been tested on: PUF and SCF
  together with different variables missing in each; wealth completed for
  PUF records, scored on held-out SCF records and on relationships no
  target covers; stretching to another year (probe ST's 2013-to-2016
  test); and the tax test and copying check, with probe V's scorecard
  script (`tests/probes/v_score.py`), which makes each step of the search
  cheap to score.

If the trees hold up, use them, with every data lesson above. If the
transformer wins on the multi-survey and stretching parts, that is the
reason for its extra machinery. I would not spend more time on draw orders
or training schedules until this is settled.

**Update, 14 October.** Round 4 ran the first version of this test. The
trees were clearly better at generating tax records and about even on
wealth; neither got top wealth right. The comparison was not quite even
(the transformer was trained with population weights, the trees on the
sample as drawn). Next, in order: apply the general weighting rule (D26)
to both methods on the same data; rerun the scorer (`tests/probes/ev_score.py`);
add the stretching test for both. If the trees still match or beat the
transformer, I would build on the trees.

## Where everything is

- Plan and decisions: `research/archive/joint_model_plan_2026-10-14_superseded.md`,
  `research/archive/design_decisions_2026-10-14_superseded.md` (D22–D25).
- Test 1: `research/rebuild/tests/puf_generator/` (`STATUS.md`,
  `EXPERIMENTS.md`).
- Test 2: `research/rebuild/tests/scf_joint/` (`MODEL_REPORT.md`,
  `EVAL_REPORT.md`, `COMPARATORS_REPORT.md`).
- Test 3: `research/rebuild/tests/posttrain/` (`SPEC.md`, `STATUS.md`,
  `B_REPORT.md`, `C_REPORT.md`, `D_REPORT.md`, `EVAL_REPORT.md`,
  `F_REPORT.md`, `G_DATA_REPORT.md`, `H_MODEL_REPORT.md`).
- Test 4 probes: `research/rebuild/tests/probes/` (`SPEC.md`, `STATUS.md`,
  and one `<NAME>_REPORT.md` each for Y, X, BR, Q, T, O, S, AS, UW, V, BL,
  ST, C, TR, EV, DD, WT; `AU_AUTHORITY_TABLE.md`).
- Large outputs (scratch, not in git):
  `/nfs/roberts/scratch/pi_nrs36/jar335/posttrain/`,
  `/nfs/roberts/scratch/pi_nrs36/jar335/scf_joint/`,
  `/nfs/roberts/scratch/pi_nrs36/jar335/probes/`.
