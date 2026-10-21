---
title: "Test 4, probe T — why generated PUF records are thin at the top"
role: report
workstream: rebuild
status: current
updated: 2026-10-10
sot: research/rebuild/tests/probes/SPEC.md
supersedes: []
superseded_by: null
---

# Why generated PUF records are thin at the top

## Summary

- **The model has learned the PUF's top.** Asked for AGI with nothing else
  known, test 3's model (version 2) puts 0.90% to 0.97% of weight in the
  top 1% of AGI against 0.99% in the PUF (seeds 1–3). Asked for AGI with every
  other PUF variable known, it puts 0.97% to 1.01% there.
- **The top is lost at one step of generation:** AGI drawn given only the
  first six variables in the generation order (filing status, dependent
  status, exemptions, children for the child credit, children for the EITC,
  form of deduction). Given only those six, the model puts 0.60% to 0.63% of
  weight in the top 1% (seeds 1–3), and it under-predicts most among
  itemizers: joint itemizers 2.7% to 3.1% against 4.7%. The six values it
  draws first are themselves right. Later draws do not compound the error,
  and decoding tokens into dollars loses nothing to speak of.
- **Drawing AGI first fixes AGI without retraining.** With the order AGI,
  then the six, then the other amounts, then demographics, the unchanged
  model gives total AGI $7.74tn to $7.98tn against $8.09tn, and a top 1%
  share of AGI of 17.3% to 18.3% against 19.4% (three seeds). The order with
  demographics last gave $7.41tn and 9.1%.
- **Of the training changes, none beats the order change alone on AGI.**
  Longer training (stopping on the PUF loss) does not repair the
  six-variable question and badly harms SCF wealth completion. Extra
  training weight on top records, weighted back afterwards, is right in
  principle but distorted incomes at the bottom. Training half the records
  on the exact questions generation asks (in the AGI-first order) removes
  most of the spurious capital and business income in the bottom half and
  leaves SCF wealth completion unchanged, but its top is less stable across
  seeds (top 1% share 18.9%, 20.6%, 22.3% in three seeds). Promising, not
  settled.
- **What remains wrong is the mix of income at the very top.** Even given
  each record's true AGI, every variant draws too little income from
  components in the top 0.1%: in seed 1, wages, capital gains, partnership
  and S corporation income, dividends, interest and Schedule C add to $532bn
  against $704bn in the PUF (AGI there is $731bn). They draw too much in the
  99th to 99.9th percentile ($1,068bn against $829bn). Nothing run tonight
  fixes this.

## What was run

All jobs ran in Slurm on 9–10 October; outputs are under
`/nfs/roberts/scratch/pi_nrs36/jar335/probes/t/`. Every number below comes
from these jobs, on test 3's version 2 data (`full_v2`) and the PUF
training records (166,156 records, 120.7 million returns).

| Step | Script | Jobs |
|---|---|---|
| Step 1: the model's predicted AGI distribution given various visible variables | `t_diag.py`, `t_diag.sbatch` | 28922334, and inside each `t_eval.sbatch` job |
| Tokenizer round trip (true tokens decoded back to dollars) | `t_compare.py` (`roundtrip`) | 28922334 |
| Components drawn given the true AGI and first six variables | `../posttrain/h_complete.py` in complete mode | 28922334, and inside each `t_eval.sbatch` job |
| Retrained variants | `t_train.py` (copy of `h_train.py` with the options below), `t_train.sbatch` | 28922237–39, 28922416, 28923343, 28924996–99, 28927465 |
| Generation from each checkpoint (AGI first, 664,624 records; and demographics last) | `t_eval.sbatch`, `t_gen.sbatch` | 28923339, 28923537–38, 28924409–12, 28924992–94, 28926257–75, 28926546–49, 28927466 |
| Levels tables | `t_compare.py`, `t_table.py`, `t_final.sbatch` | 28929416 (final), 28925594, 28924240, 28923131 |

Every generated file is compared with the PUF training records on the
training records' own thresholds: AGI at the 50th, 90th, 99th and 99.9th
weighted percentiles ($36,200, $133,700, $464,600, $2.131m). Generated
records are grouped by their own AGI against those thresholds. Capital
gains are long-term plus short-term (P23250 + P22250); dividends are
ordinary dividends (E00600).

The training variants, each one seed unless stated (one training run of
`full_v2` is 15–55 minutes on one GPU):

| Name | Change from test 3 | Checkpoint evaluated |
|---|---|---|
| base | none (test 3's three trained models) | as kept by test 3 (epoch 15–16) |
| sched | learning rate decays over 60 epochs (in test 3 it stayed near its peak), run to the end | the epoch with the lowest PUF validation loss (60) |
| pufstop | stop when the PUF part of the validation loss stops improving | lowest PUF loss so far, epoch 111 (run still going at the deadline) |
| big | model about 4 times larger (width 512, 8 layers), stop on the PUF loss | epoch 15 (lowest overall loss), and epoch 106 (lowest PUF loss at the time of copying) |
| tilt | PUF records with AGI in the top 10%, 1% and 0.1% of positive AGI get 3, 10 and 30 times their loss weight; generated records are weighted back by the inverse | epoch 16 (lowest overall loss) |
| mixord60 (2 seeds) | half the records each step get the question generation asks: a random number of variables visible in the generation order (demographics last), the next one scored; 60 epochs, decaying rate | epoch 60 |
| mixagi60 (3 seeds) | the same with AGI first in that order | epoch 60 |

## Where the error enters

**Step 1: the model's AGI distribution given only some variables.** For
each PUF training record, the variables named are set to their true
values and everything else is hidden; the model's predicted distribution
of the AGI token is averaged over records with population weights. A token
is a band of weighted rank among positive AGI values; "top 1% band" is
ranks 0.99 to 1 (0.99% of all returns, since a few have zero or negative
AGI).

| Visible to the model | Weight in top 1% band, seed 1 | seed 2 | seed 3 | PUF |
|---|---|---|---|---|
| nothing | 0.97% | 0.95% | 0.90% | 0.99% |
| the first six variables | 0.63% | 0.63% | 0.60% | 0.99% |
| the first six and age, gender, earnings split | 0.32% | 0.33% | 0.29% | 0.99% |
| every PUF variable except AGI | 1.01% | 0.97% | 0.98% | 0.99% |
| the first six as drawn in generation (demographics last) | 0.63% | 0.64% | 0.60% | 0.99% |

The same holds on validation records (first six: 0.62%, 0.62%, 0.58%), so
this is not memorization. The last row reproduces what generation drew, so
the whole shortfall in generated AGI is this one prediction. The six
variables drawn before AGI match the PUF (itemizers 31.0% generated against
29.8%; joint filers 36.1% against 36.1%).

The shortfall is concentrated among itemizers (seed 1; share of the cell in
the top 1% band):

| Cell (share of returns) | PUF | Model given the first six |
|---|---|---|
| Joint, itemizing (16.6%) | 4.71% | 2.91% |
| Single, itemizing (9.9%) | 1.00% | 0.40% |
| Married filing separately, itemizing (0.7%) | 1.78% | 0.27% |
| Head of household, itemizing (2.5%) | 0.75% | 0.75% |
| Joint, standard deduction (19.1%) | 0.30% | 0.31% |

The model's answers to different questions are inconsistent with each
other: averaged over the true mix of the six variables, its answer given
those six should equal its answer given nothing, and it does not (0.63%
against 0.97%). Training draws a random share of each record's variables to
hide, so a question with exactly six categorical variables visible and no
dollar amounts is rarely posed in training; the model must extrapolate to
it, and does so badly at the top.

**Tokenizer.** Decoding the PUF training records' own tokens back to
dollars (job 28922334) gives total AGI $8.10tn against $8.09tn, 1.00% of
weight above the true 99th percentile, and a top 1% share of 19.4% and top
0.1% share of 8.9% (PUF 8.8%). Of the 66 bands of positive AGI, 12 cover the
top 1% of positive AGI (ranks 0.99 to 1) and 3 the top 0.1% (0.999–0.9999,
0.9999–0.99999, 0.99999–1). Decoding loses a little in the top 0.1% for wages ($186bn
against $211bn) and nothing material for the other components.

**Components given the true AGI.** With each PUF training record's true AGI
and first six variables fixed, and the other variables drawn (seed 1; the
"base" columns of the composition table below): the components in the top
0.1% sum to $532bn against $704bn, and in the 99th to 99.9th percentile to
$1,068bn against $829bn (wages $627bn against $455bn). So for the mix of
income the shortfall is in what the model draws given AGI, and the
tokenizer accounts for at most $25bn of it (wages).

## Results: AGI at the top

Whole PUF records generated from nothing, 664,624 records each (four times
the PUF training count) unless marked "1x" (166,156). Validation loss is the
PUF records' part of test 3's validation loss with population weights
(lower is better; it scores any-order prediction, not generation). Wealth
completion is H's score on 6,112 SCF validation records (nats per record,
lower is better).

| | PUF | base, demographics last | base, AGI first: seed 1 | seed 2 | seed 3 | sched | pufstop | big, epoch 15 | big, epoch 106 | tilt, weighted back |
|---|---|---|---|---|---|---|---|---|---|---|
| Total AGI | $8.09tn | $7.41tn | $7.98tn | $7.95tn | $7.74tn | $8.34tn | $8.16tn | $7.82tn | $7.98tn | $8.11tn |
| Weight above true 99th pct | 1.00% | 0.66% | 0.99% | 0.95% | 0.91% | 1.16% | 1.05% | 0.92% | 1.01% | 1.10% |
| Weight above true 99.9th pct | 0.100% | 0.024% | 0.093% | 0.085% | 0.080% | 0.119% | 0.103% | 0.084% | 0.092% | 0.101% |
| Top 1% share of AGI | 19.4% | 9.1% | 18.3% | 17.3% | 17.6% | 22.2% | 19.9% | 17.2% | 19.6% | 19.4% |
| Top 0.1% share of AGI | 8.8% | 1.9% | 7.7% | 7.0% | 7.3% | 10.3% | 9.0% | 7.1% | 8.7% | 8.0% |
| PUF validation loss | | 0.7266 | 0.7266 | 0.7278 | 0.7263 | 0.7194 | 0.7160 | 0.7281 | 0.7174 | 0.7316 |
| Wealth completion | | 35.50 | 35.50 | 35.52 | 35.56 | 38.68 | 44.71 (epoch 139) | 35.44 | 51.04 (epoch 192) | 35.49 |

Everything except "base, demographics last" draws AGI first. The "tilt"
file's records are weighted back to the population; the weights leave an
effective sample of 83% of the records.

| | PUF | mixord60 seed 1, demographics last (1x) | seed 2 (1x) | mixord60 seed 1, AGI first | seed 2 | mixagi60 seed 1, AGI first | seed 2 | seed 3 |
|---|---|---|---|---|---|---|---|---|
| Total AGI | $8.09tn | $7.66tn | $7.86tn | $9.38tn | $8.71tn | $8.01tn | $8.33tn | $8.36tn |
| Weight above true 99th pct | 1.00% | 0.81% | 0.92% | 1.62% | 1.37% | 0.99% | 1.10% | 1.09% |
| Weight above true 99.9th pct | 0.100% | 0.075% | 0.088% | 0.127% | 0.123% | 0.093% | 0.107% | 0.110% |
| Top 1% share of AGI | 19.4% | 16.0% | 18.8% | 25.7% | 23.4% | 18.9% | 20.6% | 22.3% |
| Top 0.1% share of AGI | 8.8% | 6.9% | 9.0% | 10.9% | 10.5% | 8.4% | 9.4% | 11.2% |
| PUF validation loss | | 0.7494 | 0.7517 | 0.7494 | 0.7517 | 0.7469 | 0.7508 | 0.7470 |
| Wealth completion | | 35.55 | 35.53 | 35.55 | 35.53 | 35.52 | 35.60 | 35.51 |

What each training change did to the step-1 question (weight in the top 1%
band given the first six variables; PUF 0.99%) and to the question with
nothing visible (PUF 0.99%):

| | base (3 seeds) | sched | pufstop | big, epoch 15 / 106 | mixord60 (2 seeds) | mixagi60 (3 seeds) |
|---|---|---|---|---|---|---|
| Given the first six | 0.60–0.63% | 0.65% | 0.61% | 0.47% / 0.61% | 0.80%, 0.89% | 1.01%, 0.87%, 0.72% |
| Given nothing | 0.90–0.97% | 1.13% | 1.05% | 0.89% / 1.02% | 1.58%, 1.36% | 0.95%, 1.11%, 1.07% |

Reading the tables:

- **Early stopping is not the cause.** Training to the PUF-best epoch
  (sched, pufstop, big at epoch 106) lowers the PUF validation loss by about
  1% but leaves the six-variable question as wrong as before (0.61% to
  0.65%). With AGI drawn first these checkpoints do about as well as the
  base or overshoot (sched: $8.34tn, 22.2%). The SCF side pays: wealth
  completion worsens from 35.5 to 38.7 (sched, epoch 60), 44.7 (pufstop,
  epoch 139), 46.2 (tilt run, epoch 171) and 51.0 (big, epoch 192). A PUF-best checkpoint could only be used for PUF
  generation, with a separate checkpoint for wealth completion.
- **A larger model does not help** at the stopping point test 3 used (0.47%
  given the six).
- **Training on the generation questions** fixes the question it trains:
  mixord60 lifts the six-variable answer to 0.80–0.89%, and mixagi60 gets
  the nothing-visible answer roughly right (0.95%, 1.11%, 1.07%). It moves other
  questions: mixord60 now over-predicts the top given nothing (1.36–1.58%),
  so with AGI drawn first it overshoots ($8.71–9.38tn). Each variant is good
  for the order it was trained on. The any-order PUF validation loss is
  worse (0.747–0.752 against 0.727), because half the training goes to the
  other task; wealth completion is unchanged (35.52–35.60).
- **Seed-to-seed spread is large at the very top.** The top 0.1% of 664,624
  generated records is about 660 records, and a few of them carry much of
  the AGI. Base, AGI first, varies from 7.0% to 7.7% across seeds; mixagi60
  from 8.4% to 11.2%. Differences smaller than that are not findings.

## Results: the mix of income by AGI group

Totals in $bn by AGI group, whole records generated with AGI first. PUF
against base (test 3's model, seeds 1 and 2) and mixagi60 (seeds 1 to 3).

| AGI group | PUF | base s1 | base s2 | mixagi60 s1 | mixagi60 s2 | mixagi60 s3 |
|---|---|---|---|---|---|---|
| **AGI** bottom 50 | 830 | 816 | 787 | 810 | 790 | 761 |
| 50–90 | 3,415 | 3,462 | 3,545 | 3,431 | 3,426 | 3,434 |
| 90–99 | 2,245 | 2,204 | 2,206 | 2,221 | 2,360 | 2,250 |
| 99–99.9 | 873 | 864 | 844 | 863 | 958 | 953 |
| top 0.1 | 731 | 630 | 565 | 689 | 799 | 966 |
| **Wages** bottom 50 | 810 | 886 | 876 | 845 | 874 | 812 |
| 50–90 | 2,654 | 2,965 | 2,964 | 2,755 | 2,774 | 2,736 |
| 90–99 | 1,590 | 1,805 | 1,733 | 1,700 | 1,847 | 1,682 |
| 99–99.9 | 455 | 626 | 611 | 535 | 581 | 591 |
| top 0.1 | 211 | 168 | 110 | 117 | 68 | 143 |
| **Capital gains** bottom 50 | 13 | 102 | 143 | 50 | 84 | 53 |
| 50–90 | 30 | 126 | 114 | 50 | 75 | 57 |
| 90–99 | 84 | 136 | 168 | 121 | 147 | 171 |
| 99–99.9 | 104 | 125 | 189 | 121 | 207 | 183 |
| top 0.1 | 245 | 217 | 98 | 180 | 148 | 324 |
| **Partnership and S corporation** bottom 50 | -31 | 51 | 37 | -8 | 11 | 28 |
| 50–90 | 38 | 114 | 134 | 59 | 63 | 43 |
| 90–99 | 129 | 214 | 204 | 196 | 251 | 177 |
| 99–99.9 | 186 | 184 | 171 | 277 | 295 | 262 |
| top 0.1 | 175 | 116 | 86 | 146 | 95 | 170 |
| **Dividends** bottom 50 | 14 | 48 | 44 | 22 | 20 | 22 |
| 50–90 | 41 | 44 | 48 | 43 | 53 | 44 |
| 90–99 | 55 | 56 | 56 | 51 | 68 | 60 |
| 99–99.9 | 39 | 38 | 39 | 42 | 53 | 51 |
| top 0.1 | 45 | 34 | 19 | 25 | 26 | 40 |

The same components with each PUF training record's true AGI and first
six variables fixed (so AGI is right by construction; the rest drawn):

| AGI group | PUF | base s1 | base s2 | base s3 | mixagi60 s1 | mixagi60 s2 | mixagi60 s3 | mixord60 s1 |
|---|---|---|---|---|---|---|---|---|
| **Wages** 99–99.9 | 455 | 627 | 629 | 627 | 535 | 543 | 539 | 563 |
| top 0.1 | 211 | 178 | 170 | 153 | 122 | 78 | 125 | 106 |
| **Capital gains** bottom 50 | 13 | 55 | 120 | 37 | 49 | 77 | 40 | 46 |
| top 0.1 | 245 | 206 | 145 | 130 | 151 | 135 | 260 | 228 |
| **Partnership and S corporation** bottom 50 | -31 | 48 | 24 | 47 | 14 | 25 | 3 | -10 |
| 99–99.9 | 186 | 170 | 170 | 184 | 271 | 269 | 216 | 310 |
| top 0.1 | 175 | 101 | 114 | 127 | 152 | 94 | 164 | 137 |
| **Dividends** top 0.1 | 45 | 32 | 26 | 31 | 22 | 23 | 26 | 26 |
| **Interest** top 0.1 | 18 | 11 | 12 | 9 | 7 | 7 | 13 | 8 |

Full tables for every variant, including Schedule C and interest in every
group: log of job 28929416
(`/nfs/roberts/scratch/pi_nrs36/jar335/probes/logs/t_final_28929416.out`)
and `/nfs/roberts/scratch/pi_nrs36/jar335/probes/t/compare_final.json`.

What they show: with AGI right, the model still puts too much wage income
just below the top 0.1% and too little of every component in the top 0.1%.
It also generates income that is not there in the bottom half: capital
gains $37–143bn against $13bn, partnership income +$24–51bn against a net
loss of $31bn. Training on the generation questions reduces the bottom-half
errors (mixagi60 with AGI first, three seeds: capital gains $50–84bn,
partnership -$8bn to +$28bn, dividends $20–22bn against $14bn) and moves
some wages out of the 99th to 99.9th percentile. It does not reliably fill
the top 0.1% (capital gains there $148–324bn against $245bn across seeds),
and it pushes partnership income into the 99th to 99.9th percentile
($262–295bn against $186bn). Nothing in the model makes the components add up to AGI, and in
the top 0.1% they do not.

## Weighting options: exact or approximate

- **Extra weight as a function of the record's own variables, weighted
  back (run: tilt).** If the loss weight of a PUF record is multiplied by a
  known function c of its variables (here, of its AGI band), a perfectly
  fitted model learns the PUF population reweighted by c. Weighting each
  generated record by 1/c returns the population exactly, and the model's
  answers given AGI are in principle unchanged, since c depends on AGI
  alone. In practice the fitted model is not perfect: given low AGI it drew
  far more capital gains (bottom half $659bn against $13bn, with AGI first)
  and dividends. Exact in principle, worse in practice; not recommended.
- **Square-root weights with the PUF weight as a model variable (not run).**
  Here the extra weight depends on the record's sampling weight, not on its
  variables, so the weight must be in the model for generated records to be
  weighted back. The weight would be drawn as a token band and decoded
  inside it, so the reweighting is approximate. It needs the weight added to
  the data as a variable; not done tonight.
- **Oversampling top records (not run).** In expectation the same as the
  tilt, with more noise; the same exactness argument applies.

## What this means for the pretraining recipe

1. **Generation order: AGI first.** Then filing status and the other
   categorical variables, then the dollar amounts, then demographics last.
   This alone brings the unchanged model to $7.74–7.98tn of AGI and a top 1%
   share of 17.3–18.3% (PUF $8.09tn, 19.4%). More generally, draw first the
   variable whose distribution matters most and whose unconditional answer
   the model knows, rather than weakly informative categories.
2. **Keep the stopping rule and model size as they are.** Training to the
   PUF-best epoch does not repair the error found here, and it harms SCF
   wealth completion; the larger model did not help.
3. **Training on the generation questions is worth a fuller test.** Half
   the records trained on the exact sequence of questions generation asks,
   in the order generation will use (mixagi60), removed most of the false
   bottom-half capital and business income and left wealth completion
   unchanged, at the cost of the model's any-order predictions. Its top is
   less stable than the base model's (top 1% share 18.9% to 22.3% over three
   seeds, against 17.3% to 18.3%). Any variant must be trained in the order it will be used
   with; trained in one order and used in another, it overshoots.
4. **The mix of income in the top 0.1% is not solved**, and it is what
   remains between this model and a usable top. The most direct candidate
   is to make adding up part of the model: for instance, draw the
   components and compute AGI as their sum plus an explicit remainder
   variable (other income less adjustments), or encode each component as a
   share of AGI. That changes the data preparation and was not tested.
   Post-training (reweighting or tilting toward SOI tables by AGI group and
   income type) can correct the remaining shortfall, at a measurable cost
   in distance from the model.

## Unfinished

- The pufstop and big PUF-best checkpoints evaluated above are copies
  taken mid-run (epochs 111 and 106). Their wealth completion scores in the
  table are for the final PUF-best checkpoints (pufstop epoch 139, job
  28922237; big epoch 192, job 28923343); the mid-run copies were not
  scored.
- **One seed only:** sched, tilt, pufstop, big. Two seeds: mixord60.
  Three seeds: base, mixagi60. Before mixagi60 is adopted it needs more
  seeds, larger generated samples (the top 0.1% is about 660 records), and
  a check of the share of training given to the generation questions.
- **Not tested:** making components add up to AGI; square-root weights
  with the weight as a model variable; a sampling temperature at
  generation (not exact, so not tried); shares of training on the
  generation questions other than half.
- The whole-file levels here compare generated records with the training
  records the model saw. They are aggregates only; no record-level rows
  were written outside scratch.
