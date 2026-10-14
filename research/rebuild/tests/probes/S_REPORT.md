---
title: "Test 4, probe S — training the model on the questions generation asks"
role: report
workstream: rebuild
status: current
updated: 2026-10-10
sot: research/rebuild/tests/probes/SPEC.md
supersedes: []
superseded_by: null
---

# Training the model on the questions generation asks

## Summary

- **Recipe to adopt: draw AGI first, and train 10% to 25% of each batch on
  the questions generation asks in that order.** With 25% (five seeds, four
  sampling draws each), generated PUF records have total AGI of $7.97tn
  (PUF $8.09tn), a top 1% AGI share of 19.6% (PUF 19.4%) and a top 0.1%
  share of 8.9% (PUF 8.8%). Test 3's model, with AGI also drawn first, gives
  $7.88tn, 17.7% and 7.3%. False capital gains in the bottom half of AGI fall
  from $104bn to $70bn (PUF $13bn). Partnership income there falls from
  +$41bn to +$6bn (PUF a net loss of $31bn). With 10% the top is about as
  good (18.8%, 8.7%), the bottom-half fix is smaller ($83bn, +$30bn), and
  the model stays as good as test 3's in other orders. SCF wealth completion
  is unchanged at both shares (35.5 to 35.7 nats per record against 35.5).
- **More is worse.** Half the batch on ordered questions (T's recipe) and
  the whole batch both overshoot the top: top 1% share 20.1% and 21.7%,
  total AGI $8.18tn and $8.17tn. The purely ordered model also loses the
  ability to answer other questions, so it cannot be used in any other
  order.
- **Random-order training does not make the model robust to the order.**
  It asks the same questions the existing training already asks; only the
  weights differ. Its generated records are slightly worse than test 3's
  with AGI first (top 1% share 16.4–16.7%) and about as bad with
  demographics last (9.9–10.6% against 9.2%).
- **No recipe tested removes the need to choose an order.** A model is good
  in the orders it was trained on. Training on two fixed orders (AGI first
  and demographics last) improves both, but the second only partly (top 1%
  share 15.8%).
- **Most of the spread between seeds at the top is real training-seed
  variation, not sampling noise**, for every recipe that uses ordered
  questions. At 25% the top 1% share varies with a standard deviation of 0.4
  percentage points between sampling draws of one model, and 1.3 points
  between models trained with different seeds. Test 3's model shows no seed
  variation beyond sampling noise (0.3 points).
- **The mix of income in the top 0.1% is still wrong in every recipe.**
  Given each record's true AGI, the components there add to $400–537bn
  against $704bn in the PUF.

## Terms used

- **Ordered questions.** For a record and an order of its variables, a
  random number k of the first variables is visible and the model is
  scored on variable k+1. These are exactly the questions generation asks
  when it draws variable k+1 after variables 1 to k. T's code calls this
  the ordered task.
- **Any-order training.** The training test 3 used: for each record a
  random share of its variables is hidden and the model is scored on all of
  them.
- **Random-order questions.** Ordered questions where each record gets its
  own random order, drawn afresh every step (new in `s_train.py`).
- **Sampling draw.** One generated file of 664,624 records (four times the
  PUF training count) from a trained model with one random-number seed.
- **Nats.** Units of log-likelihood; lower is better. One nat per record
  means the model gives the true record about 2.7 times less probability.

## What was run

All jobs ran in Slurm on 10 October, on test 3's version 2 data
(`full_v2`); outputs are under `/nfs/roberts/scratch/pi_nrs36/jar335/probes/s/`
and the list of jobs is `jobs.txt` there.

**Training.** `s_train.py` is `t_train.py` with two additions:
`--mix-style` (ordered questions in one fixed order, in two fixed orders,
or in a random order per record) and `--keep-last` (no early stopping;
keep the last epoch). Every run: 60 epochs, learning rate decaying over
those 60 epochs, the rest as test 3. Every recipe except the purely
ordered one stops early on the any-order validation loss as test 3 did.

| Name in tables | Share of each batch on ordered questions | Order(s) | Seeds | Training jobs |
|---|---|---|---|---|
| test 3 | none | | 3 (test 3's models) | |
| 10% | 10% | AGI first | 3 | 28964785, 28964789, 28964793 |
| 25% | 25% | AGI first | 5 | 28961245, -49, -53, 28964777, 28964781 |
| 50% | 50% | AGI first | 5 (T's three, two new) | T's; 28961257, 28961261 |
| 100% | all | AGI first | 3 | 28962040, -44, -48 (`--keep-last`) |
| random 50% | 50% | random per record | 3 | 28961277, -81, -85 |
| random 100% | all | random per record | 3 | 28961289, -93, -97 |
| two orders 50% | 50% | AGI first or demographics last, half each | 2 | 28961301, 28961305 |

"AGI first" is T's order: AGI, then filing status, dependent status,
exemptions, children for the child credit, children for the EITC, form of
deduction, then the other amounts, then age, gender and earnings split.
"Demographics last" is the same without AGI moved to the front (test 3's
order). A first attempt at the 100% recipe stopped at epoch 6, because
the any-order loss it no longer trains on stopped improving; it was
cancelled and rerun with `--keep-last`.

**Evaluation of every trained model** (`s_eval.sbatch`, `s_gen.sbatch`,
`s_gen_multi.sbatch`):

- T's step-1 question (`t_diag.py`): weight the model puts in the top 1%
  band of AGI given nothing, given the first six variables, and given every
  PUF variable except AGI (PUF: 0.99%).
- Generation score (`s_gennll.py`, new): every variable of each PUF
  validation record hidden, then revealed one at a time in an order; the
  score is the log-likelihood that generation in that order gives the real
  record. Four orders: AGI first, demographics last, and two fixed random
  orders. A model whose answers are consistent with one another gives the
  same score in every order.
- Whole PUF records generated with AGI first (four sampling draws of
  664,624 records each, 2.66 million records in all) and with demographics
  last (one draw; three for the random-order and two-order recipes).
- PUF training records completed given their true AGI and first six
  variables (T's "given AGI" file).
- PUF any-order validation loss (from the training log) and SCF wealth
  completion (H's score on 6,112 SCF validation records), both at the kept
  epoch.
- Levels against the PUF training records with `t_compare.py`, gathered
  by `s_collect.sbatch` (final run 29087231) and `s_summary.py`; components
  by AGI group with `s_groups.py` (job 29087735). The logs of those two
  jobs hold every table below:
  `/nfs/roberts/scratch/pi_nrs36/jar335/probes/logs/s_collect_29087231.out`
  and `s_final_29087735.out`.

Twelve evaluation jobs first failed with an out-of-memory error, all on
one node (a1127u05n01) whose GPU was held by another process; they were
rerun elsewhere at 22:45 (jobs 29084013–24) and succeeded. No job was
preempted.

## Results: generated records with AGI drawn first

Means over seeds and sampling draws. "Sampling SD" is the standard
deviation between sampling draws of the same model; "seed SD" is the
standard deviation between models trained with different seeds, after
removing the part sampling noise contributes. Dollar components are
summed over all generated records in each AGI group, with groups set by the
PUF's own AGI percentiles.

| | PUF | test 3 | 10% | 25% | 50% | 100% | random 50% | random 100% | two orders 50% |
|---|---|---|---|---|---|---|---|---|---|
| Seeds × draws | | 3 × 4 | 3 × 4 | 5 × 4 | 5 × 4 | 3 × 4 | 3 × 4 | 3 × 4 | 2 × 4 |
| Total AGI | $8.09tn | $7.88tn | $7.92tn | $7.97tn | $8.18tn | $8.17tn | $7.69tn | $7.61tn | $7.90tn |
| Top 1% share of AGI | 19.4% | 17.7% | 18.8% | 19.6% | 20.1% | 21.7% | 16.7% | 16.4% | 19.0% |
| sampling SD | | 0.3 | 0.3 | 0.4 | 0.7 | 0.4 | 0.6 | 0.5 | 0.4 |
| seed SD | | 0.0 | 0.6 | 1.3 | 1.3 | 0.3 | 0.1 | 0.0 | 0.6 |
| Top 0.1% share of AGI | 8.8% | 7.3% | 8.7% | 8.9% | 9.4% | 10.7% | 6.2% | 5.9% | 8.7% |
| AGI in top 0.1%, $bn | 731 | 589 | 708 | 728 | 790 | 902 | 492 | 465 | 713 |
| Six components in top 0.1%, $bn | 702 | 401 | 485 | 412 | 470 | 604 | 333 | 253 | 435 |
| Capital gains, bottom 50%, $bn | 13 | 104 | 83 | 70 | 62 | 29 | 111 | 93 | 102 |
| Partnership and S corp., bottom 50%, $bn | -31 | 41 | 30 | 6 | 5 | -5 | 37 | 36 | -6 |
| Dividends, bottom 50%, $bn | 14 | 47 | 33 | 27 | 23 | 26 | 41 | 36 | 30 |
| Wages, 99th–99.9th pct, $bn | 455 | 594 | 519 | 548 | 557 | 561 | 629 | 645 | 489 |

The six components are wages, capital gains (long plus short term),
partnership and S corporation income, ordinary dividends, taxable interest
and Schedule C income.

Top 1% share of AGI for each trained model, mean of its four draws (PUF
19.4%). The range across draws of one model is at most 1.6 points; the
individual draws are in the collect log.

| Recipe | seed 1 | seed 2 | seed 3 | seed 4 | seed 5 |
|---|---|---|---|---|---|
| test 3 | 17.7% | 17.5% | 17.7% | | |
| 10% | 18.4% | 18.5% | 19.6% | | |
| 25% | 17.3% | 20.3% | 19.4% | 20.6% | 20.3% |
| 50% | 18.5% | 21.1% | 21.8% | 19.9% | 19.1% |
| 100% | 21.4% | 22.1% | 21.8% | | |
| random 50% | 17.0% | 16.8% | 16.3% | | |
| random 100% | 16.5% | 16.3% | 16.3% | | |
| two orders 50% | 18.5% | 19.4% | | | |

T's three 50% seeds (one draw each: 18.9%, 20.6%, 22.3%) are seeds 1 to 3
of the 50% row; with four draws each they are 18.5%, 21.1% and 21.8%.

## Results: the same models in another order (demographics last)

| | PUF | test 3 | 10% | 25% | 50% | 100% | random 50% | random 100% | two orders 50% |
|---|---|---|---|---|---|---|---|---|---|
| Total AGI | $8.09tn | $7.06tn | $6.84tn | $6.98tn | $6.54tn | $4.06tn | $7.29tn | $7.34tn | $7.64tn |
| Top 1% share of AGI | 19.4% | 9.2% | 9.2% | 9.2% | 12.9% | 25.6% | 9.9% | 10.6% | 15.8% |
| Top 0.1% share of AGI | 8.8% | 2.0% | 2.9% | 3.2% | 5.4% | 14.6% | 2.4% | 2.0% | 6.9% |
| AGI in top 0.1%, $bn | 731 | 148 | 207 | 228 | 360 | 945 | 180 | 153 | 549 |

Means over seeds (one draw each, except random and two-order recipes with
three draws). The 100% models are unusable in this order: one seed gives
total AGI of $3.49tn and another puts 2.1% of weight above the true 99th
percentile. Training on an order makes the model good in that order and
in no other.

## Results: the step-1 questions, consistency across orders, validation loss and SCF completion

Weight in the top 1% band of AGI (PUF 0.99%), range over seeds:

| | test 3 | 10% | 25% | 50% | 100% | random 50% | random 100% | two orders 50% |
|---|---|---|---|---|---|---|---|---|
| Given nothing | 0.90–0.97% | 0.88–0.96% | 0.91–1.04% | 0.95–1.11% | 1.03–1.10% | 0.93–0.97% | 0.92–0.93% | 0.92–0.96% |
| Given the first six | 0.60–0.63% | 0.48–0.67% | 0.49–0.72% | 0.63–1.01% | 0.35–2.89% | 0.52–0.80% | 0.60–0.97% | 0.82–0.86% |
| Given all but AGI | 0.97–1.01% | 0.96–0.99% | 1.00–1.04% | 0.96–1.07% | 3.27–7.32% | 1.01–1.04% | 1.01–1.03% | 1.04–1.19% |

With AGI drawn first, the question that matters for AGI is "given
nothing", which every recipe answers within about 10% of the truth. The
six-variable question is answered no better by random-order training than
by test 3's.

Generation score of real PUF validation records (nats per record, lower
is better; 20,768 records, of which the top 1% band is a separate row),
and the other two measures:

| | test 3 | 10% | 25% | 50% | 100% | random 50% | random 100% | two orders 50% |
|---|---|---|---|---|---|---|---|---|
| AGI first, all records | 26.58–26.65 | 26.64–26.68 | 26.50–26.62 | 26.39–26.52 | 26.21–26.31 | 26.61–26.74 | 26.58–26.70 | 26.49–26.54 |
| Demographics last, all records | 26.62–26.68 | 26.69–26.74 | 26.58–26.71 | 26.56–26.70 | 26.89–28.70 | 26.65–26.77 | 26.64–26.76 | 26.49–26.54 |
| Two random orders, all records | 26.39–26.71 | 26.66–26.92 | 26.87–27.13 | 27.39–27.83 | 40.3–46.1 | 26.41–26.77 | 26.42–26.73 | 27.47–27.80 |
| Largest minus smallest of four orders | 0.23–0.24 | 0.22–0.24 | 0.49–0.57 | 0.95–1.31 | 17.3–19.8 | 0.22–0.29 | 0.27–0.29 | 1.05–1.26 |
| AGI first, top 1% records | 59.08–59.30 | 59.22–59.31 | 59.07–59.43 | 58.92–59.25 | 58.71–58.90 | 59.17–59.48 | 59.33–59.43 | 59.25–59.45 |
| Demographics last, top 1% records | 59.61–59.85 | 59.75–60.06 | 59.93–60.33 | 59.63–60.22 | 59.27–61.95 | 59.58–59.87 | 59.34–60.04 | 59.40–59.52 |
| PUF any-order validation loss | 0.726–0.728 (T) | 0.732–0.733 | 0.736–0.738 | 0.747–0.751 | 1.074–1.121 | 0.728–0.730 | 0.728–0.730 | 0.748–0.750 |
| SCF wealth completion | 35.50–35.56 | 35.58–35.67 | 35.48–35.59 | 35.50–35.60 | 35.65–35.75 | 35.47–35.51 | 35.43–35.49 | 35.48–35.56 |

Reading the table: more training on AGI-first questions makes generation
in that order fit held-out PUF records better (26.6 to 26.2 nats) and
makes every other order fit worse. The whole-record score barely sees the
top problem: test 3's model scores real records almost equally in the AGI
first and demographics last orders (0.03 to 0.04 nats apart), yet generates
a top 1% share of 17.7% in one and 9.2% in the other. SCF wealth
completion moves by at most 0.25 nats per record in any recipe (the 100% recipe; 0.1 or less for the others).

## Results: the mix of income given the true AGI

Each PUF training record's true AGI and first six variables fixed, the
rest drawn; $bn, means over seeds.

| Component and AGI group | PUF | test 3 | 10% | 25% | 50% | 100% | random 100% |
|---|---|---|---|---|---|---|---|
| Wages, 99th–99.9th pct | 455 | 627 | 578 | 561 | 544 | 538 | 682 |
| Wages, top 0.1% | 211 | 167 | 155 | 119 | 117 | 149 | 166 |
| Capital gains, bottom 50% | 13 | 70 | 43 | 43 | 52 | 38 | 53 |
| Capital gains, top 0.1% | 245 | 160 | 202 | 157 | 168 | 199 | 127 |
| Partnership and S corp., bottom 50% | -31 | 40 | 31 | 18 | 4 | -4 | 34 |
| Partnership and S corp., 99th–99.9th pct | 186 | 175 | 196 | 198 | 245 | 234 | 145 |
| Partnership and S corp., top 0.1% | 175 | 114 | 108 | 105 | 129 | 143 | 72 |
| Dividends, top 0.1% | 45 | 30 | 28 | 24 | 23 | 30 | 23 |
| Six components, top 0.1% | 704 | 486 | 509 | 419 | 450 | 537 | 400 |

Ordered questions reduce false bottom-half income and wage income just
below the top 0.1%, but no recipe fills the top 0.1%.

## Why random-order training is not new

Test 3's training hides each variable with probability r, with r drawn
uniformly between 0 and 1 for each record. The number hidden is then
uniform between 0 and the number of variables, and which variables are
hidden is uniform given that number. Random-order questions, scored on all
remaining variables, produce the same distribution over which variables
are visible. The two differ only in weighting: test 3's training gives
each hidden variable equal weight, so records with many variables hidden
count more; random-order training gives each record equal weight. The
specific question generation asks with demographics last (AGI given six
categorical variables and no amounts) is one visible set among millions in
both, so neither trains it. The results agree: the random-order recipes
behave like test 3's model in every table.

## What this means for the plan

1. **Generation order: AGI first**, as T recommended.
2. **Pretraining: add AGI-first ordered questions on 10% to 25% of each
   batch.** 25% gives the best AGI levels and bottom-half income mix; 10%
   gives a slightly worse bottom half but keeps the model as good as test
   3's in other orders and halves the seed-to-seed spread (0.6 against 1.3
   percentage points of top 1% share). They cost 0.7% and 1.4% in the
   any-order validation loss and nothing in SCF completion. Do not use 50%
   or more.
3. **The order must still be chosen.** Random-order training is the
   existing training reweighted; training on several fixed orders helps
   each only partly. Whatever order generation will use should be among
   those trained on.
4. **Seed variation at the top is now the larger source of noise.** With
   ordered questions, models trained with different seeds differ in top 1%
   share by about 1.3 points (25% recipe) against 0.4 points between
   sampling draws. Averaging two or three trained models, or post-training
   toward the SOI top shares, would be needed for a stable top. A single
   generated file of four times the PUF count measures a model's top 1% share
   with a standard deviation of 0.3 to 0.7 points.
5. **The top 0.1% income mix needs a different fix** (T's suggestion of
   making components add up to AGI, or post-training by income type).

## Unfinished

- Nothing is running. All 140 jobs in `jobs.txt` have ended; the failures
  are listed above and were rerun.
- Not run: a single generated file of 16 times the PUF count (four draws of
  four times were pooled instead, which measures the same sampling noise);
  more than one demographics-last draw for the fixed-order recipes; shares
  between 10% and 25%; a mix of AGI-first ordered questions and random-order
  questions; the 100% recipe stopped at its best any-order epoch.
- The whole-file levels compare generated records with the PUF training
  records the model saw. Reports carry weighted aggregates only.
