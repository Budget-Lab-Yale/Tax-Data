---
title: "Test 4, probe O — how much generated PUF records depend on the order of drawing"
role: report
workstream: rebuild
status: current
updated: 2026-10-10
sot: research/rebuild/tests/probes/SPEC.md
supersedes: []
superseded_by: null
---

# How much generated PUF records depend on the order of drawing

## Summary

- **The order matters a great deal for test 3's model (version 2).** With
  the model unchanged, the top 1% share of AGI in generated records ranges
  from 4.7% to 24.5% across ten orders, against 19.4% in the PUF. Total AGI
  ranges from $6.36tn to $9.91tn, against $8.09tn. These gaps are many times
  the sampling noise.
- **No single order gets everything right.** Orders that draw AGI before
  the income components (AGI first, or a fresh random order for each record)
  get total AGI and the top 1% share roughly right. However, they produce
  spurious capital gains, partnership and dividend income in the bottom
  half. Orders that draw the components first and AGI after them nearly
  remove that false bottom-half income. However, they overshoot the top, and
  in the top 0.1% they draw AGI about $500–600bn above the sum of its
  components (PUF: $28bn).
- **A fresh random order for each record is the best compromise run.** Total
  AGI is $7.99tn, the top 1% share 17.8% and the top 0.1% share 8.0% (PUF
  $8.09tn, 19.4%, 8.8%). False bottom-half capital gains are $32bn against
  $13bn, compared with $124bn with AGI first.
- **Held-out log-probability measures the inconsistency directly.** It is
  the chain-rule probability the model gives true validation records, and
  it varies by order. Across the orders tested, it differs by 0.44 nats per
  record on average. For one record, the best and worst orders differ by
  2.3 nats on average. A consistent model would show no difference.
  Averaging the model's probability over all orders scores better than any
  single order.
- **Held-out log-probability alone does not pick the best generation
  order.** It ranks highest the orders that put AGI last. That ranking
  matches their clean bottom half, but not their overshoot at the top. It
  ranks test 3's order worst at the top, which agrees with generation.

## What was run

All jobs ran in Slurm on 10 October. Outputs are under
`/nfs/roberts/scratch/pi_nrs36/jar335/probes/o/`, and logs are under
`../logs/o_*`. The model is unchanged: test 3's version 2 checkpoints,
model seeds 1, 2 and 3.

| Step | Script | What it does |
|---|---|---|
| Orders | `o_orders.py` | Defines the fixed orders (below). |
| Generation | `o_complete.py` (a copy of `../posttrain/h_complete.py` with two additions), `o_gen.sbatch` | Generates 664,624 whole PUF records (PUF source, 2015) per order and model seed: 30 files at sampling seed 1. For model seed 1 it also generates every order except the greedy one at sampling seed 2, to measure sampling noise. |
| Greedy order | `o_greedy.py`, `o_greedy.sbatch` | Chooses a fixed order from the model's own predictions on 40,000 PUF training records (model seed 1). |
| Held-out log-probability | `o_complete.py --mode score`, `o_score.sbatch` | Scores the 20,768 PUF validation records in every order, for each model seed. |
| Tables | `o_compare.py` (a copy of `t_compare.py` with the joint checks added), `o_table.py`, `o_cpu.sbatch` | Comparison: job 29084004. Tables: the latest `o_cpu` log. |

The orders (the 40 PUF variables; wealth variables are never drawn):

| Name | Order |
|---|---|
| test 3 | Test 3's own order: six categoricals (filing status, dependent status, exemptions, children for the child credit, children for the EITC, form of deduction), then the demographics (age band, gender, earnings split), then AGI, then the other amounts. |
| demographics last | T's baseline: test 3's order with the demographics moved to the end. |
| AGI first | T's fix: AGI, then the rest of the "demographics last" order. |
| components first | Wages, long- and short-term capital gains, partnership and S corporation, dividends, interest, Schedule C, then the other amounts, then AGI, the six categoricals and the demographics. |
| AGI, then by size | AGI, then every other amount in order of its weighted total of absolute dollars in the PUF (wages, pensions, Schedule E, partnership, ...), then the six categoricals and the demographics. |
| reverse | Test 3's order reversed (the amounts, AGI, the demographics, the categoricals). |
| random | A fresh random order for each record. This averages over orders. |
| most sure first | For each record, at each step: the hidden variable whose predicted distribution has the lowest entropy, meaning the one the model is most sure about. |
| least sure first | The same, choosing the highest entropy. In practice AGI always goes first, then wages. |
| greedy | A fixed order. At each step the next variable is the one whose true value the model predicts best on average, on training records. It begins with rare amounts that are almost always zero and ends with interest, wages and AGI. |

The comparison uses T's method. Each file is compared with the PUF training
records (166,156 records, 120.7 million returns), using the training records'
own AGI thresholds. Records are grouped by their own AGI. "The six
components" are wages, capital gains (long plus short term), partnership and
S corporation income, ordinary dividends, taxable interest and Schedule C.

## Results: generated records against the PUF

The figures are means over model seeds 1–3 at sampling seed 1. Dollar
amounts are weighted totals.

| | PUF | test 3 | demographics last | AGI first | components first | AGI, then by size | reverse | random | most sure first | least sure first | greedy |
|---|---|---|---|---|---|---|---|---|---|---|---|
| Total AGI ($tn) | 8.09 | 6.36 | 7.03 | 7.84 | 8.92 | 7.86 | 9.91 | 7.99 | 6.83 | 7.82 | 8.49 |
| Weight above true 99th pct (%) | 1.00 | 0.29 | 0.61 | 0.95 | 1.34 | 0.95 | 1.56 | 0.91 | 0.56 | 0.94 | 1.07 |
| Weight above true 99.9th pct (%) | 0.100 | 0.010 | 0.023 | 0.085 | 0.150 | 0.086 | 0.157 | 0.093 | 0.057 | 0.083 | 0.132 |
| Top 1% share of AGI (%) | 19.4 | 4.7 | 8.8 | 17.3 | 24.3 | 17.5 | 24.5 | 17.8 | 13.6 | 17.2 | 22.1 |
| Top 0.1% share of AGI (%) | 8.8 | 1.1 | 1.7 | 6.9 | 10.9 | 7.2 | 10.7 | 8.0 | 6.6 | 6.8 | 11.2 |
| Capital gains, bottom 50 ($bn) | 13 | 99 | 111 | 124 | 9 | 113 | 23 | 32 | 50 | 77 | 92 |
| Partnership and S corp, bottom 50 ($bn) | -31 | 47 | 42 | 40 | -9 | 43 | -3 | 16 | -20 | 54 | -27 |
| Dividends, bottom 50 ($bn) | 14 | 54 | 57 | 48 | 13 | 41 | 15 | 21 | 31 | 36 | 35 |
| Schedule C, bottom 50 ($bn) | 59 | 78 | 85 | 73 | 57 | 81 | 66 | 71 | 72 | 84 | 73 |
| Wages, 99–99.9 ($bn) | 455 | 165 | 386 | 597 | 532 | 612 | 704 | 478 | 253 | 543 | 478 |
| Wages, top 0.1 ($bn) | 211 | 16 | 37 | 132 | 128 | 154 | 188 | 124 | 71 | 109 | 164 |
| Capital gains, top 0.1 ($bn) | 245 | 11 | 33 | 114 | 87 | 130 | 154 | 85 | 105 | 100 | 200 |
| Partnership and S corp, top 0.1 ($bn) | 175 | 10 | 26 | 92 | 132 | 122 | 183 | 96 | 71 | 100 | 145 |
| AGI, top 0.1 ($bn) | 731 | 69 | 124 | 554 | 988 | 584 | 1,064 | 654 | 463 | 543 | 981 |
| Six components, top 0.1 ($bn) | 702 | 41 | 105 | 376 | 399 | 450 | 575 | 341 | 280 | 347 | 553 |
| AGI less six components, top 0.1 ($bn) | 28 | 28 | 20 | 178 | 589 | 135 | 489 | 313 | 184 | 197 | 427 |
| Itemizers, top 0.1 (% of group) | 92.8 | 80.9 | 84.3 | 78.8 | 86.5 | 95.4 | 84.7 | 88.6 | 80.0 | 97.1 | 82.2 |
| Joint filers, top 0.1 (% of group) | 82.5 | 69.8 | 76.7 | 73.7 | 82.2 | 83.7 | 72.9 | 73.6 | 64.9 | 80.5 | 74.1 |
| Both wages and capital gains, top 0.1 (%) | 75.7 | 75.7 | 75.1 | 71.7 | 50.6 | 76.2 | 62.5 | 62.5 | 50.2 | 68.0 | 59.5 |
| Both wages and capital gains, 99–99.9 (%) | 63.7 | 59.0 | 62.6 | 63.2 | 49.5 | 65.6 | 58.5 | 57.4 | 50.6 | 62.6 | 57.0 |
| Both wages and capital gains, bottom 50 (%) | 2.0 | 2.2 | 2.6 | 2.5 | 1.6 | 2.2 | 1.9 | 1.9 | 2.3 | 2.3 | 2.0 |

The full tables cover every component, the itemizer, joint-filer and
wages-and-capital-gains shares for all five AGI groups, and the ranges by
seed. They are in the `o_cpu` log of the table job and in `compare_all.json`.

**Sampling noise against differences between orders.** The table below shows
the range over model seeds 1–3, and for model seed 1 the result at sampling
seed 1 and at sampling seed 2 [in brackets].

| | PUF | test 3 | AGI first | components first | reverse | random | greedy |
|---|---|---|---|---|---|---|---|
| Total AGI ($tn) | 8.09 | 6.19–6.58 [6.58 / 6.60] | 7.60–8.00 [7.93 / 7.97] | 8.67–9.19 [9.19 / 9.11] | 9.63–10.11 [9.98 / 10.05] | 7.84–8.11 [8.03 / 8.01] | 8.32–8.80 |
| Top 1% share (%) | 19.4 | 4.3–5.2 [4.3 / 4.6] | 16.4–17.7 [17.7 / 18.3] | 22.1–25.5 [25.5 / 25.1] | 23.3–26.0 [24.2 / 24.9] | 17.3–18.4 [17.7 / 17.6] | 20.7–22.9 |
| Top 0.1% share (%) | 8.8 | 0.6–1.5 [0.6 / 0.9] | 6.2–7.2 [7.2 / 7.7] | 8.6–12.8 [11.4 / 11.4] | 8.9–13.1 [10.0 / 10.6] | 7.6–8.6 [7.6 / 7.3] | 10.2–12.8 |
| Capital gains, bottom 50 ($bn) | 13 | 70–146 [70 / 75] | 80–150 [80 / 72] | 6–11 [6 / 5] | 12–36 [22 / 12] | 24–38 [35 / 34] | 62–127 |

With 664,624 records, a second sampling seed moves total AGI by under
$0.1tn and the top 1% share by under one percentage point. Model seeds move
these figures by more than sampling seeds do, about 1–3 points in the top 1%
share. Both are small against the differences between orders. In the top
0.1% (about 660 records) a difference of two points is within the spread.

What the generation tables show:

- **AGI drawn late, after categoricals only, loses the top.** This is T's
  finding, and drawing the demographics before AGI makes it worse: test 3's
  own order gives a 4.7% top 1% share against 8.8% with the demographics
  last. In the PUF the demographic file is missing for most high-income
  records. When age band and gender are already drawn, the model treats the
  record as one with demographics, and such records are rarely at the top.
  Choosing the variable the model is most sure of next has the same
  problem: AGI comes almost last (13.6%).
- **AGI drawn before the components gives the right top but false income
  at the bottom.** This holds for AGI first, AGI then by size, and least
  sure first, which draws AGI first in practice. Total AGI is $7.8–7.9tn and
  the top 1% share 17.2–17.5%. Given a low AGI, the model still draws
  capital gains, partnership income and dividends that are not there:
  bottom-half capital gains of $77–124bn against $13bn. It also puts too
  much wage income just below the top 0.1% ($543–612bn against $455bn).
- **Components drawn before AGI give a clean bottom but too much at the
  top.** This holds for components first, reverse and greedy. Bottom-half
  capital gains are $9bn, $23bn and $92bn against $13bn; for greedy the
  figure is high because capital gains come before wages. Partnership
  income in the bottom half is a net loss, as in the PUF. However, the top
  1% share is 22–25%, and in the top 0.1% AGI exceeds the six components
  by $427–589bn against $28bn. The model draws AGI above what the record's
  components support, so records land in the top 0.1% without the income
  that should put them there. Fewer of these records have both wages and
  capital gains (51% against 76%).
- **A random order for each record lands between the two.** Its aggregate
  top is closest to the PUF of any order. Its bottom-half false income is
  about a quarter of the AGI-first figure. In the top 0.1% its components
  still fall short ($341bn against $702bn), as in every order. No order
  fixes the mix of income at the very top.

## Results: held-out log-probability by order

For each of the 20,768 PUF validation records (none used in training), the
model scored the true record by the chain rule in each order. The score is
the sum over the 40 variables of the log-probability of the true value,
given the true values of the variables before it in that order. It is
reported as nats per record (negative log-probability; lower is better). It
is computed on the model's token bands, not on dollars. For a model whose
predictions were mutually consistent, every order would give the same
number. The variables a record does not have (demographics are missing on
most top records) stay hidden and are not scored. The validation file
over-represents the top, as the PUF does: 6,085 records are in the top 1%
and 2,505 in the top 0.1%.

The figures are means over model seeds 1–3. For each seed, all orders are
scored on the same records. The ranking of orders is the same for every
seed; seed-by-seed figures are in the table log.

| Order | All records (population-weighted) | Top 1% records (unweighted) | Top 0.1% records (unweighted) |
|---|---|---|---|
| test 3 | 26.61 | 63.40 | 64.82 |
| demographics last | 26.65 | 63.40 | 64.82 |
| AGI first | 26.62 | 62.41 | 63.37 |
| components first | 26.31 | 61.98 | 63.43 |
| AGI, then by size | 26.69 | 62.38 | 63.26 |
| reverse | 26.30 | 62.50 | 64.30 |
| random (5 draws of orders) | 26.37 | 62.52 | 64.02 |
| most sure first | 26.34 | 63.22 | 65.04 |
| least sure first | 26.69 | 62.53 | 63.73 |
| greedy | 26.25 | 62.38 | 64.23 |
| average of the model's probability over all the orders above (seed 1) | 26.14 | | |

- **Size of the inconsistency.** Across orders the mean score spans 0.44
  nats per record (26.25 to 26.69). For a single record, the gap between
  its best and worst order averages 2.3 nats, a factor of about 10 in
  probability. Among top 1% records, the per-record standard deviation
  across orders is 1.4 nats, against 0.66 for all records. The five draws
  of random orders agree to within 0.02 nats, so the differences between
  orders are not noise in the scoring.
- **Where it comes from.** AGI's own term shows it most clearly. Scored
  first, AGI costs 4.05 nats on average. Scored last, given every amount,
  it costs 1.69 nats. The components cost correspondingly more when they
  come before AGI. A consistent model would trade these off exactly, and
  this one does not. For top 1% records, test 3's order costs 7.47 nats on
  AGI. This is the six-categoricals question that T found to be wrong.
- **Averaging over orders beats every single order** (26.14 against the
  best single order's 26.19, seed 1). This is expected when the orders
  disagree, and it is the scoring counterpart of the random-order
  generation.

**Does it rank orders as the generation tables do?** It does in part.

- It ranks test 3's order and "demographics last" worst on the top 1%
  records. That matches their generated top: a 4.7% and 8.8% share against
  19.4%.
- It ranks the orders that put AGI after the components best overall:
  greedy, reverse and components first. That matches their clean bottom
  half, but not their overshoot at the top or their AGI that exceeds its
  components.
- It ranks "AGI, then by size" and "least sure first" worst overall. Yet
  their generated top is as good as AGI first.
- On the top 0.1% records, the AGI-first orders score best (63.3–63.4).
  Their generated top 0.1% share is 6.8–7.2% against 8.8%. Components
  first, which scores 63.4, gives 10.9%.

There are two reasons the score and the generation tables can disagree.
First, scoring feeds the model the true values of the earlier variables,
while generation feeds it its own draws. An early mistake in generation
therefore carries forward, but in scoring it never does. Second, the score
penalizes the model for giving true records too little probability. It
penalizes spurious records, such as a $50m AGI with modest components, only
indirectly, so it does not punish overshoot at the top in proportion to its
effect on AGI shares.

## What this means for choosing the order

1. **The order is a modelling choice that moves the top of the
   distribution by more than any training change T tried.** It should be
   chosen deliberately, and checked with generation tables, before any
   post-training is applied.
2. **Held-out log-probability is useful but not sufficient.** It is cheap
   (seconds per order) and it flags the clearly bad orders: AGI drawn after
   the categoricals and demographics. It also measures how inconsistent a
   model is: the 0.44 nats per record spread across orders is a number to
   track for any retrained model, such as probe S's. A model trained so that
   this spread shrinks would make the choice of order matter less. It should
   not be the only rule for choosing an order, because it does not see
   errors that compound during generation.
3. **For the unchanged model, a random order for each record is the most
   balanced choice run.** Of the fixed orders, the AGI-first ones are best at the
   top and components first is best at the bottom. Neither is right in both
   places. Averaging over orders is also what the held-out score favours.
4. **None of the orders fixes the mix of income in the top 0.1%.** The
   components there sum to $280–575bn against $702bn in every order, and
   orders that get AGI right at the top do so with AGI that exceeds its
   components. T's suggestion stands: make adding up part of the data (AGI
   computed from the components plus a remainder), or correct the
   composition in post-training.

## Unfinished and caveats

- **The greedy order** was chosen from model seed 1 only and used for all
  three seeds. Its sampling-noise repeat was not run. Its generation job for
  seed 3 ran out of GPU memory once (on a GPU shared with another job) and
  was rerun (job 29084003).
- **Scoring and generation use slightly different probabilities.** Scoring
  uses the model's raw probabilities. Generation, as in test 3, restricts
  draws to codes the decoder can turn into dollars.
- **The scored records skip missing variables.** Demographics are missing
  on most top records, so test 3's order and "demographics last" score the
  same there.
- **Not run:** averaging the model's predictions over several orders at
  each generation step (a stronger form of order averaging than one random
  order per record); generation with more than 664,624 records to steady
  the top 0.1%; a record-by-record check of AGI against the sum of its
  components; scoring on training records.
- **Process note:** one early read of `meta.json` to list the variable
  names ran on the login node rather than in Slurm.
- All figures are weighted aggregates. No record-level rows were written
  outside scratch.
