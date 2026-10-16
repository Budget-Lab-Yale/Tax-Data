---
title: "Minimal example, experiment 18: tilt-then-distill, a fine-tuned network against the reweighted v4b file"
role: evidence
workstream: rebuild
status: current
updated: 2026-10-15
sot: research/rebuild/design/plan.md
supersedes: []
superseded_by: null
---

# Experiment 18: tilt-then-distill

## The question

The v4b post-training (`15_BASELINE_V4.md`, last section) is a reweighting.
400,000 records are drawn from the pretrained model V4, and each gets a
weight exp(λ·f(record)) that hits the 136 IRS targets for 2019 exactly and
eight SCF wealth targets softly, with the smallest change in relative
entropy from the starting weights (0.0243 nats). Tilt-then-distill is the
first fallback after tilting in decision 8 of `design/plan.md`: train a copy
of the network so that, sampled directly with equal weights, it produces the
tilted distribution itself. This experiment builds that network and compares
it with the reweighted file on every check the v4b report uses.

Terms used below. *Fine-tuning* means continuing to train an already
trained network on new data, starting from its current parameters. A *pass*
(an "epoch") is one sweep through the training data. The *learning rate* is
the size of each training step. A record or slice is *held out* when it is
kept out of training and used only to score. *Overfitting* means the network
fits the particular records it was trained on better than new records from
the same distribution.

## Design

### What the fine-tune has to learn

V4 generates a v4b record in a fixed sequence of questions (`03_generate.py`
with `borrowed={'label': 'owner', 'foreign': 'absent'}`):

1. the five incomes (wages, interest, dividends, capital gains, other_agi),
   in a random order, each drawn given the incomes already drawn, under the
   PUF label;
2. net worth given the four shared incomes, under the SCF label, with
   other_agi left out (decision 16);
3. married, then dependents, under the PUF label, with net worth visible.

The fine-tuned network is sampled by exactly the same procedure. So the
fine-tune trains exactly these questions on tilted records, and nothing
else. Each tilted record is shown to the network in two forms:

- **As a PUF record.** The five incomes, married and dependents, with a
  random subset hidden and the network scored on predicting the hidden ones.
  This is the same masked any-subset objective as pretraining. The net worth
  slot is present as an input but is never scored under the PUF label. It is
  visible when all five incomes are visible and hidden otherwise, which is
  what the sampler shows the network when it draws married and dependents
  (net worth visible) and when it draws incomes (net worth not yet drawn).
- **As an SCF record.** The same record relabelled SCF, other_agi left out,
  the four shared incomes visible, married and dependents hidden, and the
  network scored on net worth only. This is the exact question the sampler
  asks when it draws net worth.

The second form is how decision 16 is kept. Net worth stays something the
network learns only from records it sees as SCF records, so the fine-tune
does not teach it a PUF-labelled record with net worth, a combination no
pretraining record had. Because the sampler draws net worth without
other_agi, the fine-tuned network cannot make net worth depend on other_agi
even where the tilt does; training on this view gives it the closest thing
it can represent, the tilted distribution of net worth given the four
shared incomes, averaged over other_agi.

**Training weights.** Each record is weighted by its tilted weight
(`weight_after` in `tilt/v4b_allunits/weights_v4b.parquet`), which is the
tilt factor times the importance weight from the top oversampling. Weighting
the stack, rather than redrawing it with equal weights, keeps the stack's
many records at the top of wages, capital gains, other_agi and net worth,
each with a small weight, so the top conditionals are learned from many
records rather than a few repeated ones.

**Hold-out.** 5% of the stack's records (20,183) are kept out of the
fine-tune and used only to measure it.

### Variants

| variant | training data | passes | peak learning rate | wall time of the fine-tune |
|---|---|---:|---:|---:|
| A | the tilted stack (379,817 records, two forms each) | 8 | 0.0002 | 1.4 minutes |
| A20 | the same | 20 | 0.0002 | 2.3 minutes |
| B | the same, plus the original pretraining records (2015 PUF and 2016 SCF, masked any-subset, pretraining weights) carrying one third of the total weight ("replay") | 8 | 0.0002 | 1.2 minutes |
| C | 3.2 million new records from V4, tilted (see section 8): each pass a different 400,000, so no record is seen twice | 8 | 0.0002 | 1.5 minutes |

Pretraining peaked at a learning rate of 0.001 and ended at 0.00005. The
fine-tunes warm up over the first 5% of steps, then decline along a cosine
curve to 10% of the peak. Times are on one RTX A5000 or RTX 5000 Ada GPU.
Generating the 200,000-record file and the 400,000-record stack from a
fine-tuned network took about 20 seconds.

### What each fine-tuned network produced

- A **plain file**: 200,000 records, seed 3, no oversampling, equal weights.
  This is the direct test: the network sampled as is.
- An **oversampled stack**: the v4b recipe through `03_stack.py` with the
  model folder pointed at the fine-tuned network (four files of 100,000,
  tenfold oversampling of the top 1% of wages, capital gains, other_agi and
  net worth, importance weights).
- The stack **tilted again** to the same targets as v4b (136 exact, eight
  wealth targets soft at 2%). The size of this final reweight measures what
  the fine-tune left undone.

Every file is scored with the same code as v4b (`05_tilt`, `06_wealth_tables`,
`07_baseline`), with weights scaled to SOI's 157.8 million returns for 2019,
incomes in 2019 dollars from the V4 map and net worth from the 2015 SCF map.
Standard errors for a weighted file use the usual linearised formula for a
total with weights scaled to a fixed sum; for an equal-weight file this is
the formula of the v4b redraw.

### The move of a network

The tilt's move is the relative entropy of the new weights from the old
(0.0243 nats). The fine-tune's move is the relative entropy of the
fine-tuned network's distribution from V4's, estimated as the average over
the fine-tuned plain file of log q_fine-tuned(record) − log q_V4(record).
The log-probability of a record under the sampler is the sum of the log
conditional probabilities of its draws. Because the incomes come in a random
order, the sampler's probability of the incomes is the average over all 120
orders of the product of the five conditionals; this is computed exactly
from the 32 network passes that show each subset of incomes. The code was
checked against the probabilities recorded when the v4b stack was generated:
for the 100,000 records of the net worth file and of the wages file, the
recomputed V4 probability of the oversampled band differs from the recorded
one by at most 0.0000003.

Both moves are measured on records banded into the model's bins. The tilt's
0.0243 also counts the dollar amounts drawn within bins, which the tilt
factor depends on and a network of bins cannot represent, so the part of the
tilt a network could copy is somewhat less than 0.0243.

Code: `18_distill.py` (fine-tune, generation through `03_stack.py`,
log-probabilities), `18_score.py` (scoring and the final reweight),
`18_fresh.py` (variant C's records), with `18_distill.sbatch`,
`18_fresh.sbatch` and `18_score.sbatch`. Outputs under
`/nfs/roberts/scratch/pi_nrs36/jar335/minimal/`: `model/D18/<variant>/`,
`gen/D18/<variant>/`, `gen/D18/fresh/`, `tilt/D18/<variant>/`,
`checks/D18/` (`score_long.csv`, `score_tables.md`,
`final_reweight_moves.csv`, `<variant>/logprob_summary.csv`). Slurm jobs
29705158 (A), 29705159 (A20), 29705160 (B), 29705629 (new records for C), 29706009 (tilts of the new records), 29706010 (C), 29706013 (scoring of all four; an earlier scoring of A, A20 and B was 29705599). No existing file was changed.

## Results

Column names in the tables: "V4 plain" is 200,000 records from V4 with no
post-training; "reweighted" is the v4b stack with the tilted weights (the
current post-training); "redraw" is the v4b redraw of 200,000 equal-weight
records from it; "A plain" and so on are the fine-tuned networks' plain
files; "A + final reweight" is A's stack tilted again.

### 1. The 136 IRS targets

| | V4 plain | reweighted | redraw | A plain | A20 plain | B plain | C plain | A stack | A + final reweight |
|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| within 2 standard errors (of 136) | 32 | 136 | 133 | 89 | 70 | 63 | 76 | 75 | 136 |
| mean \|z\| | 4.59 | 0 | 0.73 | 1.73 | 2.40 | 2.76 | 2.16 | 2.36 | 0 |
| median miss (%) | 9.4 | 0 | 1.7 | 3.1 | 4.0 | 6.2 | 3.9 | 3.3 | 0 |
| joint returns, miss (%) | +5.4 | 0 | −0.7 | +0.5 | −1.6 | +1.3 | +2.2 | +0.8 | 0 |

(The redraw's 133 is computed here with the standard error of the redraw
itself; the v4b report's 134 used the standard error under the tilted
weights. Both are one draw.)

The fine-tune does most of the work but not all of it. A's plain file has
89 of the 136 targets within 2 standard errors against 32 for V4 before any
post-training and 133 for an equal-weight sample from the tilted file. Its
median miss is 3.1% against 9.4% before. The stack has smaller standard
errors, so the same network's stack shows fewer targets within 2 of them
(75).

The largest misses of A's plain file are in the bracket with no AGI, where
amounts net to small numbers: capital gains −76% (z −12.4), interest −56%
(z −6.5), returns with wages +26% (z +6.2). The next are returns from
$200,000 to $500,000: wages count −6.0% and amount −6.3% (z about −5.5).
V4 before post-training missed most on the joint share (+5.4%, z 17.5) and
interest counts at $50,000 to $75,000 and $200,000 to $500,000 (about −15%).
In A's plain file those are z +1.6, −0.3 and −4.1: the first two fixed, the
third much reduced.

More passes and replay both make the targets worse. A20 overfits (section 7).
B's replay of the 2015 records pulls the network back toward 2015: interest
holders 29.6% against the 2019 target of 30.8% (A: 30.5%).

### 2. The eight wealth targets (threshold form, miss in %)

| target | V4 plain | reweighted | A plain | A20 plain | B plain | C plain | A stack | A + final reweight |
|---|---:|---:|---:|---:|---:|---:|---:|---:|
| units above the SCF's 99th percentile | −20.7 | −0.0 | −1.0 | −4.6 | −5.9 | −7.7 | −1.2 | −0.0 |
| net worth above the 99th percentile | −8.8 | +0.0 | −3.8 | −1.0 | +0.4 | −12.1 | +5.9 | −0.0 |
| units above the 90th percentile | +8.5 | −1.7 | −2.5 | −4.9 | −4.3 | −5.0 | −1.8 | −1.7 |
| net worth above the 90th percentile | −1.6 | −0.0 | −1.8 | −2.0 | −1.2 | −7.2 | +3.1 | +0.0 |
| units above the median | −2.6 | −0.0 | −1.5 | −1.2 | −1.7 | −1.7 | −1.1 | −0.0 |
| net worth above the median | +0.0 | +0.0 | −1.5 | −1.7 | −1.1 | −5.7 | +2.3 | +0.0 |
| total net worth | +0.0 | −0.0 | −1.5 | −1.7 | −1.1 | −5.7 | +2.3 | −0.0 |
| units above $1 million | +18.0 | +1.7 | +3.7 | +1.3 | +1.9 | +1.0 | +4.4 | +1.7 |

The two misses before post-training that mattered, the count above the
99th percentile (−21%) and the count above $1 million (+18%), are much
reduced in every fine-tuned file: to −1.0% and +3.7% in A, and to between
−4.6% and −7.7% and between +1.0% and +1.9% in the others. A's remaining
level error in total net worth is −1.5% in its plain file and +2.3% in its
stack, about the noise of the top of a file this size. C is low throughout
(−5.7% in total), for the reason given in section 8. The reweighted file
leaves −1.7% and +1.7% on two targets because those two contradict each
other (`15_BASELINE_V4.md`, section 4); the final reweight of A's stack lands
on the same compromise.

### 3. Wealth against the 2019 SCF (2015 dollars)

| | 2019 SCF, all units | V4 plain | reweighted | A plain | B plain | C plain | A + final reweight |
|---|---:|---:|---:|---:|---:|---:|---:|
| total ($ trillion) | 89.1 | 89.2 | 89.1 | 87.8 | 88.2 | 84.1 | 89.1 |
| mean | $623,000 | $565,000 | $565,000 | $556,000 | $559,000 | $533,000 | $565,000 |
| median | $84,500 | $78,100 | $84,500 | $81,000 | $80,500 | $80,600 | $84,500 |
| top 10% share | 77.7% | 75.0% | 78.0% | 77.9% | 78.4% | 77.3% | 78.0% |
| top 1% share | 38.4% | 38.2% | 38.4% | 37.7% | 40.0% | 37.1% | 38.4% |
| top 0.1% share | 14.7% | 16.1% | 13.2% | 12.6% | 15.0% | 12.3% | 13.1% |

The SCF has 143 million units; the files have 157.8 million returns, so the
SCF's mean is above the files' at the same total. "V4 plain" is the plain
file, whose top 0.1% (about 170 records) is high by chance; the v4b stack
before the tilt has 14.6%. The fine-tuned networks reproduce the tilted
shape: the top 10% share moves from 75.0% to 77.9% as the tilt moved it to
78.0%, and the median from $78,100 toward $84,500.

### 4. Mean net worth by wage group ($, 2015 dollars)

| wages | 2019 SCF, all units | V4 stack before the tilt | reweighted | A plain | B plain | C plain | A + final reweight |
|---|---:|---:|---:|---:|---:|---:|---:|
| 0 | 706,000 | 1,188,000 | 1,187,000 | 1,141,000 | 1,176,000 | 1,118,000 | 1,142,000 |
| 1-25k | 203,000 | 206,000 | 200,000 | 184,000 | 189,000 | 182,000 | 206,000 |
| 25-50k | 148,000 | 207,000 | 190,000 | 183,000 | 200,000 | 183,000 | 188,000 |
| 50-100k | 333,000 | 403,000 | 372,000 | 373,000 | 368,000 | 363,000 | 362,000 |
| 100-200k | 922,000 | 887,000 | 845,000 | 888,000 | 876,000 | 829,000 | 875,000 |
| 200k+ | 4,500,000 | 3,713,000 | 4,044,000 | 4,254,000 | 4,182,000 | 4,077,000 | 4,171,000 |

The share of units above $1 million by wage group differs from the
reweighted file by 0.3 points on average in A's plain file (largest 1.1
points, earners above $200,000: 64.5% against 63.3%), against 1.6 points
for V4 before the tilt. The known gaps of v4b stay as they were: returns
with no wages hold about $1.1 to $1.2 million against $706,000 in the SCF,
in every file. Distillation neither fixes nor worsens them, as expected,
since it copies the tilt.

### 5. Holder shares (% of returns)

| item | V4 plain | reweighted = IRS 2019 | A plain | A20 plain | B plain | C plain |
|---|---:|---:|---:|---:|---:|---:|
| interest | 27.3 | 30.8 | 30.5 | 30.3 | 29.6 | 30.1 |
| dividends | 17.5 | 18.4 | 18.1 | 17.7 | 17.8 | 17.6 |
| capital gains | 12.6 | 13.8 | 13.7 | 13.2 | 13.0 | 13.4 |
| joint returns | 36.6 | 34.7 | 34.9 | 34.2 | 35.2 | 35.5 |

A closes 67% to 93% of the gap on each of these four shares (dividends least).

### 6. Relationships no target covers

The 25 shares of `05_tilt.relationships` (dividends by wage group and
filing status, capital gains by dividend group, dependents by filing
status). Distance from the reweighted file, in percentage points:

| file | mean distance | largest distance |
|---|---:|---:|
| V4 plain | 1.28 | 7.9 |
| V4 stack before the tilt | 1.06 | 5.0 |
| redraw (pure sampling noise of 200,000 records) | 0.34 | 1.7 |
| A plain | 0.48 | 2.1 |
| A20 plain | 0.58 | 2.8 |
| B plain | 0.74 | 3.9 |
| C plain | 0.62 | 2.4 |
| A + final reweight | 0.48 | 2.6 |

The fine-tuned file lands close to the reweighted one on relationships the
targets do not pin, a little further than sampling noise alone. The cells
where A differs most:

| relationship | V4 before | reweighted | redraw | A plain | A + final reweight |
|---|---:|---:|---:|---:|---:|
| any dividends, wages over $200,000, not joint | 60.3 | 64.4 | 63.1 | 62.4 | 61.8 |
| any capital gains, dividends over $100,000 | 88.2 | 91.0 | 90.4 | 89.6 | 89.7 |
| any dividends, no wages, not joint | 26.8 | 27.7 | 27.6 | 26.3 | 26.7 |
| any dividends, no wages, joint | 46.2 | 47.0 | 46.3 | 48.2 | 48.0 |
| no dependents, joint | 54.0 | 54.0 | 53.9 | 54.8 | 54.1 |

In the first two rows the fine-tune moves in the same direction as the tilt
but only part of the way. The fourth and fifth move where the tilt did not
move at all. Dependents are the clearest case: no target touches them and
the tilt leaves them unchanged (joint returns with no dependents 54.0%
before and after), but the fine-tune changes the dependents conditionals by
0.0025 nats (section 7) and that change makes held-out tilted records less
likely, not more. This is the network fitting the particular 380,000
records, not the tilt. The final reweight does not undo it: reweighting to
the targets hits the targets, and leaves whatever the network did elsewhere.

### 7. The move

Average log-probability ratio, fine-tuned network over V4, in nats per
record. The first row is the fine-tune's move; the third is the part of it
that points toward the tilted distribution.

| averaged over | A | A20 | B | C |
|---|---:|---:|---:|---:|
| draws from the fine-tuned network (= the move) | 0.0284 | 0.0311 | 0.0215 | 0.0248 |
| draws from V4 (the move measured the other way, sign reversed) | 0.0290 | 0.0324 | 0.0224 | 0.0254 |
| held-out tilted records | 0.0089 | 0.0077 | 0.0113 | 0.0107 |
| tilted records used in the fine-tune | 0.0210 | 0.0270 | 0.0233 | 0.0096 (not used by C) |

Standard errors: about 0.0005 for the first two rows, 0.0017 for the held-out
row. For reference, the tilt moved 0.0243 nats.

How to read the third row. Averaged over records from the tilted
distribution, log q_fine-tuned − log q_V4 equals the tilt's own move (at the
level of bins) minus the remaining distance from the tilted distribution to
the fine-tuned network. If the fine-tune had copied the tilt exactly, the
row would show the tilt's move at the level of bins, at most 0.0243. It
shows about 0.009 to 0.011. So the fine-tuned network has closed less than
half of the distance from V4 to the tilted distribution, measured in this
way, while moving as far from V4 as the tilt did (0.028 against 0.024). The
gap between the third and fourth rows is overfitting: on the records it
trained on the network has moved 0.021 nats toward them, on new records from
the same distribution 0.009. Twenty passes make this worse (0.027 against
0.008).

By part of the record, for A (draws from the fine-tuned network, then
held-out tilted records): incomes 0.0084 and 0.0035; net worth 0.0148 and
0.0087; married 0.0026 and −0.0008; dependents 0.0025 and −0.0025. Half the
move is in net worth, and the moves in married and dependents point away
from the tilted distribution.

Yet section 1 shows the fine-tune hits the targets much better than this
suggests. The targets are 144 sums; the relative entropy counts every
detail of the joint distribution. The network gets the sums mostly right
and adds detail that the tilt did not ask for.

**Effect on the original data.** Cross-entropy per hidden slot on the 2%
held-back slice of the original training records, before and after:

| | V4 | A | A20 | B | C |
|---|---:|---:|---:|---:|---:|
| 2015 PUF records | 1.6308 | 1.6340 | 1.6344 | 1.6322 | 1.6338 |
| 2016 SCF records | 1.5307 | 1.5414 | 1.5492 | 1.5340 | 1.5428 |

The PUF slice is expected to get a little worse: the network now describes
2019. The SCF slice getting worse is a side effect: only the net worth
question is trained under the SCF label, and the network's other SCF
answers drift. Replay (B) keeps both close to V4.

### 8. Variant C: fresh records, and the tilt as a formula

The overfitting in section 7 suggests the fine-tune needs more distinct
records than the 380,000 in the stack. The first attempt to supply them was
to treat the tilt as a formula: recover λ from the v4b weights (a least
squares fit of log(weight_after / weight_before) on the 144 target columns,
exact to 2 × 10⁻¹¹), draw 3.2 million new records from V4, and give each the
factor exp(λ·f(record)). **This does not work.** The tilt targets dollar
amounts, so f is linear in dollars inside the exponent, and the few new
records with extreme amounts in a cell whose λ is positive get enormous
factors. In the eight new files the effective number of records was between
0.0003% and 46% of the records, and the pooled estimate missed one IRS
cell by 37,600%. The tilt is a set of weights for the sample it was solved
on; outside that sample its formula has no reliable meaning
(`gen/D18/fresh/lambda_check_formula_transfer.csv`).

So each of the eight new files of 400,000 records was tilted on its own to
the same targets (136 exact, eight soft), from equal starting weights. Those
tilts moved between 0.0241 and 0.0252 nats each, with 95% of the effective sample kept, the same as the v4b tilt. Variant C fine-tunes on one file per pass, so no
record is seen twice.

Results for C, next to A:

| | A | C |
|---|---:|---:|
| IRS targets within 2 standard errors, plain file | 89 | 76 |
| median miss of the IRS targets, plain file (%) | 3.1 | 3.9 |
| total net worth, plain file, miss (%) | −1.5 | −5.7 |
| units above the SCF's 99th percentile, plain file, miss (%) | −1.0 | −7.7 |
| move (nats) | 0.0284 | 0.0248 |
| toward the tilt, held-out tilted records (nats) | 0.0089 | 0.0107 |
| toward the tilt, the other v4b stack records (nats) | 0.0210 (trained on) | 0.0096 (not trained on) |
| final reweight of its stack (nats) | 0.0046 | 0.0051 (Newton stopped 0.0004% short of exact) |

C does not overfit: it scores the same on records it never saw from either
source of tilted records (0.0107 and 0.0096). But it does not get closer to
the tilted distribution than A or B either; all four land between 0.008 and
0.011 nats. So the repeated records explain why A looks better on the
records it was trained on, not why every variant falls short of the tilt.
C is also weaker at the top of wealth, because its new files were drawn
without the stack's tenfold oversampling of the top, so it saw roughly a
tenth as many records in the top 1% of each variable.

What does limit the fine-tune is not settled by these runs. Three
candidates: the part of the tilt that depends on dollars within a bin,
which a network of bins cannot copy and whose size is not measured here;
the training settings (learning rate, passes, which questions are trained);
and how finely a network of this size can shift its conditionals.

## What it means

1. **Distillation works as far as the targets are concerned, and gets most
   of the way.** Sampled with equal weights, fine-tuned network A puts 89 of
   the 136 IRS targets within 2 standard errors (V4 before post-training:
   32; an equal-weight sample from the reweighted file: 133). The median
   miss falls from 9.4% to 3.1%, the holder shares close 67% to 93% of their
   gap, and the large wealth misses before post-training (the count above
   the 99th percentile, −21%; the count above $1 million, +18%) fall to
   about 1% and 4%.

2. **It is not exact, and a small final reweight is needed.** The largest
   remaining misses are in the bracket with no AGI (capital gains −76%,
   interest −56%) and among returns from $200,000 to $500,000 (wages about
   −6%). Tilting the fine-tuned network's stack to the same targets moves it
   0.0046 nats and keeps 99.3% of the effective sample, against 0.0243 nats
   for tilting V4's stack. So the fine-tune does about four fifths of the
   reweight's work, in this measure.

3. **It costs more movement than the tilt.** The fine-tuned network is
   0.025 to 0.031 nats from V4, about as far as the tilt, but less than half
   of that distance points toward the tilted distribution (0.009 to 0.011
   nats, measured on tilted records). If the network had copied the tilt,
   the two numbers would be equal. The rest of the move goes into things no
   target asked for, most visibly the married and dependents conditionals.
   Fine-tune plus final reweight costs roughly 0.033 nats in all (the sum
   of the two moves, an approximation), against 0.024 for tilting alone. In money terms this is small: the relationships
   no target covers differ from the reweighted file's by 0.48 points on
   average, against 0.34 points for a pure resampling of the reweighted file
   and 1.06 points for V4 before post-training.

4. **Training cost is negligible.** About 1.5 minutes for the fine-tune and
   20 seconds to generate, on one GPU, on top of the tilt it copies.

5. **Settings.** Eight passes beat twenty (more passes fit the particular
   records better and the targets worse). Replaying the original training
   records keeps the network's fit to the 2015 PUF and 2016 SCF nearly
   unchanged but pulls the 2019 file back toward 2015 (63 targets within 2
   standard errors, against 89). Fresh records each pass remove the
   overfitting but do not improve the targets. Further changes worth trying
   are listed below; none was tried.

6. **The tilt is a set of weights for one sample, not a formula.** Applying
   the solved λ to new records from the same model fails (section 8),
   because the factor is exponential in dollars. This matters for chaining
   years (decision 11): to start year t+1 from year t's post-trained model,
   that model has to exist as something that can be sampled, which is what
   distillation provides. On these results a chain would carry a fine-tuned
   network forward and finish each year with a small exact reweight.

7. **Judgment.** For a single year, tilting alone is better: it is exact, it
   moves less, and it is no harder to compute. Distillation earns its place
   only where a sampleable post-trained model is needed (chaining, or files
   larger than the stack), and then as fine-tune plus a final reweight of
   about 0.005 nats, never the fine-tuned network alone. This is consistent
   with decision 8 and with item 7 of the order of tests in
   `design/plan.md`; nothing here argues for changing either.

**Not tried, and the obvious next steps if this is pursued:** train only the
questions the sampler asks (block order) rather than every subset; add a
penalty on the move, the relative entropy to V4 on V4's own draws, so the
fine-tune stops changing conditionals the tilt left alone; measure how much
of the tilt's 0.0243 nats depends on dollars within bins, which sets the
best any network of bins can do; run several seeds, since each number here
is one run.
