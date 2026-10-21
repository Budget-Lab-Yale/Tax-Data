---
title: "Minimal example: post-training by adjusting the network's parameters (direct gradient), against reweighted v4b"
role: evidence
workstream: rebuild
status: current
updated: 2026-10-15
sot: research/rebuild/design/plan.md
supersedes: []
superseded_by: null
---

# Post-training by adjusting the network's parameters

The v4b baseline (`15_BASELINE_V4.md`, last section) post-trains by
reweighting. 400,000 records are drawn from the pretrained model V4. Each
record gets a weight that makes the file hit the 136 IRS targets for 2019
exactly and the eight wealth targets softly. The weights are chosen to stay
as close as possible to the starting weights, where closeness is measured by
relative entropy (the Kullback-Leibler divergence, KL, in nats). The
reweighting moves the file by 0.0243 nats.

This experiment changes the network instead. A copy of V4 is fine-tuned,
meaning its parameters are adjusted starting from V4's values, so that the
copy's own unweighted draws hit the same targets while its distribution
stays as close as possible to V4's. This is the "direct gradient" and
"policy gradient" rung of the fallback ladder in `design/plan.md`
(decision 8).

## Short answer

- Fine-tuning works partly. The adjusted network's own equal-weight draws
  hit 113 of the 136 IRS targets within 2 standard errors. V4 before
  adjustment hits 40, and the redraw from reweighted v4b hits 134. The
  wealth targets are within 6% (reweighted v4b: within 1.7%).
- It moves further for a worse fit. Adjusting every parameter moves
  0.0332 nats from V4. Adjusting only the last layer moves 0.0256 nats,
  the same as the exact reweighting of the same sample (0.0253), and fits
  as well as adjusting everything (112 of 136), but not exactly.
  Reweighting reaches an exact fit at a smaller or equal move.
- It changes things no target asks for. The share of returns with no
  dependents falls by 2 to 3 points. No target involves dependents, and
  the reweighting leaves them where V4 put them.
- It is tied to the fill order. Drawn in the order it was adjusted in, the
  network hits 113 targets within 2 standard errors. Drawn in a random
  order within the income block, it hits 82.
- It took four designs to find one that was stable. The reported runs
  took 14 and 22 minutes on one GPU, against seconds to two minutes on a
  CPU for the reweighting.
- A side finding about v4b itself: its reweighting sets the weight of every
  record with net worth between $927,884 and $991,815 (2015 dollars) to
  essentially zero. The reason is that two of the eight wealth targets
  contradict each other: they ask for more units above the higher line
  than above the lower one. Section 9 has the details.

My judgment: for one year's post-training, reweighting dominates. Keep
decision 8 as it is. A network adjusted by gradient is worth having only if
an unweighted model is needed for its own sake (for chaining years,
decision 11), and then distilling the tilt is the more natural route (the
sibling experiment, prefix 18_).

## 1. The design

**The model whose probabilities are adjusted.** A record's probability under
a network is the product of eight conditional probabilities in one fixed
fill order: wages, interest, dividends, capital gains, other_agi (block 1,
in this order), then net worth (block 2), then married, then dependents
(block 3). Net worth follows decision 16: it is drawn and scored under the
SCF label, with other_agi left out. The other seven slots use the PUF label,
with everything drawn before them visible. Records generated this way are
exactly the records `03_generate.generate` draws with `fixed_order=True` and
`borrowed={'label':'owner','foreign':'absent'}`. Two checks:

- A record's log-probability accumulated while drawing it equals the
  log-probability recomputed afterward, step by step, to within 5 × 10⁻⁷.
- The draws match those of `03_generate.py` to within sampling noise.

V4 under this order is the reference distribution. KL(adjusted ‖ V4) is the
average over the adjusted network's own draws of the log of the ratio of
the two probabilities. It is computed exactly record by record, so the move
is measured without approximation, up to sampling noise.

**Estimating the targets and their derivatives.** A target is an
expectation: the 2019 return count times the average contribution of a
record. A contribution is 1 or 0 for a count, or dollars for an amount,
with dollars drawn from the same rank-to-dollar mapping as in the tilt. The
estimates use a sample drawn from the current network, with the top 1% of
wages, capital gains, other_agi and net worth drawn five times as often as
the network would draw them. Expectations under the network being adjusted
are then estimated by importance weighting. Each sampled record is weighted
by its probability under the adjusted network divided by its probability
under the sampling distribution, and the weights are scaled to sum to one.

A record's probability is a product of the network's conditionals, so the
estimates can be differentiated with respect to the parameters. Following
the derivative through the network gives a policy gradient: a standard
estimate of how an expectation over a model's own draws changes when the
model's parameters change. It works by moving up the log-probability of
draws that score well and moving down that of draws that score badly, each
measured against the average draw. The sample is drawn fresh from the
current network at the start of each round. Within a round the importance
weights track the network's changes.

**Updates.** Each update uses the whole sample of 400,000 records. Adam,
the usual adaptive gradient method, makes the update. The learning rate is
the size of each parameter change. It starts at 2 × 10⁻⁵ and is capped at
10⁻⁴. An update is kept only if it lowers the objective as estimated on the
round's sample. Otherwise it is undone and the learning rate is halved. A
kept update raises the learning rate by a quarter. Each run has 8 rounds of
12 updates.

**The objective of the reported runs.** KL(adjusted ‖ V4), plus a penalty on
each target's miss:

- Each of the 136 IRS targets adds half the square of its miss measured in
  standard errors of an equal-weight file of 200,000 records drawn from V4.
  That is the yardstick of v4b's redraw. These standard errors run from
  0.3% of the target (joint returns, the middle brackets) to 45% (amounts
  in the zero-AGI and lowest brackets), with a median of 2.7%.
- Each of the eight wealth targets adds half the square of its relative
  miss over 2%, as in the tilt.

**Two variants.** All 609,278 parameters adjusted ("direct, all"), or only
the last of the four encoder layers, the final normalization and the output
heads, 198,526 parameters ("direct, last layer").

**Kept the same as v4b.** The 136 targets and their AGI brackets, the
threshold form of the eight wealth targets from all 2019 SCF units, the
return count, and the dollar mappings (map/V4, incomes grown to 2019, net
worth from the SCF 2015 mapping). The year token is 2015.

## 2. What ran, and the three designs that failed

| design | what it did | what happened |
|---|---|---|
| 1. Updates on stale coefficients | 8 updates per pass over the sample, with the coefficients fixed at the start of the pass; learning rate 10⁻⁴; multipliers from the exact tilt | Diverged within 8 updates: KL 0.16, wealth targets missed by about 50% |
| 2. The tilt's multipliers, controlled steps | Every update from the whole sample with the step control above. The objective was KL plus the tilt's own multipliers (from the exact tilt on the first sample) times the misses, corrected each round by one Newton step of the tilt's dual problem | The objective fell at every update, but the network went the wrong way: KL 0.19 and wealth targets missed by up to 58% after one round. The cause is in section 9: the tilt's solution empties a narrow net worth band, and with its multipliers fixed the network is pushed to cut that band's probability by hundreds of log units |
| 3. 1% penalty with multiplier updates | Multipliers start at zero; quadratic penalty on each relative miss (1% for the 136, 2% for wealth); after each round each exact target's multiplier rises by its miss over 1%² (an augmented Lagrangian) | Stalled: the root-mean-square miss on the 136 stayed at 12 to 15%, and the multipliers grew without limit (30,000 after six rounds). The zero-AGI and lowest-bracket amount cells have sampling errors of tens of percent even in 400,000 records, so the multipliers kept adding up noise |
| 4. Penalty in standard errors (reported) | As in section 1 | Stable. The objective fell from 478 to about 100 over eight rounds and levelled off there (section 3) |

All of this is in `19_direct.py` (`D19_METHOD` = `warm` for designs 1 and 2,
`penalty` for design 3, `z` for design 4). Slurm jobs: test 29705380,
V4 reference draws 29705516, design 1 29705517 and 29705518, design 2
29706014 and 29706015, design 3 29707009 and 29707010, design 4 29707949
(all parameters) and 29707950 (last layer), scoring 29709559. GPU time for
all jobs together: 93 minutes. The reported runs took 22.4 minutes (all
parameters, 11.7 seconds per update) and 14.0 minutes (last layer, 6.1
seconds per update) on one H200.

## 3. How the reported runs converged

The objective is half the sum of the squared misses in the units above,
plus KL. If the network's expectations were exactly on target, the noise in
estimating them from 400,000 records would still leave an objective of about
36. Each row below is the end of a round, estimated on that round's sample.
"Largest miss at the round's start" is measured on a fresh sample.

| round | objective | KL (all) | root-mean-square miss on the 136 (all) | largest miss at the round's start (all) | objective (last layer) | KL (last layer) |
|---:|---:|---:|---:|---:|---:|---:|
| 1 | 478 | 0.028 | 20.6% | — | 604 | 0.013 |
| 2 | 271 | 0.028 | 14.2% | 144% | 315 | 0.019 |
| 4 | 124 | 0.023 | 10.8% | 58% | 123 | 0.022 |
| 6 | 100 | 0.028 | 11.3% | 76% | 105 | 0.022 |
| 8 | 99 | 0.032 | 13.3% | 125% | 107 | 0.025 |

After round 4 the objective barely falls and KL keeps rising slowly. The
largest misses swing from round to round between 58% and 144%, always in
the same few thin cells (capital gains, interest and dividends of returns
with no AGI or AGI under $10,000, and the top bracket). They are mostly
sampling noise in those cells. 89 of the 96 updates were kept in the
all-parameter run and 94 of 96 in the last-layer run.

## 4. The move

| file | KL from V4 (nats) | fit |
|---|---:|---|
| v4b, reweighted (stack in random order) | 0.0243 | 136 exact on the stack; redraw 134 of 136 within 2 standard errors |
| exact tilt of the fixed-order V4 sample used here | 0.0253 | 136 exact on the sample |
| direct, all parameters | 0.0332 (standard error 0.0004) | 113 of 136 within 2 standard errors |
| direct, last layer | 0.0256 (standard error 0.0004) | 112 of 136 |

The KL of each adjusted network is measured on 400,000 fresh draws from it.
For the all-parameter network, the same quantity estimated on its first
sample by importance weighting is 0.0331, so the two measurements agree.

The reweighting is the exact smallest move that hits the targets. Both
networks move at least as much while missing more, so neither represents
the tilt cheaply. Adjusting every parameter moved 30% more than adjusting
only the last layer, with no better fit.

Part of the reweighting's 0.0243 is the empty net worth band of section 9:
removing a share π of the probability costs about π nats, and the band holds
0.57% of V4's records, so about 0.006 nats.

**Where the networks' move sits, by step of the fill order** (nats; each
entry is the average log change of that step's conditional probability over
the network's own draws, and the entries sum to the total):

| step | direct, all | direct, last layer |
|---|---:|---:|
| wages | 0.0015 | 0.0019 |
| interest | 0.0036 | 0.0034 |
| dividends | 0.0009 | 0.0006 |
| capital gains | 0.0040 | 0.0035 |
| other_agi | 0.0041 | 0.0029 |
| net worth | 0.0137 | 0.0087 |
| married | 0.0018 | 0.0018 |
| dependents | 0.0037 | 0.0028 |
| total | 0.0332 | 0.0256 |

Dependents are drawn last and no target involves them. So the smallest move
that hits the targets leaves the dependents step exactly as V4 has it, and
that step's 0.0028 to 0.0037 nats is pure excess. It is a side effect of
changing parameters that the dependents step shares with the steps that do
need to change.

## 5. The 136 IRS targets

Equal-weight files. A target is "within 2 standard errors" if its miss is
within twice the sampling error of a file that size.

| file | within 2 standard errors | largest \|z\| | root-mean-square miss | the three largest misses in standard errors |
|---|---:|---:|---:|---|
| v4b reweighted, redraw of 200,000 | 134 of 136 | 2.87 | 11.0% | capital gains $75,000 to $100,000 (+15%, z 2.9); joint returns (−0.7%, z −2.1); wages, no AGI (+34%, z 1.8) |
| V4, fixed order, 200,000 | 40 of 136 | 18.9 | 21.0% | joint returns (+5.9%, z 18.9); capital gains $200,000 to $500,000 (−39%, z −15.1); interest count $200,000 to $500,000 (−15%, z −13.9) |
| direct, all, 200,000 | 113 of 136 | 13.9 | 18.6% | capital gains, no AGI (−79%, z −13.9); wages $1 to $5,000 (−7.2%, z −4.4); interest, no AGI (−56%, z −3.7) |
| direct, all, 400,000 | 105 of 136 | 7.0 | 11.2% | interest, no AGI (−61%, z −7.0); wages $1 to $5,000 (−7.1%, z −6.3); capital gains count, no AGI (−15%, z −5.9) |
| direct, last layer, 200,000 | 112 of 136 | 14.8 | 18.4% | capital gains, no AGI (−81%, z −14.8); wages $1 to $5,000 (−6.8%, z −4.1); interest, no AGI (−58%, z −3.8) |
| direct, last layer, 400,000 | 110 of 136 | 6.0 | 11.4% | wages $1 to $5,000 (−6.8%, z −6.0); capital gains count, no AGI (−15%, z −5.8); capital gains, no AGI (−60%, z −4.3) |

The joint-returns count and the count and amount cells of the middle and
upper brackets, which V4 missed by 5 to 40%, are brought within a few standard errors. The remaining misses
are concentrated in returns with no AGI and in the $1 to $5,000 bracket. The
reweighting can hit those cells exactly on its own records by moving a few
heavy records. A network has to change the conditional distribution of the
parts that make AGI small, and it did not manage to.

## 6. The eight wealth targets (miss, %)

| target | value | v4b, before | v4b, reweighted | V4, fixed order | direct, all | direct, last layer |
|---|---:|---:|---:|---:|---:|---:|
| units above the SCF 99th percentile | 1.58 million | −21.3 | −0.0 | −19.8 | −1.2 | +2.8 |
| net worth above the 99th percentile | $34.2 trillion | −13.4 | +0.0 | −14.6 | −1.0 | +2.5 |
| units above the SCF 90th percentile | 15.8 million | +9.7 | −1.7 | +10.2 | −5.9 | −4.0 |
| net worth above the 90th percentile | $69.3 trillion | −3.0 | −0.0 | −3.4 | −2.8 | −0.9 |
| units above the SCF median | 78.9 million | −2.5 | −0.0 | −2.6 | +0.2 | −0.9 |
| net worth above the median | $88.2 trillion | −1.1 | +0.0 | −1.5 | −0.7 | +1.2 |
| total net worth | $89.1 trillion | −1.1 | −0.0 | −1.5 | −0.7 | +1.2 |
| units above $1 million (2019 dollars) | 15.3 million | +19.3 | +1.7 | +19.7 | +1.4 | +3.8 |

Each target has a standard deviation of 2%. The networks miss "units above
the 90th percentile" by 2 to 3 standard deviations; the reweighting misses
it by 0.85.

## 7. Wealth against the SCF

All net worth in 2015 dollars; 157.8 million units in every generated file.

| | SCF 2019, all units | v4b, before | v4b, reweighted | V4, fixed order | direct, all | direct, last layer |
|---|---:|---:|---:|---:|---:|---:|
| total ($ trillion) | 89.1 | 88.2 | 89.1 | 87.9 | 88.5 | 90.2 |
| mean | $623,000 | $559,000 | $565,000 | $557,000 | $561,000 | $572,000 |
| median | $84,500 | $78,500 | $84,500 | $78,400 | $85,000 | $82,100 |
| share negative | 10.7% | 12.2% | 11.8% | 12.2% | 12.1% | 11.9% |
| top 10% share | 77.7% | 74.4% | 78.0% | 74.3% | 77.1% | 76.8% |
| top 1% share | 38.4% | 37.0% | 38.4% | 36.4% | 38.5% | 38.4% |
| top 0.1% share | 14.7% | 14.6% | 13.2% | 13.9% | 13.6% | 13.7% |
| 90th percentile | $992,000 | $1,080,000 | $902,000 | $1,086,000 | $898,000 | $934,000 |
| 99th percentile | $9,400,000 | $8,020,000 | $9,390,000 | $8,240,000 | $9,300,000 | $9,530,000 |
| 99.9th percentile | $39,100,000 | $36,900,000 | $38,900,000 | $36,800,000 | $37,900,000 | $38,200,000 |

On these statistics the networks and the reweighting are about equally
close to the SCF. The networks are a little closer on the top 0.1% share
(13.6 to 13.7% against 13.2%; SCF 14.7%).

**Mean net worth by wage group**

| wages | SCF 2019 | v4b, before | v4b, reweighted | direct, all | direct, last layer |
|---|---:|---:|---:|---:|---:|
| 0 | 706,000 | 1,188,000 | 1,187,000 | 1,229,000 | 1,254,000 |
| 1-25k | 203,000 | 206,000 | 200,000 | 189,000 | 192,000 |
| 25-50k | 148,000 | 207,000 | 190,000 | 189,000 | 195,000 |
| 50-100k | 333,000 | 403,000 | 372,000 | 365,000 | 370,000 |
| 100-200k | 922,000 | 887,000 | 844,000 | 839,000 | 846,000 |
| 200k+ | 4,495,000 | 3,713,000 | 4,044,000 | 3,862,000 | 3,963,000 |

**Median net worth by wage group**

| wages | SCF 2019 | v4b, before | v4b, reweighted | direct, all | direct, last layer |
|---|---:|---:|---:|---:|---:|
| 0 | 109,000 | 262,000 | 232,000 | 250,000 | 258,000 |
| 1-25k | 11,300 | 8,330 | 8,710 | 9,790 | 8,790 |
| 25-50k | 26,000 | 36,100 | 38,300 | 38,800 | 37,600 |
| 50-100k | 109,000 | 144,000 | 135,000 | 131,000 | 136,000 |
| 100-200k | 362,000 | 387,000 | 316,000 | 332,000 | 355,000 |
| 200k+ | 1,679,000 | 1,399,000 | 1,427,000 | 1,324,000 | 1,339,000 |

**Share of units with net worth above $1 million**

| wages | SCF 2019 | v4b, before | v4b, reweighted | direct, all | direct, last layer |
|---|---:|---:|---:|---:|---:|
| 0 | 10.9% | 23.3% | 21.1% | 20.4% | 20.9% |
| 1-25k | 3.7% | 3.7% | 3.1% | 2.9% | 3.0% |
| 25-50k | 2.1% | 3.3% | 2.6% | 2.5% | 2.6% |
| 50-100k | 4.5% | 7.4% | 5.9% | 5.7% | 5.8% |
| 100-200k | 18.9% | 22.4% | 19.3% | 18.4% | 18.3% |
| 200k+ | 66.7% | 64.7% | 63.3% | 60.9% | 61.0% |

The pattern by wage group is the same as the reweighting's, with the same
gaps to the SCF. Returns with no wages hold too much wealth: a mean of $1.23
million against the SCF's $706,000, slightly worse than the reweighting's
$1.19 million. Earners above $200,000 hold too little: $3.86 million
against $4.04 million after reweighting and $4.50 million in the SCF.

## 8. Holder shares and the relationships no target covers

**Holder shares (% of returns)**

| item | v4b, before | v4b, reweighted | V4, fixed order | direct, all | direct, last layer | IRS 2019 |
|---|---:|---:|---:|---:|---:|---:|
| interest | 27.4 | 30.8 | 27.4 | 30.9 | 30.8 | 30.8 |
| dividends | 17.6 | 18.4 | 17.8 | 18.2 | 18.3 | 18.4 |
| capital gains | 12.8 | 13.8 | 12.7 | 13.7 | 13.7 | 13.8 |

**Untargeted relationships (% of returns)**

"Actual 2015 PUF" is the real PUF with its weights and its dollars grown to
2019. The change from V4 is measured against V4 in the same fill order: the
v4b stack for the reweighting, V4 in the fixed order for the networks.

| relationship | group | actual 2015 PUF | v4b, before | v4b, reweighted | V4, fixed order | direct, all | direct, last layer |
|---|---|---:|---:|---:|---:|---:|---:|
| any dividends | no wages, not joint | 31.1 | 26.8 | 27.7 | 27.2 | 28.4 | 28.3 |
| any dividends | wages 200k+, joint | 61.1 | 57.5 | 62.5 | 58.6 | 60.7 | 61.1 |
| any dividends | wages 200k+, not joint | 57.7 | 60.3 | 64.4 | 55.9 | 59.1 | 59.0 |
| any capital gains | dividends 1k-10k | 67.5 | 68.2 | 70.4 | 67.1 | 71.2 | 70.5 |
| any capital gains | dividends 100k+ | 93.5 | 88.2 | 91.0 | 90.4 | 92.4 | 92.4 |
| no dependents | joint | 49.7 | 54.0 | 54.0 | 54.0 | 51.5 | 52.4 |
| no dependents | not joint | 73.3 | 77.5 | 77.5 | 77.8 | 74.7 | 76.1 |
| 1 dependent | joint | 18.1 | 17.8 | 17.7 | 17.7 | 19.9 | 20.1 |
| 1 dependent | not joint | 15.2 | 13.8 | 13.8 | 13.7 | 16.3 | 16.0 |
| 2 dependents | joint | 19.8 | 17.9 | 18.0 | 18.0 | 17.4 | 16.5 |
| 2 dependents | not joint | 8.2 | 5.9 | 5.8 | 5.7 | 6.1 | 5.3 |

Across all 25 rows the average change is 1.41 points (all parameters) and
1.44 points (last layer), against 1.06 for the reweighting. The largest
changes are 4.2, 3.4 and 5.0 points. The difference is where the changes
fall. The reweighting leaves the dependents rows exactly where V4 put them
(54.0 before and after). The networks move them by 1 to 3 points, in the
same direction in both runs. Some of that is toward the 2015 PUF (no
dependents) and some past it (one dependent). No target asked for it.

**Where the change goes, record by record.** On the first-round sample of
V4 records, the per-record log change of the all-parameter network has a
correlation of 0.70 with the per-record log change of the exact tilt (0.68
for the last layer). The 2,243 records the tilt removes (section 9) are
left out of this correlation. By wage group, the networks shift the shares
of units as the tilt does to within 0.3 points. They raise the mean net
worth of earners above $200,000 by less: to $3.82 million from V4's $3.70
million, against $4.03 million for the tilt.

**The fill order.** V4's joint distribution itself depends on the order.
Drawn in the fixed order, V4 is 0.0305 nats (standard error 0.0004) from
the same network scored in the order with block 1 reversed. That is more
than the whole reweighting move. After adjustment the gap is 0.038 (all)
and 0.036 (last layer). The adjustment holds only partly in another order:

| file, 200,000 records | within 2 standard errors |
|---|---:|
| V4, fixed order | 40 of 136 |
| V4, random order inside block 1 | 41 of 136 |
| direct, all, fixed order (as adjusted) | 113 of 136 |
| direct, all, random order inside block 1 | 82 of 136 |
| direct, last layer, random order inside block 1 | 84 of 136 |

So the fixed order matters for this method in a way it does not for
reweighting. The adjusted network is an adjusted version of one order's
distribution, and it must be drawn in that order. Reweighting adjusts
whatever records were drawn, so the order enters only through the
pretrained file.

## 9. A finding about v4b: two wealth targets contradict each other, and the tilt empties a band of net worth

Two of the eight wealth targets draw lines close together:

- units above $1 million in 2019 dollars, which is $927,884 in 2015
  dollars;
- units above the SCF's 90th percentile, $991,815.

The $1 million line is the lower of the two, so any file has at least as
many units above it as above the 90th percentile. The targets ask for the
opposite: 15.25 million units above $927,884 (the SCF's own count of units)
and 15.77 million above $991,815 (10% of the IRS's 157.8 million returns).
No distribution can meet both. The closest any set of weights can come is
to put nobody between the two lines, so that the two counts are equal, and
to settle between the targets. That is what the reweighting does. After the
tilt both counts are exactly 15,503,484. In v4b's reweighted file, every
one of the 2,268 records with net worth between $927,884 and $991,815 has a
weight factor below 10⁻¹⁸⁵. Before the tilt those records held 0.57% of the
weight; the SCF puts 0.66% of units in that band; after the tilt the band
is empty. The same happens to all 2,243 such records in the fixed-order
sample used here, and to no other records. The networks put 0.39% (all)
and 0.45% (last layer) of their draws in the band.

This is the "units above $1 million +1.7%, units above the 90th percentile
−1.7%" pair already noted in `15_BASELINE_V4.md`. It costs the reweighting
about 0.006 nats, so it is cheap for weights. It is also what made design 2
fail, and it is a hole in the published v4b file's net worth distribution.
Changing the target sheet (for example, dropping one of the two lines, or
building both from the same SCF counts so they agree) is John's call.

## 10. What the direct gradient adds and what it costs

**What it adds**

- An unweighted model whose own draws are post-trained: any number of
  equal-weight records, with no stack and no weights. That is the object
  decision 11 (chaining years) would start the next year from.
- An exact, step-by-step account of where the move sits (section 4), which
  weights do not give.

**What it costs**

- Accuracy. 113 of 136 targets within 2 standard errors, against 134 for
  the reweighting's redraw. The misses are concentrated in returns with no
  AGI and AGI under $5,000. The wealth targets are within 6% instead of
  1.7%.
- A larger move for a worse fit: 0.0332 nats with every parameter, 0.0256
  with the last layer, against the exact minimum of 0.0253 on the same
  sample (0.0243 for v4b's stack). The parameters do not represent the
  tilt cheaply.
- Changes to relationships no target covers. The dependents distribution
  moves by up to 3 points because the steps share parameters; the
  reweighting does not move it at all.
- Dependence on the fill order (section 8).
- Stability and tuning. Three designs failed before the fourth worked:
  stale coefficients diverged, the tilt's own multipliers drove the
  network toward the empty band, and multiplier updates added up sampling
  noise. The working version needs the per-update step control. It
  levelled off after about four rounds, above the noise floor.
- Compute: 14 to 22 GPU minutes per run, against seconds to two minutes on
  a CPU for the tilt. Most of the cost is going through the network eight
  times per record, once per conditional, at every update.
- The thin cells are the limit. A target can only be checked to within the
  sampling error of a fresh file, so the network can at best be "exact in
  expectation" to within that error. The reweighting is exact on its own
  records, and its redraw shows the same sampling error.

## Files

Code (new; nothing existing was changed):

- `19_direct.py`: the fill-order probabilities, the sampler, the
  importance-weighted objective, training, and the final draws.
  `test` checks the sampler; `pretrain` draws V4's reference files and
  measures the order gap; `train <tag>` runs one adjustment.
- `19_score.py`: all the tables above, against reweighted v4b.
- `19_direct.sbatch` (GPU) and `19_cpu.sbatch` (CPU).

Outputs under `/nfs/roberts/scratch/pi_nrs36/jar335/minimal/`:

- `model/G19/all_z/model.pt`, `model/G19/last_z/model.pt`: the adjusted
  networks, loadable with `02_model.load_model`.
- `gen/G19/<tag>/final_fixed.parquet` (400,000 fresh draws in the fixed
  order), `final_random.parquet` (200,000 in random order),
  `logratio_final.parquet` (per record and step, log-probability under the
  network and under V4); `gen/G19/pretrain_fixed.parquet`,
  `pretrain_random.parquet`, `pretrain_meta.json` (V4's own draws and the
  order gap).
- `tilt/G19/<tag>/`: `train_log.csv` (one row per update), `rounds.csv`,
  `tolerances.csv` (each target's standard error), `meta.json` (move, move
  by step, times), the first-round sample with the exact tilt's weights.
  `tilt/G19/all/` and `tilt/G19/last/` hold the logs of design 3.
- `checks/G19/scores.md` and its CSVs (all tables in sections 4 to 8);
  `checks/G19/19_deleted_records.py` and `19_hole_check.py` with their
  outputs (section 9).
- Slurm logs: `logs/19_*`. Markers: `markers/STEP19_*`.
