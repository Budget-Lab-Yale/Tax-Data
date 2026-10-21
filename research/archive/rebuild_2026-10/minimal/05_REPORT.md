---
title: "Minimal example, step 5: the tilt"
role: evidence
workstream: rebuild
status: current
updated: 2026-10-14
sot: research/rebuild/design/plan.md
supersedes: []
superseded_by: null
---

# Minimal example, step 5: the tilt

## Note of 2026-10-14 (second round): which SOI row the capital gains target uses

**Finding: the target should use Schedule D net gain minus Schedule D net
loss, not the 1040 capital gain line.** The code now does this
(`soi_cells.py`); the first-round numbers below used the 1040 line.

- The PUF booklet labels E01000 "Net capital gain or loss" and keeps
  E01100, "Capital gain distributions reported on Form 1040", as a separate
  field. `src/process_puf.R` also treats them as separate parts of AGI. The
  booklet's own comparison table puts SOI's figure for E01000 at $694.95
  billion. That equals Schedule D net gain ($713.60bn) minus net loss
  ($18.65bn). It does not include the $11.56bn of distributions.
- Bracket by bracket (`05_check_capital_gains.py`, job 29589668), comparing
  the real PUF's E01000 with the Schedule D figure closes the gap from
  $50,000 to $10 million: every bracket is within 3%. Against the 1040 line
  the gaps were 8–12% from $50,000 to $200,000. Below $50,000 the amounts
  are small (under $3.5 billion per bracket), and the gaps go both ways
  (−50% to +112%; in the $1–5,000 bracket both figures are net losses).
- The PUF's E01100 lines up with SOI's distributions without Schedule D:
  $11.8 billion against $11.6 billion in total, and within about 20% in
  each bracket, in both amount and count.
- The four dropped aggregate records do not explain anything below $200,000.
  Each one sits above $2 million of AGI or below zero. Adding them back
  closes the $10M+ gap (−33% to −0.3%). It makes the no-AGI bracket worse
  (+7% to +23%), because one aggregate record has negative AGI and $18
  million of capital gains.
- Knock-on change: the 2019 growth factor for `other_agi` now treats AGI
  minus the four items with capital gains as the Schedule D net, so the
  distributions sit in `other_agi`, as they do on the PUF.

## Second-round results, 2026-10-14

**What ran.** Steps 4 and 5 again, on the rebanded table (10 negative bands)
and the regenerated `gen_puf.parquet`, with the Schedule D capital gains
target. Step 5 is job 29591191. It is a rerun of job 29589992 with one
change: the redraw's z-score is left blank for the return count, which every
record counts toward equally. Both jobs gave the same numbers. The
first-round outputs are kept in `tilt/v1/`. The sections after this one are
the first round.

New in this round: a **pre-scaled start** for A1 and B1. Before tilting,
the starting weights are scaled within joint and non-joint returns. The scale
makes each group's count equal SOI's 2019 count (Table 1.2: 54.8 million
joint, 103.0 million non-joint). The tilt then runs to the same 136 targets.
The move is measured from the starting weights, so whatever goes into them
is not counted as part of the move. In the real version the groups would be
age by filing status, from the population projections.

| Run | Move, first round (nats) | Move, second round (nats) | Effective sample size ratio, first round | Second round |
|---|---|---|---|---|
| A0: real PUF, 10 totals | 0.0029 | 0.0029 | 0.996 | 0.996 |
| A1: real PUF, 136 by bracket | 0.0061 | **0.0059** | 0.992 | **0.992** |
| A1, pre-scaled start | | **0.0056** | | **0.996** |
| B0: generated, 10 totals | 0.0064 | 0.0028 | 0.987 | 0.994 |
| B1: generated, 136 by bracket | 0.0206 | **0.0072** | 0.865 | **0.984** |
| B1, pre-scaled start | | **0.0070** | | **0.985** |

- The pre-scaling's own move is 0.0004 nats for A and 0.0002 for B. Pre-scaling
  plus tilt comes to the plain run's move (0.0059 and 0.0072, measured
  from the plain start to the end). It must:
  both the joint indicator and the constant are already among the tilt's
  functions (joint returns is a target, and the brackets' return counts add
  up to all returns). So the scaling is a tilt of the same form, and the
  final weights are the same as the plain run's. Only the accounting moves.
  On that accounting the filing-status change is a small part of the move:
  about 7% for the real PUF and 3% for the generated file. With age groups
  that are not targets, the final weights would differ.
- B1 is the main change. The generated file now costs about as much as the
  real PUF (0.0072 against 0.0059 nats). The effective sample size ratio is
  0.984, up from 0.865. Weight factors run from 0.006 to 10, against nearly
  0 to 131 in the first round. Four records fall below one tenth and one goes
  above ten.
- Where B1 still starts far off: capital gains amounts (median bracket −30%,
  as for the real PUF), interest (−12%), and the no-AGI bracket (wages count
  +14% and amount +18%, down from +49% and +77%). The tilt pulls down
  generated $1 million+ records that have only wages (factor about 0.22). It
  raises records whose wages are offset by other losses into brackets of
  $10,000 to $35,000 (factors 3 to 8).
- A1 barely changed with the Schedule D target (0.0061 to 0.0059). Its
  largest factors still go to real records with large capital gains offset
  by `other_agi` losses (now 0.000005 to 216). The capital gains amount gap
  that remains (median −24% across brackets) comes from growth to 2019:
  returns with a gain rose 19% from 2015, and the map grows only the amount
  per gain return (×1.02). The missing aggregate records add to it at the top.
- The redraw of 200,000 equal-weight records: B0 is within 2 standard errors
  on all 9 targets that have sampling error (the return count is exact by
  construction). B1 is within 2 standard errors on 125 of 136, with a largest
  miss of 2.7 standard errors. The largest percent miss is interest in the
  $1 million+ bracket (+51%), which a few records carry.
- Untargeted relationships: the new generated file starts closer to the real
  PUF. Dependents by filing status now match within 1 point. Two cells stand
  out. Any dividends among joint filers with wages over $200,000 is 58.0%
  generated against 61.1% real, and 61.0% after B1. Among non-joint filers in
  that wage group it is 61.2% against 57.7%, and 63.9% after B1. Any capital
  gains among those with dividends over $100,000 is 88.0% against 93.5%, and
  90.5% after B1. Everything else after B1 is within 2.5 points of the real
  file.

## Bottom line

- On the real PUF records, grown to 2019 dollars, the tilt hits every 2019
  target exactly and moves the file very little. With 10 totals the move is
  0.003 nats. With 136 targets by AGI bracket it is 0.006 nats. The plan's
  threshold is about 1.3 nats. The effective sample size falls by 0.4% and
  0.8%.
- The brackets cost little on average but a lot for a few records. With
  totals only, weights change by factors between 0.25 and 16. With brackets,
  between 0.000006 and 164: 28 records go up more than tenfold and 24 go down
  more than tenfold. Almost all are records with large capital gains offset
  by large losses in `other_agi`, sitting in middle AGI brackets. They are
  being used to fill the capital gains amount targets in brackets under
  $200,000, where the PUF falls short of SOI even in 2015 (step 4 report,
  section 5).
- The untargeted relationships barely move: under 2 percentage points
  everywhere, mostly under 1. (The share with dividends in the no-AGI
  bracket moves 4 points, but the brackets target it.)
- On the generated 2015 file (step 3, 200,000 records, dollars from the 2019
  map), the tilt also hits every target. The move is 0.006 nats with totals
  and 0.021 nats with brackets. The effective sample size ratio is 0.987 and
  0.865. That is two to three and a half times the real-data move, and far below
  1.3 nats. The brackets cost more here because the generated file has too
  many no-AGI returns with wages, and the tilt removes them (some records'
  weights go to nearly zero).
- The redrawn equal-weight file of 200,000 hits the targets within sampling
  error: all 10 totals within 2 standard errors, and 125 of the 136 bracket
  targets. Dollar amounts in thin cells have large sampling errors.

## 1. What ran

Slurm job 29588313 (3 minutes) ran `05_tilt.py` through `05_run.sbatch`,
after step 4, with `GEN_FILE` set to
`/nfs/roberts/scratch/pi_nrs36/jar335/minimal/gen/gen_puf.parquet`. Run A's
numbers are the same as in the earlier job 29587449, which ran before the
generated file existed. Log:
`/nfs/roberts/scratch/pi_nrs36/jar335/minimal/logs/05_run_29588313.out`.
Outputs in `/nfs/roberts/scratch/pi_nrs36/jar335/minimal/tilt/`:
`summary.csv` (one row per run), and per run `fit_<run>.csv` (every target:
published, before, after), `untargeted_<run>.csv`, `weights_<run>.parquet`
(record_id, weight before and after) and `extremes_<run>.csv` (the 15
records with the largest and the 15 with the smallest weight factors). Run B
also writes `redraw_<run>.csv`.

## 2. The tilt

Each record's weight w becomes w × exp(λ₁f₁ + … + λₖfₖ), where fₖ is the
record's contribution to target k: 1 or 0 for a count, dollars for an
amount. The λ's are solved so that every weighted total equals its target.
This is the smallest change to the weights that hits the targets, measuring
change as Kullback-Leibler divergence. It is computed by Newton's method on
the dual problem, which is convex and has one λ per target.

How the move is measured:

- **Move in nats:** Σ p_new × log(p_new / p_old), with p each record's
  weight as a share of the total. It is the average log change in a
  record's weight, weighted by the new weights. Zero means no change.
- **Effective sample size:** (Σw)² / Σw², the number of equally weighted
  records that would give the same precision. Reported as after over before.

The code allows a tolerance per target, so that a target can be soft. In
these runs every target is exact.

## 3. Targets

All from SOI Pub 1304 for 2019, read from `soi_2019_targets.csv`. Totals are
the sums of the 19 published AGI brackets. They differ from the published
total row by SOI's rounding of one or two units.

**Totals (runs A0, B0), 10 targets:**

| Target | Value | Source |
|---|---|---|
| Returns | 157,796,805 | Table 1.4 |
| Joint returns | 54,796,116 | Table 1.2; includes surviving spouses. PUF side: married = filing status 2 |
| Returns with wages / amount | 129,775,753 / $8,273.1bn | Table 1.4 |
| Returns with taxable interest / amount | 48,676,014 / $153.3bn | Table 1.4, taxable interest (the variable is now taxable interest only) |
| Returns with ordinary dividends / amount | 28,980,260 / $331.8bn | Table 1.4 |
| Returns with capital gains / amount | 21,705,172 / $864.7bn | Count: returns with a Schedule D net gain (13.90m) plus returns with a Schedule D net loss (7.81m). Amount: the 1040 capital gain line (net gain + distributions reported on the 1040 − deducted loss) |

For capital gains, the count leaves out the 4.06 million returns that only
report distributions on the 1040. On 2015 data that definition matches the
PUF's count: 19.95 million in SOI, 20.06 million on the PUF.

**By AGI bracket (runs A1, B1), 136 targets:** for each bracket, the number
of returns, and for each of the four items the number of returns with it and
its amount. Plus joint returns in total. SOI publishes 19 brackets. The top
five ($1 million and up) are merged into one. The rule: merge from the top
until the top bracket would hold at least 500 records in a file of 200,000
equally weighted records. Merged, it holds about 700. That leaves 15
brackets × 9 + 1 = 136 targets. A record's bracket comes from its derived
AGI, the sum of its five amounts in 2019 dollars.

## 4. Run A: the real PUF records

Input: all 207,692 PUF records with their survey weights, without the four
aggregate records. Each amount is grown to 2019 by the step 4 factor (wages
×1.117, interest ×1.400, dividends ×1.214, positive capital gains ×1.024,
`other_agi` ×1.098).

**How far from the targets it starts:**

| | A0 (totals) | A1 (median gap across brackets) | A1 (largest gap) |
|---|---|---|---|
| Returns | −4.4% | −6.1% | no AGI: −11% |
| Wages count / amount | −3.7% / −3.6% | −5.1% / −3.7% | no AGI: −27% / −32% |
| Interest count / amount | −12% / −19% | −12% / −15% | $1M+ amount: −28% |
| Dividends count / amount | −4.4% / −12% | −2.5% / −2.7% | $10–15k amount: +21% |
| Capital gains count / amount | −7.4% / −27% | −5.9% / −27% | $1–5k amount: PUF negative, target positive |
| Joint returns | −0.8% | (total only) | |

The largest starting gaps are interest and capital gains. Two reasons. The
map grows only the amount per return that has the item; the growth in how
many returns have it is left to the tilt. And the PUF lacks the four
aggregate records, which hold a large share of top capital gains.

**Result:**

| | A0 | A1 |
|---|---|---|
| Targets | 10 | 136 |
| Largest miss after the tilt | 0.000000001% | 0.000001% |
| Newton steps | 6 | 199 (the limit; within tolerance) |
| Move | **0.0029 nats** | **0.0061 nats** |
| Effective sample size, after / before | **0.996** | **0.992** |
| Weight factor, smallest / largest | 0.25 / 16.4 | 0.000006 / 164 |
| Weight factor, 1st / 50th / 99th percentile | 0.87 / 1.05 / 1.53 | 0.72 / 1.07 / 1.58 |
| Records with factor above 10 / below 0.1 | 3 / 0 | 28 / 24 |

**Who moves most.**

- A0: the smallest factors (0.25–0.34) go to records with $100 million or
  more of wages. The largest (6.5–16) go to records with very large capital
  gains or interest. This is the tilt making up for the missing aggregate
  records by making the PUF's largest gains more common.
- A1: the extremes are almost all records with millions in capital gains
  offset by millions of `other_agi` losses, which puts them in a middle AGI
  bracket. Example: $8.4 million of gains, −$8.6 million of other income,
  AGI $68,000, factor 91. Records with large dividends offset the same way
  are pushed toward zero. All of these records start with weights between 1
  and 41, which is why the move in nats stays tiny.
- A1 needed the full 199 Newton steps for the same reason. The convergence
  test (every target within one part in ten million) was met.

**Untargeted relationships**, share of returns, before → after:

Any dividends, by wages and filing status:

| Wages (2019 $) | Joint, A0 | Joint, A1 | Not joint, A0 | Not joint, A1 |
|---|---|---|---|---|
| 0 | 43.4 → 43.3 | 43.4 → 43.7 | 31.1 → 31.3 | 31.1 → 30.7 |
| 1–25k | 22.9 → 23.3 | 22.9 → 23.3 | 5.2 → 5.2 | 5.2 → 5.1 |
| 25–50k | 15.3 → 15.5 | 15.3 → 15.0 | 5.9 → 5.9 | 5.9 → 5.7 |
| 50–100k | 18.9 → 18.8 | 18.9 → 18.7 | 13.2 → 13.1 | 13.2 → 12.9 |
| 100–200k | 30.3 → 29.9 | 30.3 → 30.9 | 28.8 → 28.7 | 28.8 → 29.5 |
| 200k+ | 61.1 → 60.5 | 61.1 → 62.8 | 57.7 → 57.2 | 57.7 → 59.4 |

Any capital gains, by dividends: no dividends 3.7 → 3.9 (A0) and 3.9 (A1);
$1–1k 38.7 → 39.5 and 39.6; $1k–10k 67.5 → 68.3 and 68.5; $10k–100k
87.0 → 87.4 and 87.9; over $100k 93.5 → 93.6 and 94.2.

Dependents, by filing status (0 / 1 / 2 / 3+):

| | Before | A0 after | A1 after |
|---|---|---|---|
| Joint | 49.7 / 18.1 / 19.8 / 12.3 | 50.3 / 18.0 / 19.6 / 12.1 | 50.4 / 17.9 / 19.6 / 12.1 |
| Not joint | 73.3 / 15.2 / 8.2 / 3.4 | 73.6 / 15.0 / 8.0 / 3.3 | 73.6 / 15.1 / 8.0 / 3.4 |

Any dividends, by AGI bracket (A1). This is partly targeted: A1 targets both
the number of returns and the number with dividends in each bracket, so the
"after" share is SOI's 2019 share. It moves by less than 1 point in most
brackets, by about 2 points from $200,000 to $500,000 (59.0 → 60.9), and
most in the no-AGI bracket (23.8 → 19.4).

## 5. What this says

- On real records, the 2019 targets are close to the grown 2015 file. The
  tilt has little to do: a few percent on returns and wages, 10–25% on
  interest and capital gains.
- The bracket targets are nearly free on average, but they show where the
  data and SOI disagree: capital gains amounts in the lower and middle
  brackets. The tilt fills them by leaning on odd records with offsetting
  gains and losses. If the guess in the step 4 report is right (the PUF's
  capital gains leave out 1040-line distributions), that target should drop
  the distributions. The tilt would then have much less to do there.
## 6. Run B: the generated records

Input: `gen_puf.parquet`, 200,000 PUF-labelled records from step 3. Dollars
are drawn from the 2019 PUF map (step 4). Every record starts with the same
weight, 157.8 million / 200,000 = 789.

**How far from the targets it starts.** With totals: returns are exact by
construction and the wage count is within 0.1%; interest is 13% short in both count and
amount; capital gains are 1.4% short in count and 26% short in amount;
joint returns 2.2% over. By bracket (median gap, worst bracket):

| | Median | Worst |
|---|---|---|
| Returns | +1.2% | no AGI: +25% |
| Wages count / amount | +1.0% / −1.2% | no AGI: +49% / +77% |
| Interest count / amount | −12% / −15% | no AGI amount: −90% |
| Dividends count / amount | +4.0% / +1.9% | no AGI amount: −74% |
| Capital gains count / amount | −0.2% / −30% | $1–5k amount: −120% |

The generated file starts about as far from 2019 as the real PUF does,
except in the no-AGI bracket.

**Result:**

| | B0 (totals) | B1 (brackets) | For comparison, A1 |
|---|---|---|---|
| Largest miss after the tilt | under 0.000001% | under 0.000001% | |
| Move | **0.0064 nats** | **0.0206 nats** | 0.0061 |
| Effective sample size, after / before | **0.987** | **0.865** | 0.992 |
| Weight factor, smallest / largest | 0.036 / 3.1 | ~0 / 131 | 0.000006 / 164 |
| Weight factor, 1st / 50th / 99th percentile | 0.79 / 0.98 / 1.31 | 0.60 / 0.98 / 1.45 | 0.72 / 1.07 / 1.58 |
| Records with factor above 10 / below 0.1 | 0 / 1 | 12 / 292 | 28 / 24 |

B1 used the full 199 Newton steps and met the convergence test.

**Who moves most in B1:** 29 of the 30 extreme records are in the no-AGI
bracket. Records with wages of $250,000 to $950,000 and an `other_agi` loss
large enough to make AGI negative go to nearly zero weight. Records with
investment income and losses, and little or no wages, go up 7 to 131 times. This is the tilt
undoing the no-AGI excess that the step 4 report traces mostly to the
dollar draw.

**Untargeted relationships**, in percent. "Real" is the real PUF before any
tilt. The generated file starts close to the real one, and the tilt moves it
as little as it moves the real file:

| | Real | Generated | B0 | B1 |
|---|---|---|---|---|
| Any dividends: joint, wages 200k+ | 61.1 | 61.5 | 60.6 | 62.4 |
| Any dividends: joint, wages 1–25k | 22.9 | 24.8 | 25.5 | 24.9 |
| Any dividends: not joint, wages 0 | 31.1 | 30.0 | 30.5 | 30.9 |
| Any capital gains: dividends over $100k | 93.5 | 89.1 | 89.1 | 90.9 |
| Any capital gains: dividends $1k–10k | 67.5 | 69.9 | 69.5 | 70.3 |
| Joint with no dependents | 49.7 | 51.7 | 52.2 | 51.9 |
| Not joint with 3+ dependents | 3.4 | 3.9 | 3.8 | 3.8 |

The other cells in `untargeted_B0.csv` and `untargeted_B1.csv` differ from
the real file by under 2.5 points. The dividends share by AGI bracket is
targeted in B1, so it lands on SOI's 2019 values, as in A1.

**The redraw.** 200,000 records drawn with replacement, with probability
proportional to the tilted weight, each given weight 789. That picked 125,000
to 126,000 distinct records. Each target is compared with the redrawn total,
measured in standard errors of a sample of that size:

| | B0 | B1 |
|---|---|---|
| Targets within 2 standard errors | 10 of 10 | 125 of 136 |
| Largest miss, in standard errors | 1.7 | 3.8 |
| Largest miss, in percent | capital gains amount: +12% | $1–5k capital gains amount: +355% |

With 136 targets, about 7 would fall outside 2 standard errors by chance;
11 did. The excess comes from dollar amounts held by a few records, where
the normal approximation is rough. The percent misses show the real limit:
a 200,000-record equal-weight file cannot pin down a total that a few dozen
records carry, such as capital gains in a low bracket. That is what
oversampling the top (and thin cells) would address.

## 7. What is not done

- **Oversampling the top** is not done. It is the next refinement. The
  `other_agi` band problem in the step 4 report matters for it: a generated
  record weighing about 790 that draws one of the PUF's largest losses
  becomes a −$100 billion record.
- Not checked: whether the capital gains targets should exclude 1040-line
  distributions (step 4 report, section 5).
- Run B used `gen_puf.parquet` only. The second-seed and fixed-order files
  were not tilted.
