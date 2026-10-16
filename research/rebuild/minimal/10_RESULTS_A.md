---
title: "Minimal example: translation experiments, Set A (a distorted third survey)"
role: evidence
workstream: rebuild
status: current
updated: 2026-10-15
sot: research/rebuild/design/plan.md
supersedes: []
superseded_by: null
---

# Set A: a third survey with known distortions

The plan is `10_TRANSLATION_EXPERIMENTS.md`, Set A. There is no third survey
prepared, so the 2016-wave SCF stands in for one. Each run distorts the
SCF's four shared amounts (wages, interest, dividends, capital gains) in a
known way, bands the table under one of three rules, retrains the same
network as the bridge grid and scores it with the same script
(`09_score.py`). The PUF is never changed, and the truth the wealth columns
compare against is the undistorted SCF. So each row shows what a
distortion costs under a banding rule.

The distortions (survey side only):

- **D1.** Every shared amount times 0.5.
- **D2.** Wages times 0.9, interest times 0.5, dividends times 0.5, capital
  gains times 2.
- **D3.** Wages replaced by wages plus business and farm income (the SCF's
  BUSSEFARMINC).
- **D4.** Amounts rounded to the nearest $1,000.
- **D5.** Amounts capped at the PUF's 99th percentile (capital gains losses
  at its 1st percentile).

The banding rules:

- **R0.** Each source on its own cut points (the v2 rule; E0 is R0 with no
  distortion).
- **R1.** The SCF's shared amounts on the PUF's cut points, no translation
  (E4 is R1 with no distortion).
- **R2.** `10_translate.py` first decides, from the distorted table alone,
  whether each variable's SCF dollars look like the PUF's, like the PUF's
  scaled by a constant, or like something else. Where it says "scale", the
  SCF amounts are divided by that constant; then the table is banded as R1.

Code: `10_distort.py` builds the tables, `10_translate.py` holds the rule,
`09_run_one.sbatch` trains, evaluates and generates, `09_score.py` scores.
Full tables per run are in
`/nfs/roberts/scratch/pi_nrs36/jar335/minimal/score/<run>/score.md`, and the
detection table of each distorted table in `.../score/detection_<D>.md`.

## The translation rule, as written before any run

For each shared variable, from the distorted table:

- s is the SCF's weighted total over the PUF's weighted total;
- at each bracket lower edge of the detection table, the cumulative ratio is
  the SCF's share of units at or above that amount over the PUF's share.

If s is within 0.15 of 1, the case is "same dollars" and nothing is done.
Otherwise, if the cumulative ratios at $500, $2,000, $10,000, $50,000 and
$200,000 are all on the same side of 1 as s (all below 1 when s is below 1,
all above when it is above), the case is "scale" and the SCF's amounts are
divided by s. Otherwise the case is "definition or mixed": nothing is done
and the variable is flagged. This is a first, deliberately simple rule.

Note that s compares totals over different populations: the SCF's weights
sum to 140 million units, the PUF's to 151 million returns. On the
undistorted table s is 1.07 for wages, 1.01 for interest, 1.04 for dividends
and 1.10 for capital gains, so the rule calls every variable "same dollars"
there, and where it calls "scale" on a distorted table its factor carries
that gap along.

## Summary table

Columns as in `09_RESULTS.md`. Net worth total is the ratio of the generated
PUF-style file's total to the actual SCF's; since the generated file has
157.8 million returns and the SCF 140 million units, 1.13 means the mean is
right. The held-out loss column says "bands differ" whenever the band
layout is not v2's, which is every R1 and R2 run and most R0 runs.

<!-- TABLE START -->
| Run | What it changes | Net worth total, ratio to SCF (PUF / SCF label) | Top 1% share, % (actual 39.7) | Median ratio (PUF / SCF label) | Holders % int/div/kg (PUF 28.3/18.4/13.3) | Interest label swap (nats) | Wealth tilt: move (nats) / ESS ratio | Held-out loss |
|---|---|---:|---:|---:|---:|---:|---:|---:|
| E0 | control: the v2 model | 1.92 / 1.53 | 44.3 / 36.3 | 1.39 / 1.75 | 27.4/17.7/12.9 | 0.301 | 0.034 / 0.95 | 21.694 |
| E4 | shared variables on PUF dollar scale | 1.11 / 1.00 | 40.1 / 36.1 | 0.94 / 1.07 | 27.5/17.6/12.5 | 0.174 | 0.011 / 0.98 | bands differ |
| A_D1_R0 | D1 all amounts x0.5; own cut points | 2.03 / 1.60 | 45.5 / 38.3 | 1.34 / 1.63 | 27.3/17.3/12.7 | 0.309 | 0.029 / 0.95 | bands differ |
| A_D1_R1 | D1 all amounts x0.5; PUF cut points | 1.90 / 1.78 | 40.1 / 34.1 | 1.92 / 2.60 | 26.9/17.0/12.4 | 0.132 | 0.043 / 0.93 | bands differ |
| A_D1_R2 | D1 all amounts x0.5; translated then PUF cut points | 1.15 / 1.06 | 38.9 / 36.4 | 0.94 / 1.15 | 27.3/17.5/12.8 | 0.183 | 0.016 / 0.97 | bands differ |
| A_D2_R0 | D2 uneven scale; own cut points | 1.92 / 1.53 | 44.3 / 36.3 | 1.39 / 1.75 | 27.4/17.7/12.9 | 0.301 | 0.034 / 0.95 | bands differ |
| A_D2_R1 | D2 uneven scale; PUF cut points | 1.27 / 1.15 | 35.6 / 33.9 | 1.02 / 1.33 | 27.4/17.7/12.9 | 0.156 | 0.019 / 0.97 | bands differ |
| A_D2_R2 | D2 uneven scale; translated then PUF cut points | 1.21 / 1.08 | 37.1 / 36.8 | 1.06 / 1.26 | 27.4/17.3/12.6 | 0.189 | 0.012 / 0.98 | bands differ |
| A_D3_R0 | D3 wages plus business income; own cut points | 1.73 / 1.55 | 42.4 / 39.0 | 1.52 / 1.67 | 26.9/17.1/12.5 | 0.303 | 0.031 / 0.95 | bands differ |
| A_D3_R1 | D3 wages plus business income; PUF cut points | 0.90 / 0.91 | 38.1 / 36.7 | 0.86 / 1.08 | 26.5/16.8/12.2 | 0.185 | 0.010 / 0.99 | bands differ |
| A_D3_R2 | D3 wages plus business income; translated then PUF cut points | 0.90 / 0.91 | 38.1 / 36.7 | 0.86 / 1.08 | 26.5/16.8/12.2 | 0.185 | 0.010 / 0.99 | bands differ |
| A_D4_R0 | D4 rounded to $1000; own cut points | 1.90 / 2.04 | 42.1 / 35.9 | 1.39 / 2.01 | 26.7/17.1/12.3 | 0.725 | 0.040 / 0.93 | bands differ |
| A_D4_R1 | D4 rounded to $1000; PUF cut points | 1.06 / 1.08 | 41.3 / 37.3 | 0.90 / 1.22 | 27.0/17.2/12.4 | 1.113 | 0.019 / 0.97 | bands differ |
| A_D4_R2 | D4 rounded to $1000; translated then PUF cut points | 1.06 / 1.08 | 41.3 / 37.3 | 0.90 / 1.22 | 27.0/17.2/12.4 | 1.113 | 0.019 / 0.97 | bands differ |
| A_D5_R0 | D5 capped at PUF 99th pct; own cut points | 2.53 / 1.93 | 53.2 / 44.6 | 1.38 / 1.60 | 27.0/17.1/12.4 | 0.357 | 0.029 / 0.96 | bands differ |
| A_D5_R1 | D5 capped at PUF 99th pct; PUF cut points | 1.15 / 1.13 | 43.8 / 39.3 | 0.91 / 1.14 | 27.4/17.7/12.8 | 0.201 | 0.010 / 0.98 | bands differ |
| A_D5_R2 | D5 capped at PUF 99th pct; translated then PUF cut points | 1.14 / 1.17 | 45.6 / 43.2 | 0.86 / 1.07 | 27.0/17.2/12.4 | 0.196 | 0.009 / 0.99 | bands differ |
<!-- TABLE END -->

Mean net worth on PUF-label records by wage group, in thousands of dollars,
for the runs discussed below (actual 2016-wave SCF in the first row):

| Run | Zero wages | $1-25k | $25-50k | $50-100k | $100-200k | $200k+ | All |
|---|---:|---:|---:|---:|---:|---:|---:|
| actual SCF | 663 | 173 | 181 | 353 | 934 | 5,119 | 614 |
| E0 | 2,376 | 411 | 443 | 788 | 1,546 | 5,565 | 1,043 |
| E4 | 1,362 | 232 | 248 | 445 | 937 | 3,336 | 601 |
| A_D1_R1 | 2,205 | 426 | 434 | 818 | 1,592 | 5,771 | 1,030 |
| A_D1_R2 | 1,506 | 235 | 231 | 443 | 908 | 3,684 | 625 |
| A_D2_R1 | 1,530 | 273 | 282 | 571 | 1,056 | 3,602 | 692 |
| A_D2_R2 | 1,504 | 235 | 279 | 480 | 1,016 | 3,634 | 660 |
| A_D3_R0 | 2,045 | 347 | 408 | 770 | 1,439 | 5,958 | 942 |
| A_D3_R1 | 1,073 | 192 | 203 | 378 | 744 | 3,082 | 487 |
| A_D5_R0 | 3,290 | 521 | 597 | 1,035 | 1,929 | 7,459 | 1,375 |
| A_D5_R1 | 1,396 | 243 | 268 | 481 | 944 | 3,324 | 626 |

## How much two runs differ by chance

Training gives exactly the same model when the table and the GPU card type
are the same: A_D2_R0 reproduces E0 to the last digit, and the R1 and R2
twins of D3 and D4 (tables identical byte for byte, since the rule left
them alone) score identically. On a different card type the same table
gives a somewhat different model. Two pairs show how much:

- E0 against A_D1_R0 (the bands are identical record for record; E0 was
  trained on an RTX A5000, A_D1_R0 on an RTX 5000 Ada): net worth total
  1.92 against 2.03, top 1% share 44.3 against 45.5, median 1.39 against
  1.34, tilt move 0.034 against 0.029.
- A_D5_R1 against A_D5_R2 (identical tables; RTX A5000 against H200):
  total 1.15 against 1.14, top 1% share 43.8 against 45.6 (under the SCF
  label 39.3 against 43.2), median 0.91 against 0.86, tilt 0.010 against
  0.009.

So a gap of about 0.1 in the total ratio, 2 points in the top 1% share (4
under the SCF label), 0.05 in the median ratio or 0.005 nats in the tilt is
within chance and should not be read as an effect. The runs used whatever
card was free; the card of each run is in its log (first lines of
`logs/09_run_<job>.out`). E4 and A_D1_R1 were trained on the same card type,
so that comparison is clean. Replicate runs would firm this up; none were
made.

The held-out loss column reads "bands differ" for every Set A run, because
the scorer compares cut points with v2's and any distortion moves the SCF's
cut points, even where (D1 and D2 under R0) every record keeps its band.

## Per distortion: what the detection table shows and what the rule decided

The full detection table of each distorted table, with the rule's inputs at
the top, is in `score/detection_<D>.md`. The rule's decisions:

| Distortion | Wages | Interest | Dividends | Capital gains |
|---|---|---|---|---|
| D1 x0.5 | scale, 0.537 | scale, 0.505 | scale, 0.519 | scale, 0.552 |
| D2 uneven | same dollars (s 0.966) | scale, 0.505 | scale, 0.519 | definition or mixed (s 2.21) |
| D3 business income | definition or mixed (s 1.32) | same dollars | same dollars | same dollars |
| D4 rounding | same dollars | same dollars | same dollars | same dollars |
| D5 top-coding | same dollars (s 0.906) | definition or mixed (s 0.333) | definition or mixed (s 0.456) | definition or mixed (s 0.220) |

So R2 differs from R1 only for D1 and D2. For D3, D4 and D5 the R2 table is
the R1 table.

**D1 (everything halved).** The SCF totals fall to about half the PUF's
(s from 0.51 to 0.55) and the SCF's share of units above every dollar line
from $500 up is below the PUF's (cumulative ratios 0.41 to 0.89). The rule
calls all four "scale", which is right. Its factor is the ratio of totals,
not 0.5: it also carries the gap the two sources had before any distortion
(the SCF's totals were 1 to 10% above the PUF's), so the translated SCF
wages end up 7% below the true SCF's and capital gains 9% below.

**D2 (wages x0.9, interest and dividends x0.5, capital gains x2).** Interest
and dividends look as in D1 and are scaled, rightly. Wages are missed: the
10% cut lands within the 15% tolerance because the SCF's wages were 7%
above the PUF's to begin with (s 0.966). Capital gains are misread: the
total doubles (s 2.21) and the shares above $10,000 and up are well above
the PUF's (1.49 to 3.09), but the shares above $500 and $2,000 stay below
(0.70 and 0.76), because the SCF reports far fewer small gains than the PUF
even when its dollars are doubled. The rule needs every ratio on one side,
so it says "definition or mixed". The rule can therefore see a survey whose
dollars are scaled down, where under-reporting of small amounts points the
same way, but not one scaled up, where the two point in opposite directions.

**D3 (wages plus business and farm income).** 10,831 of the 34,377 SCF rows
change (11,182 have nonzero business income; 3,564 zero-wage rows become
wage earners; 420 rows, 0.6% of SCF weight, have a business loss larger
than their wages and are set to zero). The business income added is $1.73
trillion in 2015 dollars, so the SCF wage total rises from $7.67 trillion
to $9.40 trillion (s 1.32). The cumulative ratios run from 0.92 at $500 to
1.96 at $200,000: fewer units than the PUF above low amounts, far more
above high ones. A scaled survey would not do this, and the rule flags it.
That is the right call; nothing in the rule can fix it.

**D4 (rounding to $1,000).** Totals do not move (s 1.01 to 1.10, as
undistorted). The detection table shows the rounding plainly: no SCF unit at
all between $1 and $500, so the SCF holder share of interest falls from
12.6% to 5.8% and of dividends from 12.0% to 8.4%. The rule says "same
dollars" for all four, which is right in the sense that there is nothing to
scale; it has no case for rounding.

**D5 (capped at the PUF's 99th percentile).** The caps are $315,000 for
wages, $8,410 for interest, $26,300 for dividends, and $41,700 for capital
gains with losses floored at -$3,000. Capping at the 99th percentile of all
units, zeros included, takes out most of the investment income: the SCF
totals fall to 33% (interest), 46% (dividends) and 22% (capital gains) of the
PUF's. The detection table shows the cap plainly: the SCF's share above the
cap is exactly zero. The rule flags the three investment incomes as
"definition or mixed", which is the right answer (scaling them would be
wrong), but it says "same dollars" for wages (s 0.906), where the cap bites
only the top 1%.

## What the runs show

**D1.** Under R0 the bands are those of v2 record for record, so the run
is E0 up to the card-type noise above. Under R1 PUF-style wealth is as high
as E0's (total 1.90, median 1.92 times the SCF's, against 0.94 for E4): a
PUF unit with a given wage looks to the model like an SCF unit with half of
it, which is a richer unit. Under R2 it comes back to E4 (total 1.15 against
1.11, median 0.94 against 0.94, tilt 0.016 against 0.011).

**D2.** Under R0 identical to E0. Under R1 the total is 1.27 and the top
1% share 35.6%, below the actual 39.7%: the halved interest and dividends
push wealth up, the doubled capital gains pull the top down (top 0.1% share
12.3% against 15.3% actual). Under R2, with interest and dividends fixed and
capital gains not, the total is 1.21 and the top 1% share 37.1%; better
than R1 on the shape, still short of E4 at the top.

**D3.** Under R0 about as bad as E0 (total 1.73, median 1.52). Under R1
(= R2) the total is 0.90, so the mean is about 80% of the SCF's, and the
top of the wage distribution is too poor: units with wages over $200,000
hold 3.1 million on average against 5.1 million in the SCF (E4: 3.3
million). Zero-wage units get closer to the SCF (1.07 million against
663,000; E4 1.36 million), since business owners without wages have left
that group in the SCF. The wealth tilt costs only 0.010 nats, as little as
E4: the meter does not see a definition problem.

**D4.** Under R0 the PUF-label file is about E0 (total 1.90), the SCF-label
file worse (2.04 against 1.53) and the tilt slightly dearer (0.040 against
0.034). Under R1 (= R2) wealth is about E4 (total 1.06, median 0.90, top 1%
41.3%), but the interest label swap rises from 0.17 to 1.11 nats: with small
amounts rounded to zero, the SCF label now means "has no small interest",
and the model learns it. Holder shares on PUF-style records are not
affected.

**D5.** Under R0 the worst row of the grid: total 2.53, top 1% share 53.2%,
top 0.1% 20.3%. With within-source ranks, the SCF's capped units sit in one
lump band at the cap and the bands above it are empty, so the PUF's richest
incomes are matched to the SCF's capped lump, which holds the SCF's very
wealthy. Under R1 (and the identical R2) the total is fine (1.15 and 1.14)
and the top 1% share somewhat high (43.8% and 45.6%, against 40.1% for E4,
partly chance).

## Expectation against outcome

The expectations in `10_TRANSLATION_EXPERIMENTS.md`, written before the
runs, against what happened:

- *D1 under R0 equals E0.* Yes in substance: the bands are identical (an
  assertion in `10_distort.py`); the scores differ only by the card-type
  noise (total 2.03 against 1.92).
- *D1 under R1 is worse than E4.* Yes, much worse: total 1.90 against 1.11,
  median 1.92 against 0.94, on the same card type as E4.
- *D1 under R2 returns to E4.* Yes: 1.15, 0.94, tilt 0.016.
- *D4 under R0 is a bit worse than E0, under R1 about E4.* Yes on wealth.
  Not expected: under R1 the interest label swap rises to 1.1 nats, second
  only to E10 (1.9) across both grids.
- *D3 is bad under every rule and the detection table flags it.* The flag:
  yes. Bad under every rule: yes, though not in the same way. R0 is as bad
  as E0; R1 and R2 put wealth 20% too low overall and 40% too low for the
  highest wage group, while the tilt meter reads as cheap as E4.
- *D5 hurts the top under R1 and R2, less under R0.* Wrong way round. R0 is
  hurt most (total 2.53, top 1% 53.2%); under R1 and R2 the total is right
  and the top 1% share is 4 to 6 points above the actual 39.7%, of which
  up to 2 points may be chance.
- Not in the expectations: the translation rule misreads a survey scaled
  up (D2 capital gains) and misses a 10% scaling (D2 wages), and it has no
  case for rounding or top-coding, though the detection table shows both
  plainly.

## Revision history

- 2026-10-15: written with the first tables; all fifteen runs scored the
  same morning.
