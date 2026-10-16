---
title: "Minimal example: results of the bridge-variable experiment grid"
role: evidence
workstream: rebuild
status: current
updated: 2026-10-15
sot: research/rebuild/design/plan.md
supersedes: []
superseded_by: null
---

# Results of the bridge-variable grid

One row per run of `09_BRIDGE_EXPERIMENTS.md`, all scored by the same
script (`09_score.py`, run by `09_score.sbatch`). Full tables per run are in
`/nfs/roberts/scratch/pi_nrs36/jar335/minimal/score/<run>/score.md`.

How to read the columns:

- **Net worth total.** Generated PUF-style records against the actual
  2016-wave SCF, both in 2015 dollars. The generated file has equal weights
  summing to SOI's 157.8 million returns for 2019; the SCF has 140 million
  units. So a total of 1.13 means the mean is right. Two numbers: net worth
  drawn under the PUF label, then under the SCF label.
- **Top 1% share** and **median ratio**, the same two files.
- **Holders.** Share of generated PUF-style records with taxable interest,
  dividends and capital gains (for a split table, either part nonzero),
  against the weighted 2015 PUF in the header. The IRS 2015 counts are the
  same to a tenth of a point (28.3/18.3/13.3).
- **Interest label swap.** How much worse the model predicts interest on
  PUF test records when told they are SCF records (nats; 0 means the label
  makes no difference).
- **Wealth tilt.** The move the tilt needs to hit the six 2019 SCF wealth
  targets on the PUF-label file, from equal weights, return count held
  fixed; and the share of the effective sample size kept. The six targets
  are used in a form without ranks: units and net worth above the 2019
  SCF's 50th, 90th and 99th percentiles, total net worth, and units above
  $1 million. 06's rank iteration, run on these six alone, often did not
  settle (E2, E4, E7) and gave nonsense. With the 136 IRS targets added it
  mostly settles; both versions are in each run's `score.md`. For E0 the
  rank version reproduces 06: 0.0090 nats for the 136 alone, 0.0749 with
  the six.
- **Held-out loss.** Sum of the half-hidden test cross-entropy over all
  variables and both sources. Only comparable when the bands match v2's.

<!-- TABLE START -->
| Run | What it changes | Net worth total, ratio to SCF (PUF / SCF label) | Top 1% share, % (actual 39.7) | Median ratio (PUF / SCF label) | Holders % int/div/kg (PUF 28.3/18.4/13.3) | Interest label swap (nats) | Wealth tilt: move (nats) / ESS ratio | Held-out loss |
|---|---|---:|---:|---:|---:|---:|---:|---:|
| E0 | control: the v2 model | 1.92 / 1.53 | 44.3 / 36.3 | 1.39 / 1.75 | 27.4/17.7/12.9 | 0.301 | 0.034 / 0.95 | 21.694 |
| E1 | zero PUF amounts under limit | 1.27 / 1.12 | 43.4 / 40.0 | 1.03 / 1.09 | 12.1/11.3/7.7 | 0.085 | 0.013 / 0.98 | bands differ |
| E2 | split at limit; small part PUF-only | 1.75 / 1.40 | 45.5 / 39.9 | 1.28 / 1.33 | 27.6/17.3/12.4 | 0.087 | 0.025 / 0.96 | bands differ |
| E3 | E2 with the limits raised | 1.62 / 1.33 | 47.2 / 38.7 | 0.75 / 1.03 | 26.6/17.0/11.7 | 0.015 | 0.031 / 0.95 | bands differ |
| E4 | shared variables on PUF dollar scale | 1.11 / 1.00 | 40.1 / 36.1 | 0.94 / 1.07 | 27.5/17.6/12.5 | 0.174 | 0.011 / 0.98 | bands differ |
| E5 | E2 split on PUF dollar scale | 1.32 / 0.97 | 40.3 / 38.1 | 1.20 / 1.00 | 27.8/17.3/12.2 | 0.062 | 0.022 / 0.96 | bands differ |
| E6 | flip SCF non-holders (translation) | 1.17 / 1.13 | 50.3 / 42.1 | 1.01 / 1.25 | 27.6/17.7/12.8 | 0.678 | 0.006 / 0.99 | bands differ |
| E7 | shared variables without source label | 1.98 / 1.96 | 30.6 / 35.0 | 3.28 / 2.69 | 26.0/18.0/12.9 | 0.000 | 0.120 / 0.81 | 22.480 |
| E8 | E7 plus coarsened SCF zeros | 1.94 / 1.99 | 28.9 / 34.4 | 3.43 / 2.75 | 27.8/19.1/13.6 | 0.000 | 0.130 / 0.80 | 22.278 |
| E9 | reporting curve in the loss | 1.59 / 1.45 | 31.2 / 38.8 | 3.04 / 2.12 | 28.7/18.7/13.5 | 0.000 | 0.111 / 0.82 | bands differ |
| E10 | SCF investment zeros not scored | 1.91 / 1.57 | 40.8 / 42.7 | 1.19 / 1.22 | 27.4/17.7/12.9 | 1.926 | 0.033 / 0.95 | 31.081 |
| E11 | tilt only, wealth by wage group | 1.04 | 38.4 | 1.13 | 24.7/15.1/10.8 | — | 0.057 / 0.90 | as E0 |
| E12 | E4 (PUF label) plus the full tilt | 1.04 | 38.4 | 1.13 | 30.8/18.4/13.8 | 0.174 | 0.025 / 0.95 | bands differ |
<!-- TABLE END -->

## Notes per run

**The answer the grid gives.** Fixing the data works; fixing the model, as
tried here, does not. Putting the shared variables on the PUF's dollar cut
points (E4) brings PUF-style wealth to the SCF's level: the mean is 0.98 of
the SCF's under the PUF label, the median 0.94, the top 1% share 40.1%
against 39.7%, and holder shares stay where they were. The tilt to the
wealth targets then costs 0.011 nats, a third of E0's 0.034. Removing the
source label from shared variables (E7, E8, E9) makes wealth worse, not
better.

- **E0 (control).** Agrees with 06 and 07 where they overlap: total $165
  trillion against $85.8 trillion (1.92), top 1% share 44.3%, the 136-target
  move 0.0090 and the 136-plus-wealth move 0.0749 (rank version). Under the
  SCF label: $131 trillion, top 1% 36.3%, as in the owner-label run. The
  wealth excess is largest in the zero-wage group (mean 3.6 times the SCF's).
- **E1 (PUF small amounts zeroed).** Wealth comes close (mean 1.12 / 0.99),
  but the PUF holder shares fall to 12/11/8%, so the 136 IRS targets then
  cost 0.19 nats on their own. Shows the holder-share gap is most of the
  problem; not usable as output.
- **E2 (split at the line).** Removes only part of the excess: mean 1.55,
  top 1% 45.5%. Holder shares intact. Interest label swap falls from 0.30
  to 0.09.
- **E3 (split, raised lines).** Mean 1.44; the median falls to 0.75 and the
  top 1% share rises to 47.2%, so the middle is now too poor and the top too
  rich. Label swap near zero (0.015): with high lines the shared part is rare
  in both surveys.
- **E4 (common dollar scale).** The best row: see above. Two gaps remain.
  Zero-wage units still hold twice the SCF's mean (1.36 million against
  663,000), and units with wages over $200,000 hold too little (3.3 million
  against 5.1 million). Part of the first is population: zero-wage units are
  16% of PUF returns and 30% of SCF units, which include non-filers.
- **E5 (split on the common scale).** Mean 1.17 under the PUF label, 0.86
  under the SCF label; tilt 0.022. Not better than E4 alone.
- **E6 (translation, D23).** Mean 1.03 and median 1.01, but the top 1%
  share is 50.3% against 39.7%, and the interest label swap is the largest
  of any data run (0.68). The tilt is cheapest here (0.006), because
  cutting a few very rich records costs little; the meter does not see a
  top that is too heavy.
- **E7 (no source label on shared variables).** Wealth does not fall (mean
  1.75) and its shape is wrong: the median is 3.3 times the SCF's and the
  top 1% share 30.6%. The SCF-style file breaks too ($131 trillion against
  $79 trillion for E0). Held-out loss rises from 21.69 to 22.48.
- **E8 (E7 plus coarsened zeros).** The same as E7.
- **E9 (reporting curve, common scale, no label).** Better than E7 (mean
  1.40) but the median is still 3 times the SCF's; tilt 0.11.
- **E10 (SCF investment zeros not scored).** PUF-style wealth barely moves
  (mean 1.69), and the SCF side breaks: SCF-style net worth totals $623
  trillion. Its held-out loss (31.1) is not comparable in practice: the
  model never learned SCF zeros, and the test still scores them.
- **E11 (tilt only).** The E0 file tilted to the six wealth targets plus
  mean net worth by wage group from the 2019 SCF: 0.057 nats, 90% of the
  effective sample kept. Every group mean is met (zero wages: 706,000
  against 2.38 million before). The six alone cost 0.034. With the 136 IRS
  targets as well, 0.116 nats and the solver stops short of full precision.
  06's rank version with the wage-group means did not settle at all. The
  tilt lowers the holder shares by two to three points.
- **E12 (E4 plus the full tilt).** E4's PUF-label file tilted to the 136 IRS
  targets plus the six wealth targets: 0.0245 nats against 0.0113 for the
  136 alone, so wealth adds 0.013 (E0: 0.055). Mean, median and top 1% then
  match the 2019 SCF. The holder shares in the row are under the 2019 IRS
  targets, so they are higher than the 2015 PUF's.

**Limits of the meter.** The tilt is cheap whenever the excess sits in a few
records, because cutting them moves little weight (E6). It also nearly
removes the 800 to 1,500 richest records in every run (weight factors near
zero), which is why the top 0.1% share after any tilt is 10% to 13% against
15% in the SCF. Read the move together with the wealth columns, not alone.
