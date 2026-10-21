---
title: "Step 26: the fake-survey test of decision 18 rerun with the source label on"
role: evidence
workstream: rebuild
status: current
updated: 2026-10-17
sot: research/rebuild/design/plan.md
supersedes: []
superseded_by: null
---

# Step 26: the fake-survey test with the source label on

John changed plan decision 14 on 17 October: the source label stays on
every record, in training and in generation. Step 22's latent method
(`22_FAKE_SURVEY.md`) and step 24's non-filer variant (`24_OVERNIGHT.md`,
section D) had removed the label. This step reruns them with the label on
and compares the two. The fake survey, the distortions D1 to D4 and the
measures are those of step 22; only the treatment of the label changes.

## What changed in the code

All in `22_fake_survey.py`, as new arms; the label-off arms run exactly
the code they ran before (the shared steps were moved into one helper with
the same operations, and no random draws were added or removed). This was
checked by reading the code, not by rerunning a label-off arm.

- Line 132: three new arms, `latent_fixed_label`, `latent_learn_label`
  and `latent_learn_label_drop` (the list `LATENT_LABELLED`); line 821
  sends them to the latent trainer.
- Lines 428 to 444 (`enc` in `train_latent`): with the label on, each row
  is encoded as today's model sees it. Survey rows carry the survey's
  label and PUF rows the PUF's. Other income is absent on PUF rows, not
  present-and-hidden as in the label-off arms.
- Lines 513 to 529 (`anchored`): the belief that the reporting settings
  are fit against. With the label off it is unchanged: the survey's row
  with other income and both broken items hidden. With the label on, the
  same row is asked under the PUF label, with other income absent as on
  PUF rows and both broken items hidden. So it is the PUF-labelled belief
  about true interest (or dividends) given married, dependents, wages and
  capital gains, which only PUF rows teach. Both the S-curve's gradient
  step and the grid search for the dollar factor use it.
- The fill-ins are unchanged in form: each pass, each survey zero gets a
  true slice drawn from the belief under the survey's own label (with the
  label on) given everything on the record, times the chance of reporting
  zero at that slice. The survey-labelled part of the model then trains
  on the filled-in slices.
- Line 462 (`drop_rule`): the "drop" rule of step 22 (survey rows do not
  train the interest and dividend slots when other income is hidden) is
  off for `latent_learn_label` and on for `latent_learn_label_drop`.
  **What I did and why:** I left it off in the main label-on arm. The
  rule existed because, without a label, the belief the settings were fit
  against was one shared belief that survey rows (with their filled-in
  amounts) also trained, so a wrong setting could feed itself through it.
  With the label on, that belief is the PUF-labelled one, and survey rows
  never train under the PUF label, so the route the rule closed is not
  there. The arm with the rule kept is a check that it makes no
  difference; it does not (results below).

New files:

- `26_label_on.py`: reads saved models and scores only. `pufcheck` asks
  each model about the PUF pile (measure 4 below); `fills` writes the
  filled-in true amounts of every survey row (asked for by John on 17
  October, below); `report` writes the tables to scratch
  `fake26/report.md`.
- `26_check.sbatch`: runs the three steps of `26_label_on.py`.

`23_train_latent.py` (the real-SCF run) was not changed: it is a
label-off trainer, and adding the fill-in file there without a test run
was not possible in the time this session had. See open questions.

## What ran

All jobs finished (state COMPLETED). Job list: scratch
`logs/f26_jobs.txt`.

| Job | What | Result |
|---|---|---|
| 29887349 | Smoke test: the three new arms on D1, 4 passes, scratch `fake26_smoke/` | ran, then discarded |
| 29887350 to 29887357 | The three label-on arms on D1, D2, D3, D4, seeds 20261016 and 20261017, 80 passes | scratch `fake26/<arm>_<D>_s<seed>/` |
| 29887358, 29887359 | Non-filers in the survey only (PX of step 24), D1, the two main label-on arms, two seeds | scratch `fake26/PX/` |
| 29887752, 29887753 | The PUF-pile check of every compared model, label off and on; the tables | scratch `fake26/pufcheck/`, `fake26/report.md` |
| 29889110 | The filled-in amounts of every latent model; the tables again | scratch `fake26/<run>/fills.parquet` (label on), `fake26/fills/` (label off), `fake26/fills/summary.json` |

The label-off results, the undistorted survey and today's rule are the
step-22 runs in scratch `fake22/` and the step-24 runs in `fake24/PX/`,
read from disk and not rerun. The tables in `fake26` were copied from
`fake22/aux_D*.json` and `fake24/PX/aux_D1.json` (the planted settings and
slice summaries, unchanged). D3 uses step 22's shift of 0.75.

Each training run took 1.5 to 3.5 minutes on one GPU.

## Results

Two numbers per cell, one per seed (20261016, 20261017), unless stated.
The measure of other income is step 22's: the mean other income of PUF
returns in a group, each amount held between the survey's 1st and 99th
percentiles, as the percentage gap from the truth. The undistorted survey
(the model trained on the survey before it was broken) is the reference:
its gap is the model's own error at this sample size.

### 1. Returns with no interest: mean other income, gap from the truth (%)

| | D1 | D2 | D3 | D4 | Mean of 8 runs |
|---|---:|---:|---:|---:|---:|
| Undistorted survey | +21, +17 | +21, +17 | +21, +17 | +21, +17 | +19 |
| Today's rule | +39, +35 | +39, +37 | +34, +29 | +40, +33 | +36 |
| Latent, settings fixed, label off | +19, +10 | +21, +11 | +15, +9 | +18, +14 | +15 |
| Latent, settings fixed, label on | +23, +14 | +21, +15 | +14, +13 | +19, +18 | +17 |
| Latent, settings learned, label off | +22, +13 | +23, +13 | +18, +12 | +24, +14 | +17 |
| Latent, settings learned, label on | +23, +17 | +22, +17 | +15, +15 | +20, +19 | +19 |
| Learned, label on, drop rule kept | +23, +15 | +22, +20 | +14, +16 | +18, +20 | +19 |

Over all PUF returns the label-on arms are at +1 to +4% (undistorted
survey +5 and +6; today's rule +8 to +13; label off -2 to +5).

### 2. Hidden holders among the survey's zeros: model against the truth (%)

The model's share is the chance, under its final belief and settings,
that a survey unit reporting zero holds the item, averaged with the
survey weights. The truth is in brackets.

Interest:

| | D1 | D2 | D3 | D4 |
|---|---:|---:|---:|---:|
| Fixed, label off | 19.2, 17.8 (18.1) | 19.9, 18.2 (18.1) | 17.1, 16.2 (16.5) | 19.2, 18.1 (18.2) |
| Fixed, label on | 19.1, 18.3 (18.1) | 21.3, 19.8 (18.1) | 16.8, 16.6 (16.5) | 18.5, 18.3 (18.2) |
| Learned, label off | 18.0, 16.9 (18.1) | 18.6, 17.3 (18.1) | 16.5, 15.7 (16.5) | 18.1, 17.2 (18.2) |
| Learned, label on | 17.9, 16.7 (18.1) | 18.8, 16.7 (18.1) | 15.8, 15.2 (16.5) | 16.8, 16.9 (18.2) |
| Learned, label on, drop rule kept | 17.5, 16.5 (18.1) | 18.8, 16.6 (18.1) | 16.0, 15.1 (16.5) | 17.6, 16.5 (18.2) |

Dividends:

| | D1 | D2 | D3 | D4 |
|---|---:|---:|---:|---:|
| Fixed, label off | 6.1, 5.7 (6.1) | 6.5, 5.9 (6.1) | 5.5, 5.0 (5.2) | 6.6, 5.8 (6.2) |
| Fixed, label on | 5.9, 5.5 (6.1) | 6.5, 5.9 (6.1) | 5.3, 5.1 (5.2) | 5.9, 5.7 (6.2) |
| Learned, label off | 5.5, 5.1 (6.1) | 5.6, 5.2 (6.1) | 4.5, 4.1 (5.2) | 5.6, 5.4 (6.2) |
| Learned, label on | 4.9, 4.9 (6.1) | 4.9, 4.8 (6.1) | 4.0, 4.0 (5.2) | 5.1, 5.2 (6.2) |
| Learned, label on, drop rule kept | 4.8, 5.0 (6.1) | 4.8, 4.5 (6.1) | 3.9, 3.9 (5.2) | 4.9, 5.0 (6.2) |

How well the fill-ins pick out which households are the hidden holders is
the same with the label on and off: 0.82 to 0.84 for interest and 0.84 to
0.85 for dividends, on a scale where 0.5 is a coin flip and 1 is perfect.

### 3. Learned reporting settings against the planted ones

Planted: 13% report at $50, 78% at $500, 98% at $5,000; dollar factor
1.00 (D2: 0.80). The D3 plant is the same curve shifted by 0.75 on the
log-odds, down for the lower half of other income and up for the upper.

| | D | Item | At $50 | At $500 | At $5,000 | Dollar factor |
|---|---|---|---:|---:|---:|---:|
| Label off | D1 | interest | 30, 29 | 75, 77 | 96, 96 | 0.97, 1.00 |
| Label on | D1 | interest | 29, 29 | 74, 78 | 95, 97 | 0.97, 1.00 |
| Label off | D1 | dividends | 10, 11 | 96, 94 | 100, 100 | 0.95, 0.95 |
| Label on | D1 | dividends | 17, 13 | 93, 94 | 100, 100 | 0.95, 0.98 |
| Label off | D2 | interest | 29, 28 | 76, 80 | 96, 98 | 0.80, 0.80 |
| Label on | D2 | interest | 29, 28 | 78, 78 | 97, 97 | 0.80, 0.80 |
| Label off | D2 | dividends | 21, 36 | 90, 80 | 100, 96 | 0.80, 0.76 |
| Label on | D2 | dividends | 30, 39 | 87, 79 | 99, 96 | 0.80, 0.76 |
| Label off | D3 | interest | 35, 33 | 87, 91 | 99, 100 | 0.93, 1.00 |
| Label on | D3 | interest | 36, 34 | 86, 90 | 99, 99 | 1.00, 1.00 |
| Label off | D3 | dividends | 35, 45 | 95, 88 | 100, 98 | 0.98, 1.04 |
| Label on | D3 | dividends | 44, 50 | 90, 85 | 99, 97 | 0.92, 1.02 |
| Label off | D4 | interest | 29, 29 | 73, 75 | 95, 96 | 0.97, 0.97 |
| Label on | D4 | interest | 31, 29 | 73, 74 | 94, 95 | 1.00, 1.00 |
| Label off | D4 | dividends | 16, 20 | 91, 86 | 100, 99 | 0.97, 0.98 |
| Label on | D4 | dividends | 22, 27 | 87, 81 | 99, 98 | 0.92, 0.98 |

The arm with the drop rule kept gives the same interest settings (29 to
36% at $50) and dividend settings a little further from the plant (11 to
55% at $50). Full table: scratch `fake26/report.md`.

### 4. The PUF pile as each model sees it

Each model was asked about the PUF pile under the PUF label (today's
model and the label-on arms; the label-off arms with no label, as they
were trained): the share married, wages given married and dependents, and
the share holding interest and dividends given married, dependents and
wages. The answers are compared with the PUF pile itself, which is the
models' own training data, so this is a check of fit. Means over the runs
in each row (8 runs: 4 distortions times 2 seeds; the undistorted survey
2 runs). The wage gap is the absolute gap of mean wages from the truth in
each of the eight married-by-dependents cells, as a percentage of the
truth, averaged with the cells' weights; the no-wages gap is the same for
the share with no wages, in points.

| | Married share (pts) | Wage gap (%) | No-wages gap (pts) | Log score of wage slice (nats) | Interest held (pts) | Dividends held (pts) |
|---|---:|---:|---:|---:|---:|---:|
| Undistorted survey | -0.67 | 2.52 | 0.40 | 3.6217 | -0.63 | -0.72 |
| Today's rule | -0.70 | 2.51 | 0.63 | 3.6217 | -0.47 | -0.61 |
| Fixed, label off | -0.18 | 1.89 | 0.57 | 3.6218 | -0.63 | -0.97 |
| Fixed, label on | -0.14 | 1.91 | 0.45 | 3.6216 | -0.80 | -0.87 |
| Learned, label off | -0.23 | 2.15 | 0.57 | 3.6217 | -0.66 | -1.02 |
| Learned, label on | -0.14 | 2.06 | 0.42 | 3.6216 | -0.83 | -0.84 |

With non-filers in the survey only (PX, D1, 2 runs per row):

| | Married share (pts) | Wage gap (%) | No-wages gap (pts) | Log score of wage slice (nats) | Interest held (pts) | Dividends held (pts) |
|---|---:|---:|---:|---:|---:|---:|
| Undistorted survey | -0.44 | 2.45 | 0.59 | 3.6214 | -0.23 | -0.35 |
| Today's rule | -0.38 | 1.74 | 0.50 | 3.6212 | -0.56 | -0.58 |
| Fixed, label off | -1.53 | 2.32 | 1.20 | 3.6220 | -1.18 | -1.34 |
| Fixed, label on | -0.54 | 1.59 | 0.48 | 3.6213 | -0.07 | -0.39 |
| Learned, label off | -1.45 | 2.13 | 1.05 | 3.6218 | -0.64 | -0.84 |
| Learned, label on | -0.54 | 1.62 | 0.51 | 3.6214 | -0.04 | -0.29 |

### 5. Non-filers in the survey only (PX), other income

| | No interest | All returns | Hidden interest holders (truth 16.7) | Hidden dividend holders (truth 5.2) |
|---|---:|---:|---:|---:|
| Undistorted survey | -0, -2 | -3, -5 | | |
| Today's rule | +16, +17 | +1, +4 | | |
| Fixed, label off | +6, -4 | -1, -8 | 17.1, 15.8 | 5.6, 4.8 |
| Fixed, label on | +3, +0 | -4, -4 | 17.3, 15.8 | 5.0, 5.3 |
| Learned, label off | +4, -5 | -3, -10 | 17.1, 16.3 | 5.4, 5.0 |
| Learned, label on | +1, -1 | -4, -6 | 16.9, 15.3 | 4.8, 5.3 |

The learned settings under PX are the same with the label on and off
(interest 28 to 29% at $50, 56 to 58% at $500, 80 to 83% at $5,000;
dividends 30 to 34% at $50). The reporting chance at $500 and $5,000 is
well below the plant there, with the label on and off: the survey's
non-filers hold less, and the curve absorbs part of that.

### 6. Survey units holding each item after filling in

From the files of filled-in amounts (`26_label_on.py fills`): for each
survey row, five independent draws of its true interest and dividends
from the final model, given everything else on the record, times the
chance of reporting what the row reported, under the run's final
settings. Shown: the survey's weighted share of units holding the item,
mean of the five draws, settings learned, one number per seed. The five
draws of one run differ by less than half a point.

| | Reported | After filling in, label off | After filling in, label on | Survey, truth | PUF pile |
|---|---:|---:|---:|---:|---:|
| Interest, D1 | 12.5 | 28.1, 27.1 | 28.1, 27.0 | 28.3 | 28.5 |
| Interest, D2 | 12.6 | 28.6, 27.6 | 28.9, 27.0 | 28.3 | 28.5 |
| Interest, D3 | 14.2 | 28.2, 27.5 | 27.6, 27.1 | 28.3 | 28.5 |
| Interest, D4 | 12.4 | 28.1, 27.3 | 27.0, 27.0 | 28.3 | 28.5 |
| Interest, PX (D1) | 11.1 | 26.3, 25.5 | 26.1, 24.6 | 25.9 | 28.5 |
| Dividends, D1 | 13.1 | 17.9, 17.6 | 17.4, 17.4 | 18.3 | 18.3 |
| Dividends, D2 | 13.1 | 18.0, 17.7 | 17.4, 17.3 | 18.3 | 18.3 |
| Dividends, D3 | 13.9 | 17.7, 17.4 | 17.3, 17.3 | 18.3 | 18.3 |
| Dividends, D4 | 13.0 | 17.9, 17.7 | 17.5, 17.6 | 18.3 | 18.3 |
| Dividends, PX (D1) | 11.3 | 16.1, 15.7 | 15.5, 16.0 | 15.9 | 18.3 |

With the settings fixed at the planted values and the label on, the
filled-in shares are 28.2 to 29.1% for interest (D2: 29.8 and 31.2) and
17.9 to 18.8% for dividends. Every run, with all its draws: scratch
`fake26/report.md` and `fake26/fills/summary.json`.

How to read it. On the main fake survey the two files hold the same kind
of returns, so the survey's true share equals the PUF pile's (28.3
against 28.5 for interest). Filling in closes almost all of the gap
between the survey's reported share (12.5%) and the PUF's (28.5%). With
the label on and learned settings, the filled-in share of interest
holders runs from 0.6 points above to 1.3 points below the survey's true
share, 0.8 points below on average, and the share of dividend holders
0.9 points below on average. With the label off the averages are 0.5 and
0.6 points below. In PX the
survey also holds non-filers, so its true share is lower than the PUF
pile's (25.9 against 28.5 for interest, 15.9 against 18.3 for
dividends). Filling in lands within 1.3 points of the survey's own
truth, not on the PUF's share: the method leaves in place most of the
2.6-point and 2.4-point gaps that come from different people, and fills
in the hidden holders.

Each file has, per survey row: `record_id`, `split`, `weight`; for each
item, the reported dollars and slice, the fake survey's true dollars
(for checking only), and `<item>_fill_slice_<k>` and `<item>_fill_<k>`
for draws k = 1 to 5. A reporter's filled-in amount is its reported
dollars over the dollar factor; a drawn slice's dollars are a PUF-pile
amount drawn from that slice. Files: `fake26/<run>/fills.parquet` for
the label-on runs, `fake26/fills/<variant>_<run>.parquet` for the
label-off runs (written there so the step-22 and step-24 folders are not
touched).

## What the results show

1. **Other income for returns with no interest comes out as well with the
   label on as with it off.** Every label-on run is within seed noise of
   the undistorted survey (+21 and +17): the learned arm averages +19
   over eight runs, against +17 with the label off and +36 for today's
   rule. The label-on runs sit about 2 points above the label-off ones on
   average and their two seeds lie closer together; at this sample size
   that difference is not distinguishable from noise.
2. **The count of hidden interest holders still comes out about right
   with the label on; the count of hidden dividend holders comes out
   short.** With learned settings and the label on, hidden interest
   holders are 0.2 to 1.4 points below the truth (about 0.9 points on
   average, against about 0.4 points with the label off). Hidden dividend
   holders are 1.0 to 1.3 points below a truth of 5 to 6%, about a fifth
   too few (label off: 0.6 to 1.1 points short). With the settings fixed
   at the planted values, both counts are right with the label on (D2's
   interest is 1.7 to 3.2 points high, as it was, less so, with the
   label off).
3. **The learned curve is as wrong at small amounts as before, and the
   interest count is closer than that error alone would give.** The
   learned curve gives 29 to 36% reporting at $50 against a planted 13%,
   the same with the label on and off, so the label is not what makes it
   too generous. With the label on, the fill-ins come from the
   survey-labelled belief, which trains only on survey rows. If training
   reached the point where the fill-ins and the belief agree, the number
   of true holders in each slice would be the survey's reporters in that
   slice divided by the curve's chance of reporting there, so a curve
   twice too generous at $50 would halve the hidden holders around $50.
   The interest count comes out only 0.9 points short on average. A
   likely reason, not tested: the PUF-labelled and survey-labelled
   beliefs share all the network's weights except the label token, so
   the survey-labelled belief stays near the PUF's where the survey's
   data do not push it away. If so, the PUF still corrects the count
   partly, as it did with the label off, but through shared weights
   rather than by design, and how much it does could change with network
   size or longer training. For dividends the curve is further off
   (13 to 50% at $50) and the count is a fifth short. Section 6 shows the
   resulting share of survey units holding each item after filling in.
4. **The PUF pile's own relationships are reproduced at least as well
   with the label on, and clearly better when the files hold different
   people.** On the main fake survey the two piles are random halves of
   the same returns, so blending them costs nothing and the label makes
   no visible difference (married share, wages by cell and the share with
   no wages all within a few tenths of a point, either way). With
   non-filers in the survey only, the label-off models put the PUF pile's
   married share 1.5 points low and its share with no wages 1.1 to 1.2
   points off, because they learn the PUF's relationships from PUF and
   survey rows blended together; the label-on models are as close as
   today's model (0.5 points). This is the cost decision 14 names, seen
   in the test.
5. **The drop rule is not needed with the label on.** Keeping it changes
   no measure beyond seed noise (no-interest gap +19 on average either
   way; hidden interest holders within 0.4 points; hidden dividend
   holders within 0.3 points), and it moved the learned dividend curve a
   little further from the plant in D2 and D3.
6. **Non-filers in the survey only (PX):** the label-on runs land at +3
   and 0 (settings fixed) and +1 and -1 (learned) for returns with no
   interest, against the undistorted survey's 0 and -2; the label-off
   runs spread wider (+6 to -5). The hidden-holder counts are close to
   the truth either way.

7. **The filled-in survey separates hidden holders from different
   people.** With non-filers in the survey only, filling in brings the
   survey's share holding interest to its own true 25.9%, not to the PUF
   pile's 28.5% (section 6). On the main fake survey, where the true
   shares are equal, it leaves 0.8 points of interest holders and 0.9
   points of dividend holders unfilled on average with the label on and
   learned settings, against 0.5 and 0.6 with the label off. In real data
   that remainder would be read as "the survey holds different people"
   when it is in fact missed hidden holders.

## Open questions

- **The learned reporting curve is wrong in shape at small amounts,**
  with the label on and off: 29 to 36% at $50 for interest against 13%,
  and for dividends anywhere from 11 to 55%. Not diagnosed. One
  candidate: the curve is fit only to whether each survey unit reports
  anything, not to which slice reporters land in, and the slice
  reporters land in carries most of the information about the shape (the
  survey's reported distribution is the PUF's distribution times the
  curve). Fitting the curve to the reporters' slices as well would test
  this. With the label on this matters more for the count of holders,
  since nothing else pins it.
- **Hidden dividend holders come out a fifth short** with learned
  settings and the label on (and somewhat short with it off). Whether a
  better-shaped curve fixes this is the same question as above.
- **The filled-in amounts for `23_train_latent.py`** (the real SCF): not
  added. That file trains with the label off; it will need the label-on
  change of this step before the real-SCF run is redone (decision 18 says
  step 23's N2 is to be rerun with the label), and the fill-in file can
  be written there at the same time. `26_label_on.py fills` shows the
  draw.
- **A bookkeeping oddity in step 22's score files:** the `clip` field of
  49 of the 50 `fake22/*/score.json` files reads [200000, inf], while
  the groups inside the same files were held at -$32,440 and $180,090
  (the 22_FAKE_SURVEY.md values, which the numbers confirm). The field
  is stale; the numbers are not affected.

## Revision history

- 2026-10-17: written; all label-on runs, two seeds each; the filled-in
  amounts added at John's request the same day.
