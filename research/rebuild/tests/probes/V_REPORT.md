---
title: "Test 4, probe V — a broad scorecard for whole PUF records generated from scratch"
role: report
workstream: rebuild
status: current
updated: 2026-10-11
sot: research/rebuild/tests/probes/SPEC.md
supersedes: []
superseded_by: null
---

# A broad scorecard for whole PUF records generated from scratch

## Summary

- **What was asked.** Probe AS made AGI the sum of its parts, and the top of
  the generated PUF came out right on AGI and its main items. Is the overall
  approach, in which one model generates whole records from scratch, now
  viable? This probe scored AS's generated records on six kinds of checks.
  The same checks were run on test 3's model and on probe BL's tree
  synthesizer, with two references for scale.
- **Tax liability is close.** The checks covered every variable, joint
  relationships, impossible records, tax liability by AGI group, a
  classifier, and copying. Under a simplified 2015 income tax, AS's records
  owe $1,126bn of net income tax against $1,095bn on the real PUF records,
  and the top 1% pays 40.1% of it against 39.7%. Tax-Calculator gives
  $1,190bn against $1,158bn. The largest gap in the average tax rate in any
  AGI group is about 2.2–2.4 points. It sits in the top 0.1%, where the rate
  is 30.2% against 28.0%. The bottom half is next, at about 1 point.
  Test 3's model is much further off: under Tax-Calculator its top 0.1% pays $122bn against $217bn,
  because its drawn AGI disagrees with its income items.
- **A simple baseline does better on almost every check.** Probe BL's
  sequential tree synthesizer (unweighted arm) gets total tax within about 2% and
  every group's average rate within 0.1 points. A classifier can barely tell
  its records from real ones. The area under the ROC curve is the chance
  that the classifier ranks a random generated record as more likely
  generated than a random real one, so 0.5 means it cannot tell them apart.
  It is 0.53 for BL, against 0.81 for AS and 0.98 for test 3.
- **Part of AS's gap comes from the tokenizer.** Take the real training
  records, encode them into the model's rank bands and decode them back to
  dollars, with no model involved. Those records already score 0.72 on the
  classifier. About 10.7% of them break a rule that real records never
  break, such as taxable pensions above total pensions. On the band codes
  themselves, where decoding plays no part, AS scores 0.68 and BL 0.52.
- **The largest problems, ranked by their effect on tax.**
  1. Model seeds disagree at the top. Top 0.1% tax ranges from $181bn to
     $236bn over 12 draws, against $204bn on the real records.
  2. Itemized deductions among high-income returns are too small. In the
     top 0.1% the six itemized amounts total $42bn against $94bn. Setting
     them to the real group totals lowers AS's tax by about $19bn, of which
     about $11bn is in the top 0.1%.
  3. The earned income credit in the bottom half is short, at $46.7bn
     against $53.4bn.
  4. Top records look too much like ordinary ones. Joint filers are 74%
     against 83%, and capital income items are held together too rarely.
- **Copying.** No sign of it. Real held-out records sit closer to the
  training records than AS's records do, and the share of AS's records with
  an exact twin among the training records matches the real rate (25.6%
  against 26.7%).
- **Nothing is left running.** Every table below comes from finished jobs.

## What was run

All computations ran in Slurm on 11 October between 00:13 and 00:40. Code
is in this folder and outputs are in
`/nfs/roberts/scratch/pi_nrs36/jar335/probes/v/`. The full tables, which
hold weighted aggregates only, are in `V_TABLES.md` and
`V_TABLES_AS_ALL_DRAWS.md` there.

| Step | Script | Jobs |
|---|---|---|
| Scorecard for one file (any generated file in AS's format) | `v_score.py`, `v_cpu.sbatch` | first pass 29098264–366; tokens and copying 29098872–883, 29099344–365; BL 29098912–920; all AS draws 29099945–975 |
| Training records decoded from their own tokens | `v_roundtrip.py` | 29098864, scored by 29098883 |
| Tax-Calculator 6.8.4, installed in a virtual environment in scratch | `v_tcinstall.sbatch`, `v_taxcalc.py`, `v_tc.sbatch` | 29097813, 29098676, 29098921, 29099975 |
| Itemized-deduction counterfactual | `v_counterfactual.py` | 29099997 |
| Tables | `v_collect.py` | 29098927 (`V_TABLES.md`), 29099976 (`V_TABLES_AS_ALL_DRAWS.md`) |

**Files scored.** Every file has 664,624 generated records except the two
real references.

| Column in the tables | What it is |
|---|---|
| Real | The 166,156 PUF training records (120.7 million returns), the records every model was trained on |
| PUF validation | The 20,768 held-out real records, scored as if they were a generated file. Their misses show how far a real sample of that size lands from the training records. |
| Tokenizer alone | The training records encoded into the model's rank bands and decoded back to dollars. This is the best any model could do with this encoding. |
| AS | Probe AS's model with AGI as a sum, a fresh random order for each record, model seeds 1–4, sampling seed 1. `V_TABLES_AS_ALL_DRAWS.md` adds all 12 draws (sampling seeds 1–3), the size-first order (12 draws), and probe S's 25% recipe (6 draws). |
| Test 3, AGI first | Test 3's version 2 model, with AGI drawn first (probe O's files, model seeds 1–3). Test 3 with a random order is in the detailed tables. |
| BL | Probe BL's sequential tree synthesizer, two sampling seeds per arm. The summary shows the unweighted arm, which was the best. The weighted size-first arm overshoots the top, and all arms are in `V_TABLES.md`. |

**Conventions.**

- **Weights and groups.** Every statistic is weighted. AGI groups are the
  bottom 50%, 50–90, 90–99, 99–99.9 and the top 0.1%. Each file is grouped
  on its own AGI, using the real PUF's thresholds: $36,200, $133,700,
  $464,600 and $2.131m.
- **How AGI is taken.** For AS and BL, AGI is the 2015 Form 1040 sum of the
  parts plus the remainder (`as_agi.py`). Test 3 draws AGI itself. For test
  3 the "AGI remainder" is the implied one: drawn AGI less the parts.

**The six checks.**

1. **Every variable.** Share nonzero and total dollars, by AGI group, for
   the 30 dollar amounts, the remainder and AGI.
2. **Joint relationships.** Filing status, form of deduction, EITC
   children and child-credit children by AGI group. Co-holding: the share
   of returns holding both items of a pair, for all 28 pairs of wages,
   interest, dividends, capital gains, Schedule C, partnership and S
   corporation, total pensions and Social Security. Age range by AGI group
   in the bottom three groups only. Above the 99th percentile the real file
   has age for almost no returns, because the PUF's demographic file leaves
   them out.
3. **Rules real records never break.** The script searched the real
   training records for rules that hold on every one of them:
   - signs, such as an amount that is never negative;
   - orderings, such as one amount never above another;
   - "this amount nonzero means that one is nonzero";
   - "this filing status or deduction form means this amount is zero";
   - the highest AGI seen on any return with EITC children;
   - combinations of the six tax categoricals.

   It found 51 such rules. Each is checked on every file, with the real
   validation records as a test of how strict it is.
4. **Tax liability by AGI group.** Two calculators were used.
   - **Simplified 2015 calculator** (`v_score.tax2015`), run on each file's
     own AGI. It has brackets by filing status, standard deductions,
     exemptions with their phase-out, and itemized deductions with the
     medical floor, the 50% charity limit and the Pease limit. Itemized
     deductions are taken when the return itemizes and they exceed the
     standard deduction. It applies 0/15/20% rates to qualified dividends,
     net long-term gains and capital gain distributions, the child tax
     credit with its refundable part, and the EITC. It has no AMT, no net
     investment income tax, and no additional standard deduction for age.
   - **Tax-Calculator 6.8.4** (2015 law, `iitax`). It received the 30
     dollar amounts, filing status, exemptions, dependent status and the
     child counts. It did not receive the remainder, because it computes
     taxable Social Security itself and the remainder contains it. The head
     is age 40 on every return.

   Both calculators are run on the real training records with the same
   inputs and limits as the generated ones. This is D19's test: compare tax
   tables after running tax law.
5. **Telling generated from real.** A gradient-boosted tree classifier
   (scikit-learn's `HistGradientBoostingClassifier`) separates the real
   training records from up to 166,156 generated ones.
   - The two classes get equal total weight, and within each class records
     keep their weights.
   - The score is the out-of-fold weighted area under the ROC curve from
     five-fold cross-validation.
   - The features are the 37 variables (six categoricals, 30 amounts and
     the remainder) plus AGI. The three demographic codes are left out,
     because the real file lacks them for 42% of returns.
   - It was run once on all returns and once within each AGI group. A
     second version uses the model's rank-band codes instead of decoded
     dollars, which removes decoding noise.
   - Variable importance is the drop in the area under the curve when that
     variable is shuffled.
6. **Copying.** Each of the 37 variables plus AGI is replaced by its
   weighted rank (0 to 1) among the real training records. For 60,000
   generated records, plus every generated record in the top 0.1%, the
   script finds the Euclidean distance to the nearest training record and
   compares it with the same distance for the 20,768 real held-out records.
   An exact copy is a record at distance zero. A second test asks whether a
   record's full pattern of rank-band codes is identical to some training
   record's.

## Scorecard summary

Means over model seeds. The range over seeds is in `V_TABLES.md`. Dollar
amounts are weighted totals in $bn. "Points" are percentage points. For
reference, the real held-out records give an area under the curve of 0.50,
share nonzero and co-holding gaps of about 0.6 points, filing status gaps
of 0.35 points, and a rule-breaking share of 0.02%.

| Check | Real | Tokenizer alone | AS | Test 3, AGI first | BL, unweighted | Verdict for AS |
|---|---|---|---|---|---|---|
| Total AGI | 8,094 | 8,093 | 7,986 | 7,844 | 8,164 | Close |
| AGI in the top 0.1% group | 731 | 746 | 703 (range 624–778) | 554 | 755 | Right on average; unstable across seeds |
| 1. Sum of absolute dollar misses, 30 amounts × 5 groups | 0 | 114 | 799 | 2,831 | 269 | Much better than test 3; 3 times BL |
| 1. Mean gap in share nonzero (points) | 0 | 0.16 | 2.51 | 2.77 | 0.62 | Not improved: top records hold too few capital items, too many ordinary ones |
| 2. Filing status by group, mean gap (points) | 0 | 0.06 | 1.88 | 2.07 | 0.21 | Off at the top |
| 2. Joint filers, top 0.1% (%) | 82.5 | 82.7 | 74.4 | 73.7 | 82.3 | Too few |
| 2. Itemizers, top 0.1% (%) | 92.8 | 92.4 | 85.6 | 78.8 | 94.3 | Too few |
| 2. Co-holding of 28 pairs, mean gap (points) | 0 | 0.18 | 2.48 | 2.47 | 0.75 | Not improved; largest gaps at the top |
| 2. Any EITC children, bottom 50 (%) | 23.8 | 23.8 | 22.2 | 19.7 | 23.4 | Slightly short |
| 2. Age range, bottom three groups, mean gap (points) | 0 | 0.04 | 0.59 | 1.64 | 0.21 | Good: same as the held-out sample |
| 3. Returns breaking a rule real records never break (%) | 0 | 10.7 | 12.9 | 12.4 | 5.8 | Mostly the tokenizer |
| 3. Taxable pensions above total by more than 5% (%) | 0 | 3.0 | 3.7 | 3.4 | 0.5 | Mostly the tokenizer |
| 3. Qualified dividends above ordinary by more than 5% (%) | 0 | 2.5 | 3.5 | 3.4 | 0.4 | Mostly the tokenizer |
| 4. Net income tax, simplified calculator | 1,095 | 1,102 | 1,126 | 1,082 | 1,118 | +2.8% |
| 4. Same, top 0.1% group | 204 | 212 | 212 (range 191–236) | 187 | 212 | Right on average; unstable across seeds |
| 4. Top 1% share of net income tax (%) | 39.7 | 40.0 | 40.1 | 37.7 | 40.6 | Close |
| 4. Largest gap in average tax rate, any group (points) | 0 | 0.47 | 2.16 | 5.68 | 0.09 | Top 0.1% and bottom half |
| 4. Net income tax, Tax-Calculator | 1,158 | 1,166 | 1,190 | 1,451 | 1,182 | +2.8%; test 3 +25% |
| 4. Same, top 0.1% group | 217 | 226 | 225 | 122 | 225 | Close; test 3 half |
| 4. Largest gap in average tax rate, Tax-Calculator (points) | 0 | 0.69 | 2.42 | 14.1 | 0.10 | Top 0.1% and bottom half |
| 5. Area under the ROC curve, decoded dollars, all returns | 0.50 | 0.72 | 0.81 | 0.98 | 0.53 | Easy to tell apart; much better than test 3 |
| 5. Same, within the top 0.1% | 0.50 | 0.77 | 0.79 | 0.94 | 0.59 | Easy to tell apart |
| 5. Area under the ROC curve, rank-band codes, all returns | 0.50 | 0.50 by construction | 0.68 | 0.74 | 0.52 | The model's own gap, clear of decoding |
| 6. Exact twin among training records (%) (real held-out: 26.7) | — | — | 25.6 | 0.3 | 31.7 | Same as real records |
| 6. Same, top 0.1% (%) (real held-out: 0.06) | — | — | 0.22 | 0.00 | 0.02 | About one record in 450; no copying |
| 6. Median distance to nearest training record, top 0.1% (real held-out: 0.44) | — | — | 0.58 | 0.57 | 0.50 | Farther than real records: no copying |

The 12 draws of AS's random order (4 model seeds × 3 sampling seeds) give
the same picture:

- net income tax $1,117bn (range 1,071–1,152) under the simplified
  calculator and $1,180bn under Tax-Calculator;
- top 0.1% tax $203bn (181–236);
- an area under the curve of 0.81.

AS's size-first order is worse on dollar misses (1,237 against 794) and on
the classifier (0.83 against 0.81), and better on filing status. Probe S's
25% recipe overshoots the top 0.1%, at $227bn of tax, though its largest
gap in the average rate is smaller, at 1.1 points
(`V_TABLES_AS_ALL_DRAWS.md`).

## Detailed results

### 1. Every variable

The largest dollar misses for AS (generated less real, $bn, mean of four
model seeds):

| Variable | Group | Real | AS | Difference |
|---|---|---|---|---|
| Total pensions (E01500) | 90–99 | 310 | 258 | −52 |
| Total pensions (E01500) | 50–90 | 462 | 422 | −40 |
| Partnership and S corporation (E26270) | top 0.1 | 175 | 142 | −33 |
| Wages (E00200) | top 0.1 | 211 | 180 | −31 |
| State and local income or sales tax (E18400) | top 0.1 | 50 | 24 | −26 |
| Wages (E00200) | 99–99.9 | 455 | 431 | −24 |
| Partnership and S corporation (E26270) | bottom 50 | −31 | −8 | +23 |
| AGI remainder | 50–90 | 138 | 116 | −22 |
| AGI remainder | bottom 50 | −165 | −144 | +21 |
| Gross Social Security (E02400) | bottom 50 | 210 | 189 | −21 |
| Long-term gains (P23250) | bottom 50 | 20 | 4 | −15 |

The itemized deductions in the top 0.1% ($bn):

| | Real | Tokenizer alone | AS | Test 3, AGI first | BL, unweighted |
|---|---|---|---|---|---|
| State and local income or sales tax | 50 | 47 | 24 | 27 | 49 |
| Real estate tax | 4 | 4 | 3 | 3 | 5 |
| Interest paid | 6 | 5 | 3 | 3 | 6 |
| Cash charity | 17 | 16 | 7 | 8 | 18 |
| Noncash charity | 17 | 16 | 5 | 6 | 19 |
| Medical | 0 | 0 | 0 | 0 | 0 |

The tokenizer keeps them, so the shortfall comes from the model.

Share nonzero shows a consistent pattern at the top. Real against AS, in
the top 0.1%:

| Items | Real (%) | AS (%) |
|---|---|---|
| **Held too rarely** | | |
| Taxable interest | 98 | 89 |
| Tax-exempt interest | 64 | 46 |
| Short-term gains | 80 | 65 |
| Long-term gains | 91 | 82 |
| Ordinary dividends | 91 | 82 |
| Partnership and S corporation | 82 | 71 |
| Rent and royalty income | 45 | 35 |
| Cash charity | 89 | 76 |
| Interest paid | 74 | 63 |
| **Held too often** | | |
| Social Security | 18 | 26 |
| Taxable pensions | 17 | 23 |
| Unemployment compensation | 0.3 | 2.3 |
| Medical deduction | 0.1 | 6.2 |
| Student loan interest deduction | 0.0 | 1.7 |

The student loan interest deduction was fully phased out above $160,000
of income for joint filers in 2015 ($80,000 for others), so any amount in
the top groups is an impossible record. The
rule search did not catch it, because it looked only for rules tied to
filing status and deduction form. The same pattern, weaker, holds in the
99th to 99.9th percentiles. Generated top records look too much like
typical records. Below the 90th percentile, shares nonzero are within about
1–2 points.

### 2. Joint relationships

Filing status and itemizing, top two groups (% of group):

| | Real | AS | Test 3, AGI first | BL, unweighted |
|---|---|---|---|---|
| Single, 99–99.9 | 11.1 | 14.5 | 12.8 | 11.5 |
| Joint, 99–99.9 | 85.5 | 80.2 | 81.7 | 84.8 |
| Single, top 0.1 | 12.7 | 18.6 | 18.5 | 12.5 |
| Joint, top 0.1 | 82.5 | 74.4 | 73.7 | 82.3 |
| Itemizers, 90–99 | 87.7 | 82.7 | 90.6 | 86.6 |
| Itemizers, 99–99.9 | 92.6 | 88.0 | 91.8 | 92.5 |
| Itemizers, top 0.1 | 92.8 | 85.6 | 78.8 | 94.3 |

AS's largest co-holding gaps are all at the top: share of the group
holding both items, real against AS.

| Pair | Group | Real (%) | AS (%) |
|---|---|---|---|
| Interest and partnership | top 0.1 | 81.1 | 63.9 |
| Dividends and partnership | top 0.1 | 75.5 | 59.2 |
| Capital gains and partnership | top 0.1 | 77.0 | 61.6 |
| Interest and capital gains | top 0.1 | 92.3 | 77.6 |
| Interest and dividends | top 0.1 | 90.6 | 76.1 |
| Interest and capital gains | 99–99.9 | 75.1 | 63.3 |

EITC children by group (% of group with 0, 1, 2 or 3 children counted for
the EITC):

| Group | Real | AS | Test 3, AGI first |
|---|---|---|---|
| Bottom 50 | 76.2, 12.4, 8.0, 3.4 | 77.8, 11.5, 7.4, 3.3 | 80.3, 10.6, 6.4, 2.7 |
| 50–90 | 95.4, 1.3, 1.9, 1.4 | 94.2, 2.2, 2.2, 1.4 | 95.3, 1.6, 2.0, 1.2 |
| Above the 90th percentile | 100.0 | 99.1–99.4 | 97.8–99.8 |

Child-tax-credit children per return in the 50th to 90th percentiles are
0.473 (real 0.483). Age ranges in the bottom three groups match about as
well as a real held-out sample (a mean gap of 0.59 points against 0.57).

### 3. Rules real records never break

Weighted share of returns breaking each rule (%). Rules broken by under
0.05% in every model are not shown.

| Rule | Real held-out | Tokenizer alone | AS | Test 3, AGI first | BL, unweighted |
|---|---|---|---|---|---|
| Taxable pensions at most total pensions | 0 | 7.31 | 7.77 | 7.29 | 4.09 |
| Same, broken by more than 5% | 0 | 3.02 | 3.69 | 3.43 | 0.50 |
| Qualified dividends at most ordinary dividends | 0 | 3.82 | 4.51 | 4.52 | 1.65 |
| Same, broken by more than 5% | 0 | 2.53 | 3.50 | 3.42 | 0.37 |
| A combination of filing status, dependent status, exemptions, child counts and deduction form never seen in the real file | 0.02 | n/a | 0.93 | 1.04 | n/a |
| EITC children with AGI above $82,000, the highest on any real return with EITC children | 0 | n/a | 0.22 | 0.05 | n/a |
| Married filing separately with a student loan interest deduction (not allowed) | 0 | n/a | 0.14 | 0.16 | n/a |
| Itemizer with no itemized amount (real share 0.04%; not a rule) | 0.04 | n/a | 0.11 | 0.10 | n/a |
| Any of the 51 rules | 0.02 | 10.66 | 12.88 | 12.42 | 5.81 |
| Any rule, top 0.1% | 0 | 21.4 | 31.3 | 30.9 | 12.3 |

"n/a" marks numbers that are in `V_TABLES.md` but were not copied here.
Most rule-breaking comes from decoding. The tokenizer draws each amount at
random inside its rank band, independently of the others, so a fully
taxable pension (taxable equal to total) comes back with taxable above
total about half the time. D25 already plans to draw such a part together
with its complement. That would remove these breaks by construction.

### 4. Tax liability by AGI group

**Simplified 2015 calculator, net income tax after credits ($bn) and average
rate (net tax / AGI, %).**

| Group | Real | Tokenizer alone | AS | Test 3, AGI first | Test 3, random | BL, unweighted |
|---|---|---|---|---|---|---|
| Bottom 50 | −37 (−4.5%) | −37 (−4.5%) | −28 (−3.5%) | −18 (−2.2%) | −29 (−3.3%) | −36 (−4.4%) |
| 50–90 | 325 (9.5%) | 324 (9.5%) | 322 (9.6%) | 334 (9.6%) | 329 (9.7%) | 322 (9.5%) |
| 90–99 | 373 (16.6%) | 373 (16.6%) | 381 (17.0%) | 358 (16.5%) | 382 (16.8%) | 378 (16.7%) |
| 99–99.9 | 230 (26.4%) | 229 (26.3%) | 240 (27.1%) | 221 (26.6%) | 216 (27.3%) | 242 (26.3%) |
| Top 0.1 | 204 (28.0%) | 212 (28.5%) | 212 (30.2%) | 187 (33.7%) | 225 (34.4%) | 212 (28.1%) |
| All | 1,095 (13.5%) | 1,102 (13.6%) | 1,126 (14.1%) | 1,082 (13.8%) | 1,124 (14.1%) | 1,118 (13.7%) |

Earned income credit in the bottom half: real $53.4bn, AS $46.7bn, test 3
(AGI first) $39.8bn, BL $52.3bn. Child tax credit in the bottom half: real
$19.6bn, AS $18.7bn.

**Tax-Calculator, `iitax` ($bn) and average rate.** Tax-Calculator
includes the AMT and the net investment income tax.

| Group | Real | Tokenizer alone | AS | Test 3, AGI first | Test 3, random | BL, unweighted |
|---|---|---|---|---|---|---|
| Bottom 50 | −21 (−2.6%) | −22 (−2.7%) | −15 (−1.8%) | 92 (11.5%) | 17 (2.0%) | −21 (−2.5%) |
| 50–90 | 329 (9.6%) | 328 (9.6%) | 330 (9.9%) | 452 (13.0%) | 380 (11.2%) | 327 (9.6%) |
| 90–99 | 389 (17.3%) | 389 (17.3%) | 395 (17.6%) | 477 (22.0%) | 444 (19.6%) | 394 (17.4%) |
| 99–99.9 | 245 (28.0%) | 244 (28.0%) | 254 (28.7%) | 308 (37.0%) | 263 (33.2%) | 257 (28.0%) |
| Top 0.1 | 217 (29.7%) | 226 (30.4%) | 225 (32.1%) | 122 (22.0%) | 115 (17.7%) | 225 (29.8%) |
| All | 1,158 (14.3%) | 1,166 (14.4%) | 1,190 (14.9%) | 1,451 (18.5%) | 1,219 (15.3%) | 1,182 (14.5%) |

**The two calculators.**

- Tax-Calculator computes AGI from the income items, while the simplified
  calculator uses each file's AGI. For AS and the real records the two
  agree, so the two calculators tell the same story: AS overstates total
  tax by 2.8%.
- For test 3 they disagree sharply. Its drawn AGI places records in one
  group while their income items would place them in another. In the top
  0.1% by drawn AGI, the items carry half the tax. Below the top, the items
  add up to far more than the drawn AGI.
- Tax-Calculator was given each record's items as generated. For AS (model
  seed 1), 4.4% of returns had qualified dividends above ordinary dividends and 7.6% had
  taxable pensions above total pensions; these were set equal so that
  Tax-Calculator would accept them.

**How much of AS's tax gap comes from itemized deductions.** Within each
AGI group, each of the six itemized amounts was scaled so its group total
equals the real one. The simplified tax was then recomputed (job 29099997,
four AS seeds, $bn).

| Group | Real | AS as generated | AS, itemized deductions set to real group totals |
|---|---|---|---|
| Bottom 50 | −37.2 | −28.3 | −27.5 |
| 50–90 | 325.1 | 322.0 | 322.0 |
| 90–99 | 372.7 | 380.7 | 376.0 |
| 99–99.9 | 230.1 | 240.0 | 236.5 |
| Top 0.1 | 204.5 | 211.8 | 200.5 |
| All | 1,095.2 | 1,126.2 | 1,107.5 |

The itemized deductions account for about $19bn of the $31bn overstatement.
Most of what remains is the bottom half, about $10bn, where the earned
income credit is short. The 99th to 99.9th percentiles add about $6bn.

### 5. Telling generated from real

Area under the ROC curve; the range over model seeds is narrow.

| Classifier | Real held-out | Tokenizer alone | AS | Test 3, AGI first | BL, unweighted |
|---|---|---|---|---|---|
| Decoded dollars, all returns | 0.50 | 0.72 | 0.81 | 0.98 | 0.53 |
| Bottom 50, within group | 0.51 | — | 0.77 | 0.98 | — |
| 90–99, within group | 0.51 | — | 0.91 | 0.97 | — |
| Top 0.1, within group | 0.51 | 0.77 | 0.79 | 0.94 | 0.59 |
| Rank-band codes, all returns | 0.50 | — | 0.68 | 0.74 | 0.52 |
| Rank-band codes, 99–99.9 | 0.50 | — | 0.81 | 0.85 | 0.56 |
| Rank-band codes, top 0.1 | 0.50 | — | 0.78 | 0.81 | 0.60 |

"—" marks numbers that are in `V_TABLES.md` but were not copied here.

**What the classifier relies on.**

- **Test 3.** It separates its records almost entirely on the implied AGI
  remainder, AGI less its parts, which shows how inconsistent drawn AGI
  was.
- **AS, decoded dollars.** It relies on the remainder and AGI, taxable
  interest, qualified and ordinary dividends, and Social Security. Within
  the top 0.1% it relies on state and local tax, with an importance of
  0.22, far ahead of the next.
- **Decoded training records.** It relies on taxable interest, the
  remainder and noncash charity: amounts whose real values bunch at
  particular dollar values that decoding smooths away.
- **AS, rank-band codes.** It relies on wages, the remainder, Social
  Security, qualified dividends and the pension pair.
- **BL.** No variable stands out on all returns: shuffling any one moves
  the area under the curve by under 0.03. Within the top 0.1%, tax-exempt
  interest or rent losses stand out in some arms.

### 6. Copying

| | Real held-out | AS | Test 3, AGI first | BL, unweighted |
|---|---|---|---|---|
| Median distance to the nearest training record | 0.053 | 0.085 | 0.129 | 0.044 |
| Exact twin (distance zero), all (%) | 26.7 | 25.6 | 0.3 | 31.7 |
| Exact twin, bottom 50 (%) | 43.0 | 40.6 | 0.6 | 49.8 |
| Exact twin, 50–90 (%) | 12.5 | 12.7 | 0.1 | 16.8 |
| Exact twin, top 0.1 (%) | 0.06 | 0.22 | 0.00 | 0.02 |
| Median distance, top 0.1 | 0.44 | 0.58 | 0.57 | 0.50 |
| Identical rank-band pattern, all (%) | 34.2 | 32.6 | 29.1 | 37.4 |
| Identical rank-band pattern, top 0.1 (%) | 0.06 | 0.2 | 0.0 | 0.0 |

**Exact twins are common in real data.** A quarter of real held-out returns
have one: simple returns whose few amounts sit at common reported values.
AS reproduces that rate and does not exceed it. In the top 0.1% the
generated records are farther from any training record than real held-out
records are. AS's 0.22% is about one record in 450 per file.

BL, which draws real values from the leaves of its trees, has somewhat more
exact twins than real records at the bottom (49.8% against 43.0%). It has
none at the top. Test 3 has almost no twins only because its drawn AGI
never exactly equals the sum of its parts.

## What it means for whether the approach is viable

1. **On the test that matters most, AS now passes roughly.** Run through
   2015 tax law, AS's records give total income tax within 3% of the real
   records under two calculators. They give the top 1% share of tax within
   half a point, and every group's average rate within about 2.4 points.
   Test 3's model failed this test badly: under Tax-Calculator its top
   0.1% paid half the real tax and its total was 25% too high. The AS
   change is what made whole-record generation usable for tax.
2. **It is not yet competitive with a standard baseline.** Probe BL's
   unweighted tree synthesizer, given the same 40 variables, beats AS on
   nearly every line:
   - dollar misses $269bn against $799bn;
   - co-holding gaps 0.75 against 2.48 points;
   - largest average-rate gap 0.1 against 2.2 points;
   - classifier 0.53 against 0.81.

   Taken on its own, this scorecard does not show that the transformer is
   the better generator of PUF records. The case for the transformer
   remains what the plan claims for it: one model that also completes SCF
   wealth and can be post-trained to targets. This probe does not test
   that.
3. **The encoding costs a fixed amount, and the model costs more.**
   Encoding and decoding alone makes records 0.72 distinguishable, breaks a
   real rule on 10.7% of returns, and moves the top 0.1% average tax rate
   by 0.5–0.7 points. On the rank-band codes, where decoding plays no part,
   AS is at 0.68 against BL's 0.52. So most of the gap to the baseline is
   the model. The tokenizer accounts for a smaller part.
4. **The top is right on average but not reliably.** Across 12 draws, top
   0.1% tax runs from $181bn to $236bn, a spread wider than any average
   bias, and each file holds only about 660 top-0.1% records. Post-training
   or reweighting to top-tail targets would pin the level, but not the
   composition problems below.

## The largest remaining problems, ranked by effect on tax liability

Effects are for AS's random order under the simplified calculator, with
the mean of four model seeds unless stated.

1. **Seed-to-seed spread in the top 0.1%.** Top 0.1% tax ranges over $55bn
   across 12 draws ($181–236bn; real $204bn). Any single file can be $20bn
   or more off in the group that pays 19% of the tax. Remedies to try:
   averaging several models, more records per file, or calibrating the
   top.
2. **Too-small itemized deductions for high-income returns.** In the top
   0.1% they total $42bn against $94bn: state and local tax $24bn against
   $50bn, charity $12bn against $34bn. Itemizers are too few there (86%
   against 93%). Setting them to the real group totals lowers tax by $19bn
   overall, $11bn in the top 0.1%. The tokenizer keeps these amounts, so
   the model loses them. D20's plan to treat non-itemizers' deductions as
   unknown, now being tried by probe C, may bear on this.
3. **Earned income credit short in the bottom half.** The credit is $46.7bn
   against $53.4bn. Returns with EITC children are 22.2% of the group
   against 23.8%. Some EITC-children records sit at incomes where real ones
   never do (0.22% of returns have EITC children with AGI above $82,000).
   Together with a smaller child credit, bottom-half net tax is about $9bn
   too high.
4. **Top records look too typical.** In the top 0.1%, capital income items
   are held too rarely and items typical of ordinary returns too often: see
   section 1. Co-holding of capital items is 12–17 points short, joint
   filers are 74% against 83%, and wages and partnership income are short
   ($180bn against $211bn; $142bn against $175bn). These move tax in
   opposite directions. Once the itemized deductions are set to the real
   totals, the top 0.1% average rate is 28.5% against 28.0%, so their net
   effect on the rate is small. Their effect on the level runs through
   top 0.1% AGI, which is $28bn short on average.
5. **Bounded pairs.** Taxable pensions above total pensions and qualified
   above ordinary dividends together affect about 12% of returns. Most of
   this comes from decoding rather than the model. The tax effect is small,
   because the excess amounts are small and Tax-Calculator caps them.
   D25's part-and-complement form removes it by construction.
6. **Impossible records not covered by the rule search.** For example,
   student loan interest deductions in the top groups (1.7–2.2% of
   returns), where the deduction is fully phased out. Small in dollars.
   Rules that depend on income levels were not searched.

## Unfinished and caveats

- **No calibration.** Every check is on raw generated files, before any
  post-training or reweighting. Most level problems, such as top 0.1% AGI
  and tax, would be targeted by calibration. Composition problems, such as
  co-holding, itemized deductions within the top and filing status, would
  not.
- **The simplified calculator is simplified.** It has no AMT and no net
  investment income tax, and its itemized deductions cover only the six
  model variables. Tax-Calculator also lacks the AGI remainder, and both
  set age aside. Real and generated records get identical treatment, so
  the comparisons are fair, but the levels are not SOI's.
- **Single files and seeds.** BL has two sampling seeds per arm. AS has
  four model seeds, and twelve draws in `V_TABLES_AS_ALL_DRAWS.md`.
- **Reuse.** `v_score.py` takes any file in AS's format. Probe C has
  already queued it on its own generated files, which are its own jobs.

## To finish

Nothing of probe V is still running. All tables above come from finished
jobs (job list in `/nfs/roberts/scratch/pi_nrs36/jar335/probes/v/jobs_main.txt`).

| Job | Writes | Fills |
|---|---|---|
| 29098927 (collector, finished; rerun by 29099978 after a fix to the age row) | `/nfs/roberts/scratch/pi_nrs36/jar335/probes/v/V_TABLES.md` | Scorecard summary and sections 1–6 |
| 29099976 (collector, finished) | `.../v/V_TABLES_AS_ALL_DRAWS.md` | The 12-draw figures in the summary |
| 29099997 (finished) | `.../v/counterfactual_itemized.json` | Section 4, itemized counterfactual |

To score another generated file:

```
sbatch v_cpu.sbatch v_score.py --gen <file.parquet> --tokens <tokens.npz | columns> \
  --meta-dir <its data folder> --label "<model> seed <k>" --out <dir>/<name>.json
sbatch v_tc.sbatch --out <dir>/taxcalc.json <Label_with_underscores_seed_k>=<file.parquet>
sbatch v_cpu.sbatch v_collect.py <dir> <dir>/TABLES.md
```

Every computation ran in Slurm. All figures in this report are weighted
aggregates, and no record-level rows were written outside scratch.
