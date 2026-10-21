---
title: "Step 27: the reporting rule learned in pretraining with the label on, capital gains in four pieces, four SCF waves"
role: evidence
workstream: rebuild
status: current
updated: 2026-10-17
sot: research/rebuild/design/plan.md
supersedes: []
superseded_by: null
---

# Step 27: results

Spec: `27_MAPPING_SPEC.md`, with John's sign-off of 17 October (wages
assumed correct; capital gains as four drawn pieces; no stop). Jobs, output
paths and how to rerun any step: `27_STATUS.md`. Every table below is in
scratch `step27/summary.md` and `step27/report.md`
(`/nfs/roberts/scratch/pi_nrs36/jar335/minimal/step27/`), written by the last
jobs of the chain.

## What ran and what did not

Everything in the order of work ran to the end. Nothing is pending.

1. **Tables.** Table E27: the 2015 PUF with capital gains in four pieces, the
   crude 2017 CPS non-filers of step 23, and the SCF waves of 2013, 2016,
   2019 and 2022. The 2013 wave's raw files were on disk and it was built
   with the same R code as the other waves. AGI adds up to the cent on
   all 207,692 PUF records (the PUF's amounts are whole dollars; checked as
   integers, no record off). New dollar maps in `map/V27/`.
2. **Fake-survey test of the capital gains split** (decision 18, test 2).
   Ran, twice: once with the first version of the settings fit and once with
   the corrected one (below).
3. **Real runs, three seeds each**: R0 (today's rule: SCF dollars as
   reported, label on, all four waves); the spec's R1 (new method, 2016 wave
   only) as R1JAB; the spec's R2 (new method, all four waves) as R2JAB; and
   R2J, the new method with family A only. Each was generated, tilted to
   2019 and reported.
4. **The tilt with the capital gains targets restated** for the pieces ran
   for every run, beside the usual 136-target tilt.

What went differently from plan, in the order it happened:

- **The first version of the settings fit failed the fake test.** As in
  steps 22, 23 and 26, I first fit the S-curve on whether each unit reports
  anything and the dollar factor separately. On the real SCF the dollar
  factor for interest and dividends ran to the bottom of its search range
  (0.5), and on the fake survey, where the planted factor is 1.00, it did
  the same (interest 0.50, dividends 0.71) and the curve came out far too
  steep (1% reporting at $50 against 16% planted). The one difference I
  found from step 26, where the same fit worked, is that the belief the
  settings are fit against now holds only married, dependents and wages,
  while step 26's also held capital gains; I did not test whether that is
  the cause. I added a second version, which fits
  the S-curve on the chance of exactly what each unit reported (zero, or
  the slice it reported) — the score of spec section 3 — so the curve and
  the dollar factor climb one likelihood. On the fake survey it recovers
  the planted settings (section 2). **The results below use the second
  version** (runs named with J). The first-version runs (R1, R2, R1AB,
  R2AB, R2W, R2P) finished and are kept in scratch and in `report.md`, but
  are not used.
- I ran the spec's method (both families, probability updated each pass) as
  R2JAB and R1JAB, and family A alone as R2J. Family B wins the held-back
  score for interest and dividends, but when it is used to fill in it does
  not settle: in three of six runs one item's filled-in holders went to an
  impossible level (section 3.4). Family A alone (R2J) is stable across
  seeds. Both are reported.
- One extra diagnostic run, R2PJ (one seed), fills in from the PUF-labelled
  belief instead of the SCF-labelled one.

## Departures from the spec

1. **The SCF's capital gains is one slot, the line-13 total; the four pieces
   are PUF-only slots.** On SCF rows the model holds the after-limit total
   (all four pieces added up), filled in through the reporting rule; the
   pieces themselves are absent there. The spec (section 4, step 2) wanted
   the pieces drawn on SCF rows so that they add up to the mapped total;
   that was not built. At generation the pieces are drawn under the PUF
   label, the total is computed from them (never drawn), and net worth is
   then drawn under the SCF label given that total. The fake test checks
   the split the other way round: the survey households' pieces drawn under
   the PUF label given their filled-in total.
2. **The two-basis mix of capital gains is not learned in training.** On
   E27's slices, a reported loss of $3,000 and a reported loss of $20,000
   fall in the same slice (the PUF's largest-loss slice holds the $3,000
   limit), so reading every SCF report on the after-limit basis, with a
   loss beyond $3,000 set to −$3,000, loses nothing at slice level. The
   share of households on each basis is estimated outside training, in
   closed form (section 3.3).
3. **Family B in the first-version runs R1 and R2** was scored only after
   training. In R1JAB and R2JAB it is in training as the spec says.
4. **Reporters' true amounts under family A** are their reported dollars
   over the dollar factor, one slice, as in step 23; they are not drawn.
5. **The tilt.** The 136 targets are unchanged in the main tilt; their
   capital gains target is the Schedule D net figure, which the file now
   gives as kg_st + kg_lt + kg_other, and the distributions reported
   without a Schedule D (kg_dist) count in other income, as on every earlier
   table. The second tilt ("restated") adds the distributions as their own
   count and amount in each AGI bracket (166 IRS targets). Short-term,
   long-term and the residual have no line of their own in SOI Table 1.4,
   so they are not targeted separately.
6. **Net worth year token**: 2015 (the 2016 wave) only; the other tokens
   were not tried beside it.
7. **Dollar draws.** The total's slice used for the net worth draw is
   computed from one within-slice dollar draw of each piece; the tilt then
   draws the pieces' dollars again from the maps. Same slices, different
   dollars within them.
8. **Maps** come from the whole PUF side (filers and crude non-filers);
   step 23's map/V4 used filers only.
9. One restated tilt of a first-version run (R2s3) missed the solver's
   tolerance by 1.5 × 10⁻⁷ percent and stopped; not rerun.

## 1. Tables

**The pieces.** Every capital gains field of the file was read (E01000,
E01100, E01200, P22250, P23250); none has missing values. kg_st = P22250,
kg_lt = P23250, kg_dist = E01100, kg_other = E01000 − P22250 − P23250. Form
4797 (E01200) is in other income. Against SOI 2015 (PUF filers, 150.8
million returns):

| Item | PUF returns with it (millions) | IRS (millions) | PUF amount ($ billion) | IRS ($ billion) |
|---|---:|---:|---:|---:|
| kg_dist (E01100) | 4.37 | 4.32 | 11.8 | 11.6 |
| kg_st + kg_lt + kg_other (E01000, Schedule D net) | 20.11 | 19.95 | 614.7 | 695.0 |
| Line 13, all four pieces | 24.48 | 24.28 | 626.5 | 706.5 |
| kg_st alone | 10.84 | no line | −42.7 | |
| kg_lt alone | 16.37 | no line | 637.1 | |
| kg_other alone | 9.90 | no line | 20.3 | |

The PUF is $80 billion short of the IRS on Schedule D net, as found in step
28; the counts agree within 1%.

**The E01000 conflict.** E01000 equals the two Schedule D lines after the
loss limit ($3,000; $1,500 for married filing separately) on 143.0 million
of 150.8 million returns. It differs by more than $1 on 7.9 million: 2.0
million have no Schedule D amounts but a nonzero E01000, 4.2 million have
both nonzero with the same sign, 1.7 million opposite signs; 3.4 million of
the conflicts have E01000 at the loss limit. None has E01100. kg_other is
nonzero on 9.9 million returns and adds $20.3 billion: +$72.2 billion on the
2.0 million returns where the limit binds, −$51.9 billion on the conflicts.
I did not find the cause. (Step 28 counted 4.5 million conflicts with a
different test; I did not reconcile the two counts.) Under John's sign-off
the conflict sits in kg_other.

**SCF waves.** 2013: 33,058 unit rows, 6,015 households, 135.6 million
units (its ids overlapped other waves' and were offset). Wages, interest,
dividends and net worth slices equal table Y1's on Y1's rows (checked).
SCF capital gains slices equal Y1's on 84.5% of rows; the rest moved
because the PUF's capital gains is now line 13, which changes the cut
points. No missing values outside the slots a source does not have.

**IRS counts** for the per-wave check were added for tax years 2012, 2018
and 2021 from the SOI Table 1.4 file (`step27/irs_counts.csv`).

## 2. Fake-survey test

Half of the PUF filers (drawn by record) became a fake survey that reports
married, dependents and wages exactly; interest and dividends through
family A with planted settings (16% of holders report at $50, 80% at $500,
99% at $5,000; dollars exact); one capital gains number, on the
before-limit basis (kg_st + kg_lt + kg_dist) for a random 40% of households
and on the after-limit basis (line 13) for the rest, with the same chance of
reporting; and true other income in its own slot. The other half stays the
PUF. Files: `step27/fake_check.md` (first version), `fake_check_F27Js1.md`
(second version).

Learned settings (chance of reporting at $50, $500, $5,000; dollar factor):

| Item | Planted | First version | Second version |
|---|---|---|---|
| Interest | 16%, 80%, 99%; 1.00 | 1%, 99%, 100%; 0.50 | 16%, 78%, 98%; 0.93 |
| Dividends | 16%, 80%, 99%; 1.00 | 3%, 89%, 100%; 0.71 | 18%, 78%, 98%; 1.00 |
| Capital gains | 16%, 80%, 99%; 1.00 | 2%, 58%, 99%; 1.00 | 25%, 70%, 94%; 1.02 |

Hidden holders among the survey's zeros, model (mean of five fill-in draws)
against the truth: second version interest 15.4% (truth 19.1%), dividends
4.8% (6.3%), capital gains 2.8% (3.4%). The first version, despite its wrong
settings, gave 18.9%, 6.4% and 5.7%.

**The split.** For each survey household, the four pieces were drawn under
the PUF label given its married, dependents, wages and filled-in interest,
dividends and capital gains total. Second version, share of survey
households:

| Piece | Holding, drawn | Holding, truth | Drawn slice equals the true slice |
|---|---:|---:|---:|
| kg_st | 6.7% | 7.2% | 89.8% |
| kg_lt | 11.7% | 10.8% | 87.9% |
| kg_dist | 2.3% | 2.9% | 95.8% |
| kg_other | 4.4% | 6.5% | 91.8% |

The split comes back close in holding shares and slice by slice; kg_other
is under-drawn by a third.

**Other income on PUF returns** (the step 22 and 26 measure: the model's
mean other income for the PUF half, under the survey label, each amount held
between the survey's 1st and 99th percentiles, gap from the truth): today's
rule +15% over all returns and +33% for returns with no interest; second
version +6% and +11%; first version −0% and +6%. Step 26's label-on arms
were +1 to +4% and +15 to +23% on their own fake survey, so this run is at
least as good on this measure.

**The mix.** The closed form I first used — the share of households
reporting a loss of $3,000 or more who report more than $3,000 — gives 21%
on the fake survey against 40% planted. It is biased down because the PUF
has many returns at exactly −$3,000 whose Schedule D lines are not beyond
the limit (the conflict above), and those report −$3,000 on either basis. A
corrected closed form uses the PUF's own counts: with R the ratio of
reports beyond −$3,000 to reports at −$3,000, A the PUF returns with line
13 at −$3,000 (5.33 million) and B those with the Schedule D lines plus
distributions below −$3,000 (2.02 million), the before-limit share is
R·A / (B + R·A). On the fake survey it gives 41% against 40% planted.

## 3. Real runs

### 3.1 The settings sheet

| SCF variable | Type | Concept (PUF formula) | Mapping fit on |
|---|---|---|---|
| married | assumed correct | married | |
| dependents | assumed correct | dependents | |
| wages | assumed correct | E00200 | |
| interest | mapped | E00300 | married, dependents, wages; 2016 wave, 80% of households |
| dividends | mapped | E00600 | the same |
| capital gains | mapped | after the limit: kg_st + kg_lt + kg_dist + kg_other (= E01000 + E01100); a reported loss beyond $3,000 read as −$3,000 | the same |

Held back from fitting: 1,209 of the 2016 wave's 6,248 households (6,620
unit rows, all five copies together). The settings are used unchanged for
2013, 2019 and 2022.

### 3.2 The two families and family A's settings

Family A: holders report with a chance that rises with the amount, and
reporters give a fixed share of it (the dollar factor). Family B: the
survey household at a given percentile of its wave, zeros included, is the
PUF household at the same percentile. The score is the log of the chance a
family gives to exactly what each held-back household reported (summed over
households, natural logs, so a difference of 1 is a factor of 2.7 in the
chance). Second-version runs, three seeds each:

| Run | Item | Score A minus B | Probability B is right | A: report at $50, $500, $5,000 | A: dollar factor |
|---|---|---|---|---|---|
| R2J (A only, four waves) | interest | −72, −70, −66 | 1.000 in all three | 31%, 56%, 78% | 0.83 |
| | dividends | −58, −49, −62 | 1.000 | 47%, 64%, 78% | 0.90, 1.00, 0.90 |
| | capital gains | +1, −7, +1 | 0.24, 1.00, 0.23 | 30%, 43%, 57% | 1.46 |
| R2JAB (both families, four waves) | interest | −81, −78, −77 | 1.000 | 32%, 56%, 78% | 0.83 |
| | dividends | −63, −66, −55 | 1.000 | 48%, 65%, 79% | 0.90, 0.90, 1.00 |
| | capital gains | −6, −4, −9 | 0.996, 0.982, 1.000 | 31%, 43%, 56% | 1.46 |
| R1JAB (both families, 2016 wave only) | interest | −76, −82, −78 | 1.000 | 32%, 57%, 79% | 0.83 |
| | dividends | −62, −66, −51 | 1.000 | 48%, 66%, 80% | 0.90, 0.90, 1.00 |
| | capital gains | −1, −8, −9 | 0.74, 1.00, 1.00 | 32%, 44%, 56% | 1.46 |

- **B is right for interest and dividends** by 50 to 80 log points on 1,209
  households, in every run. For capital gains the two are close (within 9
  points), and the probability moves between seeds.
- **A's settings sit near no special case.** The chance of reporting rises
  with the amount but slowly: about a third of interest holders with $50
  report it, three in four with $5,000. It is neither exact reporting nor a
  sharp cutoff nor a flat chance.
- **The dollar factor is not pinned.** On the held-back households, for
  interest a factor of 1.0 scores 9 to 15 points better than the learned
  0.83; for dividends 1.0 scores 7 to 9 points and 1.1 scores 15 to 18
  points better than the learned 0.90 or 1.00; 0.8 scores 15 to 26 points
  worse for both. The grid search on the other 80% of households chose the
  lower values, so the choice does not carry over to the held-back
  households. For capital gains the learned 1.46 beats 1.0 by 27 to 30
  points and 1.1 by about 21; 0.8 and 0.9 are ruled out entirely (about
  −30,800), because with the factor below one no true amount can produce
  the largest reported slices.
- In the runs with both families, family B was drawn on every pass after
  the second for interest and dividends; for capital gains A was drawn on 18
  to 29 of the 78 passes.

### 3.3 The capital gains basis mix

In the 2016 wave, 0.93 million units report exactly −$3,000 and 0.70
million report a loss beyond it. The corrected closed form of section 2
puts the share answering on the before-limit basis at **67%** (R = 0.756;
the first closed form gives 43%). On the fake survey the corrected form was
within one point of the plant.

### 3.4 Holders by wave after filling in

Millions of units holding each item: as reported, after filling in (mean of
five draws and of the seeds), and the IRS count of returns for that tax
year. The SCF wave covers 136 to 148 million units, the IRS 145 to 161
million returns.

| Tax year | Item | SCF reporting | R2J after filling in | IRS returns with it | R2J / IRS |
|---|---|---:|---:|---:|---:|
| 2012 | interest | 22.0 | 46.5 | 48.0 | 0.97 |
| 2012 | dividends | 16.0 | 23.1 | 28.0 | 0.82 |
| 2012 | capital gains | 10.2 | 17.0 | 22.7 | 0.75 |
| 2015 | interest | 17.7 | 37.5 | 42.6 | 0.88 |
| 2015 | dividends | 16.8 | 22.7 | 27.6 | 0.82 |
| 2015 | capital gains | 11.7 | 19.3 | 24.3 | 0.79 |
| 2018 | interest | 18.9 | 37.9 | 46.6 | 0.81 |
| 2018 | dividends | 16.8 | 23.2 | 28.7 | 0.81 |
| 2018 | capital gains | 12.2 | 19.8 | 25.4 | 0.78 |
| 2021 | interest | 22.7 | 43.1 | 49.0 | 0.88 |
| 2021 | dividends | 23.0 | 32.2 | 32.2 | 1.00 |
| 2021 | capital gains | 18.4 | 32.4 | 33.1 | 0.98 |

As shares, after filling in the 2016 wave holds interest 26.9% of units,
dividends 16.3%, capital gains 13.8%; the PUF side (filers and crude
non-filers, 182 million units) holds 25.9%, 15.9% and 13.4%. So the fill-in
brings the SCF to the PUF side's shares, and what remains against the IRS
count (0.75 to 0.98 of it) is in part the difference in what is counted:
the 2016 wave has 140 million units and the IRS 150 million returns, so the
same share of holders gives fewer SCF units. Against the IRS as shares
(interest 28.3% of returns, dividends 18.3%, capital gains 16.1%) the filled
2016 wave is 1.4 to 2.3 points lower. No wave misses by much more than the others; 2012 capital gains
(0.75) is the lowest. No wave stands out as needing its own rule.

**With family B in training the fill-ins do not settle.** Holders after
filling in, 2015, per seed (interest, dividends, capital gains, millions):

| Run | Seed 1 | Seed 2 | Seed 3 |
|---|---|---|---|
| R2JAB | 25.5, 17.5, 14.8 | 19.0, 16.9, **139.6** | **96.8**, 17.5, 16.8 |
| R1JAB | **77.0**, 18.7, 15.9 | 24.2, 17.8, 15.0 | 24.2, 18.3, 16.6 |

Under family B, a survey zero is equally consistent with a true zero and
with any small amount whose percentile falls inside the survey's band of
zeros, so nothing in the score pulls a filled-in zero back toward zero. In
three of six runs one item drifted: in R2JAB seed 2 every SCF unit ends up a
capital gains holder, in R2JAB seed 3 the share of interest zeros filled as
holders rose pass by pass from 11% to 69%. Family A does not have this
problem because a holder of a small amount reports with some chance, so a
zero is evidence against holding. The first-version runs drifted for the
same reason on capital gains (their curve gave a 3% chance of reporting at
$50, close to none): R2 had 77 million capital gains holders after filling
in for 2015 against 24 million IRS returns.

The diagnostic run R2PJ (fill-ins from the PUF-labelled belief, which
cannot see net worth) gives 0.80 to 0.99 of the IRS counts, close to R2J.

### 3.5 The tilt to 2019

Means over seeds. The move is the distance the tilt moves the weights
(nats, natural-log units); records kept is the effective sample after the
tilt as a share of the stack.

| Run | Move, 136 IRS targets | Move, with the 8 wealth targets | Records kept (%) |
|---|---:|---:|---:|
| R0 (today's rule, label on, four waves) | 0.0058 | 0.0449 | 90.8 |
| R2J (family A, four waves) | 0.0067 | 0.0219 | 94.8 |
| R2JAB (both families, four waves) | 0.0065 | 0.0504 | 89.5 |
| R1JAB (both families, 2016 wave) | 0.0063 | 0.0229 | 95.1 |
| R2PJ (diagnostic, one seed) | 0.0069 | 0.0182 | 95.9 |
| N1, step 23 (label off, crude non-filers) | 0.0057 | 0.0323 | 92.7 |
| N2, step 23 (label off, latent amounts) | 0.0059 | 0.0223 | 95.1 |

With the targets restated (166 IRS targets) every number in this table
moves by 0.0002 or less, and records kept by less than 0.1 point.

The move to the IRS targets is about the same in every run. The move to
the wealth targets halves from R0 to R2J (0.045 to 0.022), level with step
23's N2. R2JAB's (0.050) is slightly larger than R0's, in line with its
unstable fill-ins.

### 3.6 Net worth

Nominal 2019 dollars, all units (filers and crude non-filers, 187 million);
the SCF column is the 2019 wave's 143 million units, so its "none of
interest, dividends, capital gains" share is on the SCF's own reports and
is not comparable to the files'. Means of three seeds.

| | SCF 2019 | R0 before | R0 after | R2J before | R2J after | R2JAB after | R1JAB after |
|---|---:|---:|---:|---:|---:|---:|---:|
| Total net worth ($ trillion) | 96.1 | 114.3 | 96.1 | 91.4 | 96.1 | 96.1 | 96.1 |
| Median ($) | 91,050 | 76,429 | 39,562 | 55,381 | 38,486 | 35,630 | 37,613 |
| Top 1% share (%) | 38.4 | 38.0 | 42.7 | 42.0 | 42.8 | 42.7 | 42.7 |
| Mean, no wages ($) | 760,852 | 920,186 | 789,532 | 735,164 | 782,138 | 797,135 | 798,241 |
| Mean, wages $1-25k ($) | 222,027 | 243,996 | 182,108 | 196,845 | 193,145 | 186,374 | 174,552 |
| Mean, wages $25-50k ($) | 153,683 | 237,144 | 171,953 | 181,146 | 165,579 | 153,011 | 151,780 |
| Mean, wages $50-100k ($) | 331,640 | 415,493 | 309,340 | 348,446 | 331,116 | 323,136 | 349,812 |
| Mean, wages $100-200k ($) | 839,734 | 904,020 | 773,577 | 727,949 | 773,454 | 768,587 | 786,282 |
| Mean, wages $200k+ ($) | 4,363,409 | 3,776,479 | 3,741,249 | 3,214,602 | 3,600,429 | 3,654,002 | 3,604,380 |
| Share with none of interest, dividends, capital gains (%) | 80.0 | 68.3 | 67.4 | 69.2 | 67.3 | 66.1 | 67.2 |
| Mean, none of the three ($) | 220,873 | 150,424 | 85,192 | 110,123 | 83,915 | 68,760 | 77,402 |

Before the tilt, R2J's total ($91.4 trillion) is near the target ($96.1
trillion), where R0 is $18 trillion over; R2J's means by wage group are
lower than R0's in every group, and closer to the SCF in three of the four
groups below $100,000 of wages (all but $1-25k). After the tilt the runs are
within 10% of each other in every wage group. Every run, as in step 23, has a median well below the
SCF's ($36,000 to $40,000 against $91,000) and a top 1% share above it
(42.7% against 38.4%): the tilt's wealth targets fix the levels at three
lines, not the shape between them. Units with none of the three items have
far less net worth after the tilt than before in every run.

### 3.7 The generated PUF variables

Before the tilt, share of units holding each item (PUF side of E27 in
brackets): wages 74.0 to 75.1% (74.1), interest 24.3 to 26.4% (25.9),
dividends 14.6 to 16.1% (15.9), kg_st 5.4 to 6.1% (6.0), kg_lt 8.2 to 9.2%
(9.0), kg_dist 2.1 to 2.5% (2.4), kg_other 5.0 to 5.6% (5.4), line 13 12.4
to 13.7% (13.4), other income 58.7 to 60.4% (59.6), across all R0, R2J,
R2JAB and R1JAB seeds. Every share is within 1.6 points of the PUF side in
every run; the new method does not disturb the PUF side. The filer share is 84.3% in every run against
82.9% on the PUF side.

## What this leaves open

- Family B fits what the SCF reports better than family A for interest and
  dividends, but as a rule for filling in zeros it is not identified and
  drifted in half the runs. Whether to keep A for filling in and B only as a
  description, or to add to B something that ties survey zeros to PUF
  zeros, is a design question for John.
- The dollar factor is not pinned for interest and dividends (1.0 scores as
  well as or better than the learned 0.83 and 0.90 on held-back households).
- The capital gains mix is estimated outside training; the corrected
  estimate is 67% on the before-limit basis.
- The E01000 conflict's cause is still not found; it sits in kg_other.

## Code (new files, and options added with the old behaviour as default)

New: `27_scf2013.sbatch`, `27_tables.py`, `27_train.py`, `27_score.py`,
`27_fake.py`, `27_report.py`, `27_summary.py`, `27_run.sbatch`,
`27_tilt.sbatch`, `27_cpu.sbatch`, `27_STATUS.md`, this file.

Options added, each unset by default so earlier runs are unchanged:
`02_model.py` reads the four pieces as PUF-owned slots only when the table
has them; `03_generate.py` `MINIMAL_COMPUTED_TOTAL=1` (line 13 computed from
the pieces at generation); `03_stack.py` `MINIMAL_STACK_CG_STRATIFIER`
(stratify the second file on kg_lt); `04_dollars.py` `MINIMAL_KG_PIECES=1`
(piece dollars, and capital gains and other income put on the targets'
definitions); `soi_cells.py` `MINIMAL_KG_DIST_TARGETS=1` (distributions as
their own targets). `27_train.py` options: `MINIMAL_SETTINGS_FIT=joint`,
`MINIMAL_FAMILY_B=1`, `MINIMAL_FILL_FROM=puf`, `MINIMAL_FACTOR_GRID`,
`MINIMAL_SCF_YEARS`. No plan or design document was changed; nothing was
committed.
