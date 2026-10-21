---
title: "Step 31: the cell method, first full run — results"
role: evidence
workstream: rebuild
status: current
updated: 2026-10-18
sot: research/rebuild/design/cell_method.md
supersedes: []
superseded_by: null
---

# Step 31: the cell method, first full run

The question: run the method of `design/cell_method.md` as section 8 of
that note sets it out, on the real SCF and on step 29's fake survey, and
compare it with the three earlier versions. Run on 18 October 2026, the
same day the note was written. The conversation that led to it, and
John's decisions along the way, are in `31_DISCUSSION_2026-10-18.md`. Jobs
and how to resubmit them: `31_STATUS.md`.

## What ran

1. **Training.** One transformer (the model of `02_model.py`, 4 layers,
   width 128) on table E27: the 2015 PUF with capital gains in four pieces,
   the crude non-filers of step 23, and the SCF's 2013, 2016, 2019 and 2022
   waves. Source label on. 80 passes. Three seeds (Ks1, Ks2, Ks3). From
   pass 3, the SCF's zero-reporters of interest, dividends and capital gains
   are redrawn every pass with the note's method: the cell table from the
   2016 wave against the 2015 PUF sets how many hidden holders there are in
   each cell and size range; the model's chances, scaled to those counts,
   decide who. Code `31_train.py`, settings `31_settings_scf.json`.
2. **The file.** 400,000 PUF-style records per seed, net worth drawn
   under the SCF label with the 2016 wave's year (step 29's settings).
   Turned into 2019 dollars (`map/V27`).
3. **Reweighting to 2019.** The 136 IRS targets (met exactly) plus eight
   SCF wealth targets, twice: with the wealth counts on the SCF's own 143
   million units, and on the file's 187 million units.
4. **Comparisons**, all on the same table and the same file settings:
   - **zeros as zeros**: SCF zeros taken at face value (step 27, R0);
   - **step 27's curve**: one fitted curve for how the chance of reporting
     rises with the amount (step 27, R2J);
   - **step 29's counts by slice**: the same idea as the note, with the
     targets set slice by slice and no top line (step 29, C4).
   Their files already existed; they were reweighted on the file-units base
   here so all four compare on both bases.
5. **The fake survey** (step 29's table F29: half the PUF dressed up as a
   survey, with hidden holders planted at known rates), two seeds, scored
   with step 29's check.
6. **The combinations check** (`31_combos.py`): the share of households
   holding each combination of the three items, cell by cell.

## 1. The fake survey: does it find the right households?

The fake survey is the only place where the truth is known.

Hidden holders found, as a share of the fake survey's zeros:

| | Truth | Cell method, seed 1 | Seed 2 |
|---|---:|---:|---:|
| Interest | 19.3% | 19.0% | 18.9% |
| Dividends | 6.5% | 6.0% | 6.0% |
| Capital gains | 3.1% | 2.5% | 2.4% |

Hidden dollars per zero-reporter:

| | Truth | Cell method, seeds 1 / 2 | Step 29, counts by slice (C3), seeds 1 / 2 |
|---|---:|---:|---:|
| Interest | $19.87 | $23.33 / $22.94 | $67.50 / $63.78 |
| Dividends | $15.56 | $12.55 / $12.23 | $147.81 / $137.83 |
| Capital gains | $2.50 | −$0.99 / −$2.99 | $315.39 / $279.66 |

Step 29's version put 3 to more than 100 times too many dollars on hidden holders;
the cell method is close. Capital gains comes out slightly negative: the
filled-in small losses outweigh the small gains. Small in dollars, wrong
in sign; not looked into.

The downstream test. In the fake survey, "other income" plays the role net
worth plays in the real SCF: the variable only the survey has. How far
above the truth the model puts it for the PUF-half returns:

| Returns | Zeros as zeros | Step 27's curve | Step 29, C3 | Step 29, C6 | Cell method |
|---|---:|---:|---:|---:|---:|
| With no interest | +32.5% | +12.3% | +4.9% / +6.9% | +1.6% / +6.4% | +3.7% / +6.7% |
| With neither interest nor dividends | +32.6% | +14.4% | +6.8% / +7.7% | +2.4% / +7.3% | +4.3% / +8.0% |
| All | +16.0% | +4.1% | +1.1% / +1.9% | +0.2% / +2.8% | +1.5% / +3.3% |

The cell method removes about 80% to 90% of the error that taking zeros
at face value causes, in the same range as step 29's count methods (whose
seeds vary about as much), and better than the fitted curves.

## 2. The real SCF: net worth on the file

Nominal 2019 dollars. Mean of three seeds; the range over seeds in
brackets where it matters.

Before reweighting:

| | SCF 2019 | Cell method | Step 29, C4 | Step 27's curve | Zeros as zeros |
|---|---:|---:|---:|---:|---:|
| Total net worth | $96.1T | $96.7T (92.2–102.4) | $92.5T (91.3–94.6) | $91.4T (82.3–96.8) | $114.3T (112.8–115.2) |
| Mean net worth, units with none of interest, dividends, gains | — | $102,500 (99,700–105,400) | $105,400 | $110,100 | $150,400 (148,400–152,000) |
| Top 1% share | 38.4% | 42.8% | 42.2% | 42.0% | 38.0% |

The SCF figure for the "none of the three" group ($220,900) is left out:
it counts the SCF's zeros as reported, so it is not comparable.

Total net worth: the mean of three seeds is within 1% of the SCF, but the
seeds range from $92 trillion to $102 trillion, so that closeness is
partly luck. What is robust: zeros as zeros overstates the total by about
$18 trillion in every seed, and every version that fills in hidden holders
removes most of it.

The units with none of the three items hold a third less wealth than under
zeros as zeros, in every seed. That group is what the method is for.

By wage group, cell method before reweighting against the SCF:

| Wages | SCF 2019 | Cell method |
|---|---:|---:|
| none | $761,000 | $785,000 |
| $1–25k | $222,000 | $201,000 |
| $25–50k | $154,000 | $188,000 |
| $50–100k | $332,000 | $324,000 |
| $100–200k | $840,000 | $784,000 |
| $200k and over | $4.36 million | $3.64 million |

Close everywhere except the top group, about $700,000 short, as in every
earlier version.

## 3. How much the reweighting has to move the file

The move is in nats (smaller: the file already fit the targets better).
"Records kept" is the effective share of records after reweighting.

| | Cell method | Step 29, C4 | Step 27's curve | Zeros as zeros |
|---|---:|---:|---:|---:|
| 136 IRS targets only | 0.0063 | 0.0069 | 0.0067 | 0.0058 |
| Plus 8 wealth targets, counts on the SCF's units | 0.0186 | 0.0186 | 0.0219 | 0.0449 |
| Records kept, same | 95.6% | 95.6% | 94.8% | 90.8% |
| Plus 8 wealth targets, counts on the file's units | 0.0447 | 0.0451 | 0.0450 | 0.0489 |

On the SCF-units base the cell method and C4 tie and need less than half
the move zeros as zeros needs. On the file-units base all four are close:
putting the counts on the file's 187 million units dominates the move.
After reweighting on that base every method has the SCF's median ($90,900)
and top 1% share (38.4%); on the SCF-units base the median is about
$38,000 for all four (the known count-base issue, plan decision 17).

## 4. Who the hidden holders are (2016 wave, 2015 dollars, median net worth)

| | Reporters | Hidden holders | Remaining zeros |
|---|---:|---:|---:|
| Interest | $739,000 | $357,000 | $26,000 |
| Dividends | $1,064,500 | $383,100 | $41,800 |
| Capital gains | $1,221,600 | $658,700 | $44,600 |

Hidden holders sit between reporters and the remaining zeros, for all
three items. Shares filled in, every wave and seed: about 19% of interest
zeros, 5.5% of dividend zeros, 7.5% of capital gains zeros. No run ran
away.

## 5. Holders and dollars after filling in

Share of SCF units holding each item after filling in, 2016 wave (tax
year 2015), against the IRS share of returns: interest 29.3% against
28.3%, dividends 16.9% against 18.3%, capital gains 15.2% against 16.1%
(the two bases, SCF units and returns, are not the same population).

Dollars per unit, 2016 wave: interest $662 reported, $688 filled in;
dividends $1,824 and $1,842; capital gains $4,941 and $4,921. Filling in
adds 4% to interest dollars, against 17% in step 30 before the top line.
The top line does what it was added for.

## 6. Combinations of items (`$R/step31/combos.md`)

The method targets each item on its own; which combination a household
ends up with is left to the model. All cells, 2016 wave, PUF weighted to
the SCF's mix of cells, % of units:

| | PUF | SCF as reported | SCF filled in |
|---|---:|---:|---:|
| None of the three | 63.6 | 79.7 | 66.9 |
| Both interest and dividends | 13.7 | 6.4 | 14.5 |
| Interest, no dividends | 16.4 | 6.2 | 14.7 |
| Dividends, no interest | 5.0 | 5.6 | 2.4 |

Filling in moves the SCF most of the way to the PUF. Two misses, the same
in every cell and for step 29's C4: about 3 points too many units with
none of the three (1 to 6 points by cell), and too few with dividends but
no interest (2.4% against 5.0%).

The reason. Among dividend holders, 73% hold interest on the PUF and 53%
report it in the SCF; among the rest, 20% and 7%. So interest is reported
at roughly 73% by households that report dividends and roughly 35% by the
rest: the households that report one item tend to report the others. The
method assumes the reporting rate of interest does not depend on whether
dividends were reported, so it reads the SCF's strong link as a link in
holding and gives hidden interest too readily to dividend reporters. The
"none of the three" group therefore probably still holds a little too much
wealth, though far less than under zeros as zeros. John accepted this
assumption before the run (`31_DISCUSSION_2026-10-18.md`, section 3); the
check puts its size at about 3 points. A fix, if wanted: split each item's
cells by whether the household reported the other items, so the PUF sets
those counts too. That is a design change and John's call.

## 7. Departures from the note

- A slice counts wholly in the size range holding its 2015 PUF median, so
  the four or five slices per item that straddle a range edge are not
  split. For married couples with wages of $75,000 to $200,000, interest,
  this gives 19.7, 7.0 and 1.7 points filled in under $100, $100–$500 and
  $500–$2,000, against the note's 19.6, 7.3 and 1.3.
- The cell table uses every 2016-wave SCF training row (step 29 held 20%
  back to score its curves).
- The scaling is refit every 10 batches while filling in, and after every
  pass, as in step 29.

## 8. What this does not show, and what is open

- No check on the real SCF that the right households were picked; only the
  fake survey, where the method's assumption holds by construction.
- The combinations miss above (about 3 points on "none of the three").
- Capital gains hidden dollars slightly negative on the fake survey.
- Whether the model memorises households and locks in its own fill-ins:
  not checked (the share filled in is steady, which does not settle it).
- When filling in starts (pass 3): never tested. Noted by John as a
  possible thing to look into, not to run now.
- Net worth was drawn under the 2016 wave's year for a 2019 file. A mix of
  years (for example 80% the 2019 wave, 20% the 2016 wave) is a
  post-training question, noted for later (`31_STATUS.md`).
- The top wage group is short by about $700,000 before reweighting, as
  before.

## Files

Code: `31_train.py`, `31_settings_scf.json`, `31_report.py`,
`31_combos.py`, `31_run.sbatch`, `31_tilt.sbatch`, `31_cpu.sbatch`,
`31_submit.sh`. Scratch (`/nfs/roberts/scratch/pi_nrs36/jar335/minimal/`):
models `model/step31/Ks<k>/` (with `fills.parquet`, five draws of every
SCF record's filled-in values) and `model/step31/F29Ks<k>/`; files
`gen/gen_puf_stack_s31_ks<k>.parquet`; reweighting `tilt/step31/`; report
`step31/report.md`; combinations `step31/combos.md`; fake-survey checks
`step29/fake/F29Ks<k>.md`.
