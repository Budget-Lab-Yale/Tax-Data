---
title: "Minimal example, step 21: priors for 2019 from several SCF waves"
role: evidence
workstream: rebuild
status: current
updated: 2026-10-16
sot: research/rebuild/design/plan.md
supersedes: []
superseded_by: null
---

# Step 21: priors for 2019 from several SCF waves

Asked by John on 16 October: the v4c setup trained on several SCF waves at
once, and a comparison of priors for 2019. It is the first step of the
plan's order of work after the minimal example ("more years") and the first
real test of the year token and the rolling prior (plan decision 3, next
test 6).

## What a prior is here

The model carries a year token. The SCF waves in training give three
tokens: 2015 (the 2016 survey), 2018 (the 2019 survey) and 2021 (the 2022
survey). The PUF is 2015 only. No wave is for tax year 2019, so a 2019 file
has to be drawn from some mix of the tokens. A prior is that mix: a weight
on each token. Each record of the 2019 file carries one token, drawn with
those weights. The prior is then tilted to the 2019 targets, exactly as
v4c is.

Income dollars do not depend on the token: every record gets 2019 dollars
from the PUF's 2015 cut points grown by the IRS growth in each item, as in
v4c. Net worth dollars do: net worth is cut within each wave, so a record's
net worth band is a position in its own token's wave, and its dollars come
from that wave, in 2015 dollars (the price basis of the wealth targets).

## The two models

| Run | Training data | Year tokens |
|---|---|---|
| V5A | 2015 PUF; SCF 2016, 2019 and 2022 waves (table Y1 of `10_RESULTS_YEARS.md`, the rule adopted as decision 15) | 2015, 2018, 2021 |
| V5B | The same without the 2019 wave (table Y1H) | 2015, 2021 |

Both: v4's network and settings (4 layers, width 128, all records but a 2%
held-back slice, 80 passes). V5B is the honest test of a prior: the wave
that defines the wealth targets and the comparison tables is not in
training, so a prior that does well there does well on a year it did not
see.

## The priors

| Name | Model | Weight on 2015 / 2018 / 2021 | What it is |
|---|---|---|---|
| p21_a15 | V5A | 1 / 0 / 0 | 2015 only: v4c's prior with more data in training |
| p21_a18 | V5A | 0 / 1 / 0 | the nearest wave only |
| p21_a21 | V5A | 0 / 0 / 1 | the later wave only |
| p21_anear | V5A | 0 / 2/3 / 1/3 | straight-line interpolation between the two waves around 2019 |
| p21_awide | V5A | 0.18 / 0.45 / 0.36 | a wider triangular kernel, six years each side: weight 1 − distance/6 |
| p21_aeq | V5A | 1/3 / 1/3 / 1/3 | all waves equally |
| p21_b15 | V5B | 1 / – / 0 | 2015 only |
| p21_b21 | V5B | 0 / – / 1 | 2021 only |
| p21_binterp | V5B | 1/3 / – / 2/3 | straight-line interpolation between 2015 and 2021 |
| p21_beq | V5B | 1/2 / – / 1/2 | both equally |

Reference: v4c (one wave, 2015 token), `tilt/v4c_scfbase/`.

## What is judged

Not the move alone (plan, next test 6: the smallest tilt can favour a prior
that is good only on targeted moments). For each prior: the move and the
effective sample kept; wealth before the tilt against the 2019 SCF; and the
relationships no target covers after the tilt (wealth by wage group, the
top 0.1% share, joint and dependents shares, holder shares).

## Code

- `21_train.sbatch <run> <table>`: trains V5A on Y1, V5B on Y1H (built from
  Y1 inside the job by dropping the 2019 wave's rows).
- `21_stack.sbatch <run> <table> <year>`: the v4c stack under one token
  (`03_stack.py` with the new `MINIMAL_STACK_YEAR`).
- `21_maps.py`: the per-wave net worth maps in `map/V5/`, 2015 dollars.
- `21_priors.py`: mixes the per-token stacks into one prior's starting file
  (a mixture of samples, each token's importance weights times its prior
  weight).
- `21_run_cpu.sbatch`: maps; prior (mix, then `07_baseline.py`); v4ccheck
  (the v4c stack tilted through the per-wave net worth path must reproduce
  `tilt/v4c_scfbase/`).
- Changes to shared code, each a setting whose default keeps earlier runs
  as they were: `03_oversample.py` and `03_stack.py` take a year token;
  `04_dollars.py` draws net worth from the record's own wave when
  `MINIMAL_NW_MAP_YEARS` is set.

## What ran (2026-10-16)

- Training: V5A (job 29743118; the first try, 29743012, hit a GPU memory
  error on node a1125u14n01) and V5B (29743013). Held-back slices: 4,124 PUF
  and 1,860 SCF rows (V5A), 4,124 and 1,200 (V5B).
- Stacks, one per model and token: V5B 2015 and 2021 (29743319, 29743320);
  V5A 2015, 2018, 2021 (29743322, 29743324, 29743325).
- Maps (job 29744131; the earlier map jobs stopped on the checks described here). The checks pass: the 2015 map
  matches v4c's except one SCF row, which sits on a cut point and lands one
  band lower in table Y1 (Y1 holds the 2016 wave in 2016 dollars, E4 in
  2015 dollars; rounding); the 2019 wave totals $89.14 trillion in 2015
  dollars, the target's total. Table E4 divided the 2016 wave's net worth by
  the exact factor 1.2493059 and the wealth targets by 1.2493; the two
  differ by 0.0005%.
- The v4c stack tilted again through the per-wave path (29744507,
  `tilt/v4c_nwcheck/`): move 0.0139299 nats against v4c's 0.0139298.
- Tilts: jobs 29744655 to 29744664, `tilt/p21_*/`. Tables: job 29744665,
  `checks/V5/report_tables.md` and `report_numbers.csv`.

One training run and one seed per model, so differences of a few percent
between columns are within noise: `20_SEPARATE_MODELS.md` measured about
0.1 in the ratio of total wealth to the SCF's between runs that differ only
in seed or GPU type.

## Results

Mean and median net worth are per unit, in 2015 dollars. The file has
157.8 million returns and the SCF 143.2 million units, so means are
compared, not totals.

**The three SCF waves themselves.** Mean net worth per unit $614,000
(2016 survey), $623,000 (2019), $752,000 (2022); median $74,700, $84,500,
$119,600. Wealth grew little from 2016 to 2019 and a great deal from 2019
to 2022. Halfway between the 2016 and 2022 waves is a mean of $683,000 and
a median of $97,100, well above the 2019 survey.

**Before the tilt, against the 2019 SCF:**

| Prior | Weights 2015 / 2018 / 2021 | Mean | Gap | Median | Earners above $200,000, mean | No wages, mean |
|---|---|---:|---:|---:|---:|---:|
| SCF 2019 | | $623,000 | | $84,500 | $4,500,000 | $706,000 |
| v4c (one wave) | 1 / – / – | $578,000 | −7% | $68,400 | $3,990,000 | $1,140,000 |
| A, 2015 only | 1 / 0 / 0 | $608,000 | −2% | $77,800 | $3,710,000 | $1,320,000 |
| A, 2018 only | 0 / 1 / 0 | $665,000 | +7% | $95,500 | $3,860,000 | $1,180,000 |
| A, 2021 only | 0 / 0 / 1 | $805,000 | +29% | $125,000 | $3,840,000 | $1,430,000 |
| A, two nearest | 0 / ⅔ / ⅓ | $712,000 | +14% | $105,000 | $3,840,000 | $1,270,000 |
| A, wide kernel | .18 / .45 / .36 | $710,000 | +14% | $102,000 | $3,870,000 | $1,300,000 |
| A, equal | ⅓ / ⅓ / ⅓ | $696,000 | +12% | $98,000 | $3,850,000 | $1,310,000 |
| B, 2015 only | 1 / – / 0 | $639,000 | +3% | $71,500 | $4,280,000 | $1,360,000 |
| B, 2021 only | 0 / – / 1 | $816,000 | +31% | $129,000 | $4,360,000 | $1,270,000 |
| B, interpolation | ⅓ / – / ⅔ | $761,000 | +22% | $106,000 | $4,390,000 | $1,300,000 |
| B, equal | ½ / – / ½ | $731,000 | +17% | $96,900 | $4,360,000 | $1,310,000 |

**The tilt.** Every prior hits the 136 IRS targets exactly and the eight
wealth targets.

| Prior | Move, 136 alone (nats) | Move, with wealth (nats) | Effective sample kept | Records cut below 0.1 | Untargeted income relationships, mean gap to the 2015 PUF (points) |
|---|---:|---:|---:|---:|---:|
| v4c | 0.0068 | 0.0139 | 97.1% | 0 | 0.7 |
| A, 2015 only | 0.0078 | 0.0209 | 95.9% | 4 | 1.1 |
| A, 2018 only | 0.0231 | 0.0406 | 92.5% | 9 | 1.1 |
| A, 2021 only | 0.0263 | 0.0795 | 85.8% | 89 | 1.9 |
| A, two nearest | 0.0220 | 0.0485 | 90.5% | 42 | 1.3 |
| A, wide kernel | 0.0176 | 0.0437 | 92.4% | 40 | 1.3 |
| A, equal | 0.0146 | 0.0388 | 92.8% | 37 | 1.2 |
| B, 2015 only | 0.0071 | 0.0189 | 96.1% | 3 | 1.0 |
| B, 2021 only | 0.0513 | 0.1139 | 80.9% | 1,167 | 2.4 |
| B, interpolation | 0.0296 | 0.0688 | 90.3% | 541 | 1.8 |
| B, equal | 0.0211 | 0.0514 | 90.6% | 52 | 1.6 |

**After the tilt, what no target covers.** Mean net worth of returns with
no wages stays at $1.15 to $1.24 million in every prior against the SCF's
$706,000; no mix of years moves it. Earners above $200,000 hold $3.47 to
$3.94 million against the SCF's $4.50 million (v4c: $4.00 million), lowest
under the 2021 token. The top 0.1% holds 15.1% to 15.7% (SCF 14.7%). The
median is $62,000 to $65,000 in every column, set by the count base (the
15 million extra returns placed below every line). The tilt hardly moves
weight between tokens: under B, interpolation, the 2015 token's share goes
from 33.3% to 35.9%.

## What it shows

1. **No multi-wave prior beat v4c for 2019.** v4c, trained on the 2016
   wave alone, has the smallest move (0.014 nats) and the closest
   untargeted income relationships (0.7 points). Among the new priors, the
   2015 token alone is best on every measure, in both models.
2. **On the honest test (B, the 2019 wave left out), the 2015 token alone
   is closest on the mean (+3%), and leaning on the 2022 wave overshoots.**
   Interpolating by distance in years puts two thirds of the weight on the
   2022 wave and starts 22% too rich. Wealth did not grow evenly: little
   from 2016 to 2019, a lot from 2019 to 2022. A prior that weights waves
   by distance in years cannot know that; the tilt has to undo it, at three
   to five times the cost, and leaves the top earners poorer than the SCF.
   On the median the picture is mixed: the 2015 token undershoots ($71,500)
   and the equal mix overshoots ($96,900) the SCF's $84,500.
3. **Even with the 2019 wave in training, its own token is not the best
   prior.** A, 2018 only starts 7% rich on the mean and 13% on the median,
   and moves 0.041 nats against 0.021 for the 2015 token. The PUF exists
   only for 2015, so a PUF-style record under any later token is a
   combination the model never saw; the later tokens carry the SCF's change
   over time into the PUF-style records by extrapolation.
4. **Adding waves made the 2015 token itself a little worse than v4c** on
   the move (0.021 against 0.014) and the untargeted relationships (1.1
   against 0.7 points), though better on the mean before the tilt (−2%
   against −7%). This is within the size of seed-to-seed noise measured
   before, and was not separated from it.
5. **The no-wage gap is not a year problem.** It is the same under every
   prior, which supports the nonfiler explanation in `15_BASELINE_V4.md`.

## Not done

- Seeds: one training run per model. A second seed of V5A and V5B would say
  how much of point 4 is noise.
- The year distance used is calendar years. Plan open item: year against
  observable state as the coordinate. These results are evidence for
  state: the waves differ by how wealth moved, not by how many years apart
  they are.
- The prior's weight could be fitted rather than chosen (plan, next test 6:
  stacking by held-out log-probability). Not tried.

## Second round: the year prior on net worth only (2026-10-16, afternoon)

John's rule (plan decision 3 as changed today): the year prior is per
variable, by the years the variable's owner has. The PUF variables are
drawn at 2015 in every stack; only net worth's draw (SCF label, other
income left out) takes the prior's year. The same two models, the same ten
weightings, named `p21n_*`. The "2015 only" priors must reproduce the first
round's `p21_a15` and `p21_b15` exactly (the same draws): a check of the new
code.

Code: `03_generate.py` (`borrowed['year']`, column `net_worth_year`),
`03_stack.py` (`MINIMAL_STACK_NW_YEAR`), `04_dollars.py` (net worth dollars
from `net_worth_year` when present), `21_priors.py` and `21_stack.sbatch`
(argument `nw`), `21_report.py` (`REPORT_SET=nw`).

Jobs: stacks 29750808 to 29750810, 29750839, 29750840; tilts 29750873 to
29750882; tables 29750883, `checks/V5/report_tables_nw.md`.

**The reproduction check did not hold exactly, for a known reason.** The
"2015 only" stacks of this round ran on different GPU types from the first
round's (A40 and RTX PRO 6000 against L40S), and the same seed gives
different draws on different GPU types. Total net worth before the tilt:
$98.0 trillion against $95.9 trillion (A), $99.1 trillion against $100.8
trillion (B). That is a measure of the noise from the draws alone, about 2%
on the total, on top of the larger noise from training.

### Results

Before the tilt, against the 2019 SCF (mean $623,000, median $84,500; per
unit, 2015 dollars), and the tilt:

| Prior on net worth's year | Mean | Gap | Median | Move, 136 alone (nats) | Move, with wealth (nats) | First round: mean gap, move with wealth |
|---|---:|---:|---:|---:|---:|---|
| v4c (one wave) | $578,000 | −7% | $68,400 | 0.0068 | 0.014 | |
| A, 2015 only | $621,000 | 0% | $78,300 | 0.0079 | 0.021 | −2%, 0.021 |
| A, 2018 only | $654,000 | +5% | $84,300 | 0.0078 | 0.024 | +7%, 0.041 |
| A, 2021 only | $758,000 | +22% | $114,000 | 0.0078 | 0.062 | +29%, 0.080 |
| A, two nearest (⅔ 2018) | $690,000 | +11% | $93,500 | 0.0077 | 0.034 | +14%, 0.049 |
| A, wide kernel | $692,000 | +11% | $92,900 | 0.0076 | 0.033 | +14%, 0.044 |
| A, equal | $683,000 | +10% | $91,100 | 0.0075 | 0.032 | +12%, 0.039 |
| B, 2015 only | $628,000 | +1% | $71,000 | 0.0071 | 0.019 | +3%, 0.019 |
| B, 2021 only | $817,000 | +31% | $111,000 | 0.0071 | 0.070 | +31%, 0.114 |
| B, interpolation (⅔ 2021) | $746,000 | +20% | $96,600 | 0.0069 | 0.047 | +22%, 0.069 |
| B, equal | $717,000 | +15% | $89,000 | 0.0069 | 0.038 | +17%, 0.051 |

After the tilt: returns with no wages hold $1.23 to $1.26 million in every
prior (SCF $706,000); earners above $200,000 hold $3.48 to $4.01 million
(SCF $4.50 million), highest under B, 2015 only. Untargeted income
relationships: mean gap 1.0 to 1.5 points against v4c's 0.7.

### What the second round shows

1. **The rule does what it was meant to.** The PUF variables no longer
   depend on the prior: the move for the 136 IRS targets alone is 0.0069
   to 0.0079 nats in every column, close to v4c's 0.0068 (first round:
   0.015 to 0.051).
2. **With the 2019 wave in training, drawing net worth under its own year
   now works.** A, 2018 only: median $84,300 against the SCF's $84,500,
   mean 5% high, move 0.024 (first round: $95,500, 7% high, 0.041).
3. **On the honest test (B), the conclusion of the first round stands.**
   The 2015 wave alone is closest on the mean (+1%) and has the smallest
   move; weighting by distance in years starts 15% to 20% rich, because
   wealth grew little from 2016 to 2019 and a lot from 2019 to 2022. On
   the median, half and half is closer ($89,000) than 2015 alone ($71,000).
4. **Unchanged:** the no-wage gap and the shortfall above $200,000 of
   wages do not depend on the year prior; v4c still reproduces the
   untargeted income relationships best, which is a property of the
   models trained on three waves (or their seed), not of the prior.

## Third round: every rank into 2019 dollars (2026-10-16, evening)

John's rule (plan decisions 3 and 10 as rewritten today): post-training
takes a target year, which picks the rank-to-dollar function for every
variable, and a year mix per variable. The second round had turned each
net worth rank into dollars with its own wave's values, which put the 2022
survey's wealth level into a 2019 file. Here every net worth rank is turned
into dollars with the 2019 survey's values (`MINIMAL_NW_TARGET_MAP_YEAR=2018`),
whatever wave it was drawn under. The stacks are the second round's; only
the dollars and the tilt change. Also v4c's stack under the same rule
("v4c, 2019 dollars"; for v4c this changes the net worth function from the
2016 survey's to the 2019 survey's).

Jobs: 29753475 (v4c), 29753476 to 29753485 (tilts, `tilt/p21r_*/`),
report rerun after a lookup fix, `checks/V5/report_tables_rank.md`. Net
worth in these tables is in nominal 2019 dollars (John, 2026-10-16).

Before the tilt, against the 2019 SCF (per unit, nominal 2019 dollars;
SCF mean $671,000, median $91,100):

| Prior on net worth's year | Mean | Gap | Median | Move, with wealth (nats) | Share negative | Mean, wages over $200,000 |
|---|---:|---:|---:|---:|---:|---:|
| SCF 2019 | $671,000 | | $91,100 | | 10.7% | $4,840,000 |
| v4c, 2019 dollars | $627,000 | −7% | $82,000 | 0.015 | 12.8% | $4,300,000 |
| A, 2015 only | $672,000 | 0% | $92,300 | 0.024 | 12.4% | $4,210,000 |
| A, 2018 only | $705,000 | +5% | $90,900 | 0.024 | 12.5% | $4,200,000 |
| A, 2021 only | $696,000 | +4% | $95,600 | 0.025 | 8.2% | $3,740,000 |
| A, two nearest | $703,000 | +5% | $92,500 | 0.024 | 11.0% | $4,010,000 |
| A, equal | $696,000 | +4% | $92,900 | 0.023 | 11.0% | $4,060,000 |
| B, 2015 only | $682,000 | +2% | $84,500 | 0.021 | 12.5% | $4,520,000 |
| B, 2021 only | $753,000 | +12% | $93,500 | 0.029 | 8.5% | $4,340,000 |
| B, interpolation | $722,000 | +8% | $90,200 | 0.025 | 9.9% | $4,340,000 |
| B, equal | $712,000 | +6% | $88,600 | 0.024 | 10.5% | $4,390,000 |

(Wage groups are in 2015 wage dollars, as in every table of this report.)

### What the third round shows

1. **With one dollar function for the target year, the year mix matters
   little.** Every prior in model A starts within 0% to +5% of the SCF's
   mean and within $4,500 of its median, and moves 0.023 to 0.025 nats.
   In the second round the same stacks ranged from 0% to +22% and 0.021 to
   0.062 nats. The level differences of the first two rounds came from the
   dollar step, not from the ranks.
2. **What the mix still changes is shape.** The 2022 survey's pattern has
   fewer units with negative net worth (8.2% against 12.4% under the 2016
   pattern; SCF 2019: 10.7%) and less wealth among earners above $200,000
   ($3.74 million against $4.21 million; SCF $4.84 million).
3. **On the honest test (B), the mixes are close.** The mean is best under
   2015 only (+2%), the median under interpolation ($90,200 against
   $91,100). The moves run 0.021 to 0.029 nats. With about 2% noise from
   the draws alone, and more from training, these cannot be ranked.
4. **v4c's model still has the smallest move** (0.015) and the closest
   untargeted income relationships (0.8 points against about 1.1), but
   starts 7% poor on the mean. The three-wave models place PUF-style
   records higher in the wealth ranking than v4c's model does, under the
   same prior and the same dollars ($672,000 against $627,000). Whether
   that is the extra data or the training seed is not known.
5. **Unchanged by any prior:** after the tilt, returns with no wages hold
   $1.32 to $1.35 million against the SCF's $761,000; earners above
   $200,000 hold $3.56 to $4.32 million against $4.84 million; the median
   is $67,000 to $70,000 against $91,100, set by the count base.
