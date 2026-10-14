---
title: "Test 4, probe Y: does translation recover the truth where it is known?"
role: report
workstream: rebuild
status: current
updated: 2026-10-10
sot: research/rebuild/joint_model_plan.md
---

# Probe Y: translation tested on test 2's distorted halves

Probe Y of test 4 (`SPEC.md` in this folder), 9–10 October 2026. The
question is whether the plan's translation step (D24: each survey
household keeps its weight and its wealth, and its shared variables are
moved to the file's concepts) recovers wealth given tax-concept incomes
when the truth is known.

## What was run

**Data.** Test 2's experiment A datasets: SCF 2016 filer tax units split by
household into two halves of 2,852 households each (about 17,400 rows each,
five implicates per household). Half 1 plays the PUF: its incomes are
distorted and its wealth hidden. Half 2 plays the survey: undistorted
incomes, wealth observed. Half 1's true wealth is kept in test 2's
`records_test.parquet`, so the truth is "half 1's distorted incomes with
its real wealth". Net worth is the 14 asset items minus the 6 debts, as in
test 2 (`c_common.py`); split-off units (whose wealth the SCF does not
ask) count in the income ranking but not in the wealth statistics.

Distortions of half 1 (test 2's, and two added here):

| Variant | What is done to half 1's incomes |
|---|---|
| `clean` | nothing |
| `noise` | every nonzero income multiplied by exp(N(0, 0.3²)) |
| `recat` | 30% of positive business income moved to wages |
| `unrep` | interest and dividends zeroed for a random 25% of holders; capital gains for another random 25% |
| `unrep_poor` (new) | as `unrep`, but 40% of holders below median net worth and 10% above are zeroed |
| `unrep_rich` (new) | as `unrep`, but 10% below median net worth and 40% above |

The two new variants deliberately break D24's assumption that reporting
differences are unrelated to wealth once the true amounts are known.

**The translation** (`y_translate.py`, function `translate`). Following D24:
the seven incomes are translated one at a time in a fixed order (wages,
business, interest and dividends, capital gains, rent and other Schedule E,
Social Security and pensions, unemployment and other), each within cells of
marital status, three age groups and, after wages, a third of the running
total of incomes already translated (18 cells; thin cells merged, at least
40 households on each side). In each cell, the share holding the income is
first brought to the reference's by switching households between zero and
nonzero, chosen by a model of the probability of holding that income given
the whole survey row (other incomes, age, marital status, dependents and
all 24 wealth items), fit on the survey and predicted out of sample by
household. Then amounts among holders are mapped by weighted rank to the
reference's amounts in the same cell. Five draws per household. Three
rules for choosing whom to switch were run: in proportion to the model's
probability (the default), strictly the most likely first, and at random
(the rule D23 and D24 rule out).

**Comparators.** No adjustment (half 2 as it is); reweighting (entropy
raking of half 2's weights to half 1's distribution of marital status by
age by dependents, of each income within marital status and age group in
zero, negative and five positive bands, and of total income in tenths with
the top 1% split off — the alternative D24 rejected); and a
**perfect translation**: half 2 with the same distortion applied to it
directly, with a fresh random draw. The perfect translation shows what the
method is aiming for; its gap from the truth is purely the sampling
difference between the two halves.

**Two designs.** In test 2's set-up ("halves"), the reference is half 1, a
different sample of 2,852 households. That difference turned out to be
large (see below), so a second design was added ("same households"): the
reference is half 2 itself with the distortion applied, and the truth is
that distorted half 2 with its own wealth. The method sees only the
reference's distribution, not which household is which, so the test is
still real; but sampling differences between survey and reference are
gone, and what remains is the method's own error.

**Measures.** Weighted by the SCF weight, for each table's own income
groups (by the total of the seven incomes): mean and median net worth;
shares holding directly held stocks, a business and bonds; mean net worth
of units reporting and not reporting each income; the share of net worth
held by the top 1% by income.

Jobs: `y_run.py` through `y_py.sbatch` (one CPU job per variant and design,
about three minutes each); tables in
`/nfs/roberts/scratch/pi_nrs36/jar335/probes/y/y_tables_<variant>.csv`,
movement measures in `y_movement_<variant>.json`, markdown in
`y_summary.md` (from `y_summarize.py`).

**Survey-side variants.** The `unrep` variants have the *file* missing
amounts the survey has. The SCF-against-PUF case runs the other way (the
SCF has fewer dividend reporters than the PUF), so three more variants put
the non-reporting in the survey instead: half 2's incomes are distorted as
in `unrep`, `unrep_poor` and `unrep_rich`, and the reference and truth are
half 2 as measured (same households). These are `svy_unrep`,
`svy_unrep_poor` and `svy_unrep_rich`. In both directions, the
wealth-dependent versions split holders at the median net worth of all
units, so the actual share of holders zeroed was 13% (`_poor`) and 35%
(`_rich`) for interest and dividends, not 25%.

## Results

All numbers below are from `y_summary.md` (jobs `y_run.py`, 9–10 October,
23:46–23:54). Dollars are thousands of 2016 dollars (wealth) as in test 2.

### The halves design is dominated by sampling differences

With no distortion at all, half 2's mean net worth by income group differs
from half 1's by 13.5% on average across the seven groups (median net
worth 15.3%); in the 80–90% group, $617k against $806k, and in the top 1%,
$17.8 million against $20.7 million. That is the sampling difference
between two sets of 2,852 households, and it is as large as or larger
than the effects of the distortions and the methods measured below. A perfect translation (half 2 with the
distortion applied directly) is still 8–19% off the truth on this measure
in every variant. Translation into half 1 also takes on half 1's sampling
noise: in `clean`, it switches about 1–2% of units each way between zero
and nonzero on each income and moves ranks among holders by 5–10 points on
average (wages 1.4), all of it noise, and its error by group (16.2%) is
larger than no adjustment (13.5%). The full error table for this design is in `y_summary.md`; the
conclusions below come from the same-households design, where the
sampling difference is absent.

### Error against the truth, same-households design

Mean absolute error across the seven income groups: mean and median net
worth in percent of the truth; holding shares (stocks, a business, bonds)
in percentage points. "By reporting status": mean net worth of units
reporting and not reporting each of the seven incomes (14 cells), percent.
"Top 1%": share of net worth held by the top 1% by income, in points.
Translation is shown with the three rules for choosing whom to switch
between zero and nonzero: in proportion to the likelihood (the default in
this probe), most likely first, and at random.

| Variant | Method | Mean net worth | Median net worth | Holding shares | By reporting status | Top 1% |
|---|---|---|---|---|---|---|
| clean | translated | 0.2 | 0.5 | 0.0 | 0.0 | 0.0 |
| noise | none | 5.6 | 9.4 | 1.6 | 0.0 | 1.0 |
| noise | translated | 4.7 | 6.8 | 1.0 | 0.9 | 1.0 |
| noise | reweighted | 5.2 | 11.6 | 1.5 | 2.7 | 1.7 |
| recat | none | 0.0 | 0.0 | 0.0 | 0.4 | 0.0 |
| recat | translated | 5.3 | 4.5 | 0.5 | 0.9 | 0.2 |
| recat | reweighted | 2.9 | 2.4 | 0.5 | 1.7 | 0.1 |
| file under-reports, at random (`unrep`) | none | 5.3 | 1.7 | 0.5 | 4.4 | 0.9 |
| | translated, proportional | 4.6 | 2.0 | 0.4 | 4.7 | 0.0 |
| | translated, most likely first | 3.2 | 3.1 | 0.4 | 7.3 | 0.3 |
| | translated, random | 3.8 | 1.0 | 0.3 | 0.8 | 0.2 |
| | reweighted | 11.7 | 5.5 | 1.1 | 10.4 | 2.7 |
| file under-reports, poorer more (`unrep_poor`) | none | 3.0 | 1.0 | 0.3 | 2.4 | 0.3 |
| | translated, proportional | 3.4 | 0.7 | 0.3 | 2.4 | 0.1 |
| | translated, most likely first | 4.2 | 1.6 | 0.3 | 4.1 | 0.0 |
| | translated, random | 4.8 | 0.7 | 0.2 | 0.4 | 0.5 |
| | reweighted | 4.6 | 2.5 | 0.6 | 4.6 | 1.4 |
| file under-reports, richer more (`unrep_rich`) | none | 12.2 | 3.1 | 0.6 | 6.5 | 1.5 |
| | translated, proportional | 7.6 | 2.7 | 0.5 | 7.3 | 0.6 |
| | translated, most likely first | 9.9 | 3.9 | 0.6 | 11.2 | 0.8 |
| | translated, random | 2.4 | 1.6 | 0.4 | 0.6 | 0.0 |
| | reweighted | 20.2 | 8.1 | 1.6 | 16.6 | 4.6 |
| survey under-reports, at random (`svy_unrep`) | none | 5.9 | 1.7 | 0.5 | 6.3 | 0.9 |
| | translated, proportional | 3.0 | 1.1 | 0.4 | 3.8 | 0.9 |
| | translated, most likely first | 2.7 | 0.9 | 0.3 | 2.6 | 0.8 |
| | translated, random | 5.4 | 1.3 | 0.4 | 8.0 | 1.3 |
| | reweighted | 12.7 | 4.3 | 0.9 | 11.1 | 2.3 |
| survey under-reports, poorer more (`svy_unrep_poor`) | none | 3.1 | 1.0 | 0.3 | 2.8 | 0.3 |
| | translated, proportional | 2.9 | 0.8 | 0.3 | 2.3 | 0.2 |
| | translated, most likely first | 3.0 | 0.6 | 0.3 | 1.6 | 0.2 |
| | translated, random | 3.0 | 0.8 | 0.3 | 3.6 | 0.3 |
| | reweighted | 4.9 | 2.1 | 0.6 | 4.6 | 1.3 |
| survey under-reports, richer more (`svy_unrep_rich`) | none | 16.9 | 3.1 | 0.6 | 10.7 | 1.5 |
| | translated, proportional | 8.7 | 1.5 | 0.6 | 5.0 | 1.6 |
| | translated, most likely first | 5.5 | 1.5 | 0.5 | 2.1 | 1.2 |
| | translated, random | 14.5 | 2.3 | 0.7 | 12.3 | 1.8 |
| | reweighted | 28.2 | 6.1 | 1.6 | 20.2 | 2.4 |

For `clean`, no adjustment and reweighting are exact (the reference is the
survey itself). The `noise` and `recat` rows for the other two translation
rules are within half a point of the proportional rule on the
income-group measures (in `y_summary.md`).

### Where the survey under-reports (the SCF's direction)

Mean net worth ($ thousands) of units reporting and not reporting interest
and dividends, `svy_unrep` (25% of holders in the survey report nothing,
at random):

| | Truth | No adjustment | Translated, proportional | Most likely first | Random | Reweighted |
|---|---|---|---|---|---|---|
| Percent reporting | 18.1 | 14.0 | 18.1 | 18.1 | 18.1 | 18.1 |
| Mean net worth, reporting | 2,455 | 2,407 | 2,212 | 2,520 | 2,014 | 2,284 |
| Mean net worth, not reporting | 220 | 355 | 284 | 203 | 340 | 332 |

The hidden holders are, by construction, ordinary holders. Without
adjustment, non-reporters look too rich ($355k against $220k), because they
include the hidden holders. Switching survey zeros on at random adds the
wrong households (reporters $2.0 million, too poor). Switching on the most
likely holders given wealth gets both groups close. The same holds when
the richer forget more (`svy_unrep_rich`: reporters $2,455k truth, $2,426k
most likely first, $1,792k random) and when the poorer forget more
(`svy_unrep_poor`: $2,455k truth, $2,528k most likely first, $2,268k
random). Mean net worth by income group in `svy_unrep` (truth / none /
proportional / most likely first): bottom 20% $81k / $96k / $83k / $81k;
20–40% $135k / $150k / $134k / $129k; 90–99% $2,452k / $2,416k / $2,430k /
$2,436k.

### Where the file under-reports

Same table, `unrep` (25% of the file's holders report nothing, at random):

| | Truth | No adjustment | Translated, proportional | Most likely first | Random | Reweighted |
|---|---|---|---|---|---|---|
| Percent reporting | 14.0 | 18.1 | 14.0 | 14.0 | 14.0 | 14.0 |
| Mean net worth, reporting | 2,407 | 2,455 | 2,887 | 3,024 | 2,444 | 2,662 |
| Mean net worth, not reporting | 355 | 220 | 263 | 237 | 348 | 239 |

Here the survey has too many holders, and D23 says to remove "the least
likely holders". Doing so removes the poorest holders, so the remaining
reporters are made too rich ($2.9–3.0 million against $2.4 million) and
the non-reporters too poor. Random removal is right, and it is right for
a general reason, not only because this distortion was random: if whether
the file records an amount does not depend on wealth once the true amount
is known (D24's own assumption), then which holders the file misses is
unrelated to the rest of the row, so the households to remove should be
chosen at random within the cell. Choosing by likelihood is right only for
the other direction, adding hidden holders, where the chance that a
reported zero is a hidden holder does rise with the probability of holding.

When the file's missing reports do depend on wealth, the choice matters
in the expected way, but random removal still won here: with the richer
missing more (`unrep_rich`), reporters' mean net worth is $2,284k truth,
$2,328k random, $3,141k proportional, $3,396k most likely first; with the
poorer missing more (`unrep_poor`), $2,508k truth, $2,498k random,
$2,693k proportional. Even when non-reporting is concentrated among poorer
holders (40% against 10%), the likelihood model concentrates the removals
far more steeply than that.

### Business income reported as wages (`recat`)

Moving 30% of business income to wages leaves each unit's total income
unchanged, so no adjustment is exactly right on the income-group tables.
Translation makes them worse: it switches 4.7% of units from no wages to
wages, choosing them by the likelihood of having wages (not the business
owners whose income was moved), then translates business income
separately, so totals are reshuffled. Bottom 20% mean net worth: truth and
no adjustment $81k, translated $63k, most likely first $53k. The movement
safeguard does flag this one: 27% of business-income holders moved more
than 10 percentile points, the largest such share anywhere in the
same-households design (0–16% for every other income and variant). Money reported under the wrong heading is a definitional matter
(D24 step 1) and needs a rule, not rank translation.

### Random reporting error (`noise`)

The noise scrambles order only mildly among holders (applying it to half
2: correlation of ranks before and after 0.94 to 0.996 across the seven
incomes; mean rank move 2–8 points), but total income is reordered enough
to change wealth by income group. Translation keeps each household's order,
so it cannot undo this, and it does not: median net worth in the 90–99%
group is $1,021k in the truth, $1,304k unadjusted, $1,269k translated; in
the 80–90% group mean net worth is $687k, $617k and $630k. Errors fall
somewhat (mean 5.6% to 4.7%, median 9.4% to 6.8%) because the spread of
each income is matched. Undoing noise would need the noise model D23 lists
as later work (several draws of tax-concept values per household).

### Reweighting

Reweighting is the worst method on nearly every measure in the
same-households design (error by reporting status 5–20% in the
non-reporting variants), and it changes the overall wealth distribution, which D24
predicted: mean net worth of all units is $686k in the truth and after
translation (by construction), but $630k after reweighting in `unrep` and
$593k in `unrep_rich`. It hits its income targets (largest remaining gap
under 0.01% of total weight) with modest weight changes (99th percentile
of new over old weight 1.2–2.6).

### How far households moved

In the same-households design, translation moved total income ranks very
little: mean move 0.1–2.7 percentile points, at most 1.1% of units moved
more than 40 points (`recat`). In the halves design, the same measure in
`clean` shows mean moves of 5–10 points per income among holders (wages
1.4), with up to 8% of holders moved more than 25 points (capital gains),
all of it sampling noise between two small samples. So the safeguard's thresholds need to be set
against the sampling noise of the survey's cells; with 6,000 SCF
households a wave and cells of a few hundred, moves of 10 points carry
little information on their own.

### A rule that follows the direction of the switch

Following the result above, a fourth rule was added to `y_translate.py`
(`flip_rule="likely_on_random_off"`): survey zeros switched on are taken
most likely first; survey holders switched off are taken at random. Its
error by reporting status (same measure as the table above, percent;
from a rerun of every variant at 00:00–00:06, which reproduced all earlier
numbers exactly):

| Variant | No adjustment | Proportional (as run above) | Likely on, random off |
|---|---|---|---|
| file under-reports, at random | 4.4 | 4.7 | 0.7 |
| file under-reports, poorer more | 2.4 | 2.4 | 1.3 |
| file under-reports, richer more | 6.5 | 7.3 | 0.9 |
| survey under-reports, at random | 6.3 | 3.8 | 2.2 |
| survey under-reports, poorer more | 2.8 | 2.3 | 1.4 |
| survey under-reports, richer more | 10.7 | 5.0 | 2.1 |
| clean | 0.0 | 0.0 | 0.3 |
| noise | 0.0 | 0.9 | 2.2 |
| recat | 0.4 | 0.9 | 3.4 |

It is the best of the four rules in four of the six non-reporting
variants and within one point of the best (random switching) in the other
two, including those where reporting depends on wealth.
Mean net worth by income group under it: 3.3% error in `unrep`, 2.6% in
`unrep_rich`, 3.2% in `svy_unrep`, 5.7% in `svy_unrep_rich`.

### Retraining test 2's model on translated data (step 4)

Test 2's joint model (configuration `joint_w1`: base size, loss weighted
by the population weight, the survey weight not a model variable) was
retrained on `expA_unrep` with half 2's incomes replaced by draw 0 of their
translation into half 1's concepts, three training seeds each, for two
translations: the proportional rule and random switching (built by
`y_prepare.py`, which runs test 2's `a_prepare.py` unchanged apart from the
substitution; trained and completed by `y_pipeline.sbatch`, a copy of
`b_pipeline.sbatch` with its own log path). Half 1 was completed presented
as SCF, five draws per record, and compared with test 2's own `joint_w1`
completions of the same records (`y_eval_completion.py`,
`y_summarize_completion.py`; `y_completion_summary_unrep.md`). This is
the halves design, so the translation itself carries the sampling noise
described above.

Mean net worth ($ thousands) of half 1 by its own income groups: mean of
three seeds (range).

| Income group | Truth | Untranslated (test 2) | Translated, proportional | Translated, random switching |
|---|---|---|---|---|
| Bottom 20% | 70 | 137 (136–138) | 146 (120–163) | 155 (138–164) |
| 20–40% | 135 | 195 (187–202) | 235 (231–242) | 250 (230–268) |
| 40–60% | 197 | 285 (249–313) | 331 (291–371) | 321 (288–362) |
| 60–80% | 366 | 504 (416–592) | 550 (480–639) | 621 (535–736) |
| 80–90% | 835 | 969 (811–1,166) | 1,120 (970–1,262) | 1,056 (956–1,199) |
| 90–99% | 2,275 | 3,363 (3,016–3,969) | 3,666 (3,207–4,279) | 3,821 (3,466–4,316) |
| Top 1% | 19,992 | 19,044 (14,921–22,171) | 19,549 (16,359–23,126) | 20,081 (16,978–24,191) |

Mean absolute error against the truth, percent, mean of seeds (range):

| Run | Mean net worth by income group | Median net worth by income group | Mean net worth by interest/dividend and capital gain reporting (4 cells) |
|---|---|---|---|
| Untranslated (test 2) | 43 (32–54) | 28 (17–38) | 26 (15–42) |
| Translated, proportional | 58 (44–72) | 27 (18–39) | 36 (24–50) |
| Translated, random switching | 64 (51–79) | 35 (27–49) | 40 (28–54) |

The model's own error (completed wealth too high in every group below the
top 1%, by 16–120%) and its spread across training seeds are several
times larger than anything translation changed in the tables above
(a few percent). On this test bed, retraining shows no improvement from
translation; if anything the translated runs are further from the truth,
but the seed ranges overlap. The test bed is too small to measure an
effect of translation through the model.

## What this means for the plan

- **Translation does what D24 says when its assumptions hold.** With no
  difference in meaning it changes nothing. Where the survey misses
  amounts the file records, which is the SCF's situation for dividends, it
  removes most of the error in wealth given tax-concept incomes, provided
  the households switched on are chosen by likelihood given wealth.
  Random switching there is worse than doing nothing.
- **One rule in D23 is wrong as written.** "Deletions likewise remove the
  least likely holders" is the wrong rule when the file has fewer holders
  than the survey. Under D24's own assumption (reporting does not depend on
  wealth once the true amount is known), which holders the file misses is
  unrelated to the row, so they should be removed at random within cells.
  Removing the least likely holders made errors larger than no adjustment.
  The rule that follows the direction of the switch (likely on, random
  off) was the best or within one point of the best in all six
  non-reporting variants, including the four where reporting depends on
  wealth.
- **When reporting depends on wealth, the damage is modest at these
  settings.** With the file's non-reporting at 40% against 10% by wealth,
  the direction-aware rule still had errors by reporting status of 0.9–1.3%
  (from 2.4–6.5% unadjusted). A stronger dependence would do more harm;
  this probe did not search for the breaking point.
- **Definitional differences need rules, not ranks.** Money reported under
  a different heading (business income as wages) was made worse by
  translation, and only the movement safeguard flagged it. This supports
  D24's step 1 being done carefully before step 3.
- **Random reporting error is not undone** by rank translation, as
  expected; the error it leaves here was about 5% on mean and 7–9% on
  median net worth by income group, for noise with a standard deviation
  of 0.3 in logs. The noise model listed as later work in D23 is what would
  address it.
- **Reweighting is worse than translation in every non-reporting variant**
  and changes overall wealth, as D24 argued.
- **Sampling noise matters at survey scale.** Two random halves of the 2016
  SCF filers differ by 13% in mean net worth by income group. Translating
  into a small noisy reference imports its noise; in the real case the
  reference is the PUF, which is large, but the survey's cells are thin,
  and the movement safeguard's thresholds have to allow for that.

## Unfinished

- Only interest and dividends and capital gains were made to differ in
  reporting; the categorical crosswalk step (marital to filing status) and
  age were not tested, because the experiment A halves do not differ in
  them.
- The flip models and cell choices were not tuned; the out-of-fold
  predicted holding rates run up to 2 points below the actual rates.
- No noise model was tried.
- Step 4 ran on the halves design only, where sampling noise and the
  model's own error dominate; a same-households version of step 4 would
  need its own dataset build and was not done.

## Files

In this folder: `y_translate.py` (the function; interface in
`STATUS.md`), `y_run.py` (experiments), `y_summarize.py`,
`y_prepare.py`, `y_pipeline.sbatch`, `y_eval_completion.py`,
`y_summarize_completion.py`, `y_py.sbatch`. In
`/nfs/roberts/scratch/pi_nrs36/jar335/probes/y/`: `y_summary.md` (every
table for every variant and design), `y_tables_*.csv`,
`y_movement_*.json`, `y_completion_summary_unrep.md`, the translated
datasets under `data/` and the retrained models under `model/`. Logs in
`/nfs/roberts/scratch/pi_nrs36/jar335/probes/logs/y_*`.
