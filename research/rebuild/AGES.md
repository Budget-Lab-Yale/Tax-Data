# Ages: plan

Written 21 October 2026. The first step is built (21 October, evening):
the primary filer's age range on every source, with step 0 for the PUF
records with no range and the primary-filer rule for survey couples
(`FEATURES.md`). Sex, exact ages, spouses and dependents are still plan. It covers ROADMAP item 5 for
ages and sex: the primary filer, the spouse and the dependents.

The evidence behind it is the design note of 20 October,
`/nfs/roberts/scratch/pi_nrs36/jar335/rebuild/explore/2026-10-20/age/REPORT.md`
(cheap data checks, no model trained). Figures below are from it. A
Codex review of this plan (21 October) is in
`/nfs/roberts/scratch/pi_nrs36/jar335/rebuild/explore/2026-10-21/ages_review/review.md`;
its corrections are folded in below.

## 1. The problem

Each unit needs an exact age for the primary filer, the spouse on joint
returns, and each dependent. Age matters for three things: the survey
imputations (net worth above all), targets by age, and Tax-Simulator's age
rules (65+ standard deduction, EITC and child credit ages, retirement
accounts, Social Security).

The PUF's demographic file gives:
- the primary filer's age in six ranges (under 26, 26–34, 35–44, 45–54,
  55–64, 65+; coarser codes for dependent filers);
- the primary's sex;
- ranges for the three youngest dependents.

It never gives exact ages, or the spouse's age or sex. The records with no
age (below) have no sex either. The surveys record everyone's age and sex.
There are two separate problems.

1. **Coarse age.** Where the range is known, an exact age inside it is
   needed. This is the easier problem.
2. **No age at all, for a non-random set of records.** 88,017 records
   (2.2% of returns) have no demographics. The cut is on SOI's sampling
   income, so above about $400,000 of AGI no record has an age. These
   records hold 23.7% of AGI and 77% of long-term gains. Whether a record
   has an age depends on its income, not on its age. But above $400,000
   there are no records with ages to learn from, so the PUF alone can only
   extrapolate there.

What exists for the records with no age:
- **SOI Table 1.6:** returns by filing status, age range and 19 AGI
  brackets up to $10 million+, 2011–2023. It uses the PUF's own age
  concept: where the PUF's ranges cover a bracket fully, they reproduce the
  table. It gives totals, not which records.
- **The return itself.**
  - The standard deduction amount shows how many of the primary and spouse
    are 65+ (or blind). But only 11.8% of the no-age records take it.
  - Social Security: 79.5% of recipients with known ages are 65+.
  - Pensions, IRA distributions, and the early-withdrawal penalty (1.4% of
    payers are 65+).
- **The SCF:** exact ages for head and spouse, including rich households.

Production pools every missing-age return above $200,000 into one cell per
filing status, whatever is on the record. Within a range it draws one age
shape for everyone: the same chance of being 80+ for a 65+ filer with no
wages as for one with $150,000 of wages. The CPS shows these differ
sharply: 32% are 80+ with no wages, 5.7% with $75–200k of wages.

## 2. The plan

### In order

Before training, once:
1. Survey couples get the PUF's primary filer: the man, except the woman
   with an age-dependent probability that reproduces the PUF's share by age
   (about one couple in ten); the older partner in a same-sex couple
   (step 2).
2. Survey ages are moved to the end of the tax year (step 2).
3. PUF-style variables are computed on every survey row: the primary's age
   range and sex, the dependents' ranges, the standard-deduction 65+/blind
   count, the under-17 dependent count (step 1).
4. Step 0 on the PUF: the 88,017 records with no demographics get
   probabilities over age range and sex, scaled to Table 1.6 by AGI bracket
   and filing status.

Training:
5. PUF rows carry the range and sex (observed, or redrawn from step 0 each
   pass) and no exact ages. CPS and SCF rows carry everything, exact ages
   included. Training is otherwise as now.

Generation (or a fill-in on real PUF records):
6. Draw the PUF record, including its range and sex.
7. Under the CPS label, draw exact ages: primary, then the spouse's sex and
   age, then dependents.
8. Under the SCF label, draw net worth given the range, the exact ages and
   the shared variables.

After: reweighting to Table 1.6 by age, and later SSA population by age.

### How it connects to the survey translation work

- **Same principle** (decision 23): translate each survey into PUF concepts
  before training. Age has three translations: who (which person is the
  primary filer), when (the date age is measured) and how coarse (exact age
  to the PUF's ranges). All three are fixed rules, so they are applied
  before training. SCF interest is harder: the gap between what households
  report and what the PUF would show is unknown and is estimated during
  training.
- **The age range becomes a shared variable** on every source, like wages
  or married, so every survey-borrowed variable can depend on it: net worth
  and its parts, tips and consumption later, and the interest fill-in. The
  interest scheme's "type" shares (now dependents and wage band) could
  include it; that is John's call.
- **Exact ages are a survey-owned variable,** drawn under the survey label
  at generation, the same way net worth is drawn under the SCF label.
- **What it asks of the survey code:** the CPS joins pretraining on the SCF
  pattern (non-filers marked, only well-measured items shown);
  `build_scf.R` keeps the SCF's ages and applies the primary rule. Step 0
  is PUF-side preparation, like attaching non-filers.
- **Order:** no conflict with the interest work, but adding age changes
  what the model conditions on, so the interest fill-in chances should be
  rechecked once ages are in.

### Step 0: an age range and sex for every PUF record (before pretraining, once)

1. **Range given the record.** Fit a model of the age range on the PUF
   records that have one, from what is on the return: filing status, AGI,
   wages, Social Security, pensions, IRA distributions, the penalty,
   dependents, itemizing.
2. **Apply it to the records with no range.** The standard deduction is a
   strong predictor, not a hard rule: each addition means 65+ *or blind*,
   5.2% of standard-deduction returns do not decode to a whole number of
   additions, and 0.09 million returns with a known 65+ primary show no
   addition. Social Security, pensions and the penalty are predictors
   too. On joint returns the income may be the spouse's, not the
   primary's.
   - The 252 records coded "not present" and the dependent filers coded
     "26+" are handled the same way, limited to the ranges their code
     allows.
3. **Match Table 1.6.** Within each AGI bracket and filing status, give
   each range one multiplying factor, apply it to every record's
   probability for that range, and rescale each record's probabilities to
   sum to one. Choose the factors so the weighted count in each range
   equals the target. This is the smallest change to the record-level
   probabilities that hits the targets. In brackets where some records
   already have ranges, the target is the table's count minus the
   known-range weight, as production does now. Done by bracket, not as one
   $200,000+ cell. Three things to settle first:
   - **Combined cells.** The review found 15 of the 456 filing status ×
     age × AGI cells of the 2015 table blank and merged with a neighbour
     (for example joint returns under 26 at $200,000–500,000). These need
     one target for the merged pair.
   - **Feasibility.** A target can be negative after subtracting the
     known-range weight (the production residual shows about −900 joint
     returns under 26 above $200,000, possibly an artefact of the merged
     cells), or a range can have too little eligible weight. Rounding and
     leftover gaps need a stated rule.
   - **Negative AGI.** 12.4% of that weight has no demographics; include
     it.
   - **A cross-check from the booklet.** The demographic booklet's last
     table gives weighted counts of each code for SOI's full sample as well
     as for the public file. The difference is roughly the records with no
     demographics: by age range, about 0.04 million at 26–34, 0.29 million
     at 35–44, 1.09 million at 45–54, 0.85 million at 55–64 and 0.74
     million at 65+; by sex, 2.69 million male primaries and 0.32 million
     female. It mixes in sampling differences, so it is a check on step 0,
     not a target.

   Table 1.6 fixes how many returns in each bracket are in each range, not
   *which* returns. Whether the old filers above $400,000 are the ones with
   the gains or the ones with the pensions still comes from item 1's
   extrapolation, so item 1 should keep some probability on every
   plausible range.
4. **Sex of the primary, drawn with the range.** The same 88,017 records
   have no sex. Fit the range and sex together on the records that have
   both, because they are related on joint returns: female primaries are
   27.5% of joint returns with a primary under 26 and 3.4% at 65+. On other
   returns the record itself carries information (head-of-household filers
   are 66.9% female, single filers 48.7%). No SOI table by sex and AGI is in
   the repository, so the only outside check is the booklet's full-sample
   counts above (about 2.69 million male and 0.32 million female primaries
   among the records with no demographics). The earnings split (EARNSPLIT)
   is missing for the same records and can be handled the same way later.
5. **Store the probabilities.** A fresh range and sex are drawn for each
   pass of training. That is equivalent, on average, to splitting each record into
   weighted copies, one per range, without multiplying the records.

After step 0 every PUF record has a range and a primary's sex, or a
probability for each combination.
Step 0 is estimated from observed ranges and then held fixed during
training, so the model's own guesses never count as evidence about missing
ages. It is the same kind of preparation as attaching non-filers. Redrawing
each pass hits Table 1.6 on average, not exactly in every pass.

### Step 1: the range on every source, exact ages from the surveys

**The range goes on every source.** The PUF's six-range variable for the
primary filer's age is added to the CPS and SCF rows too, computed from the
exact age of the person who would be the primary filer (step 2 says who
that is). Every survey variable is then learned as a function of the same
age variable the PUF carries. The largest gain is net worth: the SCF side is
asked for net worth and its parts given the range.

**Variables.**

| Variable | PUF | CPS | SCF |
|---|---|---|---|
| primary's age range | observed, or step 0 | computed from the primary's age | computed from the primary's age |
| dependents' age ranges (AGEDP1–3) | observed where given | computed | computed |
| primary's sex | observed, or step 0 | the primary's (step 2) | the primary's (step 2) |
| exact age: primary, spouse, dependents 1–3 | absent | observed | observed (head, spouse, children) |
| spouse's sex | absent | observed | observed |
| standard-deduction additions (65+ or blind) | from the deduction, where taken | computed from ages | computed from ages |
| dependents under 17 (child credit count, N24) | observed | computed | computed |
| filing status, number of dependents | observed | observed | observed |

"Absent" means the PUF rows never carry exact ages or the spouse's sex, and
the model is never scored on them there.

The PUF variables are measured, not computed, so they disagree with ages
now and then: the child-credit count differs from the under-17 dependent
count on 1.5% of returns with every age known, and the deduction additions
include the blind. The CPS and SCF versions are computed from ages, so the
model will treat them as stricter than they are on the PUF. How to soften
that (for example adding the PUF's observed disagreement rate as noise to
the computed survey versions) is open.

Filing status needs its full detail here; the network now has only joint
versus not joint.

The CPS enters pretraining as a new source. It shows only what it measures
well: wages, Social Security, unemployment, retirement income, married,
dependents, ages. Its non-filers are marked with the existing filing model.
`build_scf.R` keeps the SCF's ages (it drops them now).

**At generation** (or as a fill-in on real PUF records, if ROADMAP item 7
keeps them):
1. Draw the PUF record, including its range and the primary's sex.
2. Under the CPS label, draw the exact ages of the primary, spouse and
   dependents in turn (primary, then the spouse's sex and age, then
   dependents youngest first), each given those already drawn. Show it the
   primary's range and sex, the dependents' observed ranges, the deduction
   additions where the return gives them, the child-credit count, filing
   status, the number of dependents and the PUF items the CPS also
   measures. The spouse's sex is almost always the opposite of the
   primary's; same-sex couples come from the surveys' share.
3. Under the SCF label, draw net worth given the range, the exact ages and
   the shared variables.

**Which variables the CPS side sees decides where exact age falls inside a
range.** Exact age can depend only on what that draw is shown. Items where
exact age matters: IRA distributions (required withdrawals from 70½ under
2015 law, now 73), the Social Security amount (claiming at 62 against 67),
pensions. Each needs a CPS version that is close enough to the PUF's to be
shown.

**Masking.** Because the CPS rows carry the range and the rules computed
from their own ages, the model learns to respect them, so most draws should
already fall inside the range and the rules. Any leftover probability on an
impossible age is set to zero and the rest rescaled. Record how much
probability this removes and how often a family has no possible completion
(for example an early draw that leaves too few dependents under 17 to meet
the child-credit count). If either is not small, the masking is doing real
work and changes the distribution.

**At the top, exact age is assumed, not measured.** The CPS is thin above
$400,000, top-codes wages, and codes 80 and 85 as intervals. Table 1.6
says nothing inside a range. Nothing in the data says whether a rich 65+
filer is 66 or 86, and it matters: among SCF households with $200,000+ of
wages, median net worth is $2.4 million at 55–64 and $15.9 million at 75+.
The SCF should supply or at least check the within-range shape at the top.

### Step 2: who is the primary filer in a survey

Putting the PUF's age range on every survey row needs a rule for whose age
it is. Single, head-of-household and married-separate returns have one
filer, so the question is only about couples filing jointly.

**What the PUF means.** The primary filer is the taxpayer listed first on
the return. SOI Table 1.6 says, every year 2011–2023: "Age for joint
returns was based on the primary taxpayer's age." Neither booklet says how
couples choose the order, so the evidence below comes from the data. Checks
are in `/nfs/roberts/scratch/pi_nrs36/jar335/rebuild/explore/2026-10-21/primary/`
(2015 PUF joint returns with demographics, 51.6 million returns; CPS ASEC
2016 couples, 62.6 million).

1. **The primary is the man on 90.0% of joint returns.**
2. **It is not the higher earner.** The share of male primaries barely
   moves with the earnings split: 91.0% where the primary earns 75% or more
   of the couple's earnings, 87.2% at 25–75%, 88.5% where the primary earns
   under 25%.
3. **It is not the older spouse.** On joint standard-deduction returns
   with exactly one addition (one spouse 65+ or blind; 2.23 million
   returns), the male primary is the 65+ one 79.2% of the time. In CPS
   couples with exactly one partner 65+, the man is the 65+ one 79.6% of
   the time. On the female-primary returns the primary is the 65+ one
   22.4% of the time, against 20.4% for women in the CPS. In both cases the
   order matches sex alone.
4. **Woman-first returns are more common among the young.** Female
   primaries are 27.5% of joint returns with a primary under 26, 18.9% at
   26–34, 12.8% at 35–44, 9.4% at 45–54, 7.8% at 55–64 and 3.4% at 65+.
5. **The earnings split fits the man-first reading.** Among male-primary
   joint returns with earnings, the primary earns 75% or more on 46.6%, 25–75%
   on 37.6% and under 25% on 15.8%. In CPS couples with earnings, taking the
   man as primary: 42.6%, 44.5% and 13.0%.

So the primary is the man, except on about one joint return in ten where
the woman is listed first. Those exceptions depend on the couple's age, not
on who earns more or who is older.

**What the surveys do now.**
- **SCF:** the reference person is the man in a mixed-sex couple and the
  older partner in a same-sex couple (codebook, "Unit of analysis";
  respondent and spouse data are swapped to make it so, flagged by X8000).
  The production tax-unit builder takes the reference person's age (X14)
  as the unit's age, so it is always the man's.
- **CPS:** the repository's builder makes the householder the primary,
  which is the man in only 59.3% of couples.
- **Production `ages.R`:** draws the spouse from the CPS age gap given the
  householder's age, so it inherits the householder rule.

**Proposed rule, applied the same way to every survey couple.**
1. **Mixed-sex couple:** the man is the primary, except that the woman is,
   with a probability that depends only on the couple's age. The
   probabilities are chosen so the survey reproduces the PUF's share of
   female primaries by the primary's age range (point 4). The draw is keyed
   by record id, as rule S23 requires.
2. **Same-sex couple:** the older partner (the SCF's rule; the PUF gives no
   evidence).
3. **Everything follows the choice together:** the primary's age range and
   sex, the earnings split (the primary's share of wages plus
   self-employment earnings, as the booklet defines it), and which person is
   the spouse.

Size: 31.8% of CPS couples have partners in two different PUF age ranges,
so flipping one couple in ten moves about 3% of joint units to a different
primary range. The CPS householder rule, by contrast, puts the woman first
in 41% of couples.

**The date each age is measured.**
- **PUF and Table 1.6:** not stated in the booklets or the table notes;
  probably age at the end of the tax year (to confirm with SOI
  documentation).
- **CPS:** age at the March interview after the tax year. Someone with a
  birthday between 1 January and the interview is a year older than at the
  end of the tax year, about one person in five. Rule: subtract a year with
  that probability, by record id.
- **SCF:** age at an interview in the year after the tax year, spread over
  most of that year, so the share a year older is larger, perhaps more than
  half. The interview timing in the public file needs checking before
  setting the rule.

This matters at range edges, most of all at 64 and 65.

### Considered and dropped: filling in exact ages on PUF rows each pass

The first version of this plan, modelled on the SCF interest scheme, filled
in exact ages on every PUF row at the start of each training pass, drawn
under the CPS label, so the PUF label would learn exact ages jointly with
every PUF variable. It adds nothing. Example: a joint return, $3 million
AGI, $2 million of gains, $40,000 of Social Security, range 65+. The loop
would draw an age, say 71, from the CPS-label answer, and the PUF label
would train on it. Drawing at generation uses the same CPS-label answer.
That answer never looks at the $2 million of gains, so the PUF label would
learn about age and gains only what the CPS label implied. The loop added
machinery and a feedback path: the network's parts are shared across
sources (`pipeline/model.py`), so training on filled-in ages would shift the
next pass's donor draws. Dropped 21 October 2026 on the Codex review's
point 11.

## 3. Checks

1. **Step 0 is feasible.** Merged cells resolved, residual targets
   non-negative, and how often the targets conflict with observed ranges.
2. **Step 0 at the top.** Hide the known ranges of every record with AGI of
   $250,000–400,000, and of the loss returns that have them, run step 0
   without them, and compare with the truth. Score how age relates to
   Social Security, pensions, gains and business income, not only the
   bracket totals, which step 0 forces to match. Passing this does not
   validate million-dollar returns.
3. **Range recovery.** On held-out records with ranges, compare step 0
   with production's rule.
4. **Shape inside a range.** Coarsen the ages of held-out CPS and SCF
   households into PUF ranges and see how well exact ages, spouse age gaps
   and dependents come back. Include rich SCF households, and keep each
   household's five SCF copies together.
5. **Masking.** The probability removed, families with no possible
   completion, and sensitivity to the order persons are drawn.
6. **Spouses.** Train without the deduction additions and see how often
   drawn ages give the right count on standard-deduction returns. (With
   the additions shown, agreement only tests the conditioning, not
   prediction; the same holds for the child-credit count.)
7. **Consistency audit** of a generated file: spouses on non-joint returns,
   ages that contradict the standard deduction. For Social Security below
   62, the PUF's 55–64 range cannot give a benchmark; use the CPS or SCF.
8. **Wealth and tax.** Net worth by age within wage band against the SCF,
   whether the gap for earners over $200,000 ($3.5 million against the
   SCF's $4.4 million) moves, and age-dependent tax items, before and
   after reweighting.
9. **Totals.** The generated file, before and after reweighting, against
   Table 1.6 by bracket.

## 4. Open decisions

1. **Which variables the CPS side sees** when it draws exact ages (step 1).
2. **Donors.** The CPS is thin above $400,000 and top-codes wages; the SCF
   has rich households with exact ages. Use the CPS alone, or the SCF too,
   and if both, how to combine them at the top.
3. **Which survey adult is the primary.** Step 2 proposes a rule;
   it needs John's sign-off.
4. **Single years or narrow bands, and the top age.** The CPS top-codes at
   80 and 85, the SCF at 95; the mortality code wants detail above 80.
5. **The date each age is measured.** Step 2; the PUF's reference
   date and the SCF's interview timing are still to confirm.
6. **Dependents beyond three.** The PUF caps them at three; about 4.8
   million dependents are missing against SOI's exemption count. The CPS
   has them.
7. **Step 0's model.** A simple classifier fitted on the PUF alone, or the
   pretrained network's own PUF-label prediction, matched to Table 1.6 the
   same way.
8. **Order.** ROADMAP item 7 (real PUF records or synthetic) changes how
   much of this is fill-in rather than generation, so it is best settled
   first.
