# Ages: plan

Written 21 October 2026. The first step is built (21 October, evening):
the primary filer's age range on every source, with step 0 for the PUF
records with no range and the primary-filer rule for survey couples
(`FEATURES.md`). Sex, exact ages, spouses and dependents are still plan. It covers ROADMAP item 5 for
ages and sex: the primary filer, the spouse and the dependents.

Update, 23 October. With the CPS added as a source, exact ages are built:
the primary filer's and the spouse's (to 79, then 80 and over) are shared
by the SCF, CEX and CPS and drawn right after the PUF items; the
dependents' exact ages and both sexes are CPS-only. The dependents' ages
ignore the PUF's own dependent age ranges (AGEDP1–3), contrary to the
Step 1 table below; the plan for them is Step 3.

## 0. The rule, in short (John, 23 October)

**What the CPS is for.** The PUF describes the return; it shows the people
on it only coarsely (the primary's age range, the dependents' age ranges,
the couple's earnings split in three ranges, the primary's sex). The CPS is
the person layer: it fills in the exact facts inside those coarse ones
(exact ages, the exact earnings share), and adds person items the PUF lacks
(hours, weeks, child care). The SCF and CEX then draw wealth and spending
given the people. The same CPS tax units that do not file are the
non-filers.

**The rule for every coarse PUF fact:**
1. Build the identical range on the CPS, computed from its exact values
   under the PUF's definition.
2. Share the range between the PUF and the CPS, so the model learns exact
   values given the range.
3. Where the PUF's range is missing, fill it first, as step 0 does: a model
   from the return, reconciled to IRS totals.
4. At generation, draw the exact value under the CPS label, given the range
   and everything else on the return, forced to lie inside the range.

**Where each fact stands:**

| PUF fact | Range shared with the CPS | Exact value drawn inside it |
|---|---|---|
| Primary filer's age range (AGERANGE) | yes | yes (built 23 October) |
| Dependents' age ranges (AGEDP1–3) | no | no; Step 3 |
| Earnings split (EARNSPLIT) | yes (4 November, branch `rebuild-earnsplit`) | yes: the spouse's share of wages, inside the range |
| Primary filer's sex (GENDER) | no | no: today drawn from the CPS without the PUF's value; Step 3 |

The spouse's age and sex have no PUF fact: drawn only on joint returns,
given the primary.

**The CPS's definitions must match the PUF's.** Same age ranges, same
definition of a dependent, same earnings concept. Today the dependent
definition does not match: the CPS tax units come from production's
non-filer builder (`src/nonfilers/asec_tax_units.R`), made to find people who
do not file, not to reproduce the dependents claimed on returns. On 2015
filers it finds 73.4 million dependents against the PUF's 91.7 million
(both capped as the PUF caps them), short in children as well as adults:
about 58.7 against 67 million under 17 (the PUF's child credit count is
67.4 million) and 14.7 against about 25 million aged 17 and over. Step 3
rebuilds the CPS dependents to the IRS definition.

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
   is missing for the same records; since 4 November it has its own step 0
   (below, "The earnings split").
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

### Step 3: the dependents' age ranges, the earnings split and the primary's sex (agreed with John, 23 October)

**What is built now, and the problem.** The 2019 file's dependents get
exact ages from the CPS alone: given the unit's number of dependents, the
filers' ages, wages and so on, the CPS's pattern of dependents' ages. The
PUF's own dependent age ranges are not used. Against population counts the
file has too many children and too few young adults (2019, millions;
`rebuild_cps/explore/comparisons/COMPARISONS.md`):

| | File | Census, July 2019 | SSA area |
|---|---:|---:|---:|
| Under 18 | 86.0 | 73.3 | 75.7 |
| 18–25 | 28.6 | 35.7 | 35.3 |

**The direct test, 2015** (`/nfs/roberts/scratch/pi_nrs36/jar335/rebuild/explore/2026-10-23/dep_ages/`).
The real 2015 PUF filers, every tax item fixed at its true value, only the
ages drawn by the model as generation draws them, compared with the IRS's
ranges on the same returns (first three dependents, millions):

| Age | IRS, where shown | Model, same returns |
|---|---:|---:|
| Under 5 | 14.3 | 17.9 |
| 5–12 | 30.6 | 30.0 |
| 13–16 | 14.1 | 13.6 |
| 17–18 | 6.2 | 6.0 |
| 19–23 | 6.8 | 5.2 |
| 24 and over | 5.4 | 4.7 |
| Share under 17 | 76.2% | 79.5% |

- Too young where the IRS shows the age: about 2.6 million extra children.
- Too young where the IRS withheld it (14.3 million dependents): the model
  gives 78% under 17; the booklet's full-sample counts suggest roughly 55%
  (approximate, for the reason in rule 1 below): about 3 million more.
- Poor return by return: for the youngest dependent the model picks the
  IRS's range 47% of the time.

So in 2015 the number of dependents is right (it is the PUF's) and their
ages are too young. In 2019 the file also has more dependents than the tax
data (94–96 million on filed returns against 90.5 million), from generation
and the tilt, which has no target on dependents.

**What the PUF gives** (the demographic file's booklet, 2015):
- AGEDP1, AGEDP2, AGEDP3 are the youngest, second-youngest and
  third-youngest dependent, in six ranges: under 5, 5–12, 13–16, 17–18,
  19–23, 24 and over (0 is "not present"). They are not tied to a type of
  dependent.
- The number shown is capped by filing status: three on joint and
  head-of-household returns, two on single returns, one on married-separate
  returns; where a return has more, the IRS keeps children at home first,
  then children away, other dependents, parents. The PUF's own dependent
  counts (XOCAH, XOCAWH, XOODEP, XOPAR) carry the same cap (checked: no
  return exceeds it). The translation is therefore: identify each
  dependent's type, keep dependents up to the filing status's cap in that
  type order, then sort the kept ones youngest first. Which people the IRS
  kept cannot be checked on the public file: with three children and a
  younger "other dependent", keeping by type and keeping the youngest pick
  different people.
- Two return items are related to dependents' ages: N24, the children who
  qualify for the child tax credit (67.4 million in 2015), and EIC, the
  EITC qualifying children (34.9 million). Neither is an exact age count:
  the child credit needs more than being under 17, an EITC qualifying child
  need not be a claimed dependent, a zero EIC says little outside EITC
  eligibility, and either can concern a person beyond the cap. They are
  predictors, not constraints. Neither is in the pipeline's PUF inputs yet
  (`pipeline/build_table.py`), nor is full filing status (the model has only
  joint against not joint).

**Why ranges are missing.** Code 0 means both "no dependent" and
"withheld". Three causes:
1. No demographics at all on the top sampling strata (the 88,017 records of
   step 0).
2. Dependents' ages withheld on returns with farm income or a first-time
   homebuyer credit.
3. Dependents' ages withheld where fewer than 100 returns in the population
   share the return's wages, marital status, age, sex, number of
   dependents, earnings split and presence of unemployment benefits.

Cause 3 removes older dependents most, so the public file's ranges are
tilted young. The booklet's last table gives each code's weighted count in
SOI's full sample and in the public file:

| | Full sample | Public file | Short |
|---|---:|---:|---:|
| Dependent 1, under 5 | 13.16m | 12.28m | 7% |
| Dependent 2, 19–23 | 3.30m | 2.22m | 33% |
| Dependent 3, 24 and over | 1.24m | 0.52m | 58% |

**The rule, as for the primary filer's range (step 0):**
1. **Ranges where missing, one dependent position at a time.** A return can
   show some dependents' ranges and withhold others (1.88 million weighted
   returns have both). For dependent k, code 0 means "no dependent" only
   when the return's capped count is below k; otherwise it means "withheld".
   Observed ranges are kept; only the withheld positions are filled. Code 0
   is never treated as an age category.
   - Fit a model of the ranges on the positions that have them, from what
     the return shows: the dependent counts by type, N24, EIC, full filing
     status, the primary's range (as drawn that pass), wages and income, and
     which withholding cause applies (no demographics, farm income or the
     homebuyer credit, or a small cell).
   - Draw a return's withheld positions together, given its observed ranges,
     keeping them youngest first; independent draws can make impossible
     families.
   - The booklet's full-sample counts are a benchmark, not an exact target:
     they come from a different sample, and the residual (full sample less
     the public file's known counts) does not match the withheld slots that
     exist (dependent 1: 4.80 million of residual against 6.55 million
     slots without a range; dependent 2: 3.95 against 5.27; dependent 3:
     2.25 against 2.51). Use the full sample's age shares, reconcile them to
     the weight actually available, and report the adjustment, as step 0
     does when Table 1.6's residuals do not fit (`pipeline/ages.py`).
   - Draw the ranges afresh each pass of training, as for the primary's
     range. The 2015 booklet's counts are for 2015 only; later years have no
     such check.
2. **The same ranges on the CPS.** Compute them from the CPS's ages under
   the IRS's definition: the cap by filing status (3, 2 or 1), the type
   order, youngest first, the same six ranges. Today's `build_cps.R` does not:
   it keeps the three youngest dependents for every filing status, with no
   type order. It has to be rebuilt from the person table, keeping each
   dependent's relationship to the claimant, its type, full filing status
   and the uncapped count before the cap.
   - What the CPS cannot reproduce: it counts college students at their
     parents' home, so children away cannot be told apart (0.4 million on
     the PUF); and production's builder attaches dependents through
     co-resident parent links, approximates support from household income
     and leaves out unrelated dependents (`src/nonfilers/asec_tax_units.R`),
     so it misses claims across households. Compare counts by type,
     claimant age, income and filing status before treating the two
     sources as the same.
3. **Shared between the PUF and the CPS only.** The SCF and the CEX keep the
   number of dependents as their shared item; their dependents' ranges are
   not needed (John, 23 October).
4. **At generation,** the CPS draws exact dependent ages inside each range,
   youngest first, as it does for the primary filer's exact age inside its
   range. Today's generator draws dependents' ages in random order with no
   range (`pipeline/generate.py`), so both need building. (Production
   already draws each dependent's age uniformly inside its PUF range and
   sorts them, `src/imputations/ages.R`; the gain here is ages that depend
   on the rest of the record and the same choice of dependents on both
   sources.)

**Checks specific to this step.** Hiding observed ranges at random tests
prediction where the IRS disclosed ages; it does not test the selectively
withheld ones. So also report, separately for each withholding cause, how
much the filled ranges depend on extrapolation and how sensitive they are
to the model's inputs. National totals cannot say whether older dependents
sit on high-income, farm or retirement-income returns.

**The primary's sex (GENDER).** Given on every record with demographics
(1 male, 2 female); missing on the same records as AGERANGE. Step 0 as
built fills only the range; filling sex with it (Step 0, item 4) is still
to build. Today the rebuild uses it only to
set the survey couples' primary-filer rule, and draws sex_primary from the
CPS without the PUF's value. It becomes a shared category: on the PUF the
observed (or step 0) value, on the surveys the primary's sex after the
primary-filer rule; nothing to draw at generation where it is known.

**The earnings split** (built 4 November 2026, branch `rebuild-earnsplit`;
`pipeline/earn_split.py`; checks in scratch `explore/2026-11-04/earnsplit/`).
EARNSPLIT gives the primary's share of the couple's earnings in three ranges
(75% or more, 25–75%, under 25%; 0 where there are none). Earnings are wages
plus Schedule SE self-employment earnings: E30400 is the couple's Schedule SE
earnings and E30500 the spouse's part, so the primary's are E30400 less
E30500. The data confirm this reading (`FINDINGS.md` there): E30400 is never
below E30500; on joint returns with the spouse self-employed and no
partnership income, E30400 alone is 92.35% of Schedules C and F; and on joint
returns with no wages this reading reproduces EARNSPLIT on 99.9% of them, the
other (E30400 the primary's alone, as production's
`src/imputations/earnings_split.R` reads it) on 80.7%. Production also reads
the three ranges as exactly 100%, 50% and 0%, which is why 97% of its
couples split at one of those points.

As built:
1. The spouse's share of the couple's self-employment income is a PUF slot
   shared with the CPS: on the PUF E30500 / E30400, on the CPS the same share
   of the couple's Schedule SE earnings (92.35% of each person's income, none
   below $400). It multiplies Schedules C and F and part_se (since 6 November,
   Schedule SE earnings other than C and F) alike. Joint returns with such income but no
   Schedule SE earnings up to the wage base (5.06 million: losses, under $400, or wages at the cap) have it missing.
2. EARNSPLIT is a category shared with the CPS (the CPS couple's own code on
   the same definition). It is given on joint returns with demographics;
   joint returns with none and no earnings get 0; the 2.51 million with
   earnings get probabilities from a gradient boosted classifier fitted on
   the known codes, redrawn each training pass as the age range's step 0 is.
   Fitted below $250,000 of AGI it predicts the known codes at
   $250,000–400,000 better than a logit (log score -0.90 against -1.03; a
   logit puts too little in code 1, which rises with AGI there), and gives
   the records with no demographics 1.45, 0.73 and 0.33 million returns in
   codes 1–3 against the booklet's full sample less public file, 1.36, 0.63
   and 0.39 million (a check, not a target).
3. At generation the PUF label draws the self-employment share and then the
   code after the PUF items; under the CPS label the spouse's share of wages
   is drawn only in the bands that reach the code's range given the couple's
   wages and self-employment split, and the dollar step holds the exact
   share inside the exact range.

**The number of dependents: the CPS builder's definition.** 2015 filers:
91.7 million dependents on the PUF (80.1 million children at home, 8.3
million other dependents, 2.9 million parents, 0.4 million children away)
against 73.4 million in the CPS builder's units, short in children and
adults alike (section 0). The file's count comes from the PUF, so the gap
does not set the file's number of dependents; it does make the CPS records
the model learns dependents' ages from unlike the PUF's. Likely causes, not
yet measured: the filing model puts some CPS units with children among the
non-filers (they hold 6.5 million dependents under 17); and the builder
attaches dependents only through a co-resident parent, approximates the
support test and leaves out unrelated people, so it misses children living
elsewhere, other relatives raising children, adult students and parents.
First check: the CPS's dependents, filers and non-filers together, against
tax returns plus non-filers, by age range; then fix the builder's dependent
rules to the IRS definition (rule 2 above) and compare by type, age,
income and filing status. The count part of the 2019 excess needs a tilt
target on dependents (SOI's exemption count or the child credit count).

The Codex review of this step (23 October) is in
`/nfs/roberts/scratch/pi_nrs36/jar335/rebuild/explore/2026-10-23/ages_review/review.md`;
its corrections are folded in above.

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
