---
title: "Translation learned in pretraining: actual amounts, source concepts, reported amounts"
role: design
workstream: rebuild
status: current
updated: 2026-10-16
sot: research/rebuild/design/plan.md
supersedes: []
superseded_by: null
---

# Translation learned in pretraining

The design direction for translating surveys into tax concepts, agreed
with John on 16 October 2026 (plan decision 18). It replaces the
per-variable rule of decision 15 once the tests at the end pass; until
then decision 15 stays the working rule. Background: the theory in
`minimal/11_SHARED_X_THEORY.md` and `minimal/12_CROSSWALK_THEORY.md`, the
failed in-model attempts in `minimal/16_MODEL_SIDE_FIXES.md`, and the
first worked example in `minimal/17_DISCUSSION_2026-10-15.md`, section 9.

## 1. What translation has to achieve

The model pairs records across files through the shared variables. It
learns from the SCF that households with a given interest income hold a
given net worth, and applies that to PUF returns with the same interest.
The pairing is right only if a household at a given position in the
survey is the same kind of household as a return at that position in the
PUF. No household is in both files, so the model cannot learn how one
file's scale lines up with the other's from the data alone; the source
label lets it learn that the survey has more zeros, not that a survey
$1,000 is like a PUF $2,400.

## 2. Why a survey's number differs from the PUF's

For each source and each variable, in this order:

1. **Population:** who is in the file (the SCF and CPS include
   nonfilers). Fixed before training, by the filing rule and nonfiler
   flag.
2. **Definition:** what the number measures (CPS interest includes
   interest inside IRAs). A known formula over components (section 3).
3. **Reporting,** for people in both populations under the same
   definition. Two unknowns:
   - (a) **who reports anything:** the chance that a true holder reports
     a nonzero amount, which may depend on the amount;
   - (b) **how reported dollars relate to true dollars** among those who
     report: right, all too low, too low at the top, rounded, capped.

Ranks and dollars are the two simple answers, and each assumes one
unknown away:

- **Ranks** assume (a) away (every holder reports something). Then the
  70th percentile of survey holders is the 70th percentile of PUF holders,
  whatever (b) does. Right across years within one file.
- **Dollars** assume (b) away (reported amounts are true amounts). Then a
  survey $2,600 is a PUF $2,600, whatever the pattern of who reports.
  Right for SCF interest, dividends and capital gains (totals and
  reporters' amounts match; small holders say zero).

When both (a) and (b) are wrong, neither works on its own. The general
answer is to model both, which is what this design does.

## 3. The three layers

For every shared variable:

1. **Actual:** the household's true amounts, split into components where
   the sources' concepts differ (interest: taxable, tax-exempt, inside
   retirement accounts).
2. **Concept, per source:** a known sum of components, written down, not
   learned. PUF taxable interest = taxable; CPS interest = taxable +
   tax-exempt + retirement-account. This is how definition differences
   enter.
3. **Reported, per source:** a reading of that source's concept through a
   reporting layer whose shape is stated in advance: the chance of
   reporting rises smoothly with the amount (an S-curve with two
   settings), reported dollars are a fixed share of true dollars (one
   setting), plus known rounding and top-coding. A few settings per source
   and variable, learned in training.

Everything else in the model (net worth, demographics) reads the actual
layer. Each source's records are explained as readings of it.

**The anchor.** If every source is a noisy reading, the level of the
truth is not identified ("everyone reports 20% low" fits as well as "the
truth is 20% lower"). One source's reporting has to be fixed from outside.
First version: **PUF = actual** (the PUF's reporting layer is exact).
Later: the PUF's own reporting layer fixed from the IRS's audit-based
misreporting studies (National Research Program; misreporting near zero
for wages, around half for sole-proprietor income), with the surveys
learned relative to it.

The anchor and every shape are choices, stated and given a setting, as in
a Bayesian model: the method makes the assumptions explicit rather than
removing them.

**A second output.** Tax-Simulator taxes what is on the return, so the
tax file keeps the PUF's reported concept. With the actual layer
underneath, the model can also give each household's true economic
amounts, for distributional work (for example, business income including
the part not reported).

## 4. Worked example: interest, PUF against SCF, PUF = actual

Made-up numbers, 1,000 households, four sizes.

The truth, which the PUF shows directly (the PUF has no net worth):

| True interest | Households | Mean net worth |
|---|---:|---:|
| None | 720 | $100,000 |
| Small ($50) | 150 | $150,000 |
| Medium ($500) | 90 | $300,000 |
| Large ($5,000) | 40 | $1,200,000 |

What the SCF reports for the same households:

| True interest | Households | Report it | Report $0 |
|---|---:|---:|---:|
| None | 720 | — | 720 |
| Small | 150 | 20 | 130 |
| Medium | 90 | 60 | 30 |
| Large | 40 | 40 | 0 |

The SCF's $0 group holds 720 true non-holders and 160 hidden holders; its
mean net worth is $114,200, not $100,000.

**Today's rule (decision 15)** gives a PUF return with $0 interest the
wealth of SCF households reporting $0, $114,200. Mean net worth over the
1,000 returns: $179,700 against the truth's $169,500 (+6%), all of it from
the zeros.

**The model, step by step.**

1. *Assumptions:* PUF = actual; the two files cover the same population
   (after the nonfiler work); the chance of reporting rises smoothly with
   the amount; reported dollars equal true dollars.
2. *The anchor sets how many hidden holders there are.* Once filled in,
   the SCF's true distribution has to match the PUF's. Here: chance of
   reporting 20/150 = 13% for small, 60/90 = 67% for medium, 40/40 = 100%
   for large. With 84 dollar slices the ratios are noisy; the S-curve
   smooths them and keeps them rising.
3. *The transformer decides which households they are.* For an SCF
   household reporting $0, with $300,000 of net worth and $90,000 of
   wages, suppose the model believes it holds none / small / medium /
   large with 50% / 20% / 25% / 5%. Times the chance of reporting $0 at
   each (1, 0.87, 0.33, 0): weights 0.50, 0.17, 0.08, 0, so 66% non-holder,
   23% small, 11% medium. A poorer household with no wages comes out mostly
   as a non-holder.
4. *Train on the completed records,* redrawing the fill-ins every pass.
   The households drawn as true non-holders average $100,000.
5. *Generate.* A PUF return with $0 interest is a true non-holder and
   gets $100,000-type wealth. Mean over the 1,000 returns: $169,500, the
   truth.

**The assumption the data cannot check.** The model takes the 130 hidden
small holders to be as wealthy as the 20 who reported ($150,000). If they
are poorer, say $120,000, the model subtracts too much from the zero group
and puts true non-holders at $94,600, 5% low. So every run states a
setting for it: hidden holders 0%, 20% or 40% poorer than reporters with
the same amount, and how far the answer moves.

## 5. How it runs inside pretraining

1. **The true slot for each shared variable is predicted without the
   source label.** This is the anchor inside the model: the survey's extra
   zeros cannot be explained as "survey households hold less", only as
   "survey households do not report". It requires the populations to
   match, so the nonfiler work comes first. Owned variables (net worth,
   other income) keep the label.
2. **The reporting settings are parameters of the network,** learned by
   the same gradient steps. On a survey record the loss is the
   probability of what the survey reported, summed over the possible true
   amounts. The masked network gives the model's belief about a hidden
   true slot, given everything else on the record, in one forward pass.
3. **Each pass, each survey zero gets a true amount drawn** from that
   belief times the chance of reporting $0, and everything downstream
   (net worth included) trains on the drawn amount, never on the reported
   one. Several hidden amounts on one record (interest, dividends,
   capital gains) are drawn together, in turn, each given the others.
4. **Generation needs no translation step.** PUF records carry actual
   amounts; borrowed variables are drawn given them.
5. **The reporting settings are fit only on what the source reports
   truly** (added after test 1, confirmed by John 2026-10-16). The chance
   of reporting and the dollar factor are fit against the model's belief
   about the true amount given only the variables the source reports
   truly: every filled-in or misreported slot, and the source's own
   variables, hidden. That belief is learned from the anchor source (the
   PUF) alone; a survey record trains the true slots only when its own
   variables are visible, which is where it teaches how they go with
   them. If any input to the belief were itself a fill-in, a wrong setting
   would feed itself: a too-small dollar factor for dividends inflates the
   filled-in dividends, which raises the belief about interest, which
   pushes the interest factor down. In test 1 the settings drifted to the
   edge of their range four times until this rule was enforced
   (`minimal/22_FAKE_SURVEY.md`). The fill-ins themselves (step 3) use
   everything on the record.

The earlier attempt (E9, `minimal/16_MODEL_SIDE_FIXES.md`) used the same
summed loss but failed for two reasons this design avoids: net worth still
read the reported value, and the source label was removed from every
shared variable, forcing one population on both files.

## 6. What the assumptions decide, and what the transformer decides

The shape assumption, the anchor and the survey's reported numbers decide
**how many** hidden holders there are at each amount. They do not decide
**which** survey households are the hidden holders. That comes from how
much each household resembles the households that reported, on everything
else on the record. That conditional prediction, across many variables,
is the transformer's job, and its quality decides whether the hidden
holders land on the right households.

**Why not fill in the true amounts before training.** The fill-in model
has to contain every variable whose relationships the main model will
learn: if hidden interest is filled in from wages and net worth only, and
hidden holders are in truth mostly older, the fill-ins carry no link to
age, and the transformer learns that missing link as data (the
"congeniality" requirement in the multiple imputation literature). The
main model here is the full joint distribution, so the right fill-in model
is the transformer itself. A single fill-in also treats a guess as a fact;
redrawing every pass trains on the spread. Filling in, retraining and
filling in again as an outer loop is the same algorithm in slow steps.

**Why not one variable at a time.** A household reporting $0 interest and
$0 dividends: a separate model for each fills in interest treating the
reported $0 dividends as real, then dividends treating the other zero as
real, and loses the fact that people who omit one tend to omit both,
unless the models are cycled many times, and they can disagree with each
other. One joint model fills in both together, consistently, with no
hand-built model per variable.

The gain is not a new source of identification; the assumptions supply
that. It is a better conditional model for the "which households" step,
for many variables hidden at once.

## 7. Units

- **The true slot is a position on the PUF's distribution for its year**
  (a PUF percentile slice). Within a year a slice is a dollar range
  (slice 50 of 2015 interest is $540 to $640). The model's actual layer
  lives in PUF rank space, as decision 3 requires.
- **The reporting layer speaks dollars,** because survey reports are
  dollars: a reporting household lands in the slice holding its reported
  dollars (true dollars times the dollar factor).
- **Across years,** a year with no PUF gets its slices' dollar ranges from
  the 2015 edges grown by the IRS growth in each item (an assumption: the
  shape stays, the scale moves; plan, "Future refinements"). Each survey
  wave gets its own reporting curve, which avoids deciding whether the
  chance of reporting follows dollars or position.
- Output dollars come at the end from the target year's rank-to-dollar
  function (decision 3).

## 8. Tests, in order

1. **Fake survey, PUF = actual.** Make a survey from the PUF: drop small
   holders at a known rate, scale amounts by a known factor, top-code. Let
   other income be the variable the fake survey owns. Check that training
   recovers the planted reporting settings and that other income on
   PUF-style records comes out right, against today's rule.
   **Done 2026-10-16, passed** (`minimal/22_FAKE_SURVEY.md`; top-coding not
   yet planted). Today's rule put other income of PUF returns with no
   interest about 18 points above the undistorted survey's; the latent
   method, with the settings fixed or learned, landed within seed noise of
   it.
2. **Fake survey with a definition layer.** The fake survey reports
   taxable plus tax-exempt interest; check that the model separates them.
3. **The PUF's own reporting layer,** fixed for one item (sole-proprietor
   income) from the IRS misreporting studies; check that the actual and
   reported versions come apart by the planted amount.
4. **The nonfiler work,** so that populations match.
5. **The real SCF, then the CPS,** with the sensitivity setting for hidden
   holders' wealth.

## References

- Abowd, J. M., and M. H. Stinson (2013). "Estimating Measurement Error in
  Annual Job Earnings: A Comparison of Survey and Administrative Reports."
  Review of Economics and Statistics. Neither source taken as true.
- Kapteyn, A., and J. Y. Ypma (2007). "Measurement Error and
  Misclassification: A Comparison of Survey and Administrative Data."
  Journal of Labor Economics.
- Meng, X.-L. (1994). "Multiple-Imputation Inferences with Uncongenial
  Sources of Input." Statistical Science.
- Tanner, M. A., and W. H. Wong (1987), data augmentation; Celeux, G., and
  J. Diebolt (1985), stochastic EM: the draw-then-fit loop of section 5.
- Ipsen, N. B., P.-A. Mattei and J. Frellsen (2021). "not-MIWAE: Deep
  Generative Modelling with Missing Not at Random Data." ICLR.

## Revision history

- 2026-10-16: written from the discussion with John.
- 2026-10-16: section 5, item 5 (fit the settings only on what the source
  reports truly) and test 1's result added.
