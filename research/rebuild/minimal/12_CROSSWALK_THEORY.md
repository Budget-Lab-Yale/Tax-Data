---
title: "What the crosswalk from survey X to PUF X assumes, stated precisely"
role: evidence
workstream: rebuild
status: current
updated: 2026-10-15
sot: research/rebuild/design/plan.md
supersedes: []
superseded_by: null
---

# What the crosswalk from survey X to PUF X assumes

John asked on 15 October what exactly we assume when we turn the survey's
value of a shared variable into a position on the PUF's scale, what that
"crosswalk" is, and what it says about which survey records correspond to
which PUF records. This note gives a precise formulation, states the
assumption behind each of the four rules we have used, connects each to
the literature where the same assumption has a name, and says what the
data can and cannot settle. It builds on
`research/rebuild/minimal/11_SHARED_X_THEORY.md`. The interest numbers are
the 2015 figures in `research/rebuild/minimal/01_DETECTION_TABLE.md`; the
wealth results are from `research/rebuild/minimal/09_RESULTS.md`. No code
was run for this note.

## 1. The objects

Think of a population of tax units. Each unit has a true value X* of the
shared variable: the amount it would report on a return under the tax
definition, for example taxable interest. It also has net worth Y and the
other return items Z.

The PUF is a sample of filers, and for a filer it records X_P = X*, apart
from the IRS's own rounding and the aggregated top records. Write F_P(x)
for the weighted share of filers with X* at or below x.

The survey is a sample of a different, overlapping population: households
cut into tax units, including units that do not file. Three things stand
between a survey unit's X* and the number in the survey file. The first is
who is in the file: write G_S for the distribution of X* among the
survey's units, which need not equal F_P. The second is whether a holder
reports at all: write r(x) for the probability that a unit with true
amount x reports a positive amount. The third is how a reported amount
relates to the true one: write m for that function. Together,

    X_S = R · m(X*),   with Pr(R = 1 | X* = x) = r(x).

Pure small-amount omission is m(x) = x with r rising from low values to
one. A pure scale error is m(x) = a·x with r equal to one. Noise and
rounding make m random or many-to-one.

The model sees each file's X as a band b(·). Trained on the survey, it
learns the distribution of Y among survey units in band k. When it makes a
PUF-style record, it draws Y for a PUF record in band k from what it
learned (wealth is drawn under the SCF label, as the source-label rule in
`research/rebuild/joint_model_history.md` says). So the whole requirement
on the crosswalk is one equation, for every band k:

    p(Y | survey unit in band k)  =  p(Y | filer whose X* is in band k).     (1)

In words: the survey households the model puts in band k must have the
same distribution of wealth as the PUF households in band k would have if
we could see their wealth. A crosswalk is a rule g that sends each survey
value s to a position g(s) on the PUF's scale, so that the survey unit's
band is the PUF band of g(s). It is a good crosswalk exactly when (1)
holds.

This answers the question of which records correspond. No survey record
is ever paired with a PUF record, and nothing claims that any survey
household is the same household as any PUF return. The two files are
separate samples; the overlap between them is negligible and in any case
unknown to us. Equation (1) is a statement about groups: the set of
survey units sent to band k and the set of PUF units in band k are draws
from the same conditional distribution of Y. A crosswalk therefore makes
two claims. One is about how the two distributions of X line up, which
the data can partly check. The other is that units placed at the same
position are interchangeable with respect to Y, which the data mostly
cannot check because no PUF unit has Y.

It helps to write out what the survey actually gives. Suppose the
survey's recording does not depend on wealth once the true amount is
fixed (whether a unit reports, and how accurately, depends on X* and not
on Y given X*). Then

    p(Y | X_S = s) = ∫ p(Y | X* = x) · p(X* = x | X_S = s) dx.

In words, the survey's wealth distribution at a recorded value s is an
average of the true wealth-given-interest relationship over all the true
amounts that could have produced s. An exact crosswalk exists only when,
for every reported s, p(X* | X_S = s) sits on a single point, and then
g(s) is that point. That happens when m is a fixed increasing function for
reporters, and then g = m⁻¹. For a survey zero it never happens: a zero
can come from a true non-holder or from a holder of any amount who did not
report, so p(X* | X_S = 0) is spread out and no single g(0) is right. This
is why no rule that only moves values around can fully repair omission,
and why the source label is kept.

The usual assumption of statistical matching, that Y and Z are
independent given X, is separate from all of this. It concerns X*. Even
when it holds for X*, a bad crosswalk breaks the link between Y and Z,
because Y is then attached to the wrong part of the PUF's distribution.

## 2. The four rules, each in this notation

### Rank within positives, zero as its own bin (v2)

The rule is g(0) = 0 and, for a positive s, g(s) = F_P+⁻¹(F_S+(s)), where F_+ is the
distribution among holders only. It is exact when the survey's reporting
holders have the same distribution of X* as the PUF's holders, up to an
increasing function m. That holds if the survey's holders and the PUF's
holders are the same population and the holders who fail to report are a
random draw of all holders, unrelated to their amounts. It does not need
the two holder shares to be equal; it needs omission to be unrelated to
the amount. The detection table rejects that: the survey-to-PUF ratio of
units by bracket is 0.27 under $100, 0.41 at $100 to $500, 0.77 at $500
to $2,000, 0.92 at $2,000 to $10,000, and above one beyond that. When small holders drop out, the
survey's holders lean toward large amounts, and the rule pushes each
survey reporter down the PUF's scale. Among survey holders, 25 percent
have $2,000 or more (3.14 of 12.65 percent); among PUF holders, 11 percent
do (2.99 of 28.33 percent), and the PUF holder at the survey's 75th
percentile is in the $100 to $500 bracket. So a survey household with
$2,000 of interest is placed in the PUF band for a few hundred dollars. Every
PUF band then receives the wealth of survey households with several times
its interest, which is the doubling in run E0 (total 1.92 times the SCF).

### Rank over the whole distribution including zeros (John's proposal)

The rule is g(s) = F_P⁻¹(F_S(s)), with both distributions taken over all
units including zeros. The survey's zeros fill the levels from 0 to
F_S(0) = 0.8735 and each needs a level assigned, at random or by a prior
such as wages. The rule is exact when two things hold. First, the survey
units, after the population steps, have the same distribution of X* as
the filers, including the share at zero: G_S = F_P. Second, the survey's
recorded order, with its zeros placed by the rule, agrees with the true
order. Then F_S(s) = F_P(m⁻¹(s)) and g = m⁻¹, whatever m is, without any
PUF dollars. Under pure omission and the same population, the survey's
share above s is the integral of r(x) over the PUF's units above s, so

    F_S(s) = F_P(s) + (share of all units that are unreported holders above s).

Above the amount where reporting becomes complete, the extra term is zero
and the rule returns g(s) = s exactly. Below it, each small reporter is
moved up the PUF's scale by the count of missing holders above it. The
survey zeros placed between levels 0.7167 and 0.8735 are sent to PUF
amounts from $1 to a little over $100 (the PUF's 86.4th percentile is
$100). Under random placement those are ordinary survey zeros, mostly true
non-holders, so the PUF's smallest holders receive the wealth of a
typical survey zero. In the 2015 data the first condition also fails at
the top: per head, the survey has 1.37 times the PUF's share of units at
or above $10,000 and 1.65 times at or above $200,000. Interpolating within
the brackets (an approximation), a survey $500 lands at a PUF amount of
roughly $650 to $800 and a survey $10,000 at roughly $8,000 to $9,000. The errors run in both directions.

### Band on the PUF's dollar cut points (E4)

The rule is g(s) = s, with the PUF's cut points used for both files. It assumes that a reported
dollar is a true dollar, m(x) = x for reporters, and that whether a holder
reports does not depend on wealth once the amount (and the other shared
variables the model sees) is fixed: R independent of Y given X*. Under
those two, p(Y | X_S = s, s > 0) = p(Y | X* = s, R = 1) = p(Y | X* = s),
so every positive band satisfies (1). Dropping holders according to their
amount is selection on the conditioning variable, and selection on the
conditioning variable leaves the conditional distribution of Y unchanged.
This rule says nothing about G_S or the shape of r: the survey may have
more units at the top, fewer at the bottom, or a larger zero share, and
the positive bands are still right. The source label lets the model hold
a larger zero bin for the survey. What remains wrong is the zero band: the
survey's zeros are a mixture of true non-holders, unreported small
holders, and nonfilers if they have not been removed, and the PUF's zeros
inherit that mixture's wealth. For interest the unreported holders are
mostly small, so this residual is modest (E4: net worth total 1.11 times
the SCF's under the PUF label, a mean of 0.98, and 1.00 under the SCF
label). The rule fails
when the dollars are wrong: in a test run that halved the survey's
amounts, the total rose to 1.68 times the SCF's.

### Translate, then band on the PUF's cut points

The rule is g(s) = m̂⁻¹(s), with m estimated, then the PUF's cuts. It assumes m has a
known form (a scale factor, or a monotone curve), estimated in a range
where both files report fully and the populations agree. There the survey
count per head above s must equal the PUF count per head above m⁻¹(s), so
m is the equipercentile map restricted to that range. Below the range it
assumes the same m holds. The ratio of totals is not used because holder
shares differ (Codex's point in `research/rebuild/minimal/11_SHARED_X_THEORY.md`).

### Summary

| Rule | Crosswalk g | Assumes about population | Assumes about reporting | Assumes about reported dollars |
|---|---|---|---|---|
| Positives ranked | F_P+⁻¹(F_S+(s)) | same distribution among holders | omission unrelated to amount; R ⊥ Y given X* | any increasing m |
| Ranks with zeros | F_P⁻¹(F_S(s)) | same distribution including zeros | recorded order is true order, zeros placed correctly; R ⊥ Y given X* | any increasing m |
| PUF dollar cuts | s | nothing | R ⊥ Y given X*; any r | m(x) = x |
| Translate, then cuts | m̂⁻¹(s) | same, only in the fitting range | complete reporting in that range; R ⊥ Y given X* | m of known form |

All four also assume that wealth given true interest is the same in both
populations, p(Y | X*, survey) = p(Y | X*, filer), and that Y and Z are
independent given X*.

## 3. Where each assumption has a name

### Statistical matching

Our setting is the classic one: Okner (1972)
merged the 1966 Tax File with the Survey of Economic Opportunity for
exactly this purpose, and Sims's comment on that paper raised the
conditional-independence objection. Rubin (1986) proposed file
concatenation: stack the two files, treat the unobserved blocks as
missing, and impute them several times. Our generative model is file
concatenation with a flexible model. Rässler (2002) and D'Orazio, Di Zio
and Scanu (2006) are the standard texts. Their central assumption is that
Y and Z are independent given X; without it the Y-Z association is only
bounded, and Kadane (1978) and Moriarity and Scheuren (2001) show how wide
the range is. The second assumption, usually stated as a precondition
rather than modelled, is that X is the same variable in both files and
both files sample the same population. The texts treat this as a
preparatory step of harmonizing the common variables (definitions,
classifications, reference periods), and Eurostat's guidance (Leulescu
and Agafitei 2013) recommends comparing the two marginal distributions of
each common variable before matching. As far as I know, the main texts do
not treat a common variable measured with different error in the two
files as something to model; they treat it as a failed precondition.
Survey methodology calls the repair "ex-post harmonization" (Granda, Wolf
and Hadorn 2010). Where recoding cannot make two instruments agree, Singh
(2022) borrows observed-score equating under a random-groups design, which
is John's rule with its assumption stated: both instruments measure
random samples of the same population.

### Test equating

Kolen and Brennan (2014) is the reference. Linear
equating matches means and standard deviations; equipercentile equating is
e(s) = F_P⁻¹(F_S(s)), identical to the ranks-with-zeros rule. Equating
distinguishes designs by what is assumed about the two groups. In a
single-group design the same people take both tests; for us that would be
linked survey-IRS records. In a random-groups design two random samples of
one population take one test each, and the equipercentile map is then
justified; that is John's assumption. In the nonequivalent-groups anchor
test design the groups differ, and both take a common anchor measured
identically. The frequency-estimation method then assumes that the
distribution of the score given the anchor is the same in both groups,
and reweights one group to the other's anchor distribution before
equating. Our candidate anchors are variables measured alike in both
files (age, marital status, wages where the detection table shows
agreement) and the dollar range where both files report fully, which
pins the measurement there. SOI totals and counts are a different kind of
information: they describe the PUF population in other years but say
nothing about how the survey measures. Holland and Dorans (2006) list the
requirements for two scores to be equated: same construct, equal
reliability, symmetry, equity, and population invariance. Lord's equity
condition is that, for every true score, the converted score has the same
distribution as the other test's score; that is equation (1) applied to
X itself. Lord (1980) showed that equity cannot hold when the two tests
differ in reliability. Our survey interest is less reliable than the
PUF's, because of omission, so no crosswalk of any form makes a survey
unit interchangeable with a PUF unit at every true value; something
beyond the crosswalk (the source label, or a measurement model) has to
carry the rest. Population invariance (Dorans and Holland 2000) says the
map should be the same when computed within subgroups; it gives a test
for John's rule, below. I agree that equating is the closest formal home
for a rank-to-dollar crosswalk, with the qualification that equating
usually aims at comparable scores, while we need the stricter equity
condition.

### Measurement error

Fuller (1987) and Carroll, Ruppert, Stefanski and
Crainiceanu (2006) are the references. Recording that does not depend on
Y given X* is called nondifferential error, and the survey's X is then a
surrogate for X*. With a surrogate, the relationship of Y to the recorded
X is a blurred version of its relationship to X*, attenuated toward no
relationship for classical additive error. Correcting it needs
p(X* | X_S), which comes from validation data (a subsample with both),
replicate measurements, or an instrument. Regression calibration replaces
the recorded value with E[X* | X_S]; that is a crosswalk, and it is close
to right when the outcome depends nearly linearly on X* or the error is
small. Our error is not the classical kind; Bound, Brown and Mathiowetz
(2001) survey such nonclassical survey errors, and Meyer, Mok and
Sullivan (2015) show that survey underreporting of transfer income is
mostly missed receipt rather than misstated amounts, the same pattern as
our interest. If omission followed a fixed threshold L, so that
X_S = X*·1{X* ≥ L}, it would be left-censoring (Tobin 1958; the
limit-of-detection literature in environmental and medical statistics,
for example Helsel 2012 and Lubin et al. 2004). A survey zero would then
mean "below L", a known interval, and coarsening both files to "below L"
would be exact; this is the coarsest-common-resolution rule in
`research/rebuild/minimal/08_STATE_2026-10-14.md`. With a reporting curve
that rises gradually, no threshold makes it exact. In this language, the
PUF-cuts rule is regression calibration with the identity for reporters,
and the zero is the one value where E[X* | X_S] differs from X_S.

### Missing data

Rubin (1976) and Little and Rubin (2019). A survey zero
that hides a positive amount is a missing value without a missing flag,
and the chance it is missing depends on the amount itself, which is
missing not at random. A selection model writes the joint distribution as
f(X*) times Pr(R | X*); our reporting curve r(x) is that second factor. A
pattern-mixture model (Little 1993) instead writes separate distributions
of X* for reporters and nonreporters. Missing-not-at-random models are
usually not identified, but here the PUF supplies f(X*) for the
population, and that identifies r(x) if the populations are the same.
Hirano, Imbens, Ridder and Rubin (2001) use the same device, a fresh
sample that gives the complete marginal, to identify nonignorable
attrition in panels. What stays unidentified is whether nonreporters have
the same wealth as reporters with the same true interest, R ⊥ Y given X*.
That is the assumption under the PUF-cuts rule. If it holds and r is
known, the wealth of true non-holders can be recovered by subtracting the
unreported holders' share from the survey's zero bin, which is what the
reporting-curve loss (E9) was meant to do.

### Transportability

Pearl and Bareinboim (2014) and Bareinboim and
Pearl (2016) ask when a conditional distribution learned in one
population can be used in another. Their selection diagram adds a node
pointing at each mechanism that differs between populations; a
conditional p(Y | W) transports if Y is independent of that node given W.
In our diagram the source points at the population of X* (nonfilers) and
at the recording of X (r and m), and X* points at both Y and the recorded
X. Y independent of source given X* is what every rule assumes. Y
independent of source given the recorded X fails, because the recorded
value itself depends on the source. So p(Y | recorded X) does not
transport, and transport needs a function g of the recorded value for
which Y is independent of source given g; that is equation (1). Our
source label is that selection node made explicit. It lets the model keep
source-specific distributions of recorded X, while Y is drawn from the
survey-trained mechanism.

### Record linkage

Fellegi and Sunter (1969) decide, pair by pair,
whether a record in one file and a record in another are the same unit,
using agreement on identifying fields. That presupposes that the files
share units. The PUF and the SCF are separate samples. The SCF's
list sample is drawn from IRS records, but no link is available to us, so
linkage has nothing to find. Radner et al. (1980) draw the same line
between exact matching and statistical matching.

### Quantile mapping and optimal transport

In one dimension,
F_P⁻¹(F_S(s)) is the unique increasing map that carries one distribution
onto the other, and it is the optimal transport map for any convex cost
(Villani 2003). It moves mass as little as possible; it does not say that
a transported unit is the same kind of unit. Climate scientists use it to
correct model output against station data (Panofsky and Brier 1968; Wood
et al. 2004; Gudmundsson et al. 2012), assuming the model and the
observations describe the same climate in the calibration period and the
model's error is a fixed increasing function. Precipitation has our zero
problem: models produce too many light-rain days. The usual fix sets the
model's smallest amounts to zero until the dry-day shares match. The
reverse case, where the corrected series has too many zeros and wet days
must be created, needs an extra rule; Themeßl, Gobiet and Heinrich (2012)
propose one ("frequency adaptation"; I have not checked its details).
Maraun (2013) shows that a deterministic quantile map gets the marginal
right but cannot repair the relationship with other variables when the
two series differ in noise, which is Lord's equity result in another
field. Cannon, Sobie and Murdock (2015) carry the model's change in
quantiles across periods, which is the analogue of using the survey's own
percentiles across years. In economics, the changes-in-changes estimator
of Athey and Imbens (2006) uses the same map under a rank-preservation
assumption and shows that when the outcome has ties, as the survey's zero
mass is, the map is only bounded unless an extra assumption fixes the
order within the ties. Random placement of the survey's zeros is one
point inside those bounds.

## 4. What the data can and cannot settle

Three unknown functions (the survey's population G_S, the reporting curve
r, the measurement m) produce one observed function F_S, and we also see
F_P and SOI totals. Each rule fixes two of the three by assumption and
reads off the third.

Suppose first that the populations are known to be the same. Then a pure
scale error and pure omission have different signatures. A scale error
leaves the zero share equal, makes the survey count per head above s equal
the PUF count above s/a at every s, and makes the ratio of totals equal a.
Omission raises the survey's zero share, leaves the counts per head equal
above the amount where reporting is complete, and leaves the totals
nearly equal. A mixture is identified if reporting is complete above some
line: a comes from the counts above the line, and r from the shortfall
below it. Interest in 2015 fits neither pure case. The cumulative
survey-to-PUF ratio is 0.45, 0.64, 0.90, 1.05, 1.37, 1.20 and 1.65 at
$1, $100, $500, $2,000, $10,000, $50,000 and $200,000; it rises past one
instead of levelling at one. Under equal populations, more units per head
at the top would need m(x) greater than x there, so the pattern points to
a population difference at the top (the SCF's list sample and weights
against the PUF's sampling and aggregated top) or to overstatement.

Now suppose the populations may differ. Any extra zero share can be extra
non-holders (nonfilers, different unit construction) instead of
nonreporters. Any difference in the shape of the positive amounts can be
a different population of holders: a positive density f_S can be written
as q(x)·f_P(x), rescaled, for some weight q, wherever the PUF has mass. The
two marginals of X and the totals then do not distinguish scaled dollars
from missing holders. SOI totals identify a scale factor only when the
population and the concept are the same.

Each extra source of information settles a specific part. Linked survey
and IRS records give p(X_S | X*) directly, and because the survey also
has Y, they are the only source that can test R ⊥ Y given X*. The
questionnaire and processing documents fix the form of m (concept,
rounding, ranges, top-codes) and show which zeros are structural. A range
where both report fully pins m there, given equal populations in that
range. Distributions of X given anchors measured alike separate
population from measurement in part: nonfiler zeros concentrate among
units with no wages, while omission zeros spread across wage groups in
proportion to their share of small amounts. The same conditionals give
John's rule a falsification test: if the map F_P⁻¹(F_S(s)) computed within
wage groups or filing statuses differs from the overall map, the
same-population assumption fails, in the sense of Dorans and Holland's
population invariance. A filing rule applied to the survey removes the
largest population difference by construction. SOI counts and amounts by
bracket give the PUF's distribution in years without a PUF.

Two things remain beyond reach without linked data: whether survey
nonreporters have the wealth of reporters with the same true interest, and
which particular survey zeros are latent holders. For the ranks-with-zeros
rule the second means the map within the zero mass is only bounded;
running the two placements (random, and ordered by wages) gives a rough
sense of that range.

## 5. The two assumptions, in one sentence each

For the rule we adopted (undo documented dollar errors, band on the PUF's
cut points, keep the source label): once documented errors are undone, a
dollar the survey reports is the dollar the return shows (the measurement
is the identity for reporters), and whether a holder reports depends on
the amount but not, given the amount and the other shared variables, on
wealth (item omission that is ignorable for Y given X*), so each band
holds the same kind of household in both files and the source label only
has to carry the survey's larger zero bin.

For ranking over the whole distribution including zeros: after the
population steps, the survey's tax units are a random sample from the same
population as the filers and the survey records true amounts in the true
order (equipercentile equating under a random-groups design, the
one-dimensional quantile map), so a percentile identifies the same kind
of unit in both files without PUF dollars, while the order within the
survey's zero mass is not identified and must be supplied by a placement
rule.

The two rules share the assumption that nonreporting is unrelated to
wealth given the true amount. They differ in what they assume beyond it.
The PUF-cuts rule assumes the reported dollars are right and nothing
about the population. The evidence for it is that the two interest
totals agree ($90 billion against $89 billion, as cited in
`research/archive/minimal_11_SHARED_X_THEORY_claude_2026-10-15_superseded.md`) while the counts
per head are short only below $2,000; within-bracket means agree too, but
they cannot by themselves tell a scale error from omission. The
ranks-with-zeros rule assumes the population is the same and nothing
about the dollars; the detection table shows the survey with 1.2 to 1.7
times the PUF's units per head at or above $10,000, $50,000 and $200,000.

## References

Athey, S. and G. Imbens (2006). Identification and inference in nonlinear
difference-in-differences models. Econometrica 74(2), 431-497.

Bareinboim, E. and J. Pearl (2016). Causal inference and the data-fusion
problem. PNAS 113(27), 7345-7352.

Bound, J., C. Brown and N. Mathiowetz (2001). Measurement error in survey
data. Handbook of Econometrics, vol. 5, ch. 59.

Cannon, A., S. Sobie and T. Murdock (2015). Bias correction of GCM
precipitation by quantile mapping. Journal of Climate 28, 6938-6959.

Carroll, R., D. Ruppert, L. Stefanski and C. Crainiceanu (2006).
Measurement Error in Nonlinear Models, 2nd ed. Chapman and Hall/CRC.

D'Orazio, M., M. Di Zio and M. Scanu (2006). Statistical Matching: Theory
and Practice. Wiley.

Dorans, N. and P. Holland (2000). Population invariance and the
equatability of tests. Journal of Educational Measurement 37(4), 281-306.

Fellegi, I. and A. Sunter (1969). A theory for record linkage. JASA 64,
1183-1210.

Fuller, W. (1987). Measurement Error Models. Wiley.

Granda, P., C. Wolf and R. Hadorn (2010). Harmonizing survey data. In
Harkness et al. (eds.), Survey Methods in Multinational, Multiregional,
and Multicultural Contexts. Wiley, 315-334.

Gudmundsson, L. et al. (2012). Downscaling RCM precipitation to the
station scale using statistical transformations. Hydrology and Earth
System Sciences 16, 3383-3390.

Helsel, D. (2012). Statistics for Censored Environmental Data Using
Minitab and R, 2nd ed. Wiley.

Hirano, K., G. Imbens, G. Ridder and D. Rubin (2001). Combining panel
data sets with attrition and refreshment samples. Econometrica 69(6),
1645-1659.

Holland, P. and N. Dorans (2006). Linking and equating. In R. Brennan
(ed.), Educational Measurement, 4th ed.

Kadane, J. (1978). Some statistical problems in merging data files.
Reprinted in Journal of Official Statistics 17 (2001), 423-433.

Kolen, M. and R. Brennan (2014). Test Equating, Scaling, and Linking, 3rd
ed. Springer.

Leulescu, A. and M. Agafitei (2013). Statistical matching: a model based
approach for data integration. Eurostat Methodologies and Working Papers.

Little, R. (1993). Pattern-mixture models for multivariate incomplete
data. JASA 88, 125-134.

Little, R. and D. Rubin (2019). Statistical Analysis with Missing Data,
3rd ed. Wiley.

Lord, F. (1980). Applications of Item Response Theory to Practical Testing
Problems. Erlbaum.

Lubin, J. et al. (2004). Epidemiologic evaluation of measurement data in
the presence of detection limits. Environmental Health Perspectives
112(17), 1691-1696.

Maraun, D. (2013). Bias correction, quantile mapping, and downscaling:
revisiting the inflation issue. Journal of Climate 26, 2137-2143.

Meyer, B., W. Mok and J. Sullivan (2015). Household surveys in crisis.
Journal of Economic Perspectives 29(4), 199-226.

Moriarity, C. and F. Scheuren (2001). Statistical matching: a paradigm
for assessing the uncertainty in the procedure. Journal of Official
Statistics 17(3), 407-422.

Okner, B. (1972). Constructing a new data base from existing microdata
sets: the 1966 merge file. Annals of Economic and Social Measurement 1(3),
325-362.

Panofsky, H. and G. Brier (1968). Some Applications of Statistics to
Meteorology. Pennsylvania State University.

Pearl, J. and E. Bareinboim (2014). External validity: from do-calculus to
transportability across populations. Statistical Science 29(4), 579-595.

Radner, D. et al. (1980). Report on Exact and Statistical Matching
Techniques. Statistical Policy Working Paper 5, Office of Federal
Statistical Policy and Standards.

Rässler, S. (2002). Statistical Matching: A Frequentist Theory, Practical
Applications, and Alternative Bayesian Approaches. Springer.

Rubin, D. (1976). Inference and missing data. Biometrika 63(3), 581-592.

Rubin, D. (1986). Statistical matching using file concatenation with
adjusted weights and multiple imputations. Journal of Business and
Economic Statistics 4(1), 87-94.

Singh, R. (2022). Harmonizing single-question instruments for latent
constructs with equating using political interest as an example. Survey
Research Methods.

Themeßl, M., A. Gobiet and G. Heinrich (2012). Empirical-statistical
downscaling and error correction of regional climate models and its
impact on the climate change signal. Climatic Change 112, 449-468.

Tobin, J. (1958). Estimation of relationships for limited dependent
variables. Econometrica 26(1), 24-36.

Villani, C. (2003). Topics in Optimal Transportation. American
Mathematical Society.

Wood, A. et al. (2004). Hydrologic implications of dynamical and
statistical approaches to downscaling climate model outputs. Climatic
Change 62, 189-216.

Wood et al. (2004), Gudmundsson et al. (2012), Kadane's reprint pages,
Radner et al. (1980) and the Sims comment on Okner (Annals of Economic and
Social Measurement 1(3), 1972) are cited from memory and their details
were not checked; the content claimed for Themeßl et al. (2012) beyond its
title was not checked. The others were confirmed or are standard
references.
