---
title: "Translating a survey variable into a tax concept: what kind of problem it is and who has faced it"
role: background
workstream: rebuild
status: draft
updated: 2026-10-19
---

# Translating a survey variable into a tax concept

Written overnight, 18–19 October 2026, from the local notes listed in the
brief and a web literature search done by six helper searches running in
parallel. The helpers opened the sources; the reference list says how
deeply each was read. The session's web-search allowance ran out near the
end, so a few leads were not followed (listed at the end). Nothing was
run and no other file was changed.

## Summary

**(a) What kind of problem.** Translation is itself a statistical match:
the PUF gives the distribution of the true tax-concept amount within cells
of variables measured alike, the SCF gives the joint distribution of the
report and net worth, and nobody is in both. In the literature's terms it
is data combination with a common variable measured with nonclassical
error in one file (Ridder and Moffitt 2007, section 4.5), or missing data
not at random with an outside distribution of the missing variable
(Hirano, Imbens, Ridder and Rubin 2001; Sadinle and Reiter 2019). It is not
identified without linked data. For interest, dividends and gains it is
point identified under four assumptions (7.1): same population within
cells; reporters' amounts right and no false reports; reporting depends
only on the amount and the cell; net worth given the amount carries to the
PUF. The PUF's distribution can pin down exactly one thing about
reporting: how it varies with the true amount in each cell. Because a
non-holder never reports, any dependence of reporting on net worth is an
interaction that no outside margin identifies (1.4). For business income
the second assumption fails, and one margin cannot pin both who reports
and how reported dollars relate to true ones.

**(b) Precedents.** Translating with a file that holds both versions is
common (NHIS–NHANES, occupation codes, linked SNAP corrections). Without
one, the closest precedent is Schifeling, Reiter and DeYoreo (2019):
population assumption, restricted error model, priors from an old linked
file, validated by coverage of the reference survey's shares within cells.
Transfer-program fill-ins to administrative counts (TRIM3, CBO,
Larrimore–Mortenson–Splinter) are the cell method's ancestors; the one
check of who they fill in, against linked SNAP records, found them only
partly right. No US tax model translates survey amounts; they sort on
income predicted from tax data (CBO, TPC, PWBM, taxdata) or use presence
indicators (JCT). Nobody validates the joint distribution of a translated
variable and a transferred one.

**(c) Mapping.** The cell method is the additive nonignorable model with
an outside margin, and its odds scaling is the Saerens prior-shift
adjustment, exact under its assumption and only under it. The fake-survey
behavioral model is a misreporting model without linked data, so its
unidentifiable parts return what was planted. The four-step rule is
climate science's frequency adaptation plus quantile mapping; that field
found that item-by-item maps cannot repair dependence, that who moves is
set by a tie-breaking rule, and that maps "correct" real differences. For
business income, rank-mapping a noisy report flattens the relationship
with net worth, and linked data show ranks are not preserved. The
PUF-label prediction is CBO's device and is covariate shift; it can see
only variables measured alike, so it can set counts but not choose among
households alike on them.

**(d) Combining A and B.** Yes: a reporting model fit so the implied truth
matches the PUF, with outside priors on what the PUF cannot pin. What the
margins pin is how reporting depends on the amount; what they do not pin
returns its prior. One concrete gain: the cells can identify how reporting
depends on wealth if reporting is assumed not to depend on the cell
instead (7.4, worked example). The two assumptions fit equally and give
opposite answers about who the hidden holders are, so computing both
brackets the one unverifiable assumption.

**(e) Validation.** Fake surveys extended to violations of the assumption;
the checks the real data allow; asset balances in the SCF as a second
indicator of who holds interest and dividends; the households that
consulted their tax returns; worst-case bounds next to point estimates;
downstream comparisons with capitalized tax data and estate tax receipts;
and possibly tabulations from the Fed's own list-sample match to SOI.

**(f) Recommendations**, in order (7.6): keep the cell method with its
assumptions written down; run five cheap calculations that train nothing
before building anything; treat business income with a noise model, not
a rank map; use PUF-label predictions for counts only; run the closed-form
fill-in as a benchmark for the loop; target joint holdings only if the
3-point shortfall matters for wealth; use behavioral models as priors and
tests. Iterating the network's guesses converges to the assumption, not
to the truth (6.4).

## 1. The problem stated in the literature's terms

### 1.1 The objects

Four things describe an SCF household for one mapped item, say taxable
interest:

- **W**, the variables taken as measured alike in both files: married,
  number of dependents, wage group. The cells are built from W.
- **X\***, the amount the household's tax return would show under the
  PUF's concept (line 8a for interest). Unknown for an SCF household.
- **X_S**, what the household told the SCF.
- **Y**, net worth, which only the SCF has.

What the two files give, within each cell of W:

- the PUF gives the distribution of X\* (how many returns hold nothing,
  how many hold under $100, and so on);
- the SCF gives the joint distribution of X_S and Y.

What the model needs is the relationship of Y to X\* within W, learned
on SCF households, so that it can be applied to PUF returns.

### 1.2 Translation is itself a statistical match

Put the two lists side by side. To learn Y given X\*, we need, for SCF
households, the joint distribution of three things (X\*, X_S, Y). We know
one margin of it, X\* alone (from the PUF, if the populations are the same
within W), and another margin, (X_S, Y) together (from the SCF). That is
exactly the textbook statistical matching problem: two files share no
units, one gives the distribution of A, the other the joint distribution
of B and C, and we want the joint distribution of A, B and C. Here A is
the true tax amount and B is the survey report, which is closely but not
perfectly related to A.

So everything known about statistical matching applies to translation
itself, before net worth is ever transferred to the PUF:

- Without further assumptions, the two files identify only a set of
  possible joint distributions (the "Fréchet bounds" or "uncertainty
  region" of the matching literature). "Identified" here means that the
  data, in unlimited quantity, would pin the answer down to one value;
  "partially identified" means they pin it down only to a range.
- Every method that produces one answer adds an assumption that picks one
  member of that set. The conditional independence assumption is the
  usual one in matching; in translation the usual one is a restriction on
  how people report.

This is the sense in which John's realization of 18 October is a known
result: with no unit observed in both files, one margin of the truth and
one joint distribution of the report with the outcome cannot determine
the joint distribution of truth, report and outcome.

### 1.3 Which assumptions give which answer, worked through

Use the cell of `design/reporting_rules.md`, section 0.3: 100 SCF
households and 100 PUF returns of the same kind. On the PUF, 30 hold
interest. In the SCF, 13 report interest and 87 report none. The 87
zero-reporters average $139,000 of net worth; the 13 reporters average
$300,000. What net worth should a PUF return with no interest get?

**Assumption 1, same population within the cell.** Then 30 SCF households
truly hold interest. 13 said so; 17 of the 87 zero-reporters are hidden
holders and 70 are true non-holders.

**Assumption 2, no false reports.** A household that reports interest
holds it, and reporters' dollars are right. Then the only unknown is
which 17 of the 87 zero-reporters are the hidden holders. The cell table
of the cell method is assumptions 1 and 2 applied size range by size
range.

These two assumptions fix how many hidden holders there are. They do not
say which households. The net worth of the 70 true non-holders, which is
what a PUF zero should get, depends entirely on which 17 are taken out:

| Who the 17 hidden holders are | Their mean net worth | Mean net worth of the 70 true non-holders |
|---|---:|---:|
| the 17 richest zero-reporters (hypothetical mean $400,000) | $400,000 | (87 × 139,000 − 17 × 400,000) / 70 = $75,600 |
| like the 13 reporters | $300,000 | $100,000 |
| a random 17 of the zero-reporters | $139,000 | $139,000 |
| the 17 poorest zero-reporters (hypothetical mean $20,000) | $20,000 | $167,900 |

The first and last rows are the worst-case bounds in the sense of
Horowitz and Manski (section 3): nothing in either file rules them out.
The $400,000 and $20,000 are made up for the example; with the real SCF
they would be computed from the actual zero-reporters in each cell.

**Assumption 3, reporting does not depend on net worth once the true
amount and the cell are known.** In symbols, R ⊥ Y | X\*, W, where R is
"reports a positive amount". This is what the literature on measurement
error calls *nondifferential* error: the error carries no information
about the outcome beyond what the true value carries. Under it, the
hidden holders at each true amount look, in net worth, exactly like the
reporters at that amount, and the answer is the second row, $100,000.
With assumptions 1 to 3 the whole joint distribution of X\* and Y is
point identified, for every size range at once: within a cell, the
reporters at amount x stand for all holders at x, scaled up by one over
the reporting rate at x, and the reporting rate at x is the SCF's share at
x divided by the PUF's share at x. The non-holders are what is left.

This is the latent-amount method of plan decision 18 and the cell method
of step 31. In the cell method's own words (`31_DISCUSSION_2026-10-18.md`,
section 1), the scaling step is the exact adjustment under a slightly
stronger form of assumption 3: within a cell and size range, whether a
holder reports depends on nothing else on the record, including whether it
reported the other items.

### 1.4 What the PUF's distribution can pin down, and what it cannot

A general rule, worked out here by counting unknowns and equations and
matching the literature on "refreshment samples" (section 3.2): **the PUF's
distribution of the true amount, within a cell, pins down one thing about
reporting: how the chance of reporting varies with the true amount in that
cell.** Anything else that reporting might depend on has to be assumed.

The count, for one cell and one item treated as hold or not hold, with
households split into richer and poorer halves (the split is observed in
the SCF):

- Unknowns: the share holding among the richer (h₁) and the poorer (h₂);
  the chance a holder reports among the richer (r₁) and the poorer (r₂).
  Four unknowns.
- Equations: the SCF's reporting share among the richer (h₁r₁) and among
  the poorer (h₂r₂); the PUF's holding share for the cell, which must equal
  the average of h₁ and h₂. Three equations.

Four unknowns, three equations: not identified. Setting r₁ = r₂
(assumption 3) removes one unknown and the system solves exactly. The
by-hand example in `31_DISCUSSION_2026-10-18.md`, section 4, is this
calculation: 40 rich households with 16 reporting, 60 poor with 4
reporting, PUF 40 holders in 100, so a common reporting rate of 50%, 32
rich holders and 8 poor holders.

The reason is structural. A non-holder never reports interest, whatever
its wealth, so any dependence of reporting on wealth is necessarily a
dependence that exists only among holders: an interaction between the true
amount and wealth. The refreshment-sample literature shows that an
outside distribution of the missing variable identifies the main effect of
that variable on response, not its interactions with other variables
(Hirano, Imbens, Ridder and Rubin 2001; section 3.2). In our setting the
interaction is the whole of the question.

The same counting applies across items. The PUF gives, within each cell,
the share holding each combination of interest and dividends; the SCF gives
the share reporting each combination. Pooled over cells (numbers from
`minimal/31_RESULTS.md`, section 6; PUF weighted to the SCF's cell mix, %
of units):

| | PUF holds | SCF reports |
|---|---:|---:|
| interest and dividends | 13.7 | 6.4 |
| interest, no dividends | 16.4 | 6.2 |
| dividends, no interest | 5.0 | 5.6 |

Suppose a holder of interest reports it with the same chance whether or
not it also holds dividends (ignoring, for this illustration only, that
the two groups hold different amounts of interest). The 6.4 reporting
both items all hold both, so at least 6.4 / 13.7 = 47% of
both-holders report interest. Interest reported without dividends comes
from both-holders who report only interest and from interest-only
holders: 13.7 × (r − 0.47) + 16.4 × r = 6.2, which gives r = 42%. A
reporting rate of 42% for interest cannot coexist with at least 47% of
both-holders reporting it. So, taken together with assumptions 1 and 2,
the per-item assumption is rejected by the two files directly, without
any model. In the binary version, interest-only holders report interest
at no more than 38% (6.2 / 16.4) and both-holders at no less than 47%.
Part of that gap is that both-holders hold more interest, which a
size-dependent reporting rate would absorb; the step 31 combinations
check, which conditions on size ranges and cells, still finds about 3
points too many units holding none of the three items, so the amount does
not explain all of it. (A population difference in who holds both items
could explain the remainder; the two files cannot tell which.)

Two lessons follow. First, the PUF's joint distribution of holdings is
more information than the item-by-item cell table uses, and it can test
and in part pin down joint reporting (section 7.4). Second, every added
dependence of reporting needs either an outside fact or an assumption; the
PUF's distributions alone run out after the main effect of the amount.

## 2. Statistical matching and data fusion

### 2.1 Theory

The local notes (`05_lit_survey_statistics.md`, section 5;
`09_imputation_methods.md`, section 6; `minimal/12_CROSSWALK_THEORY.md`,
section 3) already cover the conditional independence assumption,
Fréchet bounds, file concatenation and test equating. What follows is
only what is new.

**The standard texts treat a shared variable that means different things
as a failed precondition, not as something to model.** The usual practice
is to "harmonize" the shared variables before matching and to check that
their distributions agree across the files. The StatMatch package (D'Orazio)
measures the agreement with the Hellinger distance (0 for identical
distributions, 1 for distributions with no overlap), and Eurostat work
treats a distance under 0.05 as acceptable. That is a check on the two
distributions of the shared variable, which is exactly what fails for
interest; it cannot tell whether the variable means the same thing.

**One line of theory models error in the shared variables.** Di Zio and
Vantaggi (2017) study statistical matching when the common variables are
misclassified (recorded in the wrong category). Without an assumption
about the misclassification they derive the set of joint distributions
consistent with the data; restrictions on the misclassification shrink
it. Capotorti and Buonumori (2018) add that the set can be non-convex,
falling into separate pieces, because the matching problem no longer
splits into independent pieces by value of the shared variable. Both read
in abstract only. The formal answer, in other words, is a range, and an
irregular one.

**With a file that holds both versions of the variable, translation is
ordinary measurement error.** Kim, Berg and Park (2016) show that an
"external calibration" study (a small file with both the true and the
error-prone value) plus a main file (error-prone value and outcome) is a
statistical matching problem, and solve it by fractional imputation (each
missing value replaced by several weighted draws). Their simulations show
how much ignoring the error costs: imputation under conditional
independence estimated a slope of 0.54 against a true 1.00.

**Bridge samples.** Wherever the translation has been done well, it rested
on records coded both ways:

- Clogg, Rubin, Schenker, Schultz and Weidman (1991) multiply imputed
  1980 industry and occupation codes into 1970 census samples from a
  sample coded both ways.
- NCHS comparability ratios for the move from ICD-9 to ICD-10 came from
  about 1.8 million 1996 death certificates coded both ways. NCHS warns
  that applying them to age, race or sex subgroups "may not be
  appropriate".
- Schenker, Raghunathan and Bondarenko (2010) translate NHIS self-reports
  of hypertension, diabetes and obesity into clinical measures using
  NHANES, which has both for the same people. This is "translate one
  file's variable into another file's concept, then analyse it with that
  file's other variables". Identification rests entirely on NHANES
  holding both versions.
- Schenker and Parker (2003) bridged multiple-race reports to single
  races from NHIS respondents who gave both; the shares allocated varied
  from near zero to over 50% across states.

The failure these report is always the same: **a translation estimated
over a whole population is wrong within subgroups.** That is the
argument for the rebuild's cells, and against a single translation per
item.

**A small joint file is worth a lot.** Fosdick, DeYoreo and Reiter (2016)
fuse two files with a third small "glue" file that observes the
never-jointly-seen variables together, in a validation built by splitting
a real survey of 3,567 complete records. The distance to the true joint
distribution fell from 0.104 with no glue to 0.052 with the richest glue;
the Fréchet bounds alone left each cell probability uncertain within a
range about 0.16 wide. Without glue, the model's own structure pushed the
answer toward stronger dependence than conditional independence would
give: with no joint data, the result reflected the model's built-in
tendencies, the same point as section 6.2, item 4.

**The closest published analogue: Schifeling, Reiter and DeYoreo (2019),
"Data fusion for correcting measurement errors"** (Journal of Survey
Statistics and Methodology; read). The American Community Survey records
educational attainment as reported; the National Survey of College
Graduates is treated as the truth. The 2010 files are not linked. Sex,
four age groups and black or not are treated as measured without error
in both. They model three pieces: true attainment given those variables,
whether a person reports with error, and which wrong value is reported.
Identification comes from two assumptions:

1. the distribution of true attainment given the error-free variables is
   the same in both files (the rebuild's assumption 1 of section 1.3);
2. a restricted error model. They show that at most (number of reported
   categories − 1) × (number of cells of the error-free variables) error
   parameters can be identified, so they use a rich model for whether an
   error occurs and a simple one for which wrong value is given.

Validation: seven error models compared by how many of 80 population
shares from the "true" survey fall inside 95% intervals from the
corrected ACS (73, 75, 71, 76, 74, 67, 54), plus mean income by degree
against the CPS. They chose the model whose priors came from a 1993
linked file, adjusted by judgment because a definition had changed.
Ignoring the reported value entirely (conditional independence) implied
error rates of 35% to 37%, which they reject as implausible; error rates
were 5% to 9% overall but about 31% to 34% for professional degrees.
Their own caveat: diagnostics "only can reveal problems with the model
specification; they do not indicate that a particular specification is
correct."

The mapping to the rebuild is direct: the PUF is the gold standard,
the SCF the error-prone report, married, children and wages the
error-free variables. Their counting bound is the same counting as
section 1.4, and their coverage check (do the translated survey's shares
cover the gold standard's within cells?) is one the rebuild can use.

**Recoding by optimal transport.** Garès and coauthors (2020) and Garès
and Omer (2022) recode a variable coded one way in one database and
another way in a second, by optimal transport with a cost based on
distance in shared covariates (packaged as OTrecod; Guernec and coauthors
2023, R Journal, mostly read). Their stated assumption ("for all x the
probability distributions of Y^A and Z^A given that X^A = x are
respectively equal to those of Y^B and Z^B given that X^B = x") is the
rebuild's population assumption. It handles categorical outcomes only,
and its validation measures agreement between methods, not accuracy
against truth.

**Anchoring vignettes** (King, Murray, Salomon and Tandon 2004) are the one
design-based method for translating reports across groups with different
reporting styles when nobody is measured both ways: everyone rates the
same hypothetical people, whose true level is fixed by construction, and
differences in those ratings reveal differences in reporting style. Tests
against objective measures have rejected its assumptions in one study
(Bago d'Uva, Lindeboom, O'Donnell and van Doorslaer 2011) and supported it
in another (van Soest and coauthors 2011). The PUF and the SCF share no
item that plays the vignette's role.

**Bounds for data combination.** Cross and Manski (2002) give the
identified set for an outcome's mean by group when the outcome and the
group come from different files: within a cell, the mean of Y for a
group with share p can be no lower than the mean of the poorest share p
of the cell and no higher than the mean of the richest share p. Fan,
Sherman and Shum (2014) show that pairing the files by rank in the same
direction and in opposite directions are the two extremes of such a set,
so rank matching is one endpoint of the bounds, not an identified answer.

### 2.2 Practice in tax and transfer models

The question asked of each model: when a variable is on both the tax file
and the survey but means something different in each, what does the model
do about it, and how does it check the result? Sources were opened by
helper searches; reading depth is in the reference list. cbo.gov,
urban.org and brookings.edu refused automated access, so CBO's procedure
is reconstructed from quoted text in search results and from CBO slides
that did load.

**The common pattern: no US model translates survey amounts into tax
concepts before matching.** Each limits what the survey's version of the
shared variable is allowed to decide instead.

| Model | What the survey's shared income amounts are used for | Adjustment to tax concept first? | Validation reported |
|---|---|---|---|
| CBO (Perese 2017) | Within 15 demographic cells, a regression of total income on income items is fit on tax data only, applied to both files, and both are sorted on the prediction and paired in order with weights split | None described | None found; CBO's 2024 slides say "it is not clear that the statistical match preserves the relationships" with race, so race from the match is not used |
| JCT (JCX-75-15) | Only as yes/no "presence of" indicators in the cell definitions; within cells, sorted by income and paired top to top | None; using presence only avoids relying on survey dollars | None for the match; weights calibrated to about 100 targets |
| Treasury OTA (Technical Paper 12, 2023) | Wealth for tax records from SCF donors by hot deck or predictive mean matching in cells of total income, age, marital status, interest, dividends, property taxes; stocks only to units with dividends, bonds only to units with interest; estate-tax donors above $5.34 million | None described | Estate tax computed from imputed wealth calibrated to actual receipts in 10 cells |
| TPC (Rohaly, Carasso, Saleem 2005; 2021 update) | Same predicted-income sort as CBO; wealth from SCF probits and log regressions on SCF-definition incomes, then calibrated to counts and totals | None; the SCF income predictors are in SCF definitions | Means and variances of transferred variables; wealth shares by income group correlate 0.93 with the SCF before and 0.95 after calibration; estate-tax counts and amounts within 3% to 5% after reweighting |
| PWBM (Ricco 2020) | Predicted-income sort as CBO, explicitly because of "conceptual differences between income as defined in either dataset" and survey under-reporting of business income | None beyond the predicted-income device | Targets before and after reweighting |
| PSL taxdata | Predicted taxable income, fit on the PUF, sorts CPS and PUF within cells | Yes, but levels only: CPS wages, interest, dividends, business income scaled by one ratio per state per item to IRS totals; who has a zero is unchanged | Match gaps in predicted value; loose checks of totals |
| PolicyEngine Enhanced CPS | PUF variables onto the CPS from demographics only (no income predictors); SCF onto the CPS from SCF-reported wages, interest plus dividends, pensions | None | Calibration to thousands of administrative targets; no published accuracy of the imputations found |
| Urban TRIM3 | Transfer receipt: reporters kept if simulated eligible; some eligible non-reporters marked as trusted "no"; the rest drawn at random with probabilities tuned by hand until administrative counts by unit type, benefit size, state and citizenship are met | The fill-in is itself the adjustment to the administrative concept | Against administrative counts; and, by Census with linked records, how many fill-ins land on true recipients (below) |
| CBO transfers (Habib 2020 slides) | Administrative totals first converted to the survey's terms; probit of receipt on CPS; receipt assigned in proportion to predicted probability until the count is met | As TRIM3 | Receipt rates by income relative to poverty, before and after |

Read as a group, these say three things for the rebuild.

1. **The predicted-income sort (CBO, TPC, PWBM, taxdata) is a way of
   not translating.** The regression is fit on tax data and the survey's
   reported items are pushed through it, so a survey household's position
   in its cell depends on a weighted sum of its reported items. The
   weighting reduces the influence of any one badly reported item but
   assumes the survey's items mean what the tax items mean. CBO, TPC and
   PWBM all match whole records; none uses the matched file to learn a
   third variable's relationship with a translated item, and none reports
   a check of the joint distributions after matching. John's PWBM
   documentation gives the reason for the device in so many words.
2. **Treasury's wealth imputation is the closest US analogue to the
   rebuild's task** (SCF wealth onto tax records), and it handles the
   interest and dividends problem with coarse cells and two logical links
   (stocks only where there are dividends, bonds only where there is
   interest), not by adjusting SCF incomes. Its check is downstream and
   untargeted by the imputation itself: the estate tax that the imputed
   wealth implies, against receipts.
3. **The transfer-program corrections (TRIM3, CBO's transfer step) are
   the cell method's direct ancestors.** Keep reporters, add recipients
   among non-reporters until administrative counts within groups are met,
   choose them by a probability model or at random. Two details matter.
   TRIM3 sets aside a share of eligible non-reporters as trusted "no"
   answers before drawing, which is a hand-set version of "not every
   zero-reporter is a candidate". And Census staff later checked TRIM3's
   fill-ins against linked state SNAP records (Shantz and Fox 2018): per a
   search snippet of one of their tables, not verified against the paper,
   63.6% of true SNAP recipients are recipients in TRIM3 against 54.1% in
   the raw CPS. The counts come out right by construction; the fill-ins
   land on true recipients only partly. That is the only published check
   found of "who" in a count-constrained fill-in, and it is a warning about
   how much the choice of households can be trusted even when the totals
   are exact.

Outside the US:

- **UK, the HBAI "SPI adjustment"** (Burkhauser, Hérault, Jenkins and
  Wilkins 2018). Survey adults above a high income line in four cells get
  their gross income replaced by the mean of the matching cell of HMRC's
  Survey of Personal Incomes, and the SPI count becomes a weighting
  target. The authors find the under-coverage reaches well below the
  adjusted group and recommend replacement over the top 5% by 0.1
  percentile groups. One finding bears directly on the rebuild: because
  total income is replaced and its components are not, **the components
  no longer add up to anything the tax data support, and analysis by
  income source becomes invalid.** Translating one item at a time carries
  the mirror-image risk for the relationships between items.
- **Statistics Canada, SPSD/M.** Survey (now the Canadian Income Survey)
  matched to tax data by categorical matching and random imputation; high
  earners are cloned and their survey incomes replaced by tax values, with
  their count set to the tax agency's. This is the clearest case found of
  replacing survey income with tax values, and it is limited to the top.
- **EUROMOD** draws entitled households at random to match caseload
  rates, with no reporters-first rule; for Greece it splits survey incomes
  into declared and evaded parts using under-reporting factors by income
  source (Leventi, Matsaganis and Flevotomou; abstract only). **Australia's**
  Treasury model makes no alignment to tax data; an ABS comparison finds
  nil or negative income as the main income situation for 3.3% of
  households in tax and benefit records against 0.6% in the survey, the
  same "survey misses losses" pattern as SCF business income. **Eurostat's**
  matching of household wealth into EU-SILC matches on gross income and
  assumes conditional independence (Lamarche, Oehler and Rioboo 2020;
  search excerpt only).

What Federal Reserve staff have published on the SCF against tax data is
in section 3.5.

## 3. Measurement error and misreporting without linked data

### 3.1 Vocabulary

- **Classical measurement error:** report = truth + noise, with the noise
  independent of the truth. Omission of small amounts is not classical:
  the error depends on the truth (it is −X\* for a holder who says zero).
  Bound, Brown and Mathiowetz (2001) survey such *nonclassical* survey
  errors.
- **Misclassification:** error in a categorical variable, such as
  "holds interest: yes or no".
- **Nondifferential error:** the error carries no information about the
  outcome beyond the truth (assumption 3 of section 1.3).
- **Deconvolution:** recovering the distribution of the truth from the
  distribution of the error-prone report when the error distribution is
  known; the name comes from the fact that the report's density is the
  truth's density "convolved" (blurred) with the error's.
- **Validation data:** records holding both the report and the truth. With
  them, the error is estimated directly. Without them, it must be
  identified from outside information or assumed.
- **Missing not at random:** whether a value is missing depends on the
  value itself. A survey zero that hides a positive amount is a missing
  value without a missing flag, and small amounts are missing more often,
  so this is missing not at random (`minimal/12_CROSSWALK_THEORY.md`,
  section 3).

### 3.2 What an outside distribution of the truth identifies

This is the literature closest to the rebuild's structure, because the
PUF supplies exactly an outside distribution of the true amount.

- **Hirano, Imbens, Ridder and Rubin (2001, Econometrica)**: in a panel
  with attrition, a "refreshment sample" (a fresh sample of the same
  population at the second date) supplies the true distribution of the
  variable that attrition hides. With it, a model in which dropping out
  depends on the always-observed variables plus a main effect of the
  hidden variable, with no interaction between them, is just identified.
  They call it the "additive nonignorable" model.
- **Sadinle and Reiter (2019, Biometrika)**: the same idea with any number
  of variables. The response model is logit P(missing | x, y) = α(x) +
  β(y); the paper calls the model additive "because there is not enough
  information to identify interactions between X and Y". A known mean of
  the hidden variable identifies only a linear β; a known full
  distribution identifies any function β. The identified answer is the
  "information projection": the distribution closest to the observed one,
  in the entropy sense, among those matching the known margin. The paper
  assumes the margin is exactly right and does not study a wrong one.
- **Akande, Madson, Hillygus and Reiter (2019)**: each added outside
  margin identifies one more parameter of the response model. They stress
  the approach "is not assumption free" and recommend sensitivity
  analysis.
- **Ridder and Moffitt (2007), "The econometrics of data combination"**
  (Handbook of Econometrics 6B; 2005 draft read in full). Their section
  4.5 is the rebuild's problem with the cells left out: one sample has the
  outcome and a misreported binary variable, another has only the true
  variable's distribution. Assuming the report carries no information on
  the outcome beyond the truth (nondifferential error), the chance that a
  true holder reports is identified for one-sided error (only true holders
  misreport, by saying no) as P(report) / P(true holder), which is the
  cell method's reporting rate. The general model with errors in both
  directions is not identified from two margins; one rate, or their
  ratio, must be fixed. For a
  continuous variable with classical error they cite Hu and Ridder, who
  back the error distribution out by deconvolution; Schennach's review
  confirms that Hu and Ridder (2012) assume classical error, which
  omission is not.
- **The same chapter, section 4.2.2, on exclusion restrictions**: if some
  shared variable can be left out of one relationship, variation across
  its values gives a linear system that can identify what the margins
  alone cannot, and gives a test when there are more values than
  unknowns. Applied to the rebuild, this means the cells can buy
  identification of something the cell method now assumes away, at the
  price of a different assumption (section 7.4 works an example).

Section 1.4 applied this to the rebuild. The PUF's distribution within a
cell identifies how reporting depends on the true amount in that cell.
Because a non-holder never reports, any dependence of reporting on net
worth or on other items exists only among holders: it is an interaction
between the hidden amount and the observed variables, which is exactly
the part these papers say an outside margin cannot identify.

Two further results close the door on finding it in the data:

- **Molenberghs, Beunckens, Sotto and Kenward (2008)**: "each MNAR model
  fit to a set of observed data can be reproduced exactly by an MAR
  counterpart." (MNAR and MAR: missing not at random and missing at
  random.) Two models with identical fit can disagree completely about the
  hidden values.
- **Linero and Daniels (2018)**: the "extrapolation distribution" (the
  missing values given the observed ones) "is not identified"; parametric
  selection models "may 'identify away' the missing data problem" through
  their assumed shape.

### 3.3 Bounds when the truth's distribution is known

Horowitz and Manski (1995) bound a distribution when an unknown share of
observations are erroneous. Kreider, Pepper, Gundersen and Jolliffe (2012,
JASA), and Kreider, Pepper and Roy (2012, working paper read in full),
study food assistance with misreported participation and bring in the
administrative caseload, which gives the true participation rate (51%
against 39% reported for WIC). Their findings, which carry straight over:

- **Knowing the true margin alone barely narrows the bounds.** It fixes
  the *net* misreporting (true rate minus reported rate), not how many
  misreport in each direction or how the misreporters differ on the
  outcome. In the WIC example the worst-case width of the bounds on the
  effect stayed at 1.32 with or without the caseload; in the authors'
  words, "the ambiguity created by classification errors can be
  substantial even if the true and self-reported rates are similar".
- **Adding "no false positives" pins the hidden mass exactly** (12% in the
  example) and narrows the width to 1.09; adding a monotonicity
  assumption about outcomes narrows it to 0.85. Only further assumptions
  make the bounds informative.

For a binary item with no false reports and a known true share P\*, the
helper search derived the bounds (not quoted from either paper; checked
here): with P the reported share, d = P\* − P the hidden share, p11 the
share reporting and having outcome Y = 1, and p10, p00 the shares not
reporting with Y = 1 and Y = 0,

    P(Y = 1 | true holder) lies between (p11 + max(0, d − p00)) / P*
    and (p11 + min(d, p10)) / P*.

The width is roughly d / P\*, the share of true holders that are hidden.
For SCF interest in the married $75,000–$200,000 cell, d / P\* is about
(47.3 − 20.0) / 47.3 = 58%: more than half of the true holders are hidden,
so the worst-case bounds on anything about holders or non-holders are
wide. This is the formal version of the table in section 1.3.

### 3.4 Misreporting models estimated on linked data and carried elsewhere

When linked data exist somewhere, the standard practice is to estimate
the distribution of the truth given the report and covariates there and
carry it to unlinked data. The assumption is that this conditional
distribution is the same in both places. How well it carries has been
tested:

- **Mittag (2019, AEJ: Economic Policy; IZA working paper read)**: New York
  SNAP records linked to the ACS; 26% of true recipients do not report.
  He estimates the distribution of true receipt and amount given the
  report and about 35 covariates, publishes only the parameters, and
  users draw from it. Carried from 2009 to 2010 with levels re-matched to
  county totals: the poverty rate among recipients is 40.1% against a
  true 38.6% (survey reports: 44.3% ACS, 55.9% CPS). Carried from eastern
  to western New York: receipt 17.0% against true 17.4% (survey 13.5%).
  The covariates explain 98% of the variation in receipt across small
  areas but only 60% for amounts. A formal test rejects equal parameters
  across years and regions; the error is small next to the survey's.
- **Fox, Rothbaum and Shantz (2022, AEA Papers and Proceedings; read)**:
  CPS linked to SNAP records in 8 states; each state in turn imputed from
  the other seven. Receipt: 0.200 in the records, 0.201 imputed. Amounts:
  $2,613 imputed against $2,388, a significant gap. They also show the
  survey-only shortcut of inflating reported receipt by one
  under-reporting rate does not hold.
- **Meyer, Wu, Stadnicki and Langetieg (2023 slides, read via
  extraction)**: unemployment insurance in the CPS against Form 1099-G:
  39% of true recipients report none; only 0.33% report benefits they did
  not receive; 24% of 1099-G recipients do not report the benefits on
  their 1040.
- **Census NEWS** (Bee, Mitchell, Mittag, Rothbaum, Sanders, Schmidt and
  Unrath 2023; read): neither survey nor records treated as truth for
  earnings; the model is identified only by a "mean-reversion" parameter
  that "cannot be estimated, and must be assumed". Varying it from 0.7 to
  1.0 moves the share of people whose survey earnings are used from 6% to
  31%, while median income moves by less than 0.1%. When sources disagree
  about whether a person has any income, the non-zero one wins; a survey
  report with no record is treated as income the records do not see, a
  genuine coverage difference, not an error.
- **Flachaire, Lustig and Vigorito** (Uruguay, 1,471 adults linked between
  the household survey and tax records; read via extraction): the top 1%
  by tax income reports about 60% of it in the survey; "a significant
  extent of reranking"; about 10% of people with positive tax income
  report zero, concentrated in the lower tail.
- **Imboden, Voorheis and Weber (2023)**, already in
  `linked_survey_tax_evidence.md`: only 39% of the self-employed stay in
  the same fifth of the distribution in survey and tax data, against 62%
  of wage earners.

Three lessons: receipt (who holds) carries across places and years better
than amounts do; re-matching levels to known totals is part of every
successful transfer; and ranks are not preserved between survey and tax
reports, least of all for self-employment income.

### 3.5 What is known about the SCF specifically

From Johnson and Moore (2008), Bricker, Henriques, Krimmel and Sabelhaus
(2016, Brookings Papers, read with its appendix) and Kennickell (1998,
read):

- **The SCF income questions are built on Form 1040 lines**, and
  respondents are invited to consult their returns; about 21% did in 2004
  (about 25% at AGI $50,000 and over). Bricker and coauthors read this as
  making SCF and tax concepts similar "to some extent ... by construction".
  So treating SCF items as reports of 1040 lines follows the Fed's own
  design. To compare with tax data they map items to lines (X5704 to
  Schedules C and F, X5714 to Schedule E, X5708 interest, X5710
  dividends, X5712 gains), add back business losses and cap capital
  losses at $3,000, as the rebuild does.
- **The Fed does not check answers against returns**: doing so "would
  involve an implicit audit". No person-level SCF–IRS comparison will be
  published.
- **Some SCF income values are partly informed by tax data.** Kennickell
  (1998): the imputation of missing answers uses "frame data for the list
  sample", the SOI information used to draw the high-wealth list sample,
  which public users do not have. Business income is heavily imputed: in
  1995, 40.9% of business income dollars were imputed from entirely
  missing answers and 17.5% more from range answers (1.7% for wages).
- **The public file's interest zero is not all omission, and the PUF's
  zero is not all non-holding.** Banks need not send a 1099-INT for under
  $10. Bricker and coauthors show the share of tax units with taxable
  interest falling from 45% (2007) to 30% (2013) while about 60% of SCF
  families held interest-bearing accounts. The rebuild targets the PUF's
  concept, so this does not change the target, but it means the PUF's
  "none" includes holders of tiny balances.
- **Business income**: Johnson and Moore: "households often report zero
  instead of the actual loss"; respondents misclassify income across
  business lines, so they combine them. Bricker and coauthors rescale
  only three broad wealth categories because misclassification across
  finer ones makes category-by-category scaling reshuffle the
  distribution, and they read part of the SCF's capital income shortfall
  as income labelled "wages" that is really business income. A Tax Policy
  Center report (search excerpt only) also finds SCF closely held business
  income "roughly twice as large" as SOI's and suggests respondents give
  economic rather than tax income.
- **The interviewer records whether the respondent referred to tax
  returns** (X7451) and other documents (X6536, X7452–X7455). Step 35
  already found that among households with any business amount, 22.6% of
  those who consulted returns report a loss against 5.9% of the rest and
  29% on the PUF. The Canadian linked study (Grondin and Michaud 1994,
  in `linked_survey_tax_evidence.md`) found non-reporting of interest and
  dividends falling from about 37% to 9%–13% when respondents used
  records. The consulting households are a selected group (richer, more
  complex returns), so they are not a validation sample. In the
  language of section 3.2 consulting is not a "shadow variable" (one
  unrelated to reporting given the truth), since it is meant to change
  reporting; it is closer to an instrument for reporting, which needs
  conditions unlikely to hold exactly. Section 7.5 discusses what the
  consulting households can still be used for.

## 4. Distribution-matching corrections in survey–tax work

These methods change a survey so that some of its distributions match
tax or administrative data. Each makes one of two assumptions, which
Bourguignon (2018) and Lustig (2019) put plainly: **reweighting** assumes
the survey's values are right and its counts wrong (the people are
missing); **replacing** assumes the counts are right and the values wrong
(the amounts are misreported). With common support, any reweighting can
be rewritten as a replacing and the reverse, so the choice is a choice of
assumption, not of technique. Lustig's recommendation, from linked
Uruguayan data where "there is no dominant method": "report ranges rather
than single corrected inequality measures".

**Larrimore, Mortenson and Splinter (2022 FEDS Note, read; 2023 JPAM,
abstract).** Unemployment insurance in the CPS against Form 1099-G in a 5%
IRS sample. Groups are 100 percentiles of market income plus alimony and
Social Security, built the same way in both files. Within each
percentile, benefits are given to CPS people who reported none, chosen at
random, until the recipiency rate and average benefit match the IRS.
Reporters are kept, even where their report exceeds the 1099-G. Checks:
totals against BEA and Treasury; poverty estimates "are not sensitive to
which individuals are randomly assigned". No check of who was chosen.
This is the cell method's nearest ancestor (as `design/cell_method.md`
already says), with random choice where the cell method uses the model.

**Blanchet, Flores and Morgan (2022, Journal of Economic Inequality, not
Review of Income and Wealth; read via extraction).** Estimate the ratio of
survey density to tax density by income; above a "merging point" where
the ratio starts to fall, reweight survey records so counts in tax
brackets match, holding demographic margins; beyond the survey's top,
replace by cloning survey records and giving each the tax income for its
rank. Assumption: "the correction does not re-rank observations"; the
authors say it "inevitably interprets as 'nonresponse' biases that are
partly due to misreporting". Validated on simulated populations, not on
linked data. Flachaire, Lustig and Vigorito tested it on linked Uruguayan
data: substantial re-ranking, a merging point chosen too high (68th–72nd
percentile against a better 40th–50th), replacing more robust than
reweighting.

**Distributional national accounts.** Piketty, Saez and Zucman (2018) use
the CPS for non-filers (reweighted and, for very small wage earners who
report zero wages in the CPS, wages doubled) and allocate income not on
tax returns by survey probabilities in 40 bins of taxable income decile ×
married × aged, randomly within bins. Auten and Splinter (2024, JPE)
allocate under-reported income from IRS random-audit tabulations,
randomly within 11 groups by reported income and 10 groups by
misreporting ratio, and put about 15% of it in the top 1% against about
50% under the Piketty–Saez–Zucman proportional rule. **The dispute between
them is a dispute about who misreports**, which neither file settles, and
the allocation rule chosen matters for the top shares each reports.
Piketty, Saez and Zucman's reply
argues that part of what Auten and Splinter treat as evasion is legal
untaxed business income: a definitional gap, not a reporting error. The
same distinction applies to SCF business income at twice the PUF's.

**Census CPS ASEC.** Hokayem, Raghunathan and Rothbaum (2022, JSSAM; slides
read): replacing the hot deck with a richer regression imputation shows
that much of the apparent bias was "match bias", relationships with
variables left out of the hot-deck cells weakened by the imputation,
mainly at the bottom; nonresponse that depends on income itself matters
at the top (adding linked earnings raised the top 1% share from 12.9% to
14.9%). The lesson for the rebuild's cells: conditioning a fill-in on few
variables weakens the filled-in variable's relationship with everything
else. That is what the model-based choice of who inside a cell is for.

**UK and Canada** are in section 2.2. The UK finding that replacing total
income without its components leaves components that no longer add up to
anything the tax data support is the mirror of the cross-item problem.

**The Distributional Financial Accounts** align SCF categories to
Financial Accounts concepts first, then scale. Bricker and coauthors scale
only broad categories because finer ones are misclassified. Both are
instances of "fix the concept first, then match distributions, at a level
of aggregation the concept can bear".

**What this literature does not have.** No paper found measures whether
survey zero-reporters of an item hold smaller amounts than similar
reporters (the reviewer's first point). No paper found tests whether
correcting items one at a time preserves joint holding patterns (the
second point). Every source that discusses it warns that forcing a survey
to match tax data where units, coverage or concepts differ "corrects" real
differences (the fourth point); none gives a way to tell the two apart
from the two distributions alone.

## 5. Optimal transport, rank matching and quantile mapping

### 5.1 What optimal transport is, and what it does not say

Optimal transport asks for the cheapest way to move one distribution onto
another, given a cost for moving a unit of mass from value s to value p
(Peyré and Cuturi 2019 is the standard reference). A **coupling** is a
joint distribution of (survey value, PUF value) whose two margins are the
survey's and the PUF's distributions: in the discrete case, a table whose
rows sum to the survey weights and columns to the PUF weights. A
**transport map** is the special case where each survey point goes to one
PUF point.

- In one dimension, with any convex cost, the optimal coupling is the
  monotone one: sort both files and pair them in order. That is rank
  matching, F_P⁻¹(F_S(s)). In several dimensions, with squared-distance
  cost, Brenier's theorem gives a unique optimal map, the multivariate
  analogue of "monotone".
- **A lump of ties cannot be handled by a map.** One survey zero cannot
  become both $0 and $40. Only a coupling that splits the lump, or a rule
  that breaks the ties, can.
- **Optimal transport is a statement about two distributions only.** Many
  couplings reproduce both margins exactly. Choosing the cheapest under a
  convex cost is choosing perfect rank correlation between report and
  truth, the upper Fréchet bound. The data do not select it; the cost
  function does. Whether matched units are "the same kind of unit" is
  assumed through the cost.

**Rank matching with a lump at zero, ten records each.** PUF interest: 0,
0, 0, 0, 0, $40, $120, $300, $900, $5,000 (50% zero). SCF: eight zeros,
$2,000, $15,000 (80% zero).

- Ties left as ties: all eight SCF zeros share one rank and map to $0; the
  two reporters map to $900 and $5,000. The result is still 80% zero, and
  $40, $120 and $300 never appear. A quantile map cannot create holders
  out of a lump.
- Ties broken: give the zeros ranks 1 to 8. Ranks 1–5 map to $0; ranks 6,
  7, 8 map to $40, $120, $300. The margin now matches exactly, and **which
  three SCF households become holders is decided entirely by the
  tie-breaking rule**. The data say nothing about it. This is the same
  point as Athey and Imbens (2006) on ties in changes-in-changes, and as
  the dead end recorded in `31_DISCUSSION_2026-10-18.md`, section 2.

Conditional versions do not remove this. The **Knothe–Rosenblatt
rearrangement** (rank-map the first variable, then the second within the
conditional distribution given the first, and so on) is optimal transport
with a strict priority order over the variables (Carlier, Galichon and
Santambrogio). It assumes perfect rank correlation within each conditional
cell, depends on the order of the variables, and maps later variables in
ever thinner cells. **Vector quantile regression** (Carlier, Chernozhukov
and Galichon 2016) and **Monge–Kantorovich ranks** (Chernozhukov, Galichon,
Hallin and Henry 2017) define multivariate conditional quantiles and ranks
through transport maps, for continuous distributions; neither addresses
lumps at zero, as far as the abstracts show.

### 5.2 Climate-model bias correction: the four-step rule's closest relative

Climate scientists correct a model's simulated rainfall so its
distribution matches station observations ("quantile mapping"), and
then feed the corrected series to downstream models of river flow or
crops. That is translate-then-learn: the downstream model plays the role
of net worth given interest. Their experience is the largest body of
evidence on what item-by-item distribution matching does.

**Zeros ("dry days").** Two methods for a model with too many dry days,
which is the survey's situation (too many zeros):

- **Frequency adaptation** (Themeßl, Gobiet and Heinrich 2012; read through
  the xclim code that implements it, not the paper). The share of the
  model's dry days to convert is (P0_model − P0_obs) / P0_model. Those
  dry days whose rank lies between the two dry shares get random values
  between the wet-day threshold and the observed value at the model's dry
  share; quantile mapping then runs on the adapted series. With the
  rebuild's cell, (0.80 − 0.527) / 0.80 = 34% of SCF zeros would be
  converted, given values from the PUF's band between its 53rd and 80th
  percentiles. Apart from how the households are chosen, **this is the
  four-step rule.**
- **Singularity stochastic removal** (Vrac, Noël and Vautard 2016; read
  through the SBCK package documentation): replace every zero by a random
  value between 0 and the smallest positive value, quantile-map, then set
  everything below the threshold back to zero. Cannon's MBC package does
  the same with a "trace" threshold.

In both, which zeros become positive is random. A 2025 paper on France
criticises exactly this: "randomly chosen dry days are turned into wet
days … without any regard to the autocorrelation", the counterpart of
ignoring correlation with other variables.

**Dependence between variables.** Item-by-item quantile mapping leaves the
rank structure among variables (the *copula*: the joint distribution of
the ranks, which is what remains of a joint distribution once the margins
are removed) as it was, except where ties are broken and where values
cross the zero line. So it cannot repair a joint pattern that the
survey's reporting distorted. Vrac and Friederichs (2015) state that
one-dimensional methods "cannot produce correct multidimensional
properties". The fixes, from cheapest:

- **Reordering to a template** (the "Schaake shuffle", Clark and coauthors
  2004; ensemble copula coupling, Schefzik, Thorarinsdottir and Gneiting
  2013): correct each margin, then reorder the values so their joint ranks
  copy a chosen template. Ties at zero are allocated at random. The
  question becomes which template's dependence is right, the survey's
  (distorted by correlated non-reporting) or the reference's.
- **Iterated rotations** (Cannon's MBCn, 2018): rotate the variables,
  quantile-map each rotated axis, rotate back, repeat; a final step keeps
  the exact one-dimensional margins and takes only the joint ranks from
  the iteration. Stochastic, unstable as the number of variables grows
  (François, Vrac, Cannon, Robin and Allard 2020).
- **Coupling-based draws** (Robin, Vrac, Naveau and Yiou 2019, "dOTC"): bin
  both samples, solve the discrete optimal transport problem between the
  bins, then for each model point draw its destination bin from the
  coupling and a value within it. This uses the coupling, not a map, to
  decide who goes where, and is the climate method closest to the cell
  method's draw.

**The critical reviews.** Maraun (2016) and Maraun and coauthors (2017,
Nature Climate Change; abstracts read): bias correction assumes the model
represents the processes plausibly; "bias correction cannot overcome
major model errors"; "a typical cross-validation is unable to identify
improper use of bias correction". A quantile map is valid only if the gap
between the series is a fixed monotone distortion of the same quantity.
Where it is a real difference, the map removes it.

**Translate-then-learn evidence.** Meyer, Kohn, Stahl, Hakala, Seibert and
Cannon (2019, Hydrology and Earth System Sciences; abstract read) corrected
temperature and precipitation for alpine catchments by univariate and by
multivariate methods. The corrected margins were identical; the
temperature–precipitation relationship differed; and that difference
changed simulated snowfall, snowmelt and glacier loss, with the
multivariate version closer to the reference data. A 2026 review of 39
studies (EGU preprint, seen through a search summary only) describes a
"validation gap": better statistical correction does not consistently
give better downstream simulations, and it depends on whether the
downstream model is sensitive to the joint behaviour. For the rebuild,
the downstream model (net worth given income items) is sensitive to the
joint pattern: the "none of the three" group is the group the method
exists for.

**Validation practice.** François and coauthors use cross-validation
across two sub-periods. The climate literature also uses "perfect model"
or "pseudo-reality" tests, in which one model's output plays the truth and
another's is corrected toward it (known from citations here). The
rebuild's fake survey is this kind of test. The climate reviews' warning
applies to it: it confirms the method works when the planted distortion
is the kind the method assumes, and says nothing about whether the real
distortion is that kind.

### 5.3 Domain adaptation

Machine learning calls a model trained in one population and used in
another "domain adaptation". The variants differ in which piece of the
joint distribution is assumed fixed:

| Name | What differs between populations | What is assumed the same |
|---|---|---|
| Covariate shift (Shimodaira 2000) | the distribution of the inputs | the outcome given the inputs |
| Label or prior shift (Saerens, Latinne and Decaestecker 2002; Lipton, Wang and Smola 2018) | the share in each class | the inputs given the class |
| Conditional shift (Zhang, Schölkopf, Muandet and Wang 2013) | the inputs given the class, within a restricted family | the class shares, or a known part |

The rebuild's reporting problem is **conditional shift**: within a cell
the true amount has the same distribution in both files (by assumption 1)
and what differs is how the recorded value relates to it. Zhang and
coauthors show conditional shift is identified only under restrictions
(location-scale changes, in their case; the exact form known from
citations only).

Two of the rebuild's ideas are standard pieces of this literature:

- **The cell method's scaling step is the Saerens prior-shift
  adjustment.** Saerens and coauthors move a classifier to a population
  with different class shares by multiplying each case's odds by the
  ratio of new to old class odds. The cell method multiplies the odds of
  holding by one factor per cell and size range so the cell's total
  matches the PUF's. Under the assumption that, within a cell and range,
  reporting depends on nothing else on the record, the exact posterior
  for a zero-reporter is f(1 − e)/(1 − fe) (section 6.3), whose odds are
  the odds of f times (1 − e): a constant multiplier on the odds. The
  scaling step is exact under that assumption and only under it, as
  `31_DISCUSSION_2026-10-18.md`, section 1, already says.
- **The PUF-label prediction idea is covariate shift.** Fit the chance of
  holding given the assumed-correct variables on the PUF and apply it to
  SCF households. If every reporter holds, a zero-reporter's chance of
  being a hidden holder is (p_PUF − p_report) / (1 − p_report), with both
  chances given the same assumed-correct variables. Valid if the PUF's
  relationship between those variables and holding carries to SCF
  households, which is assumption 1 at the level of the individual
  household rather than the cell.

## 6. Iterative imputation with a model in the loop

John's hope is for passes that "trend toward accurately predicting the
true values": the network guesses each SCF household's true amount from
everything on its record, trains on the guess, and repeats. The cell
method is already a loop of this kind with one outside anchor, the cell
counts. What does the literature say such loops converge to?

### 6.1 The names

- **EM algorithm** (expectation–maximization; Dempster, Laird and Rubin
  1977): alternate between filling in the missing values by their expected
  values under the current model and refitting the model to the filled-in
  data.
- **Stochastic EM** (Celeux and Diebolt 1985; Nielsen 2000) and **data
  augmentation** (Tanner and Wong 1987): the same, but each pass draws the
  missing values at random from the current model instead of using
  expected values. Data augmentation with a prior on the parameters is a
  Gibbs sampler, an algorithm that draws each unknown in turn given the
  others and, run long enough, produces draws from the joint posterior
  distribution. The cell method is a stochastic EM with a constraint.
- **Chained equations** (multiple imputation by chained equations, MICE;
  van Buuren): fill in one variable at a time from a model of it given the
  others, and cycle. Filling interest, then dividends, then capital gains,
  each with its own cell constraint, is this.
- **Self-training** or **pseudo-labelling** (machine learning): a model
  labels unlabelled cases with its own predictions and retrains on them.
- **Calibrated imputation** (Beaumont 2005; Pannekoek, Shlomo and de Waal
  2013; de Waal and Daalmans 2023) and **balanced random imputation**
  (Chauvet, Deville and Haziza 2011): imputations adjusted, or random
  draws chosen, so that known totals are met exactly.
- **EM for class proportions** (Saerens, Latinne and Decaestecker 2002):
  a classifier trained in one population is moved to another by
  iterating between rescaling its predicted class chances and
  re-estimating the class shares.

No source found gives a name or a convergence theory for the exact
combination "model chances, rescaled to fixed counts, drawn, refit,
repeated". Its parts are all known.

### 6.2 What such loops converge to

The results agree with one another and are simple to state.

1. **A loop does not learn what the data cannot identify.** If the
   probability of the observed data does not depend on some feature of
   the model (call it λ, for example how hiding varies with net worth
   within a cell), then each pass fills in under the current λ, refits to
   data generated under that λ, and gets the same λ back. Deterministic EM
   therefore leaves λ where it started. No opened source states this in
   one sentence; it follows directly from the definition and was checked
   by the helper search against the sources below.
2. **In a Bayesian version, the posterior of the unidentified part equals
   its prior**, given the identified part (Florens and Simoni 2021, stated
   exactly; Linero and Daniels 2018, Statistical Science, for missing data
   not at random: "As there is no information in the data about ξ, this
   prior for ξ will also be the posterior"; Gustafson 2010 and his 2015
   book: in large samples the posterior settles on the set of values the
   data cannot rule out, and its shape within that set is the prior's).
3. **Every missing-not-at-random model has a missing-at-random twin with
   exactly the same fit** to the observed data and different predictions
   for the unobserved values (Molenberghs, Beunckens, Sotto and Kenward
   2008). Fit to the observed data therefore cannot choose between "hidden
   holders look like reporters" and "hidden holders look like the other
   zero-reporters".
4. **A flexible model "identifies away" the problem through its shape.**
   Linero and Daniels warn that parametric selection models produce an
   answer that comes from the assumed functional form, not the data. A
   network's smoothness and inductive habits play the same role: whatever
   allocation of hidden holders inside a cell it settles on, beyond what
   the counts and reporters fix, comes from how it generalizes.
5. **Random redraws add wandering and can reinforce early mistakes.**
   Self-training "overfits to incorrect pseudo-labels due to the so-called
   confirmation bias" (Arazo and coauthors 2020). Training generation
   after generation on a model's own outputs makes the tails disappear and
   the estimates drift (Shumailov and coauthors 2024, Nature); keeping real
   data in every round bounds the error (Gerstgrasser and coauthors 2024).
   The cell method keeps the PUF and SCF reporters as real data in every
   pass, and the cell counts as a fixed anchor; neither of those anchors
   fixes who inside a cell is filled in.
6. **Chained one-at-a-time fill-ins hit each margin and leave the joint
   pattern wherever the sequence takes it.** With conditional models that
   are not consistent with any single joint distribution, "it is unclear
   whether the Markov process has a probability invariant distribution"
   (Liu, Gelman, Hill, Su and Kropko 2014, Biometrika); even consistent
   ones can show order effects (Hughes and coauthors 2014). The dividends
   without interest shortfall (5.0% on the PUF, 2.4% after filling in) is
   what this literature predicts.
7. **Self-training does recover truth under a separation condition.** Wei,
   Shen, Chen and Ma (2021) prove that self-training recovers true labels
   if the classes are well separated in the features and each small group
   of a class has a much larger neighbourhood of the same class. A hidden
   holder that looks, on every recorded variable, like a true non-holder
   violates the separation condition; that is precisely the case in which
   the data cannot help.

**The contrast with Saerens's EM is instructive.** Their loop has a
well-defined, data-determined fixed point because one strong assumption
("label shift": within each class, the features are distributed the same
in both populations) makes the class shares identified, and the class
shares are what the loop estimates (Alexandari, Kundaje and Shrikumar 2020
show the likelihood is concave and EM finds its maximum). In the cell
method the class shares are supplied by the PUF and the unknown is the
allocation of individuals. The assumption that would play label shift's
role is: given true holding, size range and cell, the chance of reporting
does not depend on anything else on the record. Under that assumption the
loop is not needed for identification at all (6.3).

### 6.3 The positive-unlabelled view, and a closed form

Machine learning has a near-exact name for the extensive margin of the
problem: **learning from positive and unlabelled data** ("PU learning";
Elkan and Noto 2008; survey by Bekker and Davis 2020). A classifier must
be learned from a set of known positives and a pool of unlabelled cases
that mixes positives and negatives. SCF reporters are the known positives;
zero-reporters are the unlabelled pool.

- Under **"selected completely at random"** (the chance a positive is
  labelled, c, is the same for every positive), P(report | record) =
  c × P(hold | record), so the holding probability is the reporting
  probability divided by c. Without knowing c, even this is not
  identified (Bekker and Davis, section 3.3): an unlabelled case can come
  from a small positive class or a low labelling rate. **The PUF supplies
  c** (within each cell and size range, as the share reporting over the
  share holding), which is what makes the cell method work.
- Under **"selected at random"** (the labelling chance depends on the
  record's attributes), the problem is not identified even with the class
  prior known: "it is impossible to know if an instance did not get
  labeled because of a low propensity score or a low class probability"
  (Bekker, Robberechts and Davis 2019). Their fix assumes the labelling
  chance depends only on a known, small set of attributes. For the
  rebuild that set is "cell and true size range", which is the cell
  method's assumption.

Under that assumption the chance that a zero-reporter is a hidden holder
has a closed form. With f the chance of truly holding given the record and
e the cell-and-range reporting rate,

    P(hidden holder | reported zero, record) = f × (1 − e) / (1 − f × e).

This is the formula behind the table in `31_DISCUSSION_2026-10-18.md`,
section 5: a household with an 80% chance of holding, where holders report
at 30%, is a hidden holder with chance 0.8 × 0.7 / (1 − 0.24) = 74%.

Under the same assumption, f itself needs no loop: f = p / e, where p is
the chance of reporting given the record, which a model trained once on
the SCF as reported estimates directly (the model of passes 1 and 2). In
the example p = 0.24, so f = 0.24 / 0.3 = 0.8. The formula becomes
(p / e) × (1 − e) / (1 − p). This is the "fill in once, then divide by the
cell's reporting rate" version noted, untested, in that section of the
discussion note. It comes with a built-in check: wherever the model's p
exceeds the cell's e, f would exceed 100%, which means the reporting rate
is not constant within that cell and range, so the assumption fails there.
The count of such households is a free test of the cell method's
assumption.

### 6.4 What this means for "trending toward the true values"

- The loop can move toward the truth only along directions the data and
  the PUF constraints identify: how many hidden holders per cell and size
  range, and, given the reporting assumption, the holding probability f
  learned from reporters. It converges, at best, to the answer implied by
  the assumption, not to the truth.
- Giving the network the reported amount and net worth when it guesses
  the true amount does not add identifying information. It adds the
  network's habits. Whatever it learns about how the true amount relates
  to net worth among zero-reporters was put there by the assumption or by
  the starting point.
- The loop is still worth having, for a reason the iterative literature
  supports: it is a way to compute the fill-in that is consistent with the
  joint model of everything, rather than with a separate small model
  (`design/translation_by_latent_amounts.md`, section 6, on
  "congeniality"). That is a computational benefit, not an identifying
  one.

Diagnostics for drift or self-reinforcement, from the same sources:

1. **Different starting allocations.** Start the loop with hidden holders
   deliberately placed among the richest zero-reporters, among the
   poorest, and at random. Statistics that end up the same are fixed by
   the data and the constraints; statistics that stay apart are set by
   the start.
2. **Traces, pass by pass, of statistics the constraint does not fix:**
   the share of hidden holders in each net worth quarter within a cell,
   the spread of the model's chances, the share of last pass's hidden
   holders picked again. A steady trend is drift; chances growing more
   extreme each pass is the confirmation-bias signature.
3. **The closed form of 6.3 as a benchmark.** If the loop's fixed point
   differs much from it, the network is supplying structure the
   assumption does not.
4. **Held-out joint margins.** Treat cross-item shares from the PUF
   (dividends without interest, none of the three) as tests, not targets,
   until a decision is made to target them.

## 7. Answers to the six questions

### 7.1 (a) What kind of problem this is, and what must be assumed

**In the literature's terms** it has four names, each from a different
field, for the same structure:

- **data combination with a common variable measured with nonclassical
  error in one file** (Ridder and Moffitt 2007, section 4.5; Di Zio and
  Vantaggi 2017; Schifeling, Reiter and DeYoreo 2019);
- **data missing not at random with an outside distribution of the
  missing variable**, the "refreshment sample" structure (Hirano, Imbens,
  Ridder and Rubin 2001; Sadinle and Reiter 2019);
- for the hold-or-not margin, **learning from positive and unlabelled
  data with a known class share** (Bekker and Davis 2020);
- in machine-learning terms, **conditional shift** between two
  populations (Zhang and coauthors 2013).

**It is not identified without linked data.** The two files give one
margin of the truth and the joint distribution of report and net worth;
many joint distributions of truth, report and net worth fit both (section
1.2). For interest, the cell in section 3.3 has 58% of true holders hidden,
so the worst-case range is wide.

**What must be assumed, for interest, dividends and capital gains**, with
W the variables taken as measured alike, X\* the PUF-concept amount, X_S
the SCF report, R "reports a positive amount", Y net worth, and Z the
PUF-only variables:

1. **Same population within cells:** the distribution of X\* given W is
   the same for SCF units and PUF units (after the non-filer and tax-unit
   work).
2. **Reporters are right:** if X_S > 0 then X\* = g(X_S) for a known
   conversion g (the identity, after the capital loss cap); nobody reports
   an item they do not hold.
3. **Reporting depends only on the amount and the cell:** P(R = 1 | X\*,
   W, Y, the other items) = r(X\*, W). This contains the one assumption
   nothing can check, that reporting does not depend on net worth given
   the amount and the cell, and a checkable one, that it does not depend
   on the other items.
4. **Net worth carries to the PUF:** the distribution of Y given X\*, W
   and the PUF-only variables Z on PUF returns equals the distribution of
   Y given X\* and W among SCF units. This is the conditional independence
   assumption of statistical matching plus assumption 1 extended to Y.

Under 1 to 3, the joint distribution of X\* and Y within cells is point
identified (section 1.3), and 4 carries it to the PUF.

**What the data can check:**

- 1 and 2 together imply that at no amount does the SCF have more
  reporters per household than the PUF has holders. Where it does (SCF
  interest at $2,000 and over), one of the two fails. The cell method
  records the excess rather than removing reporters, which keeps
  assumption 2 and treats the excess as a difference in population.
- The part of 3 about other items is checked by the PUF's joint holding
  table (section 1.4); it fails by about 3 points on "none of the three".
- A free check of 3 within cells: under it, no household's chance of
  reporting can exceed its cell's reporting rate (section 6.3).
- If reporting rates are also assumed equal across cells, variation
  across cells over-identifies them and tests that (Ridder and Moffitt,
  section 4.2.2; `design/reporting_rules.md`, section 0.4, item 2).
- The part of 3 about net worth, and assumption 4, cannot be checked with
  the two files (Molenberghs and coauthors 2008).

**For business income, assumption 2 fails** (dollars about twice the PUF's,
losses reported as zero, misclassification across lines). Then two unknown
functions (who reports, and how reported dollars relate to true ones)
face one margin, and the margin pins only one of them unless reporting is
complete over some range that then fixes the dollar relationship there
(Ridder and Moffitt, section 4.5; `minimal/12_CROSSWALK_THEORY.md`,
section 4).

### 7.2 (b) Has anyone translated a variable, then learned a third variable's relationship to it?

Yes, in four settings, and validation is thin in all of them.

- **With a file holding both versions** (the common case): NHIS
  self-reports translated to NHANES clinical measures (Schenker,
  Raghunathan and Bondarenko 2010); 1970 to 1980 occupation codes (Clogg
  and coauthors 1991); SNAP receipt corrected from a linked New York file
  and carried to other states and years (Mittag 2019); Census NEWS.
  Validated by holding out states or years against records (Mittag; Fox,
  Rothbaum and Shantz): receipt carries well, amounts less well.
- **Without linked data, by a population assumption and a restricted error
  model**: Schifeling, Reiter and DeYoreo (2019), educational attainment
  in the ACS translated to the National Survey of College Graduates'
  concept, priors from a 1993 linked file, validated by whether corrected
  shares cover the reference survey's within cells. This is the closest
  precedent found.
- **Without linked data, by filling in recipients to administrative
  counts, then using the corrected file**: TRIM3, CBO's transfer step,
  Larrimore, Mortenson and Splinter. Validated by totals; the one check
  of who was filled in, made later with linked SNAP records, found the
  fill-ins land on true recipients only partly (section 2.2).
- **Climate bias correction feeding downstream models**: validated by
  cross-validation over sub-periods, by "pseudo-reality" tests, and by
  comparing downstream simulations against reference data; univariate and
  multivariate corrections with identical margins gave different
  downstream answers (Meyer and coauthors 2019).

**In tax models, nobody translates.** CBO, TPC, PWBM and PSL taxdata sort
survey records on income predicted from a regression fit on tax data;
JCT uses survey income only as yes/no indicators; Treasury imputes SCF
wealth onto tax records in coarse cells with logical links (stocks only
with dividends). Validation is by totals (estate tax receipts, program
counts). Eurostat, of its own matched income, consumption and wealth
file: "we cannot assess the accuracy of this joint distribution." No
source found validates the joint distribution of a translated variable
and the transferred one, which is the rebuild's real question.

### 7.3 (c) Where each of the rebuild's ideas sits

| Idea | Closest existing method | Its assumption, in the literature's terms | What that literature learned about failure |
|---|---|---|---|
| **A, the cell method** | Larrimore–Mortenson–Splinter, TRIM3, CBO transfers; formally the additive nonignorable model with outside margins, positive-unlabelled learning with labelling depending on known attributes, and the Saerens prior-shift adjustment | Reporting depends only on cell and true size range | Counts come out right by construction; who is filled in is right only partly (TRIM3: 63.6% of true SNAP recipients recipients in TRIM3, 54.1% in the raw CPS; search snippet). Cells on few variables weaken relationships with other variables ("match bias"), which the model-based choice addresses. Item-by-item fill-ins leave joint patterns where the sequence of conditionals puts them (chained-equations theory); the 3-point shortfall is that. |
| **B, a behavioral model taught by a fake survey** | Misreporting models estimated on linked data and carried to unlinked data (Mittag; Bollinger and David; Schifeling's 1993 priors; NEWS) | The truth-given-report relationship is the same where it was learned and where it is applied | Without linked data the parameters are ours, so the network learns what was planted: for any part the files cannot identify, the posterior is the prior. Where carried from real linked data, parameters differ across years and regions (Mittag rejects equality), receipt carries better than amounts, and a change in question wording changes reporting (the 2014 CPS redesign roughly doubled reported interest), so CPS-based settings do not carry to the SCF without judgment. |
| **The four-step rule** | Frequency adaptation and singularity stochastic removal followed by quantile mapping (climate); Blanchet–Flores–Morgan "replacing"; equipercentile equating | The report is a monotone function of the truth within sign and cell (perfect rank correlation); zeros above the PUF's share are hidden holders | Which zeros move is set by the tie-breaking rule, which the data do not inform (5.1). Item-by-item maps cannot repair dependence between items. A full-distribution map removes real population differences along with reporting differences (Maraun). Linked data reject rank preservation: substantial re-ranking in Uruguay; only 39% of the self-employed stay in the same fifth in CPS and tax data. Rebalancing "in any direction", turning reporters into zeros, contradicts the evidence that false reports are rare for these items (0.33% for unemployment insurance, 1.8% for pensions). |
| **The PUF-label prediction (after CBO)** | CBO, TPC, PWBM and taxdata's predicted-income sort; covariate shift | The PUF's relationship between assumed-correct variables and holding carries to SCF households | It can use only variables measured alike (W): anything else on the SCF record is not yet on the PUF's terms, which is why the cell method set it aside (`design/cell_method.md`, section 4). So it refines counts for the SCF's mix of W within a cell (a household-level version of the cell table), and cannot say which of two SCF households with the same W is the hidden holder. CBO itself does not trust its match to preserve relationships it did not condition on. In a shared network the PUF-label answer is not isolated from SCF fill-ins (the review of 18 October). |

**On the four-step rule for business income specifically.** Rank-mapping a
noisy report onto the true distribution gets the margin right and the
relationship with net worth wrong. If reported business income is the
truth plus noise, the households with the highest reports include some
whose true income is ordinary and whose report was high by chance. After
the rank map they carry the PUF's top amounts but ordinary wealth, so the
model learns a flatter relationship of net worth to business income than
the true one, and PUF returns with large business income get too little
wealth. This is the attenuation of errors-in-variables regression, and
it is Lord's equity result for test equating (`12_CROSSWALK_THEORY.md`)
and Maraun's (2013) for climate: no map of the margin repairs it. Only a
model of the noise (family B) can, and the linked evidence (39% stay in
the same fifth) says the noise is large.

### 7.4 (d) Combining a population constraint with a behavioral model

**Yes, and it has a standard form:** a reporting model with stated shape
and parameters, fit so that the implied true distribution matches the
PUF within cells, with outside evidence as priors on the parameters the
PUF cannot pin. Schifeling, Reiter and DeYoreo (2019) is a worked
instance; the latent-amount design of plan decision 18 was this; the cell
method is its special case with reporting depending only on cell and size
range and no other parameters.

**What the margins pin and what they do not** (sections 1.4, 3.2, 6.2):

- Pinned by the PUF's distribution within cells: how reporting depends on
  the true amount, in each cell (one function per cell).
- Pinned by the PUF's joint holding table within cells, under restrictions:
  some features of how reporting of one item relates to another. The
  binary illustration of section 1.4 shows the simplest restriction is
  already rejected, so this needs more structure than a single
  correlation.
- Not pinned by anything in the two files: how reporting depends on net
  worth given the amount and the cell; the dollar relationship when it is
  noisy rather than exact. For these the posterior equals the prior
  (Gustafson; Florens and Simoni; Linero and Daniels), so whatever prior
  is chosen is the answer, and should be reported as a dial.

**Trading one assumption for another.** The cells can identify something
the cell method assumes away if a different variable is excluded instead
(Ridder and Moffitt, section 4.2.2). Example with two cells and two
wealth groups, made-up numbers:

| Cell | Wealth group | Share of cell | SCF share reporting interest |
|---|---|---:|---:|
| A (low wages), PUF holding 30% | richer | 30% | 20% |
| | poorer | 70% | 5% |
| B (high wages), PUF holding 60% | richer | 60% | 40% |
| | poorer | 40% | 10% |

*Cell method assumption* (reporting rate depends on the cell, not on
wealth): cell A reports at (0.3 × 20 + 0.7 × 5) / 30 = 9.5 / 30 = 32%,
cell B at 28 / 60 = 47%. Then in cell A, 63% of the richer and 16% of the
poorer truly hold; holding is strongly tied to wealth.

*Alternative assumption* (reporting rate depends on wealth, not on the
cell): with r₁, r₂ the reporting rates of the richer and the poorer, cell
A gives 0.3 × 0.20 / r₁ + 0.7 × 0.05 / r₂ = 0.30 and cell B gives
0.6 × 0.40 / r₁ + 0.4 × 0.10 / r₂ = 0.60. Solving, r₁ = 67% and r₂ = 17%.
Then 30% of the richer and 30% of the poorer hold in cell A: holding is
unrelated to wealth, and the poorer zero-reporters are the ones hiding.

Both fit the data exactly. They imply opposite answers to "who are the
hidden holders, and how rich are they": the first puts them among the
richer, the second among the poorer. With more cells than wealth groups
the second is over-identified and can be tested, and so can a version
that allows both a cell effect and a wealth effect but no interaction.
On the real SCF this is a small least-squares calculation on existing
tables, not a model run. It turns the one unverifiable assumption into a
pair of assumptions whose answers can be compared, which is the most
useful thing the literature offers for it.

**Outside priors available** (`linked_survey_tax_evidence.md`):

- false reports of capital income items are rare (supports assumption 2);
- non-reporting falls with the amount but not to zero (1099-R: 72% in the
  bottom fifth of amounts to 31% in the top fifth);
- non-reporting falls sharply when records are used (Canada: about 37% to
  9%–13%);
- on whether hiding rises or falls with wealth the evidence points both
  ways: in Italy concealment of financial assets is highest in the top
  quarter of real wealth and rises with age; in the SCF higher-income
  households consult tax forms more often and their totals line up better
  with SOI. Neither is a measurement of the SCF's own pattern.

### 7.5 (e) An honest validation without a linked file

What can be validated is (i) whether the method recovers the truth when
the truth is of the assumed kind, (ii) whether the real data contradict
the assumptions where they can, (iii) whether independent evidence inside
the SCF agrees, and (iv) whether the end product agrees with outside
estimates that rest on different assumptions. No combination of these
proves the unverifiable assumption; agreement under different
assumptions is what makes an answer credible.

1. **Pseudo-reality (fake survey) tests, extended to violations.** The
   fake survey already shows the method works when hiding follows its
   assumption. Add versions where it does not: hidden holders poorer,
   and richer, than reporters with the same amount; omission correlated
   across items; a population difference with no reporting error at all
   (the review's suggestion: does the method then "correct" a real
   difference?); business dollars doubled with noise. This maps how much
   each violation costs. It cannot say which violation holds.
2. **The checks the real data allow** (7.1): SCF excess over the PUF by
   amount; the count of households whose chance of reporting exceeds
   their cell's rate; the joint holding table; equality of reporting
   rates across cells; each wave's filled-in holder shares against SOI
   counts for its tax year.
3. **A second measurement inside the SCF: asset balances.** The SCF asks
   for balances of checking, savings, money market accounts, CDs, bonds,
   directly held stocks and mutual funds outside retirement accounts, and
   about business ownership. Interest and dividends are roughly balances
   times yields. Johnson and Moore used exactly this to show that most
   SCF households with interest-bearing assets report no interest. Two
   uses: check that the cell method's hidden holders are the households
   with taxable balances (on the saved fill-ins, no training needed), and
   use balances as a predictor of hidden holding. In the measurement-error
   literature a second, independent indicator of the truth is what
   identifies an error model without validation data (Hu and Schennach
   2008); a balance is not independent of net worth, so it does not
   identify the net worth dependence, but it narrows it a great deal: if
   hidden holders are, by their balances, holders of small taxable
   accounts, then "hidden holders are like reporters with the same
   amount" can be checked on the balance itself.
4. **The households that consulted their tax returns (X7451).** If their
   holding and loss shares are close to the PUF's in the same cell while
   other households' are not, the gap is reporting, not population. If
   consulting is unrelated to true holding given the cell and wealth (an
   assumption, and a different one), comparing consulters with others of
   similar cell and wealth estimates how reporting varies with wealth.
   Step 35's figures (losses 22.6% among consulters, 5.9% among others,
   29% on the PUF) already point to reporting for business losses.
5. **Bounds next to every point estimate.** Worst-case bounds per cell
   (section 3.3; Cross and Manski) for the net worth of true non-holders,
   computed from the SCF alone. Lustig's advice from linked data: "report
   ranges rather than single corrected inequality measures".
6. **Downstream checks on the finished file against outside estimates
   with different assumptions:** wealth by income group from capitalized
   tax data (Saez and Zucman; Smith, Zidar and Zwick), and the estate tax
   implied by the file's wealth and mortality against receipts (Treasury's
   and TPC's check). These test the end product, not the translation, and
   rest on their own yield and mortality assumptions.
7. **The only true linked source:** the Federal Reserve matches its list
   sample to SOI records for sample design and uses the frame data in
   imputation (Kennickell). It will not publish person-level comparisons,
   but aggregate tabulations (share reporting zero interest by SOI
   interest range, within list-sample strata) might be requested. That is
   an outside request and John's decision.
8. **Loop diagnostics** (section 6.4) for the training-time fill-in.

### 7.6 (f) Recommendations, ranked

1. **Keep the cell method for interest, dividends and capital gains, and
   write its assumptions as the four of 7.1 on its settings sheet, with
   the checks each run reports.** Main assumption: reporting depends only
   on cell and size range. Main risk: hidden holders' net worth (the
   unverifiable part) and joint omission (about 3 points on "none of the
   three"). The literature offers no method that removes the assumption;
   it offers ways to state and bracket it.
2. **Before building anything new, run the cheap calculations, none of
   which trains a model:** worst-case bounds per cell; the balance check
   on the saved fill-ins; the consulters comparison; the count of
   households whose reporting chance exceeds their cell's rate; and the
   cells-by-wealth-groups system of 7.4. Main assumption: each has its own
   (stated with it). Main risk: small; some may be inconclusive. This
   follows John's rule of sizing a problem before adding machinery.
3. **For business income, do not use rank-within-sign as the main step.**
   Match on one broad business income line (Johnson and Moore; Bricker and
   coauthors), use the sign split for the extensive margin with the PUF's
   loss share by cell and the model choosing who, and treat amounts with
   an explicit noise model whose size is guided by the consulters and by
   the linked self-employment evidence, with the noise setting reported as
   a dial. Main assumption: consulters' reports are close to the tax
   concept. Main risk: consulters are a selected group, and the noise
   setting carries the answer.
4. **Use the PUF-label prediction for counts only, not for who.** Fit
   holding given the assumed-correct variables on the PUF and sum it over
   SCF households to set each cell's count for the SCF's own mix, as a
   comparison run against the cell table. Main assumption: the PUF's
   relationship carries to SCF households with the same W. Main risk: W
   is not measured alike at the top (SCF wages above the PUF's), and SCF
   households outside the PUF's range of W.
5. **Run the closed-form fill-in of section 6.3 next to the training-time
   loop** (the untested option of `31_DISCUSSION_2026-10-18.md`, section
   5). Main assumption: the same as the cell method. Main risk: it fills
   the three items more independently. Its value is as a benchmark: where
   the loop departs from it, the network is supplying structure the
   assumption does not.
6. **If the combinations shortfall matters for wealth, target the PUF's
   joint holding table by cell** (the multivariate version of frequency
   adaptation, with the model choosing who), after first measuring how
   much the 3 points move mean wealth of "none of the three". Main
   assumption: the joint holding table is the same in both populations
   within cells. Main risk: more parameters than the margins support
   (Schifeling's counting bound) and a design change John has already
   judged too heavy unless the size warrants it.
7. **Use behavioral models (family B) as priors and tests, not as a source
   of identification.** Plant settings from linked evidence where it
   exists, and state that for any part the files cannot identify, the
   answer is what was planted. Main risk: CPS-based settings carried to
   the SCF's different questionnaire.
8. **Not recommended:** iterating the network's guesses as a route to the
   truth (it converges to the assumption, section 6.4); optimal-transport
   or multivariate climate machinery at this stage (the problem is in who
   is chosen, which those methods also leave to a rule).

## Leads not followed

The search allowance ran out before these were opened: Chen, Hong and
Nekipelov (2011, JEL) on auxiliary data; Hu and Ridder (2012) in the
original; the full texts of Themeßl and coauthors (2012), Vrac, Noël and
Vautard (2016), Cannon (2018) and Maraun (2016); the CPS 2014 split-panel
results for interest and dividends; Brady and Bass (2021, IRS SOI,
"Comparing the CPS to income tax data", PDF not readable); Perese (2017)
in full (cbo.gov refused access); the Larrimore, Mortenson and Splinter
appendix on how amounts were drawn; Smith, Zidar and Zwick's validation of
yields; the SCF codebook's "corrected amount" variables (for example
X5704A), whose meaning was not checked; Schifeling and Reiter (2016) on
adding marginal information to latent class models; an August 2026 arXiv
paper (2608.19585) using tax records to estimate negative incomes in
survey data, abstract only.

## References

Reading depth, as reported by the helper search that opened each source:
**read** (substantial full text), **key sections** (abstract and key
sections, or the method section only), **citations only** (known from
other papers, search summaries or software that implements it). Where a
helper read a working-paper version, that is the version listed. A few
items reached only through a search-result excerpt are marked so.

### Statistical matching, data fusion, recoding

- Bago d'Uva, T., M. Lindeboom, O. O'Donnell and E. van Doorslaer (2011). "Slipping anchor? Testing the vignettes approach to identification and correction of reporting heterogeneity." Journal of Human Resources 46(4): 875–906. https://jhr.uwpress.org/content/46/4/875 — key sections (abstract).
- Capotorti, A., and G. Buonumori (2018). "Behavior of L1-based probabilistic correction applied to statistical matching with misclassification information." Conference proceedings. https://research.unipg.it/handle/11391/1433767 — key sections (abstract).
- Clogg, C. C., D. B. Rubin, N. Schenker, B. Schultz and L. Weidman (1991). "Multiple imputation of industry and occupation codes in census public-use samples using Bayesian logistic regression." JASA 86: 68–78. https://www.jstor.org/stable/2289716 — citations only.
- Cross, P. J., and C. F. Manski (2002). "Regressions, short and long." Econometrica 70(1): 357–368. https://ideas.repec.org/a/ecm/emetrp/v70y2002i1p357-368.html — key sections (abstract, and Molinari's 2020 handbook summary).
- D'Orazio, M. StatMatch R package documentation (`harmonize.x`, `comp.prop`). https://rdrr.io/cran/StatMatch/man/comp.prop.html — key sections.
- Di Zio, M., and B. Vantaggi (2017). "Partial identification in statistical matching with misclassification." International Journal of Approximate Reasoning 82: 227–241. https://iris.uniroma1.it/handle/11573/924536 — key sections (abstract).
- Eurostat. Income, consumption and wealth statistics, metadata. https://ec.europa.eu/eurostat/cache/metadata/en/icw_esms.htm — read.
- Fan, Y., R. Sherman and M. Shum (2014). "Identifying treatment effects under data combination." Econometrica 82(2): 811–822. https://www.econometricsociety.org/publications/econometrica/2014/03/01/identifying-treatment-effects-under-data-combination — key sections (abstract).
- Fosdick, B. K., M. DeYoreo and J. P. Reiter (2016). "Categorical data fusion using auxiliary information." Annals of Applied Statistics 10(4): 1907–1929. https://arxiv.org/abs/1506.05886 — read.
- Garès, V., C. Dimeglio, G. Guernec, R. Fantin, B. Lepage, M. R. Kosorok and N. Savy (2020). "On the use of optimal transportation theory to recode variables and application to database merging." International Journal of Biostatistics 16(1). https://ideas.repec.org/a/bpj/ijbist/v16y2020i1p16n8.html — key sections (abstract).
- Garès, V., and J. Omer (2022). "Regularized optimal transport of covariates and outcomes in data recoding." JASA 117(537): 320–333. https://ideas.repec.org/a/taf/jnlasa/v117y2022i537p320-333.html — key sections (abstract).
- Guernec, G., and coauthors (2023). "OTrecod: An R package for data fusion using optimal transportation theory." R Journal. https://rjournal.github.io/articles/RJ-2023-006/ — read (most).
- Kim, J. K., E. Berg and T. Park (2016). "Statistical matching using fractional imputation." Survey Methodology 42(1). https://arxiv.org/abs/1510.03782 — read.
- King, G., C. J. L. Murray, J. A. Salomon and A. Tandon (2004). "Enhancing the validity and cross-cultural comparability of measurement in survey research." American Political Science Review 98(1): 191–207. https://gking.harvard.edu/publication/enhancing-the-validity-and-cross-cultural-comparability-of-measurement-in-survey-research/ — key sections (abstract).
- Lamarche, P., F. Oehler and I. Rioboo (2020). Eurostat experimental matching of household finance survey and EU-SILC. Statistical Journal of the IAOS. https://journals.sagepub.com/doi/abs/10.3233/SJI-190528 — search excerpt only.
- Leulescu, A., and M. Agafitei (2013). "Statistical matching: a model based approach for data integration." Eurostat Statistical Working Papers KS-RA-13-020. https://ec.europa.eu/eurostat/web/products-statistical-working-papers/-/ks-ra-13-020 — citations only (catalogue description).
- NCHS. Comparability ratio (ICD-9 to ICD-10). https://www.restoredcdc.org/www.cdc.gov/nchs/hus/sources-definitions/comparability-ratio.htm — read.
- Parker, J. D., N. Schenker, D. D. Ingram and coauthors (2004). "Bridging between two standards for collecting information on race and ethnicity." Public Health Reports 119(2): 192–205. https://pmc.ncbi.nlm.nih.gov/articles/PMC1497618 — key sections (abstract). Schenker, N., and J. D. Parker (2003), Statistics in Medicine 22(9): 1571–1587 — citations only.
- Schenker, N., T. E. Raghunathan and I. Bondarenko (2010). "Improving on analyses of self-reported data in a large-scale health survey by using information from an examination-based survey." Statistics in Medicine 29(5): 533–545. https://www150.statcan.gc.ca/n1/en/catalogue/11-522-X200600110408 (2006 proceedings) — key sections (abstract).
- Schifeling, T., J. P. Reiter and M. DeYoreo (2019). "Data fusion for correcting measurement errors." Journal of Survey Statistics and Methodology 7(2): 175–200. https://arxiv.org/abs/1610.00147 — read.
- Singh, R. K. (2022). "Harmonizing single-question instruments for latent constructs with equating." Survey Research Methods 16(3): 353–369. https://ojs.ub.uni-konstanz.de/srm/article/view/7916 — key sections (abstract).
- van Soest, A., L. Delaney, C. Harmon, A. Kapteyn and J. P. Smith (2011). "Validating the use of anchoring vignettes for the correction of response scale differences in subjective questions." JRSS-A 174(3): 575–595. https://www.iza.org/publications/dp/2860 — key sections (abstract).

### Tax and transfer model practice

- Batty, M., and coauthors (2020 revision of FEDS 2019-017). "Introducing the Distributional Financial Accounts of the United States." https://www.aeaweb.org/conference/2021/preliminary/paper/ANrhSBKE — read.
- Burkhauser, R. V., N. Hérault, S. P. Jenkins and R. Wilkins (2018). "Survey under-coverage of top incomes and estimation of inequality: what is the role of the UK's SPI adjustment?" Fiscal Studies 39(2); NBER WP 23539. https://www.nber.org/papers/w23539.pdf — read.
- CBO, Habib, B. (2020). Slides on transfer under-reporting for the BEA advisory committee. https://www.bea.gov/sites/default/files/2020-05/CBO-underreporting-slides-for-BEA-panel_0.pdf — read.
- CBO (2024). AEA slides on distributional methods. https://www.cbo.gov/system/files/2024-01/59688-AEA.pdf — search excerpt only.
- DWP. Households Below Average Income, quality and methodology report FYE 2024. https://www.gov.uk/government/statistics/households-below-average-income-for-financial-years-ending-1995-to-2024/households-below-average-income-series-quality-and-methodology-information-report-fye-2024 — key sections.
- EUROMOD modelling conventions. https://euromod-web.jrc.ec.europa.eu/resources/modelling-conventions — key sections. Leventi, C., M. Matsaganis and M. Flevotomou, EUROMOD WP EM17/13. https://www.iser.essex.ac.uk/research/publications/working-papers/euromod/em17-13.pdf — key sections (abstract).
- Feenberg, D., and E. Coutts (1993). "An introduction to the TAXSIM model." JPAM 12(1): 189–194. https://taxsim.nber.org/feenberg-coutts.pdf — read.
- Gillette, R., and coauthors (2023). "U.S. Treasury Individual Income Tax Model." OTA Technical Paper 12. https://home.treasury.gov/system/files/131/TP-12.pdf — read.
- Joint Committee on Taxation (2015). "Estimating Changes in the Federal Individual Income Tax: Description of the Individual Tax Model." JCX-75-15. https://jct.gov/getattachment/0308b4da-ff48-4e9e-92f6-3592eb6a45ac/x-75-15-4776.pdf — read.
- Perese, K. (2017). "CBO's New Framework for Analyzing the Effects of Means-Tested Transfers and Federal Taxes on the Distribution of Household Income." CBO Working Paper 2017-09. https://www.cbo.gov/publication/53345 — search excerpts quoting the text only.
- PolicyEngine US data (Enhanced CPS) repository. https://github.com/PolicyEngine/policyengine-us-data — read (code and methodology).
- PSL taxdata repository (`createpuf.py`, `statmatch.py`, `targeting.py`). https://github.com/PSLmodels/taxdata — read (code).
- Ricco, J. (2020). "Data Processing for PWBM's Tax Module." https://budgetmodel.wharton.upenn.edu/model-docs/DataProcessingForPWBMTaxModule.pdf — read.
- Rohaly, J., A. Carasso and M. A. Saleem (2005). "The Urban-Brookings Tax Policy Center Microsimulation Model: Documentation and Methodology for Version 0304." https://www.urban.org/sites/default/files/publication/42911/411136-the-urban-brookings-tax-policy-center-microsimulation-model.pdf — read. TPC (2021) technical update: https://www.taxpolicycenter.org/sites/default/files/publication/163341/tpc-model-technical-update.pdf — read.
- Shantz, K., and L. Fox (2018). Census working paper SEHSD-WP2018-30, linking the CPS to state SNAP and TANF records and comparing TRIM3 (title not verified). https://www.census.gov/library/working-papers/2018/demo/SEHSD-WP2018-30.html — key sections (abstract; the 63.6% and 54.1% figures from a search excerpt of a table, not verified).
- Statistics Canada, SPSD/M overview and publication 13-607-x. https://www.statcan.gc.ca/en/microsimulation/spsdm/overview ; https://www150.statcan.gc.ca/n1/pub/13-607-x/2016001/939-eng.htm — read.
- Stevenson, S., and coauthors (2017). CAPITA, Treasury Working Paper 2017-05 (Australia). https://treasury.gov.au/sites/default/files/2019-03/170915-PT-Treasury-Research-Institute-Paper-CAPITA-MSN-edits.pdf — read.
- Urban Institute TRIM3 documentation: SNAP, SSI, TANF, federal income tax, and "Steps in Simulating". https://boreas.urban.org/documentation/foodstamps/main.php ; https://boreas.urban.org/documentation/Steps%20in%20Simulating%20Tax%20and%20Transfer%20Programs.php ; https://boreas.urban.org/documentation/federaltax/main.php — read.
- Wheaton, L. (2007). "Underreporting of means-tested transfer programs in the CPS and SIPP." Urban Institute. https://www.urban.org/research/publication/underreporting-means-tested-transfer-programs-cps-and-sipp — citations only.

### Measurement error, missing data, identification

- Akande, O., G. Madson, D. S. Hillygus and J. P. Reiter (2019). arXiv 1907.06145, on auxiliary marginal information in nonignorable missing-data models (title not recorded). https://arxiv.org/abs/1907.06145 — key sections.
- Bound, J., C. Brown and N. Mathiowetz (2001). "Measurement error in survey data." Handbook of Econometrics vol. 5, ch. 59 — citations only.
- Chen, X., H. Hong and E. Tamer (2005). "Measurement error models with auxiliary data." Review of Economic Studies 72(2): 343–366. https://ideas.repec.org/a/oup/restud/v72y2005i2p343-366.html — key sections (abstract).
- Chen, X., H. Hong and A. Tarozzi (2008). "Semiparametric efficiency in GMM models with auxiliary data." Annals of Statistics 36(2): 808–843. https://arxiv.org/abs/0705.0069 — read.
- d'Haultfoeuille, X. (2010). "A new instrumental method for dealing with endogenous selection." Journal of Econometrics 154(1): 1–15. http://crest.science/RePEc/wpstorage/2008-23.pdf — read (working paper).
- Hausman, J. A., J. Abrevaya and F. M. Scott-Morton (1998). "Misclassification of the dependent variable in a discrete-response setting." Journal of Econometrics 87(2): 239–269 — citations only (via Meyer and Mittag 2014, NBER w20509).
- Hirano, K., G. W. Imbens, G. Ridder and D. B. Rubin (2001). "Combining panel data sets with attrition and refreshment samples." Econometrica 69(6): 1645–1659. https://nber.org/papers/t0230 — key sections (abstract and secondary descriptions).
- Horowitz, J. L., and C. F. Manski (1995). "Identification and robustness with contaminated and corrupted data." Econometrica 63(2): 281–302. https://ideas.repec.org/a/ecm/emetrp/v63y1995i2p281-302.html — key sections (abstract; bound from secondary sources).
- Hu, Y. (2008). "Identification and estimation of nonlinear models with misclassification error using instrumental variables." Journal of Econometrics 144(1): 27–61. http://www.econ2.jhu.edu/People/Hu/Website/Hu2008.pdf — read.
- Hu, Y., and G. Ridder (2012). Estimation of nonlinear models with mismeasured regressors using marginal information. Journal of Applied Econometrics (title and venue as recalled; not verified because the search allowance had run out) — citations only (via Ridder and Moffitt and Schennach's review).
- Hu, Y., and S. M. Schennach (2008). "Instrumental variable treatment of nonclassical measurement error models." Econometrica 76(1): 195–216. http://www.econ2.jhu.edu/People/Hu/Hu-Schennach.pdf — read.
- Kreider, B., J. V. Pepper, C. Gundersen and D. Jolliffe (2012). "Identifying the effects of SNAP on child health outcomes when participation is endogenous and misreported." JASA 107(499): 958–975. https://ideas.repec.org/a/taf/jnlasa/v107y2012i499p958-975.html — key sections (abstract).
- Kreider, B., J. V. Pepper and M. Roy (2012). "Identifying the effect of WIC on very low food security among infants and children." UKCPR DP 2012-11. https://ukcpr.uky.edu/sites/default/files/2025-09/dp2012-11_kreider-et-al.pdf — read.
- Gundersen, C., and B. Kreider (2007). IRP Discussion Paper 1321-07 (JHR 2008). https://www.irp.wisc.edu/publications/dps/pdfs/dp132107.pdf — read.
- Linero, A. R., and M. J. Daniels (2018). "Bayesian approaches for missing not at random outcome data: the role of identifying restrictions." Statistical Science 33(2). https://pmc.ncbi.nlm.nih.gov/articles/PMC6936760 — read.
- Molenberghs, G., C. Beunckens, C. Sotto and M. G. Kenward (2008). "Every missingness not at random model has a missingness at random counterpart with equal fit." JRSS-B 70(2): 371–388. https://academic.oup.com/jrsssb/article/70/2/371/7109518 — key sections.
- Molinari, F. (2008). "Partial identification of probability distributions with misclassified data." Journal of Econometrics 144(1): 81–117. https://ideas.repec.org/a/eee/econom/v144y2008i1p81-117.html — key sections (abstract).
- Ridder, G., and R. Moffitt (2007). "The econometrics of data combination." Handbook of Econometrics vol. 6B, ch. 75. Draft: http://www.econ2.jhu.edu/people/moffitt/comsampdefmin.pdf — read (2005 draft).
- Sadinle, M., and J. P. Reiter (2019). "Sequentially additive nonignorable missing data modelling using auxiliary marginal information." Biometrika 106(4). https://arxiv.org/abs/1902.06043 — key sections.
- Schennach, S. M. Measurement error review, cemmap CWP41/12. https://www.cemmap.ac.uk/wp-content/uploads/2020/08/CWP4112.pdf — read. (Her 2016 Annual Review of Economics article: citations only.)

### Linked survey–administrative studies and the SCF

- Bee, C. A., J. Mitchell, N. Mittag, J. Rothbaum, C. Sanders, L. Schmidt and M. Unrath (2023). "National Experimental Wellbeing Statistics, Version 1." Census SEHSD WP 2023-02. https://www.census.gov/library/working-papers/2023/demo/SEHSD-WP2023-02.html — read.
- Bollinger, C. R., and M. H. David (1997). "Modeling discrete choice with response error: food stamp participation." JASA 92(439): 827–835 — citations only.
- Bricker, J., A. Henriques, J. Krimmel and J. Sabelhaus (2016). "Measuring income and wealth at the top using administrative and survey data." Brookings Papers on Economic Activity, Spring: 261–331. https://www.brookings.edu/wp-content/uploads/2016/03/brickertextspring16bpea.pdf and appendix https://www.brookings.edu/wp-content/uploads/2016/03/brickerappendixspring16bpea.pdf — read (also FEDS 2015-030).
- Flachaire, E., N. Lustig and A. Vigorito. Correction methods for top incomes, tested on linked Uruguayan data. https://iariw.org/wp-content/uploads/2021/08/FLV_correctionmethods_paper.pdf — read via text extraction; journal version not confirmed.
- Fox, L., J. Rothbaum and K. Shantz (2022). "Fixing errors in a SNAP." AEA Papers and Proceedings 112: 330–334. https://www.census.gov/content/dam/Census/newsroom/press-kits/2022/assa/assa-jan2022-paper-fixing%20errors-in-a-snap.pdf — read.
- Hokayem, C., C. Bollinger and J. P. Ziliak (2015). "The role of CPS nonresponse in the measurement of poverty." JASA 110(511): 935–945. https://gattonweb.uky.edu/Faculty/Ziliak/HBZ_JASA_110(511)_2015.pdf — read.
- Hokayem, C., T. Raghunathan and J. Rothbaum (2022). "Match bias or nonignorable nonresponse? Improved imputation and administrative data in the CPS ASEC." JSSAM 10(1): 81–114. Slides: https://www.niss.org/sites/default/files/Raghunathan%2C%20Trivellore.pdf — key sections (slides and abstract).
- Johnson, B. W., and K. Moore (2008). "Differences in income estimates derived from survey and tax data." IRS SOI Compendium, ch. 9. https://www.irs.gov/pub/irs-soi/11pwcompench9cdiff.pdf — read.
- Kennickell, A. B. (1998). "Multiple imputation in the Survey of Consumer Finances." JSM. https://www.federalreserve.gov/econresdata/scf/files/impute98.pdf — read.
- Meyer, B. D., and N. Mittag (2019). "Using linked survey and administrative data to better measure income." AEJ: Applied 11(2): 176–204. https://www.nber.org/papers/w21676 — key sections (abstract).
- Meyer, B. D., D. Wu, G. Stadnicki and P. Langetieg (2023). Unemployment insurance in survey and administrative data, slides. https://bpb-us-w2.wpmucdn.com/voices.uchicago.edu/dist/a/3122/files/2023/08/UI-Slides_public-080823.pdf — read via text extraction.
- Mittag, N. (2019). "Correcting for misreporting of government benefits." AEJ: Economic Policy 11(2): 142–164. IZA DP 10266: https://docs.iza.org/dp10266.pdf — read (working paper).
- Federal Reserve. SCF 2019 codebook. https://www.federalreserve.gov/econres/files/codebk2019.txt — variable list searched.
- Tax Policy Center (2022). Report on taxing business income (SCF closely held business income about twice SOI). https://www.brookings.edu/wp-content/uploads/2022/01/20220216_TPC_TaxingBusinessIncome_Report_Final.pdf — search excerpt only.
- Further linked studies (Bee and Mitchell 2017; Meyer, Mittag, Wu, Tatarka and Langetieg 2026; Imboden, Voorheis and Weber 2023; Grondin and Michaud 1994; D'Aurizio and coauthors 2006): see `linked_survey_tax_evidence.md`, which gives their reading depth.

### Survey–tax distribution corrections

- Auten, G., and D. Splinter (2024). "Income inequality in the United States: using tax data to measure long-term trends." JPE 132(7). https://gwern.net/doc/economics/2024-auten.pdf — read via text extraction (near-final version).
- Blanchet, T., I. Flores and M. Morgan (2022). "The weight of the rich: improving surveys using tax data." Journal of Economic Inequality 20(1): 119–150. https://ignacioflores.com/pdf/the-weight-of-the-rich.pdf — read via text extraction.
- Bourguignon, F. (2018). "Simple adjustments of observed distributions for missing income and missing people." Journal of Economic Inequality 16(2): 171–188. https://ideas.repec.org/p/hal/journl/halshs-01883896.html — key sections (abstract).
- Larrimore, J., J. Mortenson and D. Splinter (2022). "Unemployment insurance in survey and administrative data." FEDS Note. https://www.federalreserve.gov/econres/notes/feds-notes/unemployment-insurance-in-survey-and-administrative-data-20220705.html — read. JPAM 42(2): 571–579 (2023) — key sections (abstract).
- Lustig, N. (2019). "The missing rich in household surveys: causes and correction approaches." CEQ Working Paper 75. https://repec.tulane.edu/RePEc/ceq/ceq75.pdf — read via text extraction.
- Piketty, T., E. Saez and G. Zucman (2018). "Distributional national accounts: methods and estimates for the United States." QJE 133(2). https://eml.berkeley.edu/~saez/PSZ2018QJE.pdf — read via text extraction (paper and appendix). Their 2024 comment on Auten and Splinter: https://gabriel-zucman.eu/files/PSZ2024.pdf — read via text extraction.

### Optimal transport, quantile mapping, domain adaptation

- Alexandari, A., A. Kundaje and A. Shrikumar (2020). "Maximum likelihood with bias-corrected calibration is hard-to-beat at label shift adaptation." ICML. https://arxiv.org/abs/1901.06852 — key sections.
- Athey, S., and G. Imbens (2006). "Identification and inference in nonlinear difference-in-differences models." Econometrica 74(2) — via `minimal/12_CROSSWALK_THEORY.md`.
- Cannon, A. J. (2018). "Multivariate quantile mapping bias correction: an N-dimensional probability density function transform." Climate Dynamics 50: 31–49; MBC package code https://github.com/cran/MBC — key sections (abstract and code).
- Carlier, G., V. Chernozhukov and A. Galichon (2016). "Vector quantile regression: an optimal transport approach." Annals of Statistics 44(3): 1165–1192. https://arxiv.org/abs/1406.4643 — key sections (abstract).
- Carlier, G., A. Galichon and F. Santambrogio. "From Knothe's transport to Brenier's map and a continuation method for optimal transport." https://arxiv.org/abs/0810.4153 — key sections (abstract).
- Chernozhukov, V., A. Galichon, M. Hallin and M. Henry (2017). "Monge–Kantorovich depth, quantiles, ranks and signs." Annals of Statistics 45(1): 223–256. https://arxiv.org/abs/1412.8434 — key sections (abstract).
- Clark, M., and coauthors (2004). "The Schaake shuffle." Journal of Hydrometeorology 5: 243–262. https://www.usgs.gov/publications/schaake-shuffle-a-method-reconstructing-space-time-variability-forecasted — key sections (abstract).
- Courty, N., R. Flamary, D. Tuia and A. Rakotomamonjy. "Optimal transport for domain adaptation." https://arxiv.org/abs/1507.00504 — key sections. Courty, N., R. Flamary, A. Habrard and A. Rakotomamonjy (2017), "Joint distribution optimal transportation", NeurIPS, https://arxiv.org/abs/1705.08848 — key sections (abstract).
- François, B., M. Vrac, A. J. Cannon, Y. Robin and D. Allard (2020). "Multivariate bias corrections of climate simulations: which benefits for which losses?" Earth System Dynamics 11: 537–562. https://esd.copernicus.org/articles/11/537/2020/ — read (most).
- Lipton, Z., Y.-X. Wang and A. Smola (2018). "Detecting and correcting for label shift with black box predictors." ICML. https://arxiv.org/abs/1802.03916 — key sections (abstract).
- Maraun, D. (2016). "Bias correcting climate change simulations — a critical review." Current Climate Change Reports. https://doi.org/10.1007/s40641-016-0050-x — key sections (abstract). Maraun, D., and coauthors (2017), "Towards process-informed bias correction of climate change simulations", Nature Climate Change 7: 764–773, https://research.birmingham.ac.uk/files/43590189/maraun_etal_2017_bias_accepted.pdf — key sections (abstract).
- Meyer, J., I. Kohn, K. Stahl, K. Hakala, J. Seibert and A. J. Cannon (2019). "Effects of univariate and multivariate bias correction on hydrological impact projections in alpine catchments." Hydrology and Earth System Sciences 23: 1339–1354. https://hess.copernicus.org/articles/23/1339/2019/ — key sections (abstract and summary, opened by me).
- Effectiveness of multivariate bias correction in hydrology and agriculture: a systematic review (2026). EGUsphere preprint. https://egusphere.copernicus.org/preprints/2026/egusphere-2026-529 — search summary only.
- Peyré, G., and M. Cuturi (2019). "Computational optimal transport." Foundations and Trends in Machine Learning 11. https://arxiv.org/abs/1803.00567 — key sections (§2.2–2.6).
- Robin, Y., M. Vrac, P. Naveau and P. Yiou (2019). "Multivariate stochastic bias corrections with optimal transport." HESS 23: 773–786. https://hess.copernicus.org/articles/23/773/2019/ — key sections.
- Saerens, M., P. Latinne and C. Decaestecker (2002). "Adjusting the outputs of a classifier to new a priori probabilities: a simple procedure." Neural Computation 14(1): 21–41 — citations only.
- Schefzik, R., T. Thorarinsdottir and T. Gneiting (2013). "Uncertainty quantification in complex simulation models using ensemble copula coupling." Statistical Science 28: 616–640. https://arxiv.org/abs/1302.7149 — key sections.
- Shimodaira, H. (2000). "Improving predictive inference under covariate shift by weighting the log-likelihood function." Journal of Statistical Planning and Inference — citations only.
- Themeßl, M. J., A. Gobiet and G. Heinrich (2012). "Empirical-statistical downscaling and error correction of regional climate models and its impact on the climate change signal." Climatic Change 112: 449–468 — citations only (method read through xclim's implementation, https://github.com/Ouranosinc/xclim/blob/v0.31.0/xclim/sdba/_processing.py).
- Vrac, M., and P. Friederichs (2015). "Multivariate—intervariable, spatial, and temporal—bias correction." Journal of Climate 28: 218–237. https://www.lsce.ipsl.fr/wp-content/uploads/2023/11/Vrac_and_Friederichs_ECBC_Accepted_JClim_2014.pdf — key sections (abstract).
- Vrac, M., T. Noël and R. Vautard (2016). "Bias correction of precipitation through Singularity Stochastic Removal: because occurrences matter." JGR Atmospheres 121: 5237–5258 — citations only (method read through the SBCK documentation, https://www.icesi.edu.co/CRAN/web/packages/SBCK/refman/SBCK.html).
- Vrac, M. (2018). "Multivariate bias adjustment of high-dimensional climate simulations: the Rank Resampling for Distributions and Dependences (R2D2) bias correction." HESS 22: 3175–3196. https://hess.copernicus.org/articles/22/3175/2018/ — key sections.
- Zhang, K., B. Schölkopf, K. Muandet and Z. Wang (2013). "Domain adaptation under target and conditional shift." ICML. https://proceedings.mlr.press/v28/zhang13d.html — key sections (abstract).

### Iterative imputation and positive-unlabelled learning

- Arazo, E., D. Ortego, P. Albert, N. O'Connor and K. McGuinness (2020). "Pseudo-labeling and confirmation bias in deep semi-supervised learning." https://arxiv.org/abs/1908.02983 — key sections (abstract).
- Beaumont, J.-F. (2005). "Calibrated imputation in surveys under a quasi-model-assisted approach." JRSS-B 67: 445–458. https://ideas.repec.org/a/bla/jorssb/v67y2005i3p445-458.html — key sections (abstract).
- Bekker, J., and J. Davis (2020). "Learning from positive and unlabeled data: a survey." Machine Learning 109: 719–760. https://arxiv.org/abs/1811.04820 — read (most).
- Bekker, J., P. Robberechts and J. Davis (2019). "Beyond the selected completely at random assumption for learning from positive and unlabeled data." ECML. https://arxiv.org/abs/1809.03207 — key sections.
- Celeux, G., and J. Diebolt (1985); Dempster, A. P., N. M. Laird and D. B. Rubin (1977); Tanner, M. A., and W. H. Wong (1987) — citations only.
- Chauvet, G., J.-C. Deville and D. Haziza (2011). "On balanced random imputation in surveys." Biometrika 98(2). https://ideas.repec.org/a/oup/biomet/v98y2011i2p459-471.html — key sections (abstract).
- de Waal, T., and J. Daalmans (2023). "Calibrated imputation for multivariate categorical data." AStA. https://research.tilburguniversity.edu/en/publications/calibrated-imputation-for-multivariate-categorical-data/ — key sections (abstract).
- Elkan, C., and K. Noto (2008). "Learning classifiers from only positive and unlabeled data." KDD — citations only.
- Florens, J.-P., and A. Simoni (2021). "Revisiting identification concepts in Bayesian analysis." https://arxiv.org/abs/2110.09954 — key sections.
- Gerstgrasser, M., and coauthors (2024). "Is model collapse inevitable?" ICML. https://arxiv.org/abs/2404.01413 — key sections (abstract).
- Gustafson, P. (2010). "Bayesian inference for partially identified models." International Journal of Biostatistics 6(2). https://ideas.repec.org/a/bpj/ijbist/v6y2010i2n17.html — key sections (abstract). His 2005 Statistical Science paper and 2015 book — citations only.
- Hughes, R. A., I. R. White, S. R. Seaman, J. R. Carpenter, K. Tilling and J. A. C. Sterne (2014). "Joint modelling rationale for chained equations." BMC Medical Research Methodology 14: 28. https://pmc.ncbi.nlm.nih.gov/articles/PMC3936896/ — read.
- Liu, J., A. Gelman, J. Hill, Y.-S. Su and J. Kropko (2014). "On the stationary distribution of iterative imputations." Biometrika 101(1): 155–173. https://arxiv.org/abs/1012.2902 — read (most).
- Nielsen, S. F. (2000). "The stochastic EM algorithm: estimation and asymptotic results." Bernoulli 6(3): 457–489. https://projecteuclid.org/euclid.bj/1081616701 — key sections (abstract).
- Pannekoek, J., N. Shlomo and T. de Waal (2013). "Calibrated imputation of numerical data under linear edit restrictions." Annals of Applied Statistics 7(4). https://arxiv.org/abs/1401.1663 — key sections.
- Shumailov, I., and coauthors (2024). "AI models collapse when trained on recursively generated data." Nature 631: 755–759. https://arxiv.org/abs/2305.17493 — key sections.
- Wei, C., K. Shen, Y. Chen and T. Ma (2021). "Theoretical analysis of self-training with deep networks on unlabeled data." ICLR. https://arxiv.org/abs/2010.03622 — key sections (abstract).

### Local notes used

`research/rebuild/README.md`; `design/plan.md` (decisions 14–18);
`design/translation_by_latent_amounts.md`; `design/cell_method.md`;
`design/three_part_mapping.md`; `design/reporting_rules.md` (section 0);
`minimal/12_CROSSWALK_THEORY.md`; `minimal/31_RESULTS.md`;
`minimal/31_DISCUSSION_2026-10-18.md`; `minimal/35_BUSINESS_INCOME.md`
(summary); `minimal/HANDOFF_2026-10-18.md`; `background/05`, `06`, `09`
(the sections on matching, optimal transport and unfolding) and
`background/linked_survey_tax_evidence.md`; the review of 18 October at
`/nfs/roberts/scratch/pi_nrs36/jar335/minimal/litreview/codex_astra_review.md`.
