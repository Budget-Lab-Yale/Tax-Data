---
title: "Reporting rules: what links a survey's reported amount to the true amount, what the data can tell us, and how to choose"
role: design
workstream: rebuild
status: current
updated: 2026-10-17
sot: research/rebuild/design/plan.md
supersedes: []
superseded_by: null
---

# Reporting rules

The general framing behind plan decision 18, written at John's request on
17 October 2026. `translation_by_latent_amounts.md` works out one case
(interest, where small holders tell the survey "$0"); this note is about
the problem in general, for every variable a survey shares with the PUF.
Nothing here is a decision yet: the choices in section 7 are proposals.

**Section 0 is the version worked through with John on the afternoon of
17 October, step by step.** It supersedes the framing of sections 1 to 4
where they differ (chiefly: the PUF is not the truth, only the target
concept). Sections 1 to 7 are the earlier write-up and are kept for their
evidence and detail.

## 0. The problem, worked through (17 October, afternoon)

### 0.1 What we are doing

Each household has a real situation that no file sees directly. File A
(the PUF) shows one blurry reading of it, X as A measures it. File B (a
survey: the SCF, the CPS) shows a different blurry reading, X as B
measures it, plus a variable Y that A lacks (net worth). We want Y given
X **as A measures it**, for A's households.

The PUF is not the truth. It has its own errors (IRS audits find over half
of Schedule C income missing from returns). But Tax-Simulator taxes what
is on the return, so the PUF's concept is the target. "PUF = actual" in
decision 18 means "we aim at the PUF's concept, errors included", not "the
PUF is right". We never need the truth itself, only how B's reading lines
up with A's.

### 0.2 Three explanations for a gap between the files

Example: households that are married with two children and wages around
$80,000. On the PUF, 30 in 100 have taxable interest; in the SCF, 13 in
100 report any. Three explanations:

1. **Different people.** B covers a different population (non-filers,
   sample design, how tax units are built).
2. **Same people, same concept, reported differently.** Some SCF holders
   say $0. It can go the other way: for self-employment income, people
   report more to the CPS than to the IRS.
3. **Different concepts that are correlated.** For example, SCF business
   income close to book profit against taxable pass-through income on the
   return.

Explanations 2 and 3 are the same kind of thing: a link between the
household and each file's number (a measurement link). Explanation 1 is a
different kind: it is about who is in the file.

### 0.3 It matters, and one cell of data cannot tell which

Same cell, 100 households per file. The SCF's 87 zero-reporters average
$139,000 of net worth; its 13 reporters average $300,000. What should a
PUF household with $0 interest get?

- **Explanation 1:** the SCF's zeros are true non-holders: $139,000.
- **Explanation 2:** 17 of the 87 are holders who said $0. If they are
  like reporters ($300,000), the true non-holders hold
  (87 × $139,000 − 17 × $300,000) / 70 = $100,000.
- **Explanation 3:** same structure as 2 (the SCF's zero group mixes
  households the PUF would call holders): about $100,000.

The SCF data is identical in every case; only the interpretation differs.

This is the choice between two ways of carrying Y over:

- (a) **copy Y given X as the survey shows it.** Then the PUF's overall
  mean differs from the SCF's: 0.7 × $139k + 0.3 × $300k = $187k against
  $160k. Right under explanation 1.
- (b) **adjust Y given X so the overall means agree:** 0.7 × $100k +
  0.3 × $300k = $160k. Right under explanation 2.

Matching overall means does not say *how* to adjust (lower the zeros,
lower the holders, lower everyone); explanation 2 says which (take the
hidden holders out of the zero group). Reweighting to the SCF's wealth
totals forces the totals but picks its own adjustment. The truth is
probably a mix of (a) and (b).

### 0.4 What can tell the explanations apart

1. **Who goes missing, by size.** Reporting problems lose small holders
   and keep large ones, so holders go missing but dollars do not.
   Different people need not follow that pattern. SCF interest (2016 wave
   against the 2015 PUF with crude non-filers): 12.6% of units hold any
   against 25.9%, but $89.9 billion against $94.2 billion of dollars; the
   SCF finds 22% of holders under $100, 36% at $100 to $500, 66% at $500
   to $2,000, 79% at $2,000 to $10,000, about all of them above $10,000.
   This is the explanation 2 pattern.
2. **Many cells at once.** If the chance of reporting depends only on the
   amount held, one rule has to fit every kind of household. A young,
   low-wage cell whose holders hold about $50 should lose most of them; an
   older, high-wage cell whose holders hold about $5,000 should lose few.
   Cells with different mixes of amounts trace out the rule. A cell that
   misfits says something else is going on, but not whether it is
   different people or reporting that depends on more than the amount.
   This works only if the cells are defined by traits both files measure
   alike (now married, children, wages).
3. **Outside evidence.** Studies that link survey answers to the same
   people's tax records. Summary in
   `research/rebuild/background/linked_survey_tax_evidence.md`. There is
   no published person-by-person SCF–IRS link; the CPS evidence is taken
   as a prior for surveys in general (John, 2026-10-17).

### 0.5 What nothing in our data can tell

Whether the hidden holders are as wealthy as the reporters. In the cell
above, with 17 hidden holders:

| Hidden holders' net worth | What a PUF zero gets |
|---|---:|
| $400,000 | $76,000 |
| $300,000 (like reporters; the latent method) | $100,000 |
| $200,000 | $124,000 |
| $139,000 (like the zero group; today's rule) | $139,000 |

The SCF has net worth but does not say who the hidden holders are; the
PUF has no net worth. Today's rule and the latent method are two settings
of this one assumption. It is a dial set by judgment and outside
evidence, with the range reported.

### 0.6 Three choices every method makes

1. **How much of the gap is different people and how much is reporting.**
   Mostly settled outside the two files: building the survey's tax units
   and non-filers so that the populations match.
2. **The shape of the reporting rule.** Our data says a lot (0.4, items 1
   and 2); linked studies give a prior.
3. **Whether hidden holders look like reporters.** Only outside evidence
   and judgment (0.5).

| | Choice 1 | Choice 2 | Choice 3 |
|---|---|---|---|
| Today's rule (decision 15) | all different people | not modelled | like non-holders |
| Latent method as run in steps 22 to 24 (no label) | all reporting | learned from cells | like reporters |

### 0.7 The source label (decided, plan decision 14, 2026-10-17)

The label is always on. Without it, the PUF's own relationships (wages by
marriage and children) are learned from PUF and survey rows blended, so a
generated PUF record is not the PUF's, and each survey wave added shifts
it. With it, each source's weights stay coherent within that source.

Keeping the label means it could explain every gap as "different people"
(choice 1 all one way). What stops that: the reporting rule is fit
against the PUF-labelled model's belief given married, children and
wages, then held fixed while the survey's zeros are filled in under the
survey's label. Whatever the fixed rule does not explain is left to the
label, as different people. Being tested in `minimal/26_LABEL_ON.md`.

### 0.8 Open, to discuss next

What the shapes of the reporting rules are for each variable, how a shape
is given to the model, and how to choose a shape per variable.

## 1. What the problem is

For every variable the PUF and a survey share, each household has two
numbers:

- the **true amount**: what we want, in the PUF's terms;
- the **reported amount**: what the survey file shows.

The link between them is a **reporting rule**: given a household's true
amount and what kind of household it is, what does it report? We never see
both numbers for the same household; no one is in both files. What we see:

1. the distribution of true amounts among households of a given kind (from
   the PUF, taken as the truth);
2. the distribution of reported amounts among households of that kind (from
   the survey).

The task is to find a reporting rule that turns the first distribution into
the second, then use it to work back from each survey household's report
to a likely true amount. Everything the model learns from the survey about
how the survey's own variables (net worth) go with income rests on that
step.

"Households of a given kind" can only use traits that mean the same thing
in both files (marriage, children, wages, if we accept that the survey
reports these correctly). Which traits count as that is itself a choice
(section 5).

## 2. Kinds of reporting rule

Each kind has a few settings. Real variables are probably mixes.

| Kind | What it means | Settings | Possible examples |
|---|---|---|---|
| Exact | reported = true | none | marriage |
| Scale | everyone reports a fixed share of the truth | the share | broad income under-reporting |
| Omission | some holders say zero, more often when the amount is small | how the chance of reporting rises with the amount | interest |
| Noise | reports scatter around the truth, some high, some low | how wide the scatter is | wages, business income |
| Rank-preserving reshape | the household at the 70th percentile of reports is the one at the 70th percentile of truth; the dollar mapping between them is free | the mapping | the old "ranks" rule |
| Rounding and heaping | answers bunch at round numbers | the rounding unit | most survey dollars |
| Top-coding | amounts above a cap are cut to the cap | the cap | CPS |
| Definition | reported = true + a component the PUF does not count | the extra component | CPS interest includes IRA interest |
| Not a reporting rule | the survey has different people | (population work) | non-filers |

The two simple rules we started with are two entries in this table.
"Dollars" (decision 15) is the exact rule; "ranks" is the rank-preserving
reshape.

## 3. What the data can tell us

Within groups of households of the same kind, the data shows how the two
distributions differ, and that rules some reporting rules out. Examples:

- **Who is missing.** If the survey finds a smaller share of small holders
  than of large ones, omission depends on the amount (section 6: true for
  SCF interest).
- **Whether totals match.** A survey total near the IRS total, with fewer
  holders, says the missing holders are small. A total short by a fixed
  share at every size says scale.
- **The spread.** A survey distribution wider than the PUF's suggests
  noise.
- **Variation across groups.** A rule that holds across wage groups must
  fit every group at once. Groups with different mixes of true amounts pin
  settings down; this is how the chance of reporting at each amount is
  learned.

## 4. What the data can never tell us

1. **Who matches whom.** Different rules can turn the same true
   distribution into the same reported distribution while pairing
   households differently. "Everyone reports exactly" and "reports scatter
   around the truth in a way that happens to leave the totals and spread
   unchanged" look identical, but they disagree on which survey household
   is which true household, and that decides joint relationships such as
   net worth given interest. "Ranks" and "dollars" are both answers to this
   question that the data cannot check.
2. **Dependence on what only the survey has.** "Rich households misreport
   more" and "rich households have different true amounts" produce the same
   survey, since the PUF has no wealth to compare with.
3. **Errors in the anchor.** If the PUF itself is off (audit studies find
   sole-proprietor income under-reported on returns), nothing inside the two
   files shows it. Only outside evidence can.

Where the data is silent, the answer comes from the assumption. The aim is
to make each such assumption a named setting that can be changed and whose
effect can be measured, as in a Bayesian model where the prior shows
through wherever the data says nothing.

## 5. Assumptions every rule needs

- **The anchor:** the PUF is the truth (later: the PUF corrected by the
  IRS's audit studies).
- **Same population:** both files cover the same people (the non-filer
  work).
- **Traits measured alike:** the traits used to group households mean the
  same in both files. Now marriage, children and wages, set in one line of
  `minimal/23_train_latent.py` (`ANCHOR_HIDE`). If one of them were
  misreported, its error would be read as part of the reporting rule.
- **The rule's settings are fit only on those traits** (decision 18's rule
  from the fake-survey test), never on filled-in amounts, or a wrong setting
  can feed itself.

## 6. Evidence so far, SCF interest and dividends

From `minimal/25_totals.py` (2016 SCF wave against the 2015 PUF with crude
non-filers; 2015 dollars; scratch `checks/step25/totals.md`):

| Interest | SCF | PUF + non-filers | IRS 2015 |
|---|---:|---:|---:|
| Total | $89.9 billion | $94.2 billion (filers alone $89.0 billion) | $95.9 billion |
| Share of units with any | 12.6% | 25.9% | 28.3% |

SCF holders as a share of the PUF side's, by amount:

| Amount | Interest | Dividends |
|---|---:|---:|
| Under $100 | 22% | 32% |
| $100 to $500 | 36% | 49% |
| $500 to $2,000 | 66% | 57% |
| $2,000 to $10,000 | 79% | 63% |
| $10,000 to $50,000 | 118% | 98% |
| Over $50,000 | 104% | 135% |

What this says:

- **Omission that depends on the amount fits.** The share found rises from
  about a fifth of the smallest holders to all of the largest. "Holders of
  every size go missing at the same rate" does not fit.
- **A scale factor below one does not fit at the top.** At $10,000 and more
  the SCF has as many holders and dollars as the PUF, or more. The model's
  learned interest factor of 0.83 (step 23) is probably the curve and the
  factor trading off against each other. Not yet tested (section 7).
- **The top of the SCF has more than the PUF** for dividends. That could be
  the SCF's oversample of wealthy households, a population difference
  rather than a reporting rule. Not examined.
- Nothing has been checked for any other variable.

## 7. Choosing among rules (proposed)

Treat the kind of rule and its settings as choices to tune, judged by four
criteria that answer different questions:

1. **Fit.** How well does the rule predict what survey households actually
   report (zero or not, and how much), on households held back from
   training? This rules out rules the data contradicts. Where several fit
   about equally, the data cannot choose between them, and that is
   information too. (The fit measure is written into
   `minimal/23_train_latent.py`; not yet run.)
2. **Outside checks the rule never trained on.** IRS totals and counts by
   dollar range, other surveys, studies that link survey answers to tax
   records. Section 6 is one such check.
3. **Fake-survey tests.** Make surveys from the PUF with rule A planted,
   fit them with rule B, and measure the error in the outcome we care about
   (the survey's own variable given income on PUF returns). Doing this for
   every pair gives a table of how badly each rule does when the truth is
   another. Choose the rule that does acceptably under every plausible
   truth, not only the one that fits best.
4. **How much the answer moves.** Among rules that pass 1 and 2, how much
   do the final numbers (net worth by income group) change? If little, the
   choice does not matter; if much, report the range.

The Bayesian version: prior weights on the kinds of rules, reweighted by
fit where the data can tell them apart; the spread that remains is reported.

**Proposed first steps** (not started; John's call):

- the fake-survey table of criterion 3 for exact, scale, omission, noise and
  rank-preserving rules, with the outcome error for every planted-fitted
  pair;
- the real-data runs already set up for interest and dividends (written
  into `minimal/23_run.sbatch`, read from the run name): the chance of
  reporting not depending on the amount; a near-sharp cutoff; the dollar
  factor fixed at one; each with the fit of criterion 1;
- the same totals check as section 6 for the other shared variables (wages,
  capital gains) and for the CPS.

## Revision history

- 2026-10-17: written from the discussion with John.
- 2026-10-17 (afternoon): section 0 added, the problem worked through step by
  step with John; the source label decision (plan decision 14) recorded.
