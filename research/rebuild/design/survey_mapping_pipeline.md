---
title: "Mapping a new survey onto the PUF's terms, with agents: a proposed pipeline"
role: design
workstream: rebuild
status: draft
updated: 2026-10-18
sot: research/rebuild/design/plan.md
supersedes: []
superseded_by: null
---

# Mapping a new survey onto the PUF's terms, with agents

Proposed by John on 18 October 2026. Draft for discussion; nothing here is
decided or built.

## 1. The problem

Every survey added to the model needs the same decisions. Which of its
variables measure something the PUF also measures. For each of those,
whether the survey's value can be used as it is, or needs a fixed
conversion, a different dollar scale, or hidden holders filled in. Which
variables are the survey's alone. For the SCF these decisions took about a
week of hand work, spread over the interest/dividends/capital-gains work
(`design/cell_method.md`, `minimal/31_settings_scf.json`) and the
additional-income-items work (`minimal/33_settings_scf.json`). Done by hand,
this does not scale to many surveys.

The decisions are already written down in one place per survey: the
settings sheet, which the training code reads. So the work can be split
into agents whose job is to produce a proposed settings sheet, with the
evidence for every entry, for John to review.

## 2. The settings sheet is the contract

Everything an agent decides must be expressible as an entry in the sheet,
using only rules the code already supports. Today's entries, per variable:

| Entry | What it says | Allowed values today |
|---|---|---|
| Role | how the variable enters the model | taken as correct (defines cells); mapped; survey-only; not used |
| PUF counterpart | which PUF field, or sum of fields, the survey's number corresponds to | field names; a sum of fields; for a total the PUF holds as parts, the computed total |
| Survey items | which survey questions make up the variable | item codes, with the sum or difference |
| Fixed conversion | a definition difference corrected before anything else | cap losses at a value; read a code (e.g. −1) as zero; drop a component |
| Splitting to tax units | how a household amount is divided between tax units | the builder's existing shares (by earnings, by business income, equal) |
| Dollar rule | how reporters' amounts relate to PUF amounts | unchanged; a fixed factor; rank (same weighted position within sign) |
| Hidden holders | whether zero-reporters can be filled in | no; the cell method, with cells, size ranges and top lines |
| Cells, size ranges, top lines | the cell method's settings | as in `31_settings_scf.json` |

A new kind of rule (for example, a conversion that uses a second survey
question to adjust the first) is added by John, coded once, and then
becomes available to the agents. Agents may propose new rules but may not
use them.

## 3. The agents

Four agents, run in order. Each writes a file; nothing is changed until
John signs off.

### 3.1 Documentation reader

Input: the survey's codebook and questionnaire; the PUF's field
definitions (`config/variable_guide/`, IRS form instructions).

Output: one row per survey item that could bear on a PUF concept:

- item code and exact wording, quoted;
- reference year and timing (calendar year before the interview, etc.);
- unit (respondent, couple, whole household) and who is asked;
- whether it asks for the amount "as on your tax return" or as income
  received;
- what is netted out (expenses, depreciation, losses) and what is bundled
  in (for example, the SCF's unemployment item adds workers' compensation;
  its alimony item adds child support);
- special codes (−1, refused, imputed flags);
- the candidate PUF counterpart, with the PUF field's definition beside it,
  and the differences in concept listed.

### 3.2 Data comparer

Input: the survey file and the PUF side, both in the same year's terms.

Output: for each candidate pair, the same standard tables every time:

- share with income, with a loss, with nothing; overall and by cell;
- totals, and percentiles of positive amounts and of losses;
- share of units by size range, by cell (the cell method's comparison);
- for items the PUF holds as parts, where the PUF's losses and amounts
  come from, part by part;
- the survey's total against the IRS's published total for the year.

`minimal/30_compare.py` and the business-income comparison
(`minimal/35_*`) are first versions of this.

### 3.3 Mapper

Input: the two outputs above, the list of allowed rules, and the PUF
side's current variables.

Output: a proposed settings sheet, and beside each entry:

- the reason, citing the quoted wording and the table that supports it;
- what the choice assumes;
- a confidence flag: settled, or a question for John, with the options and
  what each would assume.

Guidance written into the mapper's instructions, from what the SCF work
taught:

- A difference in the share of holders that is concentrated at small
  amounts is the pattern of forgotten small items (the cell method). A
  difference in the share with losses may instead be a difference in
  concept (tax losses from depreciation and passive-loss rules against
  income received); do not treat it as forgetting without evidence.
- Bundled items are not shared unless the bundle can be undone.
- A survey amount that runs far above the PUF at every percentile, with
  the same share of holders, is a dollar-level difference; one that runs
  above only at the top may be a difference in who is in the file.
- Never map a variable whose concept cannot be stated in one sentence.

### 3.4 Checker

Input: the proposed sheet. Runs, through Slurm, the fixed checks on a
short training run:

- identities: parts add to totals, AGI to the dollar;
- holders and amounts after filling in, against the PUF by cell and range,
  and against IRS counts by year;
- combinations of mapped items against the PUF (`minimal/31_combos.py`);
- the fake survey: the method run on half the PUF dressed as a survey
  with the same reporting pattern, scored against the truth;
- net worth (or whatever the survey's own variables are) before and after
  reweighting, against the survey.

Output: a short report with pass, fail or "look at this" per check.

### 3.5 John

Reviews the sheet, the flagged questions and the checker's report, and
signs off. Only then is the sheet used in a real run.

## 4. The first test: re-derive the SCF sheet

Before using it on a new survey, run the pipeline on the SCF with no access
to the sheets built by hand (`31_settings_scf.json`, `33_settings_scf.json`)
or the notes about them. Compare its sheet with ours, entry by entry. Every
disagreement is either something the hand work missed or a gap in the
agents' instructions. This also gives a measure of how much John needs to
change in a sheet the agents propose.

The second test is the CPS, the next survey planned.

## 5. Open questions for John

1. Should the agents propose cells (which variables are taken as correct),
   or is that always John's choice?
2. How much evidence is enough for "settled" rather than "question for
   John"?
3. Where the documentation reader and the data comparer disagree (the
   wording says one thing, the numbers suggest another, as with the SCF's
   rent-and-trusts item holding partnership income), which wins by
   default?
4. Should the same agents also propose new PUF-side variables when a
   survey measures something the PUF has but the model does not yet carry?

## 6. Worked example

To be added from the business-income comparison (`minimal/35_*`) when it
is done: what the documentation reader and the data comparer produced for
business income, and what the mapper would have proposed.
