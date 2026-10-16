---
title: "Shared X between the PUF and a survey: two independent write-ups compared, and the merged rule"
role: evidence
workstream: rebuild
status: current
updated: 2026-10-15
sot: research/rebuild/design/plan.md
supersedes: []
superseded_by: null
---

# Shared X: the two write-ups compared

John asked (15 October, midday) for the exhaustive list of reasons the
PUF and a survey disagree on a shared variable, the range of fixes, and
the rule that follows. Two write-ups were made independently:
`research/archive/minimal_11_SHARED_X_THEORY_claude_2026-10-15_superseded.md` (Claude) and `research/archive/minimal_11_SHARED_X_THEORY_codex_2026-10-15_superseded.md`
(Codex, model gpt-6-sol, medium effort; read-only, no code run). This
file is the comparison and the merged rule. Read the two originals for
the full tables.

## Where they agree

1. **The mechanism list is the same.** Population (nonfilers, unit
   construction, sampling and weights), definition (concept, netting and
   sign, reference period), time (nominal growth, distributional change,
   policy change), measurement (small-amount omission, rounding, scale
   misreporting, top-coding, imputation and noise, agency editing of the
   PUF itself). Both say real surveys have several at once.
2. **Definition gaps cannot be fixed from data.** The questionnaire and
   the table builder fix them. Data read a definition gap as a scale
   error (the D3 run).
3. **Within-bracket means do not prove the dollars are right.** Under a
   uniform scaling the reported means in a bracket still match. Both
   retract the earlier claim that they did.
4. **The tilt cannot see a bad bridge.** Y given X by group (wealth by
   wage group) is the check, not only the move in nats.
5. **For years without a PUF, PUF cut points projected from SOI are the
   working rule**, and ranking over the whole distribution including
   zeros is the sensitivity case. Both say the latter assumes that the
   same population percentile is a comparable unit in both files, which
   the interest table says is off between $100 and $10,000 and at the
   top.
6. **Top-coding**: treat the capped region as PUF-owned or coarsened; do
   not infer top order from a cap.

## Where Codex adds something

1. **Do not take the scale factor from the ratio of totals** when holder
   rates or populations differ. Estimate it from matched positive
   quantiles above a line where both files report fully, after
   populations are aligned, or from component totals. This explains the
   two D2 failures (the 10% wage cut missed; doubled capital gains
   misread), and corrects step 4 of Claude's rule.
2. **A variable can be removed from the bridge** and made source-owned
   when no common concept can be built: CPS interest until its taxable
   part can be separated; CPS capital gains before 2018, where the
   question was not asked (coding it as zero invents non-holders).
3. **Structural absence is its own mechanism**: a question not asked in
   a year or a survey.
4. **Conditional comparisons are needed, not only marginals**: X given
   other shared variables and filing status. Equal marginals can hide
   different relationships with Y and Z. This is the label-indifference
   check from `08_STATE` made concrete.
5. **The detection table brackets capital gains on the absolute amount,
   which hides the sign difference** (PUF 5.6% loss units, SCF 1.8%).
   Losses need their own comparison.
6. **Under ranks including zeros, the placement of S's zeros needs an
   explicit rule**, not random assignment, and an explicit zero indicator
   should be kept. (John's suggestion was random placement; this is a
   point of disagreement to settle empirically.)

## Where Claude adds something

1. **A signature table**: the pattern each mechanism leaves in the
   detection table (zero share, cumulative ratio by amount, within-bracket
   means, total). Codex has no such table and would say signatures are
   not unique. Both are right: the signatures are a screen that narrows
   the cases, not a proof. The screen plus the questionnaire is the
   procedure.
2. **PUF cut points for other years can come from SOI's by-bracket
   tables** (counts and amounts per item per AGI bracket, every year),
   not only from one growth factor per item. Codex says the same in
   different words ("project the base reference using SOI participation
   counts and totals, with separate treatment of the positive shape").
3. The reminder that **other PUF years may exist** and would change the
   year question.

## Where they differ

- **Identification.** Claude: totals break the tie between "scaled
  dollars" and "missing holders" (omission keeps the total, scale changes
  it). Codex: only under strict assumptions (common population, exact
  zeros, pure scaling of every amount); with nonfilers, unit differences
  and rounding present, totals do not identify it, and SOI totals should
  not be forced on a survey that covers more people or a broader concept.
  Codex is right in general; Claude's version holds for the SCF's four
  variables because the population and definition steps come first.
- **Tone.** Codex marks almost every mechanism "not identified from
  marginals alone". Claude treats the screen as usable. The merged rule
  below uses the screen and names what it cannot settle.

## The merged rule

For each survey wave, before training:

1. **Documentation first.** Read the questionnaire and the unit builder.
   Fix definitions, sign and netting conventions, and reference periods
   in the table builder. Mark variables not asked (structural absence)
   as absent, never zero. If no common tax concept can be built, the
   variable is source-owned, not shared.
2. **Population.** Build tax units the same way as the PUF's. Apply a
   filing rule to the survey; drop or flag the units that would not file.
   Check zero shares on all shared variables jointly before and after.
3. **Screen each shared variable** with the detection table in the same
   year's dollars, with losses compared separately, plus the SOI total
   for the same concept and the same return population. Read the
   signature: omission, rounding, scale, top-coding, top oversample, or
   none of these (go back to step 1).
4. **Translate only a documented scale shift**, with the factor from
   matched positive quantiles above a line where both report fully, not
   from the raw ratio of totals. Mark top-coded regions as absent on the
   survey. Do nothing to the dollars for omission or rounding.
5. **Band on the PUF's cut points for that year**, keeping the source
   token so the model can hold a fatter zero bin for the survey. For a
   year without a PUF, project the cut points from SOI counts and totals
   by bracket; one growth factor per item is the crude version.
6. **Write the sheet**: survey, wave, variable, mechanism, operation,
   evidence.
7. **After training, check the bridge**: label swap on shared variables,
   PUF-style holder shares, Y by wage and interest group against the
   survey, top shares. A small tilt move is not evidence.

What the rule cannot settle from the data: definition versus scale when
totals are above one (questionnaire); scale versus shape when the
variable has no outside total (wealth, consumption); noise and imputation
effects; where each survey zero would sit if it is an unreported holder.

## What to run next (not started)

1. Ranks including zeros (John's proposal) on the single-year table,
   scored next to E4, with two placements of the survey's zeros: random
   within the zero mass, and ordered by a prior (wages). One table each,
   two trainings.
2. The same on the three-wave table, where it needs no growth factors,
   next to Y1.
3. The detection table with losses compared separately.
4. Replicate trainings of E4 on fixed GPU types, so the noise band is
   known before any of the above is read.
