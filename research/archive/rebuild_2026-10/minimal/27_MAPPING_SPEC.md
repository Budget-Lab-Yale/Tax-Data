---
title: "Step 27 (build spec, for John's sign-off): the mapping chosen in pretraining, capital gains in pieces, four SCF waves"
role: plan
workstream: rebuild
status: draft
updated: 2026-10-17
sot: research/rebuild/design/plan.md
supersedes: []
superseded_by: null
---

# Step 27: build spec

Agreed with John on 17 October 2026; the reasoning is in
`design/reporting_rules.md`, section 0. Nothing here is built or run yet.
The evidence behind the capital gains and wages choices is
`28_WAGES_CG.md`.

## What the run does, in one paragraph

The model is trained on the 2015 PUF (with crude non-filers) and four SCF
waves, with the source label always on. Every PUF variable is its own
slot, capital gains in its separate pieces. For each SCF variable that is
not trusted as reported, a stated formula says which combination of PUF
pieces it corresponds to (the definition), and a reporting rule says how
the survey's number relates to that combination. The reporting rule comes
from two families whose settings, and the probability that each family is
the right one, are learned during pretraining by how well each predicts
what SCF households actually reported. The rule is fit on the 2016 wave
against the 2015 PUF and used for every wave. Then the usual generation
and tilt to 2019.

## 1. The data

**PUF, 2015.** Capital gains enters as separate variables, not one sum.
Proposed pieces (the build lists every capital gains field the file has
and confirms this set):

| Piece | PUF field | In AGI? |
|---|---|---|
| Net short-term gain or loss, Schedule D, before the loss limit | P22250 | no (detail) |
| Net long-term gain or loss, Schedule D, before the loss limit | P23250 | no (detail) |
| Capital gain or loss on the return, after the $3,000 loss limit | E01000 | yes |
| Capital gain distributions reported without a Schedule D | E01100 | yes |
| Other gains or losses, Form 4797 | E01200 | yes |

Other income becomes AGI minus wages, interest, dividends, E01000, E01100
and E01200, so AGI still adds up exactly on every record.

**Open, for sign-off: which pieces are drawn and which are computed.**
E01000 is, by tax law, close to a function of P22250 and P23250 (their sum,
with losses limited to $3,000). The plan's rule is that a derived variable
is never drawn (decision 12, D25). If the rule holds on the file, E01000
should be computed from the two Schedule D pieces, not drawn. But
`28_WAGES_CG.md` found the Schedule D detail and E01000 disagree on 4.5
million weighted returns beyond rounding, cause unknown. Proposal: the
build first finds the cause. If E01000 can be computed from the pieces up
to a small, explained remainder, draw the pieces and compute E01000; if
not, draw E01000, E01100 and E01200 and treat P22250 and P23250 as detail
drawn after them.

**SCF waves:** 2013, 2016, 2019 and 2022 (tax years 2012, 2015, 2018 and
2021), built by `01_build_table.R --scf-wave`. 2013 is new; if its raw
files are not on disk, the run goes ahead with three waves and says so.

**Non-filers:** the crude 2017 CPS pool of step 23, unchanged (capital
gains pieces zero; the CPS does not ask).

**New tables only.** Every earlier table stays as it is, so every earlier
result can still be reproduced.

## 2. The settings sheet

One line per SCF variable. Printed into every run's output.

| SCF variable | Type | Concept (PUF formula) | Mapping fit on |
|---|---|---|---|
| married | assumed correct | married | |
| dependents | assumed correct | dependents | |
| wages | assumed correct (see open questions) | E00200 | |
| interest | mapped | E00300 | married, dependents, wages |
| dividends | mapped | E00600 | married, dependents, wages |
| capital gains | mapped | a mix of two formulas (below) | married, dependents, wages |

**Capital gains, two concepts mixed household by household.** The SCF's
answers are on a mixed basis: 0.93 million units give exactly −$3,000
(after the loss limit) and 0.70 million give a loss beyond it (before the
limit). So each SCF household's answer is taken to be one of:

- after the limit: E01000 + E01100 (+ E01200 if the SCF question covers
  sales of business property; the build checks the question wording);
- before the limit: P22250 + P23250 + E01100 (same note on E01200).

The share of households answering on each basis is a setting learned in
training. Unlike the reporting families, this is a real mix, since the
data shows different respondents doing different things.

## 3. The reporting rule: two families, one of them right

For each mapped variable, two families relate the survey's number to the
PUF concept:

- **A. Who reports, and how many dollars.** The chance a holder reports
  anything follows an S-curve in the amount (settings: the middle, where
  half report; the steepness). Reporters give a fixed share of the amount
  (setting: the dollar factor). Exact reporting, a flat chance, a sharp
  cutoff and "everyone reports, dollars scaled" are all settings of A.
- **B. Rank-preserving, zeros included.** The survey household at the
  p-th percentile is the PUF household at the p-th percentile.

The two start at equal probability. The probability that each is right is
updated by the score: for each held-back survey household, the chance the
family gives to exactly what the household reported, given only its
assumed-correct traits and what the PUF says households with those traits
hold (the model's belief under the PUF label, learned from PUF rows only).
With about 1,250 held-back households the probability will probably go
almost all to one family unless the two fit nearly alike.

For capital gains, the score uses the mix of section 2 as well.

## 4. Filling in, each training pass

For each SCF record and mapped variable:

1. Draw a family at its current probability (one draw for the whole
   survey per pass), and for capital gains a concept per household at the
   learned mix.
2. Draw the record's PUF pieces from the model's belief under the SCF
   label, given everything else on the record, keeping only draws whose
   combination, read through the family's rule, could have produced what
   the household reported. A reported $0 allows many values; a reported
   $750 allows few.
3. The SCF-labelled model trains on the drawn pieces. They are redrawn
   every pass.

Step 2 for capital gains is new: the pieces must add up, through the
formula, to the mapped total. This is test 2 of decision 18, done first on
a fake survey (section 8).

## 5. Years: one rule for every wave

The reporting rule and the capital gains mix are fit on the 2016 wave
against the 2015 PUF, and used unchanged for 2013, 2019 and 2022, with
dollar amounts read on each wave's stretched ruler (the 2015 PUF's
positions grown by the IRS growth in each item). So the chance of
reporting at a given position on the PUF's scale is the same in every
wave.

**Check per wave:** after filling in, each wave's share of units with
interest, dividends and capital gains against the IRS's count of returns
for that tax year (SOI Table 1.4 for 2012, 2018 and 2021; the targets file
now holds 2015 and 2019 only, so the build adds those years). A large miss
says that wave's reporting changed and needs its own rule.

## 6. Held back

- **SCF 2016 wave:** 20% of households (about 1,250 of 6,248), all five
  copies of each together. Kept out of fitting the mapping (A's settings,
  B's percentile map, the capital gains mix); the score is computed on
  them. They still train everything else.
- **PUF:** nothing extra; the existing 2% slice stays for the usual
  validation loss.

## 7. Training, generation, tilt

- **Training:** label on (source and tax year), the same network and 80
  passes as step 23, all records but the held-back slices.
- **Generation:** under "PUF, 2015", every PUF variable, capital gains
  pieces included, in random order; then switch to the SCF label and draw
  net worth given what the SCF has, its capital gains as the drawn PUF
  pieces (plan decisions 9 and 16). Year mix for net worth: the 2016 wave
  token alone, the best in step 21, with the others reported beside it.
- **Tilt to 2019:** the 136 IRS targets, with the capital gains targets
  restated for the pieces (Schedule D gains and losses, distributions,
  Form 4797, all in SOI Table 1.4), plus the SCF wealth targets as in step
  23.

## 8. Order of work, with a stop for John

1. **Tables.** Find the cause of the E01000 conflict; settle the pieces;
   build the PUF table with the pieces and the four SCF waves; check AGI
   adds up to the cent on every PUF record and each piece's total against
   SOI. Report.
2. **Fake-survey test of the split** (decision 18, test 2). Make a fake
   survey from the PUF that reports one capital gains number, on a planted
   mix of the two bases, with small holders hiding. Check that the run
   recovers the planted mix, the reporting settings, and the split into
   pieces, and that other income on PUF returns comes out as well as in
   step 26.
3. **Stop: report 1 and 2 to John** before any real-data training.
4. **Real runs,** three seeds each:
   - R0: today's rule (dollars as reported, label on) on the new tables,
     four waves: the comparison;
   - R1: this method, 2016 wave only, which isolates the method from the
     extra waves;
   - R2: this method, all four waves.
5. **Generate, tilt, report.**

## 9. What the report shows

1. The settings sheet.
2. For each mapped variable: the probability of A and of B; A's settings
   and which special case they sit near (chance of reporting at $50, $500,
   $5,000; the dollar factor); whether the dollar factor is pinned (the
   score with the factor held at 0.8, 0.9, 1.0, 1.1).
3. The capital gains mix: the share of SCF households on each basis.
4. The filled-in values, five draws per SCF row, and each wave's share of
   holders after filling in against the PUF side's and the IRS count: what
   is left to "different people".
5. Against R0 and the earlier runs: the tilt's move; net worth by wage
   group, for units with no wages, and for units with none of interest,
   dividends or capital gains; the top 1% share; before and after the
   tilt; the generated PUF variables against the PUF.

## Open questions for John

1. **Wages as assumed correct.** The SCF's wages run above the PUF's at
   the top (5.7 million units above $200,000 against 4.1 million returns)
   for a reason not found. Keep them on the trusted list for this run, or
   look for the cause first?
2. **The capital gains pieces:** the proposal in section 1 (find the
   conflict, then decide what is drawn and what is computed).
3. **The stop after step 3 of the order of work,** or go straight through.

## Revision history

- 2026-10-17: first written as a spec for the two families on the 2016
  wave.
- 2026-10-17: rewritten as the build spec: capital gains in pieces with a
  definition per SCF basis, four SCF waves under one rule, the order of
  work with a stop.

## John's sign-off (2026-10-17, afternoon), overriding the text above where they differ

1. **Wages:** assumed correct for this run.
2. **Capital gains pieces, simple version for now** (to be made precise
   later): four drawn PUF variables that add up exactly to the capital
   gains on the return (line 13 = E01000 + E01100):
   - `kg_st` = P22250 (net short-term, Schedule D);
   - `kg_lt` = P23250 (net long-term, Schedule D);
   - `kg_dist` = E01100 (distributions without a Schedule D);
   - `kg_other` = E01000 − P22250 − P23250, the residual (the loss limit,
     carryovers and the unexplained conflict all land here).
   Form 4797 gains (E01200) stay in other income. Other income = AGI minus
   wages, interest, dividends and the four pieces. The line-13 total is
   computed, never drawn. SCF formulas: after the limit = all four pieces;
   before the limit = `kg_st` + `kg_lt` + `kg_dist`.
3. **No stop.** Go straight through the order of work (section 8) while
   John is away; the fake-survey test of the split is still run first and
   reported, but its result does not hold up the real runs unless it fails
   outright.

## After the run (John, 2026-10-17, evening)

Family B (rank-preserving, zeros included) is dropped: it cannot say
whether a survey zero is a true zero or a small holder, which is the
question the fill-in needs answered (`27_RESULTS.md`), and it is the
"ranks including zeros" rule rejected on 15 October (`13_RESULTS_B.md`).
Family A is the only family going forward.
