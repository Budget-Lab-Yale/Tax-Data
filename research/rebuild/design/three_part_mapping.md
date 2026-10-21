---
title: "A three-part rule for putting a survey variable on the PUF's terms: who holds it, the rough level, then rank"
role: design
workstream: rebuild
status: draft
updated: 2026-10-18
sot: research/rebuild/design/plan.md
supersedes: []
superseded_by: null
---

# A three-part rule for putting a survey variable on the PUF's terms

Worked out with John on the evening of 18 October 2026. Not built, not
decided; John wants to think it through before anything runs. It would
replace or extend the cell method (`design/cell_method.md`) for mapped
variables.

## 1. Why

The cell method fills in hidden holders by size range: in each cell it
compares SCF reporters with the PUF range by range (under $100, $100–$500,
…) and fills the shortfall in each range. Because the comparison is range
by range, the reporters' dollars must already be on the PUF's terms before
it runs. That is why the cell method cannot be combined with a rank rule on
dollars: rank makes the reporters match all PUF holders, including the
small ones the fill-in is meant to supply, so the two would correct the same
gap twice.

Two cases need something better:

- **Social Security plus gross pensions.** John wants to keep this concept
  (PUF E02400 + E01500 against SCF X5722). The middles match (median
  $23,200 on the PUF side, $22,700 in the SCF) but the SCF's top is thin
  (99.9th percentile $200,000 against $1.03 million) and working-age
  married couples have the item far more often on the PUF (23% against 15%
  at wages $75k–200k; probably rollovers and lump sums in gross pensions).
- **Business income.** SCF dollars for sole proprietorships run about twice
  the PUF's, and losses are about a quarter as common in the SCF.

## 2. The rule

For each mapped variable, each SCF wave and each cell (wage group ×
married), redone every training pass from pass 3, like today's fill-in.

**Part 1. Who holds it (extensive margin).** From the PUF: the share of the
cell holding the item (for items that can be negative, the share with a
profit and the share with a loss, separately). From the SCF: the share of
reporters. If the PUF's share is higher, fill the shortfall among the SCF's
zero-reporters: the model gives each a chance of holding, learned from how
much it resembles the reporters; the chances are scaled so the cell's
total matches the PUF; then draw. If the SCF's share is higher, nothing is
removed; the excess is recorded. This is today's cell method with a single
size range, so it needs no dollar amounts.

**Part 2. The rough level.** Reporters: reported amount × a factor set per
variable (1 if the survey's dollars are taken as right). Hidden holders
from part 1: the model draws an amount for each from what it has learned
about holders like them. The only purpose is to put reporters and hidden
holders on roughly the same scale, so the ordering in part 3 is sensible.

**Part 3. The shape (rank within the cell).** Sort every holder in the
cell, reporters and hidden holders together, by amount (profits and losses
separately). Give each the PUF's amount at the same weighted position
among the PUF's holders in that cell. That is the value the model trains
on this pass. Reported values are kept aside, unchanged.

## 3. Worked example

One cell, 10 SCF households of equal weight. Two reported interest, $600
and $3,000. The PUF says 5 in 10 hold interest.

- Part 1: three zero-reporters are filled in, the ones the model finds most
  like holders.
- Part 2: factor 1, so reporters stay at $600 and $3,000; the model draws
  $40, $150 and $900 for the three hidden holders.
- Part 3:

| SCF holder | Amount after parts 1–2 | Position | PUF holders' amount there | Trained on |
|---|---:|---:|---:|---:|
| hidden | $40 | 10% | $30 | $30 |
| hidden | $150 | 30% | $120 | $120 |
| reporter | $600 | 50% | $400 | $400 |
| hidden | $900 | 70% | $1,100 | $1,100 |
| reporter | $3,000 | 90% | $4,500 | $4,500 |

The $600 reporter ends at $400 because a hidden holder was drawn above it.

## 4. Where each piece comes from

| | Source |
|---|---|
| How many hold | the PUF |
| Who holds | the model, from resemblance to reporters (learned, then drawn) |
| Order of amounts | reported amounts, and the model's draws for hidden holders |
| Dollar values | the PUF, by position within the cell |

Nothing is chosen at random except the final draw from learned chances;
the draw keeps a household with a 60% chance from being picked every time.

## 5. Against today's cell method

- Gone: size ranges, the top line, and the conflict between rank and
  filling in (rank now runs on the completed set of holders).
- Kept: counts per cell from the PUF, the model choosing who, redrawing
  every pass.
- Assumes: order within a cell is the same in both files (only the order of
  reported amounts is used, not their level); hidden holders' sizes come
  only from the model (today the PUF forces them small; here the model has
  to learn it); conditioning is only on wage group × married.
- Social Security plus gross pensions: the SCF already has more holders, so
  part 1 adds nothing; part 3 gives the SCF's top recipients the PUF's top
  amounts, rollovers included.
- Business income: part 1 fills in losses separately from profits; part 3
  ranks profits and losses separately.

## 6. Related ideas (from a pasted outside note, discussed 18 October)

- "Conditional rank translation", and its sequential version (each variable
  ranked given the ones before it), is close to part 3; our cells are a
  crude version of "given the others". The sequential version is a possible
  extension.
- Its warning about zeros ("you need randomized ranks within the mass or an
  explicitly chosen coupling") is where rank matching failed in the
  learned-reporting-curve work (`minimal/27_RESULTS.md`); part 1 is that
  coupling.
- Optimal transport (a cost for moving one record to another) is not tried.
- A shared true state with a reporting model per source is plan decision 18
  (`design/translation_by_latent_amounts.md`); the cell method is a
  restricted version of it.
- Fake survey versions of PUF records: used here as tests; using them as
  training data is a new idea.
- Not yet faced: with two donor surveys (SCF wealth, SIPP tips) the model
  will impose a relationship between their variables that no data
  identifies.

## 7. Open questions for John

1. Is ranking within wage group × married enough, or should part 3 condition
   on more (the sequential version)?
2. Should hidden holders' amounts be pushed toward small (as the cell method
   does now), or left to the model?
3. For Social Security plus gross pensions, is importing the PUF's top
   (rollovers, lump sums) acceptable, or should the PUF concept change?
4. Tests before adopting: the fake survey (where the truth is known), the
   combinations check against the PUF, and a comparison with the cell
   method on the same run.
