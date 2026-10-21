---
title: "The cell method: turning a survey's reported amounts into PUF amounts during pretraining"
role: design
workstream: rebuild
status: current
updated: 2026-10-18
sot: research/rebuild/design/plan.md
supersedes: []
superseded_by: null
---

# The cell method

Worked out with John on 17 and 18 October 2026, step by step. This note is
the full statement of the method: what problem it solves, every setting a
user supplies, every step in order with a worked example, what it assumes,
what was learned from the runs that led to it, and how the first full run
is set up. It is the translation step of plan decision 18 in its current
form. Read with `reporting_rules.md`, section 0 (the problem worked
through), which this note builds on.

## 1. The problem in one page

We want net worth (and later other survey-only variables) on PUF records.
Net worth exists only in the SCF. So the model learns, from the SCF, how
net worth goes with the variables both files have (marriage, children,
wages, interest, dividends, capital gains), and then draws net worth for
each PUF record from its values of those variables.

That works only if a variable means the same thing in both files. Some do
(marriage, children, and we take wages as one). Some do not. In the SCF,
many households that hold a little interest say "none". For married
couples with wages of $75,000 to $200,000 (2016 SCF wave against the 2015
PUF, weighted):

| Interest | PUF (per 100 households) | SCF (per 100 households) |
|---|---:|---:|
| none | 52.7 | 80.0 |
| under $100 | 27.7 | 8.1 |
| $100 to $500 | 12.4 | 5.1 |
| $500 to $2,000 | 4.6 | 3.3 |
| $2,000 to $10,000 | 1.9 | 2.7 |
| $10,000 or more | 0.7 | 0.9 |

The SCF's "none" group is a mix of households that truly hold nothing and
households that hold a little and said nothing. If the model learns net
worth for "none" from that mix, a PUF return with no interest gets too
much net worth. The method separates the two groups inside the SCF before
the model learns from it: it fills in the hidden holders.

Two questions have to be answered for that:

1. **How many** of the SCF's zero-reporters are hidden holders, and of
   what size? The method answers this from the data, by comparing the two
   files within groups of similar households (cells).
2. **Which** zero-reporters are they? The method answers this with the
   model itself: the zero-reporters who most resemble the households that
   did report are the most likely hidden holders.

This is the approach Larrimore, Mortenson and Splinter (2023, Journal of
Policy Analysis and Management) used for unemployment insurance in the
CPS: keep the reporters, add recipients within each income percentile
until the count matches the IRS. The differences here: the comparison is
by size range as well as by count, and the choice of who is added is made
by the model from everything on the record, inside pretraining, because
we then learn net worth from the filled-in values (they used the corrected
benefits as an output, so who received them mattered less).

## 2. The settings a user supplies

All of these are written on one settings sheet per survey, read by the
code and printed into every run's output. Nothing below is hard-wired.

| Setting | What it is | Value for the SCF, first run |
|---|---|---|
| Assumed correct | Survey variables taken as reported, meaning the same as on the PUF | married, number of dependents, wages |
| Mapped | Survey variables to be filled in | interest, dividends, capital gains |
| Concept | For each mapped variable, which PUF variable (or sum of PUF variables) the survey's number corresponds to, and any fixed conversion applied first | interest = PUF taxable interest (E00300); dividends = PUF ordinary dividends (E00600); capital gains = PUF line 13 (kg_st + kg_lt + kg_dist + kg_other), with every SCF loss below −$3,000 set to −$3,000 first |
| Dollar rule (g) | How reporters' amounts relate to PUF amounts | unchanged (reported amount = PUF amount) for all three |
| Cells | The groups within which the files are compared, built from assumed-correct variables only | wage group (none, under $25,000, $25,000–$75,000, $75,000–$200,000, over $200,000) × married |
| Minimum cell size | Cells with fewer survey households are merged with a neighbour | 300 households in the fitting wave |
| Comparison ranges | Broad size ranges per mapped variable, used only to compare the files | interest and dividends: under $100, $100–$500, $500–$2,000, $2,000–$10,000, $10,000–$50,000, $50,000 and over; capital gains: −$3,000 to $0, $0–$1,000, $1,000–$10,000, $10,000–$50,000, $50,000–$250,000, $250,000 and over |
| Top line | Holders are filled in only below this amount | interest $2,000; dividends $2,000; capital gains $10,000 (gains); small losses (−$3,000 to $0) may be filled in |
| Fitting wave | The survey wave compared with the PUF to build the cell table | SCF 2016 wave (tax year 2015) against the 2015 PUF |

Why each one is a choice, briefly:

- **Assumed correct.** These define the cells, so an error in them would
  put households in the wrong cell. The SCF's wages run above the PUF's at
  the top (`minimal/28_WAGES_CG.md`); kept as assumed correct for now
  (John, 17 October).
- **Concept.** The capital loss cap is a definition difference, not a
  reporting one: a household that lost $40,000 has −$3,000 on line 13, and
  some SCF respondents give the full loss. Capping first puts the SCF's
  number in line-13 terms (section 7, problem 3).
- **Dollar rule.** Within each size range, SCF reporters' amounts match
  the PUF's (for interest: $4,187 against $4,443 at $2,000–$10,000;
  $18,266 against $20,134 at $10,000–$50,000); the SCF's excess at the top
  is in the number of households, not their amounts
  (`minimal/30_compare.py`, scratch `checks/step30/compare.md`). For
  business income the rule will not be "unchanged" (linked survey and
  audit studies), and will be set variable by variable.
- **Top line.** At large amounts nearly every holder reports, so a
  shortfall there is not hidden holders; it is sampling noise in a thin
  cell or a difference in who is in the file (section 7, problem 2).

## 3. Before training: the cell table (done once)

**Step 1. Put every household in a cell.** Every PUF record and every
survey record goes into one of the cells by its wage group and marital
status. With the settings above and the 2016 wave, two pairs of cells are
merged for having fewer than 300 households (not married with wages
$75,000–$200,000 and over $200,000; married with no wages and under
$25,000), leaving 8 cells.

**Step 2. Compare the files in each cell.** For each mapped variable and
each cell, compute, weighted, the share of the cell's units in each
comparison range: on the PUF side (the PUF with non-filers) and in the
fitting wave of the survey, after the concept conversion.

**Step 3. The gap in each range below the top line.** If the PUF's share
is larger, the difference is the share of the cell's units to be filled
in as hidden holders in that range. If the survey's share is larger, the
target is zero: no survey reporter is removed or changed; the excess is
recorded (it is either a difference in who is in the file or noise).
Above the top line, nothing is filled in, whatever the gap; the gap is
recorded.

Worked example, married with wages $75,000–$200,000, interest:

| Interest | PUF | SCF | Gap | Filled in |
|---|---:|---:|---:|---:|
| under $100 | 27.7 | 8.1 | 19.6 | 19.6 |
| $100 to $500 | 12.4 | 5.1 | 7.3 | 7.3 |
| $500 to $2,000 | 4.6 | 3.3 | 1.3 | 1.3 |
| $2,000 to $10,000 | 1.9 | 2.7 | −0.7 | 0 (above the top line; recorded) |
| $10,000 or more | 0.7 | 0.9 | −0.2 | 0 (above the top line; recorded) |

28.2 of every 100 households in this cell are to be filled in, out of 80
zero-reporters: 35% of the cell's zeros. After filling in, the cell has
51.8 with no interest against the PUF's 52.7; the 0.9 difference is the
SCF's extra holders at $2,000 and over, left as reported.

**Step 4. Express the targets as shares of the cell's zero-reporters.**
19.6 / 80 = 24.5% of zeros to under $100, 9.1% to $100–$500, 1.6% to
$500–$2,000, the remaining 64.8% stay at zero. This form carries over to
other waves (step 5).

**Step 5. Other waves.** The 2013, 2019 and 2022 waves use the same table
of shares of zeros, applied to each wave's own zero-reporters in each
cell, with the comparison ranges stretched to that wave's tax year by the
IRS growth in the item (the same stretched dollar scale the model uses).
This assumes the pattern of non-reporting is the same in every wave. In
step 29 this way of carrying the table over was stable, where carrying a
fitted reporting rule over was not (capital gains fill-ins ran away).

The cell table is the only place the PUF enters the fill-in. It fixes how
many hidden holders there are in each cell and range, before training,
and nothing in training changes it.

## 4. During training: who is filled in (every pass)

Pretraining is the model practising one task over and over: take a
record, hide some of its variables, guess them from the rest, compare
with the true values, adjust slightly. After enough passes over every
record, the model is good at "given these variables, what is likely for
that one". The fill-in uses exactly that ability.

**Step 6. The first two passes train on the survey as reported,** so the
model has learned the basic patterns before it is asked to guess.

**Step 7. From pass 3, for each mapped variable in turn** (interest,
dividends and capital gains, in a random order each pass, each using the
others' current filled-in values):

a. **The model's guess.** For every survey household that reported zero,
the model, under the survey's label and seeing everything else on the
record (net worth, wages, marriage, children, the other mapped variables),
gives its chance of holding an amount in each comparison range, and of
holding nothing. It learned these chances from the survey's own
households: reporters (at their reported amounts) and, after pass 3, the
filled-in values of the previous pass. A zero-reporter that looks like
the reporters gets a high chance; one that looks like the true
non-holders gets a low one.

b. **Scale the guesses to the cell table.** Within each cell, the guesses
are adjusted so that, added up over the cell's zero-reporters with their
survey weights, they hit the cell table's targets range by range. Each
range has one multiplier per cell, applied to the odds (so no chance
exceeds 100%), and the multipliers are adjusted back and forth until
every range's total matches, since each household's chances across the
ranges must add to 100%. The order among households is kept: the most
likely holder before scaling is still the most likely after.

Example, one cell, 5 zero-reporters, one question (holder or not), target
35% of zeros:

| Household | Model's guess | After scaling |
|---|---:|---:|
| A | 60% | 74% |
| B | 30% | 44% |
| C | 20% | 32% |
| D | 10% | 17% |
| E | 5% | 9% |
| Expected holders | 1.25 | 1.75 (= 35% of 5) |

c. **Draw.** Each zero-reporter is drawn, by chance, into a range or into
"none", using its scaled chances. A household drawn into a range then gets
a fine slice within that range (the model's 84 slices), drawn from the
model's chances restricted to that range.

**Step 8. Reporters keep their reported amounts** (with the dollar rule
applied, which for now leaves them unchanged).

**Step 9. Train on the filled-in survey** together with the PUF as it is,
in the usual way.

**Step 10. Next pass, redraw** every fill-in from the updated model. The
counts per cell and range stay fixed by the cell table throughout; only
who is filled in changes, and it improves as the model improves.

What comes out of training: a model that, under the survey's label, has
learned net worth given interest (and the rest) from a survey whose
zero-reporters have been separated into true non-holders and hidden
holders.

**What the PUF contributes, and what it does not.** The PUF sets how many
hidden holders there are (the cell table). It does not, in this version,
help choose which households they are: the guess in step 7a is made under
the survey's label and learned from survey records. A version that asks
the PUF-labelled side for the tax-variable part of the guess was discussed
and set aside: the PUF side can only be shown variables that mean the same
in both files (marriage, children, wages), which the cells already use,
and the most useful predictor, dividends, is itself misreported in the
survey until it is filled in.

## 5. After training: the file

**Step 11. Generate.** Draw PUF-style records under the PUF label: every
PUF variable, capital gains in its four pieces, in a random order. Then
switch to the survey's label and the 2016 wave's year, give it the
variables the survey has (married, dependents, wages, interest,
dividends, and capital gains as the sum of the four pieces, since the
survey has one capital gains number), and draw net worth (plan decisions
9 and 16).

On the capital gains sum: plan decision 12 and D25 say a total is never
drawn beside its parts (the first-round experiments showed totals and
parts drawn separately disagree badly). Here the total is not drawn; it
is computed exactly from the drawn pieces and handed to the survey side
as an input. Nothing can disagree. (Decision 12's wording, "a derived
variable is never a model input", is wider than the evidence behind it;
narrowing it is John's call.)

**Step 12. Dollars for the target year,** from the target year's
stretched dollar scale (plan decision 10).

**Step 13. Reweight to 2019:** the 136 IRS targets and the SCF wealth
targets, done twice: with the wealth count targets on the SCF's own
units (143 million; the stopgap of decision 17) and on the file's units
(the SCF's share above each line times the file's units). The first gave a
median of about $38,000 in earlier runs, the second the SCF's median;
the real fix is the non-filer and tax-unit work.

## 6. What the method assumes

1. Marriage, number of dependents and wages mean the same in the survey
   as on the PUF.
2. Reporters' amounts are PUF amounts (for interest, dividends and capital
   gains, after the loss cap).
3. Within a cell, the PUF's distribution is what the survey's would be if
   everyone reported. Where the survey has more than the PUF, the excess
   is a difference in who is in the file or noise, never a false report.
4. Holders do not go missing above the top line.
5. **The one no data can check:** among households alike on everything the
   model sees, the ones that failed to report are like the ones that
   reported, in net worth. (Hidden holders can still differ from reporters
   on average, through the things the model sees; the assumption is about
   what is left after those.) No sensitivity run is planned for now
   (John, 18 October).
6. The pattern of non-reporting in the 2016 wave holds in every wave.
7. The non-filers are the crude stopgap of step 23; the cell comparison
   includes them on the PUF side.

## 7. What the runs leading here showed

- **Steps 22 to 26 (fake surveys).** Filling in the survey's zeros inside
  pretraining, with the model choosing who, removes most of the error that
  taking zeros at face value causes (the error on other income for PUF
  returns with no interest fell from about 18 points to within seed noise).
  Filling in once before training, with a simple model, got about half
  way. Keeping the source label on costs nothing (step 26) and stops the
  survey from blending into the PUF's own relationships.
- **Step 27.** A learned reporting curve plus dollar factor ("family A")
  works on the real SCF; rank-matching ("family B") was dropped. The
  learned dollar factors were not pinned by the data.
- **Step 29.** Three ways of setting how many are hidden: one reporting
  curve, a curve per cell, and per-cell counts (this method's step 3). On
  a fake survey where reporting truly differs by wage group, per-cell
  counts got the number of hidden holders right (18.8% of interest zeros
  against 19.3% true; the curves 15.4% to 15.7%), but its way of choosing
  their sizes was wrong. On the real SCF the reweighting moved least with
  per-cell counts.
- **Step 30 (preview, 18 October).** This method's steps 1 to 4 and one
  fill-in with an already trained model, outside training
  (`minimal/30_PROCESS.md`). Who was filled in, all cells (median net
  worth): reporters $739,000, hidden holders $326,000, remaining zeros
  $26,000; hidden holders also report dividends 26% of the time against
  1.5% for remaining zeros. Three problems, each fixed in the settings
  above:
  1. **Dollars above the PUF.** Filled in, the SCF had 15% more interest
     per household than the PUF at the same cell mix, because its excess
     holders at the top were kept and holders were added below them.
     Mostly a richer survey (assumption 3), which is acceptable for
     learning net worth given interest; part of it was problem 2.
  2. **Noise at the top of thin cells.** In one cell a gap of 0.02% at
     capital gains of $250,000 and over (less than one survey household)
     created about 4,000 weighted households with about $1.2 million of
     gains each. Fixed by the top line.
  3. **Capital losses double counted.** $21.8 billion reported became
     $34.8 billion filled in against $18.9 billion on the PUF, because
     full losses beyond −$3,000 were kept while capped losses were added.
     Fixed by capping survey losses at −$3,000 before comparing.

## 8. The first full run

- Table E27 (step 27): the 2015 PUF with capital gains in four pieces,
  crude non-filers, SCF waves 2013, 2016, 2019, 2022.
- The settings of section 2, read from a settings file.
- Training as section 4, label on, 80 passes, three seeds.
- Generation, dollars and reweighting as section 5, both count bases.
- Compared with: today's rule with the label on (step 27's R0), step 27's
  learned curve (R2J), step 29's per-cell counts (C4).
- Reported: the settings sheet; the cell table with every recorded excess;
  the filled-in survey against the PUF by cell and range; who the hidden
  holders are by cell (net worth, wages, other items) for reporters,
  hidden holders and remaining zeros; each wave's filled-in holder shares
  against the IRS counts for its tax year; the reweighting's move and
  records kept; net worth before and after reweighting (total, median, top
  1% share, by wage group, for units with none of the three items). The
  filled-in survey is saved (five draws per record) for inspection.

## 9. Not yet done, and where it goes next

- Capital gains on the survey side is one number; splitting it into the
  PUF's pieces during the fill-in is possible and not needed for net
  worth now.
- Business income and other variables where reporters' dollars are not
  the PUF's: the dollar rule will be set per variable, from the top of
  each cell and outside evidence (`background/linked_survey_tax_evidence.md`).
- The CPS as a second survey: its own settings sheet.
- The real non-filer and tax-unit work, which removes the different-people
  part that the cells now record.

## Revision history

- 2026-10-18: written from the discussion with John of 17 and 18 October.
- 2026-10-18: first full run done as section 8 sets out (step 31):
  results in `minimal/31_RESULTS.md`, the review of this note and the
  discussion while it ran in `minimal/31_DISCUSSION_2026-10-18.md`. The
  review found that assumption 5 understates what the scaling step assumes:
  within a cell and size range, whether a holder reports depends on
  nothing else on the record, including whether it reported the other
  items. The run's combinations check puts the cost of the last part at
  about 3 points on the share holding none of the three items. Text of the
  note not changed.
