---
title: "The tilt: next steps and how to choose targets"
role: design
workstream: rebuild
status: draft (open questions for John)
updated: 2026-10-19
sot: research/rebuild/design/plan.md
---

# The tilt: next steps and how to choose targets

Notes from 19 October. Nothing here is decided. The evidence behind them is
in `minimal/36_RESULTS.md` (business targets), `minimal/34_RESULTS.md`
(why the pretrained file misses), and `$R/step37/real_vs_gen.md` (the real
PUF tilted to the same targets).

## 1. What the tilt does now

Each record's new weight is its starting weight times exp(a sum of
multipliers times the record's contributions to the targets). This is the
smallest change from the starting weights, measured by Kullback–Leibler
divergence, that meets the targets (`minimal/05_tilt.py`).

**The start** (`minimal/07_baseline.py`):
- Dollars are grown from 2015 to 2019 per return holding each item: the
  SOI amount per return with the item, 2019 over 2015. Growth in how many
  hold an item is left to the tilt, so it is not counted twice.
- Filers' weights are scaled to SOI's 2019 counts of joint and non-joint
  returns. This has been the default since 19 October; it matters little
  for 2019 but grows with the distance to the target year.
- Non-filers' weights are scaled to the 2019 non-filer pool.

**The targets of the latest run** (`minimal/LATEST.md`):

| Group | What | Population | Form |
|---|---|---|---|
| 136 IRS (SOI 2019 Tables 1.4, 1.2) | in 15 AGI brackets: returns; returns with and amount of wages, taxable interest, ordinary dividends, Schedule D net capital gain; plus joint returns | filers | exact |
| 140 business (SOI 2019 Table 1.4) | six families (Schedule C, partnership + S corp, rent and royalty, estate and trust, farm, farm rental): returns and dollars with net income and with net loss, six AGI groups, merged where SOI combined cells | filers | exact; 2% if exact fails |
| 1 non-filer count | the 2019 CPS-based non-filer pool | non-filers | exact |
| 8 wealth (SCF 2019) | units and net worth above the SCF's 99th, 90th and 50th percentile lines; total net worth; units above $1m | everyone | 2% |

**Wealth counts.** Since 19 October (plan decision 17, John) they are on
the file's units: the SCF's share above each line times the file's 187
million units (IRS returns plus non-filers). Before, they were the SCF's
own counts on its 143 million units. That pushed the file's 44 million
extra units below every line and gave a median of about $39,000 against
the SCF's $91,000. Dollar amounts are always the SCF's own.

## 2. A tolerance as a fallback

Two kinds are possible:

- **Soft targets, in the code now.** The tilt pays a penalty that grows
  with the square of the miss over a tolerance (`rel_tol` in
  `05_tilt.tilt`). The wealth targets use 2%; the business targets fall
  back to 2% when the exact tilt fails. Any miss costs something, so a
  small miss is traded against a large weight change.
- **Bands, not built.** Anywhere within ±x% of the target costs nothing;
  outside is not allowed. This is closer to treating a published number
  with its sampling error. It is the calibration with range restrictions
  of Deville and Särndal (cited from memory). In the solver, each target's
  multiplier is held at zero while the target is inside its band and
  freed when it is not. About a day's work and a test.

## 3. When targets cannot be met

A set of exact targets can be met only if positive weights on the records
at hand can produce those totals: the targets must lie inside the range of
what the records can add up to. That fails in three ways.

1. **The targets contradict each other.** Example: on 16 October the
   count above $1 million came from the SCF's base and the percentile
   counts from the IRS base. Only softening or fixing the source helps.
2. **The file lacks the records.** If no record has a large estate loss at
   AGI over $1 million, no weights can produce SOI's $2 billion there. The
   no-dropout model D0s2 came close to this: its losses in that cell are
   3% of SOI's dollars, and meeting the cell needs extreme weights. D0 and
   D0s3 meet it.
3. **Too many targets for the records.** As the number of targets
   approaches the number of distinct kinds of record, the weights are
   pinned down and then impossible. Far off today: about 285 targets on
   400,000 records.

The number of targets is not the cost in itself. A target the file
already meets costs nothing. The cost is how far the file is from the
targets, and it shows in the effective sample and the largest weight
factors.

On 19 October the real PUF, grown to 2019, cost as much to tilt to the 136
plus business targets as the generated files did: 97.4% of the effective
sample kept, against 96.4–97.1% (filers, no wealth targets). So most of the
tilt's move is real change between 2015 and 2019, not model error.

## 4. Choosing targets in a principled way

Published tables are finite. The number of possible targets explodes only
if cross-tabulations are constructed. A rule could rest on four parts:

1. **A tolerance from each target's own precision.** SOI publishes
   coefficients of variation for its cells.
   - Precisely measured totals, such as returns by AGI, stay exact.
   - Thin cells get a tolerance equal to their own sampling error, so a
     cell SOI itself knows poorly cannot force extreme weights.

   This is ridge, or penalized, calibration in the survey literature
   (Bardsley and Chambers 1984; Rao and Singh 1997; cited from memory).
2. **Priority to what drives results downstream:** the items and
   population groups that matter most for tax liability and its
   distribution in Tax-Simulator.
3. **Held-out targets to choose the level of detail.** Tilt to some
   published cells and check the held-out ones. Add detail, such as more
   AGI groups or more families, while the held-out cells improve; stop
   when they do not. This turns "how many targets" into a test.
4. **The cost and the untargeted checks.** Watch the effective sample, the
   extreme weight factors, and statistics no target pins down. Examples
   for business income: units holding both partnership and S corporation
   items, the passive share, nonpassive losses.

**Targets where nothing is published** (projection years, splits SOI does
not publish):
- On 19 October, targets built from the 2015 PUF and checked against SOI
  2019 were within about 5% for large income items and off by 15–50% for
  losses and small families. Reweighting the 2015 PUF to the 136 core
  targets did no better than naive growth (`minimal/36_RESULTS.md`).
- So such targets should be soft, with a tolerance near their known
  construction error, or left untargeted and checked as diagnostics.
- For projection years the inputs are aggregates: macro projections of
  income items, and demographic projections of returns by filing status
  and, later, age. Dollars then grow per return, since no projection gives
  dollars per holder, and weights grow with the demographic projections.

## 5. Next steps (none started)

1. **Settle the business-target rule** (`minimal/36_RESULTS.md`):
   - six AGI groups, or another level of detail;
   - thin cells exact, soft, or in total only;
   - soft built targets, or none.
2. **Build bands** (section 2) and compare them with soft targets on the
   latest run.
3. **Tolerances from SOI's coefficients of variation:** read them for
   Tables 1.2 and 1.4 and set each cell's tolerance from them.
4. **A held-out-target test** of the level of detail: for example, tilt
   to three AGI groups and check the six, then to six and check the
   fifteen.
5. **Age by filing status in the pre-scaling,** once the records carry
   age.
6. **The real PUF with model-drawn net worth,** tilted to the same targets
   as the latest run (`$R/step37/real_vs_gen.md`, John's proposal). It
   changes the rebuild's premise; John's decision.
