---
title: "Tax-Data rebuild: where we are and how we got here"
role: index
workstream: rebuild
status: current
updated: 2026-10-15
sot: research/rebuild/design/plan.md
supersedes: []
superseded_by: null
---

# The Tax-Data rebuild: where we are and how we got here

Start here. This is the one document that says what the rebuild is, what
has been done, which direction we took and why, and where the details are.
Written 15 October 2026 as a reset of the paper trail. Everything under
`research/rebuild/` is self-contained; nothing in it is used by the
production pipeline yet.

## What we are trying to build

One probability model of tax units. It is trained on the PUF and on other
surveys stacked together, and it can generate whole tax-unit records from
scratch: every PUF tax variable plus variables the PUF does not have, such
as net worth from the SCF. For any year, the model is then post-trained,
meaning shifted by the smallest amount that makes its records hit that
year's targets (IRS counts and amounts by income bracket, SCF wealth
totals). How far it has to move is the measure of whether the pretrained
model was good enough.

The reason to want this instead of the current pipeline: the current
pipeline is a chain of separate imputations, each hand-built for one
variable, and it ages a 2017 file forward year by year. One model with
one post-training step would replace the chain, carry every variable's
relationships at once, and produce any year's file the same way.

## The path so far

**6 to 7 October: background research.** The problem was stated
(`BRIEF.md`) and twelve background reports plus seven reviews were written
on the options (`background/`).

**8 to 14 October: the first round of experiments.** A transformer that
fills in hidden variables of a record from the rest, tested on the PUF
alone, then on PUF plus SCF, then with post-training by tilting, then a
set of probes on draw order, weighting, overfitting and the top. The main
lessons were about data, not the model: compute totals from parts, train
on the sample as drawn so the top is not starved, translate surveys into
tax concepts before training, score on held-out records. Sequential trees
tied or beat the transformer on PUF-only generation. Record:
`joint_model_history.md`; code and reports under `tests/`.

**14 October: the design rewritten from the top.** The discussion that day
produced `design/plan.md` (what is decided, what is open, the order of
work) and `design/idea_space.md` (every alternative considered and why).
The earlier plan and decisions log were archived to `research/archive/`.
The decision that mattered most: stop tuning the large experiments and
build a minimal example that can be read end to end.

**14 to 15 October: the minimal example.** Seven variables (married,
number of dependents, wages, taxable interest, dividends, capital gains,
plus other income owned by the PUF and net worth owned by the SCF), the
2015 PUF and the 2016 SCF wave, a small transformer, generation, a
rank-to-dollar map, and a tilt to 136 IRS targets for 2019 plus SCF
wealth targets. Everything is in `minimal/`, one numbered report per step.

What the minimal example showed:

- Income works. The tilt to the 136 IRS targets moves the model by about
  as much as it moves the real PUF, and relationships no target covers
  barely change. Oversampling the top is exact. Draw order does not
  matter beyond noise.
- Wealth was wrong by a factor of two on PUF-style records. That turned
  out to be the central problem of the whole approach, and the rest of
  the 15th went into it.

## The direction we took on the wealth problem, and why

The cause: the SCF and the PUF measure the same variables differently. The
SCF leaves out small holders of interest, dividends and capital gains (87%
of SCF units report zero interest; 72% of PUF units). The dollar amounts
of those who do report match the PUF's, and so do the totals. When each
file's amounts were turned into positions on its own scale, an SCF
household with $2,600 of interest sat in the same position as a PUF
household with $590, and the model gave the richer household's wealth to
the poorer one. Production's wealth imputation had the same problem
(forest 1.3 times the SCF before its fixes) and patched it with a $100
threshold and the calibration step.

We ran a grid of fifteen fixes (`minimal/09_BRIDGE_EXPERIMENTS.md`,
results in `09_RESULTS.md`). Everything tried inside the model failed.
What worked was in the data: map the SCF's dollars onto the PUF's scale,
so that a position means the same dollars in both files, and let the
model learn through the source label that the SCF's zero bin is fatter.
Wealth went from 1.7 times the SCF to 0.98, with the right shape and the
holder shares intact. It held across the 2018 and 2021 SCF waves
(`10_RESULTS_YEARS.md`).

A second round tested the rule against distortions a different survey
might have: scaled dollars, a different definition, rounding, top-coding
(`10_RESULTS_A.md`). Scaled dollars need a correction before mapping; a
definition mismatch cannot be fixed by any mapping and the tilt cannot
see it. The CPS was scoped as a real third survey (`10_CPS_SCOPING.md`).

The theory was then written down: the exhaustive list of reasons two
files disagree on a shared variable and the fix for each
(`11_SHARED_X_THEORY.md`, merged from two independent write-ups), and the
precise statement of what each mapping rule assumes, in the language of
statistical matching, test equating, measurement error and missing data
(`12_CROSSWALK_THEORY.md`). The short version: the rule we adopted assumes
a reported dollar is a true dollar and says nothing about who is in the
survey; John's alternative, ranking over the whole distribution including
zeros, assumes the survey is the same population as the filers and says
nothing about the dollars. Both share one assumption that cannot be
tested without linked records: that a household that holds an amount and
reports zero has the same wealth as one that reports it. The alternative
was run next to the adopted rule (`13_RESULTS_B.md`): it fails, because
the SCF is not the same population as the filers. The rule as adopted is
written out case by case in `minimal/14_TRANSLATION_RULE.md`.

## The stopping point

Stated by John on 15 October: a pretrained model that works decently, and
a post-training for 2019 that hits its targets precisely and does not
move untargeted joint distributions much. The prerequisite is settling the
translation step above. Then the minimal example is a viable proof of
concept, and features are added to it: nonfilers, the CPS, more variables,
more years, network size, tax law in the loop.

**Reached 15 October (evening), by John's call.** The v4 baseline with
net worth drawn under the SCF label and other income left out (v4b,
`minimal/15_BASELINE_V4.md`) is the proof of concept: wealth on PUF-style
records within a few percent of the SCF before the tilt, the 136 IRS
targets met exactly, the tilt moving the model by 0.024 nats. Known gaps
at the stopping point: returns with no wages hold too much wealth, the
middle of the wage distribution is a little too rich, the top 0.1% a
little too thin, single parents too few. The next step is nonfilers: the
PUF holds tax units selected on having enough income to file, and the
SCF holds everyone, so part of these gaps is two populations being
compared as one. The latest run since then is v4c (16 October,
`minimal/LATEST.md`): every PUF variable drawn before net worth, which
fixes the single-parent shortfall, and one base for the wealth counts;
move 0.018 nats.

## Decided on 15 October, evening

All in `design/plan.md`; the reasoning is in
`minimal/17_DISCUSSION_2026-10-15.md`.

- **Decision 15, the translation rule.** Get each survey's dollars onto the
  PUF's terms, then place every record on the PUF's dollar scale for that
  year, source label kept. Ranks including zeros were run and rejected
  (`minimal/13_RESULTS_B.md`).
- **Decision 16, borrowed variables.** Net worth on a PUF record is drawn
  under the SCF label, with other income left out, so the model is asked
  only kinds of questions it saw in training.
- **Decision 17, targets in dollars.** A target stated by rank is converted
  to dollar lines when the target sheet is built; the tilt solves once.
- **Open: a model-side answer to translation.** The rule is good enough for
  now; John wants a mechanism inside the transformer as well
  (`minimal/16_MODEL_SIDE_FIXES.md`).
- **Deferred to "Future refinements":** growing the PUF's dollar scale by
  bracket rather than by one factor per item.

## What comes next

How to work with John on this: plain, normal English with tables and real
numbers; nothing launched and no plan rule changed without his explicit go;
he decides commits; heavy compute through Slurm.

1. **Nonfilers, starting with a discussion with John.** He wants to talk
   it through before anything is built. The questions to bring: whether the
   SCF units that would not file are dropped from training, flagged with a
   filing variable, or kept; whether the filing model of the CPS nonfiler
   pool in this repository (`src/nonfilers/filing_model.R`,
   `research/state_weights/`) can be applied to SCF units, which lack
   Social Security, pension, business and rental income in the current
   table builder; whether the 2015 nonfiler pool enters pretraining as rows
   (plan decision 7); and what the wealth targets cover. Evidence so far:
   `minimal/15_BASELINE_V4.md`, section 3 (the threshold rule leaves 91
   million SCF units against 158 million returns), and
   `minimal/13_RESULTS_B.md` (30% of SCF units have no wages against 17%
   of returns). The check that the work is done (John, 2026-10-16): the
   SCF's weighted count of tax units should equal the IRS return count
   (now 143 million against 158 million); a gap not explained by years
   means the unit construction is not right yet.
2. **Done on the evening of 15 October** (reports in `minimal/`):
   post-training as a network, by distillation (`18_DISTILL.md`) and by
   adjusting the parameters directly (`19_DIRECT.md`): both work partly,
   both move more than the tilt, the tilt stays for a single year;
   separate models against the joint model (`20_SEPARATE_MODELS.md`): the
   joint model adds nothing for the PUF variables and is the better donor
   for net worth, and drawing every PUF variable before net worth fixes
   most of the dependents shortfall. The two follow-ups are done in v4c
   (16 October, `minimal/LATEST.md`): John changed the draw order
   (decision 9) and put every wealth count target on one base (decision
   17), which removes the two targets that contradicted each other
   (`19_DIRECT.md`). One choice remains for John: whether that base is the
   IRS return count (the default) or the SCF's own units; v4c is tilted
   both ways.
3. **To discuss with John:** the true-amount model, with a worked
   example in `minimal/17_DISCUSSION_2026-10-15.md`, section 9: whether to
   build it after the nonfiler work, and what could check its assumption.
4. **Not started:** confidence intervals by bootstrap replicates of the
   training data; choosing the year mix by leaving out a wave; training
   noise replicates; E15, the model with a true amount inside it.

## Where the details are

| What | Where |
|---|---|
| **The latest run, every piece and where it is** | **`minimal/LATEST.md`** |
| The design: decided, open, order of work | `design/plan.md`, reasoning in `design/idea_space.md` |
| The problem statement of 6 October | `BRIEF.md` |
| Background reports and reviews | `background/` |
| First-round experiments, 8 to 14 October | `joint_model_history.md` (record only; its closing advice is superseded), code and reports in `tests/` |
| The minimal example, step by step | `minimal/01_REPORT.md` to `06_WEALTH_TABLES.md` |
| The wealth problem, stated abstractly | `minimal/08_STATE_2026-10-14.md` (end-of-day memo, historical; its "fix chosen" section is superseded by the grid) |
| The grid and results | `minimal/09_BRIDGE_EXPERIMENTS.md`, `09_RESULTS.md` |
| Distortions, years, CPS | `minimal/10_*.md` |
| Theory | `minimal/11_SHARED_X_THEORY.md`, `12_CROSSWALK_THEORY.md` |
| Ranks-with-zeros next to the adopted rule | `minimal/13_RESULTS_B.md` |
| The translation rule, stated plainly, case by case | `minimal/14_TRANSLATION_RULE.md` |
| The baseline: v4, v4b and v4c side by side | `minimal/15_BASELINE_V4.md` |
| Why the in-model fixes E7 to E9 failed, and the true-amount model | `minimal/16_MODEL_SIDE_FIXES.md` |
| The evening discussion: the prior, borrowed variables, the tilt, intervals, targets | `minimal/17_DISCUSSION_2026-10-15.md` |
| Post-training as a network | `minimal/18_DISTILL.md`, `minimal/19_DIRECT.md` |
| Does the joint model beat separate models for each source? | `minimal/20_SEPARATE_MODELS.md` |
| Data, models, generated files, scores | `/nfs/roberts/scratch/pi_nrs36/jar335/minimal/` (not in git) |
| Superseded documents, including v3 baseline and the two theory drafts | `research/archive/` |

Nothing from 14 or 15 October is committed yet.
