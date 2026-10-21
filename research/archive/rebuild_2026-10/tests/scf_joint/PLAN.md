---
title: "Test 2 — one model across the PUF and the SCF: plan"
role: plan
workstream: rebuild
status: current
updated: 2026-10-09
sot: research/rebuild/design/plan.md
supersedes: []
superseded_by: null
---

# Test 2: one model across the PUF and the SCF

Context: D22 in `research/archive/design_decisions_2026-10-14_superseded.md` (one transformer
model trained on all sources as the reference distribution), and test 1 in
`research/rebuild/tests/puf_generator/` (`EXPERIMENTS.md` has the summary).
Test 1 found that the model's predictions are close to calibrated when it
can see most of a record, and that the errors come from generating whole
records from nothing. Test 2 uses the model the way D22 intends:
completing records given what they show.

## The question

Does putting two surveys that observe different variables into one model
work? Specifically: trained on the PUF (incomes, no wealth) and the SCF
(incomes and wealth) together, with each source's unobserved variables
hidden, can the model complete records from one source with variables only
the other observes, as well as or better than today's methods?

The head-to-head with the current wealth imputation is secondary (John,
9 October).

## Decisions (John, 9 October)

| Question | Decision |
|---|---|
| Main aim | Show that combining sources works; beating `wealth.R` is secondary. |
| How to check | Both: a pretend-sources test inside the SCF, where the truth is known, and the real PUF + SCF run, checked on aggregates. |
| Units across sources and years | Ranks: each income and wealth amount becomes its weighted rank within its own source and year, with exact zero as its own token. Dollars come back through each year's marginal distributions. |
| Data | The 2015 PUF and the 2013 and 2016 SCF waves. |
| Non-filers | Left out for this test: SCF tax units restricted to filers, so both sides cover filers. A 2015 non-filer pool is a later step. |
| Wealth variables | All 24 that the production imputation produces: 14 assets, 6 debts, 4 unrealized gains (`src/imputations/wealth_schema.R`). |
| Split-off SCF adults (own tax units, wealth never asked; 13% of SCF filer units) | Kept, with wealth marked unobserved rather than zero; not graded (John, 9 October). |
| Comparators | The current production method (per-income-cell distributional forest) refit on the same split, and a sequential decision-tree imputation of the same 24 variables. |

## Data preparation

1. **SCF tax units for 2013 and 2016.** Run `build_scf_tax_units()`
   (`src/imputations/stage1_scf_tax_units.R`, a port of the Federal
   Reserve's SCF-to-TAXSIM code) with `syear = 2013` and `2016`. It has been
   run only on 2019 and 2022. Check each wave: unit counts, weighted
   totals of income and net worth against the SCF summary file (SCFP), and
   the share of households split into more than one unit.
2. **Filers only.** Keep SCF tax units that filed (question X5744, codes 1
   and 6, as the builder already defines). To check: whether units split
   off a household (the "NPEU" units) carry their own filing answer; if not,
   state the rule used.
3. **Implicates.** The SCF publishes five imputed versions of each
   household. Use all five, each at one-fifth of the weight.
4. **Shared variables between PUF and SCF.** Age of the older spouse (and
   the PUF's age range), married or not, number of dependents, and seven
   income components: wages; business and farm; interest and dividends;
   capital gains; rent; Social Security and pensions; unemployment and
   other. A crosswalk maps PUF lines onto the seven SCF categories; it is
   written down and checked against totals.
5. **Ranks.** Each amount becomes its weighted rank among nonzero values
   within its source and year (filers only on both sides), with tokens for
   exact zero and for negative values. Finer rank bins at the top.
6. **Year as a number.** The model takes the year as a number, through a
   learned function of it, not as a label, so that 2015 lies between the
   2013 and 2016 waves.
7. **Splits.** SCF: hold out a set of households (all their tax units and
   implicates together, so no household is in both training and test).
   PUF: as in test 1.

## The model

As in test 1: masked transformer, any-order hiding, base size (about 8.6
million parameters). New: a source token (PUF or SCF), the numeric year,
and variables a source does not observe are always hidden for its records
and never scored. Several training seeds per variant (test 1 found that
retraining alone moves results as much as most changes).

Completion: for a record, keep every observed value and draw the missing
variables one at a time, each given everything observed plus what has been
drawn. Several completions per record.

## Experiments

**A. Pretend sources (truth known).** SCF 2016 filers, split by household
into two halves. Half 1 plays the PUF: its wealth is hidden. Half 2 shows
everything. Train on both; complete half 1's wealth; compare with its
true wealth. Variants:

1. Half 1 measures income exactly as half 2 does (the clean case).
2. Half 1's incomes are measured differently. Ranks absorb any difference
   that keeps the order of records (scaling, rounding), so the variants
   test what ranks cannot absorb:
   - random reporting noise;
   - income moved between categories (some business income reported as
     wages);
   - some capital income not reported (set to zero).
3. Benchmark: a model trained only on half 2, then asked to complete half
   1 (the same task without the second source in training).

**B. Real PUF + SCF.**

1. Train on PUF 2015 filers and SCF 2013 and 2016 filers.
2. Compare with a model trained on the SCF waves alone: complete the
   held-out SCF units with each, and compare with their true wealth.
3. Complete PUF records with wealth at year 2015, and compare aggregates
   with the SCF: holders and amounts of each component by income rank
   group and age, and net worth shares at the top, against SCF 2013 and
   2016 and their interpolation.

**C. Comparators**, on the same held-out SCF units: the production method
refit on the same training units, and sequential decision trees.

## Evaluation (held-out SCF units, truth known)

- Each wealth component: share holding, and quantiles of the amount, by
  income rank group and age group.
- Joint patterns across components: which assets and debts appear
  together (for example, homeownership among stockholders), and
  correlations of amounts.
- Net worth: distribution, shares at the top, by income group.
- Cloning at the top: how often the same donor balance sheet repeats.
- A classifier test of completed records against true records.
- Noise yardsticks: two halves of the true held-out units against each
  other (sampling noise); several completions and several training seeds
  (model noise).

All in rank space and, through the SCF's own marginals for that wave, in
dollars.

## Running it

Subagents in parallel, as in test 1: data preparation (SCF waves, crosswalk,
ranks), model (source and year inputs, completion), evaluation, comparators.
Code in `research/rebuild/tests/scf_joint/`; data and outputs in
`/nfs/roberts/scratch/pi_nrs36/jar335/scf_joint/`. SCF and PUF data stay on
the cluster. All Python and R through Slurm.

## Open items

- Whether split-off SCF tax units have their own filing answer.
- The exact PUF-to-SCF income crosswalk (proposed in data preparation,
  reviewed by John).
- How the production comparator is refit outside the pipeline (it expects
  a PUF at 2022 values; for the SCF-only comparison it needs only the SCF
  side).
- The 2015 non-filer pool, for the full version.
