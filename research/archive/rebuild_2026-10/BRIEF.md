---
title: "Tax-Data rebuild — shared brief for the background research"
role: notes
workstream: rebuild
status: current
updated: 2026-10-14
sot: self
supersedes: []
superseded_by: null
---

# Shared brief for the background research

*Note added 2026-10-14.* The design built on this brief is in
`research/rebuild/design/` (`plan.md`, then `idea_space.md`). The brief
stays as the problem statement; "the framing under consideration" below
is the 6 October version.

Every background research agent reads this first. It states the problem as
John Ricco (Deputy Director of Policy Analysis, Yale Budget Lab; the lead on
this repository) and Claude agreed it on 2026-10-06, the decisions already
made, and the rules for the written output.

## What Tax-Data is

Tax-Data produces the tax-unit microdata that the Budget Lab's tax
microsimulation model, Tax-Simulator, reads. Tax-Simulator is the only
consumer. It computes tax liability under current law and policy reforms for
every year in the budget window and beyond (output runs 2017–2097). Tax-Data
does not compute taxes; it supplies the population and its incomes,
deductions, demographics and other characteristics.

John built Tax-Data and Tax-Simulator in 2023 with established, conventional
techniques. A GPU port of Tax-Simulator already exists (tax-simulator-gpu).
He now has two weeks to rethink Tax-Data, and is open to a wholesale
overhaul, including a port from R to JAX.

## The problem, stated abstractly

We want a realistic joint distribution over a vector of variables X, for
every tax unit in the population, for every year t ≥ 2015, expressed as
discrete microdata (records with weights).

What we have and do not have:

1. **Missing variables.** For tax filers in 2015 we observe part of X (call
   it X′) in the IRS Statistics of Income 2015 Public Use File (PUF). Every
   other variable must be imputed.
2. **Missing people.** Non-filers are not in the PUF. Their entire X must be
   built. (Today this comes from a CPS ASEC–based non-filer pool; see
   `research/state_weights/`.)
3. **Thin information after 2015.** For 2016–2023 there are only aggregates
   and coarse cross-tabulations (for example, AGI bracket by marital status
   from SOI Publication 1304), not microdata. **The 2015 PUF is still the
   newest IRS microdata available to us. This is the single largest
   constraint.**
4. **Censoring by tax law.** Some published tables reflect what the law makes
   visible rather than the underlying quantity. After the 2017 tax law (TCJA)
   roughly doubled the standard deduction, far fewer people itemize, so
   tabulated itemized deductions after 2017 show a censored slice of the
   underlying distribution of deductible expenses.
5. **No IRS information at all for some variables** (wealth, consumption,
   tips, overtime, and others). These come from BEA, the Federal Reserve's
   Financial Accounts and Distributional Financial Accounts, and household
   surveys (SCF, CEX, CPS, SIPP).
6. **No data at all after the last observed year.** The method must also
   project to 2097, using projections such as CBO's economic and demographic
   forecasts and SSA's.

Also: the 2015 PUF is itself disclosure-protected (blurred amounts, altered
top values, aggregate records at the very top), so even 2015 is estimated,
not observed.

## The framing under consideration

The working idea is a single estimation problem in the spirit of Bayesian
updating. A **prior** joint distribution (the 2015 PUF, with imputations from
survey microdata, aged forward) is combined with many **targets**, each a
noisy measurement with a stated precision: near-exact IRS counts, rougher
BEA or Financial Accounts totals, very rough long-run projections. The
answer is the microdata that stays as close to the prior as possible while
fitting the targets in proportion to their precision. Accounting identities
(wages of primary plus spouse equal total wages, nonnegativity, the
population partition) hold exactly; everything else is a soft target.

Consequences John liked:

- **Projection is not a separate method.** Future years are years with few,
  imprecise targets, so the prior carries more weight.
- **Imputation supplies the prior for missing variables.** A model trained on
  a survey gives the starting conditional distribution; targets adjust it.
- **Censored tables are matched by simulating the reporting process.** For
  itemized deductions in 2019, apply 2019 tax law to each record's underlying
  deductible expenses, determine who would itemize, and compare that total
  against SOI's 2019 figure. (John did a version of this at the Penn Wharton
  Budget Model.)

## Decisions already made

- A single best estimate is the goal. A full distribution over possible
  microdata files is not required.
- The method may change anything: weights, values on records, or the set of
  records itself (generating new records).
- The same records do not have to persist across years. Separate files per
  year are acceptable.
- Hard constraints and soft targets must be separated explicitly.
- A port to JAX is acceptable. GPUs (Yale's Bouchet cluster) are available on
  demand.
- No one expects the method to create information it does not have.

## The current pipeline, briefly

Read the code for detail; `src/main.R` is the entry point.

1. Read the 2015 PUF (`src/process_puf.R`).
2. Scale weights to 2017 by cell (married × senior × AGI bracket) to match
   Pub 1304 Table 1.6 counts (`src/create_2017_puf.R`).
3. Solve a linear program for weight adjustment factors against 188 SOI
   targets for 2017 (`src/reweight.R`, `config/target_info/baseline.csv`).
   The objective minimizes the sum of factors, with each factor bounded in
   [0.34, 1.66]. **Checked 2026-10-06:** in the cached solution, 62% of the
   207,692 records sit at 0.34 and 38% at 1.66; only 114 lie in between. This
   is the corner-solution behaviour of linear programs, and it is a known
   weakness of the current method.
4. Rescale dollar amounts within AGI bins to match SOI totals.
5. Attach non-filers (`src/impute_nonfilers.R`).
6. Impute about fifteen variables, one module each (`src/imputations/`),
   using quantile regression forests, distributional random forests and
   similar, each trained on a survey (CEX for consumption, SCF for wealth,
   SIPP for tips, and so on).
7. Project forward (`src/project_puf.R`): weights grow by demographic cell
   factors (filing status × age group); each dollar variable grows by a
   single aggregate factor for every record (the `grow_with` column of
   `config/variable_guide/baseline.csv`), from CBO and SOI series near-term
   and Macro-Projections long-term. So, apart from wealth, the shape of each
   income distribution is fixed at its 2015 shape and changes only through
   weights.

The full list of output variables is `config/variable_guide/baseline.csv`
(about 190 rows; columns: variable, description, source, PUF name,
`grow_with`, base year, imputation module). Current external inputs are
named in `config/runscripts/baseline.yaml` (upstream interfaces: IRS-PUF,
ASEC-Nonfilers, DINA, Compiled-SOI-Tables, Macro-Projections, CPS-ASEC, SCF,
SIPP) and in `resources/`.

The last full run took about two hours on 16 cores and peaked at 141 GB of
memory.

## Rules for the written output

**Writing style.** Write for a public finance economist who knows taxes
well and has a solid grasp of software architecture, but is not a career
software engineer and is not a trained statistician. Plain, direct English.
Say the economic meaning in full sentences. No invented shorthand or
acronyms without definition. If a statistical or machine-learning term is
needed, define it in one plain sentence the first time it appears. Describe;
do not argue or sell. No dramatic or reveal-shaped sentences, no metaphor for
a literal thing. Headers are plain descriptive nouns. Write less rather than
more, but do not leave out facts.

**Sources.** Give a URL for every source you cite. Distinguish what you
verified (you opened the page or document and saw it) from what you believe
but could not verify. Say plainly what you looked for and could not find.
Do not invent publication numbers, table numbers, years of coverage or
release dates; if unsure, say so.

**Scope.** Research and write only. Do not edit any file in the repository
other than your own output file. Do not run the pipeline. Any computation on
this cluster goes through Slurm (`sbatch` or `srun`), never the login node;
you should rarely need any.

**Format.** One Markdown file at the path your prompt gives, with the front
matter block from `research/CONVENTIONS.md` (`role: evidence`,
`workstream: rebuild`, `status: current`, `updated: 2026-10-07`,
`sot: research/rebuild/BRIEF.md`). Start with a summary of at most one
screen: the ten or so things John most needs to know. Then the detail.
