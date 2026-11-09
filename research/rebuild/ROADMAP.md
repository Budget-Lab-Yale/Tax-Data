# Goal and open work

## The goal

Tax-Data should be a general method for building a synthetic tax file for any year, historical or projected, in
which every year's file has the same variables, means the same things and is produced the same way. The production
pipeline (`src/main.R`) mixes variables taken from different historical years, ages each period by its own method, and
models similar imputation problems in different ways.

The rebuild borrows the structure of large language models:

- **Pretraining** learns one conditional probability model over tax units from microdata of many sources and years
  (the PUF, the SCF, the CEX, the CPS, the SIPP). It learns how variables go together within each source and year,
  including what the PUF lacks, such as net worth, tips and consumption.
- **Post-training** shapes it for a target year t: choose which mix of source years year t draws on, then reweight
  records drawn from the model so they meet year t's published totals, at whatever detail exists, including totals of
  computed taxes, so a tax calculator sits inside the reweighting.

The output for year t is a PUF-style file, with the non-tax variables, ready for Tax-Simulator.

Pretraining can learn how, for example, net worth goes with income only if the variables the sources share (wages,
interest, dividends, business income, marital status, dependents) mean the same thing in each source: the same
population, the same definition and the same measurement. Surveys miss small holders, misreport losses and cover
non-filers the PUF never sees. The translation step (`FEATURES.md`, section 1) lines the shared variables up with two
standard functions and one table.

What done looks like:

- One pipeline: train once, then post-train each year from a short specification (the year, its source mix, its
  targets).
- Every variable Tax-Simulator needs, with the same definitions in every year.
- Checked: targets are met, relationships no target covers stay close to their sources, and a standing set of
  benchmarks says for each variable whether the file is good enough.
- It replaces `src/main.R` as the source of Tax-Simulator's input.

## Where it stands

The pipeline builds a file for every year 2015-2075 with state weights, the Tax-Simulator file for each, and, on the
real PUF, a package for PUF holders. Targets are formal (one folder per year, each target with a standard error, one
cost in one solve), translation is built for every survey, each target year draws on its own mix of survey years,
taxes are computed inside the reweighting, and the projection years follow CBO and Macro-Projections. `FEATURES.md`
describes all of it; its last section lists the known limitations.

## Open work

**Replacing production.**
- Make the rebuild's files Tax-Simulator's input in place of `src/main.R`.
- Inheritances: the Tax-Simulator file does not yet carry the drawn item. The plan is for the simulator to read it
  and scale heir dollars to its bequests, since production's heir file is keyed by record ids the rebuild does not
  share; until then Tax-Simulator's heir data is turned off for the rebuild's files. Open with it: the top of the
  distribution (the SCF and the file have almost no heirs above $18 million), and the 2013 and 2022 waves' year
  weights.
- Move the 2025 SCF files to the shared raw-data folder and drop the setting `paths.raw.scf_wave_dirs`.

**Method.**
- Translation: the fill-in can add holders a survey misses but not remove holders it has too many of; its PUF side
  reads the 2015 PUF rather than the tax data of each survey year.
- The survey-year shares are written in each year's `prior.yaml` by hand rather than computed from a rule.
- State and local taxes are not imputed.
- The self-employment tax target is met by moving weight to records with large partnership income subject to
  self-employment tax: keep it, target the item differently, or fix the draw's split of the tax among Schedules C, F
  and partnership income.
- Records grown past $1 billion after the last Forbes list sit beside the grown list (2035: about 1,250 records, $3.6
  trillion): redraw them, scale them below $1 billion, or leave them.
- The child credit moves in steps of up to 2.5% a year past 2035; cause not checked.
- The series: the 2020-2021 files hold about 1,500 records above weight 5,000; non-filers jump in 2023 and miss their
  target in even years.
- The known flaws in the draw (`FEATURES.md`, section 8): gains too flat by age, business wealth on low-income records,
  too few units aged 80 and over.

**The real-PUF setting.**
- Real-PUF 2021 does not converge within 150 Newton steps, so the real-PUF years after it cannot chain from it.
- In every real-PUF year the solve sets most copies of the PUF's four aggregate records to a weight near zero, and
  Table 4.3's top 0.001% misses. Splitting the aggregate records, or changing how Table 4.3's top cells treat them, is
  open.

**Each year, when SOI publishes.**
- When SOI posts the next November filing-season table (tax year 2025, `26inweek47.xlsx`, expected January 2027):
  download it into `resources/filing_season/raw/`, run the stage `filing_season`, add the new pair to the backtest that
  writes `targets_config/filing_season_errors.csv`, and revisit the trust levels. While 2024 has no final tables the
  2025 table is not used.
- When SOI publishes final tables for 2024: make 2024 a past year (a past year's `targets.yaml`, its own draw), move
  `tilt.projection.filing_season` `base_year` and `year` to 2024 and 2025, move the `filing_season_groups` entry from
  2024's `targets.yaml` to 2025's, and 2025 becomes the jump-off year (every projection year's `from_year` then reads
  2024). If the November table for the year after the last SOI year is missing, the jump-off year is the last SOI year
  and the blend is off.
- Schedule C stays at no trust until its 2024 fall is understood ($47 billion of the $55 billion fall is larger losses
  on returns with no AGI).
- Revisit the Schedule A add factors at each CBO update.

**Validation.**
- Tax expenditures against JCT and OTA: overtime, tips, car loans, step-up at death and the education and dependent
  care credits are off in both the rebuild and production; on the rebuild alone, the QBI deduction is low (sole
  proprietor income below production's) and the lower rates on gains high.
- Whether the calculator's pass-through entity tax workaround subtracts taxes SOI's partnership and S corporation
  income is already net of.

**Scaling to more surveys.** With ten times the rows: state the learning-rate schedule in updates rather than passes;
draw each batch with fixed shares by source so small surveys get enough updates; retune the fill-in's refit
interval; store the table in GPU memory as 8-bit integers, since every value is a band from -1 to 83 (this matters
only at tens of millions of rows).

**Questions set aside.** Growth rates solved jointly with the reweighting; small items that move a lot in 2020-2023;
the 2020-2021 calculator targets and zero-income filers; one blown-up debt record; non-filers (credit-only filers
counted as non-filers; non-filers too rich); pandemic-year patterns the networks did not learn (2020 unemployment, the
2021 EITC); charity at the Schedule A limit; Forbes couples in a divorce kept as one unit or two; whether Tax-Simulator
should warn rather than stop when heir data is off; the CPS's 65+ count against SOI's Table 1.6.
