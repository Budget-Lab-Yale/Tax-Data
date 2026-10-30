# The Tax-Data rebuild

A general method for building a synthetic tax file for any year: one
generative model of tax units, pretrained on the PUF and household surveys (the SCF, the CEX, the CPS and the SIPP),
then post-trained (reweighted) to each year's published totals. Nothing here
feeds `src/main.R` yet.

| Read | For |
|---|---|
| `GOAL.md` | what we are building and why |
| `FEATURES.md` | what the pipeline does now |
| `ROADMAP.md` | what is left between the two |
| `NEXT_STEPS.md` | the agreed order of the next pieces of work, what is waiting on something, and open decisions |
| `TARGETS.md` | post-training: the process on one page, then each decision and how it was reached |
| `HISTORY.md` | how we got here, and the paths we tried and dropped |
| `AGES.md` | plan: ages for every person in a unit, including the high-income records with none |
| `TAX_UNITS.md` | plan: how each survey is turned into tax units, and the tests they must pass |
| `PERFORMANCE.md` | what training and generation time and memory depend on, what was measured, and what is left to do as surveys are added |

The full record up to 20 October 2026 (every experiment, report and
decision) is in `research/archive/rebuild_2026-10/`.

## Running it

Every stage runs through Slurm, from the repository root (the job runs the code of the repository it is submitted
from, so a worktree runs its own):

    sbatch research/rebuild/pipeline/run.sbatch                  # every stage
    sbatch research/rebuild/pipeline/run.sbatch generate tilt    # some stages

| Stage | Script | What it does | Time |
|---|---|---|---|
| scf | `roster_scf.py`, `build_scf.R` | SCF waves split into tax units by the shared rules (`tax_units.py`), with their income (gross Social Security from the benefits section, Schedule E as rental income and the rest), balance sheet, elective deferrals and auto loan interest | minutes |
| cex | `roster_cex.py`, `build_cex.R` | CEX survey years split into tax units by the shared rules, with their spending | a few minutes |
| filing | `filing.py`, `filing_requirement.R` | the CPS's tax units by the shared rules, each unit's statutory filing requirement, and its filing chance by the filing rule (v4), calibrated each year; separate returns; checks | about 7 minutes |
| cps | `build_cps.R`, `overtime_cps.R` | CPS ASEC tax years as tax units (the filing stage's returns), with each person's items, childcare and overtime pay (Tedeschi's rules; hours and missing pay from the March basic file) | minutes |
| sipp | `roster_sipp.py`, `sipp_read.py` | SIPP reference years split into tax units by the shared rules, with each adult's wages, elective deferrals (2021 and 2023) and tips, and the unit's dividends, gross Social Security and pensions | a few minutes (the first read of each release about a minute) |
| targets | `targets.py` | every published SOI number: the targets, and the totals that grow dollars and cut points | seconds |
| errors | `errors.py soi_cv` | SOI's published coefficients of variation, parsed from the downloaded workbooks | seconds |
| table | `build_table.py` | the training table (every source, the PUF with all its items) with the AGI group label, its bands, and the dollar maps (one per SCF wave for the balance sheet, other debt held to each wave's 99.9th percentile among holders; one per survey year for the CEX, CPS and SIPP) | about 3 minutes |
| train | `train.py` (and `fillin.py`, `qgroups.py`) | the model, with grouped weighting and the two-slot fill-in on every survey; train each pooled model with REBUILD_RUN set to its name in `generation.models` (`label_seed20261017`, `label_seed20261018`, `label_seed20261019`) and REBUILD_SEED to its seed | about 95-105 minutes on an H100 (batches of 512) |
| generate | `generate.py` | 400,000 filer records drawn AGI group by AGI group, and the non-filer batch, a third from each of three trained models, each record's balance sheet from one SCF wave and its CEX, CPS and SIPP items from one survey year | about 16 minutes |
| tilt | `tilt.py`, then `report_tilt.py` | the year's 789 targets (`targets_config/`, `target_list.py`, `errors.py`), the non-microdata imputations on the start (`non_micro_imputations.py`; new columns only, no effect on the weights), every record's taxes from tax-simulator-gpu with the QBI dials solved first (`taxes.py`, settings `taxes`; since 30 October), the target year's weights (`solver.py`) and file, and the run report | about 15 minutes (26 on a CPU node) |
| non_micro_tables | `build_non_micro_tables.py` | the published tables the non-microdata imputations read, into `research/rebuild/resources/non_micro/` (committed; not in the default list, run only when a source changes) | a minute |
| report | `report_cex.py`, `report_cps.py`, `report_sipp.py`, `report_translation.py` | consumption, the CPS's person items and overtime, and tips in the file against the target year's CEX, CPS and SIPP year mix; the translation report | seconds |
| calculator | `calculator.py`, `report_calculator.py` | not in the default stages: each record of a finished file through the GPU tax calculator (tax-simulator-gpu's batch command at the commit in settings `taxes`), the AGI check and the levels against SOI 2019 (`CALCULATOR_CONTRACT.md`). Since 30 October the tilt computes the taxes itself (`taxes.py`) | under a minute, a few minutes for the report |
| taxsim_file | `taxsim_file.py` | not in the default stages (29 October): the year file as Tax-Simulator's `tax_units_<year>.csv`, production's 188 columns in its order and with its conventions for missing values; where each column comes from is `TAXSIM_FILE_COLUMNS.csv` (written beside the output, copied to `research/rebuild/`) | a few minutes on a CPU node |

Names: every variable that Tax-Simulator's calculator reads has the calculator's name (29-30 October); the list is
`pipeline/tools/rename_map.csv` and `pipeline/tools/apply_rename.py` applies it to a branch made before (`FEATURES.md`).

Shared code: `common.py` (paths, settings, the PUF year and target year,
age ranges, wage groups), `model.py` (the network, and every call of it),
`fillin.py` (the two-slot fill-in, run by training), `qgroups.py` (grouped
weighting and the AGI group label), `dollars.py` (bands to dollars), `ages.py` (the primary filer's age range, used by the table),
`tax_units.py` (the shared tax-unit rules), `target_list.py` (the year's
targets from `targets_config/<year>/`, checked before every run), `growth.py` (how every dollar item is grown between
years, from `targets_config/<year>/growth.yaml` and the series in `resources/growth/`), `config_overview.py` (the
table of every setting by year, `targets_config/OVERVIEW.md`), `scf_targets.py` (the SCF targets' values and standard errors over the year's
mix of SCF waves), `survey_mix.py` (the CEX, CPS and SIPP year mix), `errors.py` (their standard errors),
`solver.py` (the reweighting), `non_micro_imputations.py` (the non-microdata imputations: items set from the drawn
items and a published table, and their checks).
Every setting is in one settings file: `settings.json`, or the file named by
`REBUILD_SETTINGS` (every stage, R and Python, reads it; a variant run is a
copy). The PUF year (2015) and the target year (2019) are its "years". The
training seed can be changed with `REBUILD_SEED`, the output folder with
`REBUILD_OUT`, the run's subfolder with `REBUILD_RUN`, and the number of
passes, for a quick test, with `REBUILD_PASSES`.

Outputs, under `/nfs/roberts/scratch/pi_nrs36/jar335/rebuild/`:

| Folder | Holds |
|---|---|
| `data/` | the table, band edges, dollar maps, SCF, CEX, CPS and SIPP units; `data/filing/`, the filing stage's units, returns and checks |
| `targets/` | the published SOI numbers |
| `model/<run>/` | the trained model and its training log (`<run>` is `REBUILD_RUN`, default `seed<n>`; the default training seed is 20261017, so the default run name, `seed20261017`, is the installed latest's) |
| `gen/<run>/` | the generated stack |
| `tilt/<run>/` | the final file (`file_2019.parquet`, every dollar column in nominal 2019 dollars, each grown by its line in `targets_config/2019/growth.yaml` since 29 October; until then the PUF items with no SOI Table 1.4 line stayed in 2015 dollars: `FEATURES.md`), the target list (`targets.csv`), every target before and after (`fit.csv`), a summary (`summary.md`), the run report (`report.md` and `report_*.csv`), and the consumption, CPS and SIPP reports |

## Working rules

- Plain English for John: tables and real numbers, no jargon.
- Nothing launched, and no design rule changed, without his go.
- Heavy compute through Slurm, never on the login node.
- A new feature is a change to this pipeline and its settings, with
  `FEATURES.md` updated and a line in `HISTORY.md`. Experiments that do
  not become features are not kept here.
