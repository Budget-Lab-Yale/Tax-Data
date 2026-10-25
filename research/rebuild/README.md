# The Tax-Data rebuild

A general method for building a synthetic tax file for any year: one
generative model of tax units, pretrained on the PUF and household surveys (the SCF, the CEX and the CPS),
then post-trained (reweighted) to each year's published totals. Nothing here
feeds `src/main.R` yet.

| Read | For |
|---|---|
| `GOAL.md` | what we are building and why |
| `FEATURES.md` | what the pipeline does now |
| `ROADMAP.md` | what is left between the two |
| `TARGETS.md` | post-training: the process on one page, then each decision and how it was reached |
| `HISTORY.md` | how we got here, and the paths we tried and dropped |
| `AGES.md` | plan: ages for every person in a unit, including the high-income records with none |
| `TAX_UNITS.md` | plan: how each survey is turned into tax units, and the tests they must pass |
| `PERFORMANCE.md` | what training and generation time and memory depend on, what was measured, and what is left to do as surveys are added |

The full record up to 20 October 2026 (every experiment, report and
decision) is in `research/archive/rebuild_2026-10/`.

## Running it

Every stage runs through Slurm, from the repository root:

    sbatch research/rebuild/pipeline/run.sbatch                  # every stage
    sbatch research/rebuild/pipeline/run.sbatch generate tilt    # some stages

| Stage | Script | What it does | Time |
|---|---|---|---|
| scf | `roster_scf.py`, `build_scf.R` | SCF waves split into tax units by the shared rules (`tax_units.py`), with their income and balance sheet | minutes |
| cex | `roster_cex.py`, `build_cex.R` | CEX survey years split into tax units by the shared rules, with their spending | a few minutes |
| filing | `filing.py`, `filing_requirement.R` | the CPS's tax units by the shared rules, each unit's statutory filing requirement, and its filing chance by the filing rule (v4), calibrated each year; separate returns; checks | about 7 minutes |
| cps | `build_cps.R` | CPS ASEC tax years as tax units (the filing stage's returns), with each person's items and childcare | minutes |
| targets | `targets.py` | every published SOI number: the targets, and the totals that grow dollars and cut points | seconds |
| errors | `errors.py soi_cv` | SOI's published coefficients of variation, parsed from the downloaded workbooks | seconds |
| table | `build_table.py` | the training table, its bands, and the dollar maps | about 2 minutes |
| labels | `build_table.py labels` | not in the default list: the strata labels added to a copy of the table (`data/strata/`) | minutes |
| train | `train.py` (and `fillin.py`) | the model, with the two-slot fill-in | about 15 minutes on an H200 (10 seconds a pass) |
| generate | `generate.py` | 400,000 filer records drawn by strata and the non-filer batch, half from each of two trained models | about 5 minutes |
| tilt | `tilt.py`, then `report_tilt.py` | the year's targets (`targets_config/`, `target_list.py`, `errors.py`), the target year's weights (`solver.py`) and file, and the run report | about 7 minutes on a CPU node |
| report | `report_cex.py`, `report_cps.py`, `report_translation.py` | consumption and the CPS's person items in the file against the target year's CEX and CPS; the translation report | seconds |

Shared code: `common.py` (paths, settings, the PUF year and target year,
age ranges, wage groups), `model.py` (the network, and every call of it),
`fillin.py` (the two-slot fill-in, run by training), `dollars.py` (bands to
dollars), `ages.py` (the primary filer's age range, used by the table),
`tax_units.py` (the shared tax-unit rules), `target_list.py` (the year's
targets from `targets_config/`), `errors.py` (their standard errors),
`solver.py` (the reweighting).
Every setting is in one settings file: `settings.json`, or the file named by
`REBUILD_SETTINGS` (every stage, R and Python, reads it; a variant run is a
copy). The PUF year (2015) and the target year (2019) are its "years". The
training seed can be changed with `REBUILD_SEED`, the output folder with
`REBUILD_OUT`, the run's subfolder with `REBUILD_RUN`, and the number of
passes, for a quick test, with `REBUILD_PASSES`.

Outputs, under `/nfs/roberts/scratch/pi_nrs36/jar335/rebuild/`:

| Folder | Holds |
|---|---|
| `data/` | the table, band edges, dollar maps, SCF, CEX and CPS units; `data/filing/`, the filing stage's units, returns and checks |
| `targets/` | the published SOI numbers |
| `model/<run>/` | the trained model and its training log (`<run>` is `REBUILD_RUN`, default `seed<n>`) |
| `gen/<run>/` | the generated stack |
| `tilt/<run>/` | the final file (`file_2019.parquet`, every dollar column in nominal 2019 dollars), the target list (`targets.csv`), every target before and after (`fit.csv`), a summary (`summary.md`), the run report (`report.md` and `report_*.csv`), and the consumption and CPS reports |

## Working rules

- Plain English for John: tables and real numbers, no jargon.
- Nothing launched, and no design rule changed, without his go.
- Heavy compute through Slurm, never on the login node.
- A new feature is a change to this pipeline and its settings, with
  `FEATURES.md` updated and a line in `HISTORY.md`. Experiments that do
  not become features are not kept here.
