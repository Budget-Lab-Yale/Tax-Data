# The Tax-Data rebuild

A general method for building a synthetic tax file for any year: one
generative model of tax units, pretrained on the PUF and household surveys (the SCF and the CEX),
then post-trained (reweighted) to each year's published totals. Nothing here
feeds `src/main.R` yet.

| Read | For |
|---|---|
| `GOAL.md` | what we are building and why |
| `FEATURES.md` | what the pipeline does now |
| `ROADMAP.md` | what is left between the two |
| `HISTORY.md` | how we got here, and the paths we tried and dropped |
| `AGES.md` | plan: ages for every person in a unit, including the high-income records with none |
| `PERFORMANCE.md` | what training and generation time and memory depend on, what was measured, and what is left to do as surveys are added |

The full record up to 20 October 2026 (every experiment, report and
decision) is in `research/archive/rebuild_2026-10/`.

## Running it

Every stage runs through Slurm, from the repository root:

    sbatch research/rebuild/pipeline/run.sbatch                  # every stage
    sbatch research/rebuild/pipeline/run.sbatch generate tilt    # some stages

| Stage | Script | What it does | Time |
|---|---|---|---|
| scf | `build_scf.R` | SCF waves split into tax units (the production builder) | minutes |
| cex | `build_cex.R` | CEX survey years split into tax units, with their spending | about a minute |
| table | `build_table.py` | the training table, its bands, and the dollar maps | about 2 minutes |
| targets | `targets.py` | the published SOI numbers | seconds |
| train | `train.py` | the model, with the two-slot fill-in | about 15 minutes on an H200 (10 seconds a pass) |
| generate | `generate.py` (and `generate_fast.py`) | 400,000 generated records | about 5 minutes |
| tilt | `tilt.py` | the 2019 weights and file | about a minute |
| report | `report_cex.py` | consumption in the file against the CEX 2019 | seconds |

Shared code: `common.py` (paths), `model.py` (the network), `dollars.py`
(bands to dollars), `ages.py` (the primary filer's age range, used by the
table). Every setting is in `settings.json`. The training seed
can be changed with `REBUILD_SEED`, and the output folder with
`REBUILD_OUT`.

Outputs, under `/nfs/roberts/scratch/pi_nrs36/jar335/rebuild/`:

| Folder | Holds |
|---|---|
| `data/` | the table, band edges, dollar maps, SCF and CEX units |
| `targets/` | the published SOI numbers |
| `model/seed<n>/` | the trained model and its training log |
| `gen/seed<n>/` | the generated stack |
| `tilt/seed<n>/` | the final 2019 file (`file_2019.parquet`), every target before and after (`fit.csv`), a summary (`summary.md`), and the consumption report (`cex_report.md`) |

## Working rules

- Plain English for John: tables and real numbers, no jargon.
- Nothing launched, and no design rule changed, without his go.
- Heavy compute through Slurm, never on the login node.
- A new feature is a change to this pipeline and its settings, with
  `FEATURES.md` updated and a line in `HISTORY.md`. Experiments that do
  not become features are not kept here.
