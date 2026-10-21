# The Tax-Data rebuild

A general method for building a synthetic tax file for any year: one
generative model of tax units, pretrained on the PUF and household surveys,
then post-trained (reweighted) to each year's published totals. Nothing here
feeds `src/main.R` yet.

| Read | For |
|---|---|
| `GOAL.md` | what we are building and why |
| `FEATURES.md` | what the pipeline does now |
| `ROADMAP.md` | what is left between the two |
| `HISTORY.md` | how we got here, and the paths we tried and dropped |
| `AGES.md` | plan: ages for every person in a unit, including the high-income records with none |
| `PERFORMANCE.md` | to do: what training time depends on, and how to keep it manageable as surveys are added |

The full record up to 20 October 2026 (every experiment, report and
decision) is in `research/archive/rebuild_2026-10/`.

## Running it

Every stage runs through Slurm, from the repository root:

    sbatch research/rebuild/pipeline/run.sbatch                  # every stage
    sbatch research/rebuild/pipeline/run.sbatch generate tilt    # some stages

| Stage | Script | What it does | Time |
|---|---|---|---|
| scf | `build_scf.R` | SCF waves split into tax units (the production builder) | minutes |
| table | `build_table.py` | the training table, its bands, and the dollar maps | minutes |
| targets | `targets.py` | the published SOI numbers | seconds |
| train | `train.py` | the model, with the cell method | about 40 minutes on an H100 (28 seconds a pass) |
| generate | `generate.py` | 400,000 generated records | about 10 minutes |
| tilt | `tilt.py` | the 2019 weights and file | about 10 minutes |

Shared code: `common.py` (paths), `model.py` (the network), `dollars.py`
(bands to dollars). Every setting is in `settings.json`. The training seed
can be changed with `REBUILD_SEED`, and the output folder with
`REBUILD_OUT`.

Outputs, under `/nfs/roberts/scratch/pi_nrs36/jar335/rebuild/`:

| Folder | Holds |
|---|---|
| `data/` | the table, band edges, dollar maps, SCF units |
| `targets/` | the published SOI numbers |
| `model/seed<n>/` | the trained model and its training log |
| `gen/seed<n>/` | the generated stack |
| `tilt/seed<n>/` | the final 2019 file (`file_2019.parquet`), every target before and after (`fit.csv`), and a summary (`summary.md`) |

## Working rules

- Plain English for John: tables and real numbers, no jargon.
- Nothing launched, and no design rule changed, without his go.
- Heavy compute through Slurm, never on the login node.
- A new feature is a change to this pipeline and its settings, with
  `FEATURES.md` updated and a line in `HISTORY.md`. Experiments that do
  not become features are not kept here.
