---
title: "Test 4: probes for the joint model plan — shared contract"
role: plan
workstream: rebuild
status: current
updated: 2026-10-09
sot: research/rebuild/joint_model_plan.md
supersedes: []
superseded_by: null
---

# Test 4: probes for the joint model plan

Started 9 October 2026, 23:40, by the main session at John's request: a
night of early experiments for `research/rebuild/joint_model_plan.md`
(read it first). Each probe answers one question that bears on a decision
in the plan. Six probes run in parallel, each by one subagent.

| Probe | Prefix | Question |
|---|---|---|
| T | `t_` | Why is the top of the PUF thin in generated records, and which training change fixes it? |
| Y | `y_` | Does D24's translation recover the truth when we know it (test 2's distorted halves)? |
| X | `x_` | Translate the real SCF into PUF concepts: do the targets stop contradicting each other, and does completed PUF wealth move toward the SCF? |
| Q | `q_` | How good must pretraining be? Does post-training fix the statistics it did not target? |
| BR | `br_` | Bridge variables: can the SCF supply mortgage interest and real estate tax, and do they improve wealth given tax records? |
| AU | `au_` | The authority table (plan step 0) in full. |

## Rules for every probe

- **Where things go.** Code and reports in this folder, every file named
  with the probe's prefix; report `<PROBE>_REPORT.md` (e.g. `T_REPORT.md`).
  Large outputs in `$Q = /nfs/roberts/scratch/pi_nrs36/jar335/probes/<prefix
  without underscore>/`; Slurm logs in `$Q/../logs/`. Do not edit any file
  in `../posttrain/` or `../scf_joint/`, and do not write into their scratch
  folders; copy a script into this folder under your prefix and change the
  copy. Do not cancel or touch any Slurm job you did not submit (many from
  test 3 are running).
- **Inputs.** The main model is test 3's version 2: data
  `/nfs/roberts/scratch/pi_nrs36/jar335/scf_joint/data/full_v2/` (also
  copied, with three trained checkpoints, under
  `/nfs/roberts/scratch/pi_nrs36/jar335/scf_joint/model/full_v2/`).
  Training: `../posttrain/h_train.py` with `h_train.sbatch` (about 18 minutes
  on one GPU). Completion and generation: `../posttrain/h_complete.py`.
  Tilt (post-training method 2): `../posttrain/c_tilt.py`, `c_fit.py`.
  Reports to read for context: `../posttrain/H_MODEL_REPORT.md`,
  `G_DATA_REPORT.md`, `g_crosswalk.md`, `C_REPORT.md`, `EVAL_REPORT.md`,
  `F_REPORT.md`, `STATUS.md` (the last entries), and
  `../../design_decisions.md` (D22–D24).
- **Compute.** Everything beyond reading files and listing folders runs in
  Slurm jobs (`sbatch`), including small Python checks. GPU partitions:
  `gpu_b200,gpu_h100,gpu_h200,gpu` (one GPU per job is enough; pass several
  partitions so the job starts wherever there is room; `scavenge_gpu` can be
  preempted). Copy the module line from `../posttrain/h_train.sbatch`.
- **Licensed data.** The PUF and SCF record files stay in scratch. Reports
  and committed files carry weighted aggregates only, never record-level
  rows. No git commits.
- **Time.** The session ends about 03:30 on 10 October. Write your report
  by 02:50 with whatever has finished, and say plainly what has not. Jobs
  may keep running past that; list them in the report with what to read
  when they end.
- **Status.** Append dated, signed entries to `STATUS.md` in this folder
  (e.g. "10 October, 00:40 (probe T): ...") at the start, at each result,
  and at the end. Append only; never rewrite another probe's entry.
- **Writing.** Reports are for John Ricco, a public finance economist who
  knows taxes well and software architecture reasonably well, but dislikes
  jargon and compressed insider phrasing. Plain, direct English in full
  sentences; define any term that is not standard in public finance; no
  invented shorthand or abbreviations for your own constructs; describe
  rather than argue; plain descriptive headers; write less. Lead with what
  was run, then results as levels tables (truth against model) by income
  group where that applies, then what it means for the plan, then what is
  unfinished. Every number in a report must come from a job you ran or a
  file you read; say which.

## Contract change, 10 October 07:55 (main session): probes O and S

John asked for two follow-ups to probe T: experiments with different
generation orders, and a fuller test of the sturdier fix (training the model
on the questions generation asks). Same rules as above; session ends about
11:25 on 10 October, reports due 10:45.

| Probe | Prefix | Question |
|---|---|---|
| O | `o_` | With the model unchanged, how much do generated PUF records depend on the order variables are drawn in, and how should the order be chosen? |
| S | `s_` | Does training on generation questions (in a fixed order, or in random orders) give a model that generates the PUF well and is robust to the order? |

## Contract change, 11 October 00:20 (main session): overnight viability probes

John asked, after probe AS removed the AGI problem, for overnight
experiments that test whether the overall approach is viable. Four probes,
same rules as above, with one difference: **the session ends about 02:40
and the subagents stop with it.** So every probe submits its work as Slurm
job chains that finish on their own (dependencies with `afterok`/`afterany`,
ending in a collector job that writes the tables to a markdown file in the
probe's scratch folder). Each writes its report by 02:20 with what has
finished, marks unfinished results "pending", and ends the report with a
section "To finish" that lists every job, the file its collector writes, and
which table in the report it fills. The next session finishes the reports.

| Probe | Prefix | Question |
|---|---|---|
| V | `v_` | A broad scorecard of whole PUF records generated from scratch (probe AS's model): every variable, joint relationships, impossible records, tax liability by AGI group, how well a classifier tells generated from real, and copying of real records. |
| C | `c3_` | All the fixes together (D25 applied to every total, non-itemizers' deductions unknown, bridge variables, translated SCF): does a combined model generate PUF records and complete PUF wealth better? |
| ST | `st_` | Stretching to another year where the truth is known: train without the SCF 2016 wave, post-train to SCF 2016 tables, and check what was not targeted. |
| BL | `bl_` | A standard baseline: a sequential tree synthesizer on the same PUF variables, scored the same way, so we know whether the transformer is competitive. |

## Contract change, 14 October 10:20 (main session): round 4

John asked for a few more experiments; findings go into
`../../joint_model_history.md`. Same rules as above. **The session ends
about 12:40 on 14 October and subagents stop with it**: submit
self-finishing Slurm chains; reports by 12:05 with "To finish" sections.

| Probe | Prefix | Question |
|---|---|---|
| TR | `tr_` | Sequential trees on the multi-survey job (PUF plus SCF, `full_v3cz`): whole joint records and completed PUF wealth, in probe C's file formats. |
| EV | `ev_` | One scorer for both methods: completed wealth on held-out SCF records and on PUF records, untargeted relationships, and probe V's scorecard on generated records. Transformer (probe C's models) against trees (TR). |
| DD | `dd_` | Itemized deductions as two variables, the deduction claimed and the amount paid: does one model then serve both generation and wealth completion? |
| WT | `wt_` | Why is completed PUF wealth too low in the top 0.1% ($38m against $55m)? Includes training the SCF side on the sample as drawn. |
