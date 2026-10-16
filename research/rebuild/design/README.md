---
title: "Tax-Data rebuild — the one-model design (index)"
role: index
workstream: rebuild
status: current
updated: 2026-10-16
sot: research/rebuild/design/plan.md
supersedes: []
superseded_by: null
---

# The one-model design

The rebuild as a whole starts at `research/rebuild/README.md` (where things
stand) and `research/rebuild/minimal/LATEST.md` (the current run). This
file indexes the design documents only.

| File | What it is |
|---|---|
| `plan.md` | What is decided, what is open, the next tests in order, the fallback ladder, and what happened to each earlier decision D1–D26. Read first. |
| `translation_by_latent_amounts.md` | Plan decision 18: translation learned in pretraining through an actual layer, each source's concept and reported amount; the worked example; the tests. |
| `idea_space.md` | The full record of the 14 October discussion that produced the plan: the four questions (what object, what flows in, how it is combined, how we know), every alternative considered, the evidence cited, and a glossary. Read when a decision in the plan needs its reasoning. |

Still current elsewhere in `research/rebuild/`:

- `BRIEF.md`: the problem statement of 6 October.
- `joint_model_history.md`: what each experiment of 8–14 October ran and
  found. The plan's numbers come from here and from the reports under
  `tests/`.
- `background/`: the twelve background reports and seven reviews of
  6–7 October.
- `minimal/`: the minimal example of 14 to 16 October, one numbered
  report per step; the current run is named in `minimal/LATEST.md`.
- `tests/`: the experiment code and reports (test 1 `puf_generator/`,
  test 2 `scf_joint/`, test 3 `posttrain/`, test 4 `probes/`, the pooled
  SCF test `pooled_scf/`).

Superseded on 14 October and moved to `research/archive/`, each under its
old basename with the suffix `_2026-10-14_superseded`: the joint model plan,
the design decisions log (D1–D26), the synthesis of the background research
(and its stale HTML render, suffixed `_2026-10-07_stale-render`), the RLHF
analogy note, and the D21 pooling questions. Citations of them in the test
reports, the history and the background reports were repointed the same
day. `research/archive/README.md` says what each is still useful for.

## Revision history

- 2026-10-16: points to the top-level README as the start; lists `minimal/`.
- 2026-10-14: created.
