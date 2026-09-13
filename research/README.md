---
title: "Research corpus index"
role: index
workstream: cross-cutting
status: current
updated: 2026-09-13
sot: self
supersedes: []
superseded_by: null
---

# `research/` — the population-construction research corpus (Tax-Data)

Design records, plans, evidence and reviews for the work that builds populations
and weights: the non-filer rework (the CPS ASEC pool that replaces the DINA
append) and the split state weights. **If a document is not reachable from this
file, it does not exist.**

**Moved here from Tax-Simulator on 2026-09-11** (JI): a population in PUF schema
is Tax-Data's mission, so the builder, its anchors and the state-weights fit
live here; Tax-Simulator keeps the state tax *law* (`research/state_tax/`,
`config/scenarios/tax_law_state/`) and *consumes* weights through the Tax-Data
interface. The tree's shape is unchanged so every citation below still resolves.
The shared R modules the scripts `source()` are in `src/nonfilers/`
(`asec_tax_units.R`, `filing_model.R`, `state_weights.R`). The Tax-Simulator
copy is frozen behind `research/state_weights/MOVED.md` there.

## Where to start

| You want | Read |
|---|---|
| What is true today | the workstream's **plan** (`STATUS.md` stayed in Tax-Simulator with the state-law work) |
| Why a thing is the way it is | the workstream's `role: method` document |
| What happens next | the workstream's **one** `role: plan` document |
| A settled argument, and why | [`decisions_log.md`](decisions_log.md) |
| The naming and front-matter rules | [`CONVENTIONS.md`](CONVENTIONS.md) |

## The workstreams

### `state_tax/` — per-state law encoding

**Lives in Tax-Simulator** (`research/state_tax/`, `research/source_packets/`),
next to the state tax-law YAML it documents. Not mirrored here.

### `state_weights/` — split state weights and the non-filer rework

One workstream: the non-filer rework lands before the Phase 1 weights swap-in, so
they share a plan.

| Role | Document |
|---|---|
| **plan** | [`state_weights/plan.md`](state_weights/plan.md) |
| method — non-filers | [`state_weights/nonfiler_residual_design.md`](state_weights/nonfiler_residual_design.md) |
| method — the fit | [`state_weights/state_weights_phase1_summary.md`](state_weights/state_weights_phase1_summary.md) |
| procedure | [`state_weights/nonfiler_federal_validation.md`](state_weights/nonfiler_federal_validation.md) |
| review | [`state_weights/nonfiler_federal_validation_findings.md`](state_weights/nonfiler_federal_validation_findings.md) — the battery's first run: the E2 tripwire fails, and why |
| evidence | [`state_weights/nonfiler_residual/04_findings.md`](state_weights/nonfiler_residual/04_findings.md) + `nonfiler_residual/results/` |
| notes | [`state_weights/notes/`](state_weights/notes/) |
| scripts | [`state_weights/scripts/`](state_weights/scripts/) — the Phase 1 harness (`sweep_`, `validate_`) and drivers |

## Folder rules

| Folder | Rule |
|---|---|
| `<workstream>/` | One line of work. Holds **exactly one `role: plan`** plus any number of `method` / `procedure` / `review` documents. Flat — roles are metadata, not directories. |
| `<workstream>/notes/` | Everything that is not plan, method, procedure or review: surveys, one-off analyses, deferred designs. A note with `status: open` **must** be cited from its workstream's plan. |
| `<workstream>/scripts/` | Research drivers. They may `source()` `src/`; **nothing in `src/` may invoke them.** That asymmetry is the test for where a script belongs. |
| `<workstream>/<bundle>/` | Numbered scripts whose outputs must stay adjacent to them (`nonfiler_residual/`). |
| `docx_sources/` | Word-native documents that are **not** renders: authored in Word, carrying tracked changes. The Markdown-is-truth rule does not apply to these. |
| `tools/` | The release renderer, its style reference, and one manifest per release slug. |
| `releases/` | `YYYY-MM-DD_<slug>.docx`. Committed snapshots for outside review. **Never edited, never a source.** |
| `archive/` | Nothing here is current. Every entry is justified in [`archive/README.md`](archive/README.md). |

## Cutting a Word release

Markdown is the source of truth; a release is a dated snapshot of it for someone
outside the repo. See `CONVENTIONS.md` for the manifest format.

```bash
Rscript research/tools/render_release.R state_weights_plan --dry-run
Rscript research/tools/render_release.R state_weights_plan
```

## Drift checks

One command, ten checks. Run it before pushing documentation changes.

```bash
Rscript research/tools/check_conventions.R          # exits 1 on any finding
Rscript research/tools/check_conventions.R -v       # also say what each check covered
Rscript research/tools/check_conventions.R --check 6 # just one check
Rscript research/tools/check_conventions.R --report-only   # print, always exit 0
```

| # | Check |
|---|---|
| 1 | exactly one `role: plan` per workstream |
| 2 | front matter present on every document (artifact directories exempt) |
| 3 | `role` / `status` / `workstream` drawn from the closed vocabularies |
| 4 | front-matter `updated:` not behind the file's own last commit date |
| 5 | a note with `status: open` is cited from its workstream's plan |
| 6 | cited paths resolve |
| 7 | `sot:` and `supersedes:` targets exist |
| 8 | nothing outside `archive/` cites the pre-2026-08-19 locations |
| 9 | archive names match the convention and each has an `archive/README.md` entry |
| 10 | no living document declares itself superseded |

Check 4 catches the commonest real failure: a document edited without its header
bumped. Check 5 is the one that matters most, because an open note no plan cites
is how outstanding work goes missing — the failure this whole tree exists to fix.

Check 6 needs a way to tell a moved file from one that lives in another repo or
does not exist yet. Those go in `research/tools/known_external_paths.csv` with a
`kind` and a reason, so an unresolvable citation is always a reviewed decision
rather than a silent unknown.

**It runs itself.** `.claude/settings.json` carries a `Stop` hook that runs the
checker at the end of every Claude Code session, so an agent cannot leave the
tree in a state it would flag. The hook is written to degrade rather than
mislead: it loads the R module only if `Rscript` is not already on `PATH`, and
exits silently if there is still no R, so it is inert off the cluster rather
than noisy. A finding surfaces as a non-blocking error — the session ends and
you see it. Review or disable it with `/hooks`.

**What it does not do:** it never reads prose for meaning. Whether `STATUS.md`
agrees with a plan is what `sot:` is for; a checker that tried would produce
noise, and noise is how a check gets ignored.
