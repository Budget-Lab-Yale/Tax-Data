---
title: "Design C — emit the year's live records: what it costs to build"
role: notes
workstream: state_weights
status: open
updated: 2026-09-13
sot: research/state_weights/plan.md
supersedes: []
superseded_by: null
---

# Design C — emit the year's live records: what it costs to build

Scoping note, written 2026-09-13 at JI's request, for the choice between
**design A** (the union base emitted whole, built and verified on branch
`block-e`, vintage 2026091119) and **design C** (the same base, emitting only
each year's live records). Design B — keep one cross-section and age it — is
out; it gives up the annual rebuild S18(c) requires.

**The structural point.** A and C are *the same build through Phase 3*. They
differ in one place: what `write_outputs.R` writes. This is not a second
architecture. It is an emit rule plus the downstream contract change that
follows from it.

---

## 1. The emit rule

For each year *y*, write only records with `weight > 0` in *y*.

Measured on vintage 2026091119 (design A's own output), sampling
2017/2020/2023/2024/2030/2050/2075/2097:

| | |
|---|---|
| positive-weight share | **26.8% – 28.1%** in every year |
| rows dropped, 2022 | 1,019,212 of 1,399,234 |
| PUF filers with zero weight, any year 2017–2097 | **0** |
| zero-weight non-filers, 2024 onward | constant at 1,022,757 |

Two consequences worth stating up front. The rule touches **only pool
records** — no filer is ever dropped, in any year out to 2097, so the filer
slice of the file is untouched by design C. And the record set varies at only
**seven boundaries**: from 2024 on, every year emits the same live set (filers
plus the 2023 pool), because the six non-2023 pools are permanently zero there.
For 74 of the 81 emitted years the id vector is constant.

## 2. Tax-Data changes — small

**(a) `src/write_outputs.R`, the loop at lines 36–47.** Insert the filter after
the Forbes splice (which appends synthetic rows) and before the column select:

```r
out = apply_forbes_splice_to_materialized(out, y, fs, ...)
out = out[out$weight > 0, , drop = FALSE]          # design C
out = out[, intersect(out_cols, names(out)), drop = FALSE]
```

**(b) A per-year manifest.** Emit rows written and the pool year each record
came from, so the consumer can assert what it received rather than infer it.
The non-filer contract in `src/nonfiler_contract.R` is the natural home.

**(c) Nothing else.** `materialize()` is unchanged — it already reconstructs
the full union base from the ledgers, and the filter sits outside it, so it
stays a pure reconstruction function. Phases 1–3 are untouched: the union base
is still built once and imputed once, which is the part of design A worth
keeping.

## 3. Tax-Simulator changes — the real work

**(a) `src/misc/config_parser.R:238–256`.** `sample_ids` is read once from the
**2017** file and `random_numbers` is built from it, then both are carried in
`globals` and applied to every year. Under C the 2017 id vector no longer
describes later years.

The recommended fix is not to make `sample_ids` a per-year list but to **stop
precomputing it**. S23 made every draw a function of record id alone, so
nothing is gained by hoisting the draws out of the year loop, and the
positional `bind_cols` that forced `ids_in_file` to be kept in file order
disappears with it:

```r
# src/sim/run.R:352,357 — per year, after the read
filter(keep_in_subsample(id, globals$pct_sample)) %>%
...
bind_cols(build_random_numbers(.$id)) %>%
```

**(b) An id-stable subsample.** `sample_frac(pct_sample)` at
`config_parser.R:243` draws members positionally. Replace it with a rule keyed
on the record:

```r
keep_in_subsample = function(id, pct) pct == 1 | draw_by_id(id, 'subsample') < pct
```

This directly answers the strongest objection to C. Under A, a `pct_sample < 1`
run follows the same records every year, so a year-over-year difference carries
no sampling noise; under naive per-year sampling it would. An id-keyed rule
restores that property exactly: the same filers are kept in every year, and
each pool is sampled by the same rule. Needs one new entry in `RNG_STREAMS`
(append-only, next free index).

**(c) Call sites.** `globals$sample_ids` is also read by
`src/tests/state/test_state_cross_model.R:96`.

**(d) Cost.** `build_random_numbers()` draws nine `runif(1e7)` vectors, about
0.3 s each, so roughly 3 s per year per scenario instead of once per run. With
`multicore = 'year'` this is parallel and immaterial against the ~2 min a
current full-sample run takes; memoise on the id set if it ever matters.

## 4. Why the totals should not move, and the one place they might

Every aggregate in `get_1040_totals()` and `get_pr_totals()`
(`src/data/post_processing/summary_stats.R:184–200, 260–269`) is weighted:
counts are `sum((. != 0) * weight * filer)`, amounts are `sum(. * weight)`,
marginal rates are `weighted.mean(., weight)`. A zero-weight record contributes
exactly zero to all of them. **Dropping them is arithmetically neutral, and
that is the acceptance test**: `totals/1040.csv` and `totals/payroll.csv` must
be *identical*, not close, between an A vintage and a C vintage of the same
build.

The one exception to check rather than assume: in the `by_agi = T` path, an AGI
group populated only by zero-weight records yields a `NaN` row under A and no
row at all under C. Worth confirming no published cut relies on such a row
existing.

## 5. What genuinely changes, and must be deliberate

**The Forbes splice defect flips to fixed.** Synthetic billionaire records
(749 from 2022, 935 from 2025; ids from `make_forbes_id(year, rank, ...)`) are
absent from the 2017 file that `sample_ids` is taken from, so `run.R:352` drops
every one of them — they have never reached Tax-Simulator, on `main` either.
Per-year ids include them. That is a fix, but it moves the top of the
distribution, so it should land as its own change with its own before/after
rather than arriving silently inside design C.

### 5a. It is blocked on the id space (found 2026-09-13 in execution)

Making membership a rule stops dropping the Forbes rows, and the S23 guard
then refuses them: `make_forbes_id()` is `year * 1e6 + rank`, so the 2022
cohort occupies **2,022,000,001 – 2,022,000,717** against an `RNG_ID_SPACE`
of 1e7. `draw_by_id()` stopped the run with exactly the message it was written
to give — Tax-Data is emitting ids beyond the space the draws are defined
over.

So the Forbes fix is not free. It needs either a wider id space, which moves
**every** draw for every record, or Forbes ids renumbered into the existing
space (they are synthetic, so renumbering is legitimate, but must not collide
with a pool's 1e6 block). Until one of those is chosen, the measurement branch
excludes them explicitly in `in_subsample()` rather than as a side effect of
where `sample_ids` was read from — which also makes the A vintage on the
branch comparable to the A vintage on `state-tax`, isolating the emit rule.

## 6. Validation gate changes

`nonfiler_residual/05_preflight_vintage.R` check 1 — "the id vector after
Tax-Simulator's own filter is identical, in order, to the 2017 vector in every
year" — is precisely the contract C removes. It is replaced, not deleted:

> the **filer** slice is identical in order across all years, and the
> non-filer slice is exactly the year's pool, in the pool's order.

Check 2 (the filer slice identical between vintages, matched by id) survives
unchanged and is still the E2 tripwire.

## 7. What C buys

| | A | C |
|---|---|---|
| rows / year | 1,399,234 | ~380,000 |
| file size / year | 930 MB | ~320 MB |
| total, 2017–2097 | 78 GB | ~27 GB |
| Tax-Simulator | computes full liability on ~1.0M zero-weight rows every year | none wasted |
| deflate-at-append follow-up | open | disappears for 2017–2023 |

`filter(id %in% globals$sample_ids)` at `src/sim/run.R:352` is the **only**
filter in the run path — Tax-Simulator never drops zero-weight records. Under
design A it therefore runs the whole calculator on roughly a million inert rows
per year, on every run anyone ever does. The federal battery's Block E arm
peaked at 153 GB; the record count falls by about 73%.

## 8. Risks and open questions

1. **Consumers outside Tax-Simulator.** The fixed-record-set assumption has
   been checked *only* inside Tax-Simulator. Any other Budget Lab model reading
   Tax-Data output directly and assuming stable ids across years would change
   the answer. Unverified — this is the item to close before committing.
2. **Nothing longitudinal breaks, verified.** Tax-Simulator holds no
   record-level state across years: the only id-keyed join is
   `by = c('id','year')` in `src/sim/behavior.R:89`, which is within-year, and
   years run independently under `mclapply`. Loss carryforwards are explicitly
   not modelled.
3. **The re-randomisation hazard is closed, not merely mitigated.** It was the
   highest-risk item on the Block E checklist and the reason A was preferred.
   S23 (2026-09-13) makes every draw in both repos a function of record id
   alone, verified at 0 moving imputed columns between a 374,630-record and a
   1,399,234-record build.

## 9. Order of work

1. Confirm item 8.1 — no other consumer assumes stable ids across years.
2. Tax-Data: the emit filter and the manifest (§2). Produce a C vintage from
   the same base as 2026091119.
3. Assert neutrality *before* touching Tax-Simulator: A and C vintages must
   give identical `totals/1040.csv` and `totals/payroll.csv` at full sample.
   This is possible because at `pct_sample = 1` the existing `%in%` filter is
   order-insensitive and simply keeps whatever the file holds.
4. Tax-Simulator: §3(a)–(c), the id-stable subsample, the per-year draws.
5. Rewrite preflight check 1 (§6); re-run the federal validation battery.
6. The Forbes fix (§5) as a separate, measured change.

Steps 1–3 are reversible and answer the question on their own. Step 4 is the
commitment.
