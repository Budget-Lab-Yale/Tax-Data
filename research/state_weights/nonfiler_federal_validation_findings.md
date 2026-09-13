---
title: "Federal validation, first run: the E2 tripwire fails, and why"
role: review
workstream: state_weights
status: open
updated: 2026-09-12
sot: research/state_weights/plan.md
supersedes: []
superseded_by: null
---

# Federal validation, first run: the E2 tripwire fails, and why

**Run 2026-09-12** against Tax-Data vintages `2026083115` (main, the DINA
append), `2026091118` (the ASEC swap through S21) and `2026091119` (Block E,
every pool year in the base), on Tax-Simulator `state-tax`. Procedure:
[`nonfiler_federal_validation.md`](nonfiler_federal_validation.md); its §4a
table is the acceptance gate. Runscripts
`config/runscripts/tests/nonfiler_ab_{main,pre,post}.csv` in Tax-Simulator;
preflight
[`nonfiler_residual/05_preflight_vintage.R`](nonfiler_residual/05_preflight_vintage.R).

**The headline. The E2 tripwire fails.** Under identical tax law, with only the
non-filer population changed, **187 of 209 filer-gated columns in
`totals/1040.csv` move**. §4a says every one of them must not.

**The population is not the defect.** The cause is that *both* repositories
derive their random numbers from a record's POSITION and the record set's SIZE
rather than from the record's identity. Change how many records are in the
file — which is exactly what Block E does, and what any annual-rebuild design
must do — and every draw moves. This note is the evidence for that claim, the
measured size of it, and what closing it costs.

---

## 1. What was run

| Output vintage | Tax-Data input | What it is |
|---|---|---|
| `nf_main` | 2026083115 | main: the DINA non-filer append, 220,896 records |
| `nf_pre` | 2026091118 | the ASEC swap through S21, 2017 pool only, 374,630 records |
| `nf_post` | 2026091119 | Block E: seven pool years in the base, 1,399,234 records |
| `ctl_pre`, `ctl_post` | same two | re-runs on the measurement branch of §3 |

Baseline tax law, 2017:2026, full sample, one runscript per vintage. One per
runscript is forced: `config_parser.R:219` takes `sample_ids` from the **first**
scenario's Tax-Data path and applies it to every scenario, so two vintages in
one runscript would filter the larger one down to the smaller one's records and
the comparison would be vacuous.

Each run took under two minutes on 10 cores; Block E peaked at 153 GB.

## 2. What passed

**Payroll moves, and that is the predicted signal.** §4a says
`totals/payroll.csv` must move because `get_pr_totals()` is not filer-gated and
non-filers owe payroll tax on wages whether or not they file. It does:
self-employment income +4.6%, self-employment tax +4.2%, reported tax units
+0.76% (2024). Payroll revenue moves +0.60%, income-tax revenue +0.078%.

**The preflight gate passed on its own terms**, including the property Block E
was designed to preserve: within each vintage, the id vector after
Tax-Simulator's own `filter(id %in% sample_ids)` is identical, in order, to the
2017 vector in every year.

## 3. Cause 1 — a Tax-Simulator hazard the procedure does not list

`config_parser.R:222`:

```r
read_microdata(2017) %>% sample_frac(size = pct_sample) %>% get_vector('id')
```

At a full sample `sample_frac(size = 1)` returns **every** row, in random
**order**. Its only consumer is `id %in% sample_ids` at `run.R:352`, which is
order-insensitive — so the shuffle selects nothing and changes nothing about
which records run. But it **consumes random numbers in proportion to the record
count**, so the `random_numbers` tibble drawn immediately after starts from a
different point in the stream for every vintage of a different size.
Demonstrated directly:

| | first five draws after the shuffle |
|---|---|
| n = 220,896 | 0.928461 0.853482 0.258099 0.858449 0.750933 |
| n = 374,630 | 0.110624 0.006816 0.919957 0.591335 0.401887 |
| n = 1,399,234 | 0.934597 0.705042 0.207447 0.913562 0.666874 |
| **no shuffle**, n = 220,896 *and* n = 1,399,234 | 0.624585 0.736341 0.431341 0.101978 0.467559 — identical |

A second, smaller effect sits beside it: the nine columns are drawn one after
another from one stream, so column *k*'s draws depend on the lengths of columns
1…*k*−1. Even without the shuffle only the first column would line up.

**Measured with a control.** Branch `rng-control` in Tax-Simulator (worktree
`Tax-Simulator-rngctl`, **not production**) skips the shuffle when
`pct_sample == 1` and seeds each column separately. Since filers occupy the
first rows of every Tax-Data vintage, that gives every filer an identical draw
across vintages. Re-running `nf_pre` and `nf_post` under it:

| column | uncontrolled | controlled |
|---|---|---|
| `n_excess_bus_loss` | 67.3% | 3.5% |
| `excess_bus_loss` | 17.8% | 3.2% |
| columns moving | 187 of 209 | **187 of 209** |

So this hazard inflates the extremes and explains none of the breadth. It is
worth fixing, and it is not the finding.

## 4. Cause 2 — Tax-Data's positional Phase 1 draws, which is the finding

The imputation modules in `src/imputations/` draw with `runif(nrow(.))` and
`sample_n()` under one `set.seed(76)`. The number of draws each consumes
therefore scales with the record count, so **every module after the first sees
a shifted stream** when the base grows. Block E triples the base.

`05_preflight_vintage.R` finds **46 of 188 columns differ at the record level**
between `2026091118` and `2026091119` for the *same* 207,692 filers. They are
exactly the imputed families: `male1`/`male2`, `age1`/`age2`, the dependent
slots, `wages1`/`wages2`, tips, overtime, the QBI wage bills, `kg_lt_basis`,
`auto_int_exp`, `care_exp`, the eight consumption categories, `q_death1/2`. In
aggregate, over filers:

| filer aggregate | 2017 | 2023 |
|---|---|---|
| `wagebill_sole_prop` | −3.9% | −3.4% |
| `care_exp` | +2.0% | +2.2% |
| `auto_int_exp` | −1.3% | −1.2% |
| `tips` | +0.7% | +1.3% |
| `kg_lt_basis` | +0.6% | +1.1% |
| consumption categories | ≤0.9% | ≤0.5% |
| `wages1`, `wages2` | ≤0.03% | ≤0.04% |

**How that reaches lines it never touches.** For 2020 the filer inputs for
`ui`, `sole_prop`, `sole_prop1/2` and `farm` are **record-identical** between
the two vintages — verified directly — yet their reported 1040 totals move
(`n_ui` +36%, `sole_prop` −2.9%). The chain is: the imputed variables above
enter adjusted gross income; AGI gates the 2020 unemployment exclusion and the
excess-business-loss limitation; different households cross those thresholds;
the post-calculation values of `ui` and `sole_prop` differ. One shifted draw
stream propagates into dozens of provisions.

Largest movers with the draws controlled, non-rebate years:

| column | max |Δ| | year |
|---|---|---|
| `scorp_passive_loss` | 6.1% | 2026 |
| `net_estate` | 4.5% | 2022 |
| `excess_bus_loss` | 3.2% | 2022 |
| `cdctc_nonref` | 2.3% | 2026 |
| `farm` | 2.0% | 2024 |
| `tips` | 1.6% | 2026 |

At the revenue level the effect is small — income tax 0.078%, payroll 0.60%,
the CBO comparison's own total 0.023% — but "small" is not the test. §4a is an
exact-equality test on purpose, because it is the only thing that can
distinguish a population change from a coding error.

## 5. What this means

**This was predicted.** The review brief's §9 records it under *what design A
does not fix*: "Tax-Data's Phase 1 draws stay positional, so appending seven
pools shifts every filer's imputed tips, overtime and consumption once more.
Keying those draws by id is a second, separable decision." The run promotes it
from a follow-up to a blocker, because you cannot certify "non-filer only"
while the filer side moves for an unrelated reason.

**It is not specific to Block E.** Any design that changes the record count
hits it: the DINA removal already moved the same families (consumption ≈−3%,
tips ≈−9% from commits-1-2 to the branch). Block E adds its own increment.
Design B, which keeps one cross-section, would avoid it only by giving up the
annual rebuild.

**Correction to an earlier claim.** A note on the `block-e` branch said the
filer side was bit-identical at 2017 and 2020. That holds for PUF-native
columns and weights, and not for imputed ones. It has been corrected in place.

## 6. The fix, and what it costs

Make each record's draw a function of its **id**, not its position: for module
*m* and record *i*, derive the draw from a stream seeded on `(id, m, seed)`.
Then the value a record receives does not depend on how many other records are
in the file, and the E2 tripwire becomes readable for any design.

It is needed in both repositories — `src/imputations/*.R` in Tax-Data, and
`config_parser.R` plus the positional `bind_cols` in Tax-Simulator — and **it
changes every published number once**, because every record's draw changes on
the switchover. That is a decision for the repo owners, not a cleanup, which is
why this pass measured it and stopped. The `rng-control` branch is the
measurement instrument, not a proposal to merge.

**Recommended order**: decide the id-keyed draws; make the change in both
repos as one coordinated vintage; re-run this battery, at which point §4a
becomes a real exact-equality gate rather than a tolerance argument.

## 7. Reproducing this

```bash
# Tax-Data, login node, minutes
Rscript research/state_weights/nonfiler_residual/05_preflight_vintage.R \
  --old=2026091118 --new=2026091119 --years=2017:2025 --full=2017,2020,2023

# Tax-Simulator, ~2 min each on 10 cores
Rscript src/main.R tests/nonfiler_ab_pre  NULL <user> 1 nf_pre  1 0 NULL 1 year
Rscript src/main.R tests/nonfiler_ab_post NULL <user> 1 nf_post 1 0 NULL 1 year
```

Compare `baseline/static/totals/1040.csv` between the two output vintages. For
the controlled version, run the same two from the `rng-control` worktree.

## Revision history

- **2026-09-12** — written from the first execution of the battery.
