---
title: "Minimal example, step 2: the shape of the model (for review before it is written)"
role: method
workstream: rebuild
status: current
updated: 2026-10-14
sot: research/rebuild/design/plan.md
supersedes: []
superseded_by: null
---

# The shape of the model

This note says what the step 2 script will build, so John can object before
any of it exists. Step 1 (the stacked table) is described in `01_REPORT.md`.

## What a record is to the model

A record is a short list of slots. Each slot is one variable and holds one
token:

| Slot | Tokens | Role |
|---|---|---|
| source | `puf`, `scf` | context, always given, never predicted |
| year | `2015` (later `2018`, `2021`) | context, always given, never predicted |
| married | 0, 1 | predicted |
| n_dep | 0, 1, 2, 3 | predicted |
| wages, interest (taxable only), dividends, capital_gains | band integers from step 1 | predicted |
| other_agi | band integers | predicted for PUF records only; absent from SCF records |
| net_worth | band integers | predicted for SCF records only; absent from PUF records |

Absent means the slot is left out of a PUF record entirely. It is not a
missing value to be guessed and it is not masked. The model never sees a
PUF record with net worth and never learns to predict net worth for one;
when we later generate a PUF-style record with net worth, that is the
model answering a question it was only ever trained on from the SCF side.
That is the label-indifference question in step 6.

Added 2026-10-14, after the first table was built: `other_agi` is the rest
of AGI (AGI minus wages, taxable interest, dividends and capital gains). It
is PUF-owned and absent from SCF records, the mirror image of net worth.
AGI is never a variable; for PUF-style records it is the sum of the five
parts, computed after dollars are drawn, which is what makes the IRS tables
by AGI bracket usable as targets and for validation.

## The network

A small encoder transformer. Each slot becomes a vector: the sum of an
embedding for which variable it is and an embedding for its token. The
slots pass through four transformer layers of width 128 with four heads.
Each predicted slot has an output head giving a probability over its own
tokens (two for married, four for n_dep, the band count for amounts).
About half a million parameters. The earlier tests used a model ten times
this size; the minimal example starts small and grows only if the
held-out numbers say it should.

## Training: masked any-subset

For each training record, pick a random share of the predicted slots to
hide (the share is drawn uniformly between a few percent and everything),
replace their tokens with a `mask` token, run the network, and score the
hidden slots by cross-entropy against their true tokens. Over many passes
the network learns every conditional: married given the incomes, dividends
given wages and net worth, and so on. The all-hidden case teaches it the
unconditional distribution, which is what generation starts from.

Weights. The first run uses the survey weights as they are, normalized
within each source so that each source's weights sum to its record count.
So within a source, records count by their weight; across sources, each
source counts by how many records it has. This is deliberately the plain
rule, so that the D26 rule (weights within income groups, counts across
groups) can be run second and compared to something simple.

Held-out split from step 1. Training stops when validation loss stops
falling; the test split is scored once at the end.

## What comes out of step 2 (before any generation)

1. Held-out cross-entropy in nats per predicted variable, per source,
   at three hidden shares: one slot hidden (pure conditional), half hidden,
   everything hidden (marginal).
2. Conditional tables from the model against the held-out records: the
   share with any dividends by wage band and married status; the share with
   capital gains by dividend band; n_dep by married status; for the SCF,
   net worth band by wage band. Model columns come from the predicted
   probabilities, data columns from the held-out weighted counts.
3. A label swap: each held-out PUF record scored under the `scf` source
   token and the reverse, same hidden slots. The change in cross-entropy
   and in the dividends table is the first label-indifference number.

## Not in step 2

Generation order, the rank-dollar map, dollars, targets, the tilt. Those
are steps 3 to 5 and each gets its own note.

## Open for John

- The plain weighting rule for the first run, with D26 as the second run.
- Model size: start at four layers, width 128.
- Whether year should be in the model from the start, when there is only
  one year. It is harmless (one token, always the same) and avoids a
  change later; proposed yes.

## Revision history

- 2026-10-14: written, before step 2 code exists. Same day: other_agi added, interest narrowed to taxable.
