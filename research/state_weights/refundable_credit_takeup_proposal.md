---
title: "EITC and CTC take-up: what we model, and what to report"
role: notes
workstream: state_weights
status: open
updated: 2026-09-14
sot: research/state_weights/plan.md
supersedes: []
superseded_by: null
---

# EITC and CTC take-up: what we model, and what to report

Short companion to [`filer_margin_practice.md`](filer_margin_practice.md),
which is the evidence base. This note states what our model does with
refundable-credit take-up today, what the administrative evidence says, and a
concrete proposal for the results we publish. It proposes **no behavioural
change** — only that we report what the model can and cannot see.

## 1. What we do now

**Full take-up among modelled filers.** A record with `filer == 1` that
qualifies receives the whole EITC (Earned Income Tax Credit) or refundable CTC
(Child Tax Credit); there is no claiming probability anywhere.

**A filing rule covering two provisions.** `do_taxes.R` switches a non-filer in
only via `become_filer_rebate` (`filer == 0 & rebate > 0`) or
`become_filer_ctc` (`filer == 0 & qual_ei == 0 & ctc_ref > 0`). There is no
EITC arm, no elasticity, no filing cost.

**Everything is then filer-gated.** `get_1040_totals()` computes every credit
as `sum(. * weight * filer)`, so a credit accruing to a record that stays
`filer == 0` is computed and then multiplied by zero. It is invisible, not
absent.

## 2. What the evidence says

The headline participation rates are **not** a claiming story.

| | EITC | CTC / ACTC |
|---|---|---|
| participation, units | 78–81% (TY2019–21) | **93%** (TY2020) |
| participation, dollars | 81–88% | **91%** |
| share of non-participants who **never filed** | **81%** (4.6M of 5.7M, TY2021) | **61%** |
| claiming **conditional on filing** | **96–98%** | — |

Sources: Coleman et al. (2024) Census CES-WP-24-75 and companion -76; Schafer,
Cornwall & Yeh (2024) IRS. The EITC filer-side gap is **$893M against $56B
claimed — 1.6% of dollars** — and is concentrated in self-prepared paper
returns (57% claiming, against 98% for preparer and software returns).

**So the shortfall lives at the filing margin, not the claiming margin.** Ko &
Moffitt (2024), verbatim: *"Non-takeup in the EITC program is mostly from not
filing taxes in the first place."*

Two consequences for us:

- **A flat take-up haircut on modelled filers would be wrong**, not merely
  crude. It would discard ~20% of a credit that filers claim at 96–98%, and
  leave the actual 4.6M-unit shortfall exactly where it is — outside the
  filing population.
- **Our full-take-up assumption is close to right for the population we
  model.** The error is not in what we give our filers; it is in who counts as
  a filer.

## 3. Proposal: report refundable credits at three margins, not one

Publish, for each refundable credit, in every run that changes one:

| margin | definition | status today |
|---|---|---|
| **claimed** | accrues to `filer == 1` | what we publish |
| **induced** | accrues to records that switched via `become_filer_*` | computed, then folded invisibly into *claimed* |
| **un-modelled reach** | accrues to records still `filer == 0` under the reform | computed, then multiplied by zero |

**Why each earns its place.**

*Induced* is nearly free — `become_filer_rebate` and `become_filer_ctc` already
exist in the frame and are simply not carried to the output. Reporting it makes
our one existing behavioural response visible and auditable instead of a silent
component of a filer total. It is also the only number we have that is directly
comparable to the outside evidence (Ramnath & Tong's 2.2pp).

*Un-modelled reach* is the number that makes a mechanical run honest. It says
what the estimate cannot see, in dollars, rather than leaving the reader to
infer that zero induced filers means zero forgone credit. Benchmark it against
the TY2021 administrative figures above: **4.6M non-filing eligible units,
$5.6B unclaimed**. If our reach line is wildly off that, the non-filer pool has
a problem — so this doubles as a validation check on the pool itself.

**Implementation.** Add `become_filer_ctc` and `become_filer_rebate` to
`detail_vars`, and in `get_1040_totals()` compute each credit three ways
instead of once — `* filer`, `* become_filer_*`, and `* (1 - filer)`. This is a
reporting change in one function; no calculation changes and no published
number moves.

**Stage-3 framing that follows.** The 2021-style EITC run reports the
distributional effect conditional on current-law filers, the induced total
(which will be zero for an EITC-only reform, by construction), and the
un-modelled reach. The gap between the second and third is the size of what a
filing response would have to explain — stated as a bound, not modelled.

## 4. What this note does NOT propose

**No claiming haircut** — §2. If one is ever wanted, the defensible form is
Tax-Calculator's (claim probability rising with credit size relative to the
unit's own maximum, floored ~0.40, seeded stably across baseline and reform),
not a flat rate, and it needs a two-moment calibration on dollars *and*
claimant counts.

**No filing elasticity** — no U.S. model has one, and there is no published
estimate of a filing response to EITC *generosity*. If the margin later drives
a headline, the answer is a sensitivity band across a filing propensity, not a
point estimate.

**No change to the asymmetry yet.** That we let a rebate and the CTC induce
filing and the EITC not runs backwards relative to the evidence. Adding
`become_filer_eitc` with a record-level propensity is the recommended fix in
`filer_margin_practice.md` §5(iii), and it is a modelling decision for JI —
not something to slip in alongside a reporting change.

## Revision history

- **2026-09-14** — written, alongside the filer-margin survey.
