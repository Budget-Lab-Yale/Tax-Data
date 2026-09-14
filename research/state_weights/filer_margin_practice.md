---
title: "The filing margin: how other modelers handle it"
role: notes
workstream: state_weights
status: open
updated: 2026-09-14
sot: research/state_weights/plan.md
supersedes: []
superseded_by: null
---

# The filing margin: how other modelers handle it

Survey commissioned 2026-09-14, ahead of the three-stage DINA vs design C
comparison whose stage 3 is a 2021-style EITC (Earned Income Tax Credit)
generosity expansion. Our simulator has no filing elasticity: `do_taxes.R`
switches a non-filer into filing only via `become_filer_rebate`
(`filer == 0 & rebate > 0`) or `become_filer_ctc`
(`filer == 0 & qual_ei == 0 & ctc_ref > 0`). **There is no EITC-induced
filing**, so the stage-3 run mechanically produces zero new filers. The
question was what everyone else does.

## 1. Four findings that drive everything

**No U.S. tax microsimulation model has a filing elasticity.** Not the Tax
Policy Center (TPC), the Congressional Budget Office (CBO), the Joint
Committee on Taxation (JCT), Treasury's Office of Tax Analysis (OTA), Penn
Wharton, ITEP or TRIM3. The published behavioural parameters across these
models are the elasticity of taxable income and the capital-gains realisation
elasticity, plus scattered provision-specific responses. Filing is not among
them. **We are not behind the field here; the field has not done this.**

**Exactly one model has a policy-responsive filing rule, and it is ours
generalised.** PolicyEngine's `tax_unit_is_filer` is the statutory filing
requirement OR refundable-credit eligibility × a filing propensity OR a
voluntary-filing propensity, where eligibility is literally
`(eitc + refundable_ctc) > 0` recomputed under the reform. That is
`become_filer_ctc | become_filer_rebate` with the EITC added and a propensity
gate. So extending our rule is not adopting a new architecture — it is
catching up to the one live precedent.

**A flat EITC take-up haircut would be the wrong instrument, and this is the
most useful finding in the survey.** The ~20% EITC participation shortfall is
a *filing* phenomenon, not a *claiming* one. IRS administrative data for
TY2022 puts claiming among apparently-eligible filers at **98%** (paid
preparer and software; only self-prepared paper runs low, at 57%), with the
whole filer-side gap at **$893M against $56B claimed — 1.6% of dollars**.
Census/IRS matched data for TY2021 puts **4.6 million of 5.7 million eligible
non-claimants in the non-filing category**, carrying **$5.6B of the $8.2B
unclaimed**. A haircut on modelled filers would discard a credit that filers
in fact claim at 98%, and would leave the real shortfall exactly where it is:
outside the filing population, invisible.

**Our asymmetry runs backwards relative to the evidence.** We let a rebate and
the CTC induce filing and the EITC not. The EITC is the credit most associated
with filing among the eligible-but-not-filing population. If the asymmetry is
intentional it should be stated; it currently reads as an accident of which
provisions got attention.

## 2. What the other models actually do

| Model | Filing decision modelled? | Responds to policy? | Refundable-credit take-up |
|---|---|---|---|
| **TPC** | Historically Cilke (1998) probits below the threshold; the current (2022) description drops this and says only that the match "generates a sample of individuals who do not file" | Not documented | Not documented for EITC/CTC; take-up rates used only for education incentives |
| **CBO** | Only as an imputation device — statistical match by predicted income. Mok (2017) evaluates a probit alternative but records that the match "is that method used in CBO's individual income tax model" | No | In one illustrative direct-payment analysis, "all eligible nonfilers are estimated to sign up" — full take-up, stated as a simplification |
| **JCT** | No published behavioural filing model; non-filers built from information returns with no matching 1040 plus imputed "automatons" | Not documented | Implicit in the weights — EITC claim counts by number of children are an extrapolation target |
| **OTA** | No — but the gap is explicitly acknowledged: "Changes to refundable credits or the standard deduction could change who files" | No | Observed non-claiming is preserved; "analysts may decide to model a change in take-up if appropriate" (off-model, case by case). Also states "the ITM assumes that compliance costs are zero" |
| **Tax-Calculator** | No filer variable in the tax logic at all | No | **Yes, since 6.7.0 (June 2026)** — a stochastic per-record claim probability; see §3 |
| **PolicyEngine** | **Yes — the only one** | **Yes** | `takes_up_eitc` by number of children: 0.65 / 0.86 / 0.85 / 0.85 |
| **ITEP** | Mechanism unpublished | Not documented | Nothing in baseline methodology; applied ad hoc per report (~75–80% assumed in a 2024 analysis) |
| **TRIM3** | **No, explicitly** — "does not attempt to simulate the fact that some units do not file" | n/a | **EITC: full take-up, explicitly.** Other credits do get participation probabilities tuned to published claim counts |

TRIM3 publishes the cleanest general principle: participation must be
simulated *when the number found eligible exceeds the number who actually
participate per administrative data*. Its simulated EITC already runs **below**
administrative totals (71.1% of returns, 59.5% of dollars in 2021), so a
haircut would move it further from target — whereas Tax-Calculator's PUF-based
population ran at **122%** of administrative dollars under full take-up. **A
take-up rate is only meaningful relative to a well-specified eligible
population**, which is the argument for the administrative-residual anchoring
the ASEC pool already uses.

## 3. The only real parameters anyone publishes

**Tax-Calculator** (`policy_current_law.json`, verified at HEAD):
`eitc_claim_prob_scale` 1.03, `eitc_claim_prob_min` 0.40,
`actc_claim_prob_scale` 1.1, `actc_claim_prob_min` 0.0. The probability is
`max(min, credit / the unit's own maximum credit) * scale` — so 41.2% for a
tiny credit rising to 100% near the maximum — drawn as a per-record Bernoulli
against a **deterministically seeded** uniform, identical across baseline and
reform so a reform never spuriously flips a unit. Calibration gap closed on
2022 data: EITC $60.1B actual vs $73.5B under full claiming. The floor exists
because scale alone under-counted claimant numbers by ~8% while hitting
dollars — **a two-moment calibration is required.**

**PolicyEngine**: the take-up rates above, plus a 16-cell voluntary-filing
propensity table (0.0037 to 0.60). **Documented gap worth not replicating:**
`would_file_if_eligible_for_refundable_credit` is described as assigned during
microdata construction and is never actually assigned anywhere in their data
repo — it silently falls back to `TRUE`, so shipped behaviour is full response.

**TRIM3**: participation rules exist for the elderly and child-care credits;
numeric values are behind a login and were not verified.

## 4. The literature a reviewer will expect

- **Coleman et al. (2024), Census CES-WP-24-75** — the current state of the art
  and the right anchor. TY2021: 5.7M non-claimants, **4.6M never filed**;
  $8.2B unclaimed = $5.6B non-filers + $1.3B non-claiming filers + $1.3B
  under-claimants. Only 19% of non-claimants filed at all.
- **Plueger (2009), IRS Research Bulletin** — TY2005 participation 75.3%;
  **two-thirds of non-participants are non-filers**. His Table 11 is
  effectively an empirical take-up function by credit size: 42% at $1–99
  rising to 90% at $4,000+.
- **Schafer, Cornwall & Yeh (2024), IRS** — the filer-side gap, 98% claiming
  by preparer/software vs 57% self-prepared paper.
- **Ramnath & Tong (2017), AEJ: Policy** — the closest thing to a filing
  elasticity in existence and the empirical warrant for our `become_filer_rebate`
  rule. Regression discontinuity at the $3,000 threshold on 1.2M long-term
  non-filers: eligibility for a $300–$1,200 rebate raised filing by **2.2
  percentage points**. Small response to a large, heavily publicised payment —
  but filing is near-absorbing afterwards (IV effects 0.65–0.96 through 2014).
- **Goldin et al. (2022), JPubE** — IRS outreach letters: filing +0.74pp on a
  21.5% control mean; among marginal filers 43.4% claimed EITC.
- **Linos, Prohofsky, Ramesh, Rothstein & Unrath (2022), AEJ: Policy** — six
  pre-registered RCTs, N≈1M, **no detectable effect**, ruling out 0.5pp or
  larger. Pair with any nudge result.
- **Bhargava & Manoli (2015), AER** — note this is the *claiming* margin, not
  filing: the sample is filers who had already ignored one notice.
- **Ko & Moffitt (2024)**, verbatim: "Non-takeup in the EITC program is mostly
  from not filing taxes in the first place."

**The 2021 CTC natural experiment is the most direct evidence on magnitude.**
GetCTC converted 115,451 households in 2021 and 79,758 in 2022 — under 200,000
across both years — while CBPP estimated ~3.9M children off the tax rolls. Its
2022 EITC pilot is the sharpest burden result anywhere: of 490,026 clients
offered EITC and 66,714 who started, **1,079 successfully submitted**. A
policy that was large, automatic, prepaid monthly, nationally advertised and
supported by two dedicated portals still left ~4M children unreached. **Any
filing response we build should be small in the year of enactment and
persistent afterwards, not large and transitory.**

## 5. Options, least to most invasive

Framing: **our current construction is already a filing rule keyed on the
reform-state value of a refundable credit** — the same shape as PolicyEngine's.
The question is whether to extend it to the EITC, not whether to adopt a new
architecture.

**(i) Document and report mechanically.** What CBO, JCT, OTA, TPC, Penn
Wharton and ITEP effectively all do. Cost: one paragraph. Done — the
limitation is now stated at `do_taxes.R`'s filing rule. Weakness: the
rebate/CTC-yes, EITC-no asymmetry is backwards and a reviewer will see it.

**(i-b) Report the eligible-but-non-filing population as a separate line.**
Before adding any behaviour, report how many ASEC pool records would be
EITC-eligible under the expansion and the dollars involved, as an explicitly
un-modelled "potential reach" figure, benchmarked against CES-WP-24-75's 4.6M
units / $5.6B. **One query, gives the reader the sensitivity bound, and
doubles as a validation check on the pool itself. Recommended regardless of
what else we do.**

**(ii) A static take-up haircut — do not do this in the obvious form.** See §1.
The defensible version is Tax-Calculator's: probability rising with credit size
relative to the unit's own maximum, floored ~0.40, seeded stably across
baseline and reform, calibrated on Plueger's Table 11 and constrained by the
IRS TY2022 filer-gap total. Costs: Monte Carlo noise in a deterministic
pipeline, and a two-moment calibration.

**(iii) Extend the rule: `become_filer_eitc` with a propensity.** The
recommended option. Minimal edit to existing architecture; puts the behaviour
where the evidence says it lives; has a live precedent whose documented bug we
can avoid; and our pool is already built on the right primitives — a Mok (2017)
probit below the threshold (the same CBO working paper CBO itself cites) and a
Pub 5785 hazard above it. Calibration targets in preference order: Ramnath &
Tong's 2.2pp; Plueger's Table 16 by schedule position (childless phase-in 46%,
plateau 64%, phase-out 63%); CES-WP-24-75's TY2021 level. Open design choices:
whether the propensity is a fixed record attribute or a function of the
expected refund, and whether to assume the multi-year absorption Ramnath & Tong
find (Guyton et al. find no persistence for *nudged* filers, so do not assume
it silently).

**(iv) Estimate an elasticity — a research paper, not a model feature.** There
is no published estimate of a filing response to EITC *generosity*. What exists
is a rebate level effect, outreach experiments identifying an
information/assistance response rather than a price response, and Neumark &
Williams's imprecise state-EITC result. If the margin turns out to drive a
headline, the right answer is a sensitivity band across the option (iii)
propensity, not a point estimate nobody else has.

## 6. What could not be verified

TPC: whether the Cilke probit is still in use, and whether any EITC/CTC
take-up adjustment is applied in practice. JCT: whether the ITM assumes
less-than-100% claiming (a Tax-Calculator issue asserts it; JCX-75-15 does not
say so). OTA: what the calculator does with a non-filer under a reform. ITEP:
the filing-decision mechanism entirely. TRIM3: the numeric participation
values. PolicyEngine: where, if anywhere, the filing propensity is assigned.

## Revision history

- **2026-09-14** — written, ahead of the stage-3 EITC run.
