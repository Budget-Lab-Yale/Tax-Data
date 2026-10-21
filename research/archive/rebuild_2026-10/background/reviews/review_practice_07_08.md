---
title: "Review of the practice reports 07 (US tax models) and 08 (other fields)"
role: review
workstream: rebuild
status: current
updated: 2026-10-07
true_as_of: 2026-10-07
sot: research/rebuild/BRIEF.md
supersedes: []
superseded_by: null
---

# Review of the practice reports 07 and 08

Reviewed: `research/rebuild/background/07_practice_tax_models.md` and
`research/rebuild/background/08_practice_other_fields.md`, as they stood on
6–7 October 2026. Every grade below rests on a primary source I opened
myself: cloned repositories, GitHub's API, PDFs downloaded and converted to
text, or the live web page. I did not edit either report.

## Summary

**Claims checked: 62 graded, plus 1 that the reports do not make.**

| Grade | Report 07 | Report 08 | Total |
|---|---|---|---|
| CONFIRMED | 28 | 16 | 44 |
| OVERSTATED | 9 | 2 | 11 |
| WRONG | 2 | 3 | 5 |
| UNSUPPORTED | 0 | 1 | 1 |
| UNCHECKABLE | 1 | 0 | 1 |
| Not stated in the report | 1 | 0 | 1 |

I found no invented documents, invented events or invented numbers. Every
document, release and incident I was asked to check exists: the PolicyEngine
advisory, the CRS report R49052, the Budget Lab page, the JCT and Treasury
documents, the *Journal of Public Economics* article, the HMRC table and the
PSZ passage. The errors are of three kinds: a detail misattributed, a fact
that has gone out of date, and a summary bullet that says more than the body
or the source supports.

**Errors that matter if John repeats them in public:**

1. **PolicyEngine's deduction blow-up was in charitable deductions.** Report
   07 says "itemized deductions reached about nine times SOI". The advisory
   says "modeled charitable deductions ran ~9× the SOI level". It describes
   itemized deductions in general only as "inflated".
2. **Populace's weight cap is now 5 times, not 50.** The 50× figure comes
   from the June advisory. The generated US calibration specification has set
   `max_weight_ratio: 5.0` since 17 August 2026, and static aging also uses 5.
   A 50.0 default survives in one build-configuration class.
3. **The report's cause for the PolicyEngine failure is its own inference.**
   The advisory blames a "single flat-weighted loss" and the lack of a cap on
   per-record weight growth. It does not mention the missing distance term.
   That the Enhanced CPS loss had no such term is true; that this caused the
   failure is the report's reading.
4. **Two summary generalisations in report 07 are too strong.**
   - "Every documented scorer treats targets as exact constraints." The
     report's own body contradicts this: TMD has ±0.5% bands with slack, PWBM
     uses tolerance bands, and PolicyEngine uses a soft loss.
   - "No documented method fits ... by simulating who itemizes." JCT's 2023
     documentation lists, among its yearly targets, the number of itemizers
     claiming mortgage interest and charitable giving split by itemizers and
     non-itemizers. Its calculator decides who itemizes under each year's
     law. TMD's state weights target the post-cap SALT deduction that
     Tax-Calculator computes. Neither fits SOI's post-2017 tables directly,
     but both are law-dependent targets of the kind the brief describes.
5. **Canada's SPSD/M is now at version 34.0, database year 2022.** That
   version was released 12 February 2026. Report 08 calls version 30.1.1
   (database year 2018) "the current release". Statistics Canada lists 30.1.1
   as archived.
6. **The PSZ non-filer finding is quoted correctly, but its conditions
   matter before it is applied to Tax-Data's pool.** The passage is in the PSZ
   Data Appendix, Appendix B.1, pages 20–21 (1 April 2019 version).
   - PSZ's CPS non-filers were built with Tax Policy Center filing rules,
     under which every unit with children and positive labour income is
     assumed to file. That rule pushes wage earners out of the non-filer pool.
   - The IRS comparison group includes people who should have filed and did
     not.
   - The two factors of 2 multiply: aggregate non-filer wages rose about
     fourfold.
   - Report 08 reverses the order of PSZ's steps. PSZ doubled wage earners
     before the age reweighting, not after.

   Tax-Data's pool uses different filing models, so the factor of 2 should not
   be carried over without its own comparison.

**Overall judgement.**

- **Report 07 is careful and mostly accurate.**
  - The PolicyEngine, TMD, taxdata, JCT, Treasury and TPC sections match the
    code and documents closely, down to constants and quotations.
  - Its weaknesses are in the summary, which generalises past the evidence,
    and in a few PolicyEngine figures that are out of date.
  - One figure the report marks as unverified is in fact verified: CRS
    footnote 19 says both models' income tax runs "roughly 9% below CBO's".
- **Report 08 is accurate on its central sources.** These are Realtime
  Inequality, HMRC, PSZ, BEA, PopulationSim and CAPITA.
  - It has one substantive factual error (the SPSD/M version) and two minor
    ones.
  - Two summary bullets go beyond their sources: the claim that PSZ change
    the shape of the distribution only for wages, and the claim about what
    reduces the recession bias.
  - Sections it marks "not verified" (TAXBEN, TAWA, MINT, CBOLT, HISIM2) are
    labelled honestly. I did not check them.

## Claims, report 07 (US tax models)

| # | Claim in the report | Grade | Evidence |
|---|---|---|---|
| 07-1 | The Enhanced CPS failed in production from 19 May to 12 June 2026. Total AGI reached $23.9T against SOI's ~$15.5T, and the $100M+ band went from $0.4T to $7.5T. | CONFIRMED | Advisory by Max Ghenis, 19 June 2026, read through GitHub's GraphQL API: AGI "~$14T ... ~$19.7T (05-19) and ~$23.9T (05-20) — against an SOI level of ~$15.5T"; $100M+ band "~$0.4T to ~$7.5T". https://github.com/PolicyEngine/policyengine.py/discussions/429 |
| 07-2 | "Itemized deductions reached about nine times SOI." | WRONG | The advisory says "top-concentrated itemized deductions inflated — modeled charitable deductions ran ~9× the SOI level". The ninefold figure is for charitable deductions only. Same URL. |
| 07-3 | PolicyEngine retired the Enhanced CPS for Populace. | CONFIRMED | Advisory: "The certified US dataset is now Populace"; the bundle moved at 4.16.1 on 12 June, and `policyengine-us` moved at 1.739.1. `policyengine-us-data` is archived (GitHub API: `archived: true`, last push 2 July 2026). **Omission:** on 22 June a PolicyEngine developer (anth-volk) commented under the advisory that its statement about the v2 simulation API and policyengine.org "is not correct" and that the API was still on the old bundling. The window for policyengine.org may therefore have been longer than the report states. |
| 07-4 | The Enhanced CPS fitted weights with no term for distance from the prior "and it failed", implying the first caused the second. | OVERSTATED | That the loss had no such term is confirmed (`enhanced_cps.py`: `total_loss = l_main + l0_lambda * gates.get_penalty()`, `l0_lambda=0.0`). The advisory names two causes: a "single flat-weighted loss" (issue #1107: aggregate targets "drowned out") and no cap on weight growth. It does not mention a distance term. Linking the failure to the missing distance term is the report's inference. https://github.com/PolicyEngine/policyengine-us-data/issues/1107 |
| 07-5 | Populace weights each target by the square root of its value. | CONFIRMED | `packages/microcosm-build/src/microcosm/build/us/spec/calibration.yaml` (commit 581b569): `raw_value_weight: max(abs(target_value), 1) ** value_power`, `value_power: 0.5`. Further normalisations follow: concept-group budgets and an equal split between amount and count targets. https://github.com/PolicyEngine/populace |
| 07-6 | Populace caps each record's weight at 50 times its start. | OVERSTATED (out of date) | The June advisory said 50×. The current US spec has `max_weight_ratio: 5.0` in its hard constraints and in both refit contracts. It has held that value in every version since the spec was generated on 17 August 2026 (commits f85c248, b7922b0). `docs/static-aging.md` also uses a default of 5. A `max_weight_ratio: float = 50.0` default remains in `us_runtime/__init__.py`. |
| 07-7 | Populace does not target projected income; quoted "can no longer be checked". | CONFIRMED | `docs/static-aging.md`: "It targets demographics only. A program count or an income total is an output of rules, take-up and demographics; a weight vector forced to match one can no longer be wrong about it, so it can no longer be checked." |
| 07-8 | Populace's loss caps each target's miss at 1,000%. | OVERSTATED | The solver's default is `_DEFAULT_TARGET_LOSS_CAP = 10.0` (1,000%) in `solve.py`. The production US spec sets `target_loss_cap: 1.0` (100%). |
| 07-9 | Populace's default penalty mean((w/d)²) pulls toward d², and a chi-square option exists. | CONFIRMED | `solve.py` docstring: "Its mass-conserved, target-free minimizer is `w ∝ anchor ** 2`"; `"chi_square"` is "GREG's anchor-weighted chi-square distance ... penalized ('ridge') calibration". **Addition:** the production US spec sets `l2_lambda: 0.0` and `l2_anchor_weights: null`. As configured, the US build therefore has no pull toward the prior beyond the 5× cap. |
| 07-10 | The microcalibrate loss has no term for distance from the prior weights. | CONFIRMED | `src/microcalibrate/utils/metrics.py` (commit 93e9c2a), `loss()`: mean of `(((estimate − target) + 1)/(target + 1))²`, times an optional normalisation. The only extra term is an optional L0 gate penalty (`reweight.py`). Dropout defaults to 0. https://github.com/PolicyEngine/microcalibrate |
| 07-11 | Enhanced CPS settings: 1,000 Adam steps at learning rate 0.2; prior of 0.95 for CPS and 0.05 for PUF copies; multipliers of 1,000 and 100,000. | CONFIRMED | `enhanced_cps.py`: `epochs=500`, `trange(epochs * 2)`, `lr=0.2`, `PUF_CLONE_PRIOR_TOTAL_SHARE = 0.05`, "the earlier bug starved them". `utils/loss.py`: `BEA_..._LOSS_WEIGHT = 1_000.0`, `HOUSEHOLD_COUNT_LOSS_WEIGHT = 100_000.0`. |
| 07-12 | About 13,000 targets, chosen from about 37,000. | CONFIRMED | `docs/methodology.md`: "Why ~13,000 calibration targets (~3,000 in the legacy national calibration) ... We have ~37,000 total targets in our database." **Minor omission:** the geography fit also carries an L2 term on weight size (`LAMBDA_L2 = 1e-12`, `unified_calibration.py`), not only the L0 penalty the report lists. |
| 07-13 | Paper draft: "over 7,000" targets and 72 imputed variables. | OVERSTATED | Both figures appear in `paper/`. PolicyEngine's own CHANGELOG corrected them: "Corrected calibration target count from 7,000+ to 2,813 across all docs" and "Corrected number of imputed variables from 72 to 67". The report quotes the stale figures without saying they were withdrawn. |
| 07-14 | The legacy PUF imputation used 7 demographic predictors and no income variable. | CONFIRMED | `calibration/puf_impute.py`: `DEMOGRAPHIC_PREDICTORS` = age, is_male, tax_unit_is_joint, tax_unit_count_dependents, is_tax_unit_head, is_tax_unit_spouse, is_tax_unit_dependent; `PUF_SUBSAMPLE_TARGET = 20_000`; `PUF_TOP_PERCENTILE = 99.5`. |
| 07-15 | TMD since 28 February 2026: quadratic program solved by Clarabel; ±0.5%; multipliers in [0.1, 10]; slack penalty 10⁶; the same tolerance for every target. | CONFIRMED | Commit 082133e (28 February 2026), "Add Clarabel QP solver as default reweighting method". `tmd/imputation_assumptions.py`: `REWEIGHT_MULTIPLIER_MIN = 0.1`, `_MAX = 10.0`, `CLARABEL_CONSTRAINT_TOL = 0.005`, `CLARABEL_SLACK_PENALTY = 1e6`. `tolerance_overrides` is used only in a test. I did not count the 550 targets. https://github.com/PSLmodels/tax-microdata-benchmarking |
| 07-16 | (Item from the review brief.) "TMD version 2.2.1 of 28 September 2026 ... SALT targeted after the cap." | Not stated in report 07 | Report 07 names neither 2.2.1 nor any SALT targeting. The facts: commit 7ae7a93, "Prepare TMD release 2.2.1", dated 28 September 2026; `setup.py` says `version="2.2.1"`; the repository has no GitHub release object. National reweighting has no SALT target. The **state** area weights target `c18300`, the post-cap SALT deduction computed by Tax-Calculator ("Actual SALT deduction (after cap)", `state_variable_mapping.csv`; `AREA_WEIGHTING_LESSONS.md`, "Use actual SALT (c18300, the post-$10K-cap amount)"). The specification also keeps a Census-share target for uncapped `e18400` in all bins combined. The QP, ±0.5% and 0.1×–10× parts are confirmed under 07-15. |
| 07-17 | TMD: population-only weight growth had left revenue 5.1% below CBO by 2035. | CONFIRMED | `tmd/storage/input/cbo26_returns.yaml`, added in commit 1c02462 (8 September 2026): "reaching -5.1% on revenue by 2035". |
| 07-18 | TMD was started in February 2024 by Nikhil Woodruff. | CONFIRMED | Repository created 6 February 2024; the earliest commits are by Nikhil Woodruff, 8 February 2024. |
| 07-19 | Tax-Calculator 6.5.0 released 29 March 2026. | WRONG (minor) | GitHub release published 30 March 2026, 12:41 UTC; tag commit dated 30 March. |
| 07-20 | JCT's objective is x⁴ + x⁻⁴ − 2, with K, S and α/β chaining (JCX-75-15 Appendix). | CONFIRMED | JCX-75-15, Appendix, pp. 52–55: penalty "calculating the ratio and inverse ratio of the extrapolated weight to the original weight; raising both values to the fourth power, and then subtracting two"; Newton–Raphson; K set to 0 for weights below 2.0; Table A-1 has α running from 1.0 to 0.2. **Caveats:** footnote 78 says S equalled 1 for every return in 2014, and Table A-1 is the 2014 extrapolation. JCX-48-23 (2023), footnote 60, points back to this appendix. https://www.jct.gov/getattachment/0308b4da-ff48-4e9e-92f6-3592eb6a45ac/x-75-15-4776.pdf |
| 07-21 | JCT uses about 100 targets. | CONFIRMED | JCX-48-23, p. 37: "more than 100 economic and demographic baseline variables". https://www.jct.gov/getattachment/3f7eb8cb-f024-4b37-814d-3050e47c5e43/x-48-23.pdf |
| 07-22 | Treasury uses the same penalty, with 84 targets. | CONFIRMED | TP-12 §4.2: "There are 84 targeted variables", 35 counts and 49 dollar amounts. Distortion function Φ(x) = x⁴ + x⁻⁴ − 2; "five to ten iterations"; the "balloon" passage is verbatim. https://home.treasury.gov/system/files/131/TP-12.pdf |
| 07-23 | JCT's 2023 base is the 2019 INSOLE file, 360,033 records. | CONFIRMED | JCX-48-23, pp. 2–5: "SOI Division's 2019 INSOLE data file"; "Each of the 360,033 sampled records". |
| 07-24 | JCT's January 2025 slides still describe the 2019 base. | CONFIRMED (by inference) | The slides do not say "2019". Page 10: "The 2025 Individual Tax Model uses a detailed representative sample of more than 360,000 actual income tax returns filed by U.S. taxpayers in 2020." Returns filed in 2020 are tax-year 2019 returns, and the count matches. https://www.jct.gov/getattachment/29cf59ce-31ca-450b-adea-3848d0d33350/Revenue-Estimating-Process-January-2025.pdf |
| 07-25 | JCT links 19 kinds of information return. | CONFIRMED | JCX-48-23, p. 7, lists 19 forms, from 1095-A to W-2G, plus "Other information returns are added on an as-needed basis". |
| 07-26 | JCT imputes deductions for non-itemizers by matching in 385 bins. | CONFIRMED | JCX-48-23, p. 20: "one of 385 unique combinations". |
| 07-27 | TPC, PWBM, taxdata and the Tax Foundation use O'Hare's linear program. | OVERSTATED (for TPC only) | **taxdata:** `puf_stage2/solver.jl`, `@objective(model, Min, sum(r[i] + s[i]))`. **Tax Foundation:** ModelMeth26 says it follows "the extrapolation approach developed in TaxData ... initially developed by John O'Hare", using a linear program (https://taxfoundation.org/wp-content/uploads/2026/07/ModelMeth26.pdf). **PWBM:** Ricco (2020) per the report; John's own paper, not re-checked. **TPC:** the July 2026 page says only "a linear programming algorithm" and gives no objective. The O'Hare objective is documented for TPC only in Rohaly, Carasso and Saleem (2005). |
| 07-28 | "O'Hare's own memo says most records end up unchanged and the rest move to the cap." | OVERSTATED | The memo (taxdata `docs/book/content/methods/Stage1_Stage2_Methodology.md`) says "z_i will be zero for most records". It says nothing about the remaining records sitting at the cap. That half is a general property of linear programs, not the memo's statement. |
| 07-29 | taxdata's caps: 0.30–0.40 for 2012–16, 0.45 for 2017–32, 0.50 after. | CONFIRMED | `solver.jl` `tol_list`. |
| 07-30 | TPC page of 31 July 2026: two databases; the 2015 PUF has 207,696 records; the model "currently uses" the 2006-PUF-based file; aggregate records replaced from the 2001–2007 PUFs. | CONFIRMED | Live page read through a text proxy (direct fetch returns 403): "Updated on July 31, 2026"; "207,696 records"; "TPC's model currently uses an alternative database we created with the 2006 PUF"; "several thousand of the most extreme records from the 2001-2007 PUFs". https://taxpolicycenter.org/resources/tax-model-resources/brief-description-tax-model |
| 07-31 | Treasury Paper 62 (1989): 1981 file aged to 1983 against actual 1983 SOI; Theil information gain; "impossible to improve ... by simply adding more targets"; "degrees of freedom". | CONFIRMED | Gillette, OTA Paper 62, February 1989, pp. 11 and 19: reweighting alone gave "major changes in the weights ... rejected"; non-convergence when more income targets were added. The report's word "fixed" for the inflation step is slightly strong: the paper says it "significantly reduced" the weight changes. https://home.treasury.gov/system/files/131/WP-62.pdf |
| 07-32 | CRS R49052, 30 July 2026, compares Tax-Simulator with Tax-Calculator 6.7.2; income tax about 9% below CBO. | CONFIRMED | Title and date as stated; "Tax-Simulator (Commit 18a25c8f0)" and "Tax-Calculator (Release 6.7.2)"; "typically the IRS's 2015 Public Use File [PUF], supplemented and aged forward". Footnote 19: "both models' individual income tax projections run roughly 9% below CBO's". The report marked this unverified; it is verified. The passage on gaps from JCT illustrates with the capital gains rate and §199A. NIIT appears elsewhere in the report as a divergence. https://www.everycrsreport.com/reports/R49052.html |
| 07-33 | Budget Lab methodology page, April 2024. | CONFIRMED | "Tax Microsimulation at The Budget Lab, April 12, 2024". O'Hare / Ricco (2020) aging to 2017; for 2018–19, weights by marital status and age plus per-filer scaling; 2020–23 scaled to SSA, CBO, NIPA mortgage interest and Giving USA; non-filers "from their 2017 public-use microdata file" (Piketty et al.). https://budgetlab.yale.edu/research/tax-microsimulation-budget-lab |
| 07-34 | The Tax-Data repository's `research/` tree and decisions log are public on GitHub. | CONFIRMED | `git remote`: `git@github.com:Budget-Lab-Yale/Tax-Data.git`. GitHub API: `"private": false`, `"visibility": "public"`, last push 2026-10-07 01:00 UTC. `contents/research?ref=main` lists `decisions_log.md`, `state_weights`, `CONVENTIONS.md` and others; `research/rebuild/` is not pushed. The README has a title, a one-line description and four "Outstanding tasks" bullets, one of them "Running the LP targeting algorithm for projected years". "Four-line README" is loose wording. https://github.com/Budget-Lab-Yale/Tax-Data |
| 07-35 | IRS page of 22 May 2026: the 2012–2015 PUFs "are no longer available"; a synthetic file "will be available in the next few months". | CONFIRMED | Live page, "Page Last Reviewed or Updated: 22-May-2026", wording exact. https://www.irs.gov/statistics/soi-tax-stats-individual-public-use-microdata-files |
| 07-36 | Summary 1: "Every documented scorer treats targets as exact constraints." | OVERSTATED | Contradicted by the report's own body: TMD (±0.5% plus slack), PWBM (tolerance bands of 0.1–10%), PolicyEngine (soft loss). The statement holds for JCT, OTA and taxdata. |
| 07-37 | Summary 5: "No documented method fits post-2017 SOI itemized-deduction tables by simulating who itemizes." | OVERSTATED | JCX-48-23, pp. 37–38, lists among JCT's yearly targets "the number of itemizers claiming a mortgage interest expense deduction", "the dollar value of home mortgage interest expenses claimed by itemizers" and "charitable giving, by itemizers and non-itemizers". These depend on the calculator's itemization choice under each year's law (p. 39). The targets are CBO-consistent forecasts, not SOI tables. TMD's state weights target the calculator's post-cap SALT (07-16). The narrow claim, about SOI post-2017 tables, may hold; the summary reads more broadly than that. |
| 07-38 | Summary 4: JCT and OTA both "set the non-filer count as population (plus deaths) minus people on returns". | OVERSTATED | True for JCT (JCX-48-23, Table 1, p. 11). For OTA's 2016 model, TP-12 p. 7 says the non-filer population was estimated from "information returns and records of births and deaths provided by the Social Security Administration", with demographics from CPS data linked to tax returns. Only the 2019-based model "uses an adjusted version of the Social Security Area Population" (footnote 5). The body's line "reweighted to match Census CPS data linked to returns, giving 39.5 million" also mixes up the source of the total with the source of the demographics. |
| 07-39 | ITEP: 97% of 3,264 state-by-income cells within 2% of IRS. | CONFIRMED | "across 3,264 discrete points ... within 2 percent of IRS values in more than 97 percent". **Note:** this is fit in the base year to the targets the weights were calibrated on, not an out-of-sample test. https://itep.org/who-pays-7th-edition-methodology-and-discussion/ |
| 07-40 | CBO 2016: the linear program "produces a trimodal distribution", the nonlinear version "a smooth distribution"; "One is not necessarily better than the other"; Treasury and JCT use x⁴ + x⁻⁴ − 2. | CONFIRMED | Perese (2016) slides, Internet Archive copy of 23 March 2025. All three phrases are present. https://www.cbo.gov/sites/default/files/114th-congress-2015-2016/presentation/52147-presentation.pdf |
| 07-41 | JCT non-filer arithmetic: 329.2M + 2.9M − 294.4M ... = 39.8M (Table 1). | UNCHECKABLE | The table did not survive text extraction from the PDF, and I could not render pages on this machine. The surrounding text confirms the method and the 0.6M "excess" children (p. 35) and the 8.1M + 8.1M pairing (p. 12). |

## Claims, report 08 (other fields)

| # | Claim in the report | Grade | Evidence |
|---|---|---|---|
| 08-1 | Realtime Inequality is published in *Journal of Public Economics* 260 (2026), article 105701. | CONFIRMED | Crossref: DOI 10.1016/j.jpubeco.2026.105701, vol. 260, article 105701, August 2026. Authors Blanchet, Mouillet, Saez and Zucman. The review brief's "Blanchet–Saez–Zucman" leaves out Mouillet; the report names all four. https://doi.org/10.1016/j.jpubeco.2026.105701 |
| 08-2 | The backtest covers 1975–2024. | CONFIRMED | Base years t run from 1975 to 2023 ("For each year t from 1975 to 2023 (or 2022)"), so predicted years run 1976–2024 ("all years from 1976 to 2024"). The summary's "1975–2024" and the body's "1975–2023" both describe this correctly. https://eml.berkeley.edu/~saez/BMSZjpube26realtimeinequality.pdf |
| 08-3 | In recessions, "using wage-distribution data cuts the bias for the bottom half from 3.6 to 0.2 percentage points". | OVERSTATED | The figure is right. The paper (p. 12) says "our methodology ... reducing the estimated bias from 3.6 percentage points to 0.2 percentage points compared to the uniform growth model". The credit goes to the whole method, not to wage-distribution data alone. |
| 08-4 | Backtest table: RMSE and bias by group, against uniform growth. | CONFIRMED | Table 2, factor income: bottom 50% 2.0/2.8 pp RMSE, bias −0.3/+0.8; middle 40% 0.8/1.2, +0.2/+0.4; next 9% 0.9/0.8, −0.3/−0.2; top 1% 3.1/4.4, +0.2/−1.6. The largest top-1% misses are in 1987–88 (p. 11). |
| 08-5 | HMRC grows pay in seven bands cut at P10, P25, P75, P90, P95 and P99 of the real-time payroll (RTI) data, then rescales to OBR mean growth. | CONFIRMED | HMRC, Income Tax Liabilities Statistics, June 2020, supporting documentation, p. 23: "SPI cases are assigned to one of seven quantile groups, partitioned according to percentiles P10, P25, P75, P90, P95 and P99 of the RTI pay distribution"; "a further re-scaling is applied ... to ensure that mean earnings growth does align with the OBR forecast". https://assets.publishing.service.gov.uk/media/5ef45bc2e90e075c4fa0ab9a/Income_Tax_Liabilities_Statistics_June_2020_-_Supporting_Documentation.pdf |
| 08-6 | HMRC's published projection errors are 2–4% one year ahead and 4–6% three years ahead. | CONFIRMED | Same document, Table 6, p. 35, for projections released after Spring Budgets since 2001. Mean absolute error one year ahead: 2% taxpayers, 4% higher-rate, 2% income, 3% liabilities (N = 17). Three years ahead: 4%, 5%, 5%, 6% (N = 15). Text: "mean absolute projection errors of 2-4% ... increasing to 4-6% for three-year ahead projections". |
| 08-7 | SPSD/M warns that uniform growth factors raise means rather than counts (the social-assistance example). | CONFIRMED | SPSD/M v20.0 Growth and Validation Guide: "this technique increases mean amounts per individual or per family ... may lead to false inferences"; the 20% social-assistance example; "force agreement with aggregate control totals by minimal change"; "can only be validated with other projections". https://datalib.usask.ca/dli/social/spsdm/spsdm-v20.0/doc_en/growth%20and%20validation%20guide.pdf |
| 08-8 | "The current release is version 30.1.1 with database year 2018." | WRONG | Statistics Canada lists "Version 34.0, database year 2022", released 12 February 2026, then Version 30.3 (10 February 2025). Version 30.1.1 (12 April 2024) is marked "ARCHIVED". https://www150.statcan.gc.ca/n1/en/catalogue/89F0002X |
| 08-9 | SPSD/M v20.0 has base year 2008; host survey SLID, with about 80% consenting to linkage; Greenbook top records averaged in groups of 5. | CONFIRMED | Catalogue lists "Version 20.0, database year 2008". Database Creation Guide: SLID host; "respondents (about 80%) gave Statistics Canada permission"; "clustered into groups of five". |
| 08-10 | Australian Treasury advises against using CAPITA beyond about five years. | CONFIRMED | Treasury Working Paper 2017-05, p. 5: CAPITA covers "the Budget forward estimates period (the first four years after the current year). While it is possible to extend ... this is not recommended". Reasons: recipient benchmarks "are more difficult to estimate beyond this five year horizon"; wages uprated by average weekly earnings; population structure held fixed. The report omits the first reason, about recipient benchmarks. https://treasury.gov.au/sites/default/files/2019-03/170915-PT-Treasury-Research-Institute-Paper-CAPITA-MSN-edits.pdf |
| 08-11 | PopulationSim's weighting rule: minimise Σ x ln(x/w) + Σ uᵢ zᵢ ln zᵢ; "1E10" means hard constraints and "<50" almost none; expansion cap of 30. | CONFIRMED | Documentation wording exact; "max_expansion_factor: 30". https://activitysim.github.io/populationsim/application_configuration.html |
| 08-12 | PSZ: information returns for 1999–2014 show about twice as many non-filer wage earners as the CPS-built pool, each earning about twice as much. | CONFIRMED | PSZ Data Appendix (1 April 2019 version), Appendix B.1, pp. 20–21: "Over the period 1999-2014, there about twice as many wage earners in the IRS non-filer sample than in our CPS built sample of non-filers, and conditional on being a wage earner, they earn about twice as much as in the IRS non-filer sample than in the CPS. Hence ... we simply re-weight wage earners in the CPS by a factor 2 and we uniformly increase their wages by a factor 2." Further points in the same passage: the CPS non-filers follow Tax Policy Center rules, and "we assume that all tax units with children file tax returns whenever they have positive labor income"; the cause offered is "very small wage earners may report zero wage income in CPS and a fraction of individuals with wages may fail to file"; Social Security benefits "do[] not need adjustment"; the adjustment is applied back to 1962. Mirror URL as cited in the report. |
| 08-13 | The PSZ data appendix is not on the authors' site. | WRONG (minor) | https://gabriel-zucman.eu/files/PSZ2018DataAppendix.pdf exists; it is the 9 November 2017 version. The mirror is the 1 April 2019 version, which contains the passage above. |
| 08-14 | Saez's SOI paper: filers plus information-return non-filers come within about 1% of Census adult counts in most years, 1999–2013. | CONFIRMED | "within 1% of Census (as is the case for all years except 1999, 2000, 2007, and 2013)". https://www.irs.gov/pub/irs-soi/16rpsaeztabulations.pdf |
| 08-15 | PSZ update log (February 2022): 2013–16 aged from the 2012 PUF to internal tabulations; 2017 by uniform scaling; after 2017, adults grow 0.9% a year and components grow with nominal GDP; taxes from TAXSIM. | CONFIRMED | `PSZUpdates.pdf` §3.3, pp. 12–13; the quotations are exact. The log refers the reader to Saez and Zucman (2018) for the aging method. The report says the log "does not describe the reweighting algorithm", which is true but leaves out where it is described. https://gabriel-zucman.eu/files/PSZUpdates.pdf |
| 08-16 | Summary 2: PSZ and Realtime Inequality "change the shape of the distribution only for wages". | OVERSTATED | True of Realtime Inequality. For 2013–2016, PSZ matched the 2012 file to internal tabulations "by income groups and demographic groups for each fiscal income component", which changes every component's distribution. Holding shapes fixed applies to their 2017-and-later years. |
| 08-17 | BEA multiplies SOI amounts by audit-based misreporting factors before using them. | CONFIRMED | BEA technical document, pp. 4–5: "we first multiply the SOI data for wages, self-employment, dividends, and interest by their respective adjustment factors to be consistent with the audited totals". The factors come from DeBacker et al. (2020), based on National Research Program audits, and are used to set shares across three AGI groups. Ranges for top shares in the provisional year are also confirmed (p. 15). https://apps.bea.gov/data/special-topics/distribution-of-personal-income/national/technical-document.pdf |
| 08-18 | Auten and Splinter: 56 non-filer groups; misreporting allocated in 10 × 11 groups. | CONFIRMED | Footnote 14: "There are 56 nonfiler groups: two marriage, four age, and seven income groups"; "10 ratio groups and 11 reported income groups". https://www.davidsplinter.com/AutenSplinter-Tax_Data_and_Inequality.pdf |
| 08-19 | Urban's HIPSM keeps pre-ACA data "because later ACS data reflect the ACA's transitional period". | CONFIRMED | Model page, wording exact; ACS 2012 and 2013 reweighted to 2013. |
| 08-20 | Eurostat flash-estimate note, May 2026: EU-SILC 2024 base; individual labour transitions rather than reweighting. | CONFIRMED | "Version 1 – May 2026"; "for FE 2025, EU-SILC 2024 (income 2023) microdata"; "For FE 2025, the annual labour transition approach has been applied." https://ec.europa.eu/eurostat/documents/d/experimental-statistics/methodological-note-flash-estimates-2025 |
| 08-21 | PSZ order of steps: CPS non-filers reweighted to adult population counts, "then" wage earners given double weight. | WRONG (minor) | The appendix says the doubling happens "when building the CPS sample (and before adjusting weights for age)". The reweighting to adult population comes after the doubling. |
| 08-22 | Summary 1 puts PopulationSim's objective in quotation marks: "stay close to the starting weights, and pay a user-set penalty for missing each control." | UNSUPPORTED (as a quotation) | The sentence does not appear in the documentation (string search of the page). It is a fair paraphrase, but it is presented as a quotation. |

Not checked in report 08: the DFA error figures, CPB's rebasing dates and the
Huang–Fuller algorithm, DYNASIM, FiFoSiM, Grattan, the WID merging-point
method, and every item the report itself marks "not verified".

## Omissions

Items I believe the reports should have covered. Those marked "not verified"
I have not opened.

**Report 07**

- **The dispute under PolicyEngine's advisory.** A PolicyEngine developer
  contradicted the advisory's account of when the website moved off the
  Enhanced CPS (07-3).
- **Law-dependent targets already in use.**
  - JCT targets itemizer counts and charitable giving split by itemizers and
    non-itemizers in every projected year (07-37).
  - TMD's state weights target the post-cap SALT deduction computed by its
    calculator (07-16).
  - Both bear directly on the brief's proposal to match censored tables by
    simulating the reporting process.
- **Populace's production settings.** The production US calibration sets the
  pull toward the prior to zero, caps each target's miss at 100%, and caps
  weights at 5× (07-6, 07-8, 07-9). The report describes optional features
  and solver defaults rather than what the certified build runs.
- **Stale PolicyEngine figures.** PolicyEngine withdrew the "7,000+ targets"
  and "72 variables" figures (07-13).
- **Non-filer research from inside the IRS** (not verified). Work by IRS
  researchers on the non-filing population from information returns, such as
  Larrimore, Mortenson and Splinter, and Cilke's 2004 "Missing Strangers",
  which JCX-48-23 footnote 24 cites. This is the agencies' counterpart to the
  CPS approach and is relevant to `research/state_weights/`.
- **SOI's projections of return counts** (not verified). SOI publishes return
  projections, as I recall in Publication 6292; I have not confirmed the
  number or content. These would be a candidate target for projected filer
  counts.

**Report 08**

- **Register-based tax-benefit models** (not verified). Examples are the
  Nordic ministries' models and Ireland's SWITCH. They work from
  full-population administrative data and show what models do when they
  have no missing-people problem.
- **Whether the PSZ finding transfers** (08-12). The PSZ wage-earner gap was
  measured against a CPS pool built under rules that assume filing for units
  with children and labour income. Before Tax-Data uses it as a target or a
  correction, the same comparison should be run against the current ASEC pool.
  Saez (2016) said the non-filer tabulations would be "publicly disclosed at a
  later date"; neither report found whether they were.

## Method and limits

- **Code.** Fresh shallow clones, 6–7 October 2026, under
  `/tmp/claude-22405/rv/`: `policyengine-us-data` 42ed5d4, `populace`
  581b569, `microcalibrate` 93e9c2a, `tax-microdata-benchmarking` 4b2a4f9,
  `taxdata` 608bf99. The commits match those named in report 07.
- **GitHub metadata.** Repository status, release dates, issue #1107 and
  discussion #429 were read through the authenticated `gh` command-line tool
  (REST and GraphQL).
- **PDFs.** Downloaded with `curl` and converted to text with `pypdf`. jct.gov
  and treasury.gov served the files directly. CBO's 2016 slides came from the
  Internet Archive. TPC's and ITEP's pages came through a text-extraction proxy
  (r.jina.ai), because direct requests returned 403.
- **No figures read.** No page images were rendered, so a claim that rests on
  a figure or an image-only table (07-41) is ungraded.
- **No computation** was run beyond downloading, reading files and running
  `git`.
