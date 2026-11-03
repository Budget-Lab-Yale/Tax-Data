# Roadmap: from the current pipeline to the end goal (as of 2 November 2026)

What the pipeline does now is in `FEATURES.md`; the end state is in `GOAL.md`; the next pieces of work, in John's
order, are in `NEXT_STEPS.md`. This file lists the larger items between the two and where each stands. The version
of 27 October, with its longer list of known gaps, is in git history (`git show 4c2c518:research/rebuild/ROADMAP.md`).
Paths starting `explore/` are under `/nfs/roberts/scratch/pi_nrs36/jar335/rebuild/explore/`.

| Item | Where it stands |
|---|---|
| 1. Targets, made formal | Built. One folder per year in `pipeline/targets_config/` (2015-2035), each target with a standard error, met through one cost in one solve (`TARGETS.md`). Past years have SOI's tables, the calculator against SOI, SCF wealth (also by age since 2 November), CEX, CPS and W-2 targets; Table 4.3's top 0.001% is a target since the PUF's aggregate records were rebuilt. Projection years are being redesigned (`NEXT_STEPS.md`, item 1). |
| 2. Translation | Built 23 October: two functions (a dollar mapping and the two-slot fill-in) and one table, on every survey since 28 October. Left: the fill-in can add holders a survey misses but not remove holders it has too many of; its PUF side reads the 2015 PUF, not the tax data for each survey year. |
| 3. Which survey years a target year draws on | Built for every survey: the SCF balance sheet from a mix of waves, the CEX, CPS and SIPP items from a mix of survey years, each grown per tax unit to the target year. Left: the shares are written in each year's folder by hand, not computed from a rule. |
| 4. The other surveys and imputed variables | Built: consumption (CEX), each person's items, childcare and overtime (CPS), tips (SIPP), the balance sheet, deferrals and auto loan interest (SCF), the non-microdata imputations (`FEATURES.md`). Left: state and local taxes (not now, John, 27 October); inheritances as an SCF item (`NEXT_STEPS.md`, item 3). |
| 5. Half-measured PUF variables | Ages: the primary's range from the PUF, exact ages, sex and dependents' ages from the CPS (`AGES.md`); dependent filers' ages from the CPS dependents who file (2 November). Left: the CPS ties age to capital income less closely than the SCF within 65 and over (`explore/2026-11-02/ages_cps_scf/`). |
| 6. Pretraining settings | Settled 26 October (grouped weighting, the AGI group label always shown, batches of 512, 80 passes). Known flaws in the draw are `NEXT_STEPS.md`, item 2. |
| 7. Real PUF records or synthetic ones | Tested 1-2 November as a setting (branch `rebuild-puf-start`, not merged; `explore/2026-11-01/puf_start/RESULTS.md`): closer to SOI before the reweighting, about half the effective sample. John has not decided. |
| 8. Real non-filers | Built: non-filers drawn under the CPS label from the CPS's units and their filing chances. Parked by John: credit-only filers counted as non-filers, non-filers too rich (`explore/2026-10-30/ASK_JOHN_LATER.md`, 8 and 9). |
| 9. A tax calculator in the reweighting | Built 30 October: every record's taxes from tax-simulator-gpu inside the tilt, with calculator targets against SOI. |
| 10. Prescaling by age and filing status | Built 25 October. |
| 11. Validation | Begun: tax expenditures 2025-2029 on the rebuild's files against production, JCT and OTA (`explore/2026-11-01/tax_expenditures/report/REPORT.md`; the billionaires' effect in `explore/2026-11-02/billionaires/RESULTS.md`). |
| 12. Output to Tax-Simulator | Built 29 October: `taxsim_file.py` writes production's 188 columns for every year. Left: replacing `src/main.R`; the heir data keyed by record id (`NEXT_STEPS.md`, item 3). |

## Known gaps that still stand

Figures are from the runs named; the newer ones are in `NEXT_STEPS.md`.
- Consumption is not scaled to NIPA PCE (CEX spending is about 46% of PCE in 2019); production scales each category
  up, capped at 2 times.
- The CEX: BLS stopped marking dependent filers in 2024; education spending is nearly empty in 2021 and 2022; food at
  home jumps 29% in 2024 under a new item name.
- Too many units hold both partnership and S corporation income; too few nonpassive losses (before 25 October).
- SCF household members outside the main unit (adult children, roommates) are dropped, so the SCF has fewer young
  units than the tax data.
- The 2015-2017 calculator count of $1-10k returns with taxable income is short by 8-10 standard errors; the likely
  cause is the simulator's personal exemption for dependent filers, whose fix is approved
  (`explore/2026-11-02/dep_std_deduction/RESULTS.md`).
