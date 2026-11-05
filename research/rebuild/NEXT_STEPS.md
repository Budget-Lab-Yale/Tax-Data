# Next steps (as of 5 November 2026)

The pipeline builds a file for every year 2015-2075 from one branch, `rebuild-joint-model` (`FEATURES.md`). The latest
installed run is run3 (John, 4 November; `explore/2026-11-04/run3/run/`, every year 2015-2075): the big run of 3-4
November (`explore/2026-11-03/bigrun/RESULTS.md`), its networks and draws, reweighted again on the code with Table
4.3's count errors, the age-by-AGI targets at importance 1 and the list-year ceiling (2024-2026), and 2024-2075 projected
from its 2023. Paths starting
`explore/` are under `/nfs/roberts/scratch/pi_nrs36/jar335/rebuild/explore/`. Each piece of work below is one branch,
a run of the years it touches, a comparison with the run before it, and John's approval.

## Near-term steps (John, 2 November evening; brought up to 3 November evening)

1. **Done, 3 November: 2023 and its Forbes list.** The slow 2023 solve came from the soft cap meeting Table 4.3's tight
   top 0.001% cells, which the list fills a quarter of, not from a wrong target (`explore/2026-11-03/forbes2023/`).
   John raised the Newton budget from 100 to 150 and switched the 2023 list back on.
2. **Done, 3 November: the merges** of `rebuild-integration-1102`, the 150-step budget, the code simplification, the
   calculator pin (tax-simulator-gpu c89e2729), and in the evening `rebuild-addfactors`, `rebuild-pensions`,
   `rebuild-small-fixes`, `rebuild-scf2025` and `rebuild-tabtool` (`HISTORY.md`, 3 November).
3. **Done, 3-4 November: the full run on the merged code**: three networks retrained on the new table (the rest of
   pensions as a slot, the 2025 SCF wave, the inheritance item), every year 2015-2023 drawn and reweighted, 2024-2035
   from the new 2023 (`explore/2026-11-03/bigrun/RESULTS.md`). **Installed 4 November** (John): the scratch folders
   `data/`, `targets/`, `model/`, `gen/`, `tilt/` and `taxsim/` hold its 2019 run, the old ones are in
   `previous_2026-11-04_before_bigrun/` (`INSTALLED.txt`). Its reweighting ran on the code with age by AGI, Table 4.3
   at SOI's cutoffs and married people under 18 counted unmarried, but its settings file predates the age-by-AGI
   merge, so those 75 targets a year had importance 1/1.5, not 1. No second run was made: nothing that touches
   training changed after it trained. John chose not to look further at its two unexplained moves (other income about
   $14 billion a year higher from 2023, mostly at $10 million and up; 2021 in 89 Newton steps against 58).
   **Done 4 November, in run3 (installed the same day):** Table 4.3's counts in the top 0.1% and 1% borrow their
   errors only from Table 1.4 cells at or above the group's cutoff (`TARGETS.md` S15; 9766566), and the age-by-AGI
   targets at importance 1. Every year converges (2015-2023 in 45-67 Newton steps, 2021 in 66 against 89); the
   age-by-AGI cells' mean miss falls from about 0.11 to 0.09 standard errors, the largest in 2021 from 1.85 to 1.38.
4. **The projection years (2024-2035).** John's three-step design is built and merged (`FEATURES.md`, "The projection
   years in three steps"): people by age and marital status, one dollar factor per item from John's series, then
   the main reweighting with no itemizer target and the AMT payer count kept. The Schedule A add factors (+2% a year
   2024-2028, +1% 2029-2033) are a crude step toward CBO's itemized deductions, to be revisited at each CBO update.
   The number of returns (John, 4 November: kept as built, for now): SOI's 2023 count, grown with the population at
   each age and marital group's 2023 filing share. CBO's 2023 (164.7 million) is a projection 4.4 million above SOI's
   actual 160.6 million; in 2022, where both are actual, they agree (161.3 million), so the gap is CBO's projection,
   not a definition. CBO's growth after 2023 is a little faster (+9.1% to 2035 against our +8.1%, 1.6 million
   returns), from "estimates of future additional filers" it does not describe (`explore/2026-11-03/noitem/`).

   **Past 2035 (branch `rebuild-longrun`, merged and installed 4 November; `explore/2026-11-04/longrun/`).** Years 2036-2075 run the same
   three steps with longer series: each item continues from 2035 at a Macro-Projections aggregate, borrowed from
   production's long-run mapping. First test (4 November, `longrun/test/RESULTS.md`): 2036, 2045, 2055, 2065 and 2075
   from the big run's 2023 all converge (6-12 Newton steps, largest miss 2.03 standard errors), step 1 meets every
   person count, and AGI holds at 57-58% of nominal GDP and wages at 87% of GDP wages. The other 35 years (2037-2074)
   ran the same way the same day (jobs 32023190-32023224; `longrun/test/RESULTS.md`): every year converges in 5-13
   Newton steps, the largest miss rises smoothly from 1.23 to 2.03 standard errors, and AGI, wages and tax after
   credits grow 3.5-4.8% a year with no jump between years. Merged (6239d27) and installed in `bigrun/run/2036-2075`.
   Open: the child credit moves in steps of up to 2.5% a year past 2035; cause not checked.

   **The billionaires after the last Forbes list** (John, 4 November): records like any other, aged naively and held at
   their starting weight in the solver (settings `tilt.top.after_last_list.fixed`; left free on 2 November, the solver
   cut the ten richest to weights near zero). In a list year no other record may be at or above $1 billion
   (`top_records.hold_below_ceiling`; 2024-2026 had 82-292 grown 2023 records there). After the last list, records grown
   past $1 billion stay as the new billionaires. The list's fortunes grow so that its net worth grows as Z.1 household
   net worth (John, 4 November; its share 4.5% in 2026, 4.3% in 2075; the top 0.001%'s share 5.0% to 4.6%;
   `explore/2026-11-04/listgrowth/shares.csv`); before that rule its share rose to 5.8%. The records grown past $1
   billion are under no net worth target (Z.1's covers units below $1 billion), so their wealth grows with their items
   alone ($57 trillion in 2075).
5. **Done, 3 November: tax-simulator-gpu master pushed,** with the AMT senior deduction fix, car loan interest below
   the line, and the QBI income limit net of the Schedule 1-A deductions (`explore/2026-11-03/simfixes2/`). The R
   Tax-Simulator's AMT and QBI fixes are on its branch `fix/senior-deduction-amt-itemizers`, pushed, not merged.
6. **Inheritances in the files.** The item is drawn now; open are the top (the SCF and the file have almost no heirs
   above $18 million; estate data imply about $90-100 billion more a year), the 2013 and 2022 waves' year weights,
   and Tax-Simulator's columns for it (`taxsim_file.py` does not yet write it; `explore/2026-11-03/scf2025/RESULTS.md`,
   section 6). The 2025 files should move to the shared raw-data folder, and the setting `paths.raw.scf_wave_dirs`
   then go.

7. **The November filing-season tables, each year** (branch `rebuild-nowcast`; FEATURES.md, "The first projection
   year from SOI's November filing-season tables"). Two moments a year:
   - **When SOI posts the next November table** (the tax-year-2025 table, `26inweek47.xlsx`, expected January 2027):
     download it into `resources/filing_season/raw/`, run the stage `filing_season`, add the new year's pair to the
     backtest (scratch `explore/2026-11-04/nowcast/scripts/backtest.py` and `errors_table.py`, which rewrites
     `targets_config/filing_season_errors.csv`) and revisit the trust levels with John. While 2024 has no final
     tables, the table for 2025 is not used: the blend covers only the year after the last final year.
   - **When SOI publishes final tables for 2024** (Table 1.4, expected late 2026 or 2027): 2024 becomes a past year
     (its targets.yaml a past year's, run on its own draw), settings `tilt.projection.filing_season` base_year 2024 and
     year 2025, the 2025 targets.yaml gets the `filing_season_groups` entry (to be taken out of 2024's) and 2025, built
     from the 2024 file, becomes the jump-off year: 2026-2075 start from it (every projection targets.yaml's
     `from_year` then reads 2024, which `projection_steps.jump_off` turns into 2025). The November tables for 2024 and
     2025 give 2025's blend. If the November table for the year after the last SOI year is missing, the jump-off year
     is the last SOI year and the setting is off.
   - **Schedule C** stays at no trust until its 2024 fall is understood: $47 billion of the $55 billion fall is larger
     losses on returns with no AGI, whose number did not change (scratch `explore/2026-11-04/nowcast/RESULTS.md`).

8. **Run5, the combined retrain** (4-5 November overnight; scratch `explore/2026-11-04/run5/RESULTS.md`): version A,
   the real-PUF setting and the part_se fix merged, three networks retrained, every year 2015-2075 run; held-out
   score 6.21 (2019) and 8.25 (2023) against run3's 8.14 and 9.56. **Not installed; John decides.** Open with it:
   - **The self-employment tax target** is met by moving weight to records with large partnership income subject to
     self-employment tax (2019: $27 billion before the reweighting, $153 billion after), and it costs 2020 most of its
     149 Newton steps (without it, 73). Keep it, target the item differently, or fix the draw's split of the tax
     between Schedule C, F and partnership income.
   - **The package stage with version A's rules** stops on three items (the tax benefit rule's partial refunds, the
     repealed deductions set to zero, the calculator's self-employment tax deduction). Options in RESULTS.md; then the
     stage `package` runs alone on CPU for every year of `run5/confidential/run/`.
   - **The earnings split** (`rebuild-earnsplit`, run beside run5 as `explore/2026-11-04/run5_earnsplit/`): it matches
     the PUF's EARNSPLIT codes and worsens the held-out CPS checks of the spouse's wage share. Merge or not.

## Other to-dos (not in order)

- **Tax expenditures against JCT** (`explore/2026-11-03/tax_expenditures_1103/RESULTS.md`, on the big run's
  2024-2029 and the 3 November calculator). Fiscal 2026, 19 provisions other than step-up: the absolute gaps to JCT
  sum to $79 billion on the rebuild (was $105 billion) and $65 billion on production. Still off on both: overtime,
  tips, car loans, step-up (-54%), the education and dependent care credits; on the rebuild only, the QBI deduction
  (-13%; sole proprietor income $427 billion against production's $615 billion) and the lower rates on gains (+8%).
- **The network's settings:** searched on 3-4 November and closed with no change (`HISTORY.md`).

- **Known flaws in the draw.**
  - The gain as a share of an asset's value is too flat by age. On the SCF's own units the network draws the right
    stock value at 80 and over but a gain share of 0.31, against the SCF's 0.43 (`explore/2026-11-02/kg_split/`).
    The age targets now make the reweighting meet the SCF's gains by age, at a cost of 3-7 points of effective
    sample.
  - Large business holdings on low-income records. The network puts business wealth on records the SCF hardly has
    (net worth above the SCF's 99th percentile with income below its 90th; `kg_old/`, `kg_split/`).
  - Too few units aged 80 and over: with the CPS counts as targets the file is still 0.1-0.2 million short, and the
    CPS's 65+ count conflicts with SOI's Table 1.6 (`explore/2026-11-02/age_targets/RESULTS.md`).
- **Interest against CBO's line.** The rebuild is about 50% above CBO's line for interest and non-qualified dividends
  in 2024-2029 (2024: $465 billion against $310 billion; production is about 7% below). The rebuild matches SOI's
  2023 taxable interest ($314 billion), while CBO's 2023 line is $287 billion, so part of the gap is CBO's stale
  base.
- **Series flags from 1-2 November** (`explore/2026-11-01/tax_expenditures/series/RESULTS.md`): the 2020-2021 files
  hold about 1,500 records above weight 5,000; non-filers jump 19% in 2023 and miss their target in even years.
- **Heir data in Tax-Simulator.** The rebuild's runs go without the heir file: its record ids do not match the
  rebuild's (`explore/2026-11-02/estate_heirs/RESULTS.md`). The plan is for the simulator to read the drawn
  inheritance item and scale heir dollars to its bequests (`explore/2026-11-02/inheritances/RESULTS.md`; step 6).
- **Slimming the pipeline** further (the simplification of 3 November is merged).
- **Not to be merged:** `rebuild-puf-start` (John, 2 November).

## Open questions for John

- Projection-year records grown past $1 billion from the 2023 file sit beside the grown Forbes list (2024: 71
  records; 2035: 1,256 records, $3.6 trillion): redraw them, scale them below $1 billion, or leave them
  (`explore/2026-11-02/billionaires/DECISIONS.md`, entry 10). Part of step 4.
- Couples in a divorce on the Forbes list are kept as separate units; Li Ge and the late Zhao Ning are one unit in
  2023 (same file, entries 3 and 4).
- Tax-Simulator's heir data is turned off for the rebuild's files (configuration only). Should the simulator warn
  rather than stop (same file, entry 11)?
- The CPS has more units aged 65 and over than SOI's Table 1.6 and the non-filer targets allow. With the CPS's counts
  within 65 and over as targets, Table 1.6's 65+ returns end 1.1-2.1 standard errors above SOI
  (`explore/2026-11-02/age_targets/RESULTS.md`).

## Done outside this repository (2 November)

- Seven model-law fixes committed to tax-simulator-gpu master, not pushed, among them `calc_pe_ded`: a dependent filer gets no personal exemption
  for himself (IRC 151(d)(2); `explore/2026-11-02/dep_std_deduction/RESULTS.md`). This is the likely cause of the
  2015-2017 shortfall in the calculator's count of $1-10k returns with taxable income
  (`explore/2026-11-02/dependent_filers_fix/RESULTS.md`, question 1).
- Step-up at death: dependent filers left out of the gains model's age cells (one of the seven; first built on
  simulator branch `te-stepup`; `explore/2026-11-02/stepup/RESULTS.md`).

## Parked by John

In scratch `explore/2026-10-30/ASK_JOHN_LATER.md`: growth rates solved jointly with the reweighting (1), capital
gains decision 26 (2), small items that move a lot in 2020-2023 (3), the QBI wage dial (5), the 2020-2021 calculator
targets (6), one blown-up debt record (7), non-filers (8: credit-only filers counted as non-filers, non-filers too
rich), the 2020-2021 zero-income filers (9). Also: pandemic-year shapes the networks did not learn (2020
unemployment, the 2021 EITC), charity at the Schedule A limit, years past 2035, and the next retrain.
