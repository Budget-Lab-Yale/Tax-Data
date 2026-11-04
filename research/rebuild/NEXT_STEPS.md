# Next steps (as of 4 November 2026)

The pipeline builds a file for every year 2015-2035 from one branch, `rebuild-joint-model` (`FEATURES.md`). The full
run on the merged code of 3-4 November (`explore/2026-11-03/bigrun/RESULTS.md`) is installed as the latest (John, 4
November). The code to run to 2075 is on `rebuild-longrun` and has passed its first test years. Paths starting
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
   **Done 4 November, not yet in a full run:** Table 4.3's counts in the top 0.1% and 1% borrow their errors only from
   Table 1.4 cells at or above the group's cutoff (`TARGETS.md` S15; 9766566). The next full run picks up both this
   and the age-by-AGI importance.
4. **The projection years (2024-2035).** John's three-step design is built and merged (`FEATURES.md`, "The projection
   years in three steps"): people by age and marital status, one dollar factor per item from John's series, then
   the main reweighting with no itemizer target and the AMT payer count kept. The Schedule A add factors (+2% a year
   2024-2028, +1% 2029-2033) are a crude step toward CBO's itemized deductions, to be revisited at each CBO update.
   Still to design: the number of returns (CBO's 2023 count is 4.4 million above SOI's; `explore/2026-11-03/noitem/`).

   **Past 2035 (branch `rebuild-longrun`, not merged; `explore/2026-11-04/longrun/`).** Years 2036-2075 run the same
   three steps with longer series: each item continues from 2035 at a Macro-Projections aggregate, borrowed from
   production's long-run mapping. First test (4 November, `longrun/test/RESULTS.md`): 2036, 2045, 2055, 2065 and 2075
   from the big run's 2023 all converge (6-12 Newton steps, largest miss 2.03 standard errors), step 1 meets every
   person count, and AGI holds at 57-58% of nominal GDP and wages at 87% of GDP wages. Next: the other 35 years, then
   John's review and the merge.

   **Placeholder until then:** the billionaires after the last Forbes list (2027-2035) are held at their starting
   weight in the solver (settings `tilt.top.after_last_list.fixed`). Left free on 2 November, the solver cut the ten
   richest to weights near zero (`explore/2026-11-02/billionaires/RESULTS.md`).
5. **Done, 3 November: tax-simulator-gpu master pushed,** with the AMT senior deduction fix, car loan interest below
   the line, and the QBI income limit net of the Schedule 1-A deductions (`explore/2026-11-03/simfixes2/`). The R
   Tax-Simulator's AMT and QBI fixes are on its branch `fix/senior-deduction-amt-itemizers`, pushed, not merged.
6. **Inheritances in the files.** The item is drawn now; open are the top (the SCF and the file have almost no heirs
   above $18 million; estate data imply about $90-100 billion more a year), the 2013 and 2022 waves' year weights,
   and Tax-Simulator's columns for it (`taxsim_file.py` does not yet write it; `explore/2026-11-03/scf2025/RESULTS.md`,
   section 6). The 2025 files should move to the shared raw-data folder, and the setting `paths.raw.scf_wave_dirs`
   then go.

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
