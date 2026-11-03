# Next steps (as of 3 November 2026)

The pipeline builds a file for every year 2015-2035 from one branch (`FEATURES.md`). The work of 29 October to 2
November is merged on `rebuild-integration-1102`; how its runs came out is in scratch
`explore/2026-11-02/integration/RESULTS.md`. Paths starting `explore/` are under
`/nfs/roberts/scratch/pi_nrs36/jar335/rebuild/explore/`. Each piece of work below is one branch, a run of the years it
touches, a comparison with the run before it, and John's approval.

## Near-term steps (John, 2 November evening; brought up to 3 November)

1. **Done, 3 November: 2023 and its Forbes list.** The slow 2023 solve came from the soft cap meeting Table 4.3's tight
   top 0.001% cells, which the list fills a quarter of, not from a wrong target (`explore/2026-11-03/forbes2023/`).
   John raised the Newton budget from 100 to 150 and switched the 2023 list back on; 2023 converges in 92-103 steps
   depending on the processor count (`explore/2026-11-03/budget150/`).
2. **Done, 3 November: the merge** of `rebuild-integration-1102` into `rebuild-joint-model` (c526b34), then the 150-step
   budget (branch `rebuild-budget150`).
3. **Code simplification:** combined on branch `rebuild-simplify` (0827468), every output identical; not merged
   (`explore/2026-11-03/MORNING.md`, section E).
4. **The projection years (2024-2035), being redesigned with John.** The steps agreed so far are in
   `explore/2026-11-02/projection_design/DESIGN.md`: each year starts from the 2023 file and the years stay
   independent; a small reweighting first to exact person counts by age and marital status (the 2023 file's counts
   grown with CBO's population growth); one dollar factor per item so its total grows at CBO's rate; then the main
   reweighting, with itemizer and AMT counts indexed from 2023. Next to design: the number of returns. Nothing is
   built until the whole design is agreed.

   **Placeholder until then:** the billionaires after the last Forbes list (2027-2035) are held at their starting
   weight in the solver (settings `tilt.top.after_last_list.fixed`). Left free on 2 November, the solver cut the ten
   richest to weights near zero (`explore/2026-11-02/billionaires/RESULTS.md`).
5. **Done, 3 November: tax-simulator-gpu master pushed,** with the AMT senior deduction fix, car loan interest below
   the line, and the QBI income limit net of the Schedule 1-A deductions (`explore/2026-11-03/simfixes2/`). The R
   Tax-Simulator's AMT and QBI fixes are on its branch `fix/senior-deduction-amt-itemizers`, pushed, not merged.
6. **The 2025 SCF and inheritances:** on branch `rebuild-scf2025`, not merged (`explore/2026-11-03/MORNING.md`,
   section B); not yet reviewed with John.

## Other to-dos (not in order)

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
  base (part of step 4).
- **Series flags from 1-2 November** (`explore/2026-11-01/tax_expenditures/series/RESULTS.md`): the 2020-2021 files
  hold about 1,500 records above weight 5,000; non-filers jump 19% in 2023 and miss their target in even years.
- **Inheritances** drawn by the network as an SCF item at the next retrain, the simulator scaling heir dollars to its
  bequests (`explore/2026-11-02/inheritances/RESULTS.md`). Open for John: the SCF's thin top (about 4 households a
  wave above $5 million). Until then the rebuild's runs go without heir data: its record ids do not match the heir
  file's (`explore/2026-11-02/estate_heirs/RESULTS.md`).
- **The 2025 SCF:** check whether it is out; if so, add it as a wave at the next retrain.
- **Slimming the pipeline** (after the simplification agents).
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
- From 2024 the file's gross Social Security grows faster than its source, by about 8% in total by 2035. A gross
  benefit target in projection years would remove it (`explore/2026-11-02/social_security/RESULTS.md`, item 7). Part
  of item 1.

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
