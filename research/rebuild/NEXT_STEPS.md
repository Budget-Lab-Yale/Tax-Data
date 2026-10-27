# Next steps (as of 27 October 2026)

The latest run has five sources (the PUF, SCF, CEX, CPS and SIPP), tips, auto loan interest, about 40 more PUF
items and 753 targets (`FEATURES.md`). This list is the order of work agreed with John on 27 October. Each step is
one change on a branch, a full run, a comparison with the latest run, John's approval, then the new latest.

## Before the tax calculator

1. **Rules applied after drawing.** No new data. Child credit eligibility for each dependent, dependents' Social
   Security numbers, blindness, the QBI wage and specified-service flags for each business, parents' income rank,
   death probabilities, yearly unrealized gains on each asset, and the basis and holding period of capital gains
   from SOI's Sales of Capital Assets tables (published tables, so a rule, not a source). Production's versions are
   in `src/imputations/` and `src/mortality_ledger.R`.
2. **The file Tax-Simulator reads.** Production's column names, one CSV per year, record IDs and seeds; every
   column Tax-Simulator reads checked against what the file has. Deductions the law removed (domestic production
   activities from 2020, tuition and fees from 2021) set to zero after repeal.
3. **Tax-Simulator on the rebuild's 2019 file.** Revenue, the distribution tables and the main credits against
   production's file and against SOI. The first test of the whole chain.
4. **The calculator in post-training.** Each record's taxes computed once, then targets on computed values:
   itemizers, taxable income, the child tax credit, liability by AGI. Itemized deductions, the foreign tax credit,
   the AMT items and the 13% fall in itemizers after the tilt are revisited here.

## After the calculator

5. **Other years.** 2015 to 2023 from the same method, then the projection years.
6. **The aggregate records and the Forbes 400,** so the very top (Table 4.3's top 0.001%) can be targeted.

## Waiting on something

- **Overtime.** The March CPS extract needs the earnings-question items (hourly pay, weekly pay, hours last week),
  occupation, industry and metro size from IPUMS; John will handle the IPUMS key. Then overtime is computed per
  person in `build_cps.R` by Ernie Tedeschi's rules and enters pretraining under the CPS label.
- **State and local taxes:** not now (John, 27 October).

## Open decisions

- Tips: whether to reach the W-2 count of tipped workers by reweighting, as now, or by drawing more tipped workers;
  the size of the dependent-filer allowance in the W-2 spread targets' errors.
- Production's double count of the above-the-line adjustments (about $145 billion in 2019): left as is (John, 27
  October). The check is in scratch `rebuild/explore/2026-10-27/other_inc_check.md`.
- Tips on units with $200,000 or more of wages: $5.9 billion in the latest run (the W-2 spread targets raised it
  from $4.9 billion), against the SIPP's $0.9 billion.
