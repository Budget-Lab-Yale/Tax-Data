---
title: "Capital losses in the 2017 file carry SOI's carryovers: the .loss rescale targets Schedule D lines 7/15"
role: notes
workstream: state_weights
status: open
updated: 2026-10-01
sot: research/state_weights/plan.md
supersedes: []
superseded_by: null
---

# Capital losses: the rescale brings in the carryover

*Raised 2026-10-01 by the Affordability-Index income construction (JI). Its decision D78 counts
the year's net realised capital gain or loss in household income, apart from the tax treatment;
see its `docs/26_income_beyond_tax_records.md` §2a. Vintage read: `2026091119`, TY2023 donor
pool. Code references are to `phase3b-filer-refit` at this note's commit. The working-tree edits to
`create_2017_puf.R` and `project_puf.R` do not touch the lines below.*

## 1. What we see

TY2023 filers in the donor pool, weighted:
- `kg_st + kg_lt` totals $297B.
- Losses are −$494B on 7.3M returns, −$475B of it beyond −$3,000 on 5.5M returns.
- Floored at −$3,000, the total is $772B.

## 2. Where it comes from

1. **The PUF fields exclude the carryover.** `kg_st` = P22250 and `kg_lt` = P23250
   (`config/variable_guide/baseline.csv` lines 52–53). The 2015 PUF booklet defines P22250 as
   short-term gains less losses on Schedule D *except for the short-term capital loss
   carryover*, and P23250 likewise for long-term. The PUF has no separate carryover field.
   - Weighted P22250 + P23250 on the 2015 PUF is $676B. SOI's "total realized gains less
     losses" for 2015 (Sales of Capital Assets, Wilson & Williams), which also excludes
     carryovers, is $685.9B.
   - Current-year net losses on the 2015 PUF are −$80.6B on 4.74M returns.
   - *Side note:* the variable guide's description of `kg_st` ("short-term net capital gain
     carryover") is wrong.
2. **The rescale targets a concept that includes it.**
   - `src/create_2017_puf.R` (around lines 77–131) rescales every `inc_loss_vars` `.income` and
     `.loss` series by AGI group to SOI totals.
   - `resources/variable_table_crosswalk.csv` points `kg_st.loss` and `kg_lt.loss` at table
     `1_4_kg`.
   - That compiled table (`Compiled-SOI-Tables/v3/20250319/historical/table_1_4_kg.csv`) carries
     the carryover in separate columns (`kg_st_carry`, `kg_lt_carry`), so its `.loss` amounts are
     Schedule D lines 7 and 15, *including* carryovers. Example, TY2015, AGI below $1:
     short-term loss $32.2B, of which carryover $25.9B.
   - Over all AGI, TY2015:
     - short-term: loss $250.6B including $171.1B of carryover, against the PUF's $78.1B
       current-year loss;
     - long-term: loss $312.7B including $291.7B of carryover, against the PUF's $49.0B.
   - So the rescale multiplies current-year losses by roughly 3× (short-term) and 6×
     (long-term). It spreads the carryover over records with a current-year loss rather than
     over carryover holders.
3. **The same series are reweighting targets.** `config/target_info/baseline.csv` lists
   `kg_lt.loss` (row 149) and `kg_st.loss` (row 153) for 2017.

## 3. What would fix it

Any one of these makes `kg_st + kg_lt` the year's net realised gain or loss again:
- target `.loss` at lines 7/15 **less** the carryover columns the compiled table already has; or
- drop `kg_st.loss` and `kg_lt.loss` from `to_rescale` and from the reweighting targets; or
- target SOI's Sales of Capital Assets "gains less losses" tables.

The return's own treatment, the $3,000 limit applied to the current-year amount, is the
calculator's job and is unaffected. Tax law still lets carryovers offset gains, so the tax on
records with large carryovers will be somewhat high without one. If the tax side needs that, the
carryover could become its own imputed variable, and the SOI `_carry` columns are its targets.

## 4. Related, lower priority

- **The gain side.** SOI `kg_lt.income` is $1,135B in 2015 against about $770B on the PUF.
  Unresolved: possibly a concept difference too, such as capital gain distributions, E22370.
- **Projection not traced.** How the 2017→2023 projection scales losses (`project_puf.R`
  around lines 141–158, CBO taxable gains) was not checked.
- **Passive losses.** E25920 and the related Schedule E loss fields are the amounts allowed after
  Form 8582: this year's suspended losses are dropped and prior-year losses released this year
  are added. The PUF carries P65300 (current-year passive net income), P65400 (current-year
  passive net loss), E68000 (losses allowed) and the 8582 flag; the donor file keeps none of them.
  Affordability-Index would use them to get current-year passive income.
- **`other_inc` and the adjustments.** `other_inc` (`process_puf.R`, AGI less the listed lines)
  nets every above-the-line adjustment, which the file also carries separately (`hsa_contr`,
  `trad_contr_ira`, `se_health`, ...). It also nets NOL carryforwards, while `nols` is zero on
  every record. If Tax-Simulator subtracts the adjustments again, AGI is understated. TY2023
  pool: `other_inc` −$165B, negative on 62.8M of 154.2M filers. For comparison, SOI TY2022
  shows NOL $179.8B on 1.0M returns.
