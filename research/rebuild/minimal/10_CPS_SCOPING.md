---
title: "Minimal example: scoping the CPS ASEC as a third survey"
role: evidence
workstream: rebuild
status: current
updated: 2026-10-15
sot: research/rebuild/design/plan.md
supersedes: []
superseded_by: null
---

# Scoping the CPS ASEC as a third survey (Set C)

Read-only. Nothing was built or run. "Checked" means read in the code, the
data dictionary, or a committed results file; "assumed" means not checked.

## 1. What the production builder produces

`src/nonfilers/asec_tax_units.R` reads one ASEC year and returns one row per
tax unit. Checked in the code:

- **Units.** The householder heads a unit; every married couple in a
  household is one joint unit; children attach to parents; an adult is a
  dependent only if a support test passes (own income under half the
  household's income per person). Dependents stay as dependents of their
  claimer, and dependents aged 15 and over also get their own row as a
  "dependent-headed" unit, so their own filing can be scored. Group
  quarters are dropped (handled elsewhere with the ACS).
- **Filing status.** Legal rule only: joint if a spouse is present, head of
  household if unmarried with a qualifying child, else single. No
  married-filing-separately.
- **Income.** Head plus spouse, in the income year's dollars: wages
  (`INCWAGE`), interest (`INCINT`), dividends (`INCDIVID`), rent, Social
  Security, unemployment, self-employment, retirement. **Capital gains are
  not asked** before income year 2018 (`INCCAPG` is absent from the 2016
  extract; checked in its variable list).
- **Weights.** The head's person weight (`ASECWT`).
- **Demographics.** Single-year age of head and spouse for every unit; sex,
  education, race, Hispanic origin of the head when read.
- **Filing.** `add_filing_requirement()` flags units legally required to
  file. `filing_model.R` then gives each unit a probability of filing: a
  published CBO probit below the filing threshold, and an IRS non-filing
  rate above it.
- **Years on disk.** `/nfs/roberts/project/pi_nrs36/shared/raw_data/CPS-ASEC/cps_asec_common/`
  holds IPUMS extracts for ASEC 2015 to 2026 (income years 2014 to 2025).
  ASEC 2016, income year 2015, is `cps2016_03s/`. The 2015 build has been
  run before: `research/state_weights/nonfiler_pool/results/unit_gates_2015.csv`
  and `filing_model_gates_2015.csv` exist (dated 6 October), but the unit
  file itself (`units_2015.rds`) is not on disk now.

From those 2015 results: 166.9 million non-dependent units, 119.1 million
of them required to file, plus 28.5 million dependent-headed units. The
2015 PUF represents 150.8 million returns.

## 2. Our shared variables in the CPS, and how they differ from the PUF

From `research/state_weights/nonfiler_residual/results/asec_A4_income_2015.csv`
(CPS total over SOI total, income year 2015) and the design note
`nonfiler_residual/10_asec_tax_unit_design.md` section 2.6:

| Variable | In CPS? | CPS / SOI, 2015 | Known difference |
|---|---|---:|---|
| married | yes | — | Every married couple is joint; the PUF has separate filers too (small). |
| n_dep | yes | — | Built by rule from the household, not as claimed on a return. Includes adult dependents. |
| wages | yes | 1.08 | Wider population; against SSA covered wages the design note finds about 1.0 (2017, 2022). Top-coded (`TOINCWAGE` flag). |
| interest | yes | **2.29** | Not underreporting: a definition gap. IPUMS says `INCINT` includes interest in IRAs; the PUF's is taxable interest only. Top-coded (`TINCINT` flag). |
| dividends | yes | **0.41** | Underreported in level, on a wider population. Top-coded (`TINCDIVID` flag). |
| capital gains | **no** | — | Not asked until income year 2018. |

So the brief's expectation that the CPS understates interest is wrong for
interest (it overstates it by 2.3 times) and right for dividends. The
top-coding flags are in the extract; the thresholds and how many records
hit them were not checked. That IPUMS replaces values above the top code by
swapped values from other top-coded records is assumed, not checked.

## 3. A CPS-owned variable for the test

**Recommendation: age of the primary taxpayer.** It plays the role net
worth plays for the SCF, with one advantage: a partial truth on the PUF.

- The CPS has single-year age for every unit.
- The PUF's demographic file has age in six bands (under 26, 26-34, 35-44,
  45-54, 55-64, 65 and over), but only for records in the lower sampling
  strata. Checked: it is missing for every record with AGI of $1 million or
  more, for about a third of the weight between $200,000 and $1 million, and
  present for 147.5 of 150.8 million weighted returns.
- The test: hide the PUF's age bands from training, so age is CPS-owned,
  and borrow it onto PUF records. Score generated age bands against the
  PUF's actual bands where they exist. The missing top is where borrowing
  is needed and cannot be scored on the PUF; SOI tables by age could check
  it, if they exist for 2015 (assumed, not checked).

The others: state has no truth on the PUF (the 2015 PUF carries no state).
SNAP receipt (`FOODSTMP`) is a household flag with no truth anywhere on
the tax side. The 2016 extract has Medicaid, Medicare and military coverage
but no "any coverage" variable (it appears from ASEC 2019).

## 4. What a build would take

A script like `01_build_table.R` with a `--cps 2015` mode. It would source
`src/nonfilers/state_weights.R` (for the data path) and `asec_tax_units.R`,
call `read_asec(2015)`, `add_asec_income_concepts()`,
`build_asec_tax_units()`, `build_asec_unit_table()` and
`add_filing_requirement()`, and optionally score `filing_model.R`. Columns:
`source = 'cps'`, `year = 2015`, `record_id = unit_id`,
`household_id = SERIAL`, `weight`, `married = joint`, `n_dep` capped at 3,
`wages = INCWAGE`, `interest = INCINT`, `dividends = INCDIVID`,
`capital_gains`, `other_agi` and `net_worth` missing, and a new `age`
column (CPS single-year age, plus the PUF's band, missing where the PUF
has none). Amounts are already in 2015 dollars.

Run time: the driver's header says one ASEC year is about 180,000 persons
and light enough for a login node; no log of a run was found. I assume a
few minutes as a Slurm job. Training and scoring are unchanged (about three
minutes on one GPU per run).

Two decisions for John before building:

1. **Which CPS units enter.** The PUF holds only filers. Options: keep all
   units (the population difference becomes part of the test); keep units
   required to file; or keep all units with weight times the production
   probability of filing. Dependent-headed units matter here too, since the
   PUF keeps dependent filers.
2. **Whether CPS interest is "interest".** At 2.3 times SOI it is a
   different concept. Shared, it is a live definition test; left out, the
   CPS shares only wages, dividends, married and dependents.

## 5. What the CPS would test that the SCF did not

Matched to the distortions in `10_TRANSLATION_EXPERIMENTS.md`:

- **D2, uneven scale, real not simulated:** interest 2.3 times SOI,
  dividends 0.4, wages about 1.0.
- **D3, definition:** interest includes IRA interest; dependents built by
  rule rather than claimed.
- **D4, rounding:** assumed (survey respondents round), not checked here.
- **D5, top-coding:** real, flagged on wages, interest and dividends.
- **Not in D1-D5:** a whole shared variable missing (capital gains), and a
  different population (non-filers), which the SCF had only partly.
