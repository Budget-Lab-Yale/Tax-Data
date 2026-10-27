# Stage 2: targets for Tables 1.3 and 4.3, tips and auto loan interest; results, 27 October

Candidate on branch `rebuild-tips-autoloan` (worktree `Tax-Data-tips`), outputs in
`/nfs/roberts/scratch/pi_nrs36/jar335/rebuild_tips`. Nothing was retrained or regenerated: every run below reweights
the same 454,379 records of stage 1 (`gen/seed20261016`), on a CPU node. Nothing is installed as latest.

## 1. What was added

Every target of the 496 keeps its value and standard error (checked: all 496 identical to stage 1's list, largest
relative difference 7e-14). 250 targets were added, 746 in all.

| Family | Targets | Source | Error |
|---|---:|---|---|
| SOI Table 1.3: returns, AGI, and the number with and amount of 22 income items, in each of the four filing statuses | 184 | SOI Pub. 1304 Table 1.3, 2019 (local IRS-Ind store, `bystatus_all_items.csv`) | borrowed from the nearest Table 1.4 cells |
| SOI Table 4.3: returns, AGI, and the number with and amount of 8 income items, top 0.01% (one group), top 0.1%, top 1% of AGI, returns other than dependent filers' | 54 | SOI Table 4.3, 2019 (`pctile_top_items.csv`) | borrowed; the top 0.01% (AGI cutoff $12.6 million) 1% of the value by the full-sample rule |
| Tips: returns with Social Security tips (W-2 box 7) and the amount, joint and non-joint returns | 4 | SOI Form W-2 statistics 2019, Table 4.B | borrowed, plus a definition gap on the non-joint cells |
| Auto loan interest: share of units with any, and dollars per unit, in the four income groups of the wealth grid | 8 | SCF 2016, 2019 and 2022 waves pooled with the year's shares | SCF replicate weights and implicates; held to half a standard error |

The 238 Table 1.3 and 4.3 targets are run G2's (Tax-Data-targets, commit 4bbab31): the same keys, values and
standard errors (checked: 238 of 238 identical). They are now ordinary pipeline configuration: entries in
`targets_config/historical.yaml`, variables and groupings in `catalog.yaml`, G2's general code in `target_list.py`,
the files read through settings `paths.raw.soi_tables`.

**Tips targets** (nominal 2019 dollars):

| Returns | SOI W-2, published | Target | Standard error | Start | After S2 |
|---|---:|---:|---:|---:|---:|
| joint: returns with tips | 1,158,534 | 1,158,534 | 8,689 (0.75%) | 594,500 | 1,150,000 |
| joint: tips | $9.80 bn | $9.80 bn | $0.41 bn (4.2%) | $6.76 bn | $10.21 bn |
| non-joint: returns with tips | 5,348,368 | 4,810,500 | 275,100 (5.7%) | 2,816,000 | 4,535,000 |
| non-joint: tips | $32.58 bn | $31.63 bn | $0.75 bn (2.4%) | $22.22 bn | $32.38 bn |

The file is measured as box 7 holds tips (decision 4). Non-joint targets leave dependent filers out (decision 5).
All four ended at an edge of their range: the counts one standard error low, the amounts one standard error high.

**Auto loan interest targets** (SCF pooled; units other than dependent filers; income groups by the seven income
items the SCF and the file share, each side by its own percentiles):

| Income group | Share with any: SCF | se | Start | After S2 | Dollars per unit: SCF | se | Start | After S2 |
|---|---:|---:|---:|---:|---:|---:|---:|---:|
| below p90 | 28.13% | 0.38 | 29.03% | 28.32% | $257.2 | 5.9 | $259.7 | $256.1 |
| p90-99 | 40.19% | 1.10 | 38.57% | 39.64% | $352.7 | 15.8 | $341.0 | $348.9 |
| p99-99.9 | 14.64% | 2.52 | 15.73% | 14.57% | $216.2 | 51.0 | $181.6 | $190.7 |
| p99.9+ | 3.02% | 1.13 | 6.02% | 3.58% | $48.8 | 22.0 | $91.8 | $59.9 |

## 2. The runs

| Run | Targets | Outside range | Effective sample kept (%) | KL move (nats) | Largest weight factor in a targeted cell | Thin-cell warnings | Stops | Converged | Minutes |
|---|---:|---:|---:|---:|---:|---:|---:|:---|---:|
| stage 1 (current targets) | 496 | 0 | 97.50 | 0.0149 | 307 | 53 | 27 | yes | 7.4 |
| G2 on these records | 734 | 0 | 88.74 | 0.0895 | 205 | 100 | 58 | yes | 15.5 |
| G2 + tips | 738 | 28 | 87.77 | 0.0938 | 203 | 102 | 54 | no | 39.6 |
| G2 + auto loans | 742 | 0 | 88.69 | 0.0899 | 207 | 100 | 57 | yes | 20.1 |
| **S2** (all) | **746** | **0** | **87.73** | **0.0941** | **206** | **98** | **59** | **yes** | **26.2** |

Minutes are the tilt and its report. S2 met every target, so no S2b was needed. To see what each family costs, G2
was also run with each new family alone: the tips targets cost about one point of the effective sample, the auto
loan targets about 0.05. The tips-only run stopped at the solver's step limit with 28 targets just outside their
ranges (the largest 0.42 standard errors beyond, most under 0.05); S2, which holds the same targets and more,
converged, so this is the solver's path, not a conflict among the targets. Of the new targets only one is flagged:
auto loan dollars at income p99-99.9, a warning (a weight factor of 13.5). On the latest run's records G2 kept 88.4%
of the effective sample (Tax-Data-targets); on these, 88.7%.

## 3. Tips against their sources

Tips in every unit, before the box 7 limit (nominal 2019 dollars). W-2 2019: 6.59 million taxpayers with box 7 tips
on 6.51 million returns, $42.4 billion, filers only, dependent filers included.

| | Stage 1 | S2 | SIPP 2019 | Production |
|---|---:|---:|---:|---:|
| units with tips (millions) | 3.67 | 6.04 | 3.76 | 6.93 |
| tips ($ billion) | 33.95 | 47.55 | 33.64 | 43.87 |
| mean per unit with tips ($) | 9,249 | 7,872 | 8,956 | 6,328 |
| share of tips from leisure and hospitality | 67.3% | 69.7% | 72.9% | 61% |
| units with tips, primary filer under 26 (millions) | 1.12 | 1.87 | 1.32 | |

S2's $47.6 billion includes $2.2 billion on non-filers (not targeted: W-2 statistics cover filers only) and $2.8
billion above the box 7 limit on filers.

By the unit's wages:

| Wages | Stage 1: units (m) | Stage 1: $bn | S2: units (m) | S2: $bn | S2: mean ($) | SIPP: units (m) | SIPP: $bn | Production: units (m) | Production: $bn |
|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| $1-25k | 1.68 | 6.84 | 2.72 | 10.65 | 3,919 | 1.49 | 5.45 | 3.90 | 10.40 |
| $25k-50k | 1.17 | 10.47 | 1.91 | 15.89 | 8,333 | 1.31 | 12.71 | 1.63 | 12.47 |
| $50k-100k | 0.62 | 9.32 | 1.09 | 13.38 | 12,265 | 0.71 | 10.49 | 1.07 | 12.40 |
| $100k-200k | 0.16 | 3.41 | 0.27 | 4.32 | 15,880 | 0.20 | 4.14 | 0.28 | 6.64 |
| $200k+ | 0.04 | 3.90 | 0.05 | 3.31 | 62,652 | 0.05 | 0.86 | 0.06 | 1.97 |

The tilt meets the W-2 counts by raising the weight of the records that already have tips, by 1.66 times on
average (3.43 to 5.71 million filers with tips), more on those with small tips. The file's pattern by wages and age is the SIPP's, scaled up.

**Stage 1's decision 7 (tips on primary filers whose own wages are over $200,000).** Not changed. How the targets
change the cell:

| | Primary filers with tips (millions) | Records | Their tips ($bn) | Of which inside the box 7 limit ($bn) |
|---|---:|---:|---:|---:|
| start | 0.014 | 441 | 3.06 | 0.41 |
| stage 1 | 0.013 | 441 | 2.75 | 0.39 |
| G2 | 0.013 | 441 | 2.60 | 0.33 |
| S2 | 0.012 | 441 | 2.08 | 0.21 |
| SIPP 2019 | 0.006 | 1 | 0.06 | |
| production | 0.034 | 945 | 1.23 | |

The box 7 measure counts only $0.2-0.4 billion of these tips, because the people's other wages are near or above
the $132,900 wage base, so the targets barely see the cell. It fell by a quarter in S2 against stage 1 because the
amount targets sit at the top of their ranges and the tilt lowers weight where tips per return are large. It is
still $2.1 billion against the SIPP's $0.06 billion.

## 4. Auto loan interest against its sources

| | Stage 1 | S2 | SCF pooled | Production |
|---|---:|---:|---:|---:|
| units with any (millions) | 51.05 | 49.59 | 47.32 (of 162.76) | 52.22 |
| total ($ billion) | 45.93 | 44.79 | 43.16 | 62.15 |
| mean per unit with any ($) | 900 | 903 | 912 | 1,190 |
| per unit, all units ($) | 257 | 250 | 265 | 339 |

The file had a little too many holders below p90 and twice the SCF's share at p99.9 and over before the tilt; the
targets bring both to the SCF. The total falls 2.5%. The file still has more units than the SCF (178.9 against
162.8 million), so its total stays above the SCF's at about the SCF's dollars per unit.

## 5. What else moved

Totals after the tilt ($ billion):

| Item | Stage 1 | G2 | S2 | S2 against stage 1 |
|---|---:|---:|---:|---:|
| wages | 8,630.7 | 8,617.6 | 8,616.3 | -0.2% |
| capital gains | 853.7 | 852.7 | 852.9 | -0.1% |
| business income | 1,256.8 | 1,255.4 | 1,255.5 | -0.1% |
| estate and trust income | 49.8 | 38.9 | 39.0 | -21.8% |
| other gains (Form 4797) | -6.2 | 18.5 | 18.5 | |
| taxable pensions | 869.0 | 848.0 | 847.8 | -2.4% |
| taxable IRA distributions | 313.8 | 324.6 | 324.6 | +3.4% |
| unemployment compensation | 32.5 | 23.4 | 23.4 | -27.9% |
| mortgage interest paid | 305.9 | 266.1 | 266.3 | -12.9% |
| net worth ($ trillion) | 97.26 | 97.32 | 97.31 | +0.1% |
| total consumption | 6,884.7 | 6,889.0 | 6,892.2 | +0.1% |
| childcare | 64.9 | 63.5 | 63.5 | -2.1% |
| tips | 33.9 | 34.2 | 47.6 | +40.1% |
| auto loan interest | 45.9 | 45.7 | 44.8 | -2.5% |

Every move but tips and auto loans comes from G2's Table 1.3 targets, and they are the ones the G2 page reported:
state income tax refunds fall to SOI's levels (all AGI, $27.4 billion to $5.7 billion; SOI $5.6 billion),
estate and trust income falls, Form 4797 gains turn positive. The published SOI cells not targeted fit better: the
median miss on Table 1.4's falls from 1.49 to 1.31 standard errors, Table 1.6's from 1.83 to 1.60; Table 1.7's goes
from 2.15 to 2.23.

**Tips and auto loans against G2 alone** (S2 against G2, cells of 200 or more records, tips and auto loan items left
out): the largest moves are other lines of credit with the primary filer 45-54 (+11%), partnership nonpassive
income with the primary filer under 26 (+11%), and under 5% everywhere else. Among published cells not targeted,
the largest is estate and trust net income at AGI $1-1.5 million (+10%). Filers by AGI are unchanged ($1 million
and over: 0.559 million in both; at or below zero: 2.159 million in both). Full tables:
`rebuild_tips/explore/stage2/tables.md`.

## 6. Decisions, for approval or rejection

1. **G2's targets as pipeline configuration.** Table 1.3, Table 4.3 and auto loans go in `historical.yaml` (published
   every year; the SCF through the year's mix); tips in `years/2019.yaml`, since the W-2 reader is built for the 2019
   and 2020 layout only. Kinds `soi 1.3` and `soi 4.3` are kept from G2; the returns targets are named "returns"
   (G2's had a blank variable name).
2. **The IRS-Ind store's files changed after G2 copied them.** The 2019 rows are identical except the label of Table
   4.3's AGI cutoff ("cutoff for agi" became "adjusted gross income floor on percentiles (whole dollars)"); the
   catalog uses the new label. The pipeline now reads the store itself, not G2's copies.
3. **Tips from the W-2 statistics, not the SIPP.** SOI publishes 2019 box 7 counts and amounts, so those are the
   targets; it publishes nothing on tips by wage size (Table 4.C has percentiles of box 7 among recipients, not
   used). Joint and non-joint returns are the published split; the joint one-earner and two-earner columns are left
   out to keep the set small.
4. **The file's tips measured as box 7 holds them**: each person's tips at most the Social Security wage base
   ($132,900 in 2019) less the person's other wages (catalog variable `ss_tips`, wage bases 2015-2024). On the start
   this takes $2.0 billion off joint returns and $1.4 billion off non-joint, all of it from people whose wages pass the base.
5. **Dependent filers.** The W-2 counts include them; the file gives them no tips (stage 1, decision 11). The
   non-joint targets are reduced by the SIPP 2019's share of dependents among non-joint tipped people: 10.1% of
   people, 2.9% of tips (0.32 million tipped dependents, 59 records). This assumes every tipped dependent files.
   Half the reduction is added to the error, which is why the non-joint count has a 5.7% error.
6. **Population: filers other than dependent filers.** The W-2 statistics cover only W-2s on filed returns, so the
   file's non-filers keep their tips untargeted ($2.2 billion in S2).
7. **Tips errors borrowed from Table 1.4**, the pipeline's rule for other tables of SOI's sample (the W-2 statistics
   are estimates from SOI's sample of returns).
8. **Auto loans per unit, not as totals**: the share with any and dollars per unit in each income group. The SCF's
   units add to 162.76 million, the file's units other than dependent filers to 169.5 million; totals would force
   the file's dollars per unit about 4% below the SCF's. Half a standard error, as the other SCF targets (settings
   `tilt.range_in_se_by_kind`). The two top groups are thin (250-440 SCF households a wave; errors of 17-45%).
9. **Variants by configuration copies.** G2 on these records and the split runs read a copy of `targets_config/`
   with the new families removed (settings key `targets_config`, from G2's code; `explore/stage2/make_variants.py`).
   A new environment variable, `REBUILD_TILT_RUN` (`common.py`), writes a reweighting of the run's generated records
   to `tilt/<name>`, so the variants sit next to stage 1's run: `tilt/S2`, `tilt/G2`, `tilt/G2_tips`, `tilt/G2_auto`.
10. **No S2b.** S2 met every target with 87.7% of the effective sample kept, one point below G2 alone.
11. **Stage 1's decision 7 untouched.** Tips are drawn as before; section 3 shows the cell.

## 7. Open for John

- The W-2 count of tipped returns is 1.7 times the SIPP's tipped units. The tilt closes it by raising the weight of
  tipped records 1.66 times; the generated records themselves still have the SIPP's share of tipped workers.
  Drawing more tipped workers at generation would be the other way to do it.
- Decision 7 (tips at wages over $200,000) is still open; the box 7 measure means these targets cannot fix it.

## 8. Files and jobs

- Code: `pipeline/target_list.py` (G2's code; `published_w2`, `w2_dependent_share`, the `ss_tips` measure and the
  `any` transform), `scf_targets.py` (the average statistic), `errors.py` (auto loans among the SCF kinds),
  `common.py` (`REBUILD_TILT_RUN`), `settings.json` (`paths.raw.soi_tables`, `paths.raw.soi_w2`, auto loans at half a
  standard error), `targets_config/catalog.yaml`, `historical.yaml`, `years/2019.yaml`; `FEATURES.md` has a
  candidate paragraph.
- Runs: `rebuild_tips/tilt/S2`, `tilt/G2`, `tilt/G2_tips`, `tilt/G2_auto`; tables and scripts in
  `rebuild_tips/explore/stage2/` (`check_targets.py`, `make_variants.py`, `compare.py`, `tables.md`).
- Jobs (CPU, day partition): S2 30941035, G2 30941036, G2 + tips 30942685, G2 + auto loans 30942686; target check
  30941016; tables 30951204.
