# Stage 3: the PUF items the model did not draw; results, 27 October

Candidate on branch `rebuild-puf-vars` (worktree `Tax-Data-puf`), outputs in
`/nfs/roberts/scratch/pi_nrs36/jar335/rebuild_puf`. Both models retrained, a new stack drawn, reweighted to stage 2's
746 targets (unchanged). Nothing is installed as latest. Full tables: `rebuild_puf/explore/stage3/tables_seed20261016.md`.

**The top is somewhat worse before the tilt, not clearly worse, and the tilt still meets every target.** Before the tilt the
stack has 0.0252 million returns with AGI of $10 million and over against stage 1's 0.0232 and the PUF's 0.0175
(2015 dollars and returns), and 14.1% of filer records have parts that add up to another AGI group than their label
(stage 1: 12.9%). After the tilt all 746 targets are met and 87.3% of the effective sample is kept (S2: 87.7%). No
fix was tried.

## 1. What was added

40 new slots, absent on the SCF, the CEX and the SIPP. The CPS does not measure 38 of them, so a non-filer has none
of those; it reports one dividend amount, which goes to div_pref (decision 7). Column names are production's (`config/variable_guide/baseline.csv`), so Tax-Simulator's names carry over.
"Drawn" means the model draws the item; "computed" means the file computes it from drawn parts, exactly.

| PUF item | File column | What it is | How it enters |
|---|---|---|---|
| E00650 | div_pref | qualified dividends | drawn |
| E00600 - E00650 | div_ord | ordinary dividends that are not qualified | drawn |
| E00600 | dividends (existing) | ordinary dividends | now computed: div_pref + div_ord |
| E03150 | trad_contr_ira | deductible IRA contributions | drawn |
| E03210 | sl_int_ded | student loan interest deduction | drawn |
| E03220 | ed_exp | educator expenses | drawn |
| E03230 | tuition_ded | tuition and fees deduction | drawn |
| E03240 | dpad | domestic production activities deduction | drawn |
| E03260 | se_tax_ded | deductible part of self-employment tax (not in production; part of the file's total) | drawn |
| E03270 | se_health | self-employed health insurance deduction | drawn |
| E03290 | hsa_ded | health savings account deduction (not in production; part of the file's total) | drawn |
| E03300 | keogh_contr | payments to Keogh, SEP and SIMPLE plans | drawn |
| E03400 | early_penalty | penalty on early withdrawal of savings | drawn |
| E03500 | alimony_exp | alimony paid | drawn |
| the 11 above | adjustments (existing) | statutory adjustments | now computed: the sum of the 11 |
| E24515 | kg_1250 | unrecaptured section 1250 gain | drawn |
| E24518 | kg_collect | 28% rate (collectibles) gain | drawn |
| E30400, E30500 and the partnership items | part_se | partnership income subject to self-employment tax (production's formula) | drawn |
| E17500 | med_exp | medical and dental expenses of itemizers, before the floor | drawn |
| E18400 | salt_inc_sales | state and local income or sales tax | drawn |
| E19800 | char_cash | cash charitable contributions | drawn |
| E20100 | char_noncash | noncash charitable contributions | drawn |
| E20500 | casualty_exp | casualty and theft losses | drawn |
| E20550 | job_exp | unreimbursed employee business expenses | drawn |
| E20600 | tax_prep_exp | tax preparation fees | drawn |
| E20400 | other_misc_exp | other miscellaneous deductions subject to the 2% floor | drawn |
| P04470, E21040 and the items | other_item_exp | other itemized deductions (production's formula, a remainder) | drawn |
| P60100 | amt_nols | AMT net operating loss and other adjustments | drawn |
| E62900 | amt_ftc | AMT foreign tax credit | drawn |
| E07300 | ftc | foreign tax credit | drawn |
| P87482 | aoc_exp | American opportunity credit expenses (up to $4,000) | drawn |
| E87530 | llc_exp | lifetime learning credit expenses | drawn |
| E07200 | old_cred | credit for the elderly or disabled | drawn |
| E07240 | savers_cred | retirement savings contributions credit | drawn |
| E07260 | res_energy_cred | residential energy credits | drawn |
| E07400 | gbc | general business credit | drawn |
| E07600 | prior_yr_cred | credit for prior-year minimum tax | drawn |
| P08000 | other_nonref | other nonrefundable credits | drawn |
| E11560 | net_ptc | net premium tax credit | drawn |
| E85785 | excess_ptc | excess advance premium tax credit repayment | drawn |
| E09700 | recapture_tax | recapture taxes | drawn |
| E09900 | ira_penalty | additional tax on IRAs and other retirement plans | drawn |

Every item the brief named is on the 2015 PUF. On the table's PUF rows each new item equals the raw PUF record by
record (`explore/stage3/check_table.md`); dividends and the adjustments equal their parts with no gap at all.

## 2. The new items in the 2019 file against the 2015 PUF grown to 2019

Both sides are filers other than dependent filers. The PUF side is the pipeline's tax data rule: weights scaled by SOI
Table 1.6 in each filing status and age range, dollars grown per return with the item. Items without an SOI Table 1.4
line (itemized deductions from med_exp down, AMT items, credits, other taxes) are in 2015 dollars on both sides. Units
in millions, dollars in billions. Returns: PUF grown 148.5 million, file 147.9 million.

| Item | PUF: units | file before tilt: units | file after: units | PUF: $bn | file before: $bn | file after: $bn | $ after / PUF |
|---|---:|---:|---:|---:|---:|---:|---:|
| div_pref | 26.52 | 27.28 | 26.69 | 232.9 | 273.6 | 259.5 | 1.11 |
| div_ord | 16.86 | 17.28 | 17.22 | 69.8 | 77.1 | 73.7 | 1.06 |
| trad_contr_ira | 2.74 | 2.80 | 2.85 | 14.1 | 14.7 | 15.0 | 1.06 |
| sl_int_ded | 12.89 | 12.22 | 12.33 | 14.4 | 13.7 | 14.0 | 0.97 |
| ed_exp | 3.83 | 3.71 | 3.50 | 1.0 | 1.0 | 0.9 | 0.91 |
| tuition_ded | 1.70 | 1.49 | 1.43 | 4.0 | 3.6 | 3.4 | 0.85 |
| dpad | 0.74 | 0.69 | 0.68 | 3.2 | 3.3 | 3.3 | 1.01 |
| se_tax_ded | 19.87 | 18.43 | 20.18 | 32.9 | 31.9 | 33.6 | 1.02 |
| se_health | 4.28 | 4.14 | 4.48 | 34.5 | 33.0 | 34.6 | 1.00 |
| hsa_ded | 1.40 | 1.40 | 1.44 | 4.0 | 4.0 | 4.1 | 1.02 |
| keogh_contr | 1.03 | 1.02 | 1.03 | 27.6 | 25.0 | 24.1 | 0.87 |
| early_penalty | 0.51 | 0.52 | 0.55 | 0.2 | 0.2 | 0.2 | 1.05 |
| alimony_exp | 0.55 | 0.57 | 0.57 | 9.1 | 9.7 | 9.4 | 1.04 |
| kg_1250 | 1.25 | 1.13 | 1.12 | 29.0 | 32.0 | 30.3 | 1.04 |
| kg_collect | 0.07 | 0.06 | 0.06 | 5.9 | 7.3 | 7.5 | 1.28 |
| part_se | 2.50 | 2.39 | 2.48 | 53.8 | 43.6 | 60.9 | 1.13 |
| med_exp | 9.47 | 8.64 | 7.81 | 143.2 | 132.6 | 123.6 | 0.86 |
| salt_inc_sales | 44.10 | 43.86 | 37.40 | 350.2 | 364.2 | 310.5 | 0.89 |
| char_cash | 34.65 | 34.42 | 29.69 | 162.1 | 171.8 | 152.2 | 0.94 |
| char_noncash | 23.47 | 22.65 | 19.17 | 58.7 | 69.0 | 60.2 | 1.03 |
| casualty_exp | 0.07 | 0.06 | 0.05 | 1.5 | 2.1 | 1.6 | 1.11 |
| job_exp | 15.11 | 15.08 | 12.24 | 100.2 | 100.7 | 84.5 | 0.84 |
| tax_prep_exp | 21.61 | 21.06 | 17.82 | 8.4 | 8.4 | 7.5 | 0.90 |
| other_misc_exp | 28.98 | 28.42 | 24.25 | 162.6 | 173.7 | 148.0 | 0.91 |
| other_item_exp | 45.65 | 45.29 | 39.40 | 40.3 | 50.3 | 48.2 | 1.20 |
| amt_nols | 0.36 | 0.39 | 0.35 | 68.6 | 89.4 | 75.6 | 1.10 |
| amt_ftc | 2.77 | 2.70 | 2.80 | 18.5 | 24.5 | 28.5 | 1.54 |
| ftc | 8.33 | 8.60 | 8.88 | 21.8 | 30.8 | 36.0 | 1.65 |
| aoc_exp | 9.79 | 9.54 | 9.25 | 32.5 | 31.9 | 31.0 | 0.95 |
| llc_exp | 2.62 | 2.59 | 2.65 | 18.3 | 18.4 | 19.0 | 1.04 |
| old_cred | 0.06 | 0.09 | 0.08 | 0.0 | 0.0 | 0.0 | 1.34 |
| savers_cred | 8.28 | 8.43 | 8.64 | 1.5 | 1.5 | 1.6 | 1.07 |
| res_energy_cred | 2.70 | 2.60 | 2.39 | 2.2 | 2.2 | 2.1 | 0.95 |
| gbc | 0.36 | 0.36 | 0.35 | 3.5 | 4.4 | 4.4 | 1.26 |
| prior_yr_cred | 0.31 | 0.28 | 0.27 | 0.9 | 0.9 | 0.9 | 0.93 |
| other_nonref | 0.23 | 0.22 | 0.22 | 0.7 | 0.8 | 0.7 | 1.06 |
| net_ptc | 2.54 | 2.43 | 2.50 | 1.6 | 1.6 | 1.7 | 1.02 |
| excess_ptc | 3.46 | 3.34 | 3.42 | 3.8 | 3.7 | 3.8 | 1.01 |
| recapture_tax | 0.00 | 0.00 | 0.00 | 0.0 | 0.0 | 0.0 | 0.42 |
| ira_penalty | 5.55 | 5.22 | 4.82 | 6.2 | 5.3 | 4.8 | 0.77 |

By AGI group (seven groups, each side its own 2019 AGI; `rebuild_puf/explore/stage3/tables_seed20261016.md` and
`extras.md` have every cell). Where the file departs from the PUF:
- **Itemized deductions: about 13% fewer itemizers after the tilt.** Before the tilt the file has 46.3 million
  returns with any itemized item, the PUF grown 46.6 million; after the tilt 40.4 million. The tilt multiplies the
  weight of itemizers with a state income tax refund by 0.21 on average (the 2019 Table 1.3 refund targets from stage
  2: after the 2017 law few 2019 returns have a taxable refund), and refunds sit almost only on itemizers. Every
  itemized item loses holders by 15-20% in the middle groups ($25,000-200,000), little at $200,000 and over. The mean
  among itemizers with the item is within 10% of the PUF's for every item but noncash charity (+26%), casualty
  losses (+38%, 0.05 million returns) and other itemized deductions (+39%). S2 already had this: mortgage interest
  paid fell 12.9% from stage 1 to S2.
- **Itemizers at AGI below $1: 0.26 million against the PUF's 0.04 million**, with $2.5 billion of state and local
  income tax against $0.5 billion. At AGI $1-25,000 the file has $6.5 billion of state and local income tax against
  the PUF's $2.2 billion, on fewer holders (2.17 against 2.33 million).
- **AMT net operating loss and other adjustments (amt_nols).** On the PUF $66 billion of its $69 billion is on
  returns with AGI below $1. The file has $41 billion there and spreads the rest upward: $15.1 billion at $1 million
  and over against $0.6 billion, $2-5 billion in each middle group against under $0.6 billion.
- **Foreign tax credit and AMT foreign tax credit: 54-65% too many dollars.** The holders are right (8.9 against
  8.3 million); the dollars are too high at AGI below $50,000 ($4.8 billion against $0.1 billion) and at
  $200,000-1 million ($12.8 against $7.3 billion).
- **Items the law limits by income** come out a little past the limit: student loan interest at $200,000-1 million
  (0.05 million returns, none on the PUF), the American opportunity credit's expenses at $200,000-1 million (0.12
  against 0.01 million). Tax-Simulator applies the phase-outs itself; these records' credits would be zero there.
- **28% collectibles gain** ($7.5 against $5.9 billion, 0.06 million returns): the extra is at $200,000-1 million.
- Qualified dividends are 11% high in dollars after the tilt ($259 against $233 billion; SOI 2019 published, all
  returns: $248 billion), most of it at $1 million and over (+27%). Ordinary dividends, the targeted total, are met.
- The adjustments add up to SOI's 2019 lines within a few percent where the 2015 PUF does (self-employment tax $33.6
  against SOI's $33.6 billion), but the pipeline's growth rule carries 2015 holders into 2019 for deductions the law
  changed: the domestic production deduction has 0.68 million returns and $3.3 billion against SOI 2019's 0.06
  million and $0.26 billion (repealed after 2017); alimony paid $9.4 against $12.4 billion.
- part_se and other_item_exp are net amounts with many negatives; their dollar totals move with small shifts
  (part_se $43.6 billion before the tilt, $60.9 after, PUF $53.8). Recapture taxes rest on 4 PUF records.

## 3. Did anything get worse? Against stage 2's run S2

Both reweight to the same 746 targets with the same settings; S2 reweighted stage 1's stack, stage 3 a new stack
from the retrained models.

| | S2 | Stage 3 |
|---|---:|---:|
| targets met | 746 of 746 | 746 of 746 |
| effective sample kept | 87.7% | 87.3% |
| KL move (nats) | 0.0941 | 0.0972 |
| largest weight factor in a targeted cell | 206 | 381 |
| weight factor, 1st and 99th percentiles | 0.069, 3.13 | 0.047, 3.24 |
| records moved more than tenfold up, down | 153, 12,687 | 188, 17,781 |
| thin cells: warnings, stops | 98, 59 | 109, 54 |
| converged | yes | yes |

The largest factor, 381, is one joint record with no AGI that sits in several cells (interest, farm income and
partnership losses at no AGI, and Table 1.3's joint cells).

Filer records before the tilt, 2015 dollars and returns (millions):

| | PUF 2015 | Stage 1 stack | Stage 3 stack |
|---|---:|---:|---:|
| AGI $10 million and over | 0.0175 | 0.0232 | 0.0252 |
| AGI $1 million and over | 0.443 | 0.503 | 0.541 |
| AGI at or below zero | 2.02 | 2.95 | 3.00 |
| records whose parts add up to another AGI group than their label | | 12.9% | 14.1% |

Stage 1 and the latest run (other seeds of the same design) differ by about as much ($10 million and over:
0.0232 against 0.0260 by the latest run's own report), so part of this is what a new pair of seeds gives.

Totals after the tilt, nominal 2019 dollars ($ billion): wages 8,616 and 8,598 (S2, stage 3); dividends 352.3 and
351.6; capital gains 852.9 and 848.6; business income 1,255 and 1,257; mortgage interest paid 266.3 and 268.1; net
worth $97.3 and $97.0 trillion; total consumption 6,892 and 6,878. Larger moves, none targeted: gross Social
Security plus pensions +4.5% (2,201 to 2,299), adjustments +3.7% (137.8 to 143.0; SOI 2019: 144.9), childcare -11%
(63.5 to 56.5; stage 1 to the latest moved as much), the spouse's wages -2.8%. Published SOI cells not targeted:
the median miss falls slightly (Table 1.4: 1.31 to 1.29 standard errors; Table 1.6: 1.60 to 1.55; Table 1.7: 2.23
to 2.20). The largest moves are in thin cells: tax-exempt interest at AGI under $30,000 (for example $0.27 to $1.61
billion at $20,000-25,000, published $0.40 billion), and dependent filers' tax-exempt interest ($0.09 to $0.44
billion, published $0.18 billion).

Tips and auto loans after the tilt: tips $47.6 and $49.4 billion on 6.04 and 6.11 million units; auto loan interest
$44.8 and $44.4 billion on 49.6 and 49.4 million units. Their targets are met in both runs.

## 4. Training time and memory

| Model | Card | Minutes | Seconds a pass (median) | Peak GPU memory (GB) |
|---|---|---:|---:|---:|
| stage 1, seed 20261016 | H100 | 66.8 | 50.1 | 8.78 |
| stage 1, seed 20261017 | H200 | 34.8 | 25.8 | 8.79 |
| stage 3, seed 20261016 | H100 | 72.7 | 54.7 | 9.13 |
| stage 3, seed 20261017 | H100 | 73.1 | 54.0 | 9.13 |

On the same card a pass takes 9% longer and peak memory is 4% higher: a PUF record now carries 90 slots instead
of 50, and PUF records are a fifth of the table. Generation took 15 minutes on an H100 (stage 1: 8 minutes), since a
filer record now takes 40 more drawing steps. The tilt and its report took 14 minutes on the same node. The
held-back loss per slot is lower (PUF 0.668 against 0.825), because the new slots are mostly zero and easy to
predict; it is not comparable across the two tables.

## 5. Decisions, for approval or rejection

1. **The list.** The inventory file named in the brief was gone (a scratch folder under /tmp); the list is the
   brief's, checked against production's variable guide and `src/process_puf.R`. Every item is on the 2015 PUF.
2. **Production's names** for every new column, so Tax-Simulator's names carry over.
3. **Two adjustments production does not carry** are added, the deductible part of self-employment tax (E03260) and
   the HSA deduction (E03290), because the file's adjustments total already contained them and must stay the exact
   sum of its parts. Tax-Simulator computes the self-employment tax deduction itself.
4. **Dividends and adjustments computed from parts** at generation and in the dollars; the computed dividends match
   to the dollar and the adjustments to 6e-11 dollars (floating point).
5. **Growth.** Each new item with an SOI Table 1.4 line grows per return with it (qualified dividends; ordinary
   dividends for the non-qualified part; each adjustment; Schedule D gains for the two gains, losses not grown;
   partnership and S corporation income for part_se). The itemized deduction items, AMT items, credits and other
   taxes have no Table 1.4 line and stay in 2015 dollars, as mortgage interest and real estate tax already do. A
   consequence: items the 2017 law changed (the domestic production deduction, miscellaneous deductions, itemizing)
   come out at their 2015 incidence before the tilt.
6. **part_se and other_item_exp by production's formulas**, negatives and all. other_item_exp is negative on 28,000
   PUF records (15.0 million returns, -$6.3 billion), mostly by under $500: the PUF rounds each item on its own and
   the remainder carries the rounding. Flooring it at zero would add about $6 billion of deductions; left for John.
   The medical floor in it is 7.5% of AGI at 65+ and 10% otherwise; a PUF record with no age range counts as 65+ when
   step 0 gives 65+ more than half.
7. **The CPS's dividends go to qualified dividends**, matched by rank to the tax data's ordinary dividends (a new
   translation-table setting, `rank_against`), so the CPS's translated dividends stay as before up to the new growth
   of the parts (2019: $300.8 against $308.1 billion; 2015 identical). Non-qualified dividends are zero on the CPS.
   Non-filers therefore hold only qualified dividends.
8. **The CPS's adjustments row is gone.** It was zero (the CPS never asked); with the total computed from parts it
   would have needed eleven zero rows, whose growth the tax data for 2021 could not find (below). Non-filers still
   have no adjustments.
9. **The AGI group label is unchanged.** Band-AGI reads the dividends and adjustments bands as before, so every PUF
   row's label (and q) equals stage 1's; the table's PUF, SCF, CEX and SIPP rows are identical to stage 1's.
10. **No itemizer flag.** The PUF's FDED is not drawn; Tax-Simulator chooses between the standard and itemized
    deductions itself. This page counts a unit as an itemizer when any itemized item is nonzero (on the PUF, every
    record with an itemized item but two has FDED 1).
11. **No consistency rules among the new items** (for example, unrecaptured section 1250 gain at most long-term gains:
    108 PUF records already break it).
12. **Pretraining settings unchanged** (S8): the table trains with them at 9% more time a pass.
13. **No targets added**; the 746 of S2 are used as they are (same values and errors).

## 6. Open for John

- **A target year from 2020 on will stop at the dollar maps.** SOI Table 1.4 has no line for the domestic production
  deduction from 2020 (repealed) or the tuition and fees deduction from 2021, so per-return growth has nothing to
  grow them by. 2019 is not affected. A rule is needed for deductions the law removed (zero in those years, or no
  growth).
- other_item_exp's negatives (decision 6).
- The itemizer drop after the tilt comes from stage 2's refund targets; whether 2019's itemizers should be targeted
  (SOI 2019: 17.3 million itemizers after the 2017 law) is a target question for later, as is tax calculator work.

## 7. Files and jobs

- Code (commits on `rebuild-puf-vars`): `build_table.py` (STAGE3, DIV_PARTS, ADJ_PARTS, NEW_ITEMS; rank_against in
  the tax data and function 1), `targets.py` (the growth lines), `dollars.py` and `tilt.py` (the two computed totals),
  `translate.py` (rank_against), `settings.json` (the CPS's rows), `generate.py` (docstring); `FEATURES.md` has a
  candidate paragraph.
- Outputs: `rebuild_puf/model/label_seed20261016`, `label_seed20261017`, `model/quick` (the quick test);
  `gen/seed20261016`; `tilt/seed20261016`; checks and tables in `rebuild_puf/explore/stage3/` (`check_table.md`,
  `tables_seed20261016.md`, `extras.md`, `before_tilt_*.csv`).
- Jobs: CPU stages 30953541; table 30957103; quick test 30957496; training 30957529 and 30957530 (H100);
  generate, tilt, report 30958749 (its last step, the translation report, fails as in stage 1 and was run as 30958770
  with REBUILD_RUN=label_seed20261016).

