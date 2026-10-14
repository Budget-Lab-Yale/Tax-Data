---
title: "Test 4, probe X: the real SCF translated into PUF concepts"
role: results
workstream: rebuild
status: current
updated: 2026-10-10
sot: research/rebuild/tests/probes/SPEC.md
supersedes: []
superseded_by: null
---

# Probe X: the real SCF translated into PUF concepts

The question: test 3 found that SCF wealth targets carried onto PUF groups
contradict each other (largest disagreement 3.5 percent of all weight on
the core targets, 6.9 percent on the full set), and that completed PUF
wealth has a median of $55,000 to $75,000 against the SCF's $91,000. The
plan (D24) says both come from the SCF measuring the shared variables
differently, and that translating them into PUF concepts fixes both. This
probe built a first real translation of the SCF 2013 and 2016 filer units,
rebuilt the targets, retrained the joint model and checked both claims.

**Short answer.** Translation removes the part of the target disagreement
that comes from the income items (full set: 6.9 to 3.7 percent); what
remains (3.5 percent, the same in the core set before and after) comes
from population differences that translation cannot touch: the SCF
measures wealth only for each household's main unit, not for split-off
units (other financially independent adults in the household, 12 to 14
percent of SCF filer weight), and the PUF includes dependent filers. For
completed PUF wealth, translation brings mean net worth by income group
close to the SCF (it was 35 to 100 percent too high in the bottom half and
the 50th to 90th percentiles; now within about 10 percent in every group),
but it lowers the median further (to $32,000 to $50,000), and home
ownership in the bottom half of income stays far below the SCF's. The SCF's $91,000 median is itself the wrong
benchmark: it leaves out split-off units, and counting them at zero wealth
gives $57,000. The tilt needs about a fifth less change to the model to
hit the translated targets (0.57 against 0.69 nats per unit of weight).

Every number below comes from a Slurm job run for this probe (logs in
`/nfs/roberts/scratch/pi_nrs36/jar335/probes/logs/x_*`) or from test 3's
files where marked. All are weighted aggregates.

## What was run

`$Q` = `/nfs/roberts/scratch/pi_nrs36/jar335/probes/x`. Scripts are in this
folder.

| Step | Script | Jobs | Output |
|---|---|---|---|
| Translation (main version, `full_v3x2`) | `x_translate.py` | 28923972 | `$Q/data/full_v3x2/`, movement tables `$Q/translate_full_v3x2/` |
| Translation, first version (`full_v3x`, kept as a sensitivity check) | same, `XVER=full_v3x` | 28922552 | `$Q/data/full_v3x/`, `$Q/translate/` |
| Arm P targets rebuilt (`a_full.py` unchanged, pointed at the new data) | `x_targets.py build` | 28924157, 28922600 | `$Q/a_v3x2/`, `$Q/a_v3x/` |
| Disagreement measure, before and after | `x_targets.py measure` | 28925440 | `$Q/x_targets_disagreement.json` |
| Joint model retrained, seeds 1 and 2, each version (copy of `h_train.sbatch`, same settings) | `x_train.sbatch` | 28924153/4, 28922598/9 | `$Q/model/full_v3x2/`, `$Q/model/full_v3x/` |
| PUF training records completed with wealth, five draws (copy of `h_complete.sbatch`) | `x_complete.sbatch` | 28924155/6, 28922880/1 | `h_puf_wealth_seed0N.parquet` in the model folders |
| SCF training records' wealth redrawn, seed 2 | `x_complete.sbatch ... scf_train` | 28924156, 28923753 | `h_scf_train_seed02.parquet` |
| PUF completions with only the shared variables visible, seed 2 | `x_sharedonly.py`, `x_complete_shared.sbatch` | 28923779, 28924159, 28923780/2, 28924160 | `$Q/model/sharedonly/` |
| Completed wealth against the untranslated model and the SCF | `x_compare.py` | 28925441 | `$Q/x_compare.json` |
| Model-free check: SCF reweighted so its shared variables look like the PUF's | `x_reweight.py` | 28925442 | `$Q/x_reweight.json` |
| Tilt (`c_fit.py` unchanged) on the translated core targets, seed 1 | `x_fit.py` | 28924158 (`full_v3x2`), 28922882 (`full_v3x`) | `$Q/c2/P_core/`, `$Q/c/P_core/` |
| Tables in this report | `x_tables.py` | | `$Q/x_tables_v3x2.md` |

The untranslated model is test 3's version 2 (`full_v2`, seeds 1 to 3,
its completions in `scf_joint/model/full_v2/`). I wrote my own translation;
probe Y's general function posted its interface after mine was running, and
the two have not been compared. Mine makes one draw per household, not
several.

## The translation rules

**Population and years.** The reference is the PUF 2015 returns that are
not dependent filers (141.6 million), all parts (training, validation,
test), own weights; the SCF's filer units never include dependent filers.
The SCF 2013 and 2016 waves are translated separately, each onto the one
PUF 2015 distribution: ranks are taken within the SCF wave and mapped onto
PUF 2015 amounts (the plan's "ranks within source and year", with the PUF
as the reference). Weights, wealth and every variable the PUF lacks stay as
they are.

The shared variables are translated one at a time, each within cells of
those already translated:

1. **Filing status.** Cells: age group (under 35, 35 to 64, 65 and over;
   the SCF head's age, the PUF primary filer's age range where known).
   Moves only between joint and separate, and between single and head of
   household, so the SCF's married share is kept (41 against the PUF's 38
   percent; see "What is unfinished"). In each cell enough households move
   that the shares within each pair match the PUF's. Married movers are
   chosen at random. Single to head of household takes households with
   dependents first; head of household to single takes those without
   first. In practice the SCF builder never makes a single filer with
   dependents, so the singles moved to head of household have no
   dependents in the SCF's count (probably households supporting an adult
   relative, whom the builder does not count). A first attempt that also
   used dependents as a cell moved the head-of-household share away from
   the PUF's (15.1 to 13.4 percent in 2013), because the SCF counts
   dependents differently and they are not yet translated at this step. A
   household moving between joint and separate gains or loses the spouse's
   exemption.
2. **Age range.** Within filing status, the SCF head's age at the interview
   is ranked by weight and mapped onto the PUF's age-range shares for that
   filing status. This absorbs both the head-versus-primary-filer
   difference and the interview falling about a year after the tax year.
3. **Dependents** (exemptions less filer and spouse), within filing status
   by age range: weighted rank (ties broken by children under 17, then at
   random) onto the PUF's distribution. **Children under 17**: within
   filing status, age group and dependents, the same way, then capped at
   dependents. **Sex of head**: within filing status and age group, random
   switches toward the PUF's share male.
4. **Wages**, within filing status (single and separate together, joint,
   head of household) by age group. The 2.6 percent of PUF weight with no
   age (high-income records outside the demographic file) is placed in an
   age group drawn from the SCF's age shares for the same filing status and
   income band, only to decide which cell their wages count in.
5. **The other ten shared incomes**, in this order: Schedule C, total
   Schedule E, gains, ordinary dividends, taxable interest, tax-exempt
   interest, taxable pensions, Social Security, unemployment, farm. Cells:
   filing status by age group by wage band (no wages; below the PUF median
   wage of $37,900; to the 90th percentile, $124,200; above). Cells with
   fewer than 90 PUF records fall back to filing status by wage band.

For wages and each income, in each cell:

- **Who has an amount.** The share with a positive amount (and, where the
  PUF has losses, the share with a loss) is set to the PUF's share in the
  cell by switching households. **Switching on** (SCF zeros made holders,
  or made losses) takes the most likely holders given the whole row: a
  logistic regression of holding on filing status, age, dependents, the
  other incomes as the SCF reports them and all 24 wealth components,
  fitted on the wave with its weights. These models separate holders from
  non-holders well (area under the ROC curve 0.84 to 0.99, 0.74 to 0.77 for
  unemployment). **Switching off** (SCF holders made zero) takes households
  at random within the cell. This follows probe Y, which found that
  removing the least likely holders made the remaining holders too rich,
  and the coordinator's instruction. The first version (`full_v3x`)
  removed the least likely holders; its results are reported alongside.
- **Amounts among holders**, by weighted rank within the cell, mapped onto
  the PUF's amounts in the cell. Households switched on are ranked below
  the SCF's own holders (ordered among themselves by likelihood), because
  the SCF's capital-income totals are close to the PUF's with far fewer
  holders, so the holders it misses are small ones.

Which direction applied, by item (the larger switch in each wave; full
table below): switched on for wages, Schedule C (and losses), gains,
dividends, taxable and tax-exempt interest and farm in both waves, and for
unemployment in 2016; switched off for Social Security in both waves and
for unemployment in 2013 (the SCF item adds workers' compensation, and tax
year 2012 had high unemployment); both directions of similar size for total
Schedule E (on to losses, off among gains) and taxable pensions.

**Not done:** money reported under the wrong heading. Probe Y found that
business income reported as wages needs a definitional rule rather than a
rank map; nothing in this translation moves amounts between items.

**Encoding.** The SCF rows' shared amounts are now PUF 2015 amounts, so
they are encoded with the PUF 2015 rank functions: a translated SCF
household and a PUF return with the same wages get the same token. Wealth
tokens and every PUF row are copied unchanged from `full_v2` (checked in
the script), as are the split, the weights and the file layout.

## How far households moved

Main version (`full_v3x2`). The first version's demographics are
identical (the rule change affects only switching off) and its income
moves differ by at most a point or two.

**Demographics: shares before and after (percent of weight)**

| Variable | Value | PUF 2015 (not dependent filers) | SCF 2013 before | SCF 2013 after | SCF 2016 before | SCF 2016 after |
|---|---|---:|---:|---:|---:|---:|
| Filing status | single | 43.8 | 41.9 | 41.9 | 44.9 | 43.0 |
| | joint | 38.4 | 40.8 | 40.7 | 39.1 | 39.7 |
| | separate | 2.1 | 2.2 | 2.2 | 2.7 | 2.1 |
| | head of household | 15.8 | 15.1 | 15.2 | 13.2 | 15.2 |
| Age range (PUF: known ages) | under 26 | 12.1 | 11.7 | 11.5 | 13.9 | 11.7 |
| | 26-34 | 19.3 | 16.8 | 19.0 | 15.9 | 19.1 |
| | 35-44 | 17.9 | 18.3 | 18.0 | 17.1 | 17.9 |
| | 45-54 | 17.5 | 18.9 | 17.7 | 17.7 | 17.6 |
| | 55-64 | 16.1 | 16.8 | 16.4 | 16.5 | 16.3 |
| | 65+ | 17.1 | 17.6 | 17.4 | 19.0 | 17.4 |
| Dependents | 0 | 62.5 | 65.1 | 62.0 | 69.2 | 62.5 |
| | 1 | 17.3 | 15.0 | 17.4 | 12.8 | 17.3 |
| | 2 | 13.1 | 9.6 | 13.3 | 8.8 | 13.1 |
| | 3 or more | 7.0 | 10.3 | 7.2 | 9.3 | 7.1 |
| Sex of head (PUF: primary filer) | male | 62.8 | 67.9 | 65.4 | 68.7 | 65.2 |

**Share of weight whose value changed (percent)**

| Change | SCF 2013 | SCF 2016 |
|---|---:|---:|
| Filing status | 4.7 | 2.9 |
| Age range, by one range | 7.8 | 9.7 |
| Age range, by two or more | 0.0 | 0.0 |
| Number of dependents | 17.5 | 17.6 |
| Children under 17 | 15.3 | 16.1 |
| Sex of head | 7.4 | 6.7 |

Filing-status moves are mostly between single and head of household (in
2013 about 1.9 percent of all weight each way, offsetting across age
groups; in 2016, 2.0 percent single to head of household). By the
seven-income total, 3 to 6 percent of the bottom half changed filing status
and 1 percent of the top 1 percent. Dependents changed for 17 to 18 percent
of weight: the SCF has more units with three or more dependents and fewer
with one or two. The married share is not translated, and the sex of head
moves only partway (65 against 63 percent male), because the PUF's sex is
known only for returns in the demographic file.

**Incomes: who has an amount (percent of weight)**

| Item | PUF positive | SCF 2013 before → after | SCF 2016 before → after | PUF loss | SCF 2013 loss before → after | SCF 2016 loss before → after |
|---|---:|---:|---:|---:|---:|---:|
| Wages | 82.2 | 80.4 → 82.2 | 79.9 → 82.2 | – | – | – |
| Schedule C business | 13.0 | 7.6 → 13.2 | 10.4 → 13.1 | 4.1 | 0.9 → 4.2 | 0.8 → 4.2 |
| Schedule E total | 8.0 | 8.5 → 8.2 | 8.9 → 8.1 | 4.4 | 1.0 → 4.5 | 1.2 → 4.4 |
| Gains (PUF long-term; SCF all) | 9.0 | 5.6 → 9.2 | 6.5 → 9.1 | 2.2 | 2.0 → 2.3 | 1.7 → 2.3 |
| Ordinary dividends | 18.9 | 12.0 → 19.3 | 11.9 → 19.1 | – | – | – |
| Taxable interest | 29.6 | 16.3 → 30.1 | 12.4 → 29.9 | – | – | – |
| Tax-exempt interest | 4.1 | 2.3 → 4.1 | 2.2 → 4.1 | – | – | – |
| Taxable pensions | 20.0 | 19.6 → 20.2 | 20.5 → 20.1 | – | – | – |
| Social Security | 19.7 | 20.6 → 20.0 | 21.6 → 19.9 | – | – | – |
| Unemployment | 4.3 | 6.7 → 4.4 | 3.1 → 4.3 | – | – | – |
| Farm | 0.4 | 0.3 → 0.4 | 0.3 → 0.4 | 0.9 | 0.2 → 0.9 | 0.1 → 0.9 |

The shares after translation are within half a point of the PUF's rather
than equal to it, because the cells are matched one at a time with whole
households and the SCF's total weight differs.

**Incomes: switches (percent of all weight) and totals ($ billions; SCF
own-year dollars before, 2015 PUF dollars after).** The SCF's total weight
is 132 and 140 million against the PUF's 142 million, so translated totals
sit a few percent below the PUF's.

| Item | Wave | Zero to positive | Positive to zero | To a loss | Loss to zero | Amount carried by holders switched off, % of positive total | Total before | Total after | PUF total |
|---|---|---:|---:|---:|---:|---:|---:|---:|---:|
| Wages | 2013 | 1.9 | 0.1 | – | – | 0.3 | 6,730 | 6,759 | 7,086 |
| | 2016 | 2.5 | 0.2 | – | – | 0.2 | 7,941 | 7,086 | 7,086 |
| Schedule C | 2013 | 5.7 | 0.1 | 3.3 | 0.0 | 1.5 | 381 | 319 | 331 |
| | 2016 | 3.2 | 0.5 | 3.3 | 0.0 | 3.8 | 568 | 330 | 331 |
| Schedule E total | 2013 | 1.5 | 1.3 | 3.4 | 0.0 | 21.6 | 949 | 695 | 704 |
| | 2016 | 0.5 | 1.1 | 3.2 | 0.0 | 13.1 | 1,142 | 720 | 704 |
| Gains | 2013 | 3.7 | 0.1 | 0.5 | 0.3 | 1.1 | 478 | 602 | 634 |
| | 2016 | 2.9 | 0.3 | 0.8 | 0.2 | 3.6 | 678 | 626 | 634 |
| Ordinary dividends | 2013 | 7.3 | 0.0 | – | – | 0.3 | 204 | 223 | 238 |
| | 2016 | 7.3 | 0.1 | – | – | 0.2 | 251 | 236 | 238 |
| Taxable interest | 2013 | 13.8 | 0.0 | – | – | 0.0 | 88 | 82 | 88 |
| | 2016 | 17.5 | 0.0 | – | – | 0.0 | 90 | 86 | 88 |
| Tax-exempt interest | 2013 | 1.9 | 0.1 | – | – | 3.1 | 65 | 56 | 60 |
| | 2016 | 1.9 | 0.0 | – | – | 0.3 | 81 | 59 | 60 |
| Taxable pensions | 2013 | 2.1 | 1.5 | – | – | 6.9 | 532 | 659 | 692 |
| | 2016 | 1.6 | 2.0 | – | – | 8.6 | 663 | 692 | 692 |
| Social Security | 2013 | 0.8 | 1.4 | – | – | 6.6 | 535 | 577 | 603 |
| | 2016 | 0.2 | 2.0 | – | – | 8.6 | 654 | 606 | 603 |
| Unemployment | 2013 | 0.1 | 2.5 | – | – | 38.3 | 58 | 25 | 27 |
| | 2016 | 1.4 | 0.2 | – | – | 3.7 | 20 | 26 | 27 |
| Farm | 2013 | 0.2 | 0.1 | 0.7 | 0.0 | 9.3 | 16 | -12 | -15 |
| | 2016 | 0.2 | 0.1 | 0.8 | 0.0 | 31.6 | 8 | -12 | -15 |

Taxable interest needed the most: 14 to 18 percent of SCF weight switched
on, more than doubling the share with interest in 2016. Dividends 7
percent; Schedule C 3 to 6 percent switched on and 3 percent switched to a
loss; total Schedule E 3 percent switched to a loss. Totals after
translation are close to the PUF's: Schedule C and total Schedule E come
down from 1.2 to 1.7 times the PUF's level; 2016 wages from $7.9 to $7.1
trillion. With random removal, the holders switched off carry more of the
amount than under the first version's rule (total Schedule E 2013: 21.6
percent of the positive total against 6.8).

**Incomes: rank moves.** Among households with a positive amount both
before and after: the move in percentile points of its rank among holders
(the SCF wave's own ranking before, the PUF's after), by test 2's
seven-income total. D24's safeguard names a move from the 50th to the
90th percentile (40 points) as large.

| Item | Wave | Mean move, all | Moved over 25 points | Over 40 points | Bottom 50%: mean / over 25 | 50-90%: mean / over 25 | 90-99%: mean / over 25 | Top 1%: mean / over 25 |
|---|---|---:|---:|---:|---:|---:|---:|---:|
| Wages | 2013 | 3.1 | 0.1 | 0.0 | 4.2 / 0.2 | 2.4 / 0.0 | 1.0 / 0.0 | 0.6 / 0.0 |
| | 2016 | 2.8 | 0.0 | 0.0 | 3.7 / 0.1 | 2.3 / 0.0 | 0.8 / 0.0 | 0.5 / 0.0 |
| Schedule C | 2013 | 17.5 | 20.8 | 8.5 | 23.6 / 35.1 | 15.0 / 11.7 | 12.8 / 18.4 | 13.7 / 26.0 |
| | 2016 | 11.2 | 12.1 | 3.4 | 17.8 / 27.1 | 8.5 / 4.9 | 6.5 / 3.5 | 4.0 / 1.1 |
| Schedule E total | 2013 | 8.6 | 6.0 | 1.9 | 16.8 / 21.0 | 9.3 / 3.9 | 4.9 / 2.5 | 2.3 / 0.0 |
| | 2016 | 7.8 | 3.7 | 0.4 | 9.8 / 11.1 | 7.7 / 2.6 | 7.7 / 1.3 | 3.3 / 0.8 |
| Gains | 2013 | 18.1 | 30.3 | 14.8 | 30.5 / 64.8 | 24.0 / 45.9 | 9.2 / 4.3 | 5.8 / 4.1 |
| | 2016 | 15.5 | 22.2 | 6.4 | 28.0 / 45.1 | 19.2 / 31.3 | 9.6 / 8.9 | 5.3 / 3.2 |
| Ordinary dividends | 2013 | 16.6 | 26.2 | 4.9 | 27.4 / 57.5 | 19.3 / 30.5 | 8.2 / 5.3 | 4.2 / 2.0 |
| | 2016 | 17.6 | 31.5 | 9.2 | 31.4 / 64.5 | 21.8 / 41.0 | 7.3 / 6.5 | 3.8 / 2.1 |
| Taxable interest | 2013 | 20.7 | 37.0 | 11.1 | 28.3 / 52.8 | 23.8 / 45.8 | 11.9 / 13.9 | 5.3 / 6.1 |
| | 2016 | 27.7 | 51.2 | 29.1 | 38.4 / 77.2 | 31.5 / 61.5 | 18.6 / 26.0 | 8.1 / 9.3 |
| Tax-exempt interest | 2013 | 19.2 | 28.3 | 6.9 | 30.9 / 54.2 | 23.4 / 37.5 | 14.0 / 14.8 | 9.7 / 13.7 |
| | 2016 | 20.4 | 33.8 | 16.9 | 42.2 / 76.9 | 27.6 / 47.9 | 15.2 / 24.7 | 9.7 / 10.6 |
| Taxable pensions | 2013 | 6.5 | 1.9 | 0.5 | 7.7 / 0.9 | 5.0 / 0.8 | 9.3 / 13.5 | 6.9 / 2.2 |
| | 2016 | 5.5 | 1.4 | 0.5 | 6.3 / 2.2 | 5.0 / 1.0 | 5.4 / 1.0 | 9.3 / 0.3 |
| Social Security | 2013 | 5.1 | 1.1 | 0.4 | 4.4 / 0.5 | 5.3 / 0.2 | 8.2 / 11.4 | 6.0 / 5.3 |
| | 2016 | 3.7 | 0.8 | 0.4 | 3.5 / 0.4 | 3.5 / 0.2 | 5.9 / 6.1 | 8.6 / 18.3 |
| Unemployment | 2013 | 7.4 | 4.3 | 1.4 | 7.3 / 2.5 | 6.8 / 5.9 | 12.9 / 7.7 | 12.4 / 0.0 |
| | 2016 | 15.5 | 20.3 | 6.9 | 14.2 / 16.7 | 17.3 / 25.9 | 11.7 / 0.0 | – |
| Farm | 2013 | 14.8 | 19.4 | 6.2 | 18.2 / 26.2 | 15.9 / 23.2 | 10.1 / 6.2 | 12.1 / 10.7 |
| | 2016 | 19.2 | 27.6 | 9.1 | 8.7 / 9.1 | 23.5 / 31.1 | 15.8 / 33.3 | 10.6 / 0.0 |

**Flagged under D24's safeguard: every capital income.** For taxable
interest in 2016 the average holder moved 28 points, half moved more than
25 and 29 percent more than 40; in the bottom half of income, 77 percent
moved more than 25 points. The moves are nearly all upward (mean signed
move 27.5 points). They follow from placing the new holders below the
SCF's own: if the SCF misses more than half of interest recipients and
they are the small ones, every SCF recipient sits much higher in the PUF's
distribution than in the SCF's own ranking. Whether that is right is the
assumption to check: these data cannot say how many of the PUF's small
recipients are SCF zeros rather than SCF small holders. Wages, pensions and
Social Security move little (3 to 7 points on average).

## Target disagreement before and after

The arm P targets (test 3's `a_full.py`, run unchanged on the translated
data): each wealth variable's coarse bins, the SCF's share within each
group (income group, age, married, dependents, and in the full set the
band of each shared income item), 2013 and 2016 interpolated to 2015,
times the PUF's group weight. C's measure of disagreement: for each wealth
variable and bin, the spread between the largest and smallest total that
the families covering every PUF record imply (income group, married,
dependents, and in the full set the income-item families; the age family
does not cover records without an age), as a share of all PUF weight.
"Before" is test 3's own target files; the figures reproduce C's 3.5 and
6.9 percent.

| Targets | Set | Largest spread, % of PUF weight | Mean spread, % | Largest three (variable, bin, %) |
|---|---|---:|---:|---|
| Before (`full_v2`) | core | 3.52 | 0.255 | home zero 3.52; gain on home zero 3.39; defined-contribution zero 3.33 |
| | full | 6.85 | 0.628 | defined-contribution zero 6.85; home zero 6.43; stocks zero 6.36 |
| After (`full_v3x2`) | core | 3.52 | 0.223 | home zero 3.52; gain on home zero 3.44; defined-contribution zero 2.51 |
| | full | 3.72 | 0.321 | home zero 3.72; gain on home zero 3.63; defined-contribution zero 3.34 |
| After, first version (`full_v3x`) | core | 3.52 | 0.224 | (same) |
| | full | 3.70 | 0.324 | (same) |
| Before, PUF group weights without dependent filers | core | 2.59 | 0.193 | home zero 2.59; gain on home zero 2.48; mortgage zero 2.47 |
| | full | 6.30 | 0.610 | defined-contribution zero 6.30; stocks zero 6.27; home zero 6.04 |
| After (`full_v3x2`), without dependent filers | core | 2.65 | 0.166 | home zero 2.65; gain on home zero 2.59; defined-contribution zero 1.79 |
| | full | 2.65 | 0.236 | home zero 2.65; gain on home zero 2.59; mortgage zero 2.33 |

- **Full set: the disagreement halves**, and the income-item families stop
  contributing. Without dependent filers the full set's largest spread
  equals the core set's (2.65 percent). Before, taxable interest was the
  family furthest from the families' average (4.4 percent of weight);
  after, 1.4 percent.
- **Core set: no change at the largest** (3.52 percent; the mean falls from
  0.26 to 0.22 percent). What remains is home ownership, the gain on the
  home, defined-contribution accounts and mortgages, at the zero bin, and
  the families furthest apart are married and dependents.
- **What remains is population, not meaning.** The SCF's shares are taken
  over units with wealth observed, which leaves out split-off units
  (financially independent adults in the household, whose wealth the SCF
  does not ask: 12 percent of SCF filer weight in 2013, 14 percent in 2016,
  mostly young unmarried renters). The PUF's group weights include the
  returns that correspond to them, and dependent filers (6 percent of PUF
  weight). So the married family's groups differ between the two sides by
  10 points before and after translation: married filing jointly is 46
  percent of SCF units with wealth observed, 40 to 41 percent of all SCF
  filer units, 38 percent of PUF returns that are not dependent filers and
  36 percent of all PUF returns (training parts). Translation cannot fix
  this; it is the coverage step of D23 and D24. Removing dependent filers
  from the PUF side alone takes the core set from 3.5 to 2.6 or 2.7
  percent.

## Completed PUF wealth before and after, against the SCF

PUF training records completed with wealth by each model (five draws, SCF
tag, 2015). The SCF columns are test 3's comparison (training units with
wealth observed, interpolated one third 2013, two thirds 2016, in logs) and
a second population: **all SCF filer units with split-off units counted at
zero net worth** (a lower bound for the population that matches all filers;
2013 $56,100 median and $468,000 mean, 2016 $57,100 and $588,000). Income
groups: PUF records by AGI (`E00100`), SCF units by test 2's seven-income
total (the SCF has no AGI), each ranked within its own file.

**Overall**

| Statistic | SCF interpolated | SCF with split-offs at zero, interpolated | Untranslated, seeds 1 / 2 / 3 | Translated, seeds 1 / 2 | Translated, first version, seeds 1 / 2 |
|---|---:|---:|---:|---:|---:|
| Mean net worth | $628k | $545k | $807k / $713k / $904k | $569k / $561k | $554k / $537k |
| Median net worth | $91k | $57k | $55k / $57k / $75k | $50k / $32k | $47k / $26k |
| 90th percentile | $1.09m | | $1.30m / $1.18m / $1.51m | $874k / $873k | $929k / $862k |
| Top 1% share of net worth (%) | 37.8 | | 44.1 / 40.5 / 40.5 | 43.9 / 44.4 | 41.1 / 43.4 |
| Share with negative net worth (%) | 11.2 | | 15.4 / 14.7 / 13.5 | 14.7 / 16.6 | 15.1 / 16.9 |
| Share owning a home (%) | 66.5 | | 53.9 / 52.4 / 55.8 | 55.3 / 44.9 | 55.4 / 40.3 |
| Share holding stocks (%) | 20.0 | | 23.8 / 23.8 / 24.1 | 18.4 / 18.2 | 19.2 / 17.7 |
| Share holding bonds (%) | 12.9 | | 15.0 / 13.8 / 14.1 | 11.6 / 10.8 | 10.6 / 11.0 |
| Share holding a business (%) | 12.0 | | 14.5 / 15.1 / 15.7 | 9.6 / 9.3 | 8.8 / 9.5 |
| Share with a defined-contribution account (%) | 53.5 | | 51.6 / 51.7 / 51.1 | 44.7 / 47.1 | 45.8 / 46.0 |

**Mean net worth by income group**

| Income group | SCF interpolated | Untranslated, seeds 1 / 2 / 3 | Translated, seeds 1 / 2 |
|---|---:|---:|---:|
| Bottom 50% | $113k | $206k / $188k / $228k | $116k / $110k |
| 50-90% | $370k | $547k / $497k / $637k | $339k / $348k |
| 90-99% | $2.13m | $2.86m / $2.66m / $3.38m | $2.18m / $2.17m |
| 99-99.9% | $12.32m | $17.18m / $14.60m / $18.25m | $13.66m / $13.53m |
| Top 0.1% | $54.69m | $72.44m / $49.52m / $67.13m | $56.10m / $49.86m |

**Median net worth by income group**

| Income group | SCF interpolated | Untranslated, seeds 1 / 2 / 3 | Translated, seeds 1 / 2 |
|---|---:|---:|---:|
| Bottom 50% | $17k | $7k / $10k / $11k | $8k / $5k |
| 50-90% | $146k | $153k / $145k / $194k | $124k / $103k |
| 90-99% | $1.10m | $1.24m / $1.14m / $1.50m | $1.02m / $993k |
| 99-99.9% | $7.96m | $8.98m / $8.10m / $10.38m | $8.06m / $7.83m |
| Top 0.1% | $32.73m | $48.05m / $29.18m / $37.51m | $34.31m / $27.22m |

**Share owning a home by income group (%)**

| Income group | SCF interpolated | Untranslated, seeds 1 / 2 / 3 | Translated, seeds 1 / 2 |
|---|---:|---:|---:|
| Bottom 50% | 48.8 | 31.7 / 29.7 / 33.9 | 34.1 / 22.0 |
| 50-90% | 77.2 | 71.8 / 70.0 / 73.3 | 72.3 / 61.4 |
| 90-99% | 92.0 | 93.5 / 95.2 / 94.8 | 92.8 / 93.3 |
| 99-99.9% | 95.3 | 96.1 / 98.4 / 97.9 | 96.4 / 97.9 |
| Top 0.1% | 98.6 | 98.1 / 99.4 / 98.1 | 98.6 / 98.5 |

**Share holding stocks by income group (%)**

| Income group | SCF interpolated | Untranslated, seeds 1 / 2 / 3 | Translated, seeds 1 / 2 |
|---|---:|---:|---:|
| Bottom 50% | 9.1 | 10.1 / 10.0 / 10.3 | 7.3 / 7.7 |
| 50-90% | 21.3 | 29.5 / 29.6 / 30.4 | 22.3 / 21.8 |
| 90-99% | 53.1 | 67.2 / 67.2 / 66.0 | 55.5 / 53.2 |
| 99-99.9% | 83.1 | 89.4 / 86.5 / 88.1 | 82.7 / 82.2 |
| Top 0.1% | 93.5 | 95.4 / 89.9 / 93.6 | 92.2 / 89.6 |

**Share holding a business by income group (%)**

| Income group | SCF interpolated | Untranslated, seeds 1 / 2 / 3 | Translated, seeds 1 / 2 |
|---|---:|---:|---:|
| Bottom 50% | 6.1 | 9.6 / 10.9 / 10.6 | 4.9 / 5.2 |
| 50-90% | 12.7 | 15.4 / 15.7 / 16.6 | 11.1 / 10.3 |
| 90-99% | 27.5 | 31.5 / 30.7 / 34.1 | 24.5 / 22.4 |
| 99-99.9% | 63.5 | 67.7 / 66.2 / 69.5 | 53.4 / 51.6 |
| Top 0.1% | 85.2 | 90.3 / 84.7 / 86.3 | 72.7 / 72.9 |

What these show:

- **Means by income group and stock holding move to the SCF.** The
  untranslated model put 35 to 100 percent too much wealth in the bottom
  half and the 50th to 90th percentiles and too many stockholders above the
  median; translated, every group's mean is within 9 percent of the SCF's
  except the 99th to 99.9th percentiles (11 percent high), stock holding is
  within 4 points in every group, and the two seeds agree much more closely
  (overall mean $561,000 and $569,000, against $713,000 to $904,000). The
  overall mean ($565,000) sits between the SCF's $628,000 and the
  split-offs-at-zero bound of $545,000.
- **The median and home ownership at the bottom do not.** The median falls
  from $55,000 to $75,000 to $32,000 to $50,000, and home ownership in the
  bottom half stays at 22 to 34 percent against the SCF's 49. Business
  holding moves from too high to too low (9 to 10 percent against 12).
- **Seed 2 is an outlier in both versions** (median $26,000 and $32,000,
  home ownership 40 and 45 percent); seed 1 gives $47,000 and $50,000 and
  55 percent.
- **The first version (least likely switched off) and the main version
  differ by less than the two seeds do.**

**Checks on where the remaining gap comes from**

1. *The SCF benchmark.* The SCF figures above exclude split-off units,
   which are 12 to 14 percent of SCF filer weight and are concentrated in
   the bottom half of income (roughly a quarter of it), while the PUF's
   bottom half includes their returns and dependent filers. Counting
   split-offs at zero moves the SCF median from $91,000 to $57,000. So
   part of the median and bottom-half home-ownership gap is a population
   difference in the comparison, before and after translation.
2. *Model-free version.* Reweighting the SCF so its shared variables (the
   tokens the model sees) are distributed like the PUF's (gradient-boosted
   classifier, PUF returns that are not dependent filers and have a known
   age, against SCF units with wealth observed) gives what the PUF's
   wealth would be if wealth given the shared variables were exactly the
   SCF's. How well the classifier tells the two files apart (area under
   the ROC curve, 0.5 = not at all) falls from 0.80 untranslated to 0.71
   translated: translation aligns each variable but not the combinations.
   The reweighted SCF's median is $85,000 untranslated and $65,000
   translated, home ownership 62 and 61 percent, mean $544,000 and
   $368,000 (this population leaves out the high-income records without an
   age). So translation itself implies a lower median for PUF returns, by
   about $20,000.
3. *What the model adds.* On the same PUF records the completions give
   median $63,000 to $85,000 untranslated and $56,000 (seed 1) or $36,000
   (seed 2) translated; home ownership 54 to 58 percent and 57 or 46
   percent. With the PUF-only variables (AGI, itemized deductions, the
   Schedule E pieces and the rest, which the model never sees together
   with wealth) hidden, translated seed 2 gives median $55,000 and home
   ownership 57 percent, close to seed 1 and to the reweighting. So the
   seed-2 shortfall comes from how that model reads the PUF-only
   variables, not from the translation.
4. *SCF records redrawn.* On the SCF's own training records, both models
   reproduce the SCF's wealth about equally well (seed 2; medians $83,000
   and $90,000 translated against $85,000 and $95,000 true; untranslated
   $78,000 and $88,000; home ownership 63 to 64 percent against 66 to 68
   in both).

## The tilt on the translated targets

Test 3's tilt (method 2, `c_fit.py` unchanged) fitted to the translated
core targets on the translated model (seed 1), against C's run on the
untranslated model and targets (`$P/full_v2/c/P_core/report.json`, seed 1).
The tilt changes the model's probabilities as little as possible (measured
by KL divergence from the model, in nats per unit of weight) so that the
completed PUF wealth hits the targets. A smaller KL means the model's own
completions were already closer to the targets. Fresh draws after fitting;
16 draws per record here, 32 in C's run (the KL is an average and does not
depend on the number of draws beyond noise; the standard error is 0.0001).

| | Untranslated (C) | Translated, first version | Translated (main) |
|---|---:|---:|---:|
| Largest target disagreement, % of weight | 3.52 | 3.52 | 3.52 |
| KL from the model, nats per unit of weight | 0.689 | 0.554 | 0.565 |
| KL per record: median / 90th / 99th percentile | 0.97 / 3.80 / 6.57 | 0.71 / 2.77 / 4.93 | 0.80 / 3.23 / 4.85 |
| Expected miss after fitting: mean / largest, % of group weight | 0.091 / 2.15 | 0.079 / 2.06 | 0.079 / 2.03 |
| Cells missed by more than 1% | 30 | 22 | 24 |
| Variable needing the largest move | home | home | home |

Translation lowers the move the tilt needs by about a fifth (0.69 to
0.55 or 0.57 nats). The fit is limited by the same core disagreement as before
(the largest misses are in the dependents and married families, 2.1 and 1.6
percent of a group's weight), which no method can remove, and home
ownership is still the variable the tilt has to move most.

## What it means for the plan

- **D24's claim holds for the income items and fails for the rest.** The
  income-item families' contribution to the target disagreement goes away
  once the SCF's incomes are translated, which is what D24 predicts. The
  core disagreement (3.5 percent of weight, home ownership and retirement
  accounts by marital status and dependents) does not change: it is a
  coverage difference, and needs the plan's coverage step: either give
  the SCF's split-off units a wealth concept (for instance a share of the
  household's, or zero with a stated rule) so that SCF shares describe all
  filer units, or define the PUF group weights without the returns that
  correspond to split-offs and dependent filers. The same applies to the
  wealth benchmarks in the evaluation: the SCF's "all filers" median is
  closer to $57,000 than $91,000.
- **Translation removes most of the "too rich at every income" error**
  that test 3's completed PUF wealth showed, and makes the seeds agree. The
  plan's expectation that translation moves the median toward the SCF's
  $91,000 is not borne out; against the population-matched bound of
  $57,000 the translated median ($32,000 to $50,000) is low and the
  untranslated one ($55,000 to $75,000) is about right, for the wrong
  reason (too much wealth everywhere).
- **The translation leaves a joint mismatch**: the PUF and the translated
  SCF are still told apart with an area under the ROC curve of 0.71. Each
  item matches within coarse cells, one at a time, but combinations
  (interest without dividends, for instance) do not. Matching the joint
  pattern of who holds which income, not only each item's share, is the
  next refinement.
- **The rank moves are large** for every capital income, as D24's safeguard
  anticipates. They come from the rule that the SCF's missing holders are
  small ones; a linked survey-tax study of who the SCF's non-reporters are
  would test it directly.
- **The PUF-only variables matter.** One of two retrained models reads them
  in a way that pushes bottom-half wealth down sharply; hiding them removes
  the effect. The plan should evaluate completions both ways.

## What is unfinished

- **One draw per household**, not several; probe Y's function makes five.
- **Married share not translated** (SCF 41 percent joint against the PUF's
  38). A joint SCF couple cannot become two returns without changing the
  unit count, so this belongs with the coverage step.
- **No definitional rule for money under the wrong heading** (business
  income reported as wages; probe Y).
- **Cells are coarse** and later incomes are not conditioned on earlier
  capital incomes, so the joint pattern of holdings is not matched (above).
- **Sex of head** is matched only within the PUF records that have a sex.
- **Two seeds per version**; seeds differ by more than the translation
  rule does.
- **Residual calibration and C's post-processing** (`c_post.py`) were not
  run on the tilt; the tilt used 16 draws per record (C used 32).
- Nothing is still running; every job listed above finished.
