---
title: "The top tail of Tax-Data: the issue, the short-term position, and the revamp scope"
role: notes
workstream: state_weights
status: open
updated: 2026-09-29
sot: research/state_weights/plan.md
supersedes: []
superseded_by: null
---

# The top tail of Tax-Data: the issue, the short-term position, and the revamp scope

For John Ricco, from JI's 2026-09-28 session. Three parts, one per goal:

1. **The issue** (§1): what is wrong at the very top of the file, and how it
   sits inside the wider question of how PUF records connect to the IRS
   aggregates the pipeline calibrates to.
2. **The short-term position** (§2): what we do now so work continues on a
   file whose limits are written down (S41).
3. **The revamp scope** (§3): the dimensions a larger push would cover, with
   the question each needs answered; and, as §4, the Forbes and wealth
   extension whose inputs are already gathered, deferred to that push.

Nothing here is in the pipeline. The two diagnostic scripts and their results
are committed alongside this note.

## 1. The issue

### 1.1 The narrow problem: the four aggregate records

The TY2015 PUF (the IRS public-use microdata sample this repo is built from)
takes about 1,100 returns with extreme values on some line, roughly the top or
bottom 30 amounts of any income field, out of the microdata and replaces them
with **four averaged records**, one per AGI band (`RECID` 999996–999999:
negative AGI; $0–10M; $10–100M; $100M+). Each carries the per-return average
of every dollar field and a weight of 140–465 returns. They have no
demographics: filing status is coded 0, exemptions 1, no age, no dependents,
and no row in the demographics file. `process_puf.R` drops them
("Remove aggregate returns (for now)").

**What the records hold.** On its own weights (`S006 / 100`) the raw PUF
against TY2015 Table 1.4 (Pub 1304's income by AGI class, from the IRS-Ind
aligned panel), by line
(`research/state_weights/filer_refit/06_puf_soi_gaps.R`, Slurm 27742729):

| Line | PUF ÷ Table 1.4, as the pipeline reads it | With the 4 aggregate records |
|---|---|---|
| Wages | 1.005 | 1.006 |
| Taxable interest | 0.928 | 1.021 |
| Ordinary dividends | 0.924 | 1.003 |
| Capital gains (Schedule D net gain) | 0.888 | 1.009 |
| Other gains (Form 4797) | 0.691 | 1.012 |
| Business net income | 0.994 | 0.997 |
| Taxable pensions | 1.004 | 1.004 |
| Social Security, total | 0.999 | 0.999 |

Below $10M of AGI the PUF matches Table 1.4 within 1–2% in every class. The
whole shortfall sits in the $10M+ class, and the aggregate records are it:

| Line | PUF ÷ Table 1.4, $10M+ class | Share of the national shortfall in that class |
|---|---|---|
| Taxable interest | 0.57 | 113% |
| Dividends | 0.59 | 96% |
| Capital gains | 0.67 | 105% |
| Other gains | 0.25 | 103% |
| Partnership and S-corporation income | 0.78 | 127% |

**What dropping them does.** Nothing to totals: `create_2017_puf.R` rescales
every line to Table 1.4 by AGI group, and $10M+ is its own group, so the
dollars come back to the right class. Everything to shape: the rescaling
multiplies every $10M+ record's interest by about 1.75 and gains by about 1.5,
handing the extremes' money to returns with a median AGI of $21M. The PUF's
$10M+ records (10,693 records, 17,503 returns) top out at $136M of AGI; the
dropped $100M+ aggregate alone stands for 465 returns averaging $292M. In 2015
the drop cuts the share of $10M+ AGI above $100M from **29% to 6%**. Table 1.4
cannot see this, because its top class is open-ended.

**Who is affected.** Anything nonlinear at the very top: the net investment
income tax, the rate schedule on gains, top-0.01% and top-400 distribution
tables, deemed-realization and wealth-tax bases. Class-level revenue and the
state work are not.

### 1.2 The broad problem: how the file connects to the IRS aggregates

The PUF is a subsample of the same Form 1040 sample Pub 1304 is estimated from,
so **definitions are not the issue**: same year, same lines, the file
reproduces the published totals once its extremes are counted. The 2017
Table 1.4 the pipeline targets also equals IRS-Ind's harmonized 2017 figures
exactly. The distance between file and aggregates opens in three places as
the pipeline moves the file to 2017 and beyond:

1. **Construction.** The aggregate drop, plus disclosure protection: alimony
   received is −14% even with the aggregates (removed or blurred for
   high-rate strata); partnership and S-corporation losses (−6%) and farm
   losses (+8%) are small lines hit by blurring; non-itemizers' wages carry up
   to 10% uniform noise.
2. **Aging without a stock–flow discipline.** Stages 0 and 1 move weights to
   2017 return counts but keep 2015 dollars per return; then each line is
   rescaled to its 2017 total by AGI group. Where the true total barely moved
   (pensions $1,169B → $1,172B) or fell (unemployment compensation $27.2B →
   $23.9B) the file overshoots first (+8.8%, +11%) and is scaled back second.
   Some 2017 gaps are inherited outright: taxable interest at 0.930 in 2017 is
   2015's 0.928. A one-dimensional correction to a two-dimensional drift.
3. **Class-level targets with nothing inside the class.** Every calibration
   the pipeline runs — the 2017 rescaling, S40's refit of weights to Pub 1304
   counts, the state weights — hits totals by AGI class. Nothing constrains
   the distribution inside the top class, which is where the file is weakest.
   Side finding: `div_ord` (nonqualified dividends) is never rescaled, because
   Table 1.4 carries ordinary and qualified dividends but not their
   difference; minor today (ordinary dividends −0.6% in v2).

The **Forbes billionaire splice** (`src/forbes_splice.R`, TY2022 onward) is
the one piece of code that acts inside the top class, and it acts from the
wealth side: each US billionaire gets fiscal income = Forbes net worth × a
BSYZ ratio (Balkir, Saez, Yagan and Zucman 2025; 1.75% for the top 100, 3.46%
below), and existing top records give up weight so national totals do not
move. It recovers part of the shape and none of it before 2022
(`research/state_weights/filer_refit/07_top_tail_2022.R`, Slurm 27745711, on
vintage `2026092720_filer_refit_v2`; "without" drops the Forbes rows and
divides the splice's weight factors back out):

| File | Returns above $100M | Share of $10M+ AGI above $100M | Above $500M | Top 400: share of all AGI |
|---|---|---|---|---|
| 2022 without splice | 1,279 | 18.9% | 0 | 0.57% |
| 2022 with splice | 1,412 | 23.8% | 23 returns, $26B | 0.75% (156 of the 400 are Forbes rows) |
| 2015 PUF without aggregates | 210 | 6.2% | — | 0.41% |
| 2015 PUF with aggregates | 675 | 29.4% | — | ≥ 1.14% |

Class totals match Table 1.4 either way ($10M+: AGI 0.96 with, 0.97 without;
gains 0.98 / 0.99; wages 1.16 both, a separate issue). The 2015 rows are a
shape benchmark only, but nominal growth 2015 → 2022 should push more returns
above a fixed $100M, not fewer, so 2022 falling short on both measures points
one way. The splice cannot close it: every billionaire in a rank group
realizes the same share of wealth (median fiscal income about $105M), so
nobody has the lumpy realization year that makes a $500M return; and the
aggregate records cover more than billionaires (140 returns averaging −$96M
of AGI; extremes with AGI under $10M).

**The direct benchmark: IRS SOI Table 4.3**, the top 0.001% / 0.01% / 0.1% /
1% of returns excluding dependents with income items, published 2014–2023
(TY2017–2018 under its earlier name, Table 3 of the "tax rates and tax
shares" release). Run for TY2017–2023 on the v2 vintage
(`research/state_weights/filer_refit/08_table43.R`, Slurm 27755431; our
universe of non-dependent filers matches the IRS count to 0.1% in every year;
"without" divides the Forbes splice back out, which exists only from TY2022):

| Top 0.001% | 2017 | 2018 | 2019 | 2020 | 2021 | 2022 | 2023 |
|---|---|---|---|---|---|---|---|
| IRS: returns | 1,433 | 1,443 | 1,482 | 1,575 | 1,536 | 1,538 | 1,531 |
| IRS: AGI floor | $63M | $69M | $61M | $77M | $118M | $85M | $79M |
| IRS: share of all AGI | 2.34% | 2.09% | 1.81% | 2.38% | 3.01% | 2.36% | 2.09% |
| File: share of all AGI | 1.35% | 1.41% | 1.31% | 1.57% | 2.19% | 1.74% (1.50% without) | 1.60% (1.26% without) |
| File ÷ IRS AGI | 0.57 | 0.67 | 0.72 | 0.65 | 0.72 | 0.72 (0.62) | 0.76 (0.59) |
| File ÷ IRS floor | 1.04 | 1.05 | 1.13 | 1.02 | 1.08 | 1.12 (1.07) | 1.04 (1.03) |
| AGI gap, $B | 110 | 80 | 59 | 103 | 124 | 96 (131) | 78 (130) |
| of which partnership + S-corp net income, $B | 44 | 25 | 17 | 29 | 46 | 62 | 35 |

Income items in the top 0.001%, file ÷ IRS: before the splice (2017–2021)
dividends 0.32–0.55, taxable interest 0.52–0.76, net capital gain 0.73–0.93,
partnership and S-corporation net income 0.21–0.51; with the splice
(2022–2023) dividends 0.80–0.84, interest 0.53–0.93, gains 0.82–0.86,
passthrough income 0.28–0.49. Below the top slice the file is close in every
year: the top 0.01% at 0.90–1.03 of IRS AGI, the top 0.1% and 1% within 2–5%,
though passthrough income is 0.54–0.86 at the 0.01% throughout.

Four things follow. **The compression is permanent and sits at the very top,
not at the entry:** in all seven years our floor is *above* the IRS's (by
2–13%) while our total is 24–43% short, so the top ~1,500 returns are too
uniform — more mass near the floor, too little at the extreme. **It is not a
2022 realization spike:** the gap is $59–124B every year and largest in 2017
and 2021, the two years without the splice's help at the top of a strong
market. **The splice closes 27–40% of the gap where it exists** ($35B of
$131B in 2022, $52B of $130B in 2023) and fixes the lines it targets —
dividends from about half to 0.80–0.84 of IRS, gains from 0.63–0.76 to
0.82–0.86 — so extending it back to 2017 would do the same there. **Passthrough
income is a third to two-thirds of the gap in every year** ($17–62B), and the
splice cannot supply it: BSYZ assigns billionaires *negative* business income
(−11% of fiscal income), so adding billionaires adds losses on that line. The
extreme returns the aggregate records held are, on this evidence, largely
passthrough-income and gains returns — business sales and large distributions
— not billionaires. Two observations for the revamp: top-0.001% wages are
0.33 of IRS in TY2021 (IRS $34.5B), and 0.49 in 2020, against 0.65–0.86 in
other years; and TY2021's 0.001% floor jumps to $118M in the IRS data while
ours follows only partly.

Side findings, file-wide rather than top-tail, from the same runs: Schedule C
net losses are 0.56–0.72 of the IRS total nationally in every year ($106B vs
$171B in 2022); partnership and S-corporation losses 0.78–1.00 nationally but
0.42–0.68 in every top slice; taxable interest nationally is 1.48 of IRS in
TY2021 and 0.55 in TY2023 (0.93–1.14 in other years), so its aging path needs
a look. Wages, AGI and return counts are within 2% nationally in every year.
The loss lines and interest belong to the revamp's dimension 2.

## 2. The short-term position (S41)

**The aggregate records stay dropped, and the compressed tail is a documented
caveat, not a patched one.** `CLAUDE.md` ("Top tail") now carries the numbers
above and a do-not-report rule for top-0.01% and top-400 statistics and
anything nonlinear at the very top, in the same form as the consumption
`C / Y` caveat. The file is usable as it stands for everything else in front
of us: S40, the Phase 3 re-run, Phase 4, Tax-Simulator's runs.

Two patches were considered and set aside:

- **Keep the four records as records** (assign joint status, an imputed age
  group, ids from their RECIDs; let the 2017 rescaling carry them). One day of
  work, and it would put the $10M+ rescaling factors near 1 and the
  above-$100M share near the 2015 benchmark in every year. **Rejected (JI):**
  each aggregate splices hundreds of different returns into one average — a
  465-return composite with $166M of gains and $39M of dividends is not a
  representation of any tax return — and every income imputation would train
  on it.
- **Split each aggregate into synthetic returns** (clone the PUF's own top
  records as donors, scale to each aggregate's count and line totals; the
  Forbes machinery has the id block and the income-neutral weight solver).
  Needs modelling choices about the spread inside each band that nobody can
  verify. Belongs in the revamp, where the top tail is built from evidence
  rather than by rescaling.

The Forbes backcast (§4) is deferred with them. Its inputs are gathered and
cached so the revamp starts from data, not from a search.

## 3. The revamp scope

The diagnosis in §1.2 gives the shape: the file needs a way to age
*distributions*, not only totals, and a top tail built from evidence rather
than rescaled. Candidate dimensions, in dependency order, each with the one
question it needs from John:

| # | Dimension | What it would do | Question for John |
|---|---|---|---|
| 1 | **Base construction** | Retire the aggregate drop for good, by splitting the four records into synthetic returns (§2; proposal: `notes/top_tail_base_proposal.md`) or by moving to a later PUF base if one becomes available, which would also remove two years of aging | Is a post-2015 PUF in reach, or do we design for 2015 as the permanent base? |
| 2 | **Aging discipline** | Replace "grow weights, keep 2015 dollars, rescale by class" with per-line intensive growth factors targeted to Table 1.4 by class *and* Table 4.3's top percentiles, so the inside of the top class is constrained. Prototyped off-pipeline (`filer_refit/09_bucket_aging_prototype.R`, `top_tail_base_proposal.md` §7): it carries real shape drift (2021 top-band wages ×2.1) but cannot fix the level — that is dimension 1 | Do the projection-ledger factors (`project_puf.R`, `build_factor_rows_2020plus()`) stay the vehicle, or does the 2017 base get rebuilt on this discipline too? |
| 3 | **The top tail** | The Forbes extension to TY2017 (Stage A, income-only) and TY2015 as a test (§4) | Income-only splice before wealth exists, or wait for dimension 4? |
| 4 | **Wealth** | A second wealth module at a 2019 SCF base so `value.*` exists in 2017–2021, which the Forbes design assumes (§4.5) | Backcast 2022 (cheap, frozen cross-section), a 2019-base module (recommended), or native bases per SCF wave? |
| 5 | **Imputation hygiene** | The unweighted-cache and cache-by-name problems (`CLAUDE.md`), resolved in the same push rather than confounded with it | One from-scratch rebuild under the fixed RNG as the revamp's baseline, before any of 1–4 lands? |
| 6 | **Validation** | A standing top-tail battery — Table 4.3 per year, the $10M+ bins, top-400 share, S21 partition, E2 tripwire — run on every vintage, so the revamp has a fixed scoreboard | Which of these are gates (fail the vintage) and which are reports? |

## 4. The Forbes and wealth extension: inputs gathered, work deferred

### 4.1 How the splice works today, and why it stops at 2022

`src/forbes_splice.R`, run from `src/main.R` for list years 2022–2025; 2026
onward reuses the 2025 splice, aged.

- **Inputs.** `resources/forbes/forbes_billionaires_2022_2025.csv`: one row per
  US billionaire per year, the Dec 31 snapshot of Forbes' real-time list via
  the komed3/rtb-api mirror, US citizens only, re-ranked within the US
  (`build_forbes_input.sh`). `resources/forbes/forbes_profiles.csv`: one row
  per person from the mirror's profile pages, pulled once at the latest
  snapshot (`build_forbes_profiles.sh`).
- **Fields that matter.** `net_worth` drives everything; the US rank sets the
  BSYZ group. The profile join (`forbes_uri`) supplies age, sex, marital
  status (→ filing status), children (→ dependents, only under 45), self-made
  vs inherited (→ capital-gains basis), and the public/private split of net
  worth. Everything has a documented default.
- **Income.** `resources/forbes/bsyz_fiscal_income_params.csv`: fiscal income
  = net worth × a ratio by rank group, split into six categories. Source:
  BSYZ NBER Table 1, a 2018–20 average.
- **Calibration.** Income-neutral: a ridge QP (quadratic program; osqp, L1
  fallback) cuts the weights of about 8,000 "source" records (top existing
  records by income or wealth under $1B) and raises about 500 wealth-dense
  "receivers", so the return count and six income-category totals are
  unchanged; only net worth rises. TY2022: 717 rows, $4.36T of wealth, $109B
  of fiscal income.

**What blocks earlier years:** (i) no wealth before 2022 — the module's
`base_year` is 2022 and `materialize()` skips it in earlier years, so every
`value.*` is NA in 2017–2021 (checked in v2); (ii) the mirror starts in
November 2019; (iii) BSYZ parameters are 2018–20 only; (iv) `make_forbes_id()`
places list year Y at `9e6 + (Y − 2020) × 1e4 + rank` and stops on Y < 2020.

### 4.2 Forbes year-end lists, 2015–2020, from the Wayback Machine

The Wayback Machine archived **Forbes' own JSON feed**. As a check, the
Dec 31, 2021 copy's US subset matches the mirror's 2021-12-31 list exactly
(738 people, identical net worth), so provenance is the same as today's input.

| TY | Snapshot | Source | US billionaires | Total worth | Top 100 | Quality |
|---|---|---|---|---|---|---|
| 2015 | Dec 15, 2015 | real-time list, older feed (`ajax/list/data?year=0&uri=rtb`) | 511 | $2.50T | $1.50T | clean |
| 2016 | Jan 31, 2017 | Forbes 400, real-time values | **400** | $2.47T | $1.56T | **top 400 only** |
| 2017 | Mar 5–15, 2018 | 2018 annual list, real-time values | 585 | $3.19T | $1.87T | about 10 weeks late |
| 2018 | Aug 5, 2019 | 2019 annual list, real-time values | 603 | $3.26T | $1.88T | **7 months late** |
| 2019 | Jan 19, 2020 | real-time list | 638 | $3.53T | $2.04T | clean (the mirror's first snapshot) |
| 2020 | Dec 31, 2020 | real-time list | 658 | $4.09T | $2.51T | clean |

Annual-list copies carry each person's real-time worth on the capture day
with membership fixed at publication; people with no real-time worth had died
or fallen below $1B and are dropped (TY2018 loses 9 year-end billionaires,
including Ross Perot and Christopher Cline). No real-time copy exists near
year-end 2016–2018 (`cdx_scan.tsv` is the full index scan). Two weak years:
**TY2016 misses ranks 401+** (about 165 people, ~$250B of wealth, ~$9B of
fiscal income at 3.66%; fillable from the Sept–Oct 2017 copy scaled by the
two copies' top-400 totals), and **TY2018 is probably 10–15% high** (the S&P
500 rose roughly 13% from Dec 31, 2018 to Aug 5, 2019 — to be checked; the
staged TY2018 total exceeds TY2017's). Profiles exist for 78–93% of each
year's people; the copies carry age and gender for all.

Files (gitignored cache on the shared filer): `resources/forbes/cache/wayback/`
— raw JSON copies (file names carry capture timestamps), `cdx_scan.sh` and
`cdx_scan.tsv`, `build_wayback_staging.R`, and the staging file
`forbes_billionaires_2015_2021_wayback.csv` in the splice's input schema plus
`entry_date`, `source_file`, `coverage`.

### 4.3 BSYZ parameters for 2014–17

Balkir, Saez, Yagan and Zucman, "Estimating Tax Burdens by Wealth Groups"
(IRS SOI working paper, revised Feb 2025,
https://www.irs.gov/pub/irs-soi/24rpestimatingtaxburdens.pdf), Table 2 (an
image, p. 11): per-unit wealth and income for the top 0.0002% (~400), 0.0001%
and 0.00005% (~100) in 2010–13, 2014–17 and 2018–20. Categories map as the
current file does (interest includes tax-exempt; "rent, estate, other" =
rental + estate and trust + royalties + other income excluding NOL
carryforwards); the next group is the top-400 total less the top 100.
**Check:** on 2018–20 this reproduces the current parameters (top-100 ratio
0.01751 vs 0.01751; next-300 0.0347 vs 0.0346).

| Group | Period | Fiscal income ÷ wealth | Capital gains | Dividends | Interest | Business | Rent, estate, other | Wages, pensions |
|---|---|---|---|---|---|---|---|---|
| Top 100 | 2014–17 | **2.61%** | 53.2% | 23.3% | 7.3% | **8.6%** | 3.1% | 4.5% |
| Top 100 | 2018–20 (current) | 1.75% | 62% | 28% | 11% | −11% | 6% | 5% |
| Next 268 / 300 | 2014–17 | **3.66%** | 61.8% | 14.8% | 8.8% | 7.4% | 2.5% | 4.8% |
| Next 300 | 2018–20 (current) | 3.46% | 60% | 16% | 8% | 4% | 6% | 5% |

The business sign flip is TCJA (full expensing; BSYZ NBER §3.4). Assumption
to confirm: 2014–17 parameters for TY2015–2017, 2018–20 from TY2018, no
interpolation.

### 4.4 Benchmarks

| Source | What | Years |
|---|---|---|
| IRS SOI **Table 4.3** — run for TY2017–2023 (§1.2) (`{yy}in43ts.xls`; https://www.irs.gov/statistics/soi-tax-stats-individual-statistical-tables-by-tax-rate-and-income-percentile) | Top 0.001% / 0.01% / 0.1% / 1% excluding dependents: count, AGI floor, AGI, wages, interest, dividends, net capital gain, Schedule C, partnership and S-corporation | 2014–2023 (the 2022 file reads with `readxl`; older years use other filenames) |
| IRS SOI **Table 4.1** | Top-percentile counts, AGI shares, floors | 2001–2023 |
| Pub 1304 **Table 1.4** | Class totals; top class $10M+ | every year (IRS-Ind panel) |
| **BSYZ companion Table 2** | The Forbes 400's own fiscal income by period | 2010–13, 2014–17, 2018–20 |
| IRS "400 highest AGI" report | Top-400 AGI and composition | ends 2014: context |
| **2015 PUF with its aggregates** | The shape of the tail the drop removed | 2015 |

### 4.5 The work, when the revamp takes it up

**Stage A — income-only splice, TY2017–2021 (TY2015 as a test).**
(1) Adopt the Wayback staging for 2015–2020 with the TY2016 and TY2018
treatments decided; extend the input file to 2015–2025; fetch missing profiles
or accept defaults. (2) Period-dependent BSYZ parameters (a `year_min` /
`year_max` on `bsyz_fiscal_income_params.csv`, selected in
`forbes_target_categories()`). (3) Ids for list years 2015–2019 in the spare
block below `FORBES_ID_BASE` (8.95e6–9e6), existing ids unchanged; keep the
asserts against the non-filer pools and `RNG_ID_SPACE`. (4) In
`build_splice_pools()` rank sources by income alone and drop the
wealth-density receivers; in `solve_forbes_weight_calibration()` drop the
net-worth constraint, keep count and the six income constraints. (5)
`years = 2017L:2025L` in `main.R`; check the Forbes mortality ledger covers
2017+ and the S21 partition still holds. (6) TY2015 standalone: splice the
2015 billionaires onto the raw PUF without its aggregates and compare with
the PUF with them, Table 1.4 and Table 4.3 2015 — the direct test of how much
of what the aggregates held the billionaires recover. (7) Rebuild with caches
loaded and run the tests below. Expect filer weights and top-tail income to
move from 2017; class totals barely, since the calibration is income-neutral;
rerun S40's class diagnostics.

**Stage B — wealth for TY2017–2021.** Today: `stage1_scf_tax_units.R` (SCF
2022 tax units) → `wealth.R` (per-cell distributional random forests, then a
calibrated donor tilt), pooled 2019 + 2022 donors, the Forbes purge; a 2022
base aged forward only. SCF 2016 and 2019 microdata are on the filer
(`/nfs/roberts/project/pi_nrs36/shared/raw_data/SCF/v1/{2016,2019}/`).
Options: **B1** deflate 2022 wealth by per-category indices (cheap; freezes
the 2022 cross-section); **B2** a second module at a 2019 base covering
2017–2021, with `materialize()` extended to a module valid for a year range
including backward aging (recommended); **B3** native bases per SCF wave (most
faithful, most work, seams at wave boundaries). Then re-enable the full splice
for 2017–2021 and compare with Stage A in the same year.

**Tests, both stages.** (1) Table 4.3 per year 2017–2023, with and without the
splice. (2) Table 1.4 by class, especially $2–5M, $5–10M, $10M+. (3) The
$10M+ bins and top-400 share (generalize `07_top_tail_2022.R` to a year and a
vintage; covers 1–3). (4) BSYZ Table 2 against the Forbes rows' own fiscal
income by period. (5) The TY2015 standalone. (6) Regression: S40 diagnostics
(`05_vintage_diagnostics.R`), the S21 partition, the E2 tripwire
(`research/state_weights/nonfiler_federal_validation.md`), which the splice
must not break for filer columns other than weights.

**Open decisions for that work** (each becomes an S-series entry when taken):

| # | Decision | Options | Recommendation |
|---|---|---|---|
| 1 | TY2016 ranks 401+ | fill from the Sept–Oct 2017 copy, scaled by top-400 totals · leave out | fill |
| 2 | TY2018 timing | market-index scaling · per-person interpolation TY2017 → TY2019 · accept with a note | market-index scaling, checked against the TY2017 → TY2019 path |
| 3 | Pre-2022 wealth | Stage A income-only first · straight to Stage B | Stage A first; B2 after |
| 4 | Ids before list year 2020 | spare block below 9e6 · move the epoch (renumbers every Forbes id; coordinated vintage with Tax-Simulator) | spare block |
| 5 | BSYZ by period | by period, no interpolation · interpolate | by period |

## 5. Provenance

- Scripts (committed with this note): `research/state_weights/filer_refit/08_table43.R`
  (+ `run_table43.sbatch`; Slurm 27755431; the IRS sheets and results under
  `results/table43/`), `research/state_weights/filer_refit/06_puf_soi_gaps.R`
  (+ `run_puf_soi_gaps.sbatch`; Slurm 27742729) and
  `research/state_weights/filer_refit/07_top_tail_2022.R` (+
  `run_top_tail_2022.sbatch`; Slurm 27745711); results under
  `research/state_weights/filer_refit/results/{puf_soi_gaps,top_tail_2022}/`.
  Both ran at commit `67983d1`.
- The 2015 PUF booklet (`2015 Public Use Booklet.pdf`, IRS-PUF interface):
  disclosure procedures pp. 3–4; aggregate-record tabulation pp. 31–32.
- BSYZ NBER w34170 (https://www.nber.org/papers/w34170), Table 1, Figure 3,
  §3.4; the companion IRS SOI working paper, Table 2.
- Wayback copies: `resources/forbes/cache/wayback/` (gitignored, shared filer).
- Decision: S41, `research/decisions_log.md`.

## Revision history

- **2026-09-29 (later)** — §3: dimensions 1 and 2 point at the base proposal
  and the bucket-aging prototype.
- **2026-09-29** — §1.2: the Table 4.3 comparison extended to TY2017–2023
  (`08_table43.R`, replacing the 2022-only script): the top 0.001% is 24–43%
  short of IRS AGI in every year, the floor above the IRS's in every year, the
  splice closes 27–40% where it exists, passthrough income a third to
  two-thirds of the gap throughout.
- **2026-09-28 (night)** — §1.2: the Table 4.3 TY2022 comparison run
  (`08_table43_2022.R`); the top 0.001% is 28% short of IRS AGI, the splice
  closes a third, and two-thirds of the remainder is passthrough income.
- **2026-09-28 (later)** — Restructured into the three goals: the issue, the
  short-term position (S41: aggregate records stay dropped, caveat in
  `CLAUDE.md`), the revamp scope; the Forbes extension moved to §4 as
  deferred work with its inputs.
- **2026-09-28** — Written (JI's session): the PUF-vs-Pub 1304 comparison, the
  2022 tail test, the Wayback lists, the 2014–17 BSYZ parameters, and the plan.
