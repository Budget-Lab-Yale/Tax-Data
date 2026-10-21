---
title: "Test 4, probe BR: bridge variables (mortgage interest, real estate tax, charity) from the SCF"
role: results
workstream: rebuild
status: current
updated: 2026-10-10
sot: research/rebuild/tests/probes/SPEC.md
supersedes: []
superseded_by: null
---

# Probe BR: can the SCF supply mortgage interest and real estate tax, and do they help?

The joint model learns how tax variables and wealth go together only
through variables both the PUF and the SCF measure. This probe added two
itemized deductions to that shared set, the interest paid deduction
(`E19200`) and the real estate tax deduction (`E18500`), built from SCF
items for every SCF tax unit and kept on the PUF for itemizers only. It
retrained test 3's model on that data and compared it with test 3's
version 2 model. A third variable, cash charitable contributions
(`E19800`), was tried in a second variant.

Every number below comes from a Slurm job run for this probe (logs in
`/nfs/roberts/scratch/pi_nrs36/jar335/probes/logs/`, job numbers given) or
from test 3's files, which are named where used.

## Summary

- **The SCF can supply both.** Both waves ask every homeowner the balance
  and interest rate of each loan on the home and on other real estate, and
  the real estate taxes on the home. Charitable giving is asked too, but
  only gifts of $500 or more appear as positive amounts.
- **On the PUF, `full_v2` codes the itemized deductions as 0 for the 70
  percent of returns that do not itemize.** In the new data those zeros are
  unknown, not zero.
- **Given the two bridge variables, the model completes SCF wealth much
  better**: 33.45 and 33.52 nats per record on held-out SCF records against
  34.69 to 34.71 for version 2 (two and three training runs). Almost all of
  the gain is in home value and home mortgage debt. When the bridge
  variables are hidden, the new model scores the same as version 2, so
  adding them costs nothing for records that lack them.
- **On PUF itemizers, completed housing wealth now rises with the
  deductions as it does in the SCF.** Version 2 gave itemizers who deduct
  no mortgage interest about $100,000 of mortgage debt on average, and gave
  65 to 82 percent of itemizers who deduct no real estate tax a home; the
  new model gives $0 and 7 to 12 percent. Mortgage debt of itemizers
  deducting $40,000 or more of interest goes from $0.55 to 0.64 million
  (version 2) to $0.98 to 1.04 million (SCF units in that band: $0.93
  million).
- **Net worth as a whole moves little**: median completed net worth of all
  PUF returns $65,000 in both new runs against $55,000 to $75,000 in
  version 2's three runs (SCF filer units: $81,000 in 2013, $98,000 in
  2016). The bridge variables inform housing, not financial assets.
- **Charity added nothing measurable** on top of the two housing
  variables.

## What was run

| Step | Script (this folder) | Slurm job | Output in `$Q = /nfs/roberts/scratch/pi_nrs36/jar335/probes/br/` |
|---|---|---|---|
| List SCF item labels | `br_labels.R`, `br_extract.R` | 28922037, 28922240 (the second stopped on a summary line after writing the 2013 items; not needed again) | `scf_items/items_2013.csv.gz`, `labels_2013.csv` |
| Itemized deductions on the PUF, by itemizer status | `br_puf_check.py` | 28922243 | log only |
| SCF household bridge amounts | `br_bridge.R` | 28922437 | `scf_items/bridge_hh_{2013,2016}.csv` |
| Data `full_v3br` (mortgage interest, real estate tax) | `br_prepare.py` (copy of `../posttrain/g_prepare.py`) | 28922606 | `data/full_v3br/` |
| Data `full_v3brc` (the same plus cash charity) | `br_prepare.py --charity` | 28922899 | `data/full_v3brc/` |
| Training, two seeds each | `br_train.sbatch` (runs `../posttrain/h_train.py` unchanged, test 3's settings) | 28922897, 28922898; 28923139, 28923140 | `model/full_v3br/`, `model/full_v3brc/` |
| Completions | `br_complete.sbatch` (runs `../posttrain/h_complete.py` unchanged) | 28923143 to 28923146 | same folders |
| Check of the residential property types | `br_check_res.R` | 28925439 | log only |
| Tables | `br_analyze.py`, `br_charity_bands.py` | 28924851, 28924995, 28925201 | `analysis/br_tables_main.md`, `br_tables.md`, `br_charity_bands.md` |

Version 2's checkpoints and completions are test 3's, read from
`/nfs/roberts/scratch/pi_nrs36/jar335/scf_joint/model/full_v2/`.

## Feasibility: what the SCF measures

The raw files are the ones test 3's unit builder reads:
`/nfs/roberts/project/pi_nrs36/shared/raw_data/SCF/v1/<wave>/historical/p13i6.dta`
and `p16i6.dta` (household by implicate, survey-year dollars), with the
summary files `SCFP2013.csv` and `SCFP2016.csv`. The items below exist in
both waves with the same numbers. Shares are of all households, weighted.

| Item | What it is | Households with a positive amount, 2013 / 2016 |
|---|---|---|
| X721, X722 | Real estate taxes on the home, amount and frequency (96 percent report a yearly amount; 2 percent twice a year; the rest monthly or quarterly) | 64% / 62% (owners with no tax: under 1%) |
| X805, X816; X905, X916; X1005, X1016 | Balance still owed and annual interest rate (hundredths of a percent) of the first, second and third mortgage on the home | first mortgage 41% in 2013 |
| X1044, X1045 | Other loans on the home, labelled "other purchase loans": balance, rate | under 1% |
| X1103, X1108, X1111 (and X1114/X1119/X1122, X1125/X1130/X1133) | Lines of credit 1 to 3: secured by the home or not, balance owed, rate | any line with a balance: 7% in 2013 (not all secured by the home) |
| X1703, X1705, X1715, X1726, X1729 (and X18xx) | Other real estate, properties 1 and 2: type, share owned, loan balance, rate, whether it yields income | a loan on the first property: 5% in 2013 |
| X2006 | Amount owed on remaining vacation homes (no rate asked) | 0.1% |
| X808, X809, PAYMORT1-3 (SCFP) | Regular mortgage payment and frequency | not used |
| X5822, X5823 | Any charitable contributions; amount | 36% / 38%; smallest positive amount $500 in both waves, 15 percent of givers at exactly $500 |

The SCF does not ask real estate taxes on second homes or land, and it
does not ask whether the household itemized.

**How the PUF-concept values were built** (`br_bridge.R`, then
`br_prepare.py`):

- **Interest paid (`E19200`).** For each loan, balance owed times the
  annual rate, summed over: the loans on the home (first to third
  mortgage, other purchase loans); home-equity lines in use (secured by the
  home); and loans on second homes, taken as residential other real estate
  (the residential property-type codes, checked below) from which the
  household reports no income (a rented property's interest goes on
  Schedule E, not Schedule A). The remaining-vacation-homes balance gets the
  wave's average rate on other real estate loans (5.4 and 5.1 percent).
  2015 law: interest on at most $1 million of purchase debt and $100,000 of
  home-equity debt (0.3 to 0.4 percent of households have purchase debt
  above $1 million). The household amount is split between a couple's two
  units half and half, as the builder splits housing; split-off adults get
  0.
- **Real estate tax (`E18500`).** X721 times the number of payments a year
  implied by X722; split and assigned the same way.
- **Cash charity (`E19800`, second variant only).** X5823, split by the
  earnings share as the builder splits most incomes; unobserved for
  split-off adults, whom the question does not cover.
- **Dollars.** SCF 2013 amounts (tax year 2012) are raised to 2015 by the
  CPI-U. SCF records are then ranked on the **PUF's 2015 itemizer
  distribution** for these variables, not on their own. The model's
  amounts are ranks within source and year; the PUF's positive amounts
  come only from itemizers, the larger amounts, so ranking each source on
  its own would give the same token to a $7,000 PUF deduction and a $4,000
  SCF amount. On the common scale a token means the same dollars in both.

**Checks.** Home purchase loans rebuilt here total 1.011 (2013) and 1.010
(2016) times the summary file's `NH_MORT`; home-equity lines 1.020 and
1.011 times `HELOC`. The residential property types used for second homes
reproduce the summary file's debt on other residential property: all
residential loans rebuilt this way are 0.986 (2013) and 0.995 (2016) times
`RESDBT` (job 28925439, `br_check_res.R`). The second-home loans kept
(no income from the property) are 59 and 55 percent of `RESDBT`; the rest
is debt on rented-out residential property. Implied rates on
home loans: median 4.2 percent (2013) and 3.9 percent (2016), unweighted.
Every SCF item used had no missing values; negative codes (-1, "none";
-2 on five rate records) are read as 0.

**Weaknesses.**

1. Interest is the balance at the interview times the current rate. The
   deduction is interest paid during the tax year, on a balance that was
   higher at the start of the year and at a rate that may have changed
   since (refinancing, new purchases after the tax year).
2. The PUF's `E19200` also includes points and investment interest; the
   SCF amount is mortgage and home-equity interest only.
3. Second homes are identified by "residential, no income", which misses
   part-year rentals and may include some properties whose interest is not
   deductible.
4. Real estate tax covers the home only; the PUF's amount also includes
   taxes on second homes and land. SCF owners of second homes are
   understated.
5. The household is split half and half between a couple's two units,
   whoever paid.
6. Charity: an SCF zero means "under $500", while a PUF itemizer's
   positive amount is often smaller (a tenth of itemizers who give report
   under $170). The amount is one number for the family; whether it
   includes gifts of property was not confirmed from the files.

**Totals, PUF itemizers against SCF filer units** (data build logs,
2015 dollars; all parts):

| Variable | PUF itemizers: total ($bn), share nonzero, mean of nonzero | SCF 2013 filer units | SCF 2016 filer units |
|---|---|---|---|
| `E19200` interest paid | 304, 75%, $9,100 | 384, 43%, $6,800 | 345, 40%, $6,100 |
| `E18500` real estate tax | 189, 84%, $5,000 | 248, 59%, $3,200 | 262, 56%, $3,300 |
| `E19800` cash charity | 155, 74%, $4,700 | 237, 38%, $5,400 | 310, 40%, $6,400 |

The SCF totals cover every filer unit, itemizer or not, so they should be
above the PUF's. If the two measured the same thing, non-itemizers would
hold 12 to 21 percent of the SCF's interest and 24 to 28 percent of its
real estate tax. The SCF charity total
also includes gifts the PUF records as non-cash ($56 billion).

## The PUF side: non-itemizers are coded zero in `full_v2`

Job 28922243 read `full_v2`'s records. FDED is 1 (itemized) for 29.6
percent of weighted returns, 2 (standard) for 69.0 percent and 3 (none)
for 1.3 percent. For returns with FDED 2 or 3, all six itemized deductions
(`E17500`, `E18400`, `E18500`, `E19200`, `E19800`, `E20100`) are 0 on
every record, never unknown. In version 2 this did no harm, because these
were PUF-only variables and the zero follows from FDED. Once a variable is
shared with the SCF it matters: the model would learn from the PUF that
the 70 percent of filers who take the standard deduction pay no mortgage
interest. In `full_v3br`, `E19200` and `E18500` (and `E19800` in
`full_v3brc`) are unknown for the 90,727 PUF records with FDED 2 or 3
(none had a nonzero value). The other itemized deductions stay PUF-only
and keep version 2's coding, so that the comparison isolates the bridge.
FDED stays unobserved on SCF records.

## Results

### Held-out SCF records: completion score

All 24 wealth values of each SCF test record with wealth observed (12,021
records) are hidden and scored in the fixed completion order, as in H's
report. "Bridge hidden" hides the bridge variables too and draws them after
the wealth, so the wealth is scored without them. Lower is better; the
total is in nats per record, the two right columns in nats per record for
one variable.

| Model | Seed | All 24 wealth values | Home value | Home mortgage debt |
|---|---|---|---|---|
| Version 2 | 1 | 34.69 | 2.889 | 1.864 |
| Version 2 | 2 | 34.69 | 2.907 | 1.844 |
| Version 2 | 3 | 34.71 | 2.901 | 1.853 |
| Bridge, observed | 1 | 33.52 | 2.293 | 1.365 |
| Bridge, observed | 2 | 33.45 | 2.280 | 1.337 |
| Bridge, hidden | 1 | 34.72 | 2.895 | 1.874 |
| Bridge, hidden | 2 | 34.70 | 2.902 | 1.871 |
| Bridge and charity, observed | 1 | 33.39 | 2.286 | 1.352 |
| Bridge and charity, observed | 2 | 33.46 | 2.283 | 1.350 |
| Bridge and charity, hidden | 1 | 34.64 | 2.902 | 1.867 |
| Bridge and charity, hidden | 2 | 34.73 | 2.912 | 1.880 |

The gain from observing the bridge, 1.2 nats per record, is eight times
the gain from version 1 to version 2 (six more shared incomes and
demographics: 0.15 nats). Home value and mortgage debt carry 1.1 of the
1.2. Charity adds nothing beyond the spread between seeds.

Training was the same as version 2's in every respect: best epoch 14 to 16
of 29 to 31, 11 to 18 minutes per run.

### Held-out SCF records: levels by income group

Same records, five draws each; income groups rank test 2's seven-income
total within wave; both waves pooled, each in its own dollars. "Hidden"
columns draw wealth without the bridge variables.

**Share owning a home (%)**

| Income group | Truth | Version 2, seed 1 | Version 2, seed 2 | Bridge, seed 1 | Bridge, seed 2 | Hidden, seed 1 | Hidden, seed 2 |
|---|---|---|---|---|---|---|---|
| Bottom 50% | 49.5 | 48.7 | 47.0 | 49.1 | 49.0 | 47.9 | 45.7 |
| 50-90% | 81.6 | 79.0 | 78.1 | 81.4 | 81.1 | 77.2 | 74.7 |
| 90-99% | 92.4 | 92.4 | 92.2 | 91.8 | 91.6 | 92.4 | 92.4 |
| 99-99.9% | 99.2 | 96.7 | 98.0 | 98.8 | 98.7 | 96.2 | 98.2 |
| Top 0.1% | 99.5 | 95.7 | 98.5 | 95.8 | 99.6 | 94.9 | 98.0 |
| All | 66.7 | 65.2 | 64.0 | 66.3 | 66.2 | 64.1 | 62.0 |

**Mean home value**

| Income group | Truth | Version 2, seed 1 | Version 2, seed 2 | Bridge, seed 1 | Bridge, seed 2 | Hidden, seed 1 | Hidden, seed 2 |
|---|---|---|---|---|---|---|---|
| Bottom 50% | $70k | $78k | $78k | $71k | $73k | $74k | $78k |
| 50-90% | $200k | $222k | $211k | $209k | $213k | $198k | $202k |
| 90-99% | $624k | $648k | $651k | $588k | $592k | $646k | $661k |
| 99-99.9% | $1.51m | $1.65m | $1.76m | $1.60m | $1.52m | $1.89m | $1.86m |
| Top 0.1% | $3.54m | $2.36m | $3.15m | $2.55m | $2.88m | $2.79m | $2.75m |
| All | $188k | $204k | $201k | $189k | $192k | $194k | $199k |

**Mean home mortgage debt**

| Income group | Truth | Version 2, seed 1 | Version 2, seed 2 | Bridge, seed 1 | Bridge, seed 2 | Hidden, seed 1 | Hidden, seed 2 |
|---|---|---|---|---|---|---|---|
| Bottom 50% | $24k | $26k | $24k | $25k | $24k | $24k | $25k |
| 50-90% | $92k | $94k | $92k | $92k | $91k | $86k | $86k |
| 90-99% | $218k | $242k | $236k | $237k | $234k | $238k | $248k |
| 99-99.9% | $390k | $415k | $444k | $361k | $374k | $418k | $461k |
| Top 0.1% | $892k | $662k | $575k | $764k | $798k | $698k | $582k |
| All | $73k | $77k | $75k | $74k | $74k | $72k | $74k |

**Mean and median net worth**

| Income group | Truth, mean | Version 2, mean (seeds 1, 2) | Bridge, mean (seeds 1, 2) | Truth, median | Version 2, median | Bridge, median |
|---|---|---|---|---|---|---|
| Bottom 50% | $131k | $172k, $170k | $159k, $154k | $22k | $17k, $19k | $20k, $21k |
| 50-90% | $463k | $606k, $506k | $521k, $607k | $180k | $168k, $159k | $164k, $177k |
| 90-99% | $2.43m | $3.14m, $2.82m | $2.81m, $2.87m | $1.06m | $1.18m, $1.19m | $1.08m, $1.19m |
| 99-99.9% | $16.08m | $17.24m, $16.24m | $17.40m, $15.28m | $9.02m | $10.36m, $9.09m | $10.18m, $8.88m |
| Top 0.1% | $47.53m | $51.42m, $44.45m | $47.70m, $35.86m | $14.44m | $27.51m, $26.02m | $27.71m, $19.28m |
| All | $661k | $816k, $731k | $744k, $751k | $85k | $84k, $87k | $87k, $92k |

Housing moves toward the truth where the bridge is observed: the share
owning a home in the middle group (81.4 and 81.1 against 81.6; version 2
78 to 79), mean home value overall ($189k and $192k against $188k;
version 2 $201k to $204k) and top 0.1 percent mortgage debt ($764k and
$798k against $892k; version 2 $575k to $662k). Net worth by group is
within the spread between seeds of either model, except that the bottom
half's mean is somewhat closer to the truth ($154k to $159k against $131k;
version 2 $170k to $172k).

### PUF records: completed housing wealth by deduction band

PUF training records (166,156), wealth drawn five times with the SCF tag at
2015, as test 3 did. Itemizers only. The last column is the SCF's filer
units (both waves, all parts) whose constructed amount falls in the band;
it is a reference for the positive bands, where nearly every unit has a
mortgage or owns a home. It is not a reference for the zero band, which in
the SCF is mostly renters who would not itemize.

**Mean home mortgage debt, by mortgage interest deducted (`E19200`)**

| Band | Share of itemizers | Version 2, seed 1 | Version 2, seed 2 | Version 2, seed 3 | Bridge, seed 1 | Bridge, seed 2 | SCF units in band |
|---|---|---|---|---|---|---|---|
| 0 | 25% | $103k | $102k | $105k | $0k | $0k | $0k |
| under $5k | 22% | $139k | $130k | $136k | $74k | $75k | $61k |
| $5k-10k | 29% | $164k | $155k | $162k | $172k | $173k | $154k |
| $10k-20k | 19% | $243k | $232k | $222k | $306k | $308k | $295k |
| $20k-40k | 4% | $421k | $393k | $359k | $616k | $606k | $584k |
| $40k and over | 1% | $642k | $596k | $554k | $978k | $1.04m | $932k |

**Mean home value, by mortgage interest deducted**

| Band | Version 2, seed 1 | Version 2, seed 2 | Version 2, seed 3 | Bridge, seed 1 | Bridge, seed 2 | SCF units in band |
|---|---|---|---|---|---|---|
| 0 | $335k | $353k | $381k | $228k | $235k | $93k |
| under $5k | $338k | $339k | $380k | $296k | $316k | $163k |
| $5k-10k | $323k | $322k | $364k | $330k | $340k | $264k |
| $10k-20k | $488k | $489k | $517k | $530k | $534k | $508k |
| $20k-40k | $961k | $1.00m | $944k | $1.11m | $1.07m | $1.14m |
| $40k and over | $1.86m | $2.05m | $1.77m | $2.21m | $2.21m | $1.59m |

**Share owning a home (%) and mean home value, by real estate tax deducted (`E18500`)**

| Band | Share of itemizers | Own, version 2 (seeds 1-3) | Own, bridge (seeds 1, 2) | Own, SCF units | Home value, version 2 | Home value, bridge | Home value, SCF units |
|---|---|---|---|---|---|---|---|
| 0 | 16% | 65, 82, 72 | 7, 12 | 3 | $210k, $244k, $267k | $26k, $38k | $3k |
| under $2.5k | 29% | 91, 97, 90 | 100, 100 | 100 | $253k, $264k, $293k | $224k, $240k | $139k |
| $2.5k-5k | 28% | 93, 97, 93 | 100, 100 | 100 | $359k, $361k, $394k | $354k, $369k | $289k |
| $5k-10k | 20% | 95, 98, 95 | 100, 100 | 100 | $528k, $521k, $562k | $544k, $544k | $493k |
| $10k-20k | 7% | 96, 98, 97 | 100, 100 | 100 | $871k, $857k, $886k | $972k, $939k | $916k |
| $20k and over | 2% | 97, 99, 98 | 100, 100 | 100 | $1.78m, $1.83m, $1.76m | $2.45m, $2.46m | $2.26m |

Version 2 knew a return's deductions but had never seen them next to
wealth, so its housing wealth rose with them only through income. The
bridge model reproduces the SCF's gradient in the positive bands (one
exception: home value in the top mortgage interest band, $2.21m against the
SCF's $1.59m) and removes the contradictions at zero (mortgage debt with no interest paid,
homes with no property tax). The two runs of the bridge model agree with
each other much more closely than version 2's three runs do.

Median completed net worth by band is within the spread of version 2's
three runs in almost every band (for example, itemizers deducting $10,000
to $20,000 of mortgage interest: version 2 $543k to $804k, bridge $510k and
$557k). In every model it is highest in the top bands of both deductions,
but by mortgage interest it is not monotonic: itemizers deducting no or
little interest have higher medians than those deducting $5,000 to
$10,000. Full tables: `$Q/analysis/br_tables_main.md`.

### PUF records: overall completed wealth

| PUF returns | Measure | Version 2, seeds 1, 2, 3 | Bridge, seeds 1, 2 |
|---|---|---|---|
| Itemizers | Own a home (%) | 88.7, 94.8, 89.4 | 85.0, 85.8 |
| Non-itemizers | Own a home (%) | 39.2, 34.4, 41.5 | 43.9, 43.9 |
| All returns | Own a home (%) | 53.9, 52.4, 55.8 | 56.1, 56.4 |
| All returns | Mean home value | $173k, $169k, $192k | $176k, $178k |
| All returns | Mean home mortgage debt | $72k, $65k, $70k | $68k, $70k |
| All returns | Mean net worth | $807k, $713k, $904k | $766k, $741k |
| All returns | Median net worth | $55k, $57k, $75k | $65k, $65k |
| Itemizers | Median net worth | $442k, $411k, $580k | $394k, $436k |
| Non-itemizers | Median net worth | $14k, $16k, $20k | $22k, $20k |

SCF filer units, for reference (own dollars of each wave): own a home 67.5
percent (2013) and 65.8 (2016); mean home value $173k and $193k; mean home
mortgage $69k and $67k; mean net worth $543k and $705k; median $81k and
$98k. The PUF includes dependent filers (6 percent of returns), whom the
SCF's filer units do not, so the PUF's ownership share is expected to be
lower.

Mean completed home mortgage debt by AGI group falls in the 90th to 99.9th
percentiles (version 2 $247k to $275k and $527k to $588k; bridge $219k to
$225k and $446k to $458k). The SCF's own values for its 90-99 and 99-99.9
percent income groups are $218k and $390k (table above); the income
measures differ, so this is suggestive only.

### Charity

Adding cash charity as a third bridge variable changed nothing measurable:
the SCF score (above) is within the spread of seeds, and completed net
worth of PUF itemizers by charity band is the same as without it (for
example, median net worth of itemizers giving $20,000 to $100,000: version
2 $4.30m and $3.98m, bridge $3.90m and $3.86m, bridge and charity $4.24m
and $4.30m; SCF units $4.79m; `$Q/analysis/br_charity_bands.md`). Net
worth already rises steeply with charity in version 2, through income.

## What it means for the plan

- **Bridge variables are worth adding.** Two variables the PUF records for
  30 percent of returns, and the SCF can build for everyone, improved the
  completion of the SCF's wealth by eight times as much as six extra
  shared incomes did, and made the completed housing wealth of PUF
  itemizers consistent with their deductions. They cost nothing for
  records without them. Mortgage interest and real estate tax should go
  into the main dataset.
- **Their reach is housing.** Net worth moves little because most of it,
  at the top, is financial and business wealth. More gain needs bridges to
  those.
- **The coding rule matters.** A shared variable that the PUF records for
  a subset must be unknown, not zero, outside the subset, and the two
  sources should share a dollar scale when one source's positive values
  are a selected part of the other's. Both were needed here; the second is
  a departure from "ranks within source and year" that the authority table
  (probe AU) may want to state.
- **Other candidates**, in order of likely value:
  1. **Rental income against rental property.** The SCF records net income
     and loan balances per property (X1730, X1715 and the like); the PUF
     has rent and royalty income and loss (`E25850`, `E25860`). This would
     tie Schedule E to other real estate and its mortgages, and is option
     (c) in `g_crosswalk.md`.
  2. **Taxable IRA distributions (`E01400`)** against the SCF's
     withdrawals from retirement accounts, for retirees' account balances;
     the builder's withdrawal amount would need separating by account
     type.
  3. **Student loan interest (`E03210`)** against the SCF's education
     loans (balances and payments are asked; I did not check for rates); a
     link to installment debt for younger filers. It is an adjustment, not an itemized
     deduction, but it is capped and phased out by income, so the same
     "known only where claimed" rule applies.
  4. **Charity**: not worth it now, for the reasons above.
- **The itemization flag itself** could be translated to the SCF (simulate
  which units would itemize), which would let FDED carry information on
  SCF rows; not tried.

## What is unfinished

- Two training runs per variant; three would match version 2.
- No post-training, and no generation of PUF records from scratch with
  the new model; whether the bridge helps generated records was not
  checked.
- The SCF test score treats every SCF unit as having the bridge observed.
  A version that hides it for SCF units that would not itemize (to mirror
  the PUF) was not built; the observed and hidden rows above bound it.
- The SCF amounts were not compared with SOI's 2015 totals for itemizers
  beyond the PUF itself; investment interest was not separated from
  `E19200`.
- No job is still running.
