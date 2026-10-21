---
title: "Minimal example, step 4: the rank-dollar map"
role: evidence
workstream: rebuild
status: current
updated: 2026-10-14
sot: research/rebuild/design/plan.md
supersedes: []
superseded_by: null
---

# Minimal example, step 4: the rank-dollar map

## Update of 2026-10-14, second round

**What changed.** Step 1 now cuts negative amounts into 10 bands instead of
4, finer at the extreme (84 bands in all). The model was retrained on the
new bands and `gen_puf.parquet` regenerated. The code reads the band layout
from `band_edges.json` instead of assuming it. The capital gains amount is
now compared with SOI's Schedule D net gain minus net loss, not the 1040
line (step 5 report, note of the same date). Job 29589991. The first-round
outputs are kept in `map/v1/`; the sections below this one are the first
round.

**The draw, on the held-out PUF records:**

| | First round (4 negative bands) | Second round (10) |
|---|---|---|
| `other_agi` total, spread over 20 draws | 16% ($74–215bn; true $205bn) | 1.3% ($196–207bn) |
| AGI total, spread over 20 draws | 3.1% | 0.3% |
| Share with negative AGI (true 1.04%) | 1.35% | 1.21% |
| Share with AGI exactly zero (true 0.31%) | 0.26% | 0.26% |

The finer bands fix the noisy `other_agi` draw. The draw still adds some
negative-AGI records, because each part is drawn on its own. The zero-AGI
share is unchanged: keeping AGI at exactly zero needs offsetting draws.

**Generated file against SOI 2015, by AGI bracket.** For each cell: the
number of the 19 brackets within 5% of SOI, and the median gap.

| Cell | Generated, first round | Generated, second round | Real PUF |
|---|---|---|---|
| Returns | 13, 3.6% | 16, 1.3% | 19, 0.6% |
| Wages, count | 13, 4.3% | 15, 1.6% | 18, 0.7% |
| Wages, amount | 10, 4.0% | 11, 4.1% | 17, 0.9% |
| Interest, count | 8, 6.7% | 11, 3.9% | 19, 0.8% |
| Interest, amount | 3, 20% | 4, 15% | 13, 2.0% |
| Dividends, count | 10, 4.2% | 10, 5.0% | 18, 0.9% |
| Dividends, amount | 6, 9.3% | 7, 12% | 14, 1.7% |
| Capital gains, count | 6, 12% | 6, 8.3% | 16, 1.6% |
| Capital gains, amount | 1, 24% | 6, 11% | 7, 7.9% → 11, 4.5% |

The capital gains amount rows are not like for like: the first round
compared with the 1040 line and the second with Schedule D. The real PUF's
own row moves from 7 to 11 brackets within 5% from that change alone.

The brackets that moved most (generated, gap to SOI):

| Bracket | Cell | First round | Second round |
|---|---|---|---|
| No AGI | returns | +25% | +5.4% |
| No AGI | wages amount | +80% | +20% |
| $1–1.5 million | returns | +22% | +3.3% |
| $2–5 million | returns | +19% | −1.4% |
| $10 million or more | returns | +33% (32 records) | −58% (10 records) |

- The no-AGI excess is mostly gone. The generated file changed for two
  reasons at once, the finer bands and the retrained model, and this run
  cannot separate them. The round trip above suggests the bands did much of
  it.
- The top bracket now holds 10 records, so its totals are noise. This is
  what oversampling the top is for.
- Fill rules on the 200,000 generated records: 51 lump-value draws (39
  interest, 8 capital gains, 4 dividends). 26 records sit in a negative band
  of an amount that has no negatives and were given zero (5 wages, 9
  interest, 12 dividends). `other_agi` needed no rule.

## Bottom line

- The map works on real records. Banding the held-out PUF records and drawing
  dollars back gives totals within about 1% and percentiles within about 4%
  for wages, interest, dividends and capital gains. The share at zero is
  exact by construction.
- One amount has a problem: `other_agi`. Its most negative band runs from
  −$5,300 to −$137 million. A record with a weight of 1,000 that happens to
  draw a $100 million loss adds −$100 billion. Across 20 draws its total on
  the test records ranged from $74 billion to $215 billion, against a true
  $205 billion. Derived AGI inherits this (spread about 3% of the total). The
  fix belongs in step 1: more negative bands for `other_agi`, finer at the
  extreme, as the positives already have.
- The same band also pushes too many records below zero AGI: in the round
  trip, the share of real records with negative AGI rises from 1.04% to
  1.35%.
- The generated 2015 file (step 3, `gen_puf.parquet`, 200,000 records) is
  close to SOI 2015 in the middle brackets: returns and wages mostly within
  5%. It is far off in three places: 25% too many returns with no AGI (most
  likely the band problem above, not the model), capital gains amounts below
  $200,000 (as on the real PUF, and worse), and the brackets above $1
  million, which hold only 30 to 300 records each.
- The 2019 map is the crude version: one growth factor per amount. The plan's
  version stretches the map to SOI's 2019 brackets.

## 1. What ran

Slurm job 29589134 (1 minute) ran `04_dollars.py` through `04_run.sbatch`,
on the rebuilt step 1 table (after `STEP1_DONE`): 207,692 PUF records and
34,377 SCF rows, with `GEN_FILE` set to
`/nfs/roberts/scratch/pi_nrs36/jar335/minimal/gen/gen_puf.parquet`. The
earlier runs (jobs 29586660, 29587450, 29588312) gave the same numbers. Log:
`/nfs/roberts/scratch/pi_nrs36/jar335/minimal/logs/04_run_29589134.out`.

A small shared file, `soi_cells.py`, is used by both steps 4 and 5. It
lines up SOI's published cells (returns, and each item's count and amount,
by AGI bracket) with the records' contributions to them.

Outputs in `/nfs/roberts/scratch/pi_nrs36/jar335/minimal/map/`:

| File | What it holds |
|---|---|
| `map_puf_2015.parquet`, `map_scf_2015.parquet` | The 2015 maps. One row per real record per amount: amount, band, dollar value, survey weight. Plus the fill rows described in section 2. |
| `map_puf_2019.parquet` | The 2015 PUF map grown to 2019 (section 4). |
| `growth_factors.csv` | The 2019 factors and their SOI inputs. |
| `roundtrip.csv`, `roundtrip_seeds.csv` | The round-trip check (section 3). |
| `empty_band_hits.csv` | How often a fill rule was used in the round trip. |
| `puf_vs_soi_2015.csv` | Real PUF and the generated file against SOI 2015, by AGI bracket (section 5). |

## 2. How the map works

For each source, amount and band, the map is the full list of the real
records' dollar values in that band, with their survey weights. To give a
banded record dollars, the script draws one value from its band's list, with
probability proportional to the weight of the record that holds it. Band 0 is
exactly zero. Band −1 (amount absent, such as PUF net worth) stays missing.
The seed is fixed, and each record's draw depends only on its row.

AGI is never stored. It is the sum of wages, interest, dividends, capital
gains and `other_agi`, computed after the dollars are drawn.

**Empty bands.** A band is empty when one round amount holds more weight than
the band is wide, so two cut points coincide (step 1 report, section 4). The
rules, written into the map as extra rows marked `filled`:

1. **Empty band from a lump:** the record gets the lump value itself, which
   is the coinciding cut point. That is the dollar amount at those ranks in
   the real data. Example: PUF interest band 6 sits on the $2 lump, so it
   gives $2.
2. **Negative band of an amount with no negatives** (wages, interest,
   dividends): zero, the nearest amount in dollar order.
3. **A band that is empty only in a subset of the records.** This happens
   only in the round trip below, whose map leaves out the test records. The
   record gets the band's upper cut point, a real value in that band.

How often they fire: on real records, rules 1 and 2 never fire, because a
real record cannot sit in an empty band. Rule 3 fired twice in the round trip,
both on SCF capital gains. The rules exist for generated records. In the
200,000 generated PUF records, rule 1 fired 54 times (39 interest, 8 capital
gains, 7 dividends) and rule 2 four times (3 wages, 1 dividends). `other_agi`
never needed a rule.

## 3. Round trip on held-out records

Method: take the test split (10% of each source), keep its bands, throw away
its dollars, draw dollars from a map built without the test records, and
compare with the true dollars, using the survey weights. Percentiles are over
all records, zeros included. "Spread" is the standard deviation of the total
across 20 draws, as a share of the total.

**PUF, 20,758 test records** (totals in $ billion, 2015):

| | True total | Drawn | Spread over 20 draws | P99 true / drawn | P99.9 true / drawn |
|---|---|---|---|---|---|
| Wages | 714.0 | +0.1% | 0.1% | 319,400 / 318,200 | 1,007,000 / 1,022,000 |
| Interest (taxable) | 9.39 | +1.3% | 2.4% | 8,590 / 8,260 | 80,900 / 79,500 |
| Dividends | 25.5 | +0.7% | 1.2% | 27,700 / 27,900 | 173,500 / 177,300 |
| Capital gains | 61.6 | −0.6% | 0.5% | 43,600 / 43,300 | 507,800 / 488,900 |
| other_agi | 204.7 | −5.0% | 16% | 182,060 / 181,550 | 884,860 / 885,530 |
| AGI (sum of parts) | 1,015.3 | −0.9% | 3.1% | 474,600 / 474,170 | 2,088,000 / 2,125,368 |

The 50th and 90th percentiles match to within 0.6% for every amount. The
share with AGI exactly zero falls from 0.31% to 0.26%, because a record keeps
AGI at zero only if every part draws the same offsetting values. The share
with negative AGI rises from 1.04% to 1.35%. Each part is drawn on its own
within its band, so a record with ordinary wages can draw a very large loss
from `other_agi`'s lowest band.

**SCF, 3,400 test rows**:

| | True total ($bn) | Drawn | Spread | P99.9 true / drawn |
|---|---|---|---|---|
| Wages | 775.6 | +0.1% | 0.3% | 1,260,000 / 1,210,000 |
| Interest | 5.10 | −8.9% | 1.0% | 25,000 / 25,000 |
| Dividends | 18.6 | +0.8% | 0.8% | 120,000 / 125,000 |
| Capital gains | 106.7 | +8.3% | 2.7% | 1,050,000 / 1,050,000 |
| Net worth | 8,676 | −1.5% | 0.9% | 44.0m / 44.7m |

The SCF misses on interest and capital gains are not draw noise: all 20
draws miss in the same direction. The SCF's top bands hold a handful of
households, so the test households' values inside a top band differ from the
other households'. That is a train-versus-test difference, which a map built
from all records would not have.

**The `other_agi` problem.** Step 1 gives negatives only 4 bands, by quartile
of negative weight. For `other_agi` the lowest quartile runs from −$5,300 to
−$137 million and holds 23,063 records. Ordinary records in it carry weights
in the hundreds or thousands; the very large losses belong to records with
weights near 1. The draw is right on average but very noisy, as the 16%
spread shows. The same will happen to generated records, which all carry
weights near 790 in 2019. Suggested fix, for step 1: give the negative side
the same fine bands at the extreme as the positive side. Capital gains do not
have the problem on the PUF, because their losses stop at −$3,000.

## 4. The 2019 map

This is the crude version. The plan's version stretches the 2015 map so that
each SOI 2019 AGI bracket's count and mean are matched. Here, every value of
an amount is multiplied by one factor:

| Amount | Factor | SOI item used (amount per return with the item) |
|---|---|---|
| Wages | 1.117 | Salaries and wages |
| Interest | 1.400 | Taxable interest |
| Dividends | 1.214 | Ordinary dividends |
| Capital gains | 1.024 | Schedule D net gain, per return with a gain |
| other_agi | 1.098 | AGI minus wages, taxable interest, ordinary dividends and the 1040 capital gain line, per return filed |

Choices:

- The factor is the 2019 amount per return *with the item* over the 2015
  amount per return with the item. Using all returns instead would put the
  growth in how many people have the item into the map. The tilt also targets
  that count, so it would be counted twice. The all-returns factors are in
  `growth_factors.csv` for comparison. They differ most for interest (1.52)
  and capital gains (1.16).
- Capital gains: only positive values are grown. Nearly all PUF losses are
  the deducted loss, capped at $3,000 in nominal terms in both years.
- `other_agi` has no published count, so its factor is per return filed.
  Its negative values (mostly business losses) are grown like the positives.
- Net worth is not grown. A 2019 record's net worth would still be in 2015
  dollars. No target uses it.

## 5. Real PUF against SOI 2015, by AGI bracket

This is the yardstick for the generated file. The model can be expected to
match SOI only as well as the data it learned from. The table counts, out of
19 brackets, how many are within 2% of SOI:

| Cell | Brackets within 2% | Median gap | Largest gap |
|---|---|---|---|
| Returns | 16 | 0.6% | $10M or more: −3.1% |
| Wages, count / amount | 16 / 16 | 0.7% / 0.9% | no AGI: −6.9%; $10M+: −8.7% |
| Taxable interest, count / amount | 17 / 10 | 0.8% / 2.0% | $10M+ amount: −43% |
| Dividends, count / amount | 14 / 10 | 0.9% / 1.7% | $10M+ amount: −42% |
| Capital gains, count / amount | 11 / 6 | 1.6% / 7.9% | $1–5k amount: SOI +$0.11bn, PUF −$0.18bn |

Two patterns:

- **The top bracket's amounts are short** by a third or more. These are the
  four aggregate records step 1 dropped (step 1 report, section 2).
- **Capital gains amounts are short by 8–40% in every bracket under
  $200,000.** The counts match, so the people are there and their amounts
  are smaller. My guess, not checked: the PUF's capital gains variable leaves
  out capital gain distributions reported directly on the 1040 ($11.6 billion
  in 2015, concentrated in the middle). SOI's 1040-line amount includes them.
  The counts would still match, because step 5 already counts only Schedule D
  gains and losses. If the guess is right, the capital gains amount target
  should drop the distributions. Checking means reading the PUF's
  distributions column, which step 1 did not keep.

**Generated file.** `gen_puf.parquet`: 200,000 PUF-labelled records from
step 3, drawn in the random order within the income block. Dollars come from
the 2015 PUF map. Each record gets the same weight, scaled so the file adds up
to SOI's 150.5 million returns. Out of 19 brackets, how many are within 5% of
SOI, and the median gap:

| Cell | Generated: within 5% | Generated: median gap | Real PUF: within 5% |
|---|---|---|---|
| Returns | 13 | 3.6% | 19 |
| Wages, count / amount | 13 / 10 | 4.3% / 4.0% | 18 / 17 |
| Taxable interest, count / amount | 8 / 3 | 6.7% / 20% | 19 / 13 |
| Dividends, count / amount | 10 / 6 | 4.2% / 9.3% | 18 / 14 |
| Capital gains, count / amount | 6 / 1 | 12% / 24% | 16 / 7 |

Where the generated file is off:

| Bracket | Records | Returns | Wages amount | Capital gains amount |
|---|---|---|---|---|
| No AGI | 3,440 | +25% | +80% | −99% |
| $1–$5,000 to $30–40,000 | 12,000–20,000 each | −4% to +6% | −17% to +4% | −20% to −264% (real PUF: −6% to −268%) |
| $40,000 to $1 million | 1,200–26,000 each | −4% to +5% | −7% to +6% | −13% to +7% |
| $1 million to $5 million | 114–318 each | +7% to +22% | +8% to +31% | +15% to +31% |
| $5 million and up | 32–38 each | 0% and +33% | −13% and +103% | −5% and −46% |

- **No AGI.** Too many returns, too much wage income, almost no capital
  gains. The round trip on real records raises the share with negative AGI by
  30%. So most of this probably comes from the draw (wages plus a very large
  `other_agi` loss), not from the model. I have not separated the two.
- **Capital gains in the lower brackets** are short on the real PUF too
  (section 5 above). The generated file is short by more than the PUF in
  most of these brackets.
- **Above $1 million** the file holds a few hundred records, so a bracket's
  totals move by tens of percent from a few draws. This is what oversampling
  the top is for.

## 6. Not done or not verified

- Only `gen_puf.parquet` was checked. `gen/` also holds a second seed, a
  fixed-order version and a version with net worth; the script runs on any
  one of them with `GEN_FILE` set.
- How much of the no-AGI excess comes from the draw and how much from the
  model is not separated. Drawing dollars for the real test records gives 30%
  more negative-AGI records; the generated file has 25% more no-AGI returns
  than SOI.
- The SCF has no 2019 map. Run B uses PUF-labelled records only.
- The `other_agi` band fix (section 3) is a suggestion for step 1 and is not
  made here.
- `04_dollars.py` is 351 lines, a little over the 300 asked for.
