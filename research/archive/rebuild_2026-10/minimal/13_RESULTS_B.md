---
title: "Minimal example: option B (ranks over the whole distribution, zeros included) against E4"
role: evidence
workstream: rebuild
status: current
updated: 2026-10-15
sot: research/rebuild/design/plan.md
supersedes: []
superseded_by: null
---

# Option B against E4, single-year table

John's "option B" is the rule "rank over the whole distribution including
zeros" of `research/rebuild/minimal/12_CROSSWALK_THEORY.md`, section 2. Each
source's wages, interest, dividends and capital gains are banded on that
source's own full weighted distribution, zeros and losses included, so a
survey unit and a PUF unit at the same band are at the same share of their
own population. E4 instead bands both sources on the PUF's dollar cut
points. Both are scored by the same script as `09_RESULTS.md`
(`09_score.py`); the E0 and E4 rows are copied from their own scores.

How the two option B tables were built (`13_option_b.py`, from
`R/data/stacked.parquet`, which is unchanged):

- **The bands.** 84 slices of the whole distribution of each source, by
  cumulative share of weight: 4 bands in the bottom 1% (0.01%, then to
  0.1%, 0.5% and 1%), 15 bands of 1% to the 16th percentile, 37 of 2% to
  the 90th, then the old top: 9 of 1% to the 99th, 9 of 0.1% to the 99.9th
  and 10 of 0.01% to the top. There is no special zero band and no
  separate loss bands. The cut points are in `band_edges_<T>.json` under
  `full.probs` and `meta.full_layout_probs`.
- **Zeros are ties, so each zero gets a place inside the zero mass.**
  B_rand: a random place (fixed seed). B_wage: a place by the unit's wages,
  higher wages higher up, ties at random; for wages itself, by interest
  plus dividends. The same rule in both sources. Nonzero amounts that are
  equal stay together in one band, as in step 1.
- **Unchanged:** other_agi and net_worth keep their step 1 bands (their own
  source's cut points). Record ids, splits and weights are those of
  `stacked.parquet`.
- **Checks passed:** every record with a slot has a band; within each
  source the bands are in dollar order, losses below zeros below positives;
  each band's top sits at its stated share of the source's weight, up to
  the weight of the single record or tie at the cut (the largest gap away
  from tied amounts is 0.007 points).
- **Zero masses.** PUF interest: shares 0 to 71.7% (bands 0 to 46; band 46
  holds zeros and amounts up to $2). SCF interest: 0 to 87.4% (bands 0 to
  54; band 54 holds zeros and amounts up to $10). The rank-dollar map sends
  every band that holds only zeros to $0, and a band that holds zeros and
  small amounts to $0 with the zeros' share of that band's weight (51% in
  PUF interest band 46).

How to read the columns: as in `09_RESULTS.md`. Holders for the option B
runs are records whose drawn dollars are not zero, because a band number
no longer says whether a unit holds the item.

Runs of the same table on different GPU types differ by about 0.1 in the
wealth ratio and 2 points of top 1% share
(`research/rebuild/minimal/10_RESULTS_A.md`), so differences inside that
range are noise.

<!-- TABLE START -->
| Run | What it changes | Net worth total, ratio to SCF (PUF / SCF label) | Top 1% share, % (actual 39.7) | Median ratio (PUF / SCF label) | Holders % int/div/kg (PUF 28.3/18.4/13.3) | Interest label swap (nats) | Wealth tilt: move (nats) / ESS ratio | Held-out loss |
|---|---|---:|---:|---:|---:|---:|---:|---:|
| E0 | control: the v2 model | 1.92 / 1.53 | 44.3 / 36.3 | 1.39 / 1.75 | 27.4/17.7/12.9 | 0.301 | 0.034 / 0.95 | 21.694 |
| E4 | shared variables on PUF dollar scale | 1.11 / 1.00 | 40.1 / 36.1 | 0.94 / 1.07 | 27.5/17.6/12.5 | 0.174 | 0.011 / 0.98 | bands differ |
| B_rand | option B, zeros placed at random | 1.78 / 1.41 | 41.1 / 45.4 | 1.08 / 0.60 | 29.0/19.2/14.1 | 0.134 | 0.032 / 0.95 | bands differ |
| B_wage | option B, zeros placed by wages | 1.16 / 0.96 | 36.6 / 42.2 | 1.31 / 0.84 | 26.4/16.6/12.0 | 3.786 | 0.012 / 0.98 | bands differ |
<!-- TABLE END -->

## The implied crosswalk for interest

For each SCF interest amount: the band it falls in, and the PUF dollar
range of that band. Under E4 the band is the PUF band holding the same
dollars. Under option B it is the band at the same share of the SCF's own
distribution. The two placements give the same band for every positive
amount: they move zeros only, and the zeros all sit below the first
positive amount.

| SCF interest | E4 band | PUF dollars in that band | Option B band (B_rand) | SCF share of units at or below the band's top (%) | PUF dollars in that band | Option B band (B_wage) |
|---:|---:|---|---:|---:|---|---:|
| $40 | 27 | $40 to $40 | 55 | 90.0 | $140 to $200 | 55 |
| $140 | 40 | $140 to $150 | 58 | 93.1 | $330 to $420 | 58 |
| $580 | 50 | $540 to $640 | 60 | 95.0 | $580 to $810 | 60 |
| $2,500 | 56 | $2,170 to $2,530 | 63 | 98.0 | $2,000 to $3,620 | 63 |
| $16,000 | 64 | $14,900 to $28,500 | 68 | 99.4 | $12,200 to $14,100 | 68 |
| $156,000 | 73 | $115,100 to $197,600 | 78 | 100.0 | $109,300 to $125,900 | 78 |

Band numbers are not comparable across the two rules (E4's band 27 is the
27th band of step 1's layout; option B's band 55 is the 56th slice of the
full distribution); the dollar ranges are. Under option B a survey $40 is
treated like a PUF $140 to $200, a survey $140 like a PUF $330 to $420,
and a survey $16,000 like a PUF $12,200 to $14,100. Small survey amounts
move up the PUF's scale and large ones move down, as section 2 of
`12_CROSSWALK_THEORY.md` predicts.

## Wealth by wage group

Mean net worth by wage group, PUF-style records (net worth drawn under the
PUF label) against the actual SCF, 2015 dollars. In brackets: ratio to the
SCF. The wage groups are 06's, in 2015 dollars.

| Wages | Share of units, SCF (%) | Share of units, PUF-style (%), E4 / B_rand / B_wage | Actual SCF | E4 | B_rand | B_wage |
|---|---:|---:|---:|---:|---:|---:|
| $0 | 30.4 | 16.4 / 16.8 / 17.0 | 663,000 | 1,362,000 (2.05) | 2,186,000 (3.30) | 955,000 (1.44) |
| $1 to $25,000 | 19.5 | 32.4 / 31.7 / 31.5 | 173,000 | 232,000 (1.34) | 467,000 (2.70) | 379,000 (2.19) |
| $25,000 to $50,000 | 19.1 | 21.6 / 21.1 / 21.1 | 181,000 | 248,000 (1.37) | 396,000 (2.19) | 300,000 (1.66) |
| $50,000 to $100,000 | 17.9 | 18.1 / 18.2 / 18.2 | 353,000 | 445,000 (1.26) | 616,000 (1.75) | 466,000 (1.32) |
| $100,000 to $200,000 | 9.3 | 9.0 / 9.4 / 9.5 | 934,000 | 937,000 (1.00) | 1,145,000 (1.23) | 1,010,000 (1.08) |
| Over $200,000 | 3.8 | 2.5 / 2.9 / 2.7 | 5,119,000 | 3,336,000 (0.65) | 5,121,000 (1.00) | 3,843,000 (0.75) |

Median net worth by wage group, same files:

| Wages | Actual SCF | E4 | B_rand | B_wage |
|---|---:|---:|---:|---:|
| $0 | 96,800 | 281,000 (2.90) | 482,000 (4.98) | 262,000 (2.71) |
| $1 to $25,000 | 8,490 | 8,690 (1.02) | 9,870 (1.16) | 31,300 (3.68) |
| $25,000 to $50,000 | 30,100 | 28,000 (0.93) | 21,600 (0.72) | 39,800 (1.32) |
| $50,000 to $100,000 | 109,000 | 106,000 (0.97) | 106,000 (0.98) | 108,000 (0.99) |
| $100,000 to $200,000 | 334,000 | 283,000 (0.85) | 362,000 (1.09) | 407,000 (1.22) |
| Over $200,000 | 1,794,000 | 1,105,000 (0.62) | 1,284,000 (0.72) | 1,268,000 (0.71) |

The PUF-style files have far fewer zero-wage units than the SCF (16% to 17%
against 30%) because the PUF holds filers only; the generated shares match
the PUF's own.

## What it shows

1. Where the zeros are placed decides the result. With zeros placed at
   random (B_rand), PUF-style wealth is 1.78 times the SCF's total, most of
   the way back to E0's 1.92. With zeros placed by wages (B_wage), the total
   is 1.16, within noise of E4's 1.11.
2. B_wage matches E4 on the total but not on the shape. Its median is 1.31
   times the SCF's (E4: 0.94), and its top 1% share is 36.6% against the
   SCF's 39.7% (E4: 40.1%).
3. By wage group, B_wage moves wealth from one end of the zero mass to the
   other. Units with no wages come closer to the SCF than under E4 (mean
   1.44 times the SCF against 2.05). Units with wages of $1 to $25,000 get
   further away (2.19 against 1.34; median 3.68 against 1.02).
4. The reason is the size of the two zero masses. The SCF has 30% of units
   with no wages, the PUF 17%. Under option B, PUF units between the 17th
   and 30th percentile of wages (wages of $700 to $10,900, bands 20 to 26)
   are matched with SCF units that have no wages. Under B_wage these are
   the SCF zero-wage units with the most interest and dividends: their mean
   net worth is $1.30 million, against $225,000 for the SCF zero-wage units
   matched with PUF units that also have no wages. Under B_rand both groups
   average about $660,000.
5. The crosswalk table shows the same rule acting on interest. A survey $40
   is treated like a PUF $140 to $200, and a survey $16,000 like a PUF
   $12,200 to $14,100. E4 keeps the survey's dollars as they are.
6. Holder shares are close to the PUF's under both (B_rand about one point
   high, B_wage one to two points low). The tilt to the six wealth targets
   costs 0.032 nats for B_rand and 0.012 for B_wage, against 0.011 for E4.
   But the tilt to the 136 IRS income targets alone costs 0.030 for B_wage,
   against 0.011 for E4 and 0.016 for B_rand.
7. The interest label swap of B_wage (3.79 nats) does not mean the same
   thing as in other rows. Inside the zero mass, B_wage's band is almost
   fixed by wages, and the zero mass covers bands 0 to 46 in the PUF and 0
   to 54 in the SCF. So the wrong label gives a sharp prediction in the
   wrong band.
8. On this evidence option B is not better than E4. It is as good on the
   total only when the zeros are placed by a prior. The prior then moves
   the error between wage groups rather than removing it.

The runs used different GPU types (B_rand an RTX A5000, B_wage an RTX PRO
6000, E4 an RTX 5000 Ada). Each B result is one training.

Files: tables `R/data/stacked_B_rand.parquet`, `stacked_B_wage.parquet`,
`band_edges_<T>.json`, `band_report_<T>.csv`, `crosswalk_<T>.json`, built by
`research/rebuild/minimal/13_option_b.py` (`13_option_b.sbatch`); full
scores in `R/score/<T>/score.md`, with the rank-dollar map check in
`score.json` under `map_check`.
