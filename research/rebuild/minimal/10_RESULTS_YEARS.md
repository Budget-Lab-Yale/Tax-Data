---
title: "Minimal example: results of set B, banding the SCF's later waves"
role: evidence
workstream: rebuild
status: current
updated: 2026-10-15
sot: research/rebuild/design/plan.md
supersedes: []
superseded_by: null
---

# Results of set B: four ways to band the SCF's later waves

Set B of `10_TRANSLATION_EXPERIMENTS.md`. One model per banding rule,
trained on the three-year table (PUF 2015; SCF waves for tax years 2015,
2018 and 2021), then asked for PUF-style and SCF-style records with the year
set to each of the three years. Tables built by `10_years_tables.py`, scored
by `10_score_years.py`. Full tables per run are in
`/nfs/roberts/scratch/pi_nrs36/jar335/minimal/score/<run>/score_years.md`.

The four rules, for the SCF's four shared variables (wages, interest,
dividends, capital gains). Other income stays on the PUF's own cut points
(PUF only); net worth is cut within each wave (SCF only).

- **Y0.** Each SCF wave on its own cut points (within source and year), as
  the table was built.
- **Y1.** The PUF 2015 cut points, grown to 2018 and 2021 by the growth in
  the IRS total for that item (see "Growth factors" below).
- **Y2.** The PUF 2015 cut points unchanged in every year.
- **Y3.** The PUF 2015 cut points moved by the SCF's own growth at each
  percentile: a cut that sits at the 70th percentile of SCF 2015 holders
  moves to the 70th percentile of the year's SCF holders.

In Y1, Y2 and Y3 the 2015 SCF wave gets exactly the bands it has in table
E4 of `09_RESULTS.md` (checked record by record).

A point about the test itself. The PUF exists only for 2015, so a PUF-style
record with the year set to 2018 or 2021 is a combination the model never
saw in training. Whatever it produces there is the model's guess at how the
PUF's relation to the SCF carries over to a year with no PUF. That is what
this set tests.

How to read the columns:

- **Net worth total.** PUF-style records against the actual SCF wave of the
  same tax year (the 2016, 2019 and 2022 surveys), both in the survey
  year's dollars. Generated records have equal weights summing to the
  wave's weighted count, so the total ratio is also the mean ratio. Two
  numbers: net worth drawn under the PUF label, then under the SCF label.
  Net worth dollars come from that wave's own values in each band.
- **Median ratio**, **top 1% share** and **99th percentile ratio**: the same
  files.
- **Holders.** Share of PUF-style records with interest, dividends and
  capital gains not zero; next to it the IRS share of returns with the item
  in that year (Table 1.4: taxable interest, ordinary dividends, a Schedule
  D gain or loss), and for 2015 the weighted PUF. Then the actual SCF wave.
- **SCF-style.** The model's SCF-style records for that year against the
  same wave: net worth total and median ratio, and holder shares. This asks
  whether the model reproduces each wave it was trained on.

<!-- TABLE START -->
| Run | Tax year | Net worth total, ratio to SCF wave (PUF / SCF label) | Median ratio (PUF / SCF label) | Top 1% share, % (PUF / SCF label; actual) | p99 ratio (PUF label) | Holders % int/div/kg, PUF-style | IRS holders % | SCF wave holders % | SCF-style net worth: total / median ratio | SCF-style holders % |
|---|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| Y0 | 2015 | 1.61 / 1.50 | 1.41 / 1.43 | 38.0 / 37.2 (39.7) | 1.48 | 26.1/16.6/12.0 | 28.3/18.3/13.3 (PUF 28.3/18.4/13.3) | 12.6/12.0/8.4 | 1.02 / 0.94 | 12.5/11.8/8.1 |
| Y0 | 2018 | 1.37 / 1.33 | 1.43 / 1.52 | 38.5 / 39.0 (38.4) | 1.34 | 20.7/14.5/9.5 | 30.3/18.7/13.8 | 13.2/11.7/8.5 | 0.91 / 0.95 | 13.1/11.4/7.9 |
| Y0 | 2021 | 1.50 / 1.43 | 1.35 / 1.48 | 40.2 / 38.4 (36.5) | 1.39 | 21.2/17.4/12.3 | 30.5/20.1/17.8 | 15.3/15.5/12.4 | 1.12 / 1.04 | 16.0/16.5/13.1 |
| Y1 | 2015 | 0.96 / 0.94 | 1.03 / 1.08 | 37.6 / 38.2 (39.7) | 0.95 | 27.4/17.5/12.5 | 28.3/18.3/13.3 (PUF 28.3/18.4/13.3) | 12.6/12.0/8.4 | 1.06 / 0.97 | 12.1/11.8/8.2 |
| Y1 | 2018 | 1.04 / 1.01 | 1.22 / 1.26 | 35.3 / 37.6 (38.4) | 0.98 | 26.2/17.1/12.3 | 30.3/18.7/13.8 | 13.2/11.7/8.5 | 0.94 / 1.00 | 13.0/11.9/8.1 |
| Y1 | 2021 | 1.01 / 0.97 | 1.14 / 1.16 | 35.0 / 34.6 (36.5) | 0.96 | 25.5/19.4/14.7 | 30.5/20.1/17.8 | 15.3/15.5/12.4 | 0.88 / 1.00 | 14.9/16.1/12.7 |
| Y2 | 2015 | 0.86 / 0.84 | 1.03 / 1.11 | 35.3 / 33.7 (39.7) | 0.79 | 27.4/17.1/11.8 | 28.3/18.3/13.3 (PUF 28.3/18.4/13.3) | 12.6/12.0/8.4 | 0.87 / 1.00 | 12.5/11.7/7.8 |
| Y2 | 2018 | 1.02 / 0.97 | 1.23 / 1.25 | 36.2 / 36.1 (38.4) | 0.95 | 29.7/19.0/12.9 | 30.3/18.7/13.8 | 13.2/11.7/8.5 | 0.92 / 0.98 | 12.8/11.7/7.9 |
| Y2 | 2021 | 1.00 / 0.96 | 1.24 / 1.23 | 34.7 / 33.9 (36.5) | 0.87 | 29.2/22.4/15.5 | 30.5/20.1/17.8 | 15.3/15.5/12.4 | 0.96 / 1.09 | 15.4/16.1/11.9 |
| Y3 | 2015 | 0.96 / 0.95 | 1.07 / 1.11 | 34.8 / 35.4 (39.7) | 0.93 | 27.7/17.6/12.8 | 28.3/18.3/13.3 (PUF 28.3/18.4/13.3) | 12.6/12.0/8.4 | 1.07 / 1.01 | 12.6/12.1/8.5 |
| Y3 | 2018 | 0.93 / 0.95 | 1.30 / 1.34 | 33.4 / 35.8 (38.4) | 0.86 | 27.1/17.4/11.9 | 30.3/18.7/13.8 | 13.2/11.7/8.5 | 1.02 / 1.07 | 13.3/12.0/8.6 |
| Y3 | 2021 | 1.12 / 1.06 | 1.30 / 1.34 | 33.6 / 35.0 (36.5) | 1.00 | 28.1/20.6/15.2 | 30.5/20.1/17.8 | 15.3/15.5/12.4 | 0.96 / 1.12 | 15.6/16.6/12.9 |
<!-- TABLE END -->

## Growth factors used by Y1

The IRS all-returns totals (SOI Table 1.4, read from the aligned file
`/nfs/roberts/project/pi_nrs36/shared/raw_data/IRS-Ind/aligned/bysize_income_sources.csv`),
year over 2015. Capital gains are the Form 1040 capital gain line: capital
gain distributions plus Schedule D net gain minus Schedule D net loss. Only
the positive cut points are grown; the cut points for capital losses stay
as they are, because the $3,000 limit on deductible losses is the same
dollar figure every year. The SCF's own growth in each weighted total is
shown beside the IRS factor for comparison; it was not used.

| Variable | IRS 2018 / 2015 | IRS 2021 / 2015 | SCF 2018 / 2015 | SCF 2021 / 2015 |
|---|---:|---:|---:|---:|
| Wages | 1.112 | 1.269 | 1.070 | 1.320 |
| Taxable interest | 1.341 | 1.080 | 1.300 | 1.492 |
| Ordinary dividends | 1.234 | 1.487 | 1.005 | 1.460 |
| Capital gains | 1.312 | 2.911 | 1.067 | 2.598 |

The two sources disagree most on interest in 2021 (the IRS total fell back
toward 2015 while the SCF's rose by half) and on dividends and capital gains
in 2018.

## What the table says

**The fix found for 2015 carries over to 2018 and 2021 for the level of
wealth.** With every SCF wave cut on its own quantiles (Y0), PUF-style net
worth is too high in every year: 1.61, 1.37 and 1.50 times the SCF's mean.
Putting the SCF's shared variables on the PUF's dollar cut points, in any of
the three ways tried, brings the mean to between 0.86 and 1.12 of the SCF's
in every year, and the 99th percentile to between 0.79 and 1.00.

**The middle is less well fixed in the later years.** The median of
PUF-style net worth is 1.03 to 1.07 times the SCF's in 2015 under Y1 to Y3,
but 1.14 to 1.30 times in 2018 and 2021. Y0's median is 1.35 to 1.43 in every
year, so the later years are still better than no fix, but not as good as
2015. The top 1% share is a little low under every rule (33% to 38% against
36% to 40%).

**Which way of moving the cut points.** Growing the PUF 2015 cuts by the IRS
growth in each item (Y1) gives the best row overall: the mean is 0.96, 1.04
and 1.01 of the SCF's, the 99th percentile 0.95 to 0.98, and the median
ratios the lowest of the three rules in the later years (1.22 and 1.14). Not
growing the cuts at all (Y2) is about as good on the mean in 2018 and 2021
but worse on the median in 2021 (1.24). The SCF's own percentile growth (Y3)
has the highest medians (1.30 in both later years) and a 2021 mean of 1.12.

**How much to trust the differences between rules.** Each rule is one
training run with one seed. Y1 and Y2 cut the 2015 SCF wave identically, yet
their 2015 rows differ (mean 0.96 against 0.86, top 1% share 37.6% against
35.3%). Part of that is the later waves' bands feeding back through the
shared model; part is ordinary run-to-run variation. Differences between Y1,
Y2 and Y3 of 10% or less should not be read as settled. The gap between Y0
and the other three is far larger than that.

**Holder shares on PUF-style records in years with no PUF.** Under Y0 the
model lets the PUF-style holder shares drift toward the SCF's in the later
years: interest 20.7% in 2018 against an IRS share of returns of 30.3%. Under
the PUF cut points they stay near the IRS: Y2 is closest (interest 29.7% and
29.2%, dividends 19.0% and 22.4%, against 30.3%, 30.5%, 18.7%, 20.1%); Y1 is
three to five points low on interest. Capital gains holders are short of the
IRS in 2021 under every rule (12% to 16% against 17.8%), the year of the
largest gains.

**Each wave is reproduced on the SCF side.** SCF-style records match the
wave they are drawn for to within 13% on mean net worth and 12% on the
median in every run, and their holder shares are within about a point of
the wave's.

## Notes per run

- **Y0 (each wave on its own cut points).** The 2x problem of 09's E0, in
  every year: mean 1.37 to 1.61 of the SCF's, median 1.35 to 1.43. PUF-style
  holder shares in 2018 and 2021 fall well below the IRS's (interest about
  21% against 30%), because nothing ties a later-year PUF-style record to the
  PUF's measurement of small amounts.
- **Y1 (PUF cuts grown by IRS totals).** The best row; see above. 2015 is
  close to 09's E4 (mean 0.96 here against 0.98 there, median 1.03 against
  0.94, top 1% share 37.6% against 40.1%).
- **Y2 (PUF cuts unchanged).** Close to Y1 in the later years, and nearest
  the IRS holder shares. Its 2015 row is the weakest of the three PUF-cut
  rules (mean 0.86, 99th percentile 0.79), although its 2015 bands are the
  same as Y1's.
- **Y3 (cuts moved by SCF percentile growth).** Mean within 7% in 2015 and
  2018, 1.12 in 2021; highest medians in the later years. The rule leaves
  more empty bands in the later waves (24 of 73 positive interest bands in
  2018, against 16 in 2015, which come from the PUF's own lumps), because
  many PUF cut points fall at the same SCF percentile.

Training took about two minutes per run on one GPU; the best validation
losses are 1.6254 (Y0), 1.6249 (Y1), 1.6257 (Y2) and 1.6232 (Y3). They are
not strictly comparable, since the bands differ.

## Revision history

- 2026-10-15: written, before the runs finished; findings added when all four were scored.
