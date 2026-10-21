---
title: "Step 29: how many SCF zero-reporters are hidden holders, under three rules"
role: evidence
workstream: rebuild
status: current
updated: 2026-10-17
sot: research/rebuild/design/plan.md
supersedes: []
superseded_by: null
---

# Step 29: results

The question: how many SCF households that report no interest, dividends or
capital gains actually hold some (call them hidden holders), and how large are
their holdings, under three rules. In every rule the source label is on, the
dollar factor is fixed at 1 (reporters' dollars are taken as right), and the
model decides which zero-reporters are the hidden holders: each training pass,
each zero's amount is drawn from the SCF-labelled model's belief given the
whole record, times a factor per amount that the rule sets. The rules differ
only in that factor.

Job list, output paths and how to rerun: `29_STATUS.md`. Every table here
comes from `step29/report.md` in scratch
(`/nfs/roberts/scratch/pi_nrs36/jar335/minimal/step29/`), written by the last
job of the chain. Where this file and that file differ, that file is newer.

## What ran and what did not

Everything asked for ran (fake survey and real runs, all three rules), plus
runs added during the night: C4 and C5 (two more versions of rule 3), two
more seeds each for rules 1 and 2, and F29C6 on the fake survey. All runs
below finished, were stacked, tilted to 2019 and reported.

| Name | What it is | Seeds |
|---|---|---|
| C1 | Rule 1: one S-curve per item for every household (the step 27 method R2J with the dollar factor fixed at 1) | 5 (the spec's 3, plus 2 added after the problem described in section 2.1) |
| C2 | Rule 2: S-curve whose middle varies by cell, steepness shared | 5 (same reason) |
| C3 | Rule 3 (per-cell counts and amounts, after Larrimore, Mortenson and Splinter), fit on the 2016 wave; the resulting factor carried to the other waves | 3 |
| C4 | Rule 3, with the 2016 wave's cell shares of hidden holders imposed on every wave (added) | 3 |
| C5 | As C4, with hidden holders placed in the smallest amounts first (added) | 3 (seed 2 was killed by a scratch-disk error and rerun; the report was rewritten after it) |
| R2J, R0 | Step 27's runs, read only: R2J is one S-curve with its learned dollar factor; R0 takes SCF zeros as zeros | 3 each |

Fake survey (table F29): rules 1 and 2 and the two rule-3 spreads (F29C3 and
F29C6, the fake-survey twins of C3 and C5), two seeds each; R2J's method and
R0, one seed each. All finished.

**Cells.** Wage group by married. The wage groups are made from the wages
slices, so the boundaries fall at slice edges: none; under $24,700; $24,800 to
$76,000; $76,100 to $207,800; over $207,800 (2015 dollars on the PUF's scale;
in other waves the same slices on that wave's grown scale). Two cells had
fewer than 300 households in the 2016 wave and were merged, leaving 8 cells:

| Cell | 2016-wave SCF households | SCF units (millions) | PUF-side units (millions) |
|---|---:|---:|---:|
| not married, no wages | 1,235 | 29.9 | 35.0 |
| not married, under $25k | 740 | 19.9 | 45.5 |
| not married, $25k-75k | 1,029 | 26.3 | 36.1 |
| not married, $75k and over (two groups merged) | 355 | 6.3 | 6.5 |
| married, no wages or under $25k (two groups merged) | 1,028 | 18.2 | 20.0 |
| married, $25k-75k | 790 | 17.0 | 17.5 |
| married, $75k-200k | 950 | 17.3 | 18.1 |
| married, over $200k | 568 | 4.7 | 3.3 |

### Departures from the spec, in the order they happened

1. **Rule 3's factor is refit every 10 batches, not once per pass.** In the
   smoke test (a 4-pass trial run), refitting once per pass made the share of interest zeros
   filled as holders swing from 63% in one pass to 4% in the next: within a
   pass the model learns from the fill-ins while the factor stays fixed, so it
   overshoots. Refitting every 10 batches removed the swing.
2. **Cell merging** takes the smallest cell first (the first version merged
   unmarried $75k-200k into $25k-75k before looking at the over-$200k cell).
3. **How rule 3 spreads hidden holders over amounts.** The spec's
   slice-by-slice difference (PUF holders minus SCF reporters, floored at
   zero) puts hidden holders into the top slices, where nearly every holder
   reports and the difference is sampling noise. I tried two smoothings, both
   reported:
   - *cumulative* (C3, C4, F29C3): count the difference above each amount,
     from the largest amounts down, never letting it fall, then scale to the
     cell's number of hidden holders;
   - *smallest first* (C5, F29C6): fill slices from the smallest amounts up
     (gains and losses together, by size), each with the floored difference,
     until the cell's number of hidden holders is reached; leftover
     differences in larger slices are dropped.
   On the fake survey, neither gets the dollars right (section 1). The
   number of hidden holders does not depend on this choice.
4. **Rule 3 in waves other than 2016.** The spec says fit on the 2016 wave
   and apply to every wave. My first version (C3) carried the fitted factor
   to the other waves; there the hidden-holder share rose pass after pass
   (C3 seed 2: interest zeros filled as holders, all waves together, from 21%
   to 29%; capital gains from 11% to 35%), because nothing held it in those waves. C4 and C5 instead
   carry the 2016 wave's per-cell share of zeros that are hidden holders,
   slice by slice, and impose it in each wave on that wave's own zeros. That
   is stable (in C4 seed 2 the share of capital gains zeros filled as holders
   stayed between 11.5% and 12.7% from pass 3 to pass 80, checked every tenth
   pass; in C3 seed 2 it rose from 11% to 35%).
5. **Extra seeds for rules 1 and 2**, after capital gains fill-ins ran away in
   some runs (section 2.1). Tables average over the runs that did not run
   away ("stable seeds") and say how many.
6. **Fake survey:** capital gains are reported on the line-13 basis only
   (step 27's two-basis mix is left out, so the test is about the reporting
   rule alone). No C4 or C5 on the fake survey (it has one wave, so C4 would
   equal C3); F29C6 is C5's spread on one wave.
7. Sizes of hidden holdings are measured as the PUF's mean dollars in each
   filled slice (2015 dollars on the PUF's scale). The tilt is the usual 136
   IRS targets plus the 8 wealth targets; the restated 166-target tilt of step
   27 was not run.
8. Two scratch-disk errors ("Disk quota exceeded", about 22:31 and 23:10)
   killed five jobs; all were resubmitted. The group's scratch usage is far
   below its quota, so the cause is not known.

## 1. Fake survey

Planted: the chance a holder reports follows an S-curve whose middle (half
report) is $303 for households with no wages or wages under $25,000, $175 for
$25,000 to $75,000 and $101 for $75,000 and over; dollars exact. Of the true
holders, 40% report interest, 69% dividends, 84% capital gains.

**Hidden holders among the survey's zeros, and their dollars per zero** (mean
of seeds):

| Rule | Interest: hidden (%) | Interest: $ per zero | Dividends: hidden (%) | Dividends: $ per zero | Capital gains: hidden (%) | Capital gains: $ per zero |
|---|---:|---:|---:|---:|---:|---:|
| Truth | 19.3 | 19.9 | 6.5 | 15.6 | 3.1 | 2.5 |
| Rule 1 (one curve) | 15.4 | 18.0 | 5.6 | 14.7 | 2.2 | 1.7 |
| Rule 2 (curve per cell) | 15.7 | 18.0 | 5.3 | 14.4 | 2.1 | 2.1 |
| Rule 3, cumulative spread | 18.8 | 65.6 | 5.8 | 142.8 | 2.4 | 297.5 |
| Rule 3, smallest first | 18.8 | 18.8 | 5.8 | 8.3 | 2.4 | 0.2 |
| R2J method (learned factor) | 15.4 | 18.9 | 6.1 | 18.3 | 2.4 | 1.4 |
| R0 (zeros as zeros) | 0 | 0 | 0 | 0 | 0 | 0 |

**Other income of the PUF-half returns** (the step 22 and 26 measure: the
model's mean, under the survey label, against the truth):

| Rule | All returns | Returns with no interest |
|---|---:|---:|
| Rule 1 | +4.6% | +10.2% |
| Rule 2 | +3.4% | +9.1% |
| Rule 3, cumulative | +1.5% | +5.9% |
| Rule 3, smallest first | +1.5% | +4.0% |
| R2J method | +4.1% | +12.3% |
| R0 | +16.0% | +32.5% |

**By cell, interest** (hidden holders among zeros, %):

| Cell | Truth | Rule 1 | Rule 2 | Rule 3 (smallest first) |
|---|---:|---:|---:|---:|
| not married, no wages | 31.8 | 20.0 | 27.5 | 30.9 |
| not married, under $25k | 6.1 | 3.3 | 4.3 | 5.4 |
| not married, $25k-75k | 11.3 | 8.1 | 8.2 | 11.6 |
| not married, $75k-200k | 27.4 | 29.4 | 21.3 | 26.5 |
| married, no wages | 48.4 | 34.0 | 42.0 | 46.3 |
| married, $75k-200k | 34.3 | 37.4 | 29.6 | 34.4 |
| married, over $200k | 60.1 | 62.5 | 51.5 | 60.3 |

The other cells and items show the same pattern (`report.md` section A).

**Rule 2's learned middles** match the plant: for interest $298 and $371 in
the two unmarried low-wage cells (planted $303), $152 to $193 in the
$25,000-75,000 cells (planted $175), $70 to $100 from $75,000 (planted $101),
steepness 1.32 (planted 1.3).

What this shows: when reporting truly differs by wage group,

- rule 3 gets the number of hidden holders right overall and within 3
  points in most cells; its largest misses are in the small unmarried
  $75,000-and-over cells (interest over $200,000: 57.3% against 50.4%;
  capital gains at $75,000-200,000: 0.0% against 4.1%). It gives the best
  other-income measure;
- rules 1 and 2 under-count hidden holders by about a fifth (15.4% and 15.7%
  of interest zeros against 19.3%), mostly in the low-wage cells. Rule 2
  learns the planted middles but still under-counts at low wages and now also
  at high wages;
- rule 3's dollars depend on how hidden holders are spread over amounts, and
  neither spread is right: the cumulative spread gives 3 to 120 times the true
  dollars (sampling noise in the top slices becomes large hidden holdings),
  smallest-first gives the right interest dollars but half the dividend
  dollars and less than a tenth of capital gains dollars. Rules 1 and 2 get
  dollars closer (within a third), because the curve ties hidden holdings to
  small amounts.

## 2. Real SCF

### 2.1 Capital gains fill-ins ran away in 5 of 18 runs

In five runs, capital gains fill-ins went to nearly every zero: by the end,
99.9% of the 2021 wave's capital gains zeros were filled as holders in C1
seeds 1 and 2 and C3 seed 2 (63% in C3 seed 1), and in every wave in C2 seed 1.
The jump happens within two or three passes, partway through training (C2
seed 1: 27% of zeros filled in pass 38, 72% in pass 39, 99% in pass 40), with
the held-back loss getting worse at the same time (1.12 to 1.20). It did not
happen in R2J (learned factor 1.46 for capital gains, three seeds) or in any C4
or C5 run. With the factor at 1, the capital gains curve gives a holder of $50 a
19% chance of reporting and a holder of $500 a 37% chance (middle about
$2,100), so a reported zero is weak evidence against holding. I did not find why the jump
starts in the 2021 wave. These runs are left out of every mean below, and the
tables say how many seeds remain.

| Rule | Runs that ran away |
|---|---|
| Rule 1 (C1) | 2 of 5 (2021 wave only) |
| Rule 2 (C2) | 1 of 5 (all waves) |
| Rule 3, factor carried (C3) | 2 of 3 (2021 wave) |
| Rule 3, shares imposed per wave (C4, C5) | 0 of 6 |
| R2J | 0 of 3 |

### 2.2 Hidden holders among SCF zeros, 2016 wave (tax year 2015)

Share of SCF units reporting zero who are filled as holders (mean of five
fill-in draws and of the stable seeds):

| Rule | Interest | Dividends | Capital gains |
|---|---:|---:|---:|
| R0 | 0 | 0 | 0 |
| R2J (step 27) | 16.3% | 4.8% | 5.9% |
| Rule 1 | 15.1% | 4.7% | 6.1% |
| Rule 2 | 15.5% | 4.6% | 6.1% |
| Rule 3, factor carried (C3, 1 seed) | 19.3% | 6.2% | 7.2% |
| Rule 3, shares per wave (C4) | 19.0% | 5.8% | 7.1% |
| Rule 3, smallest first (C5) | 19.1% | 6.1% | 7.2% |

The other waves (C4): interest 19.0%, 18.7%, 18.8% of zeros in 2012, 2018
and 2021; dividends 5.9%, 5.6%, 5.8%; capital gains 7.2%, 6.8%, 7.0%. Rules
1 and 2 vary more by wave (capital gains 5.9% in 2012 to 11.5% in 2021).

**By cell** (2016 wave, hidden holders among zeros, %):

| Cell | Interest: rule 1 | rule 2 | rule 3 (C4) | Dividends: rule 1 | rule 2 | rule 3 (C4) | Capital gains: rule 1 | rule 2 | rule 3 (C4) |
|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| not married, no wages | 13.3 | 14.1 | 18.5 | 3.6 | 3.6 | 4.2 | 4.7 | 4.8 | 5.2 |
| not married, under $25k | 4.5 | 5.0 | 5.4 | 1.1 | 1.1 | 1.7 | 1.8 | 1.9 | 1.9 |
| not married, $25k-75k | 9.0 | 8.0 | 8.9 | 3.3 | 2.0 | 3.1 | 3.2 | 3.0 | 3.0 |
| not married, $75k+ | 24.1 | 23.7 | 25.9 | 9.7 | 8.9 | 10.5 | 10.9 | 8.8 | 8.7 |
| married, under $25k | 24.4 | 26.4 | 30.9 | 7.2 | 7.6 | 8.0 | 10.2 | 10.9 | 12.3 |
| married, $25k-75k | 12.4 | 15.3 | 21.9 | 2.9 | 4.9 | 7.8 | 3.6 | 4.9 | 7.4 |
| married, $75k-200k | 27.1 | 25.1 | 32.3 | 7.8 | 7.6 | 9.9 | 10.1 | 8.9 | 12.0 |
| married, over $200k | 60.2 | 57.6 | 57.8 | 27.5 | 24.6 | 26.3 | 37.1 | 36.3 | 38.7 |

Rule 3 finds more hidden holders than the curves in the cells where the SCF
reports far less than the PUF side: married with $25,000 to $75,000 of wages
(7.9% of SCF units report interest against 30.0% of PUF-side units), and
unmarried with no wages. In the married over-$200,000 cell and the unmarried
$25,000-75,000 cell the curves find about as many as rule 3.

**Rule 2's curve middles by cell** (dollars at which half of holders report,
mean of stable seeds): interest $133 (married $75k-200k) to $677 (unmarried
under $25k), except married $25k-75k at $1,474; dividends $6 to $92, except
married $25k-75k at $1,551; capital gains $283 to $2,941, except married
$25k-75k at $18,369. The steepness is shared: 0.40 interest, 0.30 dividends,
0.38 capital gains, much flatter than the fake survey's 1.3. Rule 1's single
middles: interest $289, dividends $63, capital gains $2,093. The married
$25k-75k cell stands out under every rule.

**Sizes of hidden holdings** (2016 wave; 2015 dollars on the PUF's scale):

| Rule | Interest: under $500 | Interest: mean $ | Dividends: under $500 | Dividends: mean $ | Capital gains: losses | Capital gains: mean $ |
|---|---:|---:|---:|---:|---:|---:|
| R2J | 86% | 633 | 60% | 3,875 | 24% | 9,955 |
| Rule 1 | 86% | 718 | 59% | 3,757 | 26% | 9,877 |
| Rule 2 | 86% | 672 | 61% | 3,567 | 26% | 11,947 |
| Rule 3, cumulative (C4) | 92% | 843 | 65% | 5,202 | 51% | 14,962 |
| Rule 3, smallest first (C5) | 93% | 231 | 72% | 631 | 49% | −235 |

For comparison, SCF reporters average $5,239 of interest, $15,158 of
dividends and $59,057 of capital gains on the same scale. Most hidden
holdings are small under every rule; the mean is driven by the few large ones,
and that is where the rules disagree, as on the fake survey.

**Rule 3's dollar check.** In 11 of the 24 cell-item pairs, the SCF's
reporters alone already have more dollars per unit than the PUF side (for
example married over $200,000: dividends $15,808 against $13,944; capital
gains $63,255 against $51,034). So the spec's "set sizes so the cell's
dollars match the PUF side's" cannot hold there with nonnegative hidden
holdings; rule 3 matches the count of hidden holders and leaves the dollars
where the spread puts them.

### 2.3 Holders after filling in, against the PUF side and the IRS, by wave

Share of units holding after filling in (stable seeds), the PUF side's share
(2015 PUF filers and crude non-filers, 182 million units) and the IRS share of
returns for that tax year:

| Tax year | Item | R0 | R2J | Rule 1 | Rule 2 | Rule 3 (C4) | PUF side | IRS |
|---|---|---:|---:|---:|---:|---:|---:|---:|
| 2012 | interest | 16.2 | 34.3 | 32.8 | 32.7 | 32.1 | 25.9 | 33.1 |
| 2012 | dividends | 11.8 | 17.0 | 17.0 | 16.8 | 17.0 | 15.9 | 19.3 |
| 2012 | capital gains | 7.5 | 12.5 | 13.0 | 12.5 | 14.1 | 13.4 | 15.7 |
| 2015 | interest | 12.7 | 26.9 | 25.9 | 26.2 | 29.3 | 25.9 | 28.3 |
| 2015 | dividends | 12.0 | 16.3 | 16.1 | 16.1 | 17.1 | 15.9 | 18.3 |
| 2015 | capital gains | 8.4 | 13.8 | 14.0 | 14.0 | 14.9 | 13.4 | 16.1 |
| 2018 | interest | 13.2 | 26.5 | 26.1 | 25.2 | 29.4 | 25.9 | 30.3 |
| 2018 | dividends | 11.7 | 16.2 | 16.4 | 16.0 | 16.7 | 15.9 | 18.7 |
| 2018 | capital gains | 8.5 | 13.9 | 14.6 | 14.5 | 14.7 | 13.4 | 16.5 |
| 2021 | interest | 15.3 | 29.1 | 29.2 | 29.5 | 31.2 | 25.9 | 30.5 |
| 2021 | dividends | 15.5 | 21.7 | 21.7 | 21.3 | 20.5 | 15.9 | 20.1 |
| 2021 | capital gains | 12.4 | 21.9 | 22.5 | 23.9 | 18.6 | 13.4 | 20.6 |

As counts against the IRS count of returns with the item (2015): R0 0.41,
0.61, 0.48 of the IRS count for interest, dividends, capital gains; rule 1
0.85, 0.82, 0.80; rule 3 (C4) 0.96, 0.87, 0.86. The SCF wave has 140 million
units against the IRS's 150 million returns, so a count ratio below 1 is
partly a difference in units.

Rule 3 brings the 2016 wave above the PUF side's share for interest (29.3%
against 25.9%) although it matches the PUF side cell by cell: the SCF has
fewer units than the PUF side in the cells where few hold interest (unmarried
under $25,000: 19.9 million SCF units against 45.5 million PUF-side units),
so the same cell shares give a higher overall share. Against the IRS share of
returns, rule 3 leaves 1 to 2 points to "different people" in 2015 (interest
29.3% against 28.3%, dividends 17.1% against 18.3%, capital gains 14.9%
against 16.1%). In 2021, the SCF's own reporting is closest to the IRS, and
the rules that let each wave find its own share (1, 2, R2J) land above the
IRS for capital gains, while rule 3, which imposes the 2016 shares, lands
below it (18.6% against 20.6%).

### 2.4 The tilt to 2019

Means over stable seeds. Move: how far the tilt moves the weights (the
divergence of the new weights from the old, in natural-log units);
records kept: the effective sample after the tilt as a share of the stack.

| Rule | Move, 136 IRS targets | Move, with the 8 wealth targets | Records kept |
|---|---:|---:|---:|
| R0 | 0.0058 | 0.0449 | 90.8% |
| R2J | 0.0067 | 0.0219 | 94.8% |
| Rule 1 | 0.0063 | 0.0231 | 94.8% |
| Rule 2 | 0.0061 | 0.0219 | 95.1% |
| Rule 3, factor carried (C3, 1 seed) | 0.0061 | 0.0194 | 95.7% |
| Rule 3, shares per wave (C4) | 0.0069 | 0.0186 | 95.6% |
| Rule 3, smallest first (C5) | 0.0073 | 0.0200 | 95.2% |

The move to the IRS targets is about the same in every run. With the wealth
targets, the rule-3 runs move the least (0.019 to 0.020, against 0.022 to 0.023 for the
curves and 0.045 for R0).

### 2.5 Net worth, before and after the tilt

Nominal 2019 dollars, all units (187 million); means of stable seeds. The SCF
column is the 2019 wave's own 143 million units, so its "none of the three"
share is on SCF reports and not comparable.

| | SCF 2019 | R0 before | R0 after | R2J before | R2J after | Rule 1 before | Rule 1 after | Rule 2 after | Rule 3 (C4) before | Rule 3 (C4) after | Rule 3 (C5) after |
|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| Total ($ trillion) | 96.1 | 114.3 | 96.1 | 91.4 | 96.1 | 93.0 | 96.1 | 96.1 | 92.5 | 96.1 | 96.1 |
| Median ($) | 91,050 | 76,429 | 39,562 | 55,381 | 38,486 | 57,407 | 38,859 | 38,219 | 51,221 | 38,273 | 38,503 |
| Top 1% share (%) | 38.4 | 38.0 | 42.7 | 42.0 | 42.8 | 40.6 | 42.7 | 42.7 | 42.2 | 42.7 | 42.7 |
| Mean, no wages ($) | 760,852 | 920,186 | 789,532 | 735,164 | 782,138 | 737,272 | 766,887 | 754,536 | 739,267 | 764,839 | 755,711 |
| Mean, wages $1-25k | 222,027 | 243,996 | 182,108 | 196,845 | 193,145 | 202,572 | 203,086 | 197,498 | 176,026 | 175,322 | 179,147 |
| Mean, wages $25-50k | 153,683 | 237,144 | 171,953 | 181,146 | 165,579 | 184,180 | 167,248 | 159,636 | 174,081 | 163,159 | 171,511 |
| Mean, wages $50-100k | 331,640 | 415,493 | 309,340 | 348,446 | 331,116 | 347,373 | 333,481 | 325,908 | 315,347 | 305,523 | 326,013 |
| Mean, wages $100-200k | 839,734 | 904,020 | 773,577 | 727,949 | 773,454 | 741,182 | 758,596 | 810,483 | 773,951 | 806,120 | 799,709 |
| Mean, wages $200k+ | 4,363,409 | 3,776,479 | 3,741,249 | 3,214,602 | 3,600,429 | 3,287,407 | 3,658,937 | 3,746,218 | 3,706,232 | 3,960,950 | 3,850,537 |
| Share with none of the three (%) | 80.0 | 68.3 | 67.4 | 69.2 | 67.3 | 69.3 | 67.4 | 67.4 | 69.6 | 67.4 | 67.4 |
| Mean, none of the three ($) | 220,873 | 150,424 | 85,192 | 110,123 | 83,915 | 113,263 | 83,261 | 80,058 | 105,370 | 82,152 | 81,123 |

Before the tilt every rule puts total net worth within $5 trillion of the
target ($91 to $96 trillion, against R0's $114 trillion). After the tilt the
rules are within 10% of each other in every wage group below $200,000 except
wages of $1 to $25,000 (within 16%). Rule 3 (C4) gives the highest mean for
wages over $200,000 after the tilt ($3.96 million, against $3.60 to $3.75
million for the curves and R0, and $3.85 million for C5; SCF $4.36
million). Units with none of the three items have $80,000 to $85,000 on
average after the tilt under every rule. As in step 27, every run has a
median well below the SCF's ($38,000 to $40,000 against $91,000) and a top 1%
share above it (42.7% against 38.4%).

## 3. What the results show and what they do not

- **How many.** With reporters' dollars taken as right, about 15% of the
  2016 wave's interest zeros are hidden holders under the curves and about
  19% under rule 3; dividends 4.6% against 5.8% to 6.2%; capital gains 6.1%
  against 7.1% to 7.2%. On the fake survey, where reporting truly varies by
  wage group, rule 3 found the true share (18.8% against 19.3%) and the curves
  found about four fifths of it, so the curves' lower count on the real SCF is
  what a curve does when reporting varies by cell, though the fake survey
  does not prove that is the reason on the real data.
- **At what sizes.** Most hidden holdings are small under every rule (86% to
  93% of hidden interest holders under $500), but the average size, and so
  the hidden dollars, depends on the rule and is not settled by the fake
  survey: the curves were closest there (within a third), and the two rule-3
  spreads missed in opposite directions.
- **Rule 2 adds little over rule 1.** It recovered the planted middles on the
  fake survey but did not improve the overall count. On the real SCF its
  middles are scattered (one cell, married $25k-75k, at $1,474 for interest)
  and its hidden-holder shares are within 3 points of rule 1's in every cell.
- **Stability.** With the dollar factor fixed at 1, capital gains fill-ins can
  run away under the curves (3 of 10 runs) and under rule 3 when the factor
  is carried to other waves (2 of 3). Imposing the 2016 cell shares in every
  wave (C4, C5) was stable in all runs; that version is the one to use if
  rule 3 is kept. Why the jump starts in the 2021 wave is not known.
- **The tilt and net worth** change little across rules: every rule that
  fills in zeros halves the tilt's move to the wealth targets compared with
  R0, and rule 3 moves the least. Net worth after the tilt differs mostly at
  wages over $200,000.
- **Not shown.** Whether the SCF's reporters' dollars are right (taken as
  given here; in many cells they exceed the PUF side's dollars, which rule 3
  cannot undo); whether the per-wave imposition of 2016 shares is right for
  2021, where it lands 2 points below the IRS share for capital gains; and
  rule 3 as first specified (C3), with only one stable seed.

## Code

New files: `29_train.py` (rules 1, 2, 3 and the per-wave version, options
`MINIMAL_RULE`, `MINIMAL_RULE3_ALLOC`, `MINIMAL_CELL_MIN_HH`,
`MINIMAL_H_EVERY`), `29_fake.py`, `29_report.py`, `29_run.sbatch`,
`29_tilt.sbatch`, `29_cpu.sbatch`, `29_submit.sh`, `29_STATUS.md`, this file.
No existing file was changed; no plan or design document was changed; nothing
was committed.
