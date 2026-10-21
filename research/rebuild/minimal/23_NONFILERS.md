---
title: "Step 23: v4c with crude non-filers, then decision 18 on the real SCF"
role: evidence
workstream: rebuild
status: current
updated: 2026-10-16
sot: research/rebuild/design/plan.md
supersedes: []
superseded_by: null
---

# Step 23: crude non-filers, then the latent method on the real SCF

Asked for by John on 16 October, after the fake-survey test of decision 18
passed (`22_FAKE_SURVEY.md`).

## The non-filers here are crude and must be replaced

**These non-filers are a stopgap, not the method of record.** They exist so
that decision 18 can be tried on the real SCF now. They are to be replaced
by a systematic non-filer method built for the rebuild (plan decision 7),
and every result in this note is to be rerun on it. What is crude:

- The rows are the published CPS ASEC non-filer pool for **2017**, the
  nearest year built, standing in for 2015. Weights as published (31.2
  million units); the 2015 non-filer population was not built.
- Dollars brought back to 2015 with CPI-U only.
- Variables mapped by hand: married = joint filing status; interest =
  taxable interest with 409 negative values set to zero; dividends = the
  CPS's one dividends item; capital gains = 0 (the CPS does not ask);
  other income = business + taxable pensions + rent + unemployment
  insurance + other income, with taxable Social Security taken as zero.
- The CPS's own reporting problems in these items are ignored: the pool's
  amounts are taken as true.
- The SCF has no filing flag: the model learns who files from the PUF side
  only.
- The tilt's non-filer count for 2019 is the 2019 pool's weighted count;
  the wealth count targets stay on the SCF's own 143 million units (the
  decision 17 stopgap), so the extra units in the file are assumed to sit
  below every wealth line.

## The two runs

- **N1:** v4c with the crude non-filers added to the PUF side as rows with
  a filing flag (table E4N, `23_nonfilers.py`). The same model, generation
  and tilt as v4c, except that the 136 IRS targets count filers only and one
  more exact target sets the non-filer count.
- **N2:** N1 with decision 18's latent amounts for the SCF's interest and
  dividends (`23_train_latent.py`): no source label, SCF zeros filled in
  each pass, reporting settings learned on marriage, dependents and wages
  only. Capital gains as today.

Run: `23_run.sbatch N1` and `23_run.sbatch N2` (train, stack, tilt).

## Results

One training run (seed) each. Net worth in nominal 2019 dollars. Full
tables: scratch `tilt/step23_report.md` (`23_report.py`). v4c has filers
only; N1 and N2 have 157.8 million filers plus 29.4 million non-filers.

### The move to the 2019 targets (nats; smaller = the model starts closer)

| | 136 IRS targets alone | 136 IRS plus wealth | Records kept, effective |
|---|---:|---:|---:|
| v4c | 0.0068 | 0.0139 | 97.1% |
| N1, crude non-filers | 0.0057 | 0.0323 | 92.7% |
| N2, crude non-filers + latent amounts | 0.0059 | 0.0223 | 95.1% |

### Selected statistics, all units, before and after the tilt

| | SCF 2019 | N1 before | N1 after | N2 before | N2 after |
|---|---:|---:|---:|---:|---:|
| Total net worth ($ trillion) | 96.1 | 114.3 | 96.1 | 108.8 | 96.1 |
| Median ($) | 91,050 | 65,685 | 40,601 | 63,200 | 39,328 |
| Mean, no wages ($) | 760,852 | 909,086 | 761,336 | 845,106 | 764,506 |
| Mean, wages $200k+ ($) | 4,363,409 | 4,142,078 | 3,823,725 | 3,801,916 | 3,757,654 |
| Mean, neither interest nor dividends ($) | (not comparable) | 157,370 | 99,275 | 148,994 | 104,123 |

v4c, filers only, after the tilt: no wages $1,222,327; wages $200k+
$3,729,718.

### What the runs show

1. **The no-wage gap was mostly a comparison of different populations.**
   v4c's returns with no wages held $1.22 million against the SCF's
   $761,000, but the SCF figure covers every unit and v4c's only filers.
   With non-filers in the file, all units with no wages hold $761,000 to
   $765,000 after the tilt, the SCF's number. Filers with no wages still
   hold about $1.2 million, as in v4c.
2. **The tilt moves less with the latent method** (N2 against N1, the
   clean comparison: same non-filers): 0.022 nats against 0.032, and in
   three seeds each 0.022 to 0.025 against 0.032 to 0.042
   (`24_OVERNIGHT.md`, A). **Retracted (2026-10-17):** this note first said
   N2 also starts closer on total wealth ($108.8 against $114.3 trillion)
   and that units with neither item start 5% poorer; across three seeds
   both were noise.
3. **What it learned about the SCF:** 8.8% of SCF units reporting no
   interest are hidden holders (3.4% for dividends); holders report
   anything with chance 32% at $50, 61% at $500, 84% at $5,000; interest
   reporters give 83% of their true dollars, dividend reporters 100%.
4. **Both non-filer runs move more than v4c and end with a median far
   below the SCF's** ($40,000 against $91,000). The cause is the count
   base (confirmed 2026-10-17, `24_OVERNIGHT.md`, B: with counts on the
   file's units the median is the SCF's): the wealth count targets are the SCF's own counts on its
   143 million units (decision 17's stopgap), while the file now has 187
   million, so 44 million units are pushed below every line. So the move
   of v4c and of N1 or N2 cannot be compared directly; that needs the
   population check of decision 17 (the SCF's units and the file's units
   should agree), which the real non-filer work should settle.
5. **The untargeted income relationships fit a little worse** with
   non-filers: mean gap against the 2015 PUF 0.70 points for v4c, 1.10
   for N1, 0.99 for N2 (largest 2.8, 4.0, 4.1).

### Caveats

- One seed per run. How much a second seed would move these numbers is
  not measured here (`20_SEPARATE_MODELS.md` has a seed-to-seed spread for
  v4b; not checked against these differences), so small differences
  between N1 and N2, such as earners above $200,000, may be noise.
- **The tilt's solver is fragile:** N1's tilt failed to converge on a
  4-core node and converged on a 2-core node, with the same inputs; the
  same calculation converges in 6 to 7 steps outside the script. The
  convergence test (every target within one part in ten million) sits at
  the edge of rounding differences between core counts. Not fixed.
- The non-filers are crude (above) and every number here is to be rerun
  on the systematic non-filer method.

## Revision history

- 2026-10-16: written; the crude non-filers marked for replacement (John);
  results of N1 and N2.
- 2026-10-17: the claim that N2 starts closer on total wealth retracted
  (seed noise); the count-base cause confirmed.
