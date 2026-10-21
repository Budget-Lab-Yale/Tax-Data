---
title: "Step 24: overnight experiments for the review of 17 October"
role: evidence
workstream: rebuild
status: current
updated: 2026-10-17
sot: research/rebuild/design/plan.md
supersedes: []
superseded_by: null
---

# Step 24: overnight experiments

Asked for by John on the night of 16 October: experiments that give
evidence for the review on the 17th. Each answers one question. Job list:
scratch `logs/overnight24_jobs.txt`.

| | Question | What runs | Where |
|---|---|---|---|
| A | Is N2's gain over N1 (step 23) more than seed noise? | N1 and N2 retrained with seeds 20261017 and 20261018 (runs N1s2, N1s3, N2s2, N2s3), stack and tilt as step 23 | `23_run.sbatch`; scratch `tilt/n1s2/` etc.; `23_report.py` |
| B | Is the low median of step 23 caused by the count base (SCF's 143 million units against the file's 187 million)? | N1 and N2 tilted again with every wealth count target as the SCF's share above the line times the file's units (IRS returns plus non-filers) | `23_tilt.sbatch <run> file_units`; scratch `tilt/n1_file_units/`, `tilt/n2_file_units/` |
| C | How much does the unverifiable assumption cost (hidden holders like reporters)? | Fake survey D3 with the log-odds shift by other income at 0, 0.25, 1.5 and 3.0 (0.75 in step 22); today's rule and the latent method, settings fixed and learned; two seeds | scratch `fake24/S000/`, `S025/`, `S150/`, `S300/` |
| D | What happens when the two files hold different people? | Fake survey D1 with crude non-filer rows added to the survey only (PX), and to both piles (PB); undistorted, today's rule, latent fixed and learned; two seeds | scratch `fake24/PX/`, `fake24/PB/` |
| E | Is the undistorted survey's +19% error a sample-size effect? | Fake survey D0 and D1 with surveys of 10,000 and 90,000 records (30,000 in step 22) | scratch `fake24/N10/`, `fake24/N90/` |

Reports: `MINIMAL_F22_OUT=<scratch>/fake24/<name> python 22_fake_survey.py report`
for C to E; `23_report.py` for A and B.

## Results

All gaps are for the mean other income (amounts held between the 1st and
99th percentiles) of PUF returns with no interest, against the truth, one
number per seed, unless stated. Wealth in nominal 2019 dollars. The
non-filers of A and B are crude (`23_NONFILERS.md`).

### A. Seed replicates: N2's smaller move is real; its "closer total" was not

| | Seed 1 | Seed 2 | Seed 3 |
|---|---:|---:|---:|
| Move with wealth targets, N1 (nats) | 0.032 | 0.042 | 0.042 |
| Move with wealth targets, N2 (nats) | 0.022 | 0.022 | 0.025 |
| Net worth before the tilt, N1 ($ trillion; SCF 96.1) | 114.3 | 111.8 | 113.5 |
| Net worth before the tilt, N2 ($ trillion) | 108.8 | 113.8 | 115.0 |
| Mean net worth, neither interest nor dividends, before, N1 ($) | 157,370 | 169,377 | 165,674 |
| Mean net worth, neither interest nor dividends, before, N2 ($) | 148,994 | 166,221 | 152,850 |

The latent method's smaller move holds in every seed (ranges do not
overlap). Step 23's statements that N2 starts closer on total wealth and
that units with neither item start 5% poorer were seed noise: retracted.
Which part of the wealth distribution makes N2's move smaller is not
identified: no single wealth target's starting miss separates the two
runs across seeds. The 136-target move alone is the same (0.0057 to
0.0062). N2 seed 3's tilt missed the solver's tolerance with 2 threads
(largest gap 1.5 parts in 10 million) and converged with 1 and 4 threads;
the 1-thread result is used.

### B. The low median is the count base

| | Median after the tilt | Share negative | Move with wealth (nats) |
|---|---:|---:|---:|
| SCF 2019, all units | $91,050 | 10.7% | |
| N1, counts on the SCF's 143 million units | $40,600 | 14.2% | 0.032 |
| N1, counts on the file's 187 million units | $91,050 | 11.4% | 0.048 |
| N2, counts on the file's 187 million units | $91,050 | 11.7% | 0.047 |

With every wealth count target taken as the SCF's share above the line
times the file's units (IRS returns plus non-filers), the median and the
share negative come back to the SCF's. The tilt moves more, because the
file starts with too low a median ($61,000 to $74,000 before the tilt
across seeds). This base assumes the SCF's units and the file's units are
the same population in different numbers; the real fix is decision 17's
population check.

### C. The cost of the unverifiable assumption

The fake survey's D3: households with other income below the median
report less, above more, by a shift on the log-odds of reporting (0 = the
assumption holds; 0.75 was step 22's). The undistorted survey is at +21 and
+17.

| Shift (odds ratio between the two halves) | Today's rule | Latent, fixed | Latent, learned | Hidden holders among zeros, learned (truth) |
|---|---:|---:|---:|---:|
| 0 (1) | +40, +33 | +23, +13 | +23, +16 | 17.6, 16.9 (18.2) |
| 0.25 (1.6) | +37, +35 | +24, +14 | +24, +15 | 17.2, 17.0 (17.6) |
| 0.75 (4.5) | +34, +29 | +15, +9 | +18, +12 | 16.5, 15.7 (16.5) |
| 1.5 (20) | +24, +28 | +16, +8 | +16, +9 | 13.9, 13.2 (14.9) |
| 3.0 (403) | +11, +16 | +6, -5 | +6, -4 | 9.5, 9.2 (11.3) |

Measured from the undistorted survey (+19): today's rule is off by +18,
+17, +13, +7, -5 points across the shifts; the latent method by +1, +1,
-4, -7, -18. The two errors are about equal at a shift of 1.5, where
households in the lower half are 20 times less likely, in odds, to report
than those in the upper half with the same interest. Below that the
latent method is better; above it, today's rule. As the shift grows, the
learned count of hidden holders also falls short (9.3% against 11.3% at
3.0), since the S-curve's shape is then wrong.

### D. Non-filers in one file only

Crude non-filer rows (17% of records, 17% of weight) added to the
survey only (PX), and to both piles (PB). D1 distortion. Each variant has
its own undistorted run; the gaps are not comparable across variants.

| | Undistorted | Today's rule | Latent, fixed | Latent, learned | Hidden holders, learned (truth) |
|---|---:|---:|---:|---:|---:|
| PX: survey only | 0, -2 | +16, +17 | +6, -4 | +4, -5 | 17.1, 16.3 (16.7) |
| PB: both | +3, +7 | +21, +28 | +11, +5 | +11, +7 | 16.1, 15.8 (16.7) |

With non-filers in the survey only, the latent method still lands within
seed noise of the undistorted survey and finds the right share of hidden
holders. The population mismatch of this size did not hurt it in the fake
survey. Two cautions: the scoring pile in PX has no non-filers, so the
test asks only about filers; and the non-filers here mostly hold no
interest at all, so reading their zeros as true zeros is nearly right
anyway. A mismatch where the extra people hold the item would test it
harder.

### E. The undistorted survey's error shrinks with survey size; the rule's bias does not

| Survey records | Undistorted | Today's rule | Latent, learned |
|---|---:|---:|---:|
| 10,000 | +17, +24 | +32, +36 | +11, +16 |
| 30,000 (step 22) | +21, +17 | +39, +35 | +22, +13 |
| 90,000 | +6, +11 | +28, +33 | +13, +11 |

The undistorted survey's +19% at 30,000 records falls to +9% at 90,000:
most of that floor is too little survey data. Today's rule stays 14 to 22
points above the undistorted survey at every size: a bias, which
more data does not remove. The latent method stays within seed noise of
the undistorted survey (7 points below it at 10,000, 4 above at 90,000).

## What to take to the review

1. The latent method's gain on the real SCF (with crude non-filers) holds
   across three seeds: the tilt moves about a third less. What drives it is
   not yet identified.
2. The low median of step 23 is the count base, not the method or the
   non-filers themselves.
3. The unverifiable assumption has a measurable break-even: the latent
   method beats today's rule unless the hidden holders differ from the
   reporters very strongly (odds of reporting 20 times apart between
   halves of the other variable).
4. A modest population mismatch did not break the method in the fake
   survey; non-filers still matter for the real file's counts (B).
5. Today's rule has a bias that more survey data does not remove; the
   floor all methods share is mostly sample size.

## Revision history

- 2026-10-17: written when the jobs were submitted; results added the same night.
