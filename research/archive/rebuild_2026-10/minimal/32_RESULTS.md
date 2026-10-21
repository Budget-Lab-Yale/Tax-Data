---
title: "Step 32: a 2019 starting file that mixes SCF waves — results"
role: evidence
workstream: rebuild
status: current
updated: 2026-10-18
sot: research/rebuild/design/plan.md
supersedes: []
superseded_by: null
---

# Step 32: a 2019 starting file that mixes SCF waves

The question (John, 18 October): step 31 drew every record's net worth
under the 2016 SCF wave's year token (tax year 2015). What changes if about
80% of records draw net worth under the 2019 wave's token (tax year 2018)
and 20% under the 2016 wave's? No retraining. Jobs, code and how to
resubmit: `32_STATUS.md`. Full tables, every seed:
`$R/step32/report.md` (`$R` = `/nfs/roberts/scratch/pi_nrs36/jar335/minimal`).

## What ran

1. **The files.** For each of step 31's three trained models (seeds 1-3),
   step 31's stack was drawn again with one change: net worth drawn under
   the 2019 wave's year token. Run on the same GPU type as step 31, the PUF
   variables of all 400,000 records came out identical to step 31's (checked
   row by row), so the two stacks are the same PUF records with two net
   worth draws. Three starting files per seed:
   - **100% 2016 wave**: step 31's file as it stands;
   - **80/20**: step 31's file with its weights times 0.2 and the 2019-token
     file with its weights times 0.8 (800,000 records; each record appears
     once with each net worth draw);
   - **100% 2019 wave**: the 2019-token file.
2. **Net worth dollars.** A net worth band is a position within the wave it
   was drawn under. Main runs ("own wave"): records drawn under the 2016
   token get step 31's dollars (the 2016 wave's values), records drawn under
   the 2019 token get the 2019 wave's values. Second set ("2019 map"): every
   rank turned into dollars with the 2019 wave's values, which is what plan
   decisions 3 and 10 prescribe for a 2019 file; step 31 itself did not do
   this. Run for the 100% 2016 file and the 80/20 file (for the 100% 2019
   file the two rules are the same).
3. **The tilt**, with step 31's settings: 136 IRS targets met exactly, eight
   SCF 2019 wealth targets, the non-filer count; wealth counts on the SCF's
   143 million units and, separately, on the file's 187 million units.
4. **The reproduction check.** The 100% 2016 file run through the new code
   path reproduces step 31's tilt exactly: every weight before and after is
   identical, in all three seeds and on both bases (`$R/step32/check_m16.md`).
   The new path drawing under the 2016 token also reproduces step 31's stack
   row for row.

All 42 jobs completed; no solver retries were needed.

## 1. Net worth before the tilt

Nominal 2019 dollars, all 187 million units of the file (filers and the
crude non-filers), mean of three seeds; range over seeds in brackets. The
SCF column is the SCF's own 143 million units, so its median is not
comparable with the file's before the tilt.

| | SCF 2019 | Step 31 (100% 2016 wave) | 80/20 | 100% 2019 wave |
|---|---:|---:|---:|---:|
| Total net worth | $96.1T | $96.7T (92.2–102.4) | $101.3T (95.9–107.7) | $102.6T (96.6–110.5) |
| Median | $91,050 | $52,100 | $61,100 | $62,900 |
| Top 1% share | 38.4% | 42.8% | 42.4% | 42.4% |
| Mean, no wages | $761,000 | $785,000 | $830,000 | $842,000 |
| Mean, wages $1–25k | $222,000 | $201,000 | $215,000 | $213,000 |
| Mean, wages $25–50k | $154,000 | $188,000 | $183,000 | $188,000 |
| Mean, wages $50–100k | $332,000 | $324,000 | $336,000 | $339,000 |
| Mean, wages $100–200k | $840,000 | $784,000 | $801,000 | $825,000 |
| Mean, wages $200k and over | $4.36 million | $3.64 million | $3.88 million | $3.91 million |
| Mean, units with none of interest, dividends, capital gains | — | $102,500 | $109,100 | $110,000 |

The 2019 token raises total net worth by about $5 trillion (5%; higher in
every seed: $95.4T to $100.2T, $102.4T to $107.7T, $92.2T to $95.9T for the
80/20 file) and the top wage group's mean by about $250,000. The seed spread ($92
trillion to $102 trillion in step 31) is as large as the effect of the mix.

**Which part comes from the dollars and which from the ranks.** Turning
step 31's own ranks into dollars with the 2019 wave's values instead of the
2016 wave's (the "2019 map" rule) gives:

| | Step 31, own map | Step 31 ranks, 2019 map | 80/20, own map | 80/20, 2019 map |
|---|---:|---:|---:|---:|
| Total net worth | $96.7T | $96.0T | $101.3T | $101.1T |
| Median | $52,100 | $61,600 | $61,100 | $62,700 |
| Mean, wages $200k and over | $3.64 million | $3.59 million | $3.88 million | $3.87 million |
| Mean, none of the three items | $102,500 | $104,500 | $109,100 | $109,500 |

So the higher median comes from the 2019 wave's dollar values at the
middle ranks: step 31's ranks alone, priced with the 2019 wave, move the
median from $52,100 to $61,600. The higher total and the richer top earners
come from the ranks: under the 2019 token the model places PUF-style
records higher in the 2019 wave's distribution than the 2016 token places
them in the 2016 wave's. The same was seen in step 21 (third round): the
2018 token alone started about 5% above the SCF mean.

## 2. The tilt

| | Step 31 (100% 2016) | 80/20 | 100% 2019 | Step 31 ranks, 2019 map | 80/20, 2019 map |
|---|---:|---:|---:|---:|---:|
| Move, 136 IRS targets alone (nats) | 0.0063 | 0.0061 | 0.0063 | 0.0063 | 0.0061 |
| Move, plus 8 wealth targets, counts on SCF units | 0.0186 | 0.0220 | 0.0236 | 0.0222 | 0.0230 |
| Records kept (effective share), same | 95.6% | 95.6% | 95.2% | 95.6% | 95.4% |
| Move, plus 8 wealth targets, counts on file units | 0.0447 | 0.0462 | 0.0475 | 0.0467 | 0.0469 |
| Records kept, same | 91.4% | 91.4% | 91.1% | 91.3% | 91.3% |

Mean of three seeds. The ordering holds in every seed: on the SCF-units
base the 80/20 file moves 0.0197, 0.0241, 0.0222 nats against step 31's
0.0177, 0.0191, 0.0189.

Every target is met in every run: the 136 IRS targets and the non-filer
count exactly, the eight wealth targets to within 0.03%. The tilt hardly
moves weight between the two tokens: in the 80/20 file the 2019 token
carries 79.86% to 80.00% of the weight after the tilt.

The 80/20 file needs a slightly larger move than step 31, not a smaller
one, because it starts about 5% above the SCF total and the tilt has to
take that out. The difference (0.003 nats on the SCF-units base, 0.0015 on
the file-units base) is small next to everything else in the move.

## 3. Net worth after the tilt

Mean of three seeds, nominal 2019 dollars.

| | SCF 2019 | Step 31 | 80/20 | 100% 2019 |
|---|---:|---:|---:|---:|
| **Counts on SCF units** | | | | |
| Median | $91,050 | $38,100 | $42,000 | $42,600 |
| Top 1% share | 38.4% | 42.7% | 42.6% | 42.5% |
| Mean, no wages | $761,000 | $783,000 | $789,000 | $791,000 |
| Mean, wages $25–50k | $154,000 | $165,000 | $154,000 | $155,000 |
| Mean, wages $50–100k | $332,000 | $302,000 | $301,000 | $296,000 |
| Mean, wages $100–200k | $840,000 | $789,000 | $786,000 | $796,000 |
| Mean, wages $200k and over | $4.36 million | $3.70 million | $3.78 million | $3.77 million |
| Mean, none of the three items | — | $78,000 | $78,500 | $78,000 |
| **Counts on file units** | | | | |
| Median | $91,050 | $90,900 | $91,000 | $91,000 |
| Top 1% share | 38.4% | 38.4% | 38.4% | 38.4% |
| Mean, no wages | $761,000 | $771,000 | $773,000 | $773,000 |
| Mean, wages $25–50k | $154,000 | $177,000 | $171,000 | $171,000 |
| Mean, wages $50–100k | $332,000 | $326,000 | $322,000 | $319,000 |
| Mean, wages $100–200k | $840,000 | $785,000 | $786,000 | $791,000 |
| Mean, wages $200k and over | $4.36 million | $3.42 million | $3.46 million | $3.46 million |
| Mean, none of the three items | — | $106,400 | $106,000 | $106,100 |

Total net worth is $96.1 trillion in every run (targeted). After the tilt
the mix changes little: on the file-units base every column is within
about 2% of step 31 except the $25–50k wage group, which moves from
$177,000 to $171,000 (SCF $154,000). The top wage group gains about
$40,000 to $80,000, still about $600,000 to $900,000 short of the SCF. The
known gaps of step 31 (the count base, the top wage group) are unchanged.

## 4. What it shows

1. The mixed file works with step 31's models as they are, and the code
   path is exact: with 100% of the weight on the 2016 token it reproduces
   step 31 weight for weight.
2. Before the tilt, the 80/20 file starts about 5% richer than step 31 and
   than the SCF ($101 trillion against $96 trillion), with more wealth in
   the top wage group (closer to the SCF) and among units with no wages
   (further from it). The median rises by about $9,000, and that part comes
   from the 2019 wave's dollar values, not from the mix.
3. The tilt needs slightly more movement for the 80/20 file than for step
   31 (0.022 against 0.019 nats on the SCF-units base), and leaves almost
   the same file afterwards. On this evidence the mix does not improve the
   starting point for 2019; the 2016 token with its own dollars remains the
   cheapest to tilt, as in step 21.
4. The differences between files are of the same size as the differences
   between seeds before the tilt, but the ranking of the moves holds in
   every seed.

## Questions for John

1. **Step 31's net worth dollars are in 2016 dollars, not 2015 dollars.**
   Table E27 (like table Y1) holds each SCF wave in its own survey year's
   dollars; step 27's map (`map/V27`) took the 2016 wave's values as they
   are, while the wealth targets and the report's conversion to 2019
   dollars treat them as 2015 dollars. The map's total is 1.29% above the
   same wave in 2015 dollars (`map/V32/map_check_v32.csv`). Effect: step 31's
   net worth before the tilt is about 1.3% too high (about $1.2 trillion of
   its $96.7 trillion). The tilt removes it, since total net worth is a
   target. Step 21's maps converted correctly. Step 32 kept step 31's map
   for the 2016-token records so the comparison with step 31 is exact. Should
   later steps fix the V27 map (divide by 1.0129)?
2. **Which dollar rule for net worth.** Plan decisions 3 and 10 say every
   rank in a 2019 file gets the 2019 survey's dollar values. Step 31 used the
   2016 survey's values. Applying the decision to step 31's own ranks raises
   the move with the wealth targets from 0.0186 to 0.0222 nats (SCF-units
   base) and the median before the tilt from $52,100 to $61,600. Is step
   31's rule a departure to be recorded, or should runs follow decision 10?
